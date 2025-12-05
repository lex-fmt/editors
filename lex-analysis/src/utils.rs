use crate::inline::{extract_inline_spans, InlineSpan, InlineSpanKind};
use lex_core::lex::ast::traits::AstNode;
use lex_core::lex::ast::{
    Annotation, ContentItem, Definition, Document, Position, Session, TextContent,
};

/// Visits every text content node in the document, invoking the callback for each.
///
/// Traverses the full document tree including session titles, paragraph lines,
/// list item text, definition subjects, and annotation bodies. Useful for
/// extracting inline references or performing text-level analysis.
pub fn for_each_text_content<F>(document: &Document, f: &mut F)
where
    F: FnMut(&TextContent),
{
    for annotation in document.annotations() {
        visit_annotation_text(annotation, f);
    }
    visit_session_text(&document.root, true, f);
}

/// Visits every annotation in the document, invoking the callback for each.
///
/// Traverses the full document tree to find annotations at all levels:
/// document-level, session-level, and nested within content items like
/// paragraphs, lists, definitions, and verbatim blocks. Annotations are
/// visited in document order (top to bottom).
///
/// Use this for annotation-related features like navigation, resolution
/// toggling, or collecting annotation labels for completion.
pub fn for_each_annotation<F>(document: &Document, f: &mut F)
where
    F: FnMut(&Annotation),
{
    for annotation in document.annotations() {
        visit_annotation_recursive(annotation, f);
    }
    visit_session_annotations(&document.root, f);
}

/// Collects all annotations in the document into a vector.
///
/// Returns annotations in document order (top to bottom), including those
/// at document-level, session-level, and nested within content items.
/// This is a convenience wrapper around [`for_each_annotation`] for cases
/// where you need a collected result rather than a streaming callback.
pub fn collect_all_annotations(document: &Document) -> Vec<&Annotation> {
    let mut annotations = Vec::new();
    for annotation in document.annotations() {
        collect_annotation_recursive(annotation, &mut annotations);
    }
    collect_annotations_into(&document.root, &mut annotations);
    annotations
}

fn collect_annotations_into<'a>(session: &'a Session, out: &mut Vec<&'a Annotation>) {
    for annotation in session.annotations() {
        collect_annotation_recursive(annotation, out);
    }
    for child in session.children.iter() {
        collect_content_annotations(child, out);
    }
}

fn collect_annotation_recursive<'a>(annotation: &'a Annotation, out: &mut Vec<&'a Annotation>) {
    out.push(annotation);
    for child in annotation.children.iter() {
        collect_content_annotations(child, out);
    }
}

fn collect_content_annotations<'a>(item: &'a ContentItem, out: &mut Vec<&'a Annotation>) {
    match item {
        ContentItem::Annotation(annotation) => {
            collect_annotation_recursive(annotation, out);
        }
        ContentItem::Paragraph(paragraph) => {
            for annotation in paragraph.annotations() {
                collect_annotation_recursive(annotation, out);
            }
            for line in &paragraph.lines {
                collect_content_annotations(line, out);
            }
        }
        ContentItem::List(list) => {
            for annotation in list.annotations() {
                collect_annotation_recursive(annotation, out);
            }
            for entry in &list.items {
                collect_content_annotations(entry, out);
            }
        }
        ContentItem::ListItem(list_item) => {
            for annotation in list_item.annotations() {
                collect_annotation_recursive(annotation, out);
            }
            for child in list_item.children.iter() {
                collect_content_annotations(child, out);
            }
        }
        ContentItem::Definition(definition) => {
            for annotation in definition.annotations() {
                collect_annotation_recursive(annotation, out);
            }
            for child in definition.children.iter() {
                collect_content_annotations(child, out);
            }
        }
        ContentItem::Session(session) => collect_annotations_into(session, out),
        ContentItem::VerbatimBlock(verbatim) => {
            for annotation in verbatim.annotations() {
                collect_annotation_recursive(annotation, out);
            }
        }
        ContentItem::TextLine(_)
        | ContentItem::VerbatimLine(_)
        | ContentItem::BlankLineGroup(_) => {}
    }
}

fn visit_annotation_recursive<F>(annotation: &Annotation, f: &mut F)
where
    F: FnMut(&Annotation),
{
    f(annotation);
    for child in annotation.children.iter() {
        visit_content_annotations(child, f);
    }
}

fn visit_session_annotations<F>(session: &Session, f: &mut F)
where
    F: FnMut(&Annotation),
{
    for annotation in session.annotations() {
        visit_annotation_recursive(annotation, f);
    }
    for child in session.children.iter() {
        visit_content_annotations(child, f);
    }
}

fn visit_content_annotations<F>(item: &ContentItem, f: &mut F)
where
    F: FnMut(&Annotation),
{
    match item {
        ContentItem::Annotation(annotation) => {
            visit_annotation_recursive(annotation, f);
        }
        ContentItem::Paragraph(paragraph) => {
            for annotation in paragraph.annotations() {
                visit_annotation_recursive(annotation, f);
            }
            for line in &paragraph.lines {
                visit_content_annotations(line, f);
            }
        }
        ContentItem::List(list) => {
            for annotation in list.annotations() {
                visit_annotation_recursive(annotation, f);
            }
            for entry in &list.items {
                visit_content_annotations(entry, f);
            }
        }
        ContentItem::ListItem(list_item) => {
            for annotation in list_item.annotations() {
                visit_annotation_recursive(annotation, f);
            }
            for child in list_item.children.iter() {
                visit_content_annotations(child, f);
            }
        }
        ContentItem::Definition(definition) => {
            for annotation in definition.annotations() {
                visit_annotation_recursive(annotation, f);
            }
            for child in definition.children.iter() {
                visit_content_annotations(child, f);
            }
        }
        ContentItem::Session(session) => visit_session_annotations(session, f),
        ContentItem::VerbatimBlock(verbatim) => {
            for annotation in verbatim.annotations() {
                visit_annotation_recursive(annotation, f);
            }
        }
        ContentItem::TextLine(_)
        | ContentItem::VerbatimLine(_)
        | ContentItem::BlankLineGroup(_) => {}
    }
}

pub fn find_definition_by_subject<'a>(
    document: &'a Document,
    target: &str,
) -> Option<&'a Definition> {
    find_definitions_by_subject(document, target)
        .into_iter()
        .next()
}

pub fn find_definitions_by_subject<'a>(
    document: &'a Document,
    target: &str,
) -> Vec<&'a Definition> {
    let normalized = normalize_key(target);
    if normalized.is_empty() {
        return Vec::new();
    }
    let mut matches = Vec::new();
    for annotation in document.annotations() {
        collect_definitions(annotation.children.iter(), &normalized, &mut matches);
    }
    collect_definitions(document.root.children.iter(), &normalized, &mut matches);
    matches
}

pub fn find_definition_at_position(document: &Document, position: Position) -> Option<&Definition> {
    for annotation in document.annotations() {
        if let Some(definition) = find_definition_in_items(annotation.children.iter(), position) {
            return Some(definition);
        }
    }
    find_definition_in_items(document.root.children.iter(), position)
}

pub fn find_annotation_at_position(document: &Document, position: Position) -> Option<&Annotation> {
    for annotation in document.annotations() {
        if annotation.header_location().contains(position) {
            return Some(annotation);
        }
        if let Some(found) = find_annotation_in_items(annotation.children.iter(), position) {
            return Some(found);
        }
    }
    find_annotation_in_session(&document.root, position, true)
}

pub fn find_session_at_position(document: &Document, position: Position) -> Option<&Session> {
    find_session_in_branch(&document.root, position, true)
}

pub fn find_sessions_by_identifier<'a>(
    document: &'a Document,
    identifier: &str,
) -> Vec<&'a Session> {
    let normalized = normalize_key(identifier);
    if normalized.is_empty() {
        return Vec::new();
    }
    let mut matches = Vec::new();
    collect_sessions_by_identifier(&document.root, &normalized, &mut matches, true);
    matches
}

pub fn session_identifier(session: &Session) -> Option<String> {
    extract_session_identifier(session.title.as_string())
}

pub fn reference_span_at_position(document: &Document, position: Position) -> Option<InlineSpan> {
    let mut result = None;
    for_each_text_content(document, &mut |text| {
        if result.is_some() {
            return;
        }
        for span in extract_inline_spans(text) {
            if matches!(span.kind, InlineSpanKind::Reference(_)) && span.range.contains(position) {
                result = Some(span);
                break;
            }
        }
    });
    result
}

fn visit_session_text<F>(session: &Session, is_root: bool, f: &mut F)
where
    F: FnMut(&TextContent),
{
    if !is_root {
        f(&session.title);
    }
    for annotation in session.annotations() {
        visit_annotation_text(annotation, f);
    }
    for child in session.children.iter() {
        visit_content_text(child, f);
    }
}

fn visit_annotation_text<F>(annotation: &Annotation, f: &mut F)
where
    F: FnMut(&TextContent),
{
    for child in annotation.children.iter() {
        visit_content_text(child, f);
    }
}

fn visit_content_text<F>(item: &ContentItem, f: &mut F)
where
    F: FnMut(&TextContent),
{
    match item {
        ContentItem::Paragraph(paragraph) => {
            for line in &paragraph.lines {
                if let ContentItem::TextLine(text_line) = line {
                    f(&text_line.content);
                }
            }
            for annotation in paragraph.annotations() {
                visit_annotation_text(annotation, f);
            }
        }
        ContentItem::Session(session) => visit_session_text(session, false, f),
        ContentItem::List(list) => {
            for annotation in list.annotations() {
                visit_annotation_text(annotation, f);
            }
            for entry in &list.items {
                if let ContentItem::ListItem(list_item) = entry {
                    for text in &list_item.text {
                        f(text);
                    }
                    for annotation in list_item.annotations() {
                        visit_annotation_text(annotation, f);
                    }
                    for child in list_item.children.iter() {
                        visit_content_text(child, f);
                    }
                }
            }
        }
        ContentItem::ListItem(list_item) => {
            for text in &list_item.text {
                f(text);
            }
            for annotation in list_item.annotations() {
                visit_annotation_text(annotation, f);
            }
            for child in list_item.children.iter() {
                visit_content_text(child, f);
            }
        }
        ContentItem::Definition(definition) => {
            f(&definition.subject);
            for annotation in definition.annotations() {
                visit_annotation_text(annotation, f);
            }
            for child in definition.children.iter() {
                visit_content_text(child, f);
            }
        }
        ContentItem::Annotation(annotation) => visit_annotation_text(annotation, f),
        _ => {}
    }
}

fn collect_definitions<'a>(
    items: impl Iterator<Item = &'a ContentItem>,
    target: &str,
    matches: &mut Vec<&'a Definition>,
) {
    for item in items {
        collect_definitions_in_content(item, target, matches);
    }
}

fn collect_definitions_in_content<'a>(
    item: &'a ContentItem,
    target: &str,
    matches: &mut Vec<&'a Definition>,
) {
    match item {
        ContentItem::Definition(definition) => {
            if subject_matches(definition, target) {
                matches.push(definition);
            }
            collect_definitions(definition.children.iter(), target, matches);
        }
        ContentItem::Session(session) => {
            collect_definitions(session.children.iter(), target, matches);
        }
        ContentItem::List(list) => {
            for entry in &list.items {
                if let ContentItem::ListItem(list_item) = entry {
                    collect_definitions(list_item.children.iter(), target, matches);
                }
            }
        }
        ContentItem::ListItem(list_item) => {
            collect_definitions(list_item.children.iter(), target, matches);
        }
        ContentItem::Annotation(annotation) => {
            collect_definitions(annotation.children.iter(), target, matches);
        }
        ContentItem::Paragraph(paragraph) => {
            for annotation in paragraph.annotations() {
                collect_definitions(annotation.children.iter(), target, matches);
            }
        }
        _ => {}
    }
}

fn find_definition_in_items<'a>(
    items: impl Iterator<Item = &'a ContentItem>,
    position: Position,
) -> Option<&'a Definition> {
    for item in items {
        if let Some(definition) = find_definition_in_content(item, position) {
            return Some(definition);
        }
    }
    None
}

fn find_definition_in_content(item: &ContentItem, position: Position) -> Option<&Definition> {
    match item {
        ContentItem::Definition(definition) => {
            if definition
                .header_location()
                .map(|range| range.contains(position))
                .unwrap_or_else(|| definition.range().contains(position))
            {
                return Some(definition);
            }
            find_definition_in_items(definition.children.iter(), position)
        }
        ContentItem::Session(session) => {
            find_definition_in_items(session.children.iter(), position)
        }
        ContentItem::List(list) => list.items.iter().find_map(|entry| match entry {
            ContentItem::ListItem(list_item) => {
                find_definition_in_items(list_item.children.iter(), position)
            }
            _ => None,
        }),
        ContentItem::ListItem(list_item) => {
            find_definition_in_items(list_item.children.iter(), position)
        }
        ContentItem::Annotation(annotation) => {
            find_definition_in_items(annotation.children.iter(), position)
        }
        ContentItem::Paragraph(paragraph) => paragraph
            .annotations()
            .iter()
            .find_map(|annotation| find_definition_in_items(annotation.children.iter(), position)),
        _ => None,
    }
}

fn find_annotation_in_session(
    session: &Session,
    position: Position,
    is_root: bool,
) -> Option<&Annotation> {
    if !is_root {
        if let Some(annotation) = session
            .annotations()
            .iter()
            .find(|ann| ann.header_location().contains(position))
        {
            return Some(annotation);
        }
    }
    for child in session.children.iter() {
        if let Some(annotation) = find_annotation_in_content(child, position) {
            return Some(annotation);
        }
    }
    None
}

fn find_annotation_in_content(item: &ContentItem, position: Position) -> Option<&Annotation> {
    match item {
        ContentItem::Paragraph(paragraph) => paragraph
            .annotations()
            .iter()
            .find(|ann| ann.header_location().contains(position))
            .or_else(|| find_annotation_in_items(paragraph.lines.iter(), position)),
        ContentItem::Session(session) => find_annotation_in_session(session, position, false),
        ContentItem::List(list) => {
            if let Some(annotation) = list
                .annotations()
                .iter()
                .find(|ann| ann.header_location().contains(position))
            {
                return Some(annotation);
            }
            for entry in &list.items {
                if let ContentItem::ListItem(list_item) = entry {
                    if let Some(annotation) = list_item
                        .annotations()
                        .iter()
                        .find(|ann| ann.header_location().contains(position))
                    {
                        return Some(annotation);
                    }
                    if let Some(found) =
                        find_annotation_in_items(list_item.children.iter(), position)
                    {
                        return Some(found);
                    }
                }
            }
            None
        }
        ContentItem::ListItem(list_item) => list_item
            .annotations()
            .iter()
            .find(|ann| ann.header_location().contains(position))
            .or_else(|| find_annotation_in_items(list_item.children.iter(), position)),
        ContentItem::Definition(definition) => definition
            .annotations()
            .iter()
            .find(|ann| ann.header_location().contains(position))
            .or_else(|| find_annotation_in_items(definition.children.iter(), position)),
        ContentItem::Annotation(annotation) => {
            if annotation.header_location().contains(position) {
                return Some(annotation);
            }
            find_annotation_in_items(annotation.children.iter(), position)
        }
        ContentItem::VerbatimBlock(verbatim) => verbatim
            .annotations()
            .iter()
            .find(|ann| ann.header_location().contains(position))
            .or_else(|| find_annotation_in_items(verbatim.children.iter(), position)),
        ContentItem::TextLine(_) => None,
        _ => None,
    }
}

fn find_annotation_in_items<'a>(
    items: impl Iterator<Item = &'a ContentItem>,
    position: Position,
) -> Option<&'a Annotation> {
    for item in items {
        if let Some(annotation) = find_annotation_in_content(item, position) {
            return Some(annotation);
        }
    }
    None
}

fn find_session_in_branch(
    session: &Session,
    position: Position,
    is_root: bool,
) -> Option<&Session> {
    if !is_root {
        if let Some(header) = session.header_location() {
            if header.contains(position) {
                return Some(session);
            }
        }
    }
    for child in session.children.iter() {
        if let ContentItem::Session(child_session) = child {
            if let Some(found) = find_session_in_branch(child_session, position, false) {
                return Some(found);
            }
        }
    }
    None
}

fn collect_sessions_by_identifier<'a>(
    session: &'a Session,
    target: &str,
    matches: &mut Vec<&'a Session>,
    is_root: bool,
) {
    if !is_root {
        let title = session.title.as_string();
        let normalized_title = title.trim().to_ascii_lowercase();
        let title_matches =
            normalized_title.starts_with(target) && has_session_boundary(title, target.len());
        let identifier_matches = session_identifier(session)
            .as_deref()
            .map(|id| id.to_ascii_lowercase() == target)
            .unwrap_or(false);
        if title_matches || identifier_matches {
            matches.push(session);
        }
    }
    for child in session.children.iter() {
        if let ContentItem::Session(child_session) = child {
            collect_sessions_by_identifier(child_session, target, matches, false);
        }
    }
}

fn has_session_boundary(title: &str, len: usize) -> bool {
    let trimmed = title.trim();
    if trimmed.len() <= len {
        return trimmed.len() == len;
    }
    matches!(
        trimmed.chars().nth(len),
        Some(ch) if matches!(ch, ' ' | '\t' | ':' | '.')
    )
}

fn subject_matches(definition: &Definition, target: &str) -> bool {
    normalize_key(definition.subject.as_string()).eq(target)
}

fn normalize_key(input: &str) -> String {
    input.trim().to_ascii_lowercase()
}

fn extract_session_identifier(title: &str) -> Option<String> {
    let trimmed = title.trim();
    if trimmed.is_empty() {
        return None;
    }
    let mut identifier = String::new();
    for ch in trimmed.chars() {
        if ch.is_ascii_digit() || ch == '.' {
            identifier.push(ch);
        } else {
            break;
        }
    }
    if identifier.ends_with('.') {
        identifier.pop();
    }
    if identifier.is_empty() {
        None
    } else {
        Some(identifier)
    }
}
