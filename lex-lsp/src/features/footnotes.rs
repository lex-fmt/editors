use lex_analysis::inline::{extract_inline_spans, InlineSpanKind};
use lex_analysis::utils::{collect_all_annotations, find_notes_session};
use lex_core::lex::ast::traits::AstNode;
use lex_core::lex::ast::{ContentItem, Document, Range, Session, TextContent};
use lex_core::lex::inlines::ReferenceType;
use std::collections::HashMap;

/// Reorders footnotes in the document to be sequential (1, 2, 3...) based on appearance.
/// Returns the new document content.
pub fn reorder_footnotes(document: &Document, source: &str) -> String {
    let mut references = Vec::new();

    // 1. Collect all footnote references in order
    // Use local traversal to avoid issues with lex-analysis traversal
    traverse_document(document, &mut |text| {
        let spans = extract_inline_spans(text);
        for span in spans {
            if let InlineSpanKind::Reference(ReferenceType::FootnoteNumber { number }) = span.kind {
                references.push((number, span.range));
            }
        }
    });

    let mut mapping: HashMap<u32, u32> = HashMap::new();
    let mut intended_reference_replacements = Vec::new();
    let mut next_id = 1;

    for (old_number, range) in references {
        let new_number = *mapping.entry(old_number).or_insert_with(|| {
            let n = next_id;
            next_id += 1;
            n
        });
        intended_reference_replacements.push((range, new_number));
    }

    // 2. Identify Definition replacements
    // Iterate over all footnote definitions (Annotations, List Items, Legacy Sessions)
    let mut definition_replacements = Vec::new();
    let offsets = line_offsets(source);

    // Annotations
    let annotations = collect_all_annotations(document);
    for annotation in annotations {
        let label_str = annotation.data.label.value.trim();
        if let Ok(old_number) = label_str.parse::<u32>() {
            if let Some(&new_number) = mapping.get(&old_number) {
                definition_replacements.push((annotation.data.label.location.clone(), new_number));
            }
        }
    }

    // Notes Session
    if let Some(notes_session) = find_notes_session(document) {
        for child in &notes_session.children {
            match child {
                ContentItem::List(l) => {
                    for item in &l.items {
                        if let ContentItem::ListItem(li) = item {
                            let marker = li.marker();
                            let label = marker
                                .trim()
                                .trim_end_matches(['.', ')', ':'].as_ref())
                                .trim();
                            if let Ok(old_number) = label.parse::<u32>() {
                                if let Some(&new_number) = mapping.get(&old_number) {
                                    if let Some(range) =
                                        find_marker_range(source, li.range(), marker, &offsets)
                                    {
                                        definition_replacements.push((range, new_number));
                                    }
                                }
                            }
                        }
                    }
                }
                ContentItem::Paragraph(p) => {
                    if let Some(ContentItem::TextLine(line)) = p.lines.first() {
                        let raw = line.text();
                        if let Some((num_str, _)) = raw.split_once('.') {
                            let trimmed = num_str.trim();
                            if let Ok(old_number) = trimmed.parse::<u32>() {
                                if let Some(&new_number) = mapping.get(&old_number) {
                                    let marker = format!("{}.", trimmed);
                                    if let Some(range) =
                                        find_marker_range(source, p.range(), &marker, &offsets)
                                    {
                                        definition_replacements.push((range, new_number));
                                    }
                                }
                            }
                        }
                    }
                }
                ContentItem::Session(s) => {
                    let title = s.title.as_string();
                    if let Some((number, _)) = split_numbered_title(title) {
                        if let Ok(old_number) = number.parse::<u32>() {
                            if let Some(&new_number) = mapping.get(&old_number) {
                                if let Some(range) =
                                    find_title_number_range(source, s.range(), number, &offsets)
                                {
                                    definition_replacements.push((range, new_number));
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // 3. Apply replacements
    #[derive(Clone, Copy)]
    enum ReplacementKind {
        Reference(u32),
        Definition(u32),
    }

    let mut edits: Vec<(Range, ReplacementKind)> = Vec::new();
    for (range, new_val) in intended_reference_replacements {
        edits.push((range, ReplacementKind::Reference(new_val)));
    }
    for (range, new_val) in definition_replacements {
        edits.push((range, ReplacementKind::Definition(new_val)));
    }

    // Convert Range to (start_byte, end_byte, kind)
    let mut byte_edits: Vec<(usize, usize, ReplacementKind)> = edits
        .iter()
        .map(|(range, kind)| {
            let start = pos_to_byte(&offsets, range.start);
            let end = pos_to_byte(&offsets, range.end);
            (start, end, *kind)
        })
        .collect();

    // Sort by start desc
    // Note: If ranges overlap, this handling is naive. But references/definitions shouldn't overlap.
    byte_edits.sort_by(|a, b| b.0.cmp(&a.0));

    let mut new_source = source.to_string();
    for (start, end, kind) in byte_edits {
        if start <= end && end <= new_source.len() {
            let original = &new_source[start..end];
            let replacement = match kind {
                ReplacementKind::Reference(n) => n.to_string(),
                ReplacementKind::Definition(n) => {
                    // Preserve padding
                    let leading_space = original
                        .chars()
                        .take_while(|c| c.is_whitespace())
                        .collect::<String>();
                    let trailing_space = original
                        .chars()
                        .rev()
                        .take_while(|c| c.is_whitespace())
                        .collect::<String>()
                        .chars()
                        .rev()
                        .collect::<String>();
                    format!("{}{}{}", leading_space, n, trailing_space)
                }
            };

            new_source.replace_range(start..end, &replacement);
        }
    }

    new_source
}

fn line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

fn pos_to_byte(offsets: &[usize], pos: lex_core::lex::ast::Position) -> usize {
    let line_start = *offsets
        .get(pos.line)
        .unwrap_or(offsets.last().unwrap_or(&0));
    line_start + pos.column
}

/// Finds the byte range of the numeric part of a list marker within an item's range.
fn find_marker_range(
    source: &str,
    item_range: &Range,
    marker: &str,
    offsets: &[usize],
) -> Option<Range> {
    let start_byte = pos_to_byte(offsets, item_range.start);
    let end_byte = pos_to_byte(offsets, item_range.end);

    if start_byte >= source.len() {
        return None;
    }

    let search_area = if end_byte > source.len() {
        &source[start_byte..]
    } else {
        &source[start_byte..end_byte]
    };

    if let Some(idx) = search_area.find(marker) {
        // Target only the number part, not the punctuation (e.g., "1" not "1.")
        let number_part = marker.trim_end_matches(['.', ')', ':'].as_ref()).trim();
        if let Some(inner_idx) = search_area[idx..].find(number_part) {
            let abs_start = start_byte + idx + inner_idx;
            let abs_end = abs_start + number_part.len();
            return Some(byte_to_range(abs_start, abs_end, offsets));
        }
    }
    None
}

/// Finds the byte range of the number in a session title (e.g., "1" in "1. Note Title").
fn find_title_number_range(
    source: &str,
    session_range: &Range,
    number_str: &str,
    offsets: &[usize],
) -> Option<Range> {
    let start_byte = pos_to_byte(offsets, session_range.start);
    if start_byte >= source.len() {
        return None;
    }

    // Search only in the first line (the title line)
    let search_area = &source[start_byte..];
    let line_end = search_area.find('\n').unwrap_or(search_area.len());
    let line = &search_area[..line_end];

    if let Some(idx) = line.find(number_str) {
        let abs_start = start_byte + idx;
        let abs_end = abs_start + number_str.len();
        return Some(byte_to_range(abs_start, abs_end, offsets));
    }
    None
}

fn byte_to_range(start: usize, end: usize, offsets: &[usize]) -> Range {
    // Reverse lookup from byte to Position
    let start_pos = byte_to_pos(start, offsets);
    let end_pos = byte_to_pos(end, offsets);
    Range {
        start: start_pos,
        end: end_pos,
        span: start..end,
    }
}

fn byte_to_pos(byte: usize, offsets: &[usize]) -> lex_core::lex::ast::Position {
    // Binary search or linear scan
    let mut line = 0;
    for (i, &off) in offsets.iter().enumerate() {
        if off > byte {
            break;
        }
        line = i;
    }
    let line_start = offsets[line];
    let col = byte - line_start;
    lex_core::lex::ast::Position::new(line, col)
}

/// Splits a numbered title like "1. Note Title" into number and remainder.
///
/// Returns `Some(("1", ". Note Title"))` for valid numbered titles, `None` otherwise.
/// The number excludes the dot to allow direct parsing as u32.
fn split_numbered_title(title: &str) -> Option<(&str, &str)> {
    let title: &str = title.trim();
    let number_len = title.chars().take_while(|c| c.is_ascii_digit()).count();
    if number_len > 0 && title.chars().nth(number_len) == Some('.') {
        let (num, rest) = title.split_at(number_len);
        return Some((num, rest));
    }
    None
}

fn traverse_document<F>(document: &Document, f: &mut F)
where
    F: FnMut(&TextContent),
{
    f(&document.root.title);
    visit_session(&document.root, true, f);
    for annotation in document.annotations() {
        for child in annotation.children.iter() {
            visit_content(child, f);
        }
    }
}

fn visit_session<F>(session: &Session, is_root: bool, f: &mut F)
where
    F: FnMut(&TextContent),
{
    if !is_root {
        f(&session.title);
    }
    for child in &session.children {
        visit_content(child, f);
    }
    for annotation in session.annotations() {
        for child in annotation.children.iter() {
            visit_content(child, f);
        }
    }
}

fn visit_content<F>(item: &ContentItem, f: &mut F)
where
    F: FnMut(&TextContent),
{
    match item {
        ContentItem::Paragraph(p) => {
            for line in &p.lines {
                if let ContentItem::TextLine(l) = line {
                    f(&l.content);
                }
            }
        }
        ContentItem::Session(s) => visit_session(s, false, f),
        ContentItem::Definition(d) => {
            for child in &d.children {
                visit_content(child, f);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    #[test]
    fn reorders_references_and_definitions() {
        let source = "Ref [2] and [1].\n\n:: 1 ::\nNote 1.\n::\n\n:: 2 ::\nNote 2.\n::\n";
        let doc = parsing::parse_document(source).unwrap();
        println!("AST: {:#?}", doc);
        let new_source = reorder_footnotes(&doc, source);

        // Expected: First ref [2] becomes [1]. Second ref [1] becomes [2].
        // Definitions: :: 2 :: -> :: 1 ::, :: 1 :: -> :: 2 ::.

        let expected = "Ref [1] and [2].\n\n:: 2 ::\nNote 1.\n::\n\n:: 1 ::\nNote 2.\n::\n";
        assert_eq!(new_source, expected);
    }

    #[test]
    fn keeps_correct_order_for_repeated_refs() {
        let source = "Ref [10] then [10] then [5].\n\n:: 5 ::\nContent.\n::";
        // 10 appears first -> becomes 1.
        // 5 appears second -> becomes 2.
        // :: 5 :: -> :: 2 ::
        // :: 10 :: doesn't exist, so no def update for 10.

        let doc = parsing::parse_document(source).unwrap();
        let new_source = reorder_footnotes(&doc, source);

        let expected = "Ref [1] then [1] then [2].\n\n:: 2 ::\nContent.\n::";
        assert_eq!(new_source, expected);
    }

    #[test]
    fn reorders_list_items_and_legacy_sessions() {
        // [2] -> [1], [1] -> [2]
        // Notes:
        // 1. Note A (List Item) -> Should become 2.
        // 2. Note B (Session) -> Should become 1.
        let source = "Ref [2] and [1].\n\nNotes:\n\n    1. Note A\n\n    2. Note B";
        let doc = parsing::parse_document(source).unwrap();
        // println!("AST: {:#?}", doc);
        println!("AST: {:#?}", doc);
        let new_source = reorder_footnotes(&doc, source);

        let expected = "Ref [1] and [2].\n\nNotes:\n\n    2. Note A\n\n    1. Note B";
        assert_eq!(new_source, expected);
    }
}
