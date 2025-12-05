use crate::inline::{extract_inline_spans, InlineSpanKind};
use crate::reference_targets::{
    targets_from_annotation, targets_from_definition, targets_from_reference_type,
    targets_from_session, ReferenceTarget,
};
use crate::utils::{
    find_annotation_at_position, find_definition_at_position, find_definitions_by_subject,
    find_session_at_position, find_sessions_by_identifier, for_each_text_content,
    reference_span_at_position,
};
use lex_core::lex::ast::traits::AstNode;
use lex_core::lex::ast::{Document, Position, Range};

pub fn find_references(
    document: &Document,
    position: Position,
    include_declaration: bool,
) -> Vec<Range> {
    let targets = determine_targets(document, position);
    if targets.is_empty() {
        return Vec::new();
    }

    let mut ranges = Vec::new();
    if include_declaration {
        ranges.extend(declaration_ranges(document, &targets));
    }
    ranges.extend(reference_occurrences(document, &targets));
    dedup_ranges(&mut ranges);
    ranges
}

fn determine_targets(document: &Document, position: Position) -> Vec<ReferenceTarget> {
    if let Some(span) = reference_span_at_position(document, position) {
        if let InlineSpanKind::Reference(reference_type) = span.kind {
            let targets = targets_from_reference_type(&reference_type);
            if !targets.is_empty() {
                return targets;
            }
        }
    }

    if let Some(annotation) = find_annotation_at_position(document, position) {
        let targets = targets_from_annotation(annotation);
        if !targets.is_empty() {
            return targets;
        }
    }

    if let Some(definition) = find_definition_at_position(document, position) {
        let targets = targets_from_definition(definition);
        if !targets.is_empty() {
            return targets;
        }
    }

    if let Some(session) = find_session_at_position(document, position) {
        let targets = targets_from_session(session);
        if !targets.is_empty() {
            return targets;
        }
    }

    Vec::new()
}

fn declaration_ranges(document: &Document, targets: &[ReferenceTarget]) -> Vec<Range> {
    let mut ranges = Vec::new();
    for target in targets {
        match target {
            ReferenceTarget::AnnotationLabel(label) => {
                for annotation in document.find_annotations_by_label(label) {
                    ranges.push(annotation.header_location().clone());
                }
            }
            ReferenceTarget::CitationKey(key) => {
                let annotations = document.find_annotations_by_label(key);
                if annotations.is_empty() {
                    ranges.extend(definition_ranges(document, key));
                } else {
                    for annotation in annotations {
                        ranges.push(annotation.header_location().clone());
                    }
                }
            }
            ReferenceTarget::DefinitionSubject(subject) => {
                ranges.extend(definition_ranges(document, subject));
            }
            ReferenceTarget::Session(identifier) => {
                for session in find_sessions_by_identifier(document, identifier) {
                    if let Some(header) = session.header_location() {
                        ranges.push(header.clone());
                    } else {
                        ranges.push(session.range().clone());
                    }
                }
            }
        }
    }
    ranges
}

fn definition_ranges(document: &Document, subject: &str) -> Vec<Range> {
    find_definitions_by_subject(document, subject)
        .into_iter()
        .map(|definition| {
            definition
                .header_location()
                .cloned()
                .unwrap_or_else(|| definition.range().clone())
        })
        .collect()
}

fn reference_occurrences(document: &Document, targets: &[ReferenceTarget]) -> Vec<Range> {
    let mut matches = Vec::new();
    for_each_text_content(document, &mut |text| {
        for span in extract_inline_spans(text) {
            if let InlineSpanKind::Reference(reference_type) = span.kind {
                if targets
                    .iter()
                    .any(|target| reference_matches(&reference_type, target))
                {
                    matches.push(span.range.clone());
                }
            }
        }
    });
    matches
}

fn reference_matches(
    reference: &lex_core::lex::inlines::ReferenceType,
    target: &ReferenceTarget,
) -> bool {
    use lex_core::lex::inlines::ReferenceType;
    match (reference, target) {
        (ReferenceType::FootnoteLabeled { label }, ReferenceTarget::AnnotationLabel(expected)) => {
            label.eq_ignore_ascii_case(expected)
        }
        (ReferenceType::FootnoteNumber { number }, ReferenceTarget::AnnotationLabel(expected)) => {
            expected == &number.to_string()
        }
        (ReferenceType::Citation(data), ReferenceTarget::CitationKey(key)) => data
            .keys
            .iter()
            .any(|candidate| candidate.eq_ignore_ascii_case(key)),
        (ReferenceType::Citation(data), ReferenceTarget::AnnotationLabel(label)) => data
            .keys
            .iter()
            .any(|candidate| candidate.eq_ignore_ascii_case(label)),
        (ReferenceType::General { target: value }, ReferenceTarget::DefinitionSubject(subject)) => {
            normalize(value) == normalize(subject)
        }
        (
            ReferenceType::ToCome {
                identifier: Some(value),
            },
            ReferenceTarget::DefinitionSubject(subject),
        ) => normalize(value) == normalize(subject),
        (ReferenceType::Session { target }, ReferenceTarget::Session(identifier)) => {
            target.eq_ignore_ascii_case(identifier)
        }
        _ => false,
    }
}

fn normalize(text: &str) -> String {
    text.trim().to_ascii_lowercase()
}

fn dedup_ranges(ranges: &mut Vec<Range>) {
    ranges.sort_by_key(|range| (range.span.start, range.span.end));
    ranges.dedup_by(|a, b| a.span == b.span && a.start == b.start && a.end == b.end);
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    fn fixture() -> (Document, String) {
        let source = r#":: note ::
    Something.
::

Cache:
    Definition body.

1. Intro

    First reference [Cache].
    Second reference [Cache] and footnote [^note].
"#;
        let document = parsing::parse_document(source).expect("fixture parses");
        (document, source.to_string())
    }

    fn position_of(source: &str, needle: &str) -> Position {
        let offset = source
            .find(needle)
            .unwrap_or_else(|| panic!("needle not found: {needle}"));
        let mut line = 0;
        let mut col = 0;
        for ch in source[..offset].chars() {
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += ch.len_utf8();
            }
        }
        Position::new(line, col)
    }

    #[test]
    fn finds_references_from_usage() {
        let (document, source) = fixture();
        let position = position_of(&source, "Cache]");
        let ranges = find_references(&document, position, false);
        assert_eq!(ranges.len(), 2);
    }

    #[test]
    fn finds_references_from_definition() {
        let (document, source) = fixture();
        let position = position_of(&source, "Cache:");
        let ranges = find_references(&document, position, false);
        assert_eq!(ranges.len(), 2);
    }

    #[test]
    fn includes_declaration_when_requested() {
        let (document, source) = fixture();
        let position = position_of(&source, "Cache:");
        let ranges = find_references(&document, position, true);
        assert!(ranges.len() >= 3);
        let definition_header = document
            .root
            .children
            .iter()
            .find_map(|item| match item {
                lex_core::lex::ast::ContentItem::Definition(def) => def
                    .header_location()
                    .cloned()
                    .or_else(|| Some(def.range().clone())),
                _ => None,
            })
            .expect("definition header available");
        assert!(ranges.contains(&definition_header));
    }

    #[test]
    fn finds_annotation_references() {
        let (document, source) = fixture();
        let position = position_of(&source, "^note]");
        let ranges = find_references(&document, position, false);
        assert_eq!(ranges.len(), 1);
        assert!(ranges[0].contains(position));
    }
}
