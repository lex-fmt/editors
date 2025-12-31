use crate::inline::InlineSpanKind;
use crate::reference_targets::targets_from_annotation;
use crate::reference_targets::{targets_from_reference_type, ReferenceTarget};
use crate::references::reference_occurrences;
use crate::utils::{
    find_annotation_at_position, find_definitions_by_subject, find_sessions_by_identifier,
    reference_span_at_position,
};
use lex_core::lex::ast::traits::AstNode;
use lex_core::lex::ast::{Document, Position, Range};

pub fn goto_definition(document: &Document, position: Position) -> Vec<Range> {
    if let Some(span) = reference_span_at_position(document, position) {
        if let InlineSpanKind::Reference(reference_type) = span.kind {
            let targets = targets_from_reference_type(&reference_type);
            return resolve_targets(document, &targets);
        }
    }

    // Reverse lookup: If we are on an annotation (footnote definition), go to references
    if let Some(annotation) = find_annotation_at_position(document, position) {
        // Ensure we are strictly on the header (label)
        let header = annotation.header_location();
        if header.contains(position) {
            // Find references to this annotation
            // Treat this as finding usages of the annotation's label
            let targets = targets_from_annotation(annotation);
            // We want to find REFERENCES that match these targets
            return reference_occurrences(document, &targets);
        }
    }

    Vec::new()
}

fn resolve_targets(document: &Document, targets: &[ReferenceTarget]) -> Vec<Range> {
    let mut ranges = Vec::new();
    for target in targets {
        match target {
            ReferenceTarget::AnnotationLabel(label) => {
                // Find matching annotations
                for annotation in document.find_annotations_by_label(label) {
                    ranges.push(annotation.header_location().clone());
                }

                // Find matching list items in Notes session
                if let Some(notes_session) = crate::utils::find_notes_session(document) {
                    for item in &notes_session.children {
                        if let lex_core::lex::ast::ContentItem::List(l) = item {
                            for entry in &l.items {
                                if let lex_core::lex::ast::ContentItem::ListItem(li) = entry {
                                    let marker = li.marker();
                                    let item_label = marker
                                        .trim()
                                        .trim_end_matches(['.', ')', ':'].as_ref())
                                        .trim();
                                    if item_label == label {
                                        ranges.push(li.range().clone());
                                    }
                                }
                            }
                        }
                    }
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
    dedup_ranges(&mut ranges);
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

fn dedup_ranges(ranges: &mut Vec<Range>) {
    ranges.sort_by_key(|range| (range.span.start, range.span.end));
    ranges.dedup_by(|a, b| a.span == b.span && a.start == b.start && a.end == b.end);
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    fn fixture() -> (Document, String) {
        let source = r#":: source ::
    Footnote text.
::

:: spec2025 ::
    Citation entry.
::

1. Intro

    Text referencing [^source], [Cache], [@spec2025], and [#1].

Cache:
    Definition body.

2. Next
    Content.
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
    fn resolves_definition_subjects() {
        let (document, source) = fixture();
        let position = position_of(&source, "Cache]");
        let locations = goto_definition(&document, position);
        assert_eq!(locations.len(), 1);
        let definition = document
            .root
            .children
            .iter()
            .find_map(|item| match item {
                lex_core::lex::ast::ContentItem::Definition(def) => Some(def),
                _ => None,
            })
            .expect("definition in fixture");
        assert_eq!(locations[0], *definition.header_location().unwrap());
    }

    #[test]
    fn resolves_annotations() {
        let (document, source) = fixture();
        let position = position_of(&source, "^source]");
        let locations = goto_definition(&document, position);
        assert_eq!(locations.len(), 1);
        assert!(document
            .find_annotations_by_label("source")
            .iter()
            .any(|ann| ann.header_location() == &locations[0]));
    }

    #[test]
    fn resolves_citations() {
        let (document, source) = fixture();
        let position = position_of(&source, "@spec2025]");
        let locations = goto_definition(&document, position);
        assert_eq!(locations.len(), 1);
        assert!(document
            .find_annotations_by_label("spec2025")
            .iter()
            .any(|ann| ann.header_location() == &locations[0]));
    }

    #[test]
    fn resolves_session_references() {
        let (document, source) = fixture();
        let position = position_of(&source, "#1]");
        let locations = goto_definition(&document, position);
        assert_eq!(locations.len(), 1);
        let expected = document
            .root
            .children
            .iter()
            .find_map(|item| {
                if let lex_core::lex::ast::ContentItem::Session(session) = item {
                    if session.title.as_string().starts_with('1') {
                        return session.header_location().cloned();
                    }
                }
                None
            })
            .expect("session header in fixture");
        assert_eq!(locations[0], expected);
    }
}
