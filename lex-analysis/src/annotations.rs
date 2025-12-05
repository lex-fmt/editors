//! Annotation navigation and resolution editing.
//!
//! This module provides editor-oriented utilities for working with annotations:
//!
//! - **Navigation**: Jump between annotations in document order with circular wrapping.
//!   Useful for implementing "next annotation" / "previous annotation" commands.
//!
//! - **Resolution**: Toggle the `status=resolved` parameter on annotations, enabling
//!   review workflows where annotations mark items needing attention.
//!
//! All functions are stateless and operate on the parsed document AST. They return
//! enough information for editors to apply changes (ranges, text edits) without
//! needing to understand the Lex format internals.

use crate::utils::{collect_all_annotations, find_annotation_at_position};
use lex_core::lex::ast::{Annotation, AstNode, Document, Parameter, Position, Range};

/// Direction for annotation navigation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotationDirection {
    Forward,
    Backward,
}

/// Result of navigating to an annotation.
///
/// Contains the annotation's metadata and location information so editors
/// can display context and position the cursor appropriately.
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotationNavigationResult {
    /// The annotation label (e.g., "note", "todo", "warning").
    pub label: String,
    /// Key-value parameters from the annotation header.
    pub parameters: Vec<(String, String)>,
    /// Range covering the annotation header line (for cursor positioning).
    pub header: Range,
    /// Range covering the annotation body, if present.
    pub body: Option<Range>,
}

/// A text edit that modifies an annotation's header.
///
/// Used by [`toggle_annotation_resolution`] to add or remove the `status=resolved`
/// parameter. Editors should replace the text at `range` with `new_text`.
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotationEdit {
    /// The range to replace (the annotation header line).
    pub range: Range,
    /// The new header text with updated parameters.
    pub new_text: String,
}

/// Finds the next annotation after the current position, wrapping to the first if needed.
///
/// Navigation wraps circularly: if the cursor is at or after the last annotation,
/// returns the first annotation in the document. Returns `None` only if the document
/// has no annotations.
pub fn next_annotation(
    document: &Document,
    position: Position,
) -> Option<AnnotationNavigationResult> {
    navigate(document, position, AnnotationDirection::Forward)
}

/// Finds the previous annotation before the current position, wrapping to the last if needed.
///
/// Navigation wraps circularly: if the cursor is at or before the first annotation,
/// returns the last annotation in the document. Returns `None` only if the document
/// has no annotations.
pub fn previous_annotation(
    document: &Document,
    position: Position,
) -> Option<AnnotationNavigationResult> {
    navigate(document, position, AnnotationDirection::Backward)
}

/// Navigates to an annotation in the specified direction.
///
/// This is the lower-level function used by [`next_annotation`] and [`previous_annotation`].
/// Annotations are sorted by their header position, and navigation wraps at document
/// boundaries.
pub fn navigate(
    document: &Document,
    position: Position,
    direction: AnnotationDirection,
) -> Option<AnnotationNavigationResult> {
    let mut annotations = collect_annotations(document);
    if annotations.is_empty() {
        return None;
    }
    annotations.sort_by_key(|annotation| annotation.header_location().start);

    let idx = match direction {
        AnnotationDirection::Forward => next_index(&annotations, position),
        AnnotationDirection::Backward => previous_index(&annotations, position),
    };
    annotations
        .get(idx)
        .map(|annotation| annotation_to_result(annotation))
}

/// Toggles the resolution status of the annotation at the given position.
///
/// When `resolved` is `true`, adds or updates `status=resolved` in the annotation header.
/// When `resolved` is `false`, removes the `status` parameter if present.
///
/// Returns `None` if:
/// - No annotation exists at the position
/// - The annotation already has the requested status (no change needed)
///
/// The returned [`AnnotationEdit`] contains the header range and new text, which
/// the editor should apply as a text replacement.
pub fn toggle_annotation_resolution(
    document: &Document,
    position: Position,
    resolved: bool,
) -> Option<AnnotationEdit> {
    let annotation = find_annotation_at_position(document, position)
        .or_else(|| annotation_by_line(document, position))?;
    resolution_edit(annotation, resolved)
}

fn annotation_by_line(document: &Document, position: Position) -> Option<&Annotation> {
    let line = position.line;
    collect_all_annotations(document)
        .into_iter()
        .find(|annotation| annotation.header_location().start.line == line)
}

/// Computes the edit needed to change an annotation's resolution status.
///
/// This is the lower-level function that works directly on an [`Annotation`] reference.
/// Use [`toggle_annotation_resolution`] for position-based lookup.
pub fn resolution_edit(annotation: &Annotation, resolved: bool) -> Option<AnnotationEdit> {
    let mut params = annotation.data.parameters.clone();
    let status_index = params
        .iter()
        .position(|param| param.key.eq_ignore_ascii_case("status"));

    if resolved {
        match status_index {
            Some(idx) if params[idx].value.eq_ignore_ascii_case("resolved") => return None,
            Some(idx) => params[idx].value = "resolved".to_string(),
            None => params.push(Parameter::new("status".to_string(), "resolved".to_string())),
        }
    } else if let Some(idx) = status_index {
        params.remove(idx);
    } else {
        return None;
    }

    Some(AnnotationEdit {
        range: annotation.header_location().clone(),
        new_text: format_header(&annotation.data.label.value, &params),
    })
}

fn annotation_to_result(annotation: &Annotation) -> AnnotationNavigationResult {
    AnnotationNavigationResult {
        label: annotation.data.label.value.clone(),
        parameters: annotation
            .data
            .parameters
            .iter()
            .map(|param| (param.key.clone(), param.value.clone()))
            .collect(),
        header: annotation.header_location().clone(),
        body: annotation.body_location(),
    }
}

fn next_index(entries: &[&Annotation], position: Position) -> usize {
    if let Some(current) = containing_index(entries, position) {
        if current + 1 >= entries.len() {
            0
        } else {
            current + 1
        }
    } else {
        entries
            .iter()
            .enumerate()
            .find(|(_, annotation)| annotation.header_location().start > position)
            .map(|(idx, _)| idx)
            .unwrap_or(0)
    }
}

fn previous_index(entries: &[&Annotation], position: Position) -> usize {
    if let Some(current) = containing_index(entries, position) {
        if current == 0 {
            entries.len() - 1
        } else {
            current - 1
        }
    } else {
        entries
            .iter()
            .enumerate()
            .filter(|(_, annotation)| annotation.header_location().start < position)
            .map(|(idx, _)| idx)
            .next_back()
            .unwrap_or(entries.len() - 1)
    }
}

fn containing_index(entries: &[&Annotation], position: Position) -> Option<usize> {
    entries
        .iter()
        .position(|annotation| annotation.range().contains(position))
}

pub(crate) fn collect_annotations(document: &Document) -> Vec<&Annotation> {
    collect_all_annotations(document)
}

fn format_header(label: &str, params: &[Parameter]) -> String {
    let mut header = format!(":: {label}");
    for param in params {
        header.push(' ');
        header.push_str(&param.key);
        header.push('=');
        header.push_str(&param.value);
    }
    header.push_str(" ::");
    header
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::ast::SourceLocation;
    use lex_core::lex::parsing;

    const SAMPLE: &str = r#":: note ::
    Doc note.
::

Intro:

    :: todo ::
        Body
    ::

Paragraph text.

:: info ::
    Extra details.
::
"#;

    fn parse() -> Document {
        parsing::parse_document(SAMPLE).expect("fixture parses")
    }

    fn position_of(needle: &str) -> Position {
        let offset = SAMPLE.find(needle).expect("needle present");
        SourceLocation::new(SAMPLE).byte_to_position(offset)
    }

    #[test]
    fn navigates_forward_including_wrap() {
        let document = parse();
        let start = position_of("Intro:");
        let first = next_annotation(&document, start).expect("annotation");
        assert_eq!(first.label, "todo");

        let within_second = position_of("Paragraph");
        let second = next_annotation(&document, within_second).expect("next");
        assert_eq!(second.label, "info");

        let after_last = position_of("Extra details");
        let wrap = next_annotation(&document, after_last).expect("wrap");
        assert_eq!(wrap.label, "note");
    }

    #[test]
    fn navigates_backward_including_wrap() {
        let document = parse();
        let start = position_of("Paragraph text");
        let prev = previous_annotation(&document, start).expect("previous");
        assert_eq!(prev.label, "todo");

        let wrap = previous_annotation(&document, position_of(":: note")).expect("wrap");
        assert_eq!(wrap.label, "info");
    }

    #[test]
    fn adds_status_parameter_when_resolving() {
        let source = ":: note ::\n";
        let document = parsing::parse_document(source).unwrap();
        let position = SourceLocation::new(source).byte_to_position(source.find("note").unwrap());
        let edit = toggle_annotation_resolution(&document, position, true).expect("edit");
        assert_eq!(edit.new_text, ":: note status=resolved ::");
    }

    #[test]
    fn removes_status_parameter_when_unresolving() {
        use lex_core::lex::ast::{Data, Label};
        let data = Data::new(
            Label::new("note".to_string()),
            vec![
                Parameter::new("priority".to_string(), "high".to_string()),
                Parameter::new("status".to_string(), "resolved".to_string()),
            ],
        );
        let annotation = Annotation::from_data(data, Vec::new()).at(Range::new(
            0..0,
            Position::new(0, 0),
            Position::new(0, 0),
        ));
        let edit = resolution_edit(&annotation, false).expect("edit");
        assert_eq!(edit.new_text, ":: note priority=high ::");
    }

    #[test]
    fn resolves_when_cursor_at_line_start() {
        let source = ":: note ::\n";
        let document = parsing::parse_document(source).unwrap();
        let edit =
            toggle_annotation_resolution(&document, Position::new(0, 0), true).expect("edit");
        assert_eq!(edit.new_text, ":: note status=resolved ::");
    }
}
