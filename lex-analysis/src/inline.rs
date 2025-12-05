use lex_core::lex::ast::{Position, Range, TextContent};
use lex_core::lex::inlines::{parse_inlines, InlineNode, ReferenceType};

#[derive(Debug, Clone, PartialEq)]
pub enum InlineSpanKind {
    Strong,
    Emphasis,
    Code,
    Math,
    Reference(ReferenceType),
    StrongMarkerStart,
    StrongMarkerEnd,
    EmphasisMarkerStart,
    EmphasisMarkerEnd,
    CodeMarkerStart,
    CodeMarkerEnd,
    MathMarkerStart,
    MathMarkerEnd,
    RefMarkerStart,
    RefMarkerEnd,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineSpan {
    pub kind: InlineSpanKind,
    pub range: Range,
    pub raw: String,
}

/// Extract inline spans (formatting + references) from a text node.
pub fn extract_inline_spans(text: &TextContent) -> Vec<InlineSpan> {
    let Some(base_range) = text.location.as_ref() else {
        return Vec::new();
    };

    let content = text.as_string();
    if content.is_empty() {
        return Vec::new();
    }

    let mut spans = Vec::new();
    spans.extend(spans_from_marker(
        content,
        base_range,
        '*',
        InlineSpanKind::Strong,
        InlineSpanKind::StrongMarkerStart,
        InlineSpanKind::StrongMarkerEnd,
    ));
    spans.extend(spans_from_marker(
        content,
        base_range,
        '_',
        InlineSpanKind::Emphasis,
        InlineSpanKind::EmphasisMarkerStart,
        InlineSpanKind::EmphasisMarkerEnd,
    ));
    spans.extend(spans_from_marker(
        content,
        base_range,
        '`',
        InlineSpanKind::Code,
        InlineSpanKind::CodeMarkerStart,
        InlineSpanKind::CodeMarkerEnd,
    ));
    spans.extend(spans_from_marker(
        content,
        base_range,
        '#',
        InlineSpanKind::Math,
        InlineSpanKind::MathMarkerStart,
        InlineSpanKind::MathMarkerEnd,
    ));
    spans.extend(reference_spans(content, base_range));
    spans
}

fn spans_from_marker(
    text: &str,
    base_range: &Range,
    marker: char,
    content_kind: InlineSpanKind,
    start_marker_kind: InlineSpanKind,
    end_marker_kind: InlineSpanKind,
) -> Vec<InlineSpan> {
    let mut spans = Vec::new();
    for (start, end) in scan_symmetric_pairs(text, marker) {
        let marker_len = marker.len_utf8();
        let inner_start = start + marker_len;
        let inner_end = end.saturating_sub(marker_len);
        if inner_end <= inner_start {
            continue;
        }

        // Opening marker
        spans.push(InlineSpan {
            kind: start_marker_kind.clone(),
            range: sub_range(base_range, text, start, inner_start),
            raw: marker.to_string(),
        });

        // Content
        spans.push(InlineSpan {
            kind: content_kind.clone(),
            range: sub_range(base_range, text, inner_start, inner_end),
            raw: text[inner_start..inner_end].to_string(),
        });

        // Closing marker
        spans.push(InlineSpan {
            kind: end_marker_kind.clone(),
            range: sub_range(base_range, text, inner_end, end),
            raw: marker.to_string(),
        });
    }
    spans
}

fn reference_spans(text: &str, base_range: &Range) -> Vec<InlineSpan> {
    let mut spans = Vec::new();
    for (start, end) in scan_bracket_pairs(text) {
        let inner_start = start + '['.len_utf8();
        let inner_end = end.saturating_sub(']'.len_utf8());
        if inner_end <= inner_start {
            continue;
        }
        let raw = text[inner_start..inner_end].to_string();
        let reference_type = classify_reference(&raw);

        // Opening bracket
        spans.push(InlineSpan {
            kind: InlineSpanKind::RefMarkerStart,
            range: sub_range(base_range, text, start, inner_start),
            raw: "[".to_string(),
        });

        // Reference content
        spans.push(InlineSpan {
            kind: InlineSpanKind::Reference(reference_type),
            range: sub_range(base_range, text, inner_start, inner_end),
            raw,
        });

        // Closing bracket
        spans.push(InlineSpan {
            kind: InlineSpanKind::RefMarkerEnd,
            range: sub_range(base_range, text, inner_end, end),
            raw: "]".to_string(),
        });
    }
    spans
}

fn classify_reference(raw: &str) -> ReferenceType {
    let wrapped = format!("[{raw}]");
    for node in parse_inlines(&wrapped) {
        if let InlineNode::Reference { data, .. } = node {
            return data.reference_type;
        }
    }
    ReferenceType::NotSure
}

fn scan_symmetric_pairs(text: &str, marker: char) -> Vec<(usize, usize)> {
    let mut spans = Vec::new();
    let mut open: Option<usize> = None;
    let mut escape = false;
    for (idx, ch) in text.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' {
            escape = true;
            continue;
        }
        if ch == marker {
            if let Some(start_idx) = open {
                if idx > start_idx + marker.len_utf8() {
                    spans.push((start_idx, idx + marker.len_utf8()));
                }
                open = None;
            } else {
                open = Some(idx);
            }
        }
    }
    spans
}

fn scan_bracket_pairs(text: &str) -> Vec<(usize, usize)> {
    let mut spans = Vec::new();
    let mut open: Option<usize> = None;
    let mut escape = false;
    for (idx, ch) in text.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' {
            escape = true;
            continue;
        }
        if ch == '[' {
            if open.is_none() {
                open = Some(idx);
            }
        } else if ch == ']' {
            if let Some(start_idx) = open.take() {
                if idx > start_idx + '['.len_utf8() {
                    spans.push((start_idx, idx + ']'.len_utf8()));
                }
            }
        }
    }
    spans
}

fn sub_range(base: &Range, text: &str, start: usize, end: usize) -> Range {
    let start_pos = position_for_offset(base, text, start);
    let end_pos = position_for_offset(base, text, end);
    Range::new(
        (base.span.start + start)..(base.span.start + end),
        start_pos,
        end_pos,
    )
}

fn position_for_offset(base: &Range, text: &str, offset: usize) -> Position {
    let mut line = base.start.line;
    let mut column = base.start.column;
    for ch in text[..offset].chars() {
        if ch == '\n' {
            line += 1;
            column = 0;
        } else {
            column += ch.len_utf8();
        }
    }
    Position::new(line, column)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::ast::Range;

    fn text_with_range(content: &str, line: usize, column: usize) -> TextContent {
        let start = Position::new(line, column);
        let end = Position::new(line, column + content.len());
        let range = Range::new(0..content.len(), start, end);
        TextContent::from_string(content.to_string(), Some(range))
    }

    #[test]
    fn detects_basic_inline_spans() {
        let text = text_with_range("*bold* _em_ `code` #math#", 2, 4);
        let spans = extract_inline_spans(&text);
        // Each inline produces 3 spans: start marker, content, end marker
        assert_eq!(spans.len(), 12);
        assert!(spans
            .iter()
            .any(|span| matches!(span.kind, InlineSpanKind::Strong)));
        assert!(spans
            .iter()
            .any(|span| matches!(span.kind, InlineSpanKind::Emphasis)));
        assert!(spans
            .iter()
            .any(|span| matches!(span.kind, InlineSpanKind::Code)));
        assert!(spans
            .iter()
            .any(|span| matches!(span.kind, InlineSpanKind::Math)));
        assert!(spans
            .iter()
            .any(|span| matches!(span.kind, InlineSpanKind::StrongMarkerStart)));
        assert!(spans
            .iter()
            .any(|span| matches!(span.kind, InlineSpanKind::StrongMarkerEnd)));
    }

    #[test]
    fn detects_references_with_classification() {
        let text = text_with_range("See [^note] and [@spec2024] plus [42]", 0, 0);
        let spans = extract_inline_spans(&text);
        let kinds: Vec<_> = spans
            .iter()
            .filter_map(|span| match &span.kind {
                InlineSpanKind::Reference(reference) => Some(reference.clone()),
                _ => None,
            })
            .collect();
        assert_eq!(kinds.len(), 3);
        assert!(kinds
            .iter()
            .any(|kind| matches!(kind, ReferenceType::FootnoteLabeled { .. })));
        assert!(kinds
            .iter()
            .any(|kind| matches!(kind, ReferenceType::Citation(_))));
        assert!(kinds
            .iter()
            .any(|kind| matches!(kind, ReferenceType::FootnoteNumber { .. })));
    }
}
