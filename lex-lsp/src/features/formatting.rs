use lex_babel::formats::lex::formatting_rules::FormattingRules;
use lex_babel::transforms::{serialize_to_lex, serialize_to_lex_with_rules};
use lex_core::lex::ast::Document;

/// Text edit expressed as byte offsets over the original document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEditSpan {
    pub start: usize,
    pub end: usize,
    pub new_text: String,
}

/// Inclusive/exclusive line range (kept for API compatibility).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineRange {
    pub start: usize,
    pub end: usize,
}

/// Produce formatting edits for the entire document.
///
/// Returns a single TextEditSpan that replaces the entire document content
/// (full replacement strategy). This is simpler and more reliable than
/// incremental edits while the formatter and parser are maturing.
pub fn format_document(
    document: &Document,
    source: &str,
    rules: Option<FormattingRules>,
) -> Vec<TextEditSpan> {
    let formatted = match rules {
        Some(r) => serialize_to_lex_with_rules(document, r),
        None => serialize_to_lex(document),
    };
    let formatted = match formatted {
        Ok(text) => text,
        Err(_) => return Vec::new(),
    };

    // No changes needed
    if formatted == source {
        return Vec::new();
    }

    // Full document replacement: single edit from start to end
    vec![TextEditSpan {
        start: 0,
        end: source.len(),
        new_text: formatted,
    }]
}

/// Produce formatting edits for a range (currently formats entire document).
///
/// Note: Range formatting currently applies full document replacement.
/// True range-limited formatting can be added once the formatter matures.
pub fn format_range(
    document: &Document,
    source: &str,
    _range: LineRange,
    rules: Option<FormattingRules>,
) -> Vec<TextEditSpan> {
    format_document(document, source, rules)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    const FULL_FIXTURE: &str = "Section:\n\n    - item one   \n\n\n\n\n  - item two\n\n";

    fn parse(source: &str) -> Document {
        parsing::parse_document(source).expect("parse fixture")
    }

    fn apply_span(source: &str, edit: &TextEditSpan) -> String {
        let mut result = source.to_string();
        result.replace_range(edit.start..edit.end, &edit.new_text);
        result
    }

    #[test]
    fn formats_entire_document() {
        let source = FULL_FIXTURE;
        let document = parse(source);
        let formatted = serialize_to_lex(&document).unwrap();
        assert_ne!(formatted, source);

        let edits = format_document(&document, source, None);
        assert_eq!(edits.len(), 1, "should return single full-replacement edit");

        let edit = &edits[0];
        assert_eq!(edit.start, 0);
        assert_eq!(edit.end, source.len());

        let applied = apply_span(source, edit);
        assert_eq!(applied, formatted);
    }

    #[test]
    fn range_formatting_does_full_replacement() {
        // Range formatting currently does full document replacement
        let source = FULL_FIXTURE;
        let document = parse(source);
        let range = LineRange { start: 2, end: 5 }; // Range is ignored
        let edits = format_range(&document, source, range, None);

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].start, 0);
        assert_eq!(edits[0].end, source.len());
    }

    #[test]
    fn no_edits_when_already_formatted() {
        let source = "Section:\n    - item\n";
        let document = parse(source);
        let edits = format_document(&document, source, None);
        assert!(edits.is_empty());
    }

    #[test]
    fn format_with_custom_rules() {
        let source = "Section:\n    - item\n";
        let document = parse(source);
        let rules = FormattingRules {
            indent_string: "  ".to_string(), // 2-space indent
            ..Default::default()
        };

        let edits = format_document(&document, source, Some(rules));
        assert_eq!(edits.len(), 1);
        let applied = apply_span(source, &edits[0]);
        assert!(applied.contains("  - item")); // 2-space indent
    }
}
