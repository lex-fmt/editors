use lex_core::lex::ast::{Document, Range};
use lex_core::lex::inlines::ReferenceType;
use crate::inline::{extract_inline_spans, InlineSpanKind};
use crate::utils::{collect_all_annotations, for_each_text_content};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticKind {
    MissingFootnoteDefinition,
    UnusedFootnoteDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisDiagnostic {
    pub range: Range,
    pub kind: DiagnosticKind,
    pub message: String,
}

pub fn analyze(document: &Document) -> Vec<AnalysisDiagnostic> {
    let mut diagnostics = Vec::new();
    check_footnotes(document, &mut diagnostics);
    diagnostics
}

fn check_footnotes(document: &Document, diagnostics: &mut Vec<AnalysisDiagnostic>) {
    // 1. Collect all footnote references
    let mut references = Vec::new();
    for_each_text_content(document, &mut |text| {
        for span in extract_inline_spans(text) {
            if let InlineSpanKind::Reference(ReferenceType::FootnoteNumber { number }) = span.kind {
                references.push((number, span.range));
            } else if let InlineSpanKind::Reference(ReferenceType::FootnoteLabeled { label: _ }) = span.kind {
                // We handle numeric footnotes primarily as per request, but let's track labels too if needed.
                // For now, the user specifically mentioned numeric reordering and validation.
                // Let's stick to numeric for the specific "footnote" validation if the user context implies it.
                // Actually, the user said "add diagnotics for mismatched footnotes". 
                // Let's handle both if possible, but the renumbering task implies numeric.
            }
        }
    });

    // 2. Collect all footnote definitions (annotations with labels that look like numbers?)
    // Or does the user use `:: note ::`?
    // In the known `lex` format, footnotes can be annotations with specific labels or maybe list items?
    // The user example in `references.rs` tests used `:: note ::` and `[^note]`.
    // But `[1]` usually corresponds to something.
    // Let's assume for `[N]` references, we expect an annotation with label "N" or a list item starting with "N."?
    // The user said: "add completion for footnotes refernce [<integer]... then create the respective note item at the end".
    // And "re-order the number for the footnote itself".
    // This implies the definition is likely an annotation with a numeric label, e.g. `:: 1 ::`.
    
    let annotations = collect_all_annotations(document);
    let mut definitions = std::collections::HashMap::new();
    
    for annotation in &annotations {
        let label = &annotation.data.label.value;
        if let Ok(number) = label.parse::<u32>() {
             definitions.insert(number, annotation);
        }
    }

    // 3. Check for missing definitions
    for (number, range) in &references {
        if !definitions.contains_key(number) {
            diagnostics.push(AnalysisDiagnostic {
                range: range.clone(),
                kind: DiagnosticKind::MissingFootnoteDefinition,
                message: format!("Footnote [{}] is referenced but not defined", number),
            });
        }
    }

    // 4. Check for unused definitions
    // The user said "footnotes without refs are ok", but let's see. 
    // "add diagnotics for mismatched footnotes (both a ref that has no footnote content , footnotes without refs are ok...)"
    // Wait, "footnotes without refs are ok". So I should NOT warn on unused definitions.
    // "both a ref that has no footnote content , footnotes without refs are ok"
    // Rethink: "mismatched footnotes (both a ref that has no footnote content , footnotes without refs are ok)"
    // This phrasing is slightly contradictory or I'm misparsing.
    // "mismatched footnotes (both a ref that has no footnote content...)" -> Ref exists, Def missing.
    // "...footnotes without refs are ok" -> Def exists, Ref missing is OK.
    // So ONLY Ref -> Missing Def is an error.
    
    // However, usually "mismatched" implies both directions.
    // If the user explicitly said "footnotes without refs are ok", I will respect that. 
    // I will implementation missing definition check.
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    fn parse(source: &str) -> Document {
        parsing::parse_document(source).expect("parse failed")
    }

    #[test]
    fn detects_missing_footnote_definition() {
        let doc = parse("Text with [1] reference.");
        let diags = analyze(&doc);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].kind, DiagnosticKind::MissingFootnoteDefinition);
    }

    #[test]
    fn ignores_valid_footnote() {
        let doc = parse("Text [1].\n\n:: 1 ::\nNote.\n::\n");
        let diags = analyze(&doc);
        assert_eq!(diags.len(), 0);
    }
}
