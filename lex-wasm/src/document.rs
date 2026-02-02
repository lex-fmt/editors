//! WASM-bindgen wrapper for Lex documents.
//!
//! This module provides the main `LexDocument` type that wraps parsed Lex documents
//! and exposes analysis functionality to JavaScript.

use crate::conversions::{
    completion_items_to_js, diagnostics_to_js, document_symbols_to_js, folding_ranges_to_js,
    hover_to_js, locations_to_js, semantic_tokens_to_js, SemanticTokensData,
};
use crate::spellcheck::EmbeddedSpellchecker;
use lex_analysis::{
    completion, diagnostics, document_symbols, folding_ranges, go_to_definition, hover, references,
    semantic_tokens, spellcheck,
};
use lex_babel::format::Format;
use lex_babel::formats::lex::LexFormat;
use lex_babel::formats::HtmlFormat;
use lex_core::lex::ast::{Document, Position};
use lex_core::lex::transforms::standard::STRING_TO_AST;
use lsp_types::{
    CompletionItem, Diagnostic, DocumentSymbol, FoldingRange, FoldingRangeKind, Hover,
    HoverContents, Location, MarkupContent, MarkupKind, Range, SemanticToken, Url,
};
use wasm_bindgen::prelude::*;

/// A parsed Lex document with analysis capabilities.
#[wasm_bindgen]
pub struct LexDocument {
    document: Document,
    source: String,
    uri: String,
}

#[wasm_bindgen]
impl LexDocument {
    /// Parse source text into a LexDocument.
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str) -> Result<LexDocument, JsError> {
        Self::with_uri(source, "file:///document.lex")
    }

    /// Parse source text with a specific URI.
    #[wasm_bindgen(js_name = withUri)]
    pub fn with_uri(source: &str, uri: &str) -> Result<LexDocument, JsError> {
        let document = STRING_TO_AST
            .run(source.to_string())
            .map_err(|e| JsError::new(&format!("Parse error: {e}")))?;
        Ok(LexDocument {
            document,
            source: source.to_string(),
            uri: uri.to_string(),
        })
    }

    /// Get the document's source text.
    pub fn source(&self) -> String {
        self.source.clone()
    }

    /// Get the document's URI.
    pub fn uri(&self) -> String {
        self.uri.clone()
    }

    /// Get semantic tokens for syntax highlighting.
    ///
    /// Returns an array of token data in the format expected by Monaco:
    /// [deltaLine, deltaStartChar, length, tokenType, tokenModifiers, ...]
    #[wasm_bindgen(js_name = semanticTokens)]
    pub fn semantic_tokens(&self) -> Result<JsValue, JsValue> {
        let lex_tokens = semantic_tokens::collect_semantic_tokens(&self.document);

        // Convert to LSP semantic tokens with delta encoding
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;
        let tokens: Vec<SemanticToken> = lex_tokens
            .iter()
            .map(|t| {
                let line = t.range.start.line as u32;
                let start = t.range.start.column as u32;

                let delta_line = line - prev_line;
                let delta_start = if delta_line == 0 {
                    start - prev_start
                } else {
                    start
                };

                prev_line = line;
                prev_start = start;

                // Find the index of this token kind in SEMANTIC_TOKEN_KINDS
                let token_type = semantic_tokens::SEMANTIC_TOKEN_KINDS
                    .iter()
                    .position(|k| *k == t.kind)
                    .unwrap_or(0) as u32;

                SemanticToken {
                    delta_line,
                    delta_start,
                    length: (t.range.end.column - t.range.start.column) as u32,
                    token_type,
                    token_modifiers_bitset: 0,
                }
            })
            .collect();

        let data = SemanticTokensData::from_tokens(&tokens);
        semantic_tokens_to_js(&data)
    }

    /// Get the semantic token legend (token types and modifiers).
    #[wasm_bindgen(js_name = semanticTokenLegend)]
    pub fn semantic_token_legend() -> JsValue {
        let token_types: Vec<&str> = semantic_tokens::SEMANTIC_TOKEN_KINDS
            .iter()
            .map(|k| k.as_str())
            .collect();
        let legend = serde_json::json!({
            "tokenTypes": token_types,
            "tokenModifiers": []
        });
        serde_wasm_bindgen::to_value(&legend).unwrap_or(JsValue::NULL)
    }

    /// Get document symbols (outline).
    #[wasm_bindgen(js_name = documentSymbols)]
    pub fn document_symbols(&self) -> Result<JsValue, JsValue> {
        let lex_symbols = document_symbols::collect_document_symbols(&self.document);

        // Convert to LSP document symbols
        let symbols: Vec<DocumentSymbol> = lex_symbols
            .into_iter()
            .map(convert_document_symbol)
            .collect();

        document_symbols_to_js(&symbols)
    }

    /// Get hover information at a position.
    pub fn hover(&self, line: u32, character: u32) -> Result<JsValue, JsValue> {
        let position = Position::new(line as usize, character as usize);
        let result = hover::hover(&self.document, position);

        let lsp_hover = result.map(|h| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: h.contents,
            }),
            range: Some(Range {
                start: lsp_types::Position {
                    line: h.range.start.line as u32,
                    character: h.range.start.column as u32,
                },
                end: lsp_types::Position {
                    line: h.range.end.line as u32,
                    character: h.range.end.column as u32,
                },
            }),
        });

        hover_to_js(&lsp_hover)
    }

    /// Get go-to-definition locations.
    #[wasm_bindgen(js_name = gotoDefinition)]
    pub fn goto_definition(&self, line: u32, character: u32) -> Result<JsValue, JsValue> {
        let position = Position::new(line as usize, character as usize);
        let ranges = go_to_definition::goto_definition(&self.document, position);

        let uri =
            Url::parse(&self.uri).unwrap_or_else(|_| Url::parse("file:///document.lex").unwrap());
        let locations: Vec<Location> = ranges
            .into_iter()
            .map(|r| Location {
                uri: uri.clone(),
                range: Range {
                    start: lsp_types::Position {
                        line: r.start.line as u32,
                        character: r.start.column as u32,
                    },
                    end: lsp_types::Position {
                        line: r.end.line as u32,
                        character: r.end.column as u32,
                    },
                },
            })
            .collect();

        locations_to_js(&locations)
    }

    /// Find all references to the symbol at a position.
    pub fn references(
        &self,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) -> Result<JsValue, JsValue> {
        let position = Position::new(line as usize, character as usize);
        let ranges = references::find_references(&self.document, position, include_declaration);

        let uri =
            Url::parse(&self.uri).unwrap_or_else(|_| Url::parse("file:///document.lex").unwrap());
        let locations: Vec<Location> = ranges
            .into_iter()
            .map(|r| Location {
                uri: uri.clone(),
                range: Range {
                    start: lsp_types::Position {
                        line: r.start.line as u32,
                        character: r.start.column as u32,
                    },
                    end: lsp_types::Position {
                        line: r.end.line as u32,
                        character: r.end.column as u32,
                    },
                },
            })
            .collect();

        locations_to_js(&locations)
    }

    /// Get folding ranges for code folding.
    #[wasm_bindgen(js_name = foldingRanges)]
    pub fn folding_ranges(&self) -> Result<JsValue, JsValue> {
        let lex_ranges = folding_ranges::folding_ranges(&self.document);

        let ranges: Vec<FoldingRange> = lex_ranges
            .into_iter()
            .map(|r| FoldingRange {
                start_line: r.start_line,
                start_character: None,
                end_line: r.end_line,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            })
            .collect();

        folding_ranges_to_js(&ranges)
    }

    /// Get completion suggestions at a position.
    pub fn completion(&self, line: u32, character: u32) -> Result<JsValue, JsValue> {
        let position = Position::new(line as usize, character as usize);

        // Get the current line text for context
        let current_line = self.source.lines().nth(line as usize);

        // No workspace files in WASM - document-local completion only
        let candidates = completion::completion_items(
            &self.document,
            position,
            current_line,
            None, // No workspace
            None, // No trigger character
        );

        let items: Vec<CompletionItem> = candidates
            .into_iter()
            .map(|c| CompletionItem {
                label: c.label,
                kind: Some(c.kind),
                detail: c.detail,
                insert_text: c.insert_text,
                ..Default::default()
            })
            .collect();

        completion_items_to_js(&items)
    }

    /// Get diagnostics (errors, warnings) for the document.
    pub fn diagnostics(&self) -> Result<JsValue, JsValue> {
        let lex_diagnostics = diagnostics::analyze(&self.document);

        let diags: Vec<Diagnostic> = lex_diagnostics
            .into_iter()
            .map(|d| Diagnostic {
                range: Range {
                    start: lsp_types::Position {
                        line: d.range.start.line as u32,
                        character: d.range.start.column as u32,
                    },
                    end: lsp_types::Position {
                        line: d.range.end.line as u32,
                        character: d.range.end.column as u32,
                    },
                },
                severity: Some(lsp_types::DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: Some("lex".to_string()),
                message: d.message,
                related_information: None,
                tags: None,
                data: None,
            })
            .collect();

        diagnostics_to_js(&diags)
    }

    /// Get spellcheck diagnostics using the embedded dictionary.
    #[wasm_bindgen(js_name = spellcheckDiagnostics)]
    pub fn spellcheck_diagnostics(
        &self,
        checker: &EmbeddedSpellchecker,
    ) -> Result<JsValue, JsValue> {
        let result = spellcheck::check_document(&self.document, checker);
        diagnostics_to_js(&result.diagnostics)
    }

    /// Format the document source.
    pub fn format(&self) -> Result<String, JsError> {
        let format = LexFormat::default();
        format
            .serialize(&self.document)
            .map_err(|e| JsError::new(&format!("Format error: {e}")))
    }

    /// Export the document as HTML.
    #[wasm_bindgen(js_name = toHtml)]
    pub fn to_html(&self) -> Result<String, JsError> {
        let format = HtmlFormat::default();
        format
            .serialize(&self.document)
            .map_err(|e| JsError::new(&format!("HTML export error: {e}")))
    }
}

/// Convert lex-analysis document symbol to LSP document symbol.
fn convert_document_symbol(sym: document_symbols::LexDocumentSymbol) -> DocumentSymbol {
    #[allow(deprecated)]
    DocumentSymbol {
        name: sym.name,
        detail: sym.detail,
        kind: sym.kind,
        tags: None,
        deprecated: None,
        range: Range {
            start: lsp_types::Position {
                line: sym.range.start.line as u32,
                character: sym.range.start.column as u32,
            },
            end: lsp_types::Position {
                line: sym.range.end.line as u32,
                character: sym.range.end.column as u32,
            },
        },
        selection_range: Range {
            start: lsp_types::Position {
                line: sym.selection_range.start.line as u32,
                character: sym.selection_range.start.column as u32,
            },
            end: lsp_types::Position {
                line: sym.selection_range.end.line as u32,
                character: sym.selection_range.end.column as u32,
            },
        },
        children: if sym.children.is_empty() {
            None
        } else {
            Some(
                sym.children
                    .into_iter()
                    .map(convert_document_symbol)
                    .collect(),
            )
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_document() {
        let doc = LexDocument::new("1. Introduction\n\nHello world").unwrap();
        assert!(!doc.source().is_empty());
    }

    #[test]
    fn test_format_document() {
        let doc = LexDocument::new("1. Introduction\n\nHello world").unwrap();
        let formatted = doc.format().unwrap();
        assert!(formatted.contains("Introduction"));
    }
}
