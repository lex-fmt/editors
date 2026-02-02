//! Type conversions between LSP types and JS-friendly formats.
//!
//! This module converts from lsp-types to JavaScript-compatible formats
//! using serde-wasm-bindgen for efficient serialization.

use lsp_types::{
    CompletionItem, Diagnostic, DocumentSymbol, FoldingRange, Hover, Location, SemanticToken,
};
use serde::Serialize;
use wasm_bindgen::JsValue;

/// Convert any serializable value to a JsValue.
pub fn to_js<T: Serialize + ?Sized>(value: &T) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(value).map_err(|e| JsValue::from_str(&e.to_string()))
}

/// Convert a vector of diagnostics to JsValue.
pub fn diagnostics_to_js(diagnostics: &[Diagnostic]) -> Result<JsValue, JsValue> {
    to_js(diagnostics)
}

/// Convert a vector of document symbols to JsValue.
pub fn document_symbols_to_js(symbols: &[DocumentSymbol]) -> Result<JsValue, JsValue> {
    to_js(symbols)
}

/// Convert hover to JsValue.
pub fn hover_to_js(hover: &Option<Hover>) -> Result<JsValue, JsValue> {
    to_js(hover)
}

/// Convert locations to JsValue.
pub fn locations_to_js(locations: &[Location]) -> Result<JsValue, JsValue> {
    to_js(locations)
}

/// Convert folding ranges to JsValue.
pub fn folding_ranges_to_js(ranges: &[FoldingRange]) -> Result<JsValue, JsValue> {
    to_js(ranges)
}

/// Convert completion items to JsValue.
pub fn completion_items_to_js(items: &[CompletionItem]) -> Result<JsValue, JsValue> {
    to_js(items)
}

/// Semantic token data for Monaco.
///
/// Monaco expects semantic tokens as a Uint32Array with the following format:
/// [deltaLine, deltaStartChar, length, tokenType, tokenModifiers, ...]
#[derive(Serialize)]
pub struct SemanticTokensData {
    /// Raw token data as a flat array.
    pub data: Vec<u32>,
}

impl SemanticTokensData {
    /// Create from a list of semantic tokens.
    pub fn from_tokens(tokens: &[SemanticToken]) -> Self {
        let data: Vec<u32> = tokens
            .iter()
            .flat_map(|t| {
                [
                    t.delta_line,
                    t.delta_start,
                    t.length,
                    t.token_type,
                    t.token_modifiers_bitset,
                ]
            })
            .collect();
        SemanticTokensData { data }
    }
}

/// Convert semantic tokens to JsValue (Uint32Array-compatible).
pub fn semantic_tokens_to_js(tokens: &SemanticTokensData) -> Result<JsValue, JsValue> {
    to_js(&tokens.data)
}
