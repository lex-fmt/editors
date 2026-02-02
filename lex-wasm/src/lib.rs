//! WASM bindings for Lex language analysis.
//!
//! This crate provides WebAssembly bindings for the Lex language server functionality,
//! enabling browser-based editors to use the same analysis features as the native LSP.
//!
//! # Usage
//!
//! ```javascript
//! import init, { LexDocument } from '@lex-fmt/lex-wasm';
//!
//! await init();
//!
//! const doc = new LexDocument("1. Introduction\n\nHello world");
//! const tokens = doc.semanticTokens();
//! const symbols = doc.documentSymbols();
//! ```

mod conversions;
mod document;
mod spellcheck;

pub use document::LexDocument;
pub use spellcheck::EmbeddedSpellchecker;

use wasm_bindgen::prelude::*;

/// Initialize the WASM module with panic hook for better error messages.
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

/// Get the version of lex-wasm.
#[wasm_bindgen]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}
