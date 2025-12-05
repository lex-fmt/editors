// LSP-specific features (use diff algorithm, TextEdit, etc.)
pub mod commands;
pub(crate) mod document_links;
pub mod formatting;
pub mod spellcheck;

// Re-export analysis features from lex-analysis
pub use lex_analysis::{
    document_symbols, folding_ranges, go_to_definition, hover, references, semantic_tokens,
};
