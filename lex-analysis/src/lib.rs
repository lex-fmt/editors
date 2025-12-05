//! Document analysis and navigation for Lex
//!
//! This crate provides semantic analysis capabilities for Lex documents,
//! enabling features like reference resolution, symbol extraction, token
//! classification, and document navigation.
//!
//! # Architecture
//!
//! The crate is organized into several modules:
//!
//! - `utils`: Core document traversal and lookup utilities
//! - `references`: Reference resolution and target conversion
//! - `inline`: Inline span detection (bold, italic, code, references)
//! - `tokens`: Semantic token extraction and classification
//! - `symbols`: Document structure and symbol hierarchy
//! - `hover`: Preview text extraction for hover tooltips
//! - `folding`: Foldable range detection
//! - `navigation`: Go-to-definition and find-references
//!
//! # Design Principles
//!
//! - **Stateless**: All functions operate on immutable AST references
//! - **Reusable**: Not tied to LSP protocol - usable by CLI, editor plugins, etc.
//! - **Well-tested**: Comprehensive unit tests using official sample fixtures
//! - **AST-focused**: Works directly with lex-parser AST types
//!
//! # Usage
//!
//! ```rust,ignore
//! use lex_analysis::{tokens, symbols, navigation};
//! use lex_core::parse;
//!
//! let document = parse("1. Introduction\n\nHello world")?;
//!
//! // Extract tokens for syntax highlighting
//! let tokens = tokens::extract_semantic_tokens(&document);
//!
//! // Build document outline
//! let symbols = symbols::extract_document_symbols(&document);
//!
//! // Resolve references
//! let defs = navigation::find_definition(&document, position);
//! ```

// Core utilities
pub mod inline;
pub mod reference_targets;
pub mod utils;

// Analysis features
pub mod annotations;
pub mod completion;
pub mod document_symbols;
pub mod folding_ranges;
pub mod go_to_definition;
pub mod hover;
pub mod references;
pub mod semantic_tokens;

// Test support (available in tests and as dev-dependency)
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;
