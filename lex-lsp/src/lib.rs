//! Language Server Protocol (LSP) implementation for Lex
//!
//!     This crate provides language server capabilities for the Lex format, enabling rich editor
//!     support in any LSP-compatible editor (VSCode, Neovim, Emacs, Sublime, etc.).
//!
//! Design Decision: tower-lsp
//!
//!     After evaluating the Rust LSP ecosystem, we chose tower-lsp as our framework:
//!
//!     Considered Options:
//!         1. tower-lsp: High-level async framework built on Tower (~204K monthly downloads)
//!         2. lsp-server: Low-level sync library from rust-analyzer (~309K monthly downloads)
//!         3. async-lsp: Low-level async with full Tower integration (~135 GitHub stars)
//!
//!     Why tower-lsp:
//!         - Best balance of ease-of-use and functionality for a new LSP project
//!         - Strong ecosystem support with extensive documentation and examples
//!         - Modern async/await patterns ideal for Lex's structured parsing needs
//!         - Built-in LSP 3.18 support with proposed features
//!         - Active community with production usage in many language servers
//!         - Good integration with Rust async ecosystem (tokio, futures)
//!
//!     Trade-offs:
//!         - Less flexible than async-lsp for custom Tower layers (acceptable for our needs)
//!         - Requires &self for trait methods, forcing Arc<Mutex<>> for mutable state (standard pattern)
//!         - Notification ordering is async (not an issue for initial feature set)
//!
//!     Future Migration Path:
//!         If we later need precise notification ordering or custom middleware, we can migrate
//!         to async-lsp with minimal disruption as both use similar async patterns.
//!
//! Feature Set
//!
//!     Lex is a structured document format, not a programming language. LSP features are selected
//!     to optimize document authoring and navigation workflows:
//!
//!     Core Features:
//!
//!         a. Syntax Highlighting

//!             1. Semantic Tokens (textDocument/semanticTokens/*):
//!                 - Syntax highlighting for sessions, lists, definitions, annotations
//!                 - Inline formatting: bold, italic, code, math
//!                 - References, footnotes, citations
//!                 - Verbatim blocks with language-specific highlighting
//!             2. Document Symbols (textDocument/documentSymbol):
//!                 - Hierarchical outline view of document structure
//!                 - Sessions with nesting (1., 1.1., 1.1.1., etc.)
//!                 - Definitions, annotations, lists as navigable symbols
//!            3. Hover Information (textDocument/hover):
//!                 - Preview footnote/citation content on hover
//!                 - Show annotation metadata
//!                 - Preview definition content when hovering over reference
//!             4. Folding Ranges (textDocument/foldingRange):
//!                 - Fold sessions and nested content
//!                 - Fold list items with children
//!                 - Fold annotations, definitions, verbatim blocks
//!
//!         b. Navigation
//!
//!             5. Go to Definition / Find References (textDocument/definition, textDocument/references):
//!                 - Find all references to footnotes/citations
//!                 - Jump from footnote reference [42] to annotation
//!                 - Jump from citation [@spec2025] to bibliography entry
//!                 - Jump from internal reference [TK-rootlist] to target
//!             6. Document Links (textDocument/documentLink):
//!                 - Clickable links in text
//!                 - Verbatim block src parameters (images, includes)
//!                 - External references
//!
//!         c. Editing
//!
//!             7. Document Formatting (textDocument/formatting, textDocument/rangeFormatting):
//!                 - Fix indentation issues
//!                 - Normalize blank lines
//!                 - Align list markers //!         
//!
//!
//!
//! Architecture
//!
//!     The server follows a layered architecture:
//!
//!     LSP Layer (tower-lsp):
//!         - Handles JSON-RPC communication
//!         - Protocol handshaking and capability negotiation
//!         - Request/response routing
//!
//!     Server Layer (this crate):
//!         - Implements LanguageServer trait
//!         - Manages document state and parsing
//!         - Coordinates feature implementations
//!         - Very thing, mostly calls the the feature layers over lex-parser
//!         - Thin tests just asserting the right things are being called and returned
//!
//!     Feature Layer:
//!         - Each feature operates on Lex AST
//!         - Stateless transformations where possible
//!         - All logic and dense unit tests
//!
//!
//! Testing Strategy
//!
//!     Following Lex project conventions:
//!         - Use official sample files from specs/ for all tests
//!         - Use lexplore loader for consistent test data
//!         - Use ast_assertions library for AST validation
//!         - Test each feature in isolation and integration
//!         - Test against kitchensink and trifecta fixtures
//!
//! Non-Features
//!
//!     The following LSP features are intentionally excluded as they don't apply to document formats:
//!         - Code Lens: Not applicable to documents
//!         - Type Hierarchy: No type system
//!         - Implementation: No interfaces/implementations
//!         - Moniker: For cross-repo linking, not needed
//!         - Linked Editing Range: For paired tags (HTML/XML)
//!         - Diagnostics: we don't have a clear vision for how that would work.
//!
//! Error Handling and Robustness
//!
//!     The server is designed to be highly robust and crash-resistant, following these principles:
//!
//!     1. No Panics:
//!         - We strictly avoid `unwrap()` and `expect()` in production code paths.
//!         - All potential failure points (parsing, serialization, IO) return `Result`.
//!         - Errors are propagated up the stack and handled gracefully.
//!
//!     2. Graceful Degradation:
//!         - If a feature fails (e.g., semantic tokens calculation), we log the error and return
//!           an empty result or `None` rather than crashing the server.
//!         - This ensures that a bug in one feature doesn't bring down the entire editor experience.
//!
//!     3. Error Propagation:
//!         - The `lex-parser` crate returns `Result` types for all parsing operations.
//!         - The `lex-lsp` server maps these internal errors to appropriate LSP error codes
//!           (e.g., `InternalError`, `InvalidRequest`) when communicating with the client.
//!
//!     4. Property-Based Testing:
//!         - We use `proptest` to fuzz the server with random inputs (commands, document text)
//!           to uncover edge cases and ensure stability under unexpected conditions.
//!
//! Usage
//!
//!     This crate provides both a library and binary:
//!
//!     Library:
//!         ```rust
//!         use lex_lsp::LexLanguageServer;
//!         use tower_lsp::Server;
//!
//!         #[tokio::main]
//!         async fn main() {
//!             let stdin = tokio::io::stdin();
//!             let stdout = tokio::io::stdout();
//!
//!             let (service, socket) = LspService::new(|client| LexLanguageServer::new(client));
//!             Server::new(stdin, stdout, socket).serve(service).await;
//!         }
//!         ```
//!
//!     Binary:
//!         $ lex-lsp
//!         Starts the language server on stdin/stdout for editor integration.
//!

pub mod features;
pub mod server;

pub use server::LexLanguageServer;
