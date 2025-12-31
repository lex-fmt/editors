# Lex Editors Development Guide

This directory contains the implementations of Lex editor clients and the shared Language Server.

## Architecture

The core logic is centralized in the Rust-based **LSP (Language Server)** to ensure consistency across all editors. Clients should be as "thin" as possible, primarily handling UI and forwarding requests to the LSP.

### Components

- **`lex-lsp`**: The Language Server. Built with Rust.
- **`lex-analysis`**: Shared semantic analysis logic (used by LSP).
- **`vscode`**: VSCode extension.
- **`lexed`**: Standalone Electron/Monaco editor.
- **`nvim`**: Neovim configuration/plugin.

## Feature Implementation Flow

1.  **Core Logic**: Implement the feature in `lex-analysis` (e.g., finding references, reordering logic).
2.  **LSP Exposure**: Expose the feature in `lex-lsp` via standard LSP capabilities (Completion, CodeAction, etc.) or custom Commands.
3.  **Client Adoption**:
    - If standard LSP (e.g., Formatting), clients often get it for free.
    - If custom Command, clients may need a shim to invoke it (e.g., binding a key to `workspace/executeCommand`).

## Feature Matrix

See [EDITORS.lex](../EDITORS.lex) in the repository root for the current support status.

## Building and Testing

### LSP
```bash
cd editors
cargo build -p lex-lsp
cargo test -p lex-lsp
```

### VSCode
See `vscode/README.md`.

### Lexed
See `lexed/README.lex`.
