# Lex Editors Development Guide

This directory contains the implementations of Lex editor clients and the shared Language Server.

## Architecture

The core logic is centralized in the Rust-based **LSP (Language Server)** to ensure consistency across all editors. Clients should be as "thin" as possible, primarily handling UI and forwarding requests to the LSP.

### Components

- **`lex-lsp`**: The Language Server. Built with Rust.
- **`lex-analysis`**: Shared semantic analysis logic (used by LSP).
- **`vscode`**: VSCode extension.
- **`lexed`**: Multi-platform editor with shared React/Monaco UI. Runs on Electron desktop (with native LSP) and browser (with WASM LSP). Uses platform abstraction layer.
- **`nvim`**: Neovim configuration/plugin.

## Multi-Platform Editor (Lexed)

The Lexed editor is designed to run on both Electron desktop and web browser platforms with shared UI code. This is achieved through a `PlatformAdapter` abstraction:

- **Electron**: Uses IPC for file system, native dialogs, and communicates with spawned `lex-lsp` process
- **Web**: Uses browser APIs (File System Access, localStorage) and WASM-compiled `lex-wasm` module

Components use `usePlatform()` hook to access platform capabilities, enabling the same React/Monaco code to work on both platforms. See `lexed/shared/src/platform.ts` for the interface definition.

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
