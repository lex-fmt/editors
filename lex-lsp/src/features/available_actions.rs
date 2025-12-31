use lex_core::lex::ast::Document;
use tower_lsp::lsp_types::{CodeAction, CodeActionKind, CodeActionParams, TextEdit, WorkspaceEdit};
use std::collections::HashMap;
use crate::features::commands;

pub fn compute_actions(
    _document: &Document,
    params: &CodeActionParams,
) -> Vec<CodeAction> {
    let mut actions = Vec::new();

    // 1. Diagnostic-based actions
    for diagnostic in &params.context.diagnostics {
        if let Some(tower_lsp::lsp_types::NumberOrString::String(code)) = &diagnostic.code {
            match code.as_str() {
                "missing-footnote" => {
                    // QuickFix: Add footnote definition
                     if let Some(label) = parse_label_from_message(&diagnostic.message) {
                        let _action = CodeAction {
                            title: format!("Add definition for footnote [{}]", label),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diagnostic.clone()]),
                            edit: Some(WorkspaceEdit {
                                changes: Some(HashMap::from([(
                                    params.text_document.uri.clone(),
                                    vec![TextEdit {
                                        // Insert at end of file (placeholder logic)
                                        // We need the end position of the document.
                                        // But we lack easy access here without source text length.
                                        // We'll use a specific position if possible, or omit edit for now.
                                        // Since we can't implement it perfectly without text, we'll implement it as a Command instead?
                                        // "Insert missing footnote" command.
                                        // Then `server.rs` executes it.
                                        range: diagnostic.range, 
                                        new_text: format!("\n\n:: {} ::\n\n::", label),
                                    }],
                                )])),
                                ..Default::default()
                            }),
                            command: None,
                            is_preferred: Some(true),
                            disabled: None,
                            data: None,
                        };
                        // Only add if we can compute valid edit (omitted for now due to complexity without text)
                        // actions.push(action); 
                     }
                }
                _ => {}
            }
        }
    }

    // 2. Global actions (Refactor)
    let requested_kind = params.context.only.as_ref().and_then(|k| k.first());
    let wants_refactor = requested_kind.map_or(true, |k| k.as_str().starts_with("source") || k.as_str().starts_with("refactor"));

    if wants_refactor {
        actions.push(CodeAction {
            title: "Reorder footnotes".to_string(),
            kind: Some(CodeActionKind::SOURCE),
            diagnostics: None,
            edit: None,
            command: Some(tower_lsp::lsp_types::Command {
                title: "Reorder footnotes".to_string(),
                command: commands::COMMAND_FOOTNOTES_REORDER.to_string(),
                arguments: None, // Arguments must be supplied by client or handled by server specifically
            }),
            is_preferred: None,
            disabled: None,
            data: None,
        });
    }

    actions
}

fn parse_label_from_message(msg: &str) -> Option<String> {
    let prefix = "Reference to undefined footnote: ";
    if let Some(rest) = msg.strip_prefix(prefix) {
        let trimmed = rest.trim();
         if trimmed.starts_with('[') && trimmed.ends_with(']') {
            return Some(trimmed[1..trimmed.len()-1].to_string());
        }
        return Some(trimmed.to_string());
    }
    None
}
