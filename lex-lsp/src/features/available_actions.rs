use lex_core::lex::ast::Document;
use std::collections::HashMap;
use tower_lsp::lsp_types::{CodeAction, CodeActionKind, CodeActionParams, TextEdit, WorkspaceEdit};

pub fn compute_actions(
    document: &Document,
    source: &str,
    params: &CodeActionParams,
) -> Vec<CodeAction> {
    let mut actions = Vec::new();

    // 1. Diagnostic-based actions
    for diagnostic in &params.context.diagnostics {
        if let Some(tower_lsp::lsp_types::NumberOrString::String(code)) = &diagnostic.code {
            if code.as_str() == "missing-footnote" {
                // QuickFix: Add footnote definition
                if let Some(label) = parse_label_from_message(&diagnostic.message) {
                    let line_count = source.lines().count().max(1) as u32;

                    let _action = CodeAction {
                        title: format!("Add definition for footnote [{}]", label),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diagnostic.clone()]),
                        edit: Some(WorkspaceEdit {
                            changes: Some(HashMap::from([(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: tower_lsp::lsp_types::Range {
                                        start: tower_lsp::lsp_types::Position {
                                            line: line_count,
                                            character: 0,
                                        },
                                        end: tower_lsp::lsp_types::Position {
                                            line: line_count,
                                            character: 0,
                                        },
                                    },
                                    new_text: format!("\n\n:: {} ::\n\n", label),
                                }],
                            )])),
                            ..Default::default()
                        }),
                        command: None,
                        is_preferred: Some(true),
                        disabled: None,
                        data: None,
                    };
                    // actions.push(_action); // Uncomment to enable
                }
            }
        }
    }

    // 2. Global actions (Refactor)
    let requested_kind = params.context.only.as_ref().and_then(|k| k.first());
    let wants_refactor = requested_kind
        .is_none_or(|k| k.as_str().starts_with("source") || k.as_str().starts_with("refactor"));

    if wants_refactor {
        // Compute reordered content
        let new_content = crate::features::footnotes::reorder_footnotes(document, source);

        if new_content != source {
            let line_count = source.lines().count().max(1) as u32;
            let last_line_idx = line_count - 1;
            let last_char = source
                .lines()
                .last()
                .map(|l| l.chars().count())
                .unwrap_or(0) as u32;

            let end_pos = tower_lsp::lsp_types::Position {
                line: last_line_idx,
                character: last_char,
            };

            actions.push(CodeAction {
                title: "Reorder footnotes".to_string(),
                kind: Some(CodeActionKind::SOURCE),
                diagnostics: None,
                edit: Some(WorkspaceEdit {
                    changes: Some(HashMap::from([(
                        params.text_document.uri.clone(),
                        vec![TextEdit {
                            range: tower_lsp::lsp_types::Range {
                                start: tower_lsp::lsp_types::Position {
                                    line: 0,
                                    character: 0,
                                },
                                end: end_pos,
                            },
                            new_text: new_content,
                        }],
                    )])),
                    ..Default::default()
                }),
                command: None,
                is_preferred: None,
                disabled: None,
                data: None,
            });
        }
    }

    actions
}

fn parse_label_from_message(msg: &str) -> Option<String> {
    let prefix = "Reference to undefined footnote: ";
    if let Some(rest) = msg.strip_prefix(prefix) {
        let trimmed = rest.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            return Some(trimmed[1..trimmed.len() - 1].to_string());
        }
        return Some(trimmed.to_string());
    }
    None
}
