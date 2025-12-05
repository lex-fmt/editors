use lex_lsp::server::{DefaultFeatureProvider, LspClient};
use lex_lsp::LexLanguageServer;
use proptest::prelude::*;
use std::sync::Arc;
use tower_lsp::lsp_types::{
    DidOpenTextDocumentParams, ExecuteCommandParams, TextDocumentItem, Url,
};
use tower_lsp::LanguageServer;

// Mock client for testing
#[derive(Clone)]
struct MockClient;

use tower_lsp::async_trait;
use tower_lsp::lsp_types::Diagnostic;

#[async_trait]
impl LspClient for MockClient {
    async fn publish_diagnostics(&self, _: Url, _: Vec<Diagnostic>, _: Option<i32>) {}
    async fn show_message(&self, _: tower_lsp::lsp_types::MessageType, _: String) {}
}

proptest! {
    // Fuzz the execute_command handler with random commands and arguments
    #[test]
    fn test_execute_command_robustness(
        command in "\\PC*",
        args_json in "\\PC*",
    ) {
        let runtime = tokio::runtime::Runtime::new().unwrap();
        runtime.block_on(async {
            let client = MockClient;
            let features = Arc::new(DefaultFeatureProvider::new());
            let server = LexLanguageServer::with_features(client, features);

            // Try to parse args as JSON, if valid, use them, otherwise use empty array
            let arguments = serde_json::from_str(&args_json).unwrap_or_else(|_| vec![]);

            let params = ExecuteCommandParams {
                command,
                arguments,
                work_done_progress_params: Default::default(),
            };

            // Should not panic
            let _ = server.execute_command(params).await;
        });
    }

    // Fuzz the document parser via did_open
    #[test]
    fn test_document_parsing_robustness(
        text in "\\PC*",
    ) {
        let runtime = tokio::runtime::Runtime::new().unwrap();
        runtime.block_on(async {
            let client = MockClient;
            let features = Arc::new(DefaultFeatureProvider::new());
            let server = LexLanguageServer::with_features(client, features);
            let uri = Url::parse("file:///test.lex").unwrap();

            let params = DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "lex".to_string(),
                    version: 1,
                    text: text.clone(),
                },
            };

            // Should not panic
            server.did_open(params).await;

            // Try to access features on the potentially malformed document
            let _ = server.document_symbol(tower_lsp::lsp_types::DocumentSymbolParams {
                text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            }).await;
        });
    }
}
