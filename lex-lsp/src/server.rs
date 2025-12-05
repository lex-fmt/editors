//! Main language server implementation

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::features::commands::{self, execute_command};
use crate::features::document_links::collect_document_links;
use crate::features::document_symbols::{collect_document_symbols, LexDocumentSymbol};
use crate::features::folding_ranges::{folding_ranges as collect_folding_ranges, LexFoldingRange};
use crate::features::formatting::{self, LineRange as FormattingLineRange, TextEditSpan};
use crate::features::go_to_definition::goto_definition;
use crate::features::hover::{hover as compute_hover, HoverResult};
use crate::features::references::find_references;
use crate::features::semantic_tokens::{
    collect_semantic_tokens, LexSemanticToken, SEMANTIC_TOKEN_KINDS,
};
use crate::features::spellcheck::SpellcheckResult;
use lex_analysis::completion::{completion_items, CompletionCandidate, CompletionWorkspace};
use lex_babel::formats::lex::formatting_rules::FormattingRules;
use lex_babel::templates::{
    build_asset_snippet, build_verbatim_snippet, AssetSnippetRequest, VerbatimSnippetRequest,
};
use lex_core::lex::ast::links::{DocumentLink as AstDocumentLink, LinkType};
use lex_core::lex::ast::range::SourceLocation;
use lex_core::lex::ast::{Document, Position as AstPosition, Range as AstRange};
use lex_core::lex::parsing;
use serde_json::{json, Value};
use tokio::sync::RwLock;
use tower_lsp::async_trait;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    CodeAction, CodeActionOrCommand, CodeActionParams, CodeActionProviderCapability,
    CodeActionResponse, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeConfigurationParams, DocumentFormattingParams, DocumentLink, DocumentLinkOptions,
    DocumentLinkParams, DocumentRangeFormattingParams, DocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, ExecuteCommandOptions, ExecuteCommandParams, FoldingRange,
    FoldingRangeParams, FoldingRangeProviderCapability, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, Location, MarkupContent, MarkupKind,
    OneOf, Position, Range, ReferenceParams, SemanticToken, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, ServerCapabilities, ServerInfo, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url, WorkDoneProgressOptions,
};
use tower_lsp::Client;

use tower_lsp::lsp_types::Diagnostic;

use tower_lsp::lsp_types::MessageType;

#[async_trait]
pub trait LspClient: Send + Sync + Clone + 'static {
    async fn publish_diagnostics(&self, uri: Url, diags: Vec<Diagnostic>, version: Option<i32>);
    async fn show_message(&self, typ: MessageType, message: String);
}

#[async_trait]
impl LspClient for Client {
    async fn publish_diagnostics(&self, uri: Url, diags: Vec<Diagnostic>, version: Option<i32>) {
        self.publish_diagnostics(uri, diags, version).await;
    }

    async fn show_message(&self, typ: MessageType, message: String) {
        self.show_message(typ, message).await;
    }
}

pub trait FeatureProvider: Send + Sync + 'static {
    fn semantic_tokens(&self, document: &Document) -> Vec<LexSemanticToken>;
    fn document_symbols(&self, document: &Document) -> Vec<LexDocumentSymbol>;
    fn folding_ranges(&self, document: &Document) -> Vec<LexFoldingRange>;
    fn hover(&self, document: &Document, position: AstPosition) -> Option<HoverResult>;
    fn goto_definition(&self, document: &Document, position: AstPosition) -> Vec<AstRange>;
    fn references(
        &self,
        document: &Document,
        position: AstPosition,
        include_declaration: bool,
    ) -> Vec<AstRange>;
    fn document_links(&self, document: &Document) -> Vec<AstDocumentLink>;
    fn format_document(
        &self,
        document: &Document,
        source: &str,
        rules: Option<FormattingRules>,
    ) -> Vec<TextEditSpan>;
    fn format_range(
        &self,
        document: &Document,
        source: &str,
        range: FormattingLineRange,
        rules: Option<FormattingRules>,
    ) -> Vec<TextEditSpan>;
    fn completion(
        &self,
        document: &Document,
        position: AstPosition,
        workspace: Option<&CompletionWorkspace>,
        trigger_char: Option<&str>,
    ) -> Vec<CompletionCandidate>;
    fn execute_command(&self, command: &str, arguments: &[Value]) -> Result<Option<Value>>;
    fn check_spelling(&self, document: &Document, language: &str) -> SpellcheckResult;
}

#[derive(Default)]
pub struct DefaultFeatureProvider;

impl DefaultFeatureProvider {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl FeatureProvider for DefaultFeatureProvider {
    fn semantic_tokens(&self, document: &Document) -> Vec<LexSemanticToken> {
        collect_semantic_tokens(document)
    }

    fn document_symbols(&self, document: &Document) -> Vec<LexDocumentSymbol> {
        collect_document_symbols(document)
    }

    fn folding_ranges(&self, document: &Document) -> Vec<LexFoldingRange> {
        collect_folding_ranges(document)
    }

    fn hover(&self, document: &Document, position: AstPosition) -> Option<HoverResult> {
        compute_hover(document, position)
    }

    fn goto_definition(&self, document: &Document, position: AstPosition) -> Vec<AstRange> {
        goto_definition(document, position)
    }

    fn references(
        &self,
        document: &Document,
        position: AstPosition,
        include_declaration: bool,
    ) -> Vec<AstRange> {
        find_references(document, position, include_declaration)
    }

    fn document_links(&self, document: &Document) -> Vec<AstDocumentLink> {
        collect_document_links(document)
    }

    fn format_document(
        &self,
        document: &Document,
        source: &str,
        rules: Option<FormattingRules>,
    ) -> Vec<TextEditSpan> {
        formatting::format_document(document, source, rules)
    }

    fn format_range(
        &self,
        document: &Document,
        source: &str,
        range: FormattingLineRange,
        rules: Option<FormattingRules>,
    ) -> Vec<TextEditSpan> {
        formatting::format_range(document, source, range, rules)
    }

    fn completion(
        &self,
        document: &Document,
        position: AstPosition,
        workspace: Option<&CompletionWorkspace>,
        trigger_char: Option<&str>,
    ) -> Vec<CompletionCandidate> {
        completion_items(document, position, workspace, trigger_char)
    }

    fn execute_command(&self, command: &str, arguments: &[Value]) -> Result<Option<Value>> {
        execute_command(command, arguments)
    }

    fn check_spelling(&self, document: &Document, language: &str) -> SpellcheckResult {
        crate::features::spellcheck::check_document(document, language)
    }
}

#[derive(Clone)]
struct DocumentEntry {
    document: Arc<Document>,
    text: Arc<String>,
}

#[derive(Default)]
struct DocumentStore {
    entries: RwLock<HashMap<Url, Option<DocumentEntry>>>,
}

impl DocumentStore {
    async fn upsert(&self, uri: Url, text: String) -> Option<DocumentEntry> {
        let parsed = match parsing::parse_document(&text) {
            Ok(document) => Some(DocumentEntry {
                document: Arc::new(document),
                text: Arc::new(text),
            }),
            Err(_) => None,
        };
        self.entries.write().await.insert(uri, parsed.clone());
        parsed
    }

    async fn get(&self, uri: &Url) -> Option<DocumentEntry> {
        self.entries.read().await.get(uri).cloned().flatten()
    }

    async fn remove(&self, uri: &Url) {
        self.entries.write().await.remove(uri);
    }
}

fn document_directory_from_uri(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path()
        .ok()
        .and_then(|path| path.parent().map(|parent| parent.to_path_buf()))
}

fn indent_level_from_position(
    entry: &DocumentEntry,
    position: &Position,
    rules: &FormattingRules,
) -> usize {
    let indent_unit = rules.indent_string.as_str();
    if indent_unit.is_empty() {
        return 0;
    }
    let indent_len = indent_unit.len();
    let line = entry.text.lines().nth(position.line as usize).unwrap_or("");
    let prefix: String = line.chars().take(position.character as usize).collect();
    let mut level = 0;
    let mut remainder = prefix.as_str();
    while remainder.starts_with(indent_unit) {
        level += 1;
        remainder = &remainder[indent_len..];
    }
    level
}

fn semantic_tokens_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: SEMANTIC_TOKEN_KINDS
            .iter()
            .map(|kind| SemanticTokenType::new(kind.as_str()))
            .collect(),
        token_modifiers: Vec::new(),
    }
}

#[derive(Debug, Default, Clone)]
pub struct LexConfiguration {
    pub spellcheck: SpellcheckConfig,
}

#[derive(Debug, Clone)]
pub struct SpellcheckConfig {
    pub enabled: bool,
    pub language: String,
}

impl Default for SpellcheckConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            language: "en_US".to_string(),
        }
    }
}

pub struct LexLanguageServer<C = Client, P = DefaultFeatureProvider> {
    _client: C,
    documents: DocumentStore,
    features: Arc<P>,
    workspace_roots: RwLock<Vec<PathBuf>>,
    config: RwLock<LexConfiguration>,
}

impl LexLanguageServer<Client, DefaultFeatureProvider> {
    pub fn new(client: Client) -> Self {
        Self::with_features(client, Arc::new(DefaultFeatureProvider::new()))
    }
}

impl<C, P> LexLanguageServer<C, P>
where
    C: LspClient,
    P: FeatureProvider,
{
    pub fn with_features(client: C, features: Arc<P>) -> Self {
        Self {
            _client: client,
            documents: DocumentStore::default(),
            features,
            workspace_roots: RwLock::new(Vec::new()),
            config: RwLock::new(LexConfiguration::default()),
        }
    }

    async fn parse_and_store(&self, uri: Url, text: String) {
        if let Some(entry) = self.documents.upsert(uri.clone(), text).await {
            // Run spellcheck if enabled
            let config = self.config.read().await;
            if config.spellcheck.enabled {
                let spell_result = self
                    .features
                    .check_spelling(&entry.document, &config.spellcheck.language);

                if let Some(error) = spell_result.error {
                    self._client.show_message(MessageType::WARNING, error).await;
                }

                let diagnostics = spell_result.diagnostics;

                self._client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            } else {
                // Clear diagnostics if disabled
                self._client.publish_diagnostics(uri, vec![], None).await;
            }
        }
    }

    async fn document_entry(&self, uri: &Url) -> Option<DocumentEntry> {
        self.documents.get(uri).await
    }

    async fn document(&self, uri: &Url) -> Option<Arc<Document>> {
        self.document_entry(uri).await.map(|entry| entry.document)
    }

    #[allow(deprecated)]
    async fn update_workspace_roots(&self, params: &InitializeParams) {
        let mut roots = Vec::new();

        if let Some(folders) = params.workspace_folders.as_ref() {
            for folder in folders {
                if let Ok(path) = folder.uri.to_file_path() {
                    roots.push(path);
                }
            }
        }

        if roots.is_empty() {
            if let Some(root_uri) = params.root_uri.as_ref() {
                if let Ok(path) = root_uri.to_file_path() {
                    roots.push(path);
                }
            } else if let Some(root_path) = params.root_path.as_ref() {
                roots.push(PathBuf::from(root_path));
            } else if let Ok(current_dir) = std::env::current_dir() {
                roots.push(current_dir);
            }
        }

        *self.workspace_roots.write().await = roots;
    }

    async fn workspace_context_for_uri(&self, uri: &Url) -> Option<CompletionWorkspace> {
        let document_path = uri.to_file_path().ok()?;
        let roots = self.workspace_roots.read().await;
        let project_root = best_matching_root(&roots, &document_path)
            .or_else(|| document_directory_from_uri(uri))
            .or_else(|| document_path.parent().map(|path| path.to_path_buf()))
            .unwrap_or_else(|| document_path.clone());

        Some(CompletionWorkspace {
            project_root,
            document_path,
        })
    }
}

fn best_matching_root(roots: &[PathBuf], document_path: &Path) -> Option<PathBuf> {
    roots
        .iter()
        .filter(|root| document_path.starts_with(root))
        .max_by_key(|root| root.components().count())
        .cloned()
}

fn to_lsp_position(position: &AstPosition) -> Position {
    Position::new(position.line as u32, position.column as u32)
}

fn to_lsp_range(range: &AstRange) -> Range {
    Range {
        start: to_lsp_position(&range.start),
        end: to_lsp_position(&range.end),
    }
}

fn to_lsp_location(uri: &Url, range: &AstRange) -> Location {
    Location {
        uri: uri.clone(),
        range: to_lsp_range(range),
    }
}

fn spans_to_text_edits(text: &str, spans: Vec<TextEditSpan>) -> Vec<TextEdit> {
    if spans.is_empty() {
        return Vec::new();
    }
    let locator = SourceLocation::new(text);
    spans
        .into_iter()
        .map(|span| TextEdit {
            range: Range {
                start: to_lsp_position(&locator.byte_to_position(span.start)),
                end: to_lsp_position(&locator.byte_to_position(span.end)),
            },
            new_text: span.new_text,
        })
        .collect()
}

fn to_formatting_line_range(range: &Range) -> FormattingLineRange {
    let start = range.start.line as usize;
    let mut end = range.end.line as usize;
    if range.end.character > 0 || end == start {
        end += 1;
    }
    FormattingLineRange { start, end }
}

use lsp_types::{FormattingOptions, FormattingProperty};

/// Extract FormattingRules from LSP FormattingOptions.
///
/// Clients can pass custom Lex formatting options through the `properties` field
/// of FormattingOptions. Supported keys (all under "lex." prefix):
/// - lex.session_blank_lines_before
/// - lex.session_blank_lines_after
/// - lex.normalize_seq_markers
/// - lex.unordered_seq_marker
/// - lex.max_blank_lines
/// - lex.indent_string
/// - lex.preserve_trailing_blanks
/// - lex.normalize_verbatim_markers
fn extract_formatting_rules(options: &FormattingOptions) -> Option<FormattingRules> {
    // Check if any lex-specific properties are present
    let has_lex_options = options.properties.keys().any(|k| k.starts_with("lex."));

    if !has_lex_options {
        return None;
    }

    let mut rules = FormattingRules::default();

    // Apply tab_size/insert_spaces from LSP standard options
    if options.insert_spaces {
        rules.indent_string = " ".repeat(options.tab_size as usize);
    } else {
        rules.indent_string = "\t".to_string();
    }

    // Apply lex-specific overrides from properties
    for (key, value) in &options.properties {
        match key.as_str() {
            "lex.session_blank_lines_before" => {
                if let FormattingProperty::Number(n) = value {
                    rules.session_blank_lines_before = (*n).max(0) as usize;
                }
            }
            "lex.session_blank_lines_after" => {
                if let FormattingProperty::Number(n) = value {
                    rules.session_blank_lines_after = (*n).max(0) as usize;
                }
            }
            "lex.normalize_seq_markers" => {
                if let FormattingProperty::Bool(b) = value {
                    rules.normalize_seq_markers = *b;
                }
            }
            "lex.unordered_seq_marker" => {
                if let FormattingProperty::String(s) = value {
                    if let Some(c) = s.chars().next() {
                        rules.unordered_seq_marker = c;
                    }
                }
            }
            "lex.max_blank_lines" => {
                if let FormattingProperty::Number(n) = value {
                    rules.max_blank_lines = (*n).max(0) as usize;
                }
            }
            "lex.indent_string" => {
                if let FormattingProperty::String(s) = value {
                    rules.indent_string = s.clone();
                }
            }
            "lex.preserve_trailing_blanks" => {
                if let FormattingProperty::Bool(b) = value {
                    rules.preserve_trailing_blanks = *b;
                }
            }
            "lex.normalize_verbatim_markers" => {
                if let FormattingProperty::Bool(b) = value {
                    rules.normalize_verbatim_markers = *b;
                }
            }
            _ => {}
        }
    }

    Some(rules)
}

fn from_lsp_position(position: Position) -> AstPosition {
    AstPosition::new(position.line as usize, position.character as usize)
}

fn encode_semantic_tokens(tokens: &[LexSemanticToken], text: &str) -> Vec<SemanticToken> {
    let line_offsets = compute_line_offsets(text);
    let mut data = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in tokens {
        let token_type_index = SEMANTIC_TOKEN_KINDS
            .iter()
            .position(|kind| *kind == token.kind)
            .unwrap_or(0) as u32;
        for (line, start, length) in split_token_on_lines(token, text, &line_offsets) {
            if length == 0 {
                continue;
            }
            let delta_line = line.saturating_sub(prev_line);
            let delta_start = if delta_line == 0 {
                start.saturating_sub(prev_start)
            } else {
                start
            };
            data.push(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: token_type_index,
                token_modifiers_bitset: 0,
            });
            prev_line = line;
            prev_start = start;
        }
    }

    data
}

fn compute_line_offsets(text: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (idx, ch) in text.char_indices() {
        if ch == '\n' {
            offsets.push(idx + ch.len_utf8());
        }
    }
    offsets
}

/// Expand a semantic token range into single-line segments.
///
/// The LSP wire format encodes tokens as delta positions relative to the previous token
/// and disallows spanning multiple lines, so every multi-line range must be broken into
/// per-line slices before encoding.
fn split_token_on_lines(
    token: &LexSemanticToken,
    text: &str,
    line_offsets: &[usize],
) -> Vec<(u32, u32, u32)> {
    let slice = &text[token.range.span.clone()];
    let mut segments = Vec::new();
    let mut current_line = token.range.start.line as u32;
    let mut segment_start = 0;
    let base_offset = token.range.span.start;

    for (idx, ch) in slice.char_indices() {
        if ch == '\n' {
            if idx > segment_start {
                let length = (idx - segment_start) as u32;
                let absolute_start = base_offset + segment_start;
                let line_offset = line_offsets
                    .get(current_line as usize)
                    .copied()
                    .unwrap_or(0);
                let start_col = (absolute_start.saturating_sub(line_offset)) as u32;
                segments.push((current_line, start_col, length));
            }
            current_line += 1;
            segment_start = idx + ch.len_utf8();
        }
    }

    if slice.len() > segment_start {
        let length = (slice.len() - segment_start) as u32;
        let absolute_start = base_offset + segment_start;
        let line_offset = line_offsets
            .get(current_line as usize)
            .copied()
            .unwrap_or(0);
        let start_col = (absolute_start.saturating_sub(line_offset)) as u32;
        segments.push((current_line, start_col, length));
    }

    segments
}

#[allow(deprecated)]
fn to_document_symbol(symbol: &LexDocumentSymbol) -> DocumentSymbol {
    DocumentSymbol {
        name: symbol.name.clone(),
        detail: symbol.detail.clone(),
        kind: symbol.kind,
        deprecated: None,
        range: to_lsp_range(&symbol.range),
        selection_range: to_lsp_range(&symbol.selection_range),
        children: if symbol.children.is_empty() {
            None
        } else {
            Some(symbol.children.iter().map(to_document_symbol).collect())
        },
        tags: None,
    }
}

fn to_lsp_folding_range(range: &LexFoldingRange) -> FoldingRange {
    FoldingRange {
        start_line: range.start_line,
        start_character: range.start_character,
        end_line: range.end_line,
        end_character: range.end_character,
        kind: range.kind.clone(),
        collapsed_text: None,
    }
}

fn to_lsp_completion_item(candidate: &CompletionCandidate) -> CompletionItem {
    CompletionItem {
        label: candidate.label.clone(),
        kind: Some(candidate.kind),
        detail: candidate.detail.clone(),
        insert_text: candidate.insert_text.clone(),
        ..Default::default()
    }
}

fn build_document_link(uri: &Url, link: &AstDocumentLink) -> Option<DocumentLink> {
    let target = link_target_uri(uri, link)?;
    Some(DocumentLink {
        range: to_lsp_range(&link.range),
        target: Some(target),
        tooltip: None,
        data: None,
    })
}

fn link_target_uri(document_uri: &Url, link: &AstDocumentLink) -> Option<Url> {
    match link.link_type {
        LinkType::Url => Url::parse(&link.target).ok(),
        LinkType::File | LinkType::VerbatimSrc => {
            resolve_file_like_target(document_uri, &link.target)
        }
    }
}

fn resolve_file_like_target(document_uri: &Url, target: &str) -> Option<Url> {
    if target.is_empty() {
        return None;
    }
    let path = Path::new(target);
    if path.is_absolute() {
        return Url::from_file_path(path).ok();
    }
    if document_uri.scheme() == "file" {
        let mut base = document_uri.to_file_path().ok()?;
        base.pop();
        base.push(target);
        Url::from_file_path(base).ok()
    } else {
        parent_directory_uri(document_uri).join(target).ok()
    }
}

fn parent_directory_uri(uri: &Url) -> Url {
    let mut base = uri.clone();
    let mut path = base.path().to_string();
    if let Some(idx) = path.rfind('/') {
        path.truncate(idx + 1);
    } else {
        path.push('/');
    }
    base.set_path(&path);
    base.set_query(None);
    base.set_fragment(None);
    base
}

#[async_trait]
impl<C, P> tower_lsp::LanguageServer for LexLanguageServer<C, P>
where
    C: LspClient,
    P: FeatureProvider,
{
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.update_workspace_roots(&params).await;
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            document_link_provider: Some(DocumentLinkOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                resolve_provider: Some(false),
            }),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![
                    "[".to_string(),
                    ":".to_string(),
                    "=".to_string(),
                    "@".to_string(),
                ]),
                work_done_progress_options: WorkDoneProgressOptions::default(),
                all_commit_characters: None,
                ..Default::default()
            }),
            document_formatting_provider: Some(OneOf::Left(true)),
            document_range_formatting_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                    SemanticTokensOptions {
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                        legend: semantic_tokens_legend(),
                        range: None,
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                    },
                ),
            ),
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec![
                    commands::COMMAND_ECHO.to_string(),
                    commands::COMMAND_IMPORT.to_string(),
                    commands::COMMAND_EXPORT.to_string(),
                    commands::COMMAND_NEXT_ANNOTATION.to_string(),
                    commands::COMMAND_RESOLVE_ANNOTATION.to_string(),
                    commands::COMMAND_TOGGLE_ANNOTATIONS.to_string(),
                    commands::COMMAND_INSERT_ASSET.to_string(),
                    commands::COMMAND_INSERT_VERBATIM.to_string(),
                ],
                work_done_progress_options: WorkDoneProgressOptions::default(),
            }),
            ..ServerCapabilities::default()
        };

        Ok(InitializeResult {
            capabilities,
            server_info: Some(ServerInfo {
                name: "lex-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {}

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, text, .. } = params.text_document;
        self.parse_and_store(uri, text).await;
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let settings = params.settings;

        let mut config = self.config.write().await;

        if let Some(spellcheck) = settings.get("spellcheck") {
            if let Some(enabled) = spellcheck.get("enabled").and_then(|v| v.as_bool()) {
                config.spellcheck.enabled = enabled;
            }
            if let Some(language) = spellcheck.get("language").and_then(|v| v.as_str()) {
                config.spellcheck.language = language.to_string();
            }
        }

        // Re-check all documents with new settings
        let uris: Vec<Url> = self
            .documents
            .entries
            .read()
            .await
            .keys()
            .cloned()
            .collect();
        drop(config); // Release lock before async calls

        for uri in uris {
            if let Some(entry) = self.documents.get(&uri).await {
                self.parse_and_store(uri, entry.text.to_string()).await;
            }
        }
    }
    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            self.parse_and_store(params.text_document.uri, change.text)
                .await;
        }
    }

    async fn did_close(&self, params: lsp_types::DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri).await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        if let Some(entry) = self.document_entry(&params.text_document.uri).await {
            let DocumentEntry { document, text } = entry;
            let tokens = self.features.semantic_tokens(&document);
            let data = encode_semantic_tokens(&tokens, text.as_str());
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data,
            })))
        } else {
            Ok(None)
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        if let Some(document) = self.document(&params.text_document.uri).await {
            let symbols = self.features.document_symbols(&document);
            let converted: Vec<DocumentSymbol> = symbols.iter().map(to_document_symbol).collect();
            Ok(Some(DocumentSymbolResponse::Nested(converted)))
        } else {
            Ok(None)
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        if let Some(document) = self
            .document(&params.text_document_position_params.text_document.uri)
            .await
        {
            let position = from_lsp_position(params.text_document_position_params.position);
            if let Some(result) = self.features.hover(&document, position) {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: result.contents,
                    }),
                    range: Some(to_lsp_range(&result.range)),
                }));
            }
        }
        Ok(None)
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        if let Some(document) = self.document(&params.text_document.uri).await {
            let ranges = self.features.folding_ranges(&document);
            Ok(Some(ranges.iter().map(to_lsp_folding_range).collect()))
        } else {
            Ok(None)
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        if let Some(document) = self.document(&uri).await {
            let position = from_lsp_position(params.text_document_position_params.position);
            let ranges = self.features.goto_definition(&document, position);
            if ranges.is_empty() {
                Ok(None)
            } else {
                let locations: Vec<Location> = ranges
                    .iter()
                    .map(|range| to_lsp_location(&uri, range))
                    .collect();
                Ok(Some(GotoDefinitionResponse::Array(locations)))
            }
        } else {
            Ok(None)
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        if let Some(document) = self.document(&uri).await {
            let position = from_lsp_position(params.text_document_position.position);
            let include_declaration = params.context.include_declaration;
            let ranges = self
                .features
                .references(&document, position, include_declaration);
            if ranges.is_empty() {
                Ok(None)
            } else {
                Ok(Some(
                    ranges
                        .iter()
                        .map(|range| to_lsp_location(&uri, range))
                        .collect(),
                ))
            }
        } else {
            Ok(None)
        }
    }

    async fn document_link(&self, params: DocumentLinkParams) -> Result<Option<Vec<DocumentLink>>> {
        let uri = params.text_document.uri;
        if let Some(document) = self.document(&uri).await {
            let links = self.features.document_links(&document);
            let resolved: Vec<DocumentLink> = links
                .iter()
                .filter_map(|link| build_document_link(&uri, link))
                .collect();
            Ok(Some(resolved))
        } else {
            Ok(None)
        }
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        if let Some(entry) = self.document_entry(&uri).await {
            let DocumentEntry { document, text } = entry;
            let rules = extract_formatting_rules(&params.options);
            let edits = self
                .features
                .format_document(&document, text.as_str(), rules);
            Ok(Some(spans_to_text_edits(text.as_str(), edits)))
        } else {
            Ok(None)
        }
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        if let Some(entry) = self.document_entry(&uri).await {
            let DocumentEntry { document, text } = entry;
            let line_range = to_formatting_line_range(&params.range);
            let rules = extract_formatting_rules(&params.options);
            let edits = self
                .features
                .format_range(&document, text.as_str(), line_range, rules);
            Ok(Some(spans_to_text_edits(text.as_str(), edits)))
        } else {
            Ok(None)
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        if let Some(document) = self.document(&uri).await {
            let position = from_lsp_position(params.text_document_position.position);
            let workspace = self.workspace_context_for_uri(&uri).await;

            // Extract trigger character from context
            let trigger_char = params
                .context
                .as_ref()
                .and_then(|ctx| ctx.trigger_character.as_deref());

            let candidates =
                self.features
                    .completion(&document, position, workspace.as_ref(), trigger_char);
            let items: Vec<CompletionItem> =
                candidates.iter().map(to_lsp_completion_item).collect();
            Ok(Some(CompletionResponse::Array(items)))
        } else {
            Ok(None)
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let mut actions = Vec::new();

        for diagnostic in params.context.diagnostics {
            if diagnostic.source.as_deref() == Some("lex-spell") {
                // It's a spelling error
                let word = diagnostic.message.trim_start_matches("Unknown word: ");

                // 1. Suggestions
                let suggestions = crate::features::spellcheck::suggest_corrections(word, "en_US");
                for suggestion in suggestions {
                    let action = CodeAction {
                        title: suggestion.clone(),
                        kind: Some(tower_lsp::lsp_types::CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diagnostic.clone()]),
                        edit: Some(tower_lsp::lsp_types::WorkspaceEdit {
                            changes: Some(HashMap::from([(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: diagnostic.range,
                                    new_text: suggestion,
                                }],
                            )])),
                            ..Default::default()
                        }),
                        command: None,
                        is_preferred: None,
                        disabled: None,
                        data: None,
                    };
                    actions.push(CodeActionOrCommand::CodeAction(action));
                }

                // 2. Add to dictionary
                let add_action = CodeAction {
                    title: format!("Add '{word}' to dictionary"),
                    kind: Some(tower_lsp::lsp_types::CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![diagnostic.clone()]),
                    edit: None,
                    command: Some(tower_lsp::lsp_types::Command {
                        title: "Add to dictionary".to_string(),
                        command: commands::COMMAND_ADD_TO_DICTIONARY.to_string(),
                        arguments: Some(vec![
                            json!(word),
                            json!("en_US"),
                            json!(params.text_document.uri.to_string()),
                        ]),
                    }),
                    is_preferred: None,
                    disabled: None,
                    data: None,
                };
                actions.push(CodeActionOrCommand::CodeAction(add_action));
            }
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        let command = params.command.as_str();
        match command {
            commands::COMMAND_NEXT_ANNOTATION | commands::COMMAND_PREVIOUS_ANNOTATION => {
                let uri_str = params.arguments.first().and_then(|v| v.as_str());
                let pos_val = params.arguments.get(1);

                if let (Some(uri_str), Some(pos_val)) = (uri_str, pos_val) {
                    if let Ok(uri) = Url::parse(uri_str) {
                        if let Ok(position) = serde_json::from_value::<Position>(pos_val.clone()) {
                            if let Some(document) = self.document(&uri).await {
                                let ast_pos = from_lsp_position(position);
                                let navigation = if command == commands::COMMAND_NEXT_ANNOTATION {
                                    lex_analysis::annotations::next_annotation(&document, ast_pos)
                                } else {
                                    lex_analysis::annotations::previous_annotation(
                                        &document, ast_pos,
                                    )
                                };

                                if let Some(result) = navigation {
                                    let location = to_lsp_location(&uri, &result.header);
                                    return Ok(Some(
                                        serde_json::to_value(location)
                                            .map_err(|_| Error::internal_error())?,
                                    ));
                                }
                            }
                        }
                    }
                }
                Ok(None)
            }
            commands::COMMAND_RESOLVE_ANNOTATION | commands::COMMAND_TOGGLE_ANNOTATIONS => {
                let uri_str = params.arguments.first().and_then(|v| v.as_str());
                let pos_val = params.arguments.get(1);

                if let (Some(uri_str), Some(pos_val)) = (uri_str, pos_val) {
                    if let Ok(uri) = Url::parse(uri_str) {
                        if let Ok(position) = serde_json::from_value::<Position>(pos_val.clone()) {
                            if let Some(document) = self.document(&uri).await {
                                let ast_pos = from_lsp_position(position);
                                let _resolved = command == commands::COMMAND_RESOLVE_ANNOTATION;

                                // For toggle, we need to check current status, but lex-analysis toggle takes a boolean "resolved".
                                // Wait, lex-analysis toggle_annotation_resolution takes "resolved: bool".
                                // If we want to toggle, we need to know current state.
                                // But the command name "toggle_annotations" implies switching.
                                // Let's check lex-analysis signature again.
                                // toggle_annotation_resolution(doc, pos, resolved) -> Option<Edit>
                                // It sets status=resolved if resolved=true, removes it if false.
                                // So "resolve" command should pass true.
                                // "toggle" command needs to check if it's currently resolved and flip it.

                                let target_state =
                                    if command == commands::COMMAND_RESOLVE_ANNOTATION {
                                        true
                                    } else {
                                        // Check if currently resolved
                                        if let Some(annotation) =
                                            lex_analysis::utils::find_annotation_at_position(
                                                &document, ast_pos,
                                            )
                                        {
                                            let is_resolved =
                                                annotation.data.parameters.iter().any(|p| {
                                                    p.key == "status" && p.value == "resolved"
                                                });
                                            !is_resolved
                                        } else {
                                            return Ok(None);
                                        }
                                    };

                                if let Some(edit) =
                                    lex_analysis::annotations::toggle_annotation_resolution(
                                        &document,
                                        ast_pos,
                                        target_state,
                                    )
                                {
                                    let text_edit = TextEdit {
                                        range: to_lsp_range(&edit.range),
                                        new_text: edit.new_text,
                                    };
                                    let mut changes = HashMap::new();
                                    changes.insert(uri, vec![text_edit]);
                                    let workspace_edit = tower_lsp::lsp_types::WorkspaceEdit {
                                        changes: Some(changes),
                                        ..Default::default()
                                    };
                                    return Ok(Some(
                                        serde_json::to_value(workspace_edit)
                                            .map_err(|_| Error::internal_error())?,
                                    ));
                                }
                            }
                        }
                    }
                }
                Ok(None)
            }
            commands::COMMAND_INSERT_ASSET => {
                let uri_str = params.arguments.first().and_then(|v| v.as_str());
                let pos_val = params.arguments.get(1);
                let path_val = params.arguments.get(2).and_then(|v| v.as_str());

                if let (Some(uri_str), Some(pos_val), Some(path)) = (uri_str, pos_val, path_val) {
                    if let Ok(uri) = Url::parse(uri_str) {
                        if let Ok(position) = serde_json::from_value::<Position>(pos_val.clone()) {
                            let file_path = PathBuf::from(path);
                            let rules = FormattingRules::default();
                            let entry = self.document_entry(&uri).await;
                            let indent_level = entry
                                .as_ref()
                                .map(|entry| indent_level_from_position(entry, &position, &rules))
                                .unwrap_or(0);
                            let document_directory = document_directory_from_uri(&uri);
                            let snippet = {
                                let request = AssetSnippetRequest {
                                    asset_path: file_path.as_path(),
                                    document_directory: document_directory.as_deref(),
                                    formatting: &rules,
                                    indent_level,
                                };
                                build_asset_snippet(&request)
                            };

                            return Ok(Some(json!({
                                "text": snippet.text,
                                "cursorOffset": snippet.cursor_offset,
                            })));
                        }
                    }
                }
                Ok(None)
            }
            commands::COMMAND_INSERT_VERBATIM => {
                let uri_str = params.arguments.first().and_then(|v| v.as_str());
                let pos_val = params.arguments.get(1);
                let path_val = params.arguments.get(2).and_then(|v| v.as_str());

                if let (Some(uri_str), Some(pos_val), Some(path)) = (uri_str, pos_val, path_val) {
                    if let Ok(uri) = Url::parse(uri_str) {
                        if let Ok(position) = serde_json::from_value::<Position>(pos_val.clone()) {
                            let file_path = PathBuf::from(path);
                            let rules = FormattingRules::default();
                            let entry = self.document_entry(&uri).await;
                            let indent_level = entry
                                .as_ref()
                                .map(|entry| indent_level_from_position(entry, &position, &rules))
                                .unwrap_or(0);
                            let document_directory = document_directory_from_uri(&uri);
                            let snippet_result = {
                                let mut request =
                                    VerbatimSnippetRequest::new(file_path.as_path(), &rules);
                                request.document_directory = document_directory.as_deref();
                                request.indent_level = indent_level;
                                build_verbatim_snippet(&request)
                            };

                            match snippet_result {
                                Ok(snippet) => {
                                    return Ok(Some(json!({
                                        "text": snippet.text,
                                        "cursorOffset": snippet.cursor_offset,
                                    })));
                                }
                                Err(err) => {
                                    return Err(Error::invalid_params(format!(
                                        "Failed to insert verbatim block: {err}"
                                    )));
                                }
                            }
                        }
                    }
                }
                Ok(None)
            }
            commands::COMMAND_ADD_TO_DICTIONARY => {
                let word = params.arguments.first().and_then(|v| v.as_str());
                let language = params.arguments.get(1).and_then(|v| v.as_str());
                let uri_str = params.arguments.get(2).and_then(|v| v.as_str());

                if let (Some(word), Some(language), Some(uri_str)) = (word, language, uri_str) {
                    crate::features::spellcheck::add_to_dictionary(word, language);

                    // Re-validate
                    if let Ok(uri) = Url::parse(uri_str) {
                        if let Some(document) = self.document(&uri).await {
                            let spell_result = self.features.check_spelling(&document, language);
                            if let Some(error) = spell_result.error {
                                self._client.show_message(MessageType::WARNING, error).await;
                            }
                            self._client
                                .publish_diagnostics(uri, spell_result.diagnostics, None)
                                .await;
                        }
                    }
                }
                Ok(None)
            }
            _ => self
                .features
                .execute_command(&params.command, &params.arguments),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::features::semantic_tokens::LexSemanticTokenKind;
    use lex_analysis::test_support::sample_source;
    use serde::Deserialize;
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;
    use tempfile::tempdir;
    use tower_lsp::lsp_types::{
        CompletionItemKind, DidOpenTextDocumentParams, DocumentFormattingParams,
        DocumentLinkParams, DocumentRangeFormattingParams, DocumentSymbolParams, FoldingRangeKind,
        FoldingRangeParams, FormattingOptions, GotoDefinitionParams, HoverParams, Position, Range,
        ReferenceContext, ReferenceParams, SemanticTokensParams, SymbolKind,
        TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
    };
    use tower_lsp::LanguageServer;

    #[derive(Clone, Default)]
    struct NoopClient;
    #[async_trait]
    impl LspClient for NoopClient {
        async fn publish_diagnostics(&self, _: Url, _: Vec<Diagnostic>, _: Option<i32>) {}
        async fn show_message(&self, _: MessageType, _: String) {}
    }

    #[derive(Default)]
    struct MockFeatureProvider {
        semantic_tokens_called: AtomicUsize,
        document_symbols_called: AtomicUsize,
        hover_called: AtomicUsize,
        folding_called: AtomicUsize,
        last_hover_position: Mutex<Option<AstPosition>>,
        definition_called: AtomicUsize,
        references_called: AtomicUsize,
        document_links_called: AtomicUsize,
        last_references_include: Mutex<Option<bool>>,
        formatting_called: AtomicUsize,
        range_formatting_called: AtomicUsize,
        completion_called: AtomicUsize,
        execute_command_called: AtomicUsize,
    }

    impl FeatureProvider for MockFeatureProvider {
        fn semantic_tokens(&self, _: &Document) -> Vec<LexSemanticToken> {
            self.semantic_tokens_called.fetch_add(1, Ordering::SeqCst);
            vec![LexSemanticToken {
                kind: LexSemanticTokenKind::DocumentTitle,
                range: AstRange::new(0..5, AstPosition::new(0, 0), AstPosition::new(0, 5)),
            }]
        }

        fn document_symbols(&self, _: &Document) -> Vec<LexDocumentSymbol> {
            self.document_symbols_called.fetch_add(1, Ordering::SeqCst);
            vec![LexDocumentSymbol {
                name: "symbol".into(),
                detail: None,
                kind: SymbolKind::STRING,
                range: AstRange::new(0..5, AstPosition::new(0, 0), AstPosition::new(0, 5)),
                selection_range: AstRange::new(
                    0..5,
                    AstPosition::new(0, 0),
                    AstPosition::new(0, 5),
                ),
                children: Vec::new(),
            }]
        }

        fn folding_ranges(&self, _: &Document) -> Vec<LexFoldingRange> {
            self.folding_called.fetch_add(1, Ordering::SeqCst);
            vec![LexFoldingRange {
                start_line: 0,
                start_character: Some(0),
                end_line: 1,
                end_character: Some(0),
                kind: Some(FoldingRangeKind::Region),
            }]
        }

        fn hover(&self, _: &Document, position: AstPosition) -> Option<HoverResult> {
            self.hover_called.fetch_add(1, Ordering::SeqCst);
            *self.last_hover_position.lock().unwrap() = Some(position);
            Some(HoverResult {
                range: AstRange::new(0..5, AstPosition::new(0, 0), AstPosition::new(0, 5)),
                contents: "hover".into(),
            })
        }

        fn goto_definition(&self, _: &Document, _: AstPosition) -> Vec<AstRange> {
            self.definition_called.fetch_add(1, Ordering::SeqCst);
            vec![AstRange::new(
                0..5,
                AstPosition::new(0, 0),
                AstPosition::new(0, 5),
            )]
        }

        fn references(
            &self,
            _: &Document,
            _: AstPosition,
            include_declaration: bool,
        ) -> Vec<AstRange> {
            self.references_called.fetch_add(1, Ordering::SeqCst);
            *self.last_references_include.lock().unwrap() = Some(include_declaration);
            vec![AstRange::new(
                0..5,
                AstPosition::new(0, 0),
                AstPosition::new(0, 5),
            )]
        }

        fn document_links(&self, _: &Document) -> Vec<AstDocumentLink> {
            self.document_links_called.fetch_add(1, Ordering::SeqCst);
            vec![AstDocumentLink::new(
                AstRange::new(0..5, AstPosition::new(0, 0), AstPosition::new(0, 5)),
                "https://example.com".to_string(),
                LinkType::Url,
            )]
        }

        fn format_document(
            &self,
            _: &Document,
            _: &str,
            _: Option<FormattingRules>,
        ) -> Vec<TextEditSpan> {
            self.formatting_called.fetch_add(1, Ordering::SeqCst);
            vec![TextEditSpan {
                start: 0,
                end: 0,
                new_text: "formatted".into(),
            }]
        }

        fn format_range(
            &self,
            _: &Document,
            _: &str,
            _: FormattingLineRange,
            _: Option<FormattingRules>,
        ) -> Vec<TextEditSpan> {
            self.range_formatting_called.fetch_add(1, Ordering::SeqCst);
            vec![TextEditSpan {
                start: 0,
                end: 0,
                new_text: "range".into(),
            }]
        }

        fn completion(
            &self,
            _: &Document,
            _: AstPosition,
            _: Option<&CompletionWorkspace>,
            _: Option<&str>,
        ) -> Vec<CompletionCandidate> {
            self.completion_called.fetch_add(1, Ordering::SeqCst);
            vec![CompletionCandidate {
                label: "completion".into(),
                detail: None,
                kind: CompletionItemKind::TEXT,
                insert_text: None,
            }]
        }

        fn execute_command(&self, command: &str, _: &[Value]) -> Result<Option<Value>> {
            self.execute_command_called.fetch_add(1, Ordering::SeqCst);
            if command == "test.command" {
                Ok(Some(Value::String("executed".into())))
            } else {
                Ok(None)
            }
        }

        fn check_spelling(&self, _: &Document, _: &str) -> SpellcheckResult {
            SpellcheckResult {
                diagnostics: Vec::new(),
                error: None,
                misspelled_count: 0,
            }
        }
    }

    fn sample_uri() -> Url {
        Url::parse("file:///sample.lex").unwrap()
    }

    fn sample_text() -> String {
        sample_source().to_string()
    }

    fn offset_to_position(source: &str, offset: usize) -> AstPosition {
        let mut line = 0;
        let mut line_start = 0;
        for (idx, ch) in source.char_indices() {
            if idx >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                line_start = idx + ch.len_utf8();
            }
        }
        AstPosition::new(line, offset - line_start)
    }

    fn range_for_snippet(snippet: &str) -> AstRange {
        let source = sample_source();
        let start = source
            .find(snippet)
            .unwrap_or_else(|| panic!("snippet not found: {snippet}"));
        let end = start + snippet.len();
        let start_pos = offset_to_position(source, start);
        let end_pos = offset_to_position(source, end);
        AstRange::new(start..end, start_pos, end_pos)
    }

    async fn open_sample_document(server: &LexLanguageServer<NoopClient, MockFeatureProvider>) {
        let uri = sample_uri();
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri,
                    language_id: "lex".into(),
                    version: 1,
                    text: sample_text(),
                },
            })
            .await;
    }

    #[test]
    fn encode_semantic_tokens_splits_multi_line_ranges() {
        let snippet = "    CLI Example:\n        lex build\n        lex serve";
        let range = range_for_snippet(snippet);
        let tokens = vec![LexSemanticToken {
            kind: LexSemanticTokenKind::DocumentTitle,
            range,
        }];
        let source = sample_source();
        let encoded = encode_semantic_tokens(&tokens, source);
        assert_eq!(encoded.len(), 3);
        let snippet_offset = source
            .find(snippet)
            .expect("snippet not found in sample document");
        let mut cursor = 0;
        let lines: Vec<&str> = snippet.split('\n').collect();
        let mut expected_positions = Vec::new();
        for (idx, line) in lines.iter().enumerate() {
            let offset = snippet_offset + cursor;
            expected_positions.push(offset_to_position(source, offset));
            cursor += line.len();
            if idx < lines.len() - 1 {
                cursor += 1; // account for newline
            }
        }
        let mut absolute_positions = Vec::new();
        let mut line = 0u32;
        let mut column = 0u32;
        for token in &encoded {
            line += token.delta_line;
            let start = if token.delta_line == 0 {
                column + token.delta_start
            } else {
                token.delta_start
            };
            column = start;
            absolute_positions.push((line, start));
        }
        for (actual, expected) in absolute_positions.iter().zip(expected_positions.iter()) {
            assert_eq!(actual.0, expected.line as u32);
            assert_eq!(actual.1, expected.column as u32);
        }
        let expected_len: usize = snippet.lines().map(|line| line.len()).sum();
        let actual_len: usize = encoded.iter().map(|token| token.length as usize).sum();
        assert_eq!(actual_len, expected_len);
    }

    #[tokio::test]
    async fn semantic_tokens_call_feature_layer() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let result = server
            .semantic_tokens_full(SemanticTokensParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.semantic_tokens_called.load(Ordering::SeqCst), 1);
        let data_len = match result {
            SemanticTokensResult::Tokens(tokens) => tokens.data.len(),
            SemanticTokensResult::Partial(partial) => partial.data.len(),
        };
        assert!(data_len > 0);
    }

    #[tokio::test]
    async fn document_symbols_call_feature_layer() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let response = server
            .document_symbol(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        match response {
            DocumentSymbolResponse::Nested(symbols) => assert!(!symbols.is_empty()),
            _ => panic!("unexpected symbol response"),
        }
        assert_eq!(provider.document_symbols_called.load(Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn hover_uses_feature_provider_position() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let hover = server
            .hover(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: sample_uri() },
                    position: Position::new(0, 0),
                },
                work_done_progress_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert!(matches!(hover.contents, HoverContents::Markup(_)));
        assert_eq!(provider.hover_called.load(Ordering::SeqCst), 1);
        let stored = provider.last_hover_position.lock().unwrap().unwrap();
        assert_eq!(stored.line, 0);
        assert_eq!(stored.column, 0);
    }

    #[tokio::test]
    async fn folding_range_uses_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let ranges = server
            .folding_range(FoldingRangeParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.folding_called.load(Ordering::SeqCst), 1);
        assert_eq!(ranges.len(), 1);
    }

    #[tokio::test]
    async fn goto_definition_uses_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let response = server
            .goto_definition(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: sample_uri() },
                    position: Position::new(0, 0),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.definition_called.load(Ordering::SeqCst), 1);
        match response {
            GotoDefinitionResponse::Array(locations) => assert_eq!(locations.len(), 1),
            _ => panic!("unexpected goto definition response"),
        }
    }

    #[derive(Deserialize)]
    struct SnippetResponse {
        text: String,
        #[serde(rename = "cursorOffset")]
        cursor_offset: usize,
    }

    #[tokio::test]
    async fn execute_insert_commands() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let temp_dir = tempdir().unwrap();
        let asset_file = temp_dir.path().join("diagram.png");
        fs::write(&asset_file, [0u8, 159u8, 146u8, 150u8]).unwrap();

        let params = ExecuteCommandParams {
            command: commands::COMMAND_INSERT_ASSET.to_string(),
            arguments: vec![
                serde_json::to_value(sample_uri().to_string()).unwrap(),
                serde_json::to_value(Position::new(0, 0)).unwrap(),
                serde_json::to_value(asset_file.to_string_lossy()).unwrap(),
            ],
            work_done_progress_params: Default::default(),
        };
        let result = server.execute_command(params).await.unwrap();
        let snippet: SnippetResponse = serde_json::from_value(result.unwrap()).unwrap();
        assert!(snippet.text.contains(":: doc.image"));
        assert!(snippet.text.contains(asset_file.to_string_lossy().as_ref()));

        let verbatim_file = temp_dir.path().join("example.py");
        fs::write(&verbatim_file, "print('hi')\n").unwrap();

        let params = ExecuteCommandParams {
            command: commands::COMMAND_INSERT_VERBATIM.to_string(),
            arguments: vec![
                serde_json::to_value(sample_uri().to_string()).unwrap(),
                serde_json::to_value(Position::new(0, 0)).unwrap(),
                serde_json::to_value(verbatim_file.to_string_lossy()).unwrap(),
            ],
            work_done_progress_params: Default::default(),
        };
        let result = server.execute_command(params).await.unwrap();
        let snippet: SnippetResponse = serde_json::from_value(result.unwrap()).unwrap();
        assert!(snippet.text.contains(":: python"));
        assert!(snippet.text.contains("print('hi')"));
        assert_eq!(snippet.cursor_offset, 0);
    }

    #[tokio::test]
    async fn execute_annotation_navigation_commands() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        let uri = Url::parse("file:///annotations.lex").unwrap();
        let text = ":: note ::\n    First\n::\n\n:: note ::\n    Second\n::\n";
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "lex".into(),
                    version: 1,
                    text: text.to_string(),
                },
            })
            .await;

        let next_params = ExecuteCommandParams {
            command: commands::COMMAND_NEXT_ANNOTATION.to_string(),
            arguments: vec![
                serde_json::to_value(uri.to_string()).unwrap(),
                serde_json::to_value(Position::new(0, 0)).unwrap(),
            ],
            work_done_progress_params: Default::default(),
        };
        let next_location: Location =
            serde_json::from_value(server.execute_command(next_params).await.unwrap().unwrap())
                .unwrap();
        assert_eq!(next_location.range.start.line, 0);

        let previous_params = ExecuteCommandParams {
            command: commands::COMMAND_PREVIOUS_ANNOTATION.to_string(),
            arguments: vec![
                serde_json::to_value(uri.to_string()).unwrap(),
                serde_json::to_value(Position::new(0, 0)).unwrap(),
            ],
            work_done_progress_params: Default::default(),
        };
        let previous_location: Location = serde_json::from_value(
            server
                .execute_command(previous_params)
                .await
                .unwrap()
                .unwrap(),
        )
        .unwrap();
        assert_eq!(previous_location.range.start.line, 4);

        let resolve_params = ExecuteCommandParams {
            command: commands::COMMAND_RESOLVE_ANNOTATION.to_string(),
            arguments: vec![
                serde_json::to_value(uri.to_string()).unwrap(),
                serde_json::to_value(Position::new(0, 0)).unwrap(),
            ],
            work_done_progress_params: Default::default(),
        };
        let edit_value = server
            .execute_command(resolve_params)
            .await
            .unwrap()
            .unwrap();
        let workspace_edit: tower_lsp::lsp_types::WorkspaceEdit =
            serde_json::from_value(edit_value).unwrap();
        let changes = workspace_edit.changes.expect("workspace edit changes");
        let edits = changes.get(&uri).expect("edits for document");
        assert_eq!(edits[0].new_text, ":: note status=resolved ::");
    }

    #[tokio::test]
    async fn references_use_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let result = server
            .references(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: sample_uri() },
                    position: Position::new(0, 0),
                },
                context: ReferenceContext {
                    include_declaration: true,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.references_called.load(Ordering::SeqCst), 1);
        assert_eq!(result.len(), 1);
        assert_eq!(
            *provider.last_references_include.lock().unwrap(),
            Some(true)
        );
    }

    #[tokio::test]
    async fn document_links_use_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let links = server
            .document_link(DocumentLinkParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.document_links_called.load(Ordering::SeqCst), 1);
        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].target.as_ref().map(|url| url.as_str()),
            Some("https://example.com/")
        );
    }

    #[tokio::test]
    async fn formatting_uses_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let edits = server
            .formatting(DocumentFormattingParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                options: FormattingOptions::default(),
                work_done_progress_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.formatting_called.load(Ordering::SeqCst), 1);
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "formatted");
    }

    #[tokio::test]
    async fn range_formatting_uses_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());
        open_sample_document(&server).await;

        let edits = server
            .range_formatting(DocumentRangeFormattingParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                range: Range {
                    start: Position::new(0, 0),
                    end: Position::new(0, 0),
                },
                options: FormattingOptions::default(),
                work_done_progress_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.range_formatting_called.load(Ordering::SeqCst), 1);
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "range");
    }

    #[tokio::test]
    async fn semantic_tokens_returns_none_when_document_missing() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider);

        let result = server
            .semantic_tokens_full(SemanticTokensParams {
                text_document: TextDocumentIdentifier { uri: sample_uri() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .unwrap();

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn execute_command_uses_feature_provider() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider.clone());

        let result = server
            .execute_command(ExecuteCommandParams {
                command: "test.command".into(),
                arguments: vec![],
                work_done_progress_params: Default::default(),
            })
            .await
            .unwrap()
            .unwrap();

        assert_eq!(provider.execute_command_called.load(Ordering::SeqCst), 1);
        assert_eq!(result, Value::String("executed".into()));
    }

    #[tokio::test]
    async fn hover_returns_none_without_document_entry() {
        let provider = Arc::new(MockFeatureProvider::default());
        let server = LexLanguageServer::with_features(NoopClient, provider);

        let hover = server
            .hover(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: sample_uri() },
                    position: Position::new(0, 0),
                },
                work_done_progress_params: Default::default(),
            })
            .await
            .unwrap();

        assert!(hover.is_none());
    }

    #[test]
    fn extract_formatting_rules_returns_none_without_lex_properties() {
        let options = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: None,
            insert_final_newline: None,
            trim_final_newlines: None,
        };
        assert!(extract_formatting_rules(&options).is_none());
    }

    #[test]
    fn extract_formatting_rules_parses_lex_properties() {
        use std::collections::HashMap;

        let mut properties = HashMap::new();
        properties.insert(
            "lex.indent_string".to_string(),
            FormattingProperty::String("  ".to_string()),
        );
        properties.insert(
            "lex.max_blank_lines".to_string(),
            FormattingProperty::Number(3),
        );
        properties.insert(
            "lex.normalize_seq_markers".to_string(),
            FormattingProperty::Bool(false),
        );
        properties.insert(
            "lex.unordered_seq_marker".to_string(),
            FormattingProperty::String("*".to_string()),
        );

        let options = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties,
            trim_trailing_whitespace: None,
            insert_final_newline: None,
            trim_final_newlines: None,
        };

        let rules = extract_formatting_rules(&options).expect("should return Some");
        assert_eq!(rules.indent_string, "  ");
        assert_eq!(rules.max_blank_lines, 3);
        assert!(!rules.normalize_seq_markers);
        assert_eq!(rules.unordered_seq_marker, '*');
    }
}
