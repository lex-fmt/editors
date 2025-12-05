//! Context-aware completion for Lex documents.
//!
//! Provides intelligent completion suggestions based on cursor position:
//!
//! - **Reference context**: Inside `[...]` brackets, offers annotation labels,
//!   definition subjects, session identifiers, and file paths found in the document.
//!
//! - **Verbatim label context**: At a verbatim block's closing label, offers
//!   standard labels (`doc.image`, `doc.code`, etc.) and common programming languages.
//!
//! - **Verbatim src context**: Inside a `src=` parameter, offers file paths
//!   referenced elsewhere in the document.
//!
//! The completion provider is document-scoped: it only suggests items that exist
//! in the current document. For cross-document completion (e.g., bibliography
//! entries), the LSP layer would need to aggregate from multiple sources.

use crate::inline::InlineSpanKind;
use crate::utils::{for_each_annotation, reference_span_at_position, session_identifier};
use ignore::WalkBuilder;
use lex_core::lex::ast::links::LinkType;
use lex_core::lex::ast::{ContentItem, Document, Position, Session};
use lsp_types::CompletionItemKind;
use pathdiff::diff_paths;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// A completion suggestion with display metadata.
///
/// Maps to LSP `CompletionItem` but remains protocol-agnostic. The LSP layer
/// converts these to the wire format. Uses [`lsp_types::CompletionItemKind`]
/// directly for semantic classification (reference, file, module, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionCandidate {
    /// The text shown in the completion menu and inserted by default.
    pub label: String,
    /// Optional description shown alongside the label (e.g., "annotation label").
    pub detail: Option<String>,
    /// Semantic category for icon display and sorting.
    pub kind: CompletionItemKind,
    /// Alternative text to insert if different from label (e.g., quoted paths).
    pub insert_text: Option<String>,
}

/// File-system context for completion requests.
///
/// Provides the project root and on-disk path to the active document so path
/// completions can scan the repository and compute proper relative insert text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionWorkspace {
    pub project_root: PathBuf,
    pub document_path: PathBuf,
}

impl CompletionCandidate {
    fn new(label: impl Into<String>, kind: CompletionItemKind) -> Self {
        Self {
            label: label.into(),
            detail: None,
            kind,
            insert_text: None,
        }
    }

    fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    fn with_insert_text(mut self, text: impl Into<String>) -> Self {
        self.insert_text = Some(text.into());
        self
    }
}

/// Internal classification of completion trigger context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionContext {
    Reference,
    VerbatimLabel,
    VerbatimSrc,
    General,
}

/// Returns completion candidates appropriate for the cursor position.
///
/// Analyzes the position to determine context (reference, verbatim label, etc.)
/// and returns relevant suggestions. The candidates are deduplicated but not
/// sorted—the LSP layer may apply additional ordering based on user preferences.
///
/// The optional `trigger_char` allows special handling for specific triggers:
/// - `@`: Returns only file path completions (asset references)
/// - `[`: Returns reference completions (annotations, definitions, sessions, paths)
/// - `:`: Returns verbatim label completions
/// - `=`: Returns path completions for src= parameters
///
/// Returns an empty vector if no completions are available.
pub fn completion_items(
    document: &Document,
    position: Position,
    workspace: Option<&CompletionWorkspace>,
    trigger_char: Option<&str>,
) -> Vec<CompletionCandidate> {
    // Handle explicit trigger characters first
    if let Some(trigger) = trigger_char {
        if trigger == "@" {
            return asset_path_completions(workspace);
        }
    }

    match detect_context(document, position) {
        CompletionContext::VerbatimLabel => verbatim_label_completions(document),
        CompletionContext::VerbatimSrc => verbatim_path_completions(document, workspace),
        CompletionContext::Reference => reference_completions(document, workspace),
        CompletionContext::General => reference_completions(document, workspace),
    }
}

/// Returns only file path completions for asset references (@-triggered).
fn asset_path_completions(workspace: Option<&CompletionWorkspace>) -> Vec<CompletionCandidate> {
    let Some(workspace) = workspace else {
        return Vec::new();
    };

    workspace_path_completion_entries(workspace)
        .into_iter()
        .map(|entry| {
            CompletionCandidate::new(&entry.label, CompletionItemKind::FILE)
                .with_detail("file")
                .with_insert_text(entry.insert_text)
        })
        .collect()
}

fn detect_context(document: &Document, position: Position) -> CompletionContext {
    if is_inside_verbatim_label(document, position) {
        return CompletionContext::VerbatimLabel;
    }
    if is_inside_verbatim_src_parameter(document, position) {
        return CompletionContext::VerbatimSrc;
    }
    if reference_span_at_position(document, position)
        .map(|span| matches!(span.kind, InlineSpanKind::Reference(_)))
        .unwrap_or(false)
    {
        return CompletionContext::Reference;
    }
    CompletionContext::General
}

fn reference_completions(
    document: &Document,
    workspace: Option<&CompletionWorkspace>,
) -> Vec<CompletionCandidate> {
    let mut items = Vec::new();

    for label in collect_annotation_labels(document) {
        items.push(
            CompletionCandidate::new(label, CompletionItemKind::REFERENCE)
                .with_detail("annotation label"),
        );
    }

    for subject in collect_definition_subjects(document) {
        items.push(
            CompletionCandidate::new(subject, CompletionItemKind::TEXT)
                .with_detail("definition subject"),
        );
    }

    for session_id in collect_session_identifiers(document) {
        items.push(
            CompletionCandidate::new(session_id, CompletionItemKind::MODULE)
                .with_detail("session identifier"),
        );
    }

    items.extend(path_completion_candidates(
        document,
        workspace,
        "path reference",
    ));

    items
}

fn verbatim_label_completions(document: &Document) -> Vec<CompletionCandidate> {
    let mut labels: BTreeSet<String> = STANDARD_VERBATIM_LABELS
        .iter()
        .chain(COMMON_CODE_LANGUAGES.iter())
        .map(|value| value.to_string())
        .collect();

    for label in collect_document_verbatim_labels(document) {
        labels.insert(label);
    }

    labels
        .into_iter()
        .map(|label| {
            CompletionCandidate::new(label, CompletionItemKind::ENUM_MEMBER)
                .with_detail("verbatim label")
        })
        .collect()
}

fn verbatim_path_completions(
    document: &Document,
    workspace: Option<&CompletionWorkspace>,
) -> Vec<CompletionCandidate> {
    path_completion_candidates(document, workspace, "verbatim src")
}

fn collect_annotation_labels(document: &Document) -> BTreeSet<String> {
    let mut labels = BTreeSet::new();
    for_each_annotation(document, &mut |annotation| {
        labels.insert(annotation.data.label.value.clone());
    });
    labels
}

fn collect_definition_subjects(document: &Document) -> BTreeSet<String> {
    let mut subjects = BTreeSet::new();
    collect_definitions_in_session(&document.root, &mut subjects);
    subjects
}

fn collect_definitions_in_session(session: &Session, subjects: &mut BTreeSet<String>) {
    for item in session.iter_items() {
        collect_definitions_in_item(item, subjects);
    }
}

fn collect_definitions_in_item(item: &ContentItem, subjects: &mut BTreeSet<String>) {
    match item {
        ContentItem::Definition(definition) => {
            let subject = definition.subject.as_string().trim();
            if !subject.is_empty() {
                subjects.insert(subject.to_string());
            }
            for child in definition.children.iter() {
                collect_definitions_in_item(child, subjects);
            }
        }
        ContentItem::Session(session) => collect_definitions_in_session(session, subjects),
        ContentItem::List(list) => {
            for child in list.items.iter() {
                collect_definitions_in_item(child, subjects);
            }
        }
        ContentItem::ListItem(list_item) => {
            for child in list_item.children.iter() {
                collect_definitions_in_item(child, subjects);
            }
        }
        ContentItem::Annotation(annotation) => {
            for child in annotation.children.iter() {
                collect_definitions_in_item(child, subjects);
            }
        }
        ContentItem::Paragraph(paragraph) => {
            for line in &paragraph.lines {
                collect_definitions_in_item(line, subjects);
            }
        }
        ContentItem::VerbatimBlock(_) | ContentItem::TextLine(_) | ContentItem::VerbatimLine(_) => {
        }
        ContentItem::BlankLineGroup(_) => {}
    }
}

fn collect_session_identifiers(document: &Document) -> BTreeSet<String> {
    let mut identifiers = BTreeSet::new();
    collect_session_ids_recursive(&document.root, &mut identifiers, true);
    identifiers
}

fn collect_session_ids_recursive(
    session: &Session,
    identifiers: &mut BTreeSet<String>,
    is_root: bool,
) {
    if !is_root {
        if let Some(id) = session_identifier(session) {
            identifiers.insert(id);
        }
        let title = session.title_text().trim();
        if !title.is_empty() {
            identifiers.insert(title.to_string());
        }
    }

    for item in session.iter_items() {
        if let ContentItem::Session(child) = item {
            collect_session_ids_recursive(child, identifiers, false);
        }
    }
}

fn collect_document_verbatim_labels(document: &Document) -> BTreeSet<String> {
    let mut labels = BTreeSet::new();
    for (item, _) in document.root.iter_all_nodes_with_depth() {
        if let ContentItem::VerbatimBlock(verbatim) = item {
            labels.insert(verbatim.closing_data.label.value.clone());
        }
    }
    labels
}

fn path_completion_candidates(
    document: &Document,
    workspace: Option<&CompletionWorkspace>,
    detail: &'static str,
) -> Vec<CompletionCandidate> {
    collect_path_completion_entries(document, workspace)
        .into_iter()
        .map(|entry| {
            CompletionCandidate::new(&entry.label, CompletionItemKind::FILE)
                .with_detail(detail)
                .with_insert_text(entry.insert_text)
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PathCompletion {
    label: String,
    insert_text: String,
}

fn collect_path_completion_entries(
    document: &Document,
    workspace: Option<&CompletionWorkspace>,
) -> Vec<PathCompletion> {
    let mut entries = Vec::new();
    let mut seen_labels = BTreeSet::new();

    if let Some(workspace) = workspace {
        for entry in workspace_path_completion_entries(workspace) {
            if seen_labels.insert(entry.label.clone()) {
                entries.push(entry);
            }
        }
    }

    for path in collect_document_path_targets(document) {
        if seen_labels.insert(path.clone()) {
            entries.push(PathCompletion {
                label: path.clone(),
                insert_text: path,
            });
        }
    }

    entries
}

fn collect_document_path_targets(document: &Document) -> BTreeSet<String> {
    document
        .find_all_links()
        .into_iter()
        .filter(|link| matches!(link.link_type, LinkType::File | LinkType::VerbatimSrc))
        .map(|link| link.target)
        .collect()
}

const MAX_WORKSPACE_PATH_COMPLETIONS: usize = 256;

fn workspace_path_completion_entries(workspace: &CompletionWorkspace) -> Vec<PathCompletion> {
    if !workspace.project_root.is_dir() {
        return Vec::new();
    }

    let document_directory = workspace
        .document_path
        .parent()
        .map(|path| path.to_path_buf())
        .unwrap_or_else(|| workspace.project_root.clone());

    let mut entries = Vec::new();
    let mut walker = WalkBuilder::new(&workspace.project_root);
    walker
        .git_ignore(true)
        .git_global(true)
        .git_exclude(true)
        .ignore(true)
        .add_custom_ignore_filename(".gitignore")
        .hidden(false)
        .follow_links(false)
        .standard_filters(true);

    for result in walker.build() {
        let entry = match result {
            Ok(entry) => entry,
            Err(_) => continue,
        };

        let file_type = match entry.file_type() {
            Some(file_type) => file_type,
            None => continue,
        };

        if !file_type.is_file() {
            continue;
        }

        if entry.path() == workspace.document_path {
            continue;
        }

        if let Some(candidate) = path_completion_from_file(
            workspace.project_root.as_path(),
            document_directory.as_path(),
            entry.path(),
        ) {
            entries.push(candidate);
            if entries.len() >= MAX_WORKSPACE_PATH_COMPLETIONS {
                break;
            }
        }
    }

    entries.sort_by(|a, b| a.label.cmp(&b.label));
    entries
}

fn path_completion_from_file(
    project_root: &Path,
    document_directory: &Path,
    file_path: &Path,
) -> Option<PathCompletion> {
    let label_path = diff_paths(file_path, project_root).unwrap_or_else(|| file_path.to_path_buf());
    let insert_path =
        diff_paths(file_path, document_directory).unwrap_or_else(|| file_path.to_path_buf());

    let label = normalize_path(&label_path)?;
    let insert_text = normalize_path(&insert_path)?;

    if label.is_empty() || insert_text.is_empty() {
        return None;
    }

    Some(PathCompletion { label, insert_text })
}

fn normalize_path(path: &Path) -> Option<String> {
    path.components().next()?;
    let mut value = path.to_string_lossy().replace('\\', "/");
    while value.starts_with("./") {
        value = value[2..].to_string();
    }
    if value == "." {
        return None;
    }
    Some(value)
}

fn is_inside_verbatim_label(document: &Document, position: Position) -> bool {
    document.root.iter_all_nodes().any(|item| match item {
        ContentItem::VerbatimBlock(verbatim) => {
            verbatim.closing_data.label.location.contains(position)
        }
        _ => false,
    })
}

fn is_inside_verbatim_src_parameter(document: &Document, position: Position) -> bool {
    document.root.iter_all_nodes().any(|item| match item {
        ContentItem::VerbatimBlock(verbatim) => verbatim
            .closing_data
            .parameters
            .iter()
            .any(|param| param.key == "src" && param.location.contains(position)),
        _ => false,
    })
}

const STANDARD_VERBATIM_LABELS: &[&str] = &[
    "doc.code",
    "doc.data",
    "doc.image",
    "doc.table",
    "doc.video",
    "doc.audio",
    "doc.note",
];

const COMMON_CODE_LANGUAGES: &[&str] = &[
    "bash",
    "c",
    "cpp",
    "css",
    "go",
    "html",
    "java",
    "javascript",
    "json",
    "kotlin",
    "latex",
    "lex",
    "markdown",
    "python",
    "ruby",
    "rust",
    "scala",
    "sql",
    "swift",
    "toml",
    "typescript",
    "yaml",
];

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::ast::SourceLocation;
    use lex_core::lex::ast::Verbatim;
    use lex_core::lex::parsing;
    use std::fs;
    use tempfile::tempdir;

    const SAMPLE_DOC: &str = r#":: note ::
    Document level note.
::

Cache:
    Definition body.

1. Intro

    See [Cache], [^note], and [./images/chart.png].

Image placeholder:

    diagram placeholder
:: doc.image src=./images/chart.png title="Usage"

Code sample:

    fn main() {}
:: rust
"#;

    fn parse_sample() -> Document {
        parsing::parse_document(SAMPLE_DOC).expect("fixture parses")
    }

    fn position_at(offset: usize) -> Position {
        SourceLocation::new(SAMPLE_DOC).byte_to_position(offset)
    }

    fn find_verbatim<'a>(document: &'a Document, label: &str) -> &'a Verbatim {
        for (item, _) in document.root.iter_all_nodes_with_depth() {
            if let ContentItem::VerbatimBlock(verbatim) = item {
                if verbatim.closing_data.label.value == label {
                    return verbatim;
                }
            }
        }
        panic!("verbatim {label} not found");
    }

    #[test]
    fn reference_completions_expose_labels_definitions_sessions_and_paths() {
        let document = parse_sample();
        let cursor = SAMPLE_DOC.find("[Cache]").expect("reference present") + 2;
        let completions = completion_items(&document, position_at(cursor), None, None);
        let labels: BTreeSet<_> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains("Cache"));
        assert!(labels.contains("note"));
        assert!(labels.contains("1"));
        assert!(labels.contains("./images/chart.png"));
    }

    #[test]
    fn verbatim_label_completions_include_standard_labels() {
        let document = parse_sample();
        let verbatim = find_verbatim(&document, "rust");
        let mut pos = verbatim.closing_data.label.location.start;
        pos.column += 1; // inside the label text
        let completions = completion_items(&document, pos, None, None);
        assert!(completions.iter().any(|c| c.label == "doc.image"));
        assert!(completions.iter().any(|c| c.label == "rust"));
    }

    #[test]
    fn verbatim_src_completion_offers_known_paths() {
        let document = parse_sample();
        let verbatim = find_verbatim(&document, "doc.image");
        let param = verbatim
            .closing_data
            .parameters
            .iter()
            .find(|p| p.key == "src")
            .expect("src parameter exists");
        let mut pos = param.location.start;
        pos.column += 5; // after `src=`
        let completions = completion_items(&document, pos, None, None);
        assert!(completions.iter().any(|c| c.label == "./images/chart.png"));
    }

    #[test]
    fn workspace_file_completion_uses_root_label_and_document_relative_insert() {
        let document = parse_sample();
        let cursor = SAMPLE_DOC.find("[Cache]").expect("reference present") + 2;

        let temp = tempdir().expect("temp dir");
        let root = temp.path();
        fs::create_dir_all(root.join("images")).unwrap();
        fs::write(root.join("images/chart.png"), "img").unwrap();
        fs::create_dir_all(root.join("docs")).unwrap();
        let document_path = root.join("docs/chapter.lex");
        fs::write(&document_path, SAMPLE_DOC).unwrap();

        let workspace = CompletionWorkspace {
            project_root: root.to_path_buf(),
            document_path,
        };

        let completions = completion_items(&document, position_at(cursor), Some(&workspace), None);

        let candidate = completions
            .iter()
            .find(|item| item.label == "images/chart.png")
            .expect("workspace path present");
        assert_eq!(
            candidate.insert_text.as_deref(),
            Some("../images/chart.png")
        );
    }

    #[test]
    fn workspace_file_completion_respects_gitignore() {
        let document = parse_sample();
        let temp = tempdir().expect("temp dir");
        let root = temp.path();
        fs::write(root.join(".gitignore"), "ignored/\n").unwrap();
        fs::create_dir_all(root.join("assets")).unwrap();
        fs::write(root.join("assets/visible.png"), "data").unwrap();
        fs::create_dir_all(root.join("ignored")).unwrap();
        fs::write(root.join("ignored/secret.png"), "nope").unwrap();
        let document_path = root.join("doc.lex");
        fs::write(&document_path, SAMPLE_DOC).unwrap();

        let workspace = CompletionWorkspace {
            project_root: root.to_path_buf(),
            document_path,
        };

        let completions = completion_items(&document, position_at(0), Some(&workspace), None);

        assert!(completions
            .iter()
            .any(|item| item.label == "assets/visible.png"));
        assert!(!completions
            .iter()
            .any(|item| item.label.contains("ignored/secret.png")));
    }

    #[test]
    fn at_trigger_returns_only_file_paths() {
        let document = parse_sample();
        let temp = tempdir().expect("temp dir");
        let root = temp.path();
        fs::create_dir_all(root.join("images")).unwrap();
        fs::write(root.join("images/photo.jpg"), "img").unwrap();
        fs::write(root.join("script.py"), "code").unwrap();
        let document_path = root.join("doc.lex");
        fs::write(&document_path, SAMPLE_DOC).unwrap();

        let workspace = CompletionWorkspace {
            project_root: root.to_path_buf(),
            document_path,
        };

        // With @ trigger, should return only file paths (no annotation labels, etc.)
        let completions = completion_items(&document, position_at(0), Some(&workspace), Some("@"));

        // Should have file paths
        assert!(completions
            .iter()
            .any(|item| item.label == "images/photo.jpg"));
        assert!(completions.iter().any(|item| item.label == "script.py"));

        // Should NOT have annotation labels or definition subjects
        assert!(!completions.iter().any(|item| item.label == "note"));
        assert!(!completions.iter().any(|item| item.label == "Cache"));
    }
}
