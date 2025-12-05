//! This is the semantic token collector, which editors use for syntax highlighting.
//! It's worth going over the general approach.
//!
//! Semantic Tokens and Editor Highlighting Architecture
//!
//!     1. LSP emits semantic tokens using our format's native terminology (e.g., `Verbatim`
//! Annotation, etc). The LSP declares a token legend at initialization and emits tokens as indices
//! into that legend—it has no knowledge of editor-specific theming.
//!     2. Editor plugins map our token types to the editor's theme primitives. This lets users
//! leverage their existing theme choices while our core LSP code remains editor-agnostic.
//!     
//! Editor-Specific Mapping
//!
//!     VSCode — declarative mapping in `package.json`:
//!         "semanticTokenScopes": [{
//!         "language": "ourformat",
//!         "scopes": {
//!         "Verbatim": ["markup.inline.raw"],
//!         "Heading": ["markup.heading"],
//!         "Emphasis": ["markup.italic"]
//!         }
//!         }]
//!     :: javascript
//!
//!     We map to TextMate scopes (`markup.*`) as they have broad theme support and are a natural
//! fit for markup.
//!
//!     Neovim — imperative mapping in the plugin:
//!         vim.api.nvim_set_hl(0, '@lsp.type.Verbatim', { link = '@markup.raw' })
//!         `vim.api.nvim_set_hl(0, '@lsp.type.Heading', { link = '@markup.heading' })
//!         vim.api.nvim_set_hl(0, '@lsp.type.Emphasis', { link = '@markup.italic' })
//!     :: lua
//!     We link to treesitter's `@markup.*` groups for equivalent theme coverage.
//!     Benefits:
//!         - LSP speaks our format's semantics—no impedance mismatch
//!         - Users get syntax highlighting that respects their theme
//!         - Mapping logic is isolated to editor plugins; adding a new editor doesn't touch the LSP
//!
//! The file editors/vscode/themes/lex-light.json has the reocommended theming for Lex to be used in
//! tests and so forth.
use crate::inline::{extract_inline_spans, InlineSpanKind};
use lex_core::lex::ast::{
    Annotation, ContentItem, Definition, Document, List, ListItem, Paragraph, Range, Session,
    TextContent, Verbatim,
};
use lex_core::lex::inlines::ReferenceType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexSemanticTokenKind {
    DocumentTitle,
    SessionMarker,
    SessionTitleText,
    DefinitionSubject,
    DefinitionContent,
    ListMarker,
    ListItemText,
    AnnotationLabel,
    AnnotationParameter,
    AnnotationContent,
    InlineStrong,
    InlineEmphasis,
    InlineCode,
    InlineMath,
    Reference,
    ReferenceCitation,
    ReferenceFootnote,
    VerbatimSubject,
    VerbatimLanguage,
    VerbatimAttribute,
    VerbatimContent,
    InlineMarkerStrongStart,
    InlineMarkerStrongEnd,
    InlineMarkerEmphasisStart,
    InlineMarkerEmphasisEnd,
    InlineMarkerCodeStart,
    InlineMarkerCodeEnd,
    InlineMarkerMathStart,
    InlineMarkerMathEnd,
    InlineMarkerRefStart,
    InlineMarkerRefEnd,
}

impl LexSemanticTokenKind {
    /// Returns the semantic token type string for LSP.
    ///
    /// These token type names are mapped to standard TextMate scopes in editor configurations
    /// to ensure compatibility with existing themes (Neovim, VSCode, etc.).
    ///
    /// Mapping rationale (based on Lex↔Markdown mapping from lex-babel):
    /// - Session → Heading → maps to "markup.heading"
    /// - Definition → Term: Desc → maps to "variable.other.definition"
    /// - InlineStrong → bold → maps to "markup.bold"
    /// - InlineEmphasis → *italic* → maps to "markup.italic"
    /// - InlineCode → `code` → maps to "markup.inline.raw"
    /// - InlineMath → $math$ → maps to "constant.numeric"
    /// - Reference → \[citation\] → maps to "markup.underline.link"
    /// - Verbatim → ```block``` → maps to "markup.raw.block"
    /// - Annotation → <!-- comment --> → maps to "comment.block"
    /// - ListMarker → - or 1. → maps to "punctuation.definition.list"
    pub fn as_str(self) -> &'static str {
        match self {
            LexSemanticTokenKind::DocumentTitle => "DocumentTitle",
            LexSemanticTokenKind::SessionMarker => "SessionMarker",
            LexSemanticTokenKind::SessionTitleText => "SessionTitleText",
            LexSemanticTokenKind::DefinitionSubject => "DefinitionSubject",
            LexSemanticTokenKind::DefinitionContent => "DefinitionContent",
            LexSemanticTokenKind::ListMarker => "ListMarker",
            LexSemanticTokenKind::ListItemText => "ListItemText",
            LexSemanticTokenKind::AnnotationLabel => "AnnotationLabel",
            LexSemanticTokenKind::AnnotationParameter => "AnnotationParameter",
            LexSemanticTokenKind::AnnotationContent => "AnnotationContent",
            LexSemanticTokenKind::InlineStrong => "InlineStrong",
            LexSemanticTokenKind::InlineEmphasis => "InlineEmphasis",
            LexSemanticTokenKind::InlineCode => "InlineCode",
            LexSemanticTokenKind::InlineMath => "InlineMath",
            LexSemanticTokenKind::Reference => "Reference",
            LexSemanticTokenKind::ReferenceCitation => "ReferenceCitation",
            LexSemanticTokenKind::ReferenceFootnote => "ReferenceFootnote",
            LexSemanticTokenKind::VerbatimSubject => "VerbatimSubject",
            LexSemanticTokenKind::VerbatimLanguage => "VerbatimLanguage",
            LexSemanticTokenKind::VerbatimAttribute => "VerbatimAttribute",
            LexSemanticTokenKind::VerbatimContent => "VerbatimContent",
            LexSemanticTokenKind::InlineMarkerStrongStart => "InlineMarker_strong_start",
            LexSemanticTokenKind::InlineMarkerStrongEnd => "InlineMarker_strong_end",
            LexSemanticTokenKind::InlineMarkerEmphasisStart => "InlineMarker_emphasis_start",
            LexSemanticTokenKind::InlineMarkerEmphasisEnd => "InlineMarker_emphasis_end",
            LexSemanticTokenKind::InlineMarkerCodeStart => "InlineMarker_code_start",
            LexSemanticTokenKind::InlineMarkerCodeEnd => "InlineMarker_code_end",
            LexSemanticTokenKind::InlineMarkerMathStart => "InlineMarker_math_start",
            LexSemanticTokenKind::InlineMarkerMathEnd => "InlineMarker_math_end",
            LexSemanticTokenKind::InlineMarkerRefStart => "InlineMarker_ref_start",
            LexSemanticTokenKind::InlineMarkerRefEnd => "InlineMarker_ref_end",
        }
    }
}

pub const SEMANTIC_TOKEN_KINDS: &[LexSemanticTokenKind] = &[
    LexSemanticTokenKind::DocumentTitle,
    LexSemanticTokenKind::SessionMarker,
    LexSemanticTokenKind::SessionTitleText,
    LexSemanticTokenKind::DefinitionSubject,
    LexSemanticTokenKind::DefinitionContent,
    LexSemanticTokenKind::ListMarker,
    LexSemanticTokenKind::ListItemText,
    LexSemanticTokenKind::AnnotationLabel,
    LexSemanticTokenKind::AnnotationParameter,
    LexSemanticTokenKind::AnnotationContent,
    LexSemanticTokenKind::InlineStrong,
    LexSemanticTokenKind::InlineEmphasis,
    LexSemanticTokenKind::InlineCode,
    LexSemanticTokenKind::InlineMath,
    LexSemanticTokenKind::Reference,
    LexSemanticTokenKind::ReferenceCitation,
    LexSemanticTokenKind::ReferenceFootnote,
    LexSemanticTokenKind::VerbatimSubject,
    LexSemanticTokenKind::VerbatimLanguage,
    LexSemanticTokenKind::VerbatimAttribute,
    LexSemanticTokenKind::VerbatimContent,
    LexSemanticTokenKind::InlineMarkerStrongStart,
    LexSemanticTokenKind::InlineMarkerStrongEnd,
    LexSemanticTokenKind::InlineMarkerEmphasisStart,
    LexSemanticTokenKind::InlineMarkerEmphasisEnd,
    LexSemanticTokenKind::InlineMarkerCodeStart,
    LexSemanticTokenKind::InlineMarkerCodeEnd,
    LexSemanticTokenKind::InlineMarkerMathStart,
    LexSemanticTokenKind::InlineMarkerMathEnd,
    LexSemanticTokenKind::InlineMarkerRefStart,
    LexSemanticTokenKind::InlineMarkerRefEnd,
];

#[derive(Debug, Clone, PartialEq)]
pub struct LexSemanticToken {
    pub kind: LexSemanticTokenKind,
    pub range: Range,
}

pub fn collect_semantic_tokens(document: &Document) -> Vec<LexSemanticToken> {
    let mut collector = TokenCollector::new();
    collector.process_document(document);
    collector.finish()
}

struct TokenCollector {
    tokens: Vec<LexSemanticToken>,
    in_annotation: bool,
    in_definition: bool,
}

impl TokenCollector {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            in_annotation: false,
            in_definition: false,
        }
    }

    fn finish(mut self) -> Vec<LexSemanticToken> {
        self.tokens.sort_by(|a, b| {
            let a_start = (
                &a.range.start.line,
                &a.range.start.column,
                &a.range.end.line,
                &a.range.end.column,
            );
            let b_start = (
                &b.range.start.line,
                &b.range.start.column,
                &b.range.end.line,
                &b.range.end.column,
            );
            a_start.cmp(&b_start)
        });
        self.tokens
    }

    fn push_range(&mut self, range: &Range, kind: LexSemanticTokenKind) {
        if range.span.start < range.span.end {
            self.tokens.push(LexSemanticToken {
                kind,
                range: range.clone(),
            });
        }
    }

    fn process_document(&mut self, document: &Document) {
        self.process_annotations(document.annotations());
        self.process_session(&document.root, LexSemanticTokenKind::DocumentTitle);
    }

    fn process_session(&mut self, session: &Session, title_kind: LexSemanticTokenKind) {
        // Emit separate tokens for marker and title text
        if let Some(marker) = &session.marker {
            // Emit SessionMarker token for the sequence marker
            self.push_range(&marker.location, LexSemanticTokenKind::SessionMarker);
        }

        // Emit SessionTitleText token for the title text (without marker)
        // Create a range for the title text by using the full title location
        // and adjusting if there's a marker
        if let Some(header) = session.header_location() {
            if let Some(marker) = &session.marker {
                // Calculate the title text range (after the marker)
                let marker_text = marker.as_str();
                let full_title = session.full_title();

                // Find where the marker ends in the title
                if let Some(pos) = full_title.find(marker_text) {
                    let marker_end = pos + marker_text.len();
                    // Skip whitespace after marker
                    let title_start = full_title[marker_end..]
                        .chars()
                        .position(|c| !c.is_whitespace())
                        .map(|p| marker_end + p)
                        .unwrap_or(marker_end);

                    if title_start < full_title.len() {
                        // Create range for title text only
                        use lex_core::lex::ast::Position;
                        let title_text_range = Range::new(
                            header.span.start + title_start..header.span.end,
                            Position::new(header.start.line, header.start.column + title_start),
                            header.end,
                        );
                        self.push_range(&title_text_range, title_kind);
                    }
                }
            } else {
                // No marker, the entire header is title text
                self.push_range(header, title_kind);
            }
        }

        self.process_text_content(&session.title);

        self.process_annotations(session.annotations());
        for child in session.children.iter() {
            self.process_content_item(child);
        }
    }

    fn process_content_item(&mut self, item: &ContentItem) {
        match item {
            ContentItem::Paragraph(paragraph) => self.process_paragraph(paragraph),
            ContentItem::Session(session) => {
                self.process_session(session, LexSemanticTokenKind::SessionTitleText)
            }
            ContentItem::List(list) => self.process_list(list),
            ContentItem::ListItem(list_item) => self.process_list_item(list_item),
            ContentItem::Definition(definition) => self.process_definition(definition),
            ContentItem::Annotation(annotation) => self.process_annotation(annotation),
            ContentItem::VerbatimBlock(verbatim) => self.process_verbatim(verbatim),
            ContentItem::TextLine(text_line) => self.process_text_content(&text_line.content),
            ContentItem::VerbatimLine(_) => {}
            ContentItem::BlankLineGroup(_) => {}
        }
    }

    fn process_paragraph(&mut self, paragraph: &Paragraph) {
        for line in &paragraph.lines {
            if let ContentItem::TextLine(text_line) = line {
                // Don't emit full-line tokens for DefinitionContent or AnnotationContent
                // as they overlap with inline tokens. The context is already clear from
                // the DefinitionSubject and AnnotationLabel tokens.
                self.process_text_content(&text_line.content);
            }
        }
        self.process_annotations(paragraph.annotations());
    }

    fn process_list(&mut self, list: &List) {
        self.process_annotations(list.annotations());
        for item in list.items.iter() {
            if let ContentItem::ListItem(list_item) = item {
                self.process_list_item(list_item);
            }
        }
    }

    fn process_list_item(&mut self, list_item: &ListItem) {
        if let Some(marker_range) = &list_item.marker.location {
            self.push_range(marker_range, LexSemanticTokenKind::ListMarker);
        }
        for text in &list_item.text {
            if let Some(location) = &text.location {
                self.push_range(location, LexSemanticTokenKind::ListItemText);
            }
            self.process_text_content(text);
        }
        self.process_annotations(list_item.annotations());
        for child in list_item.children.iter() {
            self.process_content_item(child);
        }
    }

    fn process_definition(&mut self, definition: &Definition) {
        if let Some(header) = definition.header_location() {
            self.push_range(header, LexSemanticTokenKind::DefinitionSubject);
        }
        self.process_text_content(&definition.subject);
        self.process_annotations(definition.annotations());
        let was_in_definition = self.in_definition;
        self.in_definition = true;
        for child in definition.children.iter() {
            self.process_content_item(child);
        }
        self.in_definition = was_in_definition;
    }

    fn process_verbatim(&mut self, verbatim: &Verbatim) {
        for group in verbatim.group() {
            self.process_text_content(group.subject);
            if let Some(location) = &group.subject.location {
                self.push_range(location, LexSemanticTokenKind::VerbatimSubject);
            }
        }

        self.push_range(
            &verbatim.closing_data.label.location,
            LexSemanticTokenKind::VerbatimLanguage,
        );
        for parameter in &verbatim.closing_data.parameters {
            self.push_range(&parameter.location, LexSemanticTokenKind::VerbatimAttribute);
        }

        // Highlight verbatim content lines
        for child in &verbatim.children {
            if let ContentItem::VerbatimLine(line) = child {
                self.push_range(&line.location, LexSemanticTokenKind::VerbatimContent);
            }
        }

        self.process_annotations(verbatim.annotations());
    }

    fn process_annotation(&mut self, annotation: &Annotation) {
        self.push_range(
            annotation.header_location(),
            LexSemanticTokenKind::AnnotationLabel,
        );
        for parameter in &annotation.data.parameters {
            self.push_range(
                &parameter.location,
                LexSemanticTokenKind::AnnotationParameter,
            );
        }
        let was_in_annotation = self.in_annotation;
        self.in_annotation = true;
        for child in annotation.children.iter() {
            self.process_content_item(child);
        }
        self.in_annotation = was_in_annotation;
    }

    fn process_annotations(&mut self, annotations: &[Annotation]) {
        for annotation in annotations {
            self.process_annotation(annotation);
        }
    }

    fn process_text_content(&mut self, text: &TextContent) {
        for span in extract_inline_spans(text) {
            let kind = match span.kind {
                InlineSpanKind::Strong => Some(LexSemanticTokenKind::InlineStrong),
                InlineSpanKind::Emphasis => Some(LexSemanticTokenKind::InlineEmphasis),
                InlineSpanKind::Code => Some(LexSemanticTokenKind::InlineCode),
                InlineSpanKind::Math => Some(LexSemanticTokenKind::InlineMath),
                InlineSpanKind::Reference(reference_type) => Some(match reference_type {
                    ReferenceType::Citation(_) => LexSemanticTokenKind::ReferenceCitation,
                    ReferenceType::FootnoteNumber { .. }
                    | ReferenceType::FootnoteLabeled { .. } => {
                        LexSemanticTokenKind::ReferenceFootnote
                    }
                    _ => LexSemanticTokenKind::Reference,
                }),
                InlineSpanKind::StrongMarkerStart => {
                    Some(LexSemanticTokenKind::InlineMarkerStrongStart)
                }
                InlineSpanKind::StrongMarkerEnd => {
                    Some(LexSemanticTokenKind::InlineMarkerStrongEnd)
                }
                InlineSpanKind::EmphasisMarkerStart => {
                    Some(LexSemanticTokenKind::InlineMarkerEmphasisStart)
                }
                InlineSpanKind::EmphasisMarkerEnd => {
                    Some(LexSemanticTokenKind::InlineMarkerEmphasisEnd)
                }
                InlineSpanKind::CodeMarkerStart => {
                    Some(LexSemanticTokenKind::InlineMarkerCodeStart)
                }
                InlineSpanKind::CodeMarkerEnd => Some(LexSemanticTokenKind::InlineMarkerCodeEnd),
                InlineSpanKind::MathMarkerStart => {
                    Some(LexSemanticTokenKind::InlineMarkerMathStart)
                }
                InlineSpanKind::MathMarkerEnd => Some(LexSemanticTokenKind::InlineMarkerMathEnd),
                InlineSpanKind::RefMarkerStart => Some(LexSemanticTokenKind::InlineMarkerRefStart),
                InlineSpanKind::RefMarkerEnd => Some(LexSemanticTokenKind::InlineMarkerRefEnd),
            };
            if let Some(kind) = kind {
                self.push_range(&span.range, kind);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::{sample_document, sample_source};
    use lex_core::lex::testing::lexplore::Lexplore;

    fn snippets(
        tokens: &[LexSemanticToken],
        kind: LexSemanticTokenKind,
        source: &str,
    ) -> Vec<String> {
        tokens
            .iter()
            .filter(|token| token.kind == kind)
            .map(|token| source[token.range.span.clone()].to_string())
            .collect()
    }

    #[test]
    fn collects_structural_tokens() {
        let document = sample_document();
        let tokens = collect_semantic_tokens(&document);
        let source = sample_source();

        // Session titles are now split into SessionMarker and SessionTitleText
        assert!(
            snippets(&tokens, LexSemanticTokenKind::SessionMarker, source)
                .iter()
                .any(|snippet| snippet.trim() == "1.")
        );
        assert!(
            snippets(&tokens, LexSemanticTokenKind::SessionTitleText, source)
                .iter()
                .any(|snippet| snippet.trim() == "Intro")
        );
        // Cache is parsed as VerbatimSubject
        assert!(
            snippets(&tokens, LexSemanticTokenKind::VerbatimSubject, source)
                .iter()
                .any(|snippet| snippet.trim_end() == "Cache")
        );
        let markers = snippets(&tokens, LexSemanticTokenKind::ListMarker, source);
        assert_eq!(markers.len(), 4);
        assert!(markers
            .iter()
            .all(|snippet| snippet.trim_start().starts_with('-')
                || snippet.trim_start().chars().next().unwrap().is_numeric()));
        let annotation_labels = snippets(&tokens, LexSemanticTokenKind::AnnotationLabel, source);
        assert!(annotation_labels
            .iter()
            .any(|snippet| snippet.contains("doc.note")));
        let parameters = snippets(&tokens, LexSemanticTokenKind::AnnotationParameter, source);
        assert!(parameters
            .iter()
            .any(|snippet| snippet.contains("severity=info")));
        let verbatim_subjects = snippets(&tokens, LexSemanticTokenKind::VerbatimSubject, source);
        assert!(verbatim_subjects
            .iter()
            .any(|snippet| snippet.contains("CLI Example")));
        assert!(
            snippets(&tokens, LexSemanticTokenKind::VerbatimLanguage, source)
                .iter()
                .any(|snippet| snippet.contains("shell"))
        );
    }

    #[test]
    fn collects_inline_tokens() {
        let document = sample_document();
        let tokens = collect_semantic_tokens(&document);
        let source = sample_source();
        assert!(
            snippets(&tokens, LexSemanticTokenKind::InlineStrong, source)
                .iter()
                .any(|snippet| snippet.contains("Lex"))
        );
        assert!(
            snippets(&tokens, LexSemanticTokenKind::InlineEmphasis, source)
                .iter()
                .any(|snippet| snippet.contains("format"))
        );
        assert!(snippets(&tokens, LexSemanticTokenKind::InlineCode, source)
            .iter()
            .any(|snippet| snippet.contains("code")));
        assert!(snippets(&tokens, LexSemanticTokenKind::InlineMath, source)
            .iter()
            .any(|snippet| snippet.contains("math")));
    }

    #[test]
    fn classifies_references() {
        let document = sample_document();
        let tokens = collect_semantic_tokens(&document);
        let source = sample_source();
        assert!(
            snippets(&tokens, LexSemanticTokenKind::ReferenceCitation, source)
                .iter()
                .any(|snippet| snippet.contains("@spec2025"))
        );
        assert!(
            snippets(&tokens, LexSemanticTokenKind::ReferenceFootnote, source)
                .iter()
                .any(|snippet| snippet.contains("^source"))
        );
        assert!(
            snippets(&tokens, LexSemanticTokenKind::ReferenceFootnote, source)
                .iter()
                .any(|snippet| snippet.contains("1"))
        );
        assert!(snippets(&tokens, LexSemanticTokenKind::Reference, source)
            .iter()
            .any(|snippet| snippet.contains("Cache")));
    }

    #[test]
    fn empty_document_has_no_tokens() {
        let document = Lexplore::benchmark(0)
            .parse()
            .expect("failed to parse empty benchmark fixture");
        let tokens = collect_semantic_tokens(&document);
        assert!(tokens.is_empty());
    }
}
