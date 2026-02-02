//! Spellcheck analysis for Lex documents.
//!
//! This module provides the core spellchecking logic, decoupled from
//! dictionary loading. Consumers provide a `DictionaryProvider` implementation
//! to handle dictionary source (filesystem, embedded, etc.).
//!
//! # Architecture
//!
//! The spellcheck system is split into two parts:
//! - **Core logic** (this module): Traverses documents, extracts words, checks spelling
//! - **Dictionary provider** (consumer-provided): Loads and caches dictionaries
//!
//! This design allows the same checking logic to work in:
//! - Native LSP (filesystem-based dictionaries)
//! - WASM (embedded dictionaries)
//! - Tests (mock dictionaries)

use lex_core::lex::ast::elements::{ContentItem, Document, Session, TextLine};
use lex_core::lex::ast::{AstNode, Container};
use lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};

/// A word checker that can verify spelling and suggest corrections.
///
/// This trait abstracts the dictionary implementation, allowing different
/// backends (spellbook, hunspell, embedded, mock, etc.).
pub trait WordChecker: Send + Sync {
    /// Check if a word is spelled correctly.
    fn check(&self, word: &str) -> bool;

    /// Get spelling suggestions for a misspelled word.
    /// Returns up to `limit` suggestions.
    fn suggest(&self, word: &str, limit: usize) -> Vec<String>;
}

/// Result of checking a document for spelling errors.
#[derive(Debug, Default)]
pub struct SpellcheckResult {
    /// Diagnostics for misspelled words.
    pub diagnostics: Vec<Diagnostic>,
    /// Number of misspelled words found.
    pub misspelled_count: usize,
}

/// Check a document for spelling errors using the provided word checker.
pub fn check_document(document: &Document, checker: &dyn WordChecker) -> SpellcheckResult {
    let mut diagnostics = Vec::new();
    traverse_session(&document.root, checker, &mut diagnostics);

    let misspelled_count = diagnostics.len();
    SpellcheckResult {
        diagnostics,
        misspelled_count,
    }
}

/// Get spelling suggestions for a word.
pub fn suggest_corrections(word: &str, checker: &dyn WordChecker, limit: usize) -> Vec<String> {
    checker.suggest(word, limit)
}

fn traverse_session(
    session: &Session,
    checker: &dyn WordChecker,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for child in session.children() {
        traverse_content_item(child, checker, diagnostics);
    }
}

fn traverse_content_item(
    item: &ContentItem,
    checker: &dyn WordChecker,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match item {
        ContentItem::Paragraph(para) => {
            for line_item in &para.lines {
                if let ContentItem::TextLine(tl) = line_item {
                    check_text_line(tl, checker, diagnostics);
                }
            }
        }
        ContentItem::Session(session) => traverse_session(session, checker, diagnostics),
        ContentItem::TextLine(tl) => check_text_line(tl, checker, diagnostics),
        _ => {
            // Generic traversal for other containers
            if let Some(children) = item.children() {
                for child in children {
                    traverse_content_item(child, checker, diagnostics);
                }
            }
        }
    }
}

fn check_text_line(line: &TextLine, checker: &dyn WordChecker, diagnostics: &mut Vec<Diagnostic>) {
    let text = line.text();
    let range = line.range();

    let mut current_offset = 0;
    for word in text.split_whitespace() {
        if let Some(index) = text[current_offset..].find(word) {
            let start_offset = current_offset + index;
            // Strip punctuation
            let clean_word = word.trim_matches(|c: char| !c.is_alphabetic());
            if !clean_word.is_empty() && !checker.check(clean_word) {
                // Calculate LSP range
                // TextLine is always single line.
                let start_char = range.start.column + start_offset;
                let end_char = start_char + word.len();

                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position {
                            line: range.start.line as u32,
                            character: start_char as u32,
                        },
                        end: Position {
                            line: range.end.line as u32,
                            character: end_char as u32,
                        },
                    },
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("spelling".to_string())),
                    code_description: None,
                    source: Some("lex-spell".to_string()),
                    message: format!("Unknown word: {clean_word}"),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
            current_offset = start_offset + word.len();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::ast::elements::Paragraph;
    use lex_core::lex::ast::{Position as AstPosition, Range as AstRange};

    /// A simple mock checker for testing.
    struct MockChecker {
        known_words: Vec<&'static str>,
    }

    impl MockChecker {
        fn new(words: &[&'static str]) -> Self {
            Self {
                known_words: words.to_vec(),
            }
        }
    }

    impl WordChecker for MockChecker {
        fn check(&self, word: &str) -> bool {
            self.known_words
                .iter()
                .any(|w| w.eq_ignore_ascii_case(word))
        }

        fn suggest(&self, _word: &str, _limit: usize) -> Vec<String> {
            vec![]
        }
    }

    #[test]
    fn test_check_document_finds_misspellings() {
        let checker = MockChecker::new(&["hello", "world"]);

        let range = AstRange::new(0..17, AstPosition::new(0, 0), AstPosition::new(0, 17));
        let para = Paragraph::from_line("hello wrold test".to_string()).at(range);

        let mut session = Session::with_title("Title".to_string());
        session.children_mut().push(ContentItem::Paragraph(para));

        let doc = Document {
            root: session,
            ..Default::default()
        };

        let result = check_document(&doc, &checker);

        // "wrold" and "test" should be flagged
        assert_eq!(result.misspelled_count, 2);
        assert_eq!(result.diagnostics.len(), 2);
        assert!(result.diagnostics[0].message.contains("wrold"));
        assert!(result.diagnostics[1].message.contains("test"));
    }

    #[test]
    fn test_check_document_no_errors() {
        let checker = MockChecker::new(&["hello", "world"]);

        let range = AstRange::new(0..11, AstPosition::new(0, 0), AstPosition::new(0, 11));
        let para = Paragraph::from_line("hello world".to_string()).at(range);

        let mut session = Session::with_title("Title".to_string());
        session.children_mut().push(ContentItem::Paragraph(para));

        let doc = Document {
            root: session,
            ..Default::default()
        };

        let result = check_document(&doc, &checker);

        assert_eq!(result.misspelled_count, 0);
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn test_punctuation_stripped() {
        let checker = MockChecker::new(&["hello"]);

        let range = AstRange::new(0..8, AstPosition::new(0, 0), AstPosition::new(0, 8));
        let para = Paragraph::from_line("hello!!!".to_string()).at(range);

        let mut session = Session::with_title("Title".to_string());
        session.children_mut().push(ContentItem::Paragraph(para));

        let doc = Document {
            root: session,
            ..Default::default()
        };

        let result = check_document(&doc, &checker);

        // "hello" with punctuation should still match
        assert_eq!(result.misspelled_count, 0);
    }
}
