//! # Spellcheck Feature
//!
//! This module provides spellchecking functionality for the Lex Language Server (LSP).
//! It integrates with the `spellbook` crate to provide dictionary management and checking,
//! with support for persistent user dictionaries.
//!
//! ## Design & Integration
//!
//! The spellchecking system is designed to be:
//! - **Language Agnostic**: Dynamically loads dictionaries based on the document's language setting.
//! - **Persistent**: Custom words added by the user are saved to a standard user data directory.
//! - **LSP-Native**: Reports misspellings as standard `Diagnostics`.
//!
//! This module implements a filesystem-based `DictionaryProvider` that wraps the
//! `spellbook` crate. The core checking logic lives in `lex_analysis::spellcheck`.
//!
//! ### Dictionary Management
//!
//! Dictionaries are managed via the `spellbook` crate. We use a global `DICTIONARIES` cache
//! (protected by a `Mutex`) to store loaded dictionaries in memory, avoiding expensive reloads.
//!
//! ### Path Resolution Strategy
//!
//! To ensure robust dictionary loading across development, testing, and production (bundled) environments,
//! we employ a multi-step path resolution strategy:
//!
//! 1.  **User Data Directory**: We use the `directories` crate to locate the standard OS-specific data directory
//!     (e.g., `~/Library/Application Support/lex/lex-lsp` on macOS). This is the *primary* location for
//!     persisting the `custom.dic` file.
//! 2.  **Bundled Dictionaries**: We search relative paths (e.g., `dictionaries`, `../resources/dictionaries`)
//!     to find the base Hunspell dictionaries (`.aff` and `.dic` files) distributed with the application.
//!
//! ### Persistence (`custom.dic`)
//!
//! When a user adds a word to the dictionary:
//! 1.  The `add_to_dictionary` function resolves the user data directory.
//! 2.  It appends the new word to the `custom.dic` file in that directory.
//! 3.  The word is immediately available for future checks.
//! 4.  The in-memory dictionary cache for that language is invalidated to force a reload (incorporating the new custom word).
//!
//! ### Testing
//!
//! We support a `LEX_TEST_DATA_DIR` environment variable to override the user data directory during testing.
//! This allows unit tests (like `test_add_to_dictionary_persistence`) to verify persistence without polluting
//! the actual user's configuration.

use directories::ProjectDirs;
use lex_analysis::spellcheck::{self, SpellcheckResult, WordChecker};
use lex_core::lex::ast::elements::Document;
use spellbook::Dictionary;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, OnceLock};
use tower_lsp::lsp_types::Diagnostic;

static DICTIONARIES: OnceLock<Mutex<HashMap<String, Arc<SpellbookChecker>>>> = OnceLock::new();

fn get_dictionaries() -> &'static Mutex<HashMap<String, Arc<SpellbookChecker>>> {
    DICTIONARIES.get_or_init(|| Mutex::new(HashMap::new()))
}

fn get_data_dir() -> Option<PathBuf> {
    if let Ok(test_dir) = std::env::var("LEX_TEST_DATA_DIR") {
        return Some(PathBuf::from(test_dir));
    }
    ProjectDirs::from("org", "lex", "lex-lsp").map(|proj| proj.data_dir().to_path_buf())
}

/// Wrapper around spellbook::Dictionary that implements WordChecker.
pub struct SpellbookChecker {
    dictionary: Dictionary,
}

impl SpellbookChecker {
    pub fn new(dictionary: Dictionary) -> Self {
        Self { dictionary }
    }
}

impl WordChecker for SpellbookChecker {
    fn check(&self, word: &str) -> bool {
        self.dictionary.check(word)
    }

    fn suggest(&self, word: &str, limit: usize) -> Vec<String> {
        let mut suggestions = Vec::new();
        self.dictionary.suggest(word, &mut suggestions);
        suggestions.truncate(limit);
        suggestions
    }
}

pub enum DictionaryStatus {
    Loaded(Arc<SpellbookChecker>),
    Missing,
    FailedToLoad,
}

fn get_dictionary(language: &str) -> DictionaryStatus {
    eprintln!("[Spellcheck] get_dictionary called for language: {language}");
    let mut cache = get_dictionaries().lock().unwrap();
    if let Some(checker) = cache.get(language) {
        eprintln!("[Spellcheck] Returning cached dictionary for {language}");
        return DictionaryStatus::Loaded(checker.clone());
    }

    // Try to load from "dictionaries" folder in CWD or adjacent to executable
    let mut paths_to_try = vec![
        std::path::PathBuf::from("dictionaries"),
        std::path::PathBuf::from("resources/dictionaries"),
        std::path::PathBuf::from("../dictionaries"),
        std::path::PathBuf::from("../../dictionaries"),
        std::path::PathBuf::from("editors/lexed/dictionaries"),
        std::path::PathBuf::from("../editors/lexed/dictionaries"),
        std::path::PathBuf::from("../editors/lexed/dictionaries"),
        // Try absolute path if needed, or user home
    ];

    if let Some(data_dir) = get_data_dir() {
        paths_to_try.push(data_dir.join("dictionaries"));
    }

    let cwd = std::env::current_dir().unwrap_or_default();
    eprintln!("[Spellcheck] CWD: {cwd:?}");
    if let Ok(exe) = std::env::current_exe() {
        eprintln!("[Spellcheck] Executable path: {exe:?}");
    }

    for base_path in paths_to_try {
        let aff_path = base_path.join(format!("{language}.aff"));
        let dic_path = base_path.join(format!("{language}.dic"));

        eprintln!(
            "[Spellcheck] Trying path: {:?}",
            base_path.canonicalize().unwrap_or(base_path.to_path_buf())
        );
        eprintln!("[Spellcheck] Checking aff: {aff_path:?}, dic: {dic_path:?}");

        if aff_path.exists() && dic_path.exists() {
            eprintln!("[Spellcheck] Found dictionary files at {base_path:?}");
            if let (Ok(aff), Ok(mut dic_content)) = (
                std::fs::read_to_string(&aff_path),
                std::fs::read_to_string(&dic_path),
            ) {
                // Load custom dictionary from the same folder (Legacy/Portable)
                let custom_path = base_path.join("custom.dic");
                if custom_path.exists() {
                    eprintln!("[Spellcheck] Found custom dictionary at {custom_path:?}");
                    if let Ok(custom_words) = std::fs::read_to_string(&custom_path) {
                        dic_content.push('\n');
                        dic_content.push_str(&custom_words);
                    }
                }

                // Load global custom dictionary from user data dir
                if let Some(data_dir) = get_data_dir() {
                    let global_custom_path = data_dir.join("dictionaries").join("custom.dic");
                    // Avoid double loading if base_path IS the data dir
                    if global_custom_path.exists() && global_custom_path != custom_path {
                        eprintln!(
                            "[Spellcheck] Found global custom dictionary at {global_custom_path:?}"
                        );
                        if let Ok(custom_words) = std::fs::read_to_string(&global_custom_path) {
                            dic_content.push('\n');
                            dic_content.push_str(&custom_words);
                        }
                    }
                }

                if let Ok(dict) = Dictionary::new(&aff, &dic_content) {
                    eprintln!("[Spellcheck] Successfully loaded dictionary for {language}");
                    let checker = Arc::new(SpellbookChecker::new(dict));
                    cache.insert(language.to_string(), checker.clone());
                    return DictionaryStatus::Loaded(checker);
                } else {
                    eprintln!("[Spellcheck] Failed to parse dictionary for {language}");
                    return DictionaryStatus::FailedToLoad;
                }
            } else {
                eprintln!("[Spellcheck] Failed to read dictionary files");
                return DictionaryStatus::FailedToLoad;
            }
        }
    }

    eprintln!("[Spellcheck] Dictionary not found for {language}");
    // If we can't find a dictionary, return Missing.
    DictionaryStatus::Missing
}

/// Result wrapper that includes error information from dictionary loading.
pub struct LspSpellcheckResult {
    pub diagnostics: Vec<Diagnostic>,
    pub error: Option<String>,
    pub misspelled_count: usize,
}

pub fn check_document(document: &Document, language: &str) -> LspSpellcheckResult {
    let dict_status = get_dictionary(language);

    let checker = match dict_status {
        DictionaryStatus::Loaded(c) => c,
        DictionaryStatus::Missing => {
            return LspSpellcheckResult {
                diagnostics: vec![],
                error: Some(format!(
                    "Dictionary for language '{language}' not found. Spellchecking disabled."
                )),
                misspelled_count: 0,
            };
        }
        DictionaryStatus::FailedToLoad => {
            return LspSpellcheckResult {
                diagnostics: vec![],
                error: Some(format!(
                    "Failed to load dictionary for language '{language}'. The file might be corrupted or invalid."
                )),
                misspelled_count: 0,
            };
        }
    };

    let SpellcheckResult {
        diagnostics,
        misspelled_count,
    } = spellcheck::check_document(document, checker.as_ref());

    eprintln!("[Spellcheck] Document checked. Misspelled words: {misspelled_count}");

    LspSpellcheckResult {
        diagnostics,
        error: None,
        misspelled_count,
    }
}

pub fn suggest_corrections(word: &str, language: &str) -> Vec<String> {
    if let DictionaryStatus::Loaded(checker) = get_dictionary(language) {
        return spellcheck::suggest_corrections(word, checker.as_ref(), 4);
    }
    vec![]
}

pub fn add_to_dictionary(word: &str, language: &str) {
    // Prefer user data directory
    let target_dir = if let Some(dir) = get_data_dir() {
        dir.join("dictionaries")
    } else {
        // Fallback to local "dictionaries" folder if we can't get data dir
        PathBuf::from("dictionaries")
    };

    if let Err(e) = std::fs::create_dir_all(&target_dir) {
        eprintln!("[Spellcheck] Failed to create dictionaries directory: {e}");
        return;
    }

    let custom_path = target_dir.join("custom.dic");

    use std::io::Write;
    if let Ok(mut file) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&custom_path)
    {
        if let Err(e) = writeln!(file, "{word}") {
            eprintln!("Failed to write to custom dictionary: {e}");
        } else {
            eprintln!("[Spellcheck] Added '{word}' to {custom_path:?}");
            // Invalidate cache for this language so it reloads with new word
            let mut cache = get_dictionaries().lock().unwrap();
            cache.remove(language);
        }
        return;
    }
    eprintln!("Could not open custom dictionary for writing at {custom_path:?}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::ast::elements::{ContentItem, Paragraph, Session};
    use lex_core::lex::ast::{Container, Position, Range};

    #[test]
    fn test_check_text() {
        let aff = "SET UTF-8\nTRY esianrtolcdugmphbyfvkwzESIANRTOLCDUGMPHBYFVKWZ'";
        let dic = "1\nhello";

        let dict = Dictionary::new(aff, dic).unwrap();
        let checker = Arc::new(SpellbookChecker::new(dict));

        {
            let mut cache = get_dictionaries().lock().unwrap();
            cache.insert("test".to_string(), checker);
        }

        let range = Range::new(0..11, Position::new(0, 0), Position::new(0, 11));

        let para = Paragraph::from_line("hello world".to_string()).at(range.clone());

        // Construct a document with a root session containing the paragraph
        let mut session = Session::with_title("Title".to_string());
        session.children_mut().push(ContentItem::Paragraph(para));

        let doc = Document {
            root: session,
            ..Default::default()
        };

        let diags = check_document(&doc, "test");

        assert_eq!(diags.diagnostics.len(), 1);
        assert_eq!(diags.diagnostics[0].message, "Unknown word: world");
    }

    #[test]
    fn test_suggest() {
        let aff = "SET UTF-8\nTRY esianrtolcdugmphbyfvkwzESIANRTOLCDUGMPHBYFVKWZ'\nREP 1\nREP o 0";
        let dic = "1\nhello";
        let dict = Dictionary::new(aff, dic).unwrap();
        let checker = Arc::new(SpellbookChecker::new(dict));

        {
            let mut cache = get_dictionaries().lock().unwrap();
            cache.insert("test_suggest".to_string(), checker);
        }

        let _suggestions = suggest_corrections("helo", "test_suggest");
        // "helo" -> "hello"
    }

    #[test]
    fn test_add_to_dictionary_persistence() {
        // Create a temp dir for user data
        let temp = tempfile::tempdir().unwrap();
        let data_dir = temp.path().to_path_buf();
        let dict_dir = data_dir.join("dictionaries");
        std::fs::create_dir_all(&dict_dir).unwrap();

        // Create a fake base dictionary
        let lang = "test_persistence";
        let aff = "SET UTF-8\nTRY a";
        let dic = "1\nhello";
        std::fs::write(dict_dir.join(format!("{lang}.aff")), aff).unwrap();
        std::fs::write(dict_dir.join(format!("{lang}.dic")), dic).unwrap();

        // Set env var to trick get_data_dir
        unsafe {
            std::env::set_var("LEX_TEST_DATA_DIR", data_dir.to_str().unwrap());
        }

        // Add a word
        add_to_dictionary("foobar", lang);

        // Verify file exists
        let custom_dic = dict_dir.join("custom.dic");
        assert!(custom_dic.exists());
        let content = std::fs::read_to_string(&custom_dic).unwrap();
        assert!(content.contains("foobar"));

        // Verify check_document picks it up
        // First, ensure cache is empty or doesn't have it (add_to_dictionary should have cleared it)
        {
            let mut cache = get_dictionaries().lock().unwrap();
            cache.remove(lang);
        }

        let range = Range::new(0..11, Position::new(0, 0), Position::new(0, 11));
        let para = Paragraph::from_line("hello foobar".to_string()).at(range.clone());
        let mut session = Session::with_title("Title".to_string());
        session.children_mut().push(ContentItem::Paragraph(para));
        let doc = Document {
            root: session,
            ..Default::default()
        };

        // It should pass now
        let diags = check_document(&doc, lang);
        assert_eq!(diags.diagnostics.len(), 0);

        unsafe {
            std::env::remove_var("LEX_TEST_DATA_DIR");
        }
    }
}
