//! Embedded dictionary spellcheck for WASM.
//!
//! This module provides spellchecking with an embedded en_US dictionary,
//! suitable for browser environments where filesystem access is not available.

use lex_analysis::spellcheck::WordChecker;
use spellbook::Dictionary;
use std::sync::OnceLock;
use wasm_bindgen::prelude::*;

// Embed the en_US dictionary files at compile time.
const EN_US_AFF: &str = include_str!("../dictionaries/en_US.aff");
const EN_US_DIC: &str = include_str!("../dictionaries/en_US.dic");

static DICTIONARY: OnceLock<Dictionary> = OnceLock::new();

fn get_dictionary() -> &'static Dictionary {
    DICTIONARY.get_or_init(|| {
        Dictionary::new(EN_US_AFF, EN_US_DIC).expect("Failed to load embedded dictionary")
    })
}

/// Spellchecker using the embedded en_US dictionary.
#[wasm_bindgen]
pub struct EmbeddedSpellchecker {
    custom_words: Vec<String>,
}

#[wasm_bindgen]
impl EmbeddedSpellchecker {
    /// Create a new spellchecker with the embedded dictionary.
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        // Eagerly initialize the dictionary
        let _ = get_dictionary();
        EmbeddedSpellchecker {
            custom_words: Vec::new(),
        }
    }

    /// Check if a word is spelled correctly.
    pub fn check(&self, word: &str) -> bool {
        // Check custom words first
        if self
            .custom_words
            .iter()
            .any(|w| w.eq_ignore_ascii_case(word))
        {
            return true;
        }
        get_dictionary().check(word)
    }

    /// Get spelling suggestions for a word.
    pub fn suggest(&self, word: &str) -> Vec<String> {
        let mut suggestions = Vec::new();
        get_dictionary().suggest(word, &mut suggestions);
        suggestions.truncate(4);
        suggestions
    }

    /// Add a word to the custom dictionary (session-only, not persisted).
    #[wasm_bindgen(js_name = addCustomWord)]
    pub fn add_custom_word(&mut self, word: String) {
        if !self.custom_words.contains(&word) {
            self.custom_words.push(word);
        }
    }

    /// Get all custom words.
    #[wasm_bindgen(js_name = getCustomWords)]
    pub fn get_custom_words(&self) -> Vec<String> {
        self.custom_words.clone()
    }

    /// Load custom words (e.g., from localStorage).
    #[wasm_bindgen(js_name = loadCustomWords)]
    pub fn load_custom_words(&mut self, words: Vec<String>) {
        self.custom_words = words;
    }
}

impl Default for EmbeddedSpellchecker {
    fn default() -> Self {
        Self::new()
    }
}

impl WordChecker for EmbeddedSpellchecker {
    fn check(&self, word: &str) -> bool {
        EmbeddedSpellchecker::check(self, word)
    }

    fn suggest(&self, word: &str, limit: usize) -> Vec<String> {
        let mut suggestions = EmbeddedSpellchecker::suggest(self, word);
        suggestions.truncate(limit);
        suggestions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_embedded_dictionary_loads() {
        let checker = EmbeddedSpellchecker::new();
        assert!(checker.check("hello"));
        assert!(checker.check("world"));
        assert!(!checker.check("asdfghjkl"));
    }

    #[test]
    fn test_custom_words() {
        let mut checker = EmbeddedSpellchecker::new();
        // Use a truly fake word that won't be in any dictionary
        assert!(!checker.check("xyzzyqwfp"));
        checker.add_custom_word("xyzzyqwfp".to_string());
        assert!(checker.check("xyzzyqwfp"));
    }

    #[test]
    fn test_suggestions() {
        let checker = EmbeddedSpellchecker::new();
        let suggestions = checker.suggest("helo");
        assert!(!suggestions.is_empty());
    }
}
