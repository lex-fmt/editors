use lex_core::lex::ast::elements::{ContentItem, Document, TextLine};
use lex_core::lex::ast::{AstNode, Container}; // Import AstNode for range()
use spellbook::Dictionary;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, Position as LspPosition, Range as LspRange,
};

static DICTIONARIES: OnceLock<Mutex<HashMap<String, Arc<Dictionary>>>> = OnceLock::new();

fn get_dictionaries() -> &'static Mutex<HashMap<String, Arc<Dictionary>>> {
    DICTIONARIES.get_or_init(|| Mutex::new(HashMap::new()))
}

pub enum DictionaryStatus {
    Loaded(Arc<Dictionary>),
    Missing,
    FailedToLoad,
}

fn get_dictionary(language: &str) -> DictionaryStatus {
    eprintln!("[Spellcheck] get_dictionary called for language: {language}");
    let mut cache = get_dictionaries().lock().unwrap();
    if let Some(dict) = cache.get(language) {
        eprintln!("[Spellcheck] Returning cached dictionary for {language}");
        return DictionaryStatus::Loaded(dict.clone());
    }

    // Try to load from "dictionaries" folder in CWD or adjacent to executable
    let paths_to_try = vec![
        std::path::Path::new("dictionaries"),
        std::path::Path::new("resources/dictionaries"),
        std::path::Path::new("../dictionaries"),
        std::path::Path::new("../../dictionaries"),
        std::path::Path::new("editors/lexed/dictionaries"),
        std::path::Path::new("../editors/lexed/dictionaries"),
        // Try absolute path if needed, or user home
    ];

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
                // Load custom dictionary
                let custom_path = base_path.join("custom.dic");
                if custom_path.exists() {
                    eprintln!("[Spellcheck] Found custom dictionary at {custom_path:?}");
                    if let Ok(custom_words) = std::fs::read_to_string(&custom_path) {
                        // Append custom words to dic_content
                        // Hunspell dic format: first line is count.
                        // We can try to parse and update count, or just append and hope spellbook handles it.
                        // Safest is to append.
                        dic_content.push('\n');
                        dic_content.push_str(&custom_words);
                    }
                }

                if let Ok(dict) = Dictionary::new(&aff, &dic_content) {
                    eprintln!("[Spellcheck] Successfully loaded dictionary for {language}");
                    let dict = Arc::new(dict);
                    cache.insert(language.to_string(), dict.clone());
                    return DictionaryStatus::Loaded(dict);
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

pub struct SpellcheckResult {
    pub diagnostics: Vec<Diagnostic>,
    pub error: Option<String>,
    pub misspelled_count: usize,
}

pub fn check_document(document: &Document, language: &str) -> SpellcheckResult {
    let dict_status = get_dictionary(language);

    let dict = match dict_status {
        DictionaryStatus::Loaded(d) => d,
        DictionaryStatus::Missing => {
            return SpellcheckResult {
                diagnostics: vec![],
                error: Some(format!(
                    "Dictionary for language '{language}' not found. Spellchecking disabled."
                )),
                misspelled_count: 0,
            };
        }
        DictionaryStatus::FailedToLoad => {
            return SpellcheckResult {
                diagnostics: vec![],
                error: Some(format!(
                    "Failed to load dictionary for language '{language}'. The file might be corrupted or invalid."
                )),
                misspelled_count: 0,
            };
        }
    };

    let mut diagnostics = Vec::new();
    traverse_session(&document.root, &dict, &mut diagnostics);

    let count = diagnostics.len();
    eprintln!("[Spellcheck] Document checked. Misspelled words: {count}");

    SpellcheckResult {
        diagnostics,
        error: None,
        misspelled_count: count,
    }
}

fn traverse_content_item(item: &ContentItem, dict: &Dictionary, diagnostics: &mut Vec<Diagnostic>) {
    match item {
        ContentItem::Paragraph(para) => {
            for line_item in &para.lines {
                if let ContentItem::TextLine(tl) = line_item {
                    check_text_line(tl, dict, diagnostics);
                }
            }
        }
        ContentItem::Session(session) => traverse_session(session, dict, diagnostics),
        ContentItem::TextLine(tl) => check_text_line(tl, dict, diagnostics),
        _ => {
            // Generic traversal for other containers
            if let Some(children) = item.children() {
                for child in children {
                    traverse_content_item(child, dict, diagnostics);
                }
            }
        }
    }
}

fn traverse_session(
    session: &lex_core::lex::ast::elements::Session,
    dict: &Dictionary,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for child in session.children() {
        traverse_content_item(child, dict, diagnostics);
    }
}

fn check_text_line(line: &TextLine, dict: &Dictionary, diagnostics: &mut Vec<Diagnostic>) {
    let text = line.text();
    let range = line.range();

    let mut current_offset = 0;
    for word in text.split_whitespace() {
        if let Some(index) = text[current_offset..].find(word) {
            let start_offset = current_offset + index;
            // Strip punctuation
            let clean_word = word.trim_matches(|c: char| !c.is_alphabetic());
            if !clean_word.is_empty() {
                let is_correct = dict.check(clean_word);
                if !is_correct {
                    // Calculate LSP range
                    // TextLine is always single line.
                    let start_char = range.start.column + start_offset;
                    let end_char = start_char + word.len();

                    diagnostics.push(Diagnostic {
                        range: LspRange {
                            start: LspPosition {
                                line: range.start.line as u32,
                                character: start_char as u32,
                            },
                            end: LspPosition {
                                line: range.end.line as u32,
                                character: end_char as u32,
                            },
                        },
                        severity: Some(DiagnosticSeverity::INFORMATION),
                        code: Some(tower_lsp::lsp_types::NumberOrString::String(
                            "spelling".to_string(),
                        )),
                        code_description: None,
                        source: Some("lex-spell".to_string()),
                        message: format!("Unknown word: {clean_word}"),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
            current_offset = start_offset + word.len();
        }
    }
}

pub fn suggest_corrections(word: &str, language: &str) -> Vec<String> {
    if let DictionaryStatus::Loaded(dict) = get_dictionary(language) {
        let mut suggestions = Vec::new();
        dict.suggest(word, &mut suggestions);
        suggestions.truncate(4);
        return suggestions;
    }
    vec![]
}

pub fn add_to_dictionary(word: &str, language: &str) {
    // Append to custom.dic in the first valid dictionaries folder found
    let paths_to_try = vec![
        std::path::Path::new("dictionaries"),
        std::path::Path::new("resources/dictionaries"),
        std::path::Path::new("../dictionaries"),
        std::path::Path::new("../../dictionaries"),
        std::path::Path::new("editors/lexed/dictionaries"),
        std::path::Path::new("../editors/lexed/dictionaries"),
    ];

    for base_path in paths_to_try {
        if base_path.exists() {
            let custom_path = base_path.join("custom.dic");
            use std::io::Write;
            if let Ok(mut file) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&custom_path)
            {
                if let Err(e) = writeln!(file, "{word}") {
                    eprintln!("Failed to write to custom dictionary: {e}");
                } else {
                    // Invalidate cache for this language so it reloads with new word
                    let mut cache = get_dictionaries().lock().unwrap();
                    cache.remove(language);
                }
                return;
            }
        }
    }
    eprintln!("Could not find dictionaries folder to save custom word.");
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::ast::elements::{Paragraph, Session};
    use lex_core::lex::ast::Container;
    use lex_core::lex::ast::{Position, Range};

    #[test]
    fn test_check_text() {
        let aff = "SET UTF-8\nTRY esianrtolcdugmphbyfvkwzESIANRTOLCDUGMPHBYFVKWZ'";
        let dic = "1\nhello";

        let dict = Arc::new(Dictionary::new(aff, dic).unwrap());

        {
            let mut cache = get_dictionaries().lock().unwrap();
            cache.insert("test".to_string(), dict);
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
        let dict = Arc::new(Dictionary::new(aff, dic).unwrap());

        {
            let mut cache = get_dictionaries().lock().unwrap();
            cache.insert("test_suggest".to_string(), dict);
        }

        let _suggestions = suggest_corrections("helo", "test_suggest");
        // "helo" -> "hello"
    }
}
