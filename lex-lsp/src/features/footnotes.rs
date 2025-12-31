use lex_core::lex::ast::{
    ContentItem, Document, Range, Session, TextContent,
};
use lex_core::lex::inlines::ReferenceType;
use lex_analysis::inline::{extract_inline_spans, InlineSpanKind};
use lex_analysis::utils::collect_all_annotations;
use std::collections::HashMap;

/// Reorders footnotes in the document to be sequential (1, 2, 3...) based on appearance.
/// Returns the new document content.
pub fn reorder_footnotes(document: &Document, source: &str) -> String {
    let mut references = Vec::new();

    // 1. Collect all footnote references in order
    // Use local traversal to avoid issues with lex-analysis traversal
    traverse_document(document, &mut |text| {
        let spans = extract_inline_spans(text);
        for span in spans {
            if let InlineSpanKind::Reference(ReferenceType::FootnoteNumber { number }) = span.kind {
                references.push((number, span.range));
            }
        }
    });

    let mut mapping: HashMap<u32, u32> = HashMap::new();
    let mut intended_reference_replacements = Vec::new();
    let mut next_id = 1;

    for (old_number, range) in references {
        let new_number = *mapping.entry(old_number).or_insert_with(|| {
            let n = next_id;
            next_id += 1;
            n
        });
        intended_reference_replacements.push((range, new_number));
    }

    // 2. Identify Definition replacements
    // Iterate over all annotations. If label parses as number and exists in mapping, replace it.
    let annotations = collect_all_annotations(document);
    let mut definition_replacements = Vec::new();

    for annotation in annotations {
        let label_str = annotation.data.label.value.trim();
        if let Ok(old_number) = label_str.parse::<u32>() {
            if let Some(&new_number) = mapping.get(&old_number) {
                // Replace the label part.
                // annotation.data.label.location is the range of the label text.
                definition_replacements.push((annotation.data.label.location.clone(), new_number));
            }
        }
    }

    // 3. Apply replacements
    #[derive(Clone, Copy)]
    enum ReplacementKind {
        Reference(u32),
        Definition(u32),
    }

    let mut edits: Vec<(Range, ReplacementKind)> = Vec::new();
    for (range, new_val) in intended_reference_replacements {
        edits.push((range, ReplacementKind::Reference(new_val)));
    }
    for (range, new_val) in definition_replacements {
        edits.push((range, ReplacementKind::Definition(new_val)));
    }

    let offsets = line_offsets(source);

    // Convert Range to (start_byte, end_byte, kind)
    let mut byte_edits: Vec<(usize, usize, ReplacementKind)> = edits.iter().map(|(range, kind)| {
         let start = pos_to_byte(&offsets, range.start);
         let end = pos_to_byte(&offsets, range.end);
         (start, end, *kind)
    }).collect();

    // Sort by start desc
    // Note: If ranges overlap, this handling is naive. But references/definitions shouldn't overlap.
    byte_edits.sort_by(|a, b| b.0.cmp(&a.0));

    let mut new_source = source.to_string();
    for (start, end, kind) in byte_edits {
        if start <= end && end <= new_source.len() {
             let original = &new_source[start..end];
             let replacement = match kind {
                 ReplacementKind::Reference(n) => n.to_string(),
                 ReplacementKind::Definition(n) => {
                     // Preserve padding
                     let leading_space = original.chars().take_while(|c| c.is_whitespace()).collect::<String>();
                     let trailing_space = original.chars().rev().take_while(|c| c.is_whitespace()).collect::<String>().chars().rev().collect::<String>();
                     format!("{}{}{}", leading_space, n, trailing_space)
                 }
             };
             
             new_source.replace_range(start..end, &replacement);
        }
    }

    new_source
}

fn line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.char_indices() {
         if ch == '\n' {
             offsets.push(i + 1);
         }
    }
    offsets
}

fn pos_to_byte(offsets: &[usize], pos: lex_core::lex::ast::Position) -> usize {
    let line_idx = pos.line as usize;
    let line_start = *offsets.get(line_idx).unwrap_or(offsets.last().unwrap_or(&0));
    // As observed in server.rs, column seems to be byte offset from line start
    line_start + pos.column as usize
}

fn traverse_document<F>(document: &Document, f: &mut F)
where
    F: FnMut(&TextContent),
{
    f(&document.root.title);
    visit_session(&document.root, true, f);
    for annotation in document.annotations() {
        for child in annotation.children.iter() {
            visit_content(child, f);
        }
    }
}

fn visit_session<F>(session: &Session, is_root: bool, f: &mut F)
where
    F: FnMut(&TextContent),
{
    if !is_root {
        f(&session.title);
    }
    for child in &session.children {
        visit_content(child, f);
    }
    for annotation in session.annotations() {
        for child in annotation.children.iter() {
            visit_content(child, f);
        }
    }
}

fn visit_content<F>(item: &ContentItem, f: &mut F)
where
    F: FnMut(&TextContent),
{
    match item {
        ContentItem::Paragraph(p) => {
            for line in &p.lines {
                if let ContentItem::TextLine(l) = line {
                    f(&l.content);
                }
            }
        }
        ContentItem::Session(s) => visit_session(s, false, f),
        ContentItem::Definition(d) => {
            for child in &d.children {
                visit_content(child, f);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    #[test]
    fn reorders_references_and_definitions() {
        let source = "Ref [2] and [1].\n\n:: 1 ::\nNote 1.\n::\n\n:: 2 ::\nNote 2.\n::\n";
        let doc = parsing::parse_document(source).unwrap();
        println!("AST: {:#?}", doc);
        let new_source = reorder_footnotes(&doc, source);
        
        // Expected: First ref [2] becomes [1]. Second ref [1] becomes [2].
        // Definitions: :: 2 :: -> :: 1 ::, :: 1 :: -> :: 2 ::.
        
        let expected = "Ref [1] and [2].\n\n:: 2 ::\nNote 1.\n::\n\n:: 1 ::\nNote 2.\n::\n";
        assert_eq!(new_source, expected);
    }

    #[test]
    fn keeps_correct_order_for_repeated_refs() {
        let source = "Ref [10] then [10] then [5].\n\n:: 5 ::\nContent.\n::";
        // 10 appears first -> becomes 1.
        // 5 appears second -> becomes 2.
        // :: 5 :: -> :: 2 ::
        // :: 10 :: doesn't exist, so no def update for 10.
        
        let doc = parsing::parse_document(source).unwrap();
        let new_source = reorder_footnotes(&doc, source);
        
        let expected = "Ref [1] then [1] then [2].\n\n:: 2 ::\nContent.\n::";
        assert_eq!(new_source, expected);
    }
}
