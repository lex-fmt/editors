use lex_analysis::semantic_tokens::{collect_semantic_tokens, LexSemanticTokenKind};
use lex_core::lex::loader::DocumentLoader;
use std::ops::Range;

#[test]
fn test_reference_markers_in_definition_body() {
    let source = r#"Cache:
    A definition body referencing [Cache].
"#;

    let loader = DocumentLoader::from_string(source);
    let doc = loader.parse().expect("failed to parse");
    let tokens = collect_semantic_tokens(&doc);

    println!("\n=== All Tokens ===");
    for token in &tokens {
        let snippet = &source[token.range.span.clone()];
        println!(
            "Range {:2}..{:2} | {:40} | {:?}",
            token.range.span.start,
            token.range.span.end,
            format!("{:?}", token.kind),
            snippet
        );
    }

    // Check for overlaps
    fn ranges_overlap(a: &Range<usize>, b: &Range<usize>) -> bool {
        a.start < b.end && b.start < a.end
    }

    println!("\n=== Checking for overlaps ===");
    let mut found_overlap = false;
    for i in 0..tokens.len() {
        for j in (i + 1)..tokens.len() {
            let a = &tokens[i];
            let b = &tokens[j];

            if ranges_overlap(&a.range.span, &b.range.span) {
                let a_text = &source[a.range.span.clone()];
                let b_text = &source[b.range.span.clone()];
                println!(
                    "❌ OVERLAP: {:?} [{:?}] {}..{} WITH {:?} [{:?}] {}..{}",
                    a.kind,
                    a_text,
                    a.range.span.start,
                    a.range.span.end,
                    b.kind,
                    b_text,
                    b.range.span.start,
                    b.range.span.end
                );
                found_overlap = true;
            }
        }
    }

    if !found_overlap {
        println!("✅ No overlapping tokens found");
    }

    // Check that reference markers exist
    let ref_start_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.kind == LexSemanticTokenKind::InlineMarkerRefStart)
        .collect();

    let ref_end_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.kind == LexSemanticTokenKind::InlineMarkerRefEnd)
        .collect();

    println!("\n=== Reference Markers in Definition ===");
    println!("RefMarkerStart: {}", ref_start_tokens.len());
    println!("RefMarkerEnd: {}", ref_end_tokens.len());

    assert_eq!(
        ref_start_tokens.len(),
        1,
        "Should have 1 '[' marker in definition body"
    );
    assert_eq!(
        ref_end_tokens.len(),
        1,
        "Should have 1 ']' marker in definition body"
    );
}

#[test]
fn test_reference_markers_in_paragraph_vs_definition() {
    let source = r#"Reference in paragraph [Cache].

Cache:
    Reference in definition [Cache].
"#;

    let loader = DocumentLoader::from_string(source);
    let doc = loader.parse().expect("failed to parse");
    let tokens = collect_semantic_tokens(&doc);

    println!("\n=== Comparing Paragraph vs Definition ===");

    let paragraph_tokens: Vec<_> = tokens.iter().filter(|t| t.range.span.start < 35).collect();

    let definition_tokens: Vec<_> = tokens.iter().filter(|t| t.range.span.start >= 35).collect();

    println!("\nParagraph tokens:");
    for token in &paragraph_tokens {
        let snippet = &source[token.range.span.clone()];
        println!("  {:40} | {:?}", format!("{:?}", token.kind), snippet);
    }

    println!("\nDefinition tokens:");
    for token in &definition_tokens {
        let snippet = &source[token.range.span.clone()];
        println!("  {:40} | {:?}", format!("{:?}", token.kind), snippet);
    }

    // Both should have reference markers
    let para_ref_markers = paragraph_tokens
        .iter()
        .filter(|t| {
            matches!(
                t.kind,
                LexSemanticTokenKind::InlineMarkerRefStart
                    | LexSemanticTokenKind::InlineMarkerRefEnd
            )
        })
        .count();

    let def_ref_markers = definition_tokens
        .iter()
        .filter(|t| {
            matches!(
                t.kind,
                LexSemanticTokenKind::InlineMarkerRefStart
                    | LexSemanticTokenKind::InlineMarkerRefEnd
            )
        })
        .count();

    println!("\nParagraph reference markers: {para_ref_markers}");
    println!("Definition reference markers: {def_ref_markers}");

    assert_eq!(para_ref_markers, 2, "Paragraph should have [ and ]");
    assert_eq!(def_ref_markers, 2, "Definition should have [ and ]");
}
