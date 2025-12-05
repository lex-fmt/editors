use lex_analysis::semantic_tokens::{collect_semantic_tokens, LexSemanticTokenKind};
use lex_core::lex::loader::DocumentLoader;
use std::ops::Range;

#[test]
fn test_inline_reference_markers_present() {
    // Minimal test case with inline references
    let source = r#"Welcome to *Lex* with references [Cache] and [^source].

Cache:
    A definition body referencing [Cache].
"#;

    let loader = DocumentLoader::from_string(source);
    let doc = loader.parse().expect("failed to parse");
    let tokens = collect_semantic_tokens(&doc);

    // Debug: print all tokens
    println!("\n=== All Semantic Tokens ===");
    for token in &tokens {
        let snippet = &source[token.range.span.clone()];
        println!(
            "{:40} | range: {:?} | text: {:?}",
            format!("{:?}", token.kind),
            token.range,
            snippet
        );
    }

    // Check for reference markers
    let ref_marker_start_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.kind == LexSemanticTokenKind::InlineMarkerRefStart)
        .collect();

    let ref_marker_end_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.kind == LexSemanticTokenKind::InlineMarkerRefEnd)
        .collect();

    println!("\n=== Reference Marker Stats ===");
    println!("RefMarkerStart tokens: {}", ref_marker_start_tokens.len());
    println!("RefMarkerEnd tokens: {}", ref_marker_end_tokens.len());

    for token in &ref_marker_start_tokens {
        let snippet = &source[token.range.span.clone()];
        println!("  Start marker: {:?} at {:?}", snippet, token.range);
    }

    for token in &ref_marker_end_tokens {
        let snippet = &source[token.range.span.clone()];
        println!("  End marker: {:?} at {:?}", snippet, token.range);
    }

    // We have 3 references: [Cache], [^source], [Cache]
    // So we should have 3 start markers and 3 end markers
    assert_eq!(
        ref_marker_start_tokens.len(),
        3,
        "Expected 3 '[' reference start markers"
    );
    assert_eq!(
        ref_marker_end_tokens.len(),
        3,
        "Expected 3 ']' reference end markers"
    );

    // Verify the markers are actually '[' and ']'
    for token in &ref_marker_start_tokens {
        let snippet = &source[token.range.span.clone()];
        assert_eq!(snippet, "[", "Start marker should be '['");
    }

    for token in &ref_marker_end_tokens {
        let snippet = &source[token.range.span.clone()];
        assert_eq!(snippet, "]", "End marker should be ']'");
    }
}

#[test]
fn test_other_inline_markers_work() {
    // Test that other inline markers work correctly for comparison
    let source = "*bold* _italic_ `code` #math#";

    let loader = DocumentLoader::from_string(source);
    let doc = loader.parse().expect("failed to parse");
    let tokens = collect_semantic_tokens(&doc);

    // Debug: print all tokens
    println!("\n=== Other Inline Markers ===");
    for token in &tokens {
        let snippet = &source[token.range.span.clone()];
        println!("{:40} | text: {:?}", format!("{:?}", token.kind), snippet);
    }

    // Check that other markers are present
    assert!(
        tokens
            .iter()
            .any(|t| t.kind == LexSemanticTokenKind::InlineMarkerStrongStart),
        "Should have strong start marker"
    );
    assert!(
        tokens
            .iter()
            .any(|t| t.kind == LexSemanticTokenKind::InlineMarkerStrongEnd),
        "Should have strong end marker"
    );
    assert!(
        tokens
            .iter()
            .any(|t| t.kind == LexSemanticTokenKind::InlineMarkerEmphasisStart),
        "Should have emphasis start marker"
    );
    assert!(
        tokens
            .iter()
            .any(|t| t.kind == LexSemanticTokenKind::InlineMarkerCodeStart),
        "Should have code start marker"
    );
}

#[test]
fn test_no_overlapping_reference_tokens() {
    // Test that reference markers don't overlap with reference content
    let source = "Reference [Cache] here";

    let loader = DocumentLoader::from_string(source);
    let doc = loader.parse().expect("failed to parse");
    let tokens = collect_semantic_tokens(&doc);

    println!("\n=== Token Ranges ===");
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
                    "❌ OVERLAP: {:?} [{:?}] {}..{} with {:?} [{:?}] {}..{}",
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

    assert!(
        !found_overlap,
        "Reference tokens should not overlap with each other"
    );
}
