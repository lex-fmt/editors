use lex_core::lex::ast::Document;
use lex_core::lex::testing::lexplore::Lexplore;
use std::sync::OnceLock;

const SAMPLE_BENCHMARK: usize = 50;

struct SampleFixture {
    document: Document,
    source: String,
}

static SAMPLE_FIXTURE: OnceLock<SampleFixture> = OnceLock::new();

fn sample_fixture() -> &'static SampleFixture {
    SAMPLE_FIXTURE.get_or_init(|| {
        let loader = Lexplore::benchmark(SAMPLE_BENCHMARK);
        let document = loader
            .parse()
            .expect("failed to parse benchmark fixture for lex-lsp tests");
        let source = loader.source();
        SampleFixture { document, source }
    })
}

pub fn sample_document() -> Document {
    sample_fixture().document.clone()
}

pub fn sample_source() -> &'static str {
    sample_fixture().source.as_str()
}
