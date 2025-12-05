use lex_core::lex::ast::links::DocumentLink as AstDocumentLink;
use lex_core::lex::ast::Document;

pub fn collect_document_links(document: &Document) -> Vec<AstDocumentLink> {
    document.find_all_links()
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex_core::lex::parsing;

    #[test]
    fn collects_links() {
        let source = "Visit [https://example.com] and include [./data.csv].\n";
        let document = parsing::parse_document(source).expect("parse fixture");
        let links = collect_document_links(&document);
        assert_eq!(links.len(), 2);
    }
}
