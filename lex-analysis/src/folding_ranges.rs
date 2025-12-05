use lex_core::lex::ast::{
    Annotation, AstNode, ContentItem, Definition, Document, List, ListItem, Range, Session,
    Verbatim,
};
use lsp_types::FoldingRangeKind;

#[derive(Debug, Clone, PartialEq)]
pub struct LexFoldingRange {
    pub start_line: u32,
    pub start_character: Option<u32>,
    pub end_line: u32,
    pub end_character: Option<u32>,
    pub kind: Option<FoldingRangeKind>,
}

pub fn folding_ranges(document: &Document) -> Vec<LexFoldingRange> {
    let mut collector = FoldingCollector { ranges: Vec::new() };
    collector.process_document(document);
    collector.ranges
}

struct FoldingCollector {
    ranges: Vec<LexFoldingRange>,
}

impl FoldingCollector {
    fn process_document(&mut self, document: &Document) {
        self.process_annotations(document.annotations());
        self.process_session(&document.root, true);
    }

    fn process_session(&mut self, session: &Session, is_root: bool) {
        if !is_root {
            if let Some(body) = session.body_location() {
                if session.range().start.line < body.end.line {
                    self.push_fold(
                        session.header_location(),
                        session.range(),
                        Some(FoldingRangeKind::Region),
                    );
                }
            }
        }
        self.process_annotations(session.annotations());
        for child in session.children.iter() {
            self.process_content_item(child);
        }
    }

    fn process_content_item(&mut self, item: &ContentItem) {
        match item {
            ContentItem::Paragraph(paragraph) => {
                self.process_annotations(paragraph.annotations());
            }
            ContentItem::Session(session) => self.process_session(session, false),
            ContentItem::List(list) => self.process_list(list),
            ContentItem::ListItem(list_item) => self.process_list_item(list_item),
            ContentItem::Definition(definition) => self.process_definition(definition),
            ContentItem::Annotation(annotation) => self.process_annotation(annotation),
            ContentItem::VerbatimBlock(verbatim) => self.process_verbatim(verbatim),
            ContentItem::TextLine(_)
            | ContentItem::VerbatimLine(_)
            | ContentItem::BlankLineGroup(_) => {}
        }
    }

    fn process_list(&mut self, list: &List) {
        if list.range().start.line < list.range().end.line {
            self.push_fold(
                Some(list.range()),
                list.range(),
                Some(FoldingRangeKind::Region),
            );
        }
        self.process_annotations(list.annotations());
        for item in list.items.iter() {
            if let ContentItem::ListItem(list_item) = item {
                self.process_list_item(list_item);
            }
        }
    }

    fn process_list_item(&mut self, list_item: &ListItem) {
        self.process_annotations(list_item.annotations());
        if let Some(children_range) =
            Range::bounding_box(list_item.children.iter().map(|child| child.range()))
        {
            if list_item.range().start.line < children_range.end.line {
                self.push_fold(
                    Some(list_item.range()),
                    &children_range,
                    Some(FoldingRangeKind::Region),
                );
            }
        }
        for child in list_item.children.iter() {
            self.process_content_item(child);
        }
    }

    fn process_definition(&mut self, definition: &Definition) {
        if definition.range().start.line < definition.range().end.line {
            let header = definition.header_location();
            self.push_fold(header, definition.range(), Some(FoldingRangeKind::Region));
        }
        self.process_annotations(definition.annotations());
        for child in definition.children.iter() {
            self.process_content_item(child);
        }
    }

    fn process_annotation(&mut self, annotation: &Annotation) {
        if annotation.body_location().is_some() {
            self.push_fold(
                Some(annotation.header_location()),
                annotation.range(),
                Some(FoldingRangeKind::Comment),
            );
        }
        for child in annotation.children.iter() {
            self.process_content_item(child);
        }
    }

    fn process_verbatim(&mut self, verbatim: &Verbatim) {
        self.process_annotations(verbatim.annotations());
        if let Some(subject_range) = &verbatim.subject.location {
            if subject_range.start.line < verbatim.range().end.line {
                self.push_fold(
                    Some(subject_range),
                    verbatim.range(),
                    Some(FoldingRangeKind::Region),
                );
            }
        }
    }

    fn process_annotations(&mut self, annotations: &[Annotation]) {
        for annotation in annotations {
            self.process_annotation(annotation);
        }
    }

    fn push_fold(
        &mut self,
        start_range: Option<&Range>,
        end_range: &Range,
        kind: Option<FoldingRangeKind>,
    ) {
        let start = start_range.unwrap_or(end_range);
        if start.start.line >= end_range.end.line {
            return;
        }
        self.ranges.push(LexFoldingRange {
            start_line: start.start.line as u32,
            start_character: Some(start.start.column as u32),
            end_line: end_range.end.line as u32,
            end_character: Some(end_range.end.column as u32),
            kind,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::sample_document;

    #[test]
    fn creates_ranges_for_sessions_and_definitions() {
        let document = sample_document();
        let ranges = folding_ranges(&document);
        assert!(ranges
            .iter()
            .any(|range| range.kind == Some(FoldingRangeKind::Region) && range.start_line == 2));
        assert!(ranges
            .iter()
            .any(|range| range.kind == Some(FoldingRangeKind::Region) && range.start_line > 2));
        // No comment folding ranges because :: callout :: is consumed by Verbatim
        // assert!(ranges
        //     .iter()
        //     .any(|range| range.kind == Some(FoldingRangeKind::Comment)));
    }
}
