use crate::utils::session_identifier;
use lex_core::lex::ast::{Annotation, Definition, Session};
use lex_core::lex::inlines::ReferenceType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReferenceTarget {
    AnnotationLabel(String),
    DefinitionSubject(String),
    Session(String),
    CitationKey(String),
}

pub fn targets_from_reference_type(reference_type: &ReferenceType) -> Vec<ReferenceTarget> {
    let mut targets = Vec::new();
    match reference_type {
        ReferenceType::FootnoteLabeled { label } => {
            push_unique(
                &mut targets,
                ReferenceTarget::AnnotationLabel(label.clone()),
            );
        }
        ReferenceType::FootnoteNumber { number } => {
            push_unique(
                &mut targets,
                ReferenceTarget::AnnotationLabel(number.to_string()),
            );
        }
        ReferenceType::Citation(data) => {
            for key in &data.keys {
                push_unique(&mut targets, ReferenceTarget::CitationKey(key.clone()));
            }
        }
        ReferenceType::General { target } => {
            if !target.trim().is_empty() {
                push_unique(
                    &mut targets,
                    ReferenceTarget::DefinitionSubject(target.trim().to_string()),
                );
            }
        }
        ReferenceType::ToCome {
            identifier: Some(id),
        } if !id.trim().is_empty() => {
            push_unique(
                &mut targets,
                ReferenceTarget::DefinitionSubject(id.trim().to_string()),
            );
        }
        ReferenceType::ToCome { .. } => {}
        ReferenceType::Session { target } => {
            if !target.trim().is_empty() {
                push_unique(
                    &mut targets,
                    ReferenceTarget::Session(target.trim().to_string()),
                );
            }
        }
        _ => {}
    }
    targets
}

pub fn targets_from_annotation(annotation: &Annotation) -> Vec<ReferenceTarget> {
    let mut targets = Vec::new();
    let label = annotation.data.label.value.trim();
    if !label.is_empty() {
        push_unique(
            &mut targets,
            ReferenceTarget::AnnotationLabel(label.to_string()),
        );
        push_unique(
            &mut targets,
            ReferenceTarget::CitationKey(label.to_string()),
        );
    }
    targets
}

pub fn targets_from_definition(definition: &Definition) -> Vec<ReferenceTarget> {
    let mut targets = Vec::new();
    let subject = definition.subject.as_string().trim();
    if !subject.is_empty() {
        push_unique(
            &mut targets,
            ReferenceTarget::DefinitionSubject(subject.to_string()),
        );
    }
    targets
}

pub fn targets_from_session(session: &Session) -> Vec<ReferenceTarget> {
    let mut targets = Vec::new();
    if let Some(identifier) = session_identifier(session) {
        push_unique(&mut targets, ReferenceTarget::Session(identifier));
    }
    targets
}

fn push_unique(targets: &mut Vec<ReferenceTarget>, target: ReferenceTarget) {
    if targets.iter().any(|existing| existing == &target) {
        return;
    }
    targets.push(target);
}
