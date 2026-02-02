/**
 * Monaco diagnostics integration for Lex.
 *
 * Note: Monaco doesn't have a "diagnostics provider" like LSP.
 * Instead, diagnostics are set via `monaco.editor.setModelMarkers`.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument, EmbeddedSpellchecker, DiagnosticSeverity } from '../index';

/**
 * Update diagnostics for a model.
 *
 * Call this whenever the document content changes.
 */
export function updateDiagnostics(
  monacoInstance: typeof monaco,
  model: monaco.editor.ITextModel,
  doc: LexDocument,
  spellchecker?: EmbeddedSpellchecker
): void {
  const markers: monaco.editor.IMarkerData[] = [];

  // Get structural diagnostics
  const diagnostics = doc.diagnostics();
  for (const diag of diagnostics) {
    markers.push({
      severity: convertSeverity(monacoInstance, diag.severity),
      message: diag.message,
      startLineNumber: diag.range.start.line + 1,
      startColumn: diag.range.start.character + 1,
      endLineNumber: diag.range.end.line + 1,
      endColumn: diag.range.end.character + 1,
      source: diag.source || 'lex',
    });
  }

  // Get spellcheck diagnostics if checker is provided
  if (spellchecker) {
    const spellDiags = doc.spellcheckDiagnostics(spellchecker);
    for (const diag of spellDiags) {
      markers.push({
        severity: monacoInstance.MarkerSeverity.Info,
        message: diag.message,
        startLineNumber: diag.range.start.line + 1,
        startColumn: diag.range.start.character + 1,
        endLineNumber: diag.range.end.line + 1,
        endColumn: diag.range.end.character + 1,
        source: 'lex-spell',
        code: 'spelling',
      });
    }
  }

  monacoInstance.editor.setModelMarkers(model, 'lex', markers);
}

function convertSeverity(
  monacoInstance: typeof monaco,
  severity?: DiagnosticSeverity
): monaco.MarkerSeverity {
  switch (severity) {
    case 1:
      return monacoInstance.MarkerSeverity.Error;
    case 2:
      return monacoInstance.MarkerSeverity.Warning;
    case 3:
      return monacoInstance.MarkerSeverity.Info;
    case 4:
      return monacoInstance.MarkerSeverity.Hint;
    default:
      return monacoInstance.MarkerSeverity.Warning;
  }
}
