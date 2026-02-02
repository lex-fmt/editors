/**
 * Monaco folding range provider for Lex.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument } from '../index';
import type { DocumentProvider } from './semantic_tokens';

/**
 * Register a folding range provider for Lex in Monaco.
 */
export function registerFoldingRangeProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider
): monaco.IDisposable {
  return monacoInstance.languages.registerFoldingRangeProvider(languageId, {
    provideFoldingRanges(
      model: monaco.editor.ITextModel
    ): monaco.languages.ProviderResult<monaco.languages.FoldingRange[]> {
      const doc = documentProvider.getDocument(model.uri.toString());
      if (!doc) return [];

      const ranges = doc.foldingRanges();

      return ranges.map((r) => ({
        start: r.startLine + 1, // Monaco is 1-indexed
        end: r.endLine + 1,
        kind: convertFoldingKind(monacoInstance, r.kind),
      }));
    },
  });
}

function convertFoldingKind(
  monacoInstance: typeof monaco,
  kind?: string
): monaco.languages.FoldingRangeKind | undefined {
  switch (kind) {
    case 'comment':
      return monacoInstance.languages.FoldingRangeKind.Comment;
    case 'imports':
      return monacoInstance.languages.FoldingRangeKind.Imports;
    case 'region':
      return monacoInstance.languages.FoldingRangeKind.Region;
    default:
      return undefined;
  }
}
