/**
 * Monaco hover provider for Lex.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument } from '../index';
import type { DocumentProvider } from './semantic_tokens';

/**
 * Register a hover provider for Lex in Monaco.
 */
export function registerHoverProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider
): monaco.IDisposable {
  return monacoInstance.languages.registerHoverProvider(languageId, {
    provideHover(
      model: monaco.editor.ITextModel,
      position: monaco.Position
    ): monaco.languages.ProviderResult<monaco.languages.Hover> {
      const doc = documentProvider.getDocument(model.uri.toString());
      if (!doc) return null;

      // Monaco positions are 1-indexed, lex-wasm expects 0-indexed
      const hover = doc.hover(position.lineNumber - 1, position.column - 1);
      if (!hover) return null;

      return {
        contents: [
          {
            value: hover.contents.value,
            isTrusted: true,
          },
        ],
        range: hover.range
          ? new monacoInstance.Range(
              hover.range.start.line + 1,
              hover.range.start.character + 1,
              hover.range.end.line + 1,
              hover.range.end.character + 1
            )
          : undefined,
      };
    },
  });
}
