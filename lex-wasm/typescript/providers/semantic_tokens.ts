/**
 * Monaco semantic tokens provider for Lex.
 *
 * This provider integrates with Monaco Editor to provide semantic
 * highlighting using lex-wasm.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument, LexAnalyzer, SemanticTokensLegend } from '../index';

/**
 * A document provider that caches parsed documents.
 */
export interface DocumentProvider {
  /** Get or create a parsed document for a URI. */
  getDocument(uri: string): LexDocument | null;
}

/**
 * Register a semantic tokens provider for Lex in Monaco.
 *
 * @param monacoInstance - The Monaco editor instance
 * @param languageId - The language ID registered for Lex (e.g., "lex")
 * @param documentProvider - Function to get/create documents
 * @param analyzer - The LexAnalyzer instance
 */
export function registerSemanticTokensProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider,
  analyzer: LexAnalyzer
): monaco.IDisposable {
  const legend = analyzer.semanticTokenLegend();

  return monacoInstance.languages.registerDocumentSemanticTokensProvider(
    languageId,
    {
      getLegend(): monaco.languages.SemanticTokensLegend {
        return {
          tokenTypes: legend.tokenTypes,
          tokenModifiers: legend.tokenModifiers,
        };
      },

      provideDocumentSemanticTokens(
        model: monaco.editor.ITextModel
      ): monaco.languages.ProviderResult<monaco.languages.SemanticTokens> {
        const doc = documentProvider.getDocument(model.uri.toString());
        if (!doc) {
          return { data: new Uint32Array() };
        }

        const tokens = doc.semanticTokens();
        return {
          data: new Uint32Array(tokens),
        };
      },

      releaseDocumentSemanticTokens(): void {
        // No-op: documents are managed by the provider
      },
    }
  );
}
