/**
 * Monaco document symbol provider for Lex.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument, DocumentSymbol, SymbolKind } from '../index';
import type { DocumentProvider } from './semantic_tokens';

/**
 * Register a document symbol provider for Lex in Monaco.
 */
export function registerDocumentSymbolProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider
): monaco.IDisposable {
  return monacoInstance.languages.registerDocumentSymbolProvider(languageId, {
    provideDocumentSymbols(
      model: monaco.editor.ITextModel
    ): monaco.languages.ProviderResult<monaco.languages.DocumentSymbol[]> {
      const doc = documentProvider.getDocument(model.uri.toString());
      if (!doc) return [];

      const symbols = doc.documentSymbols();
      return symbols.map((s) => convertSymbol(monacoInstance, s));
    },
  });
}

function convertSymbol(
  monacoInstance: typeof monaco,
  sym: DocumentSymbol
): monaco.languages.DocumentSymbol {
  return {
    name: sym.name,
    detail: sym.detail || '',
    kind: convertSymbolKind(monacoInstance, sym.kind),
    tags: [],
    range: new monacoInstance.Range(
      sym.range.start.line + 1,
      sym.range.start.character + 1,
      sym.range.end.line + 1,
      sym.range.end.character + 1
    ),
    selectionRange: new monacoInstance.Range(
      sym.selectionRange.start.line + 1,
      sym.selectionRange.start.character + 1,
      sym.selectionRange.end.line + 1,
      sym.selectionRange.end.character + 1
    ),
    children: sym.children?.map((c) => convertSymbol(monacoInstance, c)) || [],
  };
}

function convertSymbolKind(
  monacoInstance: typeof monaco,
  kind: SymbolKind
): monaco.languages.SymbolKind {
  // Monaco and LSP symbol kinds mostly align
  return kind as monaco.languages.SymbolKind;
}
