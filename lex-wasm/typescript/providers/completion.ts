/**
 * Monaco completion provider for Lex.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument, CompletionItemKind } from '../index';
import type { DocumentProvider } from './semantic_tokens';

/**
 * Register a completion provider for Lex in Monaco.
 */
export function registerCompletionProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider
): monaco.IDisposable {
  return monacoInstance.languages.registerCompletionItemProvider(languageId, {
    triggerCharacters: ['[', '@', ':'],

    provideCompletionItems(
      model: monaco.editor.ITextModel,
      position: monaco.Position
    ): monaco.languages.ProviderResult<monaco.languages.CompletionList> {
      const doc = documentProvider.getDocument(model.uri.toString());
      if (!doc) return { suggestions: [] };

      // Monaco positions are 1-indexed, lex-wasm expects 0-indexed
      const items = doc.completion(position.lineNumber - 1, position.column - 1);

      const suggestions: monaco.languages.CompletionItem[] = items.map(
        (item, index) => ({
          label: item.label,
          kind: convertCompletionKind(monacoInstance, item.kind),
          detail: item.detail,
          insertText: item.insertText || item.label,
          range: new monacoInstance.Range(
            position.lineNumber,
            position.column,
            position.lineNumber,
            position.column
          ),
          sortText: String(index).padStart(5, '0'),
        })
      );

      return { suggestions };
    },
  });
}

function convertCompletionKind(
  monacoInstance: typeof monaco,
  kind?: CompletionItemKind
): monaco.languages.CompletionItemKind {
  if (!kind) return monacoInstance.languages.CompletionItemKind.Text;

  // Map LSP completion kinds to Monaco
  const map: Record<number, monaco.languages.CompletionItemKind> = {
    1: monacoInstance.languages.CompletionItemKind.Text,
    2: monacoInstance.languages.CompletionItemKind.Method,
    3: monacoInstance.languages.CompletionItemKind.Function,
    6: monacoInstance.languages.CompletionItemKind.Variable,
    14: monacoInstance.languages.CompletionItemKind.Keyword,
    15: monacoInstance.languages.CompletionItemKind.Snippet,
    17: monacoInstance.languages.CompletionItemKind.File,
    18: monacoInstance.languages.CompletionItemKind.Reference,
    19: monacoInstance.languages.CompletionItemKind.Folder,
  };

  return map[kind] || monacoInstance.languages.CompletionItemKind.Text;
}
