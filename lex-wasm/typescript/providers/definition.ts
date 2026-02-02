/**
 * Monaco go-to-definition provider for Lex.
 */

import type * as monaco from 'monaco-editor';
import type { LexDocument } from '../index';
import type { DocumentProvider } from './semantic_tokens';

/**
 * Register a definition provider for Lex in Monaco.
 */
export function registerDefinitionProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider
): monaco.IDisposable {
  return monacoInstance.languages.registerDefinitionProvider(languageId, {
    provideDefinition(
      model: monaco.editor.ITextModel,
      position: monaco.Position
    ): monaco.languages.ProviderResult<monaco.languages.Definition> {
      const doc = documentProvider.getDocument(model.uri.toString());
      if (!doc) return null;

      // Monaco positions are 1-indexed, lex-wasm expects 0-indexed
      const locations = doc.gotoDefinition(
        position.lineNumber - 1,
        position.column - 1
      );

      if (locations.length === 0) return null;

      return locations.map((loc) => ({
        uri: monacoInstance.Uri.parse(loc.uri),
        range: new monacoInstance.Range(
          loc.range.start.line + 1,
          loc.range.start.character + 1,
          loc.range.end.line + 1,
          loc.range.end.character + 1
        ),
      }));
    },
  });
}

/**
 * Register a references provider for Lex in Monaco.
 */
export function registerReferencesProvider(
  monacoInstance: typeof monaco,
  languageId: string,
  documentProvider: DocumentProvider
): monaco.IDisposable {
  return monacoInstance.languages.registerReferenceProvider(languageId, {
    provideReferences(
      model: monaco.editor.ITextModel,
      position: monaco.Position,
      context: monaco.languages.ReferenceContext
    ): monaco.languages.ProviderResult<monaco.languages.Location[]> {
      const doc = documentProvider.getDocument(model.uri.toString());
      if (!doc) return null;

      // Monaco positions are 1-indexed, lex-wasm expects 0-indexed
      const locations = doc.references(
        position.lineNumber - 1,
        position.column - 1,
        context.includeDeclaration
      );

      return locations.map((loc) => ({
        uri: monacoInstance.Uri.parse(loc.uri),
        range: new monacoInstance.Range(
          loc.range.start.line + 1,
          loc.range.start.character + 1,
          loc.range.end.line + 1,
          loc.range.end.character + 1
        ),
      }));
    },
  });
}
