/**
 * lex-wasm TypeScript bindings.
 *
 * This module provides a TypeScript wrapper around the WASM module,
 * making it easy to integrate with Monaco Editor and other browsers-based editors.
 *
 * @example
 * ```typescript
 * import { createLexAnalyzer } from '@lex-fmt/lex-wasm';
 *
 * const analyzer = await createLexAnalyzer();
 * const doc = analyzer.parse("1. Introduction\n\nHello world");
 * const tokens = doc.semanticTokens();
 * ```
 */

import type {
  CompletionItem,
  Diagnostic,
  DocumentSymbol,
  FoldingRange,
  Hover,
  Location,
  Position,
  SemanticTokensData,
  SemanticTokensLegend,
} from './types';

// Re-export types
export * from './types';

// The WASM module will be imported dynamically
let wasmModule: typeof import('../pkg/lex_wasm') | null = null;

/**
 * Initialize the WASM module.
 * Must be called before using any other functions.
 */
export async function init(): Promise<void> {
  if (wasmModule) return;

  // Dynamic import of the WASM module
  wasmModule = await import('../pkg/lex_wasm');
}

/**
 * A parsed Lex document with analysis capabilities.
 */
export interface LexDocument {
  /** Get the document source text. */
  source(): string;

  /** Get the document URI. */
  uri(): string;

  /** Get semantic tokens for syntax highlighting. */
  semanticTokens(): SemanticTokensData;

  /** Get document symbols (outline). */
  documentSymbols(): DocumentSymbol[];

  /** Get hover information at a position. */
  hover(line: number, character: number): Hover | null;

  /** Get go-to-definition locations. */
  gotoDefinition(line: number, character: number): Location[];

  /** Find all references to the symbol at a position. */
  references(
    line: number,
    character: number,
    includeDeclaration: boolean
  ): Location[];

  /** Get folding ranges. */
  foldingRanges(): FoldingRange[];

  /** Get completion suggestions. */
  completion(line: number, character: number): CompletionItem[];

  /** Get diagnostics (errors, warnings). */
  diagnostics(): Diagnostic[];

  /** Get spellcheck diagnostics using the provided checker. */
  spellcheckDiagnostics(checker: EmbeddedSpellchecker): Diagnostic[];

  /** Format the document source. */
  format(): string;

  /** Export the document as HTML. */
  toHtml(): string;
}

/**
 * Spellchecker using an embedded en_US dictionary.
 */
export interface EmbeddedSpellchecker {
  /** Check if a word is spelled correctly. */
  check(word: string): boolean;

  /** Get spelling suggestions for a word. */
  suggest(word: string): string[];

  /** Add a word to the custom dictionary (session-only). */
  addCustomWord(word: string): void;

  /** Get all custom words. */
  getCustomWords(): string[];

  /** Load custom words (e.g., from localStorage). */
  loadCustomWords(words: string[]): void;
}

/**
 * The main Lex analyzer interface.
 */
export interface LexAnalyzer {
  /** Parse source text into a LexDocument. */
  parse(source: string): LexDocument;

  /** Parse source text with a specific URI. */
  parseWithUri(source: string, uri: string): LexDocument;

  /** Get the semantic token legend for Monaco. */
  semanticTokenLegend(): SemanticTokensLegend;

  /** Create a new spellchecker with the embedded dictionary. */
  createSpellchecker(): EmbeddedSpellchecker;

  /** Get the version of lex-wasm. */
  version(): string;
}

/**
 * Create a LexAnalyzer instance.
 *
 * This initializes the WASM module if needed and returns an analyzer
 * that can parse documents and provide LSP-like features.
 */
export async function createLexAnalyzer(): Promise<LexAnalyzer> {
  await init();

  if (!wasmModule) {
    throw new Error('Failed to initialize WASM module');
  }

  const { LexDocument, EmbeddedSpellchecker, version } = wasmModule;

  return {
    parse(source: string): LexDocument {
      return new LexDocument(source) as unknown as LexDocument;
    },

    parseWithUri(source: string, uri: string): LexDocument {
      return LexDocument.withUri(source, uri) as unknown as LexDocument;
    },

    semanticTokenLegend(): SemanticTokensLegend {
      return LexDocument.semanticTokenLegend() as SemanticTokensLegend;
    },

    createSpellchecker(): EmbeddedSpellchecker {
      return new EmbeddedSpellchecker() as unknown as EmbeddedSpellchecker;
    },

    version(): string {
      return version();
    },
  };
}
