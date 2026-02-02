/**
 * TypeScript type definitions for lex-wasm.
 *
 * These types mirror the LSP protocol types for compatibility with
 * Monaco Editor and other LSP-based editors.
 */

export interface Position {
  line: number;
  character: number;
}

export interface Range {
  start: Position;
  end: Position;
}

export interface Location {
  uri: string;
  range: Range;
}

export interface Diagnostic {
  range: Range;
  severity?: DiagnosticSeverity;
  code?: string | number;
  source?: string;
  message: string;
}

export enum DiagnosticSeverity {
  Error = 1,
  Warning = 2,
  Information = 3,
  Hint = 4,
}

export interface DocumentSymbol {
  name: string;
  detail?: string;
  kind: SymbolKind;
  range: Range;
  selectionRange: Range;
  children?: DocumentSymbol[];
}

export enum SymbolKind {
  File = 1,
  Module = 2,
  Namespace = 3,
  Package = 4,
  Class = 5,
  Method = 6,
  Property = 7,
  Field = 8,
  Constructor = 9,
  Enum = 10,
  Interface = 11,
  Function = 12,
  Variable = 13,
  Constant = 14,
  String = 15,
  Number = 16,
  Boolean = 17,
  Array = 18,
  Object = 19,
  Key = 20,
  Null = 21,
  EnumMember = 22,
  Struct = 23,
  Event = 24,
  Operator = 25,
  TypeParameter = 26,
}

export interface Hover {
  contents: MarkupContent;
  range?: Range;
}

export interface MarkupContent {
  kind: 'plaintext' | 'markdown';
  value: string;
}

export interface FoldingRange {
  startLine: number;
  startCharacter?: number;
  endLine: number;
  endCharacter?: number;
  kind?: FoldingRangeKind;
}

export enum FoldingRangeKind {
  Comment = 'comment',
  Imports = 'imports',
  Region = 'region',
}

export interface CompletionItem {
  label: string;
  kind?: CompletionItemKind;
  detail?: string;
  insertText?: string;
}

export enum CompletionItemKind {
  Text = 1,
  Method = 2,
  Function = 3,
  Constructor = 4,
  Field = 5,
  Variable = 6,
  Class = 7,
  Interface = 8,
  Module = 9,
  Property = 10,
  Unit = 11,
  Value = 12,
  Enum = 13,
  Keyword = 14,
  Snippet = 15,
  Color = 16,
  File = 17,
  Reference = 18,
  Folder = 19,
  EnumMember = 20,
  Constant = 21,
  Struct = 22,
  Event = 23,
  Operator = 24,
  TypeParameter = 25,
}

export interface SemanticTokensLegend {
  tokenTypes: string[];
  tokenModifiers: string[];
}

/**
 * Semantic tokens data as a flat array.
 * Format: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers, ...]
 */
export type SemanticTokensData = number[];
