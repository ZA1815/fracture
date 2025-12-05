export interface SyntaxToFssParams {
    source: string;
    syntaxStyle: string;
    uri?: string;
}

export interface SyntaxToFssResult {
    fss: string;
    success: boolean;
    error?: string;
    diagnostics?: Diagnostic[];
}

export interface FssToSyntaxParams {
    fss: string;
    syntaxStyle: string;
    uri?: string;
}

export interface FssToSyntaxResult {
    source: string;
    success: boolean;
    error?: string;
}

export interface GetAstParams {
    source: string;
    syntaxStyle: string;
}

export interface AstNode {
    kind: string;
    type?: string;
    name?: string;
    span?: Span;
    children?: AstNode[];
    [key: string]: unknown;
}

export interface Span {
    start: Position;
    end: Position;
}

export interface Position {
    line: number;
    column: number;
    offset: number;
}

export interface Diagnostic {
    severity: 'error' | 'warning' | 'info' | 'hint';
    message: string;
    span: Span;
    code?: string;
    suggestions?: string[];
    related?: RelatedInfo[];
}

export interface RelatedInfo {
    message: string;
    span: Span;
    uri?: string;
}

export interface FormatParams {
    source: string;
    syntaxStyle: string;
    options?: FormatOptions;
}

export interface FormatOptions {
    tabSize?: number;
    insertSpaces?: boolean;
    maxLineLength?: number;
}

export interface FormatResult {
    formatted: string;
    success: boolean;
    error?: string;
}

export interface HoverParams {
    source: string;
    syntaxStyle: string;
    position: Position;
}

export interface HoverResult {
    contents: string;
    range?: Span;
}

export interface FullCheckParams {
    files?: string[];
}

export interface FullCheckResult {
    diagnostics: { [uri: string]: Diagnostic[] };
    success: boolean;
    errorCount: number;
    warningCount: number;
}

export interface StatusNotification {
    status: 'ready' | 'checking' | 'building' | 'error';
    message?: string;
}

export interface DiagnosticsNotification {
    uri: string;
    diagnostics: Diagnostic[];
}

export interface FractureServerConfig {
    workspaceRoot: string;
    defaultSyntaxStyle: string;
    unsafeMode: boolean;
    maxErrors: number;
}

export const Methods = {
    SYNTAX_TO_FSS: 'fracture/syntaxToFss',
    FSS_TO_SYNTAX: 'fracture/fssToSyntax',
    GET_AST: 'fracture/getAst',
    FULL_CHECK: 'fracture/fullCheck',
    FORMAT: 'fracture/format',
    HOVER: 'fracture/hover',
    STATUS: 'fracture/status',
    CONFIGURE: 'fracture/configure',
    SEMANTIC_TOKENS: 'fracture/semanticTokens'
} as const;

export interface SemanticTokensParams {
    source: string;
    syntaxStyle: string;
}

export interface SemanticTokensResult {
    tokens: number[];
    success: boolean;
    error?: string;
}