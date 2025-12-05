import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Executable
} from 'vscode-languageclient/node';
import {
    Methods,
    SyntaxToFssParams,
    SyntaxToFssResult,
    FssToSyntaxParams,
    FssToSyntaxResult,
    GetAstParams,
    AstNode,
    FullCheckResult,
    Diagnostic,
    FormatParams,
    FormatResult,
    HoverParams,
    HoverResult,
    SemanticTokensParams,
    SemanticTokensResult
} from './protocol';

export class FractureLspClient {
    private client: LanguageClient | undefined;
    private outputChannel: vscode.OutputChannel;
    private useMock: boolean = false;
    private mock: MockLspImplementation;

    constructor(private context: vscode.ExtensionContext) {
        this.outputChannel = vscode.window.createOutputChannel('Fracture Language Server');
        this.mock = new MockLspImplementation();
    }

    async start(): Promise<void> {
        const compilerPath = vscode.workspace.getConfiguration('fracture').get<string>('compiler.path', 'rift');

        this.outputChannel.appendLine(`Starting Fracture LSP with ${compilerPath} --lsp`);

        try {
            const serverExecutable: Executable = {
                command: compilerPath,
                args: ['--lsp']
            };
            
            const serverOptions: ServerOptions = {
                run: serverExecutable,
                debug: serverExecutable
            };
            
            const clientOptions: LanguageClientOptions = {
                documentSelector: [{ scheme: 'file', language: 'fracture' }],
                synchronize: {
                    configurationSection: 'fracture',
                    fileEvents: vscode.workspace.createFileSystemWatcher('**/*.frac')
                },
                outputChannel: this.outputChannel
            };
            
            this.client = new LanguageClient(
                'fracture-lsp',
                'Fracture Language Server',
                serverOptions,
                clientOptions
            );
            
            await this.client.start();
            this.useMock = false;
            this.outputChannel.appendLine('Fracture LSP started successfully');
            
        }
        catch (e) {
            this.outputChannel.appendLine(`Failed to start LSP (${compilerPath}): ${e}`);
            this.outputChannel.appendLine('Note: LSP mode (--lsp flag) may not be implemented in the compiler yet.');
            this.outputChannel.appendLine('Using mock LSP implementation for development/testing.');
            this.useMock = true;
            
            vscode.window.showInformationMessage(
                `Fracture: Using mock LSP (compiler LSP mode not available). ` +
                'The mock LSP provides basic functionality for development.',
                'Dismiss'
            );
        }
    }

    async stop(): Promise<void> {
        if (this.client) {
            await this.client.stop();
            this.client = undefined;
        }
        this.outputChannel.appendLine('Fracture LSP stopped');
    }

    isRunning(): boolean {
        return this.useMock || (this.client !== undefined && this.client.isRunning());
    }

    async syntaxToFss(source: string, syntaxStyle: string): Promise<string> {
        if (this.useMock) {
            return this.mock.syntaxToFss(source, syntaxStyle);
        }
        
        if (!this.client) throw new Error('LSP not running');
        
        const params: SyntaxToFssParams = { source, syntaxStyle };
        const result = await this.client.sendRequest<SyntaxToFssResult>(
            Methods.SYNTAX_TO_FSS, params
        );
        
        if (!result.success) {
            throw new Error(result.error || 'Failed to convert syntax to FSS');
        }
        return result.fss;
    }

    async fssToSyntax(fss: string, syntaxStyle: string): Promise<string> {
        if (this.useMock) {
            return this.mock.fssToSyntax(fss, syntaxStyle);
        }
        
        if (!this.client) throw new Error('LSP not running');
        
        const params: FssToSyntaxParams = { fss, syntaxStyle };
        const result = await this.client.sendRequest<FssToSyntaxResult>(
            Methods.FSS_TO_SYNTAX, params
        );
        
        if (!result.success) {
            throw new Error(result.error || 'Failed to convert FSS to syntax');
        }
        return result.source;
    }

    async getAst(source: string, syntaxStyle: string): Promise<AstNode> {
        if (this.useMock) {
            return this.mock.getAst(source, syntaxStyle);
        }
        
        if (!this.client) throw new Error('LSP not running');
        
        const params: GetAstParams = { source, syntaxStyle };
        return await this.client.sendRequest<AstNode>(Methods.GET_AST, params);
    }
    
    async getDiagnostics(source: string, syntaxStyle: string): Promise<Diagnostic[]> {
        if (this.useMock) {
            return this.mock.getDiagnostics(source, syntaxStyle);
        }
        
        if (!this.client) return [];
        
        try {
            const params: SyntaxToFssParams = { source, syntaxStyle };
            const result = await this.client.sendRequest<SyntaxToFssResult>(
                Methods.SYNTAX_TO_FSS, params
            );

            return result.diagnostics || [];
        }
        catch {
            return [];
        }
    }

    async requestFullCheck(): Promise<FullCheckResult> {
        if (this.useMock) {
            return this.mock.fullCheck();
        }
        
        if (!this.client) throw new Error('LSP not running');
        return await this.client.sendRequest<FullCheckResult>(Methods.FULL_CHECK, {});
    }

    async formatCode(source: string, syntaxStyle: string): Promise<string> {
        if (this.useMock) {
            return this.mock.formatCode(source, syntaxStyle);
        }
        
        if (!this.client) throw new Error('LSP not running');
        
        const params: FormatParams = { source, syntaxStyle };
        const result = await this.client.sendRequest<FormatResult>(Methods.FORMAT, params);
        
        if (!result.success) {
            throw new Error(result.error || 'Failed to format code');
        }
        return result.formatted;
    }

    async getHover(source: string, syntaxStyle: string, line: number, column: number): Promise<HoverResult | null> {
        if (this.useMock) {
            return this.mock.getHover(source, syntaxStyle, line, column);
        }
        
        if (!this.client) return null;
        
        const params: HoverParams = {
            source,
            syntaxStyle,
            position: { line, column, offset: 0 }
        };
        
        return await this.client.sendRequest<HoverResult>(Methods.HOVER, params);
    }

    async getSemanticTokens(source: string, syntaxStyle: string): Promise<number[]> {
        if (this.useMock) {
            return this.mock.getSemanticTokens(source, syntaxStyle);
        }
        
        if (!this.client) return [];
        
        const params: SemanticTokensParams = { source, syntaxStyle };
        const result = await this.client.sendRequest<SemanticTokensResult>(Methods.SEMANTIC_TOKENS, params);
        
        if (!result.success) {
            console.error('[Fracture] Failed to get semantic tokens:', result.error);
            return [];
        }
        return result.tokens;
    }
}

// Mock LSP client to test before linking to real compiler
class MockLspImplementation {
    syntaxToFss(source: string, syntaxStyle: string): string {
        // FSS is the canonical form - if already FSS, return as-is
        if (syntaxStyle === 'fss') {
            return source;
        }
        
        // For other syntaxes, convert to FSS format
        const lines: string[] = [
            '// FSS translation from ' + syntaxStyle + ' syntax',
            ''
        ];
        
        const sourceLines = source.split('\n');
        
        for (const line of sourceLines) {
            const trimmed = line.trim();
            
            // Pass through comments
            if (trimmed.startsWith('//')) {
                lines.push(line);
                continue;
            }
            if (trimmed.startsWith('#')) {
                lines.push(line.replace(/^(\s*)#/, '$1//'));
                continue;
            }
            if (!trimmed) {
                lines.push('');
                continue;
            }
            
            // Convert Python def to FSS fn
            if (syntaxStyle === 'python' && trimmed.startsWith('def ')) {
                const match = trimmed.match(/def\s+(\w+)\s*\(([^)]*)\)(?:\s*->\s*(\w+))?:/);
                if (match) {
                    const name = match[1];
                    const params = match[2];
                    const returnType = match[3] || 'void';
                    const indent = line.match(/^(\s*)/)?.[1] || '';
                    lines.push(`${indent}fn ${name}(${params}) -> ${returnType} {`);
                    continue;
                }
            }
            
            // Convert Python variable assignment to FSS let
            if (syntaxStyle === 'python' && trimmed.match(/^\w+\s*=\s*.+$/)) {
                const match = trimmed.match(/^(\w+)\s*=\s*(.+)$/);
                if (match) {
                    const indent = line.match(/^(\s*)/)?.[1] || '';
                    lines.push(`${indent}let ${match[1]} = ${match[2]};`);
                    continue;
                }
            }
            
            // Pass through FSS-like syntax
            lines.push(line);
        }
        
        return lines.join('\n');
    }
    
    fssToSyntax(fss: string, syntaxStyle: string): string {
        const lines: string[] = [];
        const fssLines = fss.split('\n');
        
        let currentFunc = '';
        let params: string[] = [];
        let retType = '';
        let indent = '';
        
        for (const line of fssLines) {
            const trimmed = line.trim();
            
            if (!trimmed || trimmed.startsWith(';')) {
                if (trimmed.startsWith(';')) {
                    const comment = trimmed.substring(1).trim();
                    if (comment) {
                        lines.push(indent + (syntaxStyle === 'python' ? '# ' : '// ') + comment);
                    }
                }
                continue;
            }
            
            if (trimmed.startsWith('FUNC ')) {
                currentFunc = trimmed.substring(5);
                params = [];
                retType = '';
                indent = '';
            }
            
            else if (trimmed.startsWith('PARAM ')) {
                const param = trimmed.substring(6);
                params.push(param);
            }
            
            else if (trimmed.startsWith('RET ') && !trimmed.includes(',')) {
                retType = trimmed.substring(4);
            }
            
            else if (trimmed === 'BODY') {
                if (syntaxStyle === 'python') {
                    const paramStr = params.map(p => {
                        const [name, type] = p.split(':').map(s => s.trim());
                        return type ? name + ': ' + type : name;
                    }).join(', ');
                    lines.push('def ' + currentFunc + '(' + paramStr + '):');
                    indent = '    ';
                }
                else {
                    const paramStr = params.map(p => {
                        const [name, type] = p.split(':').map(s => s.trim());
                        return name + ': ' + (type || 'auto');
                    }).join(', ');
                    const retStr = retType ? ' -> ' + retType : '';
                    lines.push('fn ' + currentFunc + '(' + paramStr + ')' + retStr + ' {');
                    indent = '    ';
                }
            }
            
            else if (trimmed.startsWith('LOCAL ')) {
                const varDecl = trimmed.substring(6);
                if (syntaxStyle === 'python') {
                } else {
                    const [name, type] = varDecl.split(':').map(s => s.trim());
                    lines.push(indent + 'let ' + name + (type ? ': ' + type : '') + ';');
                }
            }
            
            else if (trimmed.startsWith('STORE ')) {
                const parts = trimmed.substring(6).split(',').map(s => s.trim());
                if (parts.length >= 2) {
                    if (syntaxStyle === 'python') {
                        lines.push(indent + parts[0] + ' = ' + parts[1]);
                    } else {
                        lines.push(indent + parts[0] + ' = ' + parts[1] + ';');
                    }
                }
            }
            
            else if (trimmed.startsWith('CALL ')) {
                const funcName = trimmed.substring(5);
                if (funcName === 'print') {
                    if (syntaxStyle === 'python') {
                        lines.push(indent + 'print(...)');
                    } else {
                        lines.push(indent + 'println!(...);');
                    }
                } else {
                    if (syntaxStyle === 'python') {
                        lines.push(indent + funcName + '()');
                    } else {
                        lines.push(indent + funcName + '();');
                    }
                }
            }
            
            else if (trimmed.startsWith('RET ')) {
                const value = trimmed.substring(4);
                if (syntaxStyle === 'python') {
                    lines.push(indent + 'return ' + value);
                } else {
                    lines.push(indent + 'return ' + value + ';');
                }
            }
            
            else if (trimmed === 'END') {
                if (syntaxStyle === 'fss') {
                    lines.push('}');
                }
                lines.push('');
                indent = '';
            }
        }
        
        return lines.join('\n');
    }
    
    getAst(source: string, syntaxStyle: string): AstNode {
        const children: AstNode[] = [];
        const lines = source.split('\n');
        
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim();
            
            if (syntaxStyle === 'fss' && line.startsWith('fn ')) {
                const match = line.match(/fn\s+(\w+)/);
                if (match) {
                    children.push({
                        kind: 'FunctionDef',
                        name: match[1],
                        type: '() -> ()',
                        span: { start: { line: i + 1, column: 1, offset: 0 }, end: { line: i + 1, column: line.length, offset: 0 } },
                        children: []
                    });
                }
            } else if (syntaxStyle === 'python' && line.startsWith('def ')) {
                const match = line.match(/def\s+(\w+)/);
                if (match) {
                    children.push({
                        kind: 'FunctionDef',
                        name: match[1],
                        type: '() -> None',
                        span: { start: { line: i + 1, column: 1, offset: 0 }, end: { line: i + 1, column: line.length, offset: 0 } },
                        children: []
                    });
                }
            } else if (line.startsWith('let ') || line.match(/^\w+\s*=/)) {
                const match = line.match(/(?:let\s+)?(\w+)/);
                if (match) {
                    children.push({
                        kind: 'LetStmt',
                        name: match[1],
                        span: { start: { line: i + 1, column: 1, offset: 0 }, end: { line: i + 1, column: line.length, offset: 0 } }
                    });
                }
            }
        }
        
        return {
            kind: 'Module',
            name: 'main',
            children
        };
    }
    
    getDiagnostics(source: string, syntaxStyle: string): Diagnostic[] {
        const diagnostics: Diagnostic[] = [];
        const lines = source.split('\n');
        
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            
            if (line.includes('_unused')) {
                diagnostics.push({
                    severity: 'warning',
                    message: 'Unused variable',
                    span: {
                        start: { line: i + 1, column: 1, offset: 0 },
                        end: { line: i + 1, column: line.length + 1, offset: 0 }
                    },
                    code: 'W001',
                    suggestions: ['Remove the variable or use it']
                });
            }
            
            if (line.includes('TODO')) {
                diagnostics.push({
                    severity: 'info',
                    message: 'TODO comment found',
                    span: {
                        start: { line: i + 1, column: line.indexOf('TODO') + 1, offset: 0 },
                        end: { line: i + 1, column: line.length + 1, offset: 0 }
                    }
                });
            }
        }
        
        return diagnostics;
    }
    
    fullCheck(): FullCheckResult {
        return {
            diagnostics: {},
            success: true,
            errorCount: 0,
            warningCount: 0
        };
    }
    
    formatCode(source: string, syntaxStyle: string): string {
        const lines = source.split('\n');
        const formatted: string[] = [];
        let indentLevel = 0;
        const indentStr = syntaxStyle === 'python' ? '    ' : '    ';
        
        for (const line of lines) {
            const trimmed = line.trim();
            
            if (trimmed.startsWith('}') || trimmed.startsWith(']') || trimmed.startsWith(')')) {
                indentLevel = Math.max(0, indentLevel - 1);
            }
            
            if (trimmed) {
                formatted.push(indentStr.repeat(indentLevel) + trimmed);
            } else {
                formatted.push('');
            }
            
            if (trimmed.endsWith('{') || trimmed.endsWith('[') || 
                (syntaxStyle === 'python' && trimmed.endsWith(':'))) {
                indentLevel++;
            }
        }
        
        return formatted.join('\n');
    }
    
    getHover(source: string, syntaxStyle: string, line: number, column: number): HoverResult | null {
        const lines = source.split('\n');
        if (line > lines.length) return null;
        
        const currentLine = lines[line - 1];
        
        const fnMatch = currentLine.match(/(?:fn|def)\s+(\w+)/);
        if (fnMatch) {
            return {
                contents: `**${fnMatch[1]}**\n\nFunction defined at line ${line}`,
                range: {
                    start: { line, column: currentLine.indexOf(fnMatch[1]) + 1, offset: 0 },
                    end: { line, column: currentLine.indexOf(fnMatch[1]) + fnMatch[1].length + 1, offset: 0 }
                }
            };
        }
        
        return null;
    }

    getSemanticTokens(source: string, syntaxStyle: string): number[] {
        const tokens: number[] = [];
        const lines = source.split('\n');
        
        const TOKEN_TYPES = {
            keyword: 0,
            variable: 1,
            string: 2,
            number: 3,
            function: 4,
            struct: 5,
            type: 6,
            operator: 7,
            comment: 8,
            parameter: 9,
            property: 10,
            enumMember: 11,
            decorator: 12,
            macro: 13
        };

        const KEYWORDS = new Set([
            'fn', 'return', 'if', 'else', 'while', 'for', 'let', 'mut', 
            'struct', 'mod', 'use', 'pub', 'as', 'self', 'super', 
            'match', 'panic', 'break', 'continue'
        ]);

        const CONSTANTS = new Set([
            'true', 'false', 'Some', 'None', 'Ok', 'Err'
        ]);

        let prevLine = 0;
        let prevChar = 0;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            let currentLine = i;
            
            if (currentLine !== prevLine) {
                prevChar = 0;
            }

            const regex = /(\/\/.*)|(".*?")|(\b\d+(?:\.\d+)?\b)|(\b[a-zA-Z_][a-zA-Z0-9_]*\b)|(::|->|==|!=|<=|>=|\+=|-=|\*=|\/=|&&|\|\||[+\-*/=<>!&|?])/g;
            
            let match;
            while ((match = regex.exec(line)) !== null) {
                const start = match.index;
                const length = match[0].length;
                const text = match[0];
                let type = -1;
                let modifiers = 0;

                if (match[1]) type = TOKEN_TYPES.comment;
                else if (match[2]) type = TOKEN_TYPES.string;
                else if (match[3]) type = TOKEN_TYPES.number;
                else if (match[4]) {
                    if (KEYWORDS.has(text)) {
                        type = TOKEN_TYPES.keyword;
                    }
                    else if (CONSTANTS.has(text)) {
                        type = TOKEN_TYPES.enumMember;
                        modifiers = 1 << 2;
                    }
                    else if (/^[A-Z]/.test(text)) {
                        type = TOKEN_TYPES.type;
                    }
                    else {
                        const nextChar = line.substring(start + length).trim()[0];
                        const prevToken = line.substring(0, start).trim().split(/\s+/).pop();
                        
                        if (nextChar === '(') {
                            if (text === 'vec' || text === 'print' || text === 'println') {
                                type = TOKEN_TYPES.macro;
                            }
                            else {
                                type = TOKEN_TYPES.function;
                            }
                        }
                        else if (prevToken === 'fn') {
                            type = TOKEN_TYPES.function;
                            modifiers = 1 << 0;
                        }
                        else if (prevToken === 'struct') {
                            type = TOKEN_TYPES.struct;
                            modifiers = 1 << 0;
                        }
                        else if (prevToken === 'let' || prevToken === 'mut') {
                            type = TOKEN_TYPES.variable;
                            modifiers = 1 << 0;
                        }
                        else if (prevToken && prevToken.endsWith(':')) {
                            type = TOKEN_TYPES.type;
                        }
                        else if (line.trim().startsWith('#')) {
                             type = TOKEN_TYPES.decorator;
                        }
                        else {
                            type = TOKEN_TYPES.variable;
                        }
                    }
                }
                else if (match[5]) type = TOKEN_TYPES.operator;

                if (type !== -1) {
                    const deltaLine = currentLine - prevLine;
                    const deltaStart = deltaLine === 0 ? start - prevChar : start;

                    tokens.push(deltaLine, deltaStart, length, type, modifiers);

                    prevLine = currentLine;
                    prevChar = start;
                }
            }
        }
        
        return tokens;
    }
}