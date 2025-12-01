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
    HoverResult
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
        const compilerPath = vscode.workspace.getConfiguration('fracture').get<string>('compiler.path', 'fracture');

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
            this.outputChannel.appendLine(`Compiler not found, using mock LSP: ${e}`);
            this.useMock = true;
            
            vscode.window.showInformationMessage(
                'Fracture: Using mock language server (compiler not found). ' +
                'Install the Fracture compiler for full functionality.'
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
}

// Mock LSP client to test before linking to real compiler
class MockLspImplementation {
    syntaxToFss(source: string, syntaxStyle: string): string {
        const lines: string[] = [
            '; FSS generated from ' + syntaxStyle + ' syntax',
            '; Lines: ' + source.split('\n').length,
            ''
        ];
        
        const sourceLines = source.split('\n');
        let inFunction = false;
        
        for (const line of sourceLines) {
            const trimmed = line.trim();
            
            if (!trimmed || trimmed.startsWith('//') || trimmed.startsWith('#')) {
                if (trimmed.startsWith('//') || trimmed.startsWith('#')) {
                    lines.push('; ' + trimmed.substring(trimmed.startsWith('//') ? 2 : 1).trim());
                }
                continue;
            }
            
            if (syntaxStyle === 'python' && trimmed.startsWith('def ')) {
                const match = trimmed.match(/def\s+(\w+)\s*\(([^)]*)\)/);
                if (match) {
                    if (inFunction) lines.push('END');
                    lines.push('');
                    lines.push('FUNC ' + match[1]);
                    
                    if (match[2]) {
                        const params = match[2].split(',').map(p => p.trim());
                        for (const param of params) {
                            const [name, type] = param.split(':').map(s => s.trim());
                            lines.push('    PARAM ' + name + (type ? ': ' + type : ': any'));
                        }
                    }
                    lines.push('BODY');
                    inFunction = true;
                }
            }
            else if (syntaxStyle === 'rust' && trimmed.startsWith('fn ')) {
                const match = trimmed.match(/fn\s+(\w+)\s*\(([^)]*)\)(?:\s*->\s*(\w+))?/);
                if (match) {
                    if (inFunction) lines.push('END');
                    lines.push('');
                    lines.push('FUNC ' + match[1]);
                    
                    if (match[2]) {
                        const params = match[2].split(',').map(p => p.trim());
                        for (const param of params) {
                            const parts = param.split(':').map(s => s.trim());
                            if (parts.length >= 2) {
                                lines.push('    PARAM ' + parts[0] + ': ' + parts[1]);
                            }
                        }
                    }
                    if (match[3]) {
                        lines.push('    RET ' + match[3]);
                    }
                    lines.push('BODY');
                    inFunction = true;
                }
            }
            
            else if (trimmed.startsWith('let ') || trimmed.startsWith('mut ')) {
                const match = trimmed.match(/(?:let|mut)\s+(\w+)(?:\s*:\s*(\w+))?\s*=\s*(.+?);?$/);
                if (match) {
                    lines.push('    LOCAL ' + match[1] + (match[2] ? ': ' + match[2] : ''));
                    lines.push('    STORE ' + match[1] + ', ' + match[3].replace(/;$/, ''));
                }
            }
            else if (trimmed.match(/^\w+\s*=\s*.+/) && syntaxStyle === 'python') {
                const match = trimmed.match(/^(\w+)\s*=\s*(.+)$/);
                if (match) {
                    lines.push('    LOCAL ' + match[1]);
                    lines.push('    STORE ' + match[1] + ', ' + match[2]);
                }
            }
            
            else if (trimmed.match(/^\w+\s*\(/)) {
                const match = trimmed.match(/^(\w+)\s*\(([^)]*)\)/);
                if (match) {
                    lines.push('    CALL ' + match[1]);
                    if (match[2]) {
                        const args = match[2].split(',').map(a => a.trim());
                        for (const arg of args) {
                            lines.push('        ARG ' + arg);
                        }
                    }
                }
            }
            
            else if (trimmed.startsWith('return ')) {
                const value = trimmed.replace(/^return\s+/, '').replace(/;$/, '');
                lines.push('    RET ' + value);
            }
            
            else if (trimmed.startsWith('print(') || trimmed.startsWith('println!(')) {
                const match = trimmed.match(/print(?:ln!)?\(([^)]+)\)/);
                if (match) {
                    lines.push('    CALL print');
                    lines.push('        ARG ' + match[1]);
                }
            }
        }
        
        if (inFunction) {
            lines.push('END');
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
                if (syntaxStyle === 'rust') {
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
            
            if (syntaxStyle === 'rust' && line.startsWith('fn ')) {
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
        
        // Simple mock diagnostics
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            
            // Check for unused variables (mock)
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
            
            // Check for TODO comments
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
        // Simple formatting: normalize indentation
        const lines = source.split('\n');
        const formatted: string[] = [];
        let indentLevel = 0;
        const indentStr = syntaxStyle === 'python' ? '    ' : '    ';
        
        for (const line of lines) {
            const trimmed = line.trim();
            
            // Decrease indent before closing braces
            if (trimmed.startsWith('}') || trimmed.startsWith(']') || trimmed.startsWith(')')) {
                indentLevel = Math.max(0, indentLevel - 1);
            }
            
            // Add the line with proper indentation
            if (trimmed) {
                formatted.push(indentStr.repeat(indentLevel) + trimmed);
            } else {
                formatted.push('');
            }
            
            // Increase indent after opening braces or colons (Python)
            if (trimmed.endsWith('{') || trimmed.endsWith('[') || 
                (syntaxStyle === 'python' && trimmed.endsWith(':'))) {
                indentLevel++;
            }
        }
        
        return formatted.join('\n');
    }
    
    getHover(source: string, syntaxStyle: string, line: number, column: number): HoverResult | null {
        // Mock hover - return type info for known patterns
        const lines = source.split('\n');
        if (line > lines.length) return null;
        
        const currentLine = lines[line - 1];
        
        // Check if hovering over a function name
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
}