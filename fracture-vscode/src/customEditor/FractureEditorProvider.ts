import * as vscode from 'vscode';
import { FractureLspClient } from '../lsp/client';
import { ResolvedConfig, getSyntaxStyle } from '../utils/config';
import { getNonce } from '../utils/nonce';

export class FractureEditorProvider implements vscode.CustomEditorProvider<FractureDocument> {
    public static readonly viewType = 'fracture.editor';

    private readonly _onDidChangeCustomDocument = new vscode.EventEmitter<
        vscode.CustomDocumentEditEvent<FractureDocument> | vscode.CustomDocumentContentChangeEvent<FractureDocument>
    >();
    public readonly onDidChangeCustomDocument = this._onDidChangeCustomDocument.event;

    private readonly webviews = new Map<string, vscode.WebviewPanel>();
    private readonly documents = new Map<string, FractureDocument>();

    constructor(
        private readonly context: vscode.ExtensionContext,
        private lspClient: FractureLspClient | undefined,
        private config: ResolvedConfig | undefined
    ) {}

    updateLspClient(client: FractureLspClient | undefined): void {
        this.lspClient = client;
    }

    updateConfig(config: ResolvedConfig | undefined): void {
        this.config = config;
    }

    async refreshAllEditors(): Promise<void> {
        const syntaxStyle = getSyntaxStyle(this.config);
        
        for (const [uri, document] of this.documents) {
            if (this.lspClient && document.syntaxStyle !== syntaxStyle) {
                try {
                    document.userSyntax = await this.lspClient.fssToSyntax(
                        document.fssContent,
                        syntaxStyle
                    );
                    document.syntaxStyle = syntaxStyle;
                    
                    const webview = this.webviews.get(uri);
                    if (webview) {
                        webview.webview.postMessage({
                            type: 'setContent',
                            content: document.userSyntax,
                            syntaxStyle: syntaxStyle
                        });
                    }
                }
                catch (e) {
                    console.error(`[FractureEditor] Failed to refresh ${uri}:`, e);
                }
            }
        }
    }

    async formatDocument(uri: vscode.Uri): Promise<void> {
        const document = this.documents.get(uri.toString());
        if (!document || !this.lspClient) return;

        try {
            const formatted = await this.lspClient.formatCode(
                document.userSyntax,
                document.syntaxStyle
            );
            document.userSyntax = formatted;
            
            const webview = this.webviews.get(uri.toString());
            if (webview) {
                webview.webview.postMessage({
                    type: 'setContent',
                    content: formatted,
                    syntaxStyle: document.syntaxStyle
                });
            }
        }
        catch (e) {
            vscode.window.showErrorMessage('Fracture: Failed to format document');
        }
    }

    async openCustomDocument(uri: vscode.Uri, openContext: vscode.CustomDocumentOpenContext, token: vscode.CancellationToken): Promise<FractureDocument> {
        console.log(`[FractureEditor] Opening document: ${uri.fsPath}`);

        const fileData = await vscode.workspace.fs.readFile(uri);
        const fssContent = new TextDecoder().decode(fileData);

        const syntaxStyle = getSyntaxStyle(this.config);

        let userSyntax: string;
        if (this.lspClient) {
            try {
                userSyntax = await this.lspClient.fssToSyntax(fssContent, syntaxStyle);
            }
            catch (e) {
                console.error('[FractureEditor] Failed to convert FSS:', e);
                userSyntax = `// WARNING: Could not convert FSS to ${syntaxStyle} syntax\n// Showing raw FSS:\n\n${fssContent}`;
            }
        }
        else {
            userSyntax = `// LSP not available - showing raw FSS:\n\n${fssContent}`;
        }

        const document = new FractureDocument(uri, fssContent, userSyntax, syntaxStyle);
        this.documents.set(uri.toString(), document);

        return document;
    }

    async resolveCustomEditor(document: FractureDocument, webviewPanel: vscode.WebviewPanel, token: vscode.CancellationToken): Promise<void> {
        console.log(`[FractureEditor] Resolving editor for: ${document.uri.fsPath}`);

        this.webviews.set(document.uri.toString(), webviewPanel);

        webviewPanel.onDidDispose(() => {
            this.webviews.delete(document.uri.toString());
        });

        webviewPanel.webview.options = {
            enableScripts: true,
            localResourceRoots: [
                vscode.Uri.joinPath(this.context.extensionUri, 'webview'),
                vscode.Uri.joinPath(this.context.extensionUri, 'out'),
                vscode.Uri.joinPath(this.context.extensionUri, 'node_modules')
            ]
        };

        webviewPanel.webview.html = this.getEditorHtml(webviewPanel.webview, document);

        webviewPanel.webview.onDidReceiveMessage(async (message) => {
            switch (message.type) {
                case 'edit':
                    await this.handleEdit(document, message.content, webviewPanel.webview);
                    break;
                    
                case 'requestAst':
                    await this.sendAst(webviewPanel.webview, document);
                    break;
                    
                case 'requestFss':
                    await this.sendFss(webviewPanel.webview, document);
                    break;
                    
                case 'format':
                    await this.handleFormat(document, webviewPanel.webview);
                    break;
                    
                case 'ready':
                    webviewPanel.webview.postMessage({
                        type: 'setContent',
                        content: document.userSyntax,
                        syntaxStyle: document.syntaxStyle
                    });
                    break;
            }
        });

        document.onDidChange(() => {
            webviewPanel.webview.postMessage({
                type: 'setContent',
                content: document.userSyntax,
                syntaxStyle: document.syntaxStyle
            });
        });
    }

    async saveCustomDocument(document: FractureDocument, cancellation: vscode.CancellationToken): Promise<void> {
        console.log(`[FractureEditor] Saving document: ${document.uri.fsPath}`);

        let fssContent: string;
        if (this.lspClient) {
            try {
                fssContent = await this.lspClient.syntaxToFss(
                    document.userSyntax,
                    document.syntaxStyle
                );
            }
            catch (e) {
                console.error('[FractureEditor] Failed to convert to FSS:', e);
                vscode.window.showErrorMessage('Fracture: Failed to save - syntax error in code');
                throw e;
            }
        }
        else {
            fssContent = document.userSyntax;
        }

        document.fssContent = fssContent;

        const encoder = new TextEncoder();
        await vscode.workspace.fs.writeFile(document.uri, encoder.encode(fssContent));

        console.log(`[FractureEditor] Saved FSS to disk: ${document.uri.fsPath}`);
    }

    async saveCustomDocumentAs(document: FractureDocument, destination: vscode.Uri, cancellation: vscode.CancellationToken): Promise<void> {
        let fssContent: string;
        if (this.lspClient) {
            fssContent = await this.lspClient.syntaxToFss(
                document.userSyntax,
                document.syntaxStyle
            );
        }
        else {
            fssContent = document.userSyntax;
        }

        const encoder = new TextEncoder();
        await vscode.workspace.fs.writeFile(destination, encoder.encode(fssContent));
    }

    async revertCustomDocument(document: FractureDocument, cancellation: vscode.CancellationToken): Promise<void> {
        const fileData = await vscode.workspace.fs.readFile(document.uri);
        const fssContent = new TextDecoder().decode(fileData);

        if (this.lspClient) {
            document.userSyntax = await this.lspClient.fssToSyntax(
                fssContent,
                document.syntaxStyle
            );
        }
        else {
            document.userSyntax = fssContent;
        }

        document.fssContent = fssContent;
    }

    async backupCustomDocument(document: FractureDocument, context: vscode.CustomDocumentBackupContext, cancellation: vscode.CancellationToken): Promise<vscode.CustomDocumentBackup> {
        const backupUri = context.destination;

        let fssContent: string;
        if (this.lspClient) {
            try {
                fssContent = await this.lspClient.syntaxToFss(
                    document.userSyntax,
                    document.syntaxStyle
                );
            }
            catch {
                fssContent = document.fssContent;
            }
        }
        else {
            fssContent = document.fssContent;
        }

        const encoder = new TextEncoder();
        await vscode.workspace.fs.writeFile(backupUri, encoder.encode(fssContent));

        return {
            id: backupUri.toString(),
            delete: async () => {
                try {
                    await vscode.workspace.fs.delete(backupUri);
                }
                catch {
                    // Backup already deleted
                }
            }
        };
    }

    private async handleEdit(document: FractureDocument, newContent: string, webview: vscode.Webview): Promise<void> {
        const oldContent = document.userSyntax;
        document.userSyntax = newContent;

        this._onDidChangeCustomDocument.fire({
            document,
            undo: async () => {
                document.userSyntax = oldContent;
            },
            redo: async () => {
                document.userSyntax = newContent;
            }
        });

        if (this.lspClient) {
            const diagnostics = await this.lspClient.getDiagnostics(
                newContent,
                document.syntaxStyle
            );
            
            webview.postMessage({
                type: 'diagnostics',
                data: diagnostics
            });
        }
    }

    private async handleFormat(document: FractureDocument, webview: vscode.Webview): Promise<void> {
        if (!this.lspClient) return;

        try {
            const formatted = await this.lspClient.formatCode(
                document.userSyntax,
                document.syntaxStyle
            );

            document.userSyntax = formatted;

            webview.postMessage({
                type: 'setContent',
                content: formatted,
                syntaxStyle: document.syntaxStyle
            });
        }
        catch (e) {
            vscode.window.showErrorMessage('Fracture: Failed to format');
        }
    }

    private async sendAst(webview: vscode.Webview, document: FractureDocument): Promise<void> {
        if (this.lspClient) {
            try {
                const ast = await this.lspClient.getAst(document.userSyntax, document.syntaxStyle);
                webview.postMessage({
                    type: 'ast',
                    data: ast
                });
            }
            catch {
                webview.postMessage({
                    type: 'ast',
                    data: { error: 'Failed to parse AST' }
                });
            }
        }
    }

    private async sendFss(webview: vscode.Webview, document: FractureDocument): Promise<void> {
        if (this.lspClient) {
            try {
                const fss = await this.lspClient.syntaxToFss(
                    document.userSyntax,
                    document.syntaxStyle
                );
                webview.postMessage({
                    type: 'fss',
                    data: fss
                });
            }
            catch(e) {
                webview.postMessage({
                    type: 'fss',
                    data: `// Error converting to FSS:\n// ${e}`
                });
            }
        }
    }

    private getEditorHtml(webview: vscode.Webview, document: FractureDocument): string {
        const nonce = getNonce();
        
        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src ${webview.cspSource} 'unsafe-inline'; script-src 'nonce-${nonce}' https://cdnjs.cloudflare.com; font-src https://cdnjs.cloudflare.com;">
    <title>Fracture Editor</title>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs/editor/editor.main.min.css">
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        
        body {
            font-family: var(--vscode-font-family);
            background: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
            height: 100vh;
            overflow: hidden;
        }

        .toolbar {
            display: flex;
            align-items: center;
            padding: 4px 8px;
            background: var(--vscode-editorGroupHeader-tabsBackground);
            border-bottom: 1px solid var(--vscode-editorGroupHeader-tabsBorder);
            gap: 8px;
            height: 33px;
        }

        .toolbar-button {
            padding: 4px 8px;
            border: none;
            background: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            cursor: pointer;
            border-radius: 3px;
            font-size: 12px;
        }

        .toolbar-button:hover {
            background: var(--vscode-button-secondaryHoverBackground);
        }

        .toolbar-button.active {
            background: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
        }

        .toolbar-separator {
            width: 1px;
            height: 20px;
            background: var(--vscode-editorGroupHeader-tabsBorder);
        }

        .syntax-indicator {
            margin-left: auto;
            font-size: 11px;
            color: var(--vscode-badge-foreground);
            background: var(--vscode-badge-background);
            padding: 2px 8px;
            border-radius: 10px;
        }

        .main-container {
            display: flex;
            height: calc(100vh - 33px - 22px);
        }

        #editor-container {
            flex: 1;
            overflow: hidden;
        }

        .side-panel {
            width: 350px;
            background: var(--vscode-sideBar-background);
            border-left: 1px solid var(--vscode-sideBar-border);
            display: none;
            flex-direction: column;
            overflow: hidden;
        }

        .side-panel.visible { display: flex; }

        .panel-header {
            padding: 8px 12px;
            font-weight: 600;
            font-size: 11px;
            text-transform: uppercase;
            background: var(--vscode-sideBarSectionHeader-background);
            border-bottom: 1px solid var(--vscode-sideBarSectionHeader-border);
        }

        .panel-content {
            flex: 1;
            padding: 8px;
            overflow: auto;
            font-family: 'Fira Code', Consolas, monospace;
            font-size: 12px;
            white-space: pre-wrap;
        }

        .status-bar {
            display: flex;
            align-items: center;
            padding: 2px 8px;
            background: var(--vscode-statusBar-background);
            color: var(--vscode-statusBar-foreground);
            font-size: 12px;
            gap: 16px;
            height: 22px;
        }

        .status-error {
            color: var(--vscode-statusBarItem-errorForeground);
            background: var(--vscode-statusBarItem-errorBackground);
            padding: 0 6px;
            border-radius: 3px;
        }

        .status-warning {
            color: var(--vscode-statusBarItem-warningForeground);
            background: var(--vscode-statusBarItem-warningBackground);
            padding: 0 6px;
            border-radius: 3px;
        }

        .fss-keyword { color: #c586c0; }
        .fss-instruction { color: #4ec9b0; }
        .fss-register { color: #9cdcfe; }
        .fss-comment { color: #6a9955; font-style: italic; }
        .fss-string { color: #ce9178; }
        .fss-number { color: #b5cea8; }
        .fss-label { color: #dcdcaa; }

        .ast-node { padding: 2px 0 2px 16px; }
        .ast-kind { color: #4ec9b0; }
        .ast-name { color: #dcdcaa; }
        .ast-type { color: #6a9955; font-size: 10px; }

        .loading {
            display: flex;
            align-items: center;
            justify-content: center;
            height: 100%;
            color: var(--vscode-descriptionForeground);
        }

        @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
        }

        .spinner {
            width: 16px;
            height: 16px;
            border: 2px solid var(--vscode-progressBar-background);
            border-top-color: transparent;
            border-radius: 50%;
            animation: spin 1s linear infinite;
            margin-right: 8px;
        }
    </style>
</head>
<body>
    <div class="toolbar">
        <button class="toolbar-button" id="btn-ast" title="Toggle AST Panel">AST</button>
        <button class="toolbar-button" id="btn-fss" title="Toggle FSS Panel">FSS</button>
        <div class="toolbar-separator"></div>
        <button class="toolbar-button" id="btn-format" title="Format Code (Ctrl+Shift+F)">Format</button>
        <button class="toolbar-button" id="btn-check" title="Run Type Check">Check</button>
        <span class="syntax-indicator" id="syntax-indicator">${document.syntaxStyle}</span>
    </div>

    <div class="main-container">
        <div id="editor-container"></div>
        <div class="side-panel" id="side-panel">
            <div class="panel-header" id="panel-header">AST</div>
            <div class="panel-content" id="panel-content">
                <div class="loading"><div class="spinner"></div><span>Loading...</span></div>
            </div>
        </div>
    </div>

    <div class="status-bar">
        <span id="cursor-pos">Ln 1, Col 1</span>
        <span id="status-diagnostics"></span>
        <span style="margin-left: auto;" id="status-message"></span>
    </div>

    <script src="https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs/loader.min.js" nonce="${nonce}"></script>
    <script nonce="${nonce}">
        
        const vscode = acquireVsCodeApi();
        let editor = null;
        let currentContent = '';
        let syntaxStyle = '${document.syntaxStyle}';
        let activePanel = null;
        let debounceTimer = null;

        const editorContainer = document.getElementById('editor-container');
        const sidePanel = document.getElementById('side-panel');
        const panelHeader = document.getElementById('panel-header');
        const panelContent = document.getElementById('panel-content');
        const syntaxIndicator = document.getElementById('syntax-indicator');
        const cursorPos = document.getElementById('cursor-pos');
        const statusDiagnostics = document.getElementById('status-diagnostics');

        require.config({ 
            paths: { 'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs' }
        });

        require(['vs/editor/editor.main'], function() {
            monaco.languages.register({ id: 'fracture' });

            updateTokensProvider(syntaxStyle);

            editor = monaco.editor.create(editorContainer, {
                value: '',
                language: 'fracture',
                theme: document.body.classList.contains('vscode-light') ? 'vs' : 'vs-dark',
                automaticLayout: true,
                minimap: { enabled: true },
                fontSize: 14,
                fontFamily: "'Fira Code', Consolas, 'Courier New', monospace",
                fontLigatures: true,
                lineNumbers: 'on',
                renderWhitespace: 'selection',
                scrollBeyondLastLine: false,
                wordWrap: 'on',
                tabSize: 4,
                insertSpaces: true
            });

            editor.onDidChangeModelContent(() => {
                const newContent = editor.getValue();
                if (newContent !== currentContent) {
                    currentContent = newContent;
                    
                    clearTimeout(debounceTimer);
                    debounceTimer = setTimeout(() => {
                        vscode.postMessage({ type: 'edit', content: newContent });
                    }, 300);
                }
            });

            editor.onDidChangeCursorPosition((e) => {
                cursorPos.textContent = 'Ln ' + e.position.lineNumber + ', Col ' + e.position.column;
            });

            editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF, () => {
                vscode.postMessage({ type: 'format' });
            });

            vscode.postMessage({ type: 'ready' });
        });

        function updateTokensProvider(style) {
            const rustTokens = {
                keywords: ['fn', 'let', 'mut', 'const', 'if', 'else', 'match', 'for', 'while', 'loop', 'return', 'struct', 'enum', 'impl', 'trait', 'pub', 'use', 'mod', 'self', 'Self', 'true', 'false', 'where', 'type', 'async', 'await', 'move', 'ref', 'static', 'unsafe', 'extern', 'crate', 'super'],
                types: ['i8', 'i16', 'i32', 'i64', 'i128', 'u8', 'u16', 'u32', 'u64', 'u128', 'f32', 'f64', 'bool', 'char', 'str', 'String', 'Vec', 'Option', 'Result', 'Box', 'Rc', 'Arc', 'HashMap', 'HashSet']
            };

            const pythonTokens = {
                keywords: ['def', 'class', 'if', 'elif', 'else', 'for', 'while', 'return', 'import', 'from', 'as', 'try', 'except', 'finally', 'with', 'lambda', 'True', 'False', 'None', 'and', 'or', 'not', 'in', 'is', 'pass', 'break', 'continue', 'raise', 'yield', 'global', 'nonlocal', 'assert', 'del'],
                types: ['int', 'float', 'str', 'bool', 'list', 'dict', 'tuple', 'set', 'None', 'List', 'Dict', 'Tuple', 'Set', 'Optional', 'Union', 'Any']
            };

            const tokens = style === 'python' ? pythonTokens : rustTokens;

            monaco.languages.setMonarchTokensProvider('fracture', {
                keywords: tokens.keywords,
                typeKeywords: tokens.types,
                operators: ['=', '>', '<', '!', '~', '?', ':', '==', '<=', '>=', '!=', '&&', '||', '++', '--', '+', '-', '*', '/', '&', '|', '^', '%', '<<', '>>', '>>>', '+=', '-=', '*=', '/=', '&=', '|=', '^=', '%=', '<<=', '>>=', '>>>=', '->', '=>', '::'],
                symbols: /[=><!~?:&|+\\-*\\/\\^%]+/,

                tokenizer: {
                    root: [
                        [/[a-z_$][\\w$]*/, { 
                            cases: { 
                                '@keywords': 'keyword',
                                '@typeKeywords': 'type',
                                '@default': 'identifier' 
                            } 
                        }],
                        [/[A-Z][\\w$]*/, 'type.identifier'],
                        { include: '@whitespace' },
                        [/[{}()\\[\\]]/, '@brackets'],
                        [/[<>](?!@symbols)/, '@brackets'],
                        [/@symbols/, { cases: { '@operators': 'operator', '@default': '' } }],
                        [/\\d*\\.\\d+([eE][\\-+]?\\d+)?/, 'number.float'],
                        [/0[xX][0-9a-fA-F]+/, 'number.hex'],
                        [/\\d+/, 'number'],
                        [/[;,.]/, 'delimiter'],
                        [/"([^"\\\\]|\\\\.)*$/, 'string.invalid'],
                        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],
                        [/'[^\\\\']'/, 'string'],
                        [/'/, 'string.invalid']
                    ],
                    string: [
                        [/[^\\\\"]+/, 'string'],
                        [/\\\\./, 'string.escape'],
                        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
                    ],
                    whitespace: [
                        [/[ \\t\\r\\n]+/, 'white'],
                        [/\\/\\/.*$/, 'comment'],
                        [/#.*$/, 'comment'],
                        [/\\/\\*/, 'comment', '@comment']
                    ],
                    comment: [
                        [/[^\\/*]+/, 'comment'],
                        [/\\/\\*/, 'comment', '@push'],
                        [/\\*\\//, 'comment', '@pop'],
                        [/[\\/*]/, 'comment']
                    ]
                }
            });
        }

        window.addEventListener('message', event => {
            const message = event.data;
            
            switch (message.type) {
                case 'setContent':
                    currentContent = message.content;
                    syntaxStyle = message.syntaxStyle || syntaxStyle;
                    syntaxIndicator.textContent = syntaxStyle;
                    
                    if (editor) {
                        const pos = editor.getPosition();
                        editor.setValue(message.content);
                        if (pos) editor.setPosition(pos);
                        updateTokensProvider(syntaxStyle);
                    }
                    break;
                    
                case 'diagnostics':
                    renderDiagnostics(message.data);
                    break;
                    
                case 'ast':
                    renderAst(message.data);
                    break;
                    
                case 'fss':
                    renderFss(message.data);
                    break;
            }
        });

        function renderDiagnostics(diagnostics) {
            if (!editor) return;

            const markers = diagnostics.map(d => ({
                severity: d.severity === 'error' ? monaco.MarkerSeverity.Error :
                         d.severity === 'warning' ? monaco.MarkerSeverity.Warning :
                         monaco.MarkerSeverity.Info,
                message: d.message,
                startLineNumber: d.span?.start?.line || 1,
                startColumn: d.span?.start?.column || 1,
                endLineNumber: d.span?.end?.line || 1,
                endColumn: d.span?.end?.column || 1
            }));

            monaco.editor.setModelMarkers(editor.getModel(), 'fracture', markers);

            const errors = diagnostics.filter(d => d.severity === 'error').length;
            const warnings = diagnostics.filter(d => d.severity === 'warning').length;

            if (errors > 0) {
                statusDiagnostics.innerHTML = '<span class="status-error">' + errors + ' error' + (errors > 1 ? 's' : '') + '</span>';
            } else if (warnings > 0) {
                statusDiagnostics.innerHTML = '<span class="status-warning">' + warnings + ' warning' + (warnings > 1 ? 's' : '') + '</span>';
            } else {
                statusDiagnostics.textContent = '';
            }
        }

        function renderAst(data) {
            if (data.error) {
                panelContent.innerHTML = '<div style="color: var(--vscode-errorForeground);">' + data.error + '</div>';
                return;
            }
            panelContent.innerHTML = renderAstNode(data, 0);
        }

        function renderAstNode(node, depth) {
            if (!node) return '';
            let html = '<div class="ast-node" style="padding-left:' + (depth * 12) + 'px">';
            html += '<span class="ast-kind">' + (node.kind || 'Node') + '</span>';
            if (node.name) html += ' <span class="ast-name">' + node.name + '</span>';
            if (node.type) html += ' <span class="ast-type">: ' + node.type + '</span>';
            html += '</div>';
            if (node.children) {
                for (const child of node.children) {
                    html += renderAstNode(child, depth + 1);
                }
            }
            return html;
        }

        function renderFss(data) {
            const escaped = data.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
            const highlighted = escaped
                .replace(/^(;.*)$/gm, '<span class="fss-comment">$1</span>')
                .replace(/\\b(MODULE|END_MODULE|FUNC|END|BODY|PARAM|SELF|RET|LOCAL|FIELD|STRUCT|IMPL|IMPORT)\\b/g, '<span class="fss-keyword">$1</span>')
                .replace(/\\b(LOAD|STORE|ADD|SUB|MUL|DIV|CALL|ARG|JMP|JZ|JNZ|CMP|DROP)\\b/g, '<span class="fss-instruction">$1</span>')
                .replace(/\\b(r\\d+)\\b/g, '<span class="fss-register">$1</span>')
                .replace(/(\\d+\\.?\\d*)/g, '<span class="fss-number">$1</span>')
                .replace(/(".*?")/g, '<span class="fss-string">$1</span>');
            panelContent.innerHTML = highlighted;
        }

        document.getElementById('btn-ast').addEventListener('click', () => {
            togglePanel('ast', 'AST');
        });

        document.getElementById('btn-fss').addEventListener('click', () => {
            togglePanel('fss', 'FSS');
        });

        function togglePanel(type, title) {
            if (activePanel === type) {
                sidePanel.classList.remove('visible');
                activePanel = null;
            } else {
                sidePanel.classList.add('visible');
                panelHeader.textContent = title;
                panelContent.innerHTML = '<div class="loading"><div class="spinner"></div><span>Loading...</span></div>';
                activePanel = type;
                vscode.postMessage({ type: type === 'ast' ? 'requestAst' : 'requestFss' });
            }
            document.getElementById('btn-ast').classList.toggle('active', activePanel === 'ast');
            document.getElementById('btn-fss').classList.toggle('active', activePanel === 'fss');
        }

        document.getElementById('btn-format').addEventListener('click', () => {
            vscode.postMessage({ type: 'format' });
        });

        document.getElementById('btn-check').addEventListener('click', () => {
            vscode.postMessage({ type: 'check' });
        });
    </script>
</body>
</html>`;
    }
}

export class FractureDocument implements vscode.CustomDocument {
    
    private readonly _onDidChange = new vscode.EventEmitter<void>();
    public readonly onDidChange = this._onDidChange.event;

    constructor(
        public readonly uri: vscode.Uri,
        public fssContent: string,
        public userSyntax: string,
        public syntaxStyle: string
    ) {}

    dispose(): void {
        this._onDidChange.dispose();
    }

    public fireChange(): void {
        this._onDidChange.fire();
    }
}