import * as vscode from 'vscode';
import { FractureLspClient } from '../lsp/client';
import { Diagnostic } from '../lsp/protocol';
import { ResolvedConfig, getSyntaxStyle } from '../utils/config';

export class DiagnosticsProvider implements vscode.TreeDataProvider<DiagnosticItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<DiagnosticItem | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private diagnosticsMap: Map<string, Diagnostic[]> = new Map();
    private debounceTimer: ReturnType<typeof setTimeout> | undefined;

    constructor(
        private lspClient: FractureLspClient | undefined,
        private config: ResolvedConfig | undefined
    ) {
        vscode.workspace.onDidChangeTextDocument((e) => {
            if (e.document.uri.fsPath.endsWith('.frac')) {
                this.scheduleRefresh(e.document);
            }
        });
        
        vscode.window.onDidChangeActiveTextEditor((editor) => {
            if (editor && editor.document.uri.fsPath.endsWith('.frac')) {
                this.refreshForDocument(editor.document);
            }
        });
        
        vscode.workspace.onDidSaveTextDocument((doc: vscode.TextDocument) => {
            if (doc.uri.fsPath.endsWith('.frac')) {
                this.refreshForDocument(doc);
            }
        });
    }

    updateLspClient(client: FractureLspClient | undefined): void {
        this.lspClient = client;
        this.refresh();
    }
    
    updateConfig(config: ResolvedConfig | undefined): void {
        this.config = config;
        this.refresh();
    }

    private scheduleRefresh(document: vscode.TextDocument): void {
        if (this.debounceTimer) {
            global.clearTimeout(this.debounceTimer);
        }
        this.debounceTimer = global.setTimeout(() => {
            this.refreshForDocument(document);
        }, 500);
    }

    private async refreshForDocument(document: vscode.TextDocument): Promise<void> {
        if (!this.lspClient) return;
        
        const syntaxStyle = getSyntaxStyle(this.config);
        const diagnostics = await this.lspClient.getDiagnostics(
            document.getText(),
            syntaxStyle
        );
        
        if (diagnostics.length > 0) {
            this.diagnosticsMap.set(document.uri.toString(), diagnostics);
        } else {
            this.diagnosticsMap.delete(document.uri.toString());
        }
        
        this._onDidChangeTreeData.fire(undefined);
    }
    
    async refresh(): Promise<void> {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.uri.fsPath.endsWith('.frac')) {
            await this.refreshForDocument(editor.document);
        }
        else {
            this._onDidChangeTreeData.fire(undefined);
        }
    }

    updateDiagnostics(uri: string, diagnostics: Diagnostic[]): void {
        if (diagnostics.length > 0) {
            this.diagnosticsMap.set(uri, diagnostics);
        }
        else {
            this.diagnosticsMap.delete(uri);
        }

        this._onDidChangeTreeData.fire(undefined);
    }

    clear(): void {
        this.diagnosticsMap.clear();
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: DiagnosticItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: DiagnosticItem): Thenable<DiagnosticItem[]> {
        if (!element) {
            const items: DiagnosticItem[] = [];

            for (const [uri, diagnostics] of this.diagnosticsMap) {
                const errorCount = diagnostics.filter(d => d.severity === 'error').length;
                const warningCount = diagnostics.filter(d => d.severity === 'warning').length;

                items.push(new DiagnosticItem(
                    'file',
                    uri.split('/').pop() || uri,
                    `${errorCount} errors, ${warningCount} warnings`,
                    vscode.TreeItemCollapsibleState.Expanded,
                    uri
                ));
            }

            if (items.length === 0) {
                items.push(new DiagnosticItem(
                    'info',
                    'No diagnostics',
                    undefined,
                    vscode.TreeItemCollapsibleState.None
                ));
            }

            return Promise.resolve(items);
        }
        else if (element.itemType === 'file') {
            const diagnostics = this.diagnosticsMap.get(element.uri!) || [];

            return Promise.resolve(
                diagnostics.map(d => new DiagnosticItem(
                    'diagnostic',
                    d.message,
                    `Line ${d.span.start.line}`,
                    d.suggestions && d.suggestions.length > 0
                        ? vscode.TreeItemCollapsibleState.Collapsed
                        : vscode.TreeItemCollapsibleState.None,
                    element.uri,
                    d
                ))
            );
        }
        else if (element.itemType === 'diagnostic' && element.diagnostic?.suggestions) {
            return Promise.resolve(
                element.diagnostic.suggestions.map(s => new DiagnosticItem(
                    'suggestion',
                    `Did you mean: ${s}`,
                    undefined,
                    vscode.TreeItemCollapsibleState.None
                )) 
            );
        }

        return Promise.resolve([]);
    }
}

class DiagnosticItem extends vscode.TreeItem {
    constructor(
        public readonly itemType: 'file' | 'diagnostic' | 'suggestion' | 'info',
        label: string,
        description: string | undefined,
        collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly uri?: string,
        public readonly diagnostic?: Diagnostic
    ) {
        super(label, collapsibleState);
        
        this.description = description;
        
        switch (itemType) {
            case 'file':
                this.iconPath = new vscode.ThemeIcon('file-code');
                this.contextValue = 'diagnosticFile';
                break;
                
            case 'diagnostic':
                if (diagnostic) {
                    switch (diagnostic.severity) {
                        case 'error':
                            this.iconPath = new vscode.ThemeIcon(
                                'error',
                                new vscode.ThemeColor('errorForeground')
                            );
                            break;
                        case 'warning':
                            this.iconPath = new vscode.ThemeIcon(
                                'warning',
                                new vscode.ThemeColor('warningForeground')
                            );
                            break;
                        case 'info':
                            this.iconPath = new vscode.ThemeIcon('info');
                            break;
                        case 'hint':
                            this.iconPath = new vscode.ThemeIcon('lightbulb');
                            break;
                    }
                    
                    if (uri && diagnostic.span) {
                        this.command = {
                            command: 'vscode.open',
                            title: 'Go to Error',
                            arguments: [
                                vscode.Uri.file(uri),
                                {
                                    selection: new vscode.Range(
                                        diagnostic.span.start.line - 1,
                                        diagnostic.span.start.column - 1,
                                        diagnostic.span.end.line - 1,
                                        diagnostic.span.end.column - 1
                                    )
                                }
                            ]
                        };
                    }
                    
                    this.tooltip = this.buildTooltip();
                }
                this.contextValue = 'diagnostic';
                break;
                
            case 'suggestion':
                this.iconPath = new vscode.ThemeIcon('lightbulb-autofix');
                this.contextValue = 'suggestion';
                break;
            case 'info':
                this.iconPath = new vscode.ThemeIcon('check');
                break;
        }
    }
    
    private buildTooltip(): vscode.MarkdownString {
        if (!this.diagnostic) {
            return new vscode.MarkdownString(this.label as string);
        }
        
        const md = new vscode.MarkdownString();
        
        const severityIcon = {
            'error': '$(error)',
            'warning': '$(warning)',
            'info': '$(info)',
            'hint': '$(lightbulb)'
        }[this.diagnostic.severity];
        
        md.appendMarkdown(`${severityIcon} **${this.diagnostic.severity.toUpperCase()}**`);
        
        if (this.diagnostic.code) {
            md.appendMarkdown(` \`${this.diagnostic.code}\``);
        }
        
        md.appendMarkdown('\n\n');
        md.appendMarkdown(this.diagnostic.message);
        
        if (this.diagnostic.suggestions && this.diagnostic.suggestions.length > 0) {
            md.appendMarkdown('\n\n---\n\n');
            md.appendMarkdown('**Suggestions:**\n');
            for (const suggestion of this.diagnostic.suggestions) {
                md.appendMarkdown(`- ${suggestion}\n`);
            }
        }
        
        return md;
    }
}