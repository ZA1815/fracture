import * as vscode from 'vscode';
import { FractureLspClient } from '../lsp/client';
import { AstNode } from '../lsp/protocol';
import { ResolvedConfig, getSyntaxStyle } from '../utils/config';

export class AstTreeProvider implements vscode.TreeDataProvider<AstTreeItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<AstTreeItem | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private currentAst: AstNode | undefined;

    constructor(private lspClient: FractureLspClient | undefined, private config: ResolvedConfig | undefined) {
        vscode.window.onDidChangeActiveTextEditor(() => {
            this.refresh();
        });

        vscode.workspace.onDidSaveTextDocument(() => {
            this.refresh();
        })
    }

    updateLspClient(client: FractureLspClient | undefined): void {
        this.lspClient = client;
        this.refresh();
    }
    
    updateConfig(config: ResolvedConfig | undefined): void {
        this.config = config;
        this.refresh();
    }

    async refresh(): Promise<void> {
        const editor = vscode.window.activeTextEditor;
        
        if (editor && editor.document.uri.fsPath.endsWith('.frac')) {
            if (this.lspClient) {
                try {
                    const content = editor.document.getText();
                    const syntaxStyle = getSyntaxStyle(this.config);
                    this.currentAst = await this.lspClient.getAst(content, syntaxStyle);
                }
                catch {
                    this.currentAst = undefined;
                }
            }
        }
        else {
            this.currentAst = undefined;
        }
        
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: AstTreeItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: AstTreeItem): Thenable<AstTreeItem[]> {
        if (!this.currentAst) {
            return Promise.resolve([]);
        }

        if (!element) {
            return Promise.resolve([this.astNodeToTreeItem(this.currentAst)]);
        }
        else {
            return Promise.resolve((element.astNode.children || []).map(child => this.astNodeToTreeItem(child)));
        }
    }

    private astNodeToTreeItem(node: AstNode): AstTreeItem {
        const hasChildren = node.children && node.children.length > 0;
        const item = new AstTreeItem(
            node,
            hasChildren
                ? vscode.TreeItemCollapsibleState.Collapsed
                : vscode.TreeItemCollapsibleState.None
        );
        return item;
    }
}

class AstTreeItem extends vscode.TreeItem {
    constructor(public readonly astNode: AstNode, collapsibleState: vscode.TreeItemCollapsibleState) {
        const label = astNode.name ? `${astNode.kind}: ${astNode.name}` : astNode.kind;

        super(label, collapsibleState);

        if (astNode.type) {
            this.description = astNode.type;
        }

        this.tooltip = this.buildTooltip();
        this.iconPath = this.getIcon();
        this.contextValue = 'astNode';

        if (astNode.span) {
            this.command = {
                command: 'revealLine',
                title: 'Go to line',
                arguments: [{ lineNumber: astNode.span.start.line, at: 'center' }]
            };
        }
    }

    private buildTooltip(): vscode.MarkdownString {
        const md = new vscode.MarkdownString();
        md.appendMarkdown(`**${this.astNode.kind}**\n\n`);
        
        if (this.astNode.name) {
            md.appendMarkdown(`Name: \`${this.astNode.name}\`\n\n`);
        }
        if (this.astNode.type) {
            md.appendMarkdown(`Type: \`${this.astNode.type}\`\n\n`);
        }
        if (this.astNode.span) {
            md.appendMarkdown(
                `Location: Line ${this.astNode.span.start.line}, ` +
                `Col ${this.astNode.span.start.column}`
            );
        }
        
        return md;
    }

    private getIcon(): vscode.ThemeIcon {
        const iconMap: { [key: string]: string } = {
            'Module': 'symbol-module',
            'FunctionDef': 'symbol-function',
            'ClassDef': 'symbol-class',
            'StructDef': 'symbol-struct',
            'EnumDef': 'symbol-enum',
            'TraitDef': 'symbol-interface',
            'ImplBlock': 'symbol-method',
            'LetStmt': 'symbol-variable',
            'ConstStmt': 'symbol-constant',
            'IfStmt': 'symbol-boolean',
            'MatchStmt': 'symbol-boolean',
            'ForStmt': 'symbol-array',
            'WhileStmt': 'symbol-boolean',
            'ReturnStmt': 'symbol-keyword',
            'Call': 'symbol-function',
            'BinaryExpr': 'symbol-operator',
            'UnaryExpr': 'symbol-operator',
            'Literal': 'symbol-value',
            'Identifier': 'symbol-variable',
            'TypeRef': 'symbol-type-parameter',
            'Block': 'symbol-namespace',
            'Parameter': 'symbol-parameter',
            'Field': 'symbol-field',
            'Variant': 'symbol-enum-member'
        };
        
        const iconName = iconMap[this.astNode.kind] || 'symbol-misc';
        return new vscode.ThemeIcon(iconName);
    }
}