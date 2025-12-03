import * as vscode from 'vscode';
import { FractureEditorProvider } from './customEditor/FractureEditorProvider';
import { FractureLspClient } from './lsp/client';
import { AstTreeProvider } from './views/AstTreeProvider';
import { DiagnosticsProvider } from './views/DiagnosticsProvider';
import { ProjectTreeProvider } from './views/ProjectTreeProvider';
import { FractureSemanticTokensProvider, LEGEND } from './semanticTokens';
import { ResolvedConfig, loadRiftConfig, saveProjectLocalConfig } from './utils/config';

let lspClient: FractureLspClient | undefined;
let currentConfig: ResolvedConfig | undefined;
let editorProvider: FractureEditorProvider | undefined;
let astProvider: AstTreeProvider | undefined;
let diagnosticsProvider: DiagnosticsProvider | undefined;
let semanticTokensProvider: FractureSemanticTokensProvider | undefined;

export async function activate(context: vscode.ExtensionContext) {
    console.log('[Fracture] Extension activating...');
    vscode.window.showInformationMessage('Fracture Extension Active!');

    try {
        currentConfig = await loadRiftConfig();
        console.log(`[Fracture] Resolved syntax style: ${currentConfig.syntaxStyle} (from ${currentConfig.syntaxSource})`);
    }
    catch (e) {
        console.error('[Fracture] Error loading config:', e);
        currentConfig = {
            project: undefined,
            user: {},
            syntaxStyle: 'fss',
            syntaxSource: 'default'
        };
    }

    const lspEnabled = vscode.workspace.getConfiguration('fracture').get('lsp.enabled', true);

    if (lspEnabled) {
        await startLspClient(context);
    }

    const editorProvider = new FractureEditorProvider(context, lspClient, currentConfig);

    context.subscriptions.push(vscode.window.registerCustomEditorProvider(
        'fracture.editor',
        editorProvider,
        {
            webviewOptions: { retainContextWhenHidden: true },
            supportsMultipleEditorsPerDocument: false
        }
    ));

    console.log('[Fracture] Custom editor registered');

    astProvider = new AstTreeProvider(lspClient, currentConfig);
    diagnosticsProvider = new DiagnosticsProvider(lspClient, currentConfig);
    const projectProvider = new ProjectTreeProvider();

    context.subscriptions.push(
        vscode.window.registerTreeDataProvider('fracture.astView', astProvider),
        vscode.window.registerTreeDataProvider('fracture.diagnosticsView', diagnosticsProvider),
        vscode.window.registerTreeDataProvider('fracture.projectView', projectProvider)
    );

    semanticTokensProvider = new FractureSemanticTokensProvider(lspClient, currentConfig);
    context.subscriptions.push(
        vscode.languages.registerDocumentSemanticTokensProvider(
            { language: 'fracture', scheme: 'file' },
            semanticTokensProvider,
            LEGEND
        )
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.build', async () => {
            const terminal = vscode.window.createTerminal('Fracture Build');
            terminal.sendText('rift build');
            terminal.show();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.run', async () => {
            const terminal = vscode.window.createTerminal('Fracture Run');
            terminal.sendText('rift run');
            terminal.show();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.check', async () => {
            if (lspClient) {
                vscode.window.withProgress({
                    location: vscode.ProgressLocation.Notification,
                    title: 'Fracture: Running type check...',
                    cancellable: false
                }, async () => {
                    const result = await lspClient!.requestFullCheck();
                    if (result.success) {
                        vscode.window.showInformationMessage(
                            `Fracture: Check passed (${result.warningCount} warnings)`
                        );
                    }
                    else {
                        vscode.window.showErrorMessage(
                            `Fracture: Check failed (${result.errorCount} errors, ${result.warningCount} warnings)`
                        );
                    }
                });
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.showAst', () => {
            vscode.commands.executeCommand('fracture.astView.focus');
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.showFss', async () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.uri.fsPath.endsWith('.frac')) {
                await showFssPanel(context, editor.document.uri);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.showRawFile', async () => {
            const editor = vscode.window.activeTextEditor;
            if (editor) {
                await vscode.commands.executeCommand(
                    'vscode.openWith',
                    editor.document.uri,
                    'default'
                );
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.switchSyntax', async () => {
            // Update this when we add more choices
            const choice = await vscode.window.showQuickPick(
                [
                    { label: 'fss', description: 'Fracture Standard Syntax (fn, let, {}, semicolons)' },
                    { label: 'python', description: 'Python-like syntax (def, indentation)' },
                    { label: 'custom', description: 'Custom syntax from config' }
                ],
                { placeHolder: 'Select syntax style' }
            );
            
            if (choice) {
                await saveProjectLocalConfig({
                    syntax: { style: choice.label as 'fss' | 'python' | 'custom' }
                });
                
                currentConfig = await loadRiftConfig();
                
                await refreshAllEditors();
                
                if (editorProvider) {
                    editorProvider.updateConfig(currentConfig);
                }

                if (astProvider) {
                    astProvider.updateConfig(currentConfig);
                }

                if (diagnosticsProvider) {
                    diagnosticsProvider.updateConfig(currentConfig);
                }

                if (semanticTokensProvider) {
                    semanticTokensProvider.updateConfig(currentConfig);
                }
                
                vscode.window.showInformationMessage(
                    `Fracture: Switched to ${choice.label} syntax`
                );
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.restartLsp', async () => {
            if (lspClient) {
                await restartLspClient(context);
                vscode.window.showInformationMessage('Fracture: Language server restarted');
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('fracture.format', async () => {
            if (lspClient && editorProvider) {
                const editor = vscode.window.activeTextEditor;
                if (editor && editor.document.uri.fsPath.endsWith('.frac')) {
                    await editorProvider.formatDocument(editor.document.uri);
                }
            }
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(async (e) => {
            if (e.affectsConfiguration('fracture')) {
                console.log('[Fracture] VSCode configuration changed');
                if (e.affectsConfiguration('fracture.lsp') || 
                    e.affectsConfiguration('fracture.compiler')) {
                    console.log('[Fracture] LSP settings changed, restarting...');
                    await restartLspClient(context);
                }
            }
        })
    );

    const riftWatcher = vscode.workspace.createFileSystemWatcher('**/rift.toml');
    context.subscriptions.push(
        riftWatcher.onDidChange(async () => {
            console.log('[Fracture] rift.toml changed, reloading config');
            currentConfig = await loadRiftConfig();
            await refreshAllEditors();
        }),
        riftWatcher.onDidCreate(async () => {
            currentConfig = await loadRiftConfig();
        }),
        riftWatcher.onDidDelete(async () => {
            currentConfig = await loadRiftConfig();
        })
    );
    context.subscriptions.push(riftWatcher);

    const localConfigWatcher = vscode.workspace.createFileSystemWatcher('**/.rift/config.toml');
    context.subscriptions.push(
        localConfigWatcher.onDidChange(async () => {
            console.log('[Fracture] .rift/config.toml changed, reloading config');
            currentConfig = await loadRiftConfig();
            await refreshAllEditors();
        }),
        localConfigWatcher.onDidCreate(async () => {
            currentConfig = await loadRiftConfig();
            await refreshAllEditors();
        })
    );
    context.subscriptions.push(localConfigWatcher);

    console.log('[Fracture] Extension activated successfully.');
}

export async function deactivate() {
    console.log('[Fracture] Extension deactivating...');

    if (lspClient) {
        await lspClient.stop();
    }

    console.log('[Fracture] Extension deactivated');
}

async function startLspClient(context: vscode.ExtensionContext): Promise<void> {
    try {
        lspClient = new FractureLspClient(context);
        await lspClient.start();
        console.log('[Fracture] LSP client started');
    }
    catch (e) {
        console.error('[Fracture] Failed to start LSP:', e);
        vscode.window.showWarningMessage(
            'Fracture: Language server failed to start. Some features will be unavailable.'
        );
        vscode.window.showWarningMessage(
            'Fracture: Language server failed to start. Falling back to mock mode.'
        );
        // Do not set lspClient to undefined, it was initialized in constructor
        // Just ensure it's in mock mode (which it defaults to or handles internally)
        if (lspClient) {
            // Force mock mode if possible, or just rely on the fact that start() failed
            // Actually, if new FractureLspClient succeeded, we have an instance.
            // We should trust it to handle the failure.
        }
    }
}

async function restartLspClient(context: vscode.ExtensionContext): Promise<void> {
    console.log('[Fracture] Restarting LSP client...');
    
    if (lspClient) {
        await lspClient.stop();
    }
    
    await startLspClient(context);
    
    if (editorProvider) {
        editorProvider.updateLspClient(lspClient);
    }

    if (astProvider) {
        astProvider.updateLspClient(lspClient);
    }

    if (diagnosticsProvider) {
        diagnosticsProvider.updateLspClient(lspClient);
    }

    if (semanticTokensProvider) {
        semanticTokensProvider.updateClient(lspClient);
    }
}

async function refreshAllEditors(): Promise<void> {
    console.log('[Fracture] Refreshing all open editors...');
    
    if (editorProvider) {
        await editorProvider.refreshAllEditors();
    }
    
    if (astProvider) {
        await astProvider.refresh();
    }

    if (diagnosticsProvider) {
        await diagnosticsProvider.refresh();
    }
}

async function showFssPanel(context: vscode.ExtensionContext, uri: vscode.Uri): Promise<void> {
    const panel = vscode.window.createWebviewPanel(
        'fracture.fss',
        `FSS: ${uri.fsPath.split('/').pop()}`,
        vscode.ViewColumn.Beside,
        { enableScripts: true }
    );

    let fss = 'Loading...';
    try {
        const doc = await vscode.workspace.openTextDocument(uri);
        const source = doc.getText();
        
        // Translate to FSS via LSP if available
        if (lspClient && currentConfig) {
            try {
                const syntaxStyle = currentConfig.syntaxStyle;
                fss = await lspClient.syntaxToFss(source, syntaxStyle);
            } catch (e) {
                console.error('[Fracture] Failed to translate to FSS:', e);
                fss = `// Error translating to FSS: ${e}\n// Source syntax style: ${currentConfig?.syntaxStyle}\n\n${source}`;
            }
        } else {
            fss = '// LSP not available, showing source\n\n' + source;
        }
    }
    catch (e) {
        fss = `// Error: Could not read file\n// ${e}`;
    }

    // Escape HTML first
    const escaped = fss
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
    
    // Apply syntax highlighting for FSS
    const highlighted = escaped
        // Comments
        .replace(/^(\s*)(\/\/.*)$/gm, '$1<span class="comment">$2</span>')
        // Keywords: fn, let, pub, etc.
        .replace(/\b(fn|let|mut|pub|use|struct|impl|trait|mod|return|if|else|while|for|match|glyph|shard)\b/g, 
            '<span class="keyword">$1</span>')
        // Types
        .replace(/\b(String|i32|i64|u32|u64|bool|void|f32|f64)\b/g,
            '<span class="type">$1</span>')
        // String literals
        .replace(/"([^"\\]|\\.)*"/g, '<span class="string">$&</span>')
        // Numbers
        .replace(/\b(\d+(?:\.\d+)?)\b/g, '<span class="number">$1</span>')
        // Function names after fn
        .replace(/\b(fn)\s+(\w+)/g, '<span class="keyword">$1</span> <span class="function">$2</span>');

    panel.webview.html = `<!DOCTYPE html>
<html>
<head>
    <style>
        body {
            font-family: 'Fira Code', 'Consolas', monospace;
            padding: 20px;
            background: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
            line-height: 1.6;
        }
        pre {
            white-space: pre-wrap;
            margin: 0;
            font-size: 13px;
        }
        .header {
            margin-bottom: 20px;
            padding: 10px;
            background: var(--vscode-textBlockQuote-background);
            border-left: 4px solid var(--vscode-textLink-foreground);
        }
        /* FSS Syntax Colors */
        .keyword { color: #c586c0; font-weight: bold; }
        .type { color: #4ec9b0; }
        .function { color: #dcdcaa; font-weight: bold; }
        .string { color: #ce9178; }
        .number { color: #b5cea8; }
        .comment { color: #6a9955; font-style: italic; }
    </style>
</head>
<body>
    <div class="header">
        <h3>FSS (Fracture Standard Syntax)</h3>
        <p>Translation of <strong>${uri.fsPath.split('/').pop()}</strong> from <strong>${currentConfig?.syntaxStyle || 'unknown'}</strong> syntax</p>
    </div>
    <pre>${highlighted}</pre>
</body>
</html>`;
}