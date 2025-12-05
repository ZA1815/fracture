import * as vscode from 'vscode';
import { FractureLspClient } from './lsp/client';
import { ResolvedConfig } from './utils/config';

export const LEGEND = new vscode.SemanticTokensLegend(
    [
        'keyword', 'variable', 'string', 'number', 'function', 
        'struct', 'type', 'operator', 'comment', 'parameter',
        'property', 'enumMember', 'decorator', 'macro',
        'class', 'interface', 'namespace', 'method',
        'typeParameter', 'label', 'regexp', 'event'
    ],
    ['declaration', 'readonly', 'static', 'defaultLibrary', 
     'modification', 'async', 'documentation']
);

export class FractureSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
    constructor(
        private client: FractureLspClient | undefined,
        private config: ResolvedConfig | undefined
    ) {}

    updateClient(client: FractureLspClient | undefined) {
        this.client = client;
    }

    updateConfig(config: ResolvedConfig | undefined) {
        this.config = config;
    }

    async provideDocumentSemanticTokens(
        document: vscode.TextDocument,
        token: vscode.CancellationToken
    ): Promise<vscode.SemanticTokens> {
        if (!this.client) {
            return new vscode.SemanticTokens(new Uint32Array(0));
        }

        try {
            const syntaxStyle = this.config?.syntaxStyle || 'fss';
            const result = await this.client.getSemanticTokens(document.getText(), syntaxStyle);
            
            if (result && result.length > 0) {
                return new vscode.SemanticTokens(new Uint32Array(result));
            }
        } catch (e) {
            console.error('[Fracture] Semantic tokens error:', e);
        }

        return new vscode.SemanticTokens(new Uint32Array(0));
    }
}
