import * as vscode from 'vscode';
import * as toml from 'toml';
import * as os from 'os';
import * as path from 'path';

export interface RiftConfig {
    package?: PackageConfig;
    syntax?: SyntaxConfig;
    dependencies?: { [name: string]: DependencySpec };
    devDependencies?: { [name: string]: DependencySpec };
    build?: BuildConfig;
}

export interface UserConfig {
    syntax?: SyntaxConfig;
    editor?: EditorConfig;
    lsp?: LspConfig;
}

export interface PackageConfig {
    name: string;
    version: string;
    authors?: string[];
    description?: string;
    license?: string;
    repository?: string;
}

export interface SyntaxConfig {
    style: 'rust' | 'python' | 'custom';
    custom?: CustomSyntaxConfig;
}

export interface CustomSyntaxConfig {
    function_keyword?: string;
    variable_keyword?: string;
    block_style?: 'braces' | 'indent';
    semicolons?: boolean;
    type_annotation_style?: 'rust' | 'python' | 'typescript';
}

export interface EditorConfig {
    font_size?: number;
    theme?: 'auto' | 'light' | 'dark';
    show_fss_panel?: boolean;
    show_ast_panel?: boolean;
}

export interface LspConfig {
    enabled?: boolean;
    trace?: 'off' | 'messages' | 'verbose';
}

export type DependencySpec = string | GitDependency | PathDependency;

export interface GitDependency {
    git: string;
    branch?: string;
    tag?: string;
    rev?: string;
}

export interface PathDependency {
    path: string;
}

export interface BuildConfig {
    output?: string;
    optimization?: 'debug' | 'release';
    target?: string;
}

export interface ResolvedConfig {
    project: RiftConfig | undefined;
    user: UserConfig;
    syntaxStyle: string;
    syntaxSource: 'project-local' | 'global' | 'rift.toml' | 'default';
}

async function loadTomlFile<T>(uri: vscode.Uri): Promise<T | undefined> {
    try {
        const content = await vscode.workspace.fs.readFile(uri);
        const text = new TextDecoder().decode(content);
        return toml.parse(text) as T;
    }
    catch (error) {
        if (error instanceof Error && !error.message.includes('ENOENT')) {
            console.error(`[Fracture] Error parsing ${uri.fsPath}:`, error);
        }
        return undefined;
    }
}

export async function loadGlobalConfig(): Promise<UserConfig | undefined> {
    const homeDir = os.homedir();
    const globalConfigPath = vscode.Uri.file(path.join(homeDir, '.rift', 'config.toml'));
    return loadTomlFile<UserConfig>(globalConfigPath);
}

export async function loadProjectLocalConfig(): Promise<UserConfig | undefined> {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders) {
        return undefined;
    }
    
    const localConfigPath = vscode.Uri.joinPath(
        workspaceFolders[0].uri, 
        '.rift', 
        'config.toml'
    );
    return loadTomlFile<UserConfig>(localConfigPath);
}

export async function loadProjectManifest(): Promise<RiftConfig | undefined> {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders) {
        return undefined;
    }
    
    const riftPath = vscode.Uri.joinPath(workspaceFolders[0].uri, 'rift.toml');
    const config = await loadTomlFile<RiftConfig>(riftPath);
    
    if (config) {
        return normalizeProjectConfig(config);
    }
    return undefined;
}

export async function loadRiftConfig(): Promise<ResolvedConfig> {
    console.log('[Fracture] Loading configuration hierarchy...');
    
    const [projectManifest, projectLocalConfig, globalConfig] = await Promise.all([
        loadProjectManifest(),
        loadProjectLocalConfig(),
        loadGlobalConfig()
    ]);
    
    if (projectManifest) {
        console.log(`[Fracture] Found rift.toml: ${projectManifest.package?.name || 'unnamed'}`);
    }
    if (projectLocalConfig) {
        console.log('[Fracture] Found .rift/config.toml (project-local)');
    }
    if (globalConfig) {
        console.log('[Fracture] Found ~/.rift/config.toml (global)');
    }
    
    const mergedUserConfig: UserConfig = {
        ...globalConfig,
        ...projectLocalConfig,
        syntax: projectLocalConfig?.syntax || globalConfig?.syntax,
        editor: { ...globalConfig?.editor, ...projectLocalConfig?.editor },
        lsp: { ...globalConfig?.lsp, ...projectLocalConfig?.lsp }
    };
    
    let syntaxStyle: string;
    let syntaxSource: ResolvedConfig['syntaxSource'];
    
    if (projectManifest?.syntax?.style) {
        syntaxStyle = projectManifest.syntax.style;
        syntaxSource = 'rift.toml';
        console.log(`[Fracture] Using syntax from rift.toml: ${syntaxStyle}`);
    }
    else if (projectLocalConfig?.syntax?.style) {
        syntaxStyle = projectLocalConfig.syntax.style;
        syntaxSource = 'project-local';
        console.log(`[Fracture] Using syntax from .rift/config.toml: ${syntaxStyle}`);
    }
    else if (globalConfig?.syntax?.style) {
        syntaxStyle = globalConfig.syntax.style;
        syntaxSource = 'global';
        console.log(`[Fracture] Using syntax from ~/.rift/config.toml: ${syntaxStyle}`);
    }
    else {
        syntaxStyle = 'rust';
        syntaxSource = 'default';
        console.log('[Fracture] Using default syntax: rust');
    }
    
    return {
        project: projectManifest,
        user: mergedUserConfig,
        syntaxStyle,
        syntaxSource
    };
}

export async function loadProjectConfig(): Promise<RiftConfig | undefined> {
    return loadProjectManifest();
}

function normalizeProjectConfig(config: RiftConfig): RiftConfig {
    if (!config.syntax) {
        config.syntax = { style: 'rust' };
    }
    
    if (!['rust', 'python', 'custom'].includes(config.syntax.style)) {
        console.warn(`[Fracture] Unknown syntax style: ${config.syntax.style}, defaulting to 'rust'`);
        config.syntax.style = 'rust';
    }
    
    if (config.syntax.style === 'custom' && !config.syntax.custom) {
        config.syntax.custom = {
            function_keyword: 'fn',
            variable_keyword: 'let',
            block_style: 'braces',
            semicolons: true
        };
    }
    
    return config;
}

export function getSyntaxStyle(config: ResolvedConfig | undefined): string {
    return config?.syntaxStyle || 'rust';
}

export function isGitDependency(dep: DependencySpec): dep is GitDependency {
    return typeof dep === 'object' && 'git' in dep;
}

export function isPathDependency(dep: DependencySpec): dep is PathDependency {
    return typeof dep === 'object' && 'path' in dep;
}

export function getDependencyVersion(dep: DependencySpec): string {
    if (typeof dep === 'string') {
        return dep;
    }

    else if (isGitDependency(dep)) {
        return dep.tag || dep.branch || dep.rev || 'latest';
    }

    else if (isPathDependency(dep)) {
        return `path: ${dep.path}`;
    }

    return 'unknown';
}

export async function saveProjectLocalConfig(config: Partial<UserConfig>): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders) {
        throw new Error('No workspace folder open');
    }
    
    const riftDir = vscode.Uri.joinPath(workspaceFolders[0].uri, '.rift');
    const configPath = vscode.Uri.joinPath(riftDir, 'config.toml');
    
    try {
        await vscode.workspace.fs.createDirectory(riftDir);
    }
    catch {
        // Directory might already exist
    }
    
    const existing = await loadProjectLocalConfig() || {};
    const merged = {
        ...existing,
        ...config,
        syntax: config.syntax || existing.syntax
    };
    
    const tomlContent = serializeToml(merged);
    
    await vscode.workspace.fs.writeFile(configPath, Buffer.from(tomlContent));
    console.log('[Fracture] Saved project-local config to .rift/config.toml');
}

function serializeToml(config: UserConfig): string {
    const lines: string[] = [
        '# Fracture project-local user configuration',
        '# This file overrides your global ~/.rift/config.toml for this project',
        ''
    ];
    
    if (config.syntax) {
        lines.push('[syntax]');
        lines.push(`style = "${config.syntax.style}"`);
        if (config.syntax.custom) {
            lines.push('');
            lines.push('[syntax.custom]');
            const custom = config.syntax.custom;
            if (custom.function_keyword) lines.push(`function_keyword = "${custom.function_keyword}"`);
            if (custom.variable_keyword) lines.push(`variable_keyword = "${custom.variable_keyword}"`);
            if (custom.block_style) lines.push(`block_style = "${custom.block_style}"`);
            if (custom.semicolons !== undefined) lines.push(`semicolons = ${custom.semicolons}`);
        }
        lines.push('');
    }
    
    if (config.editor) {
        lines.push('[editor]');
        if (config.editor.font_size) lines.push(`font_size = ${config.editor.font_size}`);
        if (config.editor.theme) lines.push(`theme = "${config.editor.theme}"`);
        if (config.editor.show_fss_panel !== undefined) lines.push(`show_fss_panel = ${config.editor.show_fss_panel}`);
        if (config.editor.show_ast_panel !== undefined) lines.push(`show_ast_panel = ${config.editor.show_ast_panel}`);
        lines.push('');
    }
    
    return lines.join('\n');
}