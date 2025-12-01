import * as vscode from 'vscode';
import { loadRiftConfig, RiftConfig, getDependencyVersion, isGitDependency, isPathDependency } from '../utils/config';

export class ProjectTreeProvider implements vscode.TreeDataProvider<ProjectItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<ProjectItem | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private config: RiftConfig | undefined;

    constructor() {
        this.loadConfig();

        const watcher = vscode.workspace.createFileSystemWatcher('**/rift.toml');
        watcher.onDidChange(() => this.loadConfig());
        watcher.onDidCreate(() => this.loadConfig());
        watcher.onDidDelete(() => this.loadConfig());
    }

    private async loadConfig(): Promise<void> {
        const resolved = await loadRiftConfig();
        this.config = resolved.project;
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: ProjectItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: ProjectItem): Thenable<ProjectItem[]> {
        if (!element) {
            if (!this.config) {
                return Promise.resolve([
                    new ProjectItem(
                        'No rift.toml found',
                        'Create one with `rift init`',
                        vscode.TreeItemCollapsibleState.None,
                        'info'
                    )
                ]);
            }

            const items: ProjectItem[] = [];

            if (this.config.package) {
                items.push(new ProjectItem(
                    'Package',
                    this.config.package.name,
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'package'
                ));
            }

            if (this.config.syntax) {
                items.push(new ProjectItem(
                    'Syntax',
                    this.config.syntax.style,
                    vscode.TreeItemCollapsibleState.None,
                    'syntax'
                ));
            }

            if (this.config.dependencies && Object.keys(this.config.dependencies).length > 0) {
                items.push(new ProjectItem(
                    'Dependencies',
                    `${Object.keys(this.config.dependencies).length} packages`,
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'dependencies'
                ));
            }

            if (this.config.devDependencies && Object.keys(this.config.devDependencies).length > 0) {
                items.push(new ProjectItem(
                    'Dev Dependencies',
                    `${Object.keys(this.config.devDependencies).length} packages`,
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'devDependencies'
                ));
            }

            if (this.config.build) {
                items.push(new ProjectItem(
                    'Build',
                    this.config.build.optimization || 'debug',
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'build'
                ));
            }

            return Promise.resolve(items);
        }

        switch (element.itemType) {
            case 'package':
                return this.getPackageChildren();
            case 'dependencies':
                return this.getDependencyChildren(this.config?.dependencies || {});
            case 'devDependencies':
                return this.getDependencyChildren(this.config?.devDependencies || {});
            case 'build':
                return this.getBuildChildren();
        }
        
        return Promise.resolve([]);
    }

    private getPackageChildren(): Promise<ProjectItem[]> {
        if (!this.config?.package) {
            return Promise.resolve([]);
        }

        const pkg = this.config.package;
        const items: ProjectItem[] = [];

        items.push(new ProjectItem(
            'name',
            pkg.name,
            vscode.TreeItemCollapsibleState.None,
            'property'
        ));

        items.push(new ProjectItem(
            'version',
            pkg.version,
            vscode.TreeItemCollapsibleState.None,
            'property'
        ));

        if (pkg.authors && pkg.authors.length > 0) {
            items.push(new ProjectItem(
                'authors',
                pkg.authors.join(', '),
                vscode.TreeItemCollapsibleState.None,
                'property'
            ));
        }

        if (pkg.description) {
            items.push(new ProjectItem(
                'description',
                pkg.description,
                vscode.TreeItemCollapsibleState.None,
                'property'
            ));
        }

        if (pkg.license) {
            items.push(new ProjectItem(
                'license',
                pkg.license,
                vscode.TreeItemCollapsibleState.None,
                'property'
            ));
        }

        return Promise.resolve(items);
    }

    private getDependencyChildren(deps: { [name: string]: any }): Promise<ProjectItem[]> {
        const items: ProjectItem[] = [];

        for (const [name, spec] of Object.entries(deps)) {
            const version = getDependencyVersion(spec);
            let icon: string = 'package';

            if (isGitDependency(spec)) {
                icon = 'github';
            }
            else if (isPathDependency(spec)) {
                icon = 'folder';
            }

            items.push(new ProjectItem(
                name,
                version,
                vscode.TreeItemCollapsibleState.None,
                icon as any
            ));
        }

        return Promise.resolve(items);
    }

    private getBuildChildren(): Promise<ProjectItem[]> {
        if (!this.config?.build) {
            return Promise.resolve([]);
        }
        
        const build = this.config.build;
        const items: ProjectItem[] = [];
        
        if (build.output) {
            items.push(new ProjectItem(
                'output',
                build.output,
                vscode.TreeItemCollapsibleState.None,
                'property'
            ));
        }
        
        if (build.optimization) {
            items.push(new ProjectItem(
                'optimization',
                build.optimization,
                vscode.TreeItemCollapsibleState.None,
                'property'
            ));
        }
        
        if (build.target) {
            items.push(new ProjectItem(
                'target',
                build.target,
                vscode.TreeItemCollapsibleState.None,
                'property'
            ));
        }
        
        return Promise.resolve(items);
    }
}

class ProjectItem extends vscode.TreeItem {
    constructor(
        label: string,
        description: string | undefined,
        collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly itemType: 'package' | 'syntax' | 'dependencies' | 'devDependencies' | 'build' | 'property' | 'info' | 'github' | 'folder'
    ) {
        super(label, collapsibleState);
        
        this.description = description;
        
        // Set icon based on type
        switch (itemType) {
            case 'package':
                this.iconPath = new vscode.ThemeIcon('package');
                break;
            case 'syntax':
                this.iconPath = new vscode.ThemeIcon('code');
                break;
            case 'dependencies':
            case 'devDependencies':
                this.iconPath = new vscode.ThemeIcon('library');
                break;
            case 'build':
                this.iconPath = new vscode.ThemeIcon('tools');
                break;
            case 'property':
                this.iconPath = new vscode.ThemeIcon('symbol-property');
                break;
            case 'info':
                this.iconPath = new vscode.ThemeIcon('info');
                break;
            case 'github':
                this.iconPath = new vscode.ThemeIcon('github');
                break;
            case 'folder':
                this.iconPath = new vscode.ThemeIcon('folder');
                break;
        }
    }
}