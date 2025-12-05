use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};
use sha2::{Sha256, Digest};
use git2::Repository;
use colored::*;

use crate::manifest::{Manifest, DependencySpec};
use crate::lockfile::{ResolvedGraph, ResolvedPackage, PackageSource};

pub struct Resolver {
    cache_dir: PathBuf
}

impl Resolver {
    pub fn new() -> Self {
        let cache_dir = dirs::home_dir()
            .expect("Could not determine home directory")
            .join(".rift")
            .join("cache")
            .join("git");

        std::fs::create_dir_all(&cache_dir).ok();

        Self { cache_dir }
    }

    pub fn resolve(&self, root_manifest: &Manifest, root_path: &Path) -> Result<ResolvedGraph, String> {
        let mut resolved_packages = HashMap::new();
        let mut queue = VecDeque::new();

        for (name, spec) in &root_manifest.dependencies {
            queue.push_back((name.clone(), spec.clone(), true, root_path.to_path_buf()));
        }

        while let Some((name, spec, is_direct, parent_path)) = queue.pop_front() {
            if resolved_packages.contains_key(&name) {
                continue;
            }

            println!("{} Resolving {}", "→".blue(), name);

            let (pkg_source, install_path, version) = match spec {
                DependencySpec::Detailed(details) => {
                    if let Some(git_url) = details.git {
                        let (path, commit) = self.fetch_git(&name, &git_url, details.rev.as_deref())?;
                        
                        let m = Manifest::from_file(&path.join("rift.toml"))
                            .map_err(|e| format!("Bad manifest in git dependency '{}': {}", name, e))?;

                        (PackageSource::Git { url: git_url, commit }, path, m.package.version)
                    } 
                    else if let Some(rel_path) = details.path {
                        let path = parent_path.join(&rel_path)
                            .canonicalize()
                            .map_err(|e| format!("Failed to resolve path '{}' for package '{}': {}", rel_path, name, e))?;

                        if !path.exists() {
                            return Err(format!("Path dependency '{}' does not exist at {}", name, path.display()));
                        }

                        let m = Manifest::from_file(&path.join("rift.toml"))
                            .map_err(|e| format!("Bad manifest in path dep '{}': {}", name, e))?;
                        
                        (PackageSource::Path { path: path.to_string_lossy().to_string() }, path, m.package.version)
                    } 
                    else {
                        return Err(format!("Dependency '{}' must specify 'git' or 'path'", name));
                    }
                }
                DependencySpec::Simple(ver) => {
                    return Err(format!(
                        "Registry not implemented yet. Cannot fetch '{}' @ '{}'.", 
                        name, ver
                    ));
                }
            };

            let dep_manifest_path = install_path.join("rift.toml");
            let dep_manifest = Manifest::from_file(&dep_manifest_path)
                .map_err(|e| format!("Failed to read manifest for {}: {}", name, e))?;

            for (sub_name, sub_spec) in &dep_manifest.dependencies {
                queue.push_back((sub_name.clone(), sub_spec.clone(), false, install_path.clone()));
            }

            let pkg = ResolvedPackage {
                name: name.clone(),
                version,
                source: pkg_source,
                dependencies: dep_manifest.dependencies.keys().cloned().collect(),
                is_direct,
            };

            resolved_packages.insert(name, pkg);
        }

        let mut graph = ResolvedGraph::new();
        for (_, pkg) in resolved_packages {
            graph.add(pkg);
        }

        Ok(graph)
    }

    fn fetch_git(&self, name: &str, url: &str, rev: Option<&str>) -> Result<(PathBuf, String), String> {
        let mut hasher = Sha256::new();
        hasher.update(url.as_bytes());
        let hash = hex::encode(hasher.finalize());
        let folder_name = format!("{}-{}", name, &hash[0..8]); 
        let dest_path = self.cache_dir.join(&folder_name);

        let repo = if dest_path.exists() {
            let repo = Repository::open(&dest_path)
                .map_err(|e| format!("Failed to open cached repo: {}", e))?;
            
            // Only fetch if we aren't sure we have the commit (optimization possible later)
            // For now, let's assume we might need updates if it's not pinned to a specific hash
            if rev.is_none() { 
                 let mut remote = repo.find_remote("origin")
                    .map_err(|e| format!("Git remote error: {}", e))?;
                 remote.fetch(&["master"], None, None).ok(); // Ignore fetch errors if offline
            }
            repo
        } else {
            println!("  Cloning {}...", url);
            Repository::clone(url, &dest_path)
                .map_err(|e| format!("Failed to clone {}: {}", url, e))?
        };

        let (object, _) = if let Some(revision) = rev {
            let obj = repo.revparse_single(revision)
                .map_err(|e| format!("Rev '{}' not found: {}", revision, e))?;
            (obj, Some(revision))
        } else {
            let head = repo.head().map_err(|e| format!("No HEAD: {}", e))?;
            (head.peel_to_commit().unwrap().into_object(), None)
        };

        repo.checkout_tree(&object, None)
            .map_err(|e| format!("Checkout failed: {}", e))?;
            
        repo.set_head_detached(object.id())
             .map_err(|e| format!("Detach failed: {}", e))?;

        Ok((dest_path, object.id().to_string()))
    }
}