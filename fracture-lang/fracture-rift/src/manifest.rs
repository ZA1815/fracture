use fracture_ir::SyntaxConfig;
use serde::{Serialize, Deserialize};
use semver::{Version, VersionReq};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: Package,

    #[serde(default)]
    pub dependencies: HashMap<String, DependencySpec>,
    
    #[serde(default)]
    pub dev_dependencies: HashMap<String, DependencySpec>,

    #[serde(default)]
    pub syntax: SyntaxSection,

    #[serde(default)]
    pub compiler: CompilerSection
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,

    #[serde(default)]
    pub authors: Vec<String>,

    #[serde(default)]
    pub description: Option<String>,

    #[serde(default)]
    pub license: Option<String>,

    #[serde(default)]
    pub repository: Option<String>,

    #[serde(default)]
    pub entry: Option<String>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    Simple(String),
    Detailed(DetailedDependency)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetailedDependency {
    #[serde(default)]
    pub version: Option<String>,

    #[serde(default)]
    pub git: Option<String>,

    #[serde(default)]
    pub branch: Option<String>,

    #[serde(default)]
    pub tag: Option<String>,

    #[serde(default)]
    pub rev: Option<String>,

    #[serde(default)]
    pub path: Option<String>,

    #[serde(default)]
    pub optional: bool,

    #[serde(default)]
    pub features: Vec<String>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompilerSection {
    #[serde(default = "default_mode")]
    pub mode: String,

    #[serde(default)]
    pub optimization: u8,

    #[serde(default)]
    pub target: Option<String>
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SyntaxSection {
    #[serde(default)]
    pub source_style: Option<String>,
    
    #[serde(default)]
    pub source_config: Option<String>,
}

fn default_mode() -> String {
    "safe".to_string()
}

impl Default for CompilerSection {
    fn default() -> Self {
        Self {
            mode: "safe".to_string(),
            optimization: 0,
            target: None
        }
    }
}

// Improve errors later
impl Manifest {
    
    pub fn from_file(path: &Path) -> Result<Self, String> {
        let contents = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

        Self::from_str(&contents)
    }

    pub fn from_str(contents: &str) -> Result<Self, String> {
        let manifest: Manifest = toml::from_str(contents)
            .map_err(|e| format!("Failed to parse manifest: {}", e))?;

        manifest.validate()?;

        Ok(manifest)
    }

    pub fn validate(&self) -> Result<(), String> {
        if self.package.name.is_empty() {
            return Err("Package name cannot be empty".to_string());
        }

        if !self.package.name.chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-' || c == '_') {
            return Err(format!(
                "Package name '{}' contains invalid characters. Use lowercase letters, numbers, hyphens, or underscores.",
                self.package.name
            ));
        }

        Version::parse(&self.package.version)
            .map_err(|e| format!("Invalid version: '{}': {}", self.package.version, e))?;

        for (name, spec) in &self.dependencies {
            self.validate_dependency_spec(name, spec)?;
        }

        Ok(())
    }

    fn validate_dependency_spec(&self, name: &str, spec: &DependencySpec) -> Result<(), String> {
        match spec {
            DependencySpec::Simple(version) => {
                VersionReq::parse(&version)
                    .map_err(|e| format!("Invalid version '{}' for depencency '{}': {}", version, name, e))?;
            }
            DependencySpec::Detailed(detailed) => {
                let sources = [
                    detailed.version.is_some(),
                    detailed.git.is_some(),
                    detailed.path.is_some()
                ];

                let source_count = sources.iter().filter(|&&x| x).count();

                if source_count == 0 {
                    return Err(format!(
                        "Dependency '{}' must specify version, git, or path",
                        name
                    ));
                }

                if source_count > 1 {
                    return Err(format!(
                        "Dependency '{}' has multiple sources. Specify only one of: version, git, path",
                        name
                    ));
                }

                if let Some(version) = &detailed.version {
                    VersionReq::parse(version)
                        .map_err(|e| format!("Invalid version '{}' for dependency '{}': {}", version, name, e))?;
                }
            }
        }

        Ok(())
    }

    #[allow(dead_code)]
    fn to_toml_string(&self) -> Result<String, String> {
        toml::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize manifest: {}", e))
    }

    pub fn version(&self) -> Result<Version, String> {
        Version::parse(&self.package.version)
            .map_err(|e| format!("Invalid version: {}", e))
    }

    pub fn entry_point(&self, project_root: &Path) -> Option<PathBuf> {
        if let Some(entry) = &self.package.entry {
            let path = project_root.join(entry);
            if path.exists() {
                return Some(path);
            }
        }

        let candidates = [
            "src/main.frac",
            "src/lib.frac",
            "main.frac"
        ];

        for candidate in candidates {
            let path = project_root.join(candidate);
            if path.exists() {
                return Some(path);
            }
        }

        None
    }
}

impl Manifest {
    pub fn new_project(name: &str) -> Self {
        Self {
            package: Package {
                name: name.to_string(),
                version: "0.1.0".to_string(),
                authors: Vec::new(),
                description: None,
                license: None,
                repository: None,
                entry: None,
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            syntax: SyntaxSection::default(),
            compiler: CompilerSection::default()
        }
    }

    pub fn new_project_with_style(name: &str, style: &str) -> Self {
        let mut manifest = Self::new_project(name);
        manifest.syntax.source_style = Some(style.to_string());

        manifest
    }

    pub fn has_syntax_config(&self) -> bool {
        self.syntax.source_style.is_some() || self.syntax.source_config.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    /// Test parsing a minimal valid manifest
    #[test]
    fn test_parse_minimal() {
        let toml = r#"
            [package]
            name = "test-project"
            version = "0.1.0"
        "#;
        
        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.package.name, "test-project");
        assert_eq!(manifest.package.version, "0.1.0");
    }
    
    /// Test parsing with dependencies
    #[test]
    fn test_parse_with_deps() {
        let toml = r#"
            [package]
            name = "my-app"
            version = "1.0.0"
            
            [dependencies]
            some-lib = "2.0.0"
            other-lib = { version = "1.0", features = ["json"] }
        "#;
        
        let manifest = Manifest::from_str(toml).unwrap();
        assert!(manifest.dependencies.contains_key("some-lib"));
        assert!(manifest.dependencies.contains_key("other-lib"));
    }
    
    /// Test that invalid package names are rejected
    #[test]
    fn test_invalid_name() {
        let toml = r#"
            [package]
            name = "Invalid Name!"
            version = "0.1.0"
        "#;
        
        let result = Manifest::from_str(toml);
        assert!(result.is_err());
    }
    
    /// Test that invalid versions are rejected
    #[test]
    fn test_invalid_version() {
        let toml = r#"
            [package]
            name = "valid-name"
            version = "not-a-version"
        "#;
        
        let result = Manifest::from_str(toml);
        assert!(result.is_err());
    }
}