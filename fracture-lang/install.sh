#!/usr/bin/env bash
# Fracture Programming Language Installer
# This script downloads and installs the Fracture toolchain (Linux only)

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

REPO_URL="https://github.com/ZA1815/fracture"
INSTALL_DIR="/usr/local/bin"
TEMP_DIR=$(mktemp -d)
FRACTURE_HOME="${HOME}/.fracture"

detect_platform() {
    local os=$(uname -s | tr '[:upper:]' '[:lower:]')
    local arch=$(uname -m)

    # Only support Linux for now
    case "$os" in
        linux*)
            OS="linux"
            ;;
        darwin*)
            echo -e "${RED}Error: macOS is not currently supported${NC}"
            echo "Fracture currently only targets x86_64 Linux."
            echo "macOS and Windows support is planned for future releases."
            exit 1
            ;;
        mingw* | msys* | cygwin*)
            echo -e "${RED}Error: Windows is not currently supported${NC}"
            echo "Fracture currently only targets x86_64 Linux."
            echo "Please use WSL2 (Windows Subsystem for Linux) to run Fracture."
            exit 1
            ;;
        *)
            echo -e "${RED}Error: Unsupported operating system: $os${NC}"
            echo "Fracture currently only supports x86_64 Linux."
            exit 1
            ;;
    esac

    # Only support x86_64
    case "$arch" in
        x86_64 | amd64)
            ARCH="x86_64"
            ;;
        aarch64 | arm64)
            echo -e "${RED}Error: ARM64 architecture is not currently supported${NC}"
            echo "Fracture currently only targets x86_64 Linux."
            exit 1
            ;;
        *)
            echo -e "${RED}Error: Unsupported architecture: $arch${NC}"
            echo "Fracture currently only targets x86_64 Linux."
            exit 1
            ;;
    esac

    PLATFORM="${ARCH}-${OS}"
    echo -e "${GREEN}✓ Platform: ${PLATFORM}${NC}"
}

check_permissions() {
    if [ ! -w "$INSTALL_DIR" ]; then
        if [ "$EUID" -ne 0 ]; then
            echo -e "${YELLOW}Warning: $INSTALL_DIR is not writable${NC}"
            echo "You may need to run this script with sudo, or install to ~/.local/bin"
            read -p "Install to ~/.local/bin instead? (y/n) " -n 1 -r < /dev/tty
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                INSTALL_DIR="${HOME}/.local/bin"
                mkdir -p "$INSTALL_DIR"
            else
                echo -e "${RED}Installation cancelled${NC}"
                exit 1
            fi
        fi
    fi
}

check_dependencies() {
    echo -e "${BLUE}Checking dependencies...${NC}"

    local missing_deps=()

    if ! command -v cargo &> /dev/null; then
        echo -e "${YELLOW}Rust not found. Fracture requires Rust nightly to build.${NC}"
        missing_deps+=("rust")
    else
        local rust_version=$(rustc --version | awk '{print $2}')
        echo -e "${GREEN}✓ Rust ${rust_version}${NC}"

        if ! rustc --version | grep -q "nightly"; then
            echo -e "${YELLOW}  Note: Fracture requires Rust nightly${NC}"
            if command -v rustup &> /dev/null; then
                echo -e "${BLUE}  Will switch to nightly during build...${NC}"
            fi
        fi
    fi

    if ! command -v nasm &> /dev/null; then
        echo -e "${YELLOW}NASM not found. Fracture requires NASM for assembly.${NC}"
        missing_deps+=("nasm")
    else
        local nasm_version=$(nasm -version | head -1)
        echo -e "${GREEN}✓ ${nasm_version}${NC}"
    fi

    if ! command -v git &> /dev/null; then
        echo -e "${YELLOW}Git not found.${NC}"
        missing_deps+=("git")
    else
        local git_version=$(git --version)
        echo -e "${GREEN}✓ ${git_version}${NC}"
    fi

    if ! command -v ld &> /dev/null; then
        echo -e "${YELLOW}ld (linker) not found. Fracture requires GNU binutils.${NC}"
        missing_deps+=("binutils")
    else
        echo -e "${GREEN}✓ GNU ld${NC}"
    fi

    if [ ${#missing_deps[@]} -ne 0 ]; then
        echo
        echo -e "${RED}Missing required dependencies: ${missing_deps[*]}${NC}"
        echo
        echo "Installation instructions:"

        for dep in "${missing_deps[@]}"; do
            case "$dep" in
                rust)
                    echo -e "${BLUE}  Rust:${NC}"
                    echo "    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
                    echo "    After installation, run: rustup default nightly"
                    ;;
                nasm)
                    echo -e "${BLUE}  NASM:${NC}"
                    echo "    Debian/Ubuntu: sudo apt install nasm"
                    echo "    Fedora:        sudo dnf install nasm"
                    echo "    Arch:          sudo pacman -S nasm"
                    ;;
                git)
                    echo -e "${BLUE}  Git:${NC}"
                    echo "    Debian/Ubuntu: sudo apt install git"
                    echo "    Fedora:        sudo dnf install git"
                    echo "    Arch:          sudo pacman -S git"
                    ;;
                binutils)
                    echo -e "${BLUE}  GNU Binutils:${NC}"
                    echo "    Debian/Ubuntu: sudo apt install binutils"
                    echo "    Fedora:        sudo dnf install binutils"
                    echo "    Arch:          sudo pacman -S binutils"
                    ;;
            esac
        done

        echo
        exit 1
    fi
}

download_fracture() {
    echo -e "${BLUE}Downloading Fracture...${NC}"

    cd "$TEMP_DIR"

    if git clone --depth 1 "$REPO_URL" fracture; then
        echo -e "${GREEN}✓ Repository cloned${NC}"
    else
        echo -e "${RED}Error: Failed to clone repository${NC}"
        echo "Please check your internet connection and try again."
        exit 1
    fi
}

build_fracture() {
    echo -e "${BLUE}Building Fracture toolchain...${NC}"
    echo "This may take a few minutes on the first run..."
    echo

    cd "$TEMP_DIR/fracture"

    if command -v rustup &> /dev/null; then
        echo -e "${BLUE}Switching to Rust nightly...${NC}"
        rustup default nightly 2>&1 | head -3
    fi

    echo -e "${BLUE}Compiling...${NC}"
    if cargo build --release -p fracture-rift 2>&1 | grep -E "(Compiling|Finished|error|warning:)"; then
        echo
        echo -e "${GREEN}✓ Build successful${NC}"
    else
        echo -e "${RED}Error: Build failed${NC}"
        echo "Please check the error messages above."
        echo "You can try building manually:"
        echo "  git clone $REPO_URL"
        echo "  cd fracture"
        echo "  cargo build --release -p fracture-rift"
        exit 1
    fi
}

install_binaries() {
    echo -e "${BLUE}Installing binaries to ${INSTALL_DIR}...${NC}"

    local binary_path="$TEMP_DIR/fracture/target/release/rift"

    if [ ! -f "$binary_path" ]; then
        echo -e "${RED}Error: Binary not found at $binary_path${NC}"
        exit 1
    fi

    mkdir -p "$INSTALL_DIR"

    if cp "$binary_path" "$INSTALL_DIR/rift"; then
        chmod +x "$INSTALL_DIR/rift"
        echo -e "${GREEN}✓ Installed rift to $INSTALL_DIR/rift${NC}"
    else
        echo -e "${RED}Error: Failed to install binary${NC}"
        echo "You may need to run this script with sudo:"
        echo "  curl -fsSL https://raw.githubusercontent.com/ZA1815/fracture/main/fracture-lang/install.sh | sudo bash"
        exit 1
    fi
}

setup_fracture_home() {
    echo -e "${BLUE}Setting up Fracture configuration...${NC}"

    mkdir -p "$FRACTURE_HOME"

    local config_file="$FRACTURE_HOME/config.toml"
    if [ ! -f "$config_file" ]; then
        cat > "$config_file" << EOF
# Fracture Configuration

[global]
default_syntax = "fss"

[paths]
stdlib_path = "${FRACTURE_HOME}/stdlib"

[build]
default_target = "x86_64-linux"
EOF
        echo -e "${GREEN}✓ Created default configuration${NC}"
    else
        echo -e "${BLUE}✓ Configuration already exists${NC}"
    fi

    if [ -d "$TEMP_DIR/fracture/fracture-lang/fracture-stdlib" ]; then
        rm -rf "$FRACTURE_HOME/stdlib" 2>/dev/null || true
        cp -r "$TEMP_DIR/fracture/fracture-lang/fracture-stdlib" "$FRACTURE_HOME/stdlib"
        echo -e "${GREEN}✓ Installed standard library${NC}"
    fi
}

setup_path() {
    if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
        echo
        echo -e "${YELLOW}Note: $INSTALL_DIR is not in your PATH${NC}"
        echo

        local shell_config=""
        if [ -n "$BASH_VERSION" ]; then
            shell_config="$HOME/.bashrc"
        elif [ -n "$ZSH_VERSION" ]; then
            shell_config="$HOME/.zshrc"
        elif [ -f "$HOME/.bashrc" ]; then
            shell_config="$HOME/.bashrc"
        elif [ -f "$HOME/.zshrc" ]; then
            shell_config="$HOME/.zshrc"
        elif [ -f "$HOME/.profile" ]; then
            shell_config="$HOME/.profile"
        fi

        if [ -n "$shell_config" ]; then
            echo "To use Fracture, add the following line to $shell_config:"
            echo
            if [ "$INSTALL_DIR" = "${HOME}/.local/bin" ]; then
                echo -e "${GREEN}  export PATH=\"\$HOME/.local/bin:\$PATH\"${NC}"
            else
                echo -e "${GREEN}  export PATH=\"$INSTALL_DIR:\$PATH\"${NC}"
            fi
            echo
            read -p "Would you like me to add it automatically? (y/n) " -n 1 -r < /dev/tty
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                echo "" >> "$shell_config"
                echo "# Fracture" >> "$shell_config"
                if [ "$INSTALL_DIR" = "${HOME}/.local/bin" ]; then
                    echo "export PATH=\"\$HOME/.local/bin:\$PATH\"" >> "$shell_config"
                else
                    echo "export PATH=\"$INSTALL_DIR:\$PATH\"" >> "$shell_config"
                fi
                echo -e "${GREEN}✓ Added to $shell_config${NC}"
                echo
                echo "To use rift in this session, run:"
                echo -e "${GREEN}  source $shell_config${NC}"
                echo "Or open a new terminal."
            fi
        fi
    fi
}

cleanup() {
    if [ -d "$TEMP_DIR" ]; then
        rm -rf "$TEMP_DIR"
    fi
}

verify_installation() {
    echo
    echo -e "${BLUE}Verifying installation...${NC}"

    export PATH="$INSTALL_DIR:$PATH"

    if command -v rift &> /dev/null; then
        local version=$(rift --version 2>&1 || echo "unknown")
        echo -e "${GREEN}✓ Fracture installed successfully!${NC}"
        echo -e "  Version: $version"
        echo
        echo -e "${BLUE}Quick start:${NC}"
        echo "  rift init my_project"
        echo "  cd my_project"
        echo "  rift build"
        echo "  rift run"
        echo
    else
        echo -e "${YELLOW}Installation complete, but 'rift' is not in your PATH yet.${NC}"
        echo "Please restart your terminal or run:"
        echo "  source ~/.bashrc  (or ~/.zshrc)"
    fi
}

main() {
    echo
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}  Fracture Programming Language Installer${NC}"
    echo -e "${GREEN}  x86_64 Linux Only (Proof of Concept)    ${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo

    detect_platform
    check_permissions
    check_dependencies
    download_fracture
    build_fracture
    install_binaries
    setup_fracture_home
    setup_path
    verify_installation

    echo
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Installation complete! 🎉${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo
    echo "Learn more:"
    echo "  Repository: https://github.com/ZA1815/fracture"
    echo "  Documentation: https://github.com/ZA1815/fracture/tree/main/fracture-lang"
    echo "  Report issues: https://github.com/ZA1815/fracture/issues"
    echo
}

trap cleanup EXIT

main "$@"
