# GnuCOBOL Installation Guide

This guide provides detailed instructions for installing GnuCOBOL on various operating systems.

## macOS Installation

### Option 1: Using Homebrew (Recommended)

1. **Install Homebrew** (if not already installed):
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. **Install GnuCOBOL**:
   ```bash
   brew install gnu-cobol
   ```

3. **Verify Installation**:
   ```bash
   cobc --version
   ```
   
   Expected output:
   ```
   cobc (GnuCOBOL) 3.x.x
   ```

### Option 2: Using MacPorts

1. **Install MacPorts** from https://www.macports.org/

2. **Install GnuCOBOL**:
   ```bash
   sudo port install gnucobol
   ```

3. **Verify Installation**:
   ```bash
   cobc --version
   ```

## Linux Installation

### Ubuntu/Debian

```bash
# Update package list
sudo apt-get update

# Install GnuCOBOL
sudo apt-get install -y gnucobol

# Verify installation
cobc --version
```

### Fedora/RHEL/CentOS

```bash
# Install GnuCOBOL
sudo dnf install gnucobol

# Or for older versions
sudo yum install gnucobol

# Verify installation
cobc --version
```

### Arch Linux

```bash
# Install from AUR
yay -S gnucobol

# Or using pacman if available
sudo pacman -S gnucobol

# Verify installation
cobc --version
```

## Windows Installation

### Option 1: Using WSL (Windows Subsystem for Linux) - Recommended

1. **Enable WSL**:
   ```powershell
   wsl --install
   ```

2. **Install Ubuntu from Microsoft Store**

3. **Follow Ubuntu installation steps** (see above)

### Option 2: Native Windows Installation

1. **Download GnuCOBOL** from:
   - https://sourceforge.net/projects/gnucobol/
   - Or https://www.arnoldtrembley.com/GnuCOBOL.htm

2. **Run the installer** and follow the prompts

3. **Add to PATH**:
   - Right-click "This PC" → Properties
   - Advanced system settings → Environment Variables
   - Add GnuCOBOL bin directory to PATH

4. **Verify Installation** (in Command Prompt):
   ```cmd
   cobc --version
   ```

## Building from Source

If you need the latest version or your distribution doesn't have a package:

### Prerequisites

```bash
# Ubuntu/Debian
sudo apt-get install build-essential libgmp-dev libdb-dev libncurses5-dev

# macOS
brew install gmp berkeley-db ncurses

# Fedora/RHEL
sudo dnf install gcc gmp-devel db-devel ncurses-devel
```

### Build Steps

```bash
# Download source
wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2.tar.gz
tar -xzf gnucobol-3.2.tar.gz
cd gnucobol-3.2

# Configure
./configure --prefix=/usr/local

# Build
make

# Install
sudo make install

# Verify
cobc --version
```

## Post-Installation

### Set Environment Variables (Optional)

Add to your `~/.bashrc` or `~/.zshrc`:

```bash
# GnuCOBOL settings
export COB_CFLAGS="-Wall -Wextra"
export COB_LDFLAGS="-lm"
export COB_CONFIG_DIR=/usr/local/share/gnucobol/config
```

### Test Installation

Create a simple test program:

```bash
cat > hello.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello, COBOL!".
           STOP RUN.
EOF

# Compile
cobc -x -o hello hello.cbl

# Run
./hello
```

Expected output: `Hello, COBOL!`

## Troubleshooting

### Issue: `cobc: command not found`

**Solution**: 
- Verify installation completed successfully
- Check if GnuCOBOL is in your PATH
- Restart your terminal

### Issue: Compilation errors with missing libraries

**Solution**:
```bash
# Ubuntu/Debian
sudo apt-get install libgmp-dev libdb-dev

# macOS
brew install gmp berkeley-db

# Fedora/RHEL
sudo dnf install gmp-devel db-devel
```

### Issue: Permission denied when running compiled programs

**Solution**:
```bash
chmod +x your-program
```

## Version Requirements

This project requires:
- **GnuCOBOL 3.0 or higher**
- **COBOL-85 standard support**

Check your version:
```bash
cobc --version
cobc --info
```

## Additional Resources

- **Official Website**: https://gnucobol.sourceforge.io/
- **Documentation**: https://gnucobol.sourceforge.io/doc/gnucobol.html
- **GitHub**: https://github.com/OCamlPro/gnucobol
- **Mailing List**: https://sourceforge.net/p/gnucobol/mailman/

## Next Steps

After successful installation:

1. **Navigate to project directory**:
   ```bash
   cd /Users/nehaniharikakar/Documents/cobol-calculator
   ```

2. **Build the project**:
   ```bash
   ./build.sh
   ```

3. **Run tests**:
   ```bash
   ./test-runner.sh
   ```

---

**Need Help?** Check the [README.md](README.md) for project-specific instructions.