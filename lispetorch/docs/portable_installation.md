# 🚀 Portable Installation Guide

> **Self-Contained LispETorch Deployment**

The portable installation system automatically packages LispETorch with all dependencies for easy distribution and deployment across different systems.

## 🎯 **Key Features**

- **🔄 Automatic**: Triggered by `make all` command
- **📦 Self-Contained**: Bundles PyTorch, SentencePiece, and system libraries
- **🌐 Cross-Platform**: Works on macOS, Linux, and Windows
- **🔧 Smart Paths**: Automatically fixes conda, Homebrew, and system paths
- **⚡ Zero Configuration**: Works out of the box after installation

## 🚀 **Quick Start**

### Automatic Installation (Recommended)
```bash
cd lispetorch
make all
```

This single command:
1. ✅ Compiles LispETorch
2. ✅ Automatically triggers portable installation
3. ✅ Copies all dependencies to `/usr/local/lib/lispe`
4. ✅ Fixes library paths and rpaths
5. ✅ Creates self-contained deployment

### Manual Installation
```bash
cd lispetorch
make                    # Compile only
make install-portable   # Manual portable installation
```

## 📂 **Installation Structure**

After installation, libraries are organized in `/usr/local/lib/lispe`:

```
/usr/local/lib/lispe/
├── liblispe_torch.so          # Main LispETorch library
├── libtorch.dylib             # PyTorch core library
├── libtorch_cpu.dylib         # PyTorch CPU operations
├── libc10.dylib               # PyTorch tensor library
├── libsentencepiece.dylib     # SentencePiece tokenization
└── libsentencepiece_train.dylib # SentencePiece training
```

## 🔧 **What Gets Automatically Handled**

### 1. Dependency Detection
The system automatically detects and copies:

```bash
# PyTorch libraries (from conda/pip installation)
libtorch.dylib
libtorch_cpu.dylib  
libc10.dylib

# SentencePiece libraries (from Homebrew/conda)
libsentencepiece.dylib
libsentencepiece_train.dylib

# Main library
liblispe_torch.so
```

### 2. Path Correction
Automatically fixes paths from:

**Conda Environments:**
```bash
# From: /Users/user/miniconda3/envs/name/lib/python3.x/site-packages/torch/lib/
# To:   /usr/local/lib/lispe/
```

**Homebrew Installations:**
```bash
# From: /opt/homebrew/lib/
# To:   /usr/local/lib/lispe/
```

**System Libraries:**
```bash
# Updates all internal library references
# Ensures portable deployment
```

### 3. RPATH Management
```bash
# Before: Libraries reference original installation paths
# After:  Libraries reference /usr/local/lib/lispe for portability
```

## 📋 **Installation Output**

### Successful Installation
```bash
=== Compilation terminée, installation portable automatique ===
=== Copie des dépendances PyTorch dans /usr/local/lib/lispe ===
✓ libtorch.dylib copié
✓ libtorch_cpu.dylib copié  
✓ libc10.dylib copié
✓ libsentencepiece.dylib copié
✓ libsentencepiece_train.dylib copié
✓ liblispe_torch.so copié

=== Modification des rpaths pour l'installation portable ===
✓ rpath libtorch.dylib mis à jour
✓ rpath libtorch_cpu.dylib mis à jour
✓ rpath libc10.dylib mis à jour
✓ rpath libsentencepiece.dylib mis à jour
✓ rpath libsentencepiece_train.dylib mis à jour
✓ ID de liblispe_torch.so mis à jour

=== Installation portable terminée ===
LispETorch installé avec toutes les dépendances dans /usr/local/lib/lispe
La bibliothèque peut maintenant être distribuée de manière portable.
```

## 🔍 **Verification Commands**

### Check Installation
```bash
# Verify library installation
ls -la /usr/local/lib/lispe/

# Check library dependencies
otool -L /usr/local/lib/lispe/liblispe_torch.so

# Test library loading
cd lispe && bin/lispe -c "(use 'lispe_torch) (print 'Success)"
```

### Expected Output
```bash
# Library dependencies should show relative paths:
/usr/local/lib/lispe/libtorch.dylib
/usr/local/lib/lispe/libtorch_cpu.dylib
/usr/local/lib/lispe/libc10.dylib
/usr/local/lib/lispe/libsentencepiece.dylib
# NOT absolute paths like /opt/homebrew/lib/...
```

## ⚙️ **Configuration Options**

### Environment Variables
```bash
# Override installation directory
export LISPE_LIB_DIR="/custom/installation/path"
make install-portable

# Skip automatic installation
export SKIP_PORTABLE_INSTALL=1
make all

# Verbose installation output
export VERBOSE_INSTALL=1
make install-portable
```

### Makefile Targets
```bash
# Available targets
make clean                 # Clean build files
make                      # Compile only
make install-portable     # Manual portable installation
make all                  # Compile + automatic portable installation
make check-portable       # Verify portable installation
make diagnose-paths       # Diagnose path issues
```

## 🌐 **Cross-Platform Considerations**

### macOS
- ✅ Homebrew path detection and correction
- ✅ Conda environment path handling
- ✅ System Integrity Protection compatibility
- ✅ Apple Silicon (M1/M2) and Intel support

### Linux
- ✅ System library path detection
- ✅ LD_LIBRARY_PATH handling
- ✅ Distribution-specific paths (Ubuntu, CentOS, etc.)
- ✅ Container deployment support

### Windows
- ✅ DLL dependency bundling
- ✅ PATH environment handling
- ✅ Visual Studio runtime detection
- ✅ MinGW/MSYS2 compatibility

## 🚨 **Troubleshooting**

### Common Issues

**Permission Denied**
```bash
# Solution: Use sudo for /usr/local installation
sudo make install-portable
```

**Library Not Found**
```bash
# Check if libraries were copied
ls -la /usr/local/lib/lispe/

# Verify library paths
otool -L /usr/local/lib/lispe/liblispe_torch.so

# Reinstall if needed
make clean && make all
```

**Path Issues**
```bash
# Diagnose path problems
make diagnose-paths

# Manual path verification
otool -L /usr/local/lib/lispe/*.dylib | grep -v "/usr/local/lib/lispe"
```

**PyTorch Not Found**
```bash
# Ensure PyTorch is installed
python -c "import torch; print(torch.__file__)"

# Check installation path in Makefile
grep TORCH_PATH Makefile

# Manual path specification
export TORCH_PATH=/path/to/your/torch/installation
make install-portable
```

### Error Recovery
```bash
# Clean installation and retry
sudo rm -rf /usr/local/lib/lispe
make clean
make all

# Manual dependency installation
make install-deps-only

# Verify individual components
make test-pytorch
make test-sentencepiece
```

## 📦 **Deployment Options**

### Local Development
```bash
# Standard development setup
make all
# Libraries installed to /usr/local/lib/lispe
```

### Docker Container
```dockerfile
# Dockerfile example
FROM ubuntu:20.04
COPY /usr/local/lib/lispe /usr/local/lib/lispe
COPY bin/lispe /usr/local/bin/
ENV LD_LIBRARY_PATH=/usr/local/lib/lispe:$LD_LIBRARY_PATH
```

### Archive Distribution
```bash
# Create portable archive
tar -czf lispetorch-portable.tar.gz /usr/local/lib/lispe /path/to/lispe/bin

# Deploy on target system
tar -xzf lispetorch-portable.tar.gz -C /
```

### Package Creation
```bash
# macOS package
pkgbuild --root /usr/local/lib/lispe --identifier com.lispe.torch lispetorch.pkg

# Linux package  
fpm -s dir -t deb -n lispetorch -v 1.0 /usr/local/lib/lispe

# Windows installer
# Use NSIS or WiX toolset with bundled libraries
```

## 🔄 **Update Process**

### Updating Libraries
```bash
# Update PyTorch
pip install --upgrade torch

# Reinstall portable version
make clean && make all
```

### Version Management
```bash
# Tag releases
git tag v1.0-portable
git push origin v1.0-portable

# Version verification
/usr/local/lib/lispe/version.txt
```

## 🎯 **Best Practices**

### Development Workflow
1. **Development**: Use `make` for faster compilation during development
2. **Testing**: Use `make all` to test portable installation
3. **Deployment**: Use portable installation for production deployment
4. **Distribution**: Archive `/usr/local/lib/lispe` for distribution

### Performance Optimization
- ✅ Keep libraries in the same directory for faster loading
- ✅ Use SSD storage for library directory
- ✅ Consider RAM disk for ultra-fast access
- ✅ Monitor library loading times

### Security Considerations
- ✅ Verify library signatures after installation
- ✅ Use checksums to verify integrity
- ✅ Regular security updates for dependencies
- ✅ Restrict write access to library directory

---

The portable installation system makes LispETorch deployment simple and reliable across different environments. Use `make all` for automatic setup and enjoy hassle-free distribution!
