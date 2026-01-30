# LINUX_COMPILING.md

## Overview

This document describes the compilation and setup process for LispETorch on Linux, based on the current Makefile.linux. It covers automatic dependency detection, library management, and recommended usage for a stable build with PyTorch, CUDA, and SentencePiece.

---

## ✅ Features

### 1. **Automatic Dependency Detection**
- **PyTorch**: Detected automatically via conda/python
- **CUDA**: Detected automatically in `/usr/local/cuda*` and `/opt/cuda*`
- **SentencePiece**: Optional, with local build and isolation support. The SentencePiece source code is cloned automatically from the official GitHub repository (https://github.com/google/sentencepiece) if not already present, and built locally within the project.
- **Python Version**: Detected automatically from the active conda environment

### 2. **Linux-Optimized Compilation**
- Uses `.so` shared libraries
- No hardcoded Python version: Python version is extracted from the conda environment
- All required libraries (libtorch, libc10, SentencePiece, NCCL, cusparseLt, etc.) are copied to the `bin/` directory
- No RPATH embedding: runtime libraries are resolved from `bin/` or system paths

### 3. **Library Copying**
- After compilation, all necessary shared libraries are copied to `bin/`:
  - `libtorch*`, `libc10*` (from PyTorch)
  - `libsentencepiece*` (from local build)
  - `libnccl.so*` (from conda and nvidia/nccl/lib)
  - `libcusparseLt.so*` (from CUDA or conda)
- The Python version is dynamically detected and used for the correct path to NCCL libraries.

---

## 🔧 Technical Details

### Main Makefile Variables
- `CONDA_PREFIX`: Path to the active conda environment (must be activated)
- `PYTHON_VERSION`: Extracted automatically from the conda environment
- `PYTORCH_PATH`, `LIBTORCH_LIB`, `LIBTORCH_INCLUDE`: Set from the conda PyTorch installation
- `CUDA_PATH`, `CUDA_LIB`: Detected from system CUDA installation
- `SENTENCEPIECE_LOCAL_PATH`, `SENTENCEPIECE_LIB_DIR`: For local SentencePiece build

### Main Targets
- `all`: Full build, including dependency checks, compilation, and library copying
- `copy-libs`: Copies all required shared libraries to `bin/`
- `build-lispetorch`: Compiles the LispETorch shared library
- `compile-sentencepiece-local`: Builds SentencePiece locally if needed
- `status`: Prints diagnostic information about the environment and build

---

## 🚀 Usage Instructions

### 1. **Activate your conda environment**
```bash
conda activate your-env-pytorch
```

### 2. **Check system status**
```bash
make -f Makefile.linux status
```

### 3. **Clean and build everything**
```bash
make -f Makefile.linux clean && make -f Makefile.linux all
```

### 4. **Test the build**
```bash
cd ../bin
./lispe -c "(use 'lispe_torch)"
```

---

## 🐛 Troubleshooting

- **Missing libraries at runtime**: All required `.so` files should be in `bin/`. If not, check the `copy-libs` rule in the Makefile and ensure your conda environment is active.
- **CUDA/NCCL errors**: Make sure your system CUDA and conda NCCL libraries are compatible and present.
- **SentencePiece conflicts**: Use the isolated build mode if you encounter protobuf/abseil conflicts.

---

## 📝 Notes for Developers

- The Makefile is designed to be portable and robust for any conda Python version.
- All library paths are detected automatically; no hardcoded versions.
- For custom setups, adjust the `copy-libs` rule to include any additional required libraries.

