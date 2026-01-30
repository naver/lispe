# Portable Installation of LispETorch

This guide explains how to install LispETorch portably on macOS, with all dependencies included in `/usr/local/lib/lispe/`.

## Advantages of Portable Installation

- **Simplified distribution**: A single directory contains everything
- **No external dependencies**: PyTorch and SentencePiece included
- **Easy deployment**: Copy the directory to other machines
- **Isolation**: Does not interfere with other PyTorch installations

## Automatic Installation

### Simple Method (Recommended)
```bash
cd lispetorch/
./install_portable.sh
```

The script does everything automatically:
1. Checks prerequisites
2. Compiles LispETorch
3. Copies all dependencies
4. Configures rpath paths
5. Verifies the installation

### Manual Installation

If you prefer to control each step:

```bash
# 1. Compilation
make clean
make all

# 2. Portable installation
make install-portable

# 3. Verification
make check-portable
```

## Installation Structure

After installation, `/usr/local/lib/lispe/` contains:

```
/usr/local/lib/lispe/
├── liblispe_torch.so          # LispETorch extension
├── libtorch.dylib             # PyTorch core
├── libtorch_cpu.dylib         # PyTorch CPU backend
├── libc10.dylib               # PyTorch utilities
├── libtorch_cuda.dylib        # PyTorch CUDA (if available)
├── libsentencepiece.dylib     # SentencePiece (if installed)
└── libsentencepiece_train.dylib
```

## Usage in LispE

```lisp
; Load the extension
(use 'torch)

; Basic test
(setq x (torch_tensor '(1 2 3 4)))
(torch_shape x)
; Result: (4)

; Flash Attention
(setq q (torch_randn '(1 8 512 64)))
(setq k (torch_randn '(1 8 512 64)))
(setq v (torch_randn '(1 8 512 64)))
(setq result (torch_flash_attention q k v))
```

## Distribution to Other Machines

### Prerequisites on Target Machine
- macOS (same architecture: Intel or Apple Silicon)
- LispE installed

### Distribution Process
1. **On the source machine**:
   ```bash
   tar -czf lispetorch-portable.tar.gz -C /usr/local/lib lispe/
   ```

2. **On the target machine**:
   ```bash
   # Copy the file
   scp lispetorch-portable.tar.gz user@target-machine:~
   
   # Installation
   sudo mkdir -p /usr/local/lib
   sudo tar -xzf lispetorch-portable.tar.gz -C /usr/local/lib
   sudo chown -R root:wheel /usr/local/lib/lispe
   ```

3. **Test on the target machine**:
   ```bash
   lispe -c "(use 'torch) (torch_tensor '(1 2 3))"
   ```

## Troubleshooting

### Compilation Issues

```bash
# Dependency diagnostics
make check-deps

# Recommended conda environment
conda activate your-pytorch-env
make rebuild
```

### Path Issues

```bash
# Check dependencies
otool -L /usr/local/lib/lispe/liblispe_torch.so

# Reconfigure paths if necessary
make fix-rpaths
```

### Permission Errors

```bash
# Ensure correct permissions
sudo chown -R root:wheel /usr/local/lib/lispe
sudo chmod -R 755 /usr/local/lib/lispe
```

### Connectivity Test

```bash
# Minimal test from LispE
lispe -c "
(use 'torch)
(println \"✓ LispETorch loaded\")
(setq x (torch_tensor '(1 2 3)))
(println \"✓ Tensor created:\" (torch_shape x))
(println \"✓ Portable installation functional\")
"
```

## Available Make Commands

| Command | Description |
|---------|-------------|
| `make all` | Standard compilation |
| `make install-portable` | Complete portable installation |
| `make copy-dependencies` | Copy dependencies only |
| `make fix-rpaths` | Reconfigure library paths |
| `make check-portable` | Verify portable installation |
| `make uninstall-portable` | Remove portable installation |
| `make check-deps` | Dependency diagnostics |

## Uninstallation

```bash
make uninstall-portable
```

or manually:

```bash
sudo rm -rf /usr/local/lib/lispe
```

## Architecture and Compatibility

- **Intel (x86_64)**: Compatible with all Intel Macs
- **Apple Silicon (arm64)**: Compatible with M1/M2/M3 Macs
- **Cross-compatibility**: Not supported (architecture differences)

## Support and Development

For issues or contributions:
1. First check with `make check-deps`
2. Consult compilation logs
3. Test with `make check-portable`

Portable installation greatly facilitates deploying LispETorch in production or distribution environments where full PyTorch installation is not desired.
