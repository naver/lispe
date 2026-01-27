# LispE MLX - Apple Silicon Machine Learning for Lisp

**LispE MLX** is a comprehensive binding library that brings Apple's [MLX](https://github.com/ml-explore/mlx) framework to LispE, enabling high-performance machine learning and deep learning on Apple Silicon (M1/M2/M3/M4).

## 🎯 Highlights

- **Native Apple Silicon**: Leverages Metal GPU acceleration via MLX
- **Complete Tensor Operations**: 150+ MLX functions exposed to LispE
- **LLM Inference**: Run large language models (Gemma, Llama, etc.) in pure Lisp
- **Quantization Support**: 4-bit/8-bit quantized model support (QAT, GGUF)
- **Safetensors**: Native loading of HuggingFace MLX models
- **Performance**: 12+ tokens/sec on Gemma 3 27B (comparable to Python mlx-lm)

## 📋 Requirements

### Hardware
- **Apple Silicon Mac** (M1, M2, M3, M4 or later)
- macOS 13.0 (Ventura) or later
- Minimum 16GB RAM (32GB+ recommended for large models)

### Software Dependencies

Install the required libraries via [Homebrew](https://brew.sh/):

```bash
# Install MLX (Apple's ML framework)
brew install mlx

# Install nlohmann-json (for config parsing)
brew install nlohmann-json

# SentencePiece is available via lispe_torch
brew install sentencepiece
```

### Verify Installation

```bash
# Check MLX installation
brew info mlx
# Should show something like: /opt/homebrew/Cellar/mlx/0.29.3

# Check nlohmann-json
brew info nlohmann-json
```

## 🔧 Compilation

### 1. Configure Makefile (if needed)

Edit `Makefile` to match your MLX version:

```makefile
# MLX paths - update version if different
MLX_PATH = /opt/homebrew/Cellar/mlx/0.29.3
```

To find your MLX path:
```bash
brew --prefix mlx
# or
ls /opt/homebrew/Cellar/mlx/
```

### 2. Build the Library

```bash
cd lispemlx
make clean
make
```

This creates `liblispe_mlx.so` in the `../bin/` directory.

### 3. Install MLX Runtime (Optional but Recommended)

```bash
sudo mkdir -p /usr/local/lib/lispe
sudo cp /opt/homebrew/Cellar/mlx/*/lib/libmlx.dylib /usr/local/lib/lispe/
```

## 🚀 Quick Start

### Load the Library

```lisp
(use 'lispe_mlx)
```

### Basic Tensor Operations

```lisp
; Create arrays
(setq a (mlx_array '(1 2 3 4 5 6) '(2 3) "float32"))
(setq b (mlx_ones '(3 2) "float32"))

; Matrix multiplication
(setq c (mlx_matmul a b))
(println "Shape: " (mlx_shape c))  ; (2 2)

; Element-wise operations
(setq d (mlx_add a (mlx_array 10)))
(setq e (mlx_multiply a b))

; Reductions
(println "Sum: " (mlx_sum a))
(println "Mean: " (mlx_mean a))
```

### Load a Safetensors Model

```lisp
; Load a single safetensors file
(setq result (mlx_load_safetensors "/path/to/model.safetensors"))
(setq tensors (@ result 0))   ; Dictionary of tensors
(setq metadata (@ result 1))  ; Metadata

; Access a specific tensor
(setq embed (@ tensors "model.embed_tokens.weight"))
(println "Embedding shape: " (mlx_shape embed))
```

### Run Gemma 3 27B Inference

See [examples/load_gemma3.lisp](examples/load_gemma3.lisp) for a complete implementation:

```lisp
; Load the example
(load "examples/load_gemma3.lisp")

; Chat with the model
(model MLXModel (chat "What is the capital of France?"))

; Generate with custom parameters
(model MLXModel (chat "Write a haiku about coding" 100 0.8))
```

## 📚 API Reference

### Array Creation

| Function | Description | Example |
|----------|-------------|---------|
| `mlx_array` | Create array from data | `(mlx_array '(1 2 3) '(3) "float32")` |
| `mlx_zeros` | Array of zeros | `(mlx_zeros '(2 3) "float32")` |
| `mlx_ones` | Array of ones | `(mlx_ones '(2 3) "float32")` |
| `mlx_full` | Array filled with value | `(mlx_full '(2 3) 5.0 "float32")` |
| `mlx_arange` | Range of values | `(mlx_arange 0 10 1 "float32")` |
| `mlx_linspace` | Linearly spaced values | `(mlx_linspace 0 1 100 "float32")` |
| `mlx_eye` | Identity matrix | `(mlx_eye 3 "float32")` |
| `mlx_random_uniform` | Uniform random | `(mlx_random_uniform '(2 3))` |
| `mlx_random_normal` | Normal random | `(mlx_random_normal '(2 3))` |

### Array Manipulation

| Function | Description |
|----------|-------------|
| `mlx_reshape` | Change shape |
| `mlx_transpose` | Transpose axes |
| `mlx_concatenate` | Join arrays |
| `mlx_stack` | Stack arrays |
| `mlx_split` | Split array |
| `mlx_squeeze` | Remove size-1 dims |
| `mlx_expand_dims` | Add dimension |
| `mlx_flatten` | Flatten to 1D |
| `mlx_take` | Index selection |
| `mlx_slice` | Slice array |

### Math Operations

| Function | Description |
|----------|-------------|
| `mlx_add` | Addition |
| `mlx_subtract` | Subtraction |
| `mlx_multiply` | Multiplication |
| `mlx_divide` | Division |
| `mlx_matmul` | Matrix multiply |
| `mlx_power` | Exponentiation |
| `mlx_sqrt` | Square root |
| `mlx_exp` | Exponential |
| `mlx_log` | Natural log |
| `mlx_abs` | Absolute value |

### Reductions

| Function | Description |
|----------|-------------|
| `mlx_sum` | Sum |
| `mlx_mean` | Mean |
| `mlx_max` | Maximum |
| `mlx_min` | Minimum |
| `mlx_argmax` | Index of max |
| `mlx_argmin` | Index of min |
| `mlx_prod` | Product |
| `mlx_var` | Variance |
| `mlx_std` | Standard deviation |

### Deep Learning

| Function | Description |
|----------|-------------|
| `mlx_softmax` | Softmax activation |
| `mlx_relu` | ReLU activation |
| `mlx_gelu` | GELU activation |
| `mlx_gelu_tanh` | GELU (tanh approx) |
| `mlx_silu` | SiLU/Swish activation |
| `mlx_sigmoid` | Sigmoid activation |
| `mlx_tanh` | Tanh activation |
| `mlx_rms_norm` | RMS normalization |
| `mlx_layer_norm` | Layer normalization |
| `mlx_rope` | Rotary position embedding |
| `mlx_scaled_dot_product_attention` | SDPA |

### Quantization

| Function | Description |
|----------|-------------|
| `mlx_quantize` | Quantize weights |
| `mlx_dequantize` | Dequantize weights |
| `mlx_quantized_matmul` | Quantized matmul |

### I/O

| Function | Description |
|----------|-------------|
| `mlx_load_safetensors` | Load safetensors file |
| `mlx_save_safetensors` | Save safetensors file |
| `mlx_load_gguf` | Load GGUF file |
| `mlx_save_gguf` | Save GGUF file |
| `mlx_save` | Save MLX array |
| `mlx_load` | Load MLX array |

### Memory Management

| Function | Description |
|----------|-------------|
| `mlx_synchronize` | Sync GPU operations |
| `mlx_eval` | Evaluate lazy array |
| `mlx_clear_cache` | Clear memory cache |
| `mlx_get_active_memory` | Active memory (bytes) |
| `mlx_get_peak_memory` | Peak memory (bytes) |
| `mlx_get_cache_memory` | Cache memory (bytes) |
| `mlx_set_memory_limit` | Set memory limit |
| `mlx_set_cache_limit` | Set cache limit |

### Linear Algebra

| Function | Description |
|----------|-------------|
| `mlx_norm` | Matrix/vector norm |
| `mlx_inv` | Matrix inverse |
| `mlx_svd` | SVD decomposition |
| `mlx_qr` | QR decomposition |
| `mlx_cholesky` | Cholesky decomposition |
| `mlx_solve` | Solve linear system |
| `mlx_eig` | Eigendecomposition |
| `mlx_pinv` | Pseudo-inverse |

### FFT

| Function | Description |
|----------|-------------|
| `mlx_fft` | 1D FFT |
| `mlx_ifft` | 1D inverse FFT |
| `mlx_fft2` | 2D FFT |
| `mlx_rfft` | Real FFT |

### Convolutions

| Function | Description |
|----------|-------------|
| `mlx_conv1d` | 1D convolution |
| `mlx_conv2d` | 2D convolution |
| `mlx_conv3d` | 3D convolution |
| `mlx_conv_transpose1d` | 1D transposed conv |
| `mlx_conv_transpose2d` | 2D transposed conv |

## 🤖 LLM Inference Example

The `examples/load_gemma3.lisp` file demonstrates a complete LLM implementation in ~1000 lines of pure LispE:

### Features
- Sharded safetensors loading (4 files for 27B model)
- 4-bit quantized inference
- KV caching (standard + sliding window)
- RoPE positional embeddings
- Grouped Query Attention (GQA)
- Streaming text generation

### Performance
- **Model**: Gemma 3 27B IT QAT 4-bit
- **Speed**: ~12 tokens/sec
- **Load time**: ~3 seconds
- **Memory**: ~16GB

### Usage

```lisp
; Load the model
(setq model (load_mlx_model "/path/to/mlx-community/gemma-3-27b-it-qat-4bit"))

; Chat
(model MLXModel (chat "Hello, who are you?"))

; With parameters: message, max_tokens, temperature, system_prompt
(model MLXModel (chat "Explain quantum computing" 256 0.7 "You are a helpful assistant"))

; Raw generation
(model MLXModel (generate "<bos>Once upon a time" 100 0.8))

; Tokenizer
(model MLXModel (encode "Hello world"))  ; Returns token IDs
(model MLXModel (decode '(1 2 3)))       ; Returns text

; Model info
(model MLXModel (info))
(model MLXModel (vocab_size))
```

## 📁 Project Structure

```
lispemlx/
├── Makefile              # Build configuration
├── README.md             # This file
├── src/
│   └── lispe_methods_mlx.cxx   # C++ bindings (8000+ lines)
└── examples/
    ├── load_gemma3.lisp        # Complete Gemma 3 27B loader
    ├── test_tokenizer.lisp     # Tokenizer tests
    └── test_shapes.lisp        # Shape manipulation tests
```

## 🔗 Related Libraries

For tokenization, you'll also need `lispe_torch`:

```bash
cd ../lispetorch
make -f Makefile.macos
```

This provides:
- `torch_sentencepiece_tokenizer` - Load SentencePiece tokenizer
- `torch_encode` - Encode text to tokens
- `torch_decode` - Decode tokens to text
- `torch_vocab_size` - Get vocabulary size

## 🐛 Troubleshooting

### macOS Gatekeeper/Vault Blocking

If macOS blocks the library with a security warning ("cannot be opened because the developer cannot be verified"), try the following:

**Allow in System Settings**
1. Go to **System Settings** → **Privacy & Security**
2. Scroll down to find the blocked library message
3. Click **"Allow Anyway"**
4. Try loading the library again in LispE


### "MLX not found"
```bash
# Verify MLX is installed
brew list mlx
# Reinstall if needed
brew reinstall mlx
```

### "Library not loaded: @rpath/libmlx.dylib"
```bash
# Copy MLX library to lispe lib path
sudo mkdir -p /usr/local/lib/lispe
sudo cp $(brew --prefix mlx)/lib/libmlx.dylib /usr/local/lib/lispe/
```

### "nlohmann/json.hpp not found"
```bash
brew install nlohmann-json
# Verify include path
ls /opt/homebrew/include/nlohmann/
```

### Slow performance
- Ensure you're using a quantized model (4-bit or 8-bit)
- Check memory usage: `(mlx_get_active_memory)`
- Clear cache periodically: `(mlx_clear_cache)`

## 📖 References

- [MLX Documentation](https://ml-explore.github.io/mlx/)
- [MLX GitHub](https://github.com/ml-explore/mlx)
- [LispE Documentation](https://github.com/naver/lispe)
- [HuggingFace MLX Models](https://huggingface.co/mlx-community)

## 📄 License

Copyright 2020-present NAVER Corp.
The 3-Clause BSD License
