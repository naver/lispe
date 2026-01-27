# LispE GGUF Library

A high-performance GGUF model loader for LispE, powered by llama.cpp. This library enables loading and running quantized language models (GGUF format) directly from LispE with GPU acceleration support.

## Features

- **GPU Acceleration**: Automatic GPU support (Metal on macOS, CUDA on Linux)
- **Quantized Models**: Support for all GGUF quantization formats (Q4, Q5, Q8, MXFP4, etc.)
- **Zero Configuration**: Automatically downloads and builds llama.cpp
- **Dictionary-based API**: Clean, flexible configuration through dictionaries
- **Silent by Default**: No verbose logging unless explicitly enabled

## Requirements

### macOS
- Xcode Command Line Tools
- CMake (`brew install cmake`)
- Metal support (built-in on Apple Silicon)

### Linux
- GCC or Clang
- CMake (`apt install cmake` or `yum install cmake`)
- CUDA Toolkit (optional, for NVIDIA GPU acceleration)

## Installation

```bash
# Clone and build automatically
make

# Or manually step by step
make llama-cpp    # Build llama.cpp
make              # Build LispE GGUF library
make install      # Install and show usage
```

The Makefile will automatically:
1. Clone llama.cpp if not present
2. Build llama.cpp with appropriate GPU support
3. Compile the LispE GGUF library
4. Link everything together

## Usage

### Basic Example

```lisp
(use 'lispe_gguf)

; Load a model (GPU by default)
(setq model (gguf_load "/path/to/model.gguf"))

; Generate text
(setq result (gguf_generate model "Hello, world!"))
(println result)
```

### Advanced Configuration

```lisp
(use 'lispe_gguf)

; Load with custom configuration
(setq model (gguf_load "/path/to/model.gguf" {
    "n_gpu_layers":99     ; Number of layers on GPU (99 = all)
    "n_ctx":2048          ; Context window size
}))

; Load with advanced KV-cache options (experimental)
(setq model (gguf_load "/path/to/model.gguf" {
    "n_gpu_layers":99
    "n_ctx":4096
    "cache_type_k":"q8_0"  ; Quantize K cache to Q8_0 (saves memory)
    "cache_type_v":"q8_0"  ; Quantize V cache to Q8_0
    "flash_attn":true      ; Enable Flash Attention (experimental)
}))

; Generate with parameters
(setq result (gguf_generate model "Explain quantum computing" {
    "max_tokens":200      ; Maximum tokens to generate
    "temperature":0.8     ; Sampling temperature
    "top_p":0.9           ; Nucleus sampling
    "top_k":40            ; Top-k sampling
    "repeat_penalty":1.1  ; Repetition penalty (1.0 = disabled)
    "repeat_last_n":64    ; Number of tokens to check for repetition
}))

(println result)
```

### Using Callbacks

```lisp
(use 'lispe_gguf)

; Define a callback function to process each token
(defun on_token (token data)
    ; token: the generated token ID
    ; data: optional data passed from config
    (print (gguf_detokenize model (list token)))  ; Print token text
)

; Load model
(setq model (gguf_load "/path/to/model.gguf"))

; Generate with callback for streaming output
(setq result (gguf_generate model "Hello, world!" {
    "max_tokens":100
    "callback":'on_token     ; Function called for each token (note the quote)
    "data":{"info":"test"}   ; Optional data passed to callback
}))
```

### Complete API

#### Load Model
```lisp
(gguf_load filepath (config))
```
- `filepath`: Path to GGUF model file
- `config` (optional): Dictionary with:
  - `"n_gpu_layers"`: Number of layers on GPU (default: 99)
  - `"n_ctx"`: Context window size (default: 2048)
  - `"cache_type_k"`: KV cache K quantization (default: "f16")
    - Options: `"f16"`, `"q8_0"`, `"q4_0"`, `"q4_1"`, `"q5_0"`, `"q5_1"`
    - Lower precision = less memory, slightly lower quality
  - `"cache_type_v"`: KV cache V quantization (default: "f16")
    - Same options as `cache_type_k`
  - `"flash_attn"`: Enable Flash Attention (default: false)
    - Experimental: reduces memory usage during inference

#### Generate Text
```lisp
(gguf_generate model prompt (config))
```
- `model`: Model loaded with `gguf_load`
- `prompt`: Input text prompt
- `config` (optional): Dictionary with:
  - `"max_tokens"`: Max tokens to generate (default: 100)
  - `"temperature"`: Sampling temperature (default: 0.8)
  - `"top_p"`: Nucleus sampling (default: 0.9)
  - `"top_k"`: Top-k sampling (default: 40)
  - `"repeat_penalty"`: Repetition penalty factor (default: 1.1, 1.0 = disabled)
  - `"repeat_last_n"`: Number of recent tokens to check for repetition (default: 64)
  - `"callback"`: LispE function called for each generated token (optional)
  - `"data"`: Optional data passed to the callback function

#### Tokenize
```lisp
(gguf_tokenize model text)
```
Returns a list of token IDs.

#### Detokenize
```lisp
(gguf_detokenize model tokens)
```
Converts token IDs back to text.

#### Control Logging
```lisp
(gguf_set_log enable)
```
- `enable`: `true` to enable llama.cpp logs, `nil` to disable

#### Free Model
```lisp
(gguf_free model)
```
Explicitly free model resources (automatic on garbage collection).

## Configuration Defaults

| Parameter | Default | Description |
|-----------|---------|-------------|
| `n_gpu_layers` | 99 | Use GPU for all layers |
| `n_ctx` | 2048 | Context window size |
| `cache_type_k` | "f16" | KV cache K quantization |
| `cache_type_v` | "f16" | KV cache V quantization |
| `flash_attn` | false | Flash Attention (experimental) |
| `max_tokens` | 100 | Tokens to generate |
| `temperature` | 0.8 | Sampling temperature |
| `top_p` | 0.9 | Nucleus sampling threshold |
| `top_k` | 40 | Top-k sampling limit |
| `repeat_penalty` | 1.1 | Repetition penalty factor |
| `repeat_last_n` | 64 | Tokens to check for repetition |

## Examples

See the `examples/` directory:
- `quick_start.lisp`: Simple example for beginners
- `test_qwen.lisp`: Testing with Qwen models
- `advanced_kvcache.lisp`: KV-cache quantization and optimization examples

## GPU Support

### macOS (Metal)
Automatically enabled. All layers run on GPU by default.

### Linux (CUDA)
If CUDA is installed at `/usr/local/cuda`, the Makefile automatically enables CUDA support.

### CPU-only Mode
To disable GPU and run on CPU:
```lisp
(setq model (gguf_load "/path/to/model.gguf" {"n_gpu_layers":0}))
```

## Library Installation

To install llama.cpp libraries system-wide:

```bash
# macOS
sudo mkdir -p /usr/local/lib/lispe
sudo cp llama.cpp/build/bin/*.dylib /usr/local/lib/lispe/

# Linux
sudo mkdir -p /usr/local/lib/lispe
sudo cp llama.cpp/build/bin/*.so /usr/local/lib/lispe/
```

## Advanced: KV-Cache Configuration

The KV-cache stores key-value states during inference to avoid recomputation. Advanced options allow memory/quality trade-offs:

### KV-Cache Quantization

Reduce memory usage by quantizing the cache (experimental):

```lisp
; Quantize cache to Q8_0 (slight quality loss, ~50% memory savings)
(setq model (gguf_load "model.gguf" {
    "cache_type_k":"q8_0"
    "cache_type_v":"q8_0"
}))

; More aggressive: Q4_0 (moderate quality loss, ~75% memory savings)
(setq model (gguf_load "model.gguf" {
    "cache_type_k":"q4_0"
    "cache_type_v":"q4_0"
}))
```

**Available types:**
- `"f16"` - Full precision (default, best quality)
- `"q8_0"` - 8-bit quantization (good balance)
- `"q5_0"`, `"q5_1"` - 5-bit quantization
- `"q4_0"`, `"q4_1"` - 4-bit quantization (most memory savings)

### Flash Attention

Experimental optimization for reduced memory during inference:

```lisp
(setq model (gguf_load "model.gguf" {
    "flash_attn":true
}))
```

**Note:** Flash Attention is experimental and may not work with all models or configurations. It's similar to the experimental features in Ollama and LM Studio.

### When to Use

- **Large contexts** (n_ctx > 4096): Use cache quantization
- **Memory constrained**: Start with `"q8_0"`, try `"q4_0"` if needed
- **Quality critical**: Keep default `"f16"`
- **Batch inference**: Consider `flash_attn:true`

## Troubleshooting

### llama.cpp build fails
```bash
make clean-all
make llama-cpp
```

### Model won't load
- Check file path is correct
- Ensure GGUF format (not GGML or other)
- Try CPU-only mode: `{"n_gpu_layers":0}`

### Verbose logging
Disable logs:
```lisp
(gguf_set_log nil)
```

## Performance Tips

1. **Use GPU**: Default `n_gpu_layers:99` gives best performance
2. **Adjust context**: Smaller `n_ctx` uses less memory
3. **KV-cache quantization**: Use `"q8_0"` for large contexts to save memory
4. **Flash Attention**: Try `flash_attn:true` for memory-constrained scenarios
5. **Optimize sampling**: Lower `temperature` for deterministic output
6. **Batch size**: Automatically optimized (512)

## License

- LispE GGUF Library: 3-Clause BSD License
- llama.cpp: MIT License

## Credits

- Built on [llama.cpp](https://github.com/ggerganov/llama.cpp)
- Part of [LispE](https://github.com/naver/lispe)
