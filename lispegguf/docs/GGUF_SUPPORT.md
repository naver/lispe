# GGUF Support in LispeTorch

This module adds GGUF file support (format used by llama.cpp) to LispeTorch, allowing you to load and run quantized models efficiently.

## Features

- **GGUF model loading**: Support for .gguf files with complete metadata
- **On-the-fly dequantization**: Support for Q4_0, Q4_1, Q5_0, Q5_1, Q8_0, F16, BF16 formats
- **Optimized inference**: Forward pass with KV cache for auto-regressive generation
- **Tokenization**: Basic tokenization and detokenization (model vocabulary)
- **Text generation**: Sampling with temperature, top-p, top-k and repetition penalty
- **Memory mapping**: Efficient loading of large models
- **Multi-device**: CPU, CUDA and MPS (Metal on macOS) support

## Supported Quantization Types

### Fully Supported
- **F32**: 32-bit float (not quantized)
- **F16**: 16-bit float
- **BF16**: 16-bit BFloat
- **Q4_0**: 4 bits, 32 elements per block
- **Q4_1**: 4 bits with bias, 32 elements per block
- **Q5_0**: 5 bits, 32 elements per block
- **Q5_1**: 5 bits with bias, 32 elements per block
- **Q8_0**: 8 bits, 32 elements per block

### In Progress
- **Q2_K, Q3_K, Q4_K, Q5_K, Q6_K**: K-quant formats (256 elements per block)
- **IQ2_XXS, IQ2_XS, IQ3_XXS, etc.**: Advanced quantization formats

## Installation

### Prerequisites

```bash
# PyTorch with C++ support
pip install torch torchvision torchaudio

# On macOS with MPS
pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu

# On Linux with CUDA (optional)
pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu121
```

### Compilation

```bash
cd lispetorch/src
make -f Makefile.gguf

# With CUDA support
make -f Makefile.gguf USE_CUDA=1

# System installation
make -f Makefile.gguf install
```

## LispE API

### Model Loading

```lisp
;; Basic loading
(setq model (gguf-load-model "/path/to/model.gguf"))

;; With specific device
(setq model (gguf-load-model "/path/to/model.gguf" "cuda"))

;; With advanced configuration
(setq config {
  'use_mmap true                ;; Memory mapping (recommended)
  'dequantize_on_load false     ;; On-demand dequantization
  'num_threads 4                ;; Threads for dequantization
})
(setq model (gguf-load-model "/path/to/model.gguf" "cpu" config))
```

### Model Information

```lisp
;; Display detailed information
(gguf-print-info model)

;; Retrieve metadata
(setq info (gguf-model-info model))
(println "Architecture:" (@ info "architecture"))
(println "Vocabulary size:" (@ info "vocab_size"))
(println "Context length:" (@ info "context_length"))
```

### Tokenization

```lisp
;; Tokenization
(setq tokens (gguf-tokenize model "Hello world!"))
;; Result: list of integers (token IDs)

;; Detokenization
(setq text (gguf-detokenize model tokens))
;; Result: string
```

### Text Generation

```lisp
;; Basic generation
(setq prompt-tokens (gguf-tokenize model "Once upon a time"))
(setq generated (gguf-generate model prompt-tokens))
(setq result-text (gguf-detokenize model generated))

;; Generation with advanced parameters
(setq gen-config {
  'max_new_tokens 100          ;; Maximum number of tokens to generate
  'temperature 0.8             ;; Temperature (0.1 = conservative, 2.0 = creative)
  'top_p 0.9                   ;; Nucleus sampling (0.0 = disabled)
  'top_k 40                    ;; Top-k sampling (0 = disabled)
  'repetition_penalty 1.1      ;; Repetition penalty (1.0 = none)
})
(setq generated (gguf-generate model prompt-tokens gen-config))
```

### KV Cache Management

```lisp
;; Enable/disable cache
(gguf-enable-cache model true)   ;; Enable cache (default)
(gguf-enable-cache model false)  ;; Disable cache

;; Reset cache
(gguf-reset-cache model)
```

## Usage Examples

### Complete Example

```lisp
;; Load a quantized Llama model
(setq model (gguf-load-model "llama-2-7b-chat.q4_0.gguf" "cpu"))

;; Verify loading
(if (null model)
    (error "Unable to load model")
    (gguf-print-info model))

;; Text generation
(setq prompt "Functional programming is")
(setq tokens (gguf-tokenize model prompt))
(setq generated (gguf-generate model tokens {
  'max_new_tokens 50
  'temperature 0.7
  'top_p 0.9
}))
(setq result (gguf-detokenize model generated))
(println result)
```

### Simple Chat Bot

```lisp
(defun chat-with-gguf (model)
  (println "GGUF Chat started (type 'quit' to exit)")
  (loop
    (print "You: ")
    (setq user-input (read-line))
    (if (= user-input "quit") (break))
    
    ;; Simple chat format
    (setq prompt (+ "User: " user-input "\nAssistant: "))
    (setq tokens (gguf-tokenize model prompt))
    (setq response-tokens (gguf-generate model tokens {
      'max_new_tokens 100
      'temperature 0.7
      'top_p 0.9
      'repetition_penalty 1.1
    }))
    (setq response (gguf-detokenize model response-tokens))
    (println "Assistant:" response)))

;; Usage
(setq chat-model (gguf-load-model "chat-model.gguf"))
(chat-with-gguf chat-model)
```

### Performance Benchmark

```lisp
(defun benchmark-gguf (model-path iterations)
  (setq model (gguf-load-model model-path))
  (setq prompt-tokens (gguf-tokenize model "Performance test"))
  
  ;; Test with cache
  (gguf-enable-cache model true)
  (setq start (time))
  (loop i iterations
    (gguf-generate model prompt-tokens {'max_new_tokens 20}))
  (setq cache-time (- (time) start))
  
  ;; Test without cache
  (gguf-reset-cache model)
  (gguf-enable-cache model false)
  (setq start (time))
  (loop i iterations
    (gguf-generate model prompt-tokens {'max_new_tokens 20}))
  (setq no-cache-time (- (time) start))
  
  (println "With cache:" cache-time "ms")
  (println "Without cache:" no-cache-time "ms")
  (println "Speedup:" (/ no-cache-time cache-time) "x"))
```

## Performance

### Implemented Optimizations

- **Memory mapping**: Avoids loading the entire model into memory
- **Lazy dequantization**: Tensors are dequantized only when needed
- **KV Cache**: Reuses attention computations for generation
- **Zero-copy**: Memory sharing with PyTorch when possible

### Typical Comparisons

| Format | File Size | Speed | Quality |
|--------|-----------|-------|---------|
| F16    | 100%      | Slow  | Reference |
| Q8_0   | ~50%      | Medium | Excellent |
| Q5_0   | ~35%      | Fast  | Very Good |
| Q4_0   | ~25%      | Very Fast | Good |

## Current Limitations

1. **Simple tokenization**: Uses model vocabulary, no full BPE/SentencePiece algorithm
2. **K-quant formats**: Q2_K to Q6_K dequantization not yet implemented
3. **Supported models**: Mainly Llama/GPT architectures
4. **Multimodality**: No image/audio support

## Roadmap

### Version 1.1 (Next)
- [ ] Full K-quant format support
- [ ] BPE/SentencePiece tokenization
- [ ] Mistral/Mixtral model support
- [ ] SIMD optimizations for dequantization

### Version 1.2 (Future)
- [ ] Multimodal model support
- [ ] Dynamic quantization
- [ ] Fine-tuning of quantized models
- [ ] Streaming interface for large models

## Troubleshooting

### Common Errors

**"Error loading GGUF model"**
- Verify that the file exists and is accessible
- Check the GGUF version (versions 2 and 3 supported)
- Make sure you have enough memory

**"Unsupported dequantization type"**
- The quantization format is not yet implemented
- Use a model with Q4_0, Q5_0, Q8_0 or F16

**"Device not available"**
- CUDA: Install PyTorch with CUDA support
- MPS: Requires macOS 12.3+ with Apple Silicon chip

### Debug

```lisp
;; Compatibility test
(defun test-gguf-support ()
  (println "CUDA available:" (cuda-is-available))
  (println "MPS available:" (mps-is-available))
  (println "GGUF functions:")
  (maplist func '(gguf-load-model gguf-tokenize gguf-generate)
    (println " -" func ":" (bound func))))
```

## Contributions

Contributions are welcome! Priority areas:

- K-quant format implementation
- Performance optimizations
- Additional architecture support
- Tests and documentation

See `examples/gguf_example.lisp` for more detailed examples.