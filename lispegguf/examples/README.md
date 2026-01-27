# GGUF Usage Examples

This folder contains examples for using GGUF models with LispE.

## Getting Started

### 1. Quick Start (simplest)

File: `quick_start.lisp`

The simplest example to get started. Loads the model and generates text.

```bash
cd /Users/clauderoux/Documents/GitHub/lispe/gguf
lispe examples/quick_start.lisp
```

### 2. Complete GPT-OSS Example

File: `gpt_oss.lisp`

Complete example with several features:
- Model loading and information
- Simple text generation
- Tests with different temperatures
- Code completion
- Interactive chat mode

```bash
lispe gpt_oss.lisp
```

Or use it interactively:

```bash
lispe
```

Then in LispE:
```lisp
;; Load the file
(load "examples/exemple_gpt_oss.lisp")

;; Run all examples
(main)

;; Or use functions individually
(setq m (load-gpt-oss))
(generate-text m "Your text here")
(chat-gpt-oss m)  ;; Interactive mode
```

### 3. Generic Example

File: `gguf_example.lisp`

Generic example showing all available GGUF API features.

## Model Configuration

The model used in the examples:
- **Path**: `/Users/clauderoux/.lmstudio/models/lmstudio-community/gpt-oss-20b-GGUF/gpt-oss-20b-MXFP4.gguf`
- **Size**: ~20B parameters
- **Format**: MXFP4 (MX Float Point 4-bit quantization)

### Adapting to Your Model

To use another GGUF model, simply modify the `model-path` variable:

```lisp
(setq model-path "/path/to/your/model.gguf")
```

## Generation Parameters

### Temperature
Controls creativity/randomness:
- `0.1 - 0.5`: Very deterministic, predictable
- `0.6 - 0.9`: Balanced (recommended)
- `1.0 - 1.5`: Creative, varied
- `> 1.5`: Very random

### Top-p (Nucleus Sampling)
Controls token diversity:
- `0.9`: Standard value (recommended)
- `0.95`: More diversity
- `0.8`: Less diversity

### Top-k
Number of candidate tokens:
- `40`: Standard value
- Higher = more diversity

### Repetition Penalty
Penalizes repetitions:
- `1.0`: No penalty
- `1.1`: Light penalty (recommended)
- `1.2+`: Strong penalty

## Configuration Examples

### For Code
```lisp
{
  'max_new_tokens 200
  'temperature 0.2
  'top_p 0.95
  'repetition_penalty 1.05
}
```

### For Creative Text
```lisp
{
  'max_new_tokens 150
  'temperature 1.0
  'top_p 0.9
  'repetition_penalty 1.1
}
```

### For Precise Answers
```lisp
{
  'max_new_tokens 100
  'temperature 0.3
  'top_p 0.9
  'repetition_penalty 1.15
}
```

## Performance Optimization

### Memory Usage
```lisp
;; Configuration to reduce memory usage
{
  'use_mmap true           ;; Enable memory mapping
  'dequantize_on_load false ;; Dequantize on demand
  'num_threads 4            ;; Limit threads
}
```

### Device
```lisp
;; CPU (default)
(gguf-load-model path "cpu" config)

;; CUDA (if NVIDIA GPU available)
(gguf-load-model path "cuda" config)

;; MPS (if Mac Apple Silicon)
(gguf-load-model path "mps" config)
```

### KV Cache
```lisp
;; Enable cache (for faster generation)
(gguf-enable-cache model true)

;; Reset cache (if context changed)
(gguf-reset-cache model)
```

## Troubleshooting

### Model won't load
1. Check the file path
2. Verify you have enough RAM (20B model ≈ 8-12 GB)
3. Try with `use_mmap true`

### Generation too slow
1. Reduce `num_threads` if CPU overloaded
2. Use GPU if available (`cuda` or `mps`)
3. Reduce `max_new_tokens`

### Repetitive text
Increase `repetition_penalty` to 1.2 or higher

### Incoherent text
Reduce `temperature` to 0.7 or lower

## Full Documentation

See `docs/GGUF_SUPPORT.md` for complete API documentation.
