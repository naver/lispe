# 📚 LispE PyTorch Quick Reference

## 🚀 **Getting Started**
```lisp
(use 'lispe_torch)
(setq device (torch_get_best_device))  ; Auto-detect best device
```

## 📊 **LispE Data Types**
```lisp
; Optimized list types (memory efficient)
(integers 2 3 4)    ; → long* list
(shorts 1 0 1 1)    ; → short* list
(floats 1.0 2.0)    ; → float* list  
(numbers 1.5 2.5)   ; → double* list

; Sequence generators (auto-type selection)
(iota0 5)     ; → (integers 0 1 2 3 4)
(iota 5)      ; → (integers 1 2 3 4 5)
(iota 5.0)    ; → (numbers 1 2 3 4 5)
```

## 🧮 **Tensor Operations**
```lisp
; Creation
(setq t1 (torch_tensor (floats 1.0 2.0 3.0)))
(setq zeros (torch_zeros (integers 2 3)))
(setq ones (torch_ones (integers 3 3)))
(setq random_tensor (torch_rand (integers 4 4)))         ; NEW: Random values [0,1]

; Operations
(setq sum (torch_add t1 t2))
(setq product (torch_mul t1 t2))
(setq matmul (torch_matmul matrix1 matrix2))
(setq transposed (torch_transpose matrix1 0 1))   ; NEW: Transpose dimensions

; Element access with native 'at' function (NEW!)
(setq element (at tensor 0))      ; First element
(setq last (at tensor -1))        ; Last element (negative indexing)

; Device transfer
(setq cuda_tensor (torch_to_cuda t1))     ; NVIDIA GPU
(setq mps_tensor (torch_to_mps t1))       ; Apple Silicon
```

## ⚡ **Flash Attention** (NEW!)
```lisp
; Create Flash Attention module
(setq flash_attn (torch_flash_attention_create 256 8 0.1 true))

; Standard forward pass
(setq output (torch_flash_attention_forward flash_attn query thekey thevalue))

; With attention mask
(setq masked_output (torch_flash_attention_with_mask flash_attn query thekey thevalue attention_mask))

; With custom dropout
(setq dropout_output (torch_flash_attention_with_dropout flash_attn query thekey thevalue 0.2 true))

; Direct PyTorch 2.0+ native attention
(setq direct_output (torch_scaled_dot_product_attention query thekey thevalue nil 0.0 false))
```

## 📝 **Tokenization**
```lisp
; Simple tokenizer
(setq tokenizer (torch_simple_tokenizer))
(setq tokens (torch_encode tokenizer "Hello world!"))
(setq text (torch_decode tokenizer tokens))
(setq vocab_size (torch_vocab_size tokenizer))

; SentencePiece tokenizer (if available)
(setq sp_tokenizer (torch_sentencepiece_tokenizer "model.spm"))
```

## 🧠 **Neural Networks**
```lisp
; MLP Model
(setq model (torch_model 4 64 2))  ; input=4, hidden=64, output=2
(setq output (torch_forward model input_tensor))

; Embedding Layer
(setq embedding (torch_embedding vocab_size 128))
(setq embedded (torch_embedding_forward embedding tokens))

; Transformer Block
(setq transformer (torch_transformer_block 128 8 256))  ; embed=128, heads=8, ff=256
(setq result (torch_transformer_forward transformer embedded))
```

## 🎯 **Training**
```lisp
; Optimizer
(setq optimizer (torch_optimizer model 0.001 "adam"))  ; or "sgd"

; Training loop
(torch_optimizer_zero_grad optimizer)
(setq predictions (torch_forward model input))
(setq loss (torch_mse_loss predictions targets))      ; or torch_crossentropy_loss
(torch_backward loss)
(torch_optimizer_step optimizer)
```

## 📜 **Sequence Processing**
```lisp
; Multiple sequences
(setq sequences (list tokens1 tokens2 tokens3))
(setq padded (torch_pad_sequences sequences max_length))
(setq attention_mask (torch_create_attention_mask padded))
```

## 🔧 **Device Management**
```lisp
; Detection
(torch_cuda_is_available)       ; true/nil
(torch_mps_is_available)        ; true/nil  
(torch_cuda_device_count)       ; number of CUDA devices
(torch_get_best_device)         ; "mps", "cuda", or "cpu"

; Device setting
(torch_set_device 0)            ; Set CUDA device 0
```

## 🏗 **Complete Pipeline Example**
```lisp
(use 'lispe_torch)

; 1. Setup
(setq device (torch_get_best_device))
(setq tokenizer (torch_simple_tokenizer))

; 2. Text processing
(setq text "Hello world!")
(setq tokens (torch_encode tokenizer text))
(setq vocab_size (torch_vocab_size tokenizer))

; 3. Model architecture
(setq embedding (torch_embedding vocab_size 64))
(setq transformer (torch_transformer_block 64 4 128))

; 4. Forward pass
(setq embedded (torch_embedding_forward embedding tokens))
(setq output (torch_transformer_forward transformer embedded))

; 5. Training setup
(setq model (torch_model 64 32 vocab_size))
(setq optimizer (torch_optimizer model 0.001 "adam"))

; 6. Model optimization for production
(setq quantized_model (torch_model_quantize_dynamic model))  ; Dynamic quantization

; Ready for training!
```

## 🗜️ **Model Quantization** (NEW!)
```lisp
; Memory optimization for production deployment
(setq weights (torch_randn (integers 1024 768)))

; FP16 quantization (50% memory reduction)
(setq fp16_weights (torch_quantize_fp16 weights))

; INT8 quantization (75% memory reduction)
(setq int8_weights (torch_quantize_int8 weights))

; Dynamic quantization with automatic parameters
(setq dynamic_quantized (torch_quantize_dynamic weights 1.0 0))

; Static quantization with custom scale/zero-point
(setq static_quantized (torch_quantize_static weights 0.5 -128))

; Convert back when needed
(setq restored (torch_dequantize static_quantized))
```

## 📋 **Function Categories**

### Core Functions
- `torch_tensor`, `torch_zeros`, `torch_ones`, `torch_rand` (NEW!)
- `torch_add`, `torch_mul`, `torch_matmul`, `torch_transpose` (NEW!)
- `torch_get_best_device`, `torch_cuda_is_available`, `torch_mps_is_available`
- Native `at` function for tensor element access (NEW!)

### Flash Attention (NEW!)
- `torch_flash_attention_create`, `torch_flash_attention_forward`
- `torch_flash_attention_with_mask`, `torch_flash_attention_with_dropout`
- `torch_scaled_dot_product_attention`

### Tokenization
- `torch_simple_tokenizer`, `torch_sentencepiece_tokenizer`
- `torch_encode`, `torch_decode`, `torch_vocab_size`
- `torch_pad_sequences`, `torch_create_attention_mask`

### Models
- `torch_model`, `torch_forward`
- `torch_embedding`, `torch_embedding_forward`
- `torch_transformer_block`, `torch_transformer_forward`

### Training
- `torch_optimizer`, `torch_optimizer_step`, `torch_optimizer_zero_grad`
- `torch_mse_loss`, `torch_crossentropy_loss`, `torch_backward`

### Model Quantization (NEW!)
- `torch_quantize_dynamic`: Dynamic quantization with automatic scaling
- `torch_quantize_static`: Static quantization with custom parameters
- `torch_dequantize`: Convert quantized tensors back to floating point
- `torch_quantize_int8`: Automatic INT8 conversion (75% memory reduction)
- `torch_quantize_fp16`: Half-precision conversion (50% memory reduction)
- `torch_quantize_linear`: Linear quantization with scale/zero-point
- `torch_quantize_per_channel`: Per-channel quantization for precision
- `torch_tensor_to_int8`, `torch_tensor_to_fp16`: Type conversions
- `torch_model_quantize_dynamic`, `torch_model_quantize_static`: Model-level optimization

### Device Operations
- `torch_to_cuda`, `torch_to_mps`, `torch_set_device`
- `torch_cuda_device_count`

## 🚀 **Ready for Llama-3.1 with Flash Attention!**
The library now provides complete infrastructure for fine-tuning large language models with memory-efficient Flash Attention, proper tokenization, Transformer architecture, and GPU acceleration.

**NEW**: Flash Attention enables processing of 8K+ token sequences with linear memory scaling, making it ideal for long-context applications and large-scale model training.

See `README.md` and `lispe_torch.md` for detailed documentation.
Run `examples.lsp` for working demonstrations.
Check `README_Flash_Attention.md` for Flash Attention specific examples.
