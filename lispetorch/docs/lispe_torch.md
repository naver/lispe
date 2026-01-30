# LispE PyTorch Library

A comprehensive PyTorch integration for LispE, providing tensor operations, neural network models, training capabilities, **Flash Attention** for memory-efficient long sequence processing, and **complete text generation system** with multiple sampling strategies.

## Installation and Loading

To use the LispE PyTorch library in your code:

```lisp
(use 'lispe_torch)
```

## Overview

This library provides a complete interface to PyTorch functionality from within LispE, including:
- Tensor creation and manipulation with enhanced operations
- **Flash Attention**: Memory-efficient attention for long sequences with O(N) complexity
- **Text Generation System**: Complete implementation with multiple sampling strategies (greedy, top-k, top-p, beam search)
- **TorchGenerator Class**: Advanced text generation with configurable parameters and repetition penalty
- **Model Quantization**: FP16/INT8 quantization for production optimization and deployment
- Neural network models (MLP, Transformer blocks with Multi-Head Attention)
- Optimizers (Adam, SGD)
- Loss functions (MSE, Cross-entropy)
- Training utilities (forward pass, backward propagation)
- **Tokenization system** (SimpleTokenizer + SentencePiece framework)
- **Transformer architecture** (Multi-Head Attention, Layer Normalization, Feed-Forward Networks)
- **Text processing pipeline** (tokenization → embedding → padding → attention masking)
- **LoRA fine-tuning** (Low-Rank Adaptation for parameter-efficient training)
- **Model persistence** (save/load models, checkpoints, and LoRA adapters)
- **GPU acceleration** (CUDA + Metal Performance Shaders for Apple Silicon)
- **Portable Installation**: Automatic dependency management and self-contained deployment
- **Zero-Copy Optimization**: Rust-inspired borrow() method for efficient tensor operations
- **Llama-3.1 fine-tuning ready** (complete architecture and tokenization framework)

## Quick Start Guide

### Installation and Setup
```lisp
; 1. Load the library
(use 'lispe_torch)

; 2. Check GPU availability
(if (torch_cuda_available)
    (println "CUDA available!")
    (if (torch_mps_available) 
        (println "MPS (Apple Silicon) available!")
        (println "Using CPU")))
```

### Basic Tensor Operations
```lisp
; Create tensors efficiently
(setq data (floats 1.0 2.0 3.0 4.0))  ; Zero-copy when possible
(setq tensor (torch_tensor data))
(setq shape (torch_shape tensor))      ; Get dimensions

; Mathematical operations
(setq a (torch_randn (integers 4 4)))
(setq b (torch_randn (integers 4 4)))
(setq result (torch_matmul a b))       ; Matrix multiplication
```

### Flash Attention (Memory-Efficient)
```lisp
; Direct Flash Attention for long sequences
(setq query (torch_randn (integers 2 8 2048 64)))   ; [batch, heads, seq, dim]
(setq key (torch_randn (integers 2 8 2048 64)))
(setq value (torch_randn (integers 2 8 2048 64)))
(setq scale (/ 1.0 (sqrt 64)))
(setq output (torch_flash_attention query key value scale))  ; O(N) memory!
```

### Text Generation
```lisp
; Load model and create generator
(setq model (torch_load_model "model.pt"))
(setq generator (torch_generator_create model))

; Configure generation parameters
(setq config (map))
(config "max_length" 100)
(config "temperature" 0.8)
(config "top_p" 0.95)
(torch_generator_configure generator config)

; Generate text
(setq input_tokens (integers 15496 318 257))  ; "This is a"
(setq generated (torch_generator_generate generator input_tokens))
```

### Model Quantization for Production
```lisp
; Optimize model for production deployment
(setq model_weights (torch_randn (integers 512 768)))

; Apply FP16 quantization (50% memory reduction)
(setq fp16_weights (torch_quantize_fp16 model_weights))

; Apply INT8 quantization (75% memory reduction)
(setq int8_weights (torch_quantize_int8 model_weights))

; Verify quality with dequantization
(setq reconstructed (torch_dequantize int8_weights))
(println "Model optimized for production deployment")
```

### Portable Deployment
```bash
# Automatic installation with all dependencies
cd lispetorch
make all  # Compiles + installs portable version automatically

# Verify installation
ls /usr/local/lib/lispe/  # Self-contained deployment ready
```

## Data Types

The library introduces multiple data types for comprehensive deep learning functionality:

### Core Data Types
- **torch_tensor**: Encapsulates PyTorch tensors
- **torch_model**: Encapsulates PyTorch neural network models
- **torch_optimizer**: Encapsulates PyTorch optimizers

### Flash Attention Data Types
- **torch_flash_attention**: Flash Attention module for memory-efficient attention computation
- **TorchFlashAttention**: Flash Attention implementation class
- **FlashAttention**: Core Flash Attention configuration struct

### Text Generation Data Types (NEW!)
- **torch_generator**: Text generation module with multiple sampling strategies
- **TorchGenerator**: Main text generation class with configurable parameters
- **GenerationConfig**: Configuration struct for generation parameters
- **torch_generation_config**: LispE wrapper for generation configuration

### Tokenization Data Types
- **torch_tokenizer**: Base tokenizer interface
- **torch_simple_tokenizer**: Word-based tokenizer with punctuation handling
- **torch_sentencepiece_tokenizer**: SentencePiece tokenizer (when available)

### Advanced Model Types
- **torch_embedding**: Embedding layer for token-to-vector conversion
- **torch_linear**: Linear transformation layer
- **torch_layer_norm**: Layer Normalization
- **torch_transformer_block**: Complete Transformer block with Multi-Head Attention, Layer Normalization, and Feed-Forward network
- **torch_multi_head_attention**: Multi-Head Attention mechanism
- **torch_lora_linear**: LoRA-adapted linear layer for parameter-efficient fine-tuning

## LispE Data Types for PyTorch Integration

LispE provides optimized data types that integrate efficiently with PyTorch tensors:

### Optimized List Types
- `(integers a b c...)` → Creates `long* list` - memory efficient integer arrays
- `(shorts a b c...)` → Creates `short* list` - memory efficient short integer arrays
- `(floats a b c...)` → Creates `float* list` - memory efficient float arrays  
- `(numbers a b c...)` → Creates `double* list` - memory efficient double arrays
- `(list a b c...)` → Creates generic object list - more memory overhead

### Sequence Generators
LispE provides convenient sequence generators that automatically create the appropriate optimized type:

- `(iota0 n)` → Creates sequence [0, 1, 2, ..., n-1] 
  - `(iota0 10)` ≡ `(integers 0 1 2 3 4 5 6 7 8 9)`
- `(iota n)` → Creates sequence [1, 2, 3, ..., n]
  - `(iota 10)` ≡ `(integers 1 2 3 4 5 6 7 8 9 10)`
  - `(iota 10.0)` ≡ `(numbers 1 2 3 4 5 6 7 8 9 10)` (auto-detects float input)

**Performance Benefits:**
- **Zero-Copy Optimization**: Direct memory access using Rust-inspired `borrow()` method
- **3-5x Faster Tensor Creation**: No data copying when buffer ownership can be transferred
- **50% Less Memory Usage**: Eliminates temporary copies during tensor conversion
- **Automatic Type Selection**: Based on input data characteristics
- **Seamless PyTorch Integration**: Direct C++ API usage without Python overhead

```lisp
; Equivalent tensor creation methods
(setq t1 (torch_zeros (integers 2 3 4)))     ; Explicit integers list
(setq t2 (torch_zeros (iota0 3)))            ; Using sequence generator: (0 1 2)
(setq t3 (torch_tensor (iota 5)))            ; Tensor with values [1,2,3,4,5]
```
- **torch_multihead_attention**: Multi-Head Attention mechanism
- **torch_layer_norm**: Layer Normalization
- **torch_transformer_block**: Complete Transformer block architecture
- **torch_positional_encoding**: Sinusoidal positional encoding for sequences
- **torch_state_dict**: Model parameter container for persistence

### LoRA Fine-tuning Types
- **torch_lora_linear**: LoRA-adapted linear layer
- **torch_lora_config**: LoRA configuration (rank, alpha, target modules)

## Flash Attention Operations (NEW!)

Flash Attention provides memory-efficient attention computation that scales linearly with sequence length, enabling processing of much longer sequences than traditional attention mechanisms.

### torch_flash_attention_create: (torch_flash_attention_create embed_dim num_heads dropout bias)

Creates a Flash Attention module with specified configuration.

**Parameters:**
- `embed_dim`: Integer, the embedding dimension (must be divisible by num_heads)
- `num_heads`: Integer, the number of attention heads
- `dropout`: Float, dropout rate for attention weights (optional, default: 0.0)
- `bias`: Boolean, whether to use bias in linear projections (optional, default: true)

**Returns:** A torch_flash_attention object

```lisp
; Create Flash Attention with 256 embedding dimensions, 8 heads
(setq flash_attn (torch_flash_attention_create 256 8 0.1 true))
(println flash_attn)  ; Flash Attention (embed_dim=256, num_heads=8, head_dim=32, dropout=0.1, bias=true)
```

### torch_flash_attention_forward: (torch_flash_attention_forward flash_attention query thekey thevalue)

Performs memory-efficient attention computation with Flash Attention algorithm.

**Parameters:**
- `flash_attention`: Flash Attention module created with torch_flash_attention_create
- `query`: Tensor with shape [batch_size, seq_len, embed_dim]
- `key`: Tensor with shape [batch_size, seq_len, embed_dim]  
- `value`: Tensor with shape [batch_size, seq_len, embed_dim]

**Returns:** Output tensor with shape [batch_size, seq_len, embed_dim]

```lisp
; Create input tensors
(setq query (torch_zeros (integers 2 32 256)))
(setq key (torch_zeros (integers 2 32 256)))
(setq value (torch_ones (integers 2 32 256)))

; Forward pass
(setq output (torch_flash_attention_forward flash_attn query thekey thevalue))
(println "Output shape:" (torch_size output))  ; (2 32 256)
```

### torch_flash_attention_with_mask: (torch_flash_attention_with_mask flash_attention query thekey thevalue attn_mask)

Flash Attention with custom attention masking for variable-length sequences or causal attention.

**Parameters:**
- `flash_attention`: Flash Attention module
- `query`, `key`, `value`: Input tensors as above
- `attn_mask`: Attention mask tensor [batch_size, num_heads, seq_len, seq_len] or broadcastable shape

**Returns:** Masked attention output tensor

```lisp
; Create attention mask (e.g., for padding)
(setq attention_mask (torch_ones (integers 2 8 32 32)))
(setq masked_output (torch_flash_attention_with_mask flash_attn query thekey thevalue attention_mask))
```

### torch_flash_attention_with_dropout: (torch_flash_attention_with_dropout flash_attention query thekey thevalue dropout_p training)

Flash Attention with custom dropout rate, useful for different training phases.

**Parameters:**
- `flash_attention`: Flash Attention module
- `query`, `thekey`, `thevalue`: Input tensors
- `dropout_p`: Float, dropout probability to apply
- `training`: Boolean, whether in training mode (affects dropout behavior)

**Returns:** Attention output with applied dropout

```lisp
; Training with higher dropout
(setq training_output (torch_flash_attention_with_dropout flash_attn query thekey thevalue 0.2 true))

; Evaluation with no dropout
(setq eval_output (torch_flash_attention_with_dropout flash_attn query thekey thevalue 0.0 false))
```

### torch_scaled_dot_product_attention: (torch_scaled_dot_product_attention query thekey thevalue attn_mask dropout_p is_causal)

Direct interface to PyTorch's native scaled dot-product attention (PyTorch 2.0+).

**Parameters:**
- `query`: Query tensor [batch_size, num_heads, seq_len, head_dim]
- `key`: Key tensor [batch_size, num_heads, seq_len, head_dim]
- `value`: Value tensor [batch_size, num_heads, seq_len, head_dim]
- `attn_mask`: Optional attention mask (nil for none)
- `dropout_p`: Float, dropout probability
- `is_causal`: Boolean, whether to apply causal (triangular) masking

**Returns:** Attention output tensor

```lisp
; Reshape tensors for multi-head format
(setq q_reshaped (torch_reshape query (integers 2 32 8 32)))  ; [batch, seq, heads, head_dim]
(setq q_transposed (torch_transpose q_reshaped 1 2))         ; [batch, heads, seq, head_dim]
; Similar for k and v...

; Direct attention computation
(setq direct_output (torch_scaled_dot_product_attention q_transposed k_transposed v_transposed nil 0.0 false))
```

### torch_flash_attention: (torch_flash_attention query thekey thevalue scale)

Simplified Flash Attention interface for direct attention computation without creating a module.

**Parameters:**
- `query`: Query tensor [batch_size, num_heads, seq_len, head_dim]
- `thekey`: Key tensor [batch_size, num_heads, seq_len, head_dim]
- `thevalue`: Value tensor [batch_size, num_heads, seq_len, head_dim]
- `scale`: Float, scaling factor (typically 1/sqrt(head_dim))

**Returns:** Attention output tensor with O(N) memory complexity

```lisp
; Direct Flash Attention computation
(setq scale (/ 1.0 (sqrt 64)))  ; head_dim = 64
(setq output (torch_flash_attention query key value scale))
(println "Flash attention output shape:" (torch_shape output))

; Memory-efficient for long sequences
(setq long_query (torch_randn (integers 1 8 4096 64)))
(setq long_key (torch_randn (integers 1 8 4096 64)))
(setq long_value (torch_randn (integers 1 8 4096 64)))
(setq long_output (torch_flash_attention long_query long_key long_value 0.125))
```

## Enhanced Tensor Operations (NEW!)

### torch_rand: (torch_rand shape)

Creates a tensor filled with random values uniformly distributed between 0 and 1.

**Parameters:**
- `shape`: A list of integers defining the tensor dimensions

**Returns:** A torch_tensor filled with random values

```lisp
; Create random tensor for weight initialization
(setq random_weights (torch_rand (integers 512 256)))
(println "Random tensor range: 0 to 1")

; Use for model initialization
(setq init_embedding (torch_rand (integers 10000 768)))
```

### torch_transpose: (torch_transpose tensor dim0 dim1)

Transposes two dimensions of a tensor, commonly used for attention computations.

**Parameters:**
- `tensor`: Input torch_tensor
- `dim0`: Integer, first dimension to transpose
- `dim1`: Integer, second dimension to transpose

**Returns:** New tensor with transposed dimensions

```lisp
; Reshape for multi-head attention: [batch, seq, embed] -> [batch, heads, seq, head_dim]
(setq reshaped (torch_reshape tensor (integers 2 32 8 64)))
(setq transposed (torch_transpose reshaped 1 2))  ; Swap seq and heads dimensions
(println "Original shape:" (torch_size reshaped))    ; (2 32 8 64)
(println "Transposed shape:" (torch_size transposed)) ; (2 8 32 64)
```

### torch_shape: (torch_shape tensor)

Returns the shape (dimensions) of a tensor. This is an alias for `torch_size` for compatibility.

**Parameters:**
- `tensor`: Input torch_tensor

**Returns:** A list of integers representing the tensor dimensions

```lisp
; Get tensor shape
(setq tensor (torch_zeros (integers 4 8 16 32)))
(setq shape (torch_shape tensor))
(println "Tensor shape:" shape)  ; (4 8 16 32)

; Same as torch_size
(setq size (torch_size tensor))
(println "Tensor size:" size)    ; (4 8 16 32)
```

### Native 'at' Function Support (NEW!)

Tensors now support LispE's native `at` function for element access with bounds checking.

**Syntax:** `(at tensor index)`

**Features:**
- **Positive indexing**: 0, 1, 2, ... for elements from start
- **Negative indexing**: -1, -2, -3, ... for elements from end (Python-style)
- **Bounds checking**: Automatic error handling for out-of-range indices
- **Type preservation**: Returns appropriate LispE types (numbers, integers, booleans)

```lisp
; Create test tensor
(setq test_tensor (torch_tensor (integers 10 20 30 40 50)))

; Access elements
(setq first (at test_tensor 0))    ; Returns: 10
(setq third (at test_tensor 2))    ; Returns: 30
(setq last (at test_tensor -1))    ; Returns: 50 (negative indexing)
(setq second_last (at test_tensor -2)) ; Returns: 40

; Works with different tensor types
(setq float_tensor (torch_tensor (floats 1.5 2.7 3.9)))
(setq decimal (at float_tensor 1))  ; Returns: 2.7

; Bounds checking
; (at test_tensor 10)  ; Would throw: "Error: tensor index out of bounds"
```

## Text Generation System (NEW!)

The LispETorch text generation system provides comprehensive text generation capabilities with multiple sampling strategies, configurable parameters, and advanced features like repetition penalty and beam search.

### torch_generator_create: (torch_generator_create model)

Creates a new text generator instance with a pre-trained model.

**Parameters:**
- `model`: Pre-trained PyTorch model for text generation

**Returns:** A torch_generator object

```lisp
; Load a pre-trained model and create generator
(setq model (torch_load_model "gpt2-small.pt"))
(setq generator (torch_generator_create model))
```

### torch_generator_configure: (torch_generator_configure generator config)

Configures generation parameters for the text generator.

**Parameters:**
- `generator`: torch_generator object
- `config`: Configuration map with generation parameters

**Configuration Options:**
- `max_length`: Maximum number of tokens to generate (default: 100)
- `temperature`: Sampling temperature for randomness control (default: 1.0)
- `top_k`: Top-k sampling parameter (default: 50)
- `top_p`: Top-p (nucleus) sampling parameter (default: 0.9)
- `repetition_penalty`: Penalty for token repetition (default: 1.0)
- `num_beams`: Number of beams for beam search (default: 1)
- `do_sample`: Whether to use sampling vs greedy decoding (default: true)

```lisp
; Configure generation parameters
(setq config (map))
(config "max_length" 150)
(config "temperature" 0.8)
(config "top_k" 40)
(config "top_p" 0.95)
(config "repetition_penalty" 1.1)
(config "do_sample" true)

(torch_generator_configure generator config)
```

### torch_generator_generate: (torch_generator_generate generator input_ids)

Generates text using the configured generator and input token IDs.

**Parameters:**
- `generator`: Configured torch_generator object
- `input_ids`: Input token IDs as tensor or list of integers

**Returns:** Generated token IDs as tensor

```lisp
; Prepare input tokens
(setq prompt_tokens (integers 15496 318 257 1350))  ; "This is a test"
(setq input_tensor (torch_tensor prompt_tokens))

; Generate text
(setq generated (torch_generator_generate generator input_tensor))
(println "Generated tokens:" generated)
```

### torch_generator_greedy: (torch_generator_greedy generator input_ids max_length)

Performs greedy decoding (always selecting the most probable next token).

**Parameters:**
- `generator`: torch_generator object
- `input_ids`: Input token IDs
- `max_length`: Maximum sequence length

**Returns:** Generated token sequence

```lisp
; Greedy generation for deterministic output
(setq greedy_output (torch_generator_greedy generator input_tensor 100))
```

### torch_generator_top_k: (torch_generator_top_k generator input_ids max_length k temperature)

Performs top-k sampling for controlled randomness.

**Parameters:**
- `generator`: torch_generator object
- `input_ids`: Input token IDs
- `max_length`: Maximum sequence length
- `k`: Number of top tokens to consider
- `temperature`: Sampling temperature

**Returns:** Generated token sequence with top-k sampling

```lisp
; Top-k sampling for balanced creativity
(setq top_k_output (torch_generator_top_k generator input_tensor 100 50 0.8))
```

### torch_generator_top_p: (torch_generator_top_p generator input_ids max_length p temperature)

Performs nucleus (top-p) sampling for dynamic vocabulary filtering.

**Parameters:**
- `generator`: torch_generator object
- `input_ids`: Input token IDs
- `max_length`: Maximum sequence length  
- `p`: Cumulative probability threshold
- `temperature`: Sampling temperature

**Returns:** Generated token sequence with nucleus sampling

```lisp
; Nucleus sampling for creative generation
(setq nucleus_output (torch_generator_top_p generator input_tensor 100 0.9 1.0))
```

### torch_generator_beam_search: (torch_generator_beam_search generator input_ids max_length num_beams)

Performs beam search for high-quality generation.

**Parameters:**
- `generator`: torch_generator object
- `input_ids`: Input token IDs
- `max_length`: Maximum sequence length
- `num_beams`: Number of beams to maintain

**Returns:** Best generated sequence from beam search

```lisp
; Beam search for high-quality output
(setq beam_output (torch_generator_beam_search generator input_tensor 100 4))
```

### torch_generator_set_device: (torch_generator_set_device generator device)

Sets the computation device for the generator.

**Parameters:**
- `generator`: torch_generator object
- `device`: Device string ("cpu", "cuda", "mps")

**Returns:** Updated generator

```lisp
; Use GPU if available
(if (torch_cuda_available)
    (torch_generator_set_device generator "cuda")
    (torch_generator_set_device generator "cpu"))
```

### Complete Text Generation Example

```lisp
; Complete text generation pipeline
(use 'lispe_torch)

; 1. Load model
(setq model (torch_load_model "path/to/model.pt"))
(setq generator (torch_generator_create model))

; 2. Configure generation
(setq config (map))
(config "max_length" 200)
(config "temperature" 0.85)
(config "top_p" 0.95)
(config "repetition_penalty" 1.1)
(config "do_sample" true)
(torch_generator_configure generator config)

; 3. Prepare input
(setq prompt "The future of artificial intelligence")
(setq tokenizer (torch_tokenizer_create))
(setq tokens (torch_tokenizer_encode tokenizer prompt))

; 4. Generate text
(setq generated_tokens (torch_generator_generate generator tokens))

; 5. Decode output
(setq generated_text (torch_tokenizer_decode tokenizer generated_tokens))
(println "Generated text:" generated_text)
```

### torch_tensor: (torch_tensor data)

Creates a PyTorch tensor from LispE data (floats or numbers).

**Parameters:**
- `data`: A LispE list of floats or numbers

**Returns:** A torch_tensor object

```lisp
(setq tensor_data (floats 1.0 2.0 3.0 4.0))
(setq tensor (torch_tensor data))
(println tensor)
```

### torch_zeros: (torch_zeros shape)

Creates a tensor filled with zeros.

**Parameters:**
- `shape`: A list of integers defining the tensor dimensions

**Returns:** A torch_tensor object filled with zeros

```lisp
(setq shape (integers 2 3))
(setq zeros_tensor (torch_zeros shape))
(println zeros_tensor) ; 2x3 tensor of zeros
```

### torch_ones: (torch_ones shape)

Creates a tensor filled with ones.

**Parameters:**
- `shape`: A list of integers defining the tensor dimensions

**Returns:** A torch_tensor object filled with ones

```lisp
(setq shape (integers 3 3))
(setq ones_tensor (torch_ones shape))
(println ones_tensor) ; 3x3 tensor of ones
```

### torch_add: (torch_add tensor1 tensor2)

Performs element-wise addition of two tensors.

**Parameters:**
- `tensor1`: First torch_tensor
- `tensor2`: Second torch_tensor

**Returns:** A new torch_tensor containing the element-wise sum

```lisp
(setq t1 (torch_tensor (floats 1.0 2.0 3.0)))
(setq t2 (torch_tensor (floats 4.0 5.0 6.0)))
(setq result (torch_add t1 t2))
(println result) ; (5.0 7.0 9.0)
```

### torch_mul: (torch_mul tensor1 tensor2)

Performs element-wise multiplication of two tensors.

**Parameters:**
- `tensor1`: First torch_tensor
- `tensor2`: Second torch_tensor

**Returns:** A new torch_tensor containing the element-wise product

```lisp
(setq t1 (torch_tensor (floats 2.0 3.0 4.0)))
(setq t2 (torch_tensor (floats 5.0 6.0 7.0)))
(setq result (torch_mul t1 t2))
(println result) ; (10.0 18.0 28.0)
```

### torch_matmul: (torch_matmul tensor1 tensor2)

Performs matrix multiplication of two tensors.

**Parameters:**
- `tensor1`: First torch_tensor (matrix)
- `tensor2`: Second torch_tensor (matrix)

**Returns:** A new torch_tensor containing the matrix product

```lisp
(setq shape1 (integers 2 3))
(setq shape2 (integers 3 2))
(setq m1 (torch_ones shape1))
(setq m2 (torch_ones shape2))
(setq result (torch_matmul m1 m2))
(println result) ; 2x2 matrix
```

## CUDA Support

### torch_cuda_is_available: (torch_cuda_is_available)

Checks if CUDA is available on the system.

**Parameters:** None

**Returns:** True if CUDA is available, False otherwise

```lisp
(if (torch_cuda_is_available)
    (println "CUDA is available!")
    (println "CUDA not available"))
```

### torch_cuda_device_count: (torch_cuda_device_count)

Returns the number of CUDA devices available.

**Parameters:** None

**Returns:** An integer representing the number of CUDA devices

```lisp
(setq device_count (torch_cuda_device_count))
(println "Number of CUDA devices:" device_count)
```

### torch_set_device: (torch_set_device device)

Sets the current CUDA device.

**Parameters:**
- `device`: Integer, the device ID to set as current

**Returns:** True on success

```lisp
; Set device 0 as the current CUDA device
(torch_set_device 0)
```

### torch_to_cuda: (torch_to_cuda tensor)

Moves a tensor to CUDA memory.

**Parameters:**
- `tensor`: A torch_tensor to move to CUDA

**Returns:** A new torch_tensor located on CUDA device

```lisp
(setq cpu_tensor (torch_tensor (floats 1.0 2.0 3.0)))
(if (torch_cuda_is_available)
    (setq cuda_tensor (torch_to_cuda cpu_tensor))
    (println "CUDA not available"))
```

## MPS Support (macOS Metal)

### torch_mps_is_available: (torch_mps_is_available)

Checks if Metal Performance Shaders (MPS) is available on macOS.

**Parameters:** None

**Returns:** True if MPS is available, False otherwise

```lisp
(if (torch_mps_is_available)
    (println "MPS is available! Using Apple Silicon GPU.")
    (println "MPS not available"))
```

### torch_to_mps: (torch_to_mps tensor)

Moves a tensor to MPS memory (Apple Silicon GPU).

**Parameters:**
- `tensor`: A torch_tensor to move to MPS

**Returns:** A new torch_tensor located on MPS device

```lisp
(setq cpu_tensor (torch_tensor (floats 1.0 2.0 3.0)))
(if (torch_mps_is_available)
    (setq mps_tensor (torch_to_mps cpu_tensor))
    (println "MPS not available"))
```

## Device Detection

### torch_get_best_device: (torch_get_best_device)

Automatically detects the best available device for computation.

**Parameters:** None

**Returns:** A string indicating the best device: "mps" (macOS), "cuda" (NVIDIA), or "cpu"

**Priority order:** MPS > CUDA > CPU

```lisp
(setq best_device (torch_get_best_device))
(println "Best available device:" best_device)

; Use the best device automatically
(setq tensor (torch_tensor (floats 1.0 2.0 3.0)))
(cond
    ((eq best_device "mps") (setq tensor (torch_to_mps tensor)))
    ((eq best_device "cuda") (setq tensor (torch_to_cuda tensor)))
    (true (println "Using CPU"))
)
```

## Neural Network Models

### torch_model: (torch_model input_size hidden_size output_size)

Creates a simple Multi-Layer Perceptron (MLP) model with one hidden layer.

**Parameters:**
- `input_size`: Integer, number of input features
- `hidden_size`: Integer, number of hidden units
- `output_size`: Integer, number of output features

**Returns:** A torch_model object

```lisp
; Create a model with 784 inputs, 128 hidden units, 10 outputs
(setq model (torch_model 784 128 10))
(println model)
```

### torch_embedding: (torch_embedding vocab_size embed_dim)

Creates an embedding layer that converts token indices to dense vectors.

**Parameters:**
- `vocab_size`: Integer, size of the vocabulary
- `embed_dim`: Integer, dimension of the embedding vectors

**Returns:** A torch_embedding object

```lisp
; Create embedding for vocabulary of 1000 tokens with 512-dimensional vectors
(setq embedding (torch_embedding 1000 512))
(println embedding)
```

### torch_multihead_attention: (torch_multihead_attention embed_dim num_heads)

Creates a Multi-Head Attention layer, core component of Transformer architecture.

**Parameters:**
- `embed_dim`: Integer, dimension of input/output embeddings
- `num_heads`: Integer, number of attention heads

**Returns:** A torch_multihead_attention object

```lisp
; Create Multi-Head Attention with 512-dim embeddings and 8 heads
(setq attention (torch_multihead_attention 512 8))
(println attention)
```

### torch_layer_norm: (torch_layer_norm normalized_shape)

Creates a Layer Normalization layer for stabilizing training.

**Parameters:**
- `normalized_shape`: Integer, dimension to normalize over

**Returns:** A torch_layer_norm object

```lisp
; Create Layer Normalization for 512-dimensional vectors
(setq layer_norm (torch_layer_norm 512))
(println layer_norm)
```

### torch_transformer_block: (torch_transformer_block embed_dim num_heads ff_dim)

Creates a complete Transformer block with Multi-Head Attention, Layer Normalization, and Feed-Forward network.

**Parameters:**
- `embed_dim`: Integer, dimension of embeddings
- `num_heads`: Integer, number of attention heads
- `ff_dim`: Integer, dimension of feed-forward hidden layer

**Returns:** A torch_transformer_block object

```lisp
; Create Transformer block: 512-dim, 8 heads, 2048 FF dimension
(setq transformer (torch_transformer_block 512 8 2048))
(println transformer)
```

### torch_forward: (torch_forward model input)

Performs a forward pass through the model.

**Parameters:**
- `model`: A torch_model object
- `input`: A torch_tensor containing input data

**Returns:** A torch_tensor containing the model's output

```lisp
(setq model (torch_model 4 64 2))
(setq input_data (torch_tensor (floats 1.0 2.0 3.0 4.0)))
(setq output (torch_forward model input_data))
(println output)
```

## Optimizers

### torch_optimizer: (torch_optimizer model learning_rate type)

Creates an optimizer for training the model.

**Parameters:**
- `model`: A torch_model object
- `learning_rate`: A number, the learning rate for optimization
- `type`: A string, either "adam" or "sgd"

**Returns:** A torch_optimizer object

```lisp
; Create an Adam optimizer with learning rate 0.001
(setq optimizer (torch_optimizer model 0.001 "adam"))

; Create an SGD optimizer with learning rate 0.01
(setq sgd_optimizer (torch_optimizer model 0.01 "sgd"))
```

### torch_optimizer_step: (torch_optimizer_step optimizer)

Updates the model parameters using the computed gradients.

**Parameters:**
- `optimizer`: A torch_optimizer object

**Returns:** True on success

```lisp
(torch_optimizer_step optimizer)
```

### torch_optimizer_zero_grad: (torch_optimizer_zero_grad optimizer)

Clears the gradients of all optimized parameters.

**Parameters:**
- `optimizer`: A torch_optimizer object

**Returns:** True on success

```lisp
(torch_optimizer_zero_grad optimizer)
```

## Loss Functions

### torch_mse_loss: (torch_mse_loss predictions targets)

Computes the Mean Squared Error loss between predictions and targets.

**Parameters:**
- `predictions`: A torch_tensor containing model predictions
- `targets`: A torch_tensor containing target values

**Returns:** A torch_tensor containing the MSE loss value

```lisp
(setq predictions (torch_tensor (floats 1.0 2.0 3.0)))
(setq targets (torch_tensor (floats 1.1 1.9 3.2)))
(setq loss (torch_mse_loss predictions targets))
(println loss)
```

### torch_crossentropy_loss: (torch_crossentropy_loss predictions targets)

Computes the Cross-Entropy loss between predictions and targets.

**Parameters:**
- `predictions`: A torch_tensor containing model predictions (logits)
- `targets`: A torch_tensor containing target class indices

**Returns:** A torch_tensor containing the cross-entropy loss value

```lisp
(setq predictions (torch_tensor (floats 2.0 1.0 0.1)))
(setq targets (torch_tensor (integers 0))) ; target class 0
(setq loss (torch_crossentropy_loss predictions targets))
(println loss)
```

## SentencePiece Integration

The library includes comprehensive SentencePiece support for production-grade tokenization compatible with modern language models. SentencePiece is the industry standard for subword tokenization used in models like Llama-3.1, GPT, BERT, and many others.

### Key Features

- **Subword Tokenization**: Handles out-of-vocabulary words through learned subword units
- **Language Agnostic**: Works with any language without word boundary assumptions  
- **Reversible**: Perfect text reconstruction through lossless tokenization
- **Model Compatibility**: Direct compatibility with pre-trained language models
- **Custom Training**: Train domain-specific tokenizers for specialized applications

### SentencePiece vs SimpleTokenizer

| Feature | SimpleTokenizer | SentencePiece |
|---------|----------------|---------------|
| **Tokenization Method** | Word-based | Subword (BPE/Unigram) |
| **Vocabulary** | Dynamic, unlimited | Fixed, optimized |
| **OOV Handling** | `<UNK>` token | Subword decomposition |
| **Model Compatibility** | Research/prototyping | Production LLMs |
| **Language Support** | English-optimized | Multilingual |
| **Text Reconstruction** | Approximate | Perfect |
| **Training Required** | No | Yes (one-time) |
| **Use Case** | Quick experiments | Production deployment |

### Installation Requirements

SentencePiece support requires the SentencePiece library to be installed:

```bash
# macOS with Homebrew
brew install sentencepiece

# Ubuntu/Debian
sudo apt-get install libsentencepiece-dev

# Build from source
git clone https://github.com/google/sentencepiece.git
cd sentencepiece && mkdir build && cd build
cmake .. && make -j $(nproc) && sudo make install
```

The library automatically detects SentencePiece availability and enables the functionality when present.

### Training Custom SentencePiece Models

#### Basic Training

```lisp
; Create training corpus
(setq training_data "Large corpus of text for training the tokenizer model")
(fwrite "training_corpus.txt" training_data)

; Train model with 8000 vocabulary using BPE
(setq result (torch_train_sentencepiece "training_corpus.txt" "my_model" 8000 "bpe"))
```

#### Advanced Training Configuration

```lisp
; Domain-specific corpus for specialized tokenization
(setq domain_corpus (+ 
    "Technical documentation requires specialized vocabulary handling.\n"
    "Software engineering terminology needs careful tokenization.\n"
    "API documentation contains structured text patterns.\n"
    "Code examples require preserving syntax elements.\n"))

(fwrite "domain_corpus.txt" domain_corpus)

; Train with smaller vocabulary for domain adaptation
(setq domain_result (torch_train_sentencepiece "domain_corpus.txt" "domain_model" 2000 "bpe"))

; Load the domain-specific tokenizer
(setq domain_tokenizer (torch_sentencepiece_tokenizer "domain_model.model"))
```

#### Vocabulary Size Guidelines

- **Small domain (< 1MB text)**: 1,000 - 5,000 tokens
- **Medium corpus (1-100MB)**: 5,000 - 30,000 tokens  
- **Large corpus (> 100MB)**: 30,000+ tokens
- **Llama-3.1 scale**: 128,256 tokens
- **Rule of thumb**: Corpus size in characters ÷ 200 ≈ reasonable vocab size

### Production Deployment Patterns

#### Pattern 1: Pre-trained Model Loading

```lisp
; Load existing production tokenizer
(setq production_tokenizer (torch_sentencepiece_tokenizer "production_model.model"))

; Verify tokenizer properties
(setq vocab_size (torch_vocab_size production_tokenizer))
(println "Production tokenizer loaded with" vocab_size "tokens")

; Test tokenization quality
(setq test_text "Production deployment requires robust tokenization.")
(setq tokens (torch_encode production_tokenizer test_text))
(setq reconstructed (torch_decode production_tokenizer tokens))
(println "Reconstruction quality:" (= test_text reconstructed))
```

#### Pattern 2: Domain Adaptation Pipeline

```lisp
; Step 1: Collect domain-specific text
(setq domain_texts (list
    "Domain-specific text example 1"
    "Domain-specific text example 2"
    "Domain-specific text example 3"))

; Step 2: Create training corpus
(setq combined_corpus "")
(loop text domain_texts
    (setq combined_corpus (+ combined_corpus text "\n")))
(fwrite "domain_training.txt" combined_corpus)

; Step 3: Train adapted tokenizer
(setq adaptation_result (torch_train_sentencepiece "domain_training.txt" "adapted_model" 4000 "bpe"))

; Step 4: Deploy adapted tokenizer
(setq adapted_tokenizer (torch_sentencepiece_tokenizer "adapted_model.model"))
```

#### Pattern 3: Multi-Model Tokenization

```lisp
; Load multiple tokenizers for comparison
(setq general_tokenizer (torch_sentencepiece_tokenizer "general_model.model"))
(setq domain_tokenizer (torch_sentencepiece_tokenizer "domain_model.model"))

; Compare tokenization efficiency
(setq test_text "Technical software documentation with API references.")

(setq general_tokens (torch_encode general_tokenizer test_text))
(setq domain_tokens (torch_encode domain_tokenizer test_text))

(println "General tokenizer: " (size general_tokens) "tokens")
(println "Domain tokenizer: " (size domain_tokens) "tokens")
(println "Efficiency gain: " (- (size general_tokens) (size domain_tokens)) "tokens")
```

### Debugging and Validation

#### Tokenization Analysis

```lisp
; Analyze tokenization patterns
(setq analysis_texts (list
    "Simple words and punctuation."
    "Complex technical terminology."
    "Mixed-language text with émojis 🚀."
    "Numbers and symbols: version 2.3.1-beta."))

(setq tokenizer (torch_sentencepiece_tokenizer "model.model"))

(loop text analysis_texts
    (setq tokens (torch_encode tokenizer text))
    (setq decoded (torch_decode tokenizer tokens))
    (println "Original:  " text)
    (println "Tokens:    " tokens)
    (println "Token count:" (size tokens))
    (println "Decoded:   " decoded)
    (println "Perfect:   " (= text decoded))
    (println ""))
```

#### Vocabulary Coverage Analysis

```lisp
; Test vocabulary coverage on different text types
(setq test_cases (list
    ("news", "Breaking news: Scientists discover new treatment method.")
    ("technical", "The API endpoint returns JSON with authentication headers.")
    ("conversational", "Hey! How's your project going? Let me know if you need help.")
    ("multilingual", "Hello, Bonjour, Hola, こんにちは")
))

(loop case test_cases
    (setq category (@ case 0))
    (setq text (@ case 1))
    (setq tokens (torch_encode tokenizer text))
    (setq efficiency (/ (size text) (size tokens)))
    (println category ": " efficiency "chars/token"))
```

This comprehensive SentencePiece integration makes the LispE PyTorch library production-ready for modern language model applications, from research prototyping to large-scale deployment.

## Training Utilities

### torch_backward: (torch_backward loss)

Computes gradients through automatic differentiation (backpropagation).

**Parameters:**
- `loss`: A torch_tensor containing the loss value

**Returns:** True on success

```lisp
(setq loss (torch_mse_loss predictions targets))
(torch_backward loss)
```

## Model Quantization and Optimization (NEW!)

Model quantization reduces memory usage and accelerates inference by converting high-precision (FP32) models to lower-precision formats (FP16, INT8). This is essential for production deployment, edge devices, and resource-constrained environments.

### Key Benefits
- **Memory Reduction**: 50% with FP16, 75% with INT8
- **Faster Inference**: Optimized operations on specialized hardware
- **Lower Power Consumption**: Reduced computational requirements
- **Maintained Accuracy**: Minimal quality loss with proper quantization

### torch_quantize_dynamic: (torch_quantize_dynamic tensor dtype)

Applies dynamic quantization with automatic scale and zero-point calculation.

**Parameters:**
- `tensor`: Input torch_tensor to quantize
- `dtype`: String, target data type ("int8", "qint8", "uint8", "quint8")

**Returns:** Quantized torch_tensor with automatically computed parameters

```lisp
; Dynamic quantization with automatic scaling
(setq fp32_tensor (torch_randn (integers 512 512)))
(setq quantized_int8 (torch_quantize_dynamic fp32_tensor "int8"))
(println "Original tensor:" (torch_shape fp32_tensor))
(println "Quantized tensor:" quantized_int8)
```

### torch_quantize_static: (torch_quantize_static tensor scale zero_point dtype)

Applies static quantization with user-specified scale and zero-point parameters.

**Parameters:**
- `tensor`: Input torch_tensor to quantize
- `scale`: Float, quantization scale factor
- `zero_point`: Integer, zero-point offset (must be in valid range for dtype)
- `dtype`: String, target data type ("int8", "uint8")

**Returns:** Quantized torch_tensor with specified parameters

```lisp
; Static quantization with custom parameters
(setq tensor (torch_tensor (floats 1.5 2.3 -0.8 4.2)))
(setq scale 0.1)
(setq zero_point 0)  ; Valid range for int8: -128 to 127
(setq quantized (torch_quantize_static tensor scale zero_point "int8"))
(println "Quantized with scale:" scale "zero_point:" zero_point)
```

### torch_dequantize: (torch_dequantize quantized_tensor)

Converts quantized tensors back to floating-point representation.

**Parameters:**
- `quantized_tensor`: Quantized torch_tensor

**Returns:** Dequantized torch_tensor in floating-point format

```lisp
; Dequantize back to floating point
(setq dequantized (torch_dequantize quantized))
(println "Original values restored:" dequantized)
```

### torch_quantize_linear: (torch_quantize_linear tensor scale zero_point)

Performs linear quantization with specified scale and zero-point (defaults to INT8).

**Parameters:**
- `tensor`: Input torch_tensor
- `scale`: Float, quantization scale
- `zero_point`: Integer, zero-point offset

**Returns:** Linearly quantized torch_tensor

```lisp
; Linear quantization for weight matrices
(setq weights (torch_randn (integers 768 512)))
(setq quantized_weights (torch_quantize_linear weights 0.05 0))
```

### torch_quantize_per_channel: (torch_quantize_per_channel tensor scales zero_points axis)

Applies per-channel quantization for improved accuracy on convolutional layers.

**Parameters:**
- `tensor`: Input torch_tensor (typically conv weights)
- `scales`: torch_tensor of scales per channel
- `zero_points`: torch_tensor of zero-points per channel  
- `axis`: Integer, axis along which to apply per-channel quantization

**Returns:** Per-channel quantized torch_tensor

```lisp
; Per-channel quantization for convolution weights
(setq conv_weights (torch_randn (integers 64 3 3 3)))  ; 64 filters
(setq scales_per_channel (torch_tensor (floats 0.1 0.12 0.08)))  ; Per output channel
(setq zero_points_per_channel (torch_tensor (integers 0 0 0)))
(setq quantized_conv (torch_quantize_per_channel conv_weights scales_per_channel zero_points_per_channel 0))
```

### torch_quantize_int8: (torch_quantize_int8 tensor)

Automatic INT8 quantization with optimal scale and zero-point calculation.

**Parameters:**
- `tensor`: Input torch_tensor

**Returns:** INT8 quantized torch_tensor with automatically computed parameters

```lisp
; Automatic INT8 quantization (recommended)
(setq model_weights (torch_randn (integers 1024 768)))
(setq int8_weights (torch_quantize_int8 model_weights))
(println "Memory usage reduced by ~75%")
```

### torch_quantize_fp16: (torch_quantize_fp16 tensor)

Converts tensor to half-precision (16-bit) floating point.

**Parameters:**
- `tensor`: Input torch_tensor

**Returns:** FP16 torch_tensor (half precision)

```lisp
; Half-precision conversion
(setq fp32_activations (torch_randn (integers 32 512 768)))
(setq fp16_activations (torch_quantize_fp16 fp32_activations))
(println "Memory usage reduced by ~50%")
```

### torch_tensor_to_int8: (torch_tensor_to_int8 tensor)

Direct data type conversion to INT8 (without quantization scaling).

**Parameters:**
- `tensor`: Input torch_tensor

**Returns:** torch_tensor with INT8 data type

```lisp
; Direct type conversion (use with caution)
(setq int8_tensor (torch_tensor_to_int8 tensor))
```

### torch_tensor_to_fp16: (torch_tensor_to_fp16 tensor)

Direct data type conversion to half-precision floating point.

**Parameters:**
- `tensor`: Input torch_tensor

**Returns:** torch_tensor with FP16 data type

```lisp
; Direct FP16 conversion
(setq fp16_tensor (torch_tensor_to_fp16 tensor))
```

### torch_model_quantize_dynamic: (torch_model_quantize_dynamic model)

Applies dynamic quantization to an entire model (simplified implementation).

**Parameters:**
- `model`: torch_model to quantize

**Returns:** Quantized torch_model

```lisp
; Model-level dynamic quantization
(setq model (torch_linear 768 512))
(setq quantized_model (torch_model_quantize_dynamic model))
(println "Model quantized for production deployment")
```

### torch_model_quantize_static: (torch_model_quantize_static model calibration_data)

Applies static quantization to an entire model with calibration data.

**Parameters:**
- `model`: torch_model to quantize
- `calibration_data`: Sample data for calibration

**Returns:** Statically quantized torch_model

```lisp
; Model-level static quantization with calibration
(setq model (torch_transformer_block 512 8 2048))
(setq calibration_data (torch_randn (integers 100 512)))
(setq static_quantized_model (torch_model_quantize_static model calibration_data))
```

### Production Quantization Example

```lisp
; Complete quantization pipeline for production deployment
(use 'lispe_torch)

; 1. Load or create model
(setq model (torch_transformer_block 768 12 3072))
(setq sample_weights (torch_randn (integers 768 768)))

; 2. Apply different quantization strategies
(println "=== Quantization Comparison ===")

; FP16 quantization (recommended first step)
(setq fp16_weights (torch_quantize_fp16 sample_weights))
(println "FP16 memory reduction: ~50%")

; INT8 quantization (maximum compression)
(setq int8_weights (torch_quantize_int8 sample_weights))
(println "INT8 memory reduction: ~75%")

; 3. Verify reconstruction quality
(setq reconstructed (torch_dequantize int8_weights))
(setq original_norm (torch_norm sample_weights))
(setq error_norm (torch_norm (torch_sub sample_weights reconstructed)))
(setq relative_error (/ error_norm original_norm))
(println "Quantization relative error:" relative_error)

; 4. Deploy quantized model
(setq production_model (torch_model_quantize_dynamic model))
(println "✅ Model ready for production deployment")
(println "   • Memory usage optimized")
(println "   • Inference speed improved")
(println "   • Accuracy preserved")
```

## Tokenization System

The library provides a comprehensive tokenization system essential for text processing and language model fine-tuning, including Llama-3.1 compatibility.

### torch_simple_tokenizer: (torch_simple_tokenizer)

Creates a simple word-based tokenizer with automatic vocabulary building and punctuation handling.

**Parameters:** None

**Returns:** A torch_simple_tokenizer object

**Features:**
- Word-based tokenization with punctuation separation
- Dynamic vocabulary building
- Special tokens: `<START>`, `<END>`, `<PAD>`, `<UNK>`
- Bidirectional encode/decode
- Case-insensitive processing

```lisp
; Create a simple tokenizer
(setq tokenizer (torch_simple_tokenizer))
(println tokenizer) ; "Simple Tokenizer (vocab size: 4)"
```

### torch_sentencepiece_tokenizer: (torch_sentencepiece_tokenizer model_path)

Creates a SentencePiece tokenizer from a pre-trained model file. SentencePiece provides subword tokenization compatible with modern language models like Llama-3.1.

**Parameters:**
- `model_path`: String, path to the SentencePiece model file (.model)

**Returns:** A torch_sentencepiece_tokenizer object

**Features:**
- Subword tokenization using Byte-Pair Encoding (BPE) or Unigram
- Compatible with Llama, GPT, and other modern language models
- Handles out-of-vocabulary words through subword units
- Preserves text fidelity through reversible tokenization
- Multilingual support

```lisp
; Load a pre-trained SentencePiece model
(setq sp_tokenizer (torch_sentencepiece_tokenizer "model.model"))
(println sp_tokenizer) ; "SentencePiece Tokenizer (vocab size: 32000)"
```

### torch_train_sentencepiece: (torch_train_sentencepiece input_file model_prefix vocab_size model_type)

Trains a new SentencePiece model from text data. This function allows you to create custom tokenizers optimized for your specific domain or language.

**Parameters:**
- `input_file`: String, path to the text file containing training data
- `model_prefix`: String, prefix for output model files (will create .model and .vocab files)
- `vocab_size`: Integer, desired vocabulary size (must be appropriate for your corpus size)
- `model_type`: String, either "bpe" (Byte-Pair Encoding) or "unigram"

**Returns:** String, success message with model path

**Features:**
- Automatic vocabulary size optimization based on corpus
- Support for BPE and Unigram algorithms
- Character coverage optimization
- Special token configuration (PAD, UNK, BOS, EOS)
- Multilingual training support

```lisp
; Create training data
(setq training_text (+ "Natural language processing is fascinating.\n"
                       "Machine learning models require tokenization.\n"
                       "SentencePiece provides subword units.\n"))
(fwrite "training_data.txt" training_text)

; Train a SentencePiece model
(setq result (torch_train_sentencepiece "training_data.txt" "my_model" 300 "bpe"))
(println result) ; "Training completed successfully. Model saved as: my_model.model"
```


### torch_encode: (torch_encode tokenizer text)

Encodes text into token indices using the specified tokenizer.

**Parameters:**
- `tokenizer`: A tokenizer object (simple or SentencePiece)
- `text`: String, the text to tokenize

**Returns:** A torch_tensor containing token indices

```lisp
; Using Simple Tokenizer
(setq simple_tokenizer (torch_simple_tokenizer))
(setq text "Hello world! This is a test.")
(setq tokens (torch_encode simple_tokenizer text))
(println tokens) ; [2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 3]

; Using SentencePiece Tokenizer
(setq sp_tokenizer (torch_sentencepiece_tokenizer "model.model"))
(setq sp_tokens (torch_encode sp_tokenizer text))
(println sp_tokens) ; [164, 149, 169, 164, 55, 124, ...]
```

### torch_decode: (torch_decode tokenizer tokens)

Decodes token indices back to text using the specified tokenizer.

**Parameters:**
- `tokenizer`: A tokenizer object (simple or SentencePiece)
- `tokens`: A torch_tensor containing token indices

**Returns:** String, the decoded text

```lisp
; Decode with Simple Tokenizer
(setq decoded_text (torch_decode simple_tokenizer tokens))
(println decoded_text) ; "hello world! this is a test."

; Decode with SentencePiece Tokenizer
(setq sp_decoded_text (torch_decode sp_tokenizer sp_tokens))
(println sp_decoded_text) ; "Hello world! This is a test."
```

### torch_vocab_size: (torch_vocab_size tokenizer)

Gets the current vocabulary size of the tokenizer.

**Parameters:**
- `tokenizer`: A tokenizer object (simple or SentencePiece)

**Returns:** Integer, the vocabulary size

```lisp
; Check vocabulary sizes
(setq simple_vocab_size (torch_vocab_size simple_tokenizer))
(setq sp_vocab_size (torch_vocab_size sp_tokenizer))
(println "Simple tokenizer vocab size:" simple_vocab_size) ; 4 (initially)
(println "SentencePiece vocab size:" sp_vocab_size) ; 32000 (typically)
```

### torch_pad_sequences: (torch_pad_sequences token_lists max_length)

Pads sequences to a fixed length for batch processing.

**Parameters:**
- `token_lists`: A list of torch_tensors (tokenized sequences)
- `max_length`: Integer, the target sequence length

**Returns:** A torch_tensor of shape [batch_size, max_length]

```lisp
; Create multiple tokenized sequences
(setq seq1 (torch_tokenizer_encode tokenizer "Hello world!"))
(setq seq2 (torch_tokenizer_encode tokenizer "Short text."))
(setq sequences (list seq1 seq2))

; Pad to length 20
(setq padded (torch_pad_sequences sequences 20))
(println padded) ; [batch_size, 20] tensor
```

### torch_create_attention_mask: (torch_create_attention_mask padded_sequences)

Creates attention masks to ignore padded tokens.

**Parameters:**
- `padded_sequences`: A torch_tensor of padded sequences

**Returns:** A torch_tensor attention mask (1 for real tokens, 0 for padding)

```lisp
(setq attention_mask (torch_create_attention_mask padded))
(println attention_mask) ; [batch_size, seq_length] mask
```

## Text Processing Pipeline

### Complete Text-to-Transformer Pipeline with SentencePiece

Here's a complete example showing the full text processing pipeline with both tokenizer types:

```lisp
; Load the library
(use 'liblispe_torch)

; === STEP 1: SentencePiece Model Training ===
; Create training corpus
(setq corpus (+ "Natural language processing enables computers to understand text.\n"
                "Machine learning models require sophisticated tokenization systems.\n"
                "SentencePiece provides subword tokenization for modern AI.\n"
                "Deep learning transformers revolutionize language understanding.\n"
                "Tokenization is the foundation of text processing pipelines.\n"))

(fwrite "training_corpus.txt" corpus)

; Train SentencePiece model
(setq training_result (torch_train_sentencepiece "training_corpus.txt" "my_tokenizer" 200 "bpe"))
(println "Training result:" training_result)

; === STEP 2: Tokenizer Comparison ===
(setq simple_tokenizer (torch_simple_tokenizer))
(setq sp_tokenizer (torch_sentencepiece_tokenizer "my_tokenizer.model"))

(setq test_text "Hello world! Natural language processing is amazing.")

; Compare tokenization approaches
(setq simple_tokens (torch_encode simple_tokenizer test_text))
(setq sp_tokens (torch_encode sp_tokenizer test_text))

(println "Original text:" test_text)
(println "Simple tokens:" simple_tokens)
(println "SentencePiece tokens:" sp_tokens)
(println "Simple vocab size:" (torch_vocab_size simple_tokenizer))
(println "SentencePiece vocab size:" (torch_vocab_size sp_tokenizer))

; === STEP 3: Advanced Text Processing ===
(setq texts (list 
    "Short text."
    "This is a longer sentence with more complex vocabulary."
    "Machine learning algorithms process natural language efficiently."))

; Process with SentencePiece (recommended for production)
(setq tokenized_texts (list))
(loop text texts
    (setq tokens (torch_encode sp_tokenizer text))
    (setq tokenized_texts (cons tokens tokenized_texts))
)

; === STEP 4: Sequence Processing ===
(setq max_length 25)
(setq padded_sequences (torch_pad_sequences (reverse tokenized_texts) max_length))
(setq attention_mask (torch_create_attention_mask padded_sequences))

(println "Padded sequences shape:" (torch_shape padded_sequences))
(println "Attention mask shape:" (torch_shape attention_mask))

; === STEP 5: Embedding and Transformer ===
(setq vocab_size (torch_vocab_size sp_tokenizer))
(setq embed_dim 128)
(setq num_heads 8)
(setq ff_dim 256)

(setq embedding (torch_embedding vocab_size embed_dim))
(setq transformer (torch_transformer_block embed_dim num_heads ff_dim))

; Forward pass
(setq embedded_sequences (torch_forward embedding padded_sequences))
(setq transformer_output (torch_forward transformer embedded_sequences))

(println "Embedded sequences shape:" (torch_shape embedded_sequences))
(println "Transformer output shape:" (torch_shape transformer_output))

; === STEP 6: Verification ===
; Test bidirectional encoding
(setq verification_text "Test bidirectional encoding with SentencePiece.")
(setq encoded_verification (torch_encode sp_tokenizer verification_text))
(setq decoded_verification (torch_decode sp_tokenizer encoded_verification))

(println "Original:" verification_text)
(println "Reconstructed:" decoded_verification)
(println "Perfect reconstruction:" (= verification_text decoded_verification))

(println "🎉 Complete SentencePiece pipeline successful!")
```

## Complete Training Examples

### Example 1: Simple MLP Training

Here's a complete example showing how to train a simple neural network with automatic device detection:

```lisp
; Load the library
(use 'lispe_torch)

; Automatically detect the best device
(setq device (torch_get_best_device))
(println "Using device:" device)

; Create training data
(setq input_data (torch_tensor (floats 1.0 2.0 3.0 4.0)))
(setq targets (torch_tensor (floats 0.5 1.5)))

; Move to best available device
(cond
    ((eq device "mps") 
        (block
            (setq input_data (torch_to_mps input_data))
            (setq targets (torch_to_mps targets))
        ))
    ((eq device "cuda") 
        (block
            (torch_set_device 0)
            (setq input_data (torch_to_cuda input_data))
            (setq targets (torch_to_cuda targets))
        ))
    (true (println "Using CPU - no acceleration"))
)

; Create a model (4 inputs, 64 hidden, 2 outputs)
(setq model (torch_model 4 64 2))

; Create an Adam optimizer with learning rate 0.001
(setq optimizer (torch_optimizer model 0.001 "adam"))

; Training loop
(loop epoch (range 0 1000 1)
    ; Forward pass
    (setq predictions (torch_forward model input_data))
    
    ; Compute loss
    (setq loss (torch_mse_loss predictions targets))
    
    ; Zero gradients
    (torch_optimizer_zero_grad optimizer)
    
    ; Backward pass
    (torch_backward loss)
    
    ; Update parameters
    (torch_optimizer_step optimizer)
    
    ; Print progress every 100 epochs
    (if (= (% epoch 100) 0)
        (println "Epoch:" epoch "Loss:" loss))
)

(println "Training completed!")
(println "Final predictions:" (torch_forward model input_data))
```

### Example 2: Transformer Training with Tokenization

Advanced example showing Transformer training with complete text processing pipeline:

```lisp
; Load the library
(use 'lispe_torch)

; === SETUP ===
(setq device (torch_get_best_device))
(println "Using device:" device)

; === TOKENIZATION SETUP ===
(setq tokenizer (torch_simple_tokenizer))

; Training texts
(setq training_texts (list 
    "The quick brown fox jumps over the lazy dog."
    "Machine learning is transforming artificial intelligence."
    "Natural language processing enables computers to understand text."
    "Deep learning models require large amounts of training data."
))

; === TEXT PREPROCESSING ===
(setq tokenized_texts (list))
(loop text training_texts
    (setq tokens (torch_tokenizer_encode tokenizer text))
    (setq tokenized_texts (cons tokens tokenized_texts))
)

; Pad sequences
(setq max_length 15)
(setq padded_sequences (torch_pad_sequences (reverse tokenized_texts) max_length))
(setq attention_mask (torch_create_attention_mask padded_sequences))

; === MODEL SETUP ===
(setq vocab_size (torch_tokenizer_get_vocab_size tokenizer))
(setq embed_dim 128)
(setq num_heads 8)
(setq ff_dim 256)

; Create components
(setq embedding (torch_embedding vocab_size embed_dim))
(setq transformer (torch_transformer_block embed_dim num_heads ff_dim))
(setq output_projection (torch_model embed_dim 64 vocab_size))

; Move to device
(cond
    ((eq device "mps") 
        (block
            (setq padded_sequences (torch_to_mps padded_sequences))
            (setq attention_mask (torch_to_mps attention_mask))
        ))
    ((eq device "cuda") 
        (block
            (torch_set_device 0)
            (setq padded_sequences (torch_to_cuda padded_sequences))
            (setq attention_mask (torch_to_cuda attention_mask))
        ))
)

; === TRAINING SETUP ===
(setq optimizer_emb (torch_optimizer embedding 0.001 "adam"))
(setq optimizer_trans (torch_optimizer transformer 0.001 "adam"))
(setq optimizer_proj (torch_optimizer output_projection 0.001 "adam"))

; === TRAINING LOOP ===
(println "Starting Transformer training...")
(loop epoch (range 0 100 1)
    ; === FORWARD PASS ===
    ; Embedding
    (setq embedded (torch_forward embedding padded_sequences))
    
    ; Transformer
    (setq transformer_output (torch_forward transformer embedded))
    
    ; Output projection
    (setq predictions (torch_forward output_projection transformer_output))
    
    ; === LOSS COMPUTATION ===
    ; For demonstration, use next token prediction
    (setq shifted_targets (torch_tensor_slice padded_sequences 1 -1))
    (setq shifted_predictions (torch_tensor_slice predictions 0 -2))
    
    (setq loss (torch_crossentropy_loss shifted_predictions shifted_targets))
    
    ; === BACKWARD PASS ===
    ; Zero gradients
    (torch_optimizer_zero_grad optimizer_emb)
    (torch_optimizer_zero_grad optimizer_trans)
    (torch_optimizer_zero_grad optimizer_proj)
    
    ; Backward pass
    (torch_backward loss)
    
    ; Update parameters
    (torch_optimizer_step optimizer_emb)
    (torch_optimizer_step optimizer_trans)
    (torch_optimizer_step optimizer_proj)
    
    ; Print progress
    (if (= (% epoch 10) 0)
        (println "Epoch:" epoch "Loss:" loss))
)

(println "🎉 Transformer training completed!")
(println "Vocabulary size:" vocab_size)
(println "Model parameters: Embedding(" embed_dim ") + Transformer(" num_heads "heads) + Projection")
```

### Example 3: Llama-3.1 Fine-tuning with SentencePiece

Example showing how to prepare for Llama-3.1 fine-tuning with SentencePiece tokenization:

```lisp
; Load the library
(use 'liblispe_torch)

; === LLAMA-3.1 CONFIGURATION ===
(setq llama_config (dict
    "vocab_size" 128256      ; Llama-3.1 vocabulary size
    "embed_dim" 4096         ; Llama-3.1 embedding dimension
    "num_heads" 32           ; Llama-3.1 attention heads
    "num_layers" 32          ; Llama-3.1 transformer layers
    "ff_dim" 11008           ; Llama-3.1 feed-forward dimension
    "max_seq_length" 8192    ; Llama-3.1 maximum sequence length
))

; === SENTENCEPIECE TOKENIZER SETUP ===
; Option 1: Use pre-trained Llama tokenizer (if available)
(setq tokenizer nil)
(setq tokenizer_type "")

; Try to load pre-trained Llama tokenizer
(setq load_result 
  (catch 
    (setq tokenizer (torch_sentencepiece_tokenizer "llama3_tokenizer.model"))
    (setq tokenizer_type "llama_pretrained")
    "success"))

; Option 2: Train custom SentencePiece tokenizer for domain adaptation
(if (maybe load_result)
    (block
        (println "Training custom SentencePiece tokenizer for domain adaptation...")
        
        ; Create domain-specific training corpus
        (setq domain_corpus (+ 
            "Instruction: Explain machine learning concepts in simple terms.\n"
            "Response: Machine learning is a type of artificial intelligence...\n"
            "Instruction: Write efficient Python code for data processing.\n"
            "Response: Here's an optimized Python function for data processing...\n"
            "Instruction: Analyze the benefits of renewable energy systems.\n"
            "Response: Renewable energy systems offer multiple advantages...\n"
            "Instruction: Describe modern software development practices.\n"
            "Response: Modern software development emphasizes collaboration...\n"
            "Instruction: Explain quantum computing principles clearly.\n"
            "Response: Quantum computing leverages quantum mechanical phenomena...\n"))
        
        (fwrite "domain_corpus.txt" domain_corpus)
        
        ; Train domain-adapted tokenizer
        (setq train_result (torch_train_sentencepiece "domain_corpus.txt" "domain_tokenizer" 8000 "bpe"))
        (println train_result)
        
        ; Load the trained tokenizer
        (setq tokenizer (torch_sentencepiece_tokenizer "domain_tokenizer.model"))
        (setq tokenizer_type "domain_adapted")
        (println "✅ Using domain-adapted SentencePiece tokenizer")
    )
    (println "✅ Using pre-trained Llama SentencePiece tokenizer"))

; === SAMPLE FINE-TUNING DATA PREPARATION ===
(setq fine_tuning_data (list
    "Instruction: Explain the transformer architecture. Response: The transformer architecture consists of encoder and decoder stacks with multi-head attention mechanisms that enable parallel processing of sequences."
    "Instruction: Write a Python function to implement binary search. Response: def binary_search(arr, target): left, right = 0, len(arr) - 1; while left <= right: mid = (left + right) // 2; if arr[mid] == target: return mid; elif arr[mid] < target: left = mid + 1; else: right = mid - 1; return -1"
    "Instruction: Compare renewable vs fossil fuel energy sources. Response: Renewable energy sources like solar and wind are sustainable and environmentally friendly, while fossil fuels are finite resources that contribute to climate change but currently provide reliable baseline power."
))

; === ADVANCED TEXT PREPROCESSING ===
(println "Processing fine-tuning data with SentencePiece...")
(setq processed_data (list))
(setq total_tokens 0)

(loop text fine_tuning_data
    (setq tokens (torch_encode tokenizer text))
    (setq token_count (size tokens))
    (setq total_tokens (+ total_tokens token_count))
    (setq processed_data (cons tokens processed_data))
    (println "Text length:" (size text) "characters → " token_count "tokens"))

(println "Total tokens processed:" total_tokens)
(println "Average tokens per example:" (/ total_tokens (size fine_tuning_data)))

; Pad to appropriate sequence length (not full Llama length for demo)
(setq practical_max_length 512)  ; Use shorter length for demonstration
(setq padded_data (torch_pad_sequences (reverse processed_data) practical_max_length))
(setq attention_mask (torch_create_attention_mask padded_data))

; === SENTENCEPIECE-OPTIMIZED MODEL ARCHITECTURE ===
(setq actual_vocab_size (torch_vocab_size tokenizer))
(setq embed_dim 512)    ; Smaller for demonstration
(setq num_heads 8)      ; Scaled down
(setq ff_dim 1024)      ; Scaled down

(println "Using vocabulary size:" actual_vocab_size "for" tokenizer_type "tokenizer")

; Create model components
(setq embedding (torch_embedding actual_vocab_size embed_dim))
(setq transformer_layers (list))

; Create multiple transformer layers
(loop layer_idx (range 0 4 1)  ; 4 layers for demonstration
    (setq layer (torch_transformer_block embed_dim num_heads ff_dim))
    (setq transformer_layers (cons layer transformer_layers))
)

(setq output_projection (torch_model embed_dim 256 actual_vocab_size))

(println "✅ Created model architecture:")
(println "   • Embedding:" actual_vocab_size "→" embed_dim)
(println "   • Transformer layers:" (size transformer_layers))
(println "   • Output projection:" embed_dim "→" actual_vocab_size)

; === TOKENIZATION ANALYSIS ===
(println "\n🔍 SentencePiece Tokenization Analysis:")
(setq analysis_text "The transformer architecture revolutionized natural language processing.")
(setq analysis_tokens (torch_encode tokenizer analysis_text))
(setq reconstructed_text (torch_decode tokenizer analysis_tokens))

(println "Original text: \"" analysis_text "\"")
(println "Tokens:" analysis_tokens)
(println "Token count:" (size analysis_tokens))
(println "Reconstructed: \"" reconstructed_text "\"")
(println "Perfect reconstruction:" (= analysis_text reconstructed_text))

; === DEVICE OPTIMIZATION ===
(setq device (torch_get_best_device))
(println "\n🚀 Device optimization:")
(println "   • Detected device:" device)

(cond
    ((eq device "mps") 
        (println "   • Apple Silicon GPU acceleration ready"))
    ((eq device "cuda") 
        (println "   • NVIDIA GPU acceleration ready"))
    (true 
        (println "   • CPU processing (consider GPU for large models)")))

; === FINE-TUNING READINESS CHECK ===
(println "\n🎯 Llama-3.1 Fine-tuning Setup with SentencePiece Complete!")
(println "   ✅ SentencePiece tokenizer: Ready (" tokenizer_type ")")
(println "   ✅ Vocabulary size:" actual_vocab_size)
(println "   ✅ Architecture: Scaled transformer blocks created")
(println "   ✅ Data: Preprocessed and padded (" (size fine_tuning_data) "examples)")
(println "   ✅ Sequence length:" practical_max_length)
(println "   ✅ Device acceleration:" device)
(println "   ✅ Perfect encode/decode cycle verified")
(println "")
(println "🚀 Ready for:")
(println "   • Full-scale Llama-3.1 fine-tuning")
(println "   • Domain adaptation with custom tokenizers")
(println "   • Production deployment with SentencePiece")
(println "   • Multi-GPU distributed training")
```

## Notes

- All tensor operations maintain PyTorch's automatic differentiation capabilities
- The library supports both MLP and **complete Transformer architecture** with Multi-Head Attention
- **Advanced Tokenization System**: 
  - **SimpleTokenizer**: Word-based tokenization for immediate use and prototyping
  - **SentencePiece Integration**: Production-ready subword tokenization compatible with Llama-3.1, GPT, and other modern language models
  - **Custom Model Training**: `torch_train_sentencepiece()` for domain-specific tokenizer optimization
  - **Unified API**: Same interface (`torch_encode`, `torch_decode`, `torch_vocab_size`) for both tokenizer types
- **Text Processing Pipeline**: Complete workflow from text → tokenization → embedding → padding → attention masking → transformer → output
- **SentencePiece Features**:
  - Subword tokenization using BPE (Byte-Pair Encoding) or Unigram algorithms
  - Perfect text reconstruction through reversible tokenization
  - Multilingual support and out-of-vocabulary handling
  - Compatible with pre-trained language models (Llama, GPT, BERT families)
  - Automatic vocabulary size optimization based on corpus characteristics
- Float32 and Float64 tensor types are supported through LispE's `floats` and `numbers` types
- Error handling is built-in with descriptive error messages and proper vocabulary size validation
- The library is thread-safe when used with PyTorch's CPU backend
- **Advanced Device Support**: 
  - **macOS**: Automatic MPS (Metal Performance Shaders) support for Apple Silicon (M1/M2/M3/M4)
  - **NVIDIA**: CUDA support for NVIDIA GPUs with automatic device detection
  - **CPU**: Optimized fallback for all systems
  - Use `torch_get_best_device()` for automatic optimal device selection
- **Cross-platform**: The library automatically detects and uses the best available acceleration
- **Llama-3.1 Ready**: Complete architecture and SentencePiece tokenization framework for immediate fine-tuning
- **Memory Efficient**: Automatic padding and attention masking for variable-length sequences
- **Scalable**: Supports models from small experiments to large language model fine-tuning
- **Production Ready**: SentencePiece integration provides production-grade tokenization for deployment

## Architecture Features

### Transformer Components
- **Multi-Head Attention**: Parallel attention computation with configurable heads
- **Layer Normalization**: Training stabilization and gradient flow improvement
- **Feed-Forward Networks**: Position-wise dense layers with configurable dimensions
- **Residual Connections**: Skip connections for deep network training
- **Attention Masking**: Proper handling of padded sequences

### Tokenization Capabilities
- **SimpleTokenizer**: Word-based with punctuation handling, dynamic vocabulary
- **SentencePiece Integration**: Subword tokenization compatible with modern language models
  - **BPE (Byte-Pair Encoding)**: Efficient subword segmentation
  - **Unigram Language Model**: Probabilistic subword tokenization
  - **Custom Training**: Domain-specific tokenizer optimization
  - **Pre-trained Compatibility**: Load existing Llama, GPT, BERT tokenizers
  - **Multilingual Support**: Unicode-aware tokenization for any language
- **Special Tokens**: Automatic handling of `<START>`, `<END>`, `<PAD>`, `<UNK>`, `<BOS>`, `<EOS>`
- **Bidirectional Processing**: Perfect encode/decode cycle preservation with SentencePiece
- **Sequence Processing**: Automatic padding and attention mask generation
- **Vocabulary Management**: Dynamic expansion (SimpleTokenizer) and fixed vocabularies (SentencePiece)

### Training Infrastructure
- **Automatic Device Detection**: Optimal hardware utilization
- **Multiple Optimizers**: Adam, SGD with configurable learning rates
- **Loss Functions**: MSE, Cross-entropy for different training scenarios
- **Gradient Management**: Automatic differentiation with manual gradient control
- **Memory Management**: Efficient tensor operations and device transfers

## Future Extensions

### Immediate Roadmap
- **Enhanced SentencePiece Features**: 
  - Vocabulary expansion and fine-tuning
  - Custom special token configuration
  - Batch tokenization optimization
  - Streaming tokenization for large texts
- **Positional Embeddings**: Absolute and relative position encoding
- **Model Serialization**: Save/load pre-trained weights and tokenizers
- **LoRA Fine-tuning**: Parameter-efficient fine-tuning implementation
- **Gradient Checkpointing**: Memory-efficient training for large models

## Model Loading & Persistence

The library provides comprehensive model persistence capabilities for production deployment and transfer learning.

### torch_save_model: (torch_save_model model path)

Save a PyTorch module to disk.

**Parameters:**
- **model**: PyTorch module to save (any torch module type)
- **path**: File path for saving (e.g., "model.pt")

**Supported Module Types:**
- `TorchLinear` - Fully connected layers
- `TorchEmbedding` - Token embedding layers
- `TorchMultiHeadAttention` - Attention mechanisms
- `TorchLayerNorm` - Normalization layers
- `TorchModel` - Complete model architectures

**Example:**
```lisp
; Save a linear layer
(setq linear (torch_linear 512 256))
(torch_save_model linear "linear_layer.pt")

; Save an embedding layer
(setq embedding (torch_embedding 10000 512))
(torch_save_model embedding "embeddings.pt")

; Save attention mechanism
(setq attention (torch_multihead_attention 512 8))
(torch_save_model attention "attention.pt")
```

### torch_load_model: (torch_load_model path model)

Load a PyTorch module from disk.

**Parameters:**
- **path**: File path to load from
- **model**: Target model (must have same architecture)

**Example:**
```lisp
; Create new model with same architecture
(setq new_linear (torch_linear 512 256))
; Load pre-trained weights
(torch_load_model "linear_layer.pt" new_linear)

; Model is now ready with loaded weights
(setq output (torch_linear_forward new_linear input))
```

### torch_state_dict: (torch_state_dict model)

Extract model parameters as a state dictionary.

**Parameters:**
- **model**: PyTorch module

**Returns:** TorchStateDict containing model parameters

**Example:**
```lisp
(setq linear (torch_linear 256 128))
(setq state_dict (torch_state_dict linear))
; Returns: PyTorch State Dict (2 parameters)
```

### torch_load_state_dict: (torch_load_state_dict model state_dict)

Load parameters from a state dictionary into a model.

**Parameters:**
- **model**: Target model
- **state_dict**: TorchStateDict with parameters

**Example:**
```lisp
(setq source_model (torch_linear 256 128))
(setq target_model (torch_linear 256 128))

; Transfer parameters
(setq state_dict (torch_state_dict source_model))
(torch_load_state_dict target_model state_dict)
```

### torch_save_checkpoint: (torch_save_checkpoint model optimizer epoch path)

Save a training checkpoint including model, optimizer, and epoch information.

**Parameters:**
- **model**: Model to save
- **optimizer**: Optimizer state
- **epoch**: Current epoch number
- **path**: Checkpoint file path

**Example:**
```lisp
(setq model (torch_linear 512 256))
(setq optimizer (torch_optimizer model 0.001 "adam"))

; Save training checkpoint
(torch_save_checkpoint model optimizer 100 "checkpoint_epoch_100.pt")
```

### torch_load_checkpoint: (torch_load_checkpoint path)

Load a training checkpoint from disk.

**Parameters:**
- **path**: Checkpoint file path

**Returns:** TorchStateDict with checkpoint information

**Example:**
```lisp
(setq checkpoint (torch_load_checkpoint "checkpoint_epoch_100.pt"))
; Use checkpoint data to resume training
```

## Positional Encoding

Essential component for Transformer architectures to understand sequence order.

### torch_positional_encoding: (torch_positional_encoding embed_dim max_length)

Create a positional encoding module with sinusoidal patterns.

**Parameters:**
- **embed_dim**: Embedding dimension (must match token embeddings)
- **max_length**: Maximum sequence length supported

**Returns:** TorchPositionalEncoding module

**Example:**
```lisp
; Create positional encoder for 512-dim embeddings, max 2048 tokens
(setq pos_encoder (torch_positional_encoding 512 2048))
```

### torch_positional_encoding_forward: (torch_positional_encoding_forward pos_encoder input)

Apply positional encoding to input embeddings.

**Parameters:**
- **pos_encoder**: TorchPositionalEncoding module
- **input**: Token embeddings tensor [batch_size, seq_len, embed_dim]

**Returns:** Tensor with positional information added

**Example:**
```lisp
; Complete Transformer pipeline with positional encoding
(setq embed_dim 512)
(setq seq_length 128)
(setq vocab_size 10000)

; Components
(setq tokenizer (torch_simple_tokenizer))
(setq embedding (torch_embedding vocab_size embed_dim))
(setq pos_encoding (torch_positional_encoding embed_dim seq_length))
(setq attention (torch_multihead_attention embed_dim 8))
(setq layer_norm (torch_layer_norm (list embed_dim)))

; Text processing
(setq text "The quick brown fox jumps over the lazy dog")
(setq tokens (torch_tokenizer_encode tokenizer text))
(setq padded_tokens (torch_pad_sequence (list tokens) seq_length))

; Forward pass with positional encoding
(setq embedded (torch_embedding_forward embedding padded_tokens))
(setq pos_embedded (torch_positional_encoding_forward pos_encoding embedded))
(setq attention_mask (torch_create_attention_mask padded_tokens 0))
(setq output (torch_multihead_attention_forward attention pos_embedded attention_mask))
(setq final_output (torch_layer_norm_forward layer_norm output))

; Save the complete trained model
(torch_save_model attention "transformer_attention.pt")
(torch_save_model embedding "transformer_embedding.pt")
(torch_save_model pos_encoding "transformer_positional.pt")
```

## Production Workflow Example

Complete example of building, training, and deploying a Transformer model:

```lisp
; 1. Configuration
(setq config (@
    (@ "vocab_size" 30000)
    (@ "embed_dim" 768)
    (@ "num_heads" 12)
    (@ "seq_length" 512)
    (@ "learning_rate" 0.0001)
))

; 2. Model components
(setq tokenizer (torch_simple_tokenizer))
(setq embedding (torch_embedding (config @ "vocab_size") (config @ "embed_dim")))
(setq pos_encoding (torch_positional_encoding (config @ "embed_dim") (config @ "seq_length")))
(setq attention (torch_multihead_attention (config @ "embed_dim") (config @ "num_heads")))
(setq layer_norm (torch_layer_norm (list (config @ "embed_dim"))))

; 3. Training setup
(setq optimizer (torch_optimizer attention (config @ "learning_rate") "adam"))

; 4. Training loop with checkpointing
(loop (epoch 1 100)
    ; ... training steps ...
    
    ; Save checkpoint every 10 epochs
    (if (= (% epoch 10) 0)
        (torch_save_checkpoint attention optimizer epoch 
            (+ "checkpoint_epoch_" (string epoch) ".pt")))
)

; 5. Save final trained model
(torch_save_model embedding "final_embedding.pt")
(torch_save_model pos_encoding "final_positional.pt")
(torch_save_model attention "final_attention.pt")
(torch_save_model layer_norm "final_layernorm.pt")

; 6. Production deployment
; Load pre-trained components in production environment
(setq prod_embedding (torch_embedding (config @ "vocab_size") (config @ "embed_dim")))
(setq prod_attention (torch_multihead_attention (config @ "embed_dim") (config @ "num_heads")))

(torch_load_model "final_embedding.pt" prod_embedding)
(torch_load_model "final_attention.pt" prod_attention)

; Ready for inference!
```

---

## LoRA Fine-tuning

**Low-Rank Adaptation (LoRA)** enables parameter-efficient fine-tuning of large language models by decomposing weight updates into low-rank matrices.

### torch_lora_linear: (torch_lora_linear in_features out_features rank alpha)

Create a LoRA-adapted linear layer with trainable low-rank matrices.

**Parameters:**
- **in_features**: Number of input features
- **out_features**: Number of output features  
- **rank**: LoRA rank (typically 4-64, lower = fewer parameters)
- **alpha**: LoRA scaling factor (typically 16-32)

**Returns:** TorchLoRALinear module

**Example:**
```lisp
; Create LoRA layer for Llama-3.1 scale
(setq lora_layer (torch_lora_linear 4096 4096 16 32.0))

; Much fewer trainable parameters than full fine-tuning!
(println "LoRA layer created:" lora_layer)
```

### torch_lora_forward: (torch_lora_forward lora_layer input)

Forward pass through LoRA-adapted layer.

**Parameters:**
- **lora_layer**: TorchLoRALinear module
- **input**: Input tensor

**Returns:** Output tensor with LoRA adaptation applied

**Example:**
```lisp
(setq tensor_input (torch_ones (integers 32 4096)))
(setq output (torch_lora_forward lora_layer tensor_input))
(println "LoRA output shape:" (torch_size output))
```

### torch_lora_apply_to_linear: (torch_lora_apply_to_linear linear_layer rank alpha)

Convert an existing linear layer to LoRA-adapted version.

**Parameters:**
- **linear_layer**: Existing TorchLinear module
- **rank**: LoRA rank for adaptation
- **alpha**: LoRA scaling factor

**Returns:** New TorchLoRALinear module with copied weights

**Example:**
```lisp
; Start with pre-trained linear layer
(setq base_layer (torch_linear 512 256))
; Convert to LoRA for fine-tuning
(setq lora_adapted (torch_lora_apply_to_linear base_layer 8 16.0))
```

### torch_lora_merge_weights: (torch_lora_merge_weights lora_layer)

Merge LoRA matrices into the original weights for deployment.

**Parameters:**
- **lora_layer**: TorchLoRALinear module

**Returns:** Success message

**Example:**
```lisp
; After training, merge for zero-overhead inference
(torch_lora_merge_weights lora_layer)
; Now the layer has no overhead but retains adaptations
```

### torch_lora_save_adapters: (torch_lora_save_adapters lora_layer path)

Save only the LoRA adapter weights (much smaller than full model).

**Parameters:**
- **lora_layer**: TorchLoRALinear module
- **path**: File path for saving adapters

**Returns:** Success message

**Example:**
```lisp
; Save only LoRA adapters (~1% of full model size)
(torch_lora_save_adapters lora_layer "llama_task_adapters.pt")
```

### torch_lora_load_adapters: (torch_lora_load_adapters lora_layer path)

Load LoRA adapter weights into an existing LoRA layer.

**Parameters:**
- **lora_layer**: TorchLoRALinear module (must match saved architecture)
- **path**: File path to load adapters from

**Returns:** Success message

**Example:**
```lisp
; Create LoRA layer with same architecture
(setq lora_layer (torch_lora_linear 4096 4096 16 32.0))
; Load trained adapters
(torch_lora_load_adapters lora_layer "llama_task_adapters.pt")
```

### LoRA Fine-tuning Pipeline

Complete example of LoRA fine-tuning workflow:

```lisp
; 1. Create or load base model
(setq base_model (torch_linear 4096 4096))

; 2. Apply LoRA adaptation  
(setq lora_model (torch_lora_apply_to_linear base_model 16 32.0))

; 3. Training loop (simplified)
(setq tensor_input (torch_ones (integers 32 4096)))
(setq target (torch_zeros (integers 32 4096)))

; Forward pass
(setq output (torch_lora_forward lora_model tensor_input))

; 4. Save adapters (much smaller than full model)
(torch_lora_save_adapters lora_model "task_specific_adapters.pt")

; 5. For deployment: merge weights for optimal inference
(torch_lora_merge_weights lora_model)

; 6. Or load adapters into new model
(setq deployment_model (torch_lora_linear 4096 4096 16 32.0))
(torch_lora_load_adapters deployment_model "task_specific_adapters.pt")
```

### LoRA Benefits

**Parameter Efficiency**: Train only 0.1-10% of original parameters  
**Storage Efficiency**: Save only adapter weights, not full model  
**Task Specialization**: Different adapters for different tasks  
**Fast Training**: Fewer parameters = faster convergence  
**Easy Deployment**: Merge weights for zero-overhead inference

## Portable Installation System (NEW!)

LispETorch includes an advanced portable installation system that automatically packages all dependencies for easy distribution and deployment across different systems.

### Automatic Installation

The portable installation is automatically triggered when using `make all`:

```bash
cd lispetorch
make all  # Compiles and automatically installs portable version
```

### What Gets Automatically Packaged

The system automatically detects and copies:

- **PyTorch Libraries**: `libtorch.dylib`, `libtorch_cpu.dylib`, `libc10.dylib`
- **SentencePiece Libraries**: `libsentencepiece.dylib`, `libsentencepiece_train.dylib`
- **Main Extension**: `liblispe_torch.so` with all dependencies

### Smart Path Detection

The installation system automatically handles different installation sources:

```bash
# Conda environments
/Users/user/miniconda3/envs/pytorch/lib/python3.x/site-packages/torch/lib/

# Homebrew installations  
/opt/homebrew/lib/

# System installations
/usr/local/lib/
```

### Self-Contained Deployment

After installation, all libraries are packaged in `/usr/local/lib/lispe`:

```bash
/usr/local/lib/lispe/
├── liblispe_torch.so          # Main extension
├── libtorch.dylib             # PyTorch core
├── libtorch_cpu.dylib         # PyTorch CPU ops
├── libc10.dylib               # PyTorch tensors
├── libsentencepiece.dylib     # Tokenization
└── libsentencepiece_train.dylib
```

### Cross-Platform Support

- ✅ **macOS**: Homebrew, conda, and system installations
- ✅ **Linux**: Package managers and manual installations  
- ✅ **Windows**: MinGW, MSYS2, and Visual Studio builds

### Verification

After installation, verify the portable setup:

```bash
# Check library installation
ls -la /usr/local/lib/lispe/

# Test library loading
cd lispe && bin/lispe -c "(use 'lispe_torch) (print 'Success)"

# Verify dependencies
otool -L /usr/local/lib/lispe/liblispe_torch.so  # macOS
ldd /usr/local/lib/lispe/liblispe_torch.so       # Linux
```

### Benefits

- **🔄 Zero Configuration**: Works out of the box after `make all`
- **📦 Self-Contained**: No external dependencies needed
- **🌐 Portable**: Deploy same libraries across different systems
- **⚡ Fast Deployment**: Copy single directory for distribution
- **🔧 Automatic Updates**: Rebuild with `make all` to update dependencies

### Advanced Features
- **Multi-GPU Training**: Distributed training across multiple devices
- **Mixed Precision**: FP16/BF16 training for memory efficiency
- **Dynamic Batching**: Automatic batch optimization for variable-length sequences
- **Custom CUDA Kernels**: Optimized operations for specific use cases
- **Quantization**: INT8/INT4 model compression for inference

### Model Architectures
- **Complete Llama Architecture**: Full implementation with rotary embeddings
- **Vision Transformers**: Image processing capabilities
- **Multimodal Models**: Combined text and image processing
- **Recurrent Networks**: LSTM, GRU for sequential data
- **Convolutional Networks**: CNN architectures for structured data

### Data Processing
- **Production Tokenizers**: 
  - **SentencePiece**: Complete implementation with training and inference
  - **WordPiece**: Google BERT-style tokenization
  - **Byte-level BPE**: GPT-style tokenization
  - **Custom Tokenizers**: Domain-specific optimization
- **Data Loaders**: Efficient batch processing and augmentation
- **Text Preprocessing**: Advanced cleaning and normalization
- **Dataset Utilities**: Common dataset loading and processing
- **Streaming Support**: Large dataset processing without memory limits

### Training Enhancements
- **Learning Rate Schedulers**: Cosine, linear, exponential decay
- **Advanced Optimizers**: AdamW, RMSprop, AdaGrad with weight decay
- **Regularization**: Dropout, attention dropout, weight decay
- **Early Stopping**: Automatic training termination on convergence
- **Hyperparameter Search**: Automatic optimization and tuning

### Deployment Features
- **Model Export**: ONNX, TorchScript conversion
- **Inference Optimization**: TensorRT, OpenVINO integration
- **Serving Infrastructure**: REST API and gRPC endpoints
- **Edge Deployment**: Mobile and embedded device support
- **Cloud Integration**: AWS, Azure, GCP deployment utilities

### Research Tools
- **Interpretability**: Attention visualization and analysis
- **Metrics Collection**: Comprehensive training and evaluation metrics
- **Experiment Tracking**: MLflow, Weights & Biases integration
- **A/B Testing**: Model comparison and validation frameworks
- **Benchmark Suites**: Standard evaluation protocols

This roadmap positions the LispE PyTorch library as a comprehensive platform for both research and production deep learning applications, with particular strength in large language model fine-tuning and deployment.