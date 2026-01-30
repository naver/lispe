# LispE PyTorch Manual - Complete Guide

A comprehensive guide for using the LispE PyTorch library, providing complete integration between the LispE language and the PyTorch ecosystem for machine learning.

## Table of Contents

1. [Introduction](#introduction)
2. [Installation and Configuration](#installation-and-configuration)
3. [Data Types](#data-types)
4. [Basic Tensor Operations](#basic-tensor-operations)
5. [Models and Neural Networks](#models-and-neural-networks)
6. [Hugging Face Model Loading](#hugging-face-model-loading)
7. [LoRA Fine-tuning](#lora-fine-tuning)
8. [Text Generation](#text-generation)
9. [Tokenization](#tokenization)
10. [Flash Attention](#flash-attention)
11. [Optimization and Quantization](#optimization-and-quantization)
12. [Complete LoRA Training Tutorial](#complete-lora-training-tutorial)
13. [Practical Examples](#practical-examples)

## Introduction

LispE PyTorch is a library that integrates PyTorch capabilities into the LispE functional programming language. It enables creating, training, and deploying machine learning models with LispE's elegant syntax.

### Key Features

- **Native PyTorch Integration**: Direct access to PyTorch tensors and operations
- **GPU Support**: CUDA for NVIDIA and Metal Performance Shaders (MPS) for Apple Silicon
- **Hugging Face Models**: Loading and execution of pre-trained models
- **LoRA Fine-tuning**: Efficient fine-tuning with Low-Rank Adaptation
- **Flash Attention**: Memory-efficient attention for long sequences
- **Text Generation**: Complete system with multiple sampling strategies
- **Quantization**: FP16 and INT8 support for model optimization

## Installation and Configuration

### Loading the Library

```lisp
; Load the PyTorch library
(use 'lispe_torch)

; Check GPU availability
(if (torch_cuda_is_available)
    (println "✓ CUDA available")
    (if (torch_mps_available) 
        (println "✓ MPS (Apple Silicon) available")
        (println "• Using CPU")))
```

### Basic Configuration

```lisp
; Set default device
(setq device (cond 
    ((torch_cuda_is_available) "cuda")
    ((torch_mps_available) "mps")
    (true "cpu")))

(println "Selected device:" device)
```

## Data Types

### LispE Types Optimized for PyTorch

LispE provides optimized data types that integrate efficiently with PyTorch:

```lisp
; Optimized list types (zero-copy)
(setq integers_list (integers 1 2 3 4 5))      ; Integer list
(setq floats_list (floats 1.0 2.0 3.0 4.0))   ; Float list
(setq numbers_list (numbers 1.0 2.0 3.0))      ; Double list
(setq shorts_list (shorts 1 2 3))              ; Short list

; Sequence generators
(setq seq1 (iota0 5))    ; [0, 1, 2, 3, 4]
(setq seq2 (iota 5))     ; [1, 2, 3, 4, 5]
```

### PyTorch Types in LispE

```lisp
; Main types
; tensor_create    - Wraps torch::Tensor
; torch_model     - Wraps PyTorch models
; torch_optimizer - Wraps optimizers
; torch_generator - Advanced text generator
; torch_tokenizer - Tokenization interface
```

## Basic Tensor Operations

### Tensor Creation

```lisp
; Create from LispE lists
(setq data (floats 1.0 2.0 3.0 4.0))
(setq tensor (tensor_create data))

; Tensors with specific shapes
(setq zeros_tensor (tensor_zeros (integers 3 4)))
(setq ones_tensor (tensor_ones (integers 2 3)))
(setq random_tensor (tensor_randn (integers 4 4)))

; Tensors with specific device
(setq cuda_tensor (torch_to_cuda (tensor_randn (integers 2 2))))
(setq mps_tensor (torch_to_mps (tensor_randn (integers 2 2))))
```

### Mathematical Operations

```lisp
; Basic operations
(setq a (tensor_randn (integers 3 3)))
(setq b (tensor_randn (integers 3 3)))

(setq sum_result (tensor_add a b))
(setq product (tensor_matmul a b))
(setq transposed (tensor_transpose a 0 1))

; Activation functions
(setq relu_result (tensor_relu tensor))
(setq sigmoid_result (tensor_sigmoid tensor))
(setq softmax_result (tensor_softmax tensor -1))
```

### Shape Manipulation

```lisp
; Get dimensions
(setq shape (tensor_shape tensor))
(println "Tensor shape:" shape)

; Reshape
(setq reshaped (tensor_reshape tensor (integers 2 2)))
(setq squeezed (tensor_squeeze tensor))
(setq unsqueezed (tensor_unsqueeze tensor 0))
```

## Models and Neural Networks

### Simple Model Creation

```lisp
; Create individual PyTorch layers
(setq linear1 (torch_linear 784 128))    ; linear layer input->hidden
(setq linear2 (torch_linear 128 10))     ; linear layer hidden->output

; Create example data
(setq input_data (tensor_randn (integers 32 784)))  ; batch_size=32, features=784

; Manual forward pass
(setq h1 (torch_linear_forward linear1 input_data))  ; first layer
(setq h1_relu (tensor_relu h1))                       ; ReLU activation
(setq output (torch_linear_forward linear2 h1_relu)) ; second layer

; Other available components
(setq embedding (torch_embedding_create 1000 128))   ; vocabulary=1000, dim=128
(setq layer_norm (torch_layer_norm_create 128))      ; layer normalization
(setq attention (torch_multihead_attention 128 8))  ; multi-head attention
```

### Loading Pre-trained Models

```lisp
; To load pre-trained models (different from creating models)
(setq model_path "path/to/huggingface/model")
(setq config (dictionary "device" "mps"))
(setq model_id (torch_hf_load_model model_path config))

; Forward pass with HuggingFace model
(setq input_tokens (tensor_create (integers 15496 318 257)))
(setq input_batch (tensor_unsqueeze input_tokens 0))
(setq logits (torch_hf_forward model_id input_batch))
```

### Transformer Blocks

```lisp
; Create a Transformer block
(setq transformer_block (torch_transformer_block_create 512 8 2048))

; Forward pass with mask  
(setq input_seq (tensor_randn (integers 32 128 512)))  ; [batch, seq, dim]
(setq mask (tensor_ones (integers 32 128 128)))        ; attention mask
(setq output (torch_transformer_block_forward transformer_block input_seq mask))
```

### Training with LoRA

```lisp
; Training is primarily done with LoRA
; Load model with LoRA support
(setq model_name "my_model")
(torch_hf_load_model_lora model_path config model_name)

; Initialize LoRA
(setq lora_config (dictionary "rank" 16 "alpha" 32))
(torch_hf_lora_init model_name lora_config)

; Create optimizer for LoRA
(setq lora_params (torch_hf_lora_parameters model_name))
(setq optimizer (torch_adamw_optimizer_create 2e-4))
(torch_optimizer_add_params optimizer lora_params)

; Training loop
(loop epoch 3
    ; Forward pass with LoRA
    (setq logits (torch_hf_forward model_name input_batch))
    
    ; Compute loss
    (setq loss (calculate_loss logits targets))
    
    ; Backward pass
    (torch_optimizer_zero_grad optimizer)
    (torch_backward loss)
    (torch_optimizer_step optimizer)
)
```

## Hugging Face Model Loading

### Configuration and Loading

```lisp
; Complete configuration for HuggingFace loading
(setq model_path "/path/to/model")
(setq config (dictionary 
    ; === BASIC PARAMETERS ===
    "device" "mps"                    ; Device: "cuda", "mps", "cpu"
    
    ; === SEQUENCE PARAMETERS ===
    "max_seq_len" 2048                ; Maximum supported sequence length
    "rope_scaling" 1.0                ; Scaling factor for RoPE (Rotary Position Embedding)
    
    ; === GENERATION PARAMETERS ===
    "temperature" 0.7                 ; Temperature for sampling (0.1-2.0)
    "top_p" 0.9                       ; Nucleus sampling - cumulative probability
    "top_k" 50                        ; Top-K sampling - number of tokens considered
    "repetition_penalty" 1.1          ; Repetition penalty (1.0 = no penalty)
    
    ; === CACHE PARAMETERS ===
    "use_kv_cache" true               ; Enable Key-Value cache for generation
    "max_cache_len" 4096              ; Maximum cache size (sliding window)
    
    ; === ADVANCED PARAMETERS ===
    "manual_attention" false          ; Manual attention mode (debugging/fine control)
))

; Load model with complete configuration
(setq model_id (torch_hf_load_model model_path config))
(println "✓ Model loaded with ID:" model_id)
```

### Inference with KV Cache

```lisp
; Enable KV cache for efficient generation
(setq context_id (torch_hf_enable_kv_cache model_id true))

; Prepare input tokens
(setq input_tokens (tensor_create (integers 15496 318 257)))  ; "This is a"
(setq input_batch (tensor_unsqueeze input_tokens 0))         ; Add batch dimension

; Forward pass (automatically uses cache)
(setq logits (torch_hf_forward model_id input_batch context_id))

; Extract logits from last token
(setq last_logits (tensor_select logits 1 -1))
(setq next_token (tensor_argmax last_logits -1))
```

## LoRA Fine-tuning

### LoRA Initialization

```lisp
; Load model with LoRA support
(setq model_name "llama_lora")
(torch_hf_load_model_lora 
    model_path 
    config 
    model_name)

; LoRA configuration
(setq lora_config (dictionary
    "rank" 16
    "alpha" 32.0
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
    "dtype" "float16"
))

; Initialize LoRA adapters
(torch_hf_lora_init 
    model_name 
    lora_config)
```

### LoRA Training

```lisp
; Create optimizer for LoRA parameters only  
(setq lora_params (torch_hf_lora_parameters model_name))
(setq learning_rate 2e-4)
(setq optimizer (torch_optimizer_create))
(torch_optimizer_add_params optimizer lora_params)

; Training loop
(loop epoch 3
    (println "Epoch" (+ epoch 1))
    
    ; For each data batch
    (loop batch_idx num_batches
        ; Prepare data
        (setq input_ids (get_batch_data batch_idx))
        
        ; Forward pass with LoRA
        (setq logits (torch_hf_forward model_name input_ids))
        
        ; Compute loss
        (setq loss (calculate_lm_loss logits input_ids))
        
        ; Backward pass
        (torch_optimizer_zero_grad optimizer)
        (torch_backward loss)
        (torch_optimizer_step optimizer)
        
        ; Logging
        (if (== (% batch_idx 10) 0)
            (println "  Batch" batch_idx "Loss:" (tensor_item loss)))
    )
    
    ; Save LoRA adapters
    (torch_hf_lora_save model_name (+ output_dir "/epoch_" epoch))
)
```

## Text Generation

### Model Class for Generation

Here's an example implementation of a class for text generation:

```lisp
; Model class for inference
(class@ Model (model_path config tokenizer init)
    (defun configure()
        ; Initialize HuggingFace model
        (setqi model_id (torch_hf_load_model model_path config))
        (setqi temperature 0.7)
        (setqi top_p 0.9)
        (setqi max_tokens 100)
    )
    
    (defun generate(prompt max_length)
        ; Encode prompt
        (setq prompt_tokens (tokenizer (encode prompt)))
        (setq generated_tokens (clone prompt_tokens))
        
        ; Enable KV cache for performance
        (setq context_id (torch_hf_enable_kv_cache model_id true))
        
        ; Token-by-token generation
        (setq current_input (tensor_unsqueeze (tensor_create prompt_tokens) 0))
        
        (loop i max_length
            ; Forward pass
            (setq logits (torch_hf_forward model_id current_input context_id))
            (setq last_logits (tensor_select logits 1 -1))
            (setq last_logits (tensor_select last_logits 0 0))
            
            ; Apply temperature
            (setq scaled_logits (tensor_div_scalar last_logits temperature))
            
            ; Sampling with top-p (nucleus sampling)
            (setq probs (tensor_softmax scaled_logits -1))
            (setq next_token (tensor_multinomial probs 1 true))
            (setq next_token_id (tensor_item next_token))
            
            ; Display generated token
            (print (tokenizer (decode (integers next_token_id))))
            
            ; Add to sequence
            (push generated_tokens next_token_id)
            
            ; Check end token
            (check (== next_token_id (tokenizer (eos_id)))
                (break)
            )
            
            ; Prepare next input (only the new token)
            (setq current_input (tensor_unsqueeze (tensor_create (integers next_token_id)) 0))
        )
        
        (println)  ; New line
        generated_tokens
    )
)
```

### Advanced Generation with Parameters

```lisp
; Complete generation with configuration
(defun generate_with_config(model_path prompt_tokens eos_id)
    ; Examples with different sampling strategies
    
    ; 1. Default generation (simple sampling)
    (setq result_default (torch_hf_generate model_path prompt_tokens eos_id 100))
    
    ; 2. Greedy generation (deterministic)
    (setq greedy_options (dictionary "greedy" true))
    (setq result_greedy (torch_hf_generate model_path prompt_tokens eos_id 100 greedy_options))
    
    ; 3. Top-K sampling (controlled diversity)
    (setq topk_options (dictionary "topk" 50))
    (setq result_topk (torch_hf_generate model_path prompt_tokens eos_id 100 topk_options))
    
    ; 4. Nucleus/Top-P sampling (dynamic diversity)
    (setq topp_options (dictionary "topp" 0.9))
    (setq result_topp (torch_hf_generate model_path prompt_tokens eos_id 100 topp_options))
    
    ; 5. Generation with callback for real-time monitoring
    ; CONCRETE EXAMPLE: Display function to show each generated token
    (defun display_token(token_id tokenizer)
        (printerr (tokenizer (decode (integers token_id))))
    )
    
    (setq callback_options (dictionary 
        "topk" 30                      ; Top-K sampling with K=30
        "callback" 'display_token      ; Function reference (with quote)
        "data" tokenizer               ; Pass tokenizer as data
    ))
    (setq result_callback (torch_hf_generate model_path prompt_tokens eos_id 100 callback_options))
    
    ; Alternative with inline lambda
    (setq lambda_options (dictionary
        "topp" 0.95
        "callback" (lambda (token_id data)
            (println "Generated token:" token_id)
            (printerr (data (decode (integers token_id))))  ; data = tokenizer
        )
        "data" tokenizer
    ))
    
    result_callback
)
```

## Tokenization

### TikToken Tokenizer

```lisp
; Tokenizer class using TikToken
(class@ Tokenizer (tokenizer_path init)
    (defun configure()
        ; Load configuration
        (setq config (json_parse (fread (+ tokenizer_path "/tokenizer_config.json"))))
        (setq vocab (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))
        
        ; Create tokenizer
        (setqi tokenizer_obj (tiktoken_create
            (@ vocab "model" "vocab")
            (@ vocab "added_tokens") 
            (@ vocab "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex")
        ))
        
        ; Get special tokens
        (setqi bos_id (tiktoken_special_encode tokenizer_obj "<|begin_of_text|>"))
        (setqi eos_id (tiktoken_special_encode tokenizer_obj "<|end_of_text|>"))
    )
    
    (defun encode(text)
        ; Encode text with special tokens
        (setq tokens (tiktoken_encode tokenizer_obj text))
        (pushfirst tokens bos_id)
        tokens
    )
    
    (defun decode(token_list)
        ; Decode tokens
        (tiktoken_decode tokenizer_obj token_list)
    )
    
    (defun eos_id()
        eos_id
    )
)
```

### Using the Tokenizer

```lisp
; Create and configure tokenizer
(setq tok (Tokenizer "/path/to/tokenizer"))
(withclass Tokenizer
    (tok (configure))
)

; Encode text
(setq tokens (tok (encode "Hello, how are you?")))
(println "Tokens:" tokens)

; Decode tokens
(setq text (tok (decode tokens)))
(println "Decoded text:" text)
```

## Flash Attention

### Memory-Efficient Attention

```lisp
; Create attention tensors for long sequences
(setq batch_size 2)
(setq num_heads 8) 
(setq seq_length 4096)  ; Long sequence
(setq head_dim 64)

; Query, Key, Value tensors
(setq query (tensor_randn (integers batch_size num_heads seq_length head_dim)))
(setq key (tensor_randn (integers batch_size num_heads seq_length head_dim)))
(setq value (tensor_randn (integers batch_size num_heads seq_length head_dim)))

; Flash Attention - O(N) memory instead of O(N²)
(setq scale (/ 1.0 (sqrt head_dim)))
(setq attention_output (torch_flash_attention query key value scale))

(println "✓ Flash Attention computed efficiently")
(println "Output shape:" (tensor_shape attention_output))
```

### Flash Attention with Mask

```lisp
; Create causal mask
(setq causal_mask (tensor_tril (tensor_ones (integers seq_length seq_length))))

; Apply Flash Attention with mask
(setq masked_output (torch_flash_attention_masked 
    query key value scale causal_mask))
```

## Optimization and Quantization

### FP16 Quantization

```lisp
; Quantize model to FP16 (50% memory reduction)
(setq model_weights (tensor_randn (integers 512 768)))
(setq fp16_weights (torch_quantize_fp16 model_weights))

; Check size
(setq original_size (tensor_size model_weights))
(setq compressed_size (tensor_size fp16_weights))
(println "FP16 compression - Ratio:" (/ compressed_size original_size))
```

### INT8 Quantization

```lisp
; INT8 quantization (75% memory reduction)
(setq int8_weights (torch_quantize_int8 model_weights))

; Dequantize for verification
(setq reconstructed (torch_dequantize int8_weights))
(setq mse_error (torch_mse_loss model_weights reconstructed))
(println "INT8 reconstruction error:" (tensor_item mse_error))
```

### Dynamic Model Quantization

```lisp
; Complete model quantization
(setq quantized_model (torch_model_quantize_dynamic model "qint8"))
(println "✓ Model quantized for production deployment")

; Performance comparison
(setq model original_model)
(setq input_data test_input)
(setq original_time (elapse (torch_model_forward)))
(setq model quantized_model)
(setq quantized_time (elapse (torch_model_forward)))

(println "Quantized speedup:" (/ original_time quantized_time) "x")
```

## Complete LoRA Training Tutorial

This section provides a comprehensive guide for implementing LoRA (Low-Rank Adaptation) fine-tuning using LispE PyTorch, based on a real-world example with Llama 3.1-8B model.

### Overview

LoRA (Low-Rank Adaptation) is a groundbreaking fine-tuning technique that addresses one of the major challenges in adapting large language models: excessive computational and memory resource consumption.

#### The Traditional Fine-tuning Problem

Classical fine-tuning requires updating all model parameters, which for a model like Llama 3.1-8B represents:
- **8 billion parameters** to train
- **32+ GB of memory** for gradients alone
- **Hours or days of training** on high-end GPUs
- **Risk of catastrophic forgetting** of pre-learned knowledge

#### The LoRA Solution

LoRA is based on the hypothesis that updates during fine-tuning have a "low intrinsic rank". Instead of directly modifying the weights W of a layer, LoRA introduces two small matrices A and B such that:

```
W' = W + α/r × A × B
```

Where:
- **W**: original weight matrix (frozen)
- **A**: low-rank matrix (r × d)
- **B**: low-rank matrix (d × r)
- **α**: scaling factor
- **r**: decomposition rank (typically 8-64)

#### Technical Advantages

1. **Drastic Parameter Reduction**:
   - For a 4096×4096 layer with r=16: 16M → 131K parameters (99% reduction)
   - GPU memory divided by 10-100x

2. **Knowledge Preservation**:
   - Pre-trained weights remain intact
   - No catastrophic forgetting
   - Ability to combine multiple adapters

3. **Deployment Flexibility**:
   - Adapters of few MB vs models of GB
   - Runtime adapter switching
   - Simplified storage and distribution

4. **Maintained Performance**:
   - Results comparable to full fine-tuning
   - Often faster convergence
   - Less overfitting

#### Practical Applications

- **Domain-specific adaptation**: medical, legal, technical
- **Personalization**: writing style, tone, response format
- **Multi-task**: multiple adapters for different tasks
- **Rapid prototyping**: hypothesis testing with limited resources

This LispE PyTorch implementation provides a simple and powerful interface for leveraging LoRA with optimized memory management, particularly suited for resource-constrained environments.

### Project Structure

```
training_project/
├── llama3.1-8B/
│   ├── model/                 # Model files (pytorch_model.bin, config.json, etc.)
│   └── tokenizer/            # Tokenizer files (vocab, special_tokens_map.json, etc.)
├── tamgu_dataset.json        # Training dataset in chat format
├── tamgu_lora_adapters_v2/   # Output directory for LoRA adapters
├── checkpoints_v2/           # Training checkpoints
└── lora_training.lisp        # Main training script
```

### Configuration Setup

```lisp
; Load required libraries
(use 'lispe_torch)
(use 'lispe_tiktoken)

; Global configuration
(setq model-path (+ _current "llama3.1-8B/model"))
(setq tiktoken-path (+ _current "llama3.1-8B/tokenizer"))
(setq dataset-path (+ _current "tamgu_dataset.json"))
(setq output-dir (+ _current "tamgu_lora_adapters_v2"))
(setq checkpoint-dir (+ _current "checkpoints_v2"))

; LoRA configuration - KEY POINT: Parameters for decomposition W' = W + α/r × A × B
(setq lora-config (dictionary
    "rank" 16                                          ; 📊 RANK r=16: determines size of matrices A(r×d) and B(d×r)
                                                        ; Smaller = fewer parameters but reduced capacity
    "alpha" 32                                         ; 📈 SCALING α=32: controls amplitude of LoRA adaptations
                                                        ; Ratio α/r = 32/16 = 2.0 (scaling of corrections)
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")  ; 🎯 TARGET MODULES: only attention projections
                                                                      ; Avoids q_proj and k_proj to preserve semantic alignment
))

; Training configuration - OPTIMIZED for LoRA
(setq training-config (dictionary
    "learning_rate" 2e-4                               ; 🎯 HIGH LR: LoRA can handle higher rates (vs 5e-5 typical)
                                                        ; because A,B matrices are zero-initialized → no initial perturbation
    "weight_decay" 0.01                                ; 🛡️ LOW REGULARIZATION: avoids constraining small LoRA matrices
    "num_epochs" 3                                     ; ⚡ FAST CONVERGENCE: LoRA converges faster than full fine-tuning
    "batch_size" 1                                     ; 💾 REDUCED BATCH: compensated by accumulation to save memory
    "gradient_accumulation_steps" 4                    ; 🔄 ACCUMULATION: simulates batch_size=4 without memory overhead
                                                        ; Essential with LoRA memory constraints
    "max_seq_length" 256                               ; 📏 SHORT SEQUENCES: allows more samples with limited memory
    "logging_steps" 10
    "save_steps" 100                                   ; 💾 FREQUENT CHECKPOINTS: LoRA adapters are lightweight (few MB)
    "eval_steps" 50
    "warmup_steps" 100                                 ; 🔥 WARMUP IMPORTANT: stabilizes training of small matrices
    "max_grad_norm" 1.0                                ; ✂️ GRADIENT CLIPPING: prevents instability in low-rank matrices
    "scheduler_type" "linear_warmup_cosine"
    "min_lr" 1e-6
    "device" "mps"  ; "mps" for Apple Silicon, "cuda" for NVIDIA, "cpu" otherwise
))
```

### TikToken Tokenizer Implementation

```lisp
; TikToken tokenizer class for Llama models
(class@ TiktokenTokenizer (tokenizer_path init)
    (defun configure()
        (printerrln "📝 Configuration du tokenizer tiktoken...")
        
        ; Load tokenizer configuration files
        (setq spec_tokens (json_parse (fread (+ tokenizer_path "/special_tokens_map.json"))))
        (setq tok_file (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))
        
        ; Extract special tokens
        (setqi bos_token (@ spec_tokens "bos_token" "content"))
        (setqi eos_token (@ spec_tokens "eos_token" "content"))
        (setq pattern (@ tok_file "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

        ; Create tokenizer object
        (setqi tokenizer_obj (tiktoken_create
            (@ tok_file "model" "vocab")
            (@ tok_file "added_tokens")
            pattern))

        ; Get special token IDs
        (setqi bos_id (tiktoken_special_encode tokenizer_obj bos_token))
        (setqi eos_id (tiktoken_special_encode tokenizer_obj eos_token))
        (setqi pad_id 0)

        (printerrln "✓ Tokenizer configured - vocab size:" (tiktoken_vocab_size tokenizer_obj))
    )

    ; Format text in chat template format
    (defun encode_chat_format(instruction response)
        (setq formatted_text (+
            "<|begin_of_text|>"
            "<|start_header_id|>system<|end_header_id|>\n\n"
            "You are a helpful assistant knowledgeable about programming languages."
            "<|eot_id|>"
            "<|start_header_id|>user<|end_header_id|>\n\n"
            instruction
            "<|eot_id|>"
            "<|start_header_id|>assistant<|end_header_id|>\n\n"
            response
            "<|eot_id|>"
        ))

        ; Encode and add BOS token
        (setq tokens (tiktoken_encode tokenizer_obj formatted_text))
        (pushfirst tokens bos_id)

        ; Truncate if too long
        (if (> (size tokens) (@ training-config "max_seq_length"))
            (setq tokens (@@ tokens 0 (@ training-config "max_seq_length")))
        )

        tokens
    )

    ; Decode tokens back to text
    (defun decode(token_ids)
        (tiktoken_decode tokenizer_obj token_ids)
    )
)
```

### Dataset Management

```lisp
; Dataset manager class for handling training data
(class@ DatasetManager (dataset_path tiktokenizer init)
    (defun configure()
        (printerrln "📚 Loading dataset...")
        
        ; Load and parse JSON dataset
        (setq raw_data (json_parse (fread dataset_path)))
        (setqi samples (list))
        (setqi validation_samples (list))

        ; Split into train/validation (80/20)
        (setq total_size (size raw_data))
        (setq train_size (floor (* total_size 0.8)))

        (printerrln "⚙️  Preparing samples...")
        (loopcount total_size i
            (setq sample (@ raw_data i))
            (setq instruction (@ sample "instruction"))
            (setq response (@ sample "response"))

            ; Tokenize using chat format
            (setq tokens (tiktokenizer TiktokenTokenizer 
                (encode_chat_format instruction response)))
            (setq token_tensor (tensor_create tokens))

            ; Add to appropriate dataset
            (if (< i train_size)
                (push samples token_tensor)
                (push validation_samples token_tensor)
            )

            ; Progress indicator
            (if (== (% i 100) 0)
                (printerr ".")
            )
        )

        (printerrln "\n✓ Dataset prepared:")
        (printerrln "  • Train:" (size samples) "samples")
        (printerrln "  • Validation:" (size validation_samples) "samples")
    )

    ; Get training batch
    (defun get_batch(start_idx batch_size)
        (setq batch (list))
        (setq end_idx (min (+ start_idx batch_size) (size samples)))

        (loopcount (- end_idx start_idx) i
            (push batch (@ samples (+ start_idx i)))
        )
        batch
    )

    ; Get validation batch
    (defun get_validation_batch(max_samples)
        (setq val_batch (list))
        (setq num_samples (min max_samples (size validation_samples)))

        (loopcount num_samples i
            (push val_batch (@ validation_samples i))
        )
        val_batch
    )
)
```

### Model Loading and LoRA Setup

```lisp
; Main LoRA trainer class
(class@ LoRATrainerV2 (model_path tokenizer_path dataset_path config)
    (defun configure()
        (printerrln "⚙️  Configuring LoRA Trainer V2...")
        (setqi model_name "llama31_lora")
        (setqi optimizer nil)
        (setqi scheduler nil)
        (setqi global_step 0)
        (setqi best_loss 1000.0)
        true
    )

    ; Load model with LoRA support
    (defun load_model()
        (printerrln "\n🚀 Loading model with LoRA...")

        ; Load HuggingFace model with LoRA infrastructure
        (torch_hf_load_model_lora
            model_name
            model_path
            (dictionary "device" (@ config "device")))

        ; Display model info
        (setq memory_usage (torch_hf_memory_usage model_name))
        (setq memory_gb (/ memory_usage 1073741824.0))

        (printerrln "✓ HuggingFace model loaded with LoRA support:")
        (printerrln "  • Name:" model_name)
        (printerrln "  • Path:" model_path)
        (printerrln "  • Device:" (@ config "device"))
        (printerrln "  • Memory:" memory_gb "GB")

        true
    )

    ; Initialize LoRA adapters
    (defun setup_lora()
        (printerrln "\n🎨 Initializing LoRA adapters...")

        ; 🔧 CRITICAL INITIALIZATION of LoRA matrices A and B
        ; KEY POINT: Implements decomposition W' = W + α/r × A × B
        (torch_hf_lora_init
            model_name
            (@ lora-config "rank")                      ; r=16: size of matrices A(16×4096) and B(4096×16)
            (@ lora-config "alpha")                     ; α=32: scaling factor to control amplitude
            (@ lora-config "target_modules")            ; Only q_proj, k_proj, v_proj, o_proj of attention
            "bfloat16")                                 ; 🎯 PRECISION: same dtype as base model for consistency

        (printerrln "✓ LoRA adapters initialized:")
        (printerrln "  • Rank:" (@ lora-config "rank"))
        (printerrln "  • Alpha:" (@ lora-config "alpha"))
        (printerrln "  • Modules:" (@ lora-config "target_modules"))

        ; 📊 RETRIEVE LoRA parameters ONLY (matrices A and B)
        ; KEY ADVANTAGE: Only these matrices are trainable, not original weights W
        (setq lora_params (torch_hf_lora_get_parameters model_name))
        (printerrln "  • LoRA parameters:" (size lora_params) "tensors")
        
        ; 💡 REDUCTION CALCULATION: for a 4096×4096 layer with r=16
        ; Original parameters: 4096×4096 = 16M
        ; LoRA parameters: 2×(4096×16) = 131K → 99.2% reduction

        lora_params
    )

    ; Training step with gradient accumulation
    (defun train_step(input_tensor is_accumulating)
        ; Zero gradients at start of accumulation cycle
        (check (not is_accumulating)
            (torch_optimizer_zero_grad optimizer)
        )

        ; 🚀 FORWARD PASS with LoRA integrated automatically
        ; KEY POINT: torch_hf_forward applies W' = W + α/r × A × B transparently
        (setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
        (setq output (torch_hf_forward model_name input_2d))           ; LoRA corrections are automatically added
                                                                       ; to attention projections q, k, v, o

        ; Calculate language modeling loss
        (setq loss (calculate_language_modeling_loss output input_2d))

        (check loss
            ; Scale loss by accumulation steps
            (setq accum_steps (@ config "gradient_accumulation_steps"))
            (setq scaled_loss (tensor_div loss (tensor_create (floats accum_steps))))
            
            ; Backward pass
            (torch_backward scaled_loss)

            ; Memory synchronization for MPS
            (torch_mps_synchronize)

            loss
        )
    )

    ; Main training loop
    (defun train()
        (printerrln "\n🚀 Starting LoRA training...")

        (setq num_epochs (@ config "num_epochs"))
        (setq batch_size (@ config "batch_size"))

        (loopcount num_epochs epoch
            (printerrln "\n📖 Epoch" (+ epoch 1) "/" num_epochs)

            ; Training loop implementation
            ; ... (batch processing, loss calculation, validation)

            (printerrln "✓ Epoch" (+ epoch 1) "completed")
        )

        (printerrln "🎉 Training completed successfully!")
        true
    )
)
```

### Loss Calculation

```lisp
; Language modeling loss calculator
(class@ LossCalculator (init)
    (defun calculate_language_modeling_loss(logits input_tokens)
        (setq logits_shape (tensor_shape logits))
        (setq batch_size (@ logits_shape 0))
        (setq seq_len (@ logits_shape 1))
        (setq vocab_size (@ logits_shape 2))

        (check (> seq_len 1)
            ; Predictions: logits for next token at each position
            (setq pred_logits (tensor_slice logits 1 0 (- seq_len 1)))
            
            ; Targets: actual next tokens (shifted by 1)
            (setq target_tokens (tensor_slice input_tokens 1 1 seq_len))

            ; Reshape for cross-entropy loss
            (setq pred_flat (tensor_reshape pred_logits 
                (integers (* batch_size (- seq_len 1)) vocab_size)))
            (setq target_flat (tensor_reshape target_tokens 
                (integers (* batch_size (- seq_len 1)))))

            ; Calculate cross-entropy loss
            (setq loss (torch_cross_entropy pred_flat target_flat))

            ; Synchronize for memory management
            (torch_mps_synchronize)

            loss
        )
    )
)
```

### Running the Training

```lisp
; Main execution
(printerrln "🚀 Initializing LoRA fine-tuning...\n")

; Create trainer instance
(setq trainer (LoRATrainerV2
    model-path
    tiktoken-path
    dataset-path
    training-config))

; Execute training pipeline
(withclass LoRATrainerV2
    (trainer (configure))
    (if (trainer (load_model))
        (if (trainer (setup_components))
            (if (trainer (train))
                (printerrln "\n✅ LoRA fine-tuning completed successfully! 🎉")
                (printerrln "\n❌ Training failed")
            )
            (printerrln "\n❌ Component setup failed")
        )
        (printerrln "\n❌ Model loading failed")
    )
)
```

### Key Features

1. **Memory Efficiency**: Uses gradient accumulation and MPS synchronization for optimal memory usage
2. **Modular Design**: Separate classes for tokenizer, dataset, loss calculation, and training
3. **Monitoring**: Comprehensive logging of loss, learning rate, and memory usage
4. **Checkpointing**: Regular saving of LoRA adapters and training state
5. **Validation**: Periodic evaluation on held-out data
6. **Device Support**: Automatic detection of best available device (CUDA/MPS/CPU)

### Dataset Format

The training dataset should be in JSON format with instruction-response pairs:

```json
[
    {
        "instruction": "How do I define a function in Tamgu?",
        "response": "In Tamgu, you define a function using the 'function' keyword..."
    },
    {
        "instruction": "What are the basic data types in Tamgu?",
        "response": "Tamgu supports several data types including..."
    }
]
```

This complete example demonstrates how to implement efficient LoRA fine-tuning with proper memory management, comprehensive monitoring, and modular code organization using LispE PyTorch.

## Practical Examples

### Example 1: Simple Classification

```lisp
; Train a simple MNIST classifier
(defun train_mnist_classifier()
    ; Create model
    (setq input_size 784)
    (setq hidden_size 128)
    (setq output_size 10)
    (setq model (torch_model_create))
    (setq learning_rate 0.001)
    (setq optimizer (torch_optimizer_create))
    (torch_optimizer_add_params optimizer model)
    
    ; Example data (normalization required)
    (setq train_data (tensor_div_scalar (tensor_randn (integers 1000 784)) 255.0))
    (setq train_labels (tensor_randint 0 10 (integers 1000)))
    
    ; Training
    (loop epoch 50
        (setq input_data train_data)
        (setq predictions (torch_model_forward))
        (setq loss (torch_loss_crossentropy predictions train_labels))
        
        (torch_optimizer_zero_grad optimizer)
        (torch_backward loss)
        (torch_optimizer_step optimizer)
        
        (if (== (% epoch 10) 0)
            (println "Epoch" epoch "Loss:" (tensor_item loss)))
    )
    
    model
)
```

### Example 2: Language Model Fine-tuning

```lisp
; Complete fine-tuning with LoRA
(defun finetune_language_model(model_path dataset_path)
    ; Configuration
    (setq config (dictionary
        "device" "mps"
        "dtype" "float16"
        "low_memory" true
    ))
    
    ; Load model and tokenizer
    (setq model_name "llama_ft")
    (torch_hf_load_model_lora model_path config model_name)
    
    (setq tokenizer (Tokenizer (+ model_path "/tokenizer")))
    (withclass Tokenizer (tokenizer (configure)))
    
    ; Initialize LoRA
    (setq lora_config (dictionary
        "rank" 16
        "alpha" 32
        "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
    ))
    (torch_hf_lora_init model_name lora_config)
    
    ; Load dataset
    (setq dataset (json_parse (fread dataset_path)))
    
    ; Training configuration
    (setq train_config (dictionary
        "learning_rate" 2e-4
        "num_epochs" 3
        "batch_size" 1
        "max_seq_length" 512
    ))
    
    ; Create optimizer
    (setq lora_params (torch_hf_lora_parameters model_name))
    (setq learning_rate (@ train_config "learning_rate"))
    (setq optimizer (torch_optimizer_create))
    (torch_optimizer_add_params optimizer lora_params)
    
    ; Training loop
    (loop epoch (@ train_config "num_epochs")
        (println "=== Epoch" (+ epoch 1) "===")
        
        (loop sample_idx (size dataset)
            (setq sample (@ dataset sample_idx))
            (setq instruction (@ sample "instruction"))
            (setq response (@ sample "output"))
            
            ; Prepare tokens
            (setq formatted_text (+
                "<|start_header_id|>user<|end_header_id|>\n"
                instruction
                "<|eot_id|><|start_header_id|>assistant<|end_header_id|>\n"
                response
                "<|eot_id|>"
            ))
            
            (setq tokens (tokenizer (encode formatted_text)))
            (setq input_tensor (tensor_unsqueeze (tensor_create tokens) 0))
            
            ; Forward pass
            (setq logits (torch_hf_forward model_name input_tensor))
            
            ; Compute loss (language modeling)
            (setq loss (calculate_language_modeling_loss logits input_tensor))
            
            ; Backward pass
            (torch_optimizer_zero_grad optimizer)
            (torch_backward loss)
            (torch_optimizer_step optimizer)
            
            ; Logging
            (if (== (% sample_idx 50) 0)
                (println "  Sample" sample_idx "/" (size dataset) 
                        "Loss:" (tensor_item loss)))
        )
        
        ; Save LoRA adapters
        (setq checkpoint_path (+ "./checkpoints/epoch_" epoch))
        (torch_hf_lora_save model_name checkpoint_path)
        (println "✓ Checkpoint saved:" checkpoint_path)
    )
    
    model_name
)
```

### Example 3: Interactive Generation

```lisp
; Interactive generation system
(defun interactive_generation(model_path tokenizer_path)
    ; Load model
    (setq config (dictionary "device" "mps"))
    (setq model_id (torch_hf_load_model model_path config))
    
    ; Load tokenizer
    (setq tok (Tokenizer tokenizer_path))
    (withclass Tokenizer (tok (configure)))
    
    ; Interactive loop
    (loop true
        (print "Prompt (or 'quit' to exit): ")
        (setq user_input (input))
        
        (check (== user_input "quit")
            (break)
        )
        
        ; Generate response
        (println "\nGenerating...")
        (setq response (generate_response model_id tok user_input))
        (println "Generation complete.")
        (println)
    )
)

(defun generate_response(model_id tokenizer prompt)
    ; Encode prompt
    (setq prompt_tokens (tokenizer (encode prompt)))
    (setq context_id (torch_hf_enable_kv_cache model_id true))
    
    ; Generate response
    (setq current_input (tensor_unsqueeze (tensor_create prompt_tokens) 0))
    (setq generated (clone prompt_tokens))
    
    (print "Response: ")
    (loop i 150  ; max_tokens
        (setq logits (torch_hf_forward model_id current_input context_id))
        (setq last_logits (tensor_select logits 1 -1))
        (setq last_logits (tensor_select last_logits 0 0))
        
        ; Sampling with temperature
        (setq scaled_logits (tensor_div_scalar last_logits 0.7))
        (setq probs (tensor_softmax scaled_logits -1))
        (setq next_token (tensor_multinomial probs 1 true))
        (setq next_token_id (tensor_item next_token))
        
        ; Display token
        (setq token_text (tokenizer (decode (integers next_token_id))))
        (print token_text)
        
        ; Check end of generation
        (check (== next_token_id (tokenizer (eos_id)))
            (break)
        )
        
        (push generated next_token_id)
        (setq current_input (tensor_unsqueeze (tensor_create (integers next_token_id)) 0))
    )
    
    (println)
    generated
)
```

## Best Practices and Optimizations

### Memory Management

```lisp
; Use optimized LispE tensors
(setq data (floats 1.0 2.0 3.0))  ; More efficient than (list 1.0 2.0 3.0)

; Explicitly free GPU memory if needed
(torch_cuda_empty_cache)  ; CUDA
```

### Performance

```lisp
; Use the right device for your tensors
(setq tensor (torch_to_device (tensor_randn (integers 100 100)) device))

; Group operations to avoid synchronizations
(setq result (tensor_add 
    (tensor_mul a b) 
    (tensor_div c d)))
```

### Debugging

```lisp
; Check tensor shapes
(defun debug_tensor(tensor name)
    (println name "- Shape:" (tensor_shape tensor) 
            "Device:" (torch_device tensor)
            "Dtype:" (torch_dtype tensor))
)

; Monitor memory usage
(defun memory_info()
    (if (torch_cuda_is_available)
        (println "CUDA memory used:" (torch_cuda_memory_allocated) "bytes")
        (println "CPU usage"))
)
```

# Available Functions

This section lists all available functions in the LispE PyTorch library, organized by family and in alphabetical order.

## Attention Functions

### `torch_attention_forward(attention query key value)`
Applies multi-head attention to query, key and value tensors using a pre-created attention module.

### `torch_attention_mask(sequences pad_token)`
Creates an attention mask to mask padding tokens in sequences.

### `torch_flash_attention(query key value)`
Applies the memory-optimized Flash Attention algorithm to Q, K, V tensors.

### `torch_flash_attention_create(embed_dim num_heads dropout bias)`
Creates a Flash Attention module with embedding dimension, number of heads, dropout rate and bias option.

### `torch_flash_attention_forward(flash_attention query key value)`
Performs forward pass of Flash Attention module with Q, K, V tensors.

### `torch_flash_attention_with_dropout(flash_attention query key value dropout_p training)`
Applies Flash Attention with custom dropout rate in training or evaluation mode.

### `torch_flash_attention_with_mask(flash_attention query key value attn_mask)`
Applies Flash Attention with an attention mask to mask certain positions.

### `torch_multihead_attention(embed_dim num_heads)`
Creates a multi-head attention module with specified embedding dimension and number of heads.

### `torch_scaled_dot_product_attention(query key value attn_mask dropout_p is_causal scale)`
Implements scaled dot-product attention with mask, dropout, causal option and scale factor.

## Device and GPU Functions

### `torch_cuda_device_count()`
Returns the number of CUDA devices available on the system.

### `torch_cuda_empty_cache()`
Empties CUDA memory cache to free unused GPU memory.

### `torch_cuda_is_available()`
Checks if CUDA is available on the system.

### `torch_cuda_memory_allocated()`
Returns the amount of CUDA memory currently allocated in bytes.

### `torch_cuda_memory_total()`
Returns the total amount of CUDA memory available in bytes.

### `torch_get_best_device()`
Automatically determines the best available device (CUDA, MPS or CPU).

### `torch_mps_is_available()`
Checks if Metal Performance Shaders (MPS) is available on Apple Silicon.

### `torch_mps_synchronize((safemode))`
Synchronizes MPS operations with optional safe mode.

### `torch_on_mps(tensor)`
Checks if a tensor is on the MPS device.

### `torch_set_device(device)`
Sets the default device for PyTorch operations.

### `torch_to_cpu(tensor)`
Moves a tensor to CPU.

### `torch_to_cuda(tensor (device))`
Moves a tensor to a specific or default CUDA device.

### `torch_to_mps(tensor)`
Moves a tensor to MPS device (Apple Silicon).

### `torch_to_device(tensor device)`
Moves a tensor to device.

## Embedding and Encoding Functions

### `torch_apply_rotary_pos_emb(tensor cos sin)`
Applies Rotary Position Embedding (RoPE) to a tensor with pre-calculated cosine and sine.

### `torch_embedding(num_embeddings embedding_dim)`
Creates an embedding layer with specified number of embeddings and dimension.

### `torch_embedding_forward(embedding input_data)`
Performs forward pass of an embedding layer with input data.

### `torch_positional_encoding(d_model max_len)`
Creates a positional encoding module with model dimension and maximum length.

### `torch_positional_forward(positional_encoding input_data)`
Applies positional encoding to input data.

### `torch_rotary_embedding(dim max_seq_len)`
Creates a Rotary Position Embedding (RoPE) module with dimension and maximum length.

### `torch_rotary_forward(rotary_embedding seq_len device)`
Computes cosine and sine for rotary embedding for a given sequence length.

## Text Generation Functions

### `torch_generate(generator input_ids strategy)`
Generates text from a generator with input tokens and specified strategy.

### `torch_generator_config(generator config)`
Configures text generator parameters (temperature, top_k, top_p, etc.).

### `torch_generator_create(model device)`
Creates a text generator based on a specified model and device.

### `torch_set_generation_params(generator max_length temperature top_k top_p)`
Sets generation parameters for a generator (max length, temperature, top_k, top_p).

## Gradient Checkpointing Functions

### `torch_checkpoint_create(module)`
Creates a module with gradient checkpointing to save memory during training.

### `torch_checkpoint_disable(module)`
Disables gradient checkpointing for a module.

### `torch_checkpoint_enable(module)`
Enables gradient checkpointing for a module.

### `torch_checkpoint_forward(module input_data)`
Performs forward pass with gradient checkpointing.

## Hugging Face Functions

### `torch_hf_clear_attention_scores(path (kvcache))`
Clears attention scores stored in cache.

### `torch_hf_embeddings(path token_ids)`
Gets embeddings for a list of tokens from a Hugging Face model.

### `torch_hf_enable_kv_cache(path enable)`
Enables or disables Key-Value cache for a Hugging Face model.

### `torch_hf_forward(path input_ids (kvcache))`
Performs forward pass of a Hugging Face model with optional KV cache.

### `torch_hf_forward_attention_scores(path layer_index (kvcache))`
Gets attention scores for a specific layer of the model.

### `torch_hf_forward_attention_size(path (kvcache))`
Returns the size of model attention tensors.

### `torch_hf_forward_manual(path input_ids (kvcache))`
Performs manual forward pass with detailed cache control.

### `torch_hf_generate(path initial_tokens eos_id max_length (options))`
Generates text from a Hugging Face model with initial tokens and parameters.

**Parameters:**
- `path`: Path/ID of loaded model
- `initial_tokens`: Input tokens (list of integers)
- `eos_id`: End-of-sequence token ID(s) (integer or list of integers)
- `max_length`: Maximum number of tokens to generate
- `options`: Optional dictionary with:
  - `"topk"` (integer): Top-K sampling - consider only K best tokens
  - `"topp"` (float): Top-P/Nucleus sampling - cumulative probability (0.0-1.0)
  - `"greedy"` (boolean): Greedy sampling - always select most probable token
  - `"callback"` (function): Function called for each generated token
    - Signature: `(callback token_id data)`
    - `token_id` (integer): Generated token ID
    - `data`: Data passed via "data" parameter
    - Can be function reference (with quote) or lambda
  - `"data"` (any): Data passed to callback (e.g., tokenizer, context)

**Sampling strategies (mutually exclusive):**
- **None**: Simple sampling with softmax + multinomial (default)
- **`"greedy"`**: Deterministic, selects most probable token
- **`"topk"`**: Limits to K most probable tokens
- **`"topp"`**: Nucleus sampling, limits to cumulative probability P

### `torch_hf_get_down_weight(path layer)`
Retrieves down projection weights from a specific FFN layer.

### `torch_hf_get_gate_up_fused_weight(path layer)`
Retrieves fused gate and up weights from an FFN layer.

### `torch_hf_get_gate_weight(path layer)`
Retrieves gate projection weights from a specific FFN layer.

### `torch_hf_get_k_weight(path layer)`
Retrieves Key projection weights from a specific attention layer.

### `torch_hf_get_o_weight(path layer)`
Retrieves Output projection weights from a specific attention layer.

### `torch_hf_get_q_weight(path layer)`
Retrieves Query projection weights from a specific attention layer.

### `torch_hf_get_qkv_fused_weight(path layer)`
Retrieves fused Query, Key, Value weights from an attention layer.

### `torch_hf_get_rms_norm_eps(path)`
Retrieves the epsilon value used for RMS normalization in the model.

### `torch_hf_get_up_weight(path layer)`
Retrieves up projection weights from a specific FFN layer.

### `torch_hf_get_v_weight(path layer)`
Retrieves Value projection weights from a specific attention layer.

### `torch_hf_get_weight(path name)`
Retrieves a specific weight tensor from the model by name.

### `torch_hf_list_weights(path)`
Lists all available weight tensor names in the model.

### `torch_hf_load_model(path (config nil))`
Loads a Hugging Face model from a path with optional configuration.

**Config dictionary parameters:**
- `"device"` (string): Target device ("cuda", "mps", "cpu")
- `"max_seq_len"` (integer): Maximum sequence length (default: from config.json)
- `"rope_scaling"` (float): RoPE scaling factor (default: 1.0)
- `"temperature"` (float): Generation temperature (default: 1.0)
- `"top_p"` (float): Nucleus sampling (default: 1.0)
- `"top_k"` (integer): Top-K sampling (default: 0 = disabled)
- `"repetition_penalty"` (float): Repetition penalty (default: 1.0)
- `"use_kv_cache"` (boolean): Enable KV cache (default: true)
- `"max_cache_len"` (integer): Max cache size (default: max_seq_len)
- `"manual_attention"` (boolean): Manual attention mode (default: false)

### `torch_hf_lora_enable(model_name enable)`
Enables or disables LoRA adapters for a model.

### `torch_hf_lora_get_parameters(model_name)`
Retrieves LoRA parameters from a model for optimization.

### `torch_hf_lora_init(model_name rank alpha target_modules (dtype))`
Initializes LoRA adapters for a model with rank, alpha and target modules.

### `torch_hf_lora_load(model_name path)`
Loads pre-trained LoRA adapters from a file.

### `torch_hf_lora_merge(model_name)`
Merges LoRA adapters with the main model weights.

### `torch_hf_lora_save(model_name path)`
Saves trained LoRA adapters to a file.

### `torch_hf_lora_unmerge(model_name)`
Separates LoRA adapters from main model weights.

### `torch_hf_memory_usage(path)`
Returns memory usage of a loaded Hugging Face model.

### `torch_hf_model_info(path)`
Retrieves detailed information about a Hugging Face model.

### `torch_hf_model_summary(path)`
Displays a summary of Hugging Face model characteristics.

### `torch_hf_reset_kv_cache(kvcache)`
Resets Key-Value cache to restart from an empty sequence.

## JIT (TorchScript) Functions

### `torch_jit_load(model_path (device "cpu"))`
Loads a TorchScript model from a file with optional device.

### `torch_jit_model_forward(model tensor)`
Performs forward pass of a JIT model with an input tensor.

### `torch_jit_model_forward_with_lora(model input)`
Performs forward pass of a JIT model with LoRA adapters.

### `torch_jit_model_get_buffer(model buffername)`
Retrieves a specific buffer from a JIT model by name.

### `torch_jit_model_get_intermediate_states(model input layer_names)`
Gets intermediate states from specific layers during forward pass.

### `torch_jit_model_get_tensor(model tensorname)`
Retrieves a specific tensor from a JIT model by name.

### `torch_jit_model_get_tensor_shape(model tensorname)`
Returns the shape of a specific tensor in a JIT model.

### `torch_jit_model_info(model)`
Displays detailed information about a JIT model.

### `torch_jit_model_list_buffers(model)`
Lists all available buffers in a JIT model.

### `torch_jit_model_list_methods(model)`
Lists all available methods in a JIT model.

### `torch_jit_model_list_parameter_names(model)`
Lists all parameter names in a JIT model.

### `torch_jit_model_list_tensor_names(model)`
Lists all available tensor names in a JIT model.

### `torch_jit_model_register_lora_hook(model layer_name lora_layer)`
Registers a LoRA hook for a specific layer in a JIT model.

### `torch_jit_model_to_best_device(model)`
Moves a JIT model to the best available device.

### `torch_jit_model_to_device(model (device "cpu"))`
Moves a JIT model to a specific device.

### `torch_jit_model_to_mps(model)`
Moves a JIT model to MPS device (Apple Silicon).

### `torch_jit_model_update_weight(model param_name new_weight)`
Updates a specific weight in a JIT model.

### `torch_jit_unload(model)`
Unloads a JIT model from memory.

## Learning Rate Scheduling Functions

### `torch_lr_scheduler(optimizer scheduler_type config)`
Creates a learning rate scheduler with type and configuration.

### `torch_scheduler_get_lr(scheduler)`
Gets the current learning rate from a scheduler.

### `torch_scheduler_set_lr(scheduler learning_rate)`
Sets a new learning rate for a scheduler.

### `torch_scheduler_step(scheduler)`
Performs a scheduling step to update the learning rate.

## LoRA Functions

### `torch_hf_load_model_lora(model_name path config)`
Loads a Hugging Face model with integrated LoRA support.

### `torch_lora_apply_to_linear(linear_layer rank alpha)`
Applies LoRA adapters to an existing linear layer.

### `torch_lora_compute_delta(lora_layer)`
Computes the delta matrix (A×B) of a LoRA layer.

### `torch_lora_forward(lora_layer input_data)`
Performs forward pass of a LoRA layer.

### `torch_lora_forward_with_gradients(lora_layer input retain_graph)`
Performs LoRA forward pass while retaining computation graph for gradients.

### `torch_lora_get_adaptation_magnitude(lora_layer)`
Computes the magnitude of LoRA adaptation relative to original weights.

### `torch_lora_linear(in_features out_features rank alpha)`
Creates a linear layer with integrated LoRA adapters.

### `torch_lora_load_adapters(model path)`
Loads saved LoRA adapters into a model.

### `torch_lora_merge_weights(lora_layer)`
Merges LoRA weights with base layer weights.

### `torch_lora_save_adapters(model path)`
Saves model LoRA adapters to a file.

### `torch_lora_trainable_params(model)`
Retrieves only the trainable LoRA parameters from a model.

## Loss Functions

### `torch_backward(loss)`
Performs backpropagation from a loss tensor.

### `torch_cross_entropy(predictions targets)`
Computes cross-entropy loss between predictions and targets.

### `torch_crossentropy_loss(predictions targets)`
Computes cross-entropy loss (alias for torch_cross_entropy).

### `torch_mse_loss(predictions targets)`
Computes mean squared error loss between predictions and targets.

## Model Functions

### `torch_forward(model input_data)`
Performs forward pass of a model with input data.

### `torch_load_checkpoint(path)`
Loads a complete checkpoint containing model, optimizer and epoch.

### `torch_load_model(model path)`
Loads model weights from a file.

### `torch_load_state_dict(model state_dict)`
Loads a state dictionary into a model.

### `torch_model(input_size hidden_size output_size)`
Creates a simple MLP model with specified input, hidden and output sizes.

### `torch_save_checkpoint(model optimizer epoch path)`
Saves a complete checkpoint with model, optimizer and epoch number.

### `torch_save_model(model path)`
Saves model weights to a file.

### `torch_state_dict(model)`
Retrieves the state dictionary of a model (all parameters).

## Neural Network Functions

### `torch_linear(in_features out_features)`
Creates a linear (dense) layer with number of input and output features.

### `torch_layer_norm(normalized_shape)`
Creates a layer normalization layer with specified shape.

### `torch_layer_norm_forward(layer_norm input_data)`
Applies layer normalization to input data.

### `torch_linear_forward(linear input_data)`
Performs forward pass of a linear layer.

### `torch_transformer_block(embed_dim num_heads ffn_dim)`
Creates a Transformer block with embedding dimension, number of heads and FFN dimension.

### `torch_transformer_forward(block input_data)`
Performs forward pass of a Transformer block.

## Optimization Functions

### `torch_adam_optimizer(learning_rate)`
Creates an Adam optimizer with specified learning rate.

### `torch_adamw_optimizer(learning_rate)`
Creates an AdamW optimizer with specified learning rate.

### `torch_clip_grad_norm(optimizer max_norm)`
Applies gradient clipping by norm to prevent gradient explosion.

### `torch_optimizer(model learning_rate type)`
Creates a generic optimizer for a model with learning rate and type.

### `torch_optimizer_add_params(params learning_rate weight_decay)`
Adds parameters to an optimizer with learning rate and weight decay.

### `torch_optimizer_step(optimizer)`
Performs an optimization step (weight update).

### `torch_optimizer_zero_grad(optimizer)`
Zeros gradients of all optimizer parameters.

### `torch_set_grad_enabled(enabled)`
Globally enables or disables gradient computation.

### `torch_sgd_optimizer(learning_rate)`
Creates an SGD (Stochastic Gradient Descent) optimizer with learning rate.

## Quantization Functions

### `torch_dequantize(quantized_tensor)`
Dequantizes a quantized tensor to its floating-point representation.

### `torch_model_quantize_dynamic(model)`
Applies dynamic quantization to a complete model.

### `torch_model_quantize_static(model calibration_data)`
Applies static quantization to a model with calibration data.

### `torch_quantize_dynamic(tensor dtype)`
Applies dynamic quantization to a tensor with specified data type.

### `torch_quantize_fp16(tensor)`
Quantizes a tensor to half precision (16-bit float).

### `torch_quantize_int8(tensor)`
Quantizes a tensor to 8-bit integers.

### `torch_quantize_linear(tensor scale zero_point)`
Applies linear quantization with scale factor and zero point.

### `torch_quantize_per_channel(tensor scales zero_points axis)`
Applies per-channel quantization with scale factors and zero points.

### `torch_quantize_static(tensor scale zero_point dtype)`
Applies static quantization with fixed parameters.

## Sampling Functions

### `tensor_multinomial(probs num_samples replacement)`
Samples from a multinomial distribution with or without replacement.

### `torch_sort(tensor dim descending)`
Sorts a tensor along a dimension in ascending or descending order.

### `torch_topk(tensor k dim largest)`
Returns the k largest (or smallest) values along a dimension.

## Tensor Functions - Activation

### `tensor_abs(tensor)`
Computes the absolute value of each tensor element.

### `tensor_gelu(tensor)`
Applies the GELU (Gaussian Error Linear Unit) activation function.

### `tensor_relu(tensor)`
Applies the ReLU (Rectified Linear Unit) activation function.

### `tensor_sigmoid(tensor)`
Applies the sigmoid activation function.

### `tensor_silu(tensor)`
Applies the SiLU (Sigmoid Linear Unit, also called Swish) activation function.

### `tensor_softmax(tensor dim)`
Applies the softmax function along the specified dimension.

### `tensor_tanh(tensor)`
Applies the hyperbolic tangent activation function.

## Tensor Functions - Arithmetic

### `tensor_add(tensor1 tensor2)`
Element-wise addition of two tensors.

### `tensor_add_scalar(tensor scalar)`
Adds a scalar to all elements of a tensor.

### `tensor_div(tensor1 tensor2)`
Element-wise division of two tensors.

### `tensor_div_scalar(tensor scalar)`
Divides all elements of a tensor by a scalar.

### `tensor_matmul(tensor1 tensor2)`
Matrix multiplication of two tensors.

### `tensor_mul(tensor1 tensor2)`
Element-wise multiplication of two tensors.

### `tensor_mul_scalar(tensor scalar)`
Multiplies all elements of a tensor by a scalar.

### `tensor_neg(tensor)`
Computes the negation of each tensor element.

### `tensor_reciprocal(tensor)`
Computes the reciprocal (1/x) of each tensor element.

### `tensor_sub(tensor1 tensor2)`
Element-wise subtraction of two tensors.

## Tensor Functions - Creation

### `tensor_cat(tensors dim)`
Concatenates a list of tensors along the specified dimension.

### `tensor_full(shape value)`
Creates a tensor of specified shape filled with a value.

### `tensor_full_like(tensor fill_value)`
Creates a tensor with same shape as an existing tensor, filled with a value.

### `tensor_ones(shape)`
Creates a tensor of specified shape filled with ones.

### `tensor_randn(shape)`
Creates a tensor of specified shape with random normal values.

### `tensor_randint(low high shape)`
Creates a tensor of random integers between low (inclusive) and high (exclusive).

### `tensor_create(thedata) or torch_tensor(thedata)`
Creates a tensor from LispE data (lists, matrices).

### `tensor_zeros(shape)`
Creates a tensor of specified shape filled with zeros.

## Tensor Functions - Information

### `tensor_item(tensor)`
Extracts the scalar value from a single-element tensor.

### `tensor_shape(tensor)`
Returns the shape (dimensions) of a tensor.

### `tensor_size(tensor)`
Returns the total size (number of elements) of a tensor.

### `tensor_to_list(tensor)`
Converts a PyTorch tensor to a LispE list.

## Tensor Functions - Manipulation

### `tensor_clamp(tensor min_val max_val)`
Clamps tensor values between min_val and max_val.

### `tensor_contiguous(tensor)`
Ensures a tensor is stored contiguously in memory.

### `tensor_cumsum(tensor dim)`
Computes cumulative sum along a dimension.

### `tensor_gather(input dim index)`
Gathers values along a dimension according to an index tensor.

### `tensor_masked_fill_(tensor mask value)`
Replaces tensor elements with a value where mask is true.

### `tensor_reshape(tensor shape)`
Changes the shape of a tensor without modifying its data.

### `tensor_select(tensor dim index)`
Selects a slice along a dimension at a specific index.

### `tensor_set_item(tensor indices value)`
Sets the value at specific indices in a tensor.

### `tensor_slice(tensor dim start end)`
Extracts a slice from a tensor along a dimension.

### `tensor_squeeze(tensor dim)`
Removes dimensions of size 1 from the tensor.

### `tensor_transpose(tensor dim0 dim1)`
Transposes two dimensions of a tensor.

### `tensor_triu(tensor diagonal)`
Returns the upper triangular part of a matrix tensor.

### `tensor_unsqueeze(tensor dim)`
Adds a dimension of size 1 at the specified position.

## Tensor Functions - Mathematics

### `tensor_acos(tensor)`
Computes the arccosine of each tensor element.

### `tensor_asin(tensor)`
Computes the arcsine of each tensor element.

### `tensor_atan(tensor)`
Computes the arctangent of each tensor element.

### `tensor_ceil(tensor)`
Rounds each tensor element up (ceiling).

### `tensor_cos(tensor)`
Computes the cosine of each tensor element.

### `tensor_cosh(tensor)`
Computes the hyperbolic cosine of each tensor element.

### `tensor_einsum(indices tensors)`
Performs Einstein summation over tensors according to specified notation.

### `tensor_exp(tensor)`
Computes the exponential of each tensor element.

### `tensor_floor(tensor)`
Rounds each tensor element down (floor).

### `tensor_linear(tensor1 tensor2)`
Applies a linear transformation (matrix multiplication + optional bias).

### `tensor_log(tensor)`
Computes the natural logarithm of each tensor element.

### `tensor_log10(tensor)`
Computes the base-10 logarithm of each tensor element.

### `tensor_log2(tensor)`
Computes the base-2 logarithm of each tensor element.

### `tensor_log_softmax(tensor dim)`
Applies log(softmax(x)) in a numerically stable way.

### `tensor_pow(tensor exponent)`
Raises each tensor element to the power of the exponent.

### `tensor_rms_norm(input weight (eps 1e-6))`
Applies RMS (Root Mean Square) normalization with weight and epsilon.

### `tensor_round(tensor)`
Rounds each tensor element to the nearest integer.

### `tensor_rsqrt(tensor)`
Computes the reciprocal square root of each element.

### `tensor_sin(tensor)`
Computes the sine of each tensor element.

### `tensor_sinh(tensor)`
Computes the hyperbolic sine of each tensor element.

### `tensor_sqrt(tensor)`
Computes the square root of each tensor element.

### `tensor_tan(tensor)`
Computes the tangent of each tensor element.

## Tensor Functions - Reduction

### `tensor_argmax(tensor dim (keepdim true))`
Returns indices of maximum values along a dimension.

### `tensor_max(tensor)`
Returns the maximum value of the tensor.

### `tensor_mean(tensor)`
Computes the mean of all tensor elements.

### `tensor_mean_dim(tensor dim)`
Computes the mean along a specific dimension.

### `tensor_min(tensor)`
Returns the minimum value of the tensor.

### `tensor_std(tensor)`
Computes the standard deviation of all tensor elements.

### `tensor_sum(tensor)`
Computes the sum of all tensor elements.

## Tokenization Functions

### `torch_decode(tokenizer token_ids)`
Decodes a list of token IDs to text with a tokenizer.

### `torch_encode(tokenizer text)`
Encodes text to a list of token IDs with a tokenizer.

### `torch_pad_sequences(sequences max_length pad_token)`
Pads sequences to have the same length.

### `torch_sentencepiece_tokenizer(model_path)`
Creates a SentencePiece tokenizer from a pre-trained model.

### `torch_simple_tokenizer()`
Creates a simple space-based tokenizer.

### `torch_train_sentencepiece(input_file model_prefix vocab_size model_type)`
Trains a new SentencePiece model on an input file.

### `torch_vocab_size(tokenizer)`
Returns the vocabulary size of a tokenizer.

## Tucker Decomposition Functions

### `torch_khatri_rao_product(A B)`
Computes the Khatri-Rao product of two matrices.

### `torch_tucker_compression_ratio(original_shape core_shape factor_shapes)`
Computes the compression ratio of a Tucker decomposition.

### `torch_tucker_decomposition(tensor rank (max_iter 100) (tol 1e-6))`
Performs Tucker decomposition of a tensor with rank and parameters.

### `torch_tucker_reconstruct(core factors)`
Reconstructs a tensor from its Tucker decomposition core and factors.

## Utility Functions

### `tensor_in_memory()`
Returns information about tensors currently in memory.

---

This manual covers the essential aspects of the LispE PyTorch library. For specific use cases or advanced features, consult the examples in the repository and the documentation for individual functions.