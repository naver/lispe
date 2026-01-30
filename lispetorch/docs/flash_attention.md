# ⚡ Flash Attention Implementation Guide

> **Memory-Efficient Attention for Long Sequences**

Flash Attention provides O(N) memory complexity for attention computation, enabling processing of much longer sequences than traditional O(N²) implementations.

## 🎯 **Key Benefits**

- **📉 Memory Efficient**: O(N) vs O(N²) memory usage
- **⚡ Performance**: Native PyTorch 2.0+ optimized kernels
- **📏 Long Sequences**: Handle 8K+ tokens with linear memory scaling
- **🔧 Production Ready**: Masking, dropout, causal attention support
- **🚀 Easy Integration**: Drop-in replacement for standard attention

## 🚀 **Quick Start**

### Basic Flash Attention
```lisp
; Load library
(use 'lispe_torch)

; Create tensors (batch_size=1, seq_len=512, embed_dim=256)
(setq query (torch_randn (integers 1 512 256)))
(setq key_tensor (torch_randn (integers 1 512 256)))
(setq value_tensor (torch_randn (integers 1 512 256)))

; Simple Flash Attention call
(setq output (torch_flash_attention query key_tensor value_tensor))
(print "Output shape:" (torch_shape output))  ; → (1 512 256)
```

### Advanced Flash Attention Module
```lisp
; Create Flash Attention module
(setq flash_attn (torch_flash_attention_create 256 8 0.1 true))
; Parameters: embed_dim=256, num_heads=8, dropout=0.1, bias=true

; Forward pass through module
(setq module_output (torch_flash_attention_forward flash_attn query key_tensor value_tensor))
```

## 📋 **Function Reference**

### Core Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `torch_flash_attention` | Simple Flash Attention | `(query key value)` |
| `torch_flash_attention_create` | Create Flash Attention module | `(embed_dim num_heads dropout bias)` |
| `torch_flash_attention_forward` | Forward pass through module | `(module query key value)` |
| `torch_flash_attention_with_mask` | Attention with mask | `(module query key value mask)` |
| `torch_flash_attention_with_dropout` | Custom dropout | `(module query key value dropout_p training)` |
| `torch_scaled_dot_product_attention` | Native PyTorch attention | `(query key value mask dropout_p is_causal)` |

## 🔧 **Detailed Usage**

### 1. Simple Flash Attention
**Direct computation** without creating a module.

```lisp
; Input tensors
(setq query (torch_randn (integers 2 128 512)))     ; [batch, seq_len, embed_dim]
(setq key_tensor (torch_randn (integers 2 128 512)))
(setq value_tensor (torch_randn (integers 2 128 512)))

; Compute attention
(setq attention_output (torch_flash_attention query key_tensor value_tensor))

; Memory usage: O(N) instead of O(N²) for sequence length N=128
; Traditional attention: ~2GB for 8K sequence
; Flash attention: ~16MB working memory for same sequence
```

### 2. Flash Attention Module
**Stateful module** with learnable parameters.

```lisp
; Create module with specific configuration
(setq embed_dim 512)
(setq num_heads 8) 
(setq dropout_rate 0.1)
(setq use_bias true)

(setq flash_module (torch_flash_attention_create embed_dim num_heads dropout_rate use_bias))

; Forward pass
(setq output (torch_flash_attention_forward flash_module query key_tensor value_tensor))
```

### 3. Attention with Masking
**Masked attention** for padding tokens and causal modeling.

```lisp
; Create attention mask (1 = attend, 0 = mask)
(setq attention_mask (torch_ones (integers 2 8 128 128)))

; Apply padding mask (mask out last 10 tokens)
(setq seq_len 128)
(setq valid_len 118)
; ... mask creation logic ...

; Masked attention
(setq masked_output (torch_flash_attention_with_mask 
                     flash_module query key_tensor value_tensor attention_mask))
```

### 4. Custom Dropout
**Dynamic dropout** control during training/inference.

```lisp
; Training with dropout
(setq training_output (torch_flash_attention_with_dropout 
                       flash_module query key_tensor value_tensor 0.2 true))

; Inference without dropout  
(setq inference_output (torch_flash_attention_with_dropout 
                        flash_module query key_tensor value_tensor 0.0 false))
```

### 5. Native PyTorch Attention
**Direct access** to PyTorch's scaled dot-product attention.

```lisp
; Parameters: query, key, value, attn_mask, dropout_p, is_causal
(setq native_output (torch_scaled_dot_product_attention 
                     query key_tensor value_tensor nil 0.0 false))

; Causal attention (for autoregressive models)
(setq causal_output (torch_scaled_dot_product_attention 
                     query key_tensor value_tensor nil 0.0 true))
```

## 📊 **Memory Comparison**

### Traditional Attention
```lisp
; Memory usage for sequence length N
; Attention matrix: N × N × sizeof(float) × batch_size × num_heads
; For 8K sequence: 8192² × 4 bytes × 1 batch × 8 heads ≈ 2GB

; Example calculation:
(setq seq_len 8192)
(setq traditional_memory (* seq_len seq_len 4 1 8))  ; ≈ 2GB
```

### Flash Attention
```lisp
; Memory usage: O(N) working memory
; For 8K sequence: ~16MB working memory
; 99%+ memory reduction for long sequences

(setq flash_memory (* seq_len 64 4 1 8))  ; ≈ 16MB working memory
```

### Performance Benefits
```lisp
; Sequence length vs memory usage
; 512 tokens:   Traditional=8MB,    Flash=1MB     (8x reduction)
; 2048 tokens:  Traditional=128MB,  Flash=4MB     (32x reduction) 
; 8192 tokens:  Traditional=2GB,    Flash=16MB    (128x reduction)
; 16384 tokens: Traditional=8GB,    Flash=32MB    (256x reduction)
```

## 🎯 **Practical Examples**

### Long Document Processing
```lisp
; Process 4K token document
(setq document_tokens 4096)
(setq embed_dim 768)

; Create long sequence tensors
(setq long_query (torch_randn (integers 1 document_tokens embed_dim)))
(setq long_key (torch_randn (integers 1 document_tokens embed_dim)))
(setq long_value (torch_randn (integers 1 document_tokens embed_dim)))

; Process with Flash Attention (memory efficient)
(setq doc_attention (torch_flash_attention long_query long_key long_value))

; Memory used: ~12MB instead of ~512MB with traditional attention
```

### Multi-Head Attention Implementation
```lisp
; Simulate transformer layer with Flash Attention
(setq batch_size 2)
(setq seq_len 1024)
(setq embed_dim 512)
(setq num_heads 8)

; Create Flash Attention layer
(setq mha_layer (torch_flash_attention_create embed_dim num_heads 0.1 true))

; Input tensor
(setq input_seq (torch_randn (integers batch_size seq_len embed_dim)))

; Apply multi-head attention
(setq mha_output (torch_flash_attention_forward mha_layer input_seq input_seq input_seq))

; Add residual connection (typical in transformers)
(setq layer_output (torch_add input_seq mha_output))
```

### Causal Language Modeling
```lisp
; Autoregressive attention for language models
(setq seq_len 2048)
(setq vocab_size 50257)
(setq embed_dim 768)

; Input sequence (token embeddings)
(setq token_embeddings (torch_randn (integers 1 seq_len embed_dim)))

; Causal attention (tokens can only attend to previous tokens)
(setq causal_output (torch_scaled_dot_product_attention 
                     token_embeddings token_embeddings token_embeddings 
                     nil 0.0 true))  ; is_causal=true

; Memory usage: O(N) vs O(N²) for traditional causal attention
```

## 🔧 **Configuration Guidelines**

### Embedding Dimensions
```lisp
; Common configurations
(setq small_config (torch_flash_attention_create 256 4 0.1 true))    ; Small model
(setq base_config (torch_flash_attention_create 512 8 0.1 true))     ; Base model  
(setq large_config (torch_flash_attention_create 1024 16 0.1 true))  ; Large model
(setq xl_config (torch_flash_attention_create 2048 32 0.1 true))     ; XL model
```

### Number of Heads
```lisp
; Head dimension should typically be 64 or 128
; embed_dim = num_heads × head_dim

; Recommended configurations:
; 256 dims → 4 heads × 64 dim_per_head
; 512 dims → 8 heads × 64 dim_per_head  
; 768 dims → 12 heads × 64 dim_per_head
; 1024 dims → 16 heads × 64 dim_per_head
```

### Dropout Settings
```lisp
; Training: 0.1 - 0.3 dropout for regularization
(setq training_attn (torch_flash_attention_create 512 8 0.1 true))

; Inference: 0.0 dropout for deterministic results
(setq inference_attn (torch_flash_attention_create 512 8 0.0 true))
```

## ⚠️ **Limitations & Considerations**

### Current Limitations
- **Model Types**: Works best with standard transformer architectures
- **Tensor Format**: Expects [batch, seq_len, embed_dim] format
- **Memory**: Still requires GPU memory proportional to sequence length
- **Compatibility**: Requires PyTorch 2.0+ for optimal performance

### Best Practices
```lisp
; 1. Use appropriate tensor dimensions
(setq correct_query (torch_randn (integers batch_size seq_len embed_dim)))

; 2. Ensure dimension compatibility  
; query.shape[-1] == key.shape[-1] == value.shape[-1] == embed_dim

; 3. Use GPU for large sequences
(if (torch_cuda_is_available)
    (setq device "cuda")
    (setq device "cpu"))
```

### Performance Tips
```lisp
; 1. Batch sequences when possible
(setq batched_input (torch_randn (integers 4 512 768)))  ; Better than 4 separate calls

; 2. Use consistent sequence lengths in batches
; Avoid: batch with [512, 256, 1024, 128] sequence lengths
; Prefer: batch with [512, 512, 512, 512] sequence lengths

; 3. Consider tensor precision
; Use float16 for memory savings on compatible hardware
```

## 🚀 **Integration Examples**

### With Text Generation
```lisp
; Flash Attention in generation pipeline
(setq generator (torch_generator_create model "cuda"))
(setq input_tokens (torch_tensor (integers 15339 1332)))

; Flash attention during generation forward pass
(setq attention_output (torch_flash_attention query key_tensor value_tensor))
(setq generated_text (torch_generate generator attention_output "top_p"))
```

### With LoRA Fine-tuning
```lisp
; Combine Flash Attention with LoRA for efficient fine-tuning
(setq lora_layer (torch_lora_linear_create 512 512 16 0.1))
(setq flash_attn (torch_flash_attention_create 512 8 0.1 true))

; Forward pass combining both optimizations
(setq lora_output (torch_lora_linear_forward lora_layer input_seq))
(setq attention_output (torch_flash_attention_forward flash_attn lora_output lora_output lora_output))
```

### Complete Transformer Block
```lisp
; Full transformer block with Flash Attention
(defun transformer_block (input_seq flash_attn layer_norm1 mlp layer_norm2)
    ; Multi-head attention with Flash Attention
    (setq attn_input (torch_layer_norm_forward layer_norm1 input_seq))
    (setq attn_output (torch_flash_attention_forward flash_attn attn_input attn_input attn_input))
    (setq attn_residual (torch_add input_seq attn_output))
    
    ; Feed-forward network
    (setq mlp_input (torch_layer_norm_forward layer_norm2 attn_residual))
    (setq mlp_output (torch_mlp_forward mlp mlp_input))
    (setq final_output (torch_add attn_residual mlp_output))
    
    final_output
)
```

---

Flash Attention is now fully integrated into LispETorch, providing memory-efficient attention computation for long sequences. Use it anywhere you need attention mechanisms with reduced memory footprint!
