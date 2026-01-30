# 🎯 LispeTorch Model Loading & Positional Encoding

> **Advanced Transformer Components for Production-Ready AI**

## 📁 **Model Loading & Persistence**

### Save Models
```lisp
(torch_save_model model path)
```
- **model**: Any PyTorch module (Linear, Embedding, Attention, etc.)
- **path**: File path for saving (e.g., "model.pt")

**Supported module types:**
- `TorchLinear` - Fully connected layers
- `TorchEmbedding` - Token embedding layers  
- `TorchMultiHeadAttention` - Attention mechanisms
- `TorchLayerNorm` - Normalization layers
- `TorchModel` - Complete model architectures

**Example:**
```lisp
(setq linear (torch_linear 512 256))
(torch_save_model linear "my_layer.pt")
```

### Load Models
```lisp
(torch_load_model path model)
```
- **path**: File path to load from
- **model**: Target model (must have same architecture)

**Example:**
```lisp
(setq new_linear (torch_linear 512 256))
(torch_load_model "my_layer.pt" new_linear)
```

### State Dictionaries
```lisp
(torch_state_dict model)
```
Returns a `TorchStateDict` containing model parameters.

**Example:**
```lisp
(setq state_dict (torch_state_dict my_model))
; Returns: PyTorch State Dict (N parameters)
```

### Advanced: Load State Dict
```lisp
(torch_load_state_dict model state_dict)
```
Load parameters from one model into another.

### Training Checkpoints
```lisp
; Save training state
(torch_save_checkpoint model optimizer epoch path)

; Load checkpoint
(setq checkpoint (torch_load_checkpoint path))
```

## 🔄 **Positional Encoding**

Essential for Transformer architectures to understand sequence order.

### Create Positional Encoder
```lisp
(torch_positional_encoding embed_dim max_length)
```
- **embed_dim**: Embedding dimension (e.g., 512)
- **max_length**: Maximum sequence length (e.g., 1024)

**Example:**
```lisp
(setq pos_encoder (torch_positional_encoding 512 1024))
```

### Apply Positional Encoding
```lisp
(torch_positional_encoding_forward pos_encoder input_tensor)
```
- **pos_encoder**: Positional encoding module
- **input_tensor**: Token embeddings [batch, seq_len, embed_dim]

**Returns:** Tensor with positional information added

### Complete Transformer Pipeline
```lisp
; Configuration
(setq config (@
    (@ "vocab_size" 10000)
    (@ "embed_dim" 512)  
    (@ "num_heads" 8)
    (@ "seq_length" 128)
))

; Components
(setq tokenizer (torch_simple_tokenizer))
(setq embedding (torch_embedding (config @ "vocab_size") (config @ "embed_dim")))
(setq pos_encoding (torch_positional_encoding (config @ "embed_dim") (config @ "seq_length")))
(setq attention (torch_multihead_attention (config @ "embed_dim") (config @ "num_heads")))
(setq layer_norm (torch_layer_norm (list (config @ "embed_dim"))))

; Text processing
(setq text "The quick brown fox jumps over the lazy dog")
(setq tokens (torch_tokenizer_encode tokenizer text))
(setq padded_tokens (torch_pad_sequence (list tokens) (config @ "seq_length")))

; Forward pass
(setq embedded (torch_embedding_forward embedding padded_tokens))
(setq pos_embedded (torch_positional_encoding_forward pos_encoding embedded))
(setq attention_mask (torch_create_attention_mask padded_tokens 0))
(setq attention_out (torch_multihead_attention_forward attention pos_embedded attention_mask))
(setq output (torch_layer_norm_forward layer_norm attention_out))

; Save complete pipeline
(torch_save_model embedding "embedding.pt")
(torch_save_model pos_encoding "pos_encoding.pt") 
(torch_save_model attention "attention.pt")
(torch_save_model layer_norm "layer_norm.pt")
```

## 🎯 **Production Workflow**

### 1. Model Development
```lisp
; Build your Transformer
(setq transformer_components (list
    (torch_embedding vocab_size embed_dim)
    (torch_positional_encoding embed_dim max_seq)
    (torch_multihead_attention embed_dim num_heads)
    (torch_layer_norm (list embed_dim))
))
```

### 2. Training Loop
```lisp
; Training with checkpoints
(loop (i 0 num_epochs)
    ; ... training step ...
    
    ; Save checkpoint every 100 steps
    (if (= (% i 100) 0)
        (torch_save_checkpoint model optimizer i 
            (+ "checkpoint_" (string i) ".pt")))
)
```

### 3. Model Export
```lisp
; Save final trained model
(torch_save_model model "final_transformer.pt")

; Export individual components
(torch_save_model embedding "embedding_final.pt")
(torch_save_model attention "attention_final.pt")
```

### 4. Production Deployment
```lisp
; Load pre-trained model in production
(setq production_model (torch_embedding vocab_size embed_dim))
(torch_load_model "embedding_final.pt" production_model)

; Inference pipeline ready!
```

## 🚀 **Advanced Features**

### Multi-Module Persistence
Save and load complete architectures:
```lisp
; Complex model with multiple components
(setq full_transformer (@
    (@ "embedding" (torch_embedding 50000 768))
    (@ "pos_encoding" (torch_positional_encoding 768 2048))
    (@ "attention" (torch_multihead_attention 768 12))
    (@ "layer_norm" (torch_layer_norm (list 768)))
))

; Save all components
(torch_save_model (full_transformer @ "embedding") "transformer_embedding.pt")
(torch_save_model (full_transformer @ "pos_encoding") "transformer_pos.pt")
(torch_save_model (full_transformer @ "attention") "transformer_attention.pt")
(torch_save_model (full_transformer @ "layer_norm") "transformer_norm.pt")
```

### Memory-Efficient Loading
Load only required components:
```lisp
; Load specific layers for fine-tuning
(setq embedding_only (torch_embedding vocab_size embed_dim))
(torch_load_model "pre_trained_embedding.pt" embedding_only)

; Fine-tune just the embedding layer
```

## � **LoRA Adapter Management** (NEW!)

### LoRA Fine-tuning Integration
LoRA (Low-Rank Adaptation) provides parameter-efficient fine-tuning alongside standard model loading:

```lisp
; Create LoRA-adapted layer
(setq lora_layer (torch_lora_linear 4096 4096 16 32.0))

; Save only LoRA adapters (much smaller!)
(torch_lora_save_adapters lora_layer "task_adapters.pt")

; Save complete model with LoRA merged
(torch_lora_merge_weights lora_layer)
(torch_save_model lora_layer "merged_model.pt")
```

### Adapter Switching Workflow
```lisp
; Base model loading
(setq base_model (torch_linear 4096 4096))
(torch_load_model "pretrained_base.pt" base_model)

; Convert to LoRA for task adaptation
(setq lora_adapted (torch_lora_apply_to_linear base_model 16 32.0))

; Load task-specific adapters
(torch_lora_load_adapters lora_adapted "summarization_adapters.pt")

; Switch to different task
(torch_lora_load_adapters lora_adapted "translation_adapters.pt")
```

### Production Deployment Options

**Option 1: Merged Weights (Optimal Performance)**
```lisp
; During training
(setq lora_model (torch_lora_linear 4096 4096 16 32.0))
; ... training ...

; For deployment: merge weights for zero overhead
(torch_lora_merge_weights lora_model)
(torch_save_model lora_model "production_model.pt")

; Load in production (no LoRA overhead)
(setq prod_model (torch_linear 4096 4096))
(torch_load_model "production_model.pt" prod_model)
```

**Option 2: Dynamic Adapter Loading**
```lisp
; Production server with multiple tasks
(setq base_model (torch_linear 4096 4096))
(torch_load_model "base_model.pt" base_model)

(setq lora_model (torch_lora_apply_to_linear base_model 16 32.0))

; Runtime task switching
(defun switch_task (task_name)
    (torch_lora_load_adapters lora_model 
        (+ task_name "_adapters.pt")))

(switch_task "summarization")  ; Load summarization adapters
(switch_task "translation")    ; Switch to translation adapters
```

### Storage Efficiency Benefits
- **Base model**: 7B parameters (28GB)
- **LoRA adapters**: ~8M parameters (32MB) 
- **Storage savings**: 99%+ reduction per task
- **Multiple tasks**: Share base model, different adapters

## �🔬 **Technical Implementation**

### Positional Encoding Algorithm
- **Sinusoidal patterns**: `sin(pos/10000^(2i/d))` and `cos(pos/10000^(2i/d))`
- **Learnable**: No additional parameters needed
- **Position-aware**: Each position gets unique encoding
- **Scalable**: Works with variable sequence lengths

### File Format Compatibility
- **PyTorch native**: `.pt` files work with standard PyTorch
- **Cross-platform**: Compatible across macOS, Linux, Windows
- **Version stable**: Forward and backward compatible
- **Compression**: Automatic optimization for file size

## ✅ **Testing & Validation**

Run comprehensive tests:
```bash
../bin/lispe test_model_loading.lisp     # Model persistence tests
../bin/lispe test_pe_minimal.lisp        # Positional encoding tests  
../bin/lispe test_lora_complet.lisp      # LoRA fine-tuning tests (NEW!)
../bin/lispe transformer_complet.lisp    # Complete pipeline validation
```

Expected output:
```
✅ Model Save: Successfully saved to file
✅ Model Load: Successfully loaded from file  
✅ State Dict: 2 parameters extracted
✅ Positional Encoding: Sinusoidal patterns applied
✅ LoRA Fine-tuning: Parameter-efficient adaptation ready
✅ Complete Pipeline: End-to-end Transformer ready
```

---

**🎉 LispeTorch is now production-ready for Transformer architectures, LoRA fine-tuning, and Llama-3.1 deployment!**
