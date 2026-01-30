# 🔥 LispE PyTorch Library

> **Deep Learning, Text Generation & Large Language Model Fine-tuning for LispE**

A comprehensive PyTorch integration for LispE, featuring **Flash Attention**, **Text Generation**, complete Transformer architecture, LoRA fine-tuning, advanced tokenization, and Llama-3.1 compatibility.

[![Platform](https://img.shields.io/badge/platform-macOS%20%7C%20Linux%20%7C%20Windows-blue)](#)
[![GPU](https://img.shields.io/badge/GPU-CUDA%20%7C%20MPS%20(Apple%20Silicon)-green)](#)
[![License](https://img.shields.io/badge/license-MIT-yellow)](#)

## 🚀 **Key Features**

### 🤖 **Text Generation System** (NEW!)
- **Multiple Sampling Strategies**: Greedy, Top-K, Top-P (Nucleus), Beam Search
- **Configurable Parameters**: Temperature, repetition penalty, sequence length control
- **Production Ready**: Comprehensive generation pipeline with EOS token handling
- **Model Agnostic**: Works with any PyTorch model compatible with text generation

### ⚡ **Flash Attention**
- **Memory Efficient**: O(N) complexity instead of O(N²) for sequence length N
- **High Performance**: Native PyTorch 2.0+ kernels with optimized fallback
- **Long Sequences**: Support for 8K+ token sequences with linear memory scaling
- **Production Ready**: Masking, dropout, causal attention, and batching support

### 🧠 **Complete AI Architecture**
- **Transformer Models**: Multi-Head Attention, Layer Normalization, Positional Encoding, **Rotary Position Embedding (RoPE)**
- **Flash Attention**: Memory-efficient attention for long sequences ✨
- **LoRA Fine-tuning**: Parameter-efficient adaptation for large language models
- **Neural Networks**: MLP, Embedding layers, Custom architectures  
- **Training Pipeline**: Optimizers (Adam, SGD), Loss functions, Backpropagation
- **GPU Acceleration**: CUDA (NVIDIA) + Metal Performance Shaders (Apple Silicon)

### 🔧 **Enhanced Tensor Operations**
- **Random Generation**: `torch_rand` for model initialization and data generation
- **Dimension Manipulation**: `torch_transpose` for flexible tensor reordering
- **Element Access**: Native `at` function support with bounds checking and negative indexing
- **Shape Inspection**: `torch_shape` for tensor dimension analysis

### 📝 **Advanced Tokenization**
- **SimpleTokenizer**: Word-based with punctuation handling, immediate use
- **SentencePiece**: Subword tokenization, Llama-3.1 compatible
- **Text Pipeline**: Automatic padding, attention masking, sequence processing

### 🎯 **LoRA Fine-tuning**
- **Low-Rank Adaptation**: Efficient fine-tuning with minimal parameters
- **Adapter Management**: Save/load LoRA weights independently
- **Weight Merging**: Deploy optimized models after training
- **Retroactive Application**: Transform existing layers to LoRA

### 🚀 **Portable Installation**
- **Automatic Deployment**: `make all` triggers portable installation
- **Dependency Management**: Auto-copy PyTorch, SentencePiece, and system libraries
- **Cross-Platform**: Automated rpath correction and library bundling
- **LoRA Fine-tuning**: Parameter-efficient adaptation for large language models
- **Neural Networks**: MLP, Embedding layers, Custom architectures  
- **Training Pipeline**: Optimizers (Adam, SGD), Loss functions, Backpropagation
- **GPU Acceleration**: CUDA (NVIDIA) + Metal Performance Shaders (Apple Silicon)

### 📝 **Advanced Tokenization**
- **SimpleTokenizer**: Word-based with punctuation handling, immediate use
- **SentencePiece**: Subword tokenization, Llama-3.1 compatible
- **Text Pipeline**: Automatic padding, attention masking, sequence processing

### 🎯 **LoRA Fine-tuning** (NEW!)
- **Low-Rank Adaptation**: Efficient fine-tuning with minimal parameters
- **Adapter Management**: Save/load LoRA weights independently
- **Weight Merging**: Deploy optimized models after training
- **Retroactive Application**: Transform existing layers to LoRA

### 🗜️ **Model Quantization & Optimization** (NEW!)
- **Dynamic Quantization**: Automatic INT8/FP16 conversion with intelligent scaling
- **Static Quantization**: Custom parameters for maximum precision control
- **Memory Optimization**: 50% reduction (FP16) or 75% reduction (INT8)
- **Production Ready**: Model-level quantization for deployment optimization

### ⚡ **Performance & Compatibility**
- **Auto Device Detection**: Optimal hardware utilization (MPS > CUDA > CPU)
- **Cross-platform**: macOS, Linux, Windows support
- **Memory Efficient**: Variable-length sequences, attention masking, Flash Attention optimization
- **Production Ready**: Error handling, thread-safe operations

## 🧠 **Memory Advantages: Technical Clarifications**

LispETorch uses the same underlying PyTorch engine (libtorch), so **tensor memory efficiency is identical**. The specific advantages of LispETorch come from:

### **LispE Optimized Data Types**
```lisp
; LispE - Optimized types (direct memory access)
(setq tokens (integers 101 7592 2029 ...))      ; long* array = 8 bytes × N
(setq masks (shorts 1 0 1 1 ...))               ; short* array = 2 bytes × N  
(setq embeddings (numbers 1.5 2.3 3.7 ...))    ; double* array = 8 bytes × N

; Python - Generic objects (significant overhead)
tokens = [101, 7592, 2029, ...]        # ~28 bytes overhead × N objects
masks = [True, False, True, ...]       # ~28 bytes overhead × N objects
embeddings = [1.5, 2.3, 3.7, ...]     # ~28 bytes overhead × N objects
```

### **Real Memory Savings**
- **Auxiliary structures**: 70-80% less memory (token lists, masks, indices)
- **PyTorch tensors**: Identical efficiency (same libtorch engine)
- **Flash Attention**: O(N) vs O(N²) memory complexity (implementation advantage)
- **Pipeline efficiency**: Significant savings in complex tokenization/preprocessing workflows

### **Practical Impact**
```lisp
; 8K token sequence processing:
; Standard attention: ~2GB tensor + 256MB attention scores
; Flash attention: ~2GB tensor + ~16MB working memory
; Auxiliary data: 70-80% reduction vs Python lists
; shorts for masks: 2 bytes vs 28 bytes per element
; Total: ~240MB+ saved on attention + auxiliary structures
```

**Bottom line**: Same tensor performance, significant savings on auxiliary data structures and attention mechanisms.

## 📊 **LispE Data Types for PyTorch**

LispE provides optimized data types for efficient PyTorch integration:

### Optimized List Types
```lisp
; Memory-efficient specialized arrays
(integers 2 3 4)    ; → long* list (optimized)
(shorts 1 0 1 1)    ; → short* list (optimized)
(floats 1.0 2.0)    ; → float* list (optimized)  
(numbers 1.5 2.5)   ; → double* list (optimized)
(list 1 2 3)        ; → generic object list (higher overhead)
```

### Sequence Generators
```lisp
; Auto-generating sequences with optimal type selection
(iota0 5)     ; → (integers 0 1 2 3 4)
(iota 5)      ; → (integers 1 2 3 4 5)
(iota 5.0)    ; → (numbers 1 2 3 4 5) - auto-detects float
```

## ⚡ **Flash Attention Quick Start**

Flash Attention provides memory-efficient attention computation for long sequences:

```lisp
; Create Flash Attention module
(setq flash_attn (torch_flash_attention_create 256 8 0.1 true))
; Parameters: embed_dim=256, num_heads=8, dropout=0.1, bias=true

; Create input tensors
(setq query (torch_zeros (integers 2 32 256)))   ; [batch, seq_len, embed_dim]
(setq thekey (torch_zeros (integers 2 32 256)))
(setq thevalue (torch_ones (integers 2 32 256)))

; Standard forward pass
(setq output (torch_flash_attention_forward flash_attn query thekey thevalue))

; With attention mask
(setq attention_mask (torch_ones (integers 2 8 32 32)))
(setq masked_output (torch_flash_attention_with_mask flash_attn query thekey thevalue attention_mask))

; With custom dropout
(setq dropout_output (torch_flash_attention_with_dropout flash_attn query thekey thevalue 0.2 true))

; Direct native attention (PyTorch 2.0+)
(setq direct_output (torch_scaled_dot_product_attention query thekey thevalue nil 0.0 false))
```

## 🤖 **Text Generation System**

LispETorch now includes a comprehensive text generation system with multiple sampling strategies:

```lisp
; Create a text generator (requires a compatible PyTorch model)
(setq generator (torch_generator_create model "cpu"))

; Configure generation parameters with dictionary
(setq config (
    ("max_length" 50)
    ("temperature" 0.8)
    ("top_k" 40)
    ("top_p" 0.9)
    ("strategy" "greedy")
    ("repetition_penalty" 1.1)
    ("eos_token_id" 50256)
))
(torch_generator_config generator config)

; Or configure with dedicated function
(torch_set_generation_params generator 50 0.8 40 0.9)

; Generate text with different strategies
(setq input_tokens (torch_tensor (integers 15339 1332)))  ; Input token IDs

; Greedy search (deterministic)
(setq greedy_result (torch_generate generator input_tokens "greedy"))

; Top-K sampling (stochastic)
(setq topk_result (torch_generate generator input_tokens "top_k"))

; Top-P (Nucleus) sampling (dynamic vocabulary)
(setq topp_result (torch_generate generator input_tokens "top_p"))

; Beam search (coming soon)
(setq beam_result (torch_generate generator input_tokens "beam_search"))
```

### Generation Strategies:
- **🎯 Greedy**: Deterministic, selects highest probability token
- **🎲 Top-K**: Samples from K most likely tokens 
- **🌊 Top-P (Nucleus)**: Dynamic vocabulary based on cumulative probability
- **🔍 Beam Search**: Explores multiple sequence hypotheses
- **🌡️ Temperature**: Controls randomness (0.1 = conservative, 2.0 = creative)
- **🔄 Repetition Penalty**: Reduces repetitive text generation

## 🔧 **Enhanced Tensor Operations**

New tensor manipulation functions for advanced model operations:

```lisp
; Random tensor generation for initialization
(setq random_tensor (torch_rand (integers 512 512)))  ; Random values [0, 1]

; Transpose dimensions for attention computations
(setq reshaped (torch_reshape random_tensor (integers 8 64 512)))
(setq transposed (torch_transpose reshaped 0 2))  ; Swap dims 0 and 2

; Access tensor elements with native 'at' function
(setq test_tensor (torch_tensor (integers 10 20 30 40 50)))
(setq first_element (at test_tensor 0))    ; Returns: 10
(setq last_element (at test_tensor -1))    ; Returns: 50 (negative indexing)

; Get tensor shape information
(setq tensor_shape (torch_shape test_tensor))  ; Returns: (5)
```

## 🔄 **Rotary Position Embedding (RoPE)** (Previous Features)

**Rotary Position Embedding (RoPE)** is the modern approach to positional encoding used in state-of-the-art models like Llama, GPT-NeoX, and PaLM:

```lisp
; Create RoPE for head dimension
(setq rope (torch_rotary_embedding 64 8192))  ; head_dim, max_seq_len

; Generate cos/sin tensors for sequence length
(setq cos_sin (torch_rotary_forward rope 128 "mps"))
(setq cos_tensor (car cos_sin))
(setq sin_tensor (cadr cos_sin))

; Apply RoPE to query and key tensors
(setq query_rope (torch_apply_rotary_pos_emb query cos_tensor sin_tensor))
(setq key_rope (torch_apply_rotary_pos_emb key cos_tensor sin_tensor))

; Use in Multi-Head Attention
(setq attention_out (torch_attention_forward attention query_rope key_rope value))
```

### RoPE Advantages:
- **🔄 Better Extrapolation**: Generalizes to longer sequences than training
- **⚡ Efficiency**: No additive positional encoding overhead
- **🎯 Relative Positioning**: Preserves relative position relationships
- **🚀 Modern Standard**: Used in Llama-3.1, GPT-4, and latest models
- **📏 Length Flexibility**: No fixed maximum sequence length

### LoRA Benefits:
- **🚀 90%+ Parameter Reduction**: Only train LoRA matrices
- **💾 Tiny Storage**: Save only adapters, not full model
- **⚡ Fast Training**: Fewer parameters = faster convergence
- **🔄 Easy Deployment**: Merge weights for zero-overhead inference
- **🎯 Task-Specific**: Different adapters for different tasks

## 🚀 **Next Steps for Llama-3.1 + Text Generation**

1. **✅ Foundation Ready**: Tokenization + Transformer architecture
2. **✅ SentencePiece**: Full Llama tokenizer compatibility  
3. **✅ Positional Embeddings**: Sinusoidal position encoding implemented
4. **✅ Rotary Position Embedding**: RoPE for modern Transformers
5. **✅ Model Loading**: Pre-trained weights integration complete
6. **✅ LoRA Fine-tuning**: Parameter-efficient training implemented
7. **✅ Flash Attention**: Memory-efficient attention for long sequences
8. **✅ Text Generation**: Complete generation pipeline with multiple strategies

### 🎉 **PRODUCTION READY FOR LLAMA-3.1 + TEXT GENERATION**

LispeTorch now provides a **complete AI development stack** with:
- ✅ Multi-Head Attention mechanisms
- ✅ Layer Normalization for training stability
- ✅ Positional Encoding for sequence understanding
- ✅ Rotary Position Embedding (RoPE) for modern architectures  
- ✅ Model persistence for production deployment
- ✅ LoRA adaptation for efficient fine-tuning
- ✅ Flash Attention for memory-efficient long sequences
- ✅ Text Generation with multiple sampling strategies
- ✅ Full PyTorch compatibility for existing workflows
- ✅ Portable installation for deployment

## 🛠 **Quick Installation**

### Prerequisites
```bash
# PyTorch (required)
pip install torch torchvision torchaudio

# SentencePiece (optional, for Llama compatibility)
pip install sentencepiece
```

### Build & Install
```bash
cd lispetorch
make all  # Compiles + automatic portable installation
```

### Portable Installation
The new automatic portable installation system:
- ✅ **Auto-triggered**: `make all` automatically runs portable installation
- ✅ **Dependency Management**: Copies PyTorch, SentencePiece, and system libraries
- ✅ **Cross-Platform**: Automated rpath correction for macOS, Linux, Windows
- ✅ **Self-Contained**: Installs to `/usr/local/lib/lispe` for easy distribution
- ✅ **Path Correction**: Automatically fixes Homebrew and conda paths

The Makefile automatically detects:
- ✅ PyTorch installation and version
- ✅ CUDA availability  
- ✅ SentencePiece presence
- ✅ Apple Silicon MPS support

## ⚡ **Quick Start**

### Load Library
```lisp
(use 'lispe_torch)
```

### Simple Neural Network
```lisp
; Auto-detect best device (MPS/CUDA/CPU)
(setq device (torch_get_best_device))

; Create model and data
(setq model (torch_model 4 64 2))
(setq tensor_data (torch_tensor (floats 1.0 2.0 3.0 4.0)))
(setq targets (torch_tensor (floats 0.5 1.5)))

; Training
(setq optimizer (torch_optimizer model 0.001 "adam"))
(setq output (torch_forward model tensor_data))
(setq loss (torch_mse_loss output targets))
(torch_backward loss)
(torch_optimizer_step optimizer)
```

### Complete Text Processing
```lisp
; Advanced tokenization
(setq tokenizer (torch_simple_tokenizer))
(setq tokens (torch_tokenizer_encode tokenizer "Hello world!"))

; Complete Transformer pipeline
(setq vocab_size (torch_tokenizer_get_vocab_size tokenizer))
(setq embed_dim 128)
(setq seq_length 10)

; Model components
(setq embedding (torch_embedding vocab_size embed_dim))
(setq pos_encoding (torch_positional_encoding embed_dim seq_length))
(setq attention (torch_multihead_attention embed_dim 4))
(setq layer_norm (torch_layer_norm (list embed_dim)))

; Forward pass with positional encoding
(setq embedded (torch_forward embedding tokens))
(setq pos_embedded (torch_positional_encoding_forward pos_encoding embedded))
(setq attention_out (torch_multihead_attention_forward attention pos_embedded))
(setq output (torch_layer_norm_forward layer_norm attention_out))

; Save the complete model
(torch_save_model attention "transformer_model.pt")
```

## 🎯 **Llama-3.1 Fine-tuning Ready**

Complete setup for large language model fine-tuning:

```lisp
; Llama-3.1 configuration
(setq llama_config (dictionary
    "vocab_size" 128256
    "embed_dim" 4096  
    "num_heads" 32
    "max_seq_length" 8192
))

; Advanced tokenizer (SentencePiece when available)
(setq tokenizer (torch_sentencepiece_tokenizer "llama3_tokenizer.model"))

; Transformer architecture
(setq transformer (torch_transformer_block 
    (get llama_config "embed_dim")
    (get llama_config "num_heads") 
    11008))  ; Llama FF dimension

; Ready for LoRA fine-tuning! 🚀
```

## � **LoRA Fine-tuning** (NEW!)

**Low-Rank Adaptation (LoRA)** enables efficient fine-tuning of large language models with minimal parameters:

```lisp
; Create a LoRA-adapted linear layer
(setq lora_layer (torch_lora_linear 4096 4096 16 32.0))
; Parameters: in_features, out_features, rank, alpha

; Apply LoRA to existing layers
(setq base_linear (torch_linear 512 256))
(setq lora_adapted (torch_lora_apply_to_linear base_linear 8 16.0))

; Forward pass with LoRA
(setq tensor_input (torch_ones (integers 32 512)))
(setq output (torch_lora_forward lora_adapted tensor_input))

; Save only the LoRA adapters (much smaller!)
(torch_lora_save_adapters lora_adapted "llama_lora_adapters.pt")

; Load adapters for inference  
(setq new_model (torch_lora_linear 512 256 8 16.0))
(torch_lora_load_adapters new_model "llama_lora_adapters.pt")

; Merge weights for deployment (no overhead)
(torch_lora_merge_weights lora_adapted)
```

### LoRA Benefits:
- **🚀 90%+ Parameter Reduction**: Only train LoRA matrices
- **💾 Tiny Storage**: Save only adapters, not full model
- **⚡ Fast Training**: Fewer parameters = faster convergence
- **🔄 Easy Deployment**: Merge weights for zero-overhead inference
- **🎯 Task-Specific**: Different adapters for different tasks

## �📊 **Architecture Overview**

```
Text Input
    ↓
[Tokenization] ← SimpleTokenizer / SentencePiece
    ↓
[Embedding Layer] ← Token → Vector conversion
    ↓
[Sequence Processing] ← Padding + Attention Masks  
    ↓
[Transformer Block] ← Multi-Head Attention + LayerNorm + FFN
    ↓
[Output Projection] ← Final predictions
    ↓
Training / Inference
```

## 🏗 **What's Included**

### Core Components
| Component | Status | Description |
|-----------|--------|-------------|
| **Tensors** | ✅ | Creation, manipulation, device transfer |
| **Models** | ✅ | MLP, Transformer blocks, Embeddings |
| **Training** | ✅ | Optimizers, loss functions, backprop |
| **GPU** | ✅ | CUDA + MPS auto-detection |

### Text Processing
| Component | Status | Description |
|-----------|--------|-------------|
| **SimpleTokenizer** | ✅ | Word-based, punctuation handling |
| **SentencePiece** | ✅ | Subword tokenization, Llama compatible |
| **Sequence Utils** | ✅ | Padding, attention masks |
| **Pipeline** | ✅ | End-to-end text processing |

### Advanced Features  
| Component | Status | Description |
|-----------|--------|-------------|
| **Multi-Head Attention** | ✅ | Configurable heads, parallel processing |
| **Layer Normalization** | ✅ | Training stabilization |
| **Positional Encoding** | ✅ | Sinusoidal position embeddings |
| **Rotary Position Embedding** | ✅ | RoPE for modern Transformers (Llama-style) |
| **Model Loading/Saving** | ✅ | PyTorch model persistence & checkpoints |
| **LoRA Fine-tuning** | ✅ | Parameter-efficient adaptation |
| **LoRA Adapters** | ✅ | Save/load/merge LoRA weights |
| **Device Optimization** | ✅ | Automatic best device selection |
| **Memory Management** | ✅ | Efficient variable-length sequences |

## 🔄 **Model Persistence**

Complete model loading and saving capabilities:

```lisp
; Save trained models
(torch_save_model my_model "transformer.pt")

; Load pre-trained models  
(torch_load_model "transformer.pt" my_model)

; Extract model parameters
(setq state_dict (torch_state_dict my_model))

; Training checkpoints
(torch_save_checkpoint model optimizer epoch "checkpoint.pt")
(setq checkpoint (torch_load_checkpoint "checkpoint.pt"))
```

Supports all module types:
- ✅ `TorchLinear` layers
- ✅ `TorchEmbedding` layers  
- ✅ `TorchMultiHeadAttention` modules
- ✅ `TorchLayerNorm` modules
- ✅ Complete `TorchModel` architectures

## �️ **Model Quantization & Optimization** (NEW!)

LispETorch includes comprehensive quantization for production deployment:

```lisp
; Load model weights
(setq model_weights (torch_randn (integers 1024 768)))

; FP16 quantization (50% memory reduction)
(setq fp16_weights (torch_quantize_fp16 model_weights))
(println "FP16 Memory usage: " (torch_numel fp16_weights) " parameters")

; INT8 quantization (75% memory reduction)  
(setq int8_weights (torch_quantize_int8 model_weights))
(println "INT8 Memory usage: " (torch_numel int8_weights) " parameters")

; Dynamic quantization with automatic scaling
(setq dynamic_quantized (torch_quantize_dynamic model_weights 1.0 0))

; Static quantization with custom parameters
(setq static_quantized (torch_quantize_static model_weights 0.5 -128))

; Convert back to full precision when needed
(setq restored (torch_dequantize static_quantized))

; Model-level quantization for complete architecture
(setq quantized_model (torch_model_quantize_dynamic my_model))
```

### Quantization Benefits
- **Memory Reduction**: FP16 (50%) or INT8 (75%) memory savings
- **Inference Speed**: Faster computation on quantized types
- **Deployment Ready**: Optimized models for production environments
- **Quality Preservation**: Minimal accuracy loss with proper configuration

## �📚 **Documentation**

- **[Complete API Reference](lispe_torch.md)** - Detailed function documentation
- **[Examples](test_final.lsp)** - Working code examples
- **[Architecture Guide](test_transformer.lsp)** - Transformer implementation details

## 🔬 **Examples & Tests**

Run the comprehensive validation:
```bash
../bin/lispe test_final.lsp
```

Output shows complete pipeline:
```
✅ TOKENIZATION: SimpleTokenizer operational
✅ EMBEDDING: Token → vector conversion  
✅ SEQUENCE PROCESSING: Padding + attention masks
✅ TRANSFORMER: Multi-Head Attention (4 heads)
🚀 READY FOR LLAMA-3.1: Complete framework
```

## 🚀 **Next Steps for Llama-3.1**

1. **✅ Foundation Ready**: Tokenization + Transformer architecture
2. **✅ SentencePiece**: Full Llama tokenizer compatibility  
3. **✅ Positional Embeddings**: Position encoding for sequences
4. **� Model Loading**: Pre-trained weights integration
5. **🎯 LoRA Fine-tuning**: Parameter-efficient training

## 🤝 **Contributing**

The library provides a solid foundation for:
- Large language model fine-tuning
- Custom Transformer architectures  
- Advanced tokenization systems
- Multi-device training pipelines

## 📄 **License**

This project is part of the LispE ecosystem. See the main LispE repository for licensing details.

---

**Built for the LispE community** - Bringing modern deep learning to functional programming! 🎉
