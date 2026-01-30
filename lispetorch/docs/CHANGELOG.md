# 📋 LispeTorch Changelog

## v2.3.0 (2025-09-08) - Flash Attention + Enhanced Tensors + Native 'at' Support

### ⚡ NEW: Flash Attention System
- **torch_flash_attention_create**: Create Flash Attention modules with custom parameters
- **torch_flash_attention_forward**: Memory-efficient O(N) attention computation
- **torch_flash_attention_with_mask**: Custom attention masking for variable sequences
- **torch_flash_attention_with_dropout**: Training-phase dropout control
- **torch_scaled_dot_product_attention**: Direct PyTorch 2.0+ attention kernels
- **Memory Optimization**: Process 8K+ token sequences with linear memory scaling

### 🔧 NEW: Enhanced Tensor Operations  
- **torch_rand**: Random tensor generation for weight initialization
- **torch_transpose**: Dimension transposition for attention computations
- **Native 'at' function**: Element access with bounds checking and negative indexing

### 📊 NEW: LispE Data Types Documentation
- **Optimized List Types**: `integers`, `shorts`, `floats`, `numbers` for memory-efficient arrays
- **Sequence Generators**: `iota0`, `iota` with automatic type selection
- **Performance Benefits**: Direct memory access without object wrapper overhead
- **Integration Examples**: Practical usage patterns for PyTorch tensor operations

### 📚 Documentation Enhancements
- Complete Flash Attention API reference with examples
- Enhanced tensor operations guide with performance notes  
- LispE data type optimization explanations
- Updated examples demonstrating all new features
- Cross-reference updates across all documentation files

### 🎯 Technical Improvements
- Protected index method for safe tensor element access
- Boolean type handling optimization in LispE integration
- Optional parameter handling with c10::nullopt for PyTorch compatibility
- Memory-efficient attention masking and dropout implementations

---

## 🎉 Version 2.2.0 - LoRA Fine-tuning (September 2025)

### 🆕 **Major New Features**

#### **LoRA Fine-tuning** (NEW!)
- ✅ **`torch_lora_linear(in_features out_features rank alpha)`** - Create LoRA-adapted linear layers
- ✅ **`torch_lora_forward(lora_layer input)`** - Forward pass through LoRA layers
- ✅ **`torch_lora_apply_to_linear(linear_layer rank alpha)`** - Convert existing layers to LoRA
- ✅ **`torch_lora_merge_weights(lora_layer)`** - Merge LoRA weights for deployment
- ✅ **`torch_lora_save_adapters(lora_layer path)`** - Save only adapter weights
- ✅ **`torch_lora_load_adapters(lora_layer path)`** - Load adapter weights
- ✅ **`torch_lora_trainable_params(model)`** - Get trainable parameters

#### **LoRA Features**
- **Parameter Efficiency**: Train only 0.1-10% of original parameters
- **Storage Efficiency**: Save adapter weights separately (~1% of model size)
- **Weight Merging**: Zero-overhead deployment after training
- **Retroactive Application**: Convert pre-trained layers to LoRA
- **Rank Control**: Configurable adaptation capacity (rank 4-64)
- **Alpha Scaling**: Fine-tuned control over adaptation strength

#### **New Data Types**
- **`TorchLoRALinear`** - LoRA-adapted linear layer modules
- **`TorchLoRAConfig`** - LoRA configuration containers

### 🔧 **Technical Improvements**

#### **Low-Rank Matrix Decomposition**
- **Matrix A**: Input-to-rank transformation with Gaussian initialization
- **Matrix B**: Rank-to-output transformation with zero initialization
- **Scaling Factor**: Alpha/rank ratio for optimal adaptation strength
- **Weight Freezing**: Original parameters remain frozen during training

#### **Production-Ready Pipeline**
- **Training Phase**: Use LoRA layers with separate adapter weights
- **Deployment Phase**: Merge weights for optimal inference performance
- **Multi-Task Support**: Different adapters for different tasks
- **Memory Efficient**: Minimal overhead during training and inference

#### **Cross-Platform Compatibility**
- **macOS**: Full MPS acceleration for LoRA operations
- **Linux**: CUDA support for large-scale fine-tuning
- **Windows**: CPU and CUDA compatibility
- **File format**: Standard PyTorch serialization for adapters

### 🎯 **Llama-3.1 Ready**
- **Complete fine-tuning stack** for large language models
- **Parameter-efficient adaptation** for 7B, 13B, 70B models
- **Production deployment** with weight merging
- **Task specialization** with adapter switching

---

## 🎉 Version 2.1.0 - Model Loading & Positional Encoding (September 2025)

### 🆕 **Major New Features**

#### **Model Loading & Persistence**
- ✅ **`torch_save_model(model path)`** - Save PyTorch modules to disk
- ✅ **`torch_load_model(path model)`** - Load pre-trained models  
- ✅ **`torch_state_dict(model)`** - Extract model parameters
- ✅ **`torch_load_state_dict(model state_dict)`** - Load parameters between models
- ✅ **`torch_save_checkpoint(model optimizer epoch path)`** - Training checkpoints
- ✅ **`torch_load_checkpoint(path)`** - Resume training from checkpoints

**Multi-Module Support:**
- `TorchLinear` layers
- `TorchEmbedding` layers
- `TorchMultiHeadAttention` modules
- `TorchLayerNorm` modules
- Complete `TorchModel` architectures

#### **Positional Encoding**
- ✅ **`torch_positional_encoding(embed_dim max_length)`** - Create sinusoidal position encoders
- ✅ **`torch_positional_encoding_forward(pos_encoder input)`** - Apply positional information
- ✅ **Sinusoidal patterns** - Standard Transformer position encoding (Vaswani et al.)
- ✅ **Variable sequence length** - Support for different input sizes
- ✅ **GPU compatibility** - Works with CUDA and MPS acceleration

#### **New Data Types**
- **`TorchPositionalEncoding`** - Positional encoding modules
- **`TorchStateDict`** - Model parameter containers

### 🔧 **Technical Improvements**

#### **Enhanced Error Handling**
- Better error messages for model loading/saving operations
- Type validation for all new functions
- File path validation and error reporting

#### **Memory Efficiency**
- Optimized parameter copying in state dictionaries
- Efficient checkpoint serialization
- Minimal memory overhead for positional encoding

#### **Cross-Platform Compatibility**
- **macOS**: Full support with Apple Silicon MPS
- **Linux**: CUDA acceleration 
- **Windows**: CPU and CUDA support
- **File format**: Standard PyTorch `.pt` files

### 📚 **Documentation Updates**

#### **New Documentation Files**
- **`model_loading_guide.md`** - Comprehensive model persistence guide
- **Updated `README.md`** - Complete feature overview
- **Updated `lispe_torch.md`** - Detailed API documentation

#### **Enhanced Examples**
- Complete Transformer pipeline with model saving
- Production deployment workflow
- Training checkpoint management
- Multi-component model architectures

### 🧪 **Testing & Validation**

#### **New Test Suites**
- **`test_model_loading.lisp`** - Model persistence validation
- **`test_pe_minimal.lisp`** - Positional encoding tests
- **`transformer_complet.lisp`** - End-to-end pipeline tests

#### **Validated Workflows**
- ✅ Save and load linear layers
- ✅ Save and load embedding layers
- ✅ Save and load attention mechanisms
- ✅ Save and load layer normalization
- ✅ Complete Transformer pipeline with positional encoding
- ✅ Multi-component model persistence

### 🚀 **Production Readiness**

#### **Complete Transformer Architecture**
LispeTorch now provides a **fully functional Transformer implementation**:

1. **Tokenization** - SimpleTokenizer + SentencePiece
2. **Embedding** - Token to vector conversion
3. **Positional Encoding** - Sequence position information ✅ **NEW**
4. **Multi-Head Attention** - Parallel attention mechanisms
5. **Layer Normalization** - Training stabilization
6. **Feed-Forward Networks** - Non-linear transformations
7. **Model Persistence** - Save/load capabilities ✅ **NEW**

#### **Llama-3.1 Compatibility**
- ✅ Complete architecture support
- ✅ SentencePiece tokenization
- ✅ Positional encoding implementation
- ✅ Model loading for pre-trained weights
- 🎯 **Ready for LoRA fine-tuning** (next phase)

### 🔄 **Migration Guide**

#### **New Function Signatures**
```lisp
; Model persistence (NEW)
(torch_save_model model "path/to/model.pt")
(torch_load_model "path/to/model.pt" model)

; State dictionaries (NEW)
(setq state_dict (torch_state_dict model))
(torch_load_state_dict target_model state_dict)

; Positional encoding (NEW)
(setq pos_encoder (torch_positional_encoding embed_dim max_length))
(setq pos_embedded (torch_positional_encoding_forward pos_encoder embedded))

; Training checkpoints (NEW)
(torch_save_checkpoint model optimizer epoch "checkpoint.pt")
(setq checkpoint (torch_load_checkpoint "checkpoint.pt"))
```

#### **Updated Pipeline**
```lisp
; Before (v2.0)
(setq embedded (torch_embedding_forward embedding tokens))
(setq output (torch_multihead_attention_forward attention embedded))

; After (v2.1) - with positional encoding
(setq embedded (torch_embedding_forward embedding tokens))
(setq pos_embedded (torch_positional_encoding_forward pos_encoder embedded))
(setq output (torch_multihead_attention_forward attention pos_embedded))

; Save trained model
(torch_save_model attention "trained_model.pt")
```

### 🐛 **Bug Fixes**
- Fixed string conversion issues in model path handling
- Corrected parameter ordering in `torch_load_model`
- Fixed LispE comment syntax (single `;` instead of `;;`)
- Improved tensor type support for integer tensors

### ⚡ **Performance**
- Optimized model serialization speed
- Reduced memory usage in state dictionary operations
- Faster positional encoding computation
- Improved GPU memory management

---

## 📈 Previous Versions

### Version 2.0.0 - Transformer Architecture (August 2025)
- Multi-Head Attention implementation
- Layer Normalization support
- Embedding layers
- Transformer block architecture
- Advanced tokenization (SimpleTokenizer + SentencePiece)
- GPU acceleration (CUDA + MPS)

### Version 1.0.0 - Core PyTorch Integration (July 2025)
- Basic tensor operations
- Neural network models (MLP)
- Optimizers (Adam, SGD)
- Loss functions (MSE, Cross-entropy)
- Training utilities
- Device management

---

## 🔮 **Upcoming Features (v2.2.0)**

### 🎯 **LoRA Fine-tuning**
- Parameter-efficient fine-tuning implementation
- Low-rank adaptation for large models
- Memory-efficient training for Llama-3.1

### 🚀 **Advanced Model Architectures**
- Complete Llama architecture implementation
- Rotary positional embeddings (RoPE)
- Layer-wise learning rate optimization

### 📊 **Training Enhancements**
- Gradient accumulation
- Mixed precision training (FP16/BF16)
- Dynamic batching for variable-length sequences

### 🔧 **Production Tools**
- Model quantization (INT8/INT4)
- ONNX export capabilities
- TensorRT optimization

---

**🎉 LispeTorch v2.1.0 marks a major milestone - complete Transformer architecture with model persistence, making it production-ready for Llama-3.1 fine-tuning and real-world AI applications!**
