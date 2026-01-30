# 🤖 LispETorch Text Generation Guide

> **Comprehensive Text Generation with Multiple Sampling Strategies**

This guide covers the complete text generation system implemented in LispETorch, featuring multiple sampling strategies, configurable parameters, and production-ready generation pipelines.

## 🚀 **Quick Start**

```lisp
; Load the library
(use 'lispe_torch)

; Create a generator with a PyTorch model
(setq generator (torch_generator_create model "cpu"))

; Generate text with greedy search
(setq input_tokens (torch_tensor (integers 15339 1332)))
(setq result (torch_generate generator input_tokens "greedy"))
```

## 📋 **Generation Functions**

### Core Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `torch_generator_create` | Create a text generator | `(model device)` |
| `torch_generator_config` | Configure with dictionary | `(generator config_dict)` |
| `torch_generate` | Generate text | `(generator input_ids strategy)` |
| `torch_set_generation_params` | Quick parameter setting | `(generator max_length temperature top_k top_p)` |

### Configuration Dictionary

```lisp
(setq config (
    ("max_length" 50)           ; Maximum generation length
    ("temperature" 0.8)         ; Sampling temperature (0.1-2.0)
    ("top_k" 40)               ; Top-K sampling parameter
    ("top_p" 0.9)              ; Top-P (nucleus) sampling parameter  
    ("strategy" "greedy")       ; Generation strategy
    ("repetition_penalty" 1.1)  ; Repetition penalty (>1.0)
    ("eos_token_id" 50256)     ; End-of-sequence token ID
    ("pad_token_id" 50257)     ; Padding token ID
    ("do_sample" true)         ; Enable sampling vs deterministic
))
```

## 🎯 **Generation Strategies**

### 1. Greedy Search
**Deterministic** - Always selects the highest probability token.

```lisp
(setq greedy_result (torch_generate generator input_tokens "greedy"))
```

**Use Cases:**
- ✅ Consistent, predictable output
- ✅ Factual question answering
- ✅ Translation tasks
- ❌ Creative writing (can be repetitive)

### 2. Top-K Sampling
**Stochastic** - Samples from the K most likely tokens.

```lisp
; Configure Top-K
(torch_set_generation_params generator 50 0.8 40 0.9)
(setq topk_result (torch_generate generator input_tokens "top_k"))
```

**Parameters:**
- `top_k`: Number of top tokens to consider (typical: 20-50)
- `temperature`: Controls randomness (0.7-1.2 for balanced creativity)

**Use Cases:**
- ✅ Creative writing
- ✅ Conversation generation
- ✅ Balanced creativity vs coherence

### 3. Top-P (Nucleus) Sampling
**Dynamic Vocabulary** - Samples from tokens comprising top P% of probability mass.

```lisp
; Configure Top-P  
(torch_set_generation_params generator 50 0.9 50 0.92)
(setq topp_result (torch_generate generator input_tokens "top_p"))
```

**Parameters:**
- `top_p`: Cumulative probability threshold (typical: 0.85-0.95)
- `temperature`: Fine-tunes creativity within the nucleus

**Use Cases:**
- ✅ Most human-like text
- ✅ Adaptive vocabulary based on context
- ✅ High-quality creative content

### 4. Beam Search
**Multi-hypothesis** - Explores multiple sequence paths simultaneously.

```lisp
(setq beam_result (torch_generate generator input_tokens "beam_search"))
```

**Use Cases:**
- ✅ Translation tasks
- ✅ Summarization
- ✅ When quality is paramount over diversity

## 🌡️ **Parameter Tuning Guide**

### Temperature Settings
```lisp
; Conservative (factual, focused)
(torch_set_generation_params generator 50 0.3 40 0.9)

; Balanced (natural, coherent)  
(torch_set_generation_params generator 50 0.8 40 0.9)

; Creative (diverse, experimental)
(torch_set_generation_params generator 50 1.2 40 0.9)

; Wild (highly creative, potentially incoherent)
(torch_set_generation_params generator 50 2.0 40 0.9)
```

### Top-K vs Top-P Combinations
```lisp
; Conservative nucleus
(torch_set_generation_params generator 50 0.8 30 0.85)

; Balanced sampling
(torch_set_generation_params generator 50 0.8 40 0.9) 

; Creative exploration
(torch_set_generation_params generator 50 1.0 50 0.95)
```

### Repetition Control
```lisp
(setq config (
    ("repetition_penalty" 1.0)   ; No penalty
    ("repetition_penalty" 1.1)   ; Light penalty (recommended)
    ("repetition_penalty" 1.3)   ; Strong penalty (risk of incoherence)
))
```

## 🔧 **Advanced Usage**

### Model Integration
```lisp
; For PyTorch models that support text generation
; The model should accept token IDs and return logits
(setq model (torch_model_create "transformer" config))
(setq generator (torch_generator_create model device))
```

### Batch Processing
```lisp
; Process multiple inputs (future enhancement)
(setq batch_inputs (torch_tensor (integers (10 20) (30 40))))
(setq batch_results (torch_generate generator batch_inputs "top_p"))
```

### Custom Token Handling
```lisp
; Configure special tokens for your model
(setq config (
    ("eos_token_id" 2)      ; </s> for Llama models
    ("pad_token_id" 0)      ; <pad> token  
    ("bos_token_id" 1)      ; <s> beginning of sequence
))
```

## 📊 **Performance Considerations**

### Memory Optimization
- **Flash Attention**: Use for long sequences (>512 tokens)
- **Tensor Reuse**: Generator maintains internal state efficiently
- **Batch Size**: Start with batch_size=1, scale based on GPU memory

### Speed Optimization
```lisp
; Prefer GPU for large models
(setq generator (torch_generator_create model "cuda"))

; Use appropriate precision
(setq model (torch_model_to_dtype model "float16"))  ; Half precision
```

### Quality vs Speed Trade-offs
- **Greedy**: Fastest, least diverse
- **Top-K**: Good balance of speed and quality  
- **Top-P**: Best quality, moderate speed
- **Beam Search**: Highest quality, slowest

## 🎯 **Use Case Examples**

### Chatbot Response Generation
```lisp
(setq config (
    ("max_length" 100)
    ("temperature" 0.8)
    ("top_p" 0.9)
    ("strategy" "top_p")
    ("repetition_penalty" 1.1)
))
```

### Creative Writing
```lisp
(setq config (
    ("max_length" 200)
    ("temperature" 1.0)
    ("top_k" 50)
    ("strategy" "top_k") 
    ("repetition_penalty" 1.05)
))
```

### Code Generation  
```lisp
(setq config (
    ("max_length" 150)
    ("temperature" 0.3)
    ("top_p" 0.85)
    ("strategy" "top_p")
    ("repetition_penalty" 1.2)
))
```

### Translation
```lisp
(setq config (
    ("max_length" 100)
    ("temperature" 0.3)
    ("strategy" "beam_search")
    ("repetition_penalty" 1.0)
))
```

## 🚨 **Troubleshooting**

### Common Issues

**Out of Memory**
```lisp
; Reduce sequence length
(torch_set_generation_params generator 25 0.8 40 0.9)

; Use CPU instead of GPU
(setq generator (torch_generator_create model "cpu"))
```

**Repetitive Output**
```lisp
; Increase repetition penalty
(setq config (("repetition_penalty" 1.3)))

; Use nucleus sampling
(setq result (torch_generate generator input_tokens "top_p"))
```

**Incoherent Output**
```lisp
; Lower temperature
(torch_set_generation_params generator 50 0.5 40 0.9)

; Use greedy or beam search
(setq result (torch_generate generator input_tokens "greedy"))
```

**Slow Generation**
```lisp
; Use GPU if available
(if (torch_cuda_is_available)
    (setq generator (torch_generator_create model "cuda"))
    (setq generator (torch_generator_create model "cpu")))

; Reduce max_length for faster iteration
(torch_set_generation_params generator 30 0.8 40 0.9)
```

## 🔗 **Integration with Other Components**

### With Flash Attention
```lisp
; Flash attention for memory efficiency during generation
(setq attention_output (torch_flash_attention query key_tensor value_tensor))
(setq generated_text (torch_generate generator attention_output "top_p"))
```

### With Tokenization
```lisp
; Complete pipeline from text to generated text
(setq tokens (tokenizer_encode text))
(setq input_tensor (torch_tensor tokens))
(setq output_tensor (torch_generate generator input_tensor "top_p"))
(setq generated_text (tokenizer_decode output_tensor))
```

## 📈 **Future Enhancements**

Planned improvements to the text generation system:

- 🔄 **Beam Search Optimization**: Full beam search implementation
- 📦 **Batch Generation**: Efficient multi-sequence generation
- 🎯 **Guided Generation**: Constrained generation with rules
- 🔧 **Model Specialization**: Optimized generators for specific model types
- 📊 **Generation Metrics**: Quality scoring and evaluation tools

---

**Ready to generate text?** Start with the Quick Start guide and experiment with different strategies to find what works best for your use case!
