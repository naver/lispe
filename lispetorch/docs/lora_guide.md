# 🚀 LoRA Fine-tuning Guide for LispeTorch

> **Parameter-Efficient Fine-tuning with Low-Rank Adaptation**

## Table of Contents
1. [Introduction to LoRA](#introduction-to-lora)
2. [Quick Start](#quick-start)
3. [HuggingFace LoRA API (Recommended)](#huggingface-lora-api-recommended)
4. [Basic LoRA API](#basic-lora-api)
5. [Complete Training Pipeline](#complete-training-pipeline)
6. [Optimizer and Scheduler](#optimizer-and-scheduler)
7. [Memory Management](#memory-management)
8. [Production Deployment](#production-deployment)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Introduction to LoRA

**Low-Rank Adaptation (LoRA)** is a parameter-efficient fine-tuning method that enables adapting large pre-trained models with minimal trainable parameters.

### Key Benefits
- **🚀 90%+ Parameter Reduction**: Train only LoRA matrices instead of full model
- **💾 Storage Efficiency**: Save adapters separately (~1% of model size)
- **⚡ Fast Training**: Fewer parameters = faster convergence
- **🔄 Zero Overhead**: Merge weights for deployment without performance loss
- **🎯 Task Specialization**: Different adapters for different tasks

### How LoRA Works

Instead of updating the full weight matrix `W`, LoRA decomposes the update into:
```
ΔW = B × A
```
Where:
- `A` is a `d × r` matrix (input dimension to rank)
- `B` is a `r × k` matrix (rank to output dimension)  
- `r` is the rank (much smaller than `d` and `k`)

The forward pass becomes:
```
output = W × input + (α/r) × B × A × input
```

---

## Quick Start

### Using HuggingFace LoRA API (Recommended)

```lisp
(use 'lispe_torch)

; Load model with LoRA support
(torch_hf_load_model_lora "my_model" "/path/to/model" (dictionary "device" "mps"))

; Initialize LoRA adapters
(torch_hf_lora_init "my_model" 16 32 (strings "q_proj" "k_proj" "v_proj" "o_proj") "bfloat16")

; Get trainable parameters
(setq lora_params (torch_hf_lora_get_parameters "my_model"))

; Forward pass (LoRA applied automatically)
(setq output (torch_hf_forward "my_model" input_tensor))

; Save adapters
(torch_hf_lora_save "my_model" "adapters.pt")
```

### Using Basic LoRA API

```lisp
(use 'lispe_torch)

; Create a LoRA linear layer
(setq lora_layer (torch_lora_linear 512 256 16 32.0))

; Forward pass
(setq output (torch_lora_forward lora_layer input))

; Save adapters
(torch_lora_save_adapters lora_layer "adapters.pt")
```

---

## HuggingFace LoRA API (Recommended)

This API provides seamless integration with HuggingFace models and automatic LoRA application during forward passes.

### torch_hf_load_model_lora
**Load a HuggingFace model with LoRA infrastructure**

```lisp
(torch_hf_load_model_lora model_name path config)
```

**Parameters:**
- `model_name` - Unique identifier for the model
- `path` - Path to the HuggingFace model directory
- `config` - Dictionary with configuration (e.g., `{"device" "mps"}`)

**Example:**
```lisp
(torch_hf_load_model_lora 
    "llama31_lora" 
    "/path/to/llama3.1-8B/model"
    (dictionary "device" "mps"))
```

### torch_hf_lora_init
**Initialize LoRA adapters for target modules**

```lisp
(torch_hf_lora_init model_name rank alpha target_modules dtype)
```

**Parameters:**
- `model_name` - Model identifier
- `rank` - LoRA rank (typically 8-32)
- `alpha` - Scaling factor (typically 16-64)
- `target_modules` - List of module names to apply LoRA
- `dtype` - Data type (e.g., "bfloat16", "float32")

**Example:**
```lisp
(torch_hf_lora_init
    "llama31_lora"
    16                                              ; rank
    32                                              ; alpha
    (strings "q_proj" "k_proj" "v_proj" "o_proj")  ; target modules
    "bfloat16")                                     ; dtype
```

### torch_hf_lora_get_parameters
**Get trainable LoRA parameters for optimizer**

```lisp
(torch_hf_lora_get_parameters model_name)
```

**Returns:** List of trainable tensors

**Example:**
```lisp
(setq lora_params (torch_hf_lora_get_parameters "llama31_lora"))
(println "Number of LoRA tensors:" (size lora_params))
```

### torch_hf_forward
**Forward pass with automatic LoRA application**

```lisp
(torch_hf_forward model_name input_tensor)
```

**Note:** LoRA corrections are applied automatically via C++ polymorphism.

**Example:**
```lisp
(setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
(setq output (torch_hf_forward "llama31_lora" input_2d))
```

### torch_hf_lora_save / torch_hf_lora_load
**Save and load LoRA adapters**

```lisp
(torch_hf_lora_save model_name path)
(torch_hf_lora_load model_name path)
```

**Example:**
```lisp
; Save only LoRA adapters (lightweight!)
(torch_hf_lora_save "llama31_lora" "checkpoints/adapters_epoch_1.pt")

; Load adapters into model
(torch_hf_lora_load "llama31_lora" "checkpoints/adapters_epoch_1.pt")
```

### torch_hf_lora_merge / torch_hf_lora_unmerge
**Merge or unmerge LoRA weights into base model**

```lisp
(torch_hf_lora_merge model_name)
(torch_hf_lora_unmerge model_name)
```

**Example:**
```lisp
; Merge for deployment (zero overhead inference)
(torch_hf_lora_merge "llama31_lora")

; Unmerge to continue training
(torch_hf_lora_unmerge "llama31_lora")
```

### torch_hf_lora_enable
**Enable or disable LoRA application**

```lisp
(torch_hf_lora_enable model_name enable)
```

**Example:**
```lisp
; Disable LoRA (use base model only)
(torch_hf_lora_enable "llama31_lora" false)

; Re-enable LoRA
(torch_hf_lora_enable "llama31_lora" true)
```

### torch_hf_memory_usage
**Get model memory usage**

```lisp
(torch_hf_memory_usage model_name)
```

**Returns:** Memory usage in bytes

**Example:**
```lisp
(setq memory_bytes (torch_hf_memory_usage "llama31_lora"))
(setq memory_gb (/ memory_bytes 1073741824.0))
(println "Memory usage:" memory_gb "GB")
```

---

## Basic LoRA API

For standalone LoRA layers without HuggingFace integration.

### torch_lora_linear
**Create a LoRA-adapted linear layer**

```lisp
(torch_lora_linear in_features out_features rank alpha)
```

**Example:**
```lisp
(setq lora_layer (torch_lora_linear 4096 4096 16 32.0))
```

### torch_lora_forward
**Forward pass through LoRA layer**

```lisp
(torch_lora_forward lora_layer input)
```

### torch_lora_apply_to_linear
**Convert existing linear layer to LoRA**

```lisp
(torch_lora_apply_to_linear linear_layer rank alpha)
```

### torch_lora_merge_weights
**Merge LoRA weights for deployment**

```lisp
(torch_lora_merge_weights lora_layer)
```

### torch_lora_trainable_params
**Get trainable parameters from LoRA model**

```lisp
(torch_lora_trainable_params model)
```

### torch_lora_save_adapters / torch_lora_load_adapters
**Adapter persistence**

```lisp
(torch_lora_save_adapters lora_layer "path.pt")
(torch_lora_load_adapters lora_layer "path.pt")
```

### torch_lora_forward_with_gradients
**Forward pass with gradient tracking**

```lisp
(torch_lora_forward_with_gradients lora_layer input retain_graph)
```

---

## Complete Training Pipeline

### Configuration

```lisp
(use 'lispe_torch)
(use 'lispe_tiktoken)

; LoRA Configuration
(setq lora-config (dictionary
    "rank" 16
    "alpha" 32
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
))

; Training Configuration
(setq training-config (dictionary
    "learning_rate" 2e-4
    "weight_decay" 0.01
    "num_epochs" 3
    "batch_size" 1
    "gradient_accumulation_steps" 4
    "max_seq_length" 256
    "logging_steps" 10
    "save_steps" 100
    "eval_steps" 50
    "warmup_steps" 100
    "max_grad_norm" 1.0
    "scheduler_type" "linear_warmup_cosine"
    "min_lr" 1e-6
    "device" "mps"  ; "mps" for Apple Silicon, "cuda" for NVIDIA, "cpu" otherwise
))
```

### Model Loading and LoRA Setup

```lisp
; Load model with LoRA support
(torch_hf_load_model_lora
    "llama31_lora"
    model_path
    (dictionary "device" (@ training-config "device")))

; Initialize LoRA adapters
(torch_hf_lora_init
    "llama31_lora"
    (@ lora-config "rank")
    (@ lora-config "alpha")
    (@ lora-config "target_modules")
    "bfloat16")

; Get trainable parameters
(setq lora_params (torch_hf_lora_get_parameters "llama31_lora"))
(println "LoRA parameters:" (size lora_params) "tensors")
```

### Training Step

```lisp
(defun train_step(input_tensor is_accumulating optimizer config)
    ; Zero gradients at the start of new accumulation
    (check (not is_accumulating)
        (torch_optimizer_zero_grad optimizer))

    ; Forward pass - LoRA applied automatically
    (setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
    (setq output (torch_hf_forward "llama31_lora" input_2d))

    ; Calculate loss
    (setq loss (calculate_loss output input_2d))

    (check loss
        ; Scale loss by accumulation steps
        (setq accum_steps (@ config "gradient_accumulation_steps"))
        (setq scaled_loss (tensor_div loss (tensor_create (floats accum_steps))))
        
        ; Backward pass
        (torch_backward scaled_loss)

        ; Synchronize GPU memory
        (torch_mps_synchronize)

        loss))
```

### Weight Update

```lisp
(defun should_update_weights(accumulation_step optimizer scheduler config)
    (setq accum_steps (@ config "gradient_accumulation_steps"))
    (setq should_update (== (% (+ accumulation_step 1) accum_steps) 0))

    (check should_update
        ; Gradient clipping
        (setq max_norm (@ config "max_grad_norm"))
        (check (> max_norm 0)
            (torch_clip_grad_norm optimizer max_norm))

        ; Optimizer and scheduler step
        (torch_optimizer_step optimizer)
        (torch_scheduler_step scheduler)

        ; Zero gradients after update
        (torch_optimizer_zero_grad optimizer))

    should_update)
```

### Validation

```lisp
(defun validate(model_name dataset_manager loss_calculator)
    ; Disable gradient computation
    (torch_set_grad_enabled false)

    (setq total_loss 0.0)
    (setq num_samples 5)

    (loopcount num_samples i
        (setq val_batch (dataset_manager DatasetManager (get_validation_batch 1)))
        (check (> (size val_batch) 0)
            (setq input_tensor (@ val_batch 0))
            (setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
            (setq output (torch_hf_forward model_name input_2d))
            (setq loss (calculate_loss output input_2d))
            (check loss
                (+= total_loss (@ loss 0))))
        (torch_mps_synchronize))

    ; Re-enable gradient computation
    (torch_set_grad_enabled true)

    (/ total_loss num_samples))
```

### Checkpoint Saving

```lisp
(defun save_checkpoint(model_name epoch checkpoint_dir lora_config training_config)
    (setq checkpoint_path (+ checkpoint_dir "/lora_adapters_epoch_" (string epoch) ".pt"))

    ; Save only LoRA adapters (lightweight!)
    (torch_hf_lora_save model_name checkpoint_path)

    ; Save metadata
    (setq checkpoint_info (dictionary
        "epoch" epoch
        "lora_config" lora_config
        "training_config" training_config))

    (setq info_file (+ checkpoint_dir "/training_info_epoch_" (string epoch) ".json"))
    (fwrite info_file (json checkpoint_info))

    (println "Checkpoint saved:" checkpoint_path))
```

---

## Optimizer and Scheduler

### Creating Optimizer with LoRA Parameters

```lisp
(setq optimizer (torch_optimizer_add_params
    lora_params
    (@ training-config "learning_rate")
    (@ training-config "weight_decay")))
```

### Learning Rate Scheduler

```lisp
(setq scheduler (torch_lr_scheduler optimizer (@ training-config "scheduler_type")
    (dictionary
        "initial_lr" (@ training-config "learning_rate")
        "min_lr" (@ training-config "min_lr")
        "total_steps" total_steps
        "warmup_steps" (@ training-config "warmup_steps"))))
```

### Optimizer Functions

| Function | Description |
|----------|-------------|
| `torch_optimizer_add_params` | Create optimizer with parameters |
| `torch_optimizer_zero_grad` | Zero all gradients |
| `torch_optimizer_step` | Perform optimization step |
| `torch_clip_grad_norm` | Clip gradients by norm |
| `torch_backward` | Compute gradients |

### Scheduler Functions

| Function | Description |
|----------|-------------|
| `torch_lr_scheduler` | Create learning rate scheduler |
| `torch_scheduler_step` | Step the scheduler |
| `torch_scheduler_get_lr` | Get current learning rate |

---

## Memory Management

### GPU Synchronization

```lisp
; Synchronize MPS/CUDA operations
(torch_mps_synchronize)
```

### Gradient Control

```lisp
; Disable gradient computation (for validation)
(torch_set_grad_enabled false)

; Re-enable gradient computation
(torch_set_grad_enabled true)
```

### Memory Monitoring

```lisp
; Check number of tensors in memory
(println "Tensors in memory:" (tensor_in_memory))

; Check model memory usage
(setq memory_gb (/ (torch_hf_memory_usage "model_name") 1073741824.0))
(println "Model memory:" memory_gb "GB")
```

### Memory Best Practices

```lisp
; 1. Synchronize before variable reassignment
(torch_mps_synchronize)

; 2. Set tensors to nil to release memory
(setq loss_tensor nil)

; 3. Use gradient accumulation for large effective batch sizes
(setq gradient_accumulation_steps 4)
(setq effective_batch_size (* batch_size gradient_accumulation_steps))

; 4. Disable gradients during validation
(torch_set_grad_enabled false)
; ... validation code ...
(torch_set_grad_enabled true)
```

---

## Production Deployment

### Option 1: Merged Weights (Recommended)

```lisp
; After training, merge for optimal performance
(torch_hf_lora_merge "llama31_lora")

; Forward pass now has zero LoRA overhead
(setq output (torch_hf_forward "llama31_lora" input))
```

### Option 2: Adapter Switching

```lisp
; Switch between task adapters
(torch_hf_lora_load "llama31_lora" "summarization_adapters.pt")
; ... inference for summarization ...

(torch_hf_lora_load "llama31_lora" "translation_adapters.pt")
; ... inference for translation ...
```

### Option 3: Enable/Disable LoRA

```lisp
; Use base model only
(torch_hf_lora_enable "llama31_lora" false)

; Re-enable LoRA adapters
(torch_hf_lora_enable "llama31_lora" true)
```

---

## Best Practices

### LoRA Configuration

**Rank Selection:**
- **Rank 4-8**: Simple tasks, limited data
- **Rank 16-32**: Complex tasks, sufficient data  
- **Rank 64+**: Very complex adaptations

**Alpha Selection:**
- **Alpha = Rank**: Balanced adaptation
- **Alpha = 2 × Rank**: Stronger adaptation  
- **Alpha = 0.5 × Rank**: Subtle adaptation

**Target Modules:**
- For Llama models: `"q_proj" "k_proj" "v_proj" "o_proj"`
- For full attention + FFN: Add `"gate_proj" "up_proj" "down_proj"`

### Training Tips

1. **Start Small**: Begin with rank 8-16, increase if needed
2. **Monitor Overfitting**: LoRA can overfit quickly with high ranks
3. **Use Gradient Accumulation**: Simulate larger batches with limited memory
4. **Learning Rate**: Use 10x higher LR than full fine-tuning (e.g., 2e-4)
5. **Warmup**: Use warmup steps for stable training start

### File Organization

```
project/
├── model/
│   └── llama3.1-8B/
├── tokenizer/
│   ├── tokenizer.json
│   └── special_tokens_map.json
├── checkpoints/
│   ├── lora_adapters_epoch_1.pt
│   ├── lora_adapters_epoch_2.pt
│   └── training_info_epoch_1.json
├── dataset/
│   └── training_data.json
└── scripts/
    └── lora_training.lisp
```

---

## Troubleshooting

### Common Issues

**Error: "tensor size mismatch"**
```lisp
; Ensure input dimensions match model expectations
(setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
```

**Memory Issues (OOM)**
```lisp
; Reduce batch size
(setq batch_size 1)

; Use gradient accumulation
(setq gradient_accumulation_steps 8)

; Synchronize frequently
(torch_mps_synchronize)
```

**Gradient Explosion**
```lisp
; Use gradient clipping
(torch_clip_grad_norm optimizer 1.0)
```

### Debugging

**Check Memory:**
```lisp
(println "Tensors in memory:" (tensor_in_memory))
(println "Memory usage:" (/ (torch_hf_memory_usage "model") 1073741824.0) "GB")
```

**Monitor Training:**
```lisp
(println "Step" global_step
    "| Loss:" (round (* loss 10000) 10000)
    "| LR:" (torch_scheduler_get_lr scheduler))
```

---

## Resources

- **Original LoRA Paper**: "LoRA: Low-Rank Adaptation of Large Language Models"
- **LispeTorch Documentation**: Complete API reference
- **Example**: `lispetorch/exemples/lora_training.lisp` - Complete training script
- **Llama-3.1 Model**: Official Meta model weights

---

**Happy LoRA Fine-tuning with LispeTorch! 🚀**
