/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_lispetorch.cxx


#include "lispe.h"
#include "lispe_lispetorch.h"
#include "listes.h"
#include <torch/torch.h>
#include <torch/script.h>
#include <torch/csrc/jit/serialization/import.h>
#include <chrono>

Element* Lispe_lispetorch::eval(LispE* lisp) {
    switch (action) {
    case torch_is_mps:
        return tensor_is_mps(lisp);
    case torch_tensor_to_cpu:
        return tensor_to_cpu(lisp);
    case torch_cuda_empty_cache:
        return cuda_empty_cache(lisp);
    case torch_tensor_create:
        return tensor_create(lisp);
    case torch_tensor_zeros:
        return tensor_zeros(lisp);
    case torch_tensor_in_memory:
        return tensor_in_memory(lisp);
    case torch_tensor_to_lispe:
        return tensor_to_lispe(lisp);
    case torch_tensor_full:
        return tensor_full(lisp);
    case torch_tensor_ones:
        return tensor_ones(lisp);
    case torch_tensor_cat:
        return tensor_cat(lisp);    
    case torch_tensor_triu:
        return tensor_triu(lisp);
    case torch_tensor_set_item:
        return tensor_set_item(lisp);
    case torch_tensor_randn:
        return tensor_randn(lisp);
    case torch_tensor_rand:
        return tensor_rand(lisp);
    case torch_tensor_randint:
        return tensor_randint(lisp);
    case torch_tensor_transpose:
        return tensor_transpose(lisp);
    case torch_tensor_add:
        return tensor_add(lisp);
    case torch_tensor_sub:
        return tensor_sub(lisp);
    case torch_tensor_mul:
        return tensor_mul(lisp);
    case torch_tensor_div:
        return tensor_div(lisp);
    case torch_tensor_mul_scalar:
        return tensor_mul_scalar(lisp);
    case torch_tensor_add_scalar:
        return tensor_add_scalar(lisp);
    case torch_tensor_div_scalar:
        return tensor_div_scalar(lisp);
    case torch_tensor_linear:
        return tensor_linear(lisp);
    case torch_tensor_matmul:
        return tensor_matmul(lisp);
    case torch_tensor_reshape:
        return tensor_reshape(lisp);
    case torch_tensor_contiguous:
        return tensor_contiguous(lisp);
    case torch_tensor_unsqueeze:
        return tensor_unsqueeze(lisp);
    case torch_tensor_squeeze:
        return tensor_squeeze(lisp);
    case torch_tensor_slice:
        return tensor_slice(lisp);
    case torch_tensor_select:
        return tensor_select(lisp);
    case torch_tensor_to_cuda:
        return tensor_to_cuda(lisp);
    case torch_tensor_to_device:
        return tensor_to_device(lisp);
    case torch_tensor_sum:
        return tensor_sum(lisp);
    case torch_tensor_mean:
        return tensor_mean(lisp);
    case torch_tensor_mean_dim:
        return tensor_mean_dim(lisp);
    case torch_tensor_std:
        return tensor_std(lisp);
    case torch_tensor_max:
        return tensor_max(lisp);
    case torch_tensor_min:
        return tensor_min(lisp);
    case torch_tensor_argmax:
        return tensor_argmax(lisp);
    case torch_tensor_einsum:
        return tensor_einsum(lisp);
    case torch_tensor_relu:
        return tensor_relu(lisp);
    case torch_tensor_sigmoid:
        return tensor_sigmoid(lisp);
    case torch_tensor_tanh:
        return tensor_tanh(lisp);
    case torch_tensor_gelu:
        return tensor_gelu(lisp);
    case torch_tensor_silu:
        return tensor_silu(lisp);
    case torch_tensor_abs:
        return tensor_abs(lisp);
    case torch_tensor_exp:
        return tensor_exp(lisp);
    case torch_tensor_softmax:
        return tensor_softmax(lisp);
    case torch_tensor_log_softmax:
        return tensor_log_softmax(lisp);
    case torch_tensor_pow:
        return tensor_pow(lisp);
    case torch_tensor_sqrt:
        return tensor_sqrt(lisp);
    case torch_tensor_rsqrt:
        return tensor_rsqrt(lisp);
    case torch_tensor_log:
        return tensor_log(lisp);
    case torch_tensor_log10:
        return tensor_log10(lisp);
    case torch_tensor_log2:
        return tensor_log2(lisp);
    case torch_tensor_sin:
        return tensor_sin(lisp);
    case torch_tensor_cos:
        return tensor_cos(lisp);
    case torch_tensor_tan:
        return tensor_tan(lisp);
    case torch_tensor_asin:
        return tensor_asin(lisp);
    case torch_tensor_acos:
        return tensor_acos(lisp);
    case torch_tensor_atan:
        return tensor_atan(lisp);
    case torch_tensor_sinh:
        return tensor_sinh(lisp);
    case torch_tensor_cosh:
        return tensor_cosh(lisp);
    case torch_tensor_floor:
        return tensor_floor(lisp);
    case torch_tensor_ceil:
        return tensor_ceil(lisp);
    case torch_tensor_round:
        return tensor_round(lisp);
    case torch_tensor_clamp:
        return tensor_clamp(lisp);
    case torch_tensor_neg:
        return tensor_neg(lisp);
    case torch_tensor_reciprocal:
        return tensor_reciprocal(lisp);
    case torch_tensor_rms_norm:
        return tensor_rms_norm(lisp);
    case torch_tensor_full_like:
        return tensor_full_like(lisp);
    case torch_tensor_cumsum:
        return tensor_cumsum(lisp);
    case torch_tensor_gather:
        return tensor_gather(lisp);
    case torch_tensor_masked_fill_:
        return tensor_masked_fill_(lisp);
    case torch_linear_create:
        return linear_create(lisp);
    case torch_linear_forward:
        return linear_forward(lisp);
    case torch_tensor_size:
        return tensor_size(lisp);
    case torch_tensor_shape:
        return tensor_shape(lisp);
    case torch_tensor_item:
        return tensor_item(lisp);
    case torch_model_create:
        return model_create(lisp);
    case torch_model_forward:
        return model_forward(lisp);
    case torch_optimizer_create:
        return optimizer_create(lisp);
    case torch_adam_optimizer:
        return adam_optimizer_create(lisp);
    case torch_adamw_optimizer:
        return adamw_optimizer_create(lisp);
    case torch_sgd_optimizer:
        return sgd_optimizer_create(lisp);
    case torch_optimizer_step:
        return optimizer_step(lisp);
    case torch_optimizer_zero_grad:
        return optimizer_zero_grad(lisp);
    case torch_clip_grad_norm:
        return clip_grad_norm(lisp);
    case torch_optimizer_add_params:
        return optimizer_add_params(lisp);
    case torch_loss_mse:
        return loss_mse(lisp);
    case torch_loss_crossentropy:
        return loss_crossentropy(lisp);
    case torch_cross_entropy:  // Alias for torch_loss_crossentropy
        return loss_crossentropy(lisp);
    case torch_backward:
        return backward(lisp);
    case torch_set_grad_enabled:
        return set_grad_enabled(lisp);
    case torch_cuda_is_available:
        return cuda_is_available(lisp);
    case torch_cuda_device_count:
        return cuda_device_count(lisp);
    case torch_cuda_memory_allocated:
        return cuda_memory_allocated(lisp);
    case torch_cuda_memory_total:
        return cuda_memory_total(lisp);
    case torch_set_device:
        return set_device(lisp);
    case torch_mps_is_available:
        return mps_is_available(lisp);
    case torch_to_mps:
        return tensor_to_mps(lisp);
    case torch_get_best_device:
        return get_best_device(lisp);
    case torch_multihead_attention_create:
        return multihead_attention_create(lisp);
    case torch_multihead_attention_forward:
        return multihead_attention_forward(lisp);
    case torch_layer_norm_create:
        return layer_norm_create(lisp);
    case torch_layer_norm_forward:
        return layer_norm_forward(lisp);
    case torch_embedding_create:
        return embedding_create(lisp);
    case torch_embedding_forward:
        return embedding_forward(lisp);
    case torch_transformer_block_create:
        return transformer_block_create(lisp);
    case torch_transformer_block_forward:
        return transformer_block_forward(lisp);
    case torch_tokenizer_simple_create:
        return tokenizer_simple_create(lisp);
    case torch_tokenizer_sentencepiece_create:
        return tokenizer_sentencepiece_create(lisp);
    case torch_tokenizer_sentencepiece_train:
        return tokenizer_sentencepiece_train(lisp);
    case torch_tokenizer_encode:
        return tokenizer_encode(lisp);
    case torch_tokenizer_decode:
        return tokenizer_decode(lisp);
    case torch_vocabulary_size:
        return vocabulary_size(lisp);
    case torch_pad_sequence:
        return pad_sequence(lisp);
    case torch_create_attention_mask:
        return create_attention_mask(lisp);
    case torch_positional_encoding_create:
        return positional_encoding_create(lisp);
    case torch_positional_encoding_forward:
        return positional_encoding_forward(lisp);
    case torch_rotary_embedding_create:
        return rotary_embedding_create(lisp);
    case torch_rotary_embedding_forward:
        return rotary_embedding_forward(lisp);
    case torch_apply_rotary_pos_emb:
        return apply_rotary_pos_emb(lisp);
    case torch_checkpoint_enable:
        return checkpoint_enable(lisp);
    case torch_checkpoint_disable:
        return checkpoint_disable(lisp);
    case torch_checkpoint_forward:
        return checkpoint_forward(lisp);
    case torch_checkpoint_create:
        return checkpoint_create(lisp);
    case torch_load_model:
        return load_model(lisp);
    case torch_jit_model_to_mps:
        return jit_model_to_mps(lisp);
    case torch_jit_model_to_best_device:
        return jit_model_to_best_device(lisp);
    case torch_mps_synchronize:
        return mps_synchronize(lisp);        
    case torch_jit_load:
        return jit_load(lisp);
    case torch_jit_unload:
        return jit_unload(lisp);
    case torch_jit_model_to_device:
        return jit_model_to_device(lisp);
    case torch_jit_model_forward:
        return jit_model_forward(lisp);
    case torch_jit_model_info:
        return jit_model_info(lisp);
    case torch_jit_model_get_buffer:
        return jit_model_get_buffer(lisp);
    case torch_jit_model_get_tensor:
        return jit_model_get_tensor(lisp);
    case torch_jit_model_get_tensor_shape:
        return jit_model_get_tensor_shape(lisp);
    case torch_jit_model_list_methods:
        return jit_model_list_methods(lisp);
    case torch_jit_model_list_buffers:
        return jit_model_list_buffers(lisp);
        // Dans le switch (action) :
    case torch_jit_model_register_lora_hook:
        return jit_model_register_lora_hook(lisp);
    case torch_jit_model_forward_with_lora:
        return jit_model_forward_with_lora(lisp);
    case torch_jit_model_get_intermediate_states:
        return jit_model_get_intermediate_states(lisp);
    case torch_jit_model_list_tensor_names:
        return jit_model_list_tensor_names(lisp);
    case torch_jit_model_list_parameter_names:
        return jit_model_list_parameter_names(lisp);
    case torch_jit_model_update_weight:
        return jit_model_update_weight(lisp);
    case torch_lora_compute_delta:
        return lora_compute_delta(lisp);
    case torch_lora_get_adaptation_magnitude:
        return lora_get_adaptation_magnitude(lisp);
    case torch_lora_forward_with_gradients:
        return lora_forward_with_gradients(lisp);
    case torch_save_model:
        return save_model(lisp);
    case torch_load_checkpoint:
        return load_checkpoint(lisp);
    case torch_save_checkpoint:
        return save_checkpoint(lisp);
    case torch_load_state_dict:
        return load_state_dict(lisp);
    case torch_model_state_dict:
        return model_state_dict(lisp);
    case torch_lora_linear_create:
        return lora_linear_create(lisp);
    case torch_lora_linear_forward:
        return lora_linear_forward(lisp);
    case torch_lora_apply_to_linear:
        return lora_apply_to_linear(lisp);
    case torch_lora_merge_weights:
        return lora_merge_weights(lisp);
    case torch_lora_get_trainable_params:
        return lora_get_trainable_params(lisp);
    case torch_lora_save_adapters:
        return lora_save_adapters(lisp);
    case torch_lora_load_adapters:
        return lora_load_adapters(lisp);
    // Tucker Decomposition cases
    case torch_tucker_decomposition:
        return tucker_decomposition(lisp);
    case torch_tucker_reconstruct:
        return tucker_reconstruct(lisp);
    case torch_tucker_compression_ratio:
        return tucker_compression_ratio(lisp);
    case torch_khatri_rao_product:
        return khatri_rao_product(lisp);
    // Flash Attention cases
    case torch_flash_attention_create:
        return flash_attention_create(lisp);
    case torch_flash_attention_forward:
        return flash_attention_forward(lisp);
    case torch_flash_attention_with_mask:
        return flash_attention_with_mask(lisp);
    case torch_flash_attention_with_dropout:
        return flash_attention_with_dropout(lisp);
    case torch_flash_attention:
        return flash_attention_simple(lisp);
    case torch_scaled_dot_product_attention:
        return scaled_dot_product_attention(lisp);
    // === Génération de texte ===
    case torch_generator_create:
        return generator_create(lisp);
    case torch_generator_config:
        return generator_config(lisp);
    case torch_generate:
        return generate(lisp);
    case torch_set_generation_params:
        return set_generation_params(lisp);
    case torch_topk:
        return topk(lisp);
    case torch_multinomial:
        return multinomial(lisp);
    case torch_sort:
        return sort_tensor(lisp);
    // Nouveaux cas pour quantification
    case torch_quantize_dynamic:
        return quantize_dynamic(lisp);
    case torch_quantize_static:
        return quantize_static(lisp);
    case torch_dequantize:
        return dequantize(lisp);
    case torch_quantize_linear:
        return quantize_linear(lisp);
    case torch_quantize_per_channel:
        return quantize_per_channel(lisp);
    case torch_quantize_int8:
        return quantize_int8(lisp);
    case torch_quantize_fp16:
        return quantize_fp16(lisp);
    case torch_tensor_to_int8:
        return tensor_to_int8(lisp);
    case torch_tensor_to_fp16:
        return tensor_to_fp16(lisp);
    case torch_model_quantize_dynamic:
        return model_quantize_dynamic(lisp);
    case torch_model_quantize_static:
        return model_quantize_static(lisp);
    case torch_lr_scheduler_create:
        return lr_scheduler_create(lisp);
    case torch_lr_scheduler_step:
        return lr_scheduler_step(lisp);
    case torch_lr_scheduler_get_lr:
        return lr_scheduler_get_lr(lisp);
    case torch_lr_scheduler_set_lr:
        return lr_scheduler_set_lr(lisp);
    // === Nouveaux cas HuggingFace ===
    case torch_hf_load_model:
        return hf_load_model(lisp);
    case torch_hf_model_info:
        return hf_model_info(lisp);
    case torch_hf_list_weights:
        return hf_list_weights(lisp);
    case torch_hf_get_weight:
        return hf_get_weight(lisp);
    case torch_hf_get_q_weight:
        return hf_get_q_weight(lisp);
    case torch_hf_get_k_weight:
        return hf_get_k_weight(lisp);
    case torch_hf_get_v_weight:
        return hf_get_v_weight(lisp);
    case torch_hf_get_o_weight:
        return hf_get_o_weight(lisp);
    case torch_hf_get_qkv_fused_weight:
        return hf_get_qkv_fused_weight(lisp);
    case torch_hf_get_gate_weight:
        return hf_get_gate_weight(lisp);
    case torch_hf_get_up_weight:
        return hf_get_up_weight(lisp);
    case torch_hf_get_down_weight:
        return hf_get_down_weight(lisp);
    case torch_hf_get_gate_up_fused_weight:
        return hf_get_gate_up_fused_weight(lisp);
    case torch_hf_get_rms_norm_eps:
        return hf_get_rms_norm_eps(lisp);
    case torch_hf_forward:
        return hf_forward(lisp);
    case torch_hf_forward_manual:
        return hf_forward_manual(lisp);
    case torch_hf_forward_attention_scores:
        return hf_forward_attention_scores(lisp);
    case torch_hf_forward_attention_size:
        return hf_forward_attention_size(lisp);
    case torch_hf_clear_attention_scores:
        return hf_clear_attention_scores(lisp);
    case torch_hf_load_model_lora:
        return hf_load_model_lora(lisp);
    case torch_hf_lora_init:
        return hf_lora_init(lisp);
    case torch_hf_lora_get_parameters:
        return hf_lora_get_parameters(lisp);
    case torch_hf_lora_save:
        return hf_lora_save(lisp);
    case torch_hf_lora_load:
        return hf_lora_load(lisp);
    case torch_hf_lora_merge:
        return hf_lora_merge(lisp);
    case torch_hf_lora_unmerge:
        return hf_lora_unmerge(lisp);
    case torch_hf_lora_enable:
        return hf_lora_enable(lisp);
    case torch_hf_memory_usage:
        return hf_memory_usage(lisp);
    case torch_hf_model_summary:
        return hf_model_summary(lisp);
    case torch_hf_enable_kv_cache:
        return hf_enable_kv_cache(lisp);
    case torch_hf_reset_kv_cache:
        return hf_reset_kv_cache(lisp);
    case torch_hf_embeddings:
        return hf_embeddings(lisp);
    case torch_hf_generate:
        return hf_generate(lisp);
    }
    return this;
}

wstring Lispe_lispetorch::asString(LispE* lisp) {
    switch (action) {
    case torch_is_mps:
        return L"Returns true if the tensor is on MPS";
    case torch_tensor_to_cpu:
        return L"Move a tensor from GPU to CPU (torch_to_cpu)";
    case torch_cuda_empty_cache:
        return L"Empty the CUDA memory cache (torch_cuda_empty_cache)";
    case torch_tensor_create:
        return L"Create a tensor from LispE data";
    case torch_tensor_full:
        return L"Create a tensor filled with a constant value";        
    case torch_tensor_zeros:
        return L"Create a tensor filled with zeros";
    case torch_tensor_to_lispe:
        return L"Translates a tensor into a LispE list";
    case torch_tensor_ones:
        return L"Create a tensor filled with ones";
    case torch_tensor_triu:
        return L"Extract upper triangular part of a tensor";
    case torch_tensor_cat:
        return L"Concatenate a list of tensors along a specified dimension";    
    case torch_tensor_set_item:
        return L"Set a specific element in a tensor";
    case torch_tensor_rand:
        return L"Create a tensor filled with random values between 0 and 1";
    case torch_tensor_randn:
        return L"Create a tensor filled with random values from normal distribution";
    case torch_tensor_randint:
        return L"Create a tensor filled with random integers in a specified range";
    case torch_tensor_transpose:
        return L"Transpose dimensions of a tensor";
    case torch_tensor_add:
        return L"Add two tensors element-wise";
    case torch_tensor_sub:
        return L"Subtract two tensors element-wise";
    case torch_tensor_mul:
        return L"Multiply two tensors element-wise";
    case torch_tensor_div:
        return L"Divide two tensors element-wise";
    case torch_tensor_squeeze:
        return L"Remove dimensions of size 1 from a tensor";
    case torch_tensor_select:
        return L"Select a slice along a dimension at a specific index";
    case torch_tensor_mul_scalar:
        return L"Multiply a tensor by a scalar value";
    case torch_tensor_add_scalar:
        return L"Add a scalar value to all elements of a tensor";
    case torch_tensor_div_scalar:
        return L"Divide a tensor by a scalar value";
    case torch_tensor_linear:
        return L"Linear multiplication of two tensors";
    case torch_tensor_matmul:
        return L"Matrix multiplication of two tensors";
    case torch_tensor_reshape:
        return L"Reshape a tensor to new dimensions";
    case torch_tensor_contiguous:
        return L"Return a contiguous tensor in memory";
    case torch_tensor_unsqueeze:
        return L"Add dimension of size 1 at specified position";
    case torch_tensor_slice:
        return L"Slice a tensor along multiple dimensions";
    case torch_tensor_sum:
        return L"Sum tensor elements along specified dimensions";
    case torch_tensor_mean:
        return L"Calculate mean of tensor elements along specified dimensions";
    case torch_tensor_std:
        return L"Calculate standard deviation of tensor elements";
    case torch_tensor_max:
        return L"Get maximum values from tensor along specified dimensions";
    case torch_tensor_min:
        return L"Get minimum values from tensor along specified dimensions";
    case torch_tensor_argmax:
        return L"Get indices of maximum values along specified dimensions";
    case torch_tensor_einsum:
        return L"Einstein summation convention for tensor operations";
    case torch_tensor_relu:
        return L"Apply ReLU activation function to tensor";
    case torch_tensor_sigmoid:
        return L"Apply sigmoid activation function to tensor";
    case torch_tensor_tanh:
        return L"Apply hyperbolic tangent activation function to tensor";
    case torch_tensor_gelu:
        return L"Apply GELU activation function to tensor";
    case torch_tensor_abs:
        return L"Apply absolute value function to tensor";
    case torch_tensor_softmax:
        return L"Apply softmax function along specified dimension";
    case torch_tensor_pow:
        return L"Raise tensor to power (element-wise)";
    case torch_tensor_sqrt:
        return L"Apply square root function to tensor";
    case torch_tensor_rsqrt:
        return L"Apply reciprocal square root function to tensor (1/sqrt(x))";
    case torch_tensor_log:
        return L"Apply natural logarithm function to tensor";
    case torch_tensor_log10:
        return L"Apply base-10 logarithm function to tensor";
    case torch_tensor_log2:
        return L"Apply base-2 logarithm function to tensor";
    case torch_tensor_sin:
        return L"Apply sine function to tensor";
    case torch_tensor_cos:
        return L"Apply cosine function to tensor";
    case torch_tensor_tan:
        return L"Apply tangent function to tensor";
    case torch_tensor_asin:
        return L"Apply arcsine function to tensor";
    case torch_tensor_acos:
        return L"Apply arccosine function to tensor";
    case torch_tensor_atan:
        return L"Apply arctangent function to tensor";
    case torch_tensor_sinh:
        return L"Apply hyperbolic sine function to tensor";
    case torch_tensor_cosh:
        return L"Apply hyperbolic cosine function to tensor";
    case torch_tensor_floor:
        return L"Apply floor function to tensor (round down to nearest integer)";
    case torch_tensor_ceil:
        return L"Apply ceiling function to tensor (round up to nearest integer)";
    case torch_tensor_round:
        return L"Apply round function to tensor (round to nearest integer)";
    case torch_tensor_clamp:
        return L"Clamp tensor values between min and max values";
    case torch_tensor_neg:
        return L"Apply negation to tensor (multiply by -1)";
    case torch_tensor_reciprocal:
        return L"Apply reciprocal function to tensor (1/x)";
    case torch_tensor_rms_norm:
        return L"Apply RMS normalization: x * weight / sqrt(mean(x^2) + eps)";
    case torch_tensor_full_like:
        return L"Create tensor with same shape as input, filled with constant value";
    case torch_tensor_cumsum:
        return L"Cumulative sum along specified dimension";
    case torch_tensor_gather:
        return L"Gather values along dimension according to indices";
    case torch_tensor_masked_fill_:
        return L"Fill tensor elements where mask is True with specified value";
    case torch_tensor_size:
        return L"Get dimensions of tensor as list";
    case torch_tensor_shape:
        return L"Get shape information of tensor";
    case torch_tensor_item:
        return L"Extract scalar value from a single-element tensor";
    case torch_multihead_attention_create:
        return L"Create a multi-head attention layer";
    case torch_multihead_attention_forward:
        return L"Forward pass through multi-head attention";
    case torch_layer_norm_create:
        return L"Create a layer normalization layer";
    case torch_model_create:
        return L"Create a PyTorch model";
    case torch_model_train:
        return L"Set model to training mode";
    case torch_model_eval:
        return L"Set model to evaluation mode";
    case torch_optimizer_create:
        return L"Create a general optimizer";
    case torch_adam_optimizer:
        return L"Create Adam optimizer";
    case torch_adamw_optimizer:
        return L"Create AdamW optimizer";
    case torch_sgd_optimizer:
        return L"Create SGD optimizer";
    case torch_optimizer_step:
        return L"Update model parameters using optimizer";
    case torch_optimizer_zero_grad:
        return L"Zero gradients for all parameters";
    case torch_clip_grad_norm:
        return L"Clip gradients by norm to prevent exploding gradients";
    case torch_optimizer_add_params:
        return L"Add parameters to optimizer";
    case torch_loss_mse:
        return L"Calculate Mean Squared Error loss";
    case torch_loss_crossentropy:
        return L"Calculate Cross Entropy loss";
    case torch_cross_entropy:
        return L"Calculate Cross Entropy loss (alias)";
    case torch_backward:
        return L"Compute gradients via backpropagation";
    case torch_set_grad_enabled:
        return L"Enable or disable gradient computation";
    case torch_jit_load:
        return L"Load a JIT model";
    case torch_jit_unload:
        return L"Unload a JIT model";
    case torch_jit_model_to_device:
        return L"Move a JIT model to a device";
    case torch_jit_model_forward:
        return L"Forward pass on a JIT model";
    case torch_jit_model_info:
        return L"Get information about a JIT model";
    case torch_jit_model_get_buffer:
        return L"Get a buffer from JIT model by name";
    case torch_jit_model_get_tensor:
        return L"Get a tensor from JIT model by name";
    case torch_jit_model_get_tensor_shape:
        return L"Get tensor shape from JIT model by name";
    case torch_jit_model_list_methods:
        return L"List available methods in JIT model";
    case torch_jit_model_list_buffers:
        return L"List buffers in JIT model";
    case torch_jit_model_to_mps:
        return L"Move JIT model to MPS device";
    case torch_jit_model_to_best_device:
        return L"Move JIT model to best available device";
    case torch_jit_model_get_intermediate_states:
        return L"Get intermediate states from JIT model";
    case torch_jit_model_register_lora_hook:
        return L"Register LoRA hook for JIT model";
    case torch_jit_model_forward_with_lora:
        return L"Forward pass with LoRA on JIT model";
    case torch_jit_model_list_tensor_names:
        return L"List tensor names in JIT model";
    case torch_jit_model_list_parameter_names:
        return L"List parameter names in JIT model";
    case torch_jit_model_update_weight:
        return L"Update weight in JIT model";
    case torch_mps_synchronize:
        return L"Synchronize MPS operations";
    case torch_layer_norm_forward:
        return L"Forward pass through layer normalization";
    case torch_embedding_create:
        return L"Create an embedding layer";
    case torch_embedding_forward:
        return L"Forward pass through embedding layer";
    case torch_linear_create:
        return L"Create a linear (fully connected) layer";
    case torch_linear_forward:
        return L"Forward pass through linear layer";
    case torch_set_device:
        return L"Set the active CUDA device";
    case torch_tensor_to_cuda:
        return L"Move tensor to CUDA device";
    case torch_tensor_to_device:
        return L"Move tensor to device";
    case torch_get_best_device:
        return L"Get best available device (CUDA, MPS, or CPU)";
    case torch_tokenizer_simple_create:
        return L"Create a simple word-based tokenizer";
    case torch_tokenizer_sentencepiece_create:
        return L"Create a SentencePiece tokenizer from model file";
    case torch_tokenizer_sentencepiece_train:
        return L"Train a new SentencePiece model";
    case torch_tokenizer_encode:
        return L"Encode text to token IDs";
    case torch_tokenizer_decode:
        return L"Decode token IDs to text";
    case torch_vocabulary_size:
        return L"Get tokenizer vocabulary size";
    case torch_pad_sequence:
        return L"Pad sequences to same length";
    case torch_create_attention_mask:
        return L"Create attention mask for sequences";
    case torch_positional_encoding_create:
        return L"Create positional encoding for sequences";
    case torch_positional_encoding_forward:
        return L"Apply positional encoding to input";
    case torch_rotary_embedding_create:
        return L"Create rotary position embedding (RoPE)";
    case torch_rotary_embedding_forward:
        return L"Generate cos/sin tensors for rotary embedding";
    case torch_apply_rotary_pos_emb:
        return L"Apply rotary position embedding to query/key tensors";
    case torch_checkpoint_enable:
        return L"Enable gradient checkpointing for memory optimization";
    case torch_checkpoint_disable:
        return L"Disable gradient checkpointing";
    case torch_checkpoint_forward:
        return L"Forward pass with gradient checkpointing";
    case torch_checkpoint_create:
        return L"Create checkpointed module for memory optimization";
    case torch_load_model:
        return L"Load a PyTorch model from file";
    case torch_save_model:
        return L"Save a PyTorch model to file";
    case torch_load_checkpoint:
        return L"Load a training checkpoint";
    case torch_save_checkpoint:
        return L"Save a training checkpoint";
    case torch_load_state_dict:
        return L"Load model parameters from state dictionary";
    case torch_model_state_dict:
        return L"Get model state dictionary";
    case torch_lora_linear_create:
        return L"Create LoRA (Low-Rank Adaptation) linear layer";
    case torch_lora_linear_forward:
        return L"Forward pass through LoRA linear layer";
    case torch_lora_apply_to_linear:
        return L"Apply LoRA adaptation to existing linear layer";
    case torch_lora_merge_weights:
        return L"Merge LoRA weights into original layer";
    case torch_lora_get_trainable_params:
        return L"Get trainable parameters for LoRA fine-tuning";
    case torch_lora_save_adapters:
        return L"Save LoRA adapter weights";
    case torch_lora_load_adapters:
        return L"Load LoRA adapter weights";
    case torch_tucker_decomposition:
        return L"Perform Tucker decomposition of a tensor";
    case torch_tucker_reconstruct:
        return L"Reconstruct tensor from Tucker decomposition";
    case torch_tucker_compression_ratio:
        return L"Calculate compression ratio of Tucker decomposition";
    case torch_khatri_rao_product:
        return L"Compute Khatri-Rao product of two matrices (torch_khatri_rao_product A B)";
    case torch_lora_compute_delta:
        return L"Compute LoRA weight delta for analysis";
    case torch_lora_get_adaptation_magnitude:
        return L"Get magnitude of LoRA adaptation";
    case torch_lora_forward_with_gradients:
        return L"Forward pass through LoRA with gradient computation";
    case torch_flash_attention_create:
        return L"Create Flash Attention module for efficient memory usage";
    case torch_flash_attention_forward:
        return L"Forward pass through Flash Attention";
    case torch_flash_attention_with_mask:
        return L"Flash Attention forward pass with attention mask";
    case torch_flash_attention_with_dropout:
        return L"Flash Attention forward pass with custom dropout";
    case torch_flash_attention:
        return L"Simple Flash Attention with automatic configuration";
    case torch_scaled_dot_product_attention:
        return L"Scaled dot-product attention (native PyTorch implementation)";
    // === Descriptions génération de texte ===
    case torch_generator_create:
        return L"Create a text generator with a PyTorch model";
    case torch_generator_config:
        return L"Configure text generation parameters";
    case torch_generate:
        return L"Generate text using specified sampling strategy";
    case torch_set_generation_params:
        return L"Set generation parameters (max_length, temperature, top_k, top_p)";
    // === Descriptions sampling et génération avancée ===
    case torch_topk:
        return L"Get top-k values and indices from tensor";
    case torch_multinomial:
        return L"Sample from multinomial distribution";
    case torch_sort:
        return L"Sort tensor values along specified dimension";
    // === Descriptions pour quantification ===
    case torch_quantize_dynamic:
        return L"Apply dynamic quantization to tensor (automatic scaling)";
    case torch_quantize_static:
        return L"Apply static quantization to tensor with pre-computed scale/zero_point";
    case torch_dequantize:
        return L"Convert quantized tensor back to floating point";
    case torch_quantize_linear:
        return L"Linear quantization with specified scale and zero_point";
    case torch_quantize_per_channel:
        return L"Per-channel quantization for better accuracy";
    case torch_quantize_int8:
        return L"Quantize tensor to 8-bit integer format";
    case torch_quantize_fp16:
        return L"Convert tensor to half-precision (16-bit) floating point";
    case torch_tensor_to_int8:
        return L"Convert tensor data type to int8";
    case torch_tensor_to_fp16:
        return L"Convert tensor data type to float16/half";
    case torch_model_quantize_dynamic:
        return L"Apply dynamic quantization to entire model";
    case torch_model_quantize_static:
        return L"Apply static quantization to entire model";        
    case torch_lr_scheduler_create:
        return L"Create learning rate scheduler with warmup and decay";
    case torch_lr_scheduler_step:
        return L"Advance scheduler by one step and update learning rate";
    case torch_lr_scheduler_get_lr:
        return L"Get current learning rate from scheduler";
    case torch_lr_scheduler_set_lr:
        return L"Manually set learning rate in scheduler";
    case torch_lr_scheduler_linear_warmup_cosine:
        return L"Linear warmup followed by cosine annealing scheduler";
    case torch_lr_scheduler_cosine:
        return L"Cosine annealing learning rate scheduler";
    case torch_lr_scheduler_exponential:
        return L"Exponential decay learning rate scheduler";
    case torch_lr_scheduler_step_decay:
        return L"Step decay learning rate scheduler";
    case torch_lr_scheduler_linear:
        return L"Linear learning rate scheduler";
    // === Descriptions HuggingFace ===
    case torch_hf_load_model:
        return L"Load HuggingFace model directly from safetensors (avoids TorchScript limitations)";
    case torch_hf_model_info:
        return L"Get configuration information about loaded HuggingFace model";
    case torch_hf_list_weights:
        return L"List all weight tensor names in HuggingFace model";
    case torch_hf_get_weight:
        return L"Get specific weight tensor from HuggingFace model";
    case torch_hf_get_q_weight:
        return L"Get Q projection weight tensor from HuggingFace model layer";
    case torch_hf_get_k_weight:
        return L"Get K projection weight tensor from HuggingFace model layer";
    case torch_hf_get_v_weight:
        return L"Get V projection weight tensor from HuggingFace model layer";
    case torch_hf_get_o_weight:
        return L"Get O projection weight tensor from HuggingFace model layer";
    case torch_hf_get_qkv_fused_weight:
        return L"Get QKV fused weight tensor from HuggingFace model layer (for optimized attention)";
    case torch_hf_get_gate_weight:
        return L"Get Gate projection weight tensor from HuggingFace model layer (MLP component)";
    case torch_hf_get_up_weight:
        return L"Get Up projection weight tensor from HuggingFace model layer (MLP component)";
    case torch_hf_get_down_weight:
        return L"Get Down projection weight tensor from HuggingFace model layer (MLP component)";
    case torch_hf_get_gate_up_fused_weight:
        return L"Get Gate+Up fused weight tensor from HuggingFace model layer (for optimized MLP)";
    case torch_hf_get_rms_norm_eps:
        return L"Get RMS normalization epsilon value from HuggingFace model configuration";
    case torch_hf_forward:
        return L"Forward pass through HuggingFace model (no dimension limitations)";
    case torch_hf_load_model_lora:
        return L"Load HuggingFace model with LoRA support enabled";
    case torch_hf_lora_init:
        return L"Initialize LoRA adapters for a loaded model";
    case torch_hf_lora_get_parameters:
        return L"Get all trainable LoRA parameters for optimizer";
    case torch_hf_lora_save:
        return L"Save LoRA adapters to disk";
    case torch_hf_lora_load:
        return L"Load LoRA adapters from disk";
    case torch_hf_lora_merge:
        return L"Merge LoRA weights into base model (for deployment)";
    case torch_hf_lora_unmerge:
        return L"Unmerge LoRA weights from base model";
    case torch_hf_lora_enable:
        return L"Enable or disable LoRA during forward pass";
    case torch_hf_memory_usage:
        return L"Get memory usage of loaded HuggingFace model in GB";
    case torch_hf_model_summary:
        return L"Get detailed summary of HuggingFace model architecture";
    case torch_hf_enable_kv_cache:
        return L"Enable/disable KV-Cache for faster autoregressive generation";
    case torch_hf_reset_kv_cache:
        return L"Reset KV-Cache to start a new generation sequence";
    case torch_hf_embeddings:
        return L"Get embeddings for token IDs from HuggingFace model";
    case torch_hf_generate:
        return L"Generate text using HuggingFace model with KV-Cache optimization";
    case torch_hf_forward_manual:
        return L"Forward pass using manual attention implementation for analysis";
    case torch_hf_forward_attention_scores:
        return L"Retrieve attention scores from the last manual forward pass";
    case torch_hf_forward_attention_size:
        return L"Retrieve the number of attention layers from the last manual forward pass";
    case torch_hf_clear_attention_scores:
        return L"Clear stored attention scores to free memory";
    default:
        return L"PyTorch tensor operation";
    }
}

extern "C" {
    Exporting bool InitialisationModule(LispE* lisp) {
        //A small equivalence
        lisp->delegation->string_to_code[U"torch_tensor_at"] = l_at;

        HFInferenceContext::t_context_inference =  lisp->createNewType(U"context_inference_");
        TorchTensor::tensor_type = lisp->createNewType(U"torch_tensor_");
        TorchModel::model_type = lisp->createNewType(U"torch_model_");
        TorchOptimizer::optimizer_type = lisp->createNewType(U"torch_optimizer_");
        TorchMultiHeadAttention::attention_type = lisp->createNewType(U"torch_attention_");
        TorchLayerNorm::layernorm_type = lisp->createNewType(U"torch_layernorm_");
        TorchEmbedding::embedding_type = lisp->createNewType(U"torch_embedding_");
        TorchLinear::linear_type = lisp->createNewType(U"torch_linear_");
        TorchTransformerBlock::transformer_block_type = lisp->createNewType(U"torch_transformer_block_");
        TorchTokenizer::tokenizer_type = lisp->createNewType(U"torch_tokenizer_");
        TorchPositionalEncoding::positional_encoding_type = lisp->createNewType(U"torch_positional_encoding_");
        TorchRotaryEmbedding::rotary_embedding_type = lisp->createNewType(U"torch_rotary_embedding_");
        TorchCheckpointedModule::checkpointed_module_type = lisp->createNewType(U"torch_checkpointed_module_");
        TorchStateDict::state_dict_type = lisp->createNewType(U"torch_state_dict_");
        TorchLoRALinear::lora_linear_type = lisp->createNewType(U"torch_lora_linear_");
        TorchLoRAConfig::lora_config_type = lisp->createNewType(U"torch_lora_config_");
        TorchFlashAttention::flash_attention_type = lisp->createNewType(U"torch_flash_attention_");
        TorchJITModel::jit_model_type = lisp->createNewType(U"jit_model_");
        TorchLRScheduler::scheduler_type = lisp->createNewType(U"torch_lrs_scheduler_");

        // Enregistrement des fonctions tensor
        lisp->extension("deflib torch_tensor(thedata)", new Lispe_lispetorch(torch_tensor_create));
        lisp->extension("deflib tensor_create(thedata)", new Lispe_lispetorch(torch_tensor_create));
        lisp->extension("deflib tensor_zeros(shape)", new Lispe_lispetorch(torch_tensor_zeros));
        lisp->extension("deflib tensor_to_list(tensor)", new Lispe_lispetorch(torch_tensor_to_lispe));
        lisp->extension("deflib tensor_ones(shape)", new Lispe_lispetorch(torch_tensor_ones));
        lisp->extension("deflib tensor_full(shape value)", new Lispe_lispetorch(torch_tensor_full));
        lisp->extension("deflib tensor_triu(tensor diagonal)", new Lispe_lispetorch(torch_tensor_triu));
        lisp->extension("deflib tensor_set_item(tensor indices value)", new Lispe_lispetorch(torch_tensor_set_item));
        lisp->extension("deflib tensor_randn(shape)", new Lispe_lispetorch(torch_tensor_randn));
        lisp->extension("deflib tensor_randint(low high shape)", new Lispe_lispetorch(torch_tensor_randint));
        lisp->extension("deflib tensor_transpose(tensor dim0 dim1)", new Lispe_lispetorch(torch_tensor_transpose));
        lisp->extension("deflib tensor_add(tensor1 tensor2)", new Lispe_lispetorch(torch_tensor_add));
        lisp->extension("deflib tensor_sub(tensor1 tensor2)", new Lispe_lispetorch(torch_tensor_sub));
        lisp->extension("deflib tensor_mul(tensor1 tensor2)", new Lispe_lispetorch(torch_tensor_mul));
        lisp->extension("deflib tensor_div(tensor1 tensor2)", new Lispe_lispetorch(torch_tensor_div));
        lisp->extension("deflib tensor_mul_scalar(tensor scalar)", new Lispe_lispetorch(torch_tensor_mul_scalar));
        lisp->extension("deflib tensor_add_scalar(tensor scalar)", new Lispe_lispetorch(torch_tensor_add_scalar));
        lisp->extension("deflib tensor_div_scalar(tensor scalar)", new Lispe_lispetorch(torch_tensor_div_scalar));
        lisp->extension("deflib tensor_matmul(tensor1 tensor2)", new Lispe_lispetorch(torch_tensor_matmul));
        lisp->extension("deflib tensor_linear(tensor1 tensor2)", new Lispe_lispetorch(torch_tensor_linear));
        lisp->extension("deflib tensor_reshape(tensor shape)", new Lispe_lispetorch(torch_tensor_reshape));
        lisp->extension("deflib tensor_contiguous(tensor)", new Lispe_lispetorch(torch_tensor_contiguous));
        lisp->extension("deflib tensor_unsqueeze(tensor dim)", new Lispe_lispetorch(torch_tensor_unsqueeze));
        lisp->extension("deflib tensor_squeeze(tensor dim)", new Lispe_lispetorch(torch_tensor_squeeze));
        lisp->extension("deflib tensor_slice(tensor dim start end)", new Lispe_lispetorch(torch_tensor_slice));
        lisp->extension("deflib tensor_select(tensor dim index)", new Lispe_lispetorch(torch_tensor_select));
        lisp->extension("deflib tensor_sum(tensor)", new Lispe_lispetorch(torch_tensor_sum));
        lisp->extension("deflib tensor_mean(tensor)", new Lispe_lispetorch(torch_tensor_mean));
        lisp->extension("deflib tensor_mean_dim(tensor dim)", new Lispe_lispetorch(torch_tensor_mean_dim));
        lisp->extension("deflib tensor_std(tensor)", new Lispe_lispetorch(torch_tensor_std));
        lisp->extension("deflib tensor_max(tensor)", new Lispe_lispetorch(torch_tensor_max));
        lisp->extension("deflib tensor_min(tensor)", new Lispe_lispetorch(torch_tensor_min));
        lisp->extension("deflib tensor_argmax(tensor dim (keepdim true))", new Lispe_lispetorch(torch_tensor_argmax));
        lisp->extension("deflib tensor_einsum(indices tensors)", new Lispe_lispetorch(torch_tensor_einsum));
        lisp->extension("deflib tensor_relu(tensor)", new Lispe_lispetorch(torch_tensor_relu));
        lisp->extension("deflib tensor_sigmoid(tensor)", new Lispe_lispetorch(torch_tensor_sigmoid));
        lisp->extension("deflib tensor_tanh(tensor)", new Lispe_lispetorch(torch_tensor_tanh));
        lisp->extension("deflib tensor_gelu(tensor)", new Lispe_lispetorch(torch_tensor_gelu));
        lisp->extension("deflib tensor_silu(tensor)", new Lispe_lispetorch(torch_tensor_silu));
        lisp->extension("deflib tensor_abs(tensor)", new Lispe_lispetorch(torch_tensor_abs));
        lisp->extension("deflib tensor_exp(tensor)", new Lispe_lispetorch(torch_tensor_exp));
        lisp->extension("deflib tensor_softmax(tensor dim)", new Lispe_lispetorch(torch_tensor_softmax));
        lisp->extension("deflib tensor_log_softmax(tensor dim)", new Lispe_lispetorch(torch_tensor_log_softmax));
        
        // Nouvelles fonctions mathématiques
        lisp->extension("deflib tensor_pow(tensor exponent)", new Lispe_lispetorch(torch_tensor_pow));
        lisp->extension("deflib tensor_sqrt(tensor)", new Lispe_lispetorch(torch_tensor_sqrt));
        lisp->extension("deflib tensor_rsqrt(tensor)", new Lispe_lispetorch(torch_tensor_rsqrt));
        lisp->extension("deflib tensor_log(tensor)", new Lispe_lispetorch(torch_tensor_log));
        lisp->extension("deflib tensor_log10(tensor)", new Lispe_lispetorch(torch_tensor_log10));
        lisp->extension("deflib tensor_log2(tensor)", new Lispe_lispetorch(torch_tensor_log2));
        lisp->extension("deflib tensor_sin(tensor)", new Lispe_lispetorch(torch_tensor_sin));
        lisp->extension("deflib tensor_cos(tensor)", new Lispe_lispetorch(torch_tensor_cos));
        lisp->extension("deflib tensor_tan(tensor)", new Lispe_lispetorch(torch_tensor_tan));
        lisp->extension("deflib tensor_asin(tensor)", new Lispe_lispetorch(torch_tensor_asin));
        lisp->extension("deflib tensor_acos(tensor)", new Lispe_lispetorch(torch_tensor_acos));
        lisp->extension("deflib tensor_atan(tensor)", new Lispe_lispetorch(torch_tensor_atan));
        lisp->extension("deflib tensor_sinh(tensor)", new Lispe_lispetorch(torch_tensor_sinh));
        lisp->extension("deflib tensor_cosh(tensor)", new Lispe_lispetorch(torch_tensor_cosh));
        lisp->extension("deflib tensor_floor(tensor)", new Lispe_lispetorch(torch_tensor_floor));
        lisp->extension("deflib tensor_ceil(tensor)", new Lispe_lispetorch(torch_tensor_ceil));
        lisp->extension("deflib tensor_round(tensor)", new Lispe_lispetorch(torch_tensor_round));
        lisp->extension("deflib tensor_clamp(tensor min_val max_val)", new Lispe_lispetorch(torch_tensor_clamp));
        lisp->extension("deflib tensor_neg(tensor)", new Lispe_lispetorch(torch_tensor_neg));
        lisp->extension("deflib tensor_reciprocal(tensor)", new Lispe_lispetorch(torch_tensor_reciprocal));
        lisp->extension("deflib tensor_rms_norm(input weight (eps 1e-6))", new Lispe_lispetorch(torch_tensor_rms_norm));
        
        // Nouvelles fonctions tensor
        lisp->extension("deflib tensor_full_like(tensor fill_value)", new Lispe_lispetorch(torch_tensor_full_like));
        lisp->extension("deflib tensor_cumsum(tensor dim)", new Lispe_lispetorch(torch_tensor_cumsum));
        lisp->extension("deflib tensor_gather(input dim index)", new Lispe_lispetorch(torch_tensor_gather));
        lisp->extension("deflib tensor_masked_fill_(tensor mask value)", new Lispe_lispetorch(torch_tensor_masked_fill_));
        
        lisp->extension("deflib tensor_size(tensor)", new Lispe_lispetorch(torch_tensor_size));
        lisp->extension("deflib tensor_shape(tensor)", new Lispe_lispetorch(torch_tensor_shape));
        lisp->extension("deflib tensor_item(tensor)", new Lispe_lispetorch(torch_tensor_item));
        lisp->extension("deflib tensor_cat(tensors dim)", new Lispe_lispetorch(torch_tensor_cat));

        // Enregistrement des fonctions model et training
        lisp->extension("deflib torch_model(input_size hidden_size output_size)", 
                       new Lispe_lispetorch(torch_model_create));
        lisp->extension("deflib torch_forward(model input_data)", 
                       new Lispe_lispetorch(torch_model_forward));
        lisp->extension("deflib torch_optimizer(model learning_rate type)", 
                       new Lispe_lispetorch(torch_optimizer_create));
        lisp->extension("deflib torch_adam_optimizer(learning_rate)", 
                       new Lispe_lispetorch(torch_adam_optimizer));
        lisp->extension("deflib torch_adamw_optimizer(learning_rate)", 
                       new Lispe_lispetorch(torch_adamw_optimizer));
        lisp->extension("deflib torch_sgd_optimizer(learning_rate)", 
                       new Lispe_lispetorch(torch_sgd_optimizer));
        lisp->extension("deflib torch_optimizer_step(optimizer)",
                       new Lispe_lispetorch(torch_optimizer_step));
        lisp->extension("deflib torch_optimizer_zero_grad(optimizer)",
                       new Lispe_lispetorch(torch_optimizer_zero_grad));
        lisp->extension("deflib torch_clip_grad_norm(optimizer max_norm)",
                       new Lispe_lispetorch(torch_clip_grad_norm));
        lisp->extension("deflib torch_optimizer_add_params(params learning_rate weight_decay)",
                       new Lispe_lispetorch(torch_optimizer_add_params));
        lisp->extension("deflib torch_mse_loss(predictions targets)", 
                       new Lispe_lispetorch(torch_loss_mse));
        lisp->extension("deflib torch_crossentropy_loss(predictions targets)", 
                       new Lispe_lispetorch(torch_loss_crossentropy));
        lisp->extension("deflib torch_cross_entropy(predictions targets)", 
                       new Lispe_lispetorch(torch_cross_entropy));
        lisp->extension("deflib torch_backward(loss (retain_graph) (create_graph))",
                       new Lispe_lispetorch(torch_backward));
        lisp->extension("deflib torch_set_grad_enabled(enabled)",
                       new Lispe_lispetorch(torch_set_grad_enabled));
        
        // Enregistrement des fonctions CUDA
        lisp->extension("deflib torch_cuda_is_available()", 
                       new Lispe_lispetorch(torch_cuda_is_available));
        lisp->extension("deflib torch_cuda_device_count()", 
                       new Lispe_lispetorch(torch_cuda_device_count));
        lisp->extension("deflib torch_set_device(device)", 
                       new Lispe_lispetorch(torch_set_device));
        lisp->extension("deflib torch_to_cuda(tensor (device))", 
                       new Lispe_lispetorch(torch_tensor_to_cuda));
        lisp->extension("deflib torch_to_device(tensor device)", 
                       new Lispe_lispetorch(torch_tensor_to_device));
        lisp->extension("deflib torch_cuda_memory_allocated()", 
                        new Lispe_lispetorch(torch_cuda_memory_allocated));
        lisp->extension("deflib torch_cuda_memory_total()", 
                        new Lispe_lispetorch(torch_cuda_memory_total));
        // Enregistrement des fonctions MPS (macOS Metal)
        lisp->extension("deflib torch_mps_is_available()", 
                       new Lispe_lispetorch(torch_mps_is_available));
        lisp->extension("deflib torch_to_mps(tensor)", 
                       new Lispe_lispetorch(torch_to_mps));
        // Enregistrement des nouvelles fonctions CPU/CUDA
        lisp->extension("deflib torch_to_cpu(tensor)", new Lispe_lispetorch(torch_tensor_to_cpu));
        lisp->extension("deflib torch_cuda_empty_cache()", new Lispe_lispetorch(torch_cuda_empty_cache));
        lisp->extension("deflib torch_on_mps(tensor)",
                       new Lispe_lispetorch(torch_is_mps));

        // Fonction utilitaire de détection automatique
        lisp->extension("deflib torch_get_best_device()", 
                       new Lispe_lispetorch(torch_get_best_device));
        
        // Enregistrement des fonctions Transformer
        lisp->extension("deflib torch_multihead_attention(embed_dim num_heads)", 
                       new Lispe_lispetorch(torch_multihead_attention_create));
        lisp->extension("deflib torch_attention_forward(attention query key value)", 
                       new Lispe_lispetorch(torch_multihead_attention_forward));
        lisp->extension("deflib torch_layer_norm(normalized_shape)", 
                       new Lispe_lispetorch(torch_layer_norm_create));
        lisp->extension("deflib torch_layer_norm_forward(layer_norm input_data)", 
                       new Lispe_lispetorch(torch_layer_norm_forward));
        lisp->extension("deflib torch_embedding(num_embeddings embedding_dim)", 
                       new Lispe_lispetorch(torch_embedding_create));
        lisp->extension("deflib torch_embedding_forward(embedding input_data)", 
                       new Lispe_lispetorch(torch_embedding_forward));
        lisp->extension("deflib torch_linear(in_features out_features)", 
                       new Lispe_lispetorch(torch_linear_create));
        lisp->extension("deflib torch_linear_forward(linear input_data)", 
                       new Lispe_lispetorch(torch_linear_forward));
        lisp->extension("deflib torch_transformer_block(embed_dim num_heads ffn_dim)", 
                       new Lispe_lispetorch(torch_transformer_block_create));
        lisp->extension("deflib torch_transformer_forward(block input_data)", 
                       new Lispe_lispetorch(torch_transformer_block_forward));
        
        // Enregistrement des fonctions Tokenization
        lisp->extension("deflib torch_simple_tokenizer()", 
                       new Lispe_lispetorch(torch_tokenizer_simple_create));
        lisp->extension("deflib torch_sentencepiece_tokenizer(model_path)", 
                       new Lispe_lispetorch(torch_tokenizer_sentencepiece_create));
        lisp->extension("deflib torch_train_sentencepiece(input_file model_prefix vocab_size model_type)", 
                       new Lispe_lispetorch(torch_tokenizer_sentencepiece_train));
        lisp->extension("deflib torch_encode(tokenizer text)", 
                       new Lispe_lispetorch(torch_tokenizer_encode));
        lisp->extension("deflib torch_decode(tokenizer token_ids)", 
                       new Lispe_lispetorch(torch_tokenizer_decode));
        lisp->extension("deflib torch_vocab_size(tokenizer)", 
                       new Lispe_lispetorch(torch_vocabulary_size));
        lisp->extension("deflib torch_pad_sequences(sequences max_length pad_token)", 
                       new Lispe_lispetorch(torch_pad_sequence));
        lisp->extension("deflib torch_attention_mask(sequences pad_token)", 
                       new Lispe_lispetorch(torch_create_attention_mask));
        
        // Enregistrement des fonctions Positional Encoding
        lisp->extension("deflib torch_positional_encoding(d_model max_len)", 
                       new Lispe_lispetorch(torch_positional_encoding_create));
        lisp->extension("deflib torch_positional_forward(positional_encoding input_data)", 
                       new Lispe_lispetorch(torch_positional_encoding_forward));
        
        // Enregistrement des fonctions Rotary Embedding (RoPE)
        lisp->extension("deflib torch_rotary_embedding(dim max_seq_len)", 
                       new Lispe_lispetorch(torch_rotary_embedding_create));
        lisp->extension("deflib torch_rotary_forward(rotary_embedding seq_len device)", 
                       new Lispe_lispetorch(torch_rotary_embedding_forward));
        lisp->extension("deflib torch_apply_rotary_pos_emb(tensor cos sin)", 
                       new Lispe_lispetorch(torch_apply_rotary_pos_emb));
        
        // Enregistrement des fonctions Gradient Checkpointing
        lisp->extension("deflib torch_checkpoint_create(module)", 
                       new Lispe_lispetorch(torch_checkpoint_create));
        lisp->extension("deflib torch_checkpoint_enable(module)", 
                       new Lispe_lispetorch(torch_checkpoint_enable));
        lisp->extension("deflib torch_checkpoint_disable(module)", 
                       new Lispe_lispetorch(torch_checkpoint_disable));
        lisp->extension("deflib torch_checkpoint_forward(module input_data)", 
                       new Lispe_lispetorch(torch_checkpoint_forward));
        
        // Enregistrement des fonctions Model Loading
        lisp->extension("deflib torch_save_model(model path)", 
                       new Lispe_lispetorch(torch_save_model));
        lisp->extension("deflib torch_load_model(model path)", 
                       new Lispe_lispetorch(torch_load_model));
        lisp->extension("deflib torch_state_dict(model)", 
                       new Lispe_lispetorch(torch_model_state_dict));
        lisp->extension("deflib torch_load_state_dict(model state_dict)", 
                       new Lispe_lispetorch(torch_load_state_dict));
        lisp->extension("deflib torch_save_checkpoint(model optimizer epoch path)", 
                       new Lispe_lispetorch(torch_save_checkpoint));
        lisp->extension("deflib torch_load_checkpoint(path)", 
                       new Lispe_lispetorch(torch_load_checkpoint));
        
        // Enregistrement des fonctions LoRA Fine-tuning
        lisp->extension("deflib torch_lora_linear(in_features out_features rank alpha)", 
                       new Lispe_lispetorch(torch_lora_linear_create));
        lisp->extension("deflib torch_lora_forward(lora_layer input_data)", 
                       new Lispe_lispetorch(torch_lora_linear_forward));
        lisp->extension("deflib torch_lora_apply_to_linear(linear_layer rank alpha)", 
                       new Lispe_lispetorch(torch_lora_apply_to_linear));
        lisp->extension("deflib torch_lora_merge_weights(lora_layer)", 
                       new Lispe_lispetorch(torch_lora_merge_weights));
        lisp->extension("deflib torch_lora_trainable_params(model)", 
                       new Lispe_lispetorch(torch_lora_get_trainable_params));
        lisp->extension("deflib torch_lora_save_adapters(model path)", 
                       new Lispe_lispetorch(torch_lora_save_adapters));
        lisp->extension("deflib torch_lora_load_adapters(model path)", 
                       new Lispe_lispetorch(torch_lora_load_adapters));
        
        // ==================== Flash Attention Functions ====================
        lisp->extension("deflib torch_flash_attention_create(embed_dim num_heads dropout bias)", 
                       new Lispe_lispetorch(torch_flash_attention_create));
        lisp->extension("deflib torch_flash_attention_forward(flash_attention query key value)", 
                       new Lispe_lispetorch(torch_flash_attention_forward));
        lisp->extension("deflib torch_flash_attention_with_mask(flash_attention query key value attn_mask)", 
                       new Lispe_lispetorch(torch_flash_attention_with_mask));
        lisp->extension("deflib torch_flash_attention_with_dropout(flash_attention query key value dropout_p training)", 
                       new Lispe_lispetorch(torch_flash_attention_with_dropout));
        lisp->extension("deflib torch_flash_attention(query key value)", 
                       new Lispe_lispetorch(torch_flash_attention));
        lisp->extension("deflib torch_scaled_dot_product_attention(query key value attn_mask dropout_p is_causal scale)", 
                       new Lispe_lispetorch(torch_scaled_dot_product_attention));
        
        // ==================== Text Generation Functions ====================
        lisp->extension("deflib torch_generator_create(model device)", 
                       new Lispe_lispetorch(torch_generator_create));
        lisp->extension("deflib torch_generator_config(generator config)", 
                       new Lispe_lispetorch(torch_generator_config));
        lisp->extension("deflib torch_generate(generator input_ids strategy)", 
                       new Lispe_lispetorch(torch_generate));
        lisp->extension("deflib torch_set_generation_params(generator max_length temperature top_k top_p)", 
                       new Lispe_lispetorch(torch_set_generation_params));
        
        // === Fonctions sampling et génération avancée ===
        lisp->extension("deflib torch_topk(tensor k dim largest)", 
                       new Lispe_lispetorch(torch_topk));
        lisp->extension("deflib torch_multinomial(probs num_samples replacement)", 
                       new Lispe_lispetorch(torch_multinomial));
        lisp->extension("deflib torch_sort(tensor dim descending)", 
                       new Lispe_lispetorch(torch_sort));
        
        // === Fonctions de quantification ===
        lisp->extension("deflib torch_quantize_dynamic(tensor dtype)", 
                       new Lispe_lispetorch(torch_quantize_dynamic));
        lisp->extension("deflib torch_quantize_static(tensor scale zero_point dtype)", 
                       new Lispe_lispetorch(torch_quantize_static));
        lisp->extension("deflib torch_dequantize(quantized_tensor)", 
                       new Lispe_lispetorch(torch_dequantize));
        lisp->extension("deflib torch_quantize_linear(tensor scale zero_point)", 
                       new Lispe_lispetorch(torch_quantize_linear));
        lisp->extension("deflib torch_quantize_per_channel(tensor scales zero_points axis)", 
                       new Lispe_lispetorch(torch_quantize_per_channel));
        lisp->extension("deflib torch_quantize_int8(tensor)", 
                       new Lispe_lispetorch(torch_quantize_int8));
        lisp->extension("deflib torch_quantize_fp16(tensor)", 
                       new Lispe_lispetorch(torch_quantize_fp16));
        lisp->extension("deflib torch_tensor_to_int8(tensor)", 
                       new Lispe_lispetorch(torch_tensor_to_int8));
        lisp->extension("deflib torch_tensor_to_fp16(tensor)", 
                       new Lispe_lispetorch(torch_tensor_to_fp16));
        lisp->extension("deflib torch_model_quantize_dynamic(model)", 
                       new Lispe_lispetorch(torch_model_quantize_dynamic));
        lisp->extension("deflib torch_model_quantize_static(model calibration_data)", 
                       new Lispe_lispetorch(torch_model_quantize_static));

        // === Fonctions de décomposition de Tucker ===
        lisp->extension("deflib torch_tucker_decomposition(tensor rank (max_iter 100) (tol 1e-6))", 
                       new Lispe_lispetorch(torch_tucker_decomposition));
        lisp->extension("deflib torch_tucker_reconstruct(core factors)", 
                       new Lispe_lispetorch(torch_tucker_reconstruct));
        lisp->extension("deflib torch_tucker_compression_ratio(original_shape core_shape factor_shapes)", 
                       new Lispe_lispetorch(torch_tucker_compression_ratio));
        lisp->extension("deflib torch_khatri_rao_product(A B)", 
                       new Lispe_lispetorch(torch_khatri_rao_product));

        lisp->extension("deflib torch_jit_load(model_path (device \"cpu\"))", 
            new Lispe_lispetorch(torch_jit_load));
        lisp->extension("deflib torch_jit_unload(model)", 
            new Lispe_lispetorch(torch_jit_unload));
        lisp->extension("deflib torch_jit_model_to_device(model (device \"cpu\"))", 
            new Lispe_lispetorch(torch_jit_model_to_device));

        lisp->extension("deflib torch_jit_model_forward(model tensor)", 
            new Lispe_lispetorch(torch_jit_model_forward));

        lisp->extension("deflib torch_jit_model_info(model)", 
            new Lispe_lispetorch(torch_jit_model_info));

        lisp->extension("deflib torch_jit_model_get_buffer(model buffername)", 
            new Lispe_lispetorch(torch_jit_model_get_buffer));

        lisp->extension("deflib torch_jit_model_get_tensor(model tensorname)", 
            new Lispe_lispetorch(torch_jit_model_get_tensor));

        lisp->extension("deflib torch_jit_model_get_tensor_shape(model tensorname)", 
            new Lispe_lispetorch(torch_jit_model_get_tensor_shape));

        lisp->extension("deflib torch_jit_model_list_methods(model)", 
            new Lispe_lispetorch(torch_jit_model_list_methods));

        lisp->extension("deflib torch_jit_model_list_buffers(model)", 
            new Lispe_lispetorch(torch_jit_model_list_buffers));
            

        lisp->extension("deflib torch_jit_model_to_mps(model)", new Lispe_lispetorch(torch_jit_model_to_mps));
        lisp->extension("deflib torch_jit_model_to_best_device(model)", new Lispe_lispetorch(torch_jit_model_to_best_device));
        lisp->extension("deflib torch_mps_synchronize((safemode))", new Lispe_lispetorch(torch_mps_synchronize));

        // 5. ENREGISTREMENT DES FONCTIONS
        // À ajouter dans InitialisationModule()

        lisp->extension("deflib torch_jit_model_register_lora_hook(model layer_name lora_layer)",
                        new Lispe_lispetorch(torch_jit_model_register_lora_hook));

        lisp->extension("deflib torch_jit_model_forward_with_lora(model input)",
                        new Lispe_lispetorch(torch_jit_model_forward_with_lora));

        lisp->extension("deflib torch_jit_model_get_intermediate_states(model input layer_names)",
                        new Lispe_lispetorch(torch_jit_model_get_intermediate_states));

        lisp->extension("deflib torch_jit_model_list_tensor_names(model)",
                        new Lispe_lispetorch(torch_jit_model_list_tensor_names));

        lisp->extension("deflib torch_jit_model_list_parameter_names(model)",
                        new Lispe_lispetorch(torch_jit_model_list_parameter_names));

        lisp->extension("deflib torch_jit_model_update_weight(model param_name new_weight)",
                        new Lispe_lispetorch(torch_jit_model_update_weight));

        lisp->extension("deflib torch_lora_compute_delta(lora_layer)",
                        new Lispe_lispetorch(torch_lora_compute_delta));

        lisp->extension("deflib torch_lora_get_adaptation_magnitude(lora_layer)",
                        new Lispe_lispetorch(torch_lora_get_adaptation_magnitude));

        lisp->extension("deflib torch_lora_forward_with_gradients(lora_layer input retain_graph)",
                        new Lispe_lispetorch(torch_lora_forward_with_gradients));

                        // Enregistrer les fonctions
        lisp->extension("deflib torch_lr_scheduler(optimizer scheduler_type config)",
                       new Lispe_lispetorch(torch_lr_scheduler_create));
        lisp->extension("deflib torch_scheduler_step(scheduler)",
                       new Lispe_lispetorch(torch_lr_scheduler_step));
        lisp->extension("deflib torch_scheduler_get_lr(scheduler)",
                       new Lispe_lispetorch(torch_lr_scheduler_get_lr));
        lisp->extension("deflib torch_scheduler_set_lr(scheduler learning_rate)",
                       new Lispe_lispetorch(torch_lr_scheduler_set_lr));

        // Enregistrer les nouvelles fonctions HuggingFace
        lisp->extension("deflib torch_hf_load_model(path (config nil))",
                       new Lispe_lispetorch(torch_hf_load_model));
        lisp->extension("deflib torch_hf_model_info(path)",
                       new Lispe_lispetorch(torch_hf_model_info));
        lisp->extension("deflib torch_hf_list_weights(path)",
                       new Lispe_lispetorch(torch_hf_list_weights));
        lisp->extension("deflib torch_hf_get_weight(path name)",
                       new Lispe_lispetorch(torch_hf_get_weight));
        lisp->extension("deflib torch_hf_get_q_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_q_weight));
        lisp->extension("deflib torch_hf_get_k_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_k_weight));
        lisp->extension("deflib torch_hf_get_v_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_v_weight));
        lisp->extension("deflib torch_hf_get_o_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_o_weight));
        lisp->extension("deflib torch_hf_get_qkv_fused_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_qkv_fused_weight));
        lisp->extension("deflib torch_hf_get_gate_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_gate_weight));
        lisp->extension("deflib torch_hf_get_up_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_up_weight));
        lisp->extension("deflib torch_hf_get_down_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_down_weight));
        lisp->extension("deflib torch_hf_get_gate_up_fused_weight(path layer)",
                       new Lispe_lispetorch(torch_hf_get_gate_up_fused_weight));
        lisp->extension("deflib torch_hf_get_rms_norm_eps(path)",
                       new Lispe_lispetorch(torch_hf_get_rms_norm_eps));
        lisp->extension("deflib torch_hf_forward(path input_ids (kvcache))",
                       new Lispe_lispetorch(torch_hf_forward));
        lisp->extension("deflib torch_hf_memory_usage(path)",
                       new Lispe_lispetorch(torch_hf_memory_usage));
        lisp->extension("deflib torch_hf_model_summary(path)",
                       new Lispe_lispetorch(torch_hf_model_summary));

        lisp->extension("deflib tensor_in_memory()",
            new Lispe_lispetorch(torch_tensor_in_memory));

        // LoRA functions
        lisp->extension("deflib torch_hf_load_model_lora(model_name path config)",
                       new Lispe_lispetorch(torch_hf_load_model_lora));
        lisp->extension("deflib torch_hf_lora_init(model_name rank alpha target_modules (dtype))",
                       new Lispe_lispetorch(torch_hf_lora_init));
        lisp->extension("deflib torch_hf_lora_get_parameters(model_name)",
                       new Lispe_lispetorch(torch_hf_lora_get_parameters));
        lisp->extension("deflib torch_hf_lora_save(model_name path)",
                       new Lispe_lispetorch(torch_hf_lora_save));
        lisp->extension("deflib torch_hf_lora_load(model_name path)",
                       new Lispe_lispetorch(torch_hf_lora_load));
        lisp->extension("deflib torch_hf_lora_merge(model_name)",
                       new Lispe_lispetorch(torch_hf_lora_merge));
        lisp->extension("deflib torch_hf_lora_unmerge(model_name)",
                       new Lispe_lispetorch(torch_hf_lora_unmerge));
        lisp->extension("deflib torch_hf_lora_enable(model_name enable)",
                       new Lispe_lispetorch(torch_hf_lora_enable));

        lisp->extension("deflib torch_hf_enable_kv_cache(path enable)",
                       new Lispe_lispetorch(torch_hf_enable_kv_cache));

        lisp->extension("deflib torch_hf_reset_kv_cache(kvcache)",
                       new Lispe_lispetorch(torch_hf_reset_kv_cache));

        lisp->extension("deflib torch_hf_embeddings(path token_ids)",
                       new Lispe_lispetorch(torch_hf_embeddings));

        lisp->extension("deflib torch_hf_generate(path initial_tokens eos_id max_length (options))",
                       new Lispe_lispetorch(torch_hf_generate));

        lisp->extension("deflib torch_hf_forward_manual(path input_ids (kvcache))",
                       new Lispe_lispetorch(torch_hf_forward_manual));

        lisp->extension("deflib torch_hf_forward_attention_scores(path layer_index (kvcache))",
                       new Lispe_lispetorch(torch_hf_forward_attention_scores));

        lisp->extension("deflib torch_hf_forward_attention_size(path (kvcache))",
                       new Lispe_lispetorch(torch_hf_forward_attention_size));

        lisp->extension("deflib torch_hf_clear_attention_scores(path (kvcache))",
                       new Lispe_lispetorch(torch_hf_clear_attention_scores));
                
        return true;
    }
}
