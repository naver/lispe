/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_lispetorch.cxx
//

#ifndef lispe_lispetorch_h
#define lispe_lispetorch_h

#include "lispe.h"
#include <torch/torch.h>
#include <sstream>
#include <unordered_map>

#ifdef USE_HUGGINGFACE
#include "lispe_hf_loader.h"
#include "lispe_hf_loader_manual.h"
#endif

#ifdef USE_CUDA
#endif

#ifdef USE_SENTENCEPIECE
#include <sentencepiece_processor.h>
#include <sentencepiece_trainer.h>
#endif


torch::Device lookup_device(string device);

// Énumérations pour les actions comme dans BLAS
enum torch_actions {
    torch_tensor_in_memory,
    torch_tensor_create,
    torch_tensor_to_lispe,
    torch_tensor_zeros,
    torch_tensor_full,
    torch_tensor_ones,
    torch_tensor_triu,
    torch_tensor_set_item,
    torch_tensor_randn,
    torch_tensor_rand,
    torch_tensor_randint,
    torch_tensor_transpose,
    torch_tensor_add,
    torch_tensor_sub,
    torch_tensor_mul,
    torch_tensor_div,
    torch_tensor_mul_scalar,
    torch_tensor_add_scalar,
    torch_tensor_div_scalar,
    torch_tensor_matmul,
    torch_tensor_linear,
    torch_tensor_size,
    torch_tensor_shape,
    torch_tensor_item,
    torch_tensor_reshape,
    torch_tensor_contiguous,
    torch_tensor_unsqueeze,
    torch_tensor_squeeze,
    torch_is_mps,
    torch_tensor_slice,
    torch_tensor_select,
    torch_tensor_cat,
    torch_tensor_to_device,
    torch_tensor_to_cuda,
    torch_tensor_sum,
    torch_tensor_mean,
    torch_tensor_mean_dim,
    torch_tensor_std,
    torch_tensor_max,
    torch_tensor_min,
    torch_tensor_argmax,
    torch_tensor_einsum,
    torch_tensor_relu,
    torch_tensor_sigmoid,
    torch_tensor_tanh,
    torch_tensor_gelu,
    torch_tensor_silu,
    torch_tensor_abs,
    torch_tensor_exp,
    torch_tensor_softmax,
    torch_tensor_log_softmax,
    // Nouvelles fonctions mathématiques
    torch_tensor_pow,
    torch_tensor_sqrt,
    torch_tensor_rsqrt,
    torch_tensor_log,
    torch_tensor_log10,
    torch_tensor_log2,
    torch_tensor_sin,
    torch_tensor_cos,
    torch_tensor_tan,
    torch_tensor_asin,
    torch_tensor_acos,
    torch_tensor_atan,
    torch_tensor_sinh,
    torch_tensor_cosh,
    torch_tensor_floor,
    torch_tensor_ceil,
    torch_tensor_round,
    torch_tensor_clamp,
    torch_tensor_neg,
    torch_tensor_reciprocal,
    torch_tensor_rms_norm,
    torch_linear_create,
    torch_linear_forward,
    torch_model_create,
    torch_model_forward,
    torch_model_parameters,
    torch_model_train,
    torch_model_eval,
    torch_optimizer_create,
    torch_adam_optimizer,
    torch_adamw_optimizer,
    torch_sgd_optimizer,
    torch_optimizer_step,
    torch_optimizer_zero_grad,
    torch_clip_grad_norm,
    torch_optimizer_add_params,
    torch_loss_mse,
    torch_loss_crossentropy,
    torch_cross_entropy,  // Alias for torch_loss_crossentropy
    torch_backward,
    torch_set_grad_enabled,
    torch_cuda_is_available,
    torch_cuda_device_count,
    torch_cuda_memory_allocated,
    torch_cuda_memory_total,
    torch_set_device,
    torch_mps_is_available,
    torch_to_mps,
    torch_get_best_device,
    torch_multihead_attention_create,
    torch_multihead_attention_forward,
    torch_layer_norm_create,
    torch_layer_norm_forward,
    torch_embedding_create,
    torch_embedding_forward,
    torch_transformer_block_create,
    torch_transformer_block_forward,
    // Nouvelles actions pour tokenization
    torch_tokenizer_simple_create,
    torch_tokenizer_sentencepiece_create,
    torch_tokenizer_sentencepiece_train,
    torch_tokenizer_encode,
    torch_tokenizer_decode,
    torch_vocabulary_size,
    torch_pad_sequence,
    torch_create_attention_mask,
    // Nouvelles actions pour positional embeddings
    torch_positional_encoding_create,
    torch_positional_encoding_forward,
    // Nouvelles actions pour rotary embeddings (RoPE)
    torch_rotary_embedding_create,
    torch_rotary_embedding_forward,
    torch_apply_rotary_pos_emb,
    // Nouvelles actions pour gradient checkpointing
    torch_checkpoint_enable,
    torch_checkpoint_disable,
    torch_checkpoint_forward,
    torch_checkpoint_create,
    // Nouvelles actions pour model loading
    torch_load_model,
    torch_save_model,
    torch_load_checkpoint,
    torch_save_checkpoint,
    torch_load_state_dict,
    torch_model_state_dict,
    // Nouvelles actions pour LoRA fine-tuning
    torch_lora_linear_create,
    torch_lora_linear_forward,
    torch_lora_apply_to_linear,
    torch_lora_merge_weights,
    torch_lora_get_trainable_params,
    torch_lora_save_adapters,
    torch_lora_load_adapters,
    // Nouvelles actions pour Flash Attention
    torch_flash_attention_create,
    torch_flash_attention_forward,
    torch_flash_attention_with_mask,
    torch_flash_attention_with_dropout,
    torch_flash_attention,
    torch_scaled_dot_product_attention,
    torch_hf_generate,
    // Nouvelles actions pour génération de texte
    torch_generator_create,
    torch_generator_config,
    torch_generate,
    torch_set_generation_params,
    // Nouvelles actions pour sampling et génération avancée
    torch_topk,
    torch_multinomial,
    torch_sort,
    // Nouvelles actions pour quantification
    torch_quantize_dynamic,
    torch_quantize_static,
    torch_dequantize,
    torch_quantize_linear,
    torch_quantize_per_channel,
    torch_quantize_int8,
    torch_quantize_fp16,
    torch_tensor_to_int8,
    torch_tensor_to_fp16,
    torch_mps_synchronize,
    torch_model_quantize_dynamic,
    torch_model_quantize_static,
    // --- Ajout : enums pour les nouvelles fonctions CPU/CUDA ---
    torch_tensor_to_cpu,
    torch_cuda_empty_cache,
    torch_jit_load,
    torch_jit_unload,
    torch_jit_model_to_device,
    torch_jit_model_forward,
    torch_jit_model_info,
    torch_jit_model_get_buffer,
    torch_jit_model_get_tensor,
    torch_jit_model_get_tensor_shape,
    torch_jit_model_list_parameter_names,
    torch_jit_model_list_methods,
    torch_jit_model_list_buffers,
    torch_jit_model_to_mps,
    torch_jit_model_to_best_device,
    
    torch_jit_model_get_intermediate_states,
    torch_jit_model_forward_single_layer,
    torch_jit_model_register_lora_hook,
    torch_jit_model_forward_with_lora,
    torch_jit_model_apply_lora_temporarily,
    torch_jit_model_list_tensor_names,
    torch_jit_model_update_weight,
    torch_jit_model_backup_weights,
    torch_jit_model_restore_weights,

    torch_lora_compute_delta,
    torch_lora_get_adaptation_magnitude,
    torch_lora_forward_with_gradients,

    torch_lr_scheduler_create,
    torch_lr_scheduler_step,
    torch_lr_scheduler_get_lr,
    torch_lr_scheduler_set_lr,
    torch_lr_scheduler_linear_warmup_cosine,
    
    // Nouvelles actions pour chargement direct Hugging Face
    torch_hf_load_model,
    torch_hf_model_info,
    torch_hf_list_weights,
    torch_hf_get_weight,
    torch_hf_get_q_weight,
    torch_hf_get_k_weight,
    torch_hf_get_v_weight,
    torch_hf_get_o_weight,
    torch_hf_get_qkv_fused_weight,
    torch_hf_get_gate_weight,
    torch_hf_get_up_weight,
    torch_hf_get_down_weight,
    torch_hf_get_gate_up_fused_weight,
    torch_hf_get_rms_norm_eps,
    torch_hf_forward,
    torch_hf_forward_manual,
    torch_hf_forward_attention_scores,
    torch_hf_forward_attention_size,
    torch_hf_clear_attention_scores,
    torch_hf_load_model_lora,
    torch_hf_lora_init,
    torch_hf_lora_get_parameters,
    torch_hf_lora_save,
    torch_hf_lora_load,
    torch_hf_lora_merge,
    torch_hf_lora_unmerge,
    torch_hf_lora_enable,
    torch_hf_memory_usage,
    torch_hf_model_summary,
    torch_hf_enable_kv_cache,
    torch_hf_reset_kv_cache,
    torch_hf_embeddings,
    torch_lr_scheduler_cosine,
    torch_lr_scheduler_exponential,
    torch_lr_scheduler_step_decay,
    torch_lr_scheduler_linear,
    
    // Nouvelles fonctions tensor
    torch_tensor_full_like,
    torch_tensor_cumsum,
    torch_tensor_gather,
    torch_tensor_masked_fill_,
    
    // Nouvelles actions pour décomposition de Tucker
    torch_tucker_decomposition,
    torch_tucker_reconstruct,
    torch_tucker_compression_ratio,
    torch_khatri_rao_product

};

class Lispe_lispetorch : public Element {
public:
#ifdef USE_HUGGINGFACE
    static std::unordered_map<std::string, std::unique_ptr<lispe_hf::HuggingFaceLoader>> hf_loaders;
#endif
    torch_actions action;
    Lispe_lispetorch(torch_actions a) : Element(l_lib), action(a) {}

    ~Lispe_lispetorch();

    Element* eval(LispE* lisp);

    // Méthodes spécifiques pour les opérations tensor
    Element* tensor_create(LispE* lisp);
    Element* tensor_in_memory(LispE* lisp);
    Element* tensor_zeros(LispE* lisp);
    Element* tensor_to_lispe(LispE* lisp);
    Element* tensor_ones(LispE* lisp);
    Element* tensor_cat(LispE* lisp);
    Element* tensor_triu(LispE* lisp);
    Element* tensor_set_item(LispE* lisp);
    Element* tensor_randn(LispE* lisp);
    Element* tensor_rand(LispE* lisp);
    Element* tensor_randint(LispE* lisp);
    Element* tensor_transpose(LispE* lisp);
    Element* tensor_add(LispE* lisp);
    Element* tensor_sub(LispE* lisp);
    Element* tensor_mul(LispE* lisp);
    Element* tensor_full(LispE* lisp);
    Element* tensor_div(LispE* lisp);
    Element* tensor_mul_scalar(LispE* lisp);
    Element* tensor_add_scalar(LispE* lisp);
    Element* tensor_div_scalar(LispE* lisp);
    Element* tensor_linear(LispE* lisp);
    Element* tensor_matmul(LispE* lisp);
    Element* tensor_size(LispE* lisp);
    Element* tensor_shape(LispE* lisp);
    Element* tensor_item(LispE* lisp);
    Element* tensor_reshape(LispE* lisp);
    Element* tensor_contiguous(LispE* lisp);
    Element* tensor_unsqueeze(LispE* lisp);
    Element* tensor_squeeze(LispE* lisp);
    Element* tensor_slice(LispE* lisp);
    Element* tensor_is_mps(LispE* lisp);
    Element* tensor_select(LispE* lisp);
    Element* tensor_sum(LispE* lisp);
    Element* tensor_mean(LispE* lisp);
    Element* tensor_mean_dim(LispE* lisp);
    Element* tensor_std(LispE* lisp);
    Element* tensor_max(LispE* lisp);
    Element* tensor_min(LispE* lisp);
    Element* tensor_argmax(LispE* lisp);
    Element* tensor_einsum(LispE* lisp);
    Element* tensor_relu(LispE* lisp);
    Element* tensor_sigmoid(LispE* lisp);
    Element* tensor_tanh(LispE* lisp);
    Element* tensor_gelu(LispE* lisp);
    Element* tensor_silu(LispE* lisp);
    Element* tensor_abs(LispE* lisp);
    Element* tensor_exp(LispE* lisp);
    Element* tensor_softmax(LispE* lisp);
    Element* tensor_log_softmax(LispE* lisp);
    
    // Nouvelles fonctions mathématiques
    Element* tensor_pow(LispE* lisp);
    Element* tensor_sqrt(LispE* lisp);
    Element* tensor_rsqrt(LispE* lisp);
    Element* tensor_log(LispE* lisp);
    Element* tensor_log10(LispE* lisp);
    Element* tensor_log2(LispE* lisp);
    Element* tensor_sin(LispE* lisp);
    Element* tensor_cos(LispE* lisp);
    Element* tensor_tan(LispE* lisp);
    Element* tensor_asin(LispE* lisp);
    Element* tensor_acos(LispE* lisp);
    Element* tensor_atan(LispE* lisp);
    Element* tensor_sinh(LispE* lisp);
    Element* tensor_cosh(LispE* lisp);
    Element* tensor_floor(LispE* lisp);
    Element* tensor_ceil(LispE* lisp);
    Element* tensor_round(LispE* lisp);
    Element* tensor_clamp(LispE* lisp);
    Element* tensor_neg(LispE* lisp);
    Element* tensor_reciprocal(LispE* lisp);
    Element* tensor_rms_norm(LispE* lisp);
    
    // Nouvelles méthodes tensor
    Element* tensor_full_like(LispE* lisp);
    Element* tensor_cumsum(LispE* lisp);
    Element* tensor_gather(LispE* lisp);
    Element* tensor_masked_fill_(LispE* lisp);
    
    Element* linear_create(LispE* lisp);
    Element* linear_forward(LispE* lisp);
    Element* tensor_to_device(LispE* lisp);
    Element* tensor_to_cuda(LispE* lisp);

    // --- Ajout : méthodes CPU/CUDA (non static) ---
    Element* tensor_to_cpu(LispE* lisp);
    Element* cuda_empty_cache(LispE* lisp);

    // Méthodes pour les modèles et l'entraînement
    Element* model_create(LispE* lisp);
    Element* model_forward(LispE* lisp);
    Element* optimizer_create(LispE* lisp);
    Element* adam_optimizer_create(LispE* lisp);
    Element* adamw_optimizer_create(LispE* lisp);
    Element* sgd_optimizer_create(LispE* lisp);
    Element* optimizer_step(LispE* lisp);
    Element* optimizer_zero_grad(LispE* lisp);
    Element* clip_grad_norm(LispE* lisp);
    Element* optimizer_add_params(LispE* lisp);
    Element* loss_mse(LispE* lisp);
    Element* loss_crossentropy(LispE* lisp);
    Element* backward(LispE* lisp);
    Element* set_grad_enabled(LispE* lisp);
    Element* jit_load(LispE* lisp);
    Element* jit_unload(LispE* lisp);
    Element* jit_model_to_device(LispE* lisp);
    Element* jit_model_forward(LispE*);
    Element* jit_model_info(LispE*);
    Element* jit_model_get_buffer(LispE*);
    Element* jit_model_get_tensor(LispE*);
    Element* jit_model_get_tensor_shape(LispE*);
    Element* jit_model_list_parameter_names(LispE*);
    Element* jit_model_list_methods(LispE*);
    Element* jit_model_list_buffers(LispE*);
    Element* jit_model_to_mps(LispE* lisp);
    Element* jit_model_to_best_device(LispE* lisp);

    // Méthodes pour les schedulers
    Element* lr_scheduler_create(LispE* lisp);
    Element* lr_scheduler_step(LispE* lisp);
    Element* lr_scheduler_get_lr(LispE* lisp);
    Element* lr_scheduler_set_lr(LispE* lisp);

    // Méthodes HuggingFace
    Element* hf_load_model(LispE* lisp);
    Element* hf_model_info(LispE* lisp);
    Element* hf_list_weights(LispE* lisp);
    Element* hf_get_weight(LispE* lisp);
    Element* hf_get_q_weight(LispE* lisp);
    Element* hf_get_k_weight(LispE* lisp);
    Element* hf_get_v_weight(LispE* lisp);
    Element* hf_get_o_weight(LispE* lisp);
    Element* hf_get_qkv_fused_weight(LispE* lisp);
    Element* hf_get_gate_weight(LispE* lisp);
    Element* hf_get_up_weight(LispE* lisp);
    Element* hf_get_down_weight(LispE* lisp);
    Element* hf_get_gate_up_fused_weight(LispE* lisp);
    Element* hf_get_rms_norm_eps(LispE* lisp);
    Element* hf_forward(LispE* lisp);
    Element* hf_forward_manual(LispE* lisp);
    Element* hf_forward_attention_scores(LispE* lisp);
    Element* hf_forward_attention_size(LispE* lisp);
    Element* hf_clear_attention_scores(LispE* lisp);
    Element* hf_memory_usage(LispE* lisp);
    Element* hf_model_summary(LispE* lisp);
    Element* hf_enable_kv_cache(LispE* lisp);
    Element* hf_reset_kv_cache(LispE* lisp);
    Element* hf_generate(LispE* lisp);
    Element* hf_embeddings(LispE* lisp);

    // Méthodes LoRA pour HuggingFaceLoaderLoRA
    Element* hf_load_model_lora(LispE* lisp);
    Element* hf_lora_init(LispE* lisp);
    Element* hf_lora_get_parameters(LispE* lisp);
    Element* hf_lora_save(LispE* lisp);
    Element* hf_lora_load(LispE* lisp);
    Element* hf_lora_merge(LispE* lisp);
    Element* hf_lora_unmerge(LispE* lisp);
    Element* hf_lora_enable(LispE* lisp);

    Element* jit_model_get_intermediate_states(LispE* lisp);
    Element* jit_model_forward_single_layer(LispE* lisp);
    Element* jit_model_register_lora_hook(LispE* lisp);
    Element* jit_model_forward_with_lora(LispE* lisp);
    Element* jit_model_apply_lora_temporarily(LispE* lisp);
    Element* jit_model_list_tensor_names(LispE* lisp);
    Element* jit_model_update_weight(LispE* lisp);
    Element* jit_model_backup_weights(LispE* lisp);
    Element* jit_model_restore_weights(LispE* lisp);
    
    // Utilitaires LoRA avancés
    Element* lora_compute_delta(LispE* lisp);
    Element* lora_get_adaptation_magnitude(LispE* lisp);
    Element* lora_forward_with_gradients(LispE* lisp);

    Element* mps_synchronize(LispE* lisp);


    // Méthodes CUDA
    Element* cuda_is_available(LispE* lisp);
    Element* cuda_device_count(LispE* lisp);
    Element* cuda_memory_allocated(LispE* lisp);
    Element* cuda_memory_total(LispE* lisp);

    Element* set_device(LispE* lisp);

    // Méthodes MPS (Metal Performance Shaders - macOS)
    Element* mps_is_available(LispE* lisp);
    Element* tensor_to_mps(LispE* lisp);

    // Utilitaire de détection automatique du meilleur dispositif
    Element* get_best_device(LispE* lisp);
    
    // Méthodes Transformer - Phase 1
    Element* multihead_attention_create(LispE* lisp);
    Element* multihead_attention_forward(LispE* lisp);
    Element* layer_norm_create(LispE* lisp);
    Element* layer_norm_forward(LispE* lisp);
    Element* embedding_create(LispE* lisp);
    Element* embedding_forward(LispE* lisp);
    Element* transformer_block_create(LispE* lisp);
    Element* transformer_block_forward(LispE* lisp);
    
    // Nouvelles méthodes pour tokenization
    Element* tokenizer_simple_create(LispE* lisp);
    Element* tokenizer_sentencepiece_create(LispE* lisp);
    Element* tokenizer_sentencepiece_train(LispE* lisp);
    Element* tokenizer_encode(LispE* lisp);
    Element* tokenizer_decode(LispE* lisp);
    Element* vocabulary_size(LispE* lisp);
    Element* pad_sequence(LispE* lisp);
    Element* create_attention_mask(LispE* lisp);
    
    // Nouvelles méthodes pour positional embeddings
    Element* positional_encoding_create(LispE* lisp);
    Element* positional_encoding_forward(LispE* lisp);
    
    // Nouvelles méthodes pour rotary embeddings (RoPE)
    Element* rotary_embedding_create(LispE* lisp);
    Element* rotary_embedding_forward(LispE* lisp);
    Element* apply_rotary_pos_emb(LispE* lisp);
    
    // Nouvelles méthodes pour gradient checkpointing
    Element* checkpoint_enable(LispE* lisp);
    Element* checkpoint_disable(LispE* lisp);
    Element* checkpoint_forward(LispE* lisp);
    Element* checkpoint_create(LispE* lisp);
    
    // Nouvelles méthodes pour model loading
    Element* load_model(LispE* lisp);
    Element* save_model(LispE* lisp);
    Element* load_checkpoint(LispE* lisp);
    Element* save_checkpoint(LispE* lisp);
    Element* load_state_dict(LispE* lisp);
    Element* model_state_dict(LispE* lisp);
    
    // Nouvelles méthodes pour LoRA fine-tuning
    Element* lora_linear_create(LispE* lisp);
    Element* lora_linear_forward(LispE* lisp);
    Element* lora_apply_to_linear(LispE* lisp);
    Element* lora_merge_weights(LispE* lisp);
    Element* lora_get_trainable_params(LispE* lisp);
    Element* lora_save_adapters(LispE* lisp);
    Element* lora_load_adapters(LispE* lisp);
    
    // Nouvelles méthodes pour décomposition de Tucker
    Element* tucker_decomposition(LispE* lisp);
    Element* tucker_reconstruct(LispE* lisp);
    Element* tucker_compression_ratio(LispE* lisp);
    Element* khatri_rao_product(LispE* lisp);
    
    // Nouvelles méthodes pour Flash Attention
    Element* flash_attention_create(LispE* lisp);
    Element* flash_attention_forward(LispE* lisp);
    Element* flash_attention_with_mask(LispE* lisp);
    Element* flash_attention_with_dropout(LispE* lisp);
    Element* flash_attention_simple(LispE* lisp);
    Element* scaled_dot_product_attention(LispE* lisp);
    
    // Nouvelles méthodes pour génération de texte
    Element* generator_create(LispE* lisp);
    Element* generator_config(LispE* lisp);
    Element* generate(LispE* lisp);
    Element* set_generation_params(LispE* lisp);
    
    // Nouvelles méthodes pour sampling et génération avancée
    Element* topk(LispE* lisp);
    Element* multinomial(LispE* lisp);
    Element* sort_tensor(LispE* lisp);

    // Nouvelles méthodes pour quantification
    Element* quantize_dynamic(LispE* lisp);
    Element* quantize_static(LispE* lisp);
    Element* dequantize(LispE* lisp);
    Element* quantize_linear(LispE* lisp);
    Element* quantize_per_channel(LispE* lisp);
    Element* quantize_int8(LispE* lisp);
    Element* quantize_fp16(LispE* lisp);
    Element* tensor_to_int8(LispE* lisp);
    Element* tensor_to_fp16(LispE* lisp);
    Element* model_quantize_dynamic(LispE* lisp);
    Element* model_quantize_static(LispE* lisp);

    wstring asString(LispE* lisp);
};

// Classe pour encapsuler un torch::Tensor dans LispE
class TorchTensor : public Element {
public:
    static int16_t tensor_type;
    static long the_tensors;
    
#ifdef __APPLE__
    static bool enable_synchronize;
#endif    
    torch::Tensor tensor;
    Element* current;
    
    TorchTensor(const torch::Tensor& t) : Element(tensor_type), tensor(t) {
        the_tensors++;
        current = NULL;
    }

    ~TorchTensor() {
#ifdef __APPLE__
        if (TorchTensor::enable_synchronize && tensor.defined() && tensor.is_mps()) {
            torch::mps::synchronize();
        }
#endif

        the_tensors--;
        if (current != NULL)
            current->decrement();
    }
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return tensor.defined(); }
    long size() override { return tensor.numel(); }
    wstring asString(LispE* lisp);
    
    // Surcharge des méthodes car et cdr pour les tenseurs
    Element* car(LispE* lisp) override;
    Element* cdr(LispE* lisp) override;
    Element* extraction(LispE* lisp, List* l) override;
    
    bool isList() override {
        return true;
    }

    // Surcharge pour permettre l'utilisation de la fonction 'at' native de LispE
    Element* protected_index(LispE* lisp, Element* ix) override;
    Element* protected_index(LispE* lisp, long i) override;
    Element* index(long i) override;
    
    // Surcharge des méthodes nécessaires pour atshape
    Element* newInstance() override;
    void set_from(Element* container, long start, long end = -1) override;

    Element* plus_direct(LispE* lisp, Element* e) override;
    Element* minus_direct(LispE* lisp, Element* e) override;
    Element* multiply_direct(LispE* lisp, Element* e) override;
    Element* divide_direct(LispE* lisp, Element* e) override;

    void flatten(LispE*, Numbers* l) override;
    void flatten(LispE*, Integers* l) override;
    void flatten(LispE*, Floats* l) override;
    void flatten(LispE*, Shorts* l) override;
};

// Classe pour encapsuler un modèle PyTorch dans LispE
class TorchModel : public Element {
public:
    static int16_t model_type;
    std::shared_ptr<torch::nn::Module> module;
    
    TorchModel(std::shared_ptr<torch::nn::Module> m) : Element(model_type), module(m) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return module != nullptr; }
    wstring asString(LispE* lisp);
};

// Classe pour encapsuler un optimiseur PyTorch dans LispE
class TorchOptimizer : public Element {
public:
    static int16_t optimizer_type;
    std::unique_ptr<torch::optim::Optimizer> optimizer;
    
    TorchOptimizer(std::unique_ptr<torch::optim::Optimizer> opt) 
        : Element(optimizer_type), optimizer(std::move(opt)) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return optimizer != nullptr; }
    wstring asString(LispE* lisp);
};

// Modèle MLP simple
struct SimpleMLP : torch::nn::Module {
    torch::nn::Linear fc1{nullptr}, fc2{nullptr}, fc3{nullptr};
    
    SimpleMLP(int64_t input_size, int64_t hidden_size, int64_t output_size);
    torch::Tensor forward(torch::Tensor x);
};

// Classe pour Multi-Head Attention
class TorchMultiHeadAttention : public Element {
public:
    static int16_t attention_type;
    torch::nn::MultiheadAttention attention{nullptr};
    
    TorchMultiHeadAttention(int64_t embed_dim, int64_t num_heads) 
        : Element(attention_type) {
        attention = torch::nn::MultiheadAttention(
            torch::nn::MultiheadAttentionOptions(embed_dim, num_heads));
    }
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return true; }
    wstring asString(LispE* lisp);
};

// Classe pour Layer Normalization
class TorchLayerNorm : public Element {
public:
    static int16_t layernorm_type;
    torch::nn::LayerNorm layer_norm{nullptr};
    
    TorchLayerNorm(int64_t normalized_shape) : Element(layernorm_type) {
        layer_norm = torch::nn::LayerNorm(
            torch::nn::LayerNormOptions({normalized_shape}));
    }
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return true; }
    wstring asString(LispE* lisp);
};

// Classe pour Embedding
class TorchEmbedding : public Element {
public:
    static int16_t embedding_type;
    torch::nn::Embedding embedding{nullptr};
    
    TorchEmbedding(int64_t num_embeddings, int64_t embedding_dim) 
        : Element(embedding_type) {
        embedding = torch::nn::Embedding(
            torch::nn::EmbeddingOptions(num_embeddings, embedding_dim));
    }
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return true; }
    wstring asString(LispE* lisp);
};

// Classe pour Linear Layer
class TorchLinear : public Element {
public:
    static int16_t linear_type;
    torch::nn::Linear linear{nullptr};
    
    TorchLinear(int64_t in_features, int64_t out_features) : Element(linear_type) {
        linear = torch::nn::Linear(
            torch::nn::LinearOptions(in_features, out_features));
    }
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return true; }
    wstring asString(LispE* lisp);
};

// Bloc Transformer complet
struct TransformerBlock : torch::nn::Module {
    torch::nn::MultiheadAttention self_attention{nullptr};
    torch::nn::LayerNorm norm1{nullptr};
    torch::nn::LayerNorm norm2{nullptr};
    torch::nn::Linear ffn1{nullptr};
    torch::nn::Linear ffn2{nullptr};
    
    TransformerBlock(int64_t embed_dim, int64_t num_heads, int64_t ffn_dim);
    torch::Tensor forward(torch::Tensor x, torch::Tensor mask = {});
};

// Classe pour Transformer Block
class TorchTransformerBlock : public Element {
public:
    static int16_t transformer_block_type;
    std::shared_ptr<TransformerBlock> block;
    
    TorchTransformerBlock(std::shared_ptr<TransformerBlock> b) 
        : Element(transformer_block_type), block(b) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return block != nullptr; }
    wstring asString(LispE* lisp);
};

// Tokenizer simple pour commencer
class SimpleTokenizer {
private:
    std::unordered_map<std::string, int> token_to_id;
    std::vector<std::string> id_to_token;
    int next_id = 4; // Réserver 0-3 pour tokens spéciaux
    
public:
    SimpleTokenizer();
    std::vector<int> encode(const std::string& text);
    std::string decode(const std::vector<int>& token_ids);
    int vocab_size() const { return id_to_token.size(); }
    void add_token(const std::string& token);
};

// Classe pour les tokenizers
class TorchTokenizer : public Element {
public:
    static int16_t tokenizer_type;
    
    enum TokenizerType { SIMPLE, SENTENCEPIECE };
    TokenizerType type;
    
    // Tokenizer simple
    std::unique_ptr<SimpleTokenizer> simple_tokenizer;
    
#ifdef USE_SENTENCEPIECE
    // Tokenizer SentencePiece
    std::unique_ptr<sentencepiece::SentencePieceProcessor> sp_processor;
#endif
    
    // Constructeur pour tokenizer simple
    TorchTokenizer() : Element(tokenizer_type), type(SIMPLE) {
        simple_tokenizer = std::make_unique<SimpleTokenizer>();
    }
    
#ifdef USE_SENTENCEPIECE
    // Constructeur pour tokenizer SentencePiece
    TorchTokenizer(const std::string& model_path) : Element(tokenizer_type), type(SENTENCEPIECE) {
        sp_processor = std::make_unique<sentencepiece::SentencePieceProcessor>();
        if (!sp_processor->Load(model_path).ok()) {
            throw std::runtime_error("Failed to load SentencePiece model: " + model_path);
        }
    }
#endif
    
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override;
    wstring asString(LispE* lisp) override;
    
    std::vector<int> encode(const std::string& text);
    std::string decode(const std::vector<int>& token_ids);
    int vocab_size();
};

// Classe pour Positional Encoding
class PositionalEncoding : public torch::nn::Module {
private:
    torch::Tensor pe; // Pre-computed positional encodings
    int max_len;
    int d_model;
    
public:
    PositionalEncoding(int d_model, int max_len = 5000);
    torch::Tensor forward(torch::Tensor x);
};

// Classe pour Positional Encoding dans LispE
class TorchPositionalEncoding : public Element {
public:
    static int16_t positional_encoding_type;
    std::shared_ptr<PositionalEncoding> pe;
    
    TorchPositionalEncoding(std::shared_ptr<PositionalEncoding> encoding) 
        : Element(positional_encoding_type), pe(encoding) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return pe != nullptr; }
    wstring asString(LispE* lisp);
};

// Rotary Position Embedding (RoPE) for modern Transformers like Llama
struct RotaryEmbedding : torch::nn::Module {
    int dim;
    int max_seq_len;
    torch::Tensor inv_freq;
    torch::Tensor cos_cached;
    torch::Tensor sin_cached;
    
    RotaryEmbedding(int dim, int max_seq_len = 8192);
    std::pair<torch::Tensor, torch::Tensor> forward(int seq_len, torch::Device device = torch::kCPU);
    
    private:
        void _update_cos_sin_cache(int seq_len, torch::Device device);
};

class TorchRotaryEmbedding : public Element {
public:
    static int16_t rotary_embedding_type;
    std::shared_ptr<RotaryEmbedding> rope;
    
    TorchRotaryEmbedding(std::shared_ptr<RotaryEmbedding> embedding) 
        : Element(rotary_embedding_type), rope(embedding) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return rope != nullptr; }
    wstring asString(LispE* lisp);
};

// Gradient Checkpointing for memory optimization
struct CheckpointedModule {
    Element* wrapped_element;  // L'élément LispE wrappé
    bool checkpointing_enabled;
    
    CheckpointedModule(Element* elem) : wrapped_element(elem), checkpointing_enabled(false) {}
    torch::Tensor forward(torch::Tensor input);
    void enable_checkpointing() { checkpointing_enabled = true; }
    void disable_checkpointing() { checkpointing_enabled = false; }
};

class TorchCheckpointedModule : public Element {
public:
    static int16_t checkpointed_module_type;
    std::shared_ptr<CheckpointedModule> module;
    
    TorchCheckpointedModule(std::shared_ptr<CheckpointedModule> mod) 
        : Element(checkpointed_module_type), module(mod) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return module != nullptr; }
    wstring asString(LispE* lisp);
};

// ==================== Flash Attention Classes ====================

// Flash Attention for efficient memory usage
struct FlashAttention : torch::nn::Module {
    int64_t embed_dim;
    int64_t num_heads;
    int64_t head_dim;
    double dropout_p;
    bool use_bias;
    
    torch::nn::Linear q_proj{nullptr};
    torch::nn::Linear k_proj{nullptr};
    torch::nn::Linear v_proj{nullptr};
    torch::nn::Linear out_proj{nullptr};
    torch::nn::Dropout dropout{nullptr};
    
    FlashAttention(int64_t embed_dim, int64_t num_heads, double dropout_rate = 0.0, bool bias = true);
    
    // Standard Flash Attention forward pass
    torch::Tensor forward(torch::Tensor query, torch::Tensor key, torch::Tensor value);
    
    // Flash Attention with mask
    torch::Tensor forward_with_mask(torch::Tensor query, torch::Tensor key, torch::Tensor value, 
                                   torch::Tensor attn_mask);
    
    // Flash Attention with custom dropout
    torch::Tensor forward_with_dropout(torch::Tensor query, torch::Tensor key, torch::Tensor value, 
                                      double dropout_p, bool training = true);
};

// Classe pour Flash Attention dans LispE
class TorchFlashAttention : public Element {
public:
    static int16_t flash_attention_type;
    std::shared_ptr<FlashAttention> flash_attention;
    
    TorchFlashAttention(std::shared_ptr<FlashAttention> attn) 
        : Element(flash_attention_type), flash_attention(attn) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return flash_attention != nullptr; }
    wstring asString(LispE* lisp);
};

// ==================== Fin Flash Attention Classes ====================

// ==================== LoRA Fine-Tuning Classes ====================


// LoRA (Low-Rank Adaptation) Linear Layer
struct LoRALinear : torch::nn::Module {
    torch::nn::Linear original_layer{nullptr};
    torch::nn::Linear lora_A{nullptr}; // Low-rank matrix A
    torch::nn::Linear lora_B{nullptr}; // Low-rank matrix B
    double alpha; // Scaling factor
    int rank; // LoRA rank
    bool merged; // Whether LoRA weights are merged into original
    
    LoRALinear(int64_t in_features, int64_t out_features, int rank = 16, double alpha = 16.0, torch::ScalarType dtype = torch::kFloat32);
    torch::Tensor forward(torch::Tensor x);
    void merge_weights(); // Merge LoRA weights into original layer
    void unmerge_weights(); // Separate LoRA weights from original layer
    std::unordered_map<std::string, torch::Tensor> get_lora_state_dict();
    void load_lora_state_dict(const std::unordered_map<std::string, torch::Tensor>& state_dict);

        // Nouvelles fonctions pour l'intégration JIT
    torch::Tensor compute_lora_delta(); // Calcule α/r * B @ A
    void apply_to_base_weight(torch::Tensor& base_weight); // Applique directement à un poids
    void remove_from_base_weight(torch::Tensor& base_weight); // Retire de un poids
    
    // Forward avec contrôle des gradients
    torch::Tensor forward_with_gradients(torch::Tensor x, bool retain_graph = true);
    
    // Statistiques LoRA
    double get_adaptation_magnitude(); // Norme de la correction LoRA
    std::pair<torch::Tensor, torch::Tensor> get_svd_components(); // Décomposition SVD

};

// Classe pour LoRA Linear dans LispE
class TorchLoRALinear : public Element {
public:
    static int16_t lora_linear_type;
    std::shared_ptr<LoRALinear> lora_layer;
    
    TorchLoRALinear(std::shared_ptr<LoRALinear> layer) 
        : Element(lora_linear_type), lora_layer(layer) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return lora_layer != nullptr; }
    wstring asString(LispE* lisp);
};

// Configuration LoRA pour tout un modèle
struct LoRAConfig {
    int rank = 16;
    double alpha = 16.0;
    std::vector<std::string> target_modules; // Modules à adapter avec LoRA
    double dropout = 0.1;
    bool bias = false; // Adapter les bias ou non
    
    LoRAConfig() = default;
    LoRAConfig(int r, double a, const std::vector<std::string>& targets) 
        : rank(r), alpha(a), target_modules(targets) {}
};

// Classe pour la configuration LoRA dans LispE
class TorchLoRAConfig : public Element {
public:
    static int16_t lora_config_type;
    LoRAConfig config;
    
    TorchLoRAConfig(const LoRAConfig& cfg) : Element(lora_config_type), config(cfg) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return true; }
    wstring asString(LispE* lisp);
};

// ==================== Fin des Classes LoRA ====================

// Classe pour gérer les checkpoints et state dictionaries
class TorchStateDict : public Element {
public:
    static int16_t state_dict_type;
    std::unordered_map<std::string, torch::Tensor> state_dict;
    
    TorchStateDict() : Element(state_dict_type) {}
    TorchStateDict(const std::unordered_map<std::string, torch::Tensor>& dict) 
        : Element(state_dict_type), state_dict(dict) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return !state_dict.empty(); }
    wstring asString(LispE* lisp);
};

// ==================== Classes pour Génération de Texte ====================

// Structure pour les paramètres de génération
struct GenerationConfig {
    int64_t max_length = 100;
    double temperature = 1.0;
    int64_t top_k = 50;
    double top_p = 0.9;
    int64_t eos_token_id = 50256;  // GPT-2 default
    int64_t pad_token_id = 50256;
    int64_t bos_token_id = 50256;
    int64_t num_beams = 1;         // Pour beam search
    double repetition_penalty = 1.0;
    int64_t no_repeat_ngram_size = 0;
    bool do_sample = true;
    std::string strategy = "greedy"; // greedy, top_k, top_p, beam_search
    
    GenerationConfig() = default;
};

// Classe principale pour la génération
class TorchGenerator : public Element {
public:
    static int16_t generator_type;
    std::shared_ptr<torch::nn::Module> model;
    torch::Device device;
    GenerationConfig config;
    
    TorchGenerator(std::shared_ptr<torch::nn::Module> m, torch::Device dev = torch::kCPU) 
        : Element(generator_type), model(m), device(dev) {}
    
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return model != nullptr; }
    wstring asString(LispE* lisp);
    
    // Méthodes héritées d'Element
    Element* duplicate_minimal(LispE* lisp);
    Element* fullcopy();
    Element* copyatom(LispE* lisp, uint16_t s);
    bool unify(LispE* lisp, Element* value, bool record);
    bool egal(Element* value);
    
    // Méthodes de génération
    torch::Tensor generate(torch::Tensor input_ids);
    
private:
    torch::Tensor greedy_search(torch::Tensor input_ids);
    torch::Tensor top_k_sampling(torch::Tensor input_ids);
    torch::Tensor top_p_sampling(torch::Tensor input_ids);
    torch::Tensor beam_search(torch::Tensor input_ids);
    torch::Tensor sample_next_token(torch::Tensor logits, const std::string& strategy);
    torch::Tensor apply_repetition_penalty(torch::Tensor logits, torch::Tensor generated_ids);
};

enum torch_scheduler_type {
    SCHEDULER_LINEAR,
    SCHEDULER_COSINE,
    SCHEDULER_EXPONENTIAL,
    SCHEDULER_STEP,
    SCHEDULER_LINEAR_WARMUP_COSINE
};

// Nouvelle classe pour les schedulers
class TorchLRScheduler : public Element {
public:
    static int16_t scheduler_type;
    
    torch_scheduler_type sched_type;
    TorchOptimizer* optimizer_ref; // Référence au TorchOptimizer LispE
    
    // Paramètres du scheduler
    double initial_lr;
    double min_lr;
    int total_steps;
    int warmup_steps;
    int current_step;
    double gamma; // Pour exponential/step decay
    int step_size; // Pour step scheduler
    
    TorchLRScheduler(TorchOptimizer* opt, torch_scheduler_type type)
        : Element(scheduler_type), optimizer_ref(opt), sched_type(type), 
          current_step(0), initial_lr(0.001), min_lr(1e-6), 
          total_steps(1000), warmup_steps(0), gamma(0.1), step_size(100) {
            if (optimizer_ref)
                optimizer_ref->increment();
          }
    
    ~TorchLRScheduler() {
        if (optimizer_ref) {
            optimizer_ref->decrement();
        }
    }

    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return optimizer_ref != nullptr; }
    wstring asString(LispE* lisp) override;
    
    void step();
    double get_lr();
    void set_lr(double lr);
    
private:
    double compute_lr();
    double linear_warmup_cosine_lr(int step);
    double cosine_annealing_lr(int step);
    double exponential_lr(int step);
    double step_lr(int step);
    double linear_lr(int step);
};

// Classe pour encapsuler un modèle TorchScript dans LispE
class TorchJITModel : public Element {
public:
    static int16_t jit_model_type;
    torch::jit::script::Module model;
    std::unordered_map<std::string, std::string> name_mapping; // Mapping des noms originaux
    torch::Device device;
    bool is_loaded;
    std::string model_path;
    std::unordered_map<std::string, std::shared_ptr<LoRALinear>> lora_hooks;
    std::unordered_map<std::string, torch::Tensor> weight_backup;
    bool hooks_enabled = false;

    // Constructeur
    TorchJITModel() : Element(jit_model_type), device(torch::kCPU), is_loaded(false) {}
    
    TorchJITModel(const torch::jit::script::Module& m, torch::Device dev = torch::kCPU) 
        : Element(jit_model_type), model(m), device(dev), is_loaded(true) {}
    
    // Destructeur pour libérer proprement la mémoire
    ~TorchJITModel() {
        unload_model();
    }
    
    // Méthodes LispE standard
    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return is_loaded; }
    u_ustring asUString(LispE* lisp) override;
    
    // Gestion du modèle
    bool load_from_file(const std::string& path);
    bool load_from_buffer(const std::vector<char>& buffer);
    void unload_model();
    void move_to_device(torch::Device target_device);
    void move_to_mps(); // Méthode spécifique pour MPS
    torch::Device get_best_device(); // Détecte le meilleur device
    void move_to_best_device(); // Déplace vers le meilleur device
    
    // Accès aux tensors par nom
    torch::Tensor get_tensor(const std::string& name);
    std::vector<int64_t> get_tensor_shape(const std::string& name);
    std::vector<std::string> get_tensor_names();
    std::vector<std::string> get_parameter_names();
    std::vector<std::string> get_method_names();
    torch::Tensor get_buffer(const std::string& name);
    std::vector<std::string> get_buffer_names();
    torch::Tensor get_attribute(const std::string& name); // Méthode alternative
    std::vector<std::string> get_attribute_names();
    
    std::vector<std::string> get_hook_parameter_names();

    // Inférence
    torch::Tensor forward(const std::vector<torch::Tensor>& inputs);
    torch::Tensor forward(torch::Tensor input);
    
    // Informations sur le modèle
    size_t memory_usage();
    int64_t parameter_count();
    
    // Gestion de la mémoire
    void optimize_memory();
    void clear_cache();
    
    void register_lora_hook(const std::string& layer_name, std::shared_ptr<LoRALinear> lora);
    void remove_lora_hook(const std::string& layer_name);
    void clear_all_lora_hooks();
    
    // Forward pass avec hooks LoRA intégrés
    torch::Tensor forward_with_lora_hooks(torch::Tensor input);
    
    // Accès aux états cachés intermédiaires
    std::vector<torch::Tensor> get_intermediate_states(torch::Tensor input, 
                                                      const std::vector<std::string>& layer_names);
    
    // Forward couche par couche
    torch::Tensor forward_single_layer(torch::Tensor input, const std::string& layer_name);
    torch::Tensor forward_up_to_layer(torch::Tensor input, const std::string& layer_name);
    
    // Manipulation temporaire des poids
    std::unordered_map<std::string, torch::Tensor> backup_weights(const std::vector<std::string>& layer_names);
    void restore_weights(const std::unordered_map<std::string, torch::Tensor>& backup);
    void update_weight(const std::string& param_name, const torch::Tensor& new_weight);
    
    // Application temporaire de LoRA (fusion/défusion)
    void apply_lora_temporarily(const std::unordered_map<std::string, std::shared_ptr<LoRALinear>>& adapters);
    void remove_lora_temporarily(const std::unordered_map<std::string, std::shared_ptr<LoRALinear>>& adapters);
    
    // Utilitaires
    std::vector<std::string> get_all_tensor_names();
    bool has_parameter(const std::string& name);
    torch::Tensor get_parameter_gradient(const std::string& name);
    
    // Enable/disable hooks
    void enable_lora_hooks() { hooks_enabled = true; }
    void disable_lora_hooks() { hooks_enabled = false; }
    bool are_hooks_enabled() const { return hooks_enabled; }

private:
    void load_name_mapping(const std::string& mapping_file);
    std::string original_to_safe_name(const std::string& original_name);
};

// Gestionnaire de hooks LoRA pour modèles JIT
class LoRAHookManager {
private:
    TorchJITModel* jit_model;
    std::unordered_map<std::string, std::shared_ptr<LoRALinear>> active_hooks;
    std::vector<std::string> hook_order; // Ordre d'application des hooks
    
public:
    LoRAHookManager(TorchJITModel* model) : jit_model(model) {}
    
    void add_hook(const std::string& layer_name, std::shared_ptr<LoRALinear> lora, int priority = 0);
    void remove_hook(const std::string& layer_name);
    void clear_all_hooks();
    
    // Forward avec application séquentielle des hooks
    torch::Tensor forward_with_hooks(torch::Tensor input);
    
    // Collecte des paramètres entraînables de tous les hooks
    std::vector<torch::Tensor> get_all_trainable_parameters();
    
    // Sauvegarde/restauration de tous les hooks
    void save_all_adapters(const std::string& directory);
    void load_all_adapters(const std::string& directory);
};

// ==================== Fin Classes Génération ====================

// ==================== Prototypes pour décomposition de Tucker ====================

// Fonction principale de décomposition de Tucker
std::tuple<torch::Tensor, std::vector<torch::Tensor>> tucker_decompose(
    torch::Tensor tensor, 
    const std::vector<int64_t>& ranks, 
    int max_iter = 100, 
    double tol = 1e-6);

// Reconstruction d'un tenseur à partir des composantes Tucker
torch::Tensor tucker_reconstruct_tensor(torch::Tensor core, const std::vector<torch::Tensor>& factors);

// Produit de Khatri-Rao de deux matrices
torch::Tensor the_khatri_rao_product(torch::Tensor A, torch::Tensor B);

// Dépliage d'un tenseur selon un mode (matricization)
torch::Tensor tensor_unfold(torch::Tensor tensor, int mode);

// Produit mode-n d'un tenseur avec une matrice
torch::Tensor tensor_mode_product(torch::Tensor tensor, torch::Tensor matrix, int mode);

// Calculer le ratio de compression de Tucker
double calculate_tucker_compression_ratio(
    const std::vector<int64_t>& original_shape,
    const std::vector<int64_t>& core_shape,
    const std::vector<std::vector<int64_t>>& factor_shapes);


#define t_jit_model TorchJITModel::jit_model_type
#define t_tensor TorchTensor::tensor_type
#define t_model TorchModel::model_type
#define t_optimizer TorchOptimizer::optimizer_type
#define t_attention TorchMultiHeadAttention::attention_type
#define t_layernorm TorchLayerNorm::layernorm_type
#define t_embedding TorchEmbedding::embedding_type
#define t_linear TorchLinear::linear_type
#define t_transformer_block TorchTransformerBlock::transformer_block_type
#define t_positional_encoding TorchPositionalEncoding::positional_encoding_type
#define t_rotary_embedding TorchRotaryEmbedding::rotary_embedding_type
#define t_checkpointed_module TorchCheckpointedModule::checkpointed_module_type
#define t_state_dict TorchStateDict::state_dict_type
#define t_lora_linear TorchLoRALinear::lora_linear_type
#define t_lora_config TorchLoRAConfig::lora_config_type
#define t_flash_attention TorchFlashAttention::flash_attention_type
#define t_generator TorchGenerator::generator_type
#define t_lr_scheduler TorchLRScheduler::scheduler_type

#endif