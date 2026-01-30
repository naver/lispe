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

#ifdef __APPLE__
#include "TensorSVD.h"
#endif

#ifdef USE_CUDA
#include <cuda_runtime.h>
#include <cublas_v2.h>
#ifdef __linux__
#include <c10/cuda/CUDAFunctions.h>
#include <c10/cuda/CUDACachingAllocator.h>
#endif
#endif

#ifdef USE_SENTENCEPIECE
#include <sentencepiece_processor.h>
#include <sentencepiece_trainer.h>
#endif

Lispe_lispetorch::~Lispe_lispetorch() {
    // 🧹 Nettoyer tous les loaders pour éviter segfault
#ifdef USE_HUGGINGFACE
        hf_loaders.clear();
#endif
#ifdef __APPLE__
    if (action == torch_is_mps) {
        torch::mps::synchronize();
    }
#endif
}

torch::Tensor lispe_to_tensor(Element* element);

// Fonction utilitaire pour appliquer le repetition penalty
torch::Tensor applyRepetitionPenalty(torch::Tensor logits, const std::vector<int64_t>& generated_tokens, double penalty) {
    if (penalty == 1.0 || generated_tokens.empty()) return logits;
    
    auto device = logits.device();
    auto vocab_size = logits.size(-1);
    
    // Créer un tenseur de pénalités avec des valeurs par défaut
    auto penalty_weights = torch::ones({vocab_size}, torch::TensorOptions().device(device));
    
    // Fenêtre plus large pour capturer les répétitions à long terme
    int64_t window_size = std::min(static_cast<int64_t>(generated_tokens.size()), static_cast<int64_t>(100));
    int64_t start_idx = generated_tokens.size() - window_size;
    
    // Compter les occurrences de chaque token dans la fenêtre
    std::unordered_map<int64_t, int> token_counts;
    std::unordered_map<int64_t, int> recent_positions;
    
    for (int64_t i = start_idx; i < static_cast<int64_t>(generated_tokens.size()); ++i) {
        int64_t token_id = generated_tokens[i];
        if (token_id >= 0 && token_id < vocab_size) {
            token_counts[token_id]++;
            recent_positions[token_id] = i; // Position de la dernière occurrence
        }
    }
    
    // Appliquer des pénalités progressives selon la fréquence et la récence
    for (const auto& pair : token_counts) {
        int64_t token_id = pair.first;
        int count = pair.second;
        int last_pos = recent_positions[token_id];
        
        // Pénalité de base selon le nombre d'occurrences
        double base_penalty = std::pow(penalty, count);
        
        // Pénalité supplémentaire pour les tokens très récents (derniers 10 tokens)
        double recency_penalty = 1.0;
        if (last_pos >= static_cast<int64_t>(generated_tokens.size()) - 10) {
            recency_penalty = penalty * 2.0; // Double pénalité pour les tokens très récents
        }
        
        // Pénalité combinée
        double final_penalty = base_penalty * recency_penalty;
        
        // Assurer un minimum de pénalité pour éviter les valeurs trop faibles
        final_penalty = std::max(final_penalty, penalty);
        
        penalty_weights[token_id] = 1.0 / final_penalty;
    }
    
    // Appliquer les pénalités aux logits
    auto penalized_logits = logits * penalty_weights;
    
    return penalized_logits;
}

// Helper function to apply rotary position embedding to query/key tensors
torch::Tensor apply_rotary_position_embedding(torch::Tensor tensor, torch::Tensor cos, torch::Tensor sin) {
    // tensor shape: [batch_size, num_heads, seq_len, head_dim]
    // cos/sin shape: [seq_len, head_dim]
    
    auto batch_size = tensor.size(0);
    auto num_heads = tensor.size(1);
    auto seq_len = tensor.size(2);
    auto head_dim = tensor.size(3);
    
    // Reshape cos and sin to match tensor dimensions
    cos = cos.unsqueeze(0).unsqueeze(0).expand({batch_size, num_heads, seq_len, head_dim});
    sin = sin.unsqueeze(0).unsqueeze(0).expand({batch_size, num_heads, seq_len, head_dim});
    
    // Split tensor into two halves for rotation
    auto x1 = tensor.index({"...", torch::indexing::Slice(0, head_dim / 2)});
    auto x2 = tensor.index({"...", torch::indexing::Slice(head_dim / 2, torch::indexing::None)});
    
    // Apply rotation: x * cos - rotate_half(x) * sin
    auto cos1 = cos.index({"...", torch::indexing::Slice(0, head_dim / 2)});
    auto cos2 = cos.index({"...", torch::indexing::Slice(head_dim / 2, torch::indexing::None)});
    auto sin1 = sin.index({"...", torch::indexing::Slice(0, head_dim / 2)});
    auto sin2 = sin.index({"...", torch::indexing::Slice(head_dim / 2, torch::indexing::None)});
    
    // Rotated tensor
    auto rotated_x1 = x1 * cos1 - x2 * sin1;
    auto rotated_x2 = x2 * cos2 + x1 * sin2;
    
    return torch::cat({rotated_x1, rotated_x2}, -1);
}

static void callback_eval(LispE* lisp, Element* function, long word, Element* data) {
    Element* wrd = lisp->provideInteger(word);
    vector<Element*> arguments;
    arguments.push_back(wrd);
    if (data != NULL)
        arguments.push_back(data);

    Element* e = lispe_eval_callback(lisp, function, arguments);
    if (e->isError()) {
        cerr << e->toString(lisp);
    }
    e->release();
}

Element* Lispe_lispetorch::tensor_to_lispe(LispE* lisp) {
    Element* torch = lisp->get_variable(L"tensor");
    if (torch->type != t_tensor) {
        throw new Error("Error: Argument must be a tensor");
    }
    
    torch::Tensor& tensor =  ((TorchTensor*)torch)->tensor;
    auto sizes = tensor.sizes();
    long sz = sizes.size();
    if (tensor.dtype() == torch::kBFloat16) {        
        if (!sz)
            return lisp->provideFloat(tensor.item<float>());
        auto floats = lisp->provideFloats();
        auto flat = tensor.flatten();
        torch::BFloat16* data = flat.data_ptr<torch::BFloat16>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            floats->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(floats, shape);
            floats->release();
            return res;
        }
        else
            return floats;
    } 
    if (tensor.dtype() == torch::kFloat32) {        
        if (!sz)
            return lisp->provideFloat(tensor.item<float>());
        auto floats = lisp->provideFloats();
        auto flat = tensor.flatten();
        float* data = flat.data_ptr<float>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            floats->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(floats, shape);
            floats->release();
            return res;
        }
        else
            return floats;
    } else if (tensor.dtype() == torch::kFloat64) {
        if (!sz)
            return lisp->provideNumber(tensor.item<double>());
        auto numbers = lisp->provideNumbers();
        auto flat = tensor.flatten();
        double* data = flat.data_ptr<double>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            numbers->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(numbers, shape);
            numbers->release();
            return res;
        }
        else
            return numbers;
    }
    else if (tensor.dtype() == torch::kInt64) {
        if (!sz)
            return lisp->provideInteger(tensor.item<int64_t>());
        auto numbers = lisp->provideIntegers();
        auto flat = tensor.flatten();
        int64_t* data = flat.data_ptr<int64_t>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            numbers->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(numbers, shape);
            numbers->release();
            return res;
        }
        else
            return numbers;
    }
    else if (tensor.dtype() == torch::kInt32) {
        if (!sz)
            return lisp->provideInteger(tensor.item<int64_t>());
        auto numbers = lisp->provideIntegers();
        auto flat = tensor.flatten();
        int32_t* data = flat.data_ptr<int32_t>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            numbers->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(numbers, shape);
            numbers->release();
            return res;
        }
        else
            return numbers;
    }
    else if (tensor.dtype() == torch::kInt16) {
        if (!sz)
            return new Short(tensor.item<int16_t>());
        auto numbers = new Shorts();
        auto flat = tensor.flatten();
        int16_t* data = flat.data_ptr<int16_t>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            numbers->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(numbers, shape);
            numbers->release();
            return res;
        }
        else
            return numbers;
    }
    else if (tensor.dtype() == torch::kInt8) {
        if (!sz)
            return new Short(tensor.item<int16_t>());
        auto numbers = new Shorts();
        auto flat = tensor.flatten();
        int8_t* data = flat.data_ptr<int8_t>();
        for (int64_t i = 0; i < flat.numel(); i++) {
            numbers->liste.push_back(data[i]);
        }
        if (sz > 1) {
            vecte<long> shape;
            for (auto& e : sizes)
                shape.push_back(e);
            Element* res = lisp->tensor_to_lispe(numbers, shape);
            numbers->release();
            return res;
        }
        else
            return numbers;
    }
    throw new Error("Error: Unsupported tensor dtype");
}

Element* Lispe_lispetorch::lr_scheduler_create(LispE* lisp) {
    Element* optimizer_elem = lisp->get_variable(L"optimizer");
    Element* type_elem = lisp->get_variable(L"scheduler_type");
    Element* config_elem = lisp->get_variable(L"config");
    
    try {
        if (optimizer_elem->type != t_optimizer) {
            throw new Error("First argument must be an optimizer");
        }
        
        TorchOptimizer* opt = (TorchOptimizer*)optimizer_elem;
        std::string sched_type_str = type_elem->toString(lisp);
        
        torch_scheduler_type sched_type;
        if (sched_type_str == "linear_warmup_cosine") {
            sched_type = SCHEDULER_LINEAR_WARMUP_COSINE;
        } else if (sched_type_str == "cosine") {
            sched_type = SCHEDULER_COSINE;
        } else if (sched_type_str == "exponential") {
            sched_type = SCHEDULER_EXPONENTIAL;
        } else if (sched_type_str == "step") {
            sched_type = SCHEDULER_STEP;
        } else if (sched_type_str == "linear") {
            sched_type = SCHEDULER_LINEAR;
        } else {
            throw new Error("Unknown scheduler type: " + sched_type_str);
        }
        
        // Créer le scheduler avec une référence à l'optimiseur
        auto scheduler = new TorchLRScheduler(opt, sched_type);
        
        // Configurer les paramètres
        if (config_elem && config_elem->isDictionary()) {
            Dictionary* config = (Dictionary*)config_elem;
            
            for (auto& pair : config->dictionary) {
                std::string key;
                for (auto c : pair.first) key += (char)c;
                Element* value = pair.second;
                
                if (key == "initial_lr") scheduler->initial_lr = value->asNumber();
                else if (key == "min_lr") scheduler->min_lr = value->asNumber();
                else if (key == "total_steps") scheduler->total_steps = value->asInteger();
                else if (key == "warmup_steps") scheduler->warmup_steps = value->asInteger();
                else if (key == "gamma") scheduler->gamma = value->asNumber();
                else if (key == "step_size") scheduler->step_size = value->asInteger();
            }
        }
        
        return scheduler;
        
    } catch (const std::exception& e) {
        throw new Error("Error creating LR scheduler: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::lr_scheduler_step(LispE* lisp) {
    Element* scheduler_elem = lisp->get_variable(L"scheduler");
    
    try {
        if (scheduler_elem->type != t_lr_scheduler) {
            throw new Error("Argument must be a learning rate scheduler");
        }
        
        TorchLRScheduler* scheduler = (TorchLRScheduler*)scheduler_elem;
        scheduler->step();
        
        return True_;
        
    } catch (const std::exception& e) {
        throw new Error("Error in scheduler step: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::lr_scheduler_get_lr(LispE* lisp) {
    Element* scheduler_elem = lisp->get_variable(L"scheduler");
    
    try {
        if (scheduler_elem->type != t_lr_scheduler) {
            throw new Error("Argument must be a learning rate scheduler");
        }
        
        TorchLRScheduler* scheduler = (TorchLRScheduler*)scheduler_elem;
        double lr = scheduler->get_lr();
        
        return lisp->provideNumber(lr);
        
    } catch (const std::exception& e) {
        throw new Error("Error getting learning rate: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::lr_scheduler_set_lr(LispE* lisp) {
    Element* scheduler_elem = lisp->get_variable(L"scheduler");
    Element* lr_elem = lisp->get_variable(L"learning_rate");
    
    try {
        if (scheduler_elem->type != t_lr_scheduler) {
            throw new Error("First argument must be a learning rate scheduler");
        }
        
        TorchLRScheduler* scheduler = (TorchLRScheduler*)scheduler_elem;
        double lr = lr_elem->asNumber();
        
        scheduler->set_lr(lr);
        
        return True_;
        
    } catch (const std::exception& e) {
        throw new Error("Error setting learning rate: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_load_model(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);

    // Paramètre optionnel : config (dictionnaire)
    Element* config_elem = lisp->get_variable(L"config");

    // Créer la configuration avec valeurs par défaut
    using namespace lispe_hf;
    HFLoadConfig load_config;

    // Parser le dictionnaire de configuration si fourni
    if (config_elem != null_ && config_elem != NULL && config_elem->isDictionary()) {
        Dictionary* config_dict = (Dictionary*)config_elem;

        // Device
        Element* device_val = config_dict->on_index(U"device");
        if (device_val != null_) {
            string device_str = device_val->toString(lisp);
            load_config.device = lookup_device(device_str);
        }

        // max_seq_len
        Element* max_seq_val = config_dict->on_index(U"max_seq_len");
        if (max_seq_val && max_seq_val->isNumber()) {
            load_config.max_seq_len = max_seq_val->asInteger();
        }

        // rope_scaling
        Element* rope_val = config_dict->on_index(U"rope_scaling");
        if (rope_val && rope_val->isNumber()) {
            load_config.rope_scaling = rope_val->asNumber();
        }

        // temperature
        Element* temp_val = config_dict->on_index(U"temperature");
        if (temp_val && temp_val->isNumber()) {
            load_config.temperature = temp_val->asNumber();
        }

        // top_p
        Element* top_p_val = config_dict->on_index(U"top_p");
        if (top_p_val && top_p_val->isNumber()) {
            load_config.top_p = top_p_val->asNumber();
        }

        // top_k
        Element* top_k_val = config_dict->on_index(U"top_k");
        if (top_k_val && top_k_val->isNumber()) {
            load_config.top_k = top_k_val->asInteger();
        }

        // repetition_penalty
        Element* rep_val = config_dict->on_index(U"repetition_penalty");
        if (rep_val && rep_val->isNumber()) {
            load_config.repetition_penalty = rep_val->asNumber();
        }

        // use_kv_cache
        Element* cache_val = config_dict->on_index(U"use_kv_cache");
        if (cache_val) {
            load_config.use_kv_cache = cache_val->Boolean();
        }
        
        // max_cache_len pour la fenêtre glissante
        Element* max_cache_val = config_dict->on_index(U"max_cache_len");
        if (max_cache_val && max_cache_val->isNumber()) {
            load_config.max_cache_len = max_cache_val->asInteger();
        }
    }

    try {
        // Utiliser le backend PyTorch
            // Rechercher un loader existant ou en créer un nouveau
            auto it = hf_loaders.find(path);
            if (it == hf_loaders.end()) {
                // Choisir la classe appropriée selon la configuration
                std::unique_ptr<HuggingFaceLoader> loader;
                
                // Vérifier si on veut le mode manuel (via config)
                bool manual_mode = false;
                if (config_elem != null_ && config_elem->isDictionary()) {
                    Dictionary* config_dict = (Dictionary*)config_elem;
                    Element* manual_val = config_dict->on_index(U"manual_attention");
                    if (manual_val) {
                        manual_mode = manual_val->Boolean();
                    }
                }

                if (manual_mode) {
                    loader = std::make_unique<HuggingFaceLoaderManual>(load_config);
                } else {
                    loader = std::make_unique<HuggingFaceLoader>(load_config);
                }

                // Charger la config (lance une exception en cas d'erreur)
                std::string config_path = path + "/config.json";
                loader->loadConfig(config_path);

                // Charger les poids (lance une exception en cas d'erreur)
                loader->loadWeights(path);

                // Valider le modèle
                if (!loader->validateModel()) {
                    throw new Error("Error validating HF model");
                }

                hf_loaders[path] = std::move(loader);
            }

            return new String(path);  // Retourner le path comme ID

    } catch (const std::exception& e) {
        throw new Error("Error loading HF model: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_model_info(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            throw new Error("Model not loaded: " + path);
        }
        
        auto info = it->second->getModelInfo();
        
        // Créer une liste avec les informations principales
        List* result = new List();
        result->append(new String("vocab_size"));
        result->append(new Integer(info.vocab_size));
        result->append(new String("hidden_size"));
        result->append(new Integer(info.hidden_size));
        
        return result;
        
    } catch (const std::exception& e) {
        throw new Error("Error getting model info: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_memory_usage(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            throw new Error("Model not loaded: " + path);
        }
        
        return new Integer(static_cast<long long>(it->second->memory_usage));
        
    } catch (const std::exception& e) {
        throw new Error("Error getting memory usage: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_forward(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* input_elem = lisp->get_variable(L"input_ids");
    Element* context = lisp->get_variable(L"kvcache");


    try {
        // Utiliser le loader PyTorch
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            throw new Error("Model not loaded: " + path);
        }
        if (context == null_)
            context = &it->second->ctx_basic;
        else {
            if (context->type != HFInferenceContext::t_context_inference)
                throw new Error("Error: Expecting kv cache object");
        }

        it->second->threaded = lisp->threaded();

        // Convertir input_ids en tensor (éviter copie inutile)
        if (input_elem->type == TorchTensor::tensor_type) {
            TorchTensor* torch_input = (TorchTensor*)input_elem;
            // Passer directement la référence pour éviter une copie
            torch::Tensor output;
            it->second->forward(torch_input->tensor, output, *((HFInferenceContext*)context));
            return new TorchTensor(std::move(output));
        } else {
            throw new Error("Input must be a torch tensor");
        }

    } catch (const std::exception& e) {
        throw new Error("Error in HF forward: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_forward_manual(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* input_elem = lisp->get_variable(L"input_ids");
    Element* context = lisp->get_variable(L"kvcache");

    try {
        // Utiliser le loader PyTorch
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            throw new Error("Model not loaded: " + path);
        }
        
        // Vérifier si c'est bien un loader manuel
        lispe_hf::HuggingFaceLoaderManual* manual_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderManual*>(it->second.get());
        if (!manual_loader) {
            throw new Error("Error: Model was not loaded in manual mode (use appropriate load function)");
        }

        if (context == null_)
            context = &manual_loader->ctx_basic;
        else {
            if (context->type != HFInferenceContext::t_context_inference)
                throw new Error("Error: Expecting kv cache object");
        }

        manual_loader->threaded = lisp->threaded();
        
        // Activer l'enregistrement des scores dans le contexte
        HFInferenceContext* ctx = (HFInferenceContext*)context;
        ctx->enableAttentionRecording(true);

        // Convertir input_ids en tensor (éviter copie inutile)
        if (input_elem->type == TorchTensor::tensor_type) {
            TorchTensor* torch_input = (TorchTensor*)input_elem;
            // Passer directement la référence pour éviter une copie
            torch::Tensor output;
            manual_loader->forward(torch_input->tensor, output, *((HFInferenceContext*)context));
            return new TorchTensor(std::move(output));
        } else {
            throw new Error("Input must be a torch tensor");
        }

    } catch (const std::exception& e) {
        throw new Error("Error in HF manual forward: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_forward_attention_scores(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    long layer_idx = lisp->get_variable(L"layer_index")->asInteger();
    Element* kvcache_elem = lisp->get_variable(L"kvcache");
    
    auto it = hf_loaders.find(path);
    if (it == hf_loaders.end()) {
        throw new Error("Error: Model not found: " + path);
    }
    
    // Déterminer le contexte à utiliser
    HFInferenceContext* ctx;
    if (kvcache_elem == null_) {
        ctx = &it->second->ctx_basic;
    } else {
        if (kvcache_elem->type != HFInferenceContext::t_context_inference) {
            throw new Error("Error: Expecting kv cache object");
        }
        ctx = (HFInferenceContext*)kvcache_elem;
    }
    
    torch::Tensor scores = ctx->getAttentionScores(layer_idx);
    if (!scores.defined()) {
        return null_;
    }
    
    return new TorchTensor(scores);
}

Element* Lispe_lispetorch::hf_forward_attention_size(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* kvcache_elem = lisp->get_variable(L"kvcache");
    
    auto it = hf_loaders.find(path);
    if (it == hf_loaders.end()) {
        throw new Error("Error: Model not found: " + path);
    }
    
    // Déterminer le contexte à utiliser
    HFInferenceContext* ctx;
    if (kvcache_elem == null_) {
        ctx = &it->second->ctx_basic;
    } else {
        if (kvcache_elem->type != HFInferenceContext::t_context_inference) {
            throw new Error("Error: Expecting kv cache object");
        }
        ctx = (HFInferenceContext*)kvcache_elem;
    }
    
    return lisp->provideInteger(ctx->getAttentionScoresCount());
}

Element* Lispe_lispetorch::hf_clear_attention_scores(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* kvcache_elem = lisp->get_variable(L"kvcache");
    
    auto it = hf_loaders.find(path);
    if (it == hf_loaders.end()) {
        throw new Error("Error: Model not found: " + path);
    }
    
    // Déterminer le contexte à utiliser
    HFInferenceContext* ctx;
    if (kvcache_elem == null_) {
        ctx = &it->second->ctx_basic;
    } else {
        if (kvcache_elem->type != HFInferenceContext::t_context_inference) {
            throw new Error("Error: Expecting kv cache object");
        }
        ctx = (HFInferenceContext*)kvcache_elem;
    }
    
    ctx->clearAttentionScores();
    
    return True_;
}

Element* Lispe_lispetorch::hf_load_model_lora(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* config_elem = lisp->get_variable(L"config");

    try {
        // Extraire le device depuis la config
        torch::Device device = torch::kCPU;
        if (config_elem->isDictionary()) {
            Dictionary* dict = (Dictionary*)config_elem;
            Element* device_elem = dict->on_index(U"device");
            if (device_elem != null_) {
                string device_str = device_elem->toString(lisp);
                device = lookup_device(device_str);
            }
        }

        // Créer la configuration HF
        HFLoadConfig hf_config(device);

        // Créer un loader LoRA
        auto lora_loader = std::make_unique<lispe_hf::HuggingFaceLoaderLoRA>(hf_config);

        // Charger config et poids
        string config_path = path + "/config.json";
        if (!lora_loader->loadConfig(config_path)) {
            throw new Error("Failed to load model config from: " + config_path);
        }

        if (!lora_loader->loadWeights(path)) {
            throw new Error("Failed to load model weights from: " + path);
        }

        // Stocker
        hf_loaders[model_name] = std::move(lora_loader);

        return lisp->provideString(model_name);

    } catch (const std::exception& e) {
        throw new Error("Error loading LoRA model: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_init(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);
    long rank = lisp->get_variable(L"rank")->asInteger();
    double alpha = lisp->get_variable(L"alpha")->asNumber();
    Element* targets_elem = lisp->get_variable(L"target_modules");
    Element* dtype_elem = lisp->get_variable(L"dtype");

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        // Cast vers LoRA loader
        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model. Use torch_hf_load_model_lora to load.");
        }

        // Convertir target_modules
        std::vector<std::string> targets;
        if (targets_elem->isList()) {
            for (long i = 0; i < targets_elem->size(); i++) {
                targets.push_back(targets_elem->index(i)->toString(lisp));
            }
        }

        // Déterminer le dtype (par défaut: float32)
        torch::ScalarType dtype = torch::kFloat32;
        if (dtype_elem != null_ && dtype_elem != NULL) {
            string dtype_str = dtype_elem->toString(lisp);
            if (dtype_str == "bfloat16") {
                dtype = torch::kBFloat16;
            } else if (dtype_str == "float16" || dtype_str == "half") {
                dtype = torch::kFloat16;
            } else if (dtype_str == "float32" || dtype_str == "float") {
                dtype = torch::kFloat32;
            }
        }

        // Initialiser LoRA avec le dtype
        bool success = lora_loader->initializeLoRA(rank, alpha, targets, dtype);

        return success ? True_ : False_;

    } catch (const std::exception& e) {
        throw new Error("Error initializing LoRA: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_get_parameters(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model");
        }

        // Obtenir les paramètres LoRA
        auto params = lora_loader->getLoRAParameters();

        // Convertir en liste LispE de TorchTensor
        List* result = lisp->provideList();
        for (auto& param : params) {
            result->append(new TorchTensor(param));
        }

        return result;

    } catch (const std::exception& e) {
        throw new Error("Error getting LoRA parameters: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_save(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);
    string save_path = lisp->get_variable(L"path")->toString(lisp);

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model");
        }

        bool success = lora_loader->saveLoRAAdapters(save_path);
        return success ? True_ : False_;

    } catch (const std::exception& e) {
        throw new Error("Error saving LoRA adapters: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_load(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);
    string load_path = lisp->get_variable(L"path")->toString(lisp);

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model");
        }

        bool success = lora_loader->loadLoRAAdapters(load_path);
        return success ? True_ : False_;

    } catch (const std::exception& e) {
        throw new Error("Error loading LoRA adapters: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_merge(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model");
        }

        lora_loader->mergeLoRAWeights();
        return True_;

    } catch (const std::exception& e) {
        throw new Error("Error merging LoRA weights: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_unmerge(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model");
        }

        lora_loader->unmergeLoRAWeights();
        return True_;

    } catch (const std::exception& e) {
        throw new Error("Error unmerging LoRA weights: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_lora_enable(LispE* lisp) {
    string model_name = lisp->get_variable(L"model_name")->toString(lisp);
    bool enable = lisp->get_variable(L"enable")->Boolean();

    try {
        auto it = hf_loaders.find(model_name);
        if (it == hf_loaders.end()) {
            throw new Error("Model not found: " + model_name);
        }

        auto* lora_loader = dynamic_cast<lispe_hf::HuggingFaceLoaderLoRA*>(it->second.get());
        if (!lora_loader) {
            throw new Error("Model is not a LoRA model");
        }

        lora_loader->enableLoRA(enable);
        return True_;

    } catch (const std::exception& e) {
        throw new Error("Error enabling/disabling LoRA: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_list_weights(LispE* lisp) {
    return new List();  // TODO: Implémenter
}

Element* Lispe_lispetorch::hf_get_weight(LispE* lisp) {
    return null_;  // TODO: Implémenter
}

Element* Lispe_lispetorch::hf_get_q_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        // Utiliser la méthode publique pour accéder aux poids
        const auto& q_weight = loader->getQWeight(layer_idx);
        if (!q_weight.defined()) {
            return new Error("Q projection weight not found for this layer");
        }
        
        return new TorchTensor(q_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing Q weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_k_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        // Utiliser la méthode publique pour accéder aux poids
        const auto& k_weight = loader->getKWeight(layer_idx);
        if (!k_weight.defined()) {
            return new Error("K projection weight not found for this layer");
        }
        
        return new TorchTensor(k_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing K weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_v_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        // Utiliser la méthode publique pour accéder aux poids
        const auto& v_weight = loader->getVWeight(layer_idx);
        if (!v_weight.defined()) {
            return new Error("V projection weight not found for this layer");
        }
        
        return new TorchTensor(v_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing V weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_o_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        // Utiliser la méthode publique pour accéder aux poids
        const auto& o_weight = loader->getOWeight(layer_idx);
        if (!o_weight.defined()) {
            return new Error("O projection weight not found for this layer");
        }
        
        return new TorchTensor(o_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing O weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_qkv_fused_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        // Utiliser la méthode publique pour accéder aux poids fusionnés QKV
        const auto& qkv_fused_weight = loader->getQKVFusedWeight(layer_idx);
        if (!qkv_fused_weight.defined()) {
            return new Error("QKV fused weight not found for this layer (may not be initialized yet)");
        }
        
        return new TorchTensor(qkv_fused_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing QKV fused weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_gate_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        const auto& gate_weight = loader->getGateWeight(layer_idx);
        if (!gate_weight.defined()) {
            return new Error("Gate projection weight not found for this layer");
        }
        
        return new TorchTensor(gate_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing Gate weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_up_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        const auto& up_weight = loader->getUpWeight(layer_idx);
        if (!up_weight.defined()) {
            return new Error("Up projection weight not found for this layer");
        }
        
        return new TorchTensor(up_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing Up weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_down_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        const auto& down_weight = loader->getDownWeight(layer_idx);
        if (!down_weight.defined()) {
            return new Error("Down projection weight not found for this layer");
        }
        
        return new TorchTensor(down_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing Down weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_gate_up_fused_weight(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* layer_element = lisp->get_variable("layer");
    if (layer_element == null_ || layer_element->type != t_integer) {
        return new Error("Missing or invalid layer index");
    }
    
    long layer_idx = layer_element->asInteger();
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        if (layer_idx < 0 || layer_idx >= loader->getModelInfo().num_hidden_layers) {
            return new Error("Invalid layer index");
        }
        
        const auto& gate_up_fused_weight = loader->getGateUpFusedWeight(layer_idx);
        if (!gate_up_fused_weight.defined()) {
            return new Error("Gate+Up fused weight not found for this layer (may not be initialized yet)");
        }
        
        return new TorchTensor(gate_up_fused_weight);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing Gate+Up fused weight: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_get_rms_norm_eps(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    
    try {
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            return new Error("Model not loaded: " + path);
        }
        
        auto& loader = it->second;
        double eps = loader->getRMSNormEps();
        
        return new Number(eps);
    }
    catch (const std::exception& e) {
        return new Error("Error accessing RMS norm epsilon: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_model_summary(LispE* lisp) {
    return new String("HF Model Summary");  // TODO: Implémenter
}

Element* Lispe_lispetorch::hf_enable_kv_cache(LispE* lisp) {
    if (lisp->threaded())
        throw new Error("Error: You cannot create a kv cache when threads are running");

    string path = lisp->get_variable(L"path")->toString(lisp);
    bool enable = lisp->get_variable(L"enable")->Boolean();

    try {

        // Utiliser le loader PyTorch
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            throw new Error("Model not loaded: " + path);
        }
        
        it->second->enableKVCache(enable);
        // Note: Avec le nouveau système de contexte, le KV cache est géré
        // par contexte d'inférence, pas globalement. Cette méthode ne fait rien.
        // Pour activer/désactiver le cache, il faut configurer le contexte
        // lors de sa création : HFInferenceContext ctx(enable);
        if (enable) {
            HFInferenceContext* ctx = new HFInferenceContext(enable);
            // Configurer la fenêtre glissante à partir de la configuration du modèle
            const auto& config = it->second->getLoadConfig();
            if (config.max_cache_len > 0) {
                ctx->getKVCache().configureSlidingWindow(config.max_cache_len);
            }
            return ctx;
        }
        return &it->second->ctx_basic;
    } catch (const std::exception& e) {
        throw new Error("Error enabling KV cache: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_reset_kv_cache(LispE* lisp) {
    if (lisp->threaded())
        throw new Error("Error: You cannot clean a kv cache when threads are running");

    Element* e = lisp->get_variable(L"kvcache");
    if (e->type != HFInferenceContext::t_context_inference)
        throw new Error("Error: Expecting a kv cache type");
    ((HFInferenceContext*)e)->resetCache();
    return True_;
}

Element* Lispe_lispetorch::hf_embeddings(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* token_ids_elem = lisp->get_variable(L"token_ids");

    try {
        // Trouver le loader
        auto it = hf_loaders.find(path);
        if (it == hf_loaders.end()) {
            throw new Error("Model not loaded: " + path);
        }

        // Convertir token_ids en tensor
        torch::Tensor token_ids_tensor;
        if (token_ids_elem->type == TorchTensor::tensor_type) {
            // Déjà un tenseur PyTorch
            TorchTensor* torch_input = (TorchTensor*)token_ids_elem;
            token_ids_tensor = torch_input->tensor;
        } else {
            // Convertir depuis liste LispE (Integers, List, etc.)
            token_ids_tensor = lispe_to_tensor(token_ids_elem);
        }

        // S'assurer que le tensor est de type Long (int64) pour les indices
        if (token_ids_tensor.scalar_type() != torch::kLong) {
            token_ids_tensor = token_ids_tensor.to(torch::kLong);
        }

        // Appeler getEmbeddings
        torch::Tensor embeddings = it->second->getEmbeddings(token_ids_tensor);

        // Retourner le résultat
        return new TorchTensor(std::move(embeddings));

    } catch (const std::exception& e) {
        throw new Error("Error getting embeddings: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::hf_generate(LispE* lisp) {
    string path = lisp->get_variable(L"path")->toString(lisp);
    Element* input_tokens = lisp->get_variable(L"initial_tokens");
    if (!input_tokens->size())
        throw new Error("Error: the input sequence should at least constains beos_id");

    int64_t size_sequence = lisp->get_variable(L"max_length")->asInteger();
    vecte<int64_t> ends;
    Element* eos = lisp->get_variable(L"eos_id");
    if (eos->isScalar()) {
        ends.push_back(eos->asInteger());
    }
    else {
        for (long i = 0; i < eos->size(); i++)
            ends.push_back(eos->index(i)->asInteger());
    }

    auto it = hf_loaders.find(path);
    if (it == hf_loaders.end()) {
        throw new Error("Model not loaded: " + path);
    }

    // Cache le pointeur du loader pour éviter indirections répétées
    auto& loader = it->second;

    // Créer un contexte d'inférence avec cache KV activé
    HFInferenceContext ctx(true);

    // Cache la température (si elle ne change pas pendant la génération)
    double temp = loader->temperature();

    // Paramètres de sampling
    Element* options = lisp->get_variable(L"options");
    string sampling_strategy = "simple";
    // Paramètres pour top-k et top-p
    int64_t top_k = 50;
    double top_p = 0.9;
    char strategy = 0;
    Element* callback = NULL;
    Element* data = NULL;

    if (options != null_ && options->isDictionary()) {
        Dictionary* config_dict = (Dictionary*)options;
        //topk?
        Element* sampling = config_dict->on_index(U"topk");
        if (sampling != NULL) {
            top_k = sampling->asInteger();
            strategy = 2;
        }
        else {
            sampling = config_dict->on_index(U"topp");
            if (sampling != NULL) {
                top_p = sampling->asNumber();
                strategy = 3;
            }
            else {
                sampling = config_dict->on_index(U"greedy");
                if (sampling != NULL) {
                    strategy = 1;
                }
            }
        }
        callback = config_dict->on_index(U"callback");
        data = config_dict->on_index(U"data");
    }
        
    // Paramètre de repetition penalty depuis la configuration du modèle
    double repetition_penalty = loader->getLoadConfig().repetition_penalty;

    // Obtenir le device du loader pour éviter transferts CPU→GPU inutiles
    torch::Device device = loader->getDevice();

    // Créer l'input initial directement sur le device GPU (évite transfert CPU→GPU dans forward)
    torch::Tensor current_input = lispe_to_tensor(input_tokens).to(device);
    current_input = current_input.unsqueeze(0);

    Integers* output_tokens = lisp->provideIntegers();
    output_tokens->liste.reserve(size_sequence);  // Pré-allocation pour éviter réallocations
    
    // Stocker les tokens générés pour le repetition penalty
    std::vector<int64_t> generated_tokens;
    for (long i = 0; i < input_tokens->size(); i++) {
        generated_tokens.push_back(input_tokens->index(i)->asInteger());
    }

    int64_t next_token_id = input_tokens->last(lisp)->asInteger();

    for (long i = 0; i < size_sequence && !ends.check(next_token_id); i++) {
        torch::Tensor logits = loader->forward(current_input, ctx);  // Passer le contexte

        // Extraire les logits du dernier token : [batch=1, seq, vocab] -> [vocab]
        auto last_logits = logits.select(1, -1).select(0, 0);

        // Appliquer la température
        last_logits = last_logits / temp;
        
        // Appliquer repetition penalty si nécessaire
        if (repetition_penalty != 1.0) {
            last_logits = applyRepetitionPenalty(last_logits, generated_tokens, repetition_penalty);
        }

        // Sampling selon la stratégie choisie
        torch::Tensor next_token_tensor;
        switch (strategy) {
            case 0: {
                // Méthode simple (par défaut) : softmax + multinomial
                auto probs = torch::softmax(last_logits, -1);
                next_token_tensor = torch::multinomial(probs, 1, true);
                break;
            }
            case 1: { //greedy
                next_token_tensor = torch::argmax(last_logits, -1, true);                
                break;
            }
            case 2: {//top k
                // Top-k sampling
                auto [top_k_values, top_k_indices] = torch::topk(last_logits, top_k, -1);
                
                // Créer un masque pour les valeurs non top-k
                auto filtered_logits = torch::full_like(last_logits, -std::numeric_limits<float>::infinity());
                filtered_logits.scatter_(-1, top_k_indices, top_k_values);
                
                // Échantillonner depuis la distribution filtrée
                auto probs = torch::softmax(filtered_logits, -1);
                next_token_tensor = torch::multinomial(probs, 1, true);
                break;
            }
            case 3: {//top_p
                // Top-p (nucleus) sampling
                auto probs = torch::softmax(last_logits, -1);
                auto [sorted_probs, sorted_indices] = torch::sort(probs, -1, true);
                
                // Calculer les probabilités cumulatives
                auto cumulative_probs = torch::cumsum(sorted_probs, -1);
                
                // Masquer les tokens au-delà du seuil top_p
                auto mask = cumulative_probs > top_p;
                // Garder au moins le premier token
                if (mask.sizes()[0] > 0) {
                    mask[0] = false;
                }
                sorted_probs.masked_fill_(mask, 0.0);
                
                // Renormaliser
                auto sum_probs = sorted_probs.sum(-1, true);
                if (sum_probs.item<float>() > 0) {
                    sorted_probs = sorted_probs / sum_probs;
                } else {
                    // Fallback: utiliser uniform sur le premier token
                    sorted_probs = torch::zeros_like(sorted_probs);
                    sorted_probs[0] = 1.0;
                }
                
                // Échantillonner et récupérer l'index original
                auto sampled_idx = torch::multinomial(sorted_probs, 1, true);
                next_token_tensor = sorted_indices.gather(-1, sampled_idx);
                break;
            }
        }

        // Transfert GPU→CPU seulement pour vérifier EOS et stocker le résultat
        next_token_id = next_token_tensor.item<int64_t>();
        output_tokens->liste.push_back(next_token_id);
        if (callback != NULL)
            callback_eval(lisp, callback, next_token_id, data);
        
        // Ajouter le nouveau token aux tokens générés pour le repetition penalty
        generated_tokens.push_back(next_token_id);

        // Utiliser directement next_token_tensor (déjà sur GPU) comme prochain input
        // Évite le transfert GPU→CPU→GPU (torch::tensor crée sur CPU puis .to() transfère)
        current_input = next_token_tensor.unsqueeze(0);
    }

    return output_tokens;
}

Element* Lispe_lispetorch::tensor_create(LispE* lisp) {
    Element* data = lisp->get_variable(L"thedata");
    try {
        return new TorchTensor(std::move(lispe_to_tensor(data)));
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_cat(LispE* lisp) {
    // On attend deux arguments : une liste de tenseurs et un entier pour la dimension
    Element* tensors_elem = lisp->get_variable(L"tensors");
    Element* dim_elem = lisp->get_variable(L"dim");

    try {
        std::vector<torch::Tensor> tensors;
        if (tensors_elem->type == t_list) {
            List* list = (List*)tensors_elem;
            for (long i = 0; i < list->size(); i++) {
                TorchTensor* tt = dynamic_cast<TorchTensor*>(list->liste[i]);
                if (!tt)
                    throw new Error("Error: All elements must be tensors");
                tensors.push_back(tt->tensor);
            }
        } else {
            throw new Error("Error: First argument must be a list of tensors");
        }

        int64_t dim = dim_elem->asInteger();
        return new TorchTensor(std::move(torch::cat(tensors, dim)));
    } catch (const std::exception& e) {
        string msg = "Error in torch_cat: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_full(LispE* lisp) {
    Element* shape_elem = lisp->get_variable(L"shape");
    Element* value_elem = lisp->get_variable(L"value");

    try {
        std::vector<int64_t> shape;
        if (shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: shape must be a list of integers");
        }

        double value = value_elem->asNumber();
        torch::Tensor tensor = torch::full(shape, value);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error in torch_full: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_in_memory(LispE* lisp) {
    return lisp->provideInteger(TorchTensor::the_tensors);
}

Element* Lispe_lispetorch::tensor_zeros(LispE* lisp) {
    Element* shape_elem = lisp->get_variable(L"shape");
    
    try {
        std::vector<int64_t> shape;
        if (shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: shape must be a list of integers");
        }
        
        torch::Tensor tensor = torch::zeros(shape);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_ones(LispE* lisp) {
    Element* shape_elem = lisp->get_variable(L"shape");
    
    try {
        std::vector<int64_t> shape;
        if (shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: shape must be a list of integers");
        }
        
        torch::Tensor tensor = torch::ones(shape);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_is_mps(LispE* lisp) {
#ifdef __APPLE__
    Element* tensor_elem = lisp->get_variable(L"tensor");
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
    try {
        return booleans_[((TorchTensor*)tensor_elem)->tensor.is_mps()];
    }
    catch (const std::exception& e) {
        return null_;
    }
#else
        return null_;
#endif

}

Element* Lispe_lispetorch::tensor_triu(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* diagonal_elem = lisp->get_variable(L"diagonal");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* input_tensor = (TorchTensor*)tensor_elem;
        int64_t diagonal = 0;  // Default offset
        
        if (diagonal_elem != NULL && diagonal_elem->type == t_integer) {
            diagonal = diagonal_elem->asInteger();
        }
        
        return new TorchTensor(std::move(torch::triu(input_tensor->tensor, diagonal)));
    } catch (const std::exception& e) {
        string msg = "Error in torch_triu: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_set_item(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* indices_elem = lisp->get_variable(L"indices");
    Element* value_elem = lisp->get_variable(L"value");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* input_tensor = (TorchTensor*)tensor_elem;
        
        // Get indices
        std::vector<int64_t> indices;
        if (indices_elem->isList()) {
            for (long i = 0; i < indices_elem->size(); i++) {
                indices.push_back(indices_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: indices must be a list of integers");
        }
                
        // Get value
        double value = value_elem->asFloat();
        
        // Clone the tensor to avoid modifying the original
        torch::Tensor result = input_tensor->tensor.clone();
        
        // Set the item using PyTorch indexing
        if (indices.size() == 1) {
            result[indices[0]] = value;
        } else if (indices.size() == 2) {
            result[indices[0]][indices[1]] = value;
        } else if (indices.size() == 3) {
            result[indices[0]][indices[1]][indices[2]] = value;
        } else if (indices.size() == 4) {
            result[indices[0]][indices[1]][indices[2]][indices[3]] = value;
        } else {
            throw new Error("Error: Maximum 4 dimensions supported for indexing");
        }
        
        return new TorchTensor(std::move(result));
    } catch (const std::exception& e) {
        string msg = "Error in torch_set_item: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_rand(LispE* lisp) {
    Element* shape_elem = lisp->get_variable(L"shape");
    
    try {
        std::vector<int64_t> shape;
        if (shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: shape must be a list of integers");
        }
        
        torch::Tensor tensor = torch::rand(shape);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_randn(LispE* lisp) {
    Element* shape_elem = lisp->get_variable(L"shape");
    
    try {
        std::vector<int64_t> shape;
        if (shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: shape must be a list of integers");
        }
        
        torch::Tensor tensor = torch::randn(shape);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_randint(LispE* lisp) {
    Element* low_elem = lisp->get_variable(L"low");
    Element* high_elem = lisp->get_variable(L"high");
    Element* shape_elem = lisp->get_variable(L"shape");
    
    try {
        // Récupérer les bornes
        int64_t low = low_elem->asInteger();
        int64_t high = high_elem->asInteger();
        
        // Récupérer la forme
        std::vector<int64_t> shape;
        if (shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        } else {
            throw new Error("Error: shape must be a list of integers");
        }
        
        torch::Tensor tensor = torch::randint(low, high, shape);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_transpose(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim0_elem = lisp->get_variable(L"dim0");
    Element* dim1_elem = lisp->get_variable(L"dim1");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        if (dim0_elem->type != t_integer || dim1_elem->type != t_integer) {
            throw new Error("Error: Dimensions must be integers");
        }
        
        TorchTensor* t = (TorchTensor*)tensor_elem;
        int64_t dim0 = dim0_elem->asInteger();
        int64_t dim1 = dim1_elem->asInteger();
        
        return new TorchTensor(std::move(t->tensor.transpose(dim0, dim1)));
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_add(LispE* lisp) {
    Element* tensor1_elem = lisp->get_variable(L"tensor1");
    Element* tensor2_elem = lisp->get_variable(L"tensor2");
    
    try {
        if (tensor1_elem->type != t_tensor || tensor2_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* t1 = (TorchTensor*)tensor1_elem;
        TorchTensor* t2 = (TorchTensor*)tensor2_elem;
        
        return new TorchTensor(std::move(t1->tensor + t2->tensor));
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_mul(LispE* lisp) {
    Element* tensor1_elem = lisp->get_variable(L"tensor1");
    Element* tensor2_elem = lisp->get_variable(L"tensor2");
    
    try {
        if (tensor1_elem->type != t_tensor || tensor2_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* t1 = (TorchTensor*)tensor1_elem;
        TorchTensor* t2 = (TorchTensor*)tensor2_elem;
        
        return new TorchTensor(std::move(t1->tensor * t2->tensor));
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_sub(LispE* lisp) {
    Element* tensor1_elem = lisp->get_variable(L"tensor1");
    Element* tensor2_elem = lisp->get_variable(L"tensor2");
    
    try {
        if (tensor1_elem->type != t_tensor || tensor2_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* t1 = (TorchTensor*)tensor1_elem;
        TorchTensor* t2 = (TorchTensor*)tensor2_elem;
        
        return new TorchTensor(std::move(t1->tensor - t2->tensor));
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_div(LispE* lisp) {
    Element* tensor1_elem = lisp->get_variable(L"tensor1");
    Element* tensor2_elem = lisp->get_variable(L"tensor2");
    
    try {
        if (tensor1_elem->type != t_tensor || tensor2_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* t1 = (TorchTensor*)tensor1_elem;
        TorchTensor* t2 = (TorchTensor*)tensor2_elem;
        
        return new TorchTensor(std::move(t1->tensor / t2->tensor));
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_mul_scalar(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* scalar_elem = lisp->get_variable(L"scalar");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* t = (TorchTensor*)tensor_elem;
        double scalar_value = scalar_elem->asNumber();
        
        torch::Tensor result = t->tensor * scalar_value;
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_add_scalar(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* scalar_elem = lisp->get_variable(L"scalar");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* t = (TorchTensor*)tensor_elem;
        double scalar_value = scalar_elem->asNumber();
        
        torch::Tensor result = t->tensor + scalar_value;
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_div_scalar(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* scalar_elem = lisp->get_variable(L"scalar");

    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }

        TorchTensor* t = (TorchTensor*)tensor_elem;
        double scalar_value = scalar_elem->asNumber();

        if (scalar_value == 0.0) {
            throw new Error("Error: Division by zero");
        }

        torch::Tensor result = t->tensor / scalar_value;
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in tensor_div_scalar: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_shape(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* t = (TorchTensor*)tensor_elem;
        auto sizes = t->tensor.sizes();
        
        Integers* result = lisp->provideIntegers();
        for (int64_t size : sizes) {
            result->liste.push_back(size);
        }
        
        return result;
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_item(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");

    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }

        TorchTensor* t = (TorchTensor*)tensor_elem;

        // Vérifier que le tenseur ne contient qu'un seul élément
        if (t->tensor.numel() != 1) {
            string msg = "Error: tensor must contain exactly one element, but has ";
            msg += std::to_string(t->tensor.numel());
            msg += " elements";
            throw new Error(msg);
        }

        if (t->tensor.dtype() == torch::kBFloat16) {        
            return lisp->provideFloat(t->tensor.item<float>());
        } 
        if (t->tensor.dtype() == torch::kFloat32) {        
            return lisp->provideFloat(t->tensor.item<float>());
        }
        if (t->tensor.dtype() == torch::kFloat64) {
            return lisp->provideNumber(t->tensor.item<double>());
        }
        if (t->tensor.dtype() == torch::kInt64) {
            return lisp->provideInteger(t->tensor.item<int64_t>());
        }
        if (t->tensor.dtype() == torch::kInt32) {
            return lisp->provideInteger(t->tensor.item<int64_t>());
        }
        if (t->tensor.dtype() == torch::kInt16) {
            return new Short(t->tensor.item<int16_t>());
        }
        if (t->tensor.dtype() == torch::kInt8) {
            return new Short(t->tensor.item<int16_t>());
        }
        if (t->tensor.dtype() == torch::kBool) {
            return t->tensor.item<bool>() ? true_ : false_;
        }
    } catch (const std::exception& e) {
        string msg = "Error in torch_item: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_matmul(LispE* lisp) {
    Element* tensor1_elem = lisp->get_variable(L"tensor1");
    Element* tensor2_elem = lisp->get_variable(L"tensor2");
    
    try {
        if (tensor1_elem->type != t_tensor || tensor2_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* t1 = (TorchTensor*)tensor1_elem;
        TorchTensor* t2 = (TorchTensor*)tensor2_elem;
        
        torch::Tensor result = torch::matmul(t1->tensor, t2->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_linear(LispE* lisp) {
    Element* tensor1_elem = lisp->get_variable(L"tensor1");
    Element* tensor2_elem = lisp->get_variable(L"tensor2");
    
    try {
        if (tensor1_elem->type != t_tensor || tensor2_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* t1 = (TorchTensor*)tensor1_elem;
        TorchTensor* t2 = (TorchTensor*)tensor2_elem;
        
        torch::Tensor result = torch::linear(t1->tensor, t2->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_contiguous(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");

    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        torch::Tensor& tmp = ((TorchTensor*)tensor_elem)->tensor;

        torch::Tensor result = tmp.contiguous();
        // ✅ Comparer les pointeurs de données
        if (result.data_ptr() == tmp.data_ptr()) {
              // Même buffer → retourner l'élément existant
              return tensor_elem;
          }

        return new TorchTensor(std::move(result));
    } catch (const std::exception& e) {
        string msg = "Error in torch_contiguous: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_reshape(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* shape_elem = lisp->get_variable(L"shape");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        
        std::vector<int64_t> shape;
        if (shape_elem->type == t_integers) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(((Integers*)shape_elem)->liste[i]);
            }
        }
        else {
            if (shape_elem->isList()) {
                for (long i = 0; i < shape_elem->size(); i++) {
                    shape.push_back(shape_elem->index(i)->asInteger());
                }
            } else {
                throw new Error("Error: shape must be a list of integers");
            }
        }
        
        // ✅ Essayer view d'abord (pas de copie garantie)
        torch::Tensor result;
        if (tensor->tensor.is_contiguous()) {
            result = tensor->tensor.view(shape);  // Vue pure, 0 allocation
        } else {
            result = tensor->tensor.reshape(shape);  // Peut copier
        }
        return new TorchTensor(std::move(result));
    } catch (const std::exception& e) {
        string msg = "Error reshaping tensor: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_unsqueeze(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        
        int64_t dim;
        if (dim_elem->type == t_integer) {
            dim = dim_elem->asInteger();
        } else {
            throw new Error("Error: dim must be an integer");
        }
        
        torch::Tensor result = tensor->tensor.unsqueeze(dim);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_unsqueeze: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_squeeze(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");

    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }

        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result;

        // Si dim est fourni, squeeze seulement cette dimension
        if (dim_elem != null_) {
            if (dim_elem->type != t_integer) {
                throw new Error("Error: dim must be an integer");
            }
            int64_t dim = dim_elem->asInteger();
            result = tensor->tensor.squeeze(dim);
        } else {
            // Sinon squeeze toutes les dimensions de taille 1
            result = tensor->tensor.squeeze();
        }

        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_squeeze: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_slice(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    Element* start_elem = lisp->get_variable(L"start");
    Element* end_elem = lisp->get_variable(L"end");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        
        if (dim_elem->type != t_integer) {
            throw new Error("Error: Dimension must be an integer");
        }
        
        if (start_elem->type != t_integer) {
            throw new Error("Error: Start index must be an integer");
        }
        
        if (end_elem->type != t_integer) {
            throw new Error("Error: End index must be an integer");
        }
        
        long dim = dim_elem->asInteger();
        long start = start_elem->asInteger();
        long end = end_elem->asInteger();
        
        torch::Tensor result = tensor->tensor.slice(dim, start, end);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error slicing tensor: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_select(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    Element* index_elem = lisp->get_variable(L"index");

    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }

        TorchTensor* tensor = (TorchTensor*)tensor_elem;

        if (dim_elem->type != t_integer) {
            throw new Error("Error: Dimension must be an integer");
        }

        if (index_elem->type != t_integer) {
            throw new Error("Error: Index must be an integer");
        }

        long dim = dim_elem->asInteger();
        long index = index_elem->asInteger();

        // select() sélectionne un index spécifique sur une dimension et réduit le rang du tenseur
        torch::Tensor result = tensor->tensor.select(dim, index);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error selecting from tensor: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_sum(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor sum_tensor = tensor->tensor.sum();
        
        // Retourner la valeur scalaire comme nombre
        if (sum_tensor.dtype() == torch::kFloat32) {
            return lisp->provideFloat(sum_tensor.item<float>());
        } else {
            return lisp->provideFloat(sum_tensor.item<double>());
        }
    } catch (const std::exception& e) {
        string msg = "Error computing tensor sum: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_mean(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor mean_tensor = tensor->tensor.mean();
        
        // Retourner la valeur scalaire comme nombre
        if (mean_tensor.dtype() == torch::kFloat32) {
            return lisp->provideFloat(mean_tensor.item<float>());
        } else {
            return lisp->provideFloat(mean_tensor.item<double>());
        }
    } catch (const std::exception& e) {
        string msg = "Error computing tensor mean: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_mean_dim(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        int64_t dim = dim_elem->asInteger();
        
        torch::Tensor result = tensor->tensor.mean(dim);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor mean along dimension: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_std(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor std_tensor = tensor->tensor.std();
        
        // Retourner la valeur scalaire comme nombre
        if (std_tensor.dtype() == torch::kFloat32) {
            return lisp->provideFloat(std_tensor.item<float>());
        } else {
            return lisp->provideFloat(std_tensor.item<double>());
        }
    } catch (const std::exception& e) {
        string msg = "Error computing tensor std: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_max(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor max_tensor = tensor->tensor.max();
        
        // Retourner la valeur scalaire comme nombre
        if (max_tensor.dtype() == torch::kFloat32) {
            return lisp->provideFloat(max_tensor.item<float>());
        } else {
            return lisp->provideFloat(max_tensor.item<double>());
        }
    } catch (const std::exception& e) {
        string msg = "Error computing tensor max: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_min(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor min_tensor = tensor->tensor.min();
        
        // Retourner la valeur scalaire comme nombre
        if (min_tensor.dtype() == torch::kFloat32) {
            return lisp->provideFloat(min_tensor.item<float>());
        } else {
            return lisp->provideFloat(min_tensor.item<double>());
        }
    } catch (const std::exception& e) {
        string msg = "Error computing tensor min: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_einsum(LispE* lisp) {
    string indices = lisp->get_variable(L"indices")->toString(lisp);
    Element* tensor_list = lisp->get_variable(L"tensors");

    if (tensor_list->type != t_list) {
        throw new Error("Error: second argument must be a list of tensors");
    }

    List* tensors = (List*)tensor_list;
    if (tensors->size() == 0) {
        throw new Error("Error: tensor list cannot be empty");
    }

    try {
        // Convertir la liste LispE en vecteur de tensors PyTorch
        std::vector<torch::Tensor> torch_tensors;
        torch_tensors.reserve(tensors->size());
        
        for (long i = 0; i < tensors->size(); i++) {
            Element* elem = tensors->liste[i];
            if (elem->type != t_tensor) {
                throw new Error("Error: all elements in the list must be tensors");
            }
            torch_tensors.push_back(((TorchTensor*)elem)->tensor);
        }
        
        auto result = torch::einsum(indices, torch_tensors);
        return new TorchTensor(std::move(result));
    } catch (const std::exception& e) {
        string msg = "Error computing tensor einsum: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_argmax(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    bool keepdim = lisp->get_variable(L"keepdim")->Boolean();
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        
        if (dim_elem == NULL || dim_elem == null_) {
            // Pas de dimension spécifiée, argmax global
            torch::Tensor argmax_tensor = tensor->tensor.argmax();
            return new TorchTensor(argmax_tensor);
        } else {
            // Dimension spécifiée
            long dim = dim_elem->asInteger();
            torch::Tensor argmax_tensor = tensor->tensor.argmax(dim,keepdim);
            return new TorchTensor(std::move(argmax_tensor));
        }
    } catch (const std::exception& e) {
        string msg = "Error computing tensor argmax: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_relu(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::relu(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor relu: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_sigmoid(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::sigmoid(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor sigmoid: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_abs(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::abs(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor abs: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_exp(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::exp(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor exp: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_tanh(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::tanh(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor tanh: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_gelu(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::gelu(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor gelu: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_silu(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch::silu(tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor silu: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_softmax(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        int64_t dim = dim_elem->asInteger();
        
        torch::Tensor result = torch::softmax(tensor->tensor, dim);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor softmax: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_log_softmax(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        int64_t dim = dim_elem->asInteger();
        
        torch::Tensor result = torch::log_softmax(tensor->tensor, dim);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor log_softmax: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_full_like(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* fill_value_elem = lisp->get_variable(L"fill_value");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        double fill_value = fill_value_elem->asFloat();
        
        torch::Tensor result = torch::full_like(tensor->tensor, fill_value);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error creating tensor_full_like: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_cumsum(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        int64_t dim = dim_elem->asInteger();
        
        torch::Tensor result = torch::cumsum(tensor->tensor, dim);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor cumsum: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_gather(LispE* lisp) {
    Element* input_elem = lisp->get_variable(L"input");
    Element* dim_elem = lisp->get_variable(L"dim");
    Element* index_elem = lisp->get_variable(L"index");
    
    try {
        if (input_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        if (index_elem->type != t_tensor) {
            throw new Error("Error: Third argument must be a tensor");
        }
        
        TorchTensor* input_tensor = (TorchTensor*)input_elem;
        TorchTensor* index_tensor = (TorchTensor*)index_elem;
        int64_t dim = dim_elem->asInteger();
        
        torch::Tensor result = torch::gather(input_tensor->tensor, dim, index_tensor->tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor gather: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_masked_fill_(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* mask_elem = lisp->get_variable(L"mask");
    Element* value_elem = lisp->get_variable(L"value");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        if (mask_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        TorchTensor* mask = (TorchTensor*)mask_elem;
        double value = value_elem->asFloat();
        
        // Convertir le masque en booléen si nécessaire
        torch::Tensor bool_mask = mask->tensor.to(torch::kBool);
        
        // masked_fill_ modifie le tensor in-place
        tensor->tensor.masked_fill_(bool_mask, value);
        return tensor;
    } catch (const std::exception& e) {
        string msg = "Error computing tensor masked_fill_: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_size(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        auto sizes = tensor->tensor.sizes();
        
        Integers* result = lisp->provideIntegers();
        for (auto size : sizes) {
            result->liste.push_back(size);
        }
        return result;
    } catch (const std::exception& e) {
        string msg = "Error getting tensor size: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_pow(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* exponent_elem = lisp->get_variable(L"exponent");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result;
        
        if (exponent_elem->isScalar()) {
            double exponent = exponent_elem->asFloat();
            result = tensor->tensor.pow(exponent);
        } else if (exponent_elem->type == t_tensor) {
            TorchTensor* exp_tensor = (TorchTensor*)exponent_elem;
            result = tensor->tensor.pow(exp_tensor->tensor);
        } else {
            throw new Error("Error: Exponent must be a scalar or tensor");
        }
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor power: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_sqrt(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.sqrt();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor sqrt: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_rsqrt(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.rsqrt();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor rsqrt: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_log(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.log();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor log: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_log10(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.log10();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor log10: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_log2(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.log2();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor log2: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_sin(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.sin();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor sin: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_cos(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.cos();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor cos: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_tan(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.tan();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor tan: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_asin(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.asin();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor asin: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_acos(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.acos();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor acos: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_atan(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.atan();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor atan: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_sinh(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.sinh();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor sinh: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_cosh(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.cosh();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor cosh: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_floor(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.floor();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor floor: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_ceil(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.ceil();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor ceil: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_round(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.round();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor round: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_clamp(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* min_elem = lisp->get_variable(L"min_val");
    Element* max_elem = lisp->get_variable(L"max_val");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result;
        
        if (min_elem != null_ && max_elem != null_) {
            double min_val = min_elem->asFloat();
            double max_val = max_elem->asFloat();
            result = tensor->tensor.clamp(min_val, max_val);
        } else if (min_elem != null_) {
            double min_val = min_elem->asFloat();
            result = tensor->tensor.clamp_min(min_val);
        } else if (max_elem != null_) {
            double max_val = max_elem->asFloat();
            result = tensor->tensor.clamp_max(max_val);
        } else {
            throw new Error("Error: At least one of min_val or max_val must be provided");
        }
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor clamp: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_neg(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.neg();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor neg: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_reciprocal(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = tensor->tensor.reciprocal();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing tensor reciprocal: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_rms_norm(LispE* lisp) {
    Element* input_elem = lisp->get_variable(L"input");
    Element* weight_elem = lisp->get_variable(L"weight");
    Element* eps_elem = lisp->get_variable(L"eps");
    
    try {
        if (input_elem->type != t_tensor || weight_elem->type != t_tensor) {
            throw new Error("Error: Input and weight must be tensors");
        }
        
        TorchTensor* input_tensor = (TorchTensor*)input_elem;
        TorchTensor* weight_tensor = (TorchTensor*)weight_elem;
        
        double eps = 1e-6;  // valeur par défaut
        if (eps_elem != null_) {
            eps = eps_elem->asFloat();
        }
        
        // RMSNorm: x * weight / sqrt(mean(x^2) + eps)
        // 1. Calculer x^2        
        // 2. Calculer mean(x^2) avec keepdim=true sur la dernière dimension
        auto variance = input_tensor->tensor.pow(2).mean(-1, true);
        
        // 3. Calculer rsqrt(variance + eps)
        auto rms_factor = torch::rsqrt(variance + eps);
        
        // 4. Normaliser: input * rms_factor
        auto normalized = input_tensor->tensor * rms_factor;
        
        // 5. Appliquer le poids: normalized * weight
        auto result = normalized * weight_tensor->tensor;
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error computing RMS normalization: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::model_create(LispE* lisp) {
    Element* input_size_elem = lisp->get_variable(L"input_size");
    Element* hidden_size_elem = lisp->get_variable(L"hidden_size");
    Element* output_size_elem = lisp->get_variable(L"output_size");
    
    try {
        int64_t input_size = input_size_elem->asInteger();
        int64_t hidden_size = hidden_size_elem->asInteger();
        int64_t output_size = output_size_elem->asInteger();
        
        auto model = std::make_shared<SimpleMLP>(input_size, hidden_size, output_size);
        return new TorchModel(model);
    } catch (const std::exception& e) {
        string msg = "Error creating model: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::model_forward(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (model_elem->type != t_model) {
            throw new Error("Error: First argument must be a model");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchModel* model = (TorchModel*)model_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        // Cast vers SimpleMLP pour accéder à la méthode forward
        auto mlp = std::dynamic_pointer_cast<SimpleMLP>(model->module);
        if (!mlp) {
            throw new Error("Error: Unsupported model type");
        }
        
        torch::Tensor output = mlp->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in forward pass: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::optimizer_create(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* lr_elem = lisp->get_variable(L"learning_rate");
    Element* type_elem = lisp->get_variable(L"type");
    
    try {
        if (model_elem->type != t_model) {
            throw new Error("Error: First argument must be a model");
        }
        
        TorchModel* model = (TorchModel*)model_elem;
        double lr = lr_elem->asNumber();
        wstring type = type_elem->asString(lisp);
        
        std::unique_ptr<torch::optim::Optimizer> optimizer;
        
        if (type == L"adam") {
            optimizer = std::make_unique<torch::optim::Adam>(
                model->module->parameters(), torch::optim::AdamOptions(lr));
        } else if (type == L"sgd") {
            optimizer = std::make_unique<torch::optim::SGD>(
                model->module->parameters(), torch::optim::SGDOptions(lr));
        } else {
            throw new Error("Error: Unsupported optimizer type");
        }
        
        return new TorchOptimizer(std::move(optimizer));
    } catch (const std::exception& e) {
        string msg = "Error creating optimizer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::optimizer_step(LispE* lisp) {
    Element* optimizer_elem = lisp->get_variable(L"optimizer");
    
    try {
        if (optimizer_elem->type != t_optimizer) {
            throw new Error("Error: Argument must be an optimizer");
        }
        
        TorchOptimizer* opt = (TorchOptimizer*)optimizer_elem;
        opt->optimizer->step();
        
        return True_;
    } catch (const std::exception& e) {
        string msg = "Error in optimizer step: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::optimizer_zero_grad(LispE* lisp) {
    Element* optimizer_elem = lisp->get_variable(L"optimizer");

    try {
        if (optimizer_elem->type != t_optimizer) {
            throw new Error("Error: Argument must be an optimizer");
        }

        TorchOptimizer* opt = (TorchOptimizer*)optimizer_elem;
        opt->optimizer->zero_grad();

        return True_;
    } catch (const std::exception& e) {
        string msg = "Error zeroing gradients: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::clip_grad_norm(LispE* lisp) {
    Element* optimizer_elem = lisp->get_variable(L"optimizer");
    Element* max_norm_elem = lisp->get_variable(L"max_norm");

    try {
        if (optimizer_elem->type != t_optimizer) {
            throw new Error("Error: First argument must be an optimizer");
        }

        TorchOptimizer* opt = (TorchOptimizer*)optimizer_elem;
        double max_norm = max_norm_elem->asNumber();

        // Collect all parameters from optimizer's param groups
        std::vector<torch::Tensor> parameters;
        for (const auto& group : opt->optimizer->param_groups()) {
            for (const auto& param : group.params()) {
                parameters.push_back(param);
            }
        }

        // Clip gradients and get total norm
        double total_norm = torch::nn::utils::clip_grad_norm_(parameters, max_norm);

        return lisp->provideNumber(total_norm);
    } catch (const std::exception& e) {
        string msg = "Error in clip_grad_norm: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::optimizer_add_params(LispE* lisp) {
    Element* params_elem = lisp->get_variable(L"params");
    Element* lr_elem = lisp->get_variable(L"learning_rate");
    Element* wd_elem = lisp->get_variable(L"weight_decay");

    try {
        // Vérifier que params est une liste
        if (!params_elem->isList()) {
            throw new Error("Error: params must be a list of tensors");
        }

        // Extraire les tensors de la liste
        std::vector<torch::Tensor> parameters;
        for (long i = 0; i < params_elem->size(); i++) {
            Element* tensor_elem = params_elem->index(i);
            if (tensor_elem->type != t_tensor) {
                throw new Error("Error: All elements in params must be tensors");
            }
            TorchTensor* t = (TorchTensor*)tensor_elem;
            parameters.push_back(t->tensor);
        }

        // Extraire learning rate et weight decay
        double lr = lr_elem->asNumber();
        double weight_decay = wd_elem->asNumber();

        // Créer un optimiseur AdamW avec les paramètres
        auto options = torch::optim::AdamWOptions(lr);
        options.weight_decay(weight_decay);

        auto optimizer = std::make_unique<torch::optim::AdamW>(parameters, options);

        return new TorchOptimizer(std::move(optimizer));
    } catch (const std::exception& e) {
        string msg = "Error adding parameters to optimizer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::adam_optimizer_create(LispE* lisp) {
    Element* lr_elem = lisp->get_variable(L"learning_rate");
    
    try {
        double lr = lr_elem->asNumber();
        
        // Créer un optimiseur Adam vide (sera attaché à un modèle plus tard)
        auto optimizer = std::make_unique<torch::optim::Adam>(
            std::vector<torch::Tensor>(), torch::optim::AdamOptions(lr));
        
        return new TorchOptimizer(std::move(optimizer));
    } catch (const std::exception& e) {
        string msg = "Error creating Adam optimizer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::adamw_optimizer_create(LispE* lisp) {
    Element* lr_elem = lisp->get_variable(L"learning_rate");
    
    try {
        double lr = lr_elem->asNumber();
        
        // Créer un optimiseur AdamW vide (sera attaché à un modèle plus tard)
        auto optimizer = std::make_unique<torch::optim::AdamW>(
            std::vector<torch::Tensor>(), torch::optim::AdamWOptions(lr));
        
        return new TorchOptimizer(std::move(optimizer));
    } catch (const std::exception& e) {
        string msg = "Error creating AdamW optimizer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::sgd_optimizer_create(LispE* lisp) {
    Element* lr_elem = lisp->get_variable(L"learning_rate");
    
    try {
        double lr = lr_elem->asNumber();
        
        // Créer un optimiseur SGD vide (sera attaché à un modèle plus tard)
        auto optimizer = std::make_unique<torch::optim::SGD>(
            std::vector<torch::Tensor>(), torch::optim::SGDOptions(lr));
        
        return new TorchOptimizer(std::move(optimizer));
    } catch (const std::exception& e) {
        string msg = "Error creating SGD optimizer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::loss_mse(LispE* lisp) {
    Element* predictions_elem = lisp->get_variable(L"predictions");
    Element* targets_elem = lisp->get_variable(L"targets");
    
    try {
        if (predictions_elem->type != t_tensor || targets_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* pred = (TorchTensor*)predictions_elem;
        TorchTensor* target = (TorchTensor*)targets_elem;
        
        torch::Tensor loss = torch::mse_loss(pred->tensor, target->tensor);
        return new TorchTensor(std::move(loss));
    } catch (const std::exception& e) {
        string msg = "Error calculating MSE loss: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::loss_crossentropy(LispE* lisp) {
    Element* predictions_elem = lisp->get_variable(L"predictions");
    Element* targets_elem = lisp->get_variable(L"targets");
    
    try {
        if (predictions_elem->type != t_tensor || targets_elem->type != t_tensor) {
            throw new Error("Error: Both arguments must be tensors");
        }
        
        TorchTensor* pred = (TorchTensor*)predictions_elem;
        TorchTensor* target = (TorchTensor*)targets_elem;
        
        torch::Tensor loss = torch::cross_entropy_loss(pred->tensor, target->tensor);
        return new TorchTensor(std::move(loss));
    } catch (const std::exception& e) {
        string msg = "Error calculating cross-entropy loss: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::backward(LispE* lisp) {
    Element* loss_elem = lisp->get_variable(L"loss");
    Element* retain_element = lisp->get_variable(L"retain_graph");
    Element* create_element = lisp->get_variable(L"create_graph");

    bool retain = retain_element->Boolean();
    bool create = create_element->Boolean();

    try {
        if (loss_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor (loss)");
        }

        TorchTensor* loss = (TorchTensor*)loss_elem;
        loss->tensor.backward({}, retain, create);
        return True_;
    } catch (const std::exception& e) {
        string msg = "Error in backward pass: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::set_grad_enabled(LispE* lisp) {
    Element* enabled_elem = lisp->get_variable(L"enabled");

    try {
        bool enabled = enabled_elem->Boolean();
        torch::autograd::GradMode::set_enabled(enabled);

        return True_;
    } catch (const std::exception& e) {
        string msg = "Error in set_grad_enabled: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::cuda_memory_allocated(LispE* lisp) {
        try {
#ifdef USE_CUDA
        size_t free_memory, total_memory;
        cudaError_t err = cudaMemGetInfo(&free_memory, &total_memory);
        if (err != cudaSuccess) {
            throw new Error("Failed to get CUDA memory info: " + std::string(cudaGetErrorString(err)));
        }

        // Calculer la mémoire allouée (total - libre)
        size_t allocated_bytes = total_memory - free_memory;
        return lisp->provideInteger(allocated_bytes);  
#else
    return 0;
#endif
    } catch (const std::exception& e) {
        string msg = "Error checking CUDA availability: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::cuda_memory_total(LispE* lisp) {
        try {
#ifdef USE_CUDA
        size_t free_memory, total_memory;
        cudaError_t err = cudaMemGetInfo(&free_memory, &total_memory);
        if (err != cudaSuccess) {
            throw new Error("Failed to get CUDA memory info: " + std::string(cudaGetErrorString(err)));
        }
        return lisp->provideInteger(total_memory);  
#else
    return 0;
#endif
    } catch (const std::exception& e) {
        string msg = "Error checking CUDA availability: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::cuda_is_available(LispE* lisp) {
    try {
#ifdef USE_CUDA
        bool available = torch::cuda::is_available();
        return available ? True_ : False_;
#else
        return False_;
#endif
    } catch (const std::exception& e) {
        string msg = "Error checking CUDA availability: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::cuda_device_count(LispE* lisp) {
    try {
#ifdef USE_CUDA
        if (torch::cuda::is_available()) {
            int device_count = torch::cuda::device_count();
            return new Integer(device_count);
        } else {
            return new Integer(0);
        }
#else
        return new Integer(0);
#endif
    } catch (const std::exception& e) {
        string msg = "Error getting CUDA device count: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::set_device(LispE* lisp) {
    Element* device_elem = lisp->get_variable(L"device");
    
    try {
#ifdef USE_CUDA
        if (torch::cuda::is_available()) {
            int device_id = device_elem->asInteger();
            if (device_id >= 0 && device_id < torch::cuda::device_count()) {
#ifdef __linux__
                // Utiliser c10::cuda::set_device pour PyTorch 2.x sur Linux
                c10::cuda::set_device(device_id);
#else
                // Utiliser torch::cuda::set_device pour macOS (versions antérieures de PyTorch)
                torch::cuda::set_device(device_id);
#endif
                return True_;
            } else {
                throw new Error("Error: Invalid device ID");
            }
        } else {
            throw new Error("Error: CUDA not available");
        }
#else
        throw new Error("Error: CUDA support not compiled");
#endif
    } catch (const std::exception& e) {
        string msg = "Error setting CUDA device: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_to_cuda(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* cuda_device = lisp->get_variable(L"device");

    #ifdef USE_CUDA
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        torch::Device device = torch::kCUDA;
        if (cuda_device != null_) { 
            device = lookup_device(cuda_device->toString(lisp));
        }
    #endif

    try {
        
#ifdef USE_CUDA
        if (torch::cuda::is_available()) {
            TorchTensor* tensor = (TorchTensor*)tensor_elem;
            torch::Tensor cuda_tensor = tensor->tensor.to(device);
            return new TorchTensor(cuda_tensor);
        } else {
            throw new Error("Error: CUDA not available");
        }
#else
        throw new Error("Error: CUDA support not compiled");
#endif
    } catch (const std::exception& e) {
        string msg = "Error moving tensor to CUDA: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_to_device(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* cuda_device = lisp->get_variable(L"device");

        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }

        torch::Device device = lookup_device(cuda_device->toString(lisp));

    try {        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        torch::Tensor cuda_tensor = tensor->tensor.to(device);
        return new TorchTensor(cuda_tensor);
    } catch (const std::exception& e) {
        string msg = "Error moving tensor to CUDA: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::mps_is_available(LispE* lisp) {
    try {
#ifdef __APPLE__
        bool available = torch::mps::is_available();
        return available ? True_ : False_;
#else
        return False_;
#endif
    } catch (const std::exception& e) {
        string msg = "Error checking MPS availability: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_to_mps(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        
#ifdef __APPLE__
        if (torch::mps::is_available()) {
            TorchTensor* tensor = (TorchTensor*)tensor_elem;
            if (tensor->tensor.is_mps()) {
                return tensor_elem;
            }
            torch::Tensor result = tensor->tensor.to(torch::kMPS);
            return new TorchTensor(std::move(result));
        }
        else {
            throw new Error("Error: MPS not available");
        }
#else
        throw new Error("Error: MPS only available on macOS");
#endif
    }
    catch (const std::exception& e) {
        string msg = "Error moving tensor to MPS: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::get_best_device(LispE* lisp) {
    try {
        // Priorité: MPS (macOS) > CUDA > CPU
#ifdef __APPLE__
        if (torch::mps::is_available()) {
            return new String("mps");
        }
#endif
#ifdef USE_CUDA
        if (torch::cuda::is_available()) {
            return new String("cuda");
        }
#endif
        return new String("cpu");
    } catch (const std::exception& e) {
        string msg = "Error detecting best device: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::multihead_attention_create(LispE* lisp) {
    Element* embed_dim_elem = lisp->get_variable(L"embed_dim");
    Element* num_heads_elem = lisp->get_variable(L"num_heads");
    
    try {
        int64_t embed_dim = embed_dim_elem->asInteger();
        int64_t num_heads = num_heads_elem->asInteger();
        
        return new TorchMultiHeadAttention(embed_dim, num_heads);
    } catch (const std::exception& e) {
        string msg = "Error creating multi-head attention: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::multihead_attention_forward(LispE* lisp) {
    Element* attention_elem = lisp->get_variable(L"attention");
    Element* query_elem = lisp->get_variable(L"query");
    Element* key_elem = lisp->get_variable(L"key");
    Element* value_elem = lisp->get_variable(L"value");
    
    try {
        if (attention_elem->type != t_attention) {
            throw new Error("Error: First argument must be a multi-head attention layer");
        }
        if (query_elem->type != t_tensor || key_elem->type != t_tensor || value_elem->type != t_tensor) {
            throw new Error("Error: Query, key, and value must be tensors");
        }
        
        TorchMultiHeadAttention* attention = (TorchMultiHeadAttention*)attention_elem;
        TorchTensor* query = (TorchTensor*)query_elem;
        TorchTensor* key = (TorchTensor*)key_elem;
        TorchTensor* value = (TorchTensor*)value_elem;
        
        auto output = std::get<0>(attention->attention->forward(
            query->tensor, key->tensor, value->tensor));
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in multi-head attention forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::layer_norm_create(LispE* lisp) {
    Element* normalized_shape_elem = lisp->get_variable(L"normalized_shape");
    
    try {
        int64_t normalized_shape = normalized_shape_elem->asInteger();
        return new TorchLayerNorm(normalized_shape);
    } catch (const std::exception& e) {
        string msg = "Error creating layer norm: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::layer_norm_forward(LispE* lisp) {
    Element* layer_norm_elem = lisp->get_variable(L"layer_norm");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (layer_norm_elem->type != t_layernorm) {
            throw new Error("Error: First argument must be a layer norm");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchLayerNorm* layer_norm = (TorchLayerNorm*)layer_norm_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = layer_norm->layer_norm->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in layer norm forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::embedding_create(LispE* lisp) {
    Element* num_embeddings_elem = lisp->get_variable(L"num_embeddings");
    Element* embedding_dim_elem = lisp->get_variable(L"embedding_dim");
    
    try {
        int64_t num_embeddings = num_embeddings_elem->asInteger();
        int64_t embedding_dim = embedding_dim_elem->asInteger();
        
        return new TorchEmbedding(num_embeddings, embedding_dim);
    } catch (const std::exception& e) {
        string msg = "Error creating embedding: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::embedding_forward(LispE* lisp) {
    Element* embedding_elem = lisp->get_variable(L"embedding");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (embedding_elem->type != t_embedding) {
            throw new Error("Error: First argument must be an embedding layer");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchEmbedding* embedding = (TorchEmbedding*)embedding_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = embedding->embedding->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in embedding forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::linear_create(LispE* lisp) {
    Element* in_features_elem = lisp->get_variable(L"in_features");
    Element* out_features_elem = lisp->get_variable(L"out_features");
    
    try {
        int64_t in_features = in_features_elem->asInteger();
        int64_t out_features = out_features_elem->asInteger();
        
        return new TorchLinear(in_features, out_features);
    } catch (const std::exception& e) {
        string msg = "Error creating linear layer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::linear_forward(LispE* lisp) {
    Element* linear_elem = lisp->get_variable(L"linear");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (linear_elem->type != t_linear) {
            throw new Error("Error: First argument must be a linear layer");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchLinear* linear = (TorchLinear*)linear_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = linear->linear->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in linear forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::transformer_block_create(LispE* lisp) {
    Element* embed_dim_elem = lisp->get_variable(L"embed_dim");
    Element* num_heads_elem = lisp->get_variable(L"num_heads");
    Element* ffn_dim_elem = lisp->get_variable(L"ffn_dim");
    
    try {
        int64_t embed_dim = embed_dim_elem->asInteger();
        int64_t num_heads = num_heads_elem->asInteger();
        int64_t ffn_dim = ffn_dim_elem->asInteger();
        
        auto block = std::make_shared<TransformerBlock>(embed_dim, num_heads, ffn_dim);
        return new TorchTransformerBlock(block);
    } catch (const std::exception& e) {
        string msg = "Error creating transformer block: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::transformer_block_forward(LispE* lisp) {
    Element* block_elem = lisp->get_variable(L"block");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (block_elem->type != TorchTransformerBlock::transformer_block_type) {
            throw new Error("Error: First argument must be a transformer block");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchTransformerBlock* block = (TorchTransformerBlock*)block_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = block->block->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in transformer block forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tokenizer_simple_create(LispE* lisp) {
    try {
        return new TorchTokenizer(); // Crée un tokenizer simple
    } catch (const std::exception& e) {
        string msg = "Error creating simple tokenizer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tokenizer_sentencepiece_create(LispE* lisp) {
#ifdef USE_SENTENCEPIECE
    Element* model_path_elem = lisp->get_variable(L"model_path");
    
    if (model_path_elem == null_) {
        throw new Error("Error: model_path parameter required for SentencePiece tokenizer");
    }
    
    try {
        wstring wmodel_path = model_path_elem->asString(lisp);
        string model_path(wmodel_path.begin(), wmodel_path.end());
        
        return new TorchTokenizer(model_path); // Crée un tokenizer SentencePiece
    } catch (const std::exception& e) {
        string msg = "Error creating SentencePiece tokenizer: ";
        msg += e.what();
        throw new Error(msg);
    }
#else
    throw new Error("Error: SentencePiece support not compiled. Install with: brew install sentencepiece");
#endif
}

Element* Lispe_lispetorch::tokenizer_sentencepiece_train(LispE* lisp) {
#ifdef USE_SENTENCEPIECE
    Element* input_file_elem = lisp->get_variable(L"input_file");
    Element* model_prefix_elem = lisp->get_variable(L"model_prefix");
    Element* vocab_size_elem = lisp->get_variable(L"vocab_size");
    Element* model_type_elem = lisp->get_variable(L"model_type");
    
    if (input_file_elem == null_ || model_prefix_elem == null_) {
        throw new Error("Error: input_file and model_prefix parameters required");
    }
    
    try {
        wstring winput_file = input_file_elem->asString(lisp);
        wstring wmodel_prefix = model_prefix_elem->asString(lisp);
        string input_file(winput_file.begin(), winput_file.end());
        string model_prefix(wmodel_prefix.begin(), wmodel_prefix.end());
        
        int vocab_size = vocab_size_elem ? vocab_size_elem->asInteger() : 8000;
        wstring wmodel_type = model_type_elem ? model_type_elem->asString(lisp) : L"bpe";
        string model_type(wmodel_type.begin(), wmodel_type.end());
        
        // Construire la commande d'entraînement
        string train_command = 
            "--input=" + input_file +
            " --model_prefix=" + model_prefix +
            " --vocab_size=" + std::to_string(vocab_size) +
            " --model_type=" + model_type +
            " --character_coverage=1.0" +
            " --pad_id=0 --unk_id=1 --bos_id=2 --eos_id=3";
        
        auto status = sentencepiece::SentencePieceTrainer::Train(train_command);
        
        if (status.ok()) {
            string msg = "Training completed successfully. Model saved as: " + model_prefix + ".model";
            return new String(msg);
        } else {
            string msg = "Error training SentencePiece model: ";
            msg += status.ToString();
            throw new Error(msg);
        }
    } catch (const std::exception& e) {
        string msg = "Error training SentencePiece model: ";
        msg += e.what();
        throw new Error(msg);
    }
#else
    throw new Error("Error: SentencePiece support not compiled. Install with: brew install sentencepiece");
#endif
}

Element* Lispe_lispetorch::tokenizer_encode(LispE* lisp) {
    Element* tokenizer_elem = lisp->get_variable(L"tokenizer");
    Element* text_elem = lisp->get_variable(L"text");
    
    try {
        if (tokenizer_elem->type != TorchTokenizer::tokenizer_type) {
            throw new Error("Error: First argument must be a tokenizer");
        }
        
        TorchTokenizer* tokenizer = (TorchTokenizer*)tokenizer_elem;
        wstring wtext = text_elem->asString(lisp);
        string text;
        s_unicode_to_utf8(text, wtext);  // Conversion correcte UTF-32 -> UTF-8
        
        auto token_ids = tokenizer->encode(text);
        
        // Convertir en tensor PyTorch
        torch::Tensor tensor = torch::tensor(token_ids, torch::kLong);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        string msg = "Error encoding text: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tokenizer_decode(LispE* lisp) {
    Element* tokenizer_elem = lisp->get_variable(L"tokenizer");
    Element* token_ids_elem = lisp->get_variable(L"token_ids");
    
    try {
        if (tokenizer_elem->type != TorchTokenizer::tokenizer_type) {
            throw new Error("Error: First argument must be a tokenizer");
        }
        
        TorchTokenizer* tokenizer = (TorchTokenizer*)tokenizer_elem;
        std::vector<int> token_ids;
        
        // Supporter les tensors torch, les listes LispE et les integers
        if (token_ids_elem->type == t_tensor) {
            TorchTensor* token_tensor = (TorchTensor*)token_ids_elem;
            // Convertir tensor en vector d'entiers
            auto tensor_flat = token_tensor->tensor.flatten();
            for (int64_t i = 0; i < tensor_flat.size(0); i++) {
                token_ids.push_back(tensor_flat[i].item<int>());
            }
        } else if (token_ids_elem->type == t_integers) {
            // Liste d'entiers LispE (type integers_)
            Integers* integers = (Integers*)token_ids_elem;
            for (long i = 0; i < integers->size(); i++) {
                token_ids.push_back((int)integers->liste[i]);
            }
        } else if (token_ids_elem->isList()) {
            // Liste générique LispE
            for (long i = 0; i < token_ids_elem->size(); i++) {
                token_ids.push_back((int)token_ids_elem->index(i)->asInteger());
            }
        } else if (token_ids_elem->isInteger()) {
            // Un seul token (entier)
            token_ids.push_back((int)token_ids_elem->asInteger());
        } else {
            throw new Error("Error: Second argument must be a tensor, list, or integers of token IDs");
        }
        
        string decoded_text = tokenizer->decode(token_ids);
        return new String(decoded_text);
    } catch (const std::exception& e) {
        string msg = "Error decoding tokens: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::vocabulary_size(LispE* lisp) {
    Element* tokenizer_elem = lisp->get_variable(L"tokenizer");
    
    try {
        if (tokenizer_elem->type != TorchTokenizer::tokenizer_type) {
            throw new Error("Error: Argument must be a tokenizer");
        }
        
        TorchTokenizer* tokenizer = (TorchTokenizer*)tokenizer_elem;
        return new Integer(tokenizer->vocab_size());
    } catch (const std::exception& e) {
        string msg = "Error getting vocabulary size: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::pad_sequence(LispE* lisp) {
    Element* sequences_elem = lisp->get_variable(L"sequences");
    Element* max_length_elem = lisp->get_variable(L"max_length");
    Element* pad_token_elem = lisp->get_variable(L"pad_token");
    
    try {
        if (sequences_elem->type != t_list) {
            throw new Error("Error: First argument must be a list of sequences");
        }
        
        long max_length = max_length_elem->asInteger();
        long pad_token = pad_token_elem ? pad_token_elem->asInteger() : 0; // <pad> token
        
        List* sequences = (List*)sequences_elem;
        std::vector<torch::Tensor> padded_sequences;
        
        for (long i = 0; i < sequences->size(); i++) {
            Element* seq_elem = sequences->index(i);
            if (seq_elem->type != t_tensor) {
                throw new Error("Error: All sequences must be tensors");
            }
            
            TorchTensor* seq_tensor = (TorchTensor*)seq_elem;
            torch::Tensor seq = seq_tensor->tensor.flatten();
            
            if (seq.size(0) >= max_length) {
                // Tronquer si trop long
                padded_sequences.push_back(seq.slice(0, 0, max_length));
            } else {
                // Padding si trop court
                torch::Tensor padding = torch::full({max_length - seq.size(0)}, pad_token, torch::kLong);
                padded_sequences.push_back(torch::cat({seq, padding}, 0));
            }
        }
        
        torch::Tensor result = torch::stack(padded_sequences, 0);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error padding sequences: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::create_attention_mask(LispE* lisp) {
    Element* sequences_elem = lisp->get_variable(L"sequences");
    Element* pad_token_elem = lisp->get_variable(L"pad_token");
    
    try {
        if (sequences_elem->type != t_tensor) {
            throw new Error("Error: First argument must be a tensor of sequences");
        }
        
        long pad_token = pad_token_elem ? pad_token_elem->asInteger() : 0; // <pad> token
        
        TorchTensor* sequences = (TorchTensor*)sequences_elem;
        
        // Créer le masque d'attention (1 pour les tokens réels, 0 pour le padding)
        torch::Tensor mask = (sequences->tensor != pad_token).to(torch::kFloat32);
        return new TorchTensor(mask);
    } catch (const std::exception& e) {
        string msg = "Error creating attention mask: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::positional_encoding_create(LispE* lisp) {
    Element* d_model_elem = lisp->get_variable(L"d_model");
    Element* max_len_elem = lisp->get_variable(L"max_len");
    
    try {
        long d_model = d_model_elem->asInteger();
        long max_len = max_len_elem ? max_len_elem->asInteger() : 5000;
        
        auto pe = std::make_shared<PositionalEncoding>(d_model, max_len);
        return new TorchPositionalEncoding(pe);
    } catch (const std::exception& e) {
        string msg = "Error creating positional encoding: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::positional_encoding_forward(LispE* lisp) {
    Element* pe_elem = lisp->get_variable(L"positional_encoding");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (pe_elem->type != t_positional_encoding) {
            throw new Error("Error: First argument must be a positional encoding");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchPositionalEncoding* pe = (TorchPositionalEncoding*)pe_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = pe->pe->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in positional encoding forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::rotary_embedding_create(LispE* lisp) {
    Element* dim_elem = lisp->get_variable(L"dim");
    Element* max_seq_len_elem = lisp->get_variable(L"max_seq_len");
    
    try {
        int64_t dim = dim_elem->asInteger();
        int64_t max_seq_len = 8192; // valeur par défaut
        
        if (max_seq_len_elem != NULL) {
            max_seq_len = max_seq_len_elem->asInteger();
        }
        
        auto rope = std::make_shared<RotaryEmbedding>(dim, max_seq_len);
        return new TorchRotaryEmbedding(rope);
    } catch (const std::exception& e) {
        string msg = "Error creating rotary embedding: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::rotary_embedding_forward(LispE* lisp) {
    Element* rope_elem = lisp->get_variable(L"rotary_embedding");
    Element* seq_len_elem = lisp->get_variable(L"seq_len");
    Element* device_elem = lisp->get_variable(L"device");
    
    try {
        if (rope_elem->type != t_rotary_embedding) {
            throw new Error("Error: First argument must be a rotary embedding");
        }
        
        TorchRotaryEmbedding* rope = (TorchRotaryEmbedding*)rope_elem;
        int64_t seq_len = seq_len_elem->asInteger();
        
        torch::Device device = torch::kCPU;
        if (device_elem != null_) {
            string device_str = device_elem->toString(lisp);
            device = lookup_device(device_str);
        }
        
        auto [cos, sin] = rope->rope->forward(seq_len, device);
        
        // Retourner un tuple (cos, sin) sous forme de liste
        List* result = lisp->provideList();
        result->append(new TorchTensor(cos));
        result->append(new TorchTensor(sin));
        return result;
        
    } catch (const std::exception& e) {
        string msg = "Error in rotary embedding forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::apply_rotary_pos_emb(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* cos_elem = lisp->get_variable(L"cos");
    Element* sin_elem = lisp->get_variable(L"sin");
    
    try {
        if (tensor_elem->type != t_tensor || cos_elem->type != t_tensor || sin_elem->type != t_tensor) {
            throw new Error("Error: All arguments must be tensors");
        }
        
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        TorchTensor* cos = (TorchTensor*)cos_elem;
        TorchTensor* sin = (TorchTensor*)sin_elem;
        
        torch::Tensor output = apply_rotary_position_embedding(tensor->tensor, cos->tensor, sin->tensor);
        return new TorchTensor(output);
        
    } catch (const std::exception& e) {
        string msg = "Error applying rotary position embedding: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::checkpoint_enable(LispE* lisp) {
    Element* module_elem = lisp->get_variable(L"module");
    
    try {
        if (module_elem->type == t_checkpointed_module) {
            TorchCheckpointedModule* checkpointed = (TorchCheckpointedModule*)module_elem;
            checkpointed->module->enable_checkpointing();
            return True_;
        } else {
            throw new Error("Error: Argument must be a checkpointed module");
        }
    } catch (const std::exception& e) {
        string msg = "Error enabling checkpointing: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::checkpoint_disable(LispE* lisp) {
    Element* module_elem = lisp->get_variable(L"module");
    
    try {
        if (module_elem->type == t_checkpointed_module) {
            TorchCheckpointedModule* checkpointed = (TorchCheckpointedModule*)module_elem;
            checkpointed->module->disable_checkpointing();
            return True_;
        } else {
            throw new Error("Error: Argument must be a checkpointed module");
        }
    } catch (const std::exception& e) {
        string msg = "Error disabling checkpointing: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::checkpoint_forward(LispE* lisp) {
    Element* module_elem = lisp->get_variable(L"module");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (module_elem->type != t_checkpointed_module) {
            throw new Error("Error: First argument must be a checkpointed module");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchCheckpointedModule* checkpointed = (TorchCheckpointedModule*)module_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = checkpointed->module->forward(input->tensor);
        return new TorchTensor(output);
        
    } catch (const std::exception& e) {
        string msg = "Error in checkpointed forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::checkpoint_create(LispE* lisp) {
    Element* module_elem = lisp->get_variable(L"module");
    
    try {
        // Créer un module checkpointé basé sur l'élément d'entrée
        CheckpointedModule* checkpointed = new CheckpointedModule(module_elem);
        
        // Vérifier que le type de module est supporté
        if (module_elem->type != t_linear && module_elem->type != t_attention && 
            module_elem->type != t_layernorm && module_elem->type != t_embedding) {
            throw new Error("Error: Unsupported module type for checkpointing");
        }
        
        return new TorchCheckpointedModule(std::make_shared<CheckpointedModule>(*checkpointed));
        
    } catch (const std::exception& e) {
        string msg = "Error creating checkpointed module: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::save_model(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* path_elem = lisp->get_variable(L"path");
    
    try {
        string path = path_elem->toString(lisp);
        
        // Essayer différents types de modules PyTorch
        if (model_elem->type == t_model) {
            TorchModel* model = (TorchModel*)model_elem;
            torch::save(model->module, path);
        } else if (model_elem->type == t_linear) {
            TorchLinear* linear = (TorchLinear*)model_elem;
            torch::save(linear->linear, path);
        } else if (model_elem->type == t_embedding) {
            TorchEmbedding* embedding = (TorchEmbedding*)model_elem;
            torch::save(embedding->embedding, path);
        } else if (model_elem->type == t_attention) {
            TorchMultiHeadAttention* attention = (TorchMultiHeadAttention*)model_elem;
            torch::save(attention->attention, path);
        } else if (model_elem->type == t_layernorm) {
            TorchLayerNorm* layernorm = (TorchLayerNorm*)model_elem;
            torch::save(layernorm->layer_norm, path);
        } else if (model_elem->type == t_positional_encoding) {
            TorchPositionalEncoding* pos_enc = (TorchPositionalEncoding*)model_elem;
            torch::save(pos_enc->pe, path);
        } else {
            throw new Error("Error: First argument must be a PyTorch module");
        }
        
        string result = "Model saved successfully";
        return lisp->provideString(result);
    } catch (const std::exception& e) {
        string msg = "Error saving model: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::load_model(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* path_elem = lisp->get_variable(L"path");
    
    try {
        string path = path_elem->toString(lisp);
        
        // Essayer différents types de modules PyTorch
        if (model_elem->type == t_model) {
            TorchModel* model = (TorchModel*)model_elem;
            torch::load(model->module, path);
        } else if (model_elem->type == t_linear) {
            TorchLinear* linear = (TorchLinear*)model_elem;
            torch::load(linear->linear, path);
        } else if (model_elem->type == t_embedding) {
            TorchEmbedding* embedding = (TorchEmbedding*)model_elem;
            torch::load(embedding->embedding, path);
        } else if (model_elem->type == t_attention) {
            TorchMultiHeadAttention* attention = (TorchMultiHeadAttention*)model_elem;
            torch::load(attention->attention, path);
        } else if (model_elem->type == t_layernorm) {
            TorchLayerNorm* layernorm = (TorchLayerNorm*)model_elem;
            torch::load(layernorm->layer_norm, path);
        } else if (model_elem->type == t_positional_encoding) {
            TorchPositionalEncoding* pos_enc = (TorchPositionalEncoding*)model_elem;
            torch::load(pos_enc->pe, path);
        } else {
            throw new Error("Error: First argument must be a PyTorch module");
        }
        
        string result = "Model loaded successfully";
        return lisp->provideString(result);
    } catch (const std::exception& e) {
        string msg = "Error loading model: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::model_state_dict(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    
    try {
        std::unordered_map<std::string, torch::Tensor> dict_map;
        
        // Essayer différents types de modules PyTorch
        if (model_elem->type == t_model) {
            TorchModel* model = (TorchModel*)model_elem;
            auto state_dict = model->module->named_parameters();
            for (const auto& pair : state_dict) {
                dict_map[pair.key()] = pair.value().clone();
            }
        } else if (model_elem->type == t_linear) {
            TorchLinear* linear = (TorchLinear*)model_elem;
            auto state_dict = linear->linear->named_parameters();
            for (const auto& pair : state_dict) {
                dict_map[pair.key()] = pair.value().clone();
            }
        } else if (model_elem->type == t_embedding) {
            TorchEmbedding* embedding = (TorchEmbedding*)model_elem;
            auto state_dict = embedding->embedding->named_parameters();
            for (const auto& pair : state_dict) {
                dict_map[pair.key()] = pair.value().clone();
            }
        } else if (model_elem->type == t_attention) {
            TorchMultiHeadAttention* attention = (TorchMultiHeadAttention*)model_elem;
            auto state_dict = attention->attention->named_parameters();
            for (const auto& pair : state_dict) {
                dict_map[pair.key()] = pair.value().clone();
            }
        } else if (model_elem->type == t_layernorm) {
            TorchLayerNorm* layernorm = (TorchLayerNorm*)model_elem;
            auto state_dict = layernorm->layer_norm->named_parameters();
            for (const auto& pair : state_dict) {
                dict_map[pair.key()] = pair.value().clone();
            }
        } else if (model_elem->type == t_positional_encoding) {
            TorchPositionalEncoding* pos_enc = (TorchPositionalEncoding*)model_elem;
            auto state_dict = pos_enc->pe->named_parameters();
            for (const auto& pair : state_dict) {
                dict_map[pair.key()] = pair.value().clone();
            }
        } else {
            throw new Error("Error: Argument must be a PyTorch module");
        }
        
        return new TorchStateDict(dict_map);
    } catch (const std::exception& e) {
        string msg = "Error getting model state dict: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::load_state_dict(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* state_dict_elem = lisp->get_variable(L"state_dict");
    
    try {
        if (model_elem->type != t_model) {
            throw new Error("Error: First argument must be a model");
        }
        if (state_dict_elem->type != t_state_dict) {
            throw new Error("Error: Second argument must be a state dict");
        }
        
        TorchModel* model = (TorchModel*)model_elem;
        TorchStateDict* state_dict = (TorchStateDict*)state_dict_elem;
        
        auto named_params = model->module->named_parameters();
        for (const auto& pair : named_params) {
            auto it = state_dict->state_dict.find(pair.key());
            if (it != state_dict->state_dict.end()) {
                pair.value().copy_(it->second);
            }
        }
        
        string result = "State dict loaded successfully";
        return lisp->provideString(result);
    } catch (const std::exception& e) {
        string msg = "Error loading state dict: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::save_checkpoint(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* optimizer_elem = lisp->get_variable(L"optimizer");
    Element* epoch_elem = lisp->get_variable(L"epoch");
    Element* path_elem = lisp->get_variable(L"path");
    
    try {
        if (model_elem->type != t_model) {
            throw new Error("Error: First argument must be a model");
        }
        if (optimizer_elem->type != t_optimizer) {
            throw new Error("Error: Second argument must be an optimizer");
        }
        
        TorchModel* model = (TorchModel*)model_elem;
        TorchOptimizer* optimizer = (TorchOptimizer*)optimizer_elem;
        long epoch = epoch_elem->asInteger();
        string path = path_elem->toString(lisp);
        
        // Sauvegarder seulement le modèle pour l'instant
        // (PyTorch C++ ne supporte pas facilement les checkpoints complexes)
        torch::save(model->module, path);
        
        string result = "Checkpoint saved successfully";
        return lisp->provideString(result);
    } catch (const std::exception& e) {
        string msg = "Error saving checkpoint: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::load_checkpoint(LispE* lisp) {
    Element* path_elem = lisp->get_variable(L"path");
    
    try {
        string path = path_elem->toString(lisp);
        
        // Pour l'instant, on charge juste comme un modèle
        // et on retourne un état vide
        std::unordered_map<std::string, torch::Tensor> empty_state;
        
        return new TorchStateDict(empty_state);
    } catch (const std::exception& e) {
        string msg = "Error loading checkpoint: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::flash_attention_create(LispE* lisp) {
    Element* embed_dim_elem = lisp->get_variable(L"embed_dim");
    Element* num_heads_elem = lisp->get_variable(L"num_heads");
    Element* dropout_elem = lisp->get_variable(L"dropout");
    Element* bias_elem = lisp->get_variable(L"bias");
    
    try {
        int64_t embed_dim = embed_dim_elem->asInteger();
        int64_t num_heads = num_heads_elem->asInteger();
        double dropout = (dropout_elem && dropout_elem != null_) ? dropout_elem->asNumber() : 0.0;
        bool bias = (bias_elem && bias_elem != null_) ? bias_elem->Boolean() : true;
        
        auto flash_attn = std::make_shared<FlashAttention>(embed_dim, num_heads, dropout, bias);
        return new TorchFlashAttention(flash_attn);
    } catch (const std::exception& e) {
        string msg = "Error creating Flash Attention: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::flash_attention_forward(LispE* lisp) {
    Element* flash_attn_elem = lisp->get_variable(L"flash_attention");
    Element* query_elem = lisp->get_variable(L"query");
    Element* key_elem = lisp->get_variable(L"key");
    Element* value_elem = lisp->get_variable(L"value");
    
    try {
        if (flash_attn_elem->type != t_flash_attention) {
            throw new Error("Error: First argument must be a Flash Attention module");
        }
        
        TorchFlashAttention* flash_attn = (TorchFlashAttention*)flash_attn_elem;
        
        // Convertir les tenseurs d'entrée
        torch::Tensor query_tensor, key_tensor, value_tensor;
        
        if (query_elem->type == t_tensor) {
            query_tensor = ((TorchTensor*)query_elem)->tensor;
        } else {
            query_tensor = lispe_to_tensor(query_elem);
        }
        
        if (key_elem->type == t_tensor) {
            key_tensor = ((TorchTensor*)key_elem)->tensor;
        } else {
            key_tensor = lispe_to_tensor(key_elem);
        }
        
        if (value_elem->type == t_tensor) {
            value_tensor = ((TorchTensor*)value_elem)->tensor;
        } else {
            value_tensor = lispe_to_tensor(value_elem);
        }
        
        auto result = flash_attn->flash_attention->forward(query_tensor, key_tensor, value_tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in Flash Attention forward: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::flash_attention_with_mask(LispE* lisp) {
    Element* flash_attn_elem = lisp->get_variable(L"flash_attention");
    Element* query_elem = lisp->get_variable(L"query");
    Element* key_elem = lisp->get_variable(L"key");
    Element* value_elem = lisp->get_variable(L"value");
    Element* mask_elem = lisp->get_variable(L"attn_mask");
    
    try {
        if (flash_attn_elem->type != t_flash_attention) {
            throw new Error("Error: First argument must be a Flash Attention module");
        }
        
        TorchFlashAttention* flash_attn = (TorchFlashAttention*)flash_attn_elem;
        
        // Convertir les tenseurs
        torch::Tensor query_tensor, key_tensor, value_tensor, mask_tensor;
        
        if (query_elem->type == t_tensor) {
            query_tensor = ((TorchTensor*)query_elem)->tensor;
        } else {
            query_tensor = lispe_to_tensor(query_elem);
        }
        
        if (key_elem->type == t_tensor) {
            key_tensor = ((TorchTensor*)key_elem)->tensor;
        } else {
            key_tensor = lispe_to_tensor(key_elem);
        }
        
        if (value_elem->type == t_tensor) {
            value_tensor = ((TorchTensor*)value_elem)->tensor;
        } else {
            value_tensor = lispe_to_tensor(value_elem);
        }
        
        if (mask_elem->type == t_tensor) {
            mask_tensor = ((TorchTensor*)mask_elem)->tensor;
        } else {
            mask_tensor = lispe_to_tensor(mask_elem);
        }
        
        auto result = flash_attn->flash_attention->forward_with_mask(query_tensor, key_tensor, value_tensor, mask_tensor);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in Flash Attention with mask: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::flash_attention_with_dropout(LispE* lisp) {
    Element* flash_attn_elem = lisp->get_variable(L"flash_attention");
    Element* query_elem = lisp->get_variable(L"query");
    Element* key_elem = lisp->get_variable(L"key");
    Element* value_elem = lisp->get_variable(L"value");
    Element* dropout_elem = lisp->get_variable(L"dropout_p");
    Element* training_elem = lisp->get_variable(L"training");
    
    try {
        if (flash_attn_elem->type != t_flash_attention) {
            throw new Error("Error: First argument must be a Flash Attention module");
        }
        
        TorchFlashAttention* flash_attn = (TorchFlashAttention*)flash_attn_elem;
        
        // Convertir les tenseurs
        torch::Tensor query_tensor, key_tensor, value_tensor;
        
        if (query_elem->type == t_tensor) {
            query_tensor = ((TorchTensor*)query_elem)->tensor;
        } else {
            query_tensor = lispe_to_tensor(query_elem);
        }
        
        if (key_elem->type == t_tensor) {
            key_tensor = ((TorchTensor*)key_elem)->tensor;
        } else {
            key_tensor = lispe_to_tensor(key_elem);
        }
        
        if (value_elem->type == t_tensor) {
            value_tensor = ((TorchTensor*)value_elem)->tensor;
        } else {
            value_tensor = lispe_to_tensor(value_elem);
        }
        
        double dropout_p = dropout_elem->asNumber();
        bool training = (training_elem && training_elem != null_) ? training_elem->Boolean() : true;
        
        auto result = flash_attn->flash_attention->forward_with_dropout(query_tensor, key_tensor, value_tensor, dropout_p, training);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in Flash Attention with dropout: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::scaled_dot_product_attention(LispE* lisp) {
    Element* query_elem = lisp->get_variable(L"query");
    Element* key_elem = lisp->get_variable(L"key");
    Element* value_elem = lisp->get_variable(L"value");
    Element* mask_elem = lisp->get_variable(L"attn_mask");
    Element* dropout_elem = lisp->get_variable(L"dropout_p");
    Element* causal_elem = lisp->get_variable(L"is_causal");
    Element* scale_elem = lisp->get_variable(L"scale");
    
    try {
        // Convertir les tenseurs
        torch::Tensor query_tensor, key_tensor, value_tensor;
        torch::Tensor mask_tensor;
        
        if (query_elem->type == t_tensor) {
            query_tensor = ((TorchTensor*)query_elem)->tensor;
        } else {
            query_tensor = lispe_to_tensor(query_elem);
        }
        
        if (key_elem->type == t_tensor) {
            key_tensor = ((TorchTensor*)key_elem)->tensor;
        } else {
            key_tensor = lispe_to_tensor(key_elem);
        }
        
        if (value_elem->type == t_tensor) {
            value_tensor = ((TorchTensor*)value_elem)->tensor;
        } else {
            value_tensor = lispe_to_tensor(value_elem);
        }
        
        // Paramètres optionnels
        c10::optional<torch::Tensor> mask_opt = c10::nullopt;
        if (mask_elem != null_) {
            if (mask_elem->type == t_tensor) {
                mask_opt = ((TorchTensor*)mask_elem)->tensor;
            } else {
                mask_opt = lispe_to_tensor(mask_elem);
            }
        }

        c10::optional<double> scale_opt = c10::nullopt;
        if (scale_elem != null_) {
            // Gérer différents types LispE pour le paramètre scale
            if (scale_elem->isScalar()) {
                scale_opt = scale_elem->asNumber();
            } else if (scale_elem->type == t_tensor) {
                // Si c'est un tenseur, extraire le premier élément scalaire
                TorchTensor* scale_tensor = (TorchTensor*)scale_elem;
                if (scale_tensor->tensor.numel() > 0) {
                    scale_opt = scale_tensor->tensor.item<double>();
                }
            } else if (scale_elem->isList() && scale_elem->size()) {
                scale_opt = scale_elem->index(0)->asNumber();
            }
        }
        
        double dropout_p = (dropout_elem != null_) ? dropout_elem->asNumber() : 0.0;
        bool is_causal = (causal_elem != null_) ? causal_elem->Boolean() : false;
        
        torch::Tensor result;
        
        #if TORCH_VERSION_MAJOR >= 2
        // Utiliser la fonction native PyTorch 2.0+
        result = torch::scaled_dot_product_attention(
            query_tensor, key_tensor, value_tensor,
            mask_opt, dropout_p, is_causal, scale_opt
        );
        #else
        // Implémentation manuelle pour versions antérieures
        auto scale = scale_opt.has_value() ? scale_opt.value() : (1.0 / std::sqrt(static_cast<double>(query_tensor.size(-1))));
        auto scores = torch::matmul(query_tensor, key_tensor.transpose(-2, -1)) * scale;
        
        if (mask_opt.has_value()) {
            scores = scores.masked_fill(mask_opt.value() == 0, -1e9);
        }
        
        if (is_causal) {
            auto seq_len = scores.size(-1);
            auto causal_mask = torch::triu(torch::ones({seq_len, seq_len}), 1).to(scores.device());
            scores = scores.masked_fill(causal_mask == 1, -1e9);
        }
        
        auto attn_weights = torch::softmax(scores, -1);
        
        if (dropout_p > 0.0) {
            attn_weights = torch::dropout(attn_weights, dropout_p, /*training=*/true);
        }
        
        result = torch::matmul(attn_weights, value_tensor);
        #endif
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in scaled dot product attention: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::flash_attention_simple(LispE* lisp) {
    Element* query_elem = lisp->get_variable(L"query");
    Element* key_elem = lisp->get_variable(L"key");
    Element* value_elem = lisp->get_variable(L"value");
    
    try {
        torch::Tensor query_tensor, key_tensor, value_tensor;
        
        // Conversion des tenseurs
        if (query_elem->type == t_tensor) {
            query_tensor = ((TorchTensor*)query_elem)->tensor;
        } else {
            query_tensor = lispe_to_tensor(query_elem);
        }
        
        if (key_elem->type == t_tensor) {
            key_tensor = ((TorchTensor*)key_elem)->tensor;
        } else {
            key_tensor = lispe_to_tensor(key_elem);
        }
        
        if (value_elem->type == t_tensor) {
            value_tensor = ((TorchTensor*)value_elem)->tensor;
        } else {
            value_tensor = lispe_to_tensor(value_elem);
        }
        
        // Vérifications des dimensions
        auto q_sizes = query_tensor.sizes();
        auto k_sizes = key_tensor.sizes();
        auto v_sizes = value_tensor.sizes();
        
        if (q_sizes.size() < 3 || k_sizes.size() < 3 || v_sizes.size() < 3) {
            throw new Error("Flash Attention requires at least 3D tensors (batch, seq_len, embed_dim)");
        }
        
        int64_t embed_dim = q_sizes[q_sizes.size() - 1];  // Dernière dimension
        int64_t num_heads = 8;  // Valeur par défaut raisonnable
        
        // Ajuster num_heads si embed_dim n'est pas divisible
        while (embed_dim % num_heads != 0 && num_heads > 1) {
            num_heads--;
        }
        
        // Utilisation directe de torch::nn::functional::scaled_dot_product_attention si disponible
        torch::Tensor result;
        #ifdef TORCH_API_INCLUDE_EXTENSION_H
        try {
            result = torch::nn::functional::scaled_dot_product_attention(
                query_tensor, key_tensor, value_tensor,
                /*attn_mask=*/torch::Tensor{},
                /*dropout_p=*/0.0,
                /*is_causal=*/false
            );
        } catch (...) {
            // Fallback si la fonction n'est pas disponible
            double scale = 1.0 / std::sqrt(static_cast<double>(embed_dim / num_heads));
            auto scores = torch::matmul(query_tensor, key_tensor.transpose(-2, -1)) * scale;
            auto attn_weights = torch::softmax(scores, -1);
            result = torch::matmul(attn_weights, value_tensor);
        }
        #else
        // Fallback pour PyTorch < 2.0 ou versions sans support
        double scale = 1.0 / std::sqrt(static_cast<double>(embed_dim / num_heads));
        auto scores = torch::matmul(query_tensor, key_tensor.transpose(-2, -1)) * scale;
        auto attn_weights = torch::softmax(scores, -1);
        result = torch::matmul(attn_weights, value_tensor);
        #endif
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in simple Flash Attention: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::generator_create(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* device_elem = lisp->get_variable(L"device");
    
    try {
        if (model_elem->type != t_model) {
            throw new Error("Generator requires a torch model");
        }
        
        TorchModel* torch_model = (TorchModel*)model_elem;
        torch::Device device = torch::kCPU;
        
        if (device_elem != null_) {
            string device_str = device_elem->toString(lisp);
            device = lookup_device(device_str);
        }
        
        auto generator = new TorchGenerator(torch_model->module, device);
        return generator;
        
    } catch (const std::exception& e) {
        string msg = "Error creating generator: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::generator_config(LispE* lisp) {
    Element* generator_elem = lisp->get_variable(L"generator");
    Element* config_elem = lisp->get_variable(L"config");
    
    try {
        if (generator_elem->type != t_generator) {
            throw new Error("First argument must be a torch generator");
        }
        
        TorchGenerator* generator = (TorchGenerator*)generator_elem;
        
        if (config_elem->isDictionary()) {
            Dictionary* config_dict = (Dictionary*)config_elem;
            
            for (auto& pair : config_dict->dictionary) {
                u_ustring key_u = pair.first;
                string key;
                for (auto c : key_u) {
                    key += (char)c;
                }
                Element* value = pair.second;
                
                if (key == "max_length") {
                    generator->config.max_length = value->asInteger();
                } else if (key == "temperature") {
                    generator->config.temperature = value->asNumber();
                } else if (key == "top_k") {
                    generator->config.top_k = value->asInteger();
                } else if (key == "top_p") {
                    generator->config.top_p = value->asNumber();
                } else if (key == "eos_token_id") {
                    generator->config.eos_token_id = value->asInteger();
                } else if (key == "pad_token_id") {
                    generator->config.pad_token_id = value->asInteger();
                } else if (key == "repetition_penalty") {
                    generator->config.repetition_penalty = value->asNumber();
                } else if (key == "strategy") {
                    generator->config.strategy = value->toString(lisp);
                } else if (key == "do_sample") {
                    generator->config.do_sample = value->Boolean();
                }
            }
        }
        
        string success_msg = "Generator configured successfully";
        return lisp->provideString(success_msg);
        
    } catch (const std::exception& e) {
        string msg = "Error configuring generator: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::generate(LispE* lisp) {
    Element* generator_elem = lisp->get_variable(L"generator");
    Element* input_ids_elem = lisp->get_variable(L"input_ids");
    Element* strategy_elem = lisp->get_variable(L"strategy");
    
    try {
        if (generator_elem->type != t_generator) {
            throw new Error("First argument must be a torch generator");
        }
        
        TorchGenerator* generator = (TorchGenerator*)generator_elem;
        torch::Tensor input_ids;
        
        if (input_ids_elem->type == t_tensor) {
            input_ids = ((TorchTensor*)input_ids_elem)->tensor;
        } else {
            input_ids = lispe_to_tensor(input_ids_elem);
        }
        
        // Mettre à jour la stratégie si fournie
        if (strategy_elem && strategy_elem != null_) {
            generator->config.strategy = strategy_elem->toString(lisp);
        }
        
        // Générer le texte
        auto result = generator->generate(input_ids);
        return new TorchTensor(result);
        
    } catch (const std::exception& e) {
        string msg = "Error during generation: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::set_generation_params(LispE* lisp) {
    Element* generator_elem = lisp->get_variable(L"generator");
    Element* max_length_elem = lisp->get_variable(L"max_length");
    Element* temperature_elem = lisp->get_variable(L"temperature");
    Element* top_k_elem = lisp->get_variable(L"top_k");
    Element* top_p_elem = lisp->get_variable(L"top_p");
    
    try {
        if (generator_elem->type != t_generator) {
            throw new Error("First argument must be a torch generator");
        }
        
        TorchGenerator* generator = (TorchGenerator*)generator_elem;
        
        if (max_length_elem && max_length_elem != null_) {
            generator->config.max_length = max_length_elem->asInteger();
        }
        if (temperature_elem && temperature_elem != null_) {
            generator->config.temperature = temperature_elem->asNumber();
        }
        if (top_k_elem && top_k_elem != null_) {
            generator->config.top_k = top_k_elem->asInteger();
        }
        if (top_p_elem && top_p_elem != null_) {
            generator->config.top_p = top_p_elem->asNumber();
        }
        
        string success_msg = "Generation parameters updated";
        return lisp->provideString(success_msg);
        
    } catch (const std::exception& e) {
        string msg = "Error setting generation parameters: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::topk(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* k_elem = lisp->get_variable(L"k");
    Element* dim_elem = lisp->get_variable(L"dim");
    Element* largest_elem = lisp->get_variable(L"largest");
    
    try {
        if (tensor_elem->type != TorchTensor::tensor_type) {
            throw new Error("First argument must be a torch tensor");
        }
        
        TorchTensor* tensor_obj = (TorchTensor*)tensor_elem;
        int64_t k = k_elem->asInteger();
        int64_t dim = (dim_elem && dim_elem != null_) ? dim_elem->asInteger() : -1;
        bool largest = (largest_elem && largest_elem != null_) ? largest_elem->Boolean() : true;
        
        auto result = torch::topk(tensor_obj->tensor, k, dim, largest);
        
        // Retourner un tuple (values, indices)
        Element* result_list = lisp->provideList();
        result_list->append(new TorchTensor(std::get<0>(result)));  // values
        result_list->append(new TorchTensor(std::get<1>(result)));  // indices
        
        return result_list;
    } catch (const std::exception& e) {
        string msg = "Error in torch_topk: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::multinomial(LispE* lisp) {
    Element* probs_elem = lisp->get_variable(L"probs");
    Element* num_samples_elem = lisp->get_variable(L"num_samples");
    Element* replacement_elem = lisp->get_variable(L"replacement");
    
    try {
        if (probs_elem->type != TorchTensor::tensor_type) {
            throw new Error("First argument must be a torch tensor");
        }
        
        TorchTensor* probs_obj = (TorchTensor*)probs_elem;
        int64_t num_samples = num_samples_elem->asInteger();
        bool replacement = (replacement_elem && replacement_elem != null_) ? replacement_elem->Boolean() : false;
        
        torch::Tensor result = torch::multinomial(probs_obj->tensor, num_samples, replacement);
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_multinomial: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::sort_tensor(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dim_elem = lisp->get_variable(L"dim");
    Element* descending_elem = lisp->get_variable(L"descending");
    
    try {
        if (tensor_elem->type != TorchTensor::tensor_type) {
            throw new Error("First argument must be a torch tensor");
        }
        
        TorchTensor* tensor_obj = (TorchTensor*)tensor_elem;
        int64_t dim = (dim_elem && dim_elem != null_) ? dim_elem->asInteger() : -1;
        bool descending = (descending_elem && descending_elem != null_) ? descending_elem->Boolean() : false;
        
        auto result = torch::sort(tensor_obj->tensor, dim, descending);
        
        // Retourner un tuple (values, indices)
        Element* result_list = lisp->provideList();
        result_list->append(new TorchTensor(std::get<0>(result)));  // values
        result_list->append(new TorchTensor(std::get<1>(result)));  // indices
        
        return result_list;
    } catch (const std::exception& e) {
        string msg = "Error in torch_sort: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::quantize_dynamic(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* dtype_elem = lisp->get_variable(L"dtype");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        std::string dtype = dtype_elem->toString(lisp);
        
        torch::Tensor result;
        if (dtype == "int8" || dtype == "qint8") {
            result = torch::quantize_per_tensor(torch_tensor->tensor, 
                                               1.0, 0, torch::kQInt8);
        } else if (dtype == "uint8" || dtype == "quint8") {
            result = torch::quantize_per_tensor(torch_tensor->tensor, 
                                               1.0, 0, torch::kQUInt8);
        } else {
            throw new Error("Unsupported dtype for quantization: " + dtype);
        }
        
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_quantize_dynamic: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::quantize_static(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* scale_elem = lisp->get_variable(L"scale");
    Element* zero_point_elem = lisp->get_variable(L"zero_point");
    Element* dtype_elem = lisp->get_variable(L"dtype");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        double scale = scale_elem->asNumber();
        int64_t zero_point = zero_point_elem->asInteger();
        std::string dtype = dtype_elem->toString(lisp);
        
        torch::ScalarType qtype = torch::kQInt8;
        if (dtype == "uint8" || dtype == "quint8") {
            qtype = torch::kQUInt8;
        }
        
        torch::Tensor result = torch::quantize_per_tensor(torch_tensor->tensor, 
                                                         scale, zero_point, qtype);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_quantize_static: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::dequantize(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"quantized_tensor");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch_tensor->tensor.dequantize();
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_dequantize: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::quantize_linear(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* scale_elem = lisp->get_variable(L"scale");
    Element* zero_point_elem = lisp->get_variable(L"zero_point");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        double scale = scale_elem->asNumber();
        int64_t zero_point = zero_point_elem->asInteger();
        
        torch::Tensor result = torch::quantize_per_tensor(torch_tensor->tensor, 
                                                         scale, zero_point, torch::kQInt8);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_quantize_linear: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::quantize_per_channel(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* scales_elem = lisp->get_variable(L"scales");
    Element* zero_points_elem = lisp->get_variable(L"zero_points");
    Element* axis_elem = lisp->get_variable(L"axis");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        TorchTensor* scales_tensor = (TorchTensor*)scales_elem;
        TorchTensor* zero_points_tensor = (TorchTensor*)zero_points_elem;
        int axis = axis_elem->asInteger();
        
        torch::Tensor result = torch::quantize_per_channel(torch_tensor->tensor,
                                                           scales_tensor->tensor,
                                                           zero_points_tensor->tensor,
                                                           axis, torch::kQInt8);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_quantize_per_channel: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::quantize_int8(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        
        // Calcul automatique de scale et zero_point
        auto minmax = torch::aminmax(torch_tensor->tensor);
        float min_val = std::get<0>(minmax).item<float>();
        float max_val = std::get<1>(minmax).item<float>();
        
        float scale = (max_val - min_val) / 255.0f;
        int zero_point = static_cast<int>(-min_val / scale);
        
        torch::Tensor result = torch::quantize_per_tensor(torch_tensor->tensor,
                                                         scale, zero_point, torch::kQInt8);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_quantize_int8: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::quantize_fp16(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch_tensor->tensor.to(torch::kHalf);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_quantize_fp16: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_to_int8(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch_tensor->tensor.to(torch::kInt8);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_tensor_to_int8: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tensor_to_fp16(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    
    try {
        TorchTensor* torch_tensor = (TorchTensor*)tensor_elem;
        torch::Tensor result = torch_tensor->tensor.to(torch::kHalf);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in torch_tensor_to_fp16: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::model_quantize_dynamic(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    
    try {
        TorchModel* torch_model = (TorchModel*)model_elem;
        
        // Pour la quantification dynamique de modèle, on applique aux couches linéaires
        // Note: Ceci est une implémentation simplifiée
        // En production, on utiliserait torch.quantization.quantize_dynamic
        
        // Pour l'instant, on retourne le modèle tel quel avec un message
        // Dans une implémentation complète, on appliquerait la quantification
        
        return torch_model; // Retourner le modèle original pour l'instant
        
    } catch (const std::exception& e) {
        string msg = "Error in torch_model_quantize_dynamic: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::model_quantize_static(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* calibration_data_elem = lisp->get_variable(L"calibration_data");
    
    try {
        TorchModel* torch_model = (TorchModel*)model_elem;
        
        // Pour la quantification statique, on aurait besoin d'un processus de calibration
        // Note: Ceci est une implémentation simplifiée
        
        //affichage << "Static quantization applied to model with calibration data (simplified implementation)" << std::endl;
        return torch_model; // Retourner le modèle original pour l'instant
        
    } catch (const std::exception& e) {
        string msg = "Error in torch_model_quantize_static: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_linear_create(LispE* lisp) {
    Element* in_features_elem = lisp->get_variable(L"in_features");
    Element* out_features_elem = lisp->get_variable(L"out_features");
    Element* rank_elem = lisp->get_variable(L"rank");
    Element* alpha_elem = lisp->get_variable(L"alpha");
    
    try {
        int64_t in_features = in_features_elem->asInteger();
        int64_t out_features = out_features_elem->asInteger();
        int rank = rank_elem->asInteger();
        double alpha = alpha_elem->asNumber();
        
        auto lora_layer = std::make_shared<LoRALinear>(in_features, out_features, rank, alpha);
        return new TorchLoRALinear(lora_layer);
    } catch (const std::exception& e) {
        string msg = "Error creating LoRA linear layer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_linear_forward(LispE* lisp) {
    Element* lora_elem = lisp->get_variable(L"lora_layer");
    Element* input_elem = lisp->get_variable(L"input_data");
    
    try {
        if (lora_elem->type != t_lora_linear) {
            throw new Error("Error: First argument must be a LoRA linear layer");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Error: Second argument must be a tensor");
        }
        
        TorchLoRALinear* lora = (TorchLoRALinear*)lora_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        
        torch::Tensor output = lora->lora_layer->forward(input->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        string msg = "Error in LoRA forward pass: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_apply_to_linear(LispE* lisp) {
    Element* linear_elem = lisp->get_variable(L"linear_layer");
    Element* rank_elem = lisp->get_variable(L"rank");
    Element* alpha_elem = lisp->get_variable(L"alpha");
    
    try {
        if (linear_elem->type != t_linear) {
            throw new Error("Error: First argument must be a linear layer");
        }
        
        TorchLinear* linear = (TorchLinear*)linear_elem;
        int rank = rank_elem->asInteger();
        double alpha = alpha_elem->asNumber();
        
        // Récupérer les dimensions de la couche linéaire
        auto weight = linear->linear->weight;
        int64_t out_features = weight.size(0);
        int64_t in_features = weight.size(1);
        
        // Créer une nouvelle couche LoRA avec les mêmes dimensions
        auto lora_layer = std::make_shared<LoRALinear>(in_features, out_features, rank, alpha);
        
        // Copier les poids de la couche originale
        lora_layer->original_layer->weight.data().copy_(weight);
        if (linear->linear->options.bias()) {
            lora_layer->original_layer->bias.data().copy_(linear->linear->bias);
        }
        
        return new TorchLoRALinear(lora_layer);
    } catch (const std::exception& e) {
        string msg = "Error applying LoRA to linear layer: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_merge_weights(LispE* lisp) {
    Element* lora_elem = lisp->get_variable(L"lora_layer");
    
    try {
        if (lora_elem->type != t_lora_linear) {
            throw new Error("Error: Argument must be a LoRA linear layer");
        }
        
        TorchLoRALinear* lora = (TorchLoRALinear*)lora_elem;
        lora->lora_layer->merge_weights();
        
        string result = "LoRA weights merged successfully";
        return lisp->provideString(result);
    } catch (const std::exception& e) {
        string msg = "Error merging LoRA weights: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_get_trainable_params(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    
    try {
        // Pour l'instant, on retourne juste une liste vide
        // Dans une implémentation complète, on parcourrait le modèle
        // pour collecter tous les paramètres LoRA trainables
        Integers* params = lisp->provideIntegers();
        return params;
    } catch (const std::exception& e) {
        string msg = "Error getting trainable parameters: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_save_adapters(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* path_elem = lisp->get_variable(L"path");
    
    try {
        string path = path_elem->toString(lisp);
        
        if (model_elem->type == t_lora_linear) {
            TorchLoRALinear* lora = (TorchLoRALinear*)model_elem;
            
            // Créer un module temporaire pour la sérialisation
            auto temp_module = torch::nn::Sequential();
            temp_module->push_back(lora->lora_layer->lora_A);
            temp_module->push_back(lora->lora_layer->lora_B);
            
            // Sauvegarder le module temporaire
            torch::save(temp_module, path);
            
            string result = "LoRA adapters saved successfully";
            return lisp->provideString(result);
        } else {
            throw new Error("Error: Model must contain LoRA layers");
        }
    } catch (const std::exception& e) {
        string msg = "Error saving LoRA adapters: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::lora_load_adapters(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* path_elem = lisp->get_variable(L"path");
    
    try {
        string path = path_elem->toString(lisp);
        
        if (model_elem->type == t_lora_linear) {
            TorchLoRALinear* lora = (TorchLoRALinear*)model_elem;
            
            // Créer un module temporaire pour charger
            auto temp_module = torch::nn::Sequential();
            temp_module->push_back(lora->lora_layer->lora_A);
            temp_module->push_back(lora->lora_layer->lora_B);
            
            // Charger le module temporaire
            torch::load(temp_module, path);
            
            string result = "LoRA adapters loaded successfully";
            return lisp->provideString(result);
        } else {
            throw new Error("Error: Model must contain LoRA layers");
        }
    } catch (const std::exception& e) {
        string msg = "Error loading LoRA adapters: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tucker_decomposition(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    Element* rank_elem = lisp->get_variable(L"rank");
    Element* max_iter_elem = lisp->get_variable(L"max_iter");
    Element* tol_elem = lisp->get_variable(L"tol");
    
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Input must be a tensor");
        }
        
        TorchTensor* input_tensor = (TorchTensor*)tensor_elem;
        auto tensor = input_tensor->tensor;
        
        // Paramètres de la décomposition
        std::vector<int64_t> ranks;
        if (rank_elem->type == t_list) {
            List* rank_list = (List*)rank_elem;
            for (long i = 0; i < rank_list->size(); i++) {
                ranks.push_back(rank_list->index(i)->asInteger());
            }
        } else {
            // Rang uniforme pour toutes les dimensions
            int64_t rank = rank_elem->asInteger();
            for (int64_t i = 0; i < tensor.dim(); i++) {
                ranks.push_back(rank);
            }
        }
        
        int max_iter = max_iter_elem ? max_iter_elem->asInteger() : 100;
        double tol = tol_elem ? tol_elem->asNumber() : 1e-6;
        
        auto result = tucker_decompose(tensor, ranks, max_iter, tol);
        
        // Retourner le noyau et les facteurs sous forme de liste
        List* output = lisp->provideList();
        output->append(new TorchTensor(std::get<0>(result))); // Core tensor
        
        List* factors = lisp->provideList();
        for (const auto& factor : std::get<1>(result)) {
            factors->append(new TorchTensor(factor));
        }
        output->append(factors);
        
        return output;
    } catch (const std::exception& e) {
        string msg = "Error in Tucker decomposition: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tucker_reconstruct(LispE* lisp) {
    Element* core_elem = lisp->get_variable(L"core");
    Element* factors_elem = lisp->get_variable(L"factors");
    
    try {
        if (core_elem->type != t_tensor) {
            throw new Error("Error: Core must be a tensor");
        }
        if (factors_elem->type != t_list) {
            throw new Error("Error: Factors must be a list of tensors");
        }
        
        TorchTensor* core_tensor = (TorchTensor*)core_elem;
        List* factors_list = (List*)factors_elem;
        
        std::vector<torch::Tensor> factors;
        for (long i = 0; i < factors_list->size(); i++) {
            Element* factor_elem = factors_list->index(i);
            if (factor_elem->type != t_tensor) {
                throw new Error("Error: All factors must be tensors");
            }
            TorchTensor* factor_tensor = (TorchTensor*)factor_elem;
            factors.push_back(factor_tensor->tensor);
        }
        
        auto reconstructed = tucker_reconstruct_tensor(core_tensor->tensor, factors);
        return new TorchTensor(reconstructed);
        
    } catch (const std::exception& e) {
        string msg = "Error in Tucker reconstruction: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::tucker_compression_ratio(LispE* lisp) {
    Element* original_shape_elem = lisp->get_variable(L"original_shape");
    Element* core_shape_elem = lisp->get_variable(L"core_shape");
    Element* factor_shapes_elem = lisp->get_variable(L"factor_shapes");
    
    try {
        std::vector<int64_t> original_shape;
        if (original_shape_elem->type == t_list) {
            List* shape_list = (List*)original_shape_elem;
            for (long i = 0; i < shape_list->size(); i++) {
                original_shape.push_back(shape_list->index(i)->asInteger());
            }
        }
        
        std::vector<int64_t> core_shape;
        if (core_shape_elem->type == t_list) {
            List* shape_list = (List*)core_shape_elem;
            for (long i = 0; i < shape_list->size(); i++) {
                core_shape.push_back(shape_list->index(i)->asInteger());
            }
        }
        
        std::vector<std::vector<int64_t>> factor_shapes;
        if (factor_shapes_elem->type == t_list) {
            List* factors_list = (List*)factor_shapes_elem;
            for (long i = 0; i < factors_list->size(); i++) {
                Element* shape_elem = factors_list->index(i);
                if (shape_elem->type == t_list) {
                    List* shape_list = (List*)shape_elem;
                    std::vector<int64_t> shape;
                    for (long j = 0; j < shape_list->size(); j++) {
                        shape.push_back(shape_list->index(j)->asInteger());
                    }
                    factor_shapes.push_back(shape);
                }
            }
        }
        
        double ratio = calculate_tucker_compression_ratio(original_shape, core_shape, factor_shapes);
        return lisp->provideFloat(ratio);
        
    } catch (const std::exception& e) {
        string msg = "Error calculating Tucker compression ratio: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* Lispe_lispetorch::khatri_rao_product(LispE* lisp) {
    Element* A_elem = lisp->get_variable(L"A");
    Element* B_elem = lisp->get_variable(L"B");
    
    try {
        if (A_elem->type != t_tensor) {
            throw new Error("First argument must be a tensor");
        }
        if (B_elem->type != t_tensor) {
            throw new Error("Second argument must be a tensor");
        }
        
        TorchTensor* tensor_A = (TorchTensor*)A_elem;
        TorchTensor* tensor_B = (TorchTensor*)B_elem;
        
        auto result = the_khatri_rao_product(tensor_A->tensor, tensor_B->tensor);
        return new TorchTensor(result);
        
    } catch (const std::exception& e) {
        string msg = "Error in Khatri-Rao product: ";
        msg += e.what();
        throw new Error(msg);
    }
}


Element* Lispe_lispetorch::mps_synchronize(LispE* lisp) {
#ifdef __APPLE__    
    Element* mode = lisp->get_variable(L"safemode");
    if (mode != null_) {
        TorchTensor::enable_synchronize = mode->Boolean();
        return True_;
    }
#endif
    try {
        if (torch::mps::is_available()) {
            torch::mps::synchronize();
            return True_;
        } else {
            return False_;
        }
    } catch (const std::exception& e) {
        throw new Error("Error synchronizing MPS: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::lora_compute_delta(LispE* lisp) {
    Element* lora_elem = lisp->get_variable(L"lora_layer");
    
    try {
        if (lora_elem->type != t_lora_linear) {
            throw new Error("Argument must be a LoRA linear layer");
        }
        
        TorchLoRALinear* lora = (TorchLoRALinear*)lora_elem;
        auto delta = lora->lora_layer->compute_lora_delta();
        
        return new TorchTensor(delta);
        
    } catch (const std::exception& e) {
        throw new Error("Error computing LoRA delta: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::lora_get_adaptation_magnitude(LispE* lisp) {
    Element* lora_elem = lisp->get_variable(L"lora_layer");
    
    try {
        if (lora_elem->type != t_lora_linear) {
            throw new Error("Argument must be a LoRA linear layer");
        }
        
        TorchLoRALinear* lora = (TorchLoRALinear*)lora_elem;
        double magnitude = lora->lora_layer->get_adaptation_magnitude();
        
        return lisp->provideNumber(magnitude);
        
    } catch (const std::exception& e) {
        throw new Error("Error getting adaptation magnitude: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::lora_forward_with_gradients(LispE* lisp) {
    Element* lora_elem = lisp->get_variable(L"lora_layer");
    Element* input_elem = lisp->get_variable(L"input");
    Element* retain_graph_elem = lisp->get_variable(L"retain_graph");
    
    try {
        if (lora_elem->type != t_lora_linear) {
            throw new Error("First argument must be a LoRA linear layer");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Second argument must be a tensor");
        }
        
        TorchLoRALinear* lora = (TorchLoRALinear*)lora_elem;
        TorchTensor* input = (TorchTensor*)input_elem;
        bool retain_graph = (retain_graph_elem && retain_graph_elem != null_) ? 
                           retain_graph_elem->Boolean() : true;
        
        auto output = lora->lora_layer->forward_with_gradients(input->tensor, retain_graph);
        return new TorchTensor(output);
        
    } catch (const std::exception& e) {
        throw new Error("Error in LoRA forward with gradients: " + std::string(e.what()));
    }
}

