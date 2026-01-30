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

#ifdef USE_HUGGINGFACE
// Définition de la variable globale HuggingFace (PyTorch)
std::unordered_map<std::string, std::unique_ptr<lispe_hf::HuggingFaceLoader>> Lispe_lispetorch::hf_loaders;
#endif

//const bool display_debug_torch = false;
//#define affichage if (display_debug_torch) std::cout

#ifdef __APPLE__
bool TorchTensor::enable_synchronize = false;
#endif    

// ==================== Device Map ====================
static std::unordered_map<std::string, torch::Device> device_map;
// ====================================================
static void fill_device_in() {
    // Utiliser emplace() ou insert() au lieu de operator[]
    device_map.emplace("cpu", torch::kCPU);
    
    // Vérifier si MPS est disponible avant de l'ajouter
#ifdef __APPLE__
    if (torch::mps::is_available()) {
        device_map.emplace("mps", torch::kMPS);
    }
#endif

    // Vérifier si CUDA est disponible avant d'ajouter les devices CUDA
#ifdef USE_CUDA
    if (torch::cuda::is_available()) {
        device_map.emplace("cuda", torch::kCUDA);
        device_map.emplace("cuda:0", torch::kCUDA);
        
        char cuda[20];
        string name;
        for (int i = 1; i < torch::cuda::device_count(); i++) {
            sprintf_s(cuda, 20, "cuda:%d", i);
            name = cuda;
            device_map.emplace(name, torch::Device(torch::kCUDA, i));
        }
    }
#endif
}
// ====================================================
torch::Device lookup_device(string device) {
    static bool init = true;
    if (init) {
        fill_device_in();
        init = false;
    }

    auto dev = device_map.find(device);
    if (dev == device_map.end()) {
        std::stringstream err;
        err << "Error: unknown device:" << device;
        throw new Error(err.str());
    }
    return dev->second;
}
// ==================== End Device Map ====================

long TorchTensor::the_tensors = 0;

int16_t HFInferenceContext::t_context_inference = 0;

int16_t TorchTensor::tensor_type = 0;
int16_t TorchModel::model_type = 0;
int16_t TorchOptimizer::optimizer_type = 0;
int16_t TorchMultiHeadAttention::attention_type = 0;
int16_t TorchLayerNorm::layernorm_type = 0;
int16_t TorchEmbedding::embedding_type = 0;
int16_t TorchLinear::linear_type = 0;
int16_t TorchTransformerBlock::transformer_block_type = 0;
int16_t TorchPositionalEncoding::positional_encoding_type = 0;
int16_t TorchRotaryEmbedding::rotary_embedding_type = 0;
int16_t TorchCheckpointedModule::checkpointed_module_type = 0;
int16_t TorchStateDict::state_dict_type = 0;
int16_t TorchLoRALinear::lora_linear_type = 0;
int16_t TorchLoRAConfig::lora_config_type = 0;
int16_t TorchFlashAttention::flash_attention_type = 0;
int16_t TorchGenerator::generator_type = 0;
int16_t TorchJITModel::jit_model_type = 0;

// Déclaration des types pour tokenization
int16_t TorchTokenizer::tokenizer_type = 0;

// Fonction utilitaire pour convertir les vecteurs LispE en tensors
torch::Tensor lispe_to_tensor(Element* element) {
    switch (element->type) {
    case t_floats: {
        Floats* floats = (Floats*)element;
        
        // Tentative de récupération zero-copy du buffer via borrow()
        long size = floats->size();
        float* borrowed_buffer = floats->liste.borrow();
        
        if (borrowed_buffer != NULL) {
            // ✅ Zero-copy optimal : ownership transféré à PyTorch
            auto deleter = [](void* ptr) { free(ptr); };
            return torch::from_blob(borrowed_buffer, {size}, deleter, torch::dtype(torch::kFloat32));
        } else {
            // ❌ Buffer partagé → fallback avec clone
            float* data = floats->liste.items->buffer;
            return torch::from_blob(data, {size}, torch::dtype(torch::kFloat32)).clone();
        }
    }
    case t_numbers: {
        Numbers* numbers = (Numbers*)element;
        
        // Tentative de récupération zero-copy du buffer via borrow()
        long size = numbers->size();
        double* borrowed_buffer = numbers->liste.borrow();
        
        if (borrowed_buffer != NULL) {
            // ✅ Zero-copy optimal : ownership transféré à PyTorch
            auto deleter = [](void* ptr) { free(ptr); };
            return torch::from_blob(borrowed_buffer, {size}, deleter, torch::dtype(torch::kFloat64));
        } else {
            // ❌ Buffer partagé → fallback avec clone
            double* data = numbers->liste.items->buffer;
            return torch::from_blob(data, {size}, torch::dtype(torch::kFloat64)).clone();
        }
    }
    case t_integers: {
        Integers* integers = (Integers*)element;
        
        // Tentative de récupération zero-copy du buffer via borrow()
        long size = integers->size();
        long* borrowed_buffer = integers->liste.borrow();
        
        if (borrowed_buffer != NULL) {
            // ✅ Zero-copy optimal : ownership transféré à PyTorch
            auto deleter = [](void* ptr) { free(ptr); };
            return torch::from_blob(borrowed_buffer, {size}, deleter, torch::dtype(torch::kLong));
        } else {
            // ❌ Buffer partagé → fallback avec clone
            long* data = integers->liste.items->buffer;
            return torch::from_blob(data, {size}, torch::dtype(torch::kLong)).clone();
        }
    }
    case t_shorts: {
        Shorts* shorts = (Shorts*)element;
        
        // Tentative de récupération zero-copy du buffer via borrow()
        long size = shorts->size();
        int16_t* borrowed_buffer = shorts->liste.borrow();
        
        if (borrowed_buffer != NULL) {
            // ✅ Zero-copy optimal : ownership transféré à PyTorch
            auto deleter = [](void* ptr) { free(ptr); };
            return torch::from_blob(borrowed_buffer, {size}, deleter, torch::dtype(torch::kInt16));
        } else {
            // ❌ Buffer partagé → fallback avec clone
            int16_t* data = shorts->liste.items->buffer;
            return torch::from_blob(data, {size}, torch::dtype(torch::kInt16)).clone();
        }
    }
    
    // Support des matrices et tenseurs LispE (float)
    case t_matrix_float:
    case t_tensor_float: {
        // Obtenir les dimensions de la matrice/tenseur
        vecte<long> shape;
        element->getShape(shape);
        
        // Aplatir en Floats
        Floats flattened;
        element->flatten(nullptr, &flattened);
        
        // Convertir les dimensions LispE vers PyTorch
        std::vector<int64_t> torch_shape;
        for (long dim = 0; dim < shape.size(); dim++) {
            torch_shape.push_back(static_cast<int64_t>(shape[dim]));
        }
        
        // Créer le tenseur avec ownership du buffer
        float* borrowed_buffer = flattened.liste.borrow();
        auto deleter = [](void* ptr) { free(ptr); };
        return torch::from_blob(borrowed_buffer, torch_shape, deleter, torch::dtype(torch::kFloat32));
    }
    
    // Support des matrices et tenseurs LispE (double)
    case t_matrix_number:
    case t_tensor_number: {
        // Obtenir les dimensions de la matrice/tenseur
        vecte<long> shape;
        element->getShape(shape);
        
        // Aplatir en Numbers
        Numbers flattened;
        element->flatten(nullptr, &flattened);
        
        // Convertir les dimensions LispE vers PyTorch
        std::vector<int64_t> torch_shape;
        for (long dim = 0; dim < shape.size(); dim++) {
            torch_shape.push_back(static_cast<int64_t>(shape[dim]));
        }
        
        // Créer le tenseur avec ownership du buffer
        double* borrowed_buffer = flattened.liste.borrow();
        auto deleter = [](void* ptr) { free(ptr); };
        return torch::from_blob(borrowed_buffer, torch_shape, deleter, torch::dtype(torch::kFloat64));
    }
    
    // Support des matrices et tenseurs LispE (integer)
    case t_matrix_integer:
    case t_tensor_integer: {
        // Obtenir les dimensions de la matrice/tenseur
        vecte<long> shape;
        element->getShape(shape);
        
        // Aplatir en Integers
        Integers flattened;
        element->flatten(nullptr, &flattened);
        
        // Convertir les dimensions LispE vers PyTorch
        std::vector<int64_t> torch_shape;
        for (long dim = 0; dim < shape.size(); dim++) {
            torch_shape.push_back(static_cast<int64_t>(shape[dim]));
        }
        
        // Créer le tenseur avec ownership du buffer
        long* borrowed_buffer = flattened.liste.borrow();
        auto deleter = [](void* ptr) { free(ptr); };
        return torch::from_blob(borrowed_buffer, torch_shape, deleter, torch::dtype(torch::kLong));
    }
    
    // Support des matrices et tenseurs LispE (short)
    case t_matrix_short:
    case t_tensor_short: {
        // Obtenir les dimensions de la matrice/tenseur
        vecte<long> shape;
        element->getShape(shape);
        
        // Aplatir en Shorts
        Shorts flattened;
        element->flatten(nullptr, &flattened);
        
        // Convertir les dimensions LispE vers PyTorch
        std::vector<int64_t> torch_shape;
        for (long dim = 0; dim < shape.size(); dim++) {
            torch_shape.push_back(static_cast<int64_t>(shape[dim]));
        }
        
        // Créer le tenseur avec ownership du buffer
        int16_t* borrowed_buffer = flattened.liste.borrow();
        auto deleter = [](void* ptr) { free(ptr); };
        return torch::from_blob(borrowed_buffer, torch_shape, deleter, torch::dtype(torch::kInt16));
    }
    
    default:
        throw new Error("Error: Unsupported type for tensor conversion");
    }
}


// Fonction utilitaire pour convertir un tensor en élément LispE

// Implémentation du modèle MLP
SimpleMLP::SimpleMLP(int64_t input_size, int64_t hidden_size, int64_t output_size) {
    fc1 = register_module("fc1", torch::nn::Linear(input_size, hidden_size));
    fc2 = register_module("fc2", torch::nn::Linear(hidden_size, hidden_size));
    fc3 = register_module("fc3", torch::nn::Linear(hidden_size, output_size));
}

torch::Tensor SimpleMLP::forward(torch::Tensor x) {
    x = torch::relu(fc1->forward(x));
    x = torch::relu(fc2->forward(x));
    x = fc3->forward(x);
    return x;
}

// Implémentation du TransformerBlock
TransformerBlock::TransformerBlock(int64_t embed_dim, int64_t num_heads, int64_t ffn_dim) {
    self_attention = register_module("self_attention", 
        torch::nn::MultiheadAttention(torch::nn::MultiheadAttentionOptions(embed_dim, num_heads)));
    norm1 = register_module("norm1", 
        torch::nn::LayerNorm(torch::nn::LayerNormOptions({embed_dim})));
    norm2 = register_module("norm2", 
        torch::nn::LayerNorm(torch::nn::LayerNormOptions({embed_dim})));
    ffn1 = register_module("ffn1", 
        torch::nn::Linear(torch::nn::LinearOptions(embed_dim, ffn_dim)));
    ffn2 = register_module("ffn2", 
        torch::nn::Linear(torch::nn::LinearOptions(ffn_dim, embed_dim)));
}

torch::Tensor TransformerBlock::forward(torch::Tensor x, torch::Tensor mask) {
    // Self-attention avec connexion résiduelle
    auto attn_output = std::get<0>(self_attention->forward(x, x, x, mask));
    x = norm1->forward(x + attn_output);
    
    // Feed-forward avec connexion résiduelle
    auto ffn_output = ffn2->forward(torch::relu(ffn1->forward(x)));
    x = norm2->forward(x + ffn_output);
    
    return x;
}

// Implémentation du Positional Encoding
PositionalEncoding::PositionalEncoding(int d_model, int max_len) 
    : d_model(d_model), max_len(max_len) {
    
    // Initialisation du tensor de positional encoding
    pe = torch::zeros({max_len, d_model});
    
    // Calcul des positions
    auto position = torch::arange(0, max_len, torch::kFloat).unsqueeze(1);
    
    // Calcul des fréquences
    auto div_term = torch::exp(torch::arange(0, d_model, 2, torch::kFloat) * 
                              -(std::log(10000.0) / d_model));
    
    // Application des fonctions sin et cos
    pe.index_put_({"...", torch::indexing::Slice(0, torch::indexing::None, 2)}, 
                  torch::sin(position * div_term));
    pe.index_put_({"...", torch::indexing::Slice(1, torch::indexing::None, 2)}, 
                  torch::cos(position * div_term));
    
    // Ajout d'une dimension batch et rendre non-trainable
    pe = pe.unsqueeze(0).transpose(0, 1);
    register_buffer("pe", pe);
}

torch::Tensor PositionalEncoding::forward(torch::Tensor x) {
    // x shape: [seq_len, batch_size, d_model]
    auto seq_len = x.size(0);
    return x + pe.index({torch::indexing::Slice(0, seq_len), "..."});
}

// ==================== Implémentation Rotary Embedding ====================

// Implémentation de RotaryEmbedding (RoPE)
RotaryEmbedding::RotaryEmbedding(int dim, int max_seq_len)
    : dim(dim), max_seq_len(max_seq_len) {
    
    // Compute inverse frequencies for rotary embedding
    // inv_freq = 1.0 / (10000 ^ (torch.arange(0, dim, 2) / dim))
    auto arange = torch::arange(0, dim, 2, torch::dtype(torch::kFloat32));
    inv_freq = 1.0 / torch::pow(10000.0, arange / dim);
    
    // Pre-compute cos and sin values for efficiency
    _update_cos_sin_cache(max_seq_len, torch::kCPU);
    
    register_buffer("inv_freq", inv_freq);
    register_buffer("cos_cached", cos_cached);
    register_buffer("sin_cached", sin_cached);
}

void RotaryEmbedding::_update_cos_sin_cache(int seq_len, torch::Device device) {
    if (seq_len > max_seq_len || cos_cached.size(0) < seq_len || cos_cached.device() != device) {
        max_seq_len = std::max(seq_len, max_seq_len);
        
        // Create position indices: [0, 1, 2, ..., seq_len-1]
        auto t = torch::arange(seq_len, torch::dtype(torch::kFloat32).device(device));
        
        // Compute outer product: t * inv_freq -> [seq_len, dim//2]
        auto freqs = torch::outer(t, inv_freq.to(device));
        
        // Concatenate to get [seq_len, dim]
        auto emb = torch::cat({freqs, freqs}, -1);
        
        cos_cached = emb.cos();
        sin_cached = emb.sin();
    }
}

std::pair<torch::Tensor, torch::Tensor> RotaryEmbedding::forward(int seq_len, torch::Device device) {
    _update_cos_sin_cache(seq_len, device);
    
    return std::make_pair(
        cos_cached.index({torch::indexing::Slice(0, seq_len)}).to(device),
        sin_cached.index({torch::indexing::Slice(0, seq_len)}).to(device)
    );
}

// ==================== Implémentation Gradient Checkpointing ====================

// Implémentation de CheckpointedModule

torch::Tensor CheckpointedModule::forward(torch::Tensor input) {
    if (checkpointing_enabled && input.requires_grad()) {
        // Implémentation basique de checkpointing : contrôler le gradient
        torch::NoGradGuard no_grad;
        torch::Tensor output;
        {
            torch::AutoGradMode enable_grad(true);
            // Appeler la méthode forward selon le type d'élément
            if (wrapped_element->type == t_linear) {
                TorchLinear* linear = (TorchLinear*)wrapped_element;
                output = linear->linear->forward(input);
            } else if (wrapped_element->type == t_attention) {
                TorchMultiHeadAttention* attention = (TorchMultiHeadAttention*)wrapped_element;
                output = std::get<0>(attention->attention->forward(input, input, input));
            } else {
                throw new Error("Unsupported module type for checkpointing");
            }
        }
        return output;
    } else {
        // Forward normal sans checkpointing
        if (wrapped_element->type == t_linear) {
            TorchLinear* linear = (TorchLinear*)wrapped_element;
            return linear->linear->forward(input);
        } else if (wrapped_element->type == t_attention) {
            TorchMultiHeadAttention* attention = (TorchMultiHeadAttention*)wrapped_element;
            return std::get<0>(attention->attention->forward(input, input, input));
        } else {
            throw new Error("Unsupported module type for checkpointing");
        }
    }
}

// ==================== Implémentation Flash Attention ====================

// Implémentation de FlashAttention
FlashAttention::FlashAttention(int64_t embed_dim, int64_t num_heads, double dropout_rate, bool bias)
    : embed_dim(embed_dim), num_heads(num_heads), dropout_p(dropout_rate), use_bias(bias) {
    
    if (embed_dim % num_heads != 0) {
        throw new Error("embed_dim must be divisible by num_heads");
    }
    
    head_dim = embed_dim / num_heads;
    
    // Créer les projections linéaires pour query, key, value
    q_proj = torch::nn::Linear(torch::nn::LinearOptions(embed_dim, embed_dim).bias(bias));
    k_proj = torch::nn::Linear(torch::nn::LinearOptions(embed_dim, embed_dim).bias(bias));
    v_proj = torch::nn::Linear(torch::nn::LinearOptions(embed_dim, embed_dim).bias(bias));
    out_proj = torch::nn::Linear(torch::nn::LinearOptions(embed_dim, embed_dim).bias(bias));
    
    // Créer la couche de dropout
    if (dropout_p > 0.0) {
        dropout = torch::nn::Dropout(torch::nn::DropoutOptions(dropout_p));
    }
    
    // Enregistrer les modules
    register_module("q_proj", q_proj);
    register_module("k_proj", k_proj);
    register_module("v_proj", v_proj);
    register_module("out_proj", out_proj);
    if (dropout_p > 0.0) {
        register_module("dropout", dropout);
    }
    
    // Initialisation des poids avec Xavier uniform
    torch::nn::init::xavier_uniform_(q_proj->weight);
    torch::nn::init::xavier_uniform_(k_proj->weight);
    torch::nn::init::xavier_uniform_(v_proj->weight);
    torch::nn::init::xavier_uniform_(out_proj->weight);
}

torch::Tensor FlashAttention::forward(torch::Tensor query, torch::Tensor key, torch::Tensor value) {
    auto batch_size = query.size(0);
    auto seq_len = query.size(1);
    
    // Projections linéaires
    auto q = q_proj->forward(query);
    auto k = k_proj->forward(key);
    auto v = v_proj->forward(value);
    
    // Reshape pour multi-head attention
    // [batch_size, seq_len, embed_dim] -> [batch_size, seq_len, num_heads, head_dim]
    q = q.view({batch_size, seq_len, num_heads, head_dim});
    k = k.view({batch_size, seq_len, num_heads, head_dim});
    v = v.view({batch_size, seq_len, num_heads, head_dim});
    
    // Transpose pour avoir [batch_size, num_heads, seq_len, head_dim]
    q = q.transpose(1, 2);
    k = k.transpose(1, 2);
    v = v.transpose(1, 2);
    
    // Utiliser torch::nn::functional::scaled_dot_product_attention si disponible (PyTorch 2.0+)
    // Sinon, implémenter manuellement
    torch::Tensor attn_output;
    
    #if TORCH_VERSION_MAJOR >= 2
    // Utiliser la Flash Attention native de PyTorch 2.0+
    attn_output = torch::scaled_dot_product_attention(
        q, k, v,
        /*attn_mask=*/c10::nullopt,
        /*dropout_p=*/dropout_p,
        /*is_causal=*/false
    );
    #else
    // Implémentation manuelle pour les versions antérieures
    auto scale = 1.0 / std::sqrt(static_cast<double>(head_dim));
    auto scores = torch::matmul(q, k.transpose(-2, -1)) * scale;
    auto attn_weights = torch::softmax(scores, -1);
    
    if (dropout_p > 0.0 && is_training()) {
        attn_weights = dropout->forward(attn_weights);
    }
    
    attn_output = torch::matmul(attn_weights, v);
    #endif
    
    // Reshape vers [batch_size, seq_len, num_heads, head_dim]
    attn_output = attn_output.transpose(1, 2).contiguous();
    
    // Concatener les têtes: [batch_size, seq_len, embed_dim]
    attn_output = attn_output.view({batch_size, seq_len, embed_dim});
    
    // Projection finale
    return out_proj->forward(attn_output);
}

torch::Tensor FlashAttention::forward_with_mask(torch::Tensor query, torch::Tensor key, torch::Tensor value, 
                                               torch::Tensor attn_mask) {
    auto batch_size = query.size(0);
    auto seq_len = query.size(1);
    
    // Projections linéaires
    auto q = q_proj->forward(query);
    auto k = k_proj->forward(key);
    auto v = v_proj->forward(value);
    
    // Reshape pour multi-head attention
    q = q.view({batch_size, seq_len, num_heads, head_dim});
    k = k.view({batch_size, seq_len, num_heads, head_dim});
    v = v.view({batch_size, seq_len, num_heads, head_dim});
    
    // Transpose
    q = q.transpose(1, 2);
    k = k.transpose(1, 2);
    v = v.transpose(1, 2);
    
    torch::Tensor attn_output;
    
    #if TORCH_VERSION_MAJOR >= 2
    attn_output = torch::scaled_dot_product_attention(
        q, k, v,
        /*attn_mask=*/attn_mask,
        /*dropout_p=*/dropout_p,
        /*is_causal=*/false
    );
    #else
    // Implémentation manuelle avec masque
    auto scale = 1.0 / std::sqrt(static_cast<double>(head_dim));
    auto scores = torch::matmul(q, k.transpose(-2, -1)) * scale;
    
    if (attn_mask.defined()) {
        scores = scores.masked_fill(attn_mask == 0, -1e9);
    }
    
    auto attn_weights = torch::softmax(scores, -1);
    
    if (dropout_p > 0.0 && is_training()) {
        attn_weights = dropout->forward(attn_weights);
    }
    
    attn_output = torch::matmul(attn_weights, v);
    #endif
    
    // Reshape et projection finale
    attn_output = attn_output.transpose(1, 2).contiguous();
    attn_output = attn_output.view({batch_size, seq_len, embed_dim});
    
    return out_proj->forward(attn_output);
}

torch::Tensor FlashAttention::forward_with_dropout(torch::Tensor query, torch::Tensor key, torch::Tensor value, 
                                                  double custom_dropout_p, bool training) {
    auto batch_size = query.size(0);
    auto seq_len = query.size(1);
    
    // Projections linéaires
    auto q = q_proj->forward(query);
    auto k = k_proj->forward(key);
    auto v = v_proj->forward(value);
    
    // Reshape pour multi-head attention
    q = q.view({batch_size, seq_len, num_heads, head_dim});
    k = k.view({batch_size, seq_len, num_heads, head_dim});
    v = v.view({batch_size, seq_len, num_heads, head_dim});
    
    // Transpose
    q = q.transpose(1, 2);
    k = k.transpose(1, 2);
    v = v.transpose(1, 2);
    
    torch::Tensor attn_output;
    
    #if TORCH_VERSION_MAJOR >= 2
    attn_output = torch::scaled_dot_product_attention(
        q, k, v,
        /*attn_mask=*/c10::nullopt,
        /*dropout_p=*/custom_dropout_p,
        /*is_causal=*/false
    );
    #else
    // Implémentation manuelle avec dropout personnalisé
    auto scale = 1.0 / std::sqrt(static_cast<double>(head_dim));
    auto scores = torch::matmul(q, k.transpose(-2, -1)) * scale;
    auto attn_weights = torch::softmax(scores, -1);
    
    if (custom_dropout_p > 0.0 && training) {
        attn_weights = torch::dropout(attn_weights, custom_dropout_p, training);
    }
    
    attn_output = torch::matmul(attn_weights, v);
    #endif
    
    // Reshape et projection finale
    attn_output = attn_output.transpose(1, 2).contiguous();
    attn_output = attn_output.view({batch_size, seq_len, embed_dim});
    
    return out_proj->forward(attn_output);
}

// Implémentation de LoRALinear
LoRALinear::LoRALinear(int64_t in_features, int64_t out_features, int rank, double alpha, torch::ScalarType dtype)
    : rank(rank), alpha(alpha), merged(false) {

    // Créer la couche linéaire originale
    original_layer = torch::nn::Linear(torch::nn::LinearOptions(in_features, out_features));

    // Créer les matrices LoRA de bas rang
    // A: [in_features, rank] - initialisée avec distribution normale
    // B: [rank, out_features] - initialisée à zéro pour stabilité
    lora_A = torch::nn::Linear(torch::nn::LinearOptions(in_features, rank).bias(false));
    lora_B = torch::nn::Linear(torch::nn::LinearOptions(rank, out_features).bias(false));

    // Initialisation de LoRA
    torch::nn::init::normal_(lora_A->weight, 0.0, 1.0 / std::sqrt(rank));
    torch::nn::init::zeros_(lora_B->weight);

    // Enregistrer les modules
    register_module("original", original_layer);
    register_module("lora_A", lora_A);
    register_module("lora_B", lora_B);

    // Convertir tous les modules au dtype approprié
    original_layer->to(dtype);
    lora_A->to(dtype);
    lora_B->to(dtype);
    
    // Figer les poids originaux
    for (auto& param : original_layer->parameters()) {
        param.requires_grad_(false);
    }
    
    // Activer les gradients pour les paramètres LoRA
    for (auto& param : lora_A->parameters()) {
        param.requires_grad_(true);
    }
    for (auto& param : lora_B->parameters()) {
        param.requires_grad_(true);
    }
}

torch::Tensor LoRALinear::forward(torch::Tensor x) {
    // Forward pass de la couche originale
    auto result = original_layer->forward(x);
    
    if (!merged) {
        // Ajouter la contribution LoRA: result + alpha * B(A(x))
        auto lora_out = lora_B->forward(lora_A->forward(x));
        result = result + (alpha / rank) * lora_out;
    }
    
    return result;
}

void LoRALinear::merge_weights() {
    if (!merged) {
        // Calculer delta_W = alpha * B * A
        auto delta_W = (alpha / rank) * torch::matmul(lora_B->weight, lora_A->weight);
        
        // Fusionner dans les poids originaux
        original_layer->weight.data() += delta_W;
        merged = true;
    }
}

void LoRALinear::unmerge_weights() {
    if (merged) {
        // Retirer delta_W des poids originaux
        auto delta_W = (alpha / rank) * torch::matmul(lora_B->weight, lora_A->weight);
        original_layer->weight.data() -= delta_W;
        merged = false;
    }
}

std::unordered_map<std::string, torch::Tensor> LoRALinear::get_lora_state_dict() {
    std::unordered_map<std::string, torch::Tensor> state_dict;
    state_dict["lora_A.weight"] = lora_A->weight.clone();
    state_dict["lora_B.weight"] = lora_B->weight.clone();
    return state_dict;
}

void LoRALinear::load_lora_state_dict(const std::unordered_map<std::string, torch::Tensor>& state_dict) {
    auto it_A = state_dict.find("lora_A.weight");
    auto it_B = state_dict.find("lora_B.weight");
    
    if (it_A != state_dict.end()) {
        lora_A->weight.data().copy_(it_A->second);
    }
    if (it_B != state_dict.end()) {
        lora_B->weight.data().copy_(it_B->second);
    }
}

// ==================== Fin Implémentation LoRA ====================
// Initialisation du type
int16_t TorchLRScheduler::scheduler_type = 0;

// Implémentation de TorchLRScheduler
wstring TorchLRScheduler::asString(LispE* lisp) {
    std::wostringstream oss;
    oss << L"LR Scheduler (";
    switch(sched_type) {
        case SCHEDULER_LINEAR: oss << L"Linear"; break;
        case SCHEDULER_COSINE: oss << L"Cosine"; break;
        case SCHEDULER_EXPONENTIAL: oss << L"Exponential"; break;
        case SCHEDULER_STEP: oss << L"Step"; break;
        case SCHEDULER_LINEAR_WARMUP_COSINE: oss << L"LinearWarmupCosine"; break;
    }
    oss << L", step=" << current_step 
        << L", lr=" << get_lr() << L")";
    return oss.str();
}

void TorchLRScheduler::step() {
    current_step++;
    double new_lr = compute_lr();
    set_lr(new_lr);
}

void TorchLRScheduler::set_lr(double lr) {
    if (!optimizer_ref || !optimizer_ref->optimizer) return;
    
    auto& opt = optimizer_ref->optimizer;
    for (auto& group : opt->param_groups()) {
        if (auto* adam_opt = dynamic_cast<torch::optim::AdamOptions*>(&group.options())) {
            adam_opt->lr(lr);
        }
        else if (auto* sgd_opt = dynamic_cast<torch::optim::SGDOptions*>(&group.options())) {
            sgd_opt->lr(lr);
        }
        else if (auto* adamw_opt = dynamic_cast<torch::optim::AdamWOptions*>(&group.options())) {
            adamw_opt->lr(lr);
        }
    }
}

double TorchLRScheduler::get_lr() {
    if (!optimizer_ref || !optimizer_ref->optimizer) return initial_lr;
    
    auto& opt = optimizer_ref->optimizer;
    auto& param_groups = opt->param_groups();
    if (!param_groups.empty()) {
        auto& options = param_groups[0].options();
        
        if (auto* adam_opt = dynamic_cast<torch::optim::AdamOptions*>(&options)) {
            return adam_opt->lr();
        }
        else if (auto* sgd_opt = dynamic_cast<torch::optim::SGDOptions*>(&options)) {
            return sgd_opt->lr();
        }
        else if (auto* adamw_opt = dynamic_cast<torch::optim::AdamWOptions*>(&options)) {
            return adamw_opt->lr();
        }
    }
    return initial_lr;
}

double TorchLRScheduler::compute_lr() {
    switch(sched_type) {
        case SCHEDULER_LINEAR_WARMUP_COSINE:
            return linear_warmup_cosine_lr(current_step);
        case SCHEDULER_COSINE:
            return cosine_annealing_lr(current_step);
        case SCHEDULER_EXPONENTIAL:
            return exponential_lr(current_step);
        case SCHEDULER_STEP:
            return step_lr(current_step);
        case SCHEDULER_LINEAR:
            return linear_lr(current_step);
        default:
            return initial_lr;
    }
}

double TorchLRScheduler::linear_warmup_cosine_lr(int step) {
    if (step < warmup_steps) {
        // Phase de warmup linéaire
        return initial_lr * (static_cast<double>(step) / warmup_steps);
    } else {
        // Phase de cosine annealing
        int decay_steps = total_steps - warmup_steps;
        int current_decay_step = step - warmup_steps;
        double cosine_decay = 0.5 * (1.0 + std::cos(M_PI * current_decay_step / decay_steps));
        return min_lr + (initial_lr - min_lr) * cosine_decay;
    }
}

double TorchLRScheduler::cosine_annealing_lr(int step) {
    double cosine_decay = 0.5 * (1.0 + std::cos(M_PI * step / total_steps));
    return min_lr + (initial_lr - min_lr) * cosine_decay;
}

double TorchLRScheduler::exponential_lr(int step) {
    return initial_lr * std::pow(gamma, step);
}

double TorchLRScheduler::step_lr(int step) {
    int num_decays = step / step_size;
    return initial_lr * std::pow(gamma, num_decays);
}

double TorchLRScheduler::linear_lr(int step) {
    double decay = static_cast<double>(total_steps - step) / total_steps;
    return min_lr + (initial_lr - min_lr) * decay;
}

// --- Ajout : torch_tensor_to_cpu ---
Element* Lispe_lispetorch::tensor_to_cpu(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable(L"tensor");
    try {
        if (tensor_elem->type != t_tensor) {
            throw new Error("Error: Argument must be a tensor");
        }
        TorchTensor* tensor = (TorchTensor*)tensor_elem;
        return new TorchTensor(std::move(tensor->tensor.to(torch::kCPU)));
    } catch (const std::exception& e) {
        string msg = "Error moving tensor to CPU: ";
        msg += e.what();
        throw new Error(msg);
    }
}

// --- Ajout : torch_cuda_empty_cache ---
Element* Lispe_lispetorch::cuda_empty_cache(LispE* lisp) {
#ifdef USE_CUDA
    try {
        c10::cuda::CUDACachingAllocator::emptyCache();
        return True_;
    } catch (const std::exception& e) {
        string msg = "Error emptying CUDA cache: ";
        msg += e.what();
        throw new Error(msg);
    }
#else
    throw new Error("Error: CUDA support not compiled");
#endif
}

// Implémentation des méthodes de la classe TorchGenerator
torch::Tensor TorchGenerator::generate(torch::Tensor input_ids) {
    switch(config.strategy[0]) {
        case 'g': // greedy
            return greedy_search(input_ids);
        case 't': 
            if (config.strategy == "top_k") {
                return top_k_sampling(input_ids);
            } else if (config.strategy == "top_p") {
                return top_p_sampling(input_ids);
            }
            break;
        case 'b': // beam_search
            return beam_search(input_ids);
    }
    return greedy_search(input_ids); // default fallback
}

torch::Tensor TorchGenerator::greedy_search(torch::Tensor input_ids) {
    auto current_ids = input_ids.clone().to(device);
    
    for (int64_t step = 0; step < config.max_length; ++step) {
        torch::NoGradGuard no_grad;
        
        // Simple forward pass - pour l'instant on simule une génération
        // En pratique, il faudrait un modèle spécialisé ou utiliser torch::jit
        auto vocab_size = 50257; // Taille du vocabulaire par défaut GPT
        auto logits = torch::randn({1, vocab_size}).to(device);
        
        // Prendre le dernier token (séquence actuelle)
        auto last_logits = logits.select(1, -1);
        
        // Appliquer la pénalité de répétition si configurée
        if (config.repetition_penalty != 1.0) {
            last_logits = apply_repetition_penalty(last_logits, current_ids);
        }
        
        // Greedy: prendre le token avec la plus haute probabilité
        auto next_token = torch::argmax(last_logits, /*dim=*/1, /*keepdim=*/true);
        
        // Vérifier les tokens de fin
        if (next_token.item<int64_t>() == config.eos_token_id) {
            break;
        }
        
        // Concaténer le nouveau token
        current_ids = torch::cat({current_ids, next_token}, /*dim=*/1);
    }
    
    return current_ids;
}

torch::Tensor TorchGenerator::top_k_sampling(torch::Tensor input_ids) {
    auto current_ids = input_ids.clone().to(device);
    
    for (int64_t step = 0; step < config.max_length; ++step) {
        torch::NoGradGuard no_grad;
        
        // Simple forward pass - simulation pour la démonstration
        auto vocab_size = 50257;
        auto logits = torch::randn({1, vocab_size}).to(device) / config.temperature;
        
        // Appliquer la pénalité de répétition
        if (config.repetition_penalty != 1.0) {
            logits = apply_repetition_penalty(logits, current_ids);
        }
        
        // Top-k filtering
        auto [top_k_values, top_k_indices] = torch::topk(logits, config.top_k, /*dim=*/1);
        
        // Créer un masque pour les valeurs non top-k
        auto probs = torch::full_like(logits, -std::numeric_limits<double>::infinity());
        probs.scatter_(1, top_k_indices, top_k_values);
        probs = torch::softmax(probs, /*dim=*/1);
        
        // Échantillonner depuis la distribution
        auto next_token = torch::multinomial(probs, 1);
        
        if (next_token.item<int64_t>() == config.eos_token_id) break;
        current_ids = torch::cat({current_ids, next_token}, 1);
    }
    
    return current_ids;
}

torch::Tensor TorchGenerator::top_p_sampling(torch::Tensor input_ids) {
    auto current_ids = input_ids.clone().to(device);
    
    for (int64_t step = 0; step < config.max_length; ++step) {
        torch::NoGradGuard no_grad;
        
        // Simple forward pass - simulation pour la démonstration
        auto vocab_size = 50257;
        auto logits = torch::randn({1, vocab_size}).to(device) / config.temperature;
        
        // Appliquer la pénalité de répétition
        if (config.repetition_penalty != 1.0) {
            logits = apply_repetition_penalty(logits, current_ids);
        }
        
        // Convertir en probabilités et trier
        auto probs = torch::softmax(logits, /*dim=*/1);
        auto [sorted_probs, sorted_indices] = torch::sort(probs, /*dim=*/1, /*descending=*/true);
        
        // Calculer les probabilités cumulatives
        auto cumulative_probs = torch::cumsum(sorted_probs, /*dim=*/1);
        
        // Masquer les tokens au-delà du seuil top_p
        auto mask = cumulative_probs > config.top_p;
        sorted_probs.masked_fill_(mask, 0.0);
        
        // Renormaliser
        auto total = sorted_probs.sum(/*dim=*/1, /*keepdim=*/true);
        sorted_probs = sorted_probs / (total + 1e-8); // Éviter division par zéro
        
        // Échantillonner depuis la distribution nucleus
        auto next_token_idx = torch::multinomial(sorted_probs, 1);
        auto next_token = sorted_indices.gather(1, next_token_idx);
        
        if (next_token.item<int64_t>() == config.eos_token_id) break;
        current_ids = torch::cat({current_ids, next_token}, 1);
    }
    
    return current_ids;
}

torch::Tensor TorchGenerator::beam_search(torch::Tensor input_ids) {
    // Implémentation simplifiée de beam search
    // Pour l'instant, on retourne greedy search
    // TODO: Implémenter le vrai beam search avec gestion des beams multiples
    return greedy_search(input_ids);
}

torch::Tensor TorchGenerator::apply_repetition_penalty(torch::Tensor logits, torch::Tensor generated_ids) {
    if (config.repetition_penalty == 1.0) return logits;
    
    // Créer un masque simple basé sur les tokens générés
    auto penalty_mask = torch::zeros_like(logits);
    
    // Appliquer une pénalité simple sur les derniers tokens
    auto seq_len = generated_ids.size(1);
    int64_t start_idx = (seq_len > 10) ? (seq_len - 10) : 0;
    for (int64_t i = start_idx; i < seq_len; ++i) {
        auto token_id = generated_ids[0][i].item<int64_t>();
        if (token_id < logits.size(1)) {
            penalty_mask[0][token_id] = 1.0;
        }
    }
    
    // Appliquer la pénalité (diviser les logits des tokens répétés)
    auto penalized_logits = logits.clone();
    penalized_logits = torch::where(penalty_mask == 1.0, 
                                   logits / config.repetition_penalty, 
                                   logits);
    
    return penalized_logits;
}

// Méthode asString pour TorchGenerator
wstring TorchGenerator::asString(LispE* lisp) {
    stringstream ss;
    ss << "TorchGenerator(max_length=" << config.max_length 
       << ", temperature=" << config.temperature
       << ", strategy=" << config.strategy << ")";
    
    string str = ss.str();
    wstring result;
    for (char c : str) {
        result += (wchar_t)c;
    }
    return result;
}

// Méthode duplicate pour TorchGenerator
Element* TorchGenerator::duplicate_minimal(LispE* lisp) {
    return new TorchGenerator(model, device);
}

Element* TorchGenerator::fullcopy() {
    auto copy = new TorchGenerator(model, device);
    copy->config = this->config;
    return copy;
}

Element* TorchGenerator::copyatom(LispE* lisp, uint16_t s) {
    if (s == t_generator)
        return this;
    return null_;
}

bool TorchGenerator::unify(LispE* lisp, Element* value, bool record) {
    if (value->type == t_generator) {
        return true;
    }
    return false;
}

bool TorchGenerator::egal(Element* value) {
    return (value->type == t_generator);
}

// Fonction utilitaire pour la décomposition de Tucker
std::tuple<torch::Tensor, std::vector<torch::Tensor>> tucker_decompose(
    torch::Tensor tensor, 
    const std::vector<int64_t>& ranks, 
    int max_iter, 
    double tol) {
    
    auto device = tensor.device();
    auto dtype = tensor.dtype();
    int ndim = tensor.dim();
    
    if (ranks.size() != ndim) {
        throw std::runtime_error("Number of ranks must match tensor dimensions");
    }
    
    // Initialiser les facteurs avec des valeurs aléatoires orthogonales
    std::vector<torch::Tensor> factors;
    for (int mode = 0; mode < ndim; mode++) {
        auto factor = torch::randn({tensor.size(mode), ranks[mode]}, 
                                   torch::TensorOptions().dtype(dtype).device(device));
        // Orthogonalisation QR
        auto qr_result = torch::qr(factor);
        factors.push_back(std::get<0>(qr_result));
    }
    
    torch::Tensor core;
    double prev_error = std::numeric_limits<double>::infinity();
    
    for (int iter = 0; iter < max_iter; iter++) {
        // Mise à jour de chaque facteur par ALS (Alternating Least Squares)
        for (int mode = 0; mode < ndim; mode++) {
            // Mode-n matricization
            auto unfolded = tensor_unfold(tensor, mode);
            
            // Produit de Khatri-Rao de tous les autres facteurs
            torch::Tensor khatri_rao_result = factors[0];
            if (mode == 0 && ndim > 1) {
                khatri_rao_result = factors[1];
                for (int i = 2; i < ndim; i++) {
                    if (i != mode) {
                        khatri_rao_result = the_khatri_rao_product(khatri_rao_result, factors[i]);
                    }
                }
            } else if (mode > 0) {
                khatri_rao_result = factors[0];
                for (int i = 1; i < ndim; i++) {
                    if (i != mode) {
                        khatri_rao_result = the_khatri_rao_product(khatri_rao_result, factors[i]);
                    }
                }
            }
            
            // Résoudre le problème des moindres carrés
            auto pseudo_inverse = torch::pinverse(khatri_rao_result);
            factors[mode] = torch::mm(unfolded, pseudo_inverse).t();
            
            // Orthogonalisation
            auto qr_result = torch::qr(factors[mode]);
            factors[mode] = std::get<0>(qr_result);
        }
        
        // Calculer le noyau (core tensor)
        core = tensor;
        for (int mode = 0; mode < ndim; mode++) {
            core = tensor_mode_product(core, factors[mode].t(), mode);
        }
        
        // Vérifier la convergence
        auto reconstructed = tucker_reconstruct_tensor(core, factors);
        auto error = torch::norm(tensor - reconstructed).item<double>();
        
        if (std::abs(prev_error - error) < tol) {
            break;
        }
        prev_error = error;
    }
    
    return std::make_tuple(core, factors);
}

// Fonction de reconstruction à partir des composantes Tucker
torch::Tensor tucker_reconstruct_tensor(torch::Tensor core, const std::vector<torch::Tensor>& factors) {
    torch::Tensor result = core;
    
    for (int mode = 0; mode < factors.size(); mode++) {
        result = tensor_mode_product(result, factors[mode], mode);
    }
    
    return result;
}

// Produit de Khatri-Rao de deux matrices
torch::Tensor the_khatri_rao_product(torch::Tensor A, torch::Tensor B) {
    auto rows_A = A.size(0);
    auto rows_B = B.size(0);
    auto cols = A.size(1);
    
    if (cols != B.size(1)) {
        throw std::runtime_error("Matrices must have the same number of columns");
    }
    
    auto result = torch::zeros({rows_A * rows_B, cols}, A.options());
    
    for (int64_t j = 0; j < cols; j++) {
        auto col_A = A.select(1, j);
        auto col_B = B.select(1, j);
        auto kron_prod = torch::kron(col_A, col_B);
        result.select(1, j).copy_(kron_prod);
    }
    
    return result;
}

// Dépliage d'un tenseur selon un mode (matricization)
torch::Tensor tensor_unfold(torch::Tensor tensor, int mode) {
    auto sizes = tensor.sizes().vec();
    int ndim = sizes.size();
    
    // Réorganiser les dimensions: mode en premier, puis les autres
    std::vector<int64_t> permute_dims;
    permute_dims.push_back(mode);
    for (int i = 0; i < ndim; i++) {
        if (i != mode) {
            permute_dims.push_back(i);
        }
    }
    
    auto permuted = tensor.permute(permute_dims);
    
    // Reshaper en matrice
    int64_t mode_size = sizes[mode];
    int64_t other_size = 1;
    for (int i = 0; i < ndim; i++) {
        if (i != mode) {
            other_size *= sizes[i];
        }
    }
    
    return permuted.reshape({mode_size, other_size});
}

// Produit mode-n d'un tenseur avec une matrice
torch::Tensor tensor_mode_product(torch::Tensor tensor, torch::Tensor matrix, int mode) {
    auto unfolded = tensor_unfold(tensor, mode);
    auto product = torch::mm(matrix, unfolded);
    
    // Reconstuire la forme du tenseur
    auto sizes = tensor.sizes().vec();
    sizes[mode] = matrix.size(0);
    
    // Réorganiser les dimensions pour remettre en forme
    std::vector<int64_t> permute_back;
    std::vector<int64_t> temp_shape;
    
    temp_shape.push_back(matrix.size(0));
    for (int i = 0; i < tensor.dim(); i++) {
        if (i != mode) {
            temp_shape.push_back(sizes[i]);
        }
    }
    
    auto reshaped = product.reshape(temp_shape);
    
    // Permuter pour remettre les dimensions dans l'ordre original
    std::vector<int64_t> inverse_permute(tensor.dim());
    inverse_permute[mode] = 0;
    int idx = 1;
    for (int i = 0; i < tensor.dim(); i++) {
        if (i != mode) {
            inverse_permute[i] = idx++;
        }
    }
    
    // Créer la permutation inverse
    std::vector<int64_t> final_permute(tensor.dim());
    for (int i = 0; i < tensor.dim(); i++) {
        final_permute[inverse_permute[i]] = i;
    }
    
    return reshaped.permute(final_permute);
}

// Calculer le ratio de compression de Tucker
double calculate_tucker_compression_ratio(
    const std::vector<int64_t>& original_shape,
    const std::vector<int64_t>& core_shape,
    const std::vector<std::vector<int64_t>>& factor_shapes) {
    
    // Nombre d'éléments dans le tenseur original
    int64_t original_elements = 1;
    for (auto dim : original_shape) {
        original_elements *= dim;
    }
    
    // Nombre d'éléments dans le noyau
    int64_t core_elements = 1;
    for (auto dim : core_shape) {
        core_elements *= dim;
    }
    
    // Nombre d'éléments dans les facteurs
    int64_t factor_elements = 0;
    for (const auto& shape : factor_shapes) {
        int64_t factor_size = 1;
        for (auto dim : shape) {
            factor_size *= dim;
        }
        factor_elements += factor_size;
    }
    
    int64_t compressed_elements = core_elements + factor_elements;
    
    return static_cast<double>(original_elements) / static_cast<double>(compressed_elements);
}

// Implémentation de TorchModel
wstring TorchModel::asString(LispE* lisp) {
    return L"PyTorch Model";
}

// Implémentation de TorchOptimizer
wstring TorchOptimizer::asString(LispE* lisp) {
    return L"PyTorch Optimizer";
}

// Implémentation de TorchMultiHeadAttention
wstring TorchMultiHeadAttention::asString(LispE* lisp) {
    return L"PyTorch Multi-Head Attention";
}

// Implémentation de TorchLayerNorm
wstring TorchLayerNorm::asString(LispE* lisp) {
    return L"PyTorch Layer Normalization";
}

// Implémentation de TorchEmbedding
wstring TorchEmbedding::asString(LispE* lisp) {
    return L"PyTorch Embedding";
}

// Implémentation de TorchLinear
wstring TorchLinear::asString(LispE* lisp) {
    return L"PyTorch Linear Layer";
}

// Implémentation de TorchTransformerBlock
wstring TorchTransformerBlock::asString(LispE* lisp) {
    return L"PyTorch Transformer Block";
}

// Implémentation de TorchPositionalEncoding
wstring TorchPositionalEncoding::asString(LispE* lisp) {
    return L"PyTorch Positional Encoding";
}

// Implémentation de TorchRotaryEmbedding
wstring TorchRotaryEmbedding::asString(LispE* lisp) {
    std::wostringstream oss;
    if (rope) {
        oss << L"PyTorch Rotary Embedding (dim=" << rope->dim 
            << L", max_seq_len=" << rope->max_seq_len << L")";
    } else {
        oss << L"PyTorch Rotary Embedding (uninitialized)";
    }
    return oss.str();
}

// Implémentation de TorchCheckpointedModule
wstring TorchCheckpointedModule::asString(LispE* lisp) {
    std::wostringstream oss;
    if (module) {
        oss << L"PyTorch Checkpointed Module (checkpointing " 
            << (module->checkpointing_enabled ? L"enabled" : L"disabled") << L")";
    } else {
        oss << L"PyTorch Checkpointed Module (uninitialized)";
    }
    return oss.str();
}

// Implémentation de TorchStateDict
wstring TorchStateDict::asString(LispE* lisp) {
    std::wostringstream oss;
    oss << L"PyTorch State Dict (" << state_dict.size() << L" parameters)";
    return oss.str();
}

// Implémentation de TorchFlashAttention asString
wstring TorchFlashAttention::asString(LispE* lisp) {
    std::wostringstream oss;
    if (flash_attention) {
        oss << L"Flash Attention ("
            << L"embed_dim=" << flash_attention->embed_dim
            << L", num_heads=" << flash_attention->num_heads
            << L", head_dim=" << flash_attention->head_dim
            << L", dropout=" << flash_attention->dropout_p
            << L", bias=" << (flash_attention->use_bias ? L"true" : L"false") << L")";
    } else {
        oss << L"Flash Attention (null)";
    }
    return oss.str();
}

// Implémentation de TorchLoRALinear asString
wstring TorchLoRALinear::asString(LispE* lisp) {
    std::wostringstream oss;
    if (lora_layer) {
        auto& A_weight = lora_layer->lora_A->weight;
        auto& B_weight = lora_layer->lora_B->weight;
        oss << L"LoRA Linear Layer ("
            << A_weight.size(1) << L" -> " << B_weight.size(0) 
            << L", rank=" << lora_layer->rank 
            << L", alpha=" << lora_layer->alpha
            << L", merged=" << (lora_layer->merged ? L"true" : L"false") << L")";
    } else {
        oss << L"LoRA Linear Layer (null)";
    }
    return oss.str();
}

// Implémentation de TorchLoRAConfig asString
wstring TorchLoRAConfig::asString(LispE* lisp) {
    std::wostringstream oss;
    oss << L"LoRA Config (rank=" << config.rank 
        << L", alpha=" << config.alpha 
        << L", targets=" << config.target_modules.size() << L")";
    return oss.str();
}

// Implémentation de SimpleTokenizer
SimpleTokenizer::SimpleTokenizer() {
    // Tokens spéciaux
    token_to_id["<pad>"] = 0;
    token_to_id["<unk>"] = 1;
    token_to_id["<bos>"] = 2;
    token_to_id["<eos>"] = 3;
    
    id_to_token = {"<pad>", "<unk>", "<bos>", "<eos>"};
}

std::vector<int> SimpleTokenizer::encode(const std::string& text) {
    std::vector<int> tokens;
    tokens.push_back(2); // <bos>
    
    // Simple tokenization par espaces et ponctuation
    std::string current_token;
    for (char c : text) {
        if (isspace(c) || ispunct(c)) {
            if (!current_token.empty()) {
                if (token_to_id.find(current_token) == token_to_id.end()) {
                    add_token(current_token);
                }
                tokens.push_back(token_to_id[current_token]);
                current_token.clear();
            }
            
            // Ajouter la ponctuation comme token séparé
            if (ispunct(c)) {
                std::string punct_token(1, c);
                if (token_to_id.find(punct_token) == token_to_id.end()) {
                    add_token(punct_token);
                }
                tokens.push_back(token_to_id[punct_token]);
            }
        } else {
            current_token += tolower(c); // Convertir en minuscules
        }
    }
    
    // Traiter le dernier token
    if (!current_token.empty()) {
        if (token_to_id.find(current_token) == token_to_id.end()) {
            add_token(current_token);
        }
        tokens.push_back(token_to_id[current_token]);
    }
    
    tokens.push_back(3); // <eos>
    return tokens;
}

std::string SimpleTokenizer::decode(const std::vector<int>& token_ids) {
    std::string result;
    for (size_t i = 0; i < token_ids.size(); i++) {
        int id = token_ids[i];
        if (id >= 0 && id < (int)id_to_token.size()) {
            const std::string& token = id_to_token[id];
            // Skip special tokens sauf si c'est pour le debug
            if (token != "<pad>" && token != "<bos>" && token != "<eos>") {
                if (!result.empty() && !ispunct(token[0])) {
                    result += " ";
                }
                result += token;
            }
        }
    }
    return result;
}

void SimpleTokenizer::add_token(const std::string& token) {
    if (token_to_id.find(token) == token_to_id.end()) {
        token_to_id[token] = next_id;
        id_to_token.push_back(token);
        next_id++;
    }
}

// Implémentation de TorchTokenizer
std::vector<int> TorchTokenizer::encode(const std::string& text) {
    if (type == SIMPLE) {
        return simple_tokenizer->encode(text);
    }
#ifdef USE_SENTENCEPIECE
    else if (type == SENTENCEPIECE) {
        std::vector<int> ids;
        auto status = sp_processor->Encode(text, &ids);
        if (!status.ok()) {
            throw new Error("Failed to encode text with SentencePiece");
        }
        return ids;
    }
#endif
    return {};
}

std::string TorchTokenizer::decode(const std::vector<int>& token_ids) {
    if (type == SIMPLE) {
        return simple_tokenizer->decode(token_ids);
    }
#ifdef USE_SENTENCEPIECE
    else if (type == SENTENCEPIECE) {
        std::string text;
        auto status = sp_processor->Decode(token_ids, &text);
        if (!status.ok()) {
            throw new Error("Failed to decode tokens with SentencePiece");
        }
        return text;
    }
#endif
    return "";
}

int TorchTokenizer::vocab_size() {
    if (type == SIMPLE) {
        return simple_tokenizer->vocab_size();
    }
#ifdef USE_SENTENCEPIECE
    else if (type == SENTENCEPIECE) {
        return sp_processor->GetPieceSize();
    }
#endif
    return 0;
}

bool TorchTokenizer::Boolean() {
    if (type == SIMPLE) {
        return simple_tokenizer != nullptr;
    }
#ifdef USE_SENTENCEPIECE
    else if (type == SENTENCEPIECE) {
        return sp_processor != nullptr;
    }
#endif
    return false;
}

wstring TorchTokenizer::asString(LispE* lisp) {
    if (type == SIMPLE) {
        return L"Simple Tokenizer (vocab size: " + std::to_wstring(vocab_size()) + L")";
    }
#ifdef USE_SENTENCEPIECE
    else if (type == SENTENCEPIECE) {
        return L"SentencePiece Tokenizer (vocab size: " + std::to_wstring(vocab_size()) + L")";
    }
#endif
    return L"Unknown Tokenizer";
}

// 1. EXTENSIONS DE LA CLASSE LoRALinear
// À ajouter dans la section des méthodes de LoRALinear

torch::Tensor LoRALinear::compute_lora_delta() {
    // Calcule α/r * B @ A pour la correction LoRA
    auto delta = torch::matmul(lora_B->weight, lora_A->weight);
    return (alpha / rank) * delta;
}

void LoRALinear::apply_to_base_weight(torch::Tensor& base_weight) {
    if (!merged) {
        auto delta = compute_lora_delta();
        base_weight += delta;
        merged = true;
    }
}

void LoRALinear::remove_from_base_weight(torch::Tensor& base_weight) {
    if (merged) {
        auto delta = compute_lora_delta();
        base_weight -= delta;
        merged = false;
    }
}

torch::Tensor LoRALinear::forward_with_gradients(torch::Tensor x, bool retain_graph) {
    // Forward pass en conservant les gradients pour l'entraînement
    auto result = original_layer->forward(x);
    
    if (!merged) {
        // Calculer la contribution LoRA avec gradients
        torch::Tensor lora_hidden;
        {
            // S'assurer que les gradients sont conservés
            torch::AutoGradMode enable_grad(true);
            lora_hidden = lora_A->forward(x);
            lora_hidden = lora_B->forward(lora_hidden);
        }
        
        auto scaled_lora = (alpha / rank) * lora_hidden;
        result = result + scaled_lora;
    }
    
    return result;
}

double LoRALinear::get_adaptation_magnitude() {
    auto delta = compute_lora_delta();
    return torch::norm(delta).item<double>();
}

std::pair<torch::Tensor, torch::Tensor> LoRALinear::get_svd_components() {
    auto delta = compute_lora_delta();
#ifdef __APPLE__
    // macOS: Utiliser notre implémentation TensorSVD optimisée
    auto [U, S, V] = lispe_svd::compute_svd(delta, true);
    return std::make_pair(U, S);
#else
    // Autres plateformes: Utiliser torch::svd standard
    auto svd_result = torch::svd(delta);
    return std::make_pair(std::get<0>(svd_result), std::get<1>(svd_result));
#endif
}

