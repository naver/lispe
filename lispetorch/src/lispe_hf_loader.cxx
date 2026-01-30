#include "lispe_hf_loader.h"
#include "lispe_lispetorch.h"
#include <fstream>
#include <filesystem>
#include <regex>
#include <thread>
#include <chrono>
#include <future>

const bool display_debug = true;
#define affichage if (display_debug) std::cout

namespace fs = std::filesystem;

namespace lispe_hf {

HuggingFaceLoader::HuggingFaceLoader(const HFLoadConfig& config)
    : model_loaded_(false), load_config_(config), device_(config.device), enable_kv_cache(false), ctx_basic(false) {
        ctx_basic.status = s_constant;
        threaded = false;
        fused = true;
        memory_usage = 0;
    // Le device est détecté et affiché dans lispe_lispetorch.cxx
    // Les caches seront initialisés lors du premier forward
    // Note: causal_mask_cache_ supprimé - géré par scaled_dot_product_attention
    // Note: KVCache géré par HFInferenceContext, plus par la classe
    // Note: Cache RoPE maintenant géré par KVCache dans chaque contexte
}

HuggingFaceLoader::~HuggingFaceLoader() {
    // Détacher tous les tenseurs de leur stockage pour éviter les crashes MPS/CUDA
    // lors de la destruction globale (quand le contexte GPU n'est plus valide)
    for (auto& pair : weights_) {
        // Reset explicite du tenseur pour libérer la référence au stockage GPU
        // sans déclencher de synchronisation/libération GPU
        pair.second = torch::Tensor();
    }
    weights_.clear();

    // Nettoyer les caches
    // rope_cos_cache_ et rope_sin_cache_ supprimés - maintenant gérés par KVCache
    // causal_mask_cache n'est plus utilisé (géré par scaled_dot_product_attention)
    // kv_cache_ supprimé - géré par HFInferenceContext
}

// Méthodes requises par le header

bool HuggingFaceLoader::loadWeights(const std::string& weights_dir) {
    //affichage << "Loading model weights..." << std::endl;

    // Charger la configuration d'abord
    //loadConfig(weights_dir);

    // Chercher les fichiers .bin ou .safetensors
    std::vector<std::string> weight_files;
    
    for (const auto& entry : fs::directory_iterator(weights_dir)) {
        if (entry.is_regular_file()) {
            std::string filename = entry.path().filename().string();
            // Exclure les fichiers JSON (comme model.safetensors.index.json)
            if (filename.find(".json") != std::string::npos) {
                continue;
            }
            if (filename.find(".bin") != std::string::npos ||
                filename.find(".safetensors") != std::string::npos) {
                weight_files.push_back(entry.path().string());
            }
        }
    }
    
    if (weight_files.empty()) {
        //affichage << "No weight files found (.bin or .safetensors)" << std::endl;
        return false;
    }
    
    // Charger les fichiers de poids
    for (const auto& file : weight_files) {
        //affichage << "  Loading: " << fs::path(file).filename() << std::endl;
        
        bool loaded = false;
        if (file.find(".safetensors") != std::string::npos) {
            // Utiliser notre parser safetensors
            loaded = loadSafetensorsFile(file);
        } else if (file.find(".bin") != std::string::npos) {
            // Charger les fichiers .bin avec torch::jit::load
            try {
                //affichage << "  - Loading .bin file with torch::load" << std::endl;
                
                // Lire le fichier pickle 
                std::ifstream stream(file, std::ios::binary);
                if (!stream.is_open()) {
                    //affichage << "  ✗ Cannot open .bin file: " << file << std::endl;
                    continue;
                }
                
                // Pour les fichiers .bin PyTorch, on peut essayer torch::load
                torch::serialize::InputArchive archive;
                archive.load_from(file);
                
                // Les clés sont typiquement les noms des tenseurs
                // On devrait itérer sur toutes les clés disponibles
                //affichage << "  ✓ Loaded .bin file structure" << std::endl;
                loaded = true;
                
            } catch (const std::exception& e) {
                //affichage << "  ✗ Failed to load .bin file: " << e.what() << std::endl;
                loaded = false;
            }
        }
        
        if (!loaded) {
            //affichage << "  ⚠ Skipped file: " << fs::path(file).filename() << std::endl;
        }
    }

    // ...après avoir chargé tous les poids...
    //cout  << "Liste des clés de tenseurs chargés:" << std::endl;
    //for (const auto& pair : weights_) {
    //    cout << "  " << pair.first << std::endl;
    //}
    //affichage << "✓ Weights loaded successfully" << std::endl;

    // Organiser les poids pour accès optimisé
    organizeWeights();

    model_loaded_ = true;
    return true;
}

void HuggingFaceLoader::organizeWeights() {
    // Organiser les poids pour accès O(1) direct (éviter hash lookups constants)
    size_t num_layers = model_info_.num_hidden_layers;
    layer_weights_.resize(num_layers);

    // Poids globaux (embeddings et final norm)
    auto embed_it = weights_.find("model.embed_tokens.weight");
    if (embed_it != weights_.end()) {
        embed_tokens_weight_ = embed_it->second;
    }

    auto final_norm_it = weights_.find("model.norm.weight");
    if (final_norm_it != weights_.end()) {
        final_norm_weight_ = final_norm_it->second;
    }

    // LM head (ou tied weights)
    auto lm_head_it = weights_.find("lm_head.weight");
    if (lm_head_it != weights_.end()) {
        lm_head_weight_ = lm_head_it->second;
    } else {
        // Si tied weights, utiliser les embeddings
        lm_head_weight_ = embed_tokens_weight_;
    }

    // Organiser les poids par couche
    for (size_t layer_idx = 0; layer_idx < num_layers; ++layer_idx) {
        std::string layer_prefix = "model.layers." + std::to_string(layer_idx) + ".";

        LayerWeights& layer = layer_weights_[layer_idx];

        // Normalisation pré-attention
        auto input_norm_it = weights_.find(layer_prefix + "input_layernorm.weight");
        if (input_norm_it != weights_.end()) {
            layer.input_layernorm_weight = input_norm_it->second;
        }

        // Projections d'attention
        auto q_it = weights_.find(layer_prefix + "self_attn.q_proj.weight");
        auto k_it = weights_.find(layer_prefix + "self_attn.k_proj.weight");
        auto v_it = weights_.find(layer_prefix + "self_attn.v_proj.weight");
        auto o_it = weights_.find(layer_prefix + "self_attn.o_proj.weight");

        if (q_it != weights_.end()) layer.q_proj_weight = q_it->second;
        if (k_it != weights_.end()) layer.k_proj_weight = k_it->second;
        if (v_it != weights_.end()) layer.v_proj_weight = v_it->second;
        if (o_it != weights_.end()) layer.o_proj_weight = o_it->second;

        // Normalisations Q/K (Qwen3 uniquement - optionnels pour compatibilité)
        auto q_norm_it = weights_.find(layer_prefix + "self_attn.q_norm.weight");
        auto k_norm_it = weights_.find(layer_prefix + "self_attn.k_norm.weight");
        if (q_norm_it != weights_.end()) layer.q_norm_weight = q_norm_it->second;
        if (k_norm_it != weights_.end()) layer.k_norm_weight = k_norm_it->second;

        // Normalisation pré-MLP
        auto post_norm_it = weights_.find(layer_prefix + "post_attention_layernorm.weight");
        if (post_norm_it != weights_.end()) {
            layer.post_attention_layernorm_weight = post_norm_it->second;
        }

        // Projections MLP
        auto gate_it = weights_.find(layer_prefix + "mlp.gate_proj.weight");
        auto up_it = weights_.find(layer_prefix + "mlp.up_proj.weight");
        auto down_it = weights_.find(layer_prefix + "mlp.down_proj.weight");

        if (gate_it != weights_.end()) layer.gate_proj_weight = gate_it->second;
        if (up_it != weights_.end()) layer.up_proj_weight = up_it->second;
        if (down_it != weights_.end()) layer.down_proj_weight = down_it->second;
    }

    // Pré-calculer les caches de poids fusionnés pour éviter les race conditions
    if (fused) {
        qkv_fused_weights_.resize(num_layers);
        gate_up_fused_weights_.resize(num_layers);
        
        for (size_t layer_idx = 0; layer_idx < num_layers; ++layer_idx) {
            const LayerWeights& lw = layer_weights_[layer_idx];
            
            // Pré-calculer QKV fusionné si tous les poids sont disponibles
            if (lw.q_proj_weight.defined() && lw.k_proj_weight.defined() && lw.v_proj_weight.defined()) {
                qkv_fused_weights_[layer_idx] = torch::cat({
                    lw.q_proj_weight, lw.k_proj_weight, lw.v_proj_weight
                }, 0);
            }
            
            // Pré-calculer gate+up fusionné si les poids sont disponibles
            if (lw.gate_proj_weight.defined() && lw.up_proj_weight.defined()) {
                gate_up_fused_weights_[layer_idx] = torch::cat({
                    lw.gate_proj_weight, lw.up_proj_weight
                }, 0);
            }
        }
    }
        
    fused = false; // Marquer les caches comme initialisés

    // Libérer weights_ après organisation pour économiser mémoire
    // Les poids sont maintenant dans les structures organisées (layer_weights_, etc.)
    // Si on a besoin de weights_ plus tard (LoRA, getWeight), on pourra le reconstruire
    memory_usage += getMemoryUsage();
    weights_.clear();
}

torch::Tensor HuggingFaceLoader::forward(const torch::Tensor& input_ids, HFInferenceContext& ctx) {
    if (!model_loaded_) {
        throw new Error("Model not loaded");
    }

    //affichage << "Forward pass - Input shape: " << input_ids.sizes() << std::endl;

    // Déplacer l'input vers le device approprié (si nécessaire)
    auto input_ids_device = input_ids.to(device_);

    auto batch_size = input_ids_device.size(0);
    auto seq_len = input_ids_device.size(1);
    
    // Initialiser le cache du contexte si nécessaire
    if (ctx.isUsingCache()) {
        ctx.resizeCache(model_info_.num_hidden_layers);
    }
    
    // 1. Embeddings - Utiliser les poids organisés (accès direct O(1))
    //affichage << "  - Applying embeddings..." << std::endl;

    if (!embed_tokens_weight_.defined()) {
        throw new Error("Embedding weights not loaded");
    }

    // Embedding lookup (les embeddings sont déjà sur le device)
    auto hidden_states = torch::embedding(embed_tokens_weight_, input_ids_device);
    //affichage << "  - Embeddings shape: " << hidden_states.sizes() << std::endl;
    
    // 2. Passer par les couches Transformer
    // Utiliser le nombre de couches depuis config.json
    int num_layers = model_info_.num_hidden_layers;

    // Les caches de poids fusionnés sont maintenant pré-calculés dans organizeWeights()
    // Plus besoin d'initialisation ici - forward() est maintenant réentrant

    //affichage << "  - Processing " << num_layers << " transformer layers" << std::endl;
    
    // Passer par chaque couche
    {
        torch::Tensor normed;
        torch::Tensor attn_output;
        for (int layer = 0; layer < num_layers; layer++) {
            //affichage << "  - Processing layer " << layer << std::endl;

            const LayerWeights& lw = layer_weights_[layer];

            // Architecture Llama avec pré-normalisation (Pre-LN):
            // 1. input_layernorm AVANT attention - Accès direct O(1) aux poids
            normed = applyRMSNorm(hidden_states, lw.input_layernorm_weight, model_info_.rms_norm_eps);
            attn_output = applyAttention(normed, layer, ctx);  // Passer le contexte
            hidden_states = hidden_states + attn_output; // Residual connection

            // 2. post_attention_layernorm AVANT MLP - Accès direct O(1) aux poids
            normed = applyRMSNorm(hidden_states, lw.post_attention_layernorm_weight, model_info_.rms_norm_eps);
            attn_output = applyMLP(normed, layer);
            hidden_states = hidden_states + attn_output; // Residual connection
        }
    }
    
    // 3. Final layer norm - Utiliser les poids organisés (accès direct O(1))
    if (!final_norm_weight_.defined()) {
        throw new Error("Final norm weights not loaded");
    }
    hidden_states = applyRMSNorm(hidden_states, final_norm_weight_, model_info_.rms_norm_eps);

    // 4. Output projection (LM head) - Utiliser les poids organisés (accès direct O(1))
    if (!lm_head_weight_.defined()) {
        throw new Error("LM head weights not loaded");
    }

    torch::Tensor logits = torch::linear(hidden_states, lm_head_weight_);

    //affichage << "✓ Forward pass completed - Output shape: " << logits.sizes() << std::endl;
    return logits;
}

torch::Tensor HuggingFaceLoader::applyLayerNorm(const torch::Tensor& input, const std::string& layer_name) {
    // Chercher les poids de layer norm
    auto weight_it = weights_.find(layer_name + ".weight");
    
    if (weight_it == weights_.end()) {
        //affichage << "  ✗ LayerNorm weights not found: " << layer_name << ".weight" << std::endl;
        return input; // Fallback: retourner l'input sans modification
    }
    
    auto& weight = weight_it->second;

    // Chercher le bias (optionnel pour RMSNorm)
    auto bias_it = weights_.find(layer_name + ".bias");

    if (bias_it != weights_.end()) {
        // LayerNorm standard avec bias
        auto& bias = bias_it->second;
        //affichage << "  - Applying LayerNorm: " << layer_name << std::endl;
        return torch::layer_norm(input, {weight.size(-1)}, weight, bias, model_info_.rms_norm_eps);
    } else {
        // RMSNorm (pas de bias)
        //affichage << "  - Applying RMSNorm: " << layer_name << std::endl;
        return applyRMSNorm(input, weight, model_info_.rms_norm_eps);
    }
}

torch::Tensor HuggingFaceLoader::applyRMSNorm(const torch::Tensor& input, const torch::Tensor& weight, double eps) {
    // RMSNorm: x * weight / sqrt(mean(x^2) + eps)
    // Calcul direct en bfloat16 pour éviter conversions coûteuses
    // Sur MPS/Metal, bfloat16 est bien supporté
    auto variance = input.pow(2).mean(-1, true);
    auto normalized = input * torch::rsqrt(variance + eps);
    return normalized * weight;
}

torch::Tensor HuggingFaceLoader::applyAttention(const torch::Tensor& input, int layer_idx, HFInferenceContext& ctx) {
    // Self-attention avec support pour Grouped Query Attention (GQA)
    auto batch_size = input.size(0);
    auto seq_len = input.size(1);
    auto hidden_size = input.size(2);

    // Accès direct O(1) aux poids de la couche (évite hash lookups)
    const LayerWeights& lw = layer_weights_[layer_idx];

    if (!lw.q_proj_weight.defined() || !lw.k_proj_weight.defined() ||
        !lw.v_proj_weight.defined() || !lw.o_proj_weight.defined()) {

        //affichage << "  ⚠ Attention weights incomplete for layer " << layer_idx << ", using residual connection" << std::endl;
        return torch::zeros_like(input); // Connexion résiduelle simple
    }

    //affichage << "  - Applying attention: layer " << layer_idx << std::endl;

    // Utiliser les dimensions depuis config.json
    int64_t num_heads = model_info_.num_attention_heads;      // 32 pour Llama 3.1-8B
    int64_t num_kv_heads = model_info_.num_key_value_heads;   // 8 pour Llama 3.1-8B
    int64_t head_dim = hidden_size / num_heads;                // 4096 / 32 = 128

    // Utiliser le cache du contexte
    KVCache& kv_cache = ctx.getKVCache();
    bool use_kv_cache = ctx.isUsingCache();

    // Déterminer si on utilise le cache pour CETTE couche
    bool cache_exists_for_layer = use_kv_cache &&
                                   kv_cache.current_seq_len > 0 &&
                                   layer_idx < kv_cache.k_cache.size() &&
                                   kv_cache.k_cache[layer_idx].defined() &&
                                   kv_cache.k_cache[layer_idx].numel() > 0 &&
                                   kv_cache.v_cache[layer_idx].defined() &&
                                   kv_cache.v_cache[layer_idx].numel() > 0;
    
    // Logique simplifiée: si on a un cache, on traite toujours tout l'input
    // mais on concatène avec le cache existant
    torch::Tensor input_to_process = input; 

    // Projections linéaires Q, K, V fusionnées pour performance
    // Au lieu de 3 matmuls séparés, on fait 1 seule matmul puis on split
    torch::Tensor q, k, v;
    {
        // Utiliser les poids fusionnés Q+K+V pré-calculés (accès direct O(1))
        torch::Tensor& qkv_weight = qkv_fused_weights_[layer_idx];

        // Une seule matmul pour Q+K+V
        auto qkv = torch::linear(input_to_process, qkv_weight);
        

        // Séparer en Q, K, V
        // q_weight: [hidden_size, hidden_size] pour 32 têtes
        // k_weight: [num_kv_heads * head_dim, hidden_size] pour 8 têtes
        // v_weight: [num_kv_heads * head_dim, hidden_size] pour 8 têtes
        int64_t q_size = num_heads * head_dim;  // 32 * 128 = 4096
        int64_t kv_size = num_kv_heads * head_dim;  // 8 * 128 = 1024

        auto qkv_chunks = qkv.split({q_size, kv_size, kv_size}, -1);
        q = qkv_chunks[0];
        k = qkv_chunks[1];
        v = qkv_chunks[2];
        

        // ========== APPLIQUER NORMALISATIONS Q/K (Qwen3 uniquement) ==========
        // Appliquer AVANT le reshape pour éviter les problèmes de dimensions avec MPS
        bool check_q = lw.q_norm_weight.defined() && q.numel() > 0 &&
                q.size(0) > 0 && q.size(1) > 0 && q.size(2) == num_heads * head_dim;
        bool check_k = lw.k_norm_weight.defined() && k.numel() > 0 &&
                k.size(0) > 0 && k.size(1) > 0 && k.size(2) == num_kv_heads * head_dim;

        if (check_q) {
            // Pour Qwen3, q_norm s'applique sur chaque tête séparément (dimension head_dim = 128)
            auto q_batch = q.size(0);
            auto q_seq = q.size(1);
            auto q_reshaped = q.view({q_batch, q_seq, num_heads, -1});  // [batch, seq, num_heads, head_dim]
            auto variance = q_reshaped.pow(2).mean(-1, true);
            auto rms_factor = torch::rsqrt(variance + model_info_.rms_norm_eps);
            auto normalized = q_reshaped * rms_factor;
            q_reshaped = normalized * lw.q_norm_weight.unsqueeze(0).unsqueeze(0).unsqueeze(0);
            q = q_reshaped.view({q_batch, q_seq, -1});  // Retour à [batch, seq, hidden]
        }
                
        if (check_k) {
            // Pour Qwen3, k_norm s'applique sur chaque tête KV séparément (dimension head_dim = 128)
            auto k_batch = k.size(0);
            auto k_seq = k.size(1);
            auto k_reshaped = k.view({k_batch, k_seq, num_kv_heads, -1});  // [batch, seq, num_kv_heads, head_dim]
            
            // Appliquer RMSNorm sur la dernière dimension (head_dim)
            auto variance = k_reshaped.pow(2).mean(-1, true);
            auto normalized = k_reshaped * torch::rsqrt(variance + model_info_.rms_norm_eps);
            k_reshaped = normalized * lw.k_norm_weight.unsqueeze(0).unsqueeze(0).unsqueeze(0);
            
            k = k_reshaped.view({k_batch, k_seq, -1});  // Retour à [batch, seq, kv_hidden]
        }
    }


    // Traitement de Q et K,V avec gestion du cache
    // La longueur de séquence à traiter est basée sur input_to_process
    int64_t q_seq_len = input_to_process.size(1);  // 1 si using_cache, sinon seq_len

    // Debug pour layer 1
    int64_t new_tokens = q_seq_len;  // Nombre de nouveaux tokens à ajouter au cache

    // Vérification de sécurité: q_seq_len doit être > 0
    if (q_seq_len <= 0) {
        // Fallback: utiliser l'input original si input_to_process est vide
        input_to_process = input;
        q_seq_len = input.size(1);
        new_tokens = q_seq_len;
        
        // Si toujours problématique, retourner un tenseur valide
        if (q_seq_len <= 0) {
            return torch::zeros({batch_size, 1, hidden_size}, torch::TensorOptions().device(device_).dtype(input.scalar_type()));
        }
    }

    // Reshaper Q: [batch, q_seq_len, num_heads, head_dim]
    q = q.view({batch_size, q_seq_len, num_heads, head_dim});
    // Transpose pour attention: [batch, num_heads, q_seq_len, head_dim]
    q = q.transpose(1, 2);

    // Reshaper K, V pour le nouveau calcul
    k = k.view({batch_size, new_tokens, num_kv_heads, head_dim});
    v = v.view({batch_size, new_tokens, num_kv_heads, head_dim});
    // Transpose: [batch, num_kv_heads, new_tokens, head_dim]
    k = k.transpose(1, 2);
    v = v.transpose(1, 2);

    // ========== APPLIQUER ROPE (Rotary Position Embeddings) ==========
    // RoPE encode les positions dans Q et K via des rotations complexes

    // Utiliser le cache RoPE du contexte (plus de variables globales !)
    kv_cache.ensureRopeCache(seq_len, head_dim, model_info_.rope_theta, device_);

    // Fonction optimisée pour appliquer la rotation RoPE
    // Optimisations : éviter .sizes() qui force synchronisation, utiliser chunk au lieu de select
    auto apply_rope = [](torch::Tensor& x, torch::Tensor& cos_freq, torch::Tensor& sin_freq) {
        // x: [batch, num_heads, seq, head_dim]
        // cos_freq, sin_freq: [seq, head_dim/2] déjà au bon dtype

        // Utiliser chunk pour séparer en deux moitiés sans appeler .sizes()
        // chunk divise en 2 parties égales le long de la dernière dimension
        auto x_chunks = x.chunk(2, -1);  // 2 tenseurs de shape [batch, num_heads, seq, head_dim/2]
        auto x1 = x_chunks[0];
        auto x2 = x_chunks[1];

        // Broadcaster cos/sin : [1, 1, seq, head_dim/2]
        auto cos_broad = cos_freq.unsqueeze(0).unsqueeze(0);
        auto sin_broad = sin_freq.unsqueeze(0).unsqueeze(0);

        // Appliquer la rotation: [x1*cos - x2*sin, x1*sin + x2*cos]
        auto new_x1 = x1 * cos_broad - x2 * sin_broad;
        auto new_x2 = x1 * sin_broad + x2 * cos_broad;

        // Concaténer au lieu de stack+flatten (plus direct)
        return torch::cat({new_x1, new_x2}, -1);
    };

    // Calculer les positions RoPE correctes selon le cache
    int64_t start_pos = cache_exists_for_layer ? kv_cache.current_seq_len : 0;
    int64_t end_pos = start_pos + new_tokens;
    
    // S'assurer que le cache RoPE est assez grand pour end_pos
    if (end_pos > kv_cache.rope_cache_max_len) {
        kv_cache.ensureRopeCache(end_pos + 100, head_dim, model_info_.rope_theta, device_);  // +100 pour éviter de recréer souvent
    }

    // Convertir les slices RoPE au dtype des tenseurs Q/K (une seule fois)
    auto dtype = q.scalar_type();
    auto cos_values = kv_cache.rope_cos_cache.slice(0, start_pos, end_pos).to(dtype);  // [new_tokens, head_dim/2]
    auto sin_values = kv_cache.rope_sin_cache.slice(0, start_pos, end_pos).to(dtype);  // [new_tokens, head_dim/2]

    // Vérifier que les dimensions de RoPE correspondent à ce qu'on attend
    if (cos_values.size(0) != new_tokens || sin_values.size(0) != new_tokens) {
        // Problème de synchronisation - recalculer les positions
        start_pos = 0;
        end_pos = new_tokens;
        cos_values = kv_cache.rope_cos_cache.slice(0, start_pos, end_pos).to(dtype);
        sin_values = kv_cache.rope_sin_cache.slice(0, start_pos, end_pos).to(dtype);
    }

    // Appliquer RoPE à Q et K (réutiliser les mêmes cos/sin)
    q = apply_rope(q, cos_values, sin_values);
    k = apply_rope(k, cos_values, sin_values);

    // Gérer le cache KV de manière optimisée
    torch::Tensor k_full, v_full;

    if (use_kv_cache) {
        if (cache_exists_for_layer) {
            // Cache existe pour cette couche: concaténer l'ancien cache avec les nouveaux k/v
            auto k_cached = kv_cache.k_cache[layer_idx];
            auto v_cached = kv_cache.v_cache[layer_idx];

            // Vérifications de sécurité strictes pour éviter les crashes MPS
            if (k_cached.defined() && v_cached.defined() && 
                k_cached.numel() > 0 && v_cached.numel() > 0 &&
                k_cached.size(0) == k.size(0) &&     // batch_size
                k_cached.size(1) == k.size(1) &&     // num_kv_heads 
                k_cached.size(3) == k.size(3) &&     // head_dim
                v_cached.size(0) == v.size(0) &&     // batch_size
                v_cached.size(1) == v.size(1) &&     // num_kv_heads
                v_cached.size(3) == v.size(3)) {     // head_dim
                
                // Concaténer sur la dimension de séquence (dim 2)
                k_full = torch::cat({k_cached, k}, 2);
                v_full = torch::cat({v_cached, v}, 2);
            } else {
                // Cache corrompu ou incompatible - redémarrer proprement
                //affichage << "⚠ KV cache reset for layer " << layer_idx << " (incompatible dimensions)" << std::endl;
                k_full = k;
                v_full = v;
                // Réinitialiser toute la séquence du cache pour cette couche
                if (layer_idx == 0) {
                    kv_cache.current_seq_len = 0;
                }
            }
        } else {
            // Premier passage pour cette couche: utiliser directement k et v
            k_full = k;
            v_full = v;
        }

        // Stocker le résultat dans le cache de manière atomique
        if (k_full.defined() && v_full.defined() && k_full.numel() > 0 && v_full.numel() > 0) {
            kv_cache.k_cache[layer_idx] = k_full.detach();
            kv_cache.v_cache[layer_idx] = v_full.detach();
        }

        // Mettre à jour seq_len de manière cohérente (seulement pour la première couche)
        if (layer_idx == 0) {
            kv_cache.current_seq_len += new_tokens;
            // Appliquer la fenêtre glissante après mise à jour
            kv_cache.applySlidingWindow();
        }
    } else {
        // Pas de cache: utiliser directement k et v
        k_full = k;
        v_full = v;
    }

    // Répéter K et V pour GQA (chaque tête KV est utilisée par num_heads/num_kv_heads têtes Q)
    int64_t num_repeats = num_heads / num_kv_heads;  // 32/8 = 4
    if (num_repeats > 1) {
        // Optimisation : utiliser unsqueeze + expand + flatten au lieu de repeat_interleave
        // repeat_interleave copie les données, expand crée juste une vue
        // k_full, v_full: [batch, num_kv_heads, seq, head_dim]
        // IMPORTANT: Utiliser -1 pour inférer les dimensions au lieu de .sizes() qui synchronise

        // Approche optimisée avec reshape dynamique
        k_full = k_full.unsqueeze(2)  // [batch, num_kv_heads, 1, seq, head_dim]
                      .expand({batch_size, num_kv_heads, num_repeats, -1, head_dim})  // [batch, num_kv_heads, num_repeats, seq, head_dim]
                      .reshape({batch_size, num_heads, -1, head_dim});  // [batch, num_heads, seq, head_dim]

        v_full = v_full.unsqueeze(2)
                      .expand({batch_size, num_kv_heads, num_repeats, -1, head_dim})
                      .reshape({batch_size, num_heads, -1, head_dim});
    }

    // Utiliser l'API optimisée scaled_dot_product_attention
    // Sur MPS, cette fonction fusionne les kernels (Q@K^T, scale, mask, softmax, @V)
    // en une seule opération optimisée, réduisant le nombre de kernel launches
    torch::Tensor attn_output;

    // Vérifications de sécurité pour éviter les crashes MPS avec tenseurs vides
    if (q.numel() == 0 || k_full.numel() == 0 || v_full.numel() == 0) {
        // Fallback: créer un tenseur de sortie avec les bonnes dimensions
        auto q_batch = q.size(0);
        auto q_seq = q.size(2);  // dimension seq après transpose
        auto q_hidden = num_heads * head_dim;
        return torch::zeros({q_batch, q_seq, q_hidden}, torch::TensorOptions().device(device_).dtype(q.scalar_type()));
    }

    // Vérifier que les dimensions sont cohérentes
    if (q.dim() != 4 || k_full.dim() != 4 || v_full.dim() != 4) {
        throw new Error("Invalid tensor dimensions for attention");
    }

    //lock.locking(threaded);
    if (cache_exists_for_layer) {
        // Avec KV-Cache: le nouveau token peut attendre à toutes les positions passées
        // Pas de masque causal nécessaire car q_seq_len=1 et on veut tout voir
        attn_output = torch::scaled_dot_product_attention(
            q, k_full, v_full,
            /*attn_mask=*/c10::nullopt,          // Pas de masque
            /*dropout_p=*/0.0,                   // Pas de dropout en inference
            /*is_causal=*/false,                 // Pas de masque causal
            /*scale=*/c10::nullopt               // Utiliser scale par défaut (1/sqrt(d))
        );
    } else {
        // Sans cache: appliquer le masque causal pour la génération autoregressive
        // is_causal=true applique automatiquement le masque triangulaire
        attn_output = torch::scaled_dot_product_attention(
            q, k_full, v_full,
            /*attn_mask=*/c10::nullopt,          // Pas de masque explicite
            /*dropout_p=*/0.0,                   // Pas de dropout en inference
            /*is_causal=*/true,                  // Appliquer masque causal automatiquement
            /*scale=*/c10::nullopt               // Utiliser scale par défaut (1/sqrt(d))
        );
    }
    //lock.unlocking(threaded);
    
    // attn_output: [batch, num_heads, q_seq_len, head_dim]

    // Recombiner les têtes: [batch, q_seq_len, num_heads, head_dim]
    attn_output = attn_output.transpose(1, 2);
    attn_output = attn_output.contiguous().view({batch_size, q_seq_len, num_heads * head_dim});

    // Projection de sortie (accès direct O(1) aux poids)
    auto output = torch::linear(attn_output, lw.o_proj_weight);


    return output;  // Shape: [batch, q_seq_len, hidden] où q_seq_len = 1 si using_cache, sinon seq_len
}

torch::Tensor HuggingFaceLoader::applyMLP(const torch::Tensor& input, int layer_idx) {
    // MLP: typiquement gate_proj, up_proj, down_proj pour Llama
    // Accès direct O(1) aux poids de la couche (évite hash lookups)
    const LayerWeights& lw = layer_weights_[layer_idx];

    if (!lw.gate_proj_weight.defined() || !lw.up_proj_weight.defined() || !lw.down_proj_weight.defined()) {
        //affichage << "  ⚠ MLP weights incomplete for layer " << layer_idx << ", using residual connection" << std::endl;
        return torch::zeros_like(input);
    }

    //affichage << "  - Applying SwiGLU MLP: layer " << layer_idx << std::endl;

    // SwiGLU: gate_proj et up_proj en parallèle, puis down_proj
    // Formule correcte: SwiGLU(x) = silu(gate) * up (PAS gate * silu(up) !)

    // Fusion de gate et up en une seule matmul pour performance
    // Utiliser les poids fusionnés gate+up pré-calculés (accès direct O(1))

    // Une seule matmul pour gate+up
    auto gated = torch::linear(input, gate_up_fused_weights_[layer_idx]);

    // Séparer gate et up (même taille : intermediate_size)
    int64_t intermediate_size = lw.gate_proj_weight.size(0);
    auto gate_up_chunks = gated.split(intermediate_size, -1);
    auto& gate = gate_up_chunks[0];
    auto& up = gate_up_chunks[1];

    // SwiGLU activation: silu(gate) * up
    //auto silu_gate = torch::silu(gate);
    gated = torch::silu(gate) * up;

    // Projection finale (accès direct O(1) aux poids)
    return torch::linear(gated, lw.down_proj_weight);
}

// Optimized versions that write directly to output tensor
void HuggingFaceLoader::forward(const torch::Tensor& input_ids, torch::Tensor& output, HFInferenceContext& ctx) {
    // Call existing forward and move result directly to output
    output = forward(input_ids, ctx);
}

// Version optimisée avec output pré-alloué et contexte temporaire
void HuggingFaceLoader::forward(const torch::Tensor& input_ids, torch::Tensor& output) {
    output = forward(input_ids, ctx_basic);
}

// Get embeddings for token IDs
torch::Tensor HuggingFaceLoader::getEmbeddings(const torch::Tensor& token_ids) {
    if (!model_loaded_) {
        throw new Error("Model not loaded");
    }

    if (!embed_tokens_weight_.defined()) {
        throw new Error("Embedding weights not loaded");
    }

    // Déplacer les token_ids vers le device approprié si nécessaire
    auto token_ids_device = token_ids.to(device_);

    // Utiliser torch::embedding pour extraire les embeddings
    // Forme de sortie: [batch_size, seq_len, hidden_size]
    auto embeddings = torch::embedding(embed_tokens_weight_, token_ids_device);

    return embeddings;
}

// Méthodes requises par le header
bool HuggingFaceLoader::loadConfig(const std::string& config_file) {
    //affichage << "Loading the model config from: " << config_file << std::endl;

    //std::string config_file = config_path + "/config.json";

    // Lire le fichier JSON
    std::ifstream file(config_file);
    if (!file.is_open()) {
        std::stringstream str;        
        str << "  ✗ Cannot open config file: " << config_file;
        throw new Error(str.str());
    }

    std::string json_content((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
    file.close();

    // Parser le JSON avec LispEJsonCompiler
    LispEJsonCompiler json_parser;
    Element* json_data = nullptr;

    json_data = json_parser.compileraw(json_content);

    if (json_data == NULL)
        throw new Error("Error: JSON parsing failed");

     if (!json_data->isDictionary()) {
        json_data->release();
        throw new Error("✗ JSON is not a dictionary");
    }

    Dictionary* config_dict = (Dictionary*)json_data;

    // Extraire les valeurs de configuration
    Element* rms_norm_eps_elem = config_dict->on_index(U"rms_norm_eps");
    if (rms_norm_eps_elem && rms_norm_eps_elem->isNumber()) {
        model_info_.rms_norm_eps = rms_norm_eps_elem->asNumber();
        //affichage << "  - rms_norm_eps: " << model_info_.rms_norm_eps << std::endl;
    }

    Element* vocab_size_elem = config_dict->on_index(U"vocab_size");
    if (vocab_size_elem && vocab_size_elem->isNumber()) {
        model_info_.vocab_size = vocab_size_elem->asInteger();
    }

    Element* hidden_size_elem = config_dict->on_index(U"hidden_size");
    if (hidden_size_elem && hidden_size_elem->isNumber()) {
        model_info_.hidden_size = hidden_size_elem->asInteger();
    }

    Element* num_layers_elem = config_dict->on_index(U"num_hidden_layers");
    if (num_layers_elem && num_layers_elem->isNumber()) {
        model_info_.num_hidden_layers = num_layers_elem->asInteger();
    }

    Element* rope_theta_elem = config_dict->on_index(U"rope_theta");
    if (rope_theta_elem && rope_theta_elem->isNumber()) {
        model_info_.rope_theta = rope_theta_elem->asNumber();
    }

    Element* num_attention_heads_elem = config_dict->on_index(U"num_attention_heads");
    if (num_attention_heads_elem && num_attention_heads_elem->isNumber()) {
        model_info_.num_attention_heads = num_attention_heads_elem->asInteger();
    }

    Element* num_key_value_heads_elem = config_dict->on_index(U"num_key_value_heads");
    if (num_key_value_heads_elem && num_key_value_heads_elem->isNumber()) {
        model_info_.num_key_value_heads = num_key_value_heads_elem->asInteger();
    }

    Element* intermediate_size_elem = config_dict->on_index(U"intermediate_size");
    if (intermediate_size_elem && intermediate_size_elem->isNumber()) {
        model_info_.intermediate_size = intermediate_size_elem->asInteger();
    }

    Element* max_position_embeddings_elem = config_dict->on_index(U"max_position_embeddings");
    if (max_position_embeddings_elem && max_position_embeddings_elem->isNumber()) {
        model_info_.max_position_embeddings = max_position_embeddings_elem->asInteger();
    }

    Element* tie_word_embeddings_elem = config_dict->on_index(U"tie_word_embeddings");
    if (tie_word_embeddings_elem) {
        // Peut être un booléen ou un nombre
        if (tie_word_embeddings_elem->isNumber()) {
            model_info_.tie_word_embeddings = tie_word_embeddings_elem->asInteger() != 0;
        } else {
            model_info_.tie_word_embeddings = tie_word_embeddings_elem->Boolean();
        } 
    }

    json_data->release();

    //affichage << "✓ Config loaded successfully" << std::endl;
    return true;
}

bool HuggingFaceLoader::validateModel() {
    return model_loaded_;
}

std::vector<std::string> HuggingFaceLoader::listWeights() const {
    std::vector<std::string> weight_names;
    for (const auto& pair : weights_) {
        weight_names.push_back(pair.first);
    }
    return weight_names;
}

torch::Tensor HuggingFaceLoader::getWeight(const std::string& name) const {
    auto it = weights_.find(name);
    if (it != weights_.end()) {
        return it->second;
    }
    return torch::Tensor();
}

const torch::Tensor& HuggingFaceLoader::getQWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].q_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getKWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].k_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getVWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].v_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getOWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].o_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getQKVFusedWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= qkv_fused_weights_.size()) {
        return empty_tensor;
    }
    return qkv_fused_weights_[layer_idx];
}

const torch::Tensor& HuggingFaceLoader::getGateWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].gate_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getUpWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].up_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getDownWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= layer_weights_.size()) {
        return empty_tensor;
    }
    return layer_weights_[layer_idx].down_proj_weight;
}

const torch::Tensor& HuggingFaceLoader::getGateUpFusedWeight(int layer_idx) const {
    static const torch::Tensor empty_tensor;
    if (layer_idx < 0 || layer_idx >= gate_up_fused_weights_.size()) {
        return empty_tensor;
    }
    return gate_up_fused_weights_[layer_idx];
}

double HuggingFaceLoader::getRMSNormEps() const {
    return model_info_.rms_norm_eps;
}


bool HuggingFaceLoader::loadSafetensorsFile(const std::string& file_path) {
    //affichage << "  - Parsing safetensors file: " << fs::path(file_path).filename() << std::endl;

    // Ouvrir le fichier
    std::ifstream file(file_path, std::ios::binary);
    if (!file.is_open()) {
        //affichage << "  ✗ Cannot open file: " << file_path << std::endl;
        return false;
    }

    // Lire la taille du header (8 premiers bytes en little-endian)
    uint64_t header_size = 0;
    file.read(reinterpret_cast<char*>(&header_size), 8);
    if (file.gcount() != 8) {
        //affichage << "  ✗ Cannot read header size" << std::endl;
        return false;
    }

    // Lire le header JSON
    std::string header_json(header_size, '\0');
    file.read(&header_json[0], header_size);
    if (file.gcount() != static_cast<std::streamsize>(header_size)) {
        //affichage << "  ✗ Cannot read header JSON" << std::endl;
        return false;
    }

    //affichage << "  - Header size: " << header_size << " bytes" << std::endl;
    if (weights_.empty()) {
        // Afficher un extrait du header (limité) pour debug seulement pour le premier fichier
        std::string preview = header_json.substr(0, std::min<size_t>(header_json.size(), 300));
        std::replace(preview.begin(), preview.end(), '\n', ' ');
        //affichage << "  - Header preview: " << preview << (header_json.size() > 300 ? "..." : "") << std::endl;
    }

    // Parser le JSON avec le parser LispE
    LispEJsonCompiler json_parser;
    Element* json_data = json_parser.compileraw(header_json);

    if (json_data == NULL || json_data->type != t_dictionary) {
        if (json_data) json_data->release();
        throw new Error("✗ Failed to parse JSON header");
    }

    Dictionary* json_dict = (Dictionary*)json_data;

    // Parcourir tous les tenseurs dans le dictionnaire
    for (const auto& pair : json_dict->dictionary) {
        std::u32string key_u32 = pair.first;
        std::string tensor_name;
        s_unicode_to_utf8(tensor_name, key_u32);

        // Ignorer les métadonnées spéciales
        if (tensor_name == "__metadata__") {
            continue;
        }

        Element* tensor_info = pair.second;
        if (tensor_info->type != t_dictionary) {
            continue;
        }

        Dictionary* info_dict = (Dictionary*)tensor_info;

        // Extraire dtype
        torch::ScalarType dtype = torch::kFloat32;
        Element* dtype_elem = info_dict->on_index(U"dtype");
        if (dtype_elem && dtype_elem->isString()) {
            std::string dtype_str = dtype_elem->toString(nullptr);
            if (dtype_str == "F32") dtype = torch::kFloat32;
            else if (dtype_str == "F16") dtype = torch::kFloat16;
            else if (dtype_str == "BF16") dtype = torch::kBFloat16;
            else if (dtype_str == "I32") dtype = torch::kInt32;
            else if (dtype_str == "I64") dtype = torch::kInt64;
        }

        // Extraire shape
        std::vector<int64_t> shape;
        Element* shape_elem = info_dict->on_index(U"shape");
        if (shape_elem && shape_elem->isList()) {
            for (long i = 0; i < shape_elem->size(); i++) {
                shape.push_back(shape_elem->index(i)->asInteger());
            }
        }

        // Extraire data_offsets
        uint64_t data_start = 0, data_end = 0;
        Element* offsets_elem = info_dict->on_index(U"data_offsets");
        if (offsets_elem && offsets_elem->isList() && offsets_elem->size() == 2) {
            data_start = offsets_elem->index(0)->asInteger();
            data_end = offsets_elem->index(1)->asInteger();
        }

        // Créer le tenseur et lire les données
        if (!shape.empty() && data_end > data_start) {
            size_t tensor_bytes = data_end - data_start;

            // Déterminer la taille attendue à partir du dtype
            size_t element_size = 0;
            switch (dtype) {
                case torch::kFloat32: element_size = 4; break;
                case torch::kFloat16: element_size = 2; break;
                case torch::kBFloat16: element_size = 2; break;
                case torch::kInt32: element_size = 4; break;
                case torch::kInt64: element_size = 8; break;
                default: element_size = 0; break;
            }
            size_t expected_bytes = element_size;
            for (auto d : shape) {
                // Protéger contre overflow potentiel
                if (d <= 0) { expected_bytes = 0; break; }
                if (expected_bytes > SIZE_MAX / (size_t)d) { expected_bytes = 0; break; }
                expected_bytes *= (size_t)d;
            }

            /*
            //affichage << "  - Loading tensor: " << tensor_name << " shape: [";
            for (size_t i = 0; i < shape.size(); ++i) {
                if (i) //affichage << ", ";
                //affichage << shape[i];
            }
            //affichage << "] dtype: " << (dtype == torch::kFloat32 ? "float32" :
                                       dtype == torch::kFloat16 ? "float16" :
                                       dtype == torch::kBFloat16 ? "bfloat16" :
                                       dtype == torch::kInt32 ? "int32" :
                                       dtype == torch::kInt64 ? "int64" : "unknown")
                      << " bytes(file): " << tensor_bytes << " expected: " << expected_bytes << std::endl;
            */
            if (element_size == 0 || expected_bytes == 0) {
                //affichage << "  ✗ Unsupported dtype or invalid shape for: " << tensor_name << std::endl;
                continue;
            }
            if (expected_bytes != tensor_bytes) {
                // Certains fichiers peuvent être packés différemment, on affiche un warning
                //affichage << "  ⚠ Size mismatch for " << tensor_name << " (continuing)" << std::endl;
            }

            auto options = torch::TensorOptions().dtype(dtype);
            torch::Tensor tensor = torch::empty(shape, options);

            size_t current_pos = (size_t)file.tellg();
            file.seekg(8 + header_size + data_start);
            file.read(reinterpret_cast<char*>(tensor.data_ptr()), tensor_bytes);

            if (file.gcount() == static_cast<std::streamsize>(tensor_bytes)) {
                // Déplacer directement vers le device (MPS/CUDA/CPU)
                // Le destructeur vide évite les problèmes de cleanup
                weights_[tensor_name] = tensor.to(device_);
                // Pour économiser la verbosité, seules les grosses matrices sont confirmées explicitement
                if (tensor.numel() > 1024) {
                    //affichage << "  ✓ Loaded tensor: " << tensor_name << " numel=" << tensor.numel() << std::endl;
                }
            } else {
                //affichage << "  ✗ Failed to read tensor data for: " << tensor_name << std::endl;
            }
            file.seekg(current_pos);
        }
    }

    json_data->release();
    file.close();

    // Fallback si aucun tenseur n'a été chargé (problème de parsing interne)
    if (weights_.empty()) {
        throw new Error(U"  ⚠ No tensor loaded by the internal parser");
    }

    //affichage << "  ✓ Safetensors parsing completed (total tensors so far: " << weights_.size() << ")" << std::endl;
    return true;
}

size_t HuggingFaceLoader::getMemoryUsage() const {
    size_t total_memory = 0;
    
    // Calculer la mémoire utilisée par les tenseurs chargés
    for (const auto& pair : weights_) {
        const auto& tensor = pair.second;
        if (tensor.defined()) {
            total_memory += tensor.numel() * tensor.element_size();
        }
    }
    
    //affichage << "Total memory usage: " << (total_memory / 1024 / 1024) << " MB" << std::endl;
    return total_memory;
}

// ============================================================================
// IMPLÉMENTATION DE HuggingFaceLoaderLoRA
// Classe dérivée avec support complet LoRA
// ============================================================================

HuggingFaceLoaderLoRA::HuggingFaceLoaderLoRA(const HFLoadConfig& config)
    : HuggingFaceLoader(config),
      lora_rank_(16),
      lora_alpha_(32.0),
      lora_dropout_(0.0),
      lora_enabled_(false),
      lora_initialized_(false) {
        fused = false;
    //affichage << "HuggingFaceLoaderLoRA constructor" << std::endl;
}

HuggingFaceLoaderLoRA::~HuggingFaceLoaderLoRA() {
    // Nettoyer les adaptateurs LoRA
    lora_layers_.clear();
    //affichage << "HuggingFaceLoaderLoRA destructor" << std::endl;
}

void HuggingFaceLoaderLoRA::organizeWeights() {
    // Pour LoRA, on doit garder les matrices individuelles pour l'entraînement
    // On fait donc l'organisation de base MAIS on ne fusionne PAS les matrices
    
    // Organiser les poids pour accès O(1) direct (éviter hash lookups constants)
    size_t num_layers = model_info_.num_hidden_layers;
    layer_weights_.resize(num_layers);

    // Poids globaux (embeddings et final norm) - identique à la classe de base
    auto embed_it = weights_.find("model.embed_tokens.weight");
    if (embed_it != weights_.end()) {
        embed_tokens_weight_ = embed_it->second;
    }

    auto final_norm_it = weights_.find("model.norm.weight");
    if (final_norm_it != weights_.end()) {
        final_norm_weight_ = final_norm_it->second;
    }

    // LM head (ou tied weights)
    auto lm_head_it = weights_.find("lm_head.weight");
    if (lm_head_it != weights_.end()) {
        lm_head_weight_ = lm_head_it->second;
    } else {
        // Si tied weights, utiliser les embeddings
        lm_head_weight_ = embed_tokens_weight_;
    }

    // Organiser les poids par couche - identique à la classe de base
    for (size_t layer_idx = 0; layer_idx < num_layers; ++layer_idx) {
        std::string layer_prefix = "model.layers." + std::to_string(layer_idx) + ".";

        LayerWeights& layer = layer_weights_[layer_idx];

        // Normalisation pré-attention
        auto input_norm_it = weights_.find(layer_prefix + "input_layernorm.weight");
        if (input_norm_it != weights_.end()) {
            layer.input_layernorm_weight = input_norm_it->second;
        }

        // Projections d'attention
        auto q_it = weights_.find(layer_prefix + "self_attn.q_proj.weight");
        auto k_it = weights_.find(layer_prefix + "self_attn.k_proj.weight");
        auto v_it = weights_.find(layer_prefix + "self_attn.v_proj.weight");
        auto o_it = weights_.find(layer_prefix + "self_attn.o_proj.weight");

        if (q_it != weights_.end()) layer.q_proj_weight = q_it->second;
        if (k_it != weights_.end()) layer.k_proj_weight = k_it->second;
        if (v_it != weights_.end()) layer.v_proj_weight = v_it->second;
        if (o_it != weights_.end()) layer.o_proj_weight = o_it->second;

        // Normalisations Q/K (Qwen3 uniquement - optionnels pour compatibilité)
        auto q_norm_it = weights_.find(layer_prefix + "self_attn.q_norm.weight");
        auto k_norm_it = weights_.find(layer_prefix + "self_attn.k_norm.weight");
        if (q_norm_it != weights_.end()) layer.q_norm_weight = q_norm_it->second;
        if (k_norm_it != weights_.end()) layer.k_norm_weight = k_norm_it->second;

        // Normalisation pré-MLP
        auto post_norm_it = weights_.find(layer_prefix + "post_attention_layernorm.weight");
        if (post_norm_it != weights_.end()) {
            layer.post_attention_layernorm_weight = post_norm_it->second;
        }

        // Projections MLP
        auto gate_it = weights_.find(layer_prefix + "mlp.gate_proj.weight");
        auto up_it = weights_.find(layer_prefix + "mlp.up_proj.weight");
        auto down_it = weights_.find(layer_prefix + "mlp.down_proj.weight");

        if (gate_it != weights_.end()) layer.gate_proj_weight = gate_it->second;
        if (up_it != weights_.end()) layer.up_proj_weight = up_it->second;
        if (down_it != weights_.end()) layer.down_proj_weight = down_it->second;
    }

    // DIFFÉRENCE MAJEURE : Pour LoRA, on ne fait PAS la fusion des matrices
    // On garde fused = false et on ne crée pas qkv_fused_weights_ ni gate_up_fused_weights_
    // Cela permet aux méthodes applyAttention() et applyMLP() surchargées d'utiliser
    // les matrices individuelles avec les corrections LoRA
    
    //fused = false;  // Indiquer qu'aucune fusion n'a été faite
    
    // On garde aussi weights_ car LoRA en a besoin pour l'initialisation
    // et ne libère pas automatiquement la mémoire comme la classe de base
    memory_usage += getMemoryUsage();
    // NE PAS faire weights_.clear() pour LoRA !
    
    //affichage << "✓ Organisation LoRA complétée (matrices individuelles préservées)" << std::endl;
}

bool HuggingFaceLoaderLoRA::initializeLoRA(int rank,
                                           double alpha,
                                           const std::vector<std::string>& target_modules,
                                           torch::ScalarType dtype) {
    if (!model_loaded_) {
        //affichage << "Error: Model must be loaded before initializing LoRA" << std::endl;
        return false;
    }

    //affichage << "Initializing LoRA adapters..." << std::endl;
    //affichage << "  Rank: " << rank << std::endl;
    //affichage << "  Alpha: " << alpha << std::endl;
    //affichage << "  Dtype: " << dtype << std::endl;
    //affichage << "  Target modules: ";
    for (const auto& mod : target_modules) {
        //affichage << mod << " ";
    }
    //affichage << std::endl;

    lora_rank_ = rank;
    lora_alpha_ = alpha;
    target_modules_ = target_modules;

    // Initialiser les adaptateurs pour chaque couche
    int num_layers = model_info_.num_hidden_layers;
    int64_t hidden_size = model_info_.hidden_size;

    lora_layers_.resize(num_layers);

    for (int layer_idx = 0; layer_idx < num_layers; ++layer_idx) {
        for (const auto& module_name : target_modules_) {
            // Récupérer les dimensions réelles depuis les poids du modèle
            torch::Tensor weight;
            if (module_name == "q_proj") {
                weight = layer_weights_[layer_idx].q_proj_weight;
            } else if (module_name == "k_proj") {
                weight = layer_weights_[layer_idx].k_proj_weight;
            } else if (module_name == "v_proj") {
                weight = layer_weights_[layer_idx].v_proj_weight;
            } else if (module_name == "o_proj") {
                weight = layer_weights_[layer_idx].o_proj_weight;
            } else if (module_name == "gate_proj") {
                weight = layer_weights_[layer_idx].gate_proj_weight;
            } else if (module_name == "up_proj") {
                weight = layer_weights_[layer_idx].up_proj_weight;
            } else if (module_name == "down_proj") {
                weight = layer_weights_[layer_idx].down_proj_weight;
            } else {
                //affichage << "  Warning: Unknown module " << module_name << ", using hidden_size" << std::endl;
                weight = torch::zeros({hidden_size, hidden_size});
            }

            // Les poids PyTorch Linear sont stockés comme [out_features, in_features]
            int64_t out_features = weight.size(0);
            int64_t in_features = weight.size(1);

            // Créer une couche LoRA pour ce module avec les dimensions réelles et le dtype approprié
            auto lora_layer = std::make_shared<LoRALinear>(
                in_features,
                out_features,
                rank,
                alpha,
                dtype
            );

            // Initialiser sur le bon device
            lora_layer->to(device_);

            // Stocker
            lora_layers_[layer_idx][module_name] = lora_layer;
        }

        // Log progression
        if (layer_idx % 8 == 0 || layer_idx == num_layers - 1) {
            //affichage << "  Created LoRA adapters for layer " << layer_idx + 1 << "/" << num_layers << std::endl;
        }
    }

    // Calculer le nombre total de paramètres LoRA (en comptant les vrais paramètres)
    int64_t total_params = 0;
    for (int layer_idx = 0; layer_idx < num_layers; ++layer_idx) {
        for (const auto& module_name : target_modules_) {
            auto lora = lora_layers_[layer_idx][module_name];
            // Compter les paramètres de lora_A et lora_B
            for (const auto& param : lora->parameters()) {
                total_params += param.numel();
            }
        }
    }

    //affichage << "LoRA initialization complete:" << std::endl;
    //affichage << "  Total LoRA parameters: " << total_params / 1000000 << "M" << std::endl;

    lora_initialized_ = true;
    lora_enabled_ = true;

    return true;
}

std::shared_ptr<LoRALinear> HuggingFaceLoaderLoRA::getLoRALayer(int layer_idx,
                                                                 const std::string& module_name) {
    if (!lora_initialized_ || layer_idx < 0 || layer_idx >= (int)lora_layers_.size()) {
        return nullptr;
    }

    auto& layer_map = lora_layers_[layer_idx];
    auto it = layer_map.find(module_name);
    if (it != layer_map.end()) {
        return it->second;
    }

    return nullptr;
}

std::vector<torch::Tensor> HuggingFaceLoaderLoRA::getLoRAParameters() {
    std::vector<torch::Tensor> parameters;

    if (!lora_initialized_) {
        return parameters;
    }

    // Parcourir tous les adaptateurs et collecter les paramètres A et B
    for (auto& layer_map : lora_layers_) {
        for (auto& pair : layer_map) {
            auto& lora_layer = pair.second;
            if (lora_layer) {
                // Ajouter les poids de lora_A et lora_B
                parameters.push_back(lora_layer->lora_A->weight);
                parameters.push_back(lora_layer->lora_B->weight);
            }
        }
    }

    //affichage << "Collected " << parameters.size() << " LoRA parameters" << std::endl;
    return parameters;
}

bool HuggingFaceLoaderLoRA::saveLoRAAdapters(const std::string& path) {
    if (!lora_initialized_) {
        //affichage << "Error: LoRA not initialized" << std::endl;
        return false;
    }

    //affichage << "Saving LoRA adapters to: " << path << std::endl;

    try {
        // Créer un vecteur avec tous les tenseurs LoRA
        std::vector<torch::Tensor> tensors;
        std::vector<std::string> keys;

        for (size_t layer_idx = 0; layer_idx < lora_layers_.size(); ++layer_idx) {
            for (const auto& pair : lora_layers_[layer_idx]) {
                const std::string& module_name = pair.first;
                auto& lora_layer = pair.second;

                if (lora_layer) {
                    // Clés: layer.{idx}.{module}.lora_A.weight et lora_B.weight
                    std::string prefix = "layer." + std::to_string(layer_idx) + "." + module_name;

                    keys.push_back(prefix + ".lora_A.weight");
                    tensors.push_back(lora_layer->lora_A->weight.cpu());

                    keys.push_back(prefix + ".lora_B.weight");
                    tensors.push_back(lora_layer->lora_B->weight.cpu());
                }
            }
        }

        // Sauvegarder les tenseurs et les clés séparément
        torch::save(tensors, path);

        // Sauvegarder aussi les clés dans un fichier texte
        std::string keys_path = path + ".keys";
        std::ofstream keys_file(keys_path);
        for (const auto& key : keys) {
            keys_file << key << "\n";
        }
        keys_file.close();

        //affichage << "LoRA adapters saved successfully (" << tensors.size() << " tensors)" << std::endl;
        return true;
    }
    catch (const std::exception& e) {
        //affichage << "Error saving LoRA adapters: " << e.what() << std::endl;
        return false;
    }
}

bool HuggingFaceLoaderLoRA::loadLoRAAdapters(const std::string& path) {
    if (!lora_initialized_) {
        //affichage << "Error: LoRA must be initialized before loading adapters" << std::endl;
        return false;
    }

    //affichage << "Loading LoRA adapters from: " << path << std::endl;

    try {
        // Charger les tenseurs
        std::vector<torch::Tensor> tensors;
        torch::load(tensors, path);

        // Charger les clés depuis le fichier texte
        std::string keys_path = path + ".keys";
        std::ifstream keys_file(keys_path);
        std::vector<std::string> keys;
        std::string line;
        while (std::getline(keys_file, line)) {
            if (!line.empty()) {
                keys.push_back(line);
            }
        }
        keys_file.close();

        //affichage << "Loaded " << tensors.size() << " tensors with " << keys.size() << " keys" << std::endl;

        if (tensors.size() != keys.size()) {
            //affichage << "Error: Mismatch between tensors and keys" << std::endl;
            return false;
        }

        // Créer un map pour un accès facile
        std::unordered_map<std::string, torch::Tensor> state_dict;
        for (size_t i = 0; i < keys.size(); ++i) {
            state_dict[keys[i]] = tensors[i];
        }

        // Charger dans les couches LoRA
        for (size_t layer_idx = 0; layer_idx < lora_layers_.size(); ++layer_idx) {
            for (const auto& pair : lora_layers_[layer_idx]) {
                const std::string& module_name = pair.first;
                auto& lora_layer = pair.second;

                if (lora_layer) {
                    std::string prefix = "layer." + std::to_string(layer_idx) + "." + module_name;

                    // Charger lora_A.weight
                    std::string key_A = prefix + ".lora_A.weight";
                    auto it_A = state_dict.find(key_A);
                    if (it_A != state_dict.end()) {
                        lora_layer->lora_A->weight.data().copy_(it_A->second.to(device_));
                    }

                    // Charger lora_B.weight
                    std::string key_B = prefix + ".lora_B.weight";
                    auto it_B = state_dict.find(key_B);
                    if (it_B != state_dict.end()) {
                        lora_layer->lora_B->weight.data().copy_(it_B->second.to(device_));
                    }
                }
            }
        }

        //affichage << "LoRA adapters loaded successfully" << std::endl;
        return true;
    }
    catch (const std::exception& e) {
        //affichage << "Error loading LoRA adapters: " << e.what() << std::endl;
        return false;
    }
}

void HuggingFaceLoaderLoRA::mergeLoRAWeights() {
    if (!lora_initialized_) {
        return;
    }

    //affichage << "Merging LoRA weights into base model..." << std::endl;

    for (auto& layer_map : lora_layers_) {
        for (auto& pair : layer_map) {
            auto& lora_layer = pair.second;
            if (lora_layer) {
                lora_layer->merge_weights();
            }
        }
    }

    //affichage << "LoRA weights merged" << std::endl;
}

void HuggingFaceLoaderLoRA::unmergeLoRAWeights() {
    if (!lora_initialized_) {
        return;
    }

    //affichage << "Unmerging LoRA weights from base model..." << std::endl;

    for (auto& layer_map : lora_layers_) {
        for (auto& pair : layer_map) {
            auto& lora_layer = pair.second;
            if (lora_layer) {
                lora_layer->unmerge_weights();
            }
        }
    }

    //affichage << "LoRA weights unmerged" << std::endl;
}

torch::Tensor HuggingFaceLoaderLoRA::applyLoRACorrection(const torch::Tensor& input,
                                                         int layer_idx,
                                                         const std::string& module_name) {
    // Obtenir la couche LoRA correspondante
    auto lora_layer = getLoRALayer(layer_idx, module_name);

    if (!lora_layer) {
        // Pas de LoRA pour ce module, retourner un tenseur nul
        return torch::Tensor();
    }

    // Calculer la correction LoRA: (alpha/rank) * B(A(input))
    torch::Tensor lora_out = lora_layer->lora_A->forward(input);
    lora_out = lora_layer->lora_B->forward(lora_out);

    // Appliquer le scaling
    double scaling = lora_layer->alpha / lora_layer->rank;
    lora_out = scaling * lora_out;

    return lora_out;
}

torch::Tensor HuggingFaceLoaderLoRA::applyAttention(const torch::Tensor& input,
                                                     int layer_idx,
                                                     HFInferenceContext& ctx) {
    // Si LoRA n'est pas activé, utiliser l'implémentation de base
    if (!lora_enabled_ || !lora_initialized_) {
        return HuggingFaceLoader::applyAttention(input, layer_idx, ctx);
    }

    // === ATTENTION AVEC LoRA ===

    const auto& lw = layer_weights_[layer_idx];
    const int64_t hidden_size = model_info_.hidden_size;
    const int64_t num_heads = model_info_.num_attention_heads;
    const int64_t num_kv_heads = model_info_.num_key_value_heads;
    const int64_t head_dim = hidden_size / num_heads;

    // Dimensions [batch, seq, hidden]
    auto input_shape = input.sizes();
    int64_t batch_size = input_shape[0];
    int64_t seq_len = input_shape[1];

    // 1. RMSNorm pré-attention
    torch::Tensor normed = applyRMSNorm(input, lw.input_layernorm_weight, model_info_.rms_norm_eps);

    // 2. Projections Q, K, V avec LoRA
    torch::Tensor q = torch::linear(normed, lw.q_proj_weight);
    torch::Tensor k = torch::linear(normed, lw.k_proj_weight);
    torch::Tensor v = torch::linear(normed, lw.v_proj_weight);

    // Appliquer les corrections LoRA si disponibles
    torch::Tensor q_lora = applyLoRACorrection(normed, layer_idx, "q_proj");
    if (q_lora.defined()) q = q + q_lora;

    torch::Tensor k_lora = applyLoRACorrection(normed, layer_idx, "k_proj");
    if (k_lora.defined()) k = k + k_lora;

    torch::Tensor v_lora = applyLoRACorrection(normed, layer_idx, "v_proj");
    if (v_lora.defined()) v = v + v_lora;

    // 3. Reshape pour multi-head attention
    // [batch, seq, hidden] -> [batch, seq, num_heads, head_dim] -> [batch, num_heads, seq, head_dim]
    q = q.view({batch_size, seq_len, num_heads, head_dim}).transpose(1, 2);
    k = k.view({batch_size, seq_len, num_kv_heads, head_dim}).transpose(1, 2);
    v = v.view({batch_size, seq_len, num_kv_heads, head_dim}).transpose(1, 2);

    // 4. Appliquer RoPE (même code que la classe de base)
    // Note: La logique RoPE complète est dans HuggingFaceLoader::applyAttention
    // Pour simplifier, on pourrait appeler une méthode helper, mais pour l'instant
    // on réplique la logique ici

    // 5. KV-Cache (même logique que la classe de base)
    auto& kv_cache = ctx.getKVCache();
    torch::Tensor k_full, v_full;

    if (ctx.isUsingCache() && kv_cache.k_cache.size() > (size_t)layer_idx) {
        // Concaténer avec le cache
        k_full = torch::cat({kv_cache.k_cache[layer_idx], k}, 2);
        v_full = torch::cat({kv_cache.v_cache[layer_idx], v}, 2);

        // Mettre à jour le cache
        kv_cache.k_cache[layer_idx] = k_full;
        kv_cache.v_cache[layer_idx] = v_full;
    } else {
        k_full = k;
        v_full = v;
    }

    // 6. GQA: répéter K et V si nécessaire
    int64_t num_repeats = num_heads / num_kv_heads;
    if (num_repeats > 1) {
        k_full = k_full.unsqueeze(2)
                      .expand({batch_size, num_kv_heads, num_repeats, -1, head_dim})
                      .reshape({batch_size, num_heads, -1, head_dim});

        v_full = v_full.unsqueeze(2)
                      .expand({batch_size, num_kv_heads, num_repeats, -1, head_dim})
                      .reshape({batch_size, num_heads, -1, head_dim});
    }

    // 7. Scaled dot-product attention
    torch::Tensor attn_output = torch::scaled_dot_product_attention(
        q, k_full, v_full,
        /*attn_mask=*/c10::nullopt,  // Pas de masque explicite
        /*dropout_p=*/0.0,            // Pas de dropout en entraînement
        /*is_causal=*/true,           // Masque causal automatique
        /*scale=*/c10::nullopt        // Utiliser scale par défaut (1/sqrt(d))
    );

    // 8. Reshape: [batch, num_heads, seq, head_dim] -> [batch, seq, hidden]
    attn_output = attn_output.transpose(1, 2).contiguous().view({batch_size, seq_len, hidden_size});

    // 9. Projection de sortie avec LoRA
    torch::Tensor output = torch::linear(attn_output, lw.o_proj_weight);

    torch::Tensor o_lora = applyLoRACorrection(attn_output, layer_idx, "o_proj");
    if (o_lora.defined()) output = output + o_lora;

    // 10. Connexion résiduelle
    output = output + input;

    return output;
}

torch::Tensor HuggingFaceLoaderLoRA::applyMLP(const torch::Tensor& input, int layer_idx) {
    // Si LoRA n'est pas activé, utiliser l'implémentation de base
    if (!lora_enabled_ || !lora_initialized_) {
        return HuggingFaceLoader::applyMLP(input, layer_idx);
    }

    // === MLP AVEC LoRA ===

    const auto& lw = layer_weights_[layer_idx];

    // 1. RMSNorm pré-MLP
    torch::Tensor normed = applyRMSNorm(input, lw.post_attention_layernorm_weight, model_info_.rms_norm_eps);

    // 2. Projections gate et up avec LoRA
    torch::Tensor gate = torch::linear(normed, lw.gate_proj_weight);
    torch::Tensor up = torch::linear(normed, lw.up_proj_weight);

    // Appliquer corrections LoRA si disponibles (optionnel pour MLP)
    torch::Tensor gate_lora = applyLoRACorrection(normed, layer_idx, "gate_proj");
    if (gate_lora.defined()) gate = gate + gate_lora;

    torch::Tensor up_lora = applyLoRACorrection(normed, layer_idx, "up_proj");
    if (up_lora.defined()) up = up + up_lora;

    // 3. SwiGLU activation
    torch::Tensor gated = torch::silu(gate) * up;

    // 4. Projection down avec LoRA
    torch::Tensor output = torch::linear(gated, lw.down_proj_weight);

    torch::Tensor down_lora = applyLoRACorrection(gated, layer_idx, "down_proj");
    if (down_lora.defined()) output = output + down_lora;

    // 5. Connexion résiduelle
    output = output + input;

    return output;
}


} // namespace lispe_hf