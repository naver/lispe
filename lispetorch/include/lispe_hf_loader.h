#ifndef LISPE_HF_LOADER_H
#define LISPE_HF_LOADER_H

#include "lispe.h"
#include <torch/torch.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <fstream>

// Forward declaration pour éviter les problèmes de dépendance circulaire
namespace lispe_hf {
    class HuggingFaceLoader;
}

#include <regex>
#include <memory>

// Configuration structure for Hugging Face models
// IMPORTANT: Défini AVANT d'inclure lispe_lispetorch.h pour éviter les dépendances circulaires
struct HuggingFaceModelConfig {
    size_t vocab_size;
    size_t hidden_size;
    size_t intermediate_size;
    size_t num_hidden_layers;
    size_t num_attention_heads;
    size_t num_key_value_heads;
    size_t max_position_embeddings;
    double rms_norm_eps;
    std::string rope_scaling_type;
    double rope_theta;
    std::string torch_dtype;
    std::string architectures;
    bool tie_word_embeddings;
    std::string model_type;

    // Constructor with defaults for Llama
    HuggingFaceModelConfig() :
        vocab_size(128256), hidden_size(4096), intermediate_size(14336),
        num_hidden_layers(32), num_attention_heads(32), num_key_value_heads(8),
        max_position_embeddings(131072), rms_norm_eps(1e-5),
        rope_scaling_type("linear"), rope_theta(500000.0),
        torch_dtype("bfloat16"), architectures("LlamaForCausalLM"),
        tie_word_embeddings(false), model_type("llama") {}
};

// Configuration structure for HuggingFace model loading and runtime parameters
struct HFLoadConfig {
    // Device configuration
    torch::Device device;

    // Context and sequence parameters
    int64_t max_seq_len;           // Maximum sequence length (0 = use model's max_position_embeddings)
    double rope_scaling;            // RoPE scaling factor

    // Sampling parameters
    double temperature;             // Temperature for sampling (1.0 = no change)
    double top_p;                   // Nucleus sampling threshold (1.0 = disabled)
    int64_t top_k;                  // Top-k sampling (0 = disabled)
    double repetition_penalty;      // Repetition penalty (1.0 = no penalty)

    // Cache configuration
    bool use_kv_cache;              // Enable KV-Cache by default
    int64_t max_cache_len;          // Maximum cache length for sliding window (0 = unlimited)

    // Constructor with defaults
    HFLoadConfig(torch::Device dev = torch::kCPU) :
        device(dev),
        max_seq_len(0),             // 0 = use model default
        rope_scaling(1.0),
        temperature(1.0),
        top_p(1.0),                 // disabled
        top_k(0),                   // disabled
        repetition_penalty(1.0),    // no penalty
        use_kv_cache(false),
        max_cache_len(0) {}         // 0 = unlimited cache
};

// Structure pour stocker le KV-Cache pour la génération autoregressive
struct KVCache {
    std::vector<torch::Tensor> k_cache;  // Cache des Keys par couche [num_layers]
    std::vector<torch::Tensor> v_cache;  // Cache des Values par couche [num_layers]
    int64_t current_seq_len;             // Longueur actuelle de la séquence en cache

    // Fenêtre glissante pour limiter l'utilisation mémoire
    int64_t max_cache_len;               // Taille maximale du cache (0 = illimité)
    bool use_sliding_window;             // Activer la fenêtre glissante

    // Cache RoPE spécifique à ce contexte
    torch::Tensor rope_cos_cache;        // Cache des cosinus pour RoPE
    torch::Tensor rope_sin_cache;        // Cache des sinus pour RoPE
    int64_t rope_cache_max_len;          // Taille maximale du cache RoPE

    KVCache() : current_seq_len(0), rope_cache_max_len(0), max_cache_len(0), use_sliding_window(false) {}

    void clear() {
        k_cache.clear();
        v_cache.clear();
        current_seq_len = 0;
        max_cache_len = 0;
        use_sliding_window = false;
        rope_cos_cache = torch::Tensor();
        rope_sin_cache = torch::Tensor();
        rope_cache_max_len = 0;
    }

    void resize(size_t num_layers) {
        k_cache.resize(num_layers);
        v_cache.resize(num_layers);
    }
    
    void configureSlidingWindow(int64_t max_len) {
        max_cache_len = max_len;
        use_sliding_window = (max_len > 0);
    }
    
    // Méthode pour appliquer la fenêtre glissante sur les caches KV
    void applySlidingWindow() {
        if (!use_sliding_window || max_cache_len <= 0) return;
        
        for (size_t i = 0; i < k_cache.size(); ++i) {
            if (k_cache[i].defined() && k_cache[i].size(2) > max_cache_len) {
                // Garder seulement les derniers max_cache_len tokens
                int64_t start_pos = k_cache[i].size(2) - max_cache_len;
                k_cache[i] = k_cache[i].slice(2, start_pos, k_cache[i].size(2)).contiguous();
                v_cache[i] = v_cache[i].slice(2, start_pos, v_cache[i].size(2)).contiguous();
            }
        }
        
        // Ajuster current_seq_len
        if (current_seq_len > max_cache_len) {
            current_seq_len = max_cache_len;
        }
    }

    // Méthode pour s'assurer que le cache RoPE est assez grand
    void ensureRopeCache(int64_t seq_len, int64_t head_dim, double rope_theta, torch::Device device) {
        if (rope_cache_max_len < seq_len) {
            // Créer un cache plus grand (2x pour éviter de recréer trop souvent)
            int64_t cache_len = seq_len * 2;

            // Créer les fréquences pour chaque dimension
            std::vector<double> freqs;
            for (int64_t i = 0; i < head_dim / 2; ++i) {
                double freq = 1.0 / std::pow(rope_theta, (2.0 * i) / head_dim);
                freqs.push_back(freq);
            }

            auto freqs_tensor = torch::tensor(freqs, torch::TensorOptions().dtype(torch::kFloat32).device(device));
            auto positions = torch::arange(cache_len, torch::TensorOptions().dtype(torch::kFloat32).device(device));
            auto pos_freqs = torch::outer(positions, freqs_tensor);

            // Stocker cos et sin dans le cache
            rope_cos_cache = pos_freqs.cos();
            rope_sin_cache = pos_freqs.sin();
            rope_cache_max_len = cache_len;
        }
    }
};

// Contexte d'inférence thread-safe pour chaque session
class HFInferenceContext : public Element {
private:
    KVCache kv_cache_;
    bool use_kv_cache_;
    
    // Stockage des scores d'attention pour analyse
    std::vector<torch::Tensor> attention_scores_;
    bool record_attention_;
    
    // Cache du masque causal pour éviter la recréation
    torch::Tensor causal_mask_cache_;
    int64_t cached_mask_size_;
    
public:
    static int16_t t_context_inference;

    HFInferenceContext(bool use_cache = false, bool record_attention = false) : 
        use_kv_cache_(use_cache), record_attention_(record_attention), 
        cached_mask_size_(0), Element(t_context_inference) {}
    ~HFInferenceContext() {
        kv_cache_.clear();
    }
    
    // Accès au cache
    KVCache& getKVCache() { return kv_cache_; }
    const KVCache& getKVCache() const { return kv_cache_; }
    
    // Configuration du cache
    bool isUsingCache() const { return use_kv_cache_; }
    void setUseCache(bool use_cache) { use_kv_cache_ = use_cache; }
    
    // Gestion du cache
    void resetCache() { 
        kv_cache_.clear(); 
    }
    
    void enableCache(bool e) {
        use_kv_cache_ = e;
        if (use_kv_cache_)
            kv_cache_.clear(); 
    }

    void resizeCache(size_t num_layers) {
        if (use_kv_cache_) {
            kv_cache_.resize(num_layers);
        }
    }
    
    // Informations sur le cache
    int64_t getKVCacheSeqLen() const { return kv_cache_.current_seq_len; }
    
    // Gestion des scores d'attention
    void enableAttentionRecording(bool enable) { 
        record_attention_ = enable;
        if (!enable) {
            attention_scores_.clear();
        }
    }
    
    bool isRecordingAttention() const { return record_attention_; }
    
    void storeAttentionScores(int layer_idx, const torch::Tensor& scores) {
        if (!record_attention_) return;
        
        if (attention_scores_.size() <= layer_idx) {
            attention_scores_.resize(layer_idx + 1);
        }
        attention_scores_[layer_idx] = scores.detach().cpu();
    }
    
    torch::Tensor getAttentionScores(int layer_idx) const {
        if (layer_idx < 0 || layer_idx >= attention_scores_.size()) {
            return torch::Tensor();
        }
        return attention_scores_[layer_idx];
    }
    
    size_t getAttentionScoresCount() const {
        return attention_scores_.size();
    }
    
    void clearAttentionScores() {
        attention_scores_.clear();
    }
    
    torch::Tensor getCausalMask(int64_t q_len, int64_t k_len, torch::Device device) {
        if (causal_mask_cache_.numel() == 0 || 
            cached_mask_size_ < std::max(q_len, k_len)) {
            
            cached_mask_size_ = std::max(q_len, k_len);
            auto mask = torch::ones({cached_mask_size_, cached_mask_size_}, 
                                   torch::TensorOptions().device(device).dtype(torch::kBool));
            causal_mask_cache_ = torch::triu(mask, 1);
        }
        
        return causal_mask_cache_.slice(0, 0, q_len).slice(1, 0, k_len);
    }
};

// Include lispe_lispetorch.h APRÈS les définitions de structures
// pour éviter les problèmes de dépendance circulaire avec lispe_mlx_loader.h
// #include "lispe_lispetorch.h"

// Forward declaration pour LoRALinear (défini dans lispe_lispetorch.h)
struct LoRALinear;

namespace lispe_hf {

// Structure pour stocker les poids d'une couche Transformer de manière organisée
struct LayerWeights {
    // Normalisation pré-attention
    torch::Tensor input_layernorm_weight;

    // Projections d'attention
    torch::Tensor q_proj_weight;
    torch::Tensor k_proj_weight;
    torch::Tensor v_proj_weight;
    torch::Tensor o_proj_weight;

    // Normalisations Q/K (Qwen3 uniquement - optionnels pour compatibilité Llama)
    torch::Tensor q_norm_weight;
    torch::Tensor k_norm_weight;

    // Normalisation pré-MLP
    torch::Tensor post_attention_layernorm_weight;

    // Projections MLP
    torch::Tensor gate_proj_weight;
    torch::Tensor up_proj_weight;
    torch::Tensor down_proj_weight;
};

// Hugging Face Model Loader class
class HuggingFaceLoader {
protected:
    HuggingFaceModelConfig model_info_;
    HFLoadConfig load_config_;         // Configuration de chargement et runtime
    std::unordered_map<std::string, torch::Tensor> weights_;  // Temporaire pendant chargement
    bool model_loaded_;
    bool enable_kv_cache;
    std::string model_path_;
    torch::Device device_;  // Device utilisé pour les calculs (MPS/CUDA/CPU)

    // Poids organisés par couche pour accès O(1) direct (évite hash lookups)
    std::vector<LayerWeights> layer_weights_;  // [num_layers=32]
    torch::Tensor embed_tokens_weight_;
    torch::Tensor final_norm_weight_;
    torch::Tensor lm_head_weight_;

    // Caches pour optimiser les forward passes répétés
    // Note: Cache RoPE supprimé - maintenant géré par KVCache dans chaque contexte
    // Note: causal_mask_cache_ supprimé - géré par scaled_dot_product_attention
    // Note: KVCache et use_kv_cache_ supprimés - gérés par HFInferenceContext

    // Cache pour les poids fusionnés (indexés par layer_idx pour accès O(1))
    mutable std::vector<torch::Tensor> qkv_fused_weights_;      // [num_layers] - QKV fusionnés
    mutable std::vector<torch::Tensor> gate_up_fused_weights_;  // [num_layers] - gate+up fusionnés

public:
    HFInferenceContext ctx_basic;
    ThreadLock lock;
    size_t memory_usage;
    bool threaded;
    bool fused;

    HuggingFaceLoader(const HFLoadConfig& config = HFLoadConfig());

    virtual ~HuggingFaceLoader();
    
    // Core loading functions
    bool loadConfig(const std::string& config_path);
    bool loadWeights(const std::string& weights_dir);
    bool validateModel();
    
    void enableKVCache(bool enable) {
        enable_kv_cache = enable;
    }

    // Model information
    HuggingFaceModelConfig getModelInfo() const { return model_info_; }
    HFLoadConfig getLoadConfig() const { return load_config_; }
    void setLoadConfig(const HFLoadConfig& config) { load_config_ = config; }
    std::vector<std::string> listWeights() const;
    torch::Tensor getWeight(const std::string& name) const;
    
    // Model operations avec contexte d'inférence
    torch::Tensor forward(const torch::Tensor& input_ids, HFInferenceContext& ctx);

    // Optimized versions with output tensor parameter
    void forward(const torch::Tensor& input_ids, torch::Tensor& output, HFInferenceContext& ctx);

    // API de compatibilité (crée un contexte temporaire)
    void forward(const torch::Tensor& input_ids, torch::Tensor& output);

    // Get embeddings for token IDs
    torch::Tensor getEmbeddings(const torch::Tensor& token_ids);

    double temperature() {
        return load_config_.temperature;
    }

protected:
    // Safetensors parsing simplifié et fonctionnel
    bool loadSafetensorsFile(const std::string& file_path);

    // Organisation des poids pour accès optimisé
    virtual void organizeWeights();

    // Model operations simplifiées
    torch::Tensor directForwardPass(const torch::Tensor& input_ids);

    // Méthodes internes pour charger les poids réels
    torch::Tensor createEmbeddingLayer(const torch::Tensor& input_ids);
    torch::Tensor createLinearLayer(const torch::Tensor& input, const torch::Tensor& weight, const torch::Tensor& bias = {});

    // Fonctions auxiliaires pour l'architecture Transformer
    torch::Tensor applyLayerNorm(const torch::Tensor& input, const std::string& layer_name);  // Deprecated - garder pour compatibilité
    virtual torch::Tensor applyAttention(const torch::Tensor& input, int layer_idx, HFInferenceContext& ctx);
    virtual torch::Tensor applyMLP(const torch::Tensor& input, int layer_idx);
    torch::Tensor applyRMSNorm(const torch::Tensor& input, const torch::Tensor& weight, double eps = 1e-6);
    
public:
    
    // Weight access methods for specific layer projections
    const torch::Tensor& getQWeight(int layer_idx) const;
    const torch::Tensor& getKWeight(int layer_idx) const;
    const torch::Tensor& getVWeight(int layer_idx) const;
    const torch::Tensor& getOWeight(int layer_idx) const;
    const torch::Tensor& getQKVFusedWeight(int layer_idx) const;
    
    // Weight access methods for MLP projections
    const torch::Tensor& getGateWeight(int layer_idx) const;
    const torch::Tensor& getUpWeight(int layer_idx) const;
    const torch::Tensor& getDownWeight(int layer_idx) const;
    const torch::Tensor& getGateUpFusedWeight(int layer_idx) const;
    
    // Model configuration access
    double getRMSNormEps() const;

    // Utilities
    size_t getMemoryUsage() const;
    std::string getModelSummary() const;
    torch::Device getDevice() const { return device_; }

    bool isLoaded() const { return model_loaded_; }
};

// ==================== LoRA Structures ====================

// Structure pour stocker les informations d'un adaptateur LoRA (legacy, pour compatibilité)
struct LoRAAdapter {
    std::string name;
    std::vector<std::string> target_modules;
    std::unordered_map<std::string, torch::Tensor> lora_A_weights;
    std::unordered_map<std::string, torch::Tensor> lora_B_weights;
    bool is_applied;

    LoRAAdapter() : is_applied(false) {}
};

// ==================== HuggingFaceLoaderLoRA ====================
// Classe dérivée avec support complet LoRA
// Surcharge applyAttention et applyMLP pour appliquer les corrections LoRA

class HuggingFaceLoaderLoRA : public HuggingFaceLoader {
private:
    // Stockage des adaptateurs LoRA par couche
    // Structure: lora_layers_[layer_idx][module_name] = shared_ptr<LoRALinear>
    std::vector<std::unordered_map<std::string, std::shared_ptr<LoRALinear>>> lora_layers_;

    // Configuration LoRA
    int lora_rank_;
    double lora_alpha_;
    double lora_dropout_;
    std::vector<std::string> target_modules_;

    bool lora_enabled_;
    bool lora_initialized_;

public:
    HuggingFaceLoaderLoRA(const HFLoadConfig& config = HFLoadConfig());
    virtual ~HuggingFaceLoaderLoRA();

    // Initialiser les adaptateurs LoRA pour toutes les couches
    bool initializeLoRA(int rank = 16,
                       double alpha = 32.0,
                       const std::vector<std::string>& target_modules = {"q_proj", "k_proj", "v_proj", "o_proj"},
                       torch::ScalarType dtype = torch::kFloat32);

    // Activer/désactiver LoRA
    void enableLoRA(bool enable = true) { lora_enabled_ = enable; }
    bool isLoRAEnabled() const { return lora_enabled_; }
    bool isLoRAInitialized() const { return lora_initialized_; }

    void organizeWeights();

    // Accès aux adaptateurs
    std::shared_ptr<LoRALinear> getLoRALayer(int layer_idx, const std::string& module_name);
    std::vector<torch::Tensor> getLoRAParameters();  // Pour l'optimiseur

    // Sauvegarder/charger les adaptateurs
    bool saveLoRAAdapters(const std::string& path);
    bool loadLoRAAdapters(const std::string& path);

    // Fusionner les adaptateurs dans les poids de base
    void mergeLoRAWeights();
    void unmergeLoRAWeights();

    // Override des méthodes virtuelles avec support LoRA
    virtual torch::Tensor applyAttention(const torch::Tensor& input, int layer_idx, HFInferenceContext& ctx) override;
    virtual torch::Tensor applyMLP(const torch::Tensor& input, int layer_idx) override;

private:
    // Appliquer une correction LoRA à une projection linéaire
    torch::Tensor applyLoRACorrection(const torch::Tensor& input,
                                     int layer_idx,
                                     const std::string& module_name);
};

} // namespace lispe_hf

#endif // LISPE_HF_LOADER_H