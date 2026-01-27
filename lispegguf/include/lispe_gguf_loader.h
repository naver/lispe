/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_gguf_loader.h
//  Support pour le chargement des fichiers GGUF (llama.cpp format)
//

#ifndef LISPE_GGUF_LOADER_H
#define LISPE_GGUF_LOADER_H

#include "lispe.h"
#include <torch/torch.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <fstream>
#include <memory>
#include <cstdint>
#include <cstring>
#include <stdexcept>

namespace lispe_gguf {

// ============================================================================
// Constantes GGUF
// ============================================================================

constexpr uint32_t GGUF_MAGIC = 0x46554747;  // "GGUF" en little-endian
constexpr uint32_t GGUF_VERSION_MIN = 2;
constexpr uint32_t GGUF_VERSION_MAX = 3;

// ============================================================================
// Types de données GGUF pour les métadonnées
// ============================================================================

enum class GGUFMetadataType : uint32_t {
    UINT8   = 0,
    INT8    = 1,
    UINT16  = 2,
    INT16   = 3,
    UINT32  = 4,
    INT32   = 5,
    FLOAT32 = 6,
    BOOL    = 7,
    STRING  = 8,
    ARRAY   = 9,
    UINT64  = 10,
    INT64   = 11,
    FLOAT64 = 12,
};

// ============================================================================
// Types de quantification GGML
// ============================================================================

enum class GGMLType : uint32_t {
    F32     = 0,
    F16     = 1,
    Q4_0    = 2,
    Q4_1    = 3,
    // Q4_2 = 4 (removed)
    // Q4_3 = 5 (removed)
    Q5_0    = 6,
    Q5_1    = 7,
    Q8_0    = 8,
    Q8_1    = 9,
    Q2_K    = 10,
    Q3_K    = 11,
    Q4_K    = 12,
    Q5_K    = 13,
    Q6_K    = 14,
    Q8_K    = 15,
    IQ2_XXS = 16,
    IQ2_XS  = 17,
    IQ3_XXS = 18,
    IQ1_S   = 19,
    IQ4_NL  = 20,
    IQ3_S   = 21,
    IQ2_S   = 22,
    IQ4_XS  = 23,
    I8      = 24,
    I16     = 25,
    I32     = 26,
    I64     = 27,
    F64     = 28,
    BF16    = 29,
    Q4_0_4_4 = 30,
    Q4_0_4_8 = 31,
    Q4_0_8_8 = 32,
    TQ1_0   = 33,
    TQ2_0   = 34,
    IQ1_M   = 35,
    BF16_FP16 = 36,
    Q4_0_4_4_FP16 = 37,
    Q4_0_4_8_FP16 = 38,
    MXFP4   = 39,  // Mixed FP4 format utilisé par certains modèles
    COUNT
};

// Tailles de bloc pour chaque type de quantification
struct GGMLTypeInfo {
    size_t block_size;      // Nombre d'éléments par bloc
    size_t type_size;       // Taille en octets d'un bloc
    bool is_quantized;      // Indique si le type est quantifié
    const char* name;       // Nom du type
};

// Retourne les informations sur un type GGML
const GGMLTypeInfo& get_ggml_type_info(GGMLType type);

// ============================================================================
// Structure pour un tenseur GGUF
// ============================================================================

struct GGUFTensorInfo {
    std::string name;
    uint32_t n_dims;
    std::vector<int64_t> dimensions;
    GGMLType type;
    uint64_t offset;        // Offset dans le fichier (relatif au début des données tenseur)
    size_t size_bytes;      // Taille totale en octets
    
    // Calcule le nombre total d'éléments
    int64_t numel() const {
        int64_t n = 1;
        for (auto d : dimensions) n *= d;
        return n;
    }
};

// ============================================================================
// Structure pour les métadonnées du modèle
// ============================================================================

struct GGUFModelConfig {
    // Informations générales
    std::string architecture;
    std::string name;
    std::string author;
    std::string license;
    std::string description;
    std::string file_type;
    std::string quantization_version;
    
    // Architecture du modèle
    uint32_t context_length = 2048;
    uint32_t embedding_length = 4096;
    uint32_t block_count = 32;
    uint32_t attention_head_count = 32;
    uint32_t attention_head_count_kv = 32;
    uint32_t feed_forward_length = 11008;
    float attention_layer_norm_rms_epsilon = 1e-5f;
    
    // RoPE (Rotary Position Embedding)
    uint32_t rope_dimension_count = 128;
    float rope_freq_base = 10000.0f;
    float rope_scaling_factor = 1.0f;
    std::string rope_scaling_type;

    // Mixture of Experts (MoE)
    uint32_t expert_count = 0;
    uint32_t expert_used_count = 0;

    // Vocabulaire
    uint32_t vocab_size = 32000;
    std::vector<std::string> tokens;
    std::vector<float> token_scores;
    std::vector<int32_t> token_types;
    
    // Tokens spéciaux
    uint32_t bos_token_id = 1;
    uint32_t eos_token_id = 2;
    uint32_t pad_token_id = 0;
    uint32_t unk_token_id = 0;
    uint32_t sep_token_id = 0;
    
    // Chat template
    std::string chat_template;
    
    // Computed values
    uint32_t head_dim() const {
        return embedding_length / attention_head_count;
    }
    
    uint32_t kv_head_dim() const {
        return embedding_length / attention_head_count;
    }
};

// ============================================================================
// Configuration de chargement
// ============================================================================

struct GGUFLoadConfig {
    torch::Device device = torch::kCPU;
    torch::Dtype compute_dtype = torch::kFloat32;  // Type pour les calculs (après déquantification)
    bool use_mmap = true;                          // Utiliser memory mapping si possible
    bool load_weights = true;                      // Charger les poids (false = seulement metadata)
    bool dequantize_on_load = false;               // Déquantifier tous les tenseurs au chargement
    int num_threads = 4;                           // Nombre de threads pour la déquantification
    
    GGUFLoadConfig() = default;
    GGUFLoadConfig(torch::Device dev) : device(dev) {}
};

// ============================================================================
// Classe principale pour charger les fichiers GGUF
// ============================================================================

class GGUFLoader {
public:
    GGUFLoader();
    ~GGUFLoader();
    
    // Charge un fichier GGUF
    bool load(const std::string& filepath, const GGUFLoadConfig& config = GGUFLoadConfig());
    
    // Retourne la configuration du modèle
    const GGUFModelConfig& config() const { return model_config_; }
    
    // Retourne la liste des tenseurs
    const std::vector<GGUFTensorInfo>& tensor_infos() const { return tensor_infos_; }
    
    // Trouve un tenseur par nom
    const GGUFTensorInfo* find_tensor(const std::string& name) const;
    
    // Charge un tenseur spécifique (avec déquantification optionnelle)
    torch::Tensor load_tensor(const std::string& name, bool dequantize = true);
    torch::Tensor load_tensor(const GGUFTensorInfo& info, bool dequantize = true);
    
    // Déquantifie un tenseur quantifié vers float
    static torch::Tensor dequantize(const void* data, const GGUFTensorInfo& info, 
                                     torch::Device device = torch::kCPU,
                                     torch::Dtype dtype = torch::kFloat32);
    
    // Retourne les métadonnées brutes
    const std::unordered_map<std::string, std::string>& metadata_strings() const { return metadata_strings_; }
    const std::unordered_map<std::string, int64_t>& metadata_ints() const { return metadata_ints_; }
    const std::unordered_map<std::string, double>& metadata_floats() const { return metadata_floats_; }
    
    // Informations sur le fichier
    const std::string& filepath() const { return filepath_; }
    uint32_t version() const { return version_; }
    uint64_t tensor_data_offset() const { return tensor_data_offset_; }
    
    // Tokenization simple (si vocabulaire chargé)
    std::vector<int32_t> tokenize(const std::string& text) const;
    std::string detokenize(const std::vector<int32_t>& tokens) const;
    
    // Utilitaires
    void print_info() const;
    size_t total_size_bytes() const;
    size_t model_size_bytes() const;
    
private:
    // Lecture du header
    bool read_header(std::ifstream& file);
    
    // Lecture des métadonnées
    bool read_metadata(std::ifstream& file, uint64_t count);
    
    // Lecture des informations de tenseur
    bool read_tensor_infos(std::ifstream& file, uint64_t count);
    
    // Parse une valeur de métadonnée
    bool read_metadata_value(std::ifstream& file, const std::string& key, GGUFMetadataType type);
    
    // Lecture de types primitifs
    std::string read_string(std::ifstream& file);
    
    template<typename T>
    T read_value(std::ifstream& file) {
        T value;
        file.read(reinterpret_cast<char*>(&value), sizeof(T));
        return value;
    }
    
    // Configuration du modèle extraite des métadonnées
    void extract_model_config();
    
    // Données membres
    std::string filepath_;
    uint32_t version_ = 0;
    uint64_t tensor_data_offset_ = 0;
    
    GGUFModelConfig model_config_;
    GGUFLoadConfig load_config_;
    
    std::vector<GGUFTensorInfo> tensor_infos_;
    std::unordered_map<std::string, size_t> tensor_name_to_index_;
    
    // Métadonnées brutes
    std::unordered_map<std::string, std::string> metadata_strings_;
    std::unordered_map<std::string, int64_t> metadata_ints_;
    std::unordered_map<std::string, double> metadata_floats_;
    std::unordered_map<std::string, std::vector<std::string>> metadata_string_arrays_;
    std::unordered_map<std::string, std::vector<int64_t>> metadata_int_arrays_;
    std::unordered_map<std::string, std::vector<double>> metadata_float_arrays_;
    
    // Memory mapping (optionnel)
    void* mmap_data_ = nullptr;
    size_t mmap_size_ = 0;
    int mmap_fd_ = -1;
};

// ============================================================================
// Classe pour l'inférence avec un modèle GGUF
// ============================================================================

class GGUFModel {
public:
    GGUFModel();
    ~GGUFModel();
    
    // Charge un modèle GGUF
    bool load(const std::string& filepath, const GGUFLoadConfig& config = GGUFLoadConfig());
    
    // Accès au loader et à la configuration
    const GGUFLoader& loader() const { return loader_; }
    const GGUFModelConfig& config() const { return loader_.config(); }
    
    // Forward pass - retourne les logits
    torch::Tensor forward(const torch::Tensor& input_ids, 
                          int64_t start_pos = 0,
                          bool use_cache = true);
    
    // Génération de texte
    std::vector<int32_t> generate(const std::vector<int32_t>& prompt_tokens,
                                   int max_new_tokens = 100,
                                   float temperature = 1.0f,
                                   float top_p = 0.9f,
                                   int top_k = 40,
                                   float repetition_penalty = 1.0f);
    
    // Embeddings
    torch::Tensor get_embeddings(const torch::Tensor& input_ids);
    
    // Gestion du cache KV
    void reset_cache();
    void enable_cache(bool enable) { use_cache_ = enable; }
    bool cache_enabled() const { return use_cache_; }
    
    // Tokenization
    std::vector<int32_t> tokenize(const std::string& text) const { return loader_.tokenize(text); }
    std::string detokenize(const std::vector<int32_t>& tokens) const { return loader_.detokenize(tokens); }
    
    // Informations
    void print_info() const { loader_.print_info(); }
    
private:
    // Initialise les poids du modèle
    bool initialize_weights();
    
    // Pré-calcule les embeddings RoPE
    void precompute_rope_embeddings();
    
    // Couches du modèle
    torch::Tensor apply_embedding(const torch::Tensor& input_ids);
    torch::Tensor apply_rms_norm(const torch::Tensor& x, const torch::Tensor& weight, float eps);
    torch::Tensor apply_attention(const torch::Tensor& x, int layer_idx, int64_t start_pos);
    torch::Tensor apply_feed_forward(const torch::Tensor& x, int layer_idx);
    torch::Tensor apply_rotary_embedding(const torch::Tensor& x, int64_t start_pos);
    
    // Déquantification à la volée
    torch::Tensor get_weight(const std::string& name);
    
    GGUFLoader loader_;
    GGUFLoadConfig config_;
    
    // Cache des poids déquantifiés (lazy loading)
    std::unordered_map<std::string, torch::Tensor> weight_cache_;
    
    // KV Cache pour la génération
    std::vector<torch::Tensor> k_cache_;
    std::vector<torch::Tensor> v_cache_;
    int64_t cache_seq_len_ = 0;
    bool use_cache_ = true;
    
    // Poids pré-chargés pour les parties critiques
    torch::Tensor token_embedding_;
    torch::Tensor output_weight_;
    torch::Tensor output_norm_weight_;
    
    // RoPE precomputed
    torch::Tensor rope_cos_;
    torch::Tensor rope_sin_;
    int64_t rope_max_seq_len_ = 0;

    // Vrai head_dim détecté (peut différer de embedding_length / num_heads)
    int64_t real_head_dim_ = 0;
    int64_t real_q_dim_ = 0;  // Dimension totale de Q (num_heads * head_dim)
};

// ============================================================================
// Fonctions de déquantification
// ============================================================================

namespace dequant {

// Déquantifie Q4_0 vers float
void dequantize_q4_0(const void* src, float* dst, int64_t n);

// Déquantifie Q4_1 vers float
void dequantize_q4_1(const void* src, float* dst, int64_t n);

// Déquantifie Q5_0 vers float
void dequantize_q5_0(const void* src, float* dst, int64_t n);

// Déquantifie Q5_1 vers float
void dequantize_q5_1(const void* src, float* dst, int64_t n);

// Déquantifie Q8_0 vers float
void dequantize_q8_0(const void* src, float* dst, int64_t n);

// Déquantifie Q2_K vers float
void dequantize_q2_k(const void* src, float* dst, int64_t n);

// Déquantifie Q3_K vers float
void dequantize_q3_k(const void* src, float* dst, int64_t n);

// Déquantifie Q4_K vers float
void dequantize_q4_k(const void* src, float* dst, int64_t n);

// Déquantifie Q5_K vers float
void dequantize_q5_k(const void* src, float* dst, int64_t n);

// Déquantifie Q6_K vers float
void dequantize_q6_k(const void* src, float* dst, int64_t n);

// Déquantifie F16 vers float
void dequantize_f16(const void* src, float* dst, int64_t n);

// Déquantifie BF16 vers float
void dequantize_bf16(const void* src, float* dst, int64_t n);

// Déquantifie MXFP4 vers float
void dequantize_mxfp4(const void* src, float* dst, int64_t n);

} // namespace dequant

} // namespace lispe_gguf

#endif // LISPE_GGUF_LOADER_H
