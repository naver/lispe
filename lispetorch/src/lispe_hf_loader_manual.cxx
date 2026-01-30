#include "lispe_hf_loader_manual.h"
#include <cmath>
#include <limits>

namespace lispe_hf {

torch::Tensor HuggingFaceLoaderManual::applyAttention(const torch::Tensor& input, int layer_idx, HFInferenceContext& ctx) {
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
                                   kv_cache.k_cache[layer_idx].defined() &&
                                   kv_cache.k_cache[layer_idx].numel() > 0 &&
                                   kv_cache.v_cache[layer_idx].defined() &&
                                   kv_cache.v_cache[layer_idx].numel() > 0;
    torch::Tensor input_to_process = input;


    // Si on utilise le cache et qu'on a plusieurs tokens en input, ne traiter que le dernier token
    // (En pratique, avec le cache, l'input devrait déjà être de taille 1)
    if (cache_exists_for_layer && seq_len > 1) {
        input_to_process = input.slice(1, seq_len - 1, seq_len);  // Dernier token seulement [batch, 1, hidden]
    } else if (cache_exists_for_layer && seq_len == 1) {
        // Avec un seul token et du cache, traiter le token complet
        input_to_process = input;
    } 

    // Projections linéaires Q, K, V fusionnées pour performance
    // Au lieu de 3 matmuls séparés, on fait 1 seule matmul puis on split
    torch::Tensor q, k, v;
    {
        // Utiliser les poids fusionnés Q+K+V pré-calculés (accès direct O(1))
        torch::Tensor qkv_weight = qkv_fused_weights_[layer_idx];

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
        if (lw.q_norm_weight.defined() && q.numel() > 0) {
            // Pour Qwen3, q_norm s'applique sur chaque tête séparément (dimension head_dim = 128)
            auto q_batch = q.size(0);
            auto q_seq = q.size(1);
            
            // Vérifier que les dimensions sont valides avant reshape
            if (q_batch > 0 && q_seq > 0 && q.size(2) == num_heads * head_dim) {
                auto q_reshaped = q.view({q_batch, q_seq, num_heads, -1});  // [batch, seq, num_heads, head_dim]
                auto variance = q_reshaped.pow(2).mean(-1, true);
                auto rms_factor = torch::rsqrt(variance + model_info_.rms_norm_eps);
                auto normalized = q_reshaped * rms_factor;
                q_reshaped = normalized * lw.q_norm_weight.unsqueeze(0).unsqueeze(0).unsqueeze(0);
                q = q_reshaped.view({q_batch, q_seq, -1});  // Retour à [batch, seq, hidden]
            }
        }
        if (lw.k_norm_weight.defined() && k.numel() > 0) {
            // Pour Qwen3, k_norm s'applique sur chaque tête KV séparément (dimension head_dim = 128)
            auto k_batch = k.size(0);
            auto k_seq = k.size(1);
            
            // Vérifier que les dimensions sont valides avant reshape
            if (k_batch > 0 && k_seq > 0 && k.size(2) == num_kv_heads * head_dim) {
                auto k_reshaped = k.view({k_batch, k_seq, num_kv_heads, -1});  // [batch, seq, num_kv_heads, head_dim]
                
                // Appliquer RMSNorm sur la dernière dimension (head_dim)
                auto variance = k_reshaped.pow(2).mean(-1, true);
                auto normalized = k_reshaped * torch::rsqrt(variance + model_info_.rms_norm_eps);
                k_reshaped = normalized * lw.k_norm_weight.unsqueeze(0).unsqueeze(0).unsqueeze(0);
                
                k = k_reshaped.view({k_batch, k_seq, -1});  // Retour à [batch, seq, kv_hidden]
            }
        }
    }


    // CORRECTION FONDAMENTALE: Suivre exactement la logique de l'implémentation normale
    // Dans l'implémentation normale, on traite toujours input_to_process, et new_tokens = sa taille
    int64_t q_seq_len = input_to_process.size(1);  // Tokens à traiter maintenant
    int64_t new_tokens = q_seq_len;  // Exactement pareil que dans l'implémentation normale

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
    // CORRECTION CRITIQUE: Initialiser avec la séquence d'input complète comme dans l'implémentation normale
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

    // ========== CORRECTION CRITIQUE DE ROPE ==========
    // Le problème: dans la version manuelle, on applique RoPE avant la gestion du cache KV,
    // mais les positions doivent correspondre à la séquence COMPLÈTE, pas juste aux nouveaux tokens
    
    int64_t start_pos = cache_exists_for_layer ? kv_cache.current_seq_len : 0;
    int64_t end_pos = start_pos + new_tokens;
    
    // S'assurer que le cache RoPE est assez grand pour la séquence complète
    int64_t max_seq_needed = cache_exists_for_layer ? end_pos : seq_len;
    if (max_seq_needed > kv_cache.rope_cache_max_len) {
        kv_cache.ensureRopeCache(max_seq_needed, head_dim, model_info_.rope_theta, device_);
    }

    // Convertir au dtype des tenseurs Q/K
    auto dtype = q.scalar_type();
    
    // CORRECTION: Pour Q, utiliser les positions correspondant aux nouveaux tokens
    // Pour K, on appliquera RoPE après avoir géré le cache (voir plus bas)
    auto cos_values_q = kv_cache.rope_cos_cache.slice(0, start_pos, end_pos).to(dtype);
    auto sin_values_q = kv_cache.rope_sin_cache.slice(0, start_pos, end_pos).to(dtype);

    // Appliquer RoPE à Q seulement (K sera traité après la gestion du cache)
    q = apply_rope(q, cos_values_q, sin_values_q);

    // Gérer le cache KV avec application correcte de RoPE
    torch::Tensor k_full, v_full;

    if (use_kv_cache) {
        if (cache_exists_for_layer) {
            // Cache existe: d'abord appliquer RoPE aux nouveaux K, puis concaténer
            auto cos_values_k = kv_cache.rope_cos_cache.slice(0, start_pos, end_pos).to(dtype);
            auto sin_values_k = kv_cache.rope_sin_cache.slice(0, start_pos, end_pos).to(dtype);
            k = apply_rope(k, cos_values_k, sin_values_k);
            
            // Récupérer le cache existant
            auto k_cached = kv_cache.k_cache[layer_idx];
            auto v_cached = kv_cache.v_cache[layer_idx];

            // Vérifications de sécurité pour éviter les crashes MPS
            if (k_cached.defined() && v_cached.defined() && 
                k_cached.numel() > 0 && v_cached.numel() > 0 &&
                k_cached.size(0) == k.size(0) && v_cached.size(0) == v.size(0) &&
                k_cached.size(1) == k.size(1) && v_cached.size(1) == v.size(1) &&
                k_cached.size(3) == k.size(3) && v_cached.size(3) == v.size(3)) {
                
                k_full = torch::cat({k_cached, k}, 2);  // Concat sur dimension seq
                v_full = torch::cat({v_cached, v}, 2);
            } else {
                // Cache corrompu ou incompatible - réinitialiser avec RoPE complet
                //affichage << "⚠ KV cache reset for layer " << layer_idx << " (incompatible dimensions)" << std::endl;
                
                // Réappliquer RoPE depuis le début pour la cohérence
                auto cos_full = kv_cache.rope_cos_cache.slice(0, 0, end_pos).to(dtype);
                auto sin_full = kv_cache.rope_sin_cache.slice(0, 0, end_pos).to(dtype);
                k_full = apply_rope(k, cos_full, sin_full);
                v_full = v;
            }
        } else {
            // Premier passage: appliquer RoPE depuis le début
            auto cos_full = kv_cache.rope_cos_cache.slice(0, 0, end_pos).to(dtype);
            auto sin_full = kv_cache.rope_sin_cache.slice(0, 0, end_pos).to(dtype);
            k_full = apply_rope(k, cos_full, sin_full);
            v_full = v;
        }

        // Stocker le résultat dans le cache
        if (k_full.defined() && v_full.defined() && k_full.numel() > 0 && v_full.numel() > 0) {
            kv_cache.k_cache[layer_idx] = k_full;
            kv_cache.v_cache[layer_idx] = v_full;
        }

        // Mettre à jour seq_len une seule fois (dans la première couche)
        if (layer_idx == 0) {
            kv_cache.current_seq_len = end_pos;
        }
    } else {
        // Pas de cache: appliquer RoPE à K depuis le début
        auto cos_full = kv_cache.rope_cos_cache.slice(0, 0, seq_len).to(dtype);
        auto sin_full = kv_cache.rope_sin_cache.slice(0, 0, seq_len).to(dtype);
        k_full = apply_rope(k, cos_full, sin_full);
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

    // Implémentation manuelle de l'attention multi-têtes (pas d'API optimisée)
    // Montre chaque étape explicitement : Q@K^T, scale, mask, softmax, @V
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

    // ========== ÉTAPE 1: Calcul des scores d'attention Q @ K^T ==========
    // q: [batch, num_heads, q_seq_len, head_dim]
    // k_full: [batch, num_heads, k_seq_len, head_dim]
    // Résultat: [batch, num_heads, q_seq_len, k_seq_len]
    
    auto attn_scores = torch::matmul(q, k_full.transpose(-2, -1));
    
    // ========== ÉTAPE 2: Application du facteur d'échelle ==========
    double scale_factor = 1.0 / std::sqrt(static_cast<double>(head_dim));
    attn_scores = attn_scores * scale_factor;
    
    // ========== ÉTAPE 3: Application du masque causal (si nécessaire) ==========
    if (!cache_exists_for_layer) {
        // Sans cache: appliquer le masque causal pour la génération autoregressive
        // Empêche les tokens de voir les tokens futurs
        if (q_seq_len > 1) {
            auto causal_mask = ctx.getCausalMask(q.size(2), k_full.size(2), device_);
            attn_scores = attn_scores.masked_fill(causal_mask, -std::numeric_limits<double>::infinity());
        }
    }
    // Avec KV-Cache: pas de masque causal car le nouveau token peut voir tous les tokens passés
    // ========== ÉTAPE 4: Application de la fonction softmax ==========
    auto attn_weights = torch::softmax(attn_scores, -1);
    
    // ========== SAUVEGARDE DES SCORES D'ATTENTION ==========
    if (ctx.isRecordingAttention()) {
        ctx.storeAttentionScores(layer_idx, attn_weights);
    }
    
    // ========== ÉTAPE 5: Multiplication avec les valeurs V ==========
    // attn_weights: [batch, num_heads, q_seq_len, k_seq_len]
    // v_full: [batch, num_heads, k_seq_len, head_dim]
    // Résultat: [batch, num_heads, q_seq_len, head_dim]
    attn_output = torch::matmul(attn_weights, v_full);
    
    // attn_output: [batch, num_heads, q_seq_len, head_dim]

    // Recombiner les têtes: [batch, q_seq_len, num_heads, head_dim]
    attn_output = attn_output.transpose(1, 2);
    attn_output = attn_output.contiguous().view({batch_size, q_seq_len, num_heads * head_dim});

    // Projection de sortie (accès direct O(1) aux poids)
    auto output = torch::linear(attn_output, lw.o_proj_weight);


    return output;  // Shape: [batch, q_seq_len, hidden] où q_seq_len = 1 si using_cache, sinon seq_len
}

} // namespace lispe_hf
