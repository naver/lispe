#ifndef LISPE_HF_LOADER_MANUAL_H
#define LISPE_HF_LOADER_MANUAL_H

#include "lispe_hf_loader.h"

namespace lispe_hf {

class HuggingFaceLoaderManual : public HuggingFaceLoader {
public:
    HuggingFaceLoaderManual(const HFLoadConfig& config = HFLoadConfig()) 
        : HuggingFaceLoader(config) {}
        
    virtual ~HuggingFaceLoaderManual() {}

    // Les méthodes de gestion des scores sont maintenant dans HFInferenceContext
    // Ces méthodes sont des proxies pour faciliter la transition
    void enableAttentionRecording(bool enable, HFInferenceContext& ctx) {
        ctx.enableAttentionRecording(enable);
    }

    // Récupérer tous les scores du contexte
    std::vector<torch::Tensor> getAllAttentionScores(HFInferenceContext& ctx, bool clear = false) {
        std::vector<torch::Tensor> scores;
        for (size_t i = 0; i < ctx.getAttentionScoresCount(); ++i) {
            auto score = ctx.getAttentionScores(i);
            if (score.defined()) {
                scores.push_back(score);
            }
        }
        if (clear) {
            ctx.clearAttentionScores();
        }
        return scores;
    }
    
    // Récupérer le score d'une couche spécifique
    torch::Tensor getAttentionScores(HFInferenceContext& ctx, int layer_idx) {
        return ctx.getAttentionScores(layer_idx);
    }

protected:
    virtual torch::Tensor applyAttention(const torch::Tensor& input, int layer_idx, HFInferenceContext& ctx) override;
};

} // namespace lispe_hf

#endif // LISPE_HF_LOADER_MANUAL_H
