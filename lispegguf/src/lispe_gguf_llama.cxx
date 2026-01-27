/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_gguf_llama.cxx
//  LispE library for GGUF based on llama.cpp
//
//  This version uses llama.cpp directly instead of PyTorch
//  for optimal performance with MXFP4 and other quantized formats
//

#include "lispe.h"
#include "../llama.cpp/include/llama.h"

#include <string>
#include <vector>
#include <memory>
#include <iostream>

using namespace std;

// Forward declaration for callback
static void callback_eval(LispE* lisp, Element* function, llama_token token, Element* data);

// Enum for GGUF actions
enum gguf_actions {
    gguf_action_load_model,
    gguf_action_generate,
    gguf_action_tokenize,
    gguf_action_detokenize,
    gguf_action_free_model,
    gguf_action_set_log,
    gguf_action_get_chat_template,
    gguf_action_apply_chat_template,
    gguf_action_chat
};

// Type for GGUF models
int16_t t_gguf_model = 0;

// Wrapper for a llama.cpp model
struct LlamaModel {
    llama_model* model = nullptr;
    llama_context* ctx = nullptr;
    llama_sampler* sampler = nullptr;
    const llama_vocab* vocab = nullptr;

    ~LlamaModel() {
        if (sampler) llama_sampler_free(sampler);
        if (ctx) llama_free(ctx);
        if (model) llama_model_free(model);
    }
};

// LispE element wrapping a llama.cpp model
class GGUFModelElementV2 : public Element {
public:
    std::unique_ptr<LlamaModel> model;
    std::string filepath;

    GGUFModelElementV2(std::unique_ptr<LlamaModel> m, const std::string& path)
        : Element(t_gguf_model), model(std::move(m)), filepath(path) {}

    Element* eval(LispE* lisp) override { return this; }
    bool Boolean() override { return model && model->model != nullptr; }
    wstring asString(LispE* lisp) override {
        return L"GGUF Model (llama.cpp): " + wstring(filepath.begin(), filepath.end());
    }
};

// Main class for GGUF functions in LispE
class Lispe_gguf : public Element {
public:
    gguf_actions action;

    Lispe_gguf(gguf_actions a) : Element(l_lib), action(a) {}

    Element* eval(LispE* lisp) override;

    Element* load_model(LispE* lisp);
    Element* generate(LispE* lisp);
    Element* tokenize(LispE* lisp);
    Element* detokenize(LispE* lisp);
    Element* free_model(LispE* lisp);
    Element* set_log(LispE* lisp);
    Element* get_chat_template(LispE* lisp);
    Element* apply_chat_template(LispE* lisp);
    Element* chat(LispE* lisp);
};

// ============================================================================
// Helper functions
// ============================================================================

// Callback function to call LispE function during generation
static void callback_eval(LispE* lisp, Element* function, llama_token token, Element* data) {
    Element* tok = lisp->provideInteger(token);
    vector<Element*> arguments;
    arguments.push_back(tok);
    if (data != NULL)
        arguments.push_back(data);

    Element* e = lispe_eval_callback(lisp, function, arguments);
    if (e->isError()) {
        cerr << e->toString(lisp) << endl;
    }
    e->release();
}

// Convert string to GGML type for KV cache quantization
static ggml_type parse_kv_cache_type(const std::string& type_str) {
    if (type_str == "f16") return GGML_TYPE_F16;
    if (type_str == "q8_0") return GGML_TYPE_Q8_0;
    if (type_str == "q4_0") return GGML_TYPE_Q4_0;
    if (type_str == "q4_1") return GGML_TYPE_Q4_1;
    if (type_str == "q5_0") return GGML_TYPE_Q5_0;
    if (type_str == "q5_1") return GGML_TYPE_Q5_1;
    return GGML_TYPE_F16;  // Default to F16
}

// ============================================================================
// Method implementations
// ============================================================================

Element* Lispe_gguf::eval(LispE* lisp) {
    switch (action) {
        case gguf_action_load_model:
            return load_model(lisp);
        case gguf_action_generate:
            return generate(lisp);
        case gguf_action_tokenize:
            return tokenize(lisp);
        case gguf_action_detokenize:
            return detokenize(lisp);
        case gguf_action_free_model:
            return free_model(lisp);
        case gguf_action_set_log:
            return set_log(lisp);
        case gguf_action_get_chat_template:
            return get_chat_template(lisp);
        case gguf_action_apply_chat_template:
            return apply_chat_template(lisp);
        case gguf_action_chat:
            return chat(lisp);
        default:
            return null_;
    }
}

Element* Lispe_gguf::load_model(LispE* lisp) {
    // Signature: deflib gguf_load(filepath (config))

    Element* filepath_elem = lisp->get_variable("filepath");
    std::string filepath = filepath_elem->toString(lisp);

    // Optional parameter: config (dictionary)
    Element* config_elem = lisp->get_variable("config");

    // Default options
    int n_gpu_layers = 99;  // GPU by default (Metal on macOS, CUDA/ROCm on Linux)
    int n_ctx = 2048;       // Default context size

    // Advanced options (only set if explicitly provided)
    bool set_cache_type_k = false;
    bool set_cache_type_v = false;
    bool set_flash_attn = false;
    ggml_type cache_type_k = GGML_TYPE_F16;
    ggml_type cache_type_v = GGML_TYPE_F16;
    llama_flash_attn_type flash_attn_type = LLAMA_FLASH_ATTN_TYPE_DISABLED;

    // Parse configuration dictionary if provided
    if (config_elem != null_ && config_elem != NULL && config_elem->isDictionary()) {
        Dictionary* config_dict = (Dictionary*)config_elem;

        // n_gpu_layers
        Element* ngl_val = config_dict->on_index(U"n_gpu_layers");
        if (ngl_val && ngl_val->isNumber()) {
            n_gpu_layers = ngl_val->asInteger();
        }

        // n_ctx
        Element* ctx_val = config_dict->on_index(U"n_ctx");
        if (ctx_val && ctx_val->isNumber()) {
            n_ctx = ctx_val->asInteger();
        }

        // cache_type_k (KV cache quantization for keys)
        Element* cache_k_val = config_dict->on_index(U"cache_type_k");
        if (cache_k_val && cache_k_val->isString()) {
            std::string type_str = cache_k_val->toString(lisp);
            cache_type_k = parse_kv_cache_type(type_str);
            set_cache_type_k = true;
        }

        // cache_type_v (KV cache quantization for values)
        Element* cache_v_val = config_dict->on_index(U"cache_type_v");
        if (cache_v_val && cache_v_val->isString()) {
            std::string type_str = cache_v_val->toString(lisp);
            cache_type_v = parse_kv_cache_type(type_str);
            set_cache_type_v = true;
        }

        // flash_attn (Flash Attention - experimental)
        Element* flash_val = config_dict->on_index(U"flash_attn");
        if (flash_val) {
            bool enable = flash_val->Boolean();
            flash_attn_type = enable ? LLAMA_FLASH_ATTN_TYPE_ENABLED : LLAMA_FLASH_ATTN_TYPE_DISABLED;
            set_flash_attn = true;
        }
    }

    try {
        // Create the wrapper
        auto llama_wrapper = std::make_unique<LlamaModel>();

        // Model parameters
        llama_model_params model_params = llama_model_default_params();
        model_params.n_gpu_layers = n_gpu_layers;

        // Load the model
        llama_wrapper->model = llama_model_load_from_file(filepath.c_str(), model_params);

        if (!llama_wrapper->model) {
            return null_;
        }

        // Get vocabulary
        llama_wrapper->vocab = llama_model_get_vocab(llama_wrapper->model);

        // Create context
        llama_context_params ctx_params = llama_context_default_params();
        ctx_params.n_ctx = n_ctx;
        ctx_params.n_batch = 512;
        ctx_params.no_perf = false;

        // Advanced KV-cache options (only set if explicitly requested)
        if (set_cache_type_k) {
            ctx_params.type_k = cache_type_k;
        }
        if (set_cache_type_v) {
            ctx_params.type_v = cache_type_v;
        }
        if (set_flash_attn) {
            ctx_params.flash_attn_type = flash_attn_type;
        }

        llama_wrapper->ctx = llama_init_from_model(llama_wrapper->model, ctx_params);
        if (!llama_wrapper->ctx) {
            return null_;
        }

        // Create sampler with greedy sampling by default
        auto sparams = llama_sampler_chain_default_params();
        sparams.no_perf = false;
        llama_wrapper->sampler = llama_sampler_chain_init(sparams);
        llama_sampler_chain_add(llama_wrapper->sampler, llama_sampler_init_greedy());

        // Create LispE element
        return new GGUFModelElementV2(std::move(llama_wrapper), filepath);

    } catch (const std::exception& e) {
        return null_;
    }
}

Element* Lispe_gguf::tokenize(LispE* lisp) {
    // Signature: deflib gguf_tokenize(model text)

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_tokenize requires a GGUF model");
    }

    GGUFModelElementV2* gguf_model = (GGUFModelElementV2*)model_elem;
    Element* text_elem = lisp->get_variable("text");
    std::string text = text_elem->toString(lisp);

    try {
        const llama_vocab* vocab = gguf_model->model->vocab;

        // Count the number of tokens
        int n_tokens = -llama_tokenize(vocab, text.c_str(), text.size(), NULL, 0, true, true);

        // Tokenize
        std::vector<llama_token> tokens(n_tokens);
        if (llama_tokenize(vocab, text.c_str(), text.size(), tokens.data(), tokens.size(), true, true) < 0) {
            throw new Error("Error: tokenization failed");
        }

        // Convert to LispE list
        Element* result = new List;
        for (llama_token token : tokens) {
            result->append(lisp->provideInteger(token));
        }

        return result;

    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_gguf::detokenize(LispE* lisp) {
    // Signature: deflib gguf_detokenize(model tokens)

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_detokenize requires a GGUF model");
    }

    GGUFModelElementV2* gguf_model = (GGUFModelElementV2*)model_elem;
    Element* tokens_elem = lisp->get_variable("tokens");

    if (!tokens_elem->isList()) {
        throw new Error("Error: tokens must be a list");
    }

    try {
        const llama_vocab* vocab = gguf_model->model->vocab;
        std::string result;

        // Convert each token to text
        for (long i = 0; i < tokens_elem->size(); ++i) {
            llama_token token = tokens_elem->index(i)->asInteger();

            char buf[128];
            int n = llama_token_to_piece(vocab, token, buf, sizeof(buf), 0, true);
            if (n > 0) {
                result += std::string(buf, n);
            }
        }

        return lisp->provideString(result);

    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_gguf::generate(LispE* lisp) {
    // Signature: deflib gguf_generate(model prompt (config))

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_generate requires a GGUF model");
    }

    GGUFModelElementV2* gguf_model = (GGUFModelElementV2*)model_elem;
    Element* prompt_elem = lisp->get_variable("prompt");
    std::string prompt = prompt_elem->toString(lisp);

    // Optional parameter: config (dictionary)
    Element* config_elem = lisp->get_variable("config");

    // Default options
    int max_tokens = 100;
    float temperature = 0.8f;
    float top_p = 0.9f;
    int top_k = 40;
    float repeat_penalty = 1.1f;
    int repeat_last_n = 64;
    Element* callback = NULL;
    Element* callback_data = NULL;

    // Parse configuration dictionary if provided
    if (config_elem != null_ && config_elem != NULL && config_elem->isDictionary()) {
        Dictionary* config_dict = (Dictionary*)config_elem;

        // max_tokens
        Element* max_tok_val = config_dict->on_index(U"max_tokens");
        if (max_tok_val && max_tok_val->isNumber()) {
            max_tokens = max_tok_val->asInteger();
        }

        // temperature
        Element* temp_val = config_dict->on_index(U"temperature");
        if (temp_val && temp_val->isNumber()) {
            temperature = temp_val->asFloat();
        }

        // top_p
        Element* top_p_val = config_dict->on_index(U"top_p");
        if (top_p_val && top_p_val->isNumber()) {
            top_p = top_p_val->asFloat();
        }

        // top_k
        Element* top_k_val = config_dict->on_index(U"top_k");
        if (top_k_val && top_k_val->isNumber()) {
            top_k = top_k_val->asInteger();
        }

        // repeat_penalty
        Element* repeat_penalty_val = config_dict->on_index(U"repeat_penalty");
        if (repeat_penalty_val && repeat_penalty_val->isNumber()) {
            repeat_penalty = repeat_penalty_val->asFloat();
        }

        // repeat_last_n
        Element* repeat_last_n_val = config_dict->on_index(U"repeat_last_n");
        if (repeat_last_n_val && repeat_last_n_val->isNumber()) {
            repeat_last_n = repeat_last_n_val->asInteger();
        }

        // callback function
        callback = config_dict->on_index(U"callback");

        // callback data (optional)
        callback_data = config_dict->on_index(U"data");
    }

    try {
        auto& model = gguf_model->model;

        // Clear KV cache before new generation
        llama_memory_t mem = llama_get_memory(model->ctx);
        llama_memory_clear(mem, true);

        // Tokenize the prompt
        const llama_vocab* vocab = model->vocab;
        int n_prompt = -llama_tokenize(vocab, prompt.c_str(), prompt.size(), NULL, 0, true, true);

        std::vector<llama_token> tokens(n_prompt);
        if (llama_tokenize(vocab, prompt.c_str(), prompt.size(), tokens.data(), tokens.size(), true, true) < 0) {
            throw new Error("Error: failed to tokenize prompt");
        }

        // Get special tokens for EOG detection
        llama_token eos_token = llama_vocab_eos(vocab);
        llama_token eot_token = llama_vocab_eot(vocab);

        // Configure sampler with parameters
        llama_sampler_free(model->sampler);
        auto sparams = llama_sampler_chain_default_params();
        model->sampler = llama_sampler_chain_init(sparams);

        // Add samplers in the correct order:
        // 1. Penalties (must be applied before other samplers on full vocabulary)
        llama_sampler_chain_add(model->sampler, llama_sampler_init_penalties(
            repeat_last_n,    // penalty_last_n: number of tokens to check for repetition
            repeat_penalty,   // penalty_repeat: repetition penalty (1.0 = disabled)
            0.0f,             // penalty_freq: frequency penalty (0.0 = disabled)
            0.0f              // penalty_present: presence penalty (0.0 = disabled)
        ));
        // 2. Top-K sampling
        llama_sampler_chain_add(model->sampler, llama_sampler_init_top_k(top_k));
        // 3. Top-P sampling
        llama_sampler_chain_add(model->sampler, llama_sampler_init_top_p(top_p, 1));
        // 4. Temperature
        llama_sampler_chain_add(model->sampler, llama_sampler_init_temp(temperature));
        // 5. Final distribution sampling
        llama_sampler_chain_add(model->sampler, llama_sampler_init_dist(LLAMA_DEFAULT_SEED));

        // Process the prompt
        llama_batch batch = llama_batch_get_one(tokens.data(), tokens.size());

        if (llama_decode(model->ctx, batch)) {
            throw new Error("Error: failed to decode prompt");
        }

        // Generate tokens
        Integers* generated_tokens = lisp->provideIntegers();

        for (int i = 0; i < max_tokens; ++i) {
            // Sample next token
            llama_token new_token = llama_sampler_sample(model->sampler, model->ctx, -1);

            // Check if it's an end-of-generation token
            if (llama_vocab_is_eog(vocab, new_token) || 
                new_token == eos_token || 
                new_token == eot_token) {
                break;
            }

            // Add token to result list
            generated_tokens->liste.push_back(new_token);

            // Call callback if provided
            if (callback != NULL) {
                callback_eval(lisp, callback, new_token, callback_data);
            }

            // Accept token in sampler
            llama_sampler_accept(model->sampler, new_token);

            // Prepare batch for next token
            batch = llama_batch_get_one(&new_token, 1);

            if (llama_decode(model->ctx, batch)) {
                throw new Error("Error: failed to decode token");
            }
        }

        return generated_tokens;

    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_gguf::free_model(LispE* lisp) {
    // Signature: deflib gguf_free(model)

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_free requires a GGUF model");
    }

    // GGUFModelElementV2 destructor will automatically free resources
    return true_;
}

Element* Lispe_gguf::get_chat_template(LispE* lisp) {
    // Signature: deflib gguf_get_chat_template(model (name))

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_get_chat_template requires a GGUF model");
    }

    GGUFModelElementV2* gguf_model = (GGUFModelElementV2*)model_elem;

    // Optional parameter: template name (e.g., "tool_use")
    Element* name_elem = lisp->get_variable("name");
    const char* tmpl_name = nullptr;
    std::string name_str;
    if (name_elem != null_ && name_elem != NULL && name_elem->isString()) {
        name_str = name_elem->toString(lisp);
        tmpl_name = name_str.c_str();
    }

    try {
        const char* tmpl = llama_model_chat_template(gguf_model->model->model, tmpl_name);
        if (tmpl == nullptr) {
            return null_;
        }
        std::string result(tmpl);
        return lisp->provideString(result);
    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_gguf::apply_chat_template(LispE* lisp) {
    // Signature: deflib gguf_apply_chat_template(model messages (config))
    // messages is a list of dictionaries with "role" and "content" keys
    // config can contain: "add_generation_prompt" (bool), "template" (string)

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_apply_chat_template requires a GGUF model");
    }

    GGUFModelElementV2* gguf_model = (GGUFModelElementV2*)model_elem;
    Element* messages_elem = lisp->get_variable("messages");

    if (!messages_elem->isList()) {
        throw new Error("Error: messages must be a list of dictionaries");
    }

    // Optional configuration
    Element* config_elem = lisp->get_variable("config");
    bool add_generation_prompt = true;
    const char* custom_template = nullptr;
    std::string template_str;

    if (config_elem != null_ && config_elem != NULL && config_elem->isDictionary()) {
        Dictionary* config_dict = (Dictionary*)config_elem;

        // add_generation_prompt
        Element* add_gen_val = config_dict->on_index(U"add_generation_prompt");
        if (add_gen_val) {
            add_generation_prompt = add_gen_val->Boolean();
        }

        // custom template
        Element* tmpl_val = config_dict->on_index(U"template");
        if (tmpl_val && tmpl_val->isString()) {
            template_str = tmpl_val->toString(lisp);
            custom_template = template_str.c_str();
        }
    }

    try {
        // Build llama_chat_message array from LispE list
        std::vector<llama_chat_message> chat_messages;
        std::vector<std::string> roles;    // Keep strings alive
        std::vector<std::string> contents; // Keep strings alive

        long n_messages = messages_elem->size();
        chat_messages.reserve(n_messages);
        roles.reserve(n_messages);
        contents.reserve(n_messages);

        for (long i = 0; i < n_messages; ++i) {
            Element* msg = messages_elem->index(i);
            if (!msg->isDictionary()) {
                throw new Error("Error: each message must be a dictionary with 'role' and 'content'");
            }

            Dictionary* msg_dict = (Dictionary*)msg;
            Element* role_elem = msg_dict->on_index(U"role");
            Element* content_elem = msg_dict->on_index(U"content");

            if (!role_elem || !content_elem) {
                throw new Error("Error: each message must have 'role' and 'content' keys");
            }

            roles.push_back(role_elem->toString(lisp));
            contents.push_back(content_elem->toString(lisp));

            llama_chat_message chat_msg;
            chat_msg.role = roles.back().c_str();
            chat_msg.content = contents.back().c_str();
            chat_messages.push_back(chat_msg);
        }

        // Get template from model if not provided
        const char* tmpl = custom_template;
        if (tmpl == nullptr) {
            tmpl = llama_model_chat_template(gguf_model->model->model, nullptr);
        }

        // First call to get required buffer size
        int32_t res = llama_chat_apply_template(
            tmpl,
            chat_messages.data(),
            chat_messages.size(),
            add_generation_prompt,
            nullptr,
            0
        );

        if (res < 0) {
            throw new Error("Error: failed to apply chat template");
        }

        // Allocate buffer and apply template
        std::vector<char> buf(res + 1);
        int32_t actual_size = llama_chat_apply_template(
            tmpl,
            chat_messages.data(),
            chat_messages.size(),
            add_generation_prompt,
            buf.data(),
            buf.size()
        );

        if (actual_size < 0) {
            throw new Error("Error: failed to apply chat template");
        }

        std::string result(buf.data(), actual_size);
        return lisp->provideString(result);

    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_gguf::chat(LispE* lisp) {
    // Signature: deflib gguf_chat(model messages (config))
    // messages is a list of dictionaries with "role" and "content" keys
    // config can contain:
    //   - Generation options: max_tokens, temperature, top_p, top_k, repeat_penalty, repeat_last_n, callback, data
    //   - Template options: template (custom template string)
    // Returns: the messages list with a new assistant response appended

    Element* model_elem = lisp->get_variable("model");
    if (model_elem->type != t_gguf_model) {
        throw new Error("Error: gguf_chat requires a GGUF model");
    }

    GGUFModelElementV2* gguf_model = (GGUFModelElementV2*)model_elem;
    Element* messages_elem = lisp->get_variable("messages");

    if (!messages_elem->isList()) {
        throw new Error("Error: messages must be a list of dictionaries");
    }

    // Optional configuration
    Element* config_elem = lisp->get_variable("config");

    // Generation options (defaults)
    int max_tokens = 100;
    float temperature = 0.8f;
    float top_p = 0.9f;
    int top_k = 40;
    float repeat_penalty = 1.1f;
    int repeat_last_n = 64;
    Element* callback = NULL;
    Element* callback_data = NULL;

    // Template options
    const char* custom_template = nullptr;
    std::string template_str;

    // Parse configuration dictionary if provided
    if (config_elem != null_ && config_elem != NULL && config_elem->isDictionary()) {
        Dictionary* config_dict = (Dictionary*)config_elem;

        // Generation options
        Element* max_tok_val = config_dict->on_index(U"max_tokens");
        if (max_tok_val && max_tok_val->isNumber()) {
            max_tokens = max_tok_val->asInteger();
        }

        Element* temp_val = config_dict->on_index(U"temperature");
        if (temp_val && temp_val->isNumber()) {
            temperature = temp_val->asFloat();
        }

        Element* top_p_val = config_dict->on_index(U"top_p");
        if (top_p_val && top_p_val->isNumber()) {
            top_p = top_p_val->asFloat();
        }

        Element* top_k_val = config_dict->on_index(U"top_k");
        if (top_k_val && top_k_val->isNumber()) {
            top_k = top_k_val->asInteger();
        }

        Element* repeat_penalty_val = config_dict->on_index(U"repeat_penalty");
        if (repeat_penalty_val && repeat_penalty_val->isNumber()) {
            repeat_penalty = repeat_penalty_val->asFloat();
        }

        Element* repeat_last_n_val = config_dict->on_index(U"repeat_last_n");
        if (repeat_last_n_val && repeat_last_n_val->isNumber()) {
            repeat_last_n = repeat_last_n_val->asInteger();
        }

        callback = config_dict->on_index(U"callback");
        callback_data = config_dict->on_index(U"data");

        // Template option
        Element* tmpl_val = config_dict->on_index(U"template");
        if (tmpl_val && tmpl_val->isString()) {
            template_str = tmpl_val->toString(lisp);
            custom_template = template_str.c_str();
        }
    }

    try {
        auto& model = gguf_model->model;
        const llama_vocab* vocab = model->vocab;

        // ====================================================================
        // Step 1: Build the prompt using the chat template
        // ====================================================================
        std::vector<llama_chat_message> chat_messages;
        std::vector<std::string> roles;
        std::vector<std::string> contents;

        long n_messages = messages_elem->size();
        chat_messages.reserve(n_messages);
        roles.reserve(n_messages);
        contents.reserve(n_messages);

        for (long i = 0; i < n_messages; ++i) {
            Element* msg = messages_elem->index(i);
            if (!msg->isDictionary()) {
                throw new Error("Error: each message must be a dictionary with 'role' and 'content'");
            }

            Dictionary* msg_dict = (Dictionary*)msg;
            Element* role_elem = msg_dict->on_index(U"role");
            Element* content_elem = msg_dict->on_index(U"content");

            if (!role_elem || !content_elem) {
                throw new Error("Error: each message must have 'role' and 'content' keys");
            }

            roles.push_back(role_elem->toString(lisp));
            contents.push_back(content_elem->toString(lisp));

            llama_chat_message chat_msg;
            chat_msg.role = roles.back().c_str();
            chat_msg.content = contents.back().c_str();
            chat_messages.push_back(chat_msg);
        }

        // Get template from model if not provided
        const char* tmpl = custom_template;
        if (tmpl == nullptr) {
            tmpl = llama_model_chat_template(model->model, nullptr);
        }

        // Apply template with add_generation_prompt = true
        int32_t res = llama_chat_apply_template(
            tmpl,
            chat_messages.data(),
            chat_messages.size(),
            true,  // add_generation_prompt
            nullptr,
            0
        );

        if (res < 0) {
            throw new Error("Error: failed to apply chat template");
        }

        std::vector<char> prompt_buf(res + 1);
        llama_chat_apply_template(
            tmpl,
            chat_messages.data(),
            chat_messages.size(),
            true,
            prompt_buf.data(),
            prompt_buf.size()
        );

        std::string prompt(prompt_buf.data(), res);

        // ====================================================================
        // Step 2: Generate the response
        // ====================================================================

        // Clear KV cache before new generation
        llama_memory_t mem = llama_get_memory(model->ctx);
        llama_memory_clear(mem, true);

        // Tokenize the prompt
        int n_prompt = -llama_tokenize(vocab, prompt.c_str(), prompt.size(), NULL, 0, true, true);

        std::vector<llama_token> tokens(n_prompt);
        if (llama_tokenize(vocab, prompt.c_str(), prompt.size(), tokens.data(), tokens.size(), true, true) < 0) {
            throw new Error("Error: failed to tokenize prompt");
        }

        // Get special tokens for EOG detection
        llama_token eos_token = llama_vocab_eos(vocab);
        llama_token eot_token = llama_vocab_eot(vocab);

        // Configure sampler with parameters
        llama_sampler_free(model->sampler);
        auto sparams = llama_sampler_chain_default_params();
        model->sampler = llama_sampler_chain_init(sparams);

        llama_sampler_chain_add(model->sampler, llama_sampler_init_penalties(
            repeat_last_n, repeat_penalty, 0.0f, 0.0f
        ));
        llama_sampler_chain_add(model->sampler, llama_sampler_init_top_k(top_k));
        llama_sampler_chain_add(model->sampler, llama_sampler_init_top_p(top_p, 1));
        llama_sampler_chain_add(model->sampler, llama_sampler_init_temp(temperature));
        llama_sampler_chain_add(model->sampler, llama_sampler_init_dist(LLAMA_DEFAULT_SEED));

        // Process the prompt
        llama_batch batch = llama_batch_get_one(tokens.data(), tokens.size());

        if (llama_decode(model->ctx, batch)) {
            throw new Error("Error: failed to decode prompt");
        }

        // Generate tokens
        std::vector<llama_token> generated_tokens;

        for (int i = 0; i < max_tokens; ++i) {
            llama_token new_token = llama_sampler_sample(model->sampler, model->ctx, -1);

            if (llama_vocab_is_eog(vocab, new_token) ||
                new_token == eos_token ||
                new_token == eot_token) {
                break;
            }

            generated_tokens.push_back(new_token);

            if (callback != NULL) {
                callback_eval(lisp, callback, new_token, callback_data);
            }

            llama_sampler_accept(model->sampler, new_token);
            batch = llama_batch_get_one(&new_token, 1);

            if (llama_decode(model->ctx, batch)) {
                throw new Error("Error: failed to decode token");
            }
        }

        // ====================================================================
        // Step 3: Detokenize the response
        // ====================================================================
        std::string response_text;
        for (llama_token token : generated_tokens) {
            char buf[128];
            int n = llama_token_to_piece(vocab, token, buf, sizeof(buf), 0, true);
            if (n > 0) {
                response_text += std::string(buf, n);
            }
        }

        // ====================================================================
        // Step 4: Create the result - copy messages and append assistant response
        // ====================================================================
        List* result = new List;

        // Copy existing messages
        for (long i = 0; i < n_messages; ++i) {
            result->append(messages_elem->index(i)->copying(false));
        }

        // Create assistant response dictionary
        Dictionary* assistant_msg = new Dictionary;
        u_ustring role_key = U"role";
        u_ustring content_key = U"content";
        u_ustring role_value = U"assistant";

        // Convert response_text to u_ustring
        u_ustring response_ustring;
        for (unsigned char c : response_text) {
            response_ustring += (char32_t)c;
        }

        assistant_msg->recording(role_key, lisp->provideString(role_value));
        assistant_msg->recording(content_key, lisp->provideString(response_ustring));

        result->append(assistant_msg);

        return result;

    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

// Empty callback to disable llama.cpp logs
static void llama_log_callback_silent(enum ggml_log_level level, const char* text, void* user_data) {
    // Do nothing - disables all logs
}

Element* Lispe_gguf::set_log(LispE* lisp) {
    // Signature: deflib gguf_set_log(enable)

    Element* enable_elem = lisp->get_variable("enable");
    bool enable = enable_elem->Boolean();

    if (enable) {
        // Enable logs (llama.cpp default behavior)
        llama_log_set(nullptr, nullptr);
    } else {
        // Disable logs
        llama_log_set(llama_log_callback_silent, nullptr);
    }

    return enable ? true_ : false_;
}

// ============================================================================
// Library initialization function
// ============================================================================

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    // Initialize llama.cpp backend with logs disabled by default
    llama_log_set(llama_log_callback_silent, nullptr);
    llama_backend_init();

    // Allocate type for GGUF models
    string gguf_type_key = "gguf_model";
    t_gguf_model = lisp->encode(gguf_type_key);

    // Register GGUF functions
    lisp->extension("deflib gguf_load(filepath (config))",
                    new Lispe_gguf(gguf_action_load_model));

    lisp->extension("deflib gguf_tokenize(model text)",
                    new Lispe_gguf(gguf_action_tokenize));

    lisp->extension("deflib gguf_detokenize(model tokens)",
                    new Lispe_gguf(gguf_action_detokenize));

    lisp->extension("deflib gguf_generate(model prompt (config))",
                    new Lispe_gguf(gguf_action_generate));

    lisp->extension("deflib gguf_free(model)",
                    new Lispe_gguf(gguf_action_free_model));

    lisp->extension("deflib gguf_set_log(enable)",
                    new Lispe_gguf(gguf_action_set_log));

    lisp->extension("deflib gguf_get_chat_template(model (name))",
                    new Lispe_gguf(gguf_action_get_chat_template));

    lisp->extension("deflib gguf_apply_chat_template(model messages (config))",
                    new Lispe_gguf(gguf_action_apply_chat_template));

    lisp->extension("deflib gguf_chat(model messages (config))",
                    new Lispe_gguf(gguf_action_chat));

    return true;
}
}

