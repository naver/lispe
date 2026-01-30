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

// ==============================================================================
// Intégration JIT-LoRA pour lispe_lispetorch.cxx
// Code à ajouter dans le fichier principal
// ==============================================================================


Element* Lispe_lispetorch::jit_model_get_buffer(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    std::string buffer_name = lisp->get_variable("buffername")->toString(lisp);
    
    try {
        torch::Tensor tensor = jit_model->get_buffer(buffer_name);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        // Essayer avec get_attribute comme fallback
        try {
            torch::Tensor tensor = jit_model->get_attribute(buffer_name);
            return new TorchTensor(tensor);
        } catch (...) {
            throw new Error("Error: " + std::string(e.what()));
        }
    }
}

Element *Lispe_lispetorch::jit_load(LispE *lisp)
{
    // Retrieve the file path argument using LispE's convention
    std::string model_path = lisp->get_variable(L"model_path")->toString(lisp);
    std::string device_str = lisp->get_variable(L"device")->toString(lisp);

    TorchJITModel *jit_model = new TorchJITModel();

    // Charger le modèle
    if (!jit_model->load_from_file(model_path))
    {
        delete jit_model;
        throw new Error("Error: Failed to load TorchScript model from " + model_path);
    }

    // Optionnel : déplacer vers le meilleur device disponible

    try
    {
        torch::Device target_device(device_str);
        jit_model->move_to_device(target_device);
    }
    catch (...)
    {
        std::cerr << "Warning: Invalid device " << device_str << ", using CPU" << std::endl;
    }

    return jit_model;
}

Element* Lispe_lispetorch::jit_model_get_tensor(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    std::string tensor_name = lisp->get_variable(L"tensorname")->toString(lisp);
    
    try {
        torch::Tensor tensor = jit_model->get_tensor(tensor_name);
        return new TorchTensor(tensor);
    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_get_tensor_shape(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    std::string tensor_name = lisp->get_variable(L"tensorname")->toString(lisp);
    
    try {
        std::vector<int64_t> shape = jit_model->get_tensor_shape(tensor_name);
        Integers* result = lisp->provideIntegers();
        for (auto dim : shape) {
            result->liste.push_back(dim);
        }
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_list_buffers(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    auto buffer_names = jit_model->get_buffer_names();
    Strings* result = lisp->provideStrings();
    u_ustring value;
    string n;
    for (const auto& name : buffer_names) {
        value = U"";
        n = name;
        s_utf8_to_unicode(value, n, n.size());
        result->liste.push_back(value);
    }
    
    return result;
}

Element* Lispe_lispetorch::jit_model_info(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    // Retourner un dictionnaire avec les informations
    Dictionary* info = lisp->provideDictionary();
    u_ustring key;
    key = U"loaded";
    info->recording(key, jit_model->Boolean() ? True_ : False_);
    key = U"device";
    string device = jit_model->device.str();
    info->recording(key, lisp->provideString(device));
    key = U"parameter_count";
    info->recording(key, lisp->provideNumber(jit_model->parameter_count()));
    key = U"memory_mb";
    info->recording(key, lisp->provideNumber(jit_model->memory_usage() / (1024 * 1024)));
    
    return info;
}

Element* Lispe_lispetorch::jit_model_forward(LispE* lisp) {
    
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    TorchTensor* input_tensor = dynamic_cast<TorchTensor*>(lisp->get_variable(L"tensor"));
    if (!input_tensor) {
        throw new Error("Error: Second argument must be a tensor");
    }
    
    try {
        torch::Tensor output = jit_model->forward(input_tensor->tensor);
        return new TorchTensor(output);
    } catch (const std::exception& e) {
        throw new Error("Error during forward pass: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_unload(LispE* lisp) {
    Element* model = lisp->get_variable(L"model");

    if (model->type != t_jit_model)
        throw new Error("Error: First argument must be a TorchJIT model");

    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(model);

    jit_model->unload_model();
    return True_;
}

Element* Lispe_lispetorch::jit_model_to_mps(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    try {
        jit_model->move_to_mps();
        return True_;
    } catch (const std::exception& e) {
        throw new Error("Error moving model to MPS: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_to_best_device(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));

    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    try {
        jit_model->move_to_best_device();
        return True_;
    } catch (const std::exception& e) {
        throw new Error("Error moving model to best device: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_to_device(LispE* lisp) {
    TorchJITModel* jit_model = dynamic_cast<TorchJITModel*>(lisp->get_variable(L"model"));
    if (!jit_model) {
        throw new Error("Error: First argument must be a TorchJIT model");
    }
    
    std::string device_str = lisp->get_variable("device")->toString(lisp);
    
    try {
        torch::Device device(device_str);
        jit_model->move_to_device(device);
        return True_;
    } catch (const std::exception& e) {
        throw new Error("Error moving model to device: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_register_lora_hook(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* layer_name_elem = lisp->get_variable(L"layer_name");
    Element* lora_elem = lisp->get_variable(L"lora_layer");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("First argument must be a JIT model");
        }
        if (lora_elem->type != t_lora_linear) {
            throw new Error("Third argument must be a LoRA linear layer");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        TorchLoRALinear* lora = (TorchLoRALinear*)lora_elem;
        std::string layer_name = layer_name_elem->toString(lisp);
        
        jit_model->register_lora_hook(layer_name, lora->lora_layer);
        
        return True_;
        
    } catch (const std::exception& e) {
        throw new Error("Error registering LoRA hook: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_forward_with_lora(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* input_elem = lisp->get_variable(L"input");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("First argument must be a JIT model");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Second argument must be a tensor");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        TorchTensor* input_tensor = (TorchTensor*)input_elem;
        
        auto output = jit_model->forward_with_lora_hooks(input_tensor->tensor);
        return new TorchTensor(output);
        
    } catch (const std::exception& e) {
        throw new Error("Error in LoRA forward pass: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_get_intermediate_states(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* input_elem = lisp->get_variable(L"input");
    Element* layer_names_elem = lisp->get_variable(L"layer_names");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("First argument must be a JIT model");
        }
        if (input_elem->type != t_tensor) {
            throw new Error("Second argument must be a tensor");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        TorchTensor* input_tensor = (TorchTensor*)input_elem;
        
        // Convertir les noms de couches
        std::vector<std::string> layer_names;
        if (layer_names_elem->isList()) {
            for (long i = 0; i < layer_names_elem->size(); i++) {
                layer_names.push_back(layer_names_elem->index(i)->toString(lisp));
            }
        }
        
        auto states = jit_model->get_intermediate_states(input_tensor->tensor, layer_names);
        
        // Retourner une liste de tensors
        List* result = lisp->provideList();
        for (const auto& state : states) {
            result->append(new TorchTensor(state));
        }
        
        return result;
        
    } catch (const std::exception& e) {
        throw new Error("Error getting intermediate states: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_list_tensor_names(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("Argument must be a JIT model");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        auto names = jit_model->get_all_tensor_names();
        
        Strings* result = lisp->provideStrings();
        u_ustring u_name;            
        for (auto& name : names) {
            u_name = U"";
            s_utf8_to_unicode(u_name, name, name.size());
            result->liste.push_back(u_name);
        }
        
        return result;
        
    } catch (const std::exception& e) {
        throw new Error("Error listing tensor names: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_list_parameter_names(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("Argument must be a JIT model");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        auto names = jit_model->get_parameter_names();
        
        Strings* result = lisp->provideStrings();
        u_ustring u_name;            
        for (auto& name : names) {
            u_name = U"";
            s_utf8_to_unicode(u_name, name, name.size());
            result->liste.push_back(u_name);
        }
        
        return result;
        
    } catch (const std::exception& e) {
        throw new Error("Error listing parameter names: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_list_methods(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("Argument must be a JIT model");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        auto names = jit_model->get_method_names();
        
        // Debug: affichons aussi des informations sur la structure du modèle
        //affichage << "Debug: Modèle TorchScript chargé" << std::endl;
        //affichage << "Debug: Nombre de méthodes trouvées: " << names.size() << std::endl;
        
        // Essayons d'autres approches pour obtenir des informations
        try {
            auto graph = jit_model->model.get_method("forward").graph();
            //affichage << "Debug: Méthode 'forward' trouvée dans le modèle" << std::endl;
        } catch (...) {
            //affichage << "Debug: Méthode 'forward' non trouvée directement" << std::endl;
        }
        
        // Listons les attributs du module
        auto attributes = jit_model->model.named_attributes();
        //affichage << "Debug: Attributs du module:" << std::endl;
        int count = 0;
        for (const auto& attr : attributes) {
            //affichage << "  - " << attr.name;
            if (attr.name == "name_mapping") {
                //affichage << " (TROUVÉ - contient possiblement les informations de mapping!)";
                // Essayons de lire le contenu du name_mapping
                try {
                    auto mapping_value = attr.value;
                    //affichage << " Type: " << mapping_value.type()->str();
                } catch (...) {
                    //affichage << " (impossible de lire le type)";
                }
            }
            //affichage << std::endl;
            count++;
            if (count > 20) {
                //affichage << "  ... (et " << (attributes.size() - 20) << " autres attributs)" << std::endl;
                break;
            }
        }
        
        Strings* result = lisp->provideStrings();
        u_ustring u_name;            
        for (auto& name : names) {
            u_name = U"";
            s_utf8_to_unicode(u_name, name, name.size());
            result->liste.push_back(u_name);
        }
        
        return result;
        
    } catch (const std::exception& e) {
        throw new Error("Error listing method names: " + std::string(e.what()));
    }
}

Element* Lispe_lispetorch::jit_model_update_weight(LispE* lisp) {
    Element* model_elem = lisp->get_variable(L"model");
    Element* param_name_elem = lisp->get_variable(L"param_name");
    Element* new_weight_elem = lisp->get_variable(L"new_weight");
    
    try {
        if (model_elem->type != t_jit_model) {
            throw new Error("First argument must be a JIT model");
        }
        if (new_weight_elem->type != t_tensor) {
            throw new Error("Third argument must be a tensor");
        }
        
        TorchJITModel* jit_model = (TorchJITModel*)model_elem;
        TorchTensor* new_weight = (TorchTensor*)new_weight_elem;
        std::string param_name = param_name_elem->toString(lisp);
        
        jit_model->update_weight(param_name, new_weight->tensor);
        
        return True_;
        
    } catch (const std::exception& e) {
        throw new Error("Error updating weight: " + std::string(e.what()));
    }
}

bool TorchJITModel::load_from_file(const std::string& path) {
    try {
        // Libérer le modèle précédent si chargé
        if (is_loaded) {
            unload_model();
        }
        
        model_path = path;
        model = torch::jit::load(path, device);
        model.eval(); // Mode évaluation par défaut
        
        // Charger le mapping des noms si disponible
        std::string mapping_file = path;
        size_t pos = mapping_file.find_last_of('.');
        if (pos != std::string::npos) {
            mapping_file = mapping_file.substr(0, pos) + "_name_mapping.json";
        }
        load_name_mapping(mapping_file);
        
        is_loaded = true;
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "Error loading TorchScript model: " << e.what() << std::endl;
        is_loaded = false;
        return false;
    }
}

bool TorchJITModel::load_from_buffer(const std::vector<char>& buffer) {
    try {
        if (is_loaded) {
            unload_model();
        }
        
        std::istringstream stream(std::string(buffer.data(), buffer.size()));
        model = torch::jit::load(stream, device);
        model.eval();
        
        is_loaded = true;
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "Error loading TorchScript model from buffer: " << e.what() << std::endl;
        is_loaded = false;
        return false;
    }
}

void TorchJITModel::unload_model() {
    if (is_loaded) {
        // Forcer la libération de la mémoire
        model = torch::jit::script::Module();
        name_mapping.clear();
        
        // Clear CUDA cache if using CUDA
        if (device.is_cuda()) {
            #ifdef USE_CUDA
            c10::cuda::CUDACachingAllocator::emptyCache();
            #endif
        }
        
        is_loaded = false;
        model_path.clear();
    }
}

// Nouvelle méthode pour déplacer spécifiquement vers MPS
void TorchJITModel::move_to_mps() {
    if (torch::mps::is_available()) {
        move_to_device(torch::kMPS);
    } else {
        std::cerr << "Warning: MPS not available, staying on current device" << std::endl;
    }
}

// Méthode pour détecter et utiliser le meilleur device disponible
torch::Device TorchJITModel::get_best_device() {
    if (torch::cuda::is_available()) {
        return torch::kCUDA;
    } else if (torch::mps::is_available()) {
        return torch::kMPS;
    } else {
        return torch::kCPU;
    }
}

void TorchJITModel::move_to_best_device() {
    torch::Device best_device = get_best_device();
    move_to_device(best_device);
}

void TorchJITModel::move_to_device(torch::Device target_device) {
    if (!is_loaded) return;
    
    try {
        if (device != target_device) {
            model.to(target_device);
            device = target_device;
        }
    } catch (const std::exception& e) {
        throw new Error("Error: " + std::string(e.what()));
    }
}

torch::Tensor TorchJITModel::get_tensor(const std::string& name) {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    try {
        // Essayer d'abord avec le nom original
        std::string safe_name = original_to_safe_name(name);
        
        // Accéder via les named_parameters
        auto named_params = model.named_parameters();
        for (const auto& param : named_params) {
            if (param.name == safe_name || param.name == name) {
                return param.value;
            }
        }
        
        throw new Error("Parameter not found: " + name);
        
    } catch (const std::exception& e) {
        throw new Error("Error accessing tensor " + name + ": " + e.what());
    }
}

torch::Tensor TorchJITModel::get_buffer(const std::string& name) {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    try {
        std::string safe_name = original_to_safe_name(name);
        
        // Accéder via les named_buffers
        auto named_buffers = model.named_buffers();
        for (const auto& buffer : named_buffers) {
            if (buffer.name == safe_name || buffer.name == name) {
                return buffer.value;
            }
        }
        
        throw new Error("Buffer not found: " + name);
        
    } catch (const std::exception& e) {
        throw new Error("Error accessing buffer " + name + ": " + e.what());
    }
}

std::vector<int64_t> TorchJITModel::get_tensor_shape(const std::string& name) {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    try {
        std::string safe_name = original_to_safe_name(name);
        
        // Accéder via les named_parameters pour obtenir la forme
        auto named_params = model.named_parameters();
        for (const auto& param : named_params) {
            if (param.name == safe_name || param.name == name) {
                auto sizes = param.value.sizes();
                std::vector<int64_t> shape;
                for (auto size : sizes) {
                    shape.push_back(size);
                }
                return shape;
            }
        }
        
        // Essayer aussi avec les buffers
        auto named_buffers = model.named_buffers();
        for (const auto& buffer : named_buffers) {
            if (buffer.name == safe_name || buffer.name == name) {
                auto sizes = buffer.value.sizes();
                std::vector<int64_t> shape;
                for (auto size : sizes) {
                    shape.push_back(size);
                }
                return shape;
            }
        }
        
        throw new Error("Parameter not found: " + name);
        
    } catch (const std::exception& e) {
        throw new Error("Error getting tensor shape " + name + ": " + e.what());
    }
}

std::vector<std::string> TorchJITModel::get_buffer_names() {
    if (!is_loaded) {
        return {};
    }
    
    std::vector<std::string> names;
    auto named_buffers = model.named_buffers();
    
    for (const auto& buffer : named_buffers) {
        names.push_back(buffer.name);
    }
    
    return names;
}

std::vector<std::string> TorchJITModel::get_tensor_names() {
    if (!is_loaded) {
        return {};
    }
    
    std::vector<std::string> names;
    auto named_parameters = model.named_parameters();
    
    for (const auto& param : named_parameters) {
        names.push_back(param.name);
    }
    
    return names;
}

std::vector<std::string> TorchJITModel::get_parameter_names() {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    std::vector<std::string> names;
    auto named_parameters = model.named_parameters();
    
    for (const auto& param : named_parameters) {
        names.push_back(param.name);
    }
    
    return names;
}

std::vector<std::string> TorchJITModel::get_method_names() {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    std::vector<std::string> names;
    
    try {
        // Obtenir toutes les méthodes du module
        auto methods = model.get_methods();
        for (const auto& method : methods) {
            names.push_back(method.name());
        }
    } catch (const std::exception& e) {
        std::cerr << "Warning: Could not get method names: " << e.what() << std::endl;
    }
    
    return names;
}

// Méthode alternative pour accéder directement aux attributs du module
torch::Tensor TorchJITModel::get_attribute(const std::string& name) {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    try {
        std::string safe_name = original_to_safe_name(name);
        
        // Essayer d'accéder comme attribut du module
        if (model.hasattr(safe_name)) {
            auto attr = model.attr(safe_name);
            if (attr.isTensor()) {
                return attr.toTensor();
            }
        }
        
        // Essayer avec le nom original
        if (model.hasattr(name)) {
            auto attr = model.attr(name);
            if (attr.isTensor()) {
                return attr.toTensor();
            }
        }
        
        throw new Error("Attribute not found or not a tensor: " + name);
        
    } catch (const std::exception& e) {
        throw new Error("Error accessing attribute " + name + ": " + e.what());
    }
}

// Méthode pour lister tous les attributs disponibles
// Méthode pour lister tous les attributs disponibles
std::vector<std::string> TorchJITModel::get_attribute_names() {
    if (!is_loaded) {
        return {};
    }
    
    std::vector<std::string> names;
    
    // Combiner buffers et parameters
    auto buffer_names = get_buffer_names();
    auto param_names = get_tensor_names();
    
    names.insert(names.end(), buffer_names.begin(), buffer_names.end());
    names.insert(names.end(), param_names.begin(), param_names.end());
    
    return names;
}

torch::Tensor TorchJITModel::forward(const std::vector<torch::Tensor>& inputs) {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    //affichage << "Debug: Début forward avec " << inputs.size() << " tenseurs d'entrée" << std::endl;
    
    try {
        std::vector<torch::jit::IValue> jit_inputs;
        for (const auto& tensor : inputs) {
            jit_inputs.emplace_back(tensor);
            //affichage << "Debug: Tenseur d'entrée shape: [";
            //for (int i = 0; i < tensor.dim(); i++) {
                //affichage << tensor.size(i);
                //if (i < tensor.dim() - 1) affichage << ", ";
            //}
            //affichage << "]" << std::endl;
        }
        
        // Nouvelle stratégie : Essayons d'interroger le modèle pour connaître ses méthodes
        //affichage << "Debug: Recherche des méthodes disponibles..." << std::endl;
        
        // Essayons de lister toutes les méthodes pour voir ce qui est disponible
        auto methods = model.get_methods();
        //affichage << "Debug: Méthodes trouvées : " << methods.size() << std::endl;
        for (const auto& method : methods) {
            //affichage << "Debug: Méthode disponible: " << method.name() << std::endl;
        }
        
        // Essayons des noms de méthodes alternatifs courants pour Llama
        std::vector<std::string> method_names = {"forward", "__call__", "predict", "generate", "inference", "run"};
        
        for (const auto& method_name : method_names) {
            try {
                //affichage << "Debug: Tentative méthode: " << method_name << std::endl;
                auto method = model.get_method(method_name);
                torch::jit::IValue output = method(jit_inputs);
                //affichage << "Debug: Succès avec méthode: " << method_name << std::endl;
                return output.toTensor();
            } catch (const std::exception& e) {
                //affichage << "Debug: Méthode " << method_name << " échouée: " << e.what() << std::endl;
            }
        }
        
        // Si aucune méthode ne fonctionne, donnons un message d'erreur informatif
        throw new Error("Ce modèle TorchScript ne semble pas avoir de méthode d'inférence standard accessible. Méthodes disponibles: " + std::to_string(methods.size()));
        
    } catch (const std::exception& e) {
        throw new Error("Forward pass failed: " + std::string(e.what()));
    }
}

torch::Tensor TorchJITModel::forward(torch::Tensor input) {
    return forward(std::vector<torch::Tensor>{input});
}

size_t TorchJITModel::memory_usage() {
    if (!is_loaded) return 0;
    
    size_t total_size = 0;
    auto named_parameters = model.named_parameters();
    
    for (const auto& param : named_parameters) {
        total_size += param.value.numel() * param.value.element_size();
    }
    
    auto named_buffers = model.named_buffers();
    for (const auto& buffer : named_buffers) {
        total_size += buffer.value.numel() * buffer.value.element_size();
    }
    
    return total_size;
}

int64_t TorchJITModel::parameter_count() {
    if (!is_loaded) return 0;
    
    int64_t count = 0;
    auto named_parameters = model.named_parameters();
    
    for (const auto& param : named_parameters) {
        count += param.value.numel();
    }
    
    return count;
}

u_ustring TorchJITModel::asUString(LispE* lisp) {
    if (!is_loaded) {
        return U"<TorchJITModel: Not loaded>";
    }
    
    std::stringstream strm;
    strm << "=== TorchScript Model Info ===" << std::endl;
    strm << "Device: " << device << std::endl;
    strm << "Parameter count: " << parameter_count() << std::endl;
    strm << "Memory usage: " << memory_usage() / (1024 * 1024) << " MB" << std::endl;
    
    strm << "\nBuffers:" << std::endl;
    auto buffer_names = get_buffer_names();
    for (const auto& name : buffer_names) {
        try {
            auto buffer = get_buffer(name);
            strm << "  " << name << ": " << buffer.sizes() << std::endl;
        } catch (...) {
            strm << "  " << name << ": <error accessing>" << std::endl;
        }
    }
    u_ustring u;
    std::string s = strm.str();
    s_utf8_to_unicode(u, s, s.size());
    return u;
}


void TorchJITModel::optimize_memory() {
    if (!is_loaded) return;
    
    // Optimisations pour réduire l'usage mémoire
    model.eval(); // S'assurer qu'on est en mode eval
    
    if (device.is_cuda()) {
        // Vider le cache CUDA
        #ifdef USE_CUDA
        c10::cuda::CUDACachingAllocator::emptyCache();
        #endif
    }
    
    // On pourrait aussi congeler le modèle si supporté
    try {
        model = torch::jit::freeze(model);
    } catch (...) {
        // Freeze non supporté, continuer sans
    }
}

void TorchJITModel::clear_cache() {
    if (device.is_cuda()) {
        #ifdef USE_CUDA
        c10::cuda::CUDACachingAllocator::emptyCache();
        #endif
    }
}

void TorchJITModel::load_name_mapping(const std::string& mapping_file) {
    // Implémentation pour charger le fichier JSON de mapping des noms
    // (nécessiterait une lib JSON comme nlohmann/json)
    // Pour l'instant, on peut ignorer si le fichier n'existe pas
}

std::string TorchJITModel::original_to_safe_name(const std::string& original_name) {
    auto it = name_mapping.find(original_name);
    if (it != name_mapping.end()) {
        return it->second;
    }
    
    // Conversion par défaut : remplacer . et / par _
    std::string safe_name = original_name;
    std::replace(safe_name.begin(), safe_name.end(), '.', '_');
    std::replace(safe_name.begin(), safe_name.end(), '/', '_');
    std::replace(safe_name.begin(), safe_name.end(), '-', '_');
    
    return safe_name;
}

// 2. EXTENSIONS DE LA CLASSE TorchJITModel
// À ajouter dans la section des méthodes de TorchJITModel

void TorchJITModel::register_lora_hook(const std::string& layer_name, 
                                      std::shared_ptr<LoRALinear> lora) {
    if (!is_loaded) {
        throw new Error("Model must be loaded before registering hooks");
    }
    
    // Vérifier que le paramètre existe
    if (!has_parameter(layer_name)) {
        throw new Error("Parameter not found: " + layer_name);
    }
    
    lora_hooks[layer_name] = lora;
    hooks_enabled = true;
    
    //affichage << "LoRA hook registered for: " << layer_name << std::endl;
}

void TorchJITModel::remove_lora_hook(const std::string& layer_name) {
    auto it = lora_hooks.find(layer_name);
    if (it != lora_hooks.end()) {
        lora_hooks.erase(it);
    }
}

void TorchJITModel::clear_all_lora_hooks() {
    lora_hooks.clear();
    hooks_enabled = false;
}

torch::Tensor TorchJITModel::forward_with_lora_hooks(torch::Tensor input) {
    if (!is_loaded) {
        throw new Error("Model not loaded");
    }
    
    if (!hooks_enabled || lora_hooks.empty()) {
        return forward(input);
    }
    
    // Vérifier si le modèle supporte la modification de poids (modèles non-TorchScript)
    auto named_params = model.named_parameters();
    bool supports_weight_updates = false;
    
    // Vérifier s'il y a des paramètres disponibles
    for (const auto& param : named_params) {
        supports_weight_updates = true;
        break;
    }
    
    if (supports_weight_updates) {
        // Approche fusion temporaire des poids (modèles PyTorch natifs)
        auto backup = backup_weights(get_hook_parameter_names());
        
        try {
            // Appliquer temporairement les corrections LoRA
            for (const auto& [layer_name, lora] : lora_hooks) {
                auto original_weight = get_tensor(layer_name);
                auto delta = lora->compute_lora_delta();
                update_weight(layer_name, original_weight + delta);
            }
            
            // Forward pass avec poids modifiés
            auto result = forward(input);
            
            // Restaurer les poids originaux
            restore_weights(backup);
            
            return result;
            
        } catch (...) {
            // S'assurer de restaurer les poids en cas d'erreur
            restore_weights(backup);
            throw;
        }
    } else {
        // Approche alternative pour modèles TorchScript
        // Pour l'instant, faire un forward normal avec avertissement
        std::cerr << "Warning: LoRA fusion not supported for TorchScript models, using regular forward" << std::endl;
        return forward(input);
    }
}

std::vector<torch::Tensor> TorchJITModel::get_intermediate_states(
    torch::Tensor input, const std::vector<std::string>& layer_names) {
    
    // Note: Cette implémentation est simplifiée
    // Une vraie implémentation nécessiterait d'intercepter le forward pass
    std::vector<torch::Tensor> states;
    
    // Pour l'instant, on retourne juste les états d'entrée et de sortie
    states.push_back(input);
    auto output = forward_with_lora_hooks(input);
    states.push_back(output);
    
    return states;
}

torch::Tensor TorchJITModel::forward_single_layer(torch::Tensor input, 
                                                 const std::string& layer_name) {
    // Implémentation simplifiée - nécessiterait accès aux sous-modules
    throw new Error("forward_single_layer not yet implemented - requires TorchScript module introspection");
}

std::unordered_map<std::string, torch::Tensor> TorchJITModel::backup_weights(
    const std::vector<std::string>& layer_names) {
    
    std::unordered_map<std::string, torch::Tensor> backup;
    
    for (const auto& name : layer_names) {
        try {
            auto tensor = get_tensor(name);
            backup[name] = tensor.clone();
        } catch (...) {
            std::cerr << "Warning: Could not backup weight " << name << std::endl;
        }
    }
    
    return backup;
}

void TorchJITModel::restore_weights(
    const std::unordered_map<std::string, torch::Tensor>& backup) {
    
    for (const auto& [name, tensor] : backup) {
        try {
            update_weight(name, tensor);
        } catch (...) {
            std::cerr << "Warning: Could not restore weight " << name << std::endl;
        }
    }
}

void TorchJITModel::update_weight(const std::string& param_name, 
                                 const torch::Tensor& new_weight) {
    // Pour les modèles TorchScript, la modification des poids n'est pas supportée
    // de la même manière que les modèles PyTorch natifs
    try {
        // Essayer d'abord l'approche named_parameters
        auto named_params = model.named_parameters();
        
        bool found = false;
        for (const auto& param : named_params) {
            if (param.name == param_name) {
                param.value.data().copy_(new_weight);
                found = true;
                break;
            }
        }
        
        if (!found) {
            // Vérifier si named_parameters est vide (modèles TorchScript)
            bool has_params = false;
            for (const auto& param : named_params) {
                has_params = true;
                break;
            }
            
            if (!has_params) {
                // Modèle TorchScript, ignorer silencieusement
                std::cerr << "Warning: Weight update not supported for TorchScript models: " 
                          << param_name << std::endl;
                return;
            }
            
            throw new Error("Parameter not found for update: " + param_name);
        }
        
    } catch (const std::exception& e) {
        std::cerr << "Warning: Could not update weight " << param_name 
                  << ": " << e.what() << std::endl;
    }
}

std::vector<std::string> TorchJITModel::get_all_tensor_names() {
    std::vector<std::string> names;
    
    // Combiner paramètres et buffers
    auto param_names = get_tensor_names();
    auto buffer_names = get_buffer_names();
    
    names.insert(names.end(), param_names.begin(), param_names.end());
    names.insert(names.end(), buffer_names.begin(), buffer_names.end());
    
    return names;
}

bool TorchJITModel::has_parameter(const std::string& name) {
    // Pour les modèles TorchScript, utiliser la même logique que get_tensor_names
    // au lieu de named_parameters() qui ne fonctionne pas avec les modèles compilés
    if (!is_loaded) {
        return false;
    }
    
    try {
        auto tensor_names = get_all_tensor_names();
        for (const auto& tensor_name : tensor_names) {
            if (tensor_name == name) {
                return true;
            }
        }
        return false;
    } catch (...) {
        return false;
    }
}

std::vector<std::string> TorchJITModel::get_hook_parameter_names() {
    std::vector<std::string> names;
    for (const auto& [layer_name, lora] : lora_hooks) {
        names.push_back(layer_name);
    }
    return names;
}
