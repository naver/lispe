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

torch::Tensor lispe_to_tensor(Element* element);
Element* TorchTensor::plus_direct(LispE *lisp, Element *e)
{
    if (e->isScalar())
    {
        double scalar_value = e->asNumber();

        return new TorchTensor(std::move(tensor + scalar_value));
    }
    if (e->isValueList())
    {
        try
        {
            return new TorchTensor(std::move(tensor + lispe_to_tensor(e)));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    if (e->type == t_tensor)
    {
        try
        {
            TorchTensor *t2 = (TorchTensor *)e;
            return new TorchTensor(std::move(tensor + t2->tensor));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    throw new Error("Error: Both arguments must be tensors");
}

Element* TorchTensor::minus_direct(LispE* lisp, Element* e) {
    if (e->isScalar())
    {
        double scalar_value = e->asNumber();
        return new TorchTensor(std::move(tensor - scalar_value));
    }
    if (e->isValueList())
    {
        try
        {
            return new TorchTensor(std::move(tensor - lispe_to_tensor(e)));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    if (e->type == t_tensor)
    {
        try
        {
            TorchTensor *t2 = (TorchTensor *)e;
            return new TorchTensor(std::move(tensor - t2->tensor));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    throw new Error("Error: Both arguments must be tensors");
}

Element *TorchTensor::multiply_direct(LispE *lisp, Element *e)
{
    if (e->isScalar())
    {
        double scalar_value = e->asNumber();
        return new TorchTensor(std::move(tensor * scalar_value));
    }
    if (e->isValueList())
    {
        try
        {
            return new TorchTensor(std::move(tensor * lispe_to_tensor(e)));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }

    if (e->type == t_tensor)
    {
        try
        {
            TorchTensor *t2 = (TorchTensor *)e;
            return new TorchTensor(std::move(tensor * t2->tensor));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    throw new Error("Error: Both arguments must be tensors");
}

Element* TorchTensor::divide_direct(LispE* lisp, Element* e) {
    if (e->isScalar())
    {
        double scalar_value = e->asNumber();
        if (scalar_value == 0)
            throw new Error("Error: division by zero");
        return new TorchTensor(std::move(tensor / scalar_value));
    }
    if (e->isValueList()) {
        try
        {
            
            return new TorchTensor(std::move(tensor / lispe_to_tensor(e)));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    if (e->type == t_tensor)
    {
        try
        {
            TorchTensor *t2 = (TorchTensor *)e;
            return new TorchTensor(std::move(tensor / t2->tensor));
        }
        catch (const std::exception &e)
        {
            string msg = "Error: ";
            msg += e.what();
            throw new Error(msg);
        }
    }
    throw new Error("Error: Both arguments must be tensors");
}

Element* TorchTensor::extraction(LispE* lisp, List* l) {
    try {
        // Récupérer les indices de début et de fin
        long start, end;
        l->evalAsInteger(2, lisp, start);
        l->evalAsInteger(3, lisp, end);

        long sz = tensor.numel();
        // Gestion des indices négatifs
        if (start < 0) {
            start += sz;
        }
        if (end < 0) {
            end += sz;
        }

        // Vérification des limites
        if (start < 0 || start >= sz || end < 0 || end > sz || start >= end) {
            throw new Error("Error: indices out of bounds or invalid range");
        }

        // Extraction du sous-tenseur
        torch::Tensor result = tensor.narrow(0, start, end);
        return new TorchTensor(result);
    } catch (const std::exception& e) {
        string msg = "Error in tensor extraction: ";
        msg += e.what();
        throw new Error(msg);
    }
}

wstring TorchTensor::asString(LispE* lisp) {
    std::ostringstream oss;
    oss << tensor;
    std::string str = oss.str();
    return std::wstring(str.begin(), str.end());
}

Element* TorchTensor::car(LispE* lisp) {
    try {
        // Si le tensor est vide ou scalaire, retourner null
        if (tensor.dim() == 0 || tensor.numel() == 0) {
            return null_;
        }
        
        // Si le tensor est 1D, retourner le premier élément comme scalaire
        if (tensor.dim() == 1) {
            torch::Tensor first_element = tensor[0];
            return new TorchTensor(first_element);
        }
        
        // Pour les tenseurs multi-dimensionnels, retourner la première "ligne" 
        torch::Tensor first_slice = tensor[0];
        return new TorchTensor(first_slice);
        
    } catch (const std::exception& e) {
        string msg = "Error in tensor car: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* TorchTensor::cdr(LispE* lisp) {
    try {
        // Si le tensor est vide, scalaire ou n'a qu'un élément, retourner null
        if (tensor.dim() == 0 || tensor.numel() <= 1) {
            return null_;
        }
        
        // Si le tensor est 1D avec plus d'un élément, retourner tout sauf le premier
        if (tensor.dim() == 1) {
            if (tensor.size(0) <= 1) {
                return null_;
            }
            torch::Tensor rest = tensor.slice(0, 1);
            return new TorchTensor(rest);
        }
        
        // Pour les tenseurs multi-dimensionnels, retourner tout sauf la première "ligne"
        if (tensor.size(0) <= 1) {
            return null_;
        }
        torch::Tensor rest = tensor.slice(0, 1);
        return new TorchTensor(rest);
        
    } catch (const std::exception& e) {
        string msg = "Error in tensor cdr: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* TorchTensor::protected_index(LispE* lisp, Element* ix) {
    try {
        long i = ix->checkInteger(lisp);
        
        // Support des indices négatifs (comme en Python)
        if (i < 0) {
            i = tensor.numel() + i;
        }
        
        // Vérification des limites
        if (i < 0 || i >= tensor.numel()) {
            throw new Error("Error: tensor index out of bounds");
        }
        
        // Accès à l'élément du tenseur
        torch::Tensor element = tensor.flatten()[i];
        
        // Retourner un scalaire LispE ou un nouveau tenseur selon le cas
        if (element.dim() == 0) {
            // C'est un scalaire, on retourne la valeur
            if (tensor.dtype() == torch::kFloat32) {
                if (current != NULL) {
                    if (current->type == t_float) {
                        ((Float*)current)->content = element.item<float>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Float(element.item<double>());
            } else if (tensor.dtype() == torch::kFloat64) {
                if (current != NULL) {
                    if (current->type == t_number) {
                        ((Number*)current)->content = element.item<double>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideNumber(element.item<double>());
            } else if (tensor.dtype() == torch::kInt32) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<int>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideInteger(element.item<int>());
            } else if (tensor.dtype() == torch::kInt64) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<int64_t>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideInteger(element.item<int64_t>());
            } else if (tensor.dtype() == torch::kBool) {
                return booleans_[element.item<bool>()];
            } else {
                // Pour les autres types, retourner la valeur comme float
                if (current != NULL) {
                    if (current->type == t_number) {
                        ((Number*)current)->content = element.item<double>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideNumber(element.item<double>());
            }
        } else {
            // C'est un tenseur, on retourne un TorchTensor
            if (current != NULL) {
                current->decrement();
                current = NULL;
            }

            current = new TorchTensor(element);
        }
        
        current->increment();
        return current;        
    } catch (const std::exception& e) {
        string msg = "Error accessing tensor element: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* TorchTensor::protected_index(LispE* lisp, long i) {
    try {
        
        // Support des indices négatifs (comme en Python)
        if (i < 0) {
            i = tensor.numel() + i;
        }
        
        // Vérification des limites
        if (i < 0 || i >= tensor.numel()) {
            throw new Error("Error: tensor index out of bounds");
        }
        
        // Accès à l'élément du tenseur
        torch::Tensor element = tensor.flatten()[i];
        
        // Retourner un scalaire LispE ou un nouveau tenseur selon le cas
        if (element.dim() == 0) {
            // C'est un scalaire, on retourne la valeur
            if (tensor.dtype() == torch::kFloat32) {
                if (current != NULL) {
                    if (current->type == t_float) {
                        ((Float*)current)->content = element.item<float>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Float(element.item<double>());
            } else if (tensor.dtype() == torch::kFloat64) {
                if (current != NULL) {
                    if (current->type == t_number) {
                        ((Number*)current)->content = element.item<double>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideNumber(element.item<double>());
            } else if (tensor.dtype() == torch::kInt32) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<int>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideInteger(element.item<int>());
            } else if (tensor.dtype() == torch::kInt64) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<int64_t>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideInteger(element.item<int64_t>());
            } else if (tensor.dtype() == torch::kBool) {
                return booleans_[element.item<bool>()];
            } else {
                // Pour les autres types, retourner la valeur comme float
                if (current != NULL) {
                    if (current->type == t_number) {
                        ((Number*)current)->content = element.item<double>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = lisp->provideNumber(element.item<double>());
            }
        } else {
            // C'est un tenseur, on retourne un TorchTensor
            if (current != NULL) {
                current->decrement();
                current = NULL;
            }

            current = new TorchTensor(element);
        }
        
        current->increment();
        return current;        
    } catch (const std::exception& e) {
        string msg = "Error accessing tensor element: ";
        msg += e.what();
        throw new Error(msg);
    }
}

Element* TorchTensor::index(long i) {
    try {
        // Accès à l'élément du tenseur
        torch::Tensor element = tensor.flatten()[i];

        // Retourner un scalaire LispE ou un nouveau tenseur selon le cas
        if (element.dim() == 0) {
            // C'est un scalaire, on retourne la valeur
            if (tensor.dtype() == torch::kFloat32) {
                if (current != NULL) {
                    if (current->type == t_float) {
                        ((Float*)current)->content = element.item<float>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Float(element.item<float>());
            } else if (tensor.dtype() == torch::kFloat64) {
                if (current != NULL) {
                    if (current->type == t_number) {
                        ((Number*)current)->content = element.item<double>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Number(element.item<double>());
            } else if (tensor.dtype() == torch::kInt32) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<int>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Integer(element.item<int>());
            } else if (tensor.dtype() == torch::kInt64) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<int64_t>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }

                current = new Integer(element.item<int64_t>());
            } else if (tensor.dtype() == torch::kBool) {
                if (current != NULL) {
                    if (current->type == t_integer) {
                        ((Integer*)current)->content = element.item<bool>();
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Integer(element.item<bool>());
            } else {
                // Pour les autres types, retourner la valeur comme float
                if (current != NULL) {
                    if (current->type == t_number) {
                        ((Number*)current)->content = element.item<double>(); 
                        return current;
                    }
                    current->decrement();
                    current = NULL;
                }
                current = new Number(element.item<double>());
            }
        } else {
            // C'est un tenseur, on retourne un TorchTensor
            if (current != NULL) {
                current->decrement();
                current = NULL;
            }

            current = new TorchTensor(element);
        }
        current->increment();
        return current;

    } catch (const std::exception& e) {
        string msg = "Error accessing tensor element: ";
        msg += e.what();
        throw new Error(msg);
    }
}

void TorchTensor::flatten(LispE* lisp, Numbers* l) {
    // Vérifier si le tenseur est défini
    if (!tensor.defined()) {
        throw new Error("Error: Tensor is not defined");
    }

    // Vérifier si le tenseur est vide
    if (tensor.numel() == 0) {
        return;
    }

    l->reserve(tensor.numel() + 1);

    // Choisir le bon type de données en fonction du dtype du tenseur
    switch (tensor.dtype().toScalarType()) {
        case torch::kFloat32: {
            auto tensor_data = tensor.contiguous().data_ptr<float>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<double>(tensor_data[i]));
            }
            break;
        }
        case torch::kFloat64: {
            auto tensor_data = tensor.contiguous().data_ptr<double>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i]);
            }
            break;
        }
        case torch::kInt32: {
            auto tensor_data = tensor.contiguous().data_ptr<int>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<double>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt64: {
            auto tensor_data = tensor.contiguous().data_ptr<int64_t>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<double>(tensor_data[i]));
            }
            break;
        }
        case torch::kBool: {
            auto tensor_data = tensor.contiguous().data_ptr<bool>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i] ? 1.0 : 0.0);
            }
            break;
        }
        default:
            throw new Error("Error: Unsupported tensor dtype for flatten");
    }
}

void TorchTensor::flatten(LispE* lisp, Integers* l) {
    // Vérifier si le tenseur est défini
    if (!tensor.defined()) {
        throw new Error("Error: Tensor is not defined");
    }

    // Vérifier si le tenseur est vide
    if (tensor.numel() == 0) {
        return;
    }

    l->reserve(tensor.numel() + 1);

    // Choisir le bon type de données en fonction du dtype du tenseur
    switch (tensor.dtype().toScalarType()) {
        case torch::kFloat32: {
            auto tensor_data = tensor.contiguous().data_ptr<float>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<int64_t>(tensor_data[i]));
            }
            break;
        }
        case torch::kFloat64: {
            auto tensor_data = tensor.contiguous().data_ptr<double>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<int64_t>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt32: {
            auto tensor_data = tensor.contiguous().data_ptr<int>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<int64_t>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt64: {
            auto tensor_data = tensor.contiguous().data_ptr<int64_t>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i]);
            }
            break;
        }
        case torch::kBool: {
            auto tensor_data = tensor.contiguous().data_ptr<bool>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i] ? 1 : 0);
            }
            break;
        }
        default:
            throw new Error("Error: Unsupported tensor dtype for flatten");
    }
}

void TorchTensor::flatten(LispE* lisp, Floats* l) {
    // Vérifier si le tenseur est défini
    if (!tensor.defined()) {
        throw new Error("Error: Tensor is not defined");
    }

    // Vérifier si le tenseur est vide
    if (tensor.numel() == 0) {
        return;
    }

    l->reserve(tensor.numel() + 1);

    // Choisir le bon type de données en fonction du dtype du tenseur
    switch (tensor.dtype().toScalarType()) {
        case torch::kFloat32: {
            auto tensor_data = tensor.contiguous().data_ptr<float>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i]);
            }
            break;
        }
        case torch::kFloat64: {
            auto tensor_data = tensor.contiguous().data_ptr<double>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<float>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt32: {
            auto tensor_data = tensor.contiguous().data_ptr<int>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<float>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt64: {
            auto tensor_data = tensor.contiguous().data_ptr<int64_t>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<float>(tensor_data[i]));
            }
            break;
        }
        case torch::kBool: {
            auto tensor_data = tensor.contiguous().data_ptr<bool>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i] ? 1.0 : 0.0);
            }
            break;
        }
        default:
            throw new Error("Error: Unsupported tensor dtype for flatten");
    }
}

void TorchTensor::flatten(LispE* lisp, Shorts* l) {
    // Vérifier si le tenseur est défini
    if (!tensor.defined()) {
        throw new Error("Error: Tensor is not defined");
    }

    // Vérifier si le tenseur est vide
    if (tensor.numel() == 0) {
        return;
    }

    l->reserve(tensor.numel() + 1);

    // Choisir le bon type de données en fonction du dtype du tenseur
    switch (tensor.dtype().toScalarType()) {
        case torch::kFloat32: {
            auto tensor_data = tensor.contiguous().data_ptr<float>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<short>(tensor_data[i]));
            }
            break;
        }
        case torch::kFloat64: {
            auto tensor_data = tensor.contiguous().data_ptr<double>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<short>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt32: {
            auto tensor_data = tensor.contiguous().data_ptr<int>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<short>(tensor_data[i]));
            }
            break;
        }
        case torch::kInt64: {
            auto tensor_data = tensor.contiguous().data_ptr<int64_t>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(static_cast<short>(tensor_data[i]));
            }
            break;
        }
        case torch::kBool: {
            auto tensor_data = tensor.contiguous().data_ptr<bool>();
            for (int64_t i = 0; i < tensor.numel(); i++) {
                l->liste.push_back(tensor_data[i] ? 1 : 0);
            }
            break;
        }
        default:
            throw new Error("Error: Unsupported tensor dtype for flatten");
    }
}

Element* TorchTensor::newInstance() {
    // Créer un nouveau tenseur vide avec la même structure
    torch::Tensor empty_tensor = torch::empty({0}, tensor.options());
    return new TorchTensor(empty_tensor);
}

void TorchTensor::set_from(Element* container, long start, long end) {
    try {
        if (container->type != tensor_type) {
            throw new Error("Error: Can only set from another tensor");
        }
        
        TorchTensor* source = (TorchTensor*)container;
        torch::Tensor source_flat = source->tensor.flatten();
        
        if (end == -1) {
            // Ajouter un seul élément
            if (start >= source_flat.numel()) {
                throw new Error("Error: Start index out of bounds");
            }
            torch::Tensor element = source_flat[start];
            
            // Si le tenseur est vide, l'initialiser
            if (tensor.numel() == 0) {
                tensor = element.unsqueeze(0);
            } else {
                tensor = torch::cat({tensor, element.unsqueeze(0)}, 0);
            }
        } else {
            // Ajouter une plage d'éléments
            if (start >= source_flat.numel() || end > source_flat.numel() || start >= end) {
                throw new Error("Error: Invalid range for tensor slice");
            }
            torch::Tensor slice = source_flat.slice(0, start, end);
            
            // Si le tenseur est vide, l'initialiser
            if (tensor.numel() == 0) {
                tensor = slice;
            } else {
                tensor = torch::cat({tensor, slice}, 0);
            }
        }
        
    } catch (const std::exception& e) {
        string msg = "Error in tensor set_from: ";
        msg += e.what();
        throw new Error(msg);
    }
}

