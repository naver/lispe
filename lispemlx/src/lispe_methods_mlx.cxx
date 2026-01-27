/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_methods_mlx.cxx
//  LispE library for MLX methods (Apple Silicon)
//
//  Cette bibliothèque expose les fonctions de base de mlx::core
//

#include "lispe.h"
#include <mlx/mlx.h>

using namespace std;
namespace mx = mlx::core;

// Enum for MLX method actions
enum mlx_method_actions {
    mlx_method_array,        // Créer un mx::array à partir de données
    mlx_method_shape,        // Créer un mx::Shape à partir de données
    mlx_method_reshape,
    mlx_method_take,
    mlx_method_mean,
    mlx_method_add,
    mlx_method_multiply,
    mlx_method_sum,
    mlx_method_transpose,
    mlx_method_matmul,
    // 10 nouvelles fonctions
    mlx_method_subtract,
    mlx_method_divide,
    mlx_method_maximum,
    mlx_method_minimum,
    mlx_method_sqrt,
    mlx_method_exp,
    mlx_method_log,
    mlx_method_abs,
    mlx_method_concatenate,
    mlx_method_split,
    // Fonctions d'activation
    mlx_method_sigmoid,
    mlx_method_relu,
    mlx_method_softmax,
    mlx_method_tanh,
    // Fonctions trigonométriques
    mlx_method_sin,
    mlx_method_cos,
    mlx_method_tan,
    // Fonctions de réduction avancées
    mlx_method_argmax,
    mlx_method_argmin,
    // Fonctions supplémentaires
    mlx_method_power,
    mlx_method_negative,
    mlx_method_square,
    // Groupe 1: Création d'arrays
    mlx_method_zeros,
    mlx_method_ones,
    mlx_method_full,
    mlx_method_arange,
    mlx_method_linspace,
    mlx_method_eye,
    // Groupe 2: Nombres aléatoires
    mlx_method_random_uniform,
    mlx_method_random_normal,
    mlx_method_random_randint,
    // Groupe 3: Mathématiques
    mlx_method_log2,
    mlx_method_log10,
    mlx_method_log1p,
    mlx_method_floor,
    mlx_method_ceil,
    mlx_method_round,
    mlx_method_clip,
    mlx_method_reciprocal,
    mlx_method_rsqrt,
    // Groupe 4: Trigonométrie inverse
    mlx_method_arcsin,
    mlx_method_arccos,
    mlx_method_arctan,
    mlx_method_sinh,
    mlx_method_cosh,
    // Groupe 5: Réductions
    mlx_method_prod,
    mlx_method_max,
    mlx_method_min,
    mlx_method_var,
    mlx_method_std,
    mlx_method_all,
    mlx_method_any,
    // Groupe 6: Comparaisons
    mlx_method_equal,
    mlx_method_not_equal,
    mlx_method_greater,
    mlx_method_less,
    mlx_method_greater_equal,
    mlx_method_less_equal,
    mlx_method_where,
    // Groupe 7: Manipulation
    mlx_method_flatten,
    mlx_method_squeeze,
    mlx_method_expand_dims,
    mlx_method_stack,
    mlx_method_tile,
    mlx_method_repeat,
    mlx_method_pad,
    // Groupe 8: Activation deep learning
    mlx_method_gelu,
    mlx_method_gelu_tanh,
    mlx_method_silu,
    mlx_method_leaky_relu,
    mlx_method_elu,
    mlx_method_selu,
    mlx_method_log_softmax,
    // Groupe 9: Algèbre linéaire
    mlx_method_norm,
    mlx_method_inv,
    mlx_method_svd,
    mlx_method_qr,
    mlx_method_cholesky,
    mlx_method_solve,
    mlx_method_tucker,
    mlx_method_hosvd,
    mlx_method_tucker_reconstruct,
    mlx_method_tucker_compression_ratio,
    mlx_method_khatri_rao_product,
    // Groupe 10: Opérations logiques et tests
    mlx_method_logical_and,
    mlx_method_logical_or,
    mlx_method_logical_not,
    mlx_method_isnan,
    mlx_method_isinf,
    mlx_method_isfinite,
    // Groupe 11: Manipulation avancée
    mlx_method_flip,
    mlx_method_roll,
    mlx_method_sort,
    mlx_method_argsort,
    mlx_method_argwhere,
    mlx_method_moveaxis,
    mlx_method_swapaxes,
    mlx_method_broadcast_to,
    // Groupe 12: Création avancée
    mlx_method_identity,
    mlx_method_tri,
    mlx_method_tril,
    mlx_method_triu,
    mlx_method_meshgrid,
    // Groupe 13: Algèbre linéaire avancée
    mlx_method_pinv,
    mlx_method_eig,
    mlx_method_eigvals,
    mlx_method_lu,
    mlx_method_cross,
    // Groupe 14: I/O de modèles
    mlx_method_save,
    mlx_method_load,
    mlx_method_load_safetensors,
    mlx_method_save_safetensors,
    mlx_method_load_gguf,
    mlx_method_save_gguf,
    // Groupe 15: Quantification et mémoire
    mlx_method_quantize,
    mlx_method_dequantize,
    mlx_method_quantized_matmul,
    mlx_method_synchronize,
    mlx_method_clear_cache,
    mlx_method_get_active_memory,
    mlx_method_get_peak_memory,
    mlx_method_get_cache_memory,
    mlx_method_set_memory_limit,
    mlx_method_set_cache_limit,
    // Groupe 16: Opérations supplémentaires
    mlx_method_slice,
    mlx_method_trace,
    mlx_method_diagonal,
    mlx_method_diag,
    mlx_method_tensordot,
    mlx_method_inner,
    mlx_method_outer,
    mlx_method_cumsum,
    mlx_method_cumprod,
    mlx_method_topk,
    mlx_method_partition,
    // Groupe 17: FFT (Transformées de Fourier)
    mlx_method_fft,
    mlx_method_ifft,
    mlx_method_fft2,
    mlx_method_ifft2,
    mlx_method_fftn,
    mlx_method_ifftn,
    mlx_method_rfft,
    mlx_method_irfft,
    mlx_method_rfft2,
    mlx_method_irfft2,
    mlx_method_rfftn,
    mlx_method_irfftn,
    mlx_method_fftshift,
    mlx_method_ifftshift,
    // Groupe 18: Fonctions mathématiques avancées
    mlx_method_arctan2,
    mlx_method_arcsinh,
    mlx_method_arccosh,
    mlx_method_arctanh,
    mlx_method_degrees,
    mlx_method_radians,
    mlx_method_erf,
    mlx_method_erfinv,
    mlx_method_expm1,
    mlx_method_logaddexp,
    mlx_method_sign,
    mlx_method_kron,
    // Groupe 19: Opérations bitwise
    mlx_method_bitwise_and,
    mlx_method_bitwise_or,
    mlx_method_bitwise_xor,
    mlx_method_bitwise_invert,
    mlx_method_left_shift,
    mlx_method_right_shift,
    // Groupe 20: Division et reste
    mlx_method_divmod,
    mlx_method_floor_divide,
    mlx_method_remainder,
    // Groupe 21: Opérations cumulatives et logsumexp
    mlx_method_cummax,
    mlx_method_cummin,
    mlx_method_logsumexp,
    mlx_method_logcumsumexp,
    // Groupe 22: Convolutions (deep learning)
    mlx_method_conv1d,
    mlx_method_conv2d,
    mlx_method_conv3d,
    mlx_method_conv_transpose1d,
    mlx_method_conv_transpose2d,
    mlx_method_conv_transpose3d,
    mlx_method_conv_general,
    // Groupe 23: Opérations spécialisées ML
    mlx_method_rms_norm,
    mlx_method_layer_norm,
    mlx_method_rope,
    mlx_method_scaled_dot_product_attention,
    mlx_method_einsum,
    // Groupe 24: Manipulation et création avancée
    mlx_method_dtype,
    mlx_method_astype,
    mlx_method_copy,
    mlx_method_zeros_like,
    mlx_method_ones_like,
    mlx_method_nan_to_num,
    mlx_method_atleast_1d,
    mlx_method_atleast_2d,
    mlx_method_atleast_3d,
    mlx_method_array_equal,
    mlx_method_allclose,
    // Groupe 25: Random avancé
    mlx_method_random_bernoulli,
    mlx_method_random_truncated_normal,
    mlx_method_random_gumbel,
    mlx_method_random_categorical,
    mlx_method_random_laplace,
    mlx_method_random_multivariate_normal,
    mlx_method_random_permutation,
    // Groupe 26: Algèbre linéaire avancée
    mlx_method_tri_inv,
    mlx_method_cholesky_inv,
    mlx_method_lu_factor,
    mlx_method_solve_triangular,
    mlx_method_eigvalsh,
    mlx_method_eigh,
    // Groupe 27: Gather/Scatter avancé
    mlx_method_take_along_axis,
    mlx_method_put_along_axis,
    mlx_method_gather,
    mlx_method_scatter,
    mlx_method_scatter_add,
    mlx_method_slice_update,
    // Groupe 28: Nombres complexes
    mlx_method_conjugate,
    mlx_method_real,
    mlx_method_imag,
    // Groupe 29: Opérations fusionnées pour LLM
    mlx_method_fused_mlp,
    mlx_method_fused_moe,
    mlx_method_fused_moe_batch,
    // Groupe 30: Évaluation explicite (lazy evaluation)
    mlx_method_eval,
    // Groupe 31: Conversion vers LispE
    mlx_method_tolist
};

// Type pour les arrays MLX
int16_t t_mlx_array = 0;
int16_t t_mlx_shape = 0;

// ============================================================================
// MLXShape Element - Encapsule un mx::Shape pour LispE
// ============================================================================

class MLXShape : public Element {
public:
    mx::Shape shape;
    mutable Constinteger exchange_int{0};

    MLXShape() : Element(t_mlx_shape) {}
    MLXShape(mx::Shape&& s) : Element(t_mlx_shape), shape(std::move(s)) {}
    MLXShape(const mx::Shape& s) : Element(t_mlx_shape), shape(s) {}

    Element* eval(LispE* lisp) override {
        return this;
    }

    bool Boolean() override {
        return !shape.empty();
    }

    wstring asString(LispE* lisp) override {
        std::stringstream ss;
        ss << "MLX Shape: [";
        for (size_t i = 0; i < shape.size(); i++) {
            ss << shape[i];
            if (i < shape.size() - 1) ss << ", ";
        }
        ss << "]";
        std::string info = ss.str();
        return wstring(info.begin(), info.end());
    }

    long size() override {
        return shape.size();
    }

    bool isList() override {
        return true;
    }

    bool isContainer() override {
        return true;
    }

    // Accès aux éléments par index
    Element* index(long i) override {
        if (i < 0)
            i = shape.size() + i;
        if (i >= 0 && i < (long)shape.size()) {
            exchange_int.content = shape[i];
            return &exchange_int;
        }
        return &exchange_int;  // Retourne 0 si hors bornes
    }

    Element* protected_index(LispE* lisp, long i) override {
        if (i < 0)
            i = shape.size() + i;
        if (i < 0 || i >= (long)shape.size())
            throw new Error("Error: index out of bounds");
        return lisp->provideInteger(shape[i]);
    }

    Element* protected_index(LispE* lisp, Element* k) override {
        long i = k->checkInteger(lisp);
        return protected_index(lisp, i);
    }

    Element* value_on_index(LispE* lisp, long i) override {
        if (i < 0)
            i = shape.size() + i;
        if (i < 0 || i >= (long)shape.size())
            return null_;
        return lisp->provideInteger(shape[i]);
    }

    Element* value_on_index(LispE* lisp, Element* idx) override {
        long i = idx->checkInteger(lisp);
        return value_on_index(lisp, i);
    }
};

// ============================================================================
// MLX Array Element - Encapsule un mx::array pour LispE
// ============================================================================

class MLXArray : public Element {
public:
    mx::array array;

    MLXArray(mx::array&& arr) : Element(t_mlx_array), array(std::move(arr)) {
        exchange_complex.status = s_constant;
    }

    MLXArray(const mx::array& arr) : Element(t_mlx_array), array(arr) {
        exchange_complex.status = s_constant;
    }

    Element* eval(LispE* lisp) override {
        return this;
    }

    bool Boolean() override {
        return true;
    }

    wstring asString(LispE* lisp) override {
        std::stringstream ss;
        ss << "MLX Array shape: [";
        auto shape = array.shape();
        for (size_t i = 0; i < shape.size(); i++) {
            ss << shape[i];
            if (i < shape.size() - 1) ss << ", ";
        }
        ss << "] dtype: ";

        // Afficher le type
        if (array.dtype() == mx::float32) ss << "float32";
        else if (array.dtype() == mx::float64) ss << "float64";
        else if (array.dtype() == mx::int32) ss << "int32";
        else if (array.dtype() == mx::int64) ss << "int64";
        else if (array.dtype() == mx::complex64) ss << "complex64";
        else ss << "other";

        std::string info = ss.str();
        return wstring(info.begin(), info.end());
    }

    long size() override {
        return array.size();
    }

    bool isList() override {
        return true;
    }

    bool isContainer() override {
        return true;
    }

    // Accès aux éléments par index
    // Note: Pour un array multi-dimensionnel, on accède aux éléments linéairement
    Element* index(long i) override;
    Element* protected_index(LispE* lisp, long i) override;
    Element* protected_index(LispE* lisp, Element* k) override;
    Element* value_on_index(LispE* lisp, long i) override;
    Element* value_on_index(LispE* lisp, Element* idx) override;

    // Opérations arithmétiques directes
    Element* plus_direct(LispE* lisp, Element* e) override;
    Element* minus_direct(LispE* lisp, Element* e) override;
    Element* multiply_direct(LispE* lisp, Element* e) override;
    Element* divide_direct(LispE* lisp, Element* e) override;

private:
    // Valeur d'échange pour éviter les allocations répétées dans index()
    mutable Constfloat exchange_float{0};
    mutable Constinteger exchange_int{0};
    mutable Complexe exchange_complex{0, 0};
};

// ============================================================================
// Helper functions pour convertir entre LispE et MLX
// ============================================================================

// Convertir un Element LispE vers mx::array
mx::array element_to_array(LispE* lisp, Element* elem) {
    if (elem->type == t_mlx_array)
        return ((MLXArray*)elem)->array;

    switch (elem->type) {
        case t_numbers: {
            Numbers* nums = (Numbers*)elem;
            std::vector<double> data;
            nums->liste.to_vector(data);
            return mx::array(data.data(), {static_cast<int>(data.size())}, mx::float64);
        }

        case t_floats: {
            Floats* flts = (Floats*)elem;
            std::vector<float> data;
            flts->liste.to_vector(data);
            return mx::array(data.data(), {static_cast<int>(data.size())}, mx::float32);
        }

        case t_integers: {
            Integers* ints = (Integers*)elem;
            std::vector<long> data;
            ints->liste.to_vector(data);
            // Convertir long vers int64_t pour MLX
            std::vector<int64_t> data64(data.begin(), data.end());
            return mx::array(data64.data(), {static_cast<int>(data64.size())}, mx::int64);
        }

        case t_complex: {
            // Nombre complexe scalaire LispE -> mx::array complex64
            Complexe* cplx = (Complexe*)elem;
            std::complex<float> val(cplx->content.real(), cplx->content.imag());
            return mx::array(val);
        }

        case t_list: {
            // Pour les listes génériques, détecter le type via le premier élément
            if (elem->size() == 0) {
                throw new Error("Error: Cannot convert empty list to MLX array");
            }

            Element* first = elem->index(0);
            // f_type: isNumber(1) + isFloat(2) + isInteger(3)
            // isFloat=true => f_type = 3 (1+2)
            // isInteger=true => f_type = 4 (1+3)
            // isNumber only => f_type = 1
            switch (first->type) {
                case t_float: {
                    // Float (isNumber + isFloat)
                    std::vector<float> data;
                    for (long i = 0; i < elem->size(); i++) {
                        data.push_back(elem->index(i)->asFloat());
                    }
                    return mx::array(data.data(), {static_cast<int>(data.size())}, mx::float32);
                }
                case t_integer: {
                    // Integer (isNumber + isInteger)
                    std::vector<int64_t> data;
                    for (long i = 0; i < elem->size(); i++) {
                        data.push_back(elem->index(i)->asInteger());
                    }
                    return mx::array(data.data(), {static_cast<int>(data.size())}, mx::int64);
                }
                case t_number: {
                    // Number (générique) -> utiliser double
                    std::vector<double> data;
                    for (long i = 0; i < elem->size(); i++) {
                        data.push_back(elem->index(i)->asNumber());
                    }
                    return mx::array(data.data(), {static_cast<int>(data.size())}, mx::float64);
                }
                case t_complex: {
                    // Liste de nombres complexes -> mx::array complex64
                    std::vector<mx::complex64_t> data;
                    for (long i = 0; i < elem->size(); i++) {
                        Element* e = elem->index(i);
                        if (e->type == t_complex) {
                            Complexe* cplx = (Complexe*)e;
                            data.push_back(mx::complex64_t(cplx->content.real(), cplx->content.imag()));
                        } else {
                            // Nombre réel traité comme complexe avec partie imaginaire = 0
                            data.push_back(mx::complex64_t(e->asNumber(), 0));
                        }
                    }
                    return mx::array(data.data(), {static_cast<int>(data.size())});
                }
                default:
                    throw new Error("Error: List elements must be numbers");
            }
        }

        default:
            // Scalaire
            if (elem->isNumber()) {
                double val = elem->asNumber();
                return mx::array(val);
            }
            throw new Error("Error: Cannot convert element to MLX array");
    }
}

// Convertir une liste LispE en mx::Shape
void element_to_shape(Element* elem, mx::Shape& shape) {
    shape.clear();
    if (elem->type == t_mlx_shape) {
        shape = ((MLXShape*)elem)->shape;
        return;
    }
    if (elem->isList()) {
        for (long i = 0; i < elem->size(); i++) {
            shape.push_back(static_cast<mx::ShapeElem>(elem->index(i)->asInteger()));
        }
    } else {
        shape.push_back(static_cast<mx::ShapeElem>(elem->asInteger()));
    }
}

// Convertir une liste LispE en std::vector<int> pour les axes
// (Beaucoup de fonctions MLX utilisent std::vector<int> pour les axes)
std::vector<int> element_to_axes(Element* elem) {
    std::vector<int> axes;
    if (elem->type == t_mlx_shape) {
        const mx::Shape& shape = ((MLXShape*)elem)->shape;
        for (auto dim : shape) {
            axes.push_back(static_cast<int>(dim));
        }
        return axes;
    }
    if (elem->isList()) {
        for (long i = 0; i < elem->size(); i++) {
            axes.push_back(static_cast<int>(elem->index(i)->asInteger()));
        }
    } else {
        axes.push_back(static_cast<int>(elem->asInteger()));
    }
    return axes;
}

// Convertir un mx::array vers Element LispE
Element* array_to_element(LispE* lisp, const mx::array& arr) {
    // Pour l'instant, on retourne toujours un MLXArray
    // On pourrait aussi convertir vers Numbers/Floats/Integers selon le besoin
    return new MLXArray(arr);
}

// ============================================================================
// Implémentation des méthodes d'accès aux éléments pour MLXArray
// ============================================================================

// Helper pour extraire un élément à l'index i et retourner un Element LispE
// Utilise la valeur d'échange pour éviter les allocations dans index()
Element* MLXArray::index(long i) {
    mx::eval(array);  // S'assurer que l'array est évalué
    
    switch (array.dtype()) {
        case mx::float32: {
            exchange_float.content = array.data<float>()[i];
            return &exchange_float;
        }
        case mx::float64: {
            exchange_float.content = static_cast<float>(array.data<double>()[i]);
            return &exchange_float;
        }
        case mx::int32: {
            exchange_int.content = array.data<int32_t>()[i];
            return &exchange_int;
        }
        case mx::int64: {
            exchange_int.content = array.data<int64_t>()[i];
            return &exchange_int;
        }
        case mx::complex64: {
            mx::complex64_t c = array.data<mx::complex64_t>()[i];
            exchange_complex.content = std::complex<double>(c.real(), c.imag());
            return &exchange_complex;
        }
        case mx::bfloat16: {
            mx::bfloat16_t bf = array.data<mx::bfloat16_t>()[i];
            exchange_float.content = static_cast<float>(bf);
            return &exchange_float;
        }
        case mx::float16: {
            mx::float16_t f16 = array.data<mx::float16_t>()[i];
            exchange_float.content = static_cast<float>(f16);
            return &exchange_float;
        }
        default: {
            // Pour les autres types, retourner comme float
            exchange_float.content = 0;
            return &exchange_float;
        }
    }
}

// protected_index avec vérification des bornes
Element* MLXArray::protected_index(LispE* lisp, long i) {
    if (i < 0)
        i = array.size() + i;
    
    if (i < 0 || i >= array.size())
        throw new Error("Error: index out of bounds");
    
    mx::eval(array);
    
    switch (array.dtype()) {
        case mx::float32:
            return lisp->provideFloat(array.data<float>()[i]);
        case mx::float64:
            return lisp->provideNumber(array.data<double>()[i]);
        case mx::int32:
            return lisp->provideInteger(array.data<int32_t>()[i]);
        case mx::int64:
            return lisp->provideInteger(array.data<int64_t>()[i]);
        case mx::uint32:
            return lisp->provideInteger(array.data<uint32_t>()[i]);
        case mx::uint64:
            return lisp->provideInteger(array.data<uint64_t>()[i]);
        case mx::int16:
            return lisp->provideInteger(array.data<int16_t>()[i]);
        case mx::uint16:
            return lisp->provideInteger(array.data<uint16_t>()[i]);
        case mx::int8:
            return lisp->provideInteger(array.data<int8_t>()[i]);
        case mx::uint8:
            return lisp->provideInteger(array.data<uint8_t>()[i]);
        case mx::complex64: {
            mx::complex64_t c = array.data<mx::complex64_t>()[i];
            return new Complexe(c.real(), c.imag());
        }
        case mx::bfloat16: {
            mx::bfloat16_t bf = array.data<mx::bfloat16_t>()[i];
            return lisp->provideFloat(static_cast<float>(bf));
        }
        case mx::float16: {
            mx::float16_t f16 = array.data<mx::float16_t>()[i];
            return lisp->provideFloat(static_cast<float>(f16));
        }
        case mx::bool_:
            return array.data<bool>()[i] ? true_ : null_;
        default:
            return lisp->provideFloat(0);
    }
}

// protected_index avec Element comme index
Element* MLXArray::protected_index(LispE* lisp, Element* k) {
    long i = k->checkInteger(lisp);
    return protected_index(lisp, i);
}

// value_on_index - retourne null_ si hors bornes au lieu de lever une erreur
Element* MLXArray::value_on_index(LispE* lisp, long i) {
    if (i < 0)
        i = array.size() + i;
    
    if (i < 0 || i >= array.size())
        return null_;
    
    mx::eval(array);
    
    switch (array.dtype()) {
        case mx::float32:
            return lisp->provideFloat(array.data<float>()[i]);
        case mx::float64:
            return lisp->provideNumber(array.data<double>()[i]);
        case mx::int32:
            return lisp->provideInteger(array.data<int32_t>()[i]);
        case mx::int64:
            return lisp->provideInteger(array.data<int64_t>()[i]);
        case mx::uint32:
            return lisp->provideInteger(array.data<uint32_t>()[i]);
        case mx::uint64:
            return lisp->provideInteger(array.data<uint64_t>()[i]);
        case mx::int16:
            return lisp->provideInteger(array.data<int16_t>()[i]);
        case mx::uint16:
            return lisp->provideInteger(array.data<uint16_t>()[i]);
        case mx::int8:
            return lisp->provideInteger(array.data<int8_t>()[i]);
        case mx::uint8:
            return lisp->provideInteger(array.data<uint8_t>()[i]);
        case mx::complex64: {
            mx::complex64_t c = array.data<mx::complex64_t>()[i];
            return new Complexe(c.real(), c.imag());
        }
        case mx::bfloat16: {
            mx::bfloat16_t bf = array.data<mx::bfloat16_t>()[i];
            return lisp->provideFloat(static_cast<float>(bf));
        }
        case mx::float16: {
            mx::float16_t f16 = array.data<mx::float16_t>()[i];
            return lisp->provideFloat(static_cast<float>(f16));
        }
        case mx::bool_:
            return array.data<bool>()[i] ? true_ : null_;
        default:
            return null_;
    }
}

// value_on_index avec Element comme index
Element* MLXArray::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    return value_on_index(lisp, i);
}

// ============================================================================
// Implémentation des opérations arithmétiques directes pour MLXArray
// ============================================================================

Element* MLXArray::plus_direct(LispE* lisp, Element* e) {
    try {
        if (e->isNumber()) {
            mx::array result = mx::add(array, mx::array(e->asNumber()));
            return new MLXArray(std::move(result));
        }
        if (e->isValueList()) {
            mx::array other = element_to_array(lisp, e);
            mx::array result = mx::add(array, other);
            return new MLXArray(std::move(result));
        }
        if (e->type == t_mlx_array) {
            MLXArray* other = (MLXArray*)e;
            mx::array result = mx::add(array, other->array);
            return new MLXArray(std::move(result));
        }
        throw new Error("Error: incompatible types for addition");
    } catch (const std::exception& ex) {
        throw new Error("Error in MLXArray addition: " + std::string(ex.what()));
    }
}

Element* MLXArray::minus_direct(LispE* lisp, Element* e) {
    try {
        if (e->isNumber()) {
            mx::array result = mx::subtract(array, mx::array(e->asNumber()));
            return new MLXArray(std::move(result));
        }
        if (e->isValueList()) {
            mx::array other = element_to_array(lisp, e);
            mx::array result = mx::subtract(array, other);
            return new MLXArray(std::move(result));
        }
        if (e->type == t_mlx_array) {
            MLXArray* other = (MLXArray*)e;
            mx::array result = mx::subtract(array, other->array);
            return new MLXArray(std::move(result));
        }
        throw new Error("Error: incompatible types for subtraction");
    } catch (const std::exception& ex) {
        throw new Error("Error in MLXArray subtraction: " + std::string(ex.what()));
    }
}

Element* MLXArray::multiply_direct(LispE* lisp, Element* e) {
    try {
        if (e->isNumber()) {
            mx::array result = mx::multiply(array, mx::array(e->asNumber()));
            return new MLXArray(std::move(result));
        }
        if (e->isValueList()) {
            mx::array other = element_to_array(lisp, e);
            mx::array result = mx::multiply(array, other);
            return new MLXArray(std::move(result));
        }
        if (e->type == t_mlx_array) {
            MLXArray* other = (MLXArray*)e;
            mx::array result = mx::multiply(array, other->array);
            return new MLXArray(std::move(result));
        }
        throw new Error("Error: incompatible types for multiplication");
    } catch (const std::exception& ex) {
        throw new Error("Error in MLXArray multiplication: " + std::string(ex.what()));
    }
}

Element* MLXArray::divide_direct(LispE* lisp, Element* e) {
    try {
        if (e->isNumber()) {
            double scalar_value = e->asNumber();
            if (scalar_value == 0)
                throw new Error("Error: division by zero");
            mx::array result = mx::divide(array, mx::array(scalar_value));
            return new MLXArray(std::move(result));
        }
        if (e->isValueList()) {
            mx::array other = element_to_array(lisp, e);
            mx::array result = mx::divide(array, other);
            return new MLXArray(std::move(result));
        }
        if (e->type == t_mlx_array) {
            MLXArray* other = (MLXArray*)e;
            mx::array result = mx::divide(array, other->array);
            return new MLXArray(std::move(result));
        }
        throw new Error("Error: incompatible types for division");
    } catch (const std::exception& ex) {
        throw new Error("Error in MLXArray division: " + std::string(ex.what()));
    }
}

// ============================================================================
// Classe principale pour les méthodes MLX
// ============================================================================

class Lispe_mlx_methods : public Element {
public:
    mlx_method_actions action;

    Lispe_mlx_methods(mlx_method_actions a) : Element(l_lib), action(a) {}

    Element* eval(LispE* lisp) override;
    wstring asString(LispE* lisp) override;

    Element* method_array(LispE* lisp);
    Element* method_shape(LispE* lisp);
    Element* method_reshape(LispE* lisp);
    Element* method_take(LispE* lisp);
    Element* method_mean(LispE* lisp);
    Element* method_add(LispE* lisp);
    Element* method_multiply(LispE* lisp);
    Element* method_sum(LispE* lisp);
    Element* method_transpose(LispE* lisp);
    Element* method_matmul(LispE* lisp);
    Element* method_subtract(LispE* lisp);
    Element* method_divide(LispE* lisp);
    Element* method_maximum(LispE* lisp);
    Element* method_minimum(LispE* lisp);
    Element* method_sqrt(LispE* lisp);
    Element* method_exp(LispE* lisp);
    Element* method_log(LispE* lisp);
    Element* method_abs(LispE* lisp);
    Element* method_concatenate(LispE* lisp);
    Element* method_split(LispE* lisp);
    // Fonctions d'activation
    Element* method_sigmoid(LispE* lisp);
    Element* method_relu(LispE* lisp);
    Element* method_softmax(LispE* lisp);
    Element* method_tanh(LispE* lisp);
    // Fonctions trigonométriques
    Element* method_sin(LispE* lisp);
    Element* method_cos(LispE* lisp);
    Element* method_tan(LispE* lisp);
    // Fonctions de réduction avancées
    Element* method_argmax(LispE* lisp);
    Element* method_argmin(LispE* lisp);
    // Fonctions supplémentaires
    Element* method_power(LispE* lisp);
    Element* method_negative(LispE* lisp);
    Element* method_square(LispE* lisp);
    // Groupe 1: Création d'arrays
    Element* method_zeros(LispE* lisp);
    Element* method_ones(LispE* lisp);
    Element* method_full(LispE* lisp);
    Element* method_arange(LispE* lisp);
    Element* method_linspace(LispE* lisp);
    Element* method_eye(LispE* lisp);
    // Groupe 2: Nombres aléatoires
    Element* method_random_uniform(LispE* lisp);
    Element* method_random_normal(LispE* lisp);
    Element* method_random_randint(LispE* lisp);
    // Groupe 3: Mathématiques
    Element* method_log2(LispE* lisp);
    Element* method_log10(LispE* lisp);
    Element* method_log1p(LispE* lisp);
    Element* method_floor(LispE* lisp);
    Element* method_ceil(LispE* lisp);
    Element* method_round(LispE* lisp);
    Element* method_clip(LispE* lisp);
    Element* method_reciprocal(LispE* lisp);
    Element* method_rsqrt(LispE* lisp);
    // Groupe 4: Trigonométrie inverse
    Element* method_arcsin(LispE* lisp);
    Element* method_arccos(LispE* lisp);
    Element* method_arctan(LispE* lisp);
    Element* method_sinh(LispE* lisp);
    Element* method_cosh(LispE* lisp);
    // Groupe 5: Réductions
    Element* method_prod(LispE* lisp);
    Element* method_max(LispE* lisp);
    Element* method_min(LispE* lisp);
    Element* method_var(LispE* lisp);
    Element* method_std(LispE* lisp);
    Element* method_all(LispE* lisp);
    Element* method_any(LispE* lisp);
    // Groupe 6: Comparaisons
    Element* method_equal(LispE* lisp);
    Element* method_not_equal(LispE* lisp);
    Element* method_greater(LispE* lisp);
    Element* method_less(LispE* lisp);
    Element* method_greater_equal(LispE* lisp);
    Element* method_less_equal(LispE* lisp);
    Element* method_where(LispE* lisp);
    // Groupe 7: Manipulation
    Element* method_flatten(LispE* lisp);
    Element* method_squeeze(LispE* lisp);
    Element* method_expand_dims(LispE* lisp);
    Element* method_stack(LispE* lisp);
    Element* method_tile(LispE* lisp);
    Element* method_repeat(LispE* lisp);
    Element* method_pad(LispE* lisp);
    // Groupe 8: Activation deep learning
    Element* method_gelu(LispE* lisp);
    Element* method_gelu_tanh(LispE* lisp);
    Element* method_silu(LispE* lisp);
    Element* method_leaky_relu(LispE* lisp);
    Element* method_elu(LispE* lisp);
    Element* method_selu(LispE* lisp);
    Element* method_log_softmax(LispE* lisp);
    // Groupe 9: Algèbre linéaire
    Element* method_norm(LispE* lisp);
    Element* method_inv(LispE* lisp);
    Element* method_svd(LispE* lisp);
    Element* method_qr(LispE* lisp);
    Element* method_cholesky(LispE* lisp);
    Element* method_solve(LispE* lisp);
    Element* method_tucker(LispE* lisp);
    Element* method_hosvd(LispE* lisp);
    Element* method_tucker_reconstruct(LispE* lisp);
    Element* method_tucker_compression_ratio(LispE* lisp);
    Element* method_khatri_rao_product(LispE* lisp);
    // Groupe 10: Opérations logiques et tests
    Element* method_logical_and(LispE* lisp);
    Element* method_logical_or(LispE* lisp);
    Element* method_logical_not(LispE* lisp);
    Element* method_isnan(LispE* lisp);
    Element* method_isinf(LispE* lisp);
    Element* method_isfinite(LispE* lisp);
    // Groupe 11: Manipulation avancée
    Element* method_flip(LispE* lisp);
    Element* method_roll(LispE* lisp);
    Element* method_sort(LispE* lisp);
    Element* method_argsort(LispE* lisp);
    Element* method_argwhere(LispE* lisp);
    Element* method_moveaxis(LispE* lisp);
    Element* method_swapaxes(LispE* lisp);
    Element* method_broadcast_to(LispE* lisp);
    // Groupe 12: Création avancée
    Element* method_identity(LispE* lisp);
    Element* method_tri(LispE* lisp);
    Element* method_tril(LispE* lisp);
    Element* method_triu(LispE* lisp);
    Element* method_meshgrid(LispE* lisp);
    // Groupe 13: Algèbre linéaire avancée
    Element* method_pinv(LispE* lisp);
    Element* method_eig(LispE* lisp);
    Element* method_eigvals(LispE* lisp);
    Element* method_lu(LispE* lisp);
    Element* method_cross(LispE* lisp);
    // Groupe 14: I/O de modèles
    Element* method_save(LispE* lisp);
    Element* method_load(LispE* lisp);
    Element* method_load_safetensors(LispE* lisp);
    Element* method_save_safetensors(LispE* lisp);
    Element* method_load_gguf(LispE* lisp);
    Element* method_save_gguf(LispE* lisp);
    // Groupe 15: Quantification et mémoire
    Element* method_quantize(LispE* lisp);
    Element* method_dequantize(LispE* lisp);
    Element* method_quantized_matmul(LispE* lisp);
    Element* method_synchronize(LispE* lisp);
    Element* method_clear_cache(LispE* lisp);
    Element* method_get_active_memory(LispE* lisp);
    Element* method_get_peak_memory(LispE* lisp);
    Element* method_get_cache_memory(LispE* lisp);
    Element* method_set_memory_limit(LispE* lisp);
    Element* method_set_cache_limit(LispE* lisp);
    // Groupe 16: Opérations supplémentaires
    Element* method_slice(LispE* lisp);
    Element* method_trace(LispE* lisp);
    Element* method_diagonal(LispE* lisp);
    Element* method_diag(LispE* lisp);
    Element* method_tensordot(LispE* lisp);
    Element* method_inner(LispE* lisp);
    Element* method_outer(LispE* lisp);
    Element* method_cumsum(LispE* lisp);
    Element* method_cumprod(LispE* lisp);
    Element* method_topk(LispE* lisp);
    Element* method_partition(LispE* lisp);
    // Groupe 17: FFT (Transformées de Fourier)
    Element* method_fft(LispE* lisp);
    Element* method_ifft(LispE* lisp);
    Element* method_fft2(LispE* lisp);
    Element* method_ifft2(LispE* lisp);
    Element* method_fftn(LispE* lisp);
    Element* method_ifftn(LispE* lisp);
    Element* method_rfft(LispE* lisp);
    Element* method_irfft(LispE* lisp);
    Element* method_rfft2(LispE* lisp);
    Element* method_irfft2(LispE* lisp);
    Element* method_rfftn(LispE* lisp);
    Element* method_irfftn(LispE* lisp);
    Element* method_fftshift(LispE* lisp);
    Element* method_ifftshift(LispE* lisp);
    // Groupe 18: Fonctions mathématiques avancées
    Element* method_arctan2(LispE* lisp);
    Element* method_arcsinh(LispE* lisp);
    Element* method_arccosh(LispE* lisp);
    Element* method_arctanh(LispE* lisp);
    Element* method_degrees(LispE* lisp);
    Element* method_radians(LispE* lisp);
    Element* method_erf(LispE* lisp);
    Element* method_erfinv(LispE* lisp);
    Element* method_expm1(LispE* lisp);
    Element* method_logaddexp(LispE* lisp);
    Element* method_sign(LispE* lisp);
    Element* method_kron(LispE* lisp);
    // Groupe 19: Opérations bitwise
    Element* method_bitwise_and(LispE* lisp);
    Element* method_bitwise_or(LispE* lisp);
    Element* method_bitwise_xor(LispE* lisp);
    Element* method_bitwise_invert(LispE* lisp);
    Element* method_left_shift(LispE* lisp);
    Element* method_right_shift(LispE* lisp);
    // Groupe 20: Division et reste
    Element* method_divmod(LispE* lisp);
    Element* method_floor_divide(LispE* lisp);
    Element* method_remainder(LispE* lisp);
    // Groupe 21: Opérations cumulatives et logsumexp
    Element* method_cummax(LispE* lisp);
    Element* method_cummin(LispE* lisp);
    Element* method_logsumexp(LispE* lisp);
    Element* method_logcumsumexp(LispE* lisp);
    // Groupe 22: Convolutions (deep learning)
    Element* method_conv1d(LispE* lisp);
    Element* method_conv2d(LispE* lisp);
    Element* method_conv3d(LispE* lisp);
    Element* method_conv_transpose1d(LispE* lisp);
    Element* method_conv_transpose2d(LispE* lisp);
    Element* method_conv_transpose3d(LispE* lisp);
    Element* method_conv_general(LispE* lisp);
    // Groupe 23: Opérations spécialisées ML
    Element* method_rms_norm(LispE* lisp);
    Element* method_layer_norm(LispE* lisp);
    Element* method_rope(LispE* lisp);
    Element* method_scaled_dot_product_attention(LispE* lisp);
    Element* method_einsum(LispE* lisp);
    // Groupe 24: Manipulation et création avancée
    Element* method_dtype(LispE* lisp);
    Element* method_astype(LispE* lisp);
    Element* method_copy(LispE* lisp);
    Element* method_zeros_like(LispE* lisp);
    Element* method_ones_like(LispE* lisp);
    Element* method_nan_to_num(LispE* lisp);
    Element* method_atleast_1d(LispE* lisp);
    Element* method_atleast_2d(LispE* lisp);
    Element* method_atleast_3d(LispE* lisp);
    Element* method_array_equal(LispE* lisp);
    Element* method_allclose(LispE* lisp);
    // Groupe 25: Random avancé
    Element* method_random_bernoulli(LispE* lisp);
    Element* method_random_truncated_normal(LispE* lisp);
    Element* method_random_gumbel(LispE* lisp);
    Element* method_random_categorical(LispE* lisp);
    Element* method_random_laplace(LispE* lisp);
    Element* method_random_multivariate_normal(LispE* lisp);
    Element* method_random_permutation(LispE* lisp);
    // Groupe 26: Algèbre linéaire avancée
    Element* method_tri_inv(LispE* lisp);
    Element* method_cholesky_inv(LispE* lisp);
    Element* method_lu_factor(LispE* lisp);
    Element* method_solve_triangular(LispE* lisp);
    Element* method_eigvalsh(LispE* lisp);
    Element* method_eigh(LispE* lisp);
    // Groupe 27: Gather/Scatter avancé
    Element* method_take_along_axis(LispE* lisp);
    Element* method_put_along_axis(LispE* lisp);
    Element* method_gather(LispE* lisp);
    Element* method_scatter(LispE* lisp);
    Element* method_scatter_add(LispE* lisp);
    Element* method_slice_update(LispE* lisp);
    // Groupe 28: Nombres complexes
    Element* method_conjugate(LispE* lisp);
    Element* method_real(LispE* lisp);
    Element* method_imag(LispE* lisp);
    // Groupe 29: Opérations fusionnées pour LLM
    Element* method_fused_mlp(LispE* lisp);
    Element* method_fused_moe(LispE* lisp);
    Element* method_fused_moe_batch(LispE* lisp);
    // Groupe 30: Évaluation explicite (lazy evaluation)
    Element* method_eval(LispE* lisp);
    // Groupe 31: Conversion vers LispE
    Element* method_tolist(LispE* lisp);
};

// ============================================================================
// Implémentation des méthodes
// ============================================================================

Element* Lispe_mlx_methods::eval(LispE* lisp) {
    switch (action) {
        case mlx_method_array:
            return method_array(lisp);
        case mlx_method_shape:
            return method_shape(lisp);
        case mlx_method_reshape:
            return method_reshape(lisp);
        case mlx_method_take:
            return method_take(lisp);
        case mlx_method_mean:
            return method_mean(lisp);
        case mlx_method_add:
            return method_add(lisp);
        case mlx_method_multiply:
            return method_multiply(lisp);
        case mlx_method_sum:
            return method_sum(lisp);
        case mlx_method_transpose:
            return method_transpose(lisp);
        case mlx_method_matmul:
            return method_matmul(lisp);
        case mlx_method_subtract:
            return method_subtract(lisp);
        case mlx_method_divide:
            return method_divide(lisp);
        case mlx_method_maximum:
            return method_maximum(lisp);
        case mlx_method_minimum:
            return method_minimum(lisp);
        case mlx_method_sqrt:
            return method_sqrt(lisp);
        case mlx_method_exp:
            return method_exp(lisp);
        case mlx_method_log:
            return method_log(lisp);
        case mlx_method_abs:
            return method_abs(lisp);
        case mlx_method_concatenate:
            return method_concatenate(lisp);
        case mlx_method_split:
            return method_split(lisp);
        // Fonctions d'activation
        case mlx_method_sigmoid:
            return method_sigmoid(lisp);
        case mlx_method_relu:
            return method_relu(lisp);
        case mlx_method_softmax:
            return method_softmax(lisp);
        case mlx_method_tanh:
            return method_tanh(lisp);
        // Fonctions trigonométriques
        case mlx_method_sin:
            return method_sin(lisp);
        case mlx_method_cos:
            return method_cos(lisp);
        case mlx_method_tan:
            return method_tan(lisp);
        // Fonctions de réduction avancées
        case mlx_method_argmax:
            return method_argmax(lisp);
        case mlx_method_argmin:
            return method_argmin(lisp);
        // Fonctions supplémentaires
        case mlx_method_power:
            return method_power(lisp);
        case mlx_method_negative:
            return method_negative(lisp);
        case mlx_method_square:
            return method_square(lisp);
        // Groupe 1: Création d'arrays
        case mlx_method_zeros:
            return method_zeros(lisp);
        case mlx_method_ones:
            return method_ones(lisp);
        case mlx_method_full:
            return method_full(lisp);
        case mlx_method_arange:
            return method_arange(lisp);
        case mlx_method_linspace:
            return method_linspace(lisp);
        case mlx_method_eye:
            return method_eye(lisp);
        // Groupe 2: Nombres aléatoires
        case mlx_method_random_uniform:
            return method_random_uniform(lisp);
        case mlx_method_random_normal:
            return method_random_normal(lisp);
        case mlx_method_random_randint:
            return method_random_randint(lisp);
        // Groupe 3: Mathématiques
        case mlx_method_log2:
            return method_log2(lisp);
        case mlx_method_log10:
            return method_log10(lisp);
        case mlx_method_log1p:
            return method_log1p(lisp);
        case mlx_method_floor:
            return method_floor(lisp);
        case mlx_method_ceil:
            return method_ceil(lisp);
        case mlx_method_round:
            return method_round(lisp);
        case mlx_method_clip:
            return method_clip(lisp);
        case mlx_method_reciprocal:
            return method_reciprocal(lisp);
        case mlx_method_rsqrt:
            return method_rsqrt(lisp);
        // Groupe 4: Trigonométrie inverse
        case mlx_method_arcsin:
            return method_arcsin(lisp);
        case mlx_method_arccos:
            return method_arccos(lisp);
        case mlx_method_arctan:
            return method_arctan(lisp);
        case mlx_method_sinh:
            return method_sinh(lisp);
        case mlx_method_cosh:
            return method_cosh(lisp);
        // Groupe 5: Réductions
        case mlx_method_prod:
            return method_prod(lisp);
        case mlx_method_max:
            return method_max(lisp);
        case mlx_method_min:
            return method_min(lisp);
        case mlx_method_var:
            return method_var(lisp);
        case mlx_method_std:
            return method_std(lisp);
        case mlx_method_all:
            return method_all(lisp);
        case mlx_method_any:
            return method_any(lisp);
        // Groupe 6: Comparaisons
        case mlx_method_equal:
            return method_equal(lisp);
        case mlx_method_not_equal:
            return method_not_equal(lisp);
        case mlx_method_greater:
            return method_greater(lisp);
        case mlx_method_less:
            return method_less(lisp);
        case mlx_method_greater_equal:
            return method_greater_equal(lisp);
        case mlx_method_less_equal:
            return method_less_equal(lisp);
        case mlx_method_where:
            return method_where(lisp);
        // Groupe 7: Manipulation
        case mlx_method_flatten:
            return method_flatten(lisp);
        case mlx_method_squeeze:
            return method_squeeze(lisp);
        case mlx_method_expand_dims:
            return method_expand_dims(lisp);
        case mlx_method_stack:
            return method_stack(lisp);
        case mlx_method_tile:
            return method_tile(lisp);
        case mlx_method_repeat:
            return method_repeat(lisp);
        case mlx_method_pad:
            return method_pad(lisp);
        // Groupe 8: Activation deep learning
        case mlx_method_gelu:
            return method_gelu(lisp);
        case mlx_method_gelu_tanh:
            return method_gelu_tanh(lisp);
        case mlx_method_silu:
            return method_silu(lisp);
        case mlx_method_leaky_relu:
            return method_leaky_relu(lisp);
        case mlx_method_elu:
            return method_elu(lisp);
        case mlx_method_selu:
            return method_selu(lisp);
        case mlx_method_log_softmax:
            return method_log_softmax(lisp);
        // Groupe 9: Algèbre linéaire
        case mlx_method_norm:
            return method_norm(lisp);
        case mlx_method_inv:
            return method_inv(lisp);
        case mlx_method_svd:
            return method_svd(lisp);
        case mlx_method_qr:
            return method_qr(lisp);
        case mlx_method_cholesky:
            return method_cholesky(lisp);
        case mlx_method_solve:
            return method_solve(lisp);
        case mlx_method_tucker:
            return method_tucker(lisp);
        case mlx_method_hosvd:
            return method_hosvd(lisp);
        case mlx_method_tucker_reconstruct:
            return method_tucker_reconstruct(lisp);
        case mlx_method_tucker_compression_ratio:
            return method_tucker_compression_ratio(lisp);
        case mlx_method_khatri_rao_product:
            return method_khatri_rao_product(lisp);
        // Groupe 10: Opérations logiques et tests
        case mlx_method_logical_and:
            return method_logical_and(lisp);
        case mlx_method_logical_or:
            return method_logical_or(lisp);
        case mlx_method_logical_not:
            return method_logical_not(lisp);
        case mlx_method_isnan:
            return method_isnan(lisp);
        case mlx_method_isinf:
            return method_isinf(lisp);
        case mlx_method_isfinite:
            return method_isfinite(lisp);
        // Groupe 11: Manipulation avancée
        case mlx_method_flip:
            return method_flip(lisp);
        case mlx_method_roll:
            return method_roll(lisp);
        case mlx_method_sort:
            return method_sort(lisp);
        case mlx_method_argsort:
            return method_argsort(lisp);
        case mlx_method_argwhere:
            return method_argwhere(lisp);
        case mlx_method_moveaxis:
            return method_moveaxis(lisp);
        case mlx_method_swapaxes:
            return method_swapaxes(lisp);
        case mlx_method_broadcast_to:
            return method_broadcast_to(lisp);
        // Groupe 12: Création avancée
        case mlx_method_identity:
            return method_identity(lisp);
        case mlx_method_tri:
            return method_tri(lisp);
        case mlx_method_tril:
            return method_tril(lisp);
        case mlx_method_triu:
            return method_triu(lisp);
        case mlx_method_meshgrid:
            return method_meshgrid(lisp);
        // Groupe 13: Algèbre linéaire avancée
        case mlx_method_pinv:
            return method_pinv(lisp);
        case mlx_method_eig:
            return method_eig(lisp);
        case mlx_method_eigvals:
            return method_eigvals(lisp);
        case mlx_method_lu:
            return method_lu(lisp);
        case mlx_method_cross:
            return method_cross(lisp);
        // Groupe 14: I/O de modèles
        case mlx_method_save:
            return method_save(lisp);
        case mlx_method_load:
            return method_load(lisp);
        case mlx_method_load_safetensors:
            return method_load_safetensors(lisp);
        case mlx_method_save_safetensors:
            return method_save_safetensors(lisp);
        case mlx_method_load_gguf:
            return method_load_gguf(lisp);
        case mlx_method_save_gguf:
            return method_save_gguf(lisp);
        // Groupe 15: Quantification et mémoire
        case mlx_method_quantize:
            return method_quantize(lisp);
        case mlx_method_dequantize:
            return method_dequantize(lisp);
        case mlx_method_quantized_matmul:
            return method_quantized_matmul(lisp);
        case mlx_method_synchronize:
            return method_synchronize(lisp);
        case mlx_method_clear_cache:
            return method_clear_cache(lisp);
        case mlx_method_get_active_memory:
            return method_get_active_memory(lisp);
        case mlx_method_get_peak_memory:
            return method_get_peak_memory(lisp);
        case mlx_method_get_cache_memory:
            return method_get_cache_memory(lisp);
        case mlx_method_set_memory_limit:
            return method_set_memory_limit(lisp);
        case mlx_method_set_cache_limit:
            return method_set_cache_limit(lisp);
        // Groupe 16: Opérations supplémentaires
        case mlx_method_slice:
            return method_slice(lisp);
        case mlx_method_trace:
            return method_trace(lisp);
        case mlx_method_diagonal:
            return method_diagonal(lisp);
        case mlx_method_diag:
            return method_diag(lisp);
        case mlx_method_tensordot:
            return method_tensordot(lisp);
        case mlx_method_inner:
            return method_inner(lisp);
        case mlx_method_outer:
            return method_outer(lisp);
        case mlx_method_cumsum:
            return method_cumsum(lisp);
        case mlx_method_cumprod:
            return method_cumprod(lisp);
        case mlx_method_topk:
            return method_topk(lisp);
        case mlx_method_partition:
            return method_partition(lisp);
        // Groupe 17: FFT
        case mlx_method_fft:
            return method_fft(lisp);
        case mlx_method_ifft:
            return method_ifft(lisp);
        case mlx_method_fft2:
            return method_fft2(lisp);
        case mlx_method_ifft2:
            return method_ifft2(lisp);
        case mlx_method_fftn:
            return method_fftn(lisp);
        case mlx_method_ifftn:
            return method_ifftn(lisp);
        case mlx_method_rfft:
            return method_rfft(lisp);
        case mlx_method_irfft:
            return method_irfft(lisp);
        case mlx_method_rfft2:
            return method_rfft2(lisp);
        case mlx_method_irfft2:
            return method_irfft2(lisp);
        case mlx_method_rfftn:
            return method_rfftn(lisp);
        case mlx_method_irfftn:
            return method_irfftn(lisp);
        case mlx_method_fftshift:
            return method_fftshift(lisp);
        case mlx_method_ifftshift:
            return method_ifftshift(lisp);
        // Groupe 18: Fonctions mathématiques avancées
        case mlx_method_arctan2:
            return method_arctan2(lisp);
        case mlx_method_arcsinh:
            return method_arcsinh(lisp);
        case mlx_method_arccosh:
            return method_arccosh(lisp);
        case mlx_method_arctanh:
            return method_arctanh(lisp);
        case mlx_method_degrees:
            return method_degrees(lisp);
        case mlx_method_radians:
            return method_radians(lisp);
        case mlx_method_erf:
            return method_erf(lisp);
        case mlx_method_erfinv:
            return method_erfinv(lisp);
        case mlx_method_expm1:
            return method_expm1(lisp);
        case mlx_method_logaddexp:
            return method_logaddexp(lisp);
        case mlx_method_sign:
            return method_sign(lisp);
        case mlx_method_kron:
            return method_kron(lisp);
        // Groupe 19: Opérations bitwise
        case mlx_method_bitwise_and:
            return method_bitwise_and(lisp);
        case mlx_method_bitwise_or:
            return method_bitwise_or(lisp);
        case mlx_method_bitwise_xor:
            return method_bitwise_xor(lisp);
        case mlx_method_bitwise_invert:
            return method_bitwise_invert(lisp);
        case mlx_method_left_shift:
            return method_left_shift(lisp);
        case mlx_method_right_shift:
            return method_right_shift(lisp);
        // Groupe 20: Division et reste
        case mlx_method_divmod:
            return method_divmod(lisp);
        case mlx_method_floor_divide:
            return method_floor_divide(lisp);
        case mlx_method_remainder:
            return method_remainder(lisp);
        // Groupe 21: Opérations cumulatives et logsumexp
        case mlx_method_cummax:
            return method_cummax(lisp);
        case mlx_method_cummin:
            return method_cummin(lisp);
        case mlx_method_logsumexp:
            return method_logsumexp(lisp);
        case mlx_method_logcumsumexp:
            return method_logcumsumexp(lisp);
        // Groupe 22: Convolutions (deep learning)
        case mlx_method_conv1d:
            return method_conv1d(lisp);
        case mlx_method_conv2d:
            return method_conv2d(lisp);
        case mlx_method_conv3d:
            return method_conv3d(lisp);
        case mlx_method_conv_transpose1d:
            return method_conv_transpose1d(lisp);
        case mlx_method_conv_transpose2d:
            return method_conv_transpose2d(lisp);
        case mlx_method_conv_transpose3d:
            return method_conv_transpose3d(lisp);
        case mlx_method_conv_general:
            return method_conv_general(lisp);
        // Groupe 23: Opérations spécialisées ML
        case mlx_method_rms_norm:
            return method_rms_norm(lisp);
        case mlx_method_layer_norm:
            return method_layer_norm(lisp);
        case mlx_method_rope:
            return method_rope(lisp);
        case mlx_method_scaled_dot_product_attention:
            return method_scaled_dot_product_attention(lisp);
        case mlx_method_einsum:
            return method_einsum(lisp);
        // Groupe 24: Manipulation et création avancée
        case mlx_method_dtype:
            return method_dtype(lisp);
        case mlx_method_astype:
            return method_astype(lisp);
        case mlx_method_copy:
            return method_copy(lisp);
        case mlx_method_zeros_like:
            return method_zeros_like(lisp);
        case mlx_method_ones_like:
            return method_ones_like(lisp);
        case mlx_method_nan_to_num:
            return method_nan_to_num(lisp);
        case mlx_method_atleast_1d:
            return method_atleast_1d(lisp);
        case mlx_method_atleast_2d:
            return method_atleast_2d(lisp);
        case mlx_method_atleast_3d:
            return method_atleast_3d(lisp);
        case mlx_method_array_equal:
            return method_array_equal(lisp);
        case mlx_method_allclose:
            return method_allclose(lisp);
        // Groupe 25: Random avancé
        case mlx_method_random_bernoulli:
            return method_random_bernoulli(lisp);
        case mlx_method_random_truncated_normal:
            return method_random_truncated_normal(lisp);
        case mlx_method_random_gumbel:
            return method_random_gumbel(lisp);
        case mlx_method_random_categorical:
            return method_random_categorical(lisp);
        case mlx_method_random_laplace:
            return method_random_laplace(lisp);
        case mlx_method_random_multivariate_normal:
            return method_random_multivariate_normal(lisp);
        case mlx_method_random_permutation:
            return method_random_permutation(lisp);
        // Groupe 26: Algèbre linéaire avancée
        case mlx_method_tri_inv:
            return method_tri_inv(lisp);
        case mlx_method_cholesky_inv:
            return method_cholesky_inv(lisp);
        case mlx_method_lu_factor:
            return method_lu_factor(lisp);
        case mlx_method_solve_triangular:
            return method_solve_triangular(lisp);
        case mlx_method_eigvalsh:
            return method_eigvalsh(lisp);
        case mlx_method_eigh:
            return method_eigh(lisp);
        // Groupe 27: Gather/Scatter avancé
        case mlx_method_take_along_axis:
            return method_take_along_axis(lisp);
        case mlx_method_put_along_axis:
            return method_put_along_axis(lisp);
        case mlx_method_gather:
            return method_gather(lisp);
        case mlx_method_scatter:
            return method_scatter(lisp);
        case mlx_method_scatter_add:
            return method_scatter_add(lisp);
        case mlx_method_slice_update:
            return method_slice_update(lisp);
        // Groupe 28: Nombres complexes
        case mlx_method_conjugate:
            return method_conjugate(lisp);
        case mlx_method_real:
            return method_real(lisp);
        case mlx_method_imag:
            return method_imag(lisp);
        // Groupe 29: Opérations fusionnées pour LLM
        case mlx_method_fused_mlp:
            return method_fused_mlp(lisp);
        case mlx_method_fused_moe:
            return method_fused_moe(lisp);
        case mlx_method_fused_moe_batch:
            return method_fused_moe_batch(lisp);
        // Groupe 30: Évaluation explicite (lazy evaluation)
        case mlx_method_eval:
            return method_eval(lisp);
        // Groupe 31: Conversion vers LispE
        case mlx_method_tolist:
            return method_tolist(lisp);
        default:
            return null_;
    }
}

wstring Lispe_mlx_methods::asString(LispE* lisp) {
    switch (action) {
        case mlx_method_array:
            return L"Create an MLX array from numeric data";
        case mlx_method_shape:
            return L"Create an MLX shape from numeric data";
        case mlx_method_reshape:
            return L"Reshape an MLX array to a new shape";
        case mlx_method_take:
            return L"Extract elements from an MLX array using indices";
        case mlx_method_mean:
            return L"Compute the mean of an MLX array along specified axes";
        case mlx_method_add:
            return L"Add two MLX arrays element-wise";
        case mlx_method_multiply:
            return L"Multiply two MLX arrays element-wise";
        case mlx_method_sum:
            return L"Sum all elements or along specified axes";
        case mlx_method_transpose:
            return L"Transpose an MLX array";
        case mlx_method_matmul:
            return L"Matrix multiplication of two MLX arrays";
        case mlx_method_subtract:
            return L"Subtract two MLX arrays element-wise";
        case mlx_method_divide:
            return L"Divide two MLX arrays element-wise";
        case mlx_method_maximum:
            return L"Element-wise maximum of two MLX arrays";
        case mlx_method_minimum:
            return L"Element-wise minimum of two MLX arrays";
        case mlx_method_sqrt:
            return L"Element-wise square root of an MLX array";
        case mlx_method_exp:
            return L"Element-wise exponential of an MLX array";
        case mlx_method_log:
            return L"Element-wise natural logarithm of an MLX array";
        case mlx_method_abs:
            return L"Element-wise absolute value of an MLX array";
        case mlx_method_concatenate:
            return L"Concatenate MLX arrays along a specified axis";
        case mlx_method_split:
            return L"Split an MLX array into multiple sub-arrays";
        // Fonctions d'activation
        case mlx_method_sigmoid:
            return L"Apply sigmoid activation function element-wise";
        case mlx_method_relu:
            return L"Apply ReLU activation function element-wise";
        case mlx_method_softmax:
            return L"Apply softmax activation function along an axis";
        case mlx_method_tanh:
            return L"Apply hyperbolic tangent activation function element-wise";
        // Fonctions trigonométriques
        case mlx_method_sin:
            return L"Compute sine element-wise";
        case mlx_method_cos:
            return L"Compute cosine element-wise";
        case mlx_method_tan:
            return L"Compute tangent element-wise";
        // Fonctions de réduction avancées
        case mlx_method_argmax:
            return L"Return indices of maximum values along an axis";
        case mlx_method_argmin:
            return L"Return indices of minimum values along an axis";
        // Fonctions supplémentaires
        case mlx_method_power:
            return L"Raise array elements to a power element-wise";
        case mlx_method_negative:
            return L"Negate array elements element-wise";
        case mlx_method_square:
            return L"Compute square of array elements element-wise";
        // Groupe 1: Création d'arrays
        case mlx_method_zeros:
            return L"Create an array filled with zeros";
        case mlx_method_ones:
            return L"Create an array filled with ones";
        case mlx_method_full:
            return L"Create an array filled with a specified value";
        case mlx_method_arange:
            return L"Create an array with evenly spaced values";
        case mlx_method_linspace:
            return L"Create an array with linearly spaced values";
        case mlx_method_eye:
            return L"Create an identity matrix";
        // Groupe 2: Nombres aléatoires
        case mlx_method_random_uniform:
            return L"Generate uniform random values in [low, high)";
        case mlx_method_random_normal:
            return L"Generate normally distributed random values";
        case mlx_method_random_randint:
            return L"Generate random integers in [low, high)";
        // Groupe 3: Mathématiques
        case mlx_method_log2:
            return L"Compute base-2 logarithm element-wise";
        case mlx_method_log10:
            return L"Compute base-10 logarithm element-wise";
        case mlx_method_log1p:
            return L"Compute log(1+x) element-wise";
        case mlx_method_floor:
            return L"Round down to nearest integer element-wise";
        case mlx_method_ceil:
            return L"Round up to nearest integer element-wise";
        case mlx_method_round:
            return L"Round to nearest integer element-wise";
        case mlx_method_clip:
            return L"Clip array values to a range";
        case mlx_method_reciprocal:
            return L"Compute 1/x element-wise";
        case mlx_method_rsqrt:
            return L"Compute 1/sqrt(x) element-wise";
        // Groupe 4: Trigonométrie inverse
        case mlx_method_arcsin:
            return L"Compute arc sine element-wise";
        case mlx_method_arccos:
            return L"Compute arc cosine element-wise";
        case mlx_method_arctan:
            return L"Compute arc tangent element-wise";
        case mlx_method_sinh:
            return L"Compute hyperbolic sine element-wise";
        case mlx_method_cosh:
            return L"Compute hyperbolic cosine element-wise";
        // Groupe 5: Réductions
        case mlx_method_prod:
            return L"Compute product of array elements";
        case mlx_method_max:
            return L"Compute maximum of array elements";
        case mlx_method_min:
            return L"Compute minimum of array elements";
        case mlx_method_var:
            return L"Compute variance of array elements";
        case mlx_method_std:
            return L"Compute standard deviation of array elements";
        case mlx_method_all:
            return L"Test if all elements are true";
        case mlx_method_any:
            return L"Test if any element is true";
        // Groupe 6: Comparaisons
        case mlx_method_equal:
            return L"Element-wise equality comparison";
        case mlx_method_not_equal:
            return L"Element-wise inequality comparison";
        case mlx_method_greater:
            return L"Element-wise greater than comparison";
        case mlx_method_less:
            return L"Element-wise less than comparison";
        case mlx_method_greater_equal:
            return L"Element-wise greater than or equal comparison";
        case mlx_method_less_equal:
            return L"Element-wise less than or equal comparison";
        case mlx_method_where:
            return L"Select elements based on condition";
        // Groupe 7: Manipulation
        case mlx_method_flatten:
            return L"Flatten array to 1D";
        case mlx_method_squeeze:
            return L"Remove dimensions of size 1";
        case mlx_method_expand_dims:
            return L"Add a dimension at specified axis";
        case mlx_method_stack:
            return L"Stack arrays along a new axis";
        case mlx_method_tile:
            return L"Tile array by repeating along axes";
        case mlx_method_repeat:
            return L"Repeat array elements along an axis";
        case mlx_method_pad:
            return L"Pad array with values";
        // Groupe 8: Activation deep learning
        case mlx_method_gelu:
            return L"Apply GELU activation function";
        case mlx_method_silu:
            return L"Apply SiLU/Swish activation function";
        case mlx_method_leaky_relu:
            return L"Apply Leaky ReLU activation function";
        case mlx_method_elu:
            return L"Apply ELU activation function";
        case mlx_method_selu:
            return L"Apply SELU activation function";
        case mlx_method_log_softmax:
            return L"Apply log softmax function";
        // Groupe 9: Algèbre linéaire
        case mlx_method_norm:
            return L"Compute vector or matrix norm";
        case mlx_method_inv:
            return L"Compute matrix inverse";
        case mlx_method_svd:
            return L"Compute singular value decomposition";
        case mlx_method_qr:
            return L"Compute QR decomposition";
        case mlx_method_cholesky:
            return L"Compute Cholesky decomposition";
        case mlx_method_solve:
            return L"Solve linear system Ax = b";
        case mlx_method_tucker:
            return L"Tucker decomposition of a tensor (HOOI algorithm)";
        case mlx_method_hosvd:
            return L"Higher-Order SVD decomposition of a tensor";
        case mlx_method_tucker_reconstruct:
            return L"Reconstruct tensor from Tucker decomposition (core, factors)";
        case mlx_method_tucker_compression_ratio:
            return L"Compute compression ratio of Tucker decomposition";
        case mlx_method_khatri_rao_product:
            return L"Khatri-Rao product (column-wise Kronecker product)";
        // Groupe 10: Opérations logiques et tests
        case mlx_method_logical_and:
            return L"Element-wise logical AND";
        case mlx_method_logical_or:
            return L"Element-wise logical OR";
        case mlx_method_logical_not:
            return L"Element-wise logical NOT";
        case mlx_method_isnan:
            return L"Test for NaN values";
        case mlx_method_isinf:
            return L"Test for infinite values";
        case mlx_method_isfinite:
            return L"Test for finite values";
        // Groupe 11: Manipulation avancée
        case mlx_method_flip:
            return L"Reverse array along axes";
        case mlx_method_roll:
            return L"Roll array elements along axis";
        case mlx_method_sort:
            return L"Sort array along axis";
        case mlx_method_argsort:
            return L"Indices that would sort array";
        case mlx_method_argwhere:
            return L"Indices where condition is true";
        case mlx_method_moveaxis:
            return L"Move axes to new positions";
        case mlx_method_swapaxes:
            return L"Swap two axes";
        case mlx_method_broadcast_to:
            return L"Broadcast array to shape";
        // Groupe 12: Création avancée
        case mlx_method_identity:
            return L"Identity matrix (alias for eye)";
        case mlx_method_tri:
            return L"Lower triangular matrix of ones";
        case mlx_method_tril:
            return L"Extract lower triangular part";
        case mlx_method_triu:
            return L"Extract upper triangular part";
        case mlx_method_meshgrid:
            return L"Create coordinate matrices";
        // Groupe 13: Algèbre linéaire avancée
        case mlx_method_pinv:
            return L"Compute the Moore-Penrose pseudo-inverse";
        case mlx_method_eig:
            return L"Compute eigenvalues and eigenvectors";
        case mlx_method_eigvals:
            return L"Compute eigenvalues only";
        case mlx_method_lu:
            return L"Compute LU decomposition";
        case mlx_method_cross:
            return L"Compute cross product of two vectors";
        // Groupe 14: I/O de modèles
        case mlx_method_save:
            return L"Save array to .npy file";
        case mlx_method_load:
            return L"Load array from .npy file";
        case mlx_method_load_safetensors:
            return L"Load tensors from .safetensors file";
        case mlx_method_save_safetensors:
            return L"Save tensors to .safetensors file";
        case mlx_method_load_gguf:
            return L"Load arrays from .gguf file";
        case mlx_method_save_gguf:
            return L"Save arrays to .gguf file";
        // Groupe 15: Quantification et mémoire
        case mlx_method_quantize:
            return L"Quantize a matrix";
        case mlx_method_dequantize:
            return L"Dequantize a matrix";
        case mlx_method_quantized_matmul:
            return L"Quantized matrix multiplication";
        case mlx_method_synchronize:
            return L"Synchronize GPU operations";
        case mlx_method_clear_cache:
            return L"Clear memory cache";
        case mlx_method_get_active_memory:
            return L"Get active memory in bytes";
        case mlx_method_get_peak_memory:
            return L"Get peak memory in bytes";
        case mlx_method_get_cache_memory:
            return L"Get cache memory in bytes";
        case mlx_method_set_memory_limit:
            return L"Set memory limit";
        case mlx_method_set_cache_limit:
            return L"Set cache limit";
        // Groupe 16: Opérations supplémentaires
        case mlx_method_slice:
            return L"Extract a slice from an array";
        case mlx_method_trace:
            return L"Compute matrix trace";
        case mlx_method_diagonal:
            return L"Extract diagonal from matrix";
        case mlx_method_diag:
            return L"Create diagonal matrix from vector";
        case mlx_method_tensordot:
            return L"Compute tensor dot product";
        case mlx_method_inner:
            return L"Compute inner product";
        case mlx_method_outer:
            return L"Compute outer product";
        case mlx_method_cumsum:
            return L"Compute cumulative sum";
        case mlx_method_cumprod:
            return L"Compute cumulative product";
        case mlx_method_topk:
            return L"Get top-k values";
        case mlx_method_partition:
            return L"Partition array around kth element";
        // Groupe 17: FFT
        case mlx_method_fft:
            return L"Compute 1D Fast Fourier Transform";
        case mlx_method_ifft:
            return L"Compute 1D Inverse Fast Fourier Transform";
        case mlx_method_fft2:
            return L"Compute 2D Fast Fourier Transform";
        case mlx_method_ifft2:
            return L"Compute 2D Inverse Fast Fourier Transform";
        case mlx_method_fftn:
            return L"Compute N-dimensional Fast Fourier Transform";
        case mlx_method_ifftn:
            return L"Compute N-dimensional Inverse Fast Fourier Transform";
        case mlx_method_rfft:
            return L"Compute 1D Real Fast Fourier Transform";
        case mlx_method_irfft:
            return L"Compute 1D Inverse Real Fast Fourier Transform";
        case mlx_method_rfft2:
            return L"Compute 2D Real Fast Fourier Transform";
        case mlx_method_irfft2:
            return L"Compute 2D Inverse Real Fast Fourier Transform";
        case mlx_method_rfftn:
            return L"Compute N-dimensional Real Fast Fourier Transform";
        case mlx_method_irfftn:
            return L"Compute N-dimensional Inverse Real Fast Fourier Transform";
        case mlx_method_fftshift:
            return L"Shift zero-frequency component to center";
        case mlx_method_ifftshift:
            return L"Inverse of fftshift";
        // Groupe 18: Fonctions mathématiques avancées
        case mlx_method_arctan2:
            return L"Element-wise arc tangent of a/b with correct quadrant";
        case mlx_method_arcsinh:
            return L"Compute inverse hyperbolic sine element-wise";
        case mlx_method_arccosh:
            return L"Compute inverse hyperbolic cosine element-wise";
        case mlx_method_arctanh:
            return L"Compute inverse hyperbolic tangent element-wise";
        case mlx_method_degrees:
            return L"Convert radians to degrees element-wise";
        case mlx_method_radians:
            return L"Convert degrees to radians element-wise";
        case mlx_method_erf:
            return L"Compute Gauss error function element-wise";
        case mlx_method_erfinv:
            return L"Compute inverse error function element-wise";
        case mlx_method_expm1:
            return L"Compute exp(x)-1 element-wise (accurate for small x)";
        case mlx_method_logaddexp:
            return L"Compute log(exp(a)+exp(b)) in a numerically stable way";
        case mlx_method_sign:
            return L"Return element-wise sign (-1, 0, or 1)";
        case mlx_method_kron:
            return L"Compute Kronecker product of two arrays";
        // Groupe 19: Opérations bitwise
        case mlx_method_bitwise_and:
            return L"Element-wise bitwise AND";
        case mlx_method_bitwise_or:
            return L"Element-wise bitwise OR";
        case mlx_method_bitwise_xor:
            return L"Element-wise bitwise XOR";
        case mlx_method_bitwise_invert:
            return L"Element-wise bitwise NOT (complement)";
        case mlx_method_left_shift:
            return L"Element-wise left bit shift";
        case mlx_method_right_shift:
            return L"Element-wise right bit shift";
        // Groupe 20: Division et reste
        case mlx_method_divmod:
            return L"Return quotient and remainder as a list";
        case mlx_method_floor_divide:
            return L"Element-wise floor division";
        case mlx_method_remainder:
            return L"Element-wise remainder of division";
        // Groupe 21: Opérations cumulatives et logsumexp
        case mlx_method_cummax:
            return L"Cumulative maximum along an axis";
        case mlx_method_cummin:
            return L"Cumulative minimum along an axis";
        case mlx_method_logsumexp:
            return L"Compute log(sum(exp(x))) in a numerically stable way";
        case mlx_method_logcumsumexp:
            return L"Cumulative logsumexp along an axis";
        // Groupe 22: Convolutions (deep learning)
        case mlx_method_conv1d:
            return L"1D convolution with a filter";
        case mlx_method_conv2d:
            return L"2D convolution with a filter";
        case mlx_method_conv3d:
            return L"3D convolution with a filter";
        case mlx_method_conv_transpose1d:
            return L"1D transposed convolution (deconvolution)";
        case mlx_method_conv_transpose2d:
            return L"2D transposed convolution (deconvolution)";
        case mlx_method_conv_transpose3d:
            return L"3D transposed convolution (deconvolution)";
        case mlx_method_conv_general:
            return L"General N-D convolution with full control over parameters";
        // Groupe 23: Opérations spécialisées ML
        case mlx_method_rms_norm:
            return L"Root Mean Square normalization (used in LLaMA)";
        case mlx_method_layer_norm:
            return L"Layer normalization";
        case mlx_method_rope:
            return L"Rotary Position Embedding (RoPE)";
        case mlx_method_scaled_dot_product_attention:
            return L"Scaled dot-product attention mechanism";
        case mlx_method_einsum:
            return L"Einstein summation convention";
        // Groupe 24: Manipulation et création avancée
        case mlx_method_dtype:
            return L"Get the data type of an array as a string";
        case mlx_method_astype:
            return L"Cast array to a different data type";
        case mlx_method_copy:
            return L"Create an explicit copy of an array";
        case mlx_method_zeros_like:
            return L"Create array of zeros with same shape as input";
        case mlx_method_ones_like:
            return L"Create array of ones with same shape as input";
        case mlx_method_nan_to_num:
            return L"Replace NaN and infinity values with specified numbers";
        case mlx_method_atleast_1d:
            return L"Convert input to at least 1-dimensional array";
        case mlx_method_atleast_2d:
            return L"Convert input to at least 2-dimensional array";
        case mlx_method_atleast_3d:
            return L"Convert input to at least 3-dimensional array";
        case mlx_method_array_equal:
            return L"Test if two arrays are element-wise equal";
        case mlx_method_allclose:
            return L"Test if two arrays are element-wise close within tolerance";
        // Groupe 25: Random avancé
        case mlx_method_random_bernoulli:
            return L"Generate Bernoulli random samples with given probability";
        case mlx_method_random_truncated_normal:
            return L"Generate truncated normal distribution samples";
        case mlx_method_random_gumbel:
            return L"Generate Gumbel distribution samples";
        case mlx_method_random_categorical:
            return L"Generate categorical distribution samples from logits";
        case mlx_method_random_laplace:
            return L"Generate Laplace distribution samples";
        case mlx_method_random_multivariate_normal:
            return L"Generate multivariate normal distribution samples";
        case mlx_method_random_permutation:
            return L"Generate random permutation of array or indices";
        // Groupe 26: Algèbre linéaire avancée
        case mlx_method_tri_inv:
            return L"Compute the inverse of a triangular matrix";
        case mlx_method_cholesky_inv:
            return L"Compute the inverse of a matrix using Cholesky decomposition";
        case mlx_method_lu_factor:
            return L"Compute LU factorization with pivoting";
        case mlx_method_solve_triangular:
            return L"Solve a triangular system of linear equations";
        case mlx_method_eigvalsh:
            return L"Compute eigenvalues of a Hermitian/symmetric matrix";
        case mlx_method_eigh:
            return L"Compute eigenvalues and eigenvectors of a Hermitian/symmetric matrix";
        // Groupe 27: Gather/Scatter avancé
        case mlx_method_take_along_axis:
            return L"Take values along an axis at given indices";
        case mlx_method_put_along_axis:
            return L"Put values into array at indices along an axis";
        case mlx_method_gather:
            return L"Gather array entries given indices and slices";
        case mlx_method_scatter:
            return L"Scatter updates into array at given indices";
        case mlx_method_scatter_add:
            return L"Scatter and add updates at given indices";
        case mlx_method_slice_update:
            return L"Update a slice of the source array";
        // Groupe 28: Nombres complexes
        case mlx_method_conjugate:
            return L"Compute the complex conjugate of an array";
        case mlx_method_real:
            return L"Extract the real part of a complex array";
        case mlx_method_imag:
            return L"Extract the imaginary part of a complex array";
        // Groupe 31: Conversion vers LispE
        case mlx_method_tolist:
            return L"Convert MLX array to LispE list (floats, integers, or numbers)";
    }
    return L"";
}

// mlx_shape - Crée un MLXShape à partir de données numériques OU extrait la shape d'un MLXArray
// Signature: deflib mlx_shape(data)
Element* Lispe_mlx_methods::method_shape(LispE* lisp) {
    Element* data_elem = lisp->get_variable("data");

    try {
        // Si c'est un MLXArray, extraire sa shape
        if (data_elem->type == t_mlx_array) {
            mx::Shape shape = ((MLXArray*)data_elem)->array.shape();
            return new MLXShape(std::move(shape));
        }
        
        // Sinon, créer un shape à partir des données
        mx::Shape shape;
        element_to_shape(data_elem, shape);
        return new MLXShape(std::move(shape));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_shape: " + std::string(e.what()));
    }
}

// mlx::array - Crée un mx::array à partir de données numériques
// Signature: deflib mlx_array(data (shape) (dtype))
Element* Lispe_mlx_methods::method_array(LispE* lisp) {
    Element* data_elem = lisp->get_variable("data");
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        // Convertir les données en mx::array
        mx::array arr = element_to_array(lisp, data_elem);

        // Si shape est fourni, redimensionner
        if (shape_elem != null_) {
            mx::Shape new_shape;
            element_to_shape(shape_elem, new_shape);
            arr = mx::reshape(arr, new_shape);
        }

        // Si dtype est fourni, convertir le type
        if (dtype_elem != null_) {
            std::wstring dtype_str = dtype_elem->asString(lisp);
            mx::Dtype target_dtype = mx::float32; // Par défaut

            if (dtype_str == L"float32") {
                target_dtype = mx::float32;
            } else if (dtype_str == L"float64") {
                target_dtype = mx::float64;
            } else if (dtype_str == L"int32") {
                target_dtype = mx::int32;
            } else if (dtype_str == L"int64") {
                target_dtype = mx::int64;
            } else {
                throw new Error("Error in mlx_array: unsupported dtype. Use float32, float64, int32, or int64");
            }

            arr = mx::astype(arr, target_dtype);
        }

        mx::eval(arr);
        return new MLXArray(std::move(arr));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_array: " + std::string(e.what()));
    }
}

// mlx::reshape - Redimensionne un array
// Signature: deflib mlx_reshape(array shape)
Element* Lispe_mlx_methods::method_reshape(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* shape_elem = lisp->get_variable("shape");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::Shape new_shape;
        element_to_shape(shape_elem, new_shape);

        mx::array result = mx::reshape(arr, new_shape);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_reshape: " + std::string(e.what()));
    }
}

// mlx::take - Extrait des éléments selon des indices
// Signature: deflib mlx_take(array indices (axis))
Element* Lispe_mlx_methods::method_take(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* indices_elem = lisp->get_variable("indices");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array indices = element_to_array(lisp, indices_elem);

        // Déclarer result avec initialisation conditionnelle
        mx::array result = (axis_elem != null_) ?
            mx::take(arr, indices, axis_elem->asInteger()) :
            mx::take(arr, indices);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_take: " + std::string(e.what()));
    }
}

// mlx::mean - Calcule la moyenne
// Signature: deflib mlx_mean(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_mean(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::mean(arr, element_to_axes(axes_elem),
                    (keepdims_elem != null_) ?
                    keepdims_elem->Boolean() : false) :
            mx::mean(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_mean: " + std::string(e.what()));
    }
}

// mlx::add - Addition élément par élément
// Signature: deflib mlx_add(a b)
Element* Lispe_mlx_methods::method_add(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::add(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_add: " + std::string(e.what()));
    }
}

// mlx::multiply - Multiplication élément par élément
// Signature: deflib mlx_multiply(a b)
Element* Lispe_mlx_methods::method_multiply(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::multiply(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_multiply: " + std::string(e.what()));
    }
}

// mlx::sum - Somme des éléments
// Signature: deflib mlx_sum(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_sum(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::sum(arr, element_to_axes(axes_elem),
                   (keepdims_elem != null_) ?
                   keepdims_elem->Boolean() : false) :
            mx::sum(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sum: " + std::string(e.what()));
    }
}

// mlx::transpose - Transposition d'array
// Signature: deflib mlx_transpose(array (axes))
Element* Lispe_mlx_methods::method_transpose(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::transpose(arr, element_to_axes(axes_elem)) :
            mx::transpose(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_transpose: " + std::string(e.what()));
    }
}

// mlx::matmul - Multiplication matricielle
// Signature: deflib mlx_matmul(a b)
Element* Lispe_mlx_methods::method_matmul(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::matmul(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_matmul: " + std::string(e.what()));
    }
}

// mlx::subtract - Soustraction élément par élément
// Signature: deflib mlx_subtract(a b)
Element* Lispe_mlx_methods::method_subtract(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::subtract(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_subtract: " + std::string(e.what()));
    }
}

// mlx::divide - Division élément par élément
// Signature: deflib mlx_divide(a b)
Element* Lispe_mlx_methods::method_divide(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::divide(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_divide: " + std::string(e.what()));
    }
}

// mlx::maximum - Maximum élément par élément
// Signature: deflib mlx_maximum(a b)
Element* Lispe_mlx_methods::method_maximum(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::maximum(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_maximum: " + std::string(e.what()));
    }
}

// mlx::minimum - Minimum élément par élément
// Signature: deflib mlx_minimum(a b)
Element* Lispe_mlx_methods::method_minimum(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::minimum(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_minimum: " + std::string(e.what()));
    }
}

// mlx::sqrt - Racine carrée élément par élément
// Signature: deflib mlx_sqrt(array)
Element* Lispe_mlx_methods::method_sqrt(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::sqrt(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sqrt: " + std::string(e.what()));
    }
}

// mlx::exp - Exponentielle élément par élément
// Signature: deflib mlx_exp(array)
Element* Lispe_mlx_methods::method_exp(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::exp(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_exp: " + std::string(e.what()));
    }
}

// mlx::log - Logarithme naturel élément par élément
// Signature: deflib mlx_log(array)
Element* Lispe_mlx_methods::method_log(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::log(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_log: " + std::string(e.what()));
    }
}

// mlx::abs - Valeur absolue élément par élément
// Signature: deflib mlx_abs(array)
Element* Lispe_mlx_methods::method_abs(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::abs(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_abs: " + std::string(e.what()));
    }
}

// mlx::concatenate - Concatène des arrays le long d'un axe
// Signature: deflib mlx_concatenate(arrays (axis))
Element* Lispe_mlx_methods::method_concatenate(LispE* lisp) {
    Element* arrays_elem = lisp->get_variable("arrays");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        // Convertir la liste d'arrays
        if (!arrays_elem->isList()) {
            throw new Error("Error in mlx_concatenate: arrays must be a list");
        }

        std::vector<mx::array> arrays;
        for (long i = 0; i < arrays_elem->size(); i++) {
            arrays.push_back(element_to_array(lisp, arrays_elem->index(i)));
        }

        int axis = (axis_elem != null_) ? axis_elem->asInteger() : 0;

        mx::array result = mx::concatenate(arrays, axis);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_concatenate: " + std::string(e.what()));
    }
}

// mlx::split - Divise un array en plusieurs sous-arrays
// Signature: deflib mlx_split(array indices_or_sections (axis))
Element* Lispe_mlx_methods::method_split(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* sections_elem = lisp->get_variable("indices_or_sections");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int axis = (axis_elem != null_) ? axis_elem->asInteger() : 0;

        std::vector<mx::array> result_arrays;

        // Si sections_elem est un entier, diviser en N parties égales
        if (sections_elem->isInteger()) {
            int num_sections = sections_elem->asInteger();
            result_arrays = mx::split(arr, num_sections, axis);
        } else {
            // Sinon, c'est une liste d'indices
            mx::Shape indices;
            element_to_shape(sections_elem, indices);
            result_arrays = mx::split(arr, indices, axis);
        }

        // Créer une liste LispE contenant les résultats
        List* result_list = lisp->provideList();
        for (auto& sub_arr : result_arrays) {
            mx::eval(sub_arr);
            result_list->append(new MLXArray(std::move(sub_arr)));
        }

        return result_list;

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_split: " + std::string(e.what()));
    }
}

// ============================================================================
// Fonctions d'activation
// ============================================================================

// mlx::sigmoid - Fonction d'activation sigmoïde
// Signature: deflib mlx_sigmoid(array)
Element* Lispe_mlx_methods::method_sigmoid(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::sigmoid(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sigmoid: " + std::string(e.what()));
    }
}

// mlx::relu - Fonction d'activation ReLU (max(0, x))
// Signature: deflib mlx_relu(array)
Element* Lispe_mlx_methods::method_relu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        // ReLU = maximum(0, x)
        mx::array result = mx::maximum(arr, mx::array(0.0f));

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_relu: " + std::string(e.what()));
    }
}

// mlx::softmax - Fonction d'activation softmax
// Signature: deflib mlx_softmax(array (axis))
Element* Lispe_mlx_methods::method_softmax(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        int axis = (axis_elem != null_) ? axis_elem->asInteger() : -1;

        mx::array result = mx::softmax(arr, axis);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_softmax: " + std::string(e.what()));
    }
}

// mlx::tanh - Fonction d'activation tangente hyperbolique
// Signature: deflib mlx_tanh(array)
Element* Lispe_mlx_methods::method_tanh(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::tanh(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tanh: " + std::string(e.what()));
    }
}

// ============================================================================
// Fonctions trigonométriques
// ============================================================================

// mlx::sin - Sinus élément par élément
// Signature: deflib mlx_sin(array)
Element* Lispe_mlx_methods::method_sin(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::sin(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sin: " + std::string(e.what()));
    }
}

// mlx::cos - Cosinus élément par élément
// Signature: deflib mlx_cos(array)
Element* Lispe_mlx_methods::method_cos(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::cos(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cos: " + std::string(e.what()));
    }
}

// mlx::tan - Tangente élément par élément
// Signature: deflib mlx_tan(array)
Element* Lispe_mlx_methods::method_tan(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::tan(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tan: " + std::string(e.what()));
    }
}

// ============================================================================
// Fonctions de réduction avancées
// ============================================================================

// mlx::argmax - Retourne les indices des valeurs maximales
// Signature: deflib mlx_argmax(array (axis) (keepdims))
Element* Lispe_mlx_methods::method_argmax(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axis_elem != null_) ?
            mx::argmax(arr, axis_elem->asInteger(),
                      (keepdims_elem != null_) ?
                      keepdims_elem->Boolean() : false) :
            mx::argmax(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_argmax: " + std::string(e.what()));
    }
}

// mlx::argmin - Retourne les indices des valeurs minimales
// Signature: deflib mlx_argmin(array (axis) (keepdims))
Element* Lispe_mlx_methods::method_argmin(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axis_elem != null_) ?
            mx::argmin(arr, axis_elem->asInteger(),
                      (keepdims_elem != null_) ?
                      keepdims_elem->Boolean() : false) :
            mx::argmin(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_argmin: " + std::string(e.what()));
    }
}

// ============================================================================
// Fonctions supplémentaires
// ============================================================================

// mlx::power - Élévation à une puissance élément par élément
// Signature: deflib mlx_power(a b)
Element* Lispe_mlx_methods::method_power(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::power(a, b);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_power: " + std::string(e.what()));
    }
}

// mlx::negative - Négation élément par élément
// Signature: deflib mlx_negative(array)
Element* Lispe_mlx_methods::method_negative(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::negative(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_negative: " + std::string(e.what()));
    }
}

// mlx::square - Carré élément par élément
// Signature: deflib mlx_square(array)
Element* Lispe_mlx_methods::method_square(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = mx::square(arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_square: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 1: Création d'arrays
// ============================================================================

// Fonction helper pour parser le dtype
// Types supportés: float16, float32, float64, bfloat16, complex64,
//                  int8, int16, int32, int64, uint8, uint16, uint32, uint64, bool
mx::Dtype parse_dtype(LispE* lisp, Element* dtype_elem, mx::Dtype default_dtype = mx::float32) {
    if (dtype_elem == null_) return default_dtype;

    std::wstring dtype_str = dtype_elem->asString(lisp);
    
    // Types flottants
    if (dtype_str == L"float16") return mx::float16;
    if (dtype_str == L"float32") return mx::float32;
    if (dtype_str == L"float64") return mx::float64;
    if (dtype_str == L"bfloat16") return mx::bfloat16;
    
    // Type complexe
    if (dtype_str == L"complex64") return mx::complex64;
    
    // Types entiers signés
    if (dtype_str == L"int8") return mx::int8;
    if (dtype_str == L"int16") return mx::int16;
    if (dtype_str == L"int32") return mx::int32;
    if (dtype_str == L"int64") return mx::int64;
    
    // Types entiers non signés
    if (dtype_str == L"uint8") return mx::uint8;
    if (dtype_str == L"uint16") return mx::uint16;
    if (dtype_str == L"uint32") return mx::uint32;
    if (dtype_str == L"uint64") return mx::uint64;
    
    // Type booléen
    if (dtype_str == L"bool") return mx::bool_;

    return default_dtype;
}

// mlx::zeros - Crée un array rempli de zéros
// Signature: deflib mlx_zeros(shape (dtype))
Element* Lispe_mlx_methods::method_zeros(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);

        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        mx::array result = mx::zeros(shape, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_zeros: " + std::string(e.what()));
    }
}

// mlx::ones - Crée un array rempli de uns
// Signature: deflib mlx_ones(shape (dtype))
Element* Lispe_mlx_methods::method_ones(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);

        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        mx::array result = mx::ones(shape, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ones: " + std::string(e.what()));
    }
}

// mlx::full - Crée un array rempli d'une valeur spécifique
// Signature: deflib mlx_full(shape value (dtype))
Element* Lispe_mlx_methods::method_full(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* value_elem = lisp->get_variable("value");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);

        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);

        // Créer le scalar value avec le bon type
        mx::array val_arr = mx::array(value_elem->asNumber());
        val_arr = mx::astype(val_arr, dtype);

        mx::array result = mx::full(shape, val_arr);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_full: " + std::string(e.what()));
    }
}

// mlx::arange - Crée un array avec des valeurs espacées régulièrement
// Signature: deflib mlx_arange(start stop (step) (dtype))
Element* Lispe_mlx_methods::method_arange(LispE* lisp) {
    Element* start_elem = lisp->get_variable("start");
    Element* stop_elem = lisp->get_variable("stop");
    Element* step_elem = lisp->get_variable("step");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        double start = start_elem->asNumber();
        double stop = stop_elem->asNumber();
        double step = (step_elem != null_) ? step_elem->asNumber() : 1.0;
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);

        mx::array result = mx::arange(start, stop, step, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arange: " + std::string(e.what()));
    }
}

// mlx::linspace - Crée un array avec des valeurs espacées linéairement
// Signature: deflib mlx_linspace(start stop num (dtype))
Element* Lispe_mlx_methods::method_linspace(LispE* lisp) {
    Element* start_elem = lisp->get_variable("start");
    Element* stop_elem = lisp->get_variable("stop");
    Element* num_elem = lisp->get_variable("num");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        double start = start_elem->asNumber();
        double stop = stop_elem->asNumber();
        int num = num_elem->asInteger();
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);

        mx::array result = mx::linspace(start, stop, num, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_linspace: " + std::string(e.what()));
    }
}

// mlx::eye - Crée une matrice identité
// Signature: deflib mlx_eye(n (m) (dtype))
Element* Lispe_mlx_methods::method_eye(LispE* lisp) {
    Element* n_elem = lisp->get_variable("n");
    Element* m_elem = lisp->get_variable("m");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        int n = n_elem->asInteger();
        int m = (m_elem != null_) ? m_elem->asInteger() : n;
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);

        mx::array result = mx::eye(n, m, 0, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_eye: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 2: Nombres aléatoires
// ============================================================================

// mlx::random::uniform - Génère des valeurs aléatoires uniformes
// Signature: deflib mlx_random_uniform(shape (low) (high) (dtype))
Element* Lispe_mlx_methods::method_random_uniform(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* low_elem = lisp->get_variable("low");
    Element* high_elem = lisp->get_variable("high");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);

        double low = (low_elem != null_) ? low_elem->asNumber() : 0.0;
        double high = (high_elem != null_) ? high_elem->asNumber() : 1.0;
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);

        mx::array result = mx::random::uniform(mx::array(low), mx::array(high), shape, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_uniform: " + std::string(e.what()));
    }
}

// mlx::random::normal - Génère des valeurs aléatoires normales
// Signature: deflib mlx_random_normal(shape (mean) (std) (dtype))
Element* Lispe_mlx_methods::method_random_normal(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* mean_elem = lisp->get_variable("mean");
    Element* std_elem = lisp->get_variable("std");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);

        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);

        // Génère une distribution normale standard (mean=0, std=1)
        mx::array result = mx::random::normal(shape, dtype);

        // Appliquer mean et std si fournis: result = result * std + mean
        if (std_elem != null_) {
            double std_val = std_elem->asNumber();
            result = mx::multiply(result, mx::array(static_cast<float>(std_val)));
        }
        if (mean_elem != null_) {
            double mean_val = mean_elem->asNumber();
            result = mx::add(result, mx::array(static_cast<float>(mean_val)));
        }

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_normal: " + std::string(e.what()));
    }
}

// mlx::random::randint - Génère des entiers aléatoires
// Signature: deflib mlx_random_randint(low high shape (dtype))
Element* Lispe_mlx_methods::method_random_randint(LispE* lisp) {
    Element* low_elem = lisp->get_variable("low");
    Element* high_elem = lisp->get_variable("high");
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        int low = low_elem->asInteger();
        int high = high_elem->asInteger();

        mx::Shape shape;
        element_to_shape(shape_elem, shape);

        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::int32);

        mx::array result = mx::random::randint(mx::array(low), mx::array(high), shape, dtype);

        return new MLXArray(std::move(result));

    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_randint: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 3: Mathématiques
// ============================================================================

// mlx::log2 - Logarithme base 2
// Signature: deflib mlx_log2(array)
Element* Lispe_mlx_methods::method_log2(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::log2(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_log2: " + std::string(e.what()));
    }
}

// mlx::log10 - Logarithme base 10
// Signature: deflib mlx_log10(array)
Element* Lispe_mlx_methods::method_log10(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::log10(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_log10: " + std::string(e.what()));
    }
}

// mlx::log1p - Logarithme de (1+x)
// Signature: deflib mlx_log1p(array)
Element* Lispe_mlx_methods::method_log1p(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::log1p(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_log1p: " + std::string(e.what()));
    }
}

// mlx::floor - Arrondi vers le bas
// Signature: deflib mlx_floor(array)
Element* Lispe_mlx_methods::method_floor(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::floor(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_floor: " + std::string(e.what()));
    }
}

// mlx::ceil - Arrondi vers le haut
// Signature: deflib mlx_ceil(array)
Element* Lispe_mlx_methods::method_ceil(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::ceil(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ceil: " + std::string(e.what()));
    }
}

// mlx::round - Arrondi au plus proche
// Signature: deflib mlx_round(array)
Element* Lispe_mlx_methods::method_round(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::round(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_round: " + std::string(e.what()));
    }
}

// mlx::clip - Limite les valeurs à un intervalle
// Signature: deflib mlx_clip(array min max)
// min et max peuvent être nil pour ne pas avoir de limite
Element* Lispe_mlx_methods::method_clip(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* min_elem = lisp->get_variable("min");
    Element* max_elem = lisp->get_variable("max");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        
        std::optional<mx::array> min_val = std::nullopt;
        std::optional<mx::array> max_val = std::nullopt;
        
        if (min_elem != null_)
            min_val = element_to_array(lisp, min_elem);
        if (max_elem != null_)
            max_val = element_to_array(lisp, max_elem);

        mx::array result = mx::clip(arr, min_val, max_val);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_clip: " + std::string(e.what()));
    }
}

// mlx::reciprocal - Inverse (1/x)
// Signature: deflib mlx_reciprocal(array)
Element* Lispe_mlx_methods::method_reciprocal(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::reciprocal(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_reciprocal: " + std::string(e.what()));
    }
}

// mlx::rsqrt - Inverse de la racine carrée (1/sqrt(x))
// Signature: deflib mlx_rsqrt(array)
Element* Lispe_mlx_methods::method_rsqrt(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::rsqrt(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_rsqrt: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 4: Trigonométrie inverse
// ============================================================================

// mlx::arcsin - Arc sinus
// Signature: deflib mlx_arcsin(array)
Element* Lispe_mlx_methods::method_arcsin(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::arcsin(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arcsin: " + std::string(e.what()));
    }
}

// mlx::arccos - Arc cosinus
// Signature: deflib mlx_arccos(array)
Element* Lispe_mlx_methods::method_arccos(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::arccos(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arccos: " + std::string(e.what()));
    }
}

// mlx::arctan - Arc tangente
// Signature: deflib mlx_arctan(array)
Element* Lispe_mlx_methods::method_arctan(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::arctan(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arctan: " + std::string(e.what()));
    }
}

// mlx::sinh - Sinus hyperbolique
// Signature: deflib mlx_sinh(array)
Element* Lispe_mlx_methods::method_sinh(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::sinh(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sinh: " + std::string(e.what()));
    }
}

// mlx::cosh - Cosinus hyperbolique
// Signature: deflib mlx_cosh(array)
Element* Lispe_mlx_methods::method_cosh(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::cosh(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cosh: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 5: Réductions
// ============================================================================

// mlx::prod - Produit des éléments
// Signature: deflib mlx_prod(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_prod(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::prod(arr, element_to_axes(axes_elem),
                    (keepdims_elem != null_) ? keepdims_elem->Boolean() : false) :
            mx::prod(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_prod: " + std::string(e.what()));
    }
}

// mlx::max - Maximum des éléments
// Signature: deflib mlx_max(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_max(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::max(arr, element_to_axes(axes_elem),
                   (keepdims_elem != null_) ? keepdims_elem->Boolean() : false) :
            mx::max(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_max: " + std::string(e.what()));
    }
}

// mlx::min - Minimum des éléments
// Signature: deflib mlx_min(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_min(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::min(arr, element_to_axes(axes_elem),
                   (keepdims_elem != null_) ? keepdims_elem->Boolean() : false) :
            mx::min(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_min: " + std::string(e.what()));
    }
}

// mlx::var - Variance des éléments
// Signature: deflib mlx_var(array (axes) (keepdims) (ddof))
Element* Lispe_mlx_methods::method_var(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");
    Element* ddof_elem = lisp->get_variable("ddof");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        bool keepdims = (keepdims_elem != null_) ? keepdims_elem->Boolean() : false;
        int ddof = (ddof_elem != null_) ? ddof_elem->asInteger() : 0;

        mx::array result = (axes_elem != null_) ?
            mx::var(arr, element_to_axes(axes_elem), keepdims, ddof) :
            mx::var(arr, keepdims, ddof);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_var: " + std::string(e.what()));
    }
}

// mlx::std - Écart-type des éléments
// Signature: deflib mlx_std(array (axes) (keepdims) (ddof))
Element* Lispe_mlx_methods::method_std(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");
    Element* ddof_elem = lisp->get_variable("ddof");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        bool keepdims = (keepdims_elem != null_) ? keepdims_elem->Boolean() : false;
        int ddof = (ddof_elem != null_) ? ddof_elem->asInteger() : 0;

        // std = sqrt(var)
        mx::array result = (axes_elem != null_) ?
            mx::sqrt(mx::var(arr, element_to_axes(axes_elem), keepdims, ddof)) :
            mx::sqrt(mx::var(arr, keepdims, ddof));

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_std: " + std::string(e.what()));
    }
}

// mlx::all - Teste si tous les éléments sont vrais
// Signature: deflib mlx_all(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_all(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::all(arr, element_to_axes(axes_elem),
                   (keepdims_elem != null_) ? keepdims_elem->Boolean() : false) :
            mx::all(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_all: " + std::string(e.what()));
    }
}

// mlx::any - Teste si au moins un élément est vrai
// Signature: deflib mlx_any(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_any(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::any(arr, element_to_axes(axes_elem),
                   (keepdims_elem != null_) ? keepdims_elem->Boolean() : false) :
            mx::any(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_any: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 6: Comparaisons
// ============================================================================

// mlx::equal - Comparaison d'égalité
// Signature: deflib mlx_equal(a b)
Element* Lispe_mlx_methods::method_equal(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::equal(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_equal: " + std::string(e.what()));
    }
}

// mlx::not_equal - Comparaison d'inégalité
// Signature: deflib mlx_not_equal(a b)
Element* Lispe_mlx_methods::method_not_equal(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::not_equal(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_not_equal: " + std::string(e.what()));
    }
}

// mlx::greater - Comparaison supérieur
// Signature: deflib mlx_greater(a b)
Element* Lispe_mlx_methods::method_greater(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::greater(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_greater: " + std::string(e.what()));
    }
}

// mlx::less - Comparaison inférieur
// Signature: deflib mlx_less(a b)
Element* Lispe_mlx_methods::method_less(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::less(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_less: " + std::string(e.what()));
    }
}

// mlx::greater_equal - Comparaison supérieur ou égal
// Signature: deflib mlx_greater_equal(a b)
Element* Lispe_mlx_methods::method_greater_equal(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::greater_equal(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_greater_equal: " + std::string(e.what()));
    }
}

// mlx::less_equal - Comparaison inférieur ou égal
// Signature: deflib mlx_less_equal(a b)
Element* Lispe_mlx_methods::method_less_equal(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::less_equal(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_less_equal: " + std::string(e.what()));
    }
}

// mlx::where - Sélection conditionnelle
// Signature: deflib mlx_where(condition x y)
Element* Lispe_mlx_methods::method_where(LispE* lisp) {
    Element* cond_elem = lisp->get_variable("condition");
    Element* x_elem = lisp->get_variable("x");
    Element* y_elem = lisp->get_variable("y");

    try {
        mx::array cond = element_to_array(lisp, cond_elem);
        mx::array x = element_to_array(lisp, x_elem);
        mx::array y = element_to_array(lisp, y_elem);
        mx::array result = mx::where(cond, x, y);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_where: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 7: Manipulation
// ============================================================================

// mlx::flatten - Aplatit un array en 1D
// Signature: deflib mlx_flatten(array (start_axis) (end_axis))
Element* Lispe_mlx_methods::method_flatten(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* start_elem = lisp->get_variable("start_axis");
    Element* end_elem = lisp->get_variable("end_axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int start_axis = (start_elem != null_) ? start_elem->asInteger() : 0;
        int end_axis = (end_elem != null_) ? end_elem->asInteger() : -1;

        mx::array result = mx::flatten(arr, start_axis, end_axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_flatten: " + std::string(e.what()));
    }
}

// mlx::squeeze - Supprime les dimensions de taille 1
// Signature: deflib mlx_squeeze(array (axes))
Element* Lispe_mlx_methods::method_squeeze(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array result = (axes_elem != null_) ?
            mx::squeeze(arr, element_to_axes(axes_elem)) :
            mx::squeeze(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_squeeze: " + std::string(e.what()));
    }
}

// mlx::expand_dims - Ajoute une dimension
// Signature: deflib mlx_expand_dims(array axis)
Element* Lispe_mlx_methods::method_expand_dims(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int axis = axis_elem->asInteger();

        mx::array result = mx::expand_dims(arr, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_expand_dims: " + std::string(e.what()));
    }
}

// mlx::stack - Empile des arrays le long d'un nouvel axe
// Signature: deflib mlx_stack(arrays (axis))
Element* Lispe_mlx_methods::method_stack(LispE* lisp) {
    Element* arrays_elem = lisp->get_variable("arrays");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        if (!arrays_elem->isList()) {
            throw new Error("Error in mlx_stack: arrays must be a list");
        }

        std::vector<mx::array> arrays;
        for (long i = 0; i < arrays_elem->size(); i++) {
            arrays.push_back(element_to_array(lisp, arrays_elem->index(i)));
        }

        int axis = (axis_elem != null_) ? axis_elem->asInteger() : 0;

        mx::array result = mx::stack(arrays, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_stack: " + std::string(e.what()));
    }
}

// mlx::tile - Répète un array selon les axes
// Signature: deflib mlx_tile(array reps)
Element* Lispe_mlx_methods::method_tile(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* reps_elem = lisp->get_variable("reps");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        std::vector<int> reps = element_to_axes(reps_elem);

        mx::array result = mx::tile(arr, reps);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tile: " + std::string(e.what()));
    }
}

// mlx::repeat - Répète les éléments d'un array
// Signature: deflib mlx_repeat(array repeats axis)
Element* Lispe_mlx_methods::method_repeat(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* repeats_elem = lisp->get_variable("repeats");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int repeats = repeats_elem->asInteger();
        int axis = axis_elem->asInteger();

        mx::array result = mx::repeat(arr, repeats, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_repeat: " + std::string(e.what()));
    }
}

// mlx::pad - Rembourrage d'un array
// Signature: deflib mlx_pad(array pad_width (value))
Element* Lispe_mlx_methods::method_pad(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* pad_width_elem = lisp->get_variable("pad_width");
    Element* value_elem = lisp->get_variable("value");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        mx::array pad_val = (value_elem != null_) ?
            mx::array(value_elem->asNumber()) :
            mx::array(0.0);

        // pad_width peut être:
        // - Un entier simple: padding uniforme sur tous les côtés
        // - Une paire (before, after): padding uniforme par axe
        // - Une liste de paires [[before, after], ...]: padding par axe
        if (pad_width_elem->isNumber()) {
            // Entier simple: padding uniforme
            int pad = pad_width_elem->asInteger();
            mx::array result = mx::pad(arr, pad, pad_val);
            return new MLXArray(std::move(result));
        } else if (pad_width_elem->isList()) {
            long sz = pad_width_elem->size();
            if (sz == 2 && !pad_width_elem->index(0)->isList()) {
                // Paire simple (before, after)
                std::pair<int, int> pad_pair(
                    pad_width_elem->index(0)->asInteger(),
                    pad_width_elem->index(1)->asInteger()
                );
                mx::array result = mx::pad(arr, pad_pair, pad_val);
                return new MLXArray(std::move(result));
            } else {
                // Liste de paires [[before, after], ...]
                std::vector<std::pair<int, int>> pad_width;
                for (long i = 0; i < sz; i++) {
                    Element* pair = pad_width_elem->index(i);
                    if (pair->isList() && pair->size() == 2) {
                        pad_width.push_back(std::make_pair(
                            pair->index(0)->asInteger(),
                            pair->index(1)->asInteger()
                        ));
                    } else {
                        // Si c'est un entier, utiliser (val, val)
                        int val = pair->asInteger();
                        pad_width.push_back(std::make_pair(val, val));
                    }
                }
                mx::array result = mx::pad(arr, pad_width, pad_val);
                return new MLXArray(std::move(result));
            }
        } else {
            throw new Error("Error in mlx_pad: pad_width must be an integer, a pair, or a list of pairs");
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_pad: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 8: Activation deep learning
// ============================================================================

// GELU - Gaussian Error Linear Unit
// GELU(x) = 0.5 * x * (1 + erf(x / sqrt(2)))
// Signature: deflib mlx_gelu(array)
Element* Lispe_mlx_methods::method_gelu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        // GELU(x) = 0.5 * x * (1 + erf(x / sqrt(2)))
        const float sqrt2_inv = 0.7071067811865476f;  // 1/sqrt(2)
        mx::array result = mx::multiply(
            mx::multiply(mx::array(0.5f), arr),
            mx::add(mx::array(1.0f), mx::erf(mx::multiply(arr, mx::array(sqrt2_inv))))
        );
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_gelu: " + std::string(e.what()));
    }
}

// GELU with PyTorch tanh approximation (used by Gemma 3)
// gelu(x) = 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
// Signature: deflib mlx_gelu_tanh(array)
Element* Lispe_mlx_methods::method_gelu_tanh(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array x = element_to_array(lisp, array_elem);
        const float sqrt_2_over_pi = 0.7978845608028654f;
        const float coeff = 0.044715f;
        auto x_cubed = mx::multiply(mx::multiply(x, x), x);
        auto inner = mx::multiply(
            mx::array(sqrt_2_over_pi, x.dtype()),
            mx::add(x, mx::multiply(mx::array(coeff, x.dtype()), x_cubed))
        );
        mx::array result = mx::multiply(
            mx::multiply(mx::array(0.5f, x.dtype()), x),
            mx::add(mx::array(1.0f, x.dtype()), mx::tanh(inner))
        );
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_gelu_tanh: " + std::string(e.what()));
    }
}

// mlx::silu - Sigmoid Linear Unit (Swish)
// Signature: deflib mlx_silu(array)
Element* Lispe_mlx_methods::method_silu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        // SiLU = x * sigmoid(x)
        mx::array result = mx::multiply(arr, mx::sigmoid(arr));
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_silu: " + std::string(e.what()));
    }
}

// Leaky ReLU - Leaky Rectified Linear Unit
// leaky_relu(x) = x if x > 0, else negative_slope * x
// Signature: deflib mlx_leaky_relu(array (negative_slope))
Element* Lispe_mlx_methods::method_leaky_relu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* slope_elem = lisp->get_variable("negative_slope");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        float negative_slope = (slope_elem != null_) ? slope_elem->asFloat() : 0.01f;
        // leaky_relu(x) = where(x > 0, x, negative_slope * x)
        mx::array result = mx::where(
            mx::greater(arr, mx::array(0.0f)),
            arr,
            mx::multiply(mx::array(negative_slope), arr)
        );
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_leaky_relu: " + std::string(e.what()));
    }
}

// ELU - Exponential Linear Unit
// elu(x) = x if x > 0, else alpha * (exp(x) - 1)
// Signature: deflib mlx_elu(array (alpha))
Element* Lispe_mlx_methods::method_elu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* alpha_elem = lisp->get_variable("alpha");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        float alpha = (alpha_elem != null_) ? alpha_elem->asFloat() : 1.0f;
        // elu(x) = where(x > 0, x, alpha * (exp(x) - 1))
        mx::array result = mx::where(
            mx::greater(arr, mx::array(0.0f)),
            arr,
            mx::multiply(mx::array(alpha), mx::subtract(mx::exp(arr), mx::array(1.0f)))
        );
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_elu: " + std::string(e.what()));
    }
}

// mlx::selu - Scaled Exponential Linear Unit
// Signature: deflib mlx_selu(array)
Element* Lispe_mlx_methods::method_selu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        // SELU constants
        const float alpha = 1.6732632423543772f;
        const float scale = 1.0507009873554805f;
        // SELU = scale * (max(0, x) + min(0, alpha * (exp(x) - 1)))
        mx::array result = mx::multiply(
            mx::array(scale),
            mx::add(
                mx::maximum(arr, mx::array(0.0f)),
                mx::minimum(
                    mx::array(0.0f),
                    mx::multiply(mx::array(alpha), mx::subtract(mx::exp(arr), mx::array(1.0f)))
                )
            )
        );
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_selu: " + std::string(e.what()));
    }
}

// Log Softmax
// log_softmax(x) = x - logsumexp(x, axis, keepdims=true)
// Signature: deflib mlx_log_softmax(array (axis))
Element* Lispe_mlx_methods::method_log_softmax(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int axis = (axis_elem != null_) ? axis_elem->asInteger() : -1;
        // log_softmax(x) = x - logsumexp(x, axis, keepdims=true)
        mx::array result = mx::subtract(arr, mx::logsumexp(arr, axis, true));
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_log_softmax: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 9: Algèbre linéaire
// ============================================================================

// linalg::norm - Norme vectorielle ou matricielle
// Signature: deflib mlx_norm(array (ord) (axes) (keepdims))
Element* Lispe_mlx_methods::method_norm(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* ord_elem = lisp->get_variable("ord");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        bool keepdims = (keepdims_elem != null_) ? keepdims_elem->Boolean() : false;

        // Helper pour extraire les axes
        auto get_axes = [&]() -> std::vector<int> {
            std::vector<int> axes;
            if (axes_elem->isList()) {
                for (long i = 0; i < axes_elem->size(); i++) {
                    axes.push_back(axes_elem->index(i)->asInteger());
                }
            } else {
                axes.push_back(axes_elem->asInteger());
            }
            return axes;
        };

        mx::array result = (ord_elem != null_ && axes_elem != null_) ?
            mx::linalg::norm(arr, ord_elem->asNumber(), get_axes(), keepdims) :
            (ord_elem != null_) ?
                mx::linalg::norm(arr, ord_elem->asNumber(), std::nullopt, keepdims) :
                (axes_elem != null_) ?
                    mx::linalg::norm(arr, get_axes(), keepdims) :
                    mx::linalg::norm(arr);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_norm: " + std::string(e.what()));
    }
}

// linalg::inv - Inverse de matrice
// Signature: deflib mlx_inv(array)
Element* Lispe_mlx_methods::method_inv(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::linalg::inv(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_inv: " + std::string(e.what()));
    }
}

// linalg::svd - Décomposition en valeurs singulières
// Signature: deflib mlx_svd(array)
// Retourne une liste (U, S, Vt)
Element* Lispe_mlx_methods::method_svd(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        std::vector<mx::array> usv = mx::linalg::svd(arr);

        // Évaluer tous les résultats
        for (auto& a : usv) {
            mx::eval(a);
        }

        // Retourner une liste LispE [U, S, Vt]
        List* result = lisp->provideList();
        for (auto& a : usv) {
            result->append(new MLXArray(std::move(a)));
        }
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_svd: " + std::string(e.what()));
    }
}

// linalg::qr - Décomposition QR
// Signature: deflib mlx_qr(array)
// Retourne une liste (Q, R)
Element* Lispe_mlx_methods::method_qr(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        std::pair<mx::array, mx::array> qr = mx::linalg::qr(arr);

        mx::eval(qr.first);
        mx::eval(qr.second);

        // Retourner une liste LispE [Q, R]
        List* result = lisp->provideList();
        result->append(new MLXArray(std::move(qr.first)));
        result->append(new MLXArray(std::move(qr.second)));
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_qr: " + std::string(e.what()));
    }
}

// linalg::cholesky - Décomposition de Cholesky
// Signature: deflib mlx_cholesky(array (upper))
Element* Lispe_mlx_methods::method_cholesky(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* upper_elem = lisp->get_variable("upper");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        bool upper = (upper_elem != null_) ? upper_elem->Boolean() : false;
        mx::array result = mx::linalg::cholesky(arr, upper);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cholesky: " + std::string(e.what()));
    }
}

// linalg::solve - Résolution de système linéaire Ax = b
// Signature: deflib mlx_solve(a b)
Element* Lispe_mlx_methods::method_solve(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::linalg::solve(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_solve: " + std::string(e.what()));
    }
}

// ============================================================================
// Helper pour le mode-n unfolding (matricization)
// ============================================================================

// Unfold un tenseur le long du mode n
// Transforme un tenseur de shape [I0, I1, ..., IN-1] en matrice [In, I0*...*In-1*In+1*...*IN-1]
static mx::array mode_n_unfold(const mx::array& tensor, int mode) {
    auto shape = tensor.shape();
    int ndim = shape.size();
    
    // Créer les axes pour la transposition: mode en premier, puis les autres
    std::vector<int> axes;
    axes.push_back(mode);
    for (int i = 0; i < ndim; i++) {
        if (i != mode) axes.push_back(i);
    }
    
    // Transposer
    mx::array transposed = mx::transpose(tensor, axes);
    
    // Calculer la nouvelle shape [In, prod(autres)]
    int mode_size = shape[mode];
    int other_size = 1;
    for (int i = 0; i < ndim; i++) {
        if (i != mode) other_size *= shape[i];
    }
    
    // Reshape en matrice
    return mx::reshape(transposed, {mode_size, other_size});
}

// Fold (inverse de unfold) - reconstruit le tenseur depuis une matrice
static mx::array mode_n_fold(const mx::array& matrix, int mode, const std::vector<int>& original_shape) {
    int ndim = original_shape.size();
    
    // Shape après transposition (mode en premier)
    mx::Shape transposed_shape;
    transposed_shape.push_back(original_shape[mode]);
    for (int i = 0; i < ndim; i++) {
        if (i != mode) transposed_shape.push_back(original_shape[i]);
    }
    
    // Reshape depuis matrice vers tenseur transposé
    mx::array reshaped = mx::reshape(matrix, transposed_shape);
    
    // Inverse de la transposition
    std::vector<int> inverse_axes(ndim);
    inverse_axes[mode] = 0;
    int j = 1;
    for (int i = 0; i < ndim; i++) {
        if (i != mode) inverse_axes[i] = j++;
    }
    
    return mx::transpose(reshaped, inverse_axes);
}

// n-mode product: tenseur × matrice le long du mode n
static mx::array n_mode_product(const mx::array& tensor, const mx::array& matrix, int mode) {
    // Unfold le tenseur
    mx::array unfolded = mode_n_unfold(tensor, mode);
    
    // Multiplier: matrix @ unfolded
    mx::array product = mx::matmul(matrix, unfolded);
    
    // Reconstruire avec la nouvelle taille pour le mode
    auto original_shape = tensor.shape();
    std::vector<int> new_shape(original_shape.begin(), original_shape.end());
    new_shape[mode] = matrix.shape()[0];
    
    return mode_n_fold(product, mode, new_shape);
}

// ============================================================================
// Tucker decomposition (HOSVD - Higher-Order SVD)
// ============================================================================

// mlx_hosvd - Higher-Order SVD (Tucker decomposition non-itérative)
// Signature: deflib mlx_hosvd(tensor ranks)
// ranks: liste des rangs pour chaque mode (ou nil pour garder la dimension complète)
// Retourne: (core, factors) où factors est une liste de matrices
Element* Lispe_mlx_methods::method_hosvd(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable("tensor");
    Element* ranks_elem = lisp->get_variable("ranks");

    try {
        mx::array tensor = element_to_array(lisp, tensor_elem);
        auto shape = tensor.shape();
        int ndim = shape.size();
        
        // Parser les rangs
        std::vector<int> ranks(ndim);
        if (ranks_elem == null_ || !ranks_elem->isList()) {
            // Rangs par défaut = dimensions originales
            for (int i = 0; i < ndim; i++) {
                ranks[i] = shape[i];
            }
        } else {
            if (ranks_elem->size() != ndim) {
                throw new Error("Error in mlx_hosvd: ranks list must have same length as tensor dimensions");
            }
            for (int i = 0; i < ndim; i++) {
                Element* r = ranks_elem->index(i);
                if (r == null_ || r->isNULL()) {
                    ranks[i] = shape[i];  // Garder dimension complète
                } else {
                    ranks[i] = std::min((int)r->asInteger(), (int)shape[i]);
                }
            }
        }
        
        // Calculer les facteurs par SVD de chaque mode-n unfolding
        std::vector<mx::array> factors;
        mx::array core = tensor;
        
        for (int mode = 0; mode < ndim; mode++) {
            // Unfold le tenseur courant
            mx::array unfolded = mode_n_unfold(core, mode);
            
            // SVD
            auto usv = mx::linalg::svd(unfolded);
            mx::array U = usv[0];
            
            // Tronquer U aux rangs demandés
            int rank = ranks[mode];
            if (rank < U.shape()[1]) {
                U = mx::slice(U, {0, 0}, {(int)U.shape()[0], rank});
            }
            
            factors.push_back(U);
            
            // Projeter le core: core = U^T × core (mode-n product avec U^T)
            core = n_mode_product(core, mx::transpose(U), mode);
        }
        
        // Évaluer
        mx::eval(core);
        for (auto& f : factors) {
            mx::eval(f);
        }
        
        // Retourner (core, factors)
        List* factors_list = lisp->provideList();
        for (auto& f : factors) {
            factors_list->append(new MLXArray(std::move(f)));
        }
        
        List* result = lisp->provideList();
        result->append(new MLXArray(std::move(core)));
        result->append(factors_list);
        
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_hosvd: " + std::string(e.what()));
    }
}

// mlx_tucker - Tucker decomposition (HOOI - Higher-Order Orthogonal Iteration)
// Signature: deflib mlx_tucker(tensor ranks (max_iter) (tol))
// ranks: liste des rangs pour chaque mode
// max_iter: nombre maximum d'itérations (défaut: 50)
// tol: tolérance pour la convergence (défaut: 1e-5)
// Retourne: (core, factors) où factors est une liste de matrices
Element* Lispe_mlx_methods::method_tucker(LispE* lisp) {
    Element* tensor_elem = lisp->get_variable("tensor");
    Element* ranks_elem = lisp->get_variable("ranks");
    Element* max_iter_elem = lisp->get_variable("max_iter");
    Element* tol_elem = lisp->get_variable("tol");

    try {
        mx::array tensor = element_to_array(lisp, tensor_elem);
        auto shape = tensor.shape();
        int ndim = shape.size();
        
        // Parser les rangs
        std::vector<int> ranks(ndim);
        if (ranks_elem == null_ || !ranks_elem->isList()) {
            throw new Error("Error in mlx_tucker: ranks must be a list of integers");
        }
        if (ranks_elem->size() != ndim) {
            throw new Error("Error in mlx_tucker: ranks list must have same length as tensor dimensions");
        }
        for (int i = 0; i < ndim; i++) {
            Element* r = ranks_elem->index(i);
            if (r == null_ || r->isNULL()) {
                ranks[i] = shape[i];
            } else {
                ranks[i] = std::min((int)r->asInteger(), (int)shape[i]);
            }
        }
        
        int max_iter = (max_iter_elem != null_) ? max_iter_elem->asInteger() : 50;
        double tol = (tol_elem != null_) ? tol_elem->asFloat() : 1e-5;
        
        // Initialisation avec HOSVD
        std::vector<mx::array> factors;
        factors.reserve(ndim);
        for (int mode = 0; mode < ndim; mode++) {
            mx::array unfolded = mode_n_unfold(tensor, mode);
            auto usv = mx::linalg::svd(unfolded);
            mx::array U = usv[0];
            int rank = ranks[mode];
            if (rank < U.shape()[1]) {
                U = mx::slice(U, {0, 0}, {(int)U.shape()[0], rank});
            }
            factors.push_back(std::move(U));
        }
        
        // HOOI: itération jusqu'à convergence
        double prev_fit = 0.0;
        for (int iter = 0; iter < max_iter; iter++) {
            for (int mode = 0; mode < ndim; mode++) {
                // Calculer Y = tensor ×_1 U1^T ×_2 U2^T ... (sauf mode n)
                mx::array Y = tensor;
                for (int m = 0; m < ndim; m++) {
                    if (m != mode) {
                        Y = n_mode_product(Y, mx::transpose(factors[m]), m);
                    }
                }
                
                // Unfold Y selon le mode courant
                mx::array unfolded = mode_n_unfold(Y, mode);
                
                // SVD et mise à jour du facteur
                auto usv = mx::linalg::svd(unfolded);
                mx::array U = usv[0];
                int rank = ranks[mode];
                if (rank < U.shape()[1]) {
                    U = mx::slice(U, {0, 0}, {(int)U.shape()[0], rank});
                }
                factors[mode] = U;
            }
            
            // Calculer le core
            mx::array core = tensor;
            for (int m = 0; m < ndim; m++) {
                core = n_mode_product(core, mx::transpose(factors[m]), m);
            }
            
            // Vérifier la convergence (norme du core)
            mx::array norm_sq = mx::sum(mx::square(core));
            mx::eval(norm_sq);
            double fit = std::sqrt(norm_sq.item<float>());
            
            if (iter > 0 && std::abs(fit - prev_fit) < tol) {
                break;
            }
            prev_fit = fit;
        }
        
        // Calculer le core final
        mx::array core = tensor;
        for (int m = 0; m < ndim; m++) {
            core = n_mode_product(core, mx::transpose(factors[m]), m);
        }
        
        // Évaluer
        mx::eval(core);
        for (auto& f : factors) {
            mx::eval(f);
        }
        
        // Retourner (core, factors)
        List* factors_list = lisp->provideList();
        for (auto& f : factors) {
            factors_list->append(new MLXArray(std::move(f)));
        }
        
        List* result = lisp->provideList();
        result->append(new MLXArray(std::move(core)));
        result->append(factors_list);
        
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tucker: " + std::string(e.what()));
    }
}

// mlx_tucker_reconstruct - Reconstruit un tenseur à partir de sa décomposition Tucker
// Signature: deflib mlx_tucker_reconstruct(core factors)
// core: tenseur noyau
// factors: liste des matrices de facteurs
// Retourne: tenseur reconstruit
Element* Lispe_mlx_methods::method_tucker_reconstruct(LispE* lisp) {
    Element* core_elem = lisp->get_variable("core");
    Element* factors_elem = lisp->get_variable("factors");

    try {
        mx::array core = element_to_array(lisp, core_elem);
        
        if (!factors_elem->isList()) {
            throw new Error("Error in mlx_tucker_reconstruct: factors must be a list");
        }
        
        int ndim = factors_elem->size();
        if (ndim != core.ndim()) {
            throw new Error("Error in mlx_tucker_reconstruct: number of factors must match core dimensions");
        }
        
        // Reconstruire: tensor = core ×_1 U1 ×_2 U2 ... ×_n Un
        mx::array result = core;
        for (int mode = 0; mode < ndim; mode++) {
            Element* factor_elem = factors_elem->index(mode);
            mx::array factor = element_to_array(lisp, factor_elem);
            result = n_mode_product(result, factor, mode);
        }
        
        mx::eval(result);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tucker_reconstruct: " + std::string(e.what()));
    }
}

// mlx_tucker_compression_ratio - Calcule le ratio de compression d'une décomposition Tucker
// Signature: deflib mlx_tucker_compression_ratio(original_shape core factors)
// original_shape: forme du tenseur original (liste d'entiers)
// core: tenseur noyau
// factors: liste des matrices de facteurs
// Retourne: dictionnaire avec ratio, original_size, compressed_size
Element* Lispe_mlx_methods::method_tucker_compression_ratio(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("original_shape");
    Element* core_elem = lisp->get_variable("core");
    Element* factors_elem = lisp->get_variable("factors");

    try {
        // Calculer la taille originale
        long original_size = 1;
        if (!shape_elem->isList()) {
            throw new Error("Error in mlx_tucker_compression_ratio: original_shape must be a list");
        }
        for (long i = 0; i < shape_elem->size(); i++) {
            original_size *= shape_elem->index(i)->asInteger();
        }
        
        // Calculer la taille du core
        mx::array core = element_to_array(lisp, core_elem);
        long core_size = core.size();
        
        // Calculer la taille des facteurs
        if (!factors_elem->isList()) {
            throw new Error("Error in mlx_tucker_compression_ratio: factors must be a list");
        }
        
        long factors_size = 0;
        for (long i = 0; i < factors_elem->size(); i++) {
            Element* factor_elem = factors_elem->index(i);
            mx::array factor = element_to_array(lisp, factor_elem);
            factors_size += factor.size();
        }
        
        long compressed_size = core_size + factors_size;
        double ratio = (double)original_size / (double)compressed_size;
        
        // Retourner un dictionnaire
        Dictionary* dict = lisp->provideDictionary();
        u_ustring key_ratio = U"ratio";
        u_ustring key_original = U"original_size";
        u_ustring key_compressed = U"compressed_size";
        u_ustring key_core = U"core_size";
        u_ustring key_factors = U"factors_size";
        dict->recording(key_ratio, lisp->provideNumber(ratio));
        dict->recording(key_original, lisp->provideInteger(original_size));
        dict->recording(key_compressed, lisp->provideInteger(compressed_size));
        dict->recording(key_core, lisp->provideInteger(core_size));
        dict->recording(key_factors, lisp->provideInteger(factors_size));
        
        return dict;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tucker_compression_ratio: " + std::string(e.what()));
    }
}

// mlx_khatri_rao_product - Produit de Khatri-Rao (produit colonne par colonne de Kronecker)
// Signature: deflib mlx_khatri_rao_product(a b)
// a: matrice de taille (I, K)
// b: matrice de taille (J, K)
// Retourne: matrice de taille (I*J, K)
Element* Lispe_mlx_methods::method_khatri_rao_product(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        
        if (a.ndim() != 2 || b.ndim() != 2) {
            throw new Error("Error in mlx_khatri_rao_product: both inputs must be 2D matrices");
        }
        
        int I = a.shape()[0];
        int J = b.shape()[0];
        int K_a = a.shape()[1];
        int K_b = b.shape()[1];
        
        if (K_a != K_b) {
            throw new Error("Error in mlx_khatri_rao_product: matrices must have same number of columns");
        }
        int K = K_a;
        
        // Khatri-Rao: pour chaque colonne k, faire le produit de Kronecker des colonnes
        // result[:, k] = kron(a[:, k], b[:, k])
        // Résultat: matrice de taille (I*J, K)
        
        // Méthode efficace: utiliser le broadcasting
        // a[:, None, :] a forme (I, 1, K)
        // b[None, :, :] a forme (1, J, K)
        // Le produit a forme (I, J, K)
        // Puis reshape en (I*J, K)
        
        mx::array a_expanded = mx::reshape(a, {I, 1, K});
        mx::array b_expanded = mx::reshape(b, {1, J, K});
        mx::array product = mx::multiply(a_expanded, b_expanded);
        mx::array result = mx::reshape(product, {I * J, K});
        
        mx::eval(result);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_khatri_rao_product: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 10: Opérations logiques et tests
// ============================================================================

// logical_and - ET logique élément par élément
// Signature: deflib mlx_logical_and(a b)
Element* Lispe_mlx_methods::method_logical_and(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::logical_and(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_logical_and: " + std::string(e.what()));
    }
}

// logical_or - OU logique élément par élément
// Signature: deflib mlx_logical_or(a b)
Element* Lispe_mlx_methods::method_logical_or(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::logical_or(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_logical_or: " + std::string(e.what()));
    }
}

// logical_not - NON logique élément par élément
// Signature: deflib mlx_logical_not(array)
Element* Lispe_mlx_methods::method_logical_not(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::logical_not(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_logical_not: " + std::string(e.what()));
    }
}

// isnan - Test si les valeurs sont NaN
// Signature: deflib mlx_isnan(array)
Element* Lispe_mlx_methods::method_isnan(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::isnan(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_isnan: " + std::string(e.what()));
    }
}

// isinf - Test si les valeurs sont infinies
// Signature: deflib mlx_isinf(array)
Element* Lispe_mlx_methods::method_isinf(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::isinf(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_isinf: " + std::string(e.what()));
    }
}

// isfinite - Test si les valeurs sont finies (ni NaN ni inf)
// Signature: deflib mlx_isfinite(array)
Element* Lispe_mlx_methods::method_isfinite(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        // isfinite = not(isnan or isinf)
        mx::array result = mx::logical_not(mx::logical_or(mx::isnan(arr), mx::isinf(arr)));
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_isfinite: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 11: Manipulation avancée
// ============================================================================

// flip - Inverser l'array selon les axes spécifiés
// Implémentation manuelle car mx::flip n'existe pas dans MLX 0.29.3
// Signature: deflib mlx_flip(array (axes))
Element* Lispe_mlx_methods::method_flip(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        auto shape = arr.shape();
        int ndim = shape.size();

        // Déterminer les axes à inverser
        std::vector<int> axes;
        if (axes_elem != null_) {
            if (axes_elem->isList()) {
                for (long i = 0; i < axes_elem->size(); i++) {
                    int ax = axes_elem->index(i)->asInteger();
                    if (ax < 0) ax += ndim;
                    axes.push_back(ax);
                }
            } else {
                int ax = axes_elem->asInteger();
                if (ax < 0) ax += ndim;
                axes.push_back(ax);
            }
        } else {
            // Par défaut, inverser tous les axes
            for (int i = 0; i < ndim; i++) {
                axes.push_back(i);
            }
        }

        // Inverser chaque axe en utilisant take avec indices inversés
        mx::array result = arr;
        for (int axis : axes) {
            int dim_size = shape[axis];
            // Créer indices inversés: [dim_size-1, dim_size-2, ..., 1, 0]
            std::vector<int32_t> rev_indices(dim_size);
            for (int i = 0; i < dim_size; i++) {
                rev_indices[i] = dim_size - 1 - i;
            }
            mx::array indices(rev_indices.data(), {dim_size}, mx::int32);
            result = mx::take(result, indices, axis);
        }

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_flip: " + std::string(e.what()));
    }
}

// roll - Décaler circulairement les éléments
// Signature: deflib mlx_roll(array shift (axis))
Element* Lispe_mlx_methods::method_roll(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* shift_elem = lisp->get_variable("shift");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int shift = shift_elem->asInteger();

        mx::array result = (axis_elem != null_) ?
            mx::roll(arr, shift, axis_elem->asInteger()) :
            mx::roll(arr, shift);

        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_roll: " + std::string(e.what()));
    }
}

// sort - Trier l'array selon un axe
// Signature: deflib mlx_sort(array (axis))
Element* Lispe_mlx_methods::method_sort(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int axis = (axis_elem != null_) ? axis_elem->asInteger() : -1;
        mx::array result = mx::sort(arr, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sort: " + std::string(e.what()));
    }
}

// argsort - Indices qui trieraient l'array
// Signature: deflib mlx_argsort(array (axis))
Element* Lispe_mlx_methods::method_argsort(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int axis = (axis_elem != null_) ? axis_elem->asInteger() : -1;
        mx::array result = mx::argsort(arr, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_argsort: " + std::string(e.what()));
    }
}

// argwhere - Indices où la condition est vraie
// Signature: deflib mlx_argwhere(array)
// Retourne un array [N, 1] pour 1D où N est le nombre d'éléments non-nuls
// Implémentation manuelle car mx::argwhere n'existe pas dans MLX C++
Element* Lispe_mlx_methods::method_argwhere(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        
        // Aplatir l'array si nécessaire
        mx::array flat = mx::flatten(arr);
        int size = flat.shape()[0];
        
        // Créer un array d'indices [0, 1, 2, ..., size-1]
        mx::array indices = mx::arange(0, size, 1, mx::int32);
        
        // Convertir en booléen si pas déjà
        mx::array mask = mx::astype(flat, mx::bool_);
        
        // Utiliser where pour sélectionner les indices où mask est vrai
        // where(cond, x, y) - on veut les indices, donc on utilise une astuce:
        // On compte d'abord combien d'éléments sont vrais
        mx::array sum = mx::sum(mask, false);
        mx::eval(sum);  // Force l'évaluation pour obtenir la valeur
        
        // Récupérer le nombre d'éléments vrais
        int32_t* sum_ptr = sum.data<int32_t>();
        int num_true = (sum_ptr != nullptr) ? *sum_ptr : 0;
        
        if (num_true == 0) {
            // Retourner un array vide [0, 1]
            return new MLXArray(mx::zeros({0, 1}, mx::int32));
        }
        
        // Créer le résultat en utilisant une boucle sur les données
        // D'abord évaluer le masque
        mx::eval(mask);
        mx::eval(indices);
        
        bool* mask_data = mask.data<bool>();
        int32_t* indices_data = indices.data<int32_t>();
        
        // Collecter les indices où mask est vrai
        std::vector<int32_t> result_indices;
        result_indices.reserve(num_true);
        
        for (int i = 0; i < size; i++) {
            if (mask_data[i]) {
                result_indices.push_back(indices_data[i]);
            }
        }
        
        // Créer l'array de résultat [N, 1]
        mx::array result = mx::array(result_indices.data(), {(int)result_indices.size(), 1}, mx::int32);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_argwhere: " + std::string(e.what()));
    }
}

// moveaxis - Déplacer des axes vers de nouvelles positions
// Signature: deflib mlx_moveaxis(array source destination)
Element* Lispe_mlx_methods::method_moveaxis(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* source_elem = lisp->get_variable("source");
    Element* dest_elem = lisp->get_variable("destination");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int source = source_elem->asInteger();
        int destination = dest_elem->asInteger();
        mx::array result = mx::moveaxis(arr, source, destination);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_moveaxis: " + std::string(e.what()));
    }
}

// swapaxes - Échanger deux axes
// Signature: deflib mlx_swapaxes(array axis1 axis2)
Element* Lispe_mlx_methods::method_swapaxes(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* axis1_elem = lisp->get_variable("axis1");
    Element* axis2_elem = lisp->get_variable("axis2");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int axis1 = axis1_elem->asInteger();
        int axis2 = axis2_elem->asInteger();
        mx::array result = mx::swapaxes(arr, axis1, axis2);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_swapaxes: " + std::string(e.what()));
    }
}

// broadcast_to - Diffuser l'array vers une nouvelle forme
// Signature: deflib mlx_broadcast_to(array shape)
Element* Lispe_mlx_methods::method_broadcast_to(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* shape_elem = lisp->get_variable("shape");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::Shape new_shape;
        element_to_shape(shape_elem, new_shape);

        mx::array result = mx::broadcast_to(arr, new_shape);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_broadcast_to: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 12: Création avancée
// ============================================================================

// identity - Matrice identité (alias de eye)
// Signature: deflib mlx_identity(n (dtype))
Element* Lispe_mlx_methods::method_identity(LispE* lisp) {
    Element* n_elem = lisp->get_variable("n");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        int n = n_elem->asInteger();
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        mx::array result = mx::identity(n, dtype);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_identity: " + std::string(e.what()));
    }
}

// tri - Matrice triangulaire inférieure remplie de 1
// Signature: deflib mlx_tri(n (m) (k) (dtype))
Element* Lispe_mlx_methods::method_tri(LispE* lisp) {
    Element* n_elem = lisp->get_variable("n");
    Element* m_elem = lisp->get_variable("m");
    Element* k_elem = lisp->get_variable("k");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        int n = n_elem->asInteger();
        int m = (m_elem != null_) ? m_elem->asInteger() : n;
        int k = (k_elem != null_) ? k_elem->asInteger() : 0;
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        mx::array result = mx::tri(n, m, k, dtype);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tri: " + std::string(e.what()));
    }
}

// tril - Extraire la partie triangulaire inférieure
// Signature: deflib mlx_tril(array (k))
Element* Lispe_mlx_methods::method_tril(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* k_elem = lisp->get_variable("k");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int k = (k_elem != null_) ? k_elem->asInteger() : 0;
        mx::array result = mx::tril(arr, k);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tril: " + std::string(e.what()));
    }
}

// triu - Extraire la partie triangulaire supérieure
// Signature: deflib mlx_triu(array (k))
Element* Lispe_mlx_methods::method_triu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* k_elem = lisp->get_variable("k");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        int k = (k_elem != null_) ? k_elem->asInteger() : 0;
        mx::array result = mx::triu(arr, k);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_triu: " + std::string(e.what()));
    }
}

// meshgrid - Créer des matrices de coordonnées
// Signature: deflib mlx_meshgrid(arrays (indexing))
// indexing: "xy" (default) ou "ij"
Element* Lispe_mlx_methods::method_meshgrid(LispE* lisp) {
    Element* arrays_elem = lisp->get_variable("arrays");
    Element* indexing_elem = lisp->get_variable("indexing");

    try {
        if (!arrays_elem->isList()) {
            throw new Error("Error in mlx_meshgrid: arrays must be a list");
        }

        std::vector<mx::array> input_arrays;
        for (long i = 0; i < arrays_elem->size(); i++) {
            input_arrays.push_back(element_to_array(lisp, arrays_elem->index(i)));
        }

        bool sparse = false;
        std::string indexing = "xy";
        if (indexing_elem != null_) {
            indexing = indexing_elem->toString(lisp);
        }

        std::vector<mx::array> result_arrays = mx::meshgrid(input_arrays, sparse, indexing);

        // Évaluer et retourner comme liste LispE
        List* result = lisp->provideList();
        for (auto& arr : result_arrays) {
            mx::eval(arr);
            result->append(new MLXArray(std::move(arr)));
        }
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_meshgrid: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 13: Algèbre linéaire avancée
// ============================================================================

// mlx::linalg::pinv - Calcule la pseudo-inverse de Moore-Penrose
// Signature: deflib mlx_pinv(array)
Element* Lispe_mlx_methods::method_pinv(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mlx::core::linalg::pinv(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_pinv: " + std::string(e.what()));
    }
}

// mlx::linalg::eig - Calcule les valeurs propres et vecteurs propres
// Signature: deflib mlx_eig(array)
// Retourne une liste (eigenvalues eigenvectors)
Element* Lispe_mlx_methods::method_eig(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        auto [eigenvalues, eigenvectors] = mlx::core::linalg::eig(arr);
        mx::eval(eigenvalues);
        mx::eval(eigenvectors);

        List* result = lisp->provideList();
        result->append(new MLXArray(std::move(eigenvalues)));
        result->append(new MLXArray(std::move(eigenvectors)));
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_eig: " + std::string(e.what()));
    }
}

// mlx::linalg::eigvals - Calcule uniquement les valeurs propres
// Signature: deflib mlx_eigvals(array)
Element* Lispe_mlx_methods::method_eigvals(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mlx::core::linalg::eigvals(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_eigvals: " + std::string(e.what()));
    }
}

// mlx::linalg::lu - Décomposition LU
// Signature: deflib mlx_lu(array)
// Retourne une liste (P L U)
Element* Lispe_mlx_methods::method_lu(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        std::vector<mx::array> lu_result = mlx::core::linalg::lu(arr);

        List* result = lisp->provideList();
        for (auto& arr : lu_result) {
            mx::eval(arr);
            result->append(new MLXArray(std::move(arr)));
        }
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_lu: " + std::string(e.what()));
    }
}

// mlx::linalg::cross - Produit vectoriel de deux vecteurs
// Signature: deflib mlx_cross(a b (axis))
Element* Lispe_mlx_methods::method_cross(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        int axis = -1;
        if (axis_elem != null_) {
            axis = axis_elem->asInteger();
        }

        mx::array result = mlx::core::linalg::cross(a, b, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cross: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 14: I/O de modèles
// ============================================================================

// mlx::save - Sauvegarde un array en format NPY
// Signature: deflib mlx_save(file array)
Element* Lispe_mlx_methods::method_save(LispE* lisp) {
    Element* file_elem = lisp->get_variable("file");
    Element* array_elem = lisp->get_variable("array");

    try {
        std::string filename = file_elem->toString(lisp);
        mx::array arr = element_to_array(lisp, array_elem);
        mx::eval(arr);
        mx::save(filename, arr);
        return true_;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_save: " + std::string(e.what()));
    }
}

// mlx::load - Charge un array depuis un fichier NPY
// Signature: deflib mlx_load(file)
Element* Lispe_mlx_methods::method_load(LispE* lisp) {
    Element* file_elem = lisp->get_variable("file");

    try {
        std::string filename = file_elem->toString(lisp);
        mx::array result = mx::load(filename);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_load: " + std::string(e.what()));
    }
}

// mlx::load_safetensors - Charge un fichier safetensors
// Signature: deflib mlx_load_safetensors(file)
// Retourne une liste (tensors_dict metadata_dict)
Element* Lispe_mlx_methods::method_load_safetensors(LispE* lisp) {
    Element* file_elem = lisp->get_variable("file");

    try {
        std::string filename = file_elem->toString(lisp);
        auto [tensors, metadata] = mx::load_safetensors(filename);

        // Créer le dictionnaire des tenseurs
        Dictionary* tensors_dict = lisp->provideDictionary();
        for (auto& [name, arr] : tensors) {
            mx::eval(arr);
            MLXArray* mlx_arr = new MLXArray(std::move(arr));
            std::string key_copy = name;  // Copie non-const
            tensors_dict->recording(key_copy, mlx_arr);
        }

        // Créer le dictionnaire des métadonnées
        Dictionary* metadata_dict = lisp->provideDictionary();
        for (auto& [key, value] : metadata) {
            std::string key_copy = key;  // Copie non-const
            std::string value_copy = value;  // Copie non-const
            Element* str_val = lisp->provideString(value_copy);
            metadata_dict->recording(key_copy, str_val);
        }

        // Retourner une liste (tensors, metadata)
        List* result = lisp->provideList();
        result->append(tensors_dict);
        result->append(metadata_dict);
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_load_safetensors: " + std::string(e.what()));
    }
}

// mlx::save_safetensors - Sauvegarde en format safetensors
// Signature: deflib mlx_save_safetensors(file arrays (metadata))
Element* Lispe_mlx_methods::method_save_safetensors(LispE* lisp) {
    Element* file_elem = lisp->get_variable("file");
    Element* arrays_elem = lisp->get_variable("arrays");
    Element* metadata_elem = lisp->get_variable("metadata");

    try {
        std::string filename = file_elem->toString(lisp);

        // Convertir le dictionnaire LispE en unordered_map
        if (!arrays_elem->isDictionary()) {
            throw new Error("Error in mlx_save_safetensors: arrays must be a dictionary");
        }
        Dictionary* arrays_dict = (Dictionary*)arrays_elem;

        std::unordered_map<std::string, mx::array> tensors;
        for (auto& [key, elem] : arrays_dict->dictionary) {
            std::string name;
            u_ustring key_copy = key;  // Copie non-const
            s_unicode_to_utf8(name, key_copy);
            mx::array arr = element_to_array(lisp, elem);
            mx::eval(arr);
            tensors.emplace(name, arr);
        }

        // Convertir les métadonnées si présentes
        std::unordered_map<std::string, std::string> metadata;
        if (metadata_elem != null_ && metadata_elem->isDictionary()) {
            Dictionary* meta_dict = (Dictionary*)metadata_elem;
            for (auto& [key, elem] : meta_dict->dictionary) {
                std::string k, v;
                u_ustring key_copy = key;  // Copie non-const
                s_unicode_to_utf8(k, key_copy);
                v = elem->toString(lisp);
                metadata[k] = v;
            }
        }

        mx::save_safetensors(filename, tensors, metadata);
        return true_;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_save_safetensors: " + std::string(e.what()));
    }
}

// mlx::load_gguf - Charge un fichier GGUF
// Signature: deflib mlx_load_gguf(file)
// Retourne une liste (arrays_dict metadata_dict)
Element* Lispe_mlx_methods::method_load_gguf(LispE* lisp) {
    Element* file_elem = lisp->get_variable("file");

    try {
        std::string filename = file_elem->toString(lisp);
        auto [arrays, metadata] = mx::load_gguf(filename);

        // Créer le dictionnaire des arrays
        Dictionary* arrays_dict = lisp->provideDictionary();
        for (auto& [name, arr] : arrays) {
            mx::eval(arr);
            MLXArray* mlx_arr = new MLXArray(std::move(arr));
            std::string key_copy = name;  // Copie non-const
            arrays_dict->recording(key_copy, mlx_arr);
        }

        // Créer le dictionnaire des métadonnées
        // GGUFMetaData = std::variant<std::monostate, array, std::string, std::vector<std::string>>
        Dictionary* metadata_dict = lisp->provideDictionary();
        for (auto& [key, value] : metadata) {
            Element* meta_elem = null_;
            if (std::holds_alternative<std::string>(value)) {
                std::string str_copy = std::get<std::string>(value);
                meta_elem = lisp->provideString(str_copy);
            } else if (std::holds_alternative<mx::array>(value)) {
                mx::array arr = std::get<mx::array>(value);
                mx::eval(arr);
                meta_elem = new MLXArray(std::move(arr));
            } else if (std::holds_alternative<std::vector<std::string>>(value)) {
                List* str_list = lisp->provideList();
                for (const auto& s : std::get<std::vector<std::string>>(value)) {
                    std::string s_copy = s;
                    str_list->append(lisp->provideString(s_copy));
                }
                meta_elem = str_list;
            }
            if (meta_elem != null_) {
                std::string key_copy = key;  // Copie non-const
                metadata_dict->recording(key_copy, meta_elem);
            }
        }

        // Retourner une liste (arrays, metadata)
        List* result = lisp->provideList();
        result->append(arrays_dict);
        result->append(metadata_dict);
        return result;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_load_gguf: " + std::string(e.what()));
    }
}

// mlx::save_gguf - Sauvegarde en format GGUF
// Signature: deflib mlx_save_gguf(file arrays (metadata))
Element* Lispe_mlx_methods::method_save_gguf(LispE* lisp) {
    Element* file_elem = lisp->get_variable("file");
    Element* arrays_elem = lisp->get_variable("arrays");
    Element* metadata_elem = lisp->get_variable("metadata");

    try {
        std::string filename = file_elem->toString(lisp);

        // Convertir le dictionnaire LispE en unordered_map
        if (!arrays_elem->isDictionary()) {
            throw new Error("Error in mlx_save_gguf: arrays must be a dictionary");
        }
        Dictionary* arrays_dict = (Dictionary*)arrays_elem;

        std::unordered_map<std::string, mx::array> arrays_map;
        for (auto& [key, elem] : arrays_dict->dictionary) {
            std::string name;
            u_ustring key_copy = key;  // Copie non-const
            s_unicode_to_utf8(name, key_copy);
            mx::array arr = element_to_array(lisp, elem);
            mx::eval(arr);
            arrays_map.emplace(name, arr);
        }

        // Convertir les métadonnées si présentes
        // Pour simplifier, on ne supporte que les métadonnées de type string
        std::unordered_map<std::string, mx::GGUFMetaData> metadata;
        if (metadata_elem != null_ && metadata_elem->isDictionary()) {
            Dictionary* meta_dict = (Dictionary*)metadata_elem;
            for (auto& [key, elem] : meta_dict->dictionary) {
                std::string k;
                u_ustring key_copy = key;  // Copie non-const
                s_unicode_to_utf8(k, key_copy);
                if (elem->type == t_mlx_array) {
                    metadata[k] = ((MLXArray*)elem)->array;
                } else if (elem->isList()) {
                    std::vector<std::string> str_vec;
                    for (long i = 0; i < elem->size(); i++) {
                        str_vec.push_back(elem->index(i)->toString(lisp));
                    }
                    metadata[k] = str_vec;
                } else {
                    metadata[k] = elem->toString(lisp);
                }
            }
        }

        mx::save_gguf(filename, arrays_map, metadata);
        return true_;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_save_gguf: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 15: Quantification et mémoire
// ============================================================================

// mlx::quantize - Quantifie une matrice
// Signature: deflib mlx_quantize(array (group_size) (bits))
// Retourne une liste (w scales biases)
Element* Lispe_mlx_methods::method_quantize(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");
    Element* group_size_elem = lisp->get_variable("group_size");
    Element* bits_elem = lisp->get_variable("bits");

    try {
        mx::array arr = element_to_array(lisp, array_elem);

        int group_size = 64;
        int bits = 4;
        if (group_size_elem != null_) {
            group_size = group_size_elem->asInteger();
        }
        if (bits_elem != null_) {
            bits = bits_elem->asInteger();
        }

        std::vector<mx::array> result = mx::quantize(arr, group_size, bits);

        List* result_list = lisp->provideList();
        for (auto& r : result) {
            mx::eval(r);
            result_list->append(new MLXArray(std::move(r)));
        }
        return result_list;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_quantize: " + std::string(e.what()));
    }
}

// mlx::dequantize - Dé-quantifie une matrice
// Signature: deflib mlx_dequantize(w scales (biases) (group_size) (bits))
Element* Lispe_mlx_methods::method_dequantize(LispE* lisp) {
    Element* w_elem = lisp->get_variable("w");
    Element* scales_elem = lisp->get_variable("scales");
    Element* biases_elem = lisp->get_variable("biases");
    Element* group_size_elem = lisp->get_variable("group_size");
    Element* bits_elem = lisp->get_variable("bits");

    try {
        mx::array w = element_to_array(lisp, w_elem);
        mx::array scales = element_to_array(lisp, scales_elem);

        std::optional<mx::array> biases = std::nullopt;
        if (biases_elem != null_) {
            biases = element_to_array(lisp, biases_elem);
        }

        int group_size = 64;
        int bits = 4;
        if (group_size_elem != null_) {
            group_size = group_size_elem->asInteger();
        }
        if (bits_elem != null_) {
            bits = bits_elem->asInteger();
        }

        mx::array result = mx::dequantize(w, scales, biases, group_size, bits);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_dequantize: " + std::string(e.what()));
    }
}

// mlx::quantized_matmul - Multiplication matricielle quantifiée
// Signature: deflib mlx_quantized_matmul(x w scales (biases) (transposed) (group_size) (bits))
Element* Lispe_mlx_methods::method_quantized_matmul(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* w_elem = lisp->get_variable("w");
    Element* scales_elem = lisp->get_variable("scales");
    Element* biases_elem = lisp->get_variable("biases");
    Element* transpose_elem = lisp->get_variable("transposed");
    Element* group_size_elem = lisp->get_variable("group_size");
    Element* bits_elem = lisp->get_variable("bits");

    try {
        mx::array x = element_to_array(lisp, x_elem);
        mx::array w = element_to_array(lisp, w_elem);
        mx::array scales = element_to_array(lisp, scales_elem);

        std::optional<mx::array> biases = std::nullopt;
        if (biases_elem != null_) {
            biases = element_to_array(lisp, biases_elem);
        }

        bool transpose = true;
        int group_size = 64;
        int bits = 4;

        if (transpose_elem != null_) {
            transpose = transpose_elem->Boolean();
        }
        if (group_size_elem != null_) {
            group_size = group_size_elem->asInteger();
        }
        if (bits_elem != null_) {
            bits = bits_elem->asInteger();
        }

        mx::array result = mx::quantized_matmul(x, w, scales, biases, transpose, group_size, bits);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_quantized_matmul: " + std::string(e.what()));
    }
}

// mlx::synchronize - Synchronise les opérations GPU
// Signature: deflib mlx_synchronize()
Element* Lispe_mlx_methods::method_synchronize(LispE* lisp) {
    try {
        mx::synchronize();
        return true_;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_synchronize: " + std::string(e.what()));
    }
}

// mlx::clear_cache - Nettoie le cache mémoire
// Signature: deflib mlx_clear_cache()
Element* Lispe_mlx_methods::method_clear_cache(LispE* lisp) {
    try {
        mx::clear_cache();
        return true_;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_clear_cache: " + std::string(e.what()));
    }
}

// mlx::get_active_memory - Retourne la mémoire active en bytes
// Signature: deflib mlx_get_active_memory()
Element* Lispe_mlx_methods::method_get_active_memory(LispE* lisp) {
    try {
        size_t mem = mx::get_active_memory();
        return lisp->provideInteger(static_cast<long>(mem));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_get_active_memory: " + std::string(e.what()));
    }
}

// mlx::get_peak_memory - Retourne la mémoire pic en bytes
// Signature: deflib mlx_get_peak_memory()
Element* Lispe_mlx_methods::method_get_peak_memory(LispE* lisp) {
    try {
        size_t mem = mx::get_peak_memory();
        return lisp->provideInteger(static_cast<long>(mem));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_get_peak_memory: " + std::string(e.what()));
    }
}

// mlx::get_cache_memory - Retourne la mémoire cache en bytes
// Signature: deflib mlx_get_cache_memory()
Element* Lispe_mlx_methods::method_get_cache_memory(LispE* lisp) {
    try {
        size_t mem = mx::get_cache_memory();
        return lisp->provideInteger(static_cast<long>(mem));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_get_cache_memory: " + std::string(e.what()));
    }
}

// mlx::set_memory_limit - Définit la limite mémoire
// Signature: deflib mlx_set_memory_limit(limit)
// Retourne la limite précédente
Element* Lispe_mlx_methods::method_set_memory_limit(LispE* lisp) {
    Element* limit_elem = lisp->get_variable("limit");

    try {
        size_t limit = static_cast<size_t>(limit_elem->asInteger());
        size_t prev = mx::set_memory_limit(limit);
        return lisp->provideInteger(static_cast<long>(prev));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_set_memory_limit: " + std::string(e.what()));
    }
}

// mlx::set_cache_limit - Définit la limite du cache
// Signature: deflib mlx_set_cache_limit(limit)
// Retourne la limite précédente
Element* Lispe_mlx_methods::method_set_cache_limit(LispE* lisp) {
    Element* limit_elem = lisp->get_variable("limit");

    try {
        size_t limit = static_cast<size_t>(limit_elem->asInteger());
        size_t prev = mx::set_cache_limit(limit);
        return lisp->provideInteger(static_cast<long>(prev));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_set_cache_limit: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 16: Opérations supplémentaires
// ============================================================================

// mx::slice - Extraire une tranche d'un array
// Signature: deflib mlx_slice(array start stop (strides))
Element* Lispe_mlx_methods::method_slice(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* start_elem = lisp->get_variable("start");
    Element* stop_elem = lisp->get_variable("stop");
    Element* strides_elem = lisp->get_variable("strides");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        // Convertir start en Shape
        mx::Shape start;
        if (start_elem->isList()) {
            for (long i = 0; i < start_elem->size(); i++) {
                start.push_back(start_elem->index(i)->asInteger());
            }
        } else {
            start.push_back(start_elem->asInteger());
        }

        // Convertir stop en Shape
        mx::Shape stop;
        if (stop_elem->isList()) {
            for (long i = 0; i < stop_elem->size(); i++) {
                stop.push_back(stop_elem->index(i)->asInteger());
            }
        } else {
            stop.push_back(stop_elem->asInteger());
        }

        if (strides_elem != null_ && strides_elem->type != v_emptyatom) {
            // Convertir strides en Shape
            mx::Shape strides;
            if (strides_elem->isList()) {
                for (long i = 0; i < strides_elem->size(); i++) {
                    strides.push_back(strides_elem->index(i)->asInteger());
                }
            } else {
                strides.push_back(strides_elem->asInteger());
            }
            mx::array result = mx::slice(arr, start, stop, strides);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::slice(arr, start, stop);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_slice: " + std::string(e.what()));
    }
}

// mx::trace - Calcule la trace d'une matrice (somme de la diagonale)
// Signature: deflib mlx_trace(array (offset) (axis1) (axis2))
Element* Lispe_mlx_methods::method_trace(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* offset_elem = lisp->get_variable("offset");
    Element* axis1_elem = lisp->get_variable("axis1");
    Element* axis2_elem = lisp->get_variable("axis2");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (offset_elem != null_ && offset_elem->type != v_emptyatom) {
            int offset = offset_elem->asInteger();
            int axis1 = (axis1_elem != null_ && axis1_elem->type != v_emptyatom) ? axis1_elem->asInteger() : 0;
            int axis2 = (axis2_elem != null_ && axis2_elem->type != v_emptyatom) ? axis2_elem->asInteger() : 1;
            mx::array result = mx::trace(arr, offset, axis1, axis2);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::trace(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_trace: " + std::string(e.what()));
    }
}

// mx::diagonal - Extraire la diagonale d'une matrice
// Signature: deflib mlx_diagonal(array (offset) (axis1) (axis2))
Element* Lispe_mlx_methods::method_diagonal(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* offset_elem = lisp->get_variable("offset");
    Element* axis1_elem = lisp->get_variable("axis1");
    Element* axis2_elem = lisp->get_variable("axis2");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        int offset = (offset_elem != null_ && offset_elem->type != v_emptyatom) ? offset_elem->asInteger() : 0;
        int axis1 = (axis1_elem != null_ && axis1_elem->type != v_emptyatom) ? axis1_elem->asInteger() : 0;
        int axis2 = (axis2_elem != null_ && axis2_elem->type != v_emptyatom) ? axis2_elem->asInteger() : 1;

        mx::array result = mx::diagonal(arr, offset, axis1, axis2);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_diagonal: " + std::string(e.what()));
    }
}

// mx::diag - Créer une matrice diagonale à partir d'un vecteur ou extraire la diagonale
// Signature: deflib mlx_diag(array (k))
Element* Lispe_mlx_methods::method_diag(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* k_elem = lisp->get_variable("k");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        int k = (k_elem != null_ && k_elem->type != v_emptyatom) ? k_elem->asInteger() : 0;

        mx::array result = mx::diag(arr, k);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_diag: " + std::string(e.what()));
    }
}

// mx::tensordot - Produit tensoriel
// Signature: deflib mlx_tensordot(a b (axes))
Element* Lispe_mlx_methods::method_tensordot(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            int axes = axes_elem->asInteger();
            mx::array result = mx::tensordot(a, b, axes);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::tensordot(a, b, 2);  // default: 2 axes
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tensordot: " + std::string(e.what()));
    }
}

// mx::inner - Produit intérieur de deux vecteurs
// Signature: deflib mlx_inner(a b)
Element* Lispe_mlx_methods::method_inner(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::inner(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_inner: " + std::string(e.what()));
    }
}

// mx::outer - Produit extérieur de deux vecteurs
// Signature: deflib mlx_outer(a b)
Element* Lispe_mlx_methods::method_outer(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        mx::array result = mx::outer(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_outer: " + std::string(e.what()));
    }
}

// mx::cumsum - Somme cumulative
// Signature: deflib mlx_cumsum(array (axis) (reverse) (inclusive))
Element* Lispe_mlx_methods::method_cumsum(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("thearray");
    Element* axis_elem = lisp->get_variable("theaxis");
    Element* reverse_elem = lisp->get_variable("thereverse");
    Element* inclusive_elem = lisp->get_variable("inclusive");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        bool reverse = (reverse_elem != null_ && reverse_elem->type != v_emptyatom) ? reverse_elem->Boolean() : false;
        bool inclusive = (inclusive_elem != null_ && inclusive_elem->type != v_emptyatom) ? inclusive_elem->Boolean() : true;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::cumsum(arr, axis, reverse, inclusive);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::cumsum(arr, reverse, inclusive);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cumsum: " + std::string(e.what()));
    }
}

// mx::cumprod - Produit cumulatif
// Signature: deflib mlx_cumprod(array (axis) (reverse) (inclusive))
Element* Lispe_mlx_methods::method_cumprod(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("thearray");
    Element* axis_elem = lisp->get_variable("theaxis");
    Element* reverse_elem = lisp->get_variable("thereverse");
    Element* inclusive_elem = lisp->get_variable("inclusive");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        bool reverse = (reverse_elem != null_ && reverse_elem->type != v_emptyatom) ? reverse_elem->Boolean() : false;
        bool inclusive = (inclusive_elem != null_ && inclusive_elem->type != v_emptyatom) ? inclusive_elem->Boolean() : true;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::cumprod(arr, axis, reverse, inclusive);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::cumprod(arr, reverse, inclusive);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cumprod: " + std::string(e.what()));
    }
}

// mx::topk - Retourne les k plus grandes valeurs
// Signature: deflib mlx_topk(array k (axis))
Element* Lispe_mlx_methods::method_topk(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* k_elem = lisp->get_variable("k");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        int k = k_elem->asInteger();

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::topk(arr, k, axis);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::topk(arr, k);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_topk: " + std::string(e.what()));
    }
}

// mx::partition - Partitionne l'array autour du k-ième élément
// Signature: deflib mlx_partition(array kth (axis))
Element* Lispe_mlx_methods::method_partition(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* kth_elem = lisp->get_variable("kth");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        int kth = kth_elem->asInteger();

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::partition(arr, kth, axis);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::partition(arr, kth);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_partition: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 17: FFT (Transformées de Fourier)
// ============================================================================

// mlx_fft - Transformée de Fourier 1D
// Signature: deflib mlx_fft(array (n) (axis))
Element* Lispe_mlx_methods::method_fft(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* n_elem = lisp->get_variable("n");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        int axis = -1;
        int n = 0;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            axis = axis_elem->asInteger();
        }

        if (n_elem != null_ && n_elem->type != v_emptyatom) {
            n = n_elem->asInteger();
            mx::array result = mx::fft::fft(arr, n, axis);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::fft(arr, axis);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fft: " + std::string(e.what()));
    }
}

// mlx_ifft - Transformée de Fourier inverse 1D
// Signature: deflib mlx_ifft(array (n) (axis))
Element* Lispe_mlx_methods::method_ifft(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* n_elem = lisp->get_variable("n");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        int axis = -1;
        int n = 0;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            axis = axis_elem->asInteger();
        }

        if (n_elem != null_ && n_elem->type != v_emptyatom) {
            n = n_elem->asInteger();
            mx::array result = mx::fft::ifft(arr, n, axis);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::ifft(arr, axis);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ifft: " + std::string(e.what()));
    }
}

// mlx_fft2 - Transformée de Fourier 2D
// Signature: deflib mlx_fft2(array (s) (axes))
Element* Lispe_mlx_methods::method_fft2(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        std::vector<int> axes_vec = {-2, -1};  // Default axes
        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            axes_vec = element_to_axes(axes_elem);
        }

        if (s_elem != null_ && s_elem->type != v_emptyatom) {
            mx::Shape s_shape;
            element_to_shape(s_elem, s_shape);
            mx::array result = mx::fft::fft2(arr, s_shape, axes_vec);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::fft2(arr, axes_vec);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fft2: " + std::string(e.what()));
    }
}

// mlx_ifft2 - Transformée de Fourier inverse 2D
// Signature: deflib mlx_ifft2(array (s) (axes))
Element* Lispe_mlx_methods::method_ifft2(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        std::vector<int> axes_vec = {-2, -1};  // Default axes
        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            axes_vec = element_to_axes(axes_elem);
        }

        if (s_elem != null_ && s_elem->type != v_emptyatom) {
            mx::Shape s_shape;
            element_to_shape(s_elem, s_shape);
            mx::array result = mx::fft::ifft2(arr, s_shape, axes_vec);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::ifft2(arr, axes_vec);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ifft2: " + std::string(e.what()));
    }
}

// mlx_fftn - Transformée de Fourier N-dimensionnelle
// Signature: deflib mlx_fftn(array (s) (axes))
Element* Lispe_mlx_methods::method_fftn(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes_vec = element_to_axes(axes_elem);
            if (s_elem != null_ && s_elem->type != v_emptyatom) {
                mx::Shape s_shape;
                element_to_shape(s_elem, s_shape);
                mx::array result = mx::fft::fftn(arr, s_shape, axes_vec);
                return new MLXArray(std::move(result));
            } else {
                mx::array result = mx::fft::fftn(arr, axes_vec);
                return new MLXArray(std::move(result));
            }
        } else {
            mx::array result = mx::fft::fftn(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fftn: " + std::string(e.what()));
    }
}

// mlx_ifftn - Transformée de Fourier inverse N-dimensionnelle
// Signature: deflib mlx_ifftn(array (s) (axes))
Element* Lispe_mlx_methods::method_ifftn(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes_vec = element_to_axes(axes_elem);
            if (s_elem != null_ && s_elem->type != v_emptyatom) {
                mx::Shape s_shape;
                element_to_shape(s_elem, s_shape);
                mx::array result = mx::fft::ifftn(arr, s_shape, axes_vec);
                return new MLXArray(std::move(result));
            } else {
                mx::array result = mx::fft::ifftn(arr, axes_vec);
                return new MLXArray(std::move(result));
            }
        } else {
            mx::array result = mx::fft::ifftn(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ifftn: " + std::string(e.what()));
    }
}

// mlx_rfft - Transformée de Fourier réelle 1D
// Signature: deflib mlx_rfft(array (n) (axis))
Element* Lispe_mlx_methods::method_rfft(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* n_elem = lisp->get_variable("n");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        int axis = -1;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            axis = axis_elem->asInteger();
        }

        if (n_elem != null_ && n_elem->type != v_emptyatom) {
            int n = n_elem->asInteger();
            mx::array result = mx::fft::rfft(arr, n, axis);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::rfft(arr, axis);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_rfft: " + std::string(e.what()));
    }
}

// mlx_irfft - Transformée de Fourier réelle inverse 1D
// Signature: deflib mlx_irfft(array (n) (axis))
Element* Lispe_mlx_methods::method_irfft(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* n_elem = lisp->get_variable("n");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        int axis = -1;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            axis = axis_elem->asInteger();
        }

        if (n_elem != null_ && n_elem->type != v_emptyatom) {
            int n = n_elem->asInteger();
            mx::array result = mx::fft::irfft(arr, n, axis);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::irfft(arr, axis);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_irfft: " + std::string(e.what()));
    }
}

// mlx_rfft2 - Transformée de Fourier réelle 2D
// Signature: deflib mlx_rfft2(array (s) (axes))
Element* Lispe_mlx_methods::method_rfft2(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        std::vector<int> axes_vec = {-2, -1};  // Default axes
        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            axes_vec = element_to_axes(axes_elem);
        }

        if (s_elem != null_ && s_elem->type != v_emptyatom) {
            mx::Shape s_shape;
            element_to_shape(s_elem, s_shape);
            mx::array result = mx::fft::rfft2(arr, s_shape, axes_vec);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::rfft2(arr, axes_vec);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_rfft2: " + std::string(e.what()));
    }
}

// mlx_irfft2 - Transformée de Fourier réelle inverse 2D
// Signature: deflib mlx_irfft2(array (s) (axes))
Element* Lispe_mlx_methods::method_irfft2(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        std::vector<int> axes_vec = {-2, -1};  // Default axes
        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            axes_vec = element_to_axes(axes_elem);
        }

        if (s_elem != null_ && s_elem->type != v_emptyatom) {
            mx::Shape s_shape;
            element_to_shape(s_elem, s_shape);
            mx::array result = mx::fft::irfft2(arr, s_shape, axes_vec);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::irfft2(arr, axes_vec);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_irfft2: " + std::string(e.what()));
    }
}

// mlx_rfftn - Transformée de Fourier réelle N-dimensionnelle
// Signature: deflib mlx_rfftn(array (s) (axes))
Element* Lispe_mlx_methods::method_rfftn(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes_vec = element_to_axes(axes_elem);
            if (s_elem != null_ && s_elem->type != v_emptyatom) {
                mx::Shape s_shape;
                element_to_shape(s_elem, s_shape);
                mx::array result = mx::fft::rfftn(arr, s_shape, axes_vec);
                return new MLXArray(std::move(result));
            } else {
                mx::array result = mx::fft::rfftn(arr, axes_vec);
                return new MLXArray(std::move(result));
            }
        } else {
            mx::array result = mx::fft::rfftn(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_rfftn: " + std::string(e.what()));
    }
}

// mlx_irfftn - Transformée de Fourier réelle inverse N-dimensionnelle
// Signature: deflib mlx_irfftn(array (s) (axes))
Element* Lispe_mlx_methods::method_irfftn(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* s_elem = lisp->get_variable("s");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes_vec = element_to_axes(axes_elem);
            if (s_elem != null_ && s_elem->type != v_emptyatom) {
                mx::Shape s_shape;
                element_to_shape(s_elem, s_shape);
                mx::array result = mx::fft::irfftn(arr, s_shape, axes_vec);
                return new MLXArray(std::move(result));
            } else {
                mx::array result = mx::fft::irfftn(arr, axes_vec);
                return new MLXArray(std::move(result));
            }
        } else {
            mx::array result = mx::fft::irfftn(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_irfftn: " + std::string(e.what()));
    }
}

// mlx_fftshift - Décale la composante fréquence zéro au centre
// Signature: deflib mlx_fftshift(array (axes))
Element* Lispe_mlx_methods::method_fftshift(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes_vec = element_to_axes(axes_elem);
            mx::array result = mx::fft::fftshift(arr, axes_vec);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::fftshift(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fftshift: " + std::string(e.what()));
    }
}

// mlx_ifftshift - Inverse de fftshift
// Signature: deflib mlx_ifftshift(array (axes))
Element* Lispe_mlx_methods::method_ifftshift(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes_vec = element_to_axes(axes_elem);
            mx::array result = mx::fft::ifftshift(arr, axes_vec);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::fft::ifftshift(arr);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ifftshift: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 18: Fonctions mathématiques avancées
// ============================================================================

// mlx_arctan2 - Arc tangente à 2 arguments (angle en radians)
// Signature: deflib mlx_arctan2(a b)
Element* Lispe_mlx_methods::method_arctan2(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::arctan2(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arctan2: " + std::string(e.what()));
    }
}

// mlx_arcsinh - Sinus hyperbolique inverse
// Signature: deflib mlx_arcsinh(array)
Element* Lispe_mlx_methods::method_arcsinh(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::arcsinh(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arcsinh: " + std::string(e.what()));
    }
}

// mlx_arccosh - Cosinus hyperbolique inverse
// Signature: deflib mlx_arccosh(array)
Element* Lispe_mlx_methods::method_arccosh(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::arccosh(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arccosh: " + std::string(e.what()));
    }
}

// mlx_arctanh - Tangente hyperbolique inverse
// Signature: deflib mlx_arctanh(array)
Element* Lispe_mlx_methods::method_arctanh(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::arctanh(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_arctanh: " + std::string(e.what()));
    }
}

// mlx_degrees - Conversion radians vers degrés
// Signature: deflib mlx_degrees(array)
Element* Lispe_mlx_methods::method_degrees(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::degrees(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_degrees: " + std::string(e.what()));
    }
}

// mlx_radians - Conversion degrés vers radians
// Signature: deflib mlx_radians(array)
Element* Lispe_mlx_methods::method_radians(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::radians(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_radians: " + std::string(e.what()));
    }
}

// mlx_erf - Fonction erreur de Gauss
// Signature: deflib mlx_erf(array)
Element* Lispe_mlx_methods::method_erf(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::erf(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_erf: " + std::string(e.what()));
    }
}

// mlx_erfinv - Fonction erreur inverse
// Signature: deflib mlx_erfinv(array)
Element* Lispe_mlx_methods::method_erfinv(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::erfinv(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_erfinv: " + std::string(e.what()));
    }
}

// mlx_expm1 - exp(x) - 1 (précis pour petits x)
// Signature: deflib mlx_expm1(array)
Element* Lispe_mlx_methods::method_expm1(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::expm1(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_expm1: " + std::string(e.what()));
    }
}

// mlx_logaddexp - log(exp(a) + exp(b)) (numériquement stable)
// Signature: deflib mlx_logaddexp(a b)
Element* Lispe_mlx_methods::method_logaddexp(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::logaddexp(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_logaddexp: " + std::string(e.what()));
    }
}

// mlx_sign - Signe (-1, 0, ou 1)
// Signature: deflib mlx_sign(array)
Element* Lispe_mlx_methods::method_sign(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::sign(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_sign: " + std::string(e.what()));
    }
}

// mlx_kron - Produit de Kronecker
// Signature: deflib mlx_kron(a b)
Element* Lispe_mlx_methods::method_kron(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::kron(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_kron: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 19: Opérations bitwise
// ============================================================================

// mlx_bitwise_and - ET bit à bit
// Signature: deflib mlx_bitwise_and(a b)
Element* Lispe_mlx_methods::method_bitwise_and(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::bitwise_and(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_bitwise_and: " + std::string(e.what()));
    }
}

// mlx_bitwise_or - OU bit à bit
// Signature: deflib mlx_bitwise_or(a b)
Element* Lispe_mlx_methods::method_bitwise_or(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::bitwise_or(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_bitwise_or: " + std::string(e.what()));
    }
}

// mlx_bitwise_xor - XOR bit à bit
// Signature: deflib mlx_bitwise_xor(a b)
Element* Lispe_mlx_methods::method_bitwise_xor(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::bitwise_xor(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_bitwise_xor: " + std::string(e.what()));
    }
}

// mlx_bitwise_invert - NON bit à bit (complément)
// Signature: deflib mlx_bitwise_invert(array)
Element* Lispe_mlx_methods::method_bitwise_invert(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array result = mx::bitwise_invert(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_bitwise_invert: " + std::string(e.what()));
    }
}

// mlx_left_shift - Décalage à gauche
// Signature: deflib mlx_left_shift(a b)
Element* Lispe_mlx_methods::method_left_shift(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::left_shift(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_left_shift: " + std::string(e.what()));
    }
}

// mlx_right_shift - Décalage à droite
// Signature: deflib mlx_right_shift(a b)
Element* Lispe_mlx_methods::method_right_shift(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::right_shift(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_right_shift: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 20: Division et reste
// ============================================================================

// mlx_divmod - Retourne quotient et reste comme liste
// Signature: deflib mlx_divmod(a b)
Element* Lispe_mlx_methods::method_divmod(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        std::vector<mx::array> results = mx::divmod(a, b);
        
        mx::eval(results[0]);
        mx::eval(results[1]);
        
        List* result_list = lisp->provideList();
        result_list->append(new MLXArray(std::move(results[0])));
        result_list->append(new MLXArray(std::move(results[1])));
        return result_list;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_divmod: " + std::string(e.what()));
    }
}

// mlx_floor_divide - Division entière (floor(a/b))
// Signature: deflib mlx_floor_divide(a b)
Element* Lispe_mlx_methods::method_floor_divide(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::floor_divide(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_floor_divide: " + std::string(e.what()));
    }
}

// mlx_remainder - Reste de la division (modulo)
// Signature: deflib mlx_remainder(a b)
Element* Lispe_mlx_methods::method_remainder(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        mx::array result = mx::remainder(a, b);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_remainder: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 21: Opérations cumulatives et logsumexp
// ============================================================================

// mlx_cummax - Maximum cumulatif
// Signature: deflib mlx_cummax(array (axis) (reverse) (inclusive))
Element* Lispe_mlx_methods::method_cummax(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");
    Element* reverse_elem = lisp->get_variable("thereverse");
    Element* inclusive_elem = lisp->get_variable("inclusive");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        bool reverse = (reverse_elem != null_ && reverse_elem->type != v_emptyatom) ? reverse_elem->Boolean() : false;
        bool inclusive = (inclusive_elem != null_ && inclusive_elem->type != v_emptyatom) ? inclusive_elem->Boolean() : true;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::cummax(arr, axis, reverse, inclusive);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::cummax(arr, reverse, inclusive);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cummax: " + std::string(e.what()));
    }
}

// mlx_cummin - Minimum cumulatif
// Signature: deflib mlx_cummin(array (axis) (reverse) (inclusive))
Element* Lispe_mlx_methods::method_cummin(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");
    Element* reverse_elem = lisp->get_variable("thereverse");
    Element* inclusive_elem = lisp->get_variable("inclusive");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        bool reverse = (reverse_elem != null_ && reverse_elem->type != v_emptyatom) ? reverse_elem->Boolean() : false;
        bool inclusive = (inclusive_elem != null_ && inclusive_elem->type != v_emptyatom) ? inclusive_elem->Boolean() : true;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::cummin(arr, axis, reverse, inclusive);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::cummin(arr, reverse, inclusive);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cummin: " + std::string(e.what()));
    }
}

// mlx_logsumexp - log(sum(exp(x))) numériquement stable
// Signature: deflib mlx_logsumexp(array (axes) (keepdims))
Element* Lispe_mlx_methods::method_logsumexp(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* axes_elem = lisp->get_variable("axes");
    Element* keepdims_elem = lisp->get_variable("keepdims");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        bool keepdims = (keepdims_elem != null_ && keepdims_elem->type != v_emptyatom) ? keepdims_elem->Boolean() : false;

        if (axes_elem != null_ && axes_elem->type != v_emptyatom) {
            std::vector<int> axes = element_to_axes(axes_elem);
            mx::array result = mx::logsumexp(arr, axes, keepdims);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::logsumexp(arr, keepdims);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_logsumexp: " + std::string(e.what()));
    }
}

// mlx_logcumsumexp - logsumexp cumulatif
// Signature: deflib mlx_logcumsumexp(array (axis) (reverse) (inclusive))
Element* Lispe_mlx_methods::method_logcumsumexp(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* axis_elem = lisp->get_variable("axis");
    Element* reverse_elem = lisp->get_variable("thereverse");
    Element* inclusive_elem = lisp->get_variable("inclusive");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        bool reverse = (reverse_elem != null_ && reverse_elem->type != v_emptyatom) ? reverse_elem->Boolean() : false;
        bool inclusive = (inclusive_elem != null_ && inclusive_elem->type != v_emptyatom) ? inclusive_elem->Boolean() : true;

        if (axis_elem != null_ && axis_elem->type != v_emptyatom) {
            int axis = axis_elem->asInteger();
            mx::array result = mx::logcumsumexp(arr, axis, reverse, inclusive);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::logcumsumexp(arr, reverse, inclusive);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_logcumsumexp: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 22: Convolutions (deep learning)
// ============================================================================

// mlx_conv1d - Convolution 1D
// Signature: deflib mlx_conv1d(input weight (stride) (padding) (dilation) (groups))
Element* Lispe_mlx_methods::method_conv1d(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_elem = lisp->get_variable("padding");
    Element* dilation_elem = lisp->get_variable("dilation");
    Element* groups_elem = lisp->get_variable("groups");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        int stride = (stride_elem != null_ && stride_elem->type != v_emptyatom) ? stride_elem->asInteger() : 1;
        int padding = (padding_elem != null_ && padding_elem->type != v_emptyatom) ? padding_elem->asInteger() : 0;
        int dilation = (dilation_elem != null_ && dilation_elem->type != v_emptyatom) ? dilation_elem->asInteger() : 1;
        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;

        mx::array result = mx::conv1d(input, weight, stride, padding, dilation, groups);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv1d: " + std::string(e.what()));
    }
}

// mlx_conv2d - Convolution 2D
// Signature: deflib mlx_conv2d(input weight (stride) (padding) (dilation) (groups))
// stride, padding, dilation peuvent être des entiers ou des listes de 2 entiers
Element* Lispe_mlx_methods::method_conv2d(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_elem = lisp->get_variable("padding");
    Element* dilation_elem = lisp->get_variable("dilation");
    Element* groups_elem = lisp->get_variable("groups");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        // Stride par défaut: {1, 1}
        std::pair<int, int> stride = {1, 1};
        if (stride_elem != null_ && stride_elem->type != v_emptyatom) {
            if (stride_elem->isList()) {
                stride.first = stride_elem->index(0)->asInteger();
                stride.second = stride_elem->index(1)->asInteger();
            } else {
                int s = stride_elem->asInteger();
                stride = {s, s};
            }
        }

        // Padding par défaut: {0, 0}
        std::pair<int, int> padding = {0, 0};
        if (padding_elem != null_ && padding_elem->type != v_emptyatom) {
            if (padding_elem->isList()) {
                padding.first = padding_elem->index(0)->asInteger();
                padding.second = padding_elem->index(1)->asInteger();
            } else {
                int p = padding_elem->asInteger();
                padding = {p, p};
            }
        }

        // Dilation par défaut: {1, 1}
        std::pair<int, int> dilation = {1, 1};
        if (dilation_elem != null_ && dilation_elem->type != v_emptyatom) {
            if (dilation_elem->isList()) {
                dilation.first = dilation_elem->index(0)->asInteger();
                dilation.second = dilation_elem->index(1)->asInteger();
            } else {
                int d = dilation_elem->asInteger();
                dilation = {d, d};
            }
        }

        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;

        mx::array result = mx::conv2d(input, weight, stride, padding, dilation, groups);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv2d: " + std::string(e.what()));
    }
}

// mlx_conv3d - Convolution 3D
// Signature: deflib mlx_conv3d(input weight (stride) (padding) (dilation) (groups))
// stride, padding, dilation peuvent être des entiers ou des listes de 3 entiers
Element* Lispe_mlx_methods::method_conv3d(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_elem = lisp->get_variable("padding");
    Element* dilation_elem = lisp->get_variable("dilation");
    Element* groups_elem = lisp->get_variable("groups");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        // Stride par défaut: {1, 1, 1}
        std::tuple<int, int, int> stride = {1, 1, 1};
        if (stride_elem != null_ && stride_elem->type != v_emptyatom) {
            if (stride_elem->isList()) {
                stride = {stride_elem->index(0)->asInteger(),
                          stride_elem->index(1)->asInteger(),
                          stride_elem->index(2)->asInteger()};
            } else {
                int s = stride_elem->asInteger();
                stride = {s, s, s};
            }
        }

        // Padding par défaut: {0, 0, 0}
        std::tuple<int, int, int> padding = {0, 0, 0};
        if (padding_elem != null_ && padding_elem->type != v_emptyatom) {
            if (padding_elem->isList()) {
                padding = {padding_elem->index(0)->asInteger(),
                           padding_elem->index(1)->asInteger(),
                           padding_elem->index(2)->asInteger()};
            } else {
                int p = padding_elem->asInteger();
                padding = {p, p, p};
            }
        }

        // Dilation par défaut: {1, 1, 1}
        std::tuple<int, int, int> dilation = {1, 1, 1};
        if (dilation_elem != null_ && dilation_elem->type != v_emptyatom) {
            if (dilation_elem->isList()) {
                dilation = {dilation_elem->index(0)->asInteger(),
                            dilation_elem->index(1)->asInteger(),
                            dilation_elem->index(2)->asInteger()};
            } else {
                int d = dilation_elem->asInteger();
                dilation = {d, d, d};
            }
        }

        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;

        mx::array result = mx::conv3d(input, weight, stride, padding, dilation, groups);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv3d: " + std::string(e.what()));
    }
}

// mlx_conv_transpose1d - Convolution transposée 1D (déconvolution)
// Signature: deflib mlx_conv_transpose1d(input weight (stride) (padding) (dilation) (output_padding) (groups))
Element* Lispe_mlx_methods::method_conv_transpose1d(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_elem = lisp->get_variable("padding");
    Element* dilation_elem = lisp->get_variable("dilation");
    Element* output_padding_elem = lisp->get_variable("output_padding");
    Element* groups_elem = lisp->get_variable("groups");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        int stride = (stride_elem != null_ && stride_elem->type != v_emptyatom) ? stride_elem->asInteger() : 1;
        int padding = (padding_elem != null_ && padding_elem->type != v_emptyatom) ? padding_elem->asInteger() : 0;
        int dilation = (dilation_elem != null_ && dilation_elem->type != v_emptyatom) ? dilation_elem->asInteger() : 1;
        int output_padding = (output_padding_elem != null_ && output_padding_elem->type != v_emptyatom) ? output_padding_elem->asInteger() : 0;
        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;

        mx::array result = mx::conv_transpose1d(input, weight, stride, padding, dilation, output_padding, groups);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv_transpose1d: " + std::string(e.what()));
    }
}

// mlx_conv_transpose2d - Convolution transposée 2D (déconvolution)
// Signature: deflib mlx_conv_transpose2d(input weight (stride) (padding) (dilation) (output_padding) (groups))
Element* Lispe_mlx_methods::method_conv_transpose2d(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_elem = lisp->get_variable("padding");
    Element* dilation_elem = lisp->get_variable("dilation");
    Element* output_padding_elem = lisp->get_variable("output_padding");
    Element* groups_elem = lisp->get_variable("groups");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        // Stride par défaut: {1, 1}
        std::pair<int, int> stride = {1, 1};
        if (stride_elem != null_ && stride_elem->type != v_emptyatom) {
            if (stride_elem->isList()) {
                stride.first = stride_elem->index(0)->asInteger();
                stride.second = stride_elem->index(1)->asInteger();
            } else {
                int s = stride_elem->asInteger();
                stride = {s, s};
            }
        }

        // Padding par défaut: {0, 0}
        std::pair<int, int> padding = {0, 0};
        if (padding_elem != null_ && padding_elem->type != v_emptyatom) {
            if (padding_elem->isList()) {
                padding.first = padding_elem->index(0)->asInteger();
                padding.second = padding_elem->index(1)->asInteger();
            } else {
                int p = padding_elem->asInteger();
                padding = {p, p};
            }
        }

        // Dilation par défaut: {1, 1}
        std::pair<int, int> dilation = {1, 1};
        if (dilation_elem != null_ && dilation_elem->type != v_emptyatom) {
            if (dilation_elem->isList()) {
                dilation.first = dilation_elem->index(0)->asInteger();
                dilation.second = dilation_elem->index(1)->asInteger();
            } else {
                int d = dilation_elem->asInteger();
                dilation = {d, d};
            }
        }

        // Output padding par défaut: {0, 0}
        std::pair<int, int> output_padding = {0, 0};
        if (output_padding_elem != null_ && output_padding_elem->type != v_emptyatom) {
            if (output_padding_elem->isList()) {
                output_padding.first = output_padding_elem->index(0)->asInteger();
                output_padding.second = output_padding_elem->index(1)->asInteger();
            } else {
                int op = output_padding_elem->asInteger();
                output_padding = {op, op};
            }
        }

        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;

        mx::array result = mx::conv_transpose2d(input, weight, stride, padding, dilation, output_padding, groups);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv_transpose2d: " + std::string(e.what()));
    }
}

// mlx_conv_transpose3d - Convolution transposée 3D (déconvolution)
// Signature: deflib mlx_conv_transpose3d(input weight (stride) (padding) (dilation) (output_padding) (groups))
Element* Lispe_mlx_methods::method_conv_transpose3d(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_elem = lisp->get_variable("padding");
    Element* dilation_elem = lisp->get_variable("dilation");
    Element* output_padding_elem = lisp->get_variable("output_padding");
    Element* groups_elem = lisp->get_variable("groups");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        // Stride par défaut: {1, 1, 1}
        std::tuple<int, int, int> stride = {1, 1, 1};
        if (stride_elem != null_ && stride_elem->type != v_emptyatom) {
            if (stride_elem->isList()) {
                stride = {stride_elem->index(0)->asInteger(),
                          stride_elem->index(1)->asInteger(),
                          stride_elem->index(2)->asInteger()};
            } else {
                int s = stride_elem->asInteger();
                stride = {s, s, s};
            }
        }

        // Padding par défaut: {0, 0, 0}
        std::tuple<int, int, int> padding = {0, 0, 0};
        if (padding_elem != null_ && padding_elem->type != v_emptyatom) {
            if (padding_elem->isList()) {
                padding = {padding_elem->index(0)->asInteger(),
                           padding_elem->index(1)->asInteger(),
                           padding_elem->index(2)->asInteger()};
            } else {
                int p = padding_elem->asInteger();
                padding = {p, p, p};
            }
        }

        // Dilation par défaut: {1, 1, 1}
        std::tuple<int, int, int> dilation = {1, 1, 1};
        if (dilation_elem != null_ && dilation_elem->type != v_emptyatom) {
            if (dilation_elem->isList()) {
                dilation = {dilation_elem->index(0)->asInteger(),
                            dilation_elem->index(1)->asInteger(),
                            dilation_elem->index(2)->asInteger()};
            } else {
                int d = dilation_elem->asInteger();
                dilation = {d, d, d};
            }
        }

        // Output padding par défaut: {0, 0, 0}
        std::tuple<int, int, int> output_padding = {0, 0, 0};
        if (output_padding_elem != null_ && output_padding_elem->type != v_emptyatom) {
            if (output_padding_elem->isList()) {
                output_padding = {output_padding_elem->index(0)->asInteger(),
                                  output_padding_elem->index(1)->asInteger(),
                                  output_padding_elem->index(2)->asInteger()};
            } else {
                int op = output_padding_elem->asInteger();
                output_padding = {op, op, op};
            }
        }

        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;

        mx::array result = mx::conv_transpose3d(input, weight, stride, padding, dilation, output_padding, groups);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv_transpose3d: " + std::string(e.what()));
    }
}

// mlx_conv_general - Convolution générale N-dimensionnelle
// Signature: deflib mlx_conv_general(input weight (stride) (padding_lo) (padding_hi) (kernel_dilation) (input_dilation) (groups) (flip))
Element* Lispe_mlx_methods::method_conv_general(LispE* lisp) {
    Element* input_elem = lisp->get_variable("input");
    Element* weight_elem = lisp->get_variable("weight");
    Element* stride_elem = lisp->get_variable("stride");
    Element* padding_lo_elem = lisp->get_variable("padding_lo");
    Element* padding_hi_elem = lisp->get_variable("padding_hi");
    Element* kernel_dilation_elem = lisp->get_variable("kernel_dilation");
    Element* input_dilation_elem = lisp->get_variable("input_dilation");
    Element* groups_elem = lisp->get_variable("groups");
    Element* flip_elem = lisp->get_variable("theflip");

    try {
        mx::array input = element_to_array(lisp, input_elem);
        mx::array weight = element_to_array(lisp, weight_elem);

        std::vector<int> stride;
        if (stride_elem != null_ && stride_elem->type != v_emptyatom) {
            stride = element_to_axes(stride_elem);
        }

        std::vector<int> padding_lo;
        if (padding_lo_elem != null_ && padding_lo_elem->type != v_emptyatom) {
            padding_lo = element_to_axes(padding_lo_elem);
        }

        std::vector<int> padding_hi;
        if (padding_hi_elem != null_ && padding_hi_elem->type != v_emptyatom) {
            padding_hi = element_to_axes(padding_hi_elem);
        }

        std::vector<int> kernel_dilation;
        if (kernel_dilation_elem != null_ && kernel_dilation_elem->type != v_emptyatom) {
            kernel_dilation = element_to_axes(kernel_dilation_elem);
        }

        std::vector<int> input_dilation;
        if (input_dilation_elem != null_ && input_dilation_elem->type != v_emptyatom) {
            input_dilation = element_to_axes(input_dilation_elem);
        }

        int groups = (groups_elem != null_ && groups_elem->type != v_emptyatom) ? groups_elem->asInteger() : 1;
        bool flip = (flip_elem != null_ && flip_elem->type != v_emptyatom) ? flip_elem->Boolean() : false;

        mx::array result = mx::conv_general(input, weight, stride, padding_lo, padding_hi,
                                            kernel_dilation, input_dilation, groups, flip);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conv_general: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 23: Opérations spécialisées ML
// ============================================================================

// mlx_rms_norm - Root Mean Square normalization (utilisé dans LLaMA, Gemma)
// Signature: deflib mlx_rms_norm(x (weight) (eps) (gemma_style))
// gemma_style: si true, applique (1 + weight) au lieu de weight (défaut: false)
Element* Lispe_mlx_methods::method_rms_norm(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* weight_elem = lisp->get_variable("weight");
    Element* eps_elem = lisp->get_variable("eps");
    Element* gemma_style_elem = lisp->get_variable("gemma_style");

    try {
        mx::array x = element_to_array(lisp, x_elem);

        // Eps par défaut: 1e-5
        float eps = (eps_elem != null_ && eps_elem->type != v_emptyatom) ? eps_elem->asNumber() : 1e-5f;
        
        // Gemma style: utilise (1 + weight) au lieu de weight
        bool gemma_style = (gemma_style_elem != null_ && gemma_style_elem->type != v_emptyatom) 
                           ? gemma_style_elem->Boolean() : false;

        // Weight est optionnel
        std::optional<mx::array> weight = std::nullopt;
        if (weight_elem != null_ && weight_elem->type != v_emptyatom) {
            mx::array w = element_to_array(lisp, weight_elem);
            if (gemma_style) {
                // Gemma utilise (1 + weight) au lieu de weight
                w = mx::add(mx::ones_like(w), w);
            }
            weight = w;
        }

        mx::array result = mx::fast::rms_norm(x, weight, eps);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_rms_norm: " + std::string(e.what()));
    }
}

// mlx_layer_norm - Layer normalization
// Signature: deflib mlx_layer_norm(x (weight) (bias) (eps))
Element* Lispe_mlx_methods::method_layer_norm(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* weight_elem = lisp->get_variable("weight");
    Element* bias_elem = lisp->get_variable("bias");
    Element* eps_elem = lisp->get_variable("eps");

    try {
        mx::array x = element_to_array(lisp, x_elem);

        // Weight est optionnel
        std::optional<mx::array> weight = std::nullopt;
        if (weight_elem != null_ && weight_elem->type != v_emptyatom) {
            weight = element_to_array(lisp, weight_elem);
        }

        // Bias est optionnel
        std::optional<mx::array> bias = std::nullopt;
        if (bias_elem != null_ && bias_elem->type != v_emptyatom) {
            bias = element_to_array(lisp, bias_elem);
        }

        // Eps par défaut: 1e-5
        float eps = (eps_elem != null_ && eps_elem->type != v_emptyatom) ? eps_elem->asNumber() : 1e-5f;

        mx::array result = mx::fast::layer_norm(x, weight, bias, eps);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_layer_norm: " + std::string(e.what()));
    }
}

// mlx_rope - Rotary Position Embedding
// Signature: deflib mlx_rope(x dims (traditional) (base) (scale) (offset) (freqs))
Element* Lispe_mlx_methods::method_rope(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* dims_elem = lisp->get_variable("dims");
    Element* traditional_elem = lisp->get_variable("traditional");
    Element* base_elem = lisp->get_variable("base");
    Element* scale_elem = lisp->get_variable("scale");
    Element* offset_elem = lisp->get_variable("offset");
    Element* freqs_elem = lisp->get_variable("freqs");

    try {
        mx::array x = element_to_array(lisp, x_elem);
        int dims = dims_elem->asInteger();

        // Traditional par défaut: false
        bool traditional = (traditional_elem != null_ && traditional_elem->type != v_emptyatom) ? traditional_elem->Boolean() : false;

        // Base par défaut: 10000.0
        std::optional<float> base = std::nullopt;
        if (base_elem != null_ && base_elem->type != v_emptyatom) {
            base = base_elem->asNumber();
        }

        // Scale par défaut: 1.0
        float scale = (scale_elem != null_ && scale_elem->type != v_emptyatom) ? scale_elem->asNumber() : 1.0f;

        // Offset par défaut: 0
        int offset = (offset_elem != null_ && offset_elem->type != v_emptyatom) ? offset_elem->asInteger() : 0;

        // Freqs est optionnel
        std::optional<mx::array> freqs = std::nullopt;
        if (freqs_elem != null_ && freqs_elem->type != v_emptyatom) {
            freqs = element_to_array(lisp, freqs_elem);
        }

        mx::array result = mx::fast::rope(x, dims, traditional, base, scale, offset, freqs);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_rope: " + std::string(e.what()));
    }
}

// mlx_scaled_dot_product_attention - Attention mechanism
// Signature: deflib mlx_scaled_dot_product_attention(queries keys values scale (mask_mode) (mask_arrays) (sinks))
Element* Lispe_mlx_methods::method_scaled_dot_product_attention(LispE* lisp) {
    Element* queries_elem = lisp->get_variable("queries");
    Element* keys_elem = lisp->get_variable("keys");
    Element* values_elem = lisp->get_variable("values");
    Element* scale_elem = lisp->get_variable("scale");
    Element* mask_mode_elem = lisp->get_variable("mask_mode");
    Element* mask_arrays_elem = lisp->get_variable("mask_arrays");
    Element* sinks_elem = lisp->get_variable("sinks");

    try {
        mx::array queries = element_to_array(lisp, queries_elem);
        mx::array keys = element_to_array(lisp, keys_elem);
        mx::array values = element_to_array(lisp, values_elem);
        float scale = scale_elem->asNumber();

        // Mask mode par défaut: "" (pas de masque)
        std::string mask_mode = "";
        if (mask_mode_elem != null_ && mask_mode_elem->type != v_emptyatom) {
            mask_mode = mask_mode_elem->toString(lisp);
        }

        // Mask arrays est optionnel
        std::vector<mx::array> mask_arrs;
        if (mask_arrays_elem != null_ && mask_arrays_elem->type != v_emptyatom && mask_arrays_elem->isList()) {
            long sz = mask_arrays_elem->size();
            for (long i = 0; i < sz; i++) {
                mask_arrs.push_back(element_to_array(lisp, mask_arrays_elem->index(i)));
            }
        }

        // Sinks est optionnel (pour attention sinks)
        std::optional<mx::array> sinks;
        if (sinks_elem != null_ && sinks_elem->type != v_emptyatom) {
            sinks = element_to_array(lisp, sinks_elem);
        }

        mx::array result = mx::fast::scaled_dot_product_attention(queries, keys, values, scale, mask_mode, mask_arrs, sinks);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_scaled_dot_product_attention: " + std::string(e.what()));
    }
}

// mlx_einsum - Einstein summation convention
// Signature: deflib mlx_einsum(subscripts operands)
Element* Lispe_mlx_methods::method_einsum(LispE* lisp) {
    Element* subscripts_elem = lisp->get_variable("subscripts");
    Element* operands_elem = lisp->get_variable("operands");

    try {
        std::string subscripts = subscripts_elem->toString(lisp);

        // Collecter les operands (doit être une liste d'arrays)
        std::vector<mx::array> operands;
        if (!operands_elem->isList()) {
            throw new Error("Error in mlx_einsum: operands must be a list of arrays");
        }

        long sz = operands_elem->size();
        for (long i = 0; i < sz; i++) {
            operands.push_back(element_to_array(lisp, operands_elem->index(i)));
        }

        mx::array result = mx::einsum(subscripts, operands);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_einsum: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 24: Manipulation et création avancée
// ============================================================================

// mlx_dtype - Retourne le type d'un array sous forme de chaîne
// Signature: deflib mlx_dtype(array)
Element* Lispe_mlx_methods::method_dtype(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::Dtype dt = arr.dtype();
        
        std::string dtype_str;
        if (dt == mx::float32) dtype_str = "float32";
        else if (dt == mx::float64) dtype_str = "float64";
        else if (dt == mx::float16) dtype_str = "float16";
        else if (dt == mx::bfloat16) dtype_str = "bfloat16";
        else if (dt == mx::int32) dtype_str = "int32";
        else if (dt == mx::int64) dtype_str = "int64";
        else if (dt == mx::int16) dtype_str = "int16";
        else if (dt == mx::int8) dtype_str = "int8";
        else if (dt == mx::uint32) dtype_str = "uint32";
        else if (dt == mx::uint64) dtype_str = "uint64";
        else if (dt == mx::uint16) dtype_str = "uint16";
        else if (dt == mx::uint8) dtype_str = "uint8";
        else if (dt == mx::bool_) dtype_str = "bool";
        else if (dt == mx::complex64) dtype_str = "complex64";
        else dtype_str = "unknown";

        return lisp->provideString(dtype_str);
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_dtype: " + std::string(e.what()));
    }
}

// mlx_astype - Conversion de type explicite
// Signature: deflib mlx_astype(array dtype)
Element* Lispe_mlx_methods::method_astype(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        // Parser le dtype
        std::string dtype_str = dtype_elem->toString(lisp);
        mx::Dtype target_dtype = mx::float32; // Par défaut

        if (dtype_str == "float32") {
            target_dtype = mx::float32;
        } else if (dtype_str == "float64") {
            target_dtype = mx::float64;
        } else if (dtype_str == "int32") {
            target_dtype = mx::int32;
        } else if (dtype_str == "int64") {
            target_dtype = mx::int64;
        } else if (dtype_str == "int16") {
            target_dtype = mx::int16;
        } else if (dtype_str == "int8") {
            target_dtype = mx::int8;
        } else if (dtype_str == "uint32") {
            target_dtype = mx::uint32;
        } else if (dtype_str == "uint64") {
            target_dtype = mx::uint64;
        } else if (dtype_str == "uint16") {
            target_dtype = mx::uint16;
        } else if (dtype_str == "uint8") {
            target_dtype = mx::uint8;
        } else if (dtype_str == "bool") {
            target_dtype = mx::bool_;
        } else if (dtype_str == "bfloat16") {
            target_dtype = mx::bfloat16;
        } else if (dtype_str == "float16") {
            target_dtype = mx::float16;
        } else {
            throw new Error("Error in mlx_astype: unknown dtype '" + dtype_str + "'");
        }

        mx::array result = mx::astype(arr, target_dtype);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_astype: " + std::string(e.what()));
    }
}

// mlx_copy - Copie explicite d'un array
// Signature: deflib mlx_copy(array)
Element* Lispe_mlx_methods::method_copy(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        // Créer une copie en faisant une opération qui force la copie
        mx::array result = mx::array(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_copy: " + std::string(e.what()));
    }
}

// mlx_zeros_like - Crée des zéros avec même forme
// Signature: deflib mlx_zeros_like(array)
Element* Lispe_mlx_methods::method_zeros_like(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        mx::array result = mx::zeros_like(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_zeros_like: " + std::string(e.what()));
    }
}

// mlx_ones_like - Crée des uns avec même forme
// Signature: deflib mlx_ones_like(array)
Element* Lispe_mlx_methods::method_ones_like(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        mx::array result = mx::ones_like(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_ones_like: " + std::string(e.what()));
    }
}

// mlx_nan_to_num - Remplace NaN et inf par des valeurs
// Signature: deflib mlx_nan_to_num(array (nan) (posinf) (neginf))
Element* Lispe_mlx_methods::method_nan_to_num(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* nan_elem = lisp->get_variable("nan");
    Element* posinf_elem = lisp->get_variable("posinf");
    Element* neginf_elem = lisp->get_variable("neginf");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        float nan_val = (nan_elem != null_ && nan_elem->type != v_emptyatom) ? nan_elem->asNumber() : 0.0f;

        std::optional<float> posinf = std::nullopt;
        if (posinf_elem != null_ && posinf_elem->type != v_emptyatom) {
            posinf = posinf_elem->asNumber();
        }

        std::optional<float> neginf = std::nullopt;
        if (neginf_elem != null_ && neginf_elem->type != v_emptyatom) {
            neginf = neginf_elem->asNumber();
        }

        mx::array result = mx::nan_to_num(arr, nan_val, posinf, neginf);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_nan_to_num: " + std::string(e.what()));
    }
}

// mlx_atleast_1d - Convertit en au moins 1D
// Signature: deflib mlx_atleast_1d(array)
Element* Lispe_mlx_methods::method_atleast_1d(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        mx::array result = mx::atleast_1d(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_atleast_1d: " + std::string(e.what()));
    }
}

// mlx_atleast_2d - Convertit en au moins 2D
// Signature: deflib mlx_atleast_2d(array)
Element* Lispe_mlx_methods::method_atleast_2d(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        mx::array result = mx::atleast_2d(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_atleast_2d: " + std::string(e.what()));
    }
}

// mlx_atleast_3d - Convertit en au moins 3D
// Signature: deflib mlx_atleast_3d(array)
Element* Lispe_mlx_methods::method_atleast_3d(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);

        mx::array result = mx::atleast_3d(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_atleast_3d: " + std::string(e.what()));
    }
}

// mlx_array_equal - Test d'égalité de deux arrays
// Signature: deflib mlx_array_equal(a b (equal_nan))
Element* Lispe_mlx_methods::method_array_equal(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");
    Element* equal_nan_elem = lisp->get_variable("equal_nan");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        bool equal_nan = (equal_nan_elem != null_ && equal_nan_elem->type != v_emptyatom) ? equal_nan_elem->Boolean() : false;

        mx::array result = mx::array_equal(a, b, equal_nan);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_array_equal: " + std::string(e.what()));
    }
}

// mlx_allclose - Test de proximité avec tolérance
// Signature: deflib mlx_allclose(a b (rtol) (atol) (equal_nan))
Element* Lispe_mlx_methods::method_allclose(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");
    Element* rtol_elem = lisp->get_variable("rtol");
    Element* atol_elem = lisp->get_variable("atol");
    Element* equal_nan_elem = lisp->get_variable("equal_nan");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);

        double rtol = (rtol_elem != null_ && rtol_elem->type != v_emptyatom) ? rtol_elem->asNumber() : 1e-5;
        double atol = (atol_elem != null_ && atol_elem->type != v_emptyatom) ? atol_elem->asNumber() : 1e-8;
        bool equal_nan = (equal_nan_elem != null_ && equal_nan_elem->type != v_emptyatom) ? equal_nan_elem->Boolean() : false;

        mx::array result = mx::allclose(a, b, rtol, atol, equal_nan);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_allclose: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 25: Random avancé
// ============================================================================

// mlx_random_bernoulli - Distribution de Bernoulli
// Signature: deflib mlx_random_bernoulli(p (shape))
Element* Lispe_mlx_methods::method_random_bernoulli(LispE* lisp) {
    Element* p_elem = lisp->get_variable("p");
    Element* shape_elem = lisp->get_variable("shape");

    try {
        if (shape_elem != null_ && shape_elem->type != v_emptyatom) {
            // Avec shape spécifié
            mx::Shape shape;
            element_to_shape(shape_elem, shape);
            
            mx::array result = p_elem->isNumber()
                ? mx::random::bernoulli(mx::array(p_elem->asNumber()), shape)
                : mx::random::bernoulli(element_to_array(lisp, p_elem), shape);
            
            return new MLXArray(std::move(result));
        } else {
            // Sans shape, utilise la forme de p
            mx::array result = p_elem->isNumber()
                ? mx::random::bernoulli(mx::array(p_elem->asNumber()))
                : mx::random::bernoulli(element_to_array(lisp, p_elem));
            
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_bernoulli: " + std::string(e.what()));
    }
}

// mlx_random_truncated_normal - Distribution normale tronquée
// Signature: deflib mlx_random_truncated_normal(lower upper (shape) (dtype))
Element* Lispe_mlx_methods::method_random_truncated_normal(LispE* lisp) {
    Element* lower_elem = lisp->get_variable("lower");
    Element* upper_elem = lisp->get_variable("upper");
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::array lower_arr = element_to_array(lisp, lower_elem);
        mx::array upper_arr = element_to_array(lisp, upper_elem);
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        
        mx::array result = (shape_elem != null_ && shape_elem->type != v_emptyatom)
            ? [&]() { mx::Shape shape; element_to_shape(shape_elem, shape); return mx::random::truncated_normal(lower_arr, upper_arr, shape, dtype); }()
            : mx::random::truncated_normal(lower_arr, upper_arr, dtype);
        
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_truncated_normal: " + std::string(e.what()));
    }
}

// mlx_random_gumbel - Distribution de Gumbel
// Signature: deflib mlx_random_gumbel(shape (dtype))
Element* Lispe_mlx_methods::method_random_gumbel(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        
        mx::array result = mx::random::gumbel(shape, dtype);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_gumbel: " + std::string(e.what()));
    }
}

// mlx_random_categorical - Distribution catégorielle
// Signature: deflib mlx_random_categorical(logits (axis) (num_samples) (shape))
Element* Lispe_mlx_methods::method_random_categorical(LispE* lisp) {
    Element* logits_elem = lisp->get_variable("logits");
    Element* axis_elem = lisp->get_variable("axis");
    Element* num_samples_elem = lisp->get_variable("num_samples");
    Element* shape_elem = lisp->get_variable("shape");

    try {
        mx::array logits = element_to_array(lisp, logits_elem);
        int axis = (axis_elem != null_ && axis_elem->type != v_emptyatom) ? axis_elem->asInteger() : -1;
        
        if (shape_elem != null_ && shape_elem->type != v_emptyatom) {
            // Version avec shape
            mx::Shape shape;
            element_to_shape(shape_elem, shape);
            mx::array result = mx::random::categorical(logits, axis, shape);
            return new MLXArray(std::move(result));
        } else if (num_samples_elem != null_ && num_samples_elem->type != v_emptyatom) {
            // Version avec num_samples
            int num_samples = num_samples_elem->asInteger();
            mx::array result = mx::random::categorical(logits, axis, num_samples);
            return new MLXArray(std::move(result));
        } else {
            // Version simple
            mx::array result = mx::random::categorical(logits, axis);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_categorical: " + std::string(e.what()));
    }
}

// mlx_random_laplace - Distribution de Laplace
// Signature: deflib mlx_random_laplace(shape (loc) (scale) (dtype))
Element* Lispe_mlx_methods::method_random_laplace(LispE* lisp) {
    Element* shape_elem = lisp->get_variable("shape");
    Element* loc_elem = lisp->get_variable("loc");
    Element* scale_elem = lisp->get_variable("scale");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::Shape shape;
        element_to_shape(shape_elem, shape);
        
        float loc = (loc_elem != null_ && loc_elem->type != v_emptyatom) ? static_cast<float>(loc_elem->asNumber()) : 0.0f;
        float scale = (scale_elem != null_ && scale_elem->type != v_emptyatom) ? static_cast<float>(scale_elem->asNumber()) : 1.0f;
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        
        mx::array result = mx::random::laplace(shape, dtype, loc, scale);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_laplace: " + std::string(e.what()));
    }
}

// mlx_random_multivariate_normal - Distribution normale multivariée
// Signature: deflib mlx_random_multivariate_normal(mean cov (shape) (dtype))
Element* Lispe_mlx_methods::method_random_multivariate_normal(LispE* lisp) {
    Element* mean_elem = lisp->get_variable("mean");
    Element* cov_elem = lisp->get_variable("cov");
    Element* shape_elem = lisp->get_variable("shape");
    Element* dtype_elem = lisp->get_variable("dtype");

    try {
        mx::array mean = element_to_array(lisp, mean_elem);
        mx::array cov = element_to_array(lisp, cov_elem);
        mx::Dtype dtype = parse_dtype(lisp, dtype_elem, mx::float32);
        
        mx::Shape shape;
        if (shape_elem != null_ && shape_elem->type != v_emptyatom) {
            element_to_shape(shape_elem, shape);
        }
        // Si shape est vide, on prend {} comme shape
        
        mx::array result = mx::random::multivariate_normal(mean, cov, shape, dtype);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_multivariate_normal: " + std::string(e.what()));
    }
}

// mlx_random_permutation - Permutation aléatoire
// Signature: deflib mlx_random_permutation(x (axis))
Element* Lispe_mlx_methods::method_random_permutation(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        int axis = (axis_elem != null_ && axis_elem->type != v_emptyatom) ? axis_elem->asInteger() : 0;
        
        if (x_elem->isNumber()) {
            // Version avec entier: permutation de arange(x)
            int x = x_elem->asInteger();
            mx::array result = mx::random::permutation(x);
            return new MLXArray(std::move(result));
        } else {
            // Version avec array: permutation des éléments
            mx::array x = element_to_array(lisp, x_elem);
            mx::array result = mx::random::permutation(x, axis);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_random_permutation: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 26: Algèbre linéaire avancée
// ============================================================================

// mlx_tri_inv - Inverse de matrice triangulaire
// Signature: deflib mlx_tri_inv(array (upper))
Element* Lispe_mlx_methods::method_tri_inv(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* upper_elem = lisp->get_variable("upper");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        bool upper = (upper_elem != null_ && upper_elem->type != v_emptyatom) ? upper_elem->Boolean() : false;
        
        mx::array result = mx::linalg::tri_inv(arr, upper);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tri_inv: " + std::string(e.what()));
    }
}

// mlx_cholesky_inv - Inverse via décomposition de Cholesky
// Signature: deflib mlx_cholesky_inv(array (upper))
Element* Lispe_mlx_methods::method_cholesky_inv(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* upper_elem = lisp->get_variable("upper");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        bool upper = (upper_elem != null_ && upper_elem->type != v_emptyatom) ? upper_elem->Boolean() : false;
        
        mx::array result = mx::linalg::cholesky_inv(arr, upper);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_cholesky_inv: " + std::string(e.what()));
    }
}

// mlx_lu_factor - Factorisation LU avec pivoting
// Signature: deflib mlx_lu_factor(array)
// Retourne: liste (L_and_U, pivots)
Element* Lispe_mlx_methods::method_lu_factor(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        
        auto [lu, pivots] = mx::linalg::lu_factor(arr);
        mx::eval(lu);
        mx::eval(pivots);
        
        List* result_list = lisp->provideList();
        result_list->append(new MLXArray(std::move(lu)));
        result_list->append(new MLXArray(std::move(pivots)));
        return result_list;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_lu_factor: " + std::string(e.what()));
    }
}

// mlx_solve_triangular - Résolution de système triangulaire
// Signature: deflib mlx_solve_triangular(a b (upper))
Element* Lispe_mlx_methods::method_solve_triangular(LispE* lisp) {
    Element* a_elem = lisp->get_variable("a");
    Element* b_elem = lisp->get_variable("b");
    Element* upper_elem = lisp->get_variable("upper");

    try {
        mx::array a = element_to_array(lisp, a_elem);
        mx::array b = element_to_array(lisp, b_elem);
        bool upper = (upper_elem != null_ && upper_elem->type != v_emptyatom) ? upper_elem->Boolean() : false;
        
        mx::array result = mx::linalg::solve_triangular(a, b, upper);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_solve_triangular: " + std::string(e.what()));
    }
}

// mlx_eigvalsh - Valeurs propres d'une matrice hermitienne/symétrique
// Signature: deflib mlx_eigvalsh(array (UPLO))
Element* Lispe_mlx_methods::method_eigvalsh(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* uplo_elem = lisp->get_variable("UPLO");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        std::string uplo = (uplo_elem != null_ && uplo_elem->type != v_emptyatom) ? uplo_elem->toString(lisp) : "L";
        
        mx::array result = mx::linalg::eigvalsh(arr, uplo);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_eigvalsh: " + std::string(e.what()));
    }
}

// mlx_eigh - Valeurs propres et vecteurs propres d'une matrice hermitienne/symétrique
// Signature: deflib mlx_eigh(array (UPLO))
// Retourne: liste (eigenvalues, eigenvectors)
Element* Lispe_mlx_methods::method_eigh(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* uplo_elem = lisp->get_variable("UPLO");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        std::string uplo = (uplo_elem != null_ && uplo_elem->type != v_emptyatom) ? uplo_elem->toString(lisp) : "L";
        
        auto [eigenvalues, eigenvectors] = mx::linalg::eigh(arr, uplo);
        mx::eval(eigenvalues);
        mx::eval(eigenvectors);
        
        List* result_list = lisp->provideList();
        result_list->append(new MLXArray(std::move(eigenvalues)));
        result_list->append(new MLXArray(std::move(eigenvectors)));
        return result_list;
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_eigh: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 27: Gather/Scatter avancé
// ============================================================================

// mlx_take_along_axis - Extraction le long d'un axe selon indices
// Signature: deflib mlx_take_along_axis(array indices axis)
Element* Lispe_mlx_methods::method_take_along_axis(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* indices_elem = lisp->get_variable("indices");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array indices = element_to_array(lisp, indices_elem);
        int axis = axis_elem->asInteger();
        
        mx::array result = mx::take_along_axis(arr, indices, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_take_along_axis: " + std::string(e.what()));
    }
}

// mlx_put_along_axis - Insertion le long d'un axe selon indices
// Signature: deflib mlx_put_along_axis(array indices values axis)
Element* Lispe_mlx_methods::method_put_along_axis(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* indices_elem = lisp->get_variable("indices");
    Element* values_elem = lisp->get_variable("values");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array indices = element_to_array(lisp, indices_elem);
        mx::array values = element_to_array(lisp, values_elem);
        int axis = axis_elem->asInteger();
        
        mx::array result = mx::put_along_axis(arr, indices, values, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_put_along_axis: " + std::string(e.what()));
    }
}

// mlx_gather - Gather array entries given indices and slices
// Signature: deflib mlx_gather(array indices axis slice_sizes)
Element* Lispe_mlx_methods::method_gather(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* indices_elem = lisp->get_variable("indices");
    Element* axis_elem = lisp->get_variable("axis");
    Element* slice_sizes_elem = lisp->get_variable("slice_sizes");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array indices = element_to_array(lisp, indices_elem);
        int axis = axis_elem->asInteger();
        
        mx::Shape slice_sizes;
        element_to_shape(slice_sizes_elem, slice_sizes);
        
        mx::array result = mx::gather(arr, indices, axis, slice_sizes);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_gather: " + std::string(e.what()));
    }
}

// mlx_scatter - Scatter updates into array at given indices
// Signature: deflib mlx_scatter(array indices updates axis)
Element* Lispe_mlx_methods::method_scatter(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* indices_elem = lisp->get_variable("indices");
    Element* updates_elem = lisp->get_variable("updates");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array indices = element_to_array(lisp, indices_elem);
        mx::array updates = element_to_array(lisp, updates_elem);
        int axis = axis_elem->asInteger();
        
        mx::array result = mx::scatter(arr, indices, updates, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_scatter: " + std::string(e.what()));
    }
}

// mlx_scatter_add - Scatter and add updates at given indices
// Signature: deflib mlx_scatter_add(array indices updates axis)
Element* Lispe_mlx_methods::method_scatter_add(LispE* lisp) {
    Element* arr_elem = lisp->get_variable("array");
    Element* indices_elem = lisp->get_variable("indices");
    Element* updates_elem = lisp->get_variable("updates");
    Element* axis_elem = lisp->get_variable("axis");

    try {
        mx::array arr = element_to_array(lisp, arr_elem);
        mx::array indices = element_to_array(lisp, indices_elem);
        mx::array updates = element_to_array(lisp, updates_elem);
        int axis = axis_elem->asInteger();
        
        mx::array result = mx::scatter_add(arr, indices, updates, axis);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_scatter_add: " + std::string(e.what()));
    }
}

// mlx_slice_update - Update a slice of the source array
// Signature: deflib mlx_slice_update(src update start stop (strides))
Element* Lispe_mlx_methods::method_slice_update(LispE* lisp) {
    Element* src_elem = lisp->get_variable("src");
    Element* update_elem = lisp->get_variable("update");
    Element* start_elem = lisp->get_variable("start");
    Element* stop_elem = lisp->get_variable("stop");
    Element* strides_elem = lisp->get_variable("strides");

    try {
        mx::array src = element_to_array(lisp, src_elem);
        mx::array update = element_to_array(lisp, update_elem);
        
        mx::Shape start_shape;
        element_to_shape(start_elem, start_shape);
        
        mx::Shape stop_shape;
        element_to_shape(stop_elem, stop_shape);
        
        if (strides_elem != null_ && strides_elem->type != v_emptyatom) {
            mx::Shape strides_shape;
            element_to_shape(strides_elem, strides_shape);
            mx::array result = mx::slice_update(src, update, start_shape, stop_shape, strides_shape);
            return new MLXArray(std::move(result));
        } else {
            mx::array result = mx::slice_update(src, update, start_shape, stop_shape);
            return new MLXArray(std::move(result));
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_slice_update: " + std::string(e.what()));
    }
}

// mlx_conjugate - Conjugué complexe d'un array
// Signature: deflib mlx_conjugate(array)
Element* Lispe_mlx_methods::method_conjugate(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::conjugate(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_conjugate: " + std::string(e.what()));
    }
}

// mlx_real - Extrait la partie réelle d'un array complexe
// Signature: deflib mlx_real(array)
Element* Lispe_mlx_methods::method_real(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::real(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_real: " + std::string(e.what()));
    }
}

// mlx_imag - Extrait la partie imaginaire d'un array complexe
// Signature: deflib mlx_imag(array)
Element* Lispe_mlx_methods::method_imag(LispE* lisp) {
    Element* array_elem = lisp->get_variable("array");

    try {
        mx::array arr = element_to_array(lisp, array_elem);
        mx::array result = mx::imag(arr);
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_imag: " + std::string(e.what()));
    }
}

// ============================================================================
// Groupe 29: Opérations fusionnées pour LLM
// ============================================================================

// mlx_fused_mlp - MLP fusionné : down_proj(gelu(gate_proj(x)) * up_proj(x))
// Signature: deflib mlx_fused_mlp(x gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b (group_size) (bits))
// Cette fonction fusionne les 3 multiplications matricielles quantifiées du MLP en un seul appel C++
// pour permettre à MLX d'optimiser le graphe de calcul et réduire l'overhead de l'interpréteur LispE.
Element* Lispe_mlx_methods::method_fused_mlp(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* gate_w_elem = lisp->get_variable("gate_w");
    Element* gate_s_elem = lisp->get_variable("gate_s");
    Element* gate_b_elem = lisp->get_variable("gate_b");
    Element* up_w_elem = lisp->get_variable("up_w");
    Element* up_s_elem = lisp->get_variable("up_s");
    Element* up_b_elem = lisp->get_variable("up_b");
    Element* down_w_elem = lisp->get_variable("down_w");
    Element* down_s_elem = lisp->get_variable("down_s");
    Element* down_b_elem = lisp->get_variable("down_b");
    Element* group_size_elem = lisp->get_variable("group_size");
    Element* bits_elem = lisp->get_variable("bits");

    try {
        mx::array x = element_to_array(lisp, x_elem);
        
        // Gate projection weights
        mx::array gate_w = element_to_array(lisp, gate_w_elem);
        mx::array gate_s = element_to_array(lisp, gate_s_elem);
        std::optional<mx::array> gate_b = std::nullopt;
        if (gate_b_elem != null_) {
            gate_b = element_to_array(lisp, gate_b_elem);
        }
        
        // Up projection weights
        mx::array up_w = element_to_array(lisp, up_w_elem);
        mx::array up_s = element_to_array(lisp, up_s_elem);
        std::optional<mx::array> up_b = std::nullopt;
        if (up_b_elem != null_) {
            up_b = element_to_array(lisp, up_b_elem);
        }
        
        // Down projection weights
        mx::array down_w = element_to_array(lisp, down_w_elem);
        mx::array down_s = element_to_array(lisp, down_s_elem);
        std::optional<mx::array> down_b = std::nullopt;
        if (down_b_elem != null_) {
            down_b = element_to_array(lisp, down_b_elem);
        }
        
        // Quantization parameters
        int group_size = 64;
        int bits = 4;
        if (group_size_elem != null_) {
            group_size = group_size_elem->asInteger();
        }
        if (bits_elem != null_) {
            bits = bits_elem->asInteger();
        }
        
        // Fused MLP: down_proj(gelu(gate_proj(x)) * up_proj(x))
        // Step 1: gate = gate_proj(x) - quantized matmul
        mx::array gate = mx::quantized_matmul(x, gate_w, gate_s, gate_b, true, group_size, bits);
        
        // Step 2: up = up_proj(x) - quantized matmul
        mx::array up = mx::quantized_matmul(x, up_w, up_s, up_b, true, group_size, bits);
        
        // Step 3: hidden = gelu(gate) * up
        // GELU(x) = 0.5 * x * (1 + erf(x / sqrt(2)))
        const float sqrt2_inv = 0.7071067811865476f;  // 1/sqrt(2)
        mx::array gate_gelu = mx::multiply(
            mx::multiply(mx::array(0.5f), gate),
            mx::add(mx::array(1.0f), mx::erf(mx::multiply(gate, mx::array(sqrt2_inv))))
        );
        mx::array hidden = mx::multiply(gate_gelu, up);
        
        // Step 4: output = down_proj(hidden) - quantized matmul
        mx::array result = mx::quantized_matmul(hidden, down_w, down_s, down_b, true, group_size, bits);
        
        // Ne pas appeler mx::eval ici pour permettre à MLX d'optimiser le graphe complet
        // L'évaluation sera faite au moment où les valeurs sont réellement nécessaires
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fused_mlp: " + std::string(e.what()));
    }
}

// ============================================================================
// mlx_fused_moe - MoE fusionné pour GPT-OSS et modèles similaires
// ============================================================================
// Signature: deflib mlx_fused_moe(x expert_indices expert_weights
//                                  gate_w gate_s gate_qb gate_lb
//                                  up_w up_s up_qb up_lb
//                                  down_w down_s down_qb down_lb
//                                  num_experts experts_per_tok (group_size) (bits) (activation))
//
// Cette fonction fusionne tout le calcul MoE en C++ pour éviter les boucles LispE
// et permettre à MLX d'optimiser le graphe de calcul complet.
//
// Arguments:
//   x: input [B, L, hidden_size]
//   expert_indices: indices des top-k experts [B, L, k] (pré-calculés)
//   expert_weights: poids des experts [B, L, k] (pré-calculés, normalisés)
//   gate_w, gate_s, gate_qb, gate_lb: poids gate_proj (weight, scales, quant_biases, linear_bias)
//   up_w, up_s, up_qb, up_lb: poids up_proj
//   down_w, down_s, down_qb, down_lb: poids down_proj
//   num_experts: nombre total d'experts
//   experts_per_tok: nombre d'experts par token (k)
//   group_size: taille de groupe pour la quantification (default: 64)
//   bits: bits de quantification (default: 8)
//   activation: "swiglu" ou "gelu" (default: "swiglu")
//
// Retourne: output [B, L, hidden_size]
Element* Lispe_mlx_methods::method_fused_moe(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* expert_indices_elem = lisp->get_variable("expert_indices");
    Element* expert_weights_elem = lisp->get_variable("expert_weights");
    // Gate projection
    Element* gate_w_elem = lisp->get_variable("gate_w");
    Element* gate_s_elem = lisp->get_variable("gate_s");
    Element* gate_qb_elem = lisp->get_variable("gate_qb");
    Element* gate_lb_elem = lisp->get_variable("gate_lb");
    // Up projection
    Element* up_w_elem = lisp->get_variable("up_w");
    Element* up_s_elem = lisp->get_variable("up_s");
    Element* up_qb_elem = lisp->get_variable("up_qb");
    Element* up_lb_elem = lisp->get_variable("up_lb");
    // Down projection
    Element* down_w_elem = lisp->get_variable("down_w");
    Element* down_s_elem = lisp->get_variable("down_s");
    Element* down_qb_elem = lisp->get_variable("down_qb");
    Element* down_lb_elem = lisp->get_variable("down_lb");
    // Parameters
    Element* num_experts_elem = lisp->get_variable("num_experts");
    Element* experts_per_tok_elem = lisp->get_variable("experts_per_tok");
    Element* group_size_elem = lisp->get_variable("group_size");
    Element* bits_elem = lisp->get_variable("bits");
    Element* activation_elem = lisp->get_variable("activation");

    try {
        mx::array x = element_to_array(lisp, x_elem);
        mx::array expert_indices = element_to_array(lisp, expert_indices_elem);
        mx::array expert_weights = element_to_array(lisp, expert_weights_elem);
        
        // Gate projection weights
        mx::array gate_w = element_to_array(lisp, gate_w_elem);
        mx::array gate_s = element_to_array(lisp, gate_s_elem);
        std::optional<mx::array> gate_qb = std::nullopt;
        if (gate_qb_elem != null_) {
            gate_qb = element_to_array(lisp, gate_qb_elem);
        }
        std::optional<mx::array> gate_lb = std::nullopt;
        if (gate_lb_elem != null_) {
            gate_lb = element_to_array(lisp, gate_lb_elem);
        }
        
        // Up projection weights
        mx::array up_w = element_to_array(lisp, up_w_elem);
        mx::array up_s = element_to_array(lisp, up_s_elem);
        std::optional<mx::array> up_qb = std::nullopt;
        if (up_qb_elem != null_) {
            up_qb = element_to_array(lisp, up_qb_elem);
        }
        std::optional<mx::array> up_lb = std::nullopt;
        if (up_lb_elem != null_) {
            up_lb = element_to_array(lisp, up_lb_elem);
        }
        
        // Down projection weights
        mx::array down_w = element_to_array(lisp, down_w_elem);
        mx::array down_s = element_to_array(lisp, down_s_elem);
        std::optional<mx::array> down_qb = std::nullopt;
        if (down_qb_elem != null_) {
            down_qb = element_to_array(lisp, down_qb_elem);
        }
        std::optional<mx::array> down_lb = std::nullopt;
        if (down_lb_elem != null_) {
            down_lb = element_to_array(lisp, down_lb_elem);
        }
        
        int num_experts = num_experts_elem->asInteger();
        int experts_per_tok = experts_per_tok_elem->asInteger();
        
        int group_size = 64;
        int bits = 8;
        if (group_size_elem != null_) {
            group_size = group_size_elem->asInteger();
        }
        if (bits_elem != null_) {
            bits = bits_elem->asInteger();
        }
        
        std::string activation = "swiglu";
        if (activation_elem != null_) {
            wstring ws = activation_elem->asString(lisp);
            activation = std::string(ws.begin(), ws.end());
        }
        
        // Obtenir les dimensions
        auto x_shape = x.shape();
        int B = x_shape[0];
        int L = x_shape[1];
        int hidden_size = x_shape[2];
        
        // Cette fonction optimisée ne supporte que L=1 (génération token par token)
        // Pour le prefill (L>1), utiliser l'implémentation LispE
        if (L != 1) {
            throw new Error("mlx_fused_moe: Only L=1 supported. Use LispE implementation for prefill (L>1).");
        }
        
        // Initialiser output à zéros [B, L, hidden_size]
        mx::array output = mx::zeros({B, L, hidden_size}, mx::float32);
        
        // Pour L=1 (génération token par token), on peut utiliser les indices MLX directement
        // sans forcer l'évaluation. On boucle seulement sur k (4 experts actifs).
        
        for (int k = 0; k < experts_per_tok; k++) {
            // Extraire l'indice d'expert pour ce slot: [B, L] -> [B]
            mx::array slot_indices = mx::squeeze(mx::slice(expert_indices, 
                {0, 0, k}, {B, L, k + 1}), 2);
            
            // Extraire les poids pour ce slot: [B, L, 1]
            mx::array slot_weights = mx::slice(expert_weights, 
                {0, 0, k}, {B, L, k + 1});
            
            // Pour B=1, L=1: slot_indices est [1], on peut l'utiliser directement avec take
            mx::array flat_indices = mx::reshape(slot_indices, {B});
            
            // Extraire les poids de l'expert sélectionné
            mx::array e_gate_w = mx::squeeze(mx::take(gate_w, flat_indices, 0), 0);
            mx::array e_gate_s = mx::squeeze(mx::take(gate_s, flat_indices, 0), 0);
            std::optional<mx::array> e_gate_qb = std::nullopt;
            if (gate_qb.has_value()) {
                e_gate_qb = mx::squeeze(mx::take(*gate_qb, flat_indices, 0), 0);
            }
            std::optional<mx::array> e_gate_lb = std::nullopt;
            if (gate_lb.has_value()) {
                e_gate_lb = mx::squeeze(mx::take(*gate_lb, flat_indices, 0), 0);
            }
            
            mx::array e_up_w = mx::squeeze(mx::take(up_w, flat_indices, 0), 0);
            mx::array e_up_s = mx::squeeze(mx::take(up_s, flat_indices, 0), 0);
            std::optional<mx::array> e_up_qb = std::nullopt;
            if (up_qb.has_value()) {
                e_up_qb = mx::squeeze(mx::take(*up_qb, flat_indices, 0), 0);
            }
            std::optional<mx::array> e_up_lb = std::nullopt;
            if (up_lb.has_value()) {
                e_up_lb = mx::squeeze(mx::take(*up_lb, flat_indices, 0), 0);
            }
            
            mx::array e_down_w = mx::squeeze(mx::take(down_w, flat_indices, 0), 0);
            mx::array e_down_s = mx::squeeze(mx::take(down_s, flat_indices, 0), 0);
            std::optional<mx::array> e_down_qb = std::nullopt;
            if (down_qb.has_value()) {
                e_down_qb = mx::squeeze(mx::take(*down_qb, flat_indices, 0), 0);
            }
            std::optional<mx::array> e_down_lb = std::nullopt;
            if (down_lb.has_value()) {
                e_down_lb = mx::squeeze(mx::take(*down_lb, flat_indices, 0), 0);
            }
            
            // Forward: gate = gate_proj(x)
            mx::array gate_out = mx::quantized_matmul(x, e_gate_w, e_gate_s, e_gate_qb, true, group_size, bits);
            if (e_gate_lb.has_value()) {
                gate_out = mx::add(gate_out, *e_gate_lb);
            }
            
            // up = up_proj(x)
            mx::array up_out = mx::quantized_matmul(x, e_up_w, e_up_s, e_up_qb, true, group_size, bits);
            if (e_up_lb.has_value()) {
                up_out = mx::add(up_out, *e_up_lb);
            }
            
            // Activation
            mx::array hidden = [&]() -> mx::array {
                if (activation == "swiglu") {
                    const float alpha = 1.702f;
                    const float limit = 7.0f;
                    
                    mx::array gate_clamped = mx::clip(gate_out, mx::array(-limit), mx::array(limit));
                    mx::array up_clamped = mx::clip(up_out, mx::array(-limit), mx::array(limit));
                    
                    mx::array glu_scaled = mx::multiply(mx::array(alpha), gate_clamped);
                    mx::array sig = mx::sigmoid(glu_scaled);
                    mx::array out_glu = mx::multiply(gate_clamped, sig);
                    
                    return mx::multiply(out_glu, mx::add(up_clamped, mx::array(1.0f)));
                } else {
                    const float sqrt2_inv = 0.7071067811865476f;
                    mx::array gate_gelu = mx::multiply(
                        mx::multiply(mx::array(0.5f), gate_out),
                        mx::add(mx::array(1.0f), mx::erf(mx::multiply(gate_out, mx::array(sqrt2_inv))))
                    );
                    return mx::multiply(gate_gelu, up_out);
                }
            }();
            
            // down = down_proj(hidden)
            mx::array expert_out = mx::quantized_matmul(hidden, e_down_w, e_down_s, e_down_qb, true, group_size, bits);
            if (e_down_lb.has_value()) {
                expert_out = mx::add(expert_out, *e_down_lb);
            }
            
            // Pondérer par le poids du slot et accumuler
            mx::array weighted_out = mx::multiply(expert_out, slot_weights);
            output = mx::add(output, weighted_out);
        }
        
        return new MLXArray(std::move(output));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fused_moe: " + std::string(e.what()));
    }
}

// ============================================================================
// mlx_fused_moe_batch - Version batch du MoE (déquantifie puis matmul batché)
// ============================================================================
// Même signature que mlx_fused_moe mais utilise une approche batch:
// - Extrait les k experts d'un coup avec take
// - Déquantifie en batch
// - Matmul batché avec broadcasting
// - Somme pondérée finale
//
// Avantage potentiel: moins d'opérations séparées dans le graphe MLX
// Inconvénient: déquantification explicite (mais seulement 4 experts, pas 32)
Element* Lispe_mlx_methods::method_fused_moe_batch(LispE* lisp) {
    Element* x_elem = lisp->get_variable("x");
    Element* expert_indices_elem = lisp->get_variable("expert_indices");
    Element* expert_weights_elem = lisp->get_variable("expert_weights");
    // Gate projection
    Element* gate_w_elem = lisp->get_variable("gate_w");
    Element* gate_s_elem = lisp->get_variable("gate_s");
    Element* gate_qb_elem = lisp->get_variable("gate_qb");
    Element* gate_lb_elem = lisp->get_variable("gate_lb");
    // Up projection
    Element* up_w_elem = lisp->get_variable("up_w");
    Element* up_s_elem = lisp->get_variable("up_s");
    Element* up_qb_elem = lisp->get_variable("up_qb");
    Element* up_lb_elem = lisp->get_variable("up_lb");
    // Down projection
    Element* down_w_elem = lisp->get_variable("down_w");
    Element* down_s_elem = lisp->get_variable("down_s");
    Element* down_qb_elem = lisp->get_variable("down_qb");
    Element* down_lb_elem = lisp->get_variable("down_lb");
    // Parameters
    Element* num_experts_elem = lisp->get_variable("num_experts");
    Element* experts_per_tok_elem = lisp->get_variable("experts_per_tok");
    Element* group_size_elem = lisp->get_variable("group_size");
    Element* bits_elem = lisp->get_variable("bits");
    Element* activation_elem = lisp->get_variable("activation");

    try {
        mx::array x = element_to_array(lisp, x_elem);
        mx::array expert_indices = element_to_array(lisp, expert_indices_elem);
        mx::array expert_weights = element_to_array(lisp, expert_weights_elem);
        
        // Gate projection weights [num_experts, out_features, in_features_packed]
        mx::array gate_w = element_to_array(lisp, gate_w_elem);
        mx::array gate_s = element_to_array(lisp, gate_s_elem);
        std::optional<mx::array> gate_qb = std::nullopt;
        if (gate_qb_elem != null_) {
            gate_qb = element_to_array(lisp, gate_qb_elem);
        }
        std::optional<mx::array> gate_lb = std::nullopt;
        if (gate_lb_elem != null_) {
            gate_lb = element_to_array(lisp, gate_lb_elem);
        }
        
        // Up projection weights
        mx::array up_w = element_to_array(lisp, up_w_elem);
        mx::array up_s = element_to_array(lisp, up_s_elem);
        std::optional<mx::array> up_qb = std::nullopt;
        if (up_qb_elem != null_) {
            up_qb = element_to_array(lisp, up_qb_elem);
        }
        std::optional<mx::array> up_lb = std::nullopt;
        if (up_lb_elem != null_) {
            up_lb = element_to_array(lisp, up_lb_elem);
        }
        
        // Down projection weights
        mx::array down_w = element_to_array(lisp, down_w_elem);
        mx::array down_s = element_to_array(lisp, down_s_elem);
        std::optional<mx::array> down_qb = std::nullopt;
        if (down_qb_elem != null_) {
            down_qb = element_to_array(lisp, down_qb_elem);
        }
        std::optional<mx::array> down_lb = std::nullopt;
        if (down_lb_elem != null_) {
            down_lb = element_to_array(lisp, down_lb_elem);
        }
        
        int num_experts = num_experts_elem->asInteger();
        int experts_per_tok = experts_per_tok_elem->asInteger();
        
        int group_size = 64;
        int bits = 8;
        if (group_size_elem != null_) {
            group_size = group_size_elem->asInteger();
        }
        if (bits_elem != null_) {
            bits = bits_elem->asInteger();
        }
        
        std::string activation = "swiglu";
        if (activation_elem != null_) {
            wstring ws = activation_elem->asString(lisp);
            activation = std::string(ws.begin(), ws.end());
        }
        
        // Obtenir les dimensions
        auto x_shape = x.shape();
        int B = x_shape[0];
        int L = x_shape[1];
        int hidden_size = x_shape[2];
        
        // Cette fonction optimisée ne supporte que L=1, B=1 (génération token par token)
        if (L != 1 || B != 1) {
            throw new Error("mlx_fused_moe_batch: Only B=1, L=1 supported. Use LispE implementation for prefill.");
        }
        
        int k = experts_per_tok;  // nombre d'experts actifs (généralement 4)
        
        // expert_indices: [1, 1, k] -> flat_indices: [k]
        mx::array flat_indices = mx::reshape(expert_indices, {k});
        
        // expert_weights: [1, 1, k] -> weights: [k, 1, 1] pour broadcasting
        mx::array weights = mx::reshape(expert_weights, {k, 1, 1});
        
        // === Extraire les k experts en batch ===
        // gate_w: [num_experts, out, in_packed] -> batch_gate_w: [k, out, in_packed]
        mx::array batch_gate_w = mx::take(gate_w, flat_indices, 0);
        mx::array batch_gate_s = mx::take(gate_s, flat_indices, 0);
        std::optional<mx::array> batch_gate_qb = std::nullopt;
        if (gate_qb.has_value()) {
            batch_gate_qb = mx::take(*gate_qb, flat_indices, 0);
        }
        std::optional<mx::array> batch_gate_lb = std::nullopt;
        if (gate_lb.has_value()) {
            batch_gate_lb = mx::take(*gate_lb, flat_indices, 0);
        }
        
        mx::array batch_up_w = mx::take(up_w, flat_indices, 0);
        mx::array batch_up_s = mx::take(up_s, flat_indices, 0);
        std::optional<mx::array> batch_up_qb = std::nullopt;
        if (up_qb.has_value()) {
            batch_up_qb = mx::take(*up_qb, flat_indices, 0);
        }
        std::optional<mx::array> batch_up_lb = std::nullopt;
        if (up_lb.has_value()) {
            batch_up_lb = mx::take(*up_lb, flat_indices, 0);
        }
        
        mx::array batch_down_w = mx::take(down_w, flat_indices, 0);
        mx::array batch_down_s = mx::take(down_s, flat_indices, 0);
        std::optional<mx::array> batch_down_qb = std::nullopt;
        if (down_qb.has_value()) {
            batch_down_qb = mx::take(*down_qb, flat_indices, 0);
        }
        std::optional<mx::array> batch_down_lb = std::nullopt;
        if (down_lb.has_value()) {
            batch_down_lb = mx::take(*down_lb, flat_indices, 0);
        }
        
        // === Déquantifier en batch ===
        // batch_gate_w: [k, out, in_packed] -> batch_gate_full: [k, out, in]
        mx::array batch_gate_full = mx::dequantize(batch_gate_w, batch_gate_s, batch_gate_qb, group_size, bits);
        mx::array batch_up_full = mx::dequantize(batch_up_w, batch_up_s, batch_up_qb, group_size, bits);
        mx::array batch_down_full = mx::dequantize(batch_down_w, batch_down_s, batch_down_qb, group_size, bits);
        
        // === Préparer x pour le batch ===
        // x: [1, 1, hidden] -> x_batch: [k, 1, hidden]
        mx::array x_squeezed = mx::squeeze(x, 0);  // [1, hidden]
        mx::array x_batch = mx::broadcast_to(x_squeezed, {k, 1, hidden_size});
        
        // === Matmul batché ===
        // gate_proj: x_batch [k, 1, hidden] @ batch_gate_full^T [k, hidden, out] -> [k, 1, intermediate]
        // Avec transpose=true sur la dernière dim de batch_gate_full: [k, out, in] -> [k, in, out]
        mx::array batch_gate_t = mx::transpose(batch_gate_full, {0, 2, 1});  // [k, in, out]
        mx::array gate_out = mx::matmul(x_batch, batch_gate_t);  // [k, 1, intermediate]
        if (batch_gate_lb.has_value()) {
            // batch_gate_lb: [k, out] -> [k, 1, out]
            mx::array lb_expanded = mx::expand_dims(*batch_gate_lb, 1);
            gate_out = mx::add(gate_out, lb_expanded);
        }
        
        // up_proj
        mx::array batch_up_t = mx::transpose(batch_up_full, {0, 2, 1});
        mx::array up_out = mx::matmul(x_batch, batch_up_t);  // [k, 1, intermediate]
        if (batch_up_lb.has_value()) {
            mx::array lb_expanded = mx::expand_dims(*batch_up_lb, 1);
            up_out = mx::add(up_out, lb_expanded);
        }
        
        // === Activation (swiglu ou gelu) ===
        mx::array hidden = [&]() -> mx::array {
            if (activation == "swiglu") {
                const float alpha = 1.702f;
                const float limit = 7.0f;
                
                mx::array gate_clamped = mx::clip(gate_out, mx::array(-limit), mx::array(limit));
                mx::array up_clamped = mx::clip(up_out, mx::array(-limit), mx::array(limit));
                
                mx::array glu_scaled = mx::multiply(mx::array(alpha), gate_clamped);
                mx::array sig = mx::sigmoid(glu_scaled);
                mx::array out_glu = mx::multiply(gate_clamped, sig);
                
                return mx::multiply(out_glu, mx::add(up_clamped, mx::array(1.0f)));
            } else {
                // GELU
                const float sqrt2_inv = 0.7071067811865476f;
                mx::array gate_gelu = mx::multiply(
                    mx::multiply(mx::array(0.5f), gate_out),
                    mx::add(mx::array(1.0f), mx::erf(mx::multiply(gate_out, mx::array(sqrt2_inv))))
                );
                return mx::multiply(gate_gelu, up_out);
            }
        }();
        
        // down_proj: hidden [k, 1, intermediate] @ batch_down_full^T [k, intermediate, hidden] -> [k, 1, hidden]
        mx::array batch_down_t = mx::transpose(batch_down_full, {0, 2, 1});
        mx::array expert_out = mx::matmul(hidden, batch_down_t);  // [k, 1, hidden]
        if (batch_down_lb.has_value()) {
            mx::array lb_expanded = mx::expand_dims(*batch_down_lb, 1);
            expert_out = mx::add(expert_out, lb_expanded);
        }
        
        // === Pondération et somme ===
        // expert_out: [k, 1, hidden], weights: [k, 1, 1]
        mx::array weighted = mx::multiply(expert_out, weights);  // [k, 1, hidden]
        
        // Somme sur la dimension k: [k, 1, hidden] -> [1, hidden]
        mx::array summed = mx::sum(weighted, 0, false);  // [1, hidden]
        
        // Reshape en [B, L, hidden] = [1, 1, hidden]
        mx::array output = mx::reshape(summed, {1, 1, hidden_size});
        
        return new MLXArray(std::move(output));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_fused_moe_batch: " + std::string(e.what()));
    }
}

// ============================================================================
// mlx_eval: Force l'évaluation d'un tableau MLX
// ============================================================================
Element* Lispe_mlx_methods::method_eval(LispE* lisp) {
    try {
        Element* array_elem = lisp->get_variable(U"array");
        if (array_elem->type != t_mlx_array) {
            throw new Error("mlx_eval: Expected MLX array");
        }
        
        MLXArray* mlx_arr = static_cast<MLXArray*>(array_elem);
        mx::array result = mlx_arr->array;
        mx::eval(result);
        
        return new MLXArray(std::move(result));
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_eval: " + std::string(e.what()));
    }
}

// ============================================================================
// mlx_tolist: Convertit un tableau MLX en liste LispE (floats, integers, numbers)
// ============================================================================
Element* Lispe_mlx_methods::method_tolist(LispE* lisp) {
    try {
        Element* array_elem = lisp->get_variable(U"array");
        if (array_elem->type != t_mlx_array) {
            throw new Error("mlx_tolist: Expected MLX array");
        }
        
        MLXArray* mlx_arr = static_cast<MLXArray*>(array_elem);
        mx::array& arr = mlx_arr->array;
        
        // Force l'évaluation pour accéder aux données
        mx::eval(arr);
        
        long sz = arr.size();
        
        switch (arr.dtype()) {
            case mx::float32: {
                // Retourner un Floats (float)
                Floats* result = lisp->provideFloats();
                const float* data = arr.data<float>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(data[i]);
                }
                return result;
            }
            case mx::float64: {
                // Retourner un Numbers (double)
                Numbers* result = lisp->provideNumbers();
                const double* data = arr.data<double>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(data[i]);
                }
                return result;
            }
            case mx::int32: {
                // Retourner un Integers (long)
                Integers* result = lisp->provideIntegers();
                const int32_t* data = arr.data<int32_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::int64: {
                // Retourner un Integers (long)
                Integers* result = lisp->provideIntegers();
                const int64_t* data = arr.data<int64_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::uint32: {
                Integers* result = lisp->provideIntegers();
                const uint32_t* data = arr.data<uint32_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::uint64: {
                Integers* result = lisp->provideIntegers();
                const uint64_t* data = arr.data<uint64_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::int16: {
                Integers* result = lisp->provideIntegers();
                const int16_t* data = arr.data<int16_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::uint16: {
                Integers* result = lisp->provideIntegers();
                const uint16_t* data = arr.data<uint16_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::int8: {
                Integers* result = lisp->provideIntegers();
                const int8_t* data = arr.data<int8_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::uint8: {
                Integers* result = lisp->provideIntegers();
                const uint8_t* data = arr.data<uint8_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<long>(data[i]));
                }
                return result;
            }
            case mx::bfloat16: {
                // bfloat16 -> float
                Floats* result = lisp->provideFloats();
                const mx::bfloat16_t* data = arr.data<mx::bfloat16_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<float>(data[i]));
                }
                return result;
            }
            case mx::float16: {
                // float16 -> float
                Floats* result = lisp->provideFloats();
                const mx::float16_t* data = arr.data<mx::float16_t>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(static_cast<float>(data[i]));
                }
                return result;
            }
            case mx::bool_: {
                // bool -> integers (0 ou 1)
                Integers* result = lisp->provideIntegers();
                const bool* data = arr.data<bool>();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(data[i] ? 1 : 0);
                }
                return result;
            }
            default: {
                // Par défaut, essayer de convertir en floats
                Floats* result = lisp->provideFloats();
                for (long i = 0; i < sz; i++) {
                    result->liste.push_back(0.0f);
                }
                return result;
            }
        }
    } catch (const std::exception& e) {
        throw new Error("Error in mlx_tolist: " + std::string(e.what()));
    }
}

// ============================================================================
// Fonction d'initialisation du module
// ============================================================================

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    // Allouer le type pour MLX arrays
    std::string mlx_array_type_key = "mlx_array";
    t_mlx_array = lisp->encode(mlx_array_type_key);

    // Allouer le type pour MLX shapes
    std::string mlx_shape_type_key = "mlx_shape";
    t_mlx_shape = lisp->encode(mlx_shape_type_key);

    // Enregistrer les méthodes MLX
    lisp->extension("deflib mlx_shape(data)",
                    new Lispe_mlx_methods(mlx_method_shape));

    lisp->extension("deflib mlx_array(data (shape) (dtype))",
                    new Lispe_mlx_methods(mlx_method_array));

    lisp->extension("deflib mlx_reshape(array shape)",
                    new Lispe_mlx_methods(mlx_method_reshape));

    lisp->extension("deflib mlx_take(array indices (axis))",
                    new Lispe_mlx_methods(mlx_method_take));

    lisp->extension("deflib mlx_mean(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_mean));

    lisp->extension("deflib mlx_add(a b)",
                    new Lispe_mlx_methods(mlx_method_add));

    lisp->extension("deflib mlx_multiply(a b)",
                    new Lispe_mlx_methods(mlx_method_multiply));

    lisp->extension("deflib mlx_sum(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_sum));

    lisp->extension("deflib mlx_transpose(array (axes))",
                    new Lispe_mlx_methods(mlx_method_transpose));

    lisp->extension("deflib mlx_matmul(a b)",
                    new Lispe_mlx_methods(mlx_method_matmul));

    lisp->extension("deflib mlx_subtract(a b)",
                    new Lispe_mlx_methods(mlx_method_subtract));

    lisp->extension("deflib mlx_divide(a b)",
                    new Lispe_mlx_methods(mlx_method_divide));

    lisp->extension("deflib mlx_maximum(a b)",
                    new Lispe_mlx_methods(mlx_method_maximum));

    lisp->extension("deflib mlx_minimum(a b)",
                    new Lispe_mlx_methods(mlx_method_minimum));

    lisp->extension("deflib mlx_sqrt(array)",
                    new Lispe_mlx_methods(mlx_method_sqrt));

    lisp->extension("deflib mlx_exp(array)",
                    new Lispe_mlx_methods(mlx_method_exp));

    lisp->extension("deflib mlx_log(array)",
                    new Lispe_mlx_methods(mlx_method_log));

    lisp->extension("deflib mlx_abs(array)",
                    new Lispe_mlx_methods(mlx_method_abs));

    lisp->extension("deflib mlx_concatenate(arrays (axis))",
                    new Lispe_mlx_methods(mlx_method_concatenate));

    lisp->extension("deflib mlx_split(array indices_or_sections (axis))",
                    new Lispe_mlx_methods(mlx_method_split));

    // Fonctions d'activation
    lisp->extension("deflib mlx_sigmoid(array)",
                    new Lispe_mlx_methods(mlx_method_sigmoid));

    lisp->extension("deflib mlx_relu(array)",
                    new Lispe_mlx_methods(mlx_method_relu));

    lisp->extension("deflib mlx_softmax(array (axis))",
                    new Lispe_mlx_methods(mlx_method_softmax));

    lisp->extension("deflib mlx_tanh(array)",
                    new Lispe_mlx_methods(mlx_method_tanh));

    // Fonctions trigonométriques
    lisp->extension("deflib mlx_sin(array)",
                    new Lispe_mlx_methods(mlx_method_sin));

    lisp->extension("deflib mlx_cos(array)",
                    new Lispe_mlx_methods(mlx_method_cos));

    lisp->extension("deflib mlx_tan(array)",
                    new Lispe_mlx_methods(mlx_method_tan));

    // Fonctions de réduction avancées
    lisp->extension("deflib mlx_argmax(array (axis) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_argmax));

    lisp->extension("deflib mlx_argmin(array (axis) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_argmin));

    // Fonctions supplémentaires
    lisp->extension("deflib mlx_power(a b)",
                    new Lispe_mlx_methods(mlx_method_power));

    lisp->extension("deflib mlx_negative(array)",
                    new Lispe_mlx_methods(mlx_method_negative));

    lisp->extension("deflib mlx_square(array)",
                    new Lispe_mlx_methods(mlx_method_square));

    // Groupe 1: Création d'arrays
    lisp->extension("deflib mlx_zeros(shape (dtype))",
                    new Lispe_mlx_methods(mlx_method_zeros));

    lisp->extension("deflib mlx_ones(shape (dtype))",
                    new Lispe_mlx_methods(mlx_method_ones));

    lisp->extension("deflib mlx_full(shape value (dtype))",
                    new Lispe_mlx_methods(mlx_method_full));

    lisp->extension("deflib mlx_arange(start stop (step) (dtype))",
                    new Lispe_mlx_methods(mlx_method_arange));

    lisp->extension("deflib mlx_linspace(start stop num (dtype))",
                    new Lispe_mlx_methods(mlx_method_linspace));

    lisp->extension("deflib mlx_eye(n (m) (dtype))",
                    new Lispe_mlx_methods(mlx_method_eye));

    // Groupe 2: Nombres aléatoires
    lisp->extension("deflib mlx_random_uniform(shape (low) (high) (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_uniform));

    lisp->extension("deflib mlx_random_normal(shape (mean) (std) (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_normal));

    lisp->extension("deflib mlx_random_randint(low high shape (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_randint));

    // Groupe 3: Mathématiques
    lisp->extension("deflib mlx_log2(array)",
                    new Lispe_mlx_methods(mlx_method_log2));

    lisp->extension("deflib mlx_log10(array)",
                    new Lispe_mlx_methods(mlx_method_log10));

    lisp->extension("deflib mlx_log1p(array)",
                    new Lispe_mlx_methods(mlx_method_log1p));

    lisp->extension("deflib mlx_floor(array)",
                    new Lispe_mlx_methods(mlx_method_floor));

    lisp->extension("deflib mlx_ceil(array)",
                    new Lispe_mlx_methods(mlx_method_ceil));

    lisp->extension("deflib mlx_round(array)",
                    new Lispe_mlx_methods(mlx_method_round));

    lisp->extension("deflib mlx_clip(array min max)",
                    new Lispe_mlx_methods(mlx_method_clip));

    lisp->extension("deflib mlx_reciprocal(array)",
                    new Lispe_mlx_methods(mlx_method_reciprocal));

    lisp->extension("deflib mlx_rsqrt(array)",
                    new Lispe_mlx_methods(mlx_method_rsqrt));

    // Groupe 4: Trigonométrie inverse
    lisp->extension("deflib mlx_arcsin(array)",
                    new Lispe_mlx_methods(mlx_method_arcsin));

    lisp->extension("deflib mlx_arccos(array)",
                    new Lispe_mlx_methods(mlx_method_arccos));

    lisp->extension("deflib mlx_arctan(array)",
                    new Lispe_mlx_methods(mlx_method_arctan));

    lisp->extension("deflib mlx_sinh(array)",
                    new Lispe_mlx_methods(mlx_method_sinh));

    lisp->extension("deflib mlx_cosh(array)",
                    new Lispe_mlx_methods(mlx_method_cosh));

    // Groupe 5: Réductions
    lisp->extension("deflib mlx_prod(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_prod));

    lisp->extension("deflib mlx_max(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_max));

    lisp->extension("deflib mlx_min(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_min));

    lisp->extension("deflib mlx_var(array (axes) (keepdims) (ddof))",
                    new Lispe_mlx_methods(mlx_method_var));

    lisp->extension("deflib mlx_std(array (axes) (keepdims) (ddof))",
                    new Lispe_mlx_methods(mlx_method_std));

    lisp->extension("deflib mlx_all(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_all));

    lisp->extension("deflib mlx_any(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_any));

    // Groupe 6: Comparaisons
    lisp->extension("deflib mlx_equal(a b)",
                    new Lispe_mlx_methods(mlx_method_equal));

    lisp->extension("deflib mlx_not_equal(a b)",
                    new Lispe_mlx_methods(mlx_method_not_equal));

    lisp->extension("deflib mlx_greater(a b)",
                    new Lispe_mlx_methods(mlx_method_greater));

    lisp->extension("deflib mlx_less(a b)",
                    new Lispe_mlx_methods(mlx_method_less));

    lisp->extension("deflib mlx_greater_equal(a b)",
                    new Lispe_mlx_methods(mlx_method_greater_equal));

    lisp->extension("deflib mlx_less_equal(a b)",
                    new Lispe_mlx_methods(mlx_method_less_equal));

    lisp->extension("deflib mlx_where(condition x y)",
                    new Lispe_mlx_methods(mlx_method_where));

    // Groupe 7: Manipulation
    lisp->extension("deflib mlx_flatten(array (start_axis) (end_axis))",
                    new Lispe_mlx_methods(mlx_method_flatten));

    lisp->extension("deflib mlx_squeeze(array (axes))",
                    new Lispe_mlx_methods(mlx_method_squeeze));

    lisp->extension("deflib mlx_expand_dims(array axis)",
                    new Lispe_mlx_methods(mlx_method_expand_dims));

    lisp->extension("deflib mlx_stack(arrays (axis))",
                    new Lispe_mlx_methods(mlx_method_stack));

    lisp->extension("deflib mlx_tile(array reps)",
                    new Lispe_mlx_methods(mlx_method_tile));

    lisp->extension("deflib mlx_repeat(array repeats (axis))",
                    new Lispe_mlx_methods(mlx_method_repeat));

    lisp->extension("deflib mlx_pad(array pad_width (value))",
                    new Lispe_mlx_methods(mlx_method_pad));

    // Groupe 8: Activation deep learning
    lisp->extension("deflib mlx_gelu(array)",
                    new Lispe_mlx_methods(mlx_method_gelu));

    lisp->extension("deflib mlx_gelu_tanh(array)",
                    new Lispe_mlx_methods(mlx_method_gelu_tanh));

    lisp->extension("deflib mlx_silu(array)",
                    new Lispe_mlx_methods(mlx_method_silu));

    lisp->extension("deflib mlx_leaky_relu(array (negative_slope))",
                    new Lispe_mlx_methods(mlx_method_leaky_relu));

    lisp->extension("deflib mlx_elu(array (alpha))",
                    new Lispe_mlx_methods(mlx_method_elu));

    lisp->extension("deflib mlx_selu(array)",
                    new Lispe_mlx_methods(mlx_method_selu));

    lisp->extension("deflib mlx_log_softmax(array (axis))",
                    new Lispe_mlx_methods(mlx_method_log_softmax));

    // Groupe 9: Algèbre linéaire
    lisp->extension("deflib mlx_norm(array (ord) (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_norm));

    lisp->extension("deflib mlx_inv(array)",
                    new Lispe_mlx_methods(mlx_method_inv));

    lisp->extension("deflib mlx_svd(array)",
                    new Lispe_mlx_methods(mlx_method_svd));

    lisp->extension("deflib mlx_qr(array)",
                    new Lispe_mlx_methods(mlx_method_qr));

    lisp->extension("deflib mlx_cholesky(array (upper))",
                    new Lispe_mlx_methods(mlx_method_cholesky));

    lisp->extension("deflib mlx_solve(a b)",
                    new Lispe_mlx_methods(mlx_method_solve));

    lisp->extension("deflib mlx_hosvd(tensor ranks)",
                    new Lispe_mlx_methods(mlx_method_hosvd));

    lisp->extension("deflib mlx_tucker(tensor ranks (max_iter) (tol))",
                    new Lispe_mlx_methods(mlx_method_tucker));

    lisp->extension("deflib mlx_tucker_reconstruct(core factors)",
                    new Lispe_mlx_methods(mlx_method_tucker_reconstruct));

    lisp->extension("deflib mlx_tucker_compression_ratio(original_shape core factors)",
                    new Lispe_mlx_methods(mlx_method_tucker_compression_ratio));

    lisp->extension("deflib mlx_khatri_rao_product(a b)",
                    new Lispe_mlx_methods(mlx_method_khatri_rao_product));

    // Groupe 10: Opérations logiques et tests
    lisp->extension("deflib mlx_logical_and(a b)",
                    new Lispe_mlx_methods(mlx_method_logical_and));

    lisp->extension("deflib mlx_logical_or(a b)",
                    new Lispe_mlx_methods(mlx_method_logical_or));

    lisp->extension("deflib mlx_logical_not(array)",
                    new Lispe_mlx_methods(mlx_method_logical_not));

    lisp->extension("deflib mlx_isnan(array)",
                    new Lispe_mlx_methods(mlx_method_isnan));

    lisp->extension("deflib mlx_isinf(array)",
                    new Lispe_mlx_methods(mlx_method_isinf));

    lisp->extension("deflib mlx_isfinite(array)",
                    new Lispe_mlx_methods(mlx_method_isfinite));

    // Groupe 11: Manipulation avancée
    lisp->extension("deflib mlx_flip(array (axes))",
                    new Lispe_mlx_methods(mlx_method_flip));

    lisp->extension("deflib mlx_roll(array shift (axis))",
                    new Lispe_mlx_methods(mlx_method_roll));

    lisp->extension("deflib mlx_sort(array (axis))",
                    new Lispe_mlx_methods(mlx_method_sort));

    lisp->extension("deflib mlx_argsort(array (axis))",
                    new Lispe_mlx_methods(mlx_method_argsort));

    lisp->extension("deflib mlx_argwhere(array)",
                    new Lispe_mlx_methods(mlx_method_argwhere));

    lisp->extension("deflib mlx_moveaxis(array source destination)",
                    new Lispe_mlx_methods(mlx_method_moveaxis));

    lisp->extension("deflib mlx_swapaxes(array axis1 axis2)",
                    new Lispe_mlx_methods(mlx_method_swapaxes));

    lisp->extension("deflib mlx_broadcast_to(array shape)",
                    new Lispe_mlx_methods(mlx_method_broadcast_to));

    // Groupe 12: Création avancée
    lisp->extension("deflib mlx_identity(n (dtype))",
                    new Lispe_mlx_methods(mlx_method_identity));

    lisp->extension("deflib mlx_tri(n (m) (k) (dtype))",
                    new Lispe_mlx_methods(mlx_method_tri));

    lisp->extension("deflib mlx_tril(array (k))",
                    new Lispe_mlx_methods(mlx_method_tril));

    lisp->extension("deflib mlx_triu(array (k))",
                    new Lispe_mlx_methods(mlx_method_triu));

    lisp->extension("deflib mlx_meshgrid(arrays (indexing))",
                    new Lispe_mlx_methods(mlx_method_meshgrid));

    // Groupe 13: Algèbre linéaire avancée
    lisp->extension("deflib mlx_pinv(array)",
                    new Lispe_mlx_methods(mlx_method_pinv));

    lisp->extension("deflib mlx_eig(array)",
                    new Lispe_mlx_methods(mlx_method_eig));

    lisp->extension("deflib mlx_eigvals(array)",
                    new Lispe_mlx_methods(mlx_method_eigvals));

    lisp->extension("deflib mlx_lu(array)",
                    new Lispe_mlx_methods(mlx_method_lu));

    lisp->extension("deflib mlx_cross(a b (axis))",
                    new Lispe_mlx_methods(mlx_method_cross));

    // Groupe 14: I/O de modèles
    lisp->extension("deflib mlx_save(file array)",
                    new Lispe_mlx_methods(mlx_method_save));

    lisp->extension("deflib mlx_load(file)",
                    new Lispe_mlx_methods(mlx_method_load));

    lisp->extension("deflib mlx_load_safetensors(file)",
                    new Lispe_mlx_methods(mlx_method_load_safetensors));

    lisp->extension("deflib mlx_save_safetensors(file arrays (metadata))",
                    new Lispe_mlx_methods(mlx_method_save_safetensors));

    lisp->extension("deflib mlx_load_gguf(file)",
                    new Lispe_mlx_methods(mlx_method_load_gguf));

    lisp->extension("deflib mlx_save_gguf(file arrays (metadata))",
                    new Lispe_mlx_methods(mlx_method_save_gguf));

    // Groupe 15: Quantification et mémoire
    lisp->extension("deflib mlx_quantize(array (group_size) (bits))",
                    new Lispe_mlx_methods(mlx_method_quantize));

    lisp->extension("deflib mlx_dequantize(w scales (biases) (group_size) (bits))",
                    new Lispe_mlx_methods(mlx_method_dequantize));

    lisp->extension("deflib mlx_quantized_matmul(x w scales (biases) (transposed) (group_size) (bits))",
                    new Lispe_mlx_methods(mlx_method_quantized_matmul));

    lisp->extension("deflib mlx_synchronize()",
                    new Lispe_mlx_methods(mlx_method_synchronize));

    lisp->extension("deflib mlx_clear_cache()",
                    new Lispe_mlx_methods(mlx_method_clear_cache));

    lisp->extension("deflib mlx_get_active_memory()",
                    new Lispe_mlx_methods(mlx_method_get_active_memory));

    lisp->extension("deflib mlx_get_peak_memory()",
                    new Lispe_mlx_methods(mlx_method_get_peak_memory));

    lisp->extension("deflib mlx_get_cache_memory()",
                    new Lispe_mlx_methods(mlx_method_get_cache_memory));

    lisp->extension("deflib mlx_set_memory_limit(limit)",
                    new Lispe_mlx_methods(mlx_method_set_memory_limit));

    lisp->extension("deflib mlx_set_cache_limit(limit)",
                    new Lispe_mlx_methods(mlx_method_set_cache_limit));

    // Groupe 16: Opérations supplémentaires
    lisp->extension("deflib mlx_slice(array start stop (strides))",
                    new Lispe_mlx_methods(mlx_method_slice));

    lisp->extension("deflib mlx_trace(array (offset) (axis1) (axis2))",
                    new Lispe_mlx_methods(mlx_method_trace));

    lisp->extension("deflib mlx_diagonal(array (offset) (axis1) (axis2))",
                    new Lispe_mlx_methods(mlx_method_diagonal));

    lisp->extension("deflib mlx_diag(array (k))",
                    new Lispe_mlx_methods(mlx_method_diag));

    lisp->extension("deflib mlx_tensordot(a b (axes))",
                    new Lispe_mlx_methods(mlx_method_tensordot));

    lisp->extension("deflib mlx_inner(a b)",
                    new Lispe_mlx_methods(mlx_method_inner));

    lisp->extension("deflib mlx_outer(a b)",
                    new Lispe_mlx_methods(mlx_method_outer));

    lisp->extension("deflib mlx_cumsum(thearray (theaxis) (thereverse) (inclusive))",
                    new Lispe_mlx_methods(mlx_method_cumsum));

    lisp->extension("deflib mlx_cumprod(thearray (theaxis) (thereverse) (inclusive))",
                    new Lispe_mlx_methods(mlx_method_cumprod));

    lisp->extension("deflib mlx_topk(array k (axis))",
                    new Lispe_mlx_methods(mlx_method_topk));

    lisp->extension("deflib mlx_partition(array kth (axis))",
                    new Lispe_mlx_methods(mlx_method_partition));

    // Groupe 17: FFT (Transformées de Fourier)
    lisp->extension("deflib mlx_fft(array (n) (axis))",
                    new Lispe_mlx_methods(mlx_method_fft));

    lisp->extension("deflib mlx_ifft(array (n) (axis))",
                    new Lispe_mlx_methods(mlx_method_ifft));

    lisp->extension("deflib mlx_fft2(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_fft2));

    lisp->extension("deflib mlx_ifft2(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_ifft2));

    lisp->extension("deflib mlx_fftn(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_fftn));

    lisp->extension("deflib mlx_ifftn(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_ifftn));

    lisp->extension("deflib mlx_rfft(array (n) (axis))",
                    new Lispe_mlx_methods(mlx_method_rfft));

    lisp->extension("deflib mlx_irfft(array (n) (axis))",
                    new Lispe_mlx_methods(mlx_method_irfft));

    lisp->extension("deflib mlx_rfft2(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_rfft2));

    lisp->extension("deflib mlx_irfft2(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_irfft2));

    lisp->extension("deflib mlx_rfftn(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_rfftn));

    lisp->extension("deflib mlx_irfftn(array (s) (axes))",
                    new Lispe_mlx_methods(mlx_method_irfftn));

    lisp->extension("deflib mlx_fftshift(array (axes))",
                    new Lispe_mlx_methods(mlx_method_fftshift));

    lisp->extension("deflib mlx_ifftshift(array (axes))",
                    new Lispe_mlx_methods(mlx_method_ifftshift));

    // Groupe 18: Fonctions mathématiques avancées
    lisp->extension("deflib mlx_arctan2(a b)",
                    new Lispe_mlx_methods(mlx_method_arctan2));

    lisp->extension("deflib mlx_arcsinh(array)",
                    new Lispe_mlx_methods(mlx_method_arcsinh));

    lisp->extension("deflib mlx_arccosh(array)",
                    new Lispe_mlx_methods(mlx_method_arccosh));

    lisp->extension("deflib mlx_arctanh(array)",
                    new Lispe_mlx_methods(mlx_method_arctanh));

    lisp->extension("deflib mlx_degrees(array)",
                    new Lispe_mlx_methods(mlx_method_degrees));

    lisp->extension("deflib mlx_radians(array)",
                    new Lispe_mlx_methods(mlx_method_radians));

    lisp->extension("deflib mlx_erf(array)",
                    new Lispe_mlx_methods(mlx_method_erf));

    lisp->extension("deflib mlx_erfinv(array)",
                    new Lispe_mlx_methods(mlx_method_erfinv));

    lisp->extension("deflib mlx_expm1(array)",
                    new Lispe_mlx_methods(mlx_method_expm1));

    lisp->extension("deflib mlx_logaddexp(a b)",
                    new Lispe_mlx_methods(mlx_method_logaddexp));

    lisp->extension("deflib mlx_sign(array)",
                    new Lispe_mlx_methods(mlx_method_sign));

    lisp->extension("deflib mlx_kron(a b)",
                    new Lispe_mlx_methods(mlx_method_kron));

    // Groupe 19: Opérations bitwise
    lisp->extension("deflib mlx_bitwise_and(a b)",
                    new Lispe_mlx_methods(mlx_method_bitwise_and));

    lisp->extension("deflib mlx_bitwise_or(a b)",
                    new Lispe_mlx_methods(mlx_method_bitwise_or));

    lisp->extension("deflib mlx_bitwise_xor(a b)",
                    new Lispe_mlx_methods(mlx_method_bitwise_xor));

    lisp->extension("deflib mlx_bitwise_invert(array)",
                    new Lispe_mlx_methods(mlx_method_bitwise_invert));

    lisp->extension("deflib mlx_left_shift(a b)",
                    new Lispe_mlx_methods(mlx_method_left_shift));

    lisp->extension("deflib mlx_right_shift(a b)",
                    new Lispe_mlx_methods(mlx_method_right_shift));

    // Groupe 20: Division et reste
    lisp->extension("deflib mlx_divmod(a b)",
                    new Lispe_mlx_methods(mlx_method_divmod));

    lisp->extension("deflib mlx_floor_divide(a b)",
                    new Lispe_mlx_methods(mlx_method_floor_divide));

    lisp->extension("deflib mlx_remainder(a b)",
                    new Lispe_mlx_methods(mlx_method_remainder));

    // Groupe 21: Opérations cumulatives et logsumexp
    lisp->extension("deflib mlx_cummax(array (axis) (thereverse) (inclusive))",
                    new Lispe_mlx_methods(mlx_method_cummax));

    lisp->extension("deflib mlx_cummin(array (axis) (thereverse) (inclusive))",
                    new Lispe_mlx_methods(mlx_method_cummin));

    lisp->extension("deflib mlx_logsumexp(array (axes) (keepdims))",
                    new Lispe_mlx_methods(mlx_method_logsumexp));

    lisp->extension("deflib mlx_logcumsumexp(array (axis) (thereverse) (inclusive))",
                    new Lispe_mlx_methods(mlx_method_logcumsumexp));

    // Groupe 22: Convolutions (deep learning)
    lisp->extension("deflib mlx_conv1d(input weight (stride) (padding) (dilation) (groups))",
                    new Lispe_mlx_methods(mlx_method_conv1d));

    lisp->extension("deflib mlx_conv2d(input weight (stride) (padding) (dilation) (groups))",
                    new Lispe_mlx_methods(mlx_method_conv2d));

    lisp->extension("deflib mlx_conv3d(input weight (stride) (padding) (dilation) (groups))",
                    new Lispe_mlx_methods(mlx_method_conv3d));

    lisp->extension("deflib mlx_conv_transpose1d(input weight (stride) (padding) (dilation) (output_padding) (groups))",
                    new Lispe_mlx_methods(mlx_method_conv_transpose1d));

    lisp->extension("deflib mlx_conv_transpose2d(input weight (stride) (padding) (dilation) (output_padding) (groups))",
                    new Lispe_mlx_methods(mlx_method_conv_transpose2d));

    lisp->extension("deflib mlx_conv_transpose3d(input weight (stride) (padding) (dilation) (output_padding) (groups))",
                    new Lispe_mlx_methods(mlx_method_conv_transpose3d));

    lisp->extension("deflib mlx_conv_general(input weight (stride) (padding_lo) (padding_hi) (kernel_dilation) (input_dilation) (groups) (theflip))",
                    new Lispe_mlx_methods(mlx_method_conv_general));

    // Groupe 23: Opérations spécialisées ML
    lisp->extension("deflib mlx_rms_norm(x (weight) (eps) (gemma_style))",
                    new Lispe_mlx_methods(mlx_method_rms_norm));

    lisp->extension("deflib mlx_layer_norm(x (weight) (bias) (eps))",
                    new Lispe_mlx_methods(mlx_method_layer_norm));

    lisp->extension("deflib mlx_rope(x dims (traditional) (base) (scale) (offset) (freqs))",
                    new Lispe_mlx_methods(mlx_method_rope));

    lisp->extension("deflib mlx_scaled_dot_product_attention(queries keys values scale (mask_mode) (mask_arrays) (sinks))",
                    new Lispe_mlx_methods(mlx_method_scaled_dot_product_attention));

    lisp->extension("deflib mlx_einsum(subscripts operands)",
                    new Lispe_mlx_methods(mlx_method_einsum));

    // Groupe 24: Manipulation et création avancée
    lisp->extension("deflib mlx_dtype(array)",
                    new Lispe_mlx_methods(mlx_method_dtype));

    lisp->extension("deflib mlx_astype(array dtype)",
                    new Lispe_mlx_methods(mlx_method_astype));

    lisp->extension("deflib mlx_copy(array)",
                    new Lispe_mlx_methods(mlx_method_copy));

    lisp->extension("deflib mlx_zeros_like(array)",
                    new Lispe_mlx_methods(mlx_method_zeros_like));

    lisp->extension("deflib mlx_ones_like(array)",
                    new Lispe_mlx_methods(mlx_method_ones_like));

    lisp->extension("deflib mlx_nan_to_num(array (nan) (posinf) (neginf))",
                    new Lispe_mlx_methods(mlx_method_nan_to_num));

    lisp->extension("deflib mlx_atleast_1d(array)",
                    new Lispe_mlx_methods(mlx_method_atleast_1d));

    lisp->extension("deflib mlx_atleast_2d(array)",
                    new Lispe_mlx_methods(mlx_method_atleast_2d));

    lisp->extension("deflib mlx_atleast_3d(array)",
                    new Lispe_mlx_methods(mlx_method_atleast_3d));

    lisp->extension("deflib mlx_array_equal(a b (equal_nan))",
                    new Lispe_mlx_methods(mlx_method_array_equal));

    lisp->extension("deflib mlx_allclose(a b (rtol) (atol) (equal_nan))",
                    new Lispe_mlx_methods(mlx_method_allclose));

    // Groupe 25: Random avancé
    lisp->extension("deflib mlx_random_bernoulli(p (shape))",
                    new Lispe_mlx_methods(mlx_method_random_bernoulli));

    lisp->extension("deflib mlx_random_truncated_normal(lower upper (shape) (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_truncated_normal));

    lisp->extension("deflib mlx_random_gumbel(shape (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_gumbel));

    lisp->extension("deflib mlx_random_categorical(logits (axis) (num_samples) (shape))",
                    new Lispe_mlx_methods(mlx_method_random_categorical));

    lisp->extension("deflib mlx_random_laplace(shape (loc) (scale) (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_laplace));

    lisp->extension("deflib mlx_random_multivariate_normal(mean cov (shape) (dtype))",
                    new Lispe_mlx_methods(mlx_method_random_multivariate_normal));

    lisp->extension("deflib mlx_random_permutation(x (axis))",
                    new Lispe_mlx_methods(mlx_method_random_permutation));

    // Groupe 26: Algèbre linéaire avancée
    lisp->extension("deflib mlx_tri_inv(array (upper))",
                    new Lispe_mlx_methods(mlx_method_tri_inv));

    lisp->extension("deflib mlx_cholesky_inv(array (upper))",
                    new Lispe_mlx_methods(mlx_method_cholesky_inv));

    lisp->extension("deflib mlx_lu_factor(array)",
                    new Lispe_mlx_methods(mlx_method_lu_factor));

    lisp->extension("deflib mlx_solve_triangular(a b (upper))",
                    new Lispe_mlx_methods(mlx_method_solve_triangular));

    lisp->extension("deflib mlx_eigvalsh(array (UPLO))",
                    new Lispe_mlx_methods(mlx_method_eigvalsh));

    lisp->extension("deflib mlx_eigh(array (UPLO))",
                    new Lispe_mlx_methods(mlx_method_eigh));

    // Groupe 27: Gather/Scatter avancé
    lisp->extension("deflib mlx_take_along_axis(array indices axis)",
                    new Lispe_mlx_methods(mlx_method_take_along_axis));

    lisp->extension("deflib mlx_put_along_axis(array indices values axis)",
                    new Lispe_mlx_methods(mlx_method_put_along_axis));

    lisp->extension("deflib mlx_gather(array indices axis slice_sizes)",
                    new Lispe_mlx_methods(mlx_method_gather));

    lisp->extension("deflib mlx_scatter(array indices updates axis)",
                    new Lispe_mlx_methods(mlx_method_scatter));

    lisp->extension("deflib mlx_scatter_add(array indices updates axis)",
                    new Lispe_mlx_methods(mlx_method_scatter_add));

    lisp->extension("deflib mlx_slice_update(src update start stop (strides))",
                    new Lispe_mlx_methods(mlx_method_slice_update));

    // Groupe 28: Nombres complexes
    lisp->extension("deflib mlx_conjugate(array)",
                    new Lispe_mlx_methods(mlx_method_conjugate));

    lisp->extension("deflib mlx_real(array)",
                    new Lispe_mlx_methods(mlx_method_real));

    lisp->extension("deflib mlx_imag(array)",
                    new Lispe_mlx_methods(mlx_method_imag));

    // Groupe 29: Opérations fusionnées pour LLM
    lisp->extension("deflib mlx_fused_mlp(x gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b (group_size) (bits))",
                    new Lispe_mlx_methods(mlx_method_fused_mlp));

    lisp->extension("deflib mlx_fused_moe(x expert_indices expert_weights gate_w gate_s gate_qb gate_lb up_w up_s up_qb up_lb down_w down_s down_qb down_lb num_experts experts_per_tok (group_size) (bits) (activation))",
                    new Lispe_mlx_methods(mlx_method_fused_moe));

    lisp->extension("deflib mlx_fused_moe_batch(x expert_indices expert_weights gate_w gate_s gate_qb gate_lb up_w up_s up_qb up_lb down_w down_s down_qb down_lb num_experts experts_per_tok (group_size) (bits) (activation))",
                    new Lispe_mlx_methods(mlx_method_fused_moe_batch));

    // Groupe 30: Évaluation explicite
    lisp->extension("deflib mlx_eval(array)",
                    new Lispe_mlx_methods(mlx_method_eval));

    // Groupe 31: Conversion vers LispE
    lisp->extension("deflib mlx_tolist(array)",
                    new Lispe_mlx_methods(mlx_method_tolist));

    return true;
}
}
