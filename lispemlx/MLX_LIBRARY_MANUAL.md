# lispe_mlx Library Manual

> **Apple MLX Framework for LispE**  
> High-performance machine learning on Apple Silicon

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Array Creation](#array-creation)
4. [Basic Operations](#basic-operations)
5. [Mathematical Functions](#mathematical-functions)
6. [Reduction Operations](#reduction-operations)
7. [Shape Manipulation](#shape-manipulation)
8. [Linear Algebra](#linear-algebra)
9. [Activation Functions](#activation-functions)
10. [Deep Learning Operations](#deep-learning-operations)
11. [Quantization](#quantization)
12. [Model I/O](#model-io)
13. [Memory Management](#memory-management)
14. [Random Number Generation](#random-number-generation)
15. [FFT Operations](#fft-operations)
16. [Advanced Operations](#advanced-operations)
17. [Real-World Examples](#real-world-examples)
18. [Function Reference (Quick Lookup)](#function-reference-quick-lookup)
19. [Alphabetical Index](#alphabetical-index)
20. [Tips and Best Practices](#tips-and-best-practices)

---

## Introduction

The `lispe_mlx` library provides LispE bindings for Apple's MLX framework, enabling high-performance machine learning computations on Apple Silicon (M1/M2/M3/M4). MLX is designed for efficient GPU acceleration using the unified memory architecture of Apple chips.

### Key Features

- **Native Apple Silicon support**: Optimized for M-series chips
- **Unified memory**: No CPU-GPU data transfers needed
- **Lazy evaluation**: Computations are only performed when needed
- **Quantization support**: Load and run quantized models (4-bit, 8-bit)
- **Safetensors/GGUF support**: Load models from HuggingFace

### Loading the Library

```lisp
(use 'lispe_mlx)
```

---

## Getting Started

### Creating Your First Array

```lisp
(use 'lispe_mlx)

; Create an array from a LispE list
(setq a (mlx_array (floats 1.0 2.0 3.0 4.0)))

; Create with specific shape
(setq b (mlx_array (floats 1.0 2.0 3.0 4.0 5.0 6.0) (integers 2 3)))

; Check the shape
(println "Shape:" (mlx_shape a))  ; → (4)
(println "Shape:" (mlx_shape b))  ; → (2 3)

; Convert back to LispE list
(println "Data:" (mlx_tolist a))  ; → (1.0 2.0 3.0 4.0)
```

### Basic Arithmetic

```lisp
(setq x (mlx_array (floats 1.0 2.0 3.0)))
(setq y (mlx_array (floats 4.0 5.0 6.0)))

; Element-wise operations
(setq sum (mlx_add x y))          ; → (5.0 7.0 9.0)
(setq diff (mlx_subtract x y))    ; → (-3.0 -3.0 -3.0)
(setq prod (mlx_multiply x y))    ; → (4.0 10.0 18.0)
(setq quot (mlx_divide x y))      ; → (0.25 0.4 0.5)

; Scalar operations (broadcasting)
(setq scaled (mlx_multiply x 2.0))  ; → (2.0 4.0 6.0)
```

---

## Array Creation

### Basic Creation Functions

| Function | Description | Example |
|----------|-------------|---------|
| `mlx_array` | Create from data | `(mlx_array (floats 1.0 2.0))` |
| `mlx_zeros` | Array of zeros | `(mlx_zeros (integers 3 4))` |
| `mlx_ones` | Array of ones | `(mlx_ones (integers 2 2))` |
| `mlx_full` | Array filled with value | `(mlx_full (integers 3 3) 5.0)` |
| `mlx_eye` | Identity matrix | `(mlx_eye 4)` |
| `mlx_identity` | Identity matrix | `(mlx_identity 4)` |

### Range and Sequence

```lisp
; Create a range [start, stop) with step
(setq r (mlx_arange 0 10 1))        ; → (0 1 2 3 4 5 6 7 8 9)
(setq r (mlx_arange 0.0 1.0 0.1))   ; → (0.0 0.1 0.2 ... 0.9)

; Linearly spaced values
(setq l (mlx_linspace 0.0 1.0 5))   ; → (0.0 0.25 0.5 0.75 1.0)
```

### Data Types

Supported dtypes: `"float32"`, `"float16"`, `"bfloat16"`, `"int32"`, `"int64"`, `"uint32"`, `"uint64"`, `"int8"`, `"uint8"`, `"bool"`, `"complex64"`

```lisp
; Create with specific dtype
(setq a (mlx_zeros (integers 3 3) "float16"))
(setq b (mlx_ones (integers 2 2) "int32"))

; Check dtype
(println (mlx_dtype a))  ; → "float16"

; Convert dtype
(setq c (mlx_astype b "float32"))
```

### Special Matrices

```lisp
; Triangular matrices
(setq lower (mlx_tril (mlx_ones (integers 4 4))))
(setq upper (mlx_triu (mlx_ones (integers 4 4))))

; Tri matrix
(setq tri (mlx_tri 4 4 0))

; Diagonal matrix
(setq d (mlx_diag (mlx_array (floats 1.0 2.0 3.0))))

; Meshgrid
(setq grids (mlx_meshgrid (list (mlx_arange 0 3 1) (mlx_arange 0 2 1))))
```

---

## Basic Operations

### Arithmetic Operations

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0 4.0)))
(setq b (mlx_array (floats 2.0 2.0 2.0 2.0)))

; Basic arithmetic
(mlx_add a b)        ; Element-wise addition
(mlx_subtract a b)   ; Element-wise subtraction
(mlx_multiply a b)   ; Element-wise multiplication
(mlx_divide a b)     ; Element-wise division
(mlx_power a b)      ; Element-wise power (a^b)

; Unary operations
(mlx_negative a)     ; Negate: -a
(mlx_square a)       ; Square: a²
(mlx_sqrt a)         ; Square root
(mlx_abs a)          ; Absolute value
(mlx_reciprocal a)   ; 1/a
(mlx_rsqrt a)        ; 1/√a
```

### Comparison Operations

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))
(setq b (mlx_array (floats 2.0 2.0 2.0)))

; Comparisons return boolean arrays
(mlx_equal a b)          ; → (false true false)
(mlx_not_equal a b)      ; → (true false true)
(mlx_greater a b)        ; → (false false true)
(mlx_less a b)           ; → (true false false)
(mlx_greater_equal a b)  ; → (false true true)
(mlx_less_equal a b)     ; → (true true false)

; Conditional selection
(setq cond (mlx_greater a 1.5))
(mlx_where cond a b)     ; → (2.0 2.0 3.0)
```

### Logical Operations

```lisp
(setq a (mlx_array (integers 1 0 1 0)))
(setq b (mlx_array (integers 1 1 0 0)))

(mlx_logical_and a b)    ; → (1 0 0 0)
(mlx_logical_or a b)     ; → (1 1 1 0)
(mlx_logical_not a)      ; → (0 1 0 1)

; Value tests
(mlx_isnan a)            ; Check for NaN
(mlx_isinf a)            ; Check for infinity
(mlx_isfinite a)         ; Check for finite values
```

### Bitwise Operations

```lisp
(setq a (mlx_array (integers 5 3)))   ; binary: 101, 011
(setq b (mlx_array (integers 3 1)))   ; binary: 011, 001

(mlx_bitwise_and a b)      ; → (1 1)  ; 001, 001
(mlx_bitwise_or a b)       ; → (7 3)  ; 111, 011
(mlx_bitwise_xor a b)      ; → (6 2)  ; 110, 010
(mlx_bitwise_invert a)     ; Bitwise NOT
(mlx_left_shift a 1)       ; → (10 6)
(mlx_right_shift a 1)      ; → (2 1)
```

---

## Mathematical Functions

### Exponential and Logarithmic

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))

(mlx_exp a)       ; e^x
(mlx_expm1 a)     ; e^x - 1 (more accurate for small x)
(mlx_log a)       ; Natural log
(mlx_log2 a)      ; Log base 2
(mlx_log10 a)     ; Log base 10
(mlx_log1p a)     ; log(1 + x) (more accurate for small x)

; Log-sum-exp (numerically stable)
(mlx_logaddexp a b)  ; log(e^a + e^b)
```

### Trigonometric Functions

```lisp
; Note: LispE provides _pi or π as built-in constants
(setq angles (mlx_array (floats 0.0 (/ _pi 2) _pi)))  ; 0, π/2, π
; Or using the Unicode symbol:
(setq angles (mlx_array (floats 0.0 (/ π 2) π)))

; Basic trig
(mlx_sin angles)
(mlx_cos angles)
(mlx_tan angles)

; Inverse trig
(mlx_arcsin a)
(mlx_arccos a)
(mlx_arctan a)
(mlx_arctan2 y x)   ; Two-argument arctangent

; Hyperbolic
(mlx_sinh a)
(mlx_cosh a)
(mlx_tanh a)

; Inverse hyperbolic
(mlx_arcsinh a)
(mlx_arccosh a)
(mlx_arctanh a)

; Angle conversions
(mlx_degrees angles)  ; Radians to degrees
(mlx_radians degrees) ; Degrees to radians
```

### Rounding Functions

```lisp
(setq a (mlx_array (floats 1.2 2.7 -1.5 -2.8)))

(mlx_floor a)    ; → (1.0 2.0 -2.0 -3.0)
(mlx_ceil a)     ; → (2.0 3.0 -1.0 -2.0)
(mlx_round a)    ; → (1.0 3.0 -2.0 -3.0)

; Clipping
(mlx_clip a 0.0 2.0)  ; → (1.2 2.0 0.0 0.0)
```

### Special Functions

```lisp
(mlx_erf a)      ; Error function
(mlx_erfinv a)   ; Inverse error function
(mlx_sign a)     ; Sign of elements (-1, 0, or 1)
```

---

## Reduction Operations

### Sum, Mean, Product

```lisp
(setq a (mlx_reshape (mlx_arange 1 13 1 "float32") (integers 3 4)))
; a = [[1 2 3 4]
;      [5 6 7 8]
;      [9 10 11 12]]

; Full reduction
(mlx_sum a)      ; → 78.0
(mlx_mean a)     ; → 6.5
(mlx_prod a)     ; → 479001600.0

; Along axis
(mlx_sum a (integers 0))    ; Sum along rows → (15 18 21 24)
(mlx_sum a (integers 1))    ; Sum along cols → (10 26 42)
(mlx_mean a (integers 0))   ; Mean along rows → (5 6 7 8)

; Keep dimensions
(mlx_sum a (integers 1) true)  ; → [[10] [26] [42]]
```

### Min, Max, Argmin, Argmax

```lisp
(setq a (mlx_array (floats 3.0 1.0 4.0 1.0 5.0 9.0)))

(mlx_min a)      ; → 1.0
(mlx_max a)      ; → 9.0
(mlx_argmin a)   ; → 1 (index of minimum)
(mlx_argmax a)   ; → 5 (index of maximum)

; Along axis
(setq b (mlx_reshape a (integers 2 3)))
(mlx_min b (integers 1))     ; Min per row
(mlx_argmax b (integers 0))  ; Argmax per column
```

### Variance and Standard Deviation

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0 4.0 5.0)))

(mlx_var a)          ; Variance
(mlx_std a)          ; Standard deviation

; With degrees of freedom correction
(mlx_var a nil false 1)  ; Bessel's correction (ddof=1)
```

### Logical Reductions

```lisp
(setq a (mlx_array (integers 1 1 1 0)))

(mlx_all a)      ; → false (not all true)
(mlx_any a)      ; → true (at least one true)

; Along axis
(setq b (mlx_reshape a (integers 2 2)))
(mlx_all b (integers 1))  ; All true per row?
```

### Cumulative Operations

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0 4.0)))

(mlx_cumsum a)    ; → (1.0 3.0 6.0 10.0)
(mlx_cumprod a)   ; → (1.0 2.0 6.0 24.0)
(mlx_cummax a)    ; Cumulative maximum
(mlx_cummin a)    ; Cumulative minimum

; Reverse cumulative
(mlx_cumsum a 0 true)  ; From end to start

; Exclusive (not including current element)
(mlx_cumsum a 0 false false)
```

### Log-Sum-Exp Reductions

```lisp
(setq logits (mlx_array (floats 1.0 2.0 3.0)))

; Numerically stable log(sum(exp(x)))
(mlx_logsumexp logits)               ; Full reduction
(mlx_logsumexp logits (integers 0))  ; Along axis

; Cumulative log-sum-exp
(mlx_logcumsumexp logits)
```

---

## Shape Manipulation

### Reshape and Transpose

```lisp
(setq a (mlx_arange 0 12 1 "float32"))

; Reshape
(setq b (mlx_reshape a (integers 3 4)))
(setq c (mlx_reshape a (integers 2 2 3)))

; Transpose
(mlx_transpose b)                    ; 2D transpose
(mlx_transpose c (integers 2 0 1))   ; Permute axes
```

### Squeeze and Expand

```lisp
; Remove dimensions of size 1
(setq a (mlx_reshape (mlx_ones (integers 6)) (integers 1 2 1 3 1)))
(println (mlx_shape a))               ; → (1 2 1 3 1)
(println (mlx_shape (mlx_squeeze a))) ; → (2 3)
(println (mlx_shape (mlx_squeeze a (integers 0 2)))) ; Remove specific axes

; Add dimension
(setq b (mlx_array (floats 1.0 2.0 3.0)))
(println (mlx_shape b))                        ; → (3)
(println (mlx_shape (mlx_expand_dims b 0)))    ; → (1 3)
(println (mlx_shape (mlx_expand_dims b -1)))   ; → (3 1)
```

### Flatten

```lisp
(setq a (mlx_reshape (mlx_arange 0 24 1) (integers 2 3 4)))

(mlx_flatten a)          ; Flatten all → (24,)
(mlx_flatten a 0 1)      ; Flatten axes 0-1 → (6, 4)
(mlx_flatten a 1 2)      ; Flatten axes 1-2 → (2, 12)
```

### Concatenate and Stack

```lisp
(setq a (mlx_array (floats 1.0 2.0)))
(setq b (mlx_array (floats 3.0 4.0)))

; Concatenate along existing axis
(mlx_concatenate (list a b) 0)  ; → (1 2 3 4)

; Stack along new axis
(mlx_stack (list a b) 0)        ; → [[1 2] [3 4]]
(mlx_stack (list a b) 1)        ; → [[1 3] [2 4]]
```

### Split

```lisp
(setq a (mlx_arange 0 12 1))

; Split into equal parts
(setq parts (mlx_split a 3 0))  ; 3 parts of 4 elements

; Split at specific indices
(setq parts (mlx_split a (integers 2 5 9) 0))
```

### Tile and Repeat

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))

; Tile (repeat entire array)
(mlx_tile a (integers 2))      ; → (1 2 3 1 2 3)
(mlx_tile (mlx_reshape a (integers 1 3)) (integers 2 1))  ; Repeat rows

; Repeat (repeat elements)
(mlx_repeat a 2 0)             ; → (1 1 2 2 3 3)
```

### Padding

```lisp
(setq a (mlx_ones (integers 2 3)))

; Pad with zeros
(setq padded (mlx_pad a (integers 1 1 2 2) 0.0))
; Adds 1 before/after dim 0, 2 before/after dim 1
```

### Flip and Roll

```lisp
(setq a (mlx_arange 0 6 1))

; Flip (reverse)
(mlx_flip a)           ; → (5 4 3 2 1 0)
(mlx_flip a (integers 0))  ; Flip along specific axis

; Roll (circular shift)
(mlx_roll a 2)         ; → (4 5 0 1 2 3)
(mlx_roll a -1)        ; → (1 2 3 4 5 0)
```

### Broadcast

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))

; Broadcast to new shape
(mlx_broadcast_to a (integers 4 3))  ; → [[1 2 3] [1 2 3] [1 2 3] [1 2 3]]
```

### Axis Manipulation

```lisp
(setq a (mlx_reshape (mlx_arange 0 24 1) (integers 2 3 4)))

; Move axis
(println (mlx_shape (mlx_moveaxis a 0 2)))  ; → (3 4 2)

; Swap axes
(println (mlx_shape (mlx_swapaxes a 0 2)))  ; → (4 3 2)
```

---

## Linear Algebra

### Matrix Multiplication

```lisp
(setq a (mlx_reshape (mlx_arange 0 6 1 "float32") (integers 2 3)))
(setq b (mlx_reshape (mlx_arange 0 6 1 "float32") (integers 3 2)))

; Matrix multiplication
(setq c (mlx_matmul a b))
(println (mlx_shape c))  ; → (2 2)

; Batched matmul
(setq batch_a (mlx_reshape (mlx_arange 0 12 1 "float32") (integers 2 2 3)))
(setq batch_b (mlx_reshape (mlx_arange 0 12 1 "float32") (integers 2 3 2)))
(setq batch_c (mlx_matmul batch_a batch_b))  ; → (2 2 2)
```

### Vector Products

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))
(setq b (mlx_array (floats 4.0 5.0 6.0)))

; Inner product (dot product)
(mlx_inner a b)   ; → 32.0

; Outer product
(mlx_outer a b)   ; → [[4 5 6] [8 10 12] [12 15 18]]

; Cross product (3D vectors)
(mlx_cross a b)   ; → (-3.0 6.0 -3.0)
```

### Tensor Contractions

```lisp
; Tensordot
(setq a (mlx_reshape (mlx_arange 0 24 1 "float32") (integers 2 3 4)))
(setq b (mlx_reshape (mlx_arange 0 24 1 "float32") (integers 4 3 2)))
(mlx_tensordot a b 2)  ; Contract last 2 axes of a with first 2 of b

; Einstein summation
(setq a (mlx_reshape (mlx_arange 0 6 1 "float32") (integers 2 3)))
(setq b (mlx_reshape (mlx_arange 0 6 1 "float32") (integers 3 2)))
(mlx_einsum "ij,jk->ik" (list a b))  ; Matrix multiplication
(mlx_einsum "ii->" (list (mlx_eye 3 3 "float32")))  ; Trace

; Kronecker product
(mlx_kron a b)
```

### Matrix Decompositions

```lisp
(setq a (mlx_reshape (mlx_array (floats 4.0 12.0 -16.0 12.0 37.0 -43.0 -16.0 -43.0 98.0)) (integers 3 3)))

; SVD: A = U @ S @ V^T
(setq svd_result (mlx_svd a))
(setq U (@ svd_result 0))
(setq S (@ svd_result 1))
(setq Vt (@ svd_result 2))

; QR decomposition: A = Q @ R
(setq qr_result (mlx_qr a))
(setq Q (@ qr_result 0))
(setq R (@ qr_result 1))

; Cholesky decomposition: A = L @ L^T (for positive definite matrices)
(setq L (mlx_cholesky a))
(setq U (mlx_cholesky a true))  ; Upper triangular

; LU decomposition
(setq lu_result (mlx_lu a))
```

### Eigenvalues and Eigenvectors

```lisp
(setq a (mlx_reshape (mlx_array (floats 1.0 2.0 2.0 3.0)) (integers 2 2)))

; Eigenvalues only
(setq eigvals (mlx_eigvals a))

; Eigenvalues and eigenvectors
(setq eig_result (mlx_eig a))
(setq eigenvalues (@ eig_result 0))
(setq eigenvectors (@ eig_result 1))

; For symmetric/Hermitian matrices (more stable)
(setq eigvalsh (mlx_eigvalsh a))
(setq eigh_result (mlx_eigh a))
```

### Linear Systems

```lisp
(setq A (mlx_reshape (mlx_array (floats 3.0 1.0 1.0 2.0)) (integers 2 2)))
(setq b (mlx_array (floats 9.0 8.0)))

; Solve Ax = b
(setq x (mlx_solve A b))

; Solve triangular system
(setq L (mlx_tril A))
(mlx_solve_triangular L b)        ; Lower triangular
(mlx_solve_triangular L b true)   ; Upper triangular
```

### Matrix Inverse and Pseudo-Inverse

```lisp
(setq a (mlx_reshape (mlx_array (floats 1.0 2.0 3.0 4.0)) (integers 2 2)))

; Inverse
(setq a_inv (mlx_inv a))

; Pseudo-inverse (Moore-Penrose)
(setq a_pinv (mlx_pinv a))

; Triangular inverse
(setq L (mlx_tril a))
(mlx_tri_inv L)        ; Lower triangular inverse
(mlx_tri_inv L true)   ; Upper triangular inverse

; Cholesky inverse
(mlx_cholesky_inv a)
```

### Matrix Norms

```lisp
(setq a (mlx_reshape (mlx_array (floats 1.0 2.0 3.0 4.0)) (integers 2 2)))

; Frobenius norm (default)
(mlx_norm a)

; Vector norms
(setq v (mlx_array (floats 3.0 4.0)))
(mlx_norm v 2)    ; L2 norm → 5.0
(mlx_norm v 1)    ; L1 norm → 7.0
(mlx_norm v "inf")  ; Infinity norm → 4.0

; Along specific axes
(mlx_norm a 2 (integers 1) true)  ; L2 norm per row, keep dims
```

### Diagonal Operations

```lisp
(setq a (mlx_reshape (mlx_arange 0 9 1 "float32") (integers 3 3)))

; Extract diagonal
(mlx_diagonal a)         ; Main diagonal
(mlx_diagonal a 1)       ; Diagonal above main
(mlx_diagonal a -1)      ; Diagonal below main

; Create diagonal matrix
(setq d (mlx_diag (mlx_array (floats 1.0 2.0 3.0))))

; Matrix trace
(mlx_trace a)            ; Sum of diagonal
```

### Tucker Decomposition

Tucker decomposition factorizes a tensor into a core tensor and factor matrices along each mode. It's useful for **tensor compression**, **dimensionality reduction**, and **multi-way data analysis**.

**HOSVD** (Higher-Order Singular Value Decomposition) is a non-iterative method that computes the Tucker decomposition by performing SVD along each mode of the tensor. It's fast but may not give the optimal low-rank approximation.

**HOOI** (Higher-Order Orthogonal Iteration), implemented as `mlx_tucker`, is an iterative refinement of HOSVD that converges to a better approximation.

```lisp
; For tensor decomposition (compression, dimensionality reduction)
(setq tensor (mlx_random_normal (integers 10 20 30)))
(setq ranks (integers 5 10 15))  ; Target ranks for each mode

; HOSVD (Higher-Order SVD) - fast, non-iterative
; Decomposes tensor T ≈ core ×₁ U₁ ×₂ U₂ ×₃ U₃
; Returns (core_tensor, (U1 U2 U3))
(setq hosvd_result (mlx_hosvd tensor ranks))
(setq core (@ hosvd_result 0))      ; Core tensor of shape (5, 10, 15)
(setq factors (@ hosvd_result 1))   ; List of factor matrices

; Tucker (HOOI - iterative, more accurate)
; max_iter=50, tolerance=1e-5
(setq tucker_result (mlx_tucker tensor ranks 50 1e-5))
(setq core (@ tucker_result 0))
(setq factors (@ tucker_result 1))

; Reconstruct tensor from decomposition
(setq reconstructed (mlx_tucker_reconstruct core factors))

; Check reconstruction error
(setq error (mlx_norm (mlx_subtract tensor reconstructed)))
(println "Reconstruction error:" (mlx_tolist error))

; Compute compression ratio
; Returns dictionary with ratio, original_size, compressed_size, etc.
(setq ratio_info (mlx_tucker_compression_ratio (mlx_shape tensor) core factors))
(println "Compression ratio:" ratio_info)

; Khatri-Rao product (column-wise Kronecker product)
; Used in CP/PARAFAC decomposition
; For matrices A (I×K) and B (J×K), returns (I*J × K)
(setq a (mlx_random_normal (integers 4 3)))
(setq b (mlx_random_normal (integers 5 3)))
(setq kr (mlx_khatri_rao_product a b))  ; Shape: (20, 3)
```

**Use cases for Tucker decomposition:**
- **Model compression**: Reduce size of neural network weight tensors
- **Denoising**: Low-rank approximation filters noise
- **Feature extraction**: Core tensor captures essential structure
- **Multi-way PCA**: Generalization of PCA to higher-order tensors

---

## Activation Functions

### Standard Activations

```lisp
(setq x (mlx_array (floats -2.0 -1.0 0.0 1.0 2.0)))

; ReLU family
(mlx_relu x)                ; max(0, x)
(mlx_leaky_relu x 0.01)     ; max(0.01*x, x)
(mlx_elu x 1.0)             ; x if x>0 else alpha*(exp(x)-1)
(mlx_selu x)                ; Scaled ELU

; Sigmoid and Tanh
(mlx_sigmoid x)             ; 1 / (1 + exp(-x))
(mlx_tanh x)                ; Hyperbolic tangent

; GELU (used in transformers)
(mlx_gelu x)                ; Gaussian Error Linear Unit
(mlx_gelu_tanh x)           ; GELU with tanh approximation

; SiLU/Swish (used in modern architectures)
(mlx_silu x)                ; x * sigmoid(x)
```

### Softmax

```lisp
(setq logits (mlx_array (floats 1.0 2.0 3.0 4.0)))

; Softmax (normalizes to probability distribution)
(mlx_softmax logits)        ; Along last axis by default
(mlx_softmax logits 0)      ; Along specific axis

; Log-softmax (numerically stable log of softmax)
(mlx_log_softmax logits)
```

---

## Deep Learning Operations

### Normalization

```lisp
(setq x (mlx_random_normal (integers 2 4 8)))  ; (batch, seq, hidden)
(setq weight (mlx_ones (integers 8)))
(setq bias (mlx_zeros (integers 8)))

; RMS Normalization
(mlx_rms_norm x weight 1e-5)        ; Standard RMSNorm
(mlx_rms_norm x weight 1e-5 true)   ; Gemma-style (1 + weight)

; Layer Normalization
(mlx_layer_norm x weight bias 1e-5)
```

### Rotary Position Embeddings (RoPE)

```lisp
; RoPE is essential for modern transformers (Llama, Mistral, etc.)
(setq x (mlx_random_normal (integers 1 8 10 64)))  ; (B, heads, seq, head_dim)

; Basic RoPE
(mlx_rope x 64)                    ; dims to rotate

; With parameters
(mlx_rope x 64 false 10000.0 1.0 0)  ; traditional=false, base=10000, scale=1, offset=0

; With precomputed frequencies
(setq freqs (mlx_array (floats 0.1 0.2 0.3)))
(mlx_rope x 64 false 10000.0 1.0 0 freqs)
```

### Attention

```lisp
; Scaled dot-product attention (Flash Attention on Apple Silicon)
(setq queries (mlx_random_normal (integers 1 8 10 64)))
(setq keys (mlx_random_normal (integers 1 8 10 64)))
(setq values (mlx_random_normal (integers 1 8 10 64)))
(setq scale (/ 1.0 (sqrt 64.0)))

; Basic attention
(mlx_scaled_dot_product_attention queries keys values scale)

; With causal mask (for autoregressive generation)
(mlx_scaled_dot_product_attention queries keys values scale "causal")

; With custom mask
(setq mask (mlx_triu (mlx_full (integers 10 10) -10000.0) 1))
(mlx_scaled_dot_product_attention queries keys values scale "additive" (list mask))

; With attention sinks (for streaming)
(setq sinks (mlx_ones (integers 1 8 4 64)))
(mlx_scaled_dot_product_attention queries keys values scale "causal" nil sinks)
```

### Convolutions

```lisp
; 1D Convolution
(setq input (mlx_random_normal (integers 1 10 16)))   ; (N, L, C_in)
(setq weight (mlx_random_normal (integers 32 3 16)))  ; (C_out, K, C_in)
(mlx_conv1d input weight 1 1 1 1)  ; stride, padding, dilation, groups

; 2D Convolution
(setq input (mlx_random_normal (integers 1 28 28 3)))    ; (N, H, W, C_in)
(setq weight (mlx_random_normal (integers 32 3 3 3)))    ; (C_out, Kh, Kw, C_in)
(mlx_conv2d input weight (integers 1 1) (integers 1 1) (integers 1 1) 1)

; 3D Convolution
(setq input (mlx_random_normal (integers 1 16 28 28 3)))
(setq weight (mlx_random_normal (integers 32 3 3 3 3)))
(mlx_conv3d input weight)

; Transposed convolutions
(mlx_conv_transpose1d input weight)
(mlx_conv_transpose2d input weight)
```

### Fused Operations (Optimized for LLMs)

```lisp
; Fused MLP (gate + up + down projections with SiLU)
; More efficient than separate operations
(setq x (mlx_random_normal (integers 1 10 512)))
(setq gate_w (mlx_random_normal (integers 256 512)))
(setq up_w (mlx_random_normal (integers 256 512)))
(setq down_w (mlx_random_normal (integers 512 256)))

; For quantized weights
(setq gate_s (mlx_ones (integers 64 8)))    ; scales
(setq gate_b (mlx_zeros (integers 256 64))) ; biases

(mlx_fused_mlp x gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b 64 4)

; Fused Mixture of Experts (MoE)
; Extremely efficient for MoE models like Mixtral, GPT-OSS
(mlx_fused_moe x expert_indices expert_weights 
    gate_w gate_s gate_qb gate_lb
    up_w up_s up_qb up_lb
    down_w down_s down_qb down_lb
    32 4  ; num_experts, experts_per_token
    64 4  ; group_size, bits
    "silu")  ; activation
```

---

## Quantization

### Quantize Weights

```lisp
(setq weights (mlx_random_normal (integers 512 256)))

; Quantize to 4-bit
(setq quant_result (mlx_quantize weights 64 4))  ; group_size=64, bits=4
(setq quantized_weights (@ quant_result 0))
(setq scales (@ quant_result 1))
(setq biases (@ quant_result 2))

; Quantize to 8-bit
(setq quant_result_8bit (mlx_quantize weights 128 8))
```

### Dequantize

```lisp
; Dequantize back to full precision
(setq dequantized (mlx_dequantize quantized_weights scales biases 64 4))
```

### Quantized Matrix Multiplication

```lisp
; Direct quantized matmul (much faster than dequantize + matmul)
(setq x (mlx_random_normal (integers 1 10 512)))

; x @ quantized_weights^T
(setq output (mlx_quantized_matmul x quantized_weights scales biases true 64 4))
; transpose=true for typical (seq, hidden) @ (out, hidden)^T = (seq, out)
```

---

## Model I/O

### Safetensors (HuggingFace Format)

```lisp
; Load safetensors file (returns (tensors_dict, metadata_dict))
(setq result (mlx_load_safetensors "/path/to/model.safetensors"))
(setq tensors (@ result 0))
(setq metadata (@ result 1))

; Access individual tensors
(setq embed_weights (@ tensors "model.embed_tokens.weight"))

; Save safetensors
(setq tensors_to_save (dictionary
    "layer1.weight" (mlx_random_normal (integers 512 256))
    "layer1.bias" (mlx_zeros (integers 512))))
(setq metadata (dictionary "__format__" "pt"))
(mlx_save_safetensors "/path/to/output.safetensors" tensors_to_save metadata)
```

### GGUF Format (llama.cpp Compatible)

```lisp
; Load GGUF file
(setq result (mlx_load_gguf "/path/to/model.gguf"))
(setq tensors (@ result 0))
(setq metadata (@ result 1))

; Save GGUF
(mlx_save_gguf "/path/to/output.gguf" tensors metadata)
```

### Simple Array Save/Load

```lisp
; Save single array
(setq arr (mlx_random_normal (integers 100 100)))
(mlx_save "/path/to/array.npy" arr)

; Load single array
(setq loaded (mlx_load "/path/to/array.npy"))
```

### Loading Sharded Models (Real-World Example)

```lisp
; Load a sharded model (multiple safetensor files)
(defun load_sharded_safetensors(dir_path num_shards)
    (setq all_tensors (list))
    (setq tensor_index (dictionary))
    (setq total_tensors 0)
    
    (loopcount num_shards i
        (setq idx (+ i 1))
        (setq idx_str (string idx))
        (while (< (size idx_str) 5)
            (setq idx_str (+ "0" idx_str)))
        (setq num_str (string num_shards))
        (while (< (size num_str) 5)
            (setq num_str (+ "0" num_str)))
        (setq filepath (+ dir_path "/model-" idx_str "-of-" num_str ".safetensors"))
        
        (println "Loading shard " idx "/" num_shards)
        (setq result (mlx_load_safetensors filepath))
        (setq tensors (@ result 0))
        
        (loop name (keys@ tensors)
            (set@ tensor_index name total_tensors)
            (push all_tensors (@ tensors name))
            (+= total_tensors 1)))
    
    (list all_tensors tensor_index))

; Usage
(setq model_data (load_sharded_safetensors "/path/to/model" 4))
(setq weights_list (@ model_data 0))
(setq tensor_index (@ model_data 1))
```

---

## Memory Management

### Memory Statistics

```lisp
; Get memory usage (in bytes)
(println "Active memory:" (/ (mlx_get_active_memory) 1048576) "MB")
(println "Peak memory:  " (/ (mlx_get_peak_memory) 1048576) "MB")
(println "Cache memory: " (/ (mlx_get_cache_memory) 1048576) "MB")
```

### Memory Control

```lisp
; Set memory limits
(mlx_set_memory_limit (* 8 1024 1024 1024))  ; 8 GB limit
(mlx_set_cache_limit (* 4 1024 1024 1024))   ; 4 GB cache limit

; Clear GPU cache
(mlx_clear_cache)
```

### Synchronization

```lisp
; MLX uses lazy evaluation - operations are not executed until needed
; Force evaluation with synchronize
(setq a (mlx_matmul x y))  ; Computation graph created, not executed
(mlx_synchronize)          ; Force execution now

; Or use eval for explicit evaluation
(mlx_eval a)
```

### Memory-Efficient Patterns

```lisp
; Process in batches to control memory
(defun process_in_batches(data batch_size process_fn)
    (setq total (@ (mlx_shape data) 0))
    (setq results (list))
    
    (loopcount (ceiling (/ total batch_size)) i
        (setq start (* i batch_size))
        (setq end (min (+ start batch_size) total))
        (setq batch (mlx_slice data (integers start) (integers end)))
        
        (push results (process_fn batch))
        
        ; Clear cache periodically
        (check (zerop (% i 10))
            (mlx_clear_cache)))
    
    (mlx_concatenate results 0))
```

---

## Random Number Generation

### Basic Distributions

```lisp
; Uniform distribution [0, 1)
(setq u (mlx_random_uniform (integers 3 4)))

; Uniform with range [low, high)
(setq u (mlx_random_uniform (integers 3 4) -1.0 1.0))

; Normal (Gaussian) distribution
(setq n (mlx_random_normal (integers 3 4)))          ; mean=0, std=1
(setq n (mlx_random_normal (integers 3 4) 5.0 2.0))  ; mean=5, std=2

; Random integers
(setq i (mlx_random_randint 0 10 (integers 5 5)))    ; [0, 10)
```

### Advanced Distributions

```lisp
; Bernoulli (binary outcomes)
(setq p (mlx_array (floats 0.3 0.7 0.5)))
(mlx_random_bernoulli p)                    ; Sample with given probabilities
(mlx_random_bernoulli 0.5 (integers 10))    ; Fixed probability, given shape

; Truncated normal (bounded Gaussian)
(mlx_random_truncated_normal -2.0 2.0 (integers 100))

; Gumbel distribution (for Gumbel-softmax)
(mlx_random_gumbel (integers 10 5))

; Categorical (sample from discrete distribution)
(setq logits (mlx_array (floats 1.0 2.0 3.0)))
(mlx_random_categorical logits)             ; Single sample
(mlx_random_categorical logits 0 10)        ; 10 samples

; Laplace distribution
(mlx_random_laplace (integers 100) 0.0 1.0)  ; loc=0, scale=1

; Multivariate normal
(setq mean (mlx_array (floats 0.0 0.0)))
(setq cov (mlx_eye 2 2 "float32"))
(mlx_random_multivariate_normal mean cov (integers 100))
```

### Permutations and Shuffling

```lisp
; Random permutation
(setq perm (mlx_random_permutation 10))     ; Permutation of 0..9

; Shuffle array
(setq arr (mlx_arange 0 10 1))
(setq shuffled (mlx_random_permutation arr))

; Shuffle along specific axis
(setq matrix (mlx_reshape (mlx_arange 0 12 1) (integers 3 4)))
(mlx_random_permutation matrix 0)           ; Shuffle rows
(mlx_random_permutation matrix 1)           ; Shuffle columns
```

---

## FFT Operations

### 1D FFT

```lisp
(setq signal (mlx_random_normal (integers 128)))

; Forward FFT
(setq spectrum (mlx_fft signal))
(setq spectrum (mlx_fft signal 128 0))  ; n=128, axis=0

; Inverse FFT
(setq reconstructed (mlx_ifft spectrum))

; Real-valued FFT (more efficient for real signals)
(setq rfft_result (mlx_rfft signal))
(setq irfft_result (mlx_irfft rfft_result))
```

### 2D FFT

```lisp
(setq image (mlx_random_normal (integers 64 64)))

; 2D FFT
(setq fft2_result (mlx_fft2 image))
(setq ifft2_result (mlx_ifft2 fft2_result))

; Real-valued 2D FFT
(mlx_rfft2 image)
(mlx_irfft2 rfft2_result)
```

### N-D FFT

```lisp
(setq data (mlx_random_normal (integers 32 32 32)))

; N-dimensional FFT
(mlx_fftn data)
(mlx_ifftn data)

; Real-valued N-D FFT
(mlx_rfftn data)
(mlx_irfftn data)
```

### FFT Shift

```lisp
; Shift zero-frequency component to center
(setq shifted (mlx_fftshift spectrum))

; Inverse shift
(setq unshifted (mlx_ifftshift shifted))
```

---

## Advanced Operations

### Sorting and Selection

```lisp
(setq a (mlx_array (floats 3.0 1.0 4.0 1.0 5.0 9.0)))

; Sort
(mlx_sort a)             ; → (1.0 1.0 3.0 4.0 5.0 9.0)
(mlx_sort a 0)           ; Along specific axis

; Argsort (indices that would sort)
(mlx_argsort a)          ; → (1 3 0 2 4 5)

; Top-k elements
(setq topk_result (mlx_topk a 3))  ; Top 3 values
(setq values (@ topk_result 0))
(setq indices (@ topk_result 1))

; Partition (partial sort)
(mlx_partition a 2)      ; Elements split around k-th smallest
```

### Where and Argwhere

```lisp
(setq a (mlx_array (floats -1.0 0.0 1.0 2.0 -2.0)))

; Find indices where condition is true
(setq positive_mask (mlx_greater a 0.0))
(setq indices (mlx_argwhere positive_mask))  ; → [[2] [3]]

; Conditional selection
(setq pos_or_zero (mlx_where positive_mask a (mlx_zeros_like a)))
```

### Gather and Scatter

```lisp
(setq a (mlx_reshape (mlx_arange 0 12 1) (integers 3 4)))
(setq indices (mlx_array (integers 0 2)))

; Take along axis
(mlx_take a indices 0)                    ; Take rows 0 and 2
(mlx_take_along_axis a indices 0)         ; Advanced take

; Scatter (inverse of gather)
(setq updates (mlx_ones (integers 2 4)))
(mlx_scatter a indices updates 0)

; Scatter with addition
(mlx_scatter_add a indices updates 0)
```

### Slicing

```lisp
(setq a (mlx_reshape (mlx_arange 0 24 1) (integers 2 3 4)))

; Basic slice
(setq s (mlx_slice a (integers 0 0 0) (integers 2 2 2)))

; With strides
(setq s (mlx_slice a (integers 0 0 0) (integers 2 3 4) (integers 1 1 2)))

; Update slice
(setq updated (mlx_slice_update a (mlx_zeros (integers 1 2 2)) 
                                  (integers 0 0 0) (integers 1 2 2)))
```

### NaN Handling

```lisp
(setq a (mlx_array (floats 1.0 nan inf -inf 2.0)))

; Replace special values
(mlx_nan_to_num a)           ; nan→0, inf→large, -inf→small
(mlx_nan_to_num a 0.0 1e10 -1e10)  ; Custom replacements
```

### Array Comparison

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))
(setq b (mlx_array (floats 1.0 2.0 3.0)))

; Exact equality
(mlx_array_equal a b)        ; → true

; Approximate equality (for floating point)
(mlx_allclose a b)           ; Default rtol=1e-5, atol=1e-8
(mlx_allclose a b 1e-3 1e-6) ; Custom tolerances
(mlx_allclose a b 1e-5 1e-8 true)  ; Treat NaN as equal
```

### Atleast Functions

```lisp
(setq scalar (mlx_array 5.0))

; Ensure minimum dimensions
(println (mlx_shape (mlx_atleast_1d scalar)))  ; → (1)
(println (mlx_shape (mlx_atleast_2d scalar)))  ; → (1 1)
(println (mlx_shape (mlx_atleast_3d scalar)))  ; → (1 1 1)
```

### Copy and Type Utilities

```lisp
(setq a (mlx_array (floats 1.0 2.0 3.0)))

; Deep copy
(setq b (mlx_copy a))

; Get dtype as string
(println (mlx_dtype a))      ; → "float32"

; Create arrays with same shape
(setq zeros (mlx_zeros_like a))
(setq ones (mlx_ones_like a))
```

---

## Real-World Examples

### Example 1: Simple Neural Network Layer

```lisp
(use 'lispe_mlx)

; Define a simple linear layer with ReLU
(defun linear_relu(x weight bias)
    (setq z (mlx_add (mlx_matmul x weight) bias))
    (mlx_relu z))

; Create random weights
(setq input_dim 784)
(setq hidden_dim 256)
(setq output_dim 10)

(setq W1 (mlx_multiply (mlx_random_normal (integers input_dim hidden_dim)) 0.01))
(setq b1 (mlx_zeros (integers hidden_dim)))
(setq W2 (mlx_multiply (mlx_random_normal (integers hidden_dim output_dim)) 0.01))
(setq b2 (mlx_zeros (integers output_dim)))

; Forward pass
(defun forward(x)
    (setq h (linear_relu x W1 b1))
    (mlx_softmax (mlx_add (mlx_matmul h W2) b2) -1))

; Test
(setq batch (mlx_random_normal (integers 32 784)))
(setq output (forward batch))
(println "Output shape:" (mlx_shape output))  ; → (32 10)
```

### Example 2: Attention Mechanism

```lisp
(use 'lispe_mlx)

; Multi-head self-attention
(defun self_attention(x num_heads)
    (setq shape (mlx_shape x))
    (setq B (@ shape 0))
    (setq L (@ shape 1))
    (setq D (@ shape 2))
    (setq head_dim (/ D num_heads))
    (setq scale (/ 1.0 (sqrt (float head_dim))))
    
    ; For simplicity, assume x is already projected to Q, K, V
    ; In practice, you'd have separate projection weights
    
    ; Reshape to (B, num_heads, L, head_dim)
    (setq x_heads (mlx_reshape x (integers B L num_heads head_dim)))
    (setq x_heads (mlx_transpose x_heads (integers 0 2 1 3)))
    
    ; Self-attention
    (setq attn_output (mlx_scaled_dot_product_attention 
                        x_heads x_heads x_heads scale "causal"))
    
    ; Reshape back
    (setq attn_output (mlx_transpose attn_output (integers 0 2 1 3)))
    (mlx_reshape attn_output (integers B L D)))

; Test
(setq x (mlx_random_normal (integers 2 10 64)))
(setq output (self_attention x 8))
(println "Attention output shape:" (mlx_shape output))
```

### Example 3: Loading and Using a Quantized Model

```lisp
(use 'lispe_mlx)

; Load quantized weights
(defun load_quantized_layer(tensors prefix)
    (list
        (@ tensors (+ prefix ".weight"))
        (@ tensors (+ prefix ".scales"))
        (@ tensors (+ prefix ".biases"))))

; Quantized linear projection
(defun qlinear(x weight scales biases (group_size 64) (bits 4))
    (mlx_quantized_matmul x weight scales biases true group_size bits))

; Load model
(setq result (mlx_load_safetensors "/path/to/model.safetensors"))
(setq tensors (@ result 0))

; Get layer weights
(setq q_proj (load_quantized_layer tensors "model.layers.0.self_attn.q_proj"))
(setq k_proj (load_quantized_layer tensors "model.layers.0.self_attn.k_proj"))
(setq v_proj (load_quantized_layer tensors "model.layers.0.self_attn.v_proj"))

; Forward pass
(setq hidden (mlx_random_normal (integers 1 10 512)))
(setq q (qlinear hidden (@ q_proj 0) (@ q_proj 1) (@ q_proj 2)))
(setq k (qlinear hidden (@ k_proj 0) (@ k_proj 1) (@ k_proj 2)))
(setq v (qlinear hidden (@ v_proj 0) (@ v_proj 1) (@ v_proj 2)))

(println "Q shape:" (mlx_shape q))
```

### Example 4: RMSNorm + RoPE (Transformer Components)

```lisp
(use 'lispe_mlx)

; Compute RoPE frequencies
(defun compute_rope_freqs(head_dim base max_seq_len)
    (setq half_dim (/ head_dim 2))
    (setq inv_freq (mlx_arange 0 half_dim 1 "float32"))
    (setq inv_freq (mlx_divide inv_freq (float half_dim)))
    (setq inv_freq (mlx_power (float base) inv_freq))
    (mlx_reciprocal inv_freq))

; Apply RoPE to queries/keys
(defun apply_rope(x freqs offset)
    (mlx_rope x (@ (mlx_shape x) 3) false 10000.0 1.0 offset freqs))

; Full transformer block (simplified)
(defun transformer_block(x norm_weight q_w q_s q_b freqs offset)
    ; RMSNorm
    (setq normed (mlx_rms_norm x norm_weight 1e-5))
    
    ; Project to Q, K, V (using same weights for simplicity)
    (setq qkv (mlx_quantized_matmul normed q_w q_s q_b true 64 4))
    
    ; Reshape and apply RoPE
    (setq shape (mlx_shape qkv))
    (setq B (@ shape 0))
    (setq L (@ shape 1))
    (setq qkv (mlx_reshape qkv (integers B L 8 64)))  ; 8 heads, 64 dim
    (setq qkv (mlx_transpose qkv (integers 0 2 1 3)))  ; (B, heads, L, dim)
    
    ; Apply RoPE
    (setq qkv_rope (apply_rope qkv freqs offset))
    
    ; Attention
    (setq scale (/ 1.0 (sqrt 64.0)))
    (setq attn (mlx_scaled_dot_product_attention qkv_rope qkv_rope qkv_rope scale "causal"))
    
    ; Reshape back and add residual
    (setq attn (mlx_transpose attn (integers 0 2 1 3)))
    (setq attn (mlx_reshape attn (integers B L 512)))
    (mlx_add x attn))

; Setup
(setq freqs (compute_rope_freqs 64 10000.0 4096))
(setq norm_weight (mlx_ones (integers 512)))
(setq q_w (mlx_random_normal (integers 128 512)))  ; Quantized weight shape
(setq q_s (mlx_ones (integers 64 8)))
(setq q_b (mlx_zeros (integers 512 64)))

; Test
(setq x (mlx_random_normal (integers 1 10 512)))
(setq output (transformer_block x norm_weight q_w q_s q_b freqs 0))
(println "Output shape:" (mlx_shape output))
```

### Example 5: Memory-Efficient Batch Processing

```lisp
(use 'lispe_mlx)

; Process large dataset in memory-efficient batches
(defun process_embeddings(data embed_weight batch_size)
    (setq num_samples (@ (mlx_shape data) 0))
    (setq num_batches (ceiling (/ num_samples batch_size)))
    (setq results (list))
    
    (loopcount num_batches i
        (setq start (* i batch_size))
        (setq end (min (+ start batch_size) num_samples))
        
        ; Extract batch
        (setq batch (mlx_slice data (integers start) (integers end)))
        
        ; Process (e.g., embedding lookup and mean pooling)
        (setq embedded (mlx_take embed_weight batch 0))
        (setq pooled (mlx_mean embedded (integers 1)))
        
        (push results pooled)
        
        ; Sync and clear cache every few batches
        (check (zerop (% i 5))
            (mlx_synchronize)
            (mlx_clear_cache))
        
        (println "Processed batch " (+ i 1) "/" num_batches))
    
    (mlx_concatenate results 0))

; Test
(setq vocab_size 32000)
(setq embed_dim 512)
(setq embed_weight (mlx_random_normal (integers vocab_size embed_dim)))
(setq token_ids (mlx_random_randint 0 vocab_size (integers 1000 128)))

(setq embeddings (process_embeddings token_ids embed_weight 100))
(println "Final embeddings shape:" (mlx_shape embeddings))
```

---

## Function Reference (Quick Lookup)

### Array Creation
| Function | Signature |
|----------|-----------|
| `mlx_array` | `(data (shape) (dtype))` |
| `mlx_zeros` | `(shape (dtype))` |
| `mlx_ones` | `(shape (dtype))` |
| `mlx_full` | `(shape value (dtype))` |
| `mlx_eye` | `(n (m) (dtype))` |
| `mlx_identity` | `(n (dtype))` |
| `mlx_arange` | `(start stop (step) (dtype))` |
| `mlx_linspace` | `(start stop num (dtype))` |
| `mlx_tri` | `(n (m) (k) (dtype))` |
| `mlx_tril` | `(array (k))` |
| `mlx_triu` | `(array (k))` |
| `mlx_diag` | `(array (k))` |
| `mlx_meshgrid` | `(arrays (indexing))` |

### Basic Operations
| Function | Signature |
|----------|-----------|
| `mlx_add` | `(a b)` |
| `mlx_subtract` | `(a b)` |
| `mlx_multiply` | `(a b)` |
| `mlx_divide` | `(a b)` |
| `mlx_power` | `(a b)` |
| `mlx_maximum` | `(a b)` |
| `mlx_minimum` | `(a b)` |
| `mlx_matmul` | `(a b)` |

### Reductions
| Function | Signature |
|----------|-----------|
| `mlx_sum` | `(array (axes) (keepdims))` |
| `mlx_mean` | `(array (axes) (keepdims))` |
| `mlx_prod` | `(array (axes) (keepdims))` |
| `mlx_max` | `(array (axes) (keepdims))` |
| `mlx_min` | `(array (axes) (keepdims))` |
| `mlx_var` | `(array (axes) (keepdims) (ddof))` |
| `mlx_std` | `(array (axes) (keepdims) (ddof))` |
| `mlx_argmax` | `(array (axis) (keepdims))` |
| `mlx_argmin` | `(array (axis) (keepdims))` |

### Shape Manipulation
| Function | Signature |
|----------|-----------|
| `mlx_reshape` | `(array shape)` |
| `mlx_transpose` | `(array (axes))` |
| `mlx_squeeze` | `(array (axes))` |
| `mlx_expand_dims` | `(array axis)` |
| `mlx_flatten` | `(array (start_axis) (end_axis))` |
| `mlx_concatenate` | `(arrays (axis))` |
| `mlx_stack` | `(arrays (axis))` |
| `mlx_split` | `(array indices_or_sections (axis))` |

### Deep Learning
| Function | Signature |
|----------|-----------|
| `mlx_rms_norm` | `(x (weight) (eps) (gemma_style))` |
| `mlx_layer_norm` | `(x (weight) (bias) (eps))` |
| `mlx_rope` | `(x dims (traditional) (base) (scale) (offset) (freqs))` |
| `mlx_scaled_dot_product_attention` | `(q k v scale (mask_mode) (mask_arrays) (sinks))` |
| `mlx_softmax` | `(array (axis))` |
| `mlx_relu` | `(array)` |
| `mlx_gelu` | `(array)` |
| `mlx_silu` | `(array)` |

### Quantization
| Function | Signature |
|----------|-----------|
| `mlx_quantize` | `(array (group_size) (bits))` |
| `mlx_dequantize` | `(w scales (biases) (group_size) (bits))` |
| `mlx_quantized_matmul` | `(x w scales (biases) (transposed) (group_size) (bits))` |

### I/O
| Function | Signature |
|----------|-----------|
| `mlx_load_safetensors` | `(file)` |
| `mlx_save_safetensors` | `(file arrays (metadata))` |
| `mlx_load_gguf` | `(file)` |
| `mlx_save_gguf` | `(file arrays (metadata))` |
| `mlx_load` | `(file)` |
| `mlx_save` | `(file array)` |

### Memory
| Function | Signature |
|----------|-----------|
| `mlx_synchronize` | `()` |
| `mlx_eval` | `(array)` |
| `mlx_clear_cache` | `()` |
| `mlx_get_active_memory` | `()` |
| `mlx_get_peak_memory` | `()` |
| `mlx_get_cache_memory` | `()` |
| `mlx_set_memory_limit` | `(limit)` |
| `mlx_set_cache_limit` | `(limit)` |

---

## Alphabetical Index

| Function | Signature | Description |
|----------|-----------|-------------|
| `mlx_abs` | `(array)` | Element-wise absolute value |
| `mlx_add` | `(a b)` | Add two arrays element-wise |
| `mlx_all` | `(array (axes) (keepdims))` | Test if all elements are true |
| `mlx_allclose` | `(a b (rtol) (atol) (equal_nan))` | Test if two arrays are element-wise close within tolerance |
| `mlx_any` | `(array (axes) (keepdims))` | Test if any element is true |
| `mlx_arange` | `(start stop (step) (dtype))` | Create array with evenly spaced values |
| `mlx_arccos` | `(array)` | Compute arc cosine element-wise |
| `mlx_arccosh` | `(array)` | Compute inverse hyperbolic cosine element-wise |
| `mlx_arcsin` | `(array)` | Compute arc sine element-wise |
| `mlx_arcsinh` | `(array)` | Compute inverse hyperbolic sine element-wise |
| `mlx_arctan` | `(array)` | Compute arc tangent element-wise |
| `mlx_arctan2` | `(a b)` | Element-wise arc tangent of a/b with correct quadrant |
| `mlx_arctanh` | `(array)` | Compute inverse hyperbolic tangent element-wise |
| `mlx_argmax` | `(array (axis) (keepdims))` | Return indices of maximum values along an axis |
| `mlx_argmin` | `(array (axis) (keepdims))` | Return indices of minimum values along an axis |
| `mlx_argsort` | `(array (axis))` | Indices that would sort array |
| `mlx_argwhere` | `(array)` | Indices where condition is true |
| `mlx_array` | `(data (shape) (dtype))` | Create an MLX array from numeric data |
| `mlx_array_equal` | `(a b (equal_nan))` | Test if two arrays are element-wise equal |
| `mlx_astype` | `(array dtype)` | Cast array to a different data type |
| `mlx_atleast_1d` | `(array)` | Convert input to at least 1-dimensional array |
| `mlx_atleast_2d` | `(array)` | Convert input to at least 2-dimensional array |
| `mlx_atleast_3d` | `(array)` | Convert input to at least 3-dimensional array |
| `mlx_bitwise_and` | `(a b)` | Element-wise bitwise AND |
| `mlx_bitwise_invert` | `(array)` | Element-wise bitwise NOT (complement) |
| `mlx_bitwise_or` | `(a b)` | Element-wise bitwise OR |
| `mlx_bitwise_xor` | `(a b)` | Element-wise bitwise XOR |
| `mlx_broadcast_to` | `(array shape)` | Broadcast array to shape |
| `mlx_ceil` | `(array)` | Round up to nearest integer element-wise |
| `mlx_cholesky` | `(array (upper))` | Compute Cholesky decomposition |
| `mlx_cholesky_inv` | `(array (upper))` | Compute inverse using Cholesky decomposition |
| `mlx_clear_cache` | `()` | Clear memory cache |
| `mlx_clip` | `(array min max)` | Clip array values to a range |
| `mlx_concatenate` | `(arrays (axis))` | Concatenate arrays along a specified axis |
| `mlx_conjugate` | `(array)` | Compute the complex conjugate |
| `mlx_conv1d` | `(input weight (stride) (padding) (dilation) (groups))` | 1D convolution with a filter |
| `mlx_conv2d` | `(input weight (stride) (padding) (dilation) (groups))` | 2D convolution with a filter |
| `mlx_conv3d` | `(input weight (stride) (padding) (dilation) (groups))` | 3D convolution with a filter |
| `mlx_conv_general` | `(input weight (stride) (padding_lo) (padding_hi) (kernel_dilation) (input_dilation) (groups) (flip))` | General N-D convolution |
| `mlx_conv_transpose1d` | `(input weight (stride) (padding) (dilation) (output_padding) (groups))` | 1D transposed convolution |
| `mlx_conv_transpose2d` | `(input weight (stride) (padding) (dilation) (output_padding) (groups))` | 2D transposed convolution |
| `mlx_conv_transpose3d` | `(input weight (stride) (padding) (dilation) (output_padding) (groups))` | 3D transposed convolution |
| `mlx_copy` | `(array)` | Create an explicit copy of an array |
| `mlx_cos` | `(array)` | Compute cosine element-wise |
| `mlx_cosh` | `(array)` | Compute hyperbolic cosine element-wise |
| `mlx_cross` | `(a b (axis))` | Compute cross product of two vectors |
| `mlx_cummax` | `(array (axis) (reverse) (inclusive))` | Cumulative maximum along an axis |
| `mlx_cummin` | `(array (axis) (reverse) (inclusive))` | Cumulative minimum along an axis |
| `mlx_cumprod` | `(array (axis) (reverse) (inclusive))` | Compute cumulative product |
| `mlx_cumsum` | `(array (axis) (reverse) (inclusive))` | Compute cumulative sum |
| `mlx_degrees` | `(array)` | Convert radians to degrees element-wise |
| `mlx_dequantize` | `(w scales (biases) (group_size) (bits))` | Dequantize a matrix |
| `mlx_diag` | `(array (k))` | Create diagonal matrix from vector |
| `mlx_diagonal` | `(array (offset) (axis1) (axis2))` | Extract diagonal from matrix |
| `mlx_divide` | `(a b)` | Divide two arrays element-wise |
| `mlx_divmod` | `(a b)` | Return quotient and remainder as a list |
| `mlx_dtype` | `(array)` | Get the data type of an array as a string |
| `mlx_eig` | `(array)` | Compute eigenvalues and eigenvectors |
| `mlx_eigh` | `(array (UPLO))` | Eigenvalues/eigenvectors of Hermitian matrix |
| `mlx_eigvals` | `(array)` | Compute eigenvalues only |
| `mlx_eigvalsh` | `(array (UPLO))` | Eigenvalues of Hermitian/symmetric matrix |
| `mlx_einsum` | `(subscripts operands)` | Einstein summation convention |
| `mlx_elu` | `(array (alpha))` | Apply ELU activation function |
| `mlx_equal` | `(a b)` | Element-wise equality comparison |
| `mlx_erf` | `(array)` | Compute Gauss error function element-wise |
| `mlx_erfinv` | `(array)` | Compute inverse error function element-wise |
| `mlx_eval` | `(array)` | Force evaluation of lazy computation |
| `mlx_exp` | `(array)` | Element-wise exponential |
| `mlx_expand_dims` | `(array axis)` | Add a dimension at specified axis |
| `mlx_expm1` | `(array)` | Compute exp(x)-1 element-wise |
| `mlx_eye` | `(n (m) (dtype))` | Create an identity matrix |
| `mlx_fft` | `(array (n) (axis))` | Compute 1D Fast Fourier Transform |
| `mlx_fft2` | `(array (s) (axes))` | Compute 2D Fast Fourier Transform |
| `mlx_fftn` | `(array (s) (axes))` | Compute N-dimensional FFT |
| `mlx_fftshift` | `(array (axes))` | Shift zero-frequency component to center |
| `mlx_flatten` | `(array (start_axis) (end_axis))` | Flatten array to 1D |
| `mlx_flip` | `(array (axes))` | Reverse array along axes |
| `mlx_floor` | `(array)` | Round down to nearest integer element-wise |
| `mlx_floor_divide` | `(a b)` | Element-wise floor division |
| `mlx_full` | `(shape value (dtype))` | Create array filled with a specified value |
| `mlx_fused_mlp` | `(x gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b (group_size) (bits))` | Fused MLP for LLMs |
| `mlx_fused_moe` | `(x expert_indices expert_weights gate_w gate_s gate_qb gate_lb up_w up_s up_qb up_lb down_w down_s down_qb down_lb num_experts experts_per_tok (group_size) (bits) (activation))` | Fused Mixture of Experts |
| `mlx_fused_moe_batch` | `(x expert_indices expert_weights ...)` | Batched fused MoE |
| `mlx_gather` | `(array indices axis slice_sizes)` | Gather array entries given indices and slices |
| `mlx_gelu` | `(array)` | Apply GELU activation function |
| `mlx_gelu_tanh` | `(array)` | Apply GELU with tanh approximation |
| `mlx_get_active_memory` | `()` | Get active memory in bytes |
| `mlx_get_cache_memory` | `()` | Get cache memory in bytes |
| `mlx_get_peak_memory` | `()` | Get peak memory in bytes |
| `mlx_greater` | `(a b)` | Element-wise greater than comparison |
| `mlx_greater_equal` | `(a b)` | Element-wise greater than or equal comparison |
| `mlx_hosvd` | `(tensor ranks)` | Higher-Order SVD decomposition |
| `mlx_identity` | `(n (dtype))` | Identity matrix (alias for eye) |
| `mlx_ifft` | `(array (n) (axis))` | Compute 1D Inverse FFT |
| `mlx_ifft2` | `(array (s) (axes))` | Compute 2D Inverse FFT |
| `mlx_ifftn` | `(array (s) (axes))` | Compute N-dimensional Inverse FFT |
| `mlx_ifftshift` | `(array (axes))` | Inverse of fftshift |
| `mlx_imag` | `(array)` | Extract imaginary part of complex array |
| `mlx_inner` | `(a b)` | Compute inner product |
| `mlx_inv` | `(array)` | Compute matrix inverse |
| `mlx_irfft` | `(array (n) (axis))` | Compute 1D Inverse Real FFT |
| `mlx_irfft2` | `(array (s) (axes))` | Compute 2D Inverse Real FFT |
| `mlx_irfftn` | `(array (s) (axes))` | Compute N-dimensional Inverse Real FFT |
| `mlx_isfinite` | `(array)` | Test for finite values |
| `mlx_isinf` | `(array)` | Test for infinite values |
| `mlx_isnan` | `(array)` | Test for NaN values |
| `mlx_khatri_rao_product` | `(a b)` | Khatri-Rao product (column-wise Kronecker) |
| `mlx_kron` | `(a b)` | Compute Kronecker product |
| `mlx_layer_norm` | `(x (weight) (bias) (eps))` | Layer normalization |
| `mlx_leaky_relu` | `(array (negative_slope))` | Apply Leaky ReLU activation |
| `mlx_left_shift` | `(a b)` | Element-wise left bit shift |
| `mlx_less` | `(a b)` | Element-wise less than comparison |
| `mlx_less_equal` | `(a b)` | Element-wise less than or equal comparison |
| `mlx_linspace` | `(start stop num (dtype))` | Create array with linearly spaced values |
| `mlx_load` | `(file)` | Load array from .npy file |
| `mlx_load_gguf` | `(file)` | Load arrays from .gguf file |
| `mlx_load_safetensors` | `(file)` | Load tensors from .safetensors file |
| `mlx_log` | `(array)` | Element-wise natural logarithm |
| `mlx_log10` | `(array)` | Compute base-10 logarithm element-wise |
| `mlx_log1p` | `(array)` | Compute log(1+x) element-wise |
| `mlx_log2` | `(array)` | Compute base-2 logarithm element-wise |
| `mlx_log_softmax` | `(array (axis))` | Apply log softmax function |
| `mlx_logaddexp` | `(a b)` | Compute log(exp(a)+exp(b)) numerically stable |
| `mlx_logcumsumexp` | `(array (axis) (reverse) (inclusive))` | Cumulative logsumexp along an axis |
| `mlx_logical_and` | `(a b)` | Element-wise logical AND |
| `mlx_logical_not` | `(array)` | Element-wise logical NOT |
| `mlx_logical_or` | `(a b)` | Element-wise logical OR |
| `mlx_logsumexp` | `(array (axes) (keepdims))` | Compute log(sum(exp(x))) numerically stable |
| `mlx_lu` | `(array)` | Compute LU decomposition |
| `mlx_lu_factor` | `(array)` | Compute LU factorization with pivoting |
| `mlx_matmul` | `(a b)` | Matrix multiplication |
| `mlx_max` | `(array (axes) (keepdims))` | Compute maximum of array elements |
| `mlx_maximum` | `(a b)` | Element-wise maximum |
| `mlx_mean` | `(array (axes) (keepdims))` | Compute mean along specified axes |
| `mlx_meshgrid` | `(arrays (indexing))` | Create coordinate matrices |
| `mlx_min` | `(array (axes) (keepdims))` | Compute minimum of array elements |
| `mlx_minimum` | `(a b)` | Element-wise minimum |
| `mlx_moveaxis` | `(array source destination)` | Move axes to new positions |
| `mlx_multiply` | `(a b)` | Multiply two arrays element-wise |
| `mlx_nan_to_num` | `(array (nan) (posinf) (neginf))` | Replace NaN and infinity values |
| `mlx_negative` | `(array)` | Negate array elements element-wise |
| `mlx_norm` | `(array (ord) (axes) (keepdims))` | Compute vector or matrix norm |
| `mlx_not_equal` | `(a b)` | Element-wise inequality comparison |
| `mlx_ones` | `(shape (dtype))` | Create array filled with ones |
| `mlx_ones_like` | `(array)` | Create array of ones with same shape |
| `mlx_outer` | `(a b)` | Compute outer product |
| `mlx_pad` | `(array pad_width (value))` | Pad array with values |
| `mlx_partition` | `(array kth (axis))` | Partition array around kth element |
| `mlx_pinv` | `(array)` | Compute Moore-Penrose pseudo-inverse |
| `mlx_power` | `(a b)` | Raise array elements to a power |
| `mlx_prod` | `(array (axes) (keepdims))` | Compute product of array elements |
| `mlx_put_along_axis` | `(array indices values axis)` | Put values at indices along an axis |
| `mlx_qr` | `(array)` | Compute QR decomposition |
| `mlx_quantize` | `(array (group_size) (bits))` | Quantize a matrix |
| `mlx_quantized_matmul` | `(x w scales (biases) (transposed) (group_size) (bits))` | Quantized matrix multiplication |
| `mlx_radians` | `(array)` | Convert degrees to radians element-wise |
| `mlx_random_bernoulli` | `(p (shape))` | Generate Bernoulli random samples |
| `mlx_random_categorical` | `(logits (axis) (num_samples) (shape))` | Generate categorical distribution samples |
| `mlx_random_gumbel` | `(shape (dtype))` | Generate Gumbel distribution samples |
| `mlx_random_laplace` | `(shape (loc) (scale) (dtype))` | Generate Laplace distribution samples |
| `mlx_random_multivariate_normal` | `(mean cov (shape) (dtype))` | Generate multivariate normal samples |
| `mlx_random_normal` | `(shape (mean) (std) (dtype))` | Generate normally distributed random values |
| `mlx_random_permutation` | `(x (axis))` | Generate random permutation |
| `mlx_random_randint` | `(low high shape (dtype))` | Generate random integers in [low, high) |
| `mlx_random_truncated_normal` | `(lower upper (shape) (dtype))` | Generate truncated normal samples |
| `mlx_random_uniform` | `(shape (low) (high) (dtype))` | Generate uniform random values |
| `mlx_real` | `(array)` | Extract real part of complex array |
| `mlx_reciprocal` | `(array)` | Compute 1/x element-wise |
| `mlx_relu` | `(array)` | Apply ReLU activation function |
| `mlx_remainder` | `(a b)` | Element-wise remainder of division |
| `mlx_repeat` | `(array repeats (axis))` | Repeat array elements along an axis |
| `mlx_reshape` | `(array shape)` | Reshape array to a new shape |
| `mlx_rfft` | `(array (n) (axis))` | Compute 1D Real FFT |
| `mlx_rfft2` | `(array (s) (axes))` | Compute 2D Real FFT |
| `mlx_rfftn` | `(array (s) (axes))` | Compute N-dimensional Real FFT |
| `mlx_right_shift` | `(a b)` | Element-wise right bit shift |
| `mlx_rms_norm` | `(x (weight) (eps) (gemma_style))` | RMS normalization (used in LLaMA) |
| `mlx_roll` | `(array shift (axis))` | Roll array elements along axis |
| `mlx_rope` | `(x dims (traditional) (base) (scale) (offset) (freqs))` | Rotary Position Embedding |
| `mlx_round` | `(array)` | Round to nearest integer element-wise |
| `mlx_rsqrt` | `(array)` | Compute 1/sqrt(x) element-wise |
| `mlx_save` | `(file array)` | Save array to .npy file |
| `mlx_save_gguf` | `(file arrays (metadata))` | Save arrays to .gguf file |
| `mlx_save_safetensors` | `(file arrays (metadata))` | Save tensors to .safetensors file |
| `mlx_scaled_dot_product_attention` | `(queries keys values scale (mask_mode) (mask_arrays) (sinks))` | Scaled dot-product attention |
| `mlx_scatter` | `(array indices updates axis)` | Scatter updates into array |
| `mlx_scatter_add` | `(array indices updates axis)` | Scatter and add updates |
| `mlx_selu` | `(array)` | Apply SELU activation function |
| `mlx_set_cache_limit` | `(limit)` | Set cache limit |
| `mlx_set_memory_limit` | `(limit)` | Set memory limit |
| `mlx_shape` | `(data)` | Create an MLX shape from numeric data |
| `mlx_sigmoid` | `(array)` | Apply sigmoid activation function |
| `mlx_sign` | `(array)` | Return element-wise sign (-1, 0, or 1) |
| `mlx_silu` | `(array)` | Apply SiLU/Swish activation function |
| `mlx_sin` | `(array)` | Compute sine element-wise |
| `mlx_sinh` | `(array)` | Compute hyperbolic sine element-wise |
| `mlx_slice` | `(array start stop (strides))` | Extract a slice from an array |
| `mlx_slice_update` | `(src update start stop (strides))` | Update a slice of the source array |
| `mlx_softmax` | `(array (axis))` | Apply softmax activation function |
| `mlx_solve` | `(a b)` | Solve linear system Ax = b |
| `mlx_solve_triangular` | `(a b (upper))` | Solve triangular system of linear equations |
| `mlx_sort` | `(array (axis))` | Sort array along axis |
| `mlx_split` | `(array indices_or_sections (axis))` | Split array into multiple sub-arrays |
| `mlx_sqrt` | `(array)` | Element-wise square root |
| `mlx_square` | `(array)` | Compute square of array elements |
| `mlx_squeeze` | `(array (axes))` | Remove dimensions of size 1 |
| `mlx_stack` | `(arrays (axis))` | Stack arrays along a new axis |
| `mlx_std` | `(array (axes) (keepdims) (ddof))` | Compute standard deviation |
| `mlx_subtract` | `(a b)` | Subtract two arrays element-wise |
| `mlx_sum` | `(array (axes) (keepdims))` | Sum all elements or along specified axes |
| `mlx_svd` | `(array)` | Compute singular value decomposition |
| `mlx_swapaxes` | `(array axis1 axis2)` | Swap two axes |
| `mlx_synchronize` | `()` | Synchronize GPU operations |
| `mlx_take` | `(array indices (axis))` | Extract elements using indices |
| `mlx_take_along_axis` | `(array indices axis)` | Take values along an axis at given indices |
| `mlx_tan` | `(array)` | Compute tangent element-wise |
| `mlx_tanh` | `(array)` | Apply hyperbolic tangent |
| `mlx_tensordot` | `(a b (axes))` | Compute tensor dot product |
| `mlx_tile` | `(array reps)` | Tile array by repeating along axes |
| `mlx_tolist` | `(array)` | Convert MLX array to LispE list |
| `mlx_topk` | `(array k (axis))` | Get top-k values |
| `mlx_trace` | `(array (offset) (axis1) (axis2))` | Compute matrix trace |
| `mlx_transpose` | `(array (axes))` | Transpose an array |
| `mlx_tri` | `(n (m) (k) (dtype))` | Lower triangular matrix of ones |
| `mlx_tri_inv` | `(array (upper))` | Compute inverse of triangular matrix |
| `mlx_tril` | `(array (k))` | Extract lower triangular part |
| `mlx_triu` | `(array (k))` | Extract upper triangular part |
| `mlx_tucker` | `(tensor ranks (max_iter) (tol))` | Tucker decomposition (HOOI algorithm) |
| `mlx_tucker_compression_ratio` | `(original_shape core factors)` | Compute Tucker compression ratio |
| `mlx_tucker_reconstruct` | `(core factors)` | Reconstruct tensor from Tucker decomposition |
| `mlx_var` | `(array (axes) (keepdims) (ddof))` | Compute variance |
| `mlx_where` | `(condition x y)` | Select elements based on condition |
| `mlx_zeros` | `(shape (dtype))` | Create array filled with zeros |
| `mlx_zeros_like` | `(array)` | Create array of zeros with same shape |

---

## Tips and Best Practices

### 1. Use Lazy Evaluation Wisely
MLX uses lazy evaluation - computations build a graph and only execute when needed. Call `mlx_synchronize` or `mlx_eval` to force execution at strategic points.

### 2. Prefer Fused Operations
For LLM inference, use `mlx_fused_mlp` and `mlx_fused_moe` instead of separate operations - they're significantly faster.

### 3. Batch Operations
Group operations to maximize GPU utilization. Process multiple sequences at once when possible.

### 4. Memory Management
- Clear cache periodically with `mlx_clear_cache`
- Monitor memory with `mlx_get_active_memory`
- Process large datasets in batches

### 5. Use Quantization
4-bit and 8-bit quantization provide massive memory savings with minimal quality loss. Always use `mlx_quantized_matmul` for quantized weights.

### 6. Avoid Python-style Loops
Use vectorized operations instead of loops over individual elements. MLX operations work on entire arrays efficiently.

---

**End of Manual**
