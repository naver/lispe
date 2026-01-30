# 🛠️ Developer Guide

> **LispETorch Development and Extension Reference**

This guide covers advanced development topics, internal architecture, and extension development for LispETorch.

## 🏗️ **Architecture Overview**

### Core Components

```cpp
// Main extension structure
LispETorch Extension
├── Tensor Operations (lispe_to_tensor, tensor_to_lispe)
├── Flash Attention Implementation (6 functions)
├── Text Generation System (TorchGenerator class)
├── Memory Management (borrow() optimization)
├── Model Integration (PyTorch C++ API)
└── Portable Installation System
```

### Key Design Principles

1. **🚀 Zero-Copy Optimization**: Use `borrow()` method when possible
2. **🔧 Type Safety**: Comprehensive LispE type checking
3. **💡 Memory Efficiency**: Flash Attention O(N) complexity
4. **🌐 Portability**: Self-contained dependency management
5. **⚡ Performance**: Direct PyTorch C++ API integration

## 📂 **Project Structure**

```
lispetorch/
├── src/
│   ├── lispetorch.cxx           # Main extension implementation
│   ├── tensor_operations.cxx     # Tensor conversion utilities
│   ├── flash_attention.cxx       # Flash Attention implementation
│   ├── text_generation.cxx       # TorchGenerator class
│   └── memory_utils.cxx          # Memory optimization utilities
├── include/
│   ├── lispetorch.h             # Main header
│   ├── tensor_utils.h           # Tensor utilities
│   ├── generation_config.h      # Generation configuration
│   └── flash_attention.h        # Flash Attention headers
├── examples/
│   ├── basic_usage.lisp         # Basic examples
│   ├── flash_attention.lisp     # Attention examples
│   ├── text_generation.lisp     # Generation examples
│   └── advanced_models.lisp     # Complex model examples
├── tests/
│   ├── test_tensors.lisp        # Tensor operation tests
│   ├── test_attention.lisp      # Flash Attention tests
│   ├── test_generation.lisp     # Text generation tests
│   └── test_memory.lisp         # Memory optimization tests
├── docs/
│   ├── README.md                # Main documentation
│   ├── flash_attention.md       # Flash Attention guide
│   ├── text_generation.md       # Generation guide
│   ├── portable_installation.md # Installation guide
│   └── developer_guide.md       # This document
└── Makefile                     # Build system
```

## 🔧 **Development Setup**

### Prerequisites
```bash
# Required tools
brew install cmake pkg-config     # macOS
sudo apt install cmake pkg-config # Ubuntu

# Python dependencies
pip install torch sentencepiece

# Build tools
xcode-select --install            # macOS
sudo apt install build-essential  # Ubuntu
```

### Development Environment
```bash
# Clone and setup
git clone <repository>
cd lispe/lispetorch

# Development build
make clean
make                    # Fast compilation for development
make test              # Run test suite

# Production build
make all               # Includes portable installation
```

### IDE Configuration

#### VS Code Settings
```json
{
  "C_Cpp.default.includePath": [
    "/usr/local/include",
    "/opt/homebrew/include", 
    "${workspaceFolder}/include",
    "${workspaceFolder}/../include"
  ],
  "C_Cpp.default.defines": [
    "MACOS",
    "TORCH_API_INCLUDE_EXTENSION_H"
  ]
}
```

#### Xcode Project
```bash
# Generate Xcode project
cd Xcode
open LispE.xcodeproj
# Add lispetorch target with proper frameworks
```

## 🧩 **Core Implementation Details**

### 1. Tensor Conversion System

```cpp
// Zero-copy optimization using borrow()
torch::Tensor lispe_to_tensor(Element* e) {
    if (e->isList()) {
        List* lst = (List*)e;
        if (lst->canBorrow()) {
            // Zero-copy: borrow the buffer
            auto borrowed = lst->borrow();
            return torch::from_blob(borrowed.data(), {borrowed.size()}, torch::kFloat32);
        }
    }
    // Fallback: copy data
    return create_tensor_copy(e);
}

// LispE list creation with proper memory management
Element* tensor_to_lispe(const torch::Tensor& tensor, LispE* lisp) {
    if (tensor.dtype() == torch::kFloat32) {
        auto data = tensor.contiguous().data_ptr<float>();
        auto size = tensor.numel();
        
        // Create LispE list with proper type
        Floats* result = lisp->provideFloats();
        result->reserve(size);
        
        for (int64_t i = 0; i < size; i++) {
            result->push_back(data[i]);
        }
        return result;
    }
    return lisp->provideNULL();
}
```

### 2. Flash Attention Implementation

```cpp
// Memory-efficient Flash Attention
torch::Tensor flash_attention_forward(
    const torch::Tensor& query,     // [B, H, N, D]
    const torch::Tensor& key,       // [B, H, N, D] 
    const torch::Tensor& value,     // [B, H, N, D]
    float scale,
    int block_size
) {
    auto [B, H, N, D] = query.sizes();
    auto output = torch::zeros_like(query);
    auto l = torch::zeros({B, H, N}, query.options());
    auto m = torch::full({B, H, N}, -INFINITY, query.options());
    
    // Process in blocks for O(N) memory complexity
    for (int start = 0; start < N; start += block_size) {
        int end = std::min(start + block_size, (int)N);
        
        // Block computation with memory optimization
        auto q_block = query.slice(2, start, end);
        auto k_block = key.slice(2, start, end);  
        auto v_block = value.slice(2, start, end);
        
        // Compute attention block
        compute_attention_block(q_block, k_block, v_block, output, l, m, scale);
    }
    
    return output;
}
```

### 3. Text Generation Architecture

```cpp
// Generation configuration structure
struct GenerationConfig {
    int max_length = 100;
    float temperature = 1.0f;
    int top_k = 50;
    float top_p = 0.9f;
    float repetition_penalty = 1.0f;
    int num_beams = 1;
    bool do_sample = true;
    torch::Device device = torch::kCPU;
};

// Main generator class
class TorchGenerator {
private:
    torch::jit::script::Module model_;
    GenerationConfig config_;
    std::vector<int64_t> generated_tokens_;
    
public:
    // Sampling strategies
    torch::Tensor greedy_sampling(const torch::Tensor& logits);
    torch::Tensor top_k_sampling(const torch::Tensor& logits, int k);
    torch::Tensor top_p_sampling(const torch::Tensor& logits, float p);
    torch::Tensor beam_search(const torch::Tensor& input_ids, int num_beams);
    
    // Main generation loop
    torch::Tensor generate(const torch::Tensor& input_ids);
};
```

## 🔍 **Function Registration System**

### Adding New Functions

```cpp
// 1. Declare function in header
Element* torch_new_function(LispE* lisp, Element* input_list);

// 2. Implement function
Element* torch_new_function(LispE* lisp, Element* input_list) {
    // Input validation
    if (input_list->size() != expected_args) {
        throw new Error("Error: torch_new_function expects N arguments");
    }
    
    // Extract arguments
    Element* arg1 = input_list->index(1);
    Element* arg2 = input_list->index(2);
    
    // Type checking
    if (!arg1->isTensor() || !arg2->isNumber()) {
        throw new Error("Error: Invalid argument types");
    }
    
    // Tensor operations
    torch::Tensor tensor1 = lispe_to_tensor(arg1);
    float scalar = arg2->asFloat();
    
    torch::Tensor result = tensor1 * scalar;
    
    // Return LispE element
    return tensor_to_lispe(result, lisp);
}

// 3. Register in extension initialization
case l_torch_new_function:
    return torch_new_function(lisp, e);
```

### Function ID Registration

```cpp
// Add to function enumeration
enum torch_functions {
    l_torch_tensor = 0,
    l_torch_add,
    l_torch_mul,
    l_torch_new_function,    // Add new function
    l_torch_last
};

// Update string-to-ID mapping
const char* torch_function_names[] = {
    "torch_tensor",
    "torch_add", 
    "torch_mul",
    "torch_new_function",    // Add name mapping
    NULL
};
```

## 🧪 **Testing Framework**

### Unit Test Structure

```lisp
; tests/test_new_feature.lisp
(use 'lispe_torch)

; Test setup
(setq test_data (torch_tensor (floats 1.0 2.0 3.0)))

; Test new function
(setq result (torch_new_function test_data 2.0))
(setq expected (floats 2.0 4.0 6.0))

; Assertions
(if (!= result expected)
    (throw "Test failed: torch_new_function")
    (print "✓ torch_new_function test passed"))
```

### Automated Testing

```bash
# Test runner script
#!/bin/bash
echo "🧪 Running LispETorch test suite..."

for test_file in tests/test_*.lisp; do
    echo "Running $(basename $test_file)..."
    ../bin/lispe $test_file
    if [ $? -ne 0 ]; then
        echo "❌ Test failed: $test_file"
        exit 1
    fi
done

echo "✅ All tests passed!"
```

## 🔧 **Memory Management**

### Ownership Patterns

```cpp
// Pattern 1: LispE owns the data
Element* create_owned_tensor(LispE* lisp, const std::vector<float>& data) {
    Floats* result = lisp->provideFloats();
    result->reserve(data.size());
    for (float val : data) {
        result->push_back(val);
    }
    return result;  // LispE will manage memory
}

// Pattern 2: Shared ownership with PyTorch
torch::Tensor create_shared_tensor(Element* e) {
    if (auto* floats = dynamic_cast<Floats*>(e)) {
        if (floats->canBorrow()) {
            auto borrowed = floats->borrow();
            // Create tensor sharing the buffer
            return torch::from_blob(borrowed.data(), {borrowed.size()});
        }
    }
    // Fallback to copy
    return copy_to_tensor(e);
}

// Pattern 3: RAII for complex objects
class TensorWrapper {
    torch::Tensor tensor_;
    bool owns_data_;
    
public:
    TensorWrapper(torch::Tensor t, bool owns = false) 
        : tensor_(t), owns_data_(owns) {}
    
    ~TensorWrapper() {
        if (owns_data_) {
            // Custom cleanup if needed
        }
    }
};
```

### Memory Leak Prevention

```cpp
// Use smart pointers for complex objects
std::unique_ptr<TorchGenerator> create_generator() {
    return std::make_unique<TorchGenerator>();
}

// Automatic cleanup in LispE functions
Element* torch_function_with_cleanup(LispE* lisp, Element* input) {
    try {
        // Function implementation
        auto result = perform_operation(input);
        return tensor_to_lispe(result, lisp);
    } catch (...) {
        // LispE will handle cleanup automatically
        throw;
    }
}
```

## 📊 **Performance Optimization**

### Profiling Tools

```cpp
// Timing utilities
#include <chrono>

class Timer {
    std::chrono::high_resolution_clock::time_point start_;
public:
    Timer() : start_(std::chrono::high_resolution_clock::now()) {}
    
    double elapsed() const {
        auto end = std::chrono::high_resolution_clock::now();
        return std::chrono::duration<double>(end - start_).count();
    }
};

// Usage in functions
Element* torch_optimized_function(LispE* lisp, Element* input) {
    Timer timer;
    
    // Function implementation
    auto result = perform_computation(input);
    
    if (timer.elapsed() > 1.0) {
        std::cout << "Warning: Function took " << timer.elapsed() << "s" << std::endl;
    }
    
    return result;
}
```

### Memory Pool Optimization

```cpp
// Pre-allocate common tensor sizes
class TensorPool {
    std::unordered_map<std::vector<int64_t>, std::vector<torch::Tensor>> pools_;
    
public:
    torch::Tensor get_tensor(const std::vector<int64_t>& shape) {
        auto& pool = pools_[shape];
        if (!pool.empty()) {
            auto tensor = pool.back();
            pool.pop_back();
            return tensor;
        }
        return torch::zeros(shape);
    }
    
    void return_tensor(const torch::Tensor& tensor) {
        auto shape = tensor.sizes().vec();
        pools_[shape].push_back(tensor);
    }
};
```

## 🔄 **Build System Integration**

### Makefile Extension

```makefile
# Custom targets for development
.PHONY: dev test profile benchmark docs

# Development build with debug symbols
dev: CXXFLAGS += -g -O0 -DDEBUG
dev: $(TARGET)

# Run test suite
test: $(TARGET)
	@echo "Running test suite..."
	@for test in tests/test_*.lisp; do \
		echo "Testing $$test..."; \
		../bin/lispe $$test || exit 1; \
	done

# Performance profiling
profile: CXXFLAGS += -pg
profile: $(TARGET)
	../bin/lispe examples/benchmark.lisp
	gprof $(TARGET) gmon.out > profile_report.txt

# Benchmark suite
benchmark: $(TARGET)
	@echo "Running benchmarks..."
	../bin/lispe benchmarks/tensor_ops.lisp
	../bin/lispe benchmarks/flash_attention.lisp
	../bin/lispe benchmarks/text_generation.lisp

# Generate documentation
docs:
	doxygen Doxyfile
	cd docs && python generate_api_docs.py
```

### Continuous Integration

```yaml
# .github/workflows/ci.yml
name: LispETorch CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        
    steps:
    - uses: actions/checkout@v2
    
    - name: Install dependencies
      run: |
        if [ "$RUNNER_OS" == "macOS" ]; then
          brew install pytorch sentencepiece
        else
          pip install torch sentencepiece
        fi
    
    - name: Build and test
      run: |
        cd lispetorch
        make test
        make benchmark
```

## 📚 **Extension Guidelines**

### Best Practices

1. **🔍 Type Safety**: Always validate LispE element types
2. **⚡ Performance**: Use borrow() for zero-copy when possible
3. **🛡️ Error Handling**: Provide clear error messages
4. **📝 Documentation**: Document all public functions
5. **🧪 Testing**: Include comprehensive test cases

### Code Style

```cpp
// Function naming: lowercase with underscores
Element* torch_matrix_multiply(LispE* lisp, Element* input_list);

// Variable naming: snake_case for locals, camelCase for members
torch::Tensor input_tensor = lispe_to_tensor(input);
auto batchSize = input_tensor.size(0);

// Constants: UPPER_CASE
const int MAX_SEQUENCE_LENGTH = 2048;
const float DEFAULT_TEMPERATURE = 1.0f;

// Error messages: descriptive and actionable
throw new Error("Error: torch_attention expects 4 arguments (query, key, value, mask), got " + std::to_string(input_list->size() - 1));
```

### Documentation Standards

```cpp
/**
 * @brief Compute scaled dot-product attention with Flash Attention optimization
 * 
 * Implements memory-efficient attention computation with O(N) memory complexity
 * instead of O(N²). Processes attention in blocks to minimize memory usage.
 * 
 * @param query Query tensor [batch_size, num_heads, seq_len, head_dim]
 * @param key Key tensor [batch_size, num_heads, seq_len, head_dim]  
 * @param value Value tensor [batch_size, num_heads, seq_len, head_dim]
 * @param scale Scaling factor (typically 1/sqrt(head_dim))
 * @param block_size Block size for memory optimization (default: 64)
 * 
 * @return Attention output tensor [batch_size, num_heads, seq_len, head_dim]
 * 
 * @throws Error if tensor dimensions don't match
 * @throws Error if tensors are not on the same device
 * 
 * @example
 * ```lisp
 * (setq attention_out (torch_flash_attention query key value 0.125 64))
 * ```
 */
Element* torch_flash_attention(LispE* lisp, Element* input_list);
```

## 🚀 **Contributing**

### Development Workflow

1. **Fork & Clone**: Fork the repository and clone locally
2. **Feature Branch**: Create a feature branch for your changes
3. **Development**: Implement your feature with tests
4. **Testing**: Run full test suite and benchmarks
5. **Documentation**: Update relevant documentation
6. **Pull Request**: Submit PR with detailed description

### Submission Checklist

- [ ] ✅ Code compiles without warnings
- [ ] ✅ All existing tests pass
- [ ] ✅ New tests added for new functionality  
- [ ] ✅ Documentation updated
- [ ] ✅ Performance benchmarks run
- [ ] ✅ Memory leaks checked
- [ ] ✅ Cross-platform compatibility verified

---

This developer guide provides the foundation for extending and contributing to LispETorch. Happy coding! 🎉
