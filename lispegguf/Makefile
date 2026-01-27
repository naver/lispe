# Makefile for GGUF library based on llama.cpp
# Compatible with macOS and Linux
#
# This version uses llama.cpp natively for optimal performance with quantized models

include ../Makefile.in

# Basic configuration
COMPPLUSPLUS = clang++
CXXFLAGS = -std=c++17 -O3 -fPIC -w
INCLUDES = -Iinclude -I../include -I../src

# Library name
LIB_NAME = liblispe_gguf.so
LBINPATH = ../bin
SRC_DIR = src
LOBJPATH = ../objs

# llama.cpp paths
LLAMA_DIR = llama.cpp
LLAMA_REPO = https://github.com/ggerganov/llama.cpp.git
LLAMA_BUILD_DIR = $(LLAMA_DIR)/build
LLAMA_LIB_DIR = $(LLAMA_BUILD_DIR)/bin
LLAMA_INCLUDES = -I$(LLAMA_DIR)/include -I$(LLAMA_DIR)/ggml/include

# Auto-detect OS
UNAME_S := $(shell uname -s)
ARCH := $(shell uname -m)

# OS-specific configuration
ifeq ($(UNAME_S), Darwin)
    # macOS with Metal (dynamic libraries)
    CXXFLAGS += -D__APPLE__ -DGGML_USE_METAL -DGGML_VERSION=\"1.0.0\" -DGGML_COMMIT=\"unknown\"
    LDFLAGS = -shared -undefined dynamic_lookup
    LLAMA_LIBS = -L$(LLAMA_LIB_DIR) -lllama -lggml -lggml-metal -lggml-cpu -lggml-base -lggml-blas
    LLAMA_FRAMEWORKS = -framework Foundation -framework Metal -framework MetalKit -framework Accelerate
    LLAMA_CMAKE_FLAGS = -DGGML_METAL=ON -DBUILD_SHARED_LIBS=ON
else ifeq ($(UNAME_S), Linux)
    # Linux with CUDA support (if available)
    CXXFLAGS += -D__linux__ -DGGML_VERSION=\"1.0.0\" -DGGML_COMMIT=\"unknown\"
    LDFLAGS = -shared -Wl,--no-undefined
    LLAMA_LIBS = -L$(LLAMA_LIB_DIR) -lllama -lggml
    LLAMA_FRAMEWORKS = -pthread
    # Try to detect CUDA
    CUDA_PATH ?= /usr/local/cuda
    ifneq ("$(wildcard $(CUDA_PATH))","")
        LLAMA_CMAKE_FLAGS = -DGGML_CUDA=ON -DBUILD_SHARED_LIBS=ON
    else
        LLAMA_CMAKE_FLAGS = -DBUILD_SHARED_LIBS=ON
    endif
endif

# Source files
SOURCES = lispe_gguf_llama.cxx

# Object files
OBJECTS = $(SOURCES:%.cxx=$(LOBJPATH)/lispegguf/%.o)

# Final library
TARGET = $(LBINPATH)/$(LIB_NAME)

.PHONY: all clean install llama-cpp check-llama help

all: check-llama $(TARGET)

# Check if llama.cpp exists, clone and build if not
check-llama:
	@if [ ! -d "$(LLAMA_DIR)" ]; then \
		echo "llama.cpp not found. Cloning repository..."; \
		git clone $(LLAMA_REPO) $(LLAMA_DIR); \
		echo "✓ llama.cpp cloned"; \
	fi
	@if [ ! -f "$(LLAMA_LIB_DIR)/libllama.dylib" ] && [ ! -f "$(LLAMA_LIB_DIR)/libllama.so" ]; then \
		echo "llama.cpp libraries not found. Building..."; \
		$(MAKE) llama-cpp; \
	fi

# Build llama.cpp with appropriate options
llama-cpp:
	@echo "Building llama.cpp..."
	@mkdir -p $(LLAMA_BUILD_DIR)
	@cd $(LLAMA_BUILD_DIR) && cmake .. $(LLAMA_CMAKE_FLAGS)
	@cd $(LLAMA_BUILD_DIR) && cmake --build . --config Release
ifeq ($(UNAME_S), Darwin)
	@echo "✓ llama.cpp built with Metal support"
else
	@echo "✓ llama.cpp built"
endif

$(TARGET): $(OBJECTS)
	@echo "Linking $(LIB_NAME)..."
	$(COMPPLUSPLUS) $(LDFLAGS) $(OBJECTS) $(LLAMA_LIBS) $(LLAMA_FRAMEWORKS) -o $@
	@echo "✓ Library created: $(TARGET)"
ifeq ($(UNAME_S), Darwin)
	@echo "Replacing @rpath with absolute paths for macOS..."
	@for lib in `otool -L $@ | grep '@rpath' | awk '{print $$1}'`; do \
		lib_name=`basename $$lib | sed 's/@rpath\///'`; \
		echo "  Replacing: $$lib -> /usr/local/lib/lispe/$$lib_name"; \
		install_name_tool -change $$lib /usr/local/lib/lispe/$$lib_name $@; \
	done
	@echo "✓ Paths modified"
endif
	@echo "NOTE: llama.cpp libraries must be accessible at runtime"
	@echo "      They are located in: $(LLAMA_LIB_DIR)"

$(LOBJPATH)/lispegguf/%.o: $(SRC_DIR)/%.cxx
	@mkdir -p $(LOBJPATH)/lispegguf
	@echo "Compiling $<..."
	$(COMPPLUSPLUS) $(CXXFLAGS) $(INCLUDES) $(LLAMA_INCLUDES) -c $< -o $@

clean:
	rm -rf $(LOBJPATH)/lispegguf
	rm -f $(TARGET)
	@echo "Cleanup complete"

clean-all: clean
	@if [ -d "$(LLAMA_BUILD_DIR)" ]; then \
		rm -rf $(LLAMA_BUILD_DIR); \
		echo "llama.cpp build cleaned"; \
	fi
	@echo "Complete cleanup done"

install: all
	@echo "Library installed in: $(LBINPATH)"
	@echo ""
	@echo "Usage:"
	@echo "  (use 'lispe_gguf)"
	@echo "  ; Load with GPU by default"
	@echo "  (setq model (gguf_load \"/path/to/model.gguf\"))"
	@echo ""
	@echo "  ; With custom configuration"
	@echo "  (setq model (gguf_load \"/path/to/model.gguf\" {\"n_gpu_layers\":99 \"n_ctx\":2048}))"
	@echo ""
	@echo "  ; With KV-cache options (experimental)"
	@echo "  (setq model (gguf_load \"/path/to/model.gguf\" {"
	@echo "      \"n_ctx\":4096"
	@echo "      \"cache_type_k\":\"q8_0\""
	@echo "      \"cache_type_v\":\"q8_0\""
	@echo "      \"flash_attn\":true"
	@echo "  }))"
	@echo ""
	@echo "  ; Generate text"
	@echo "  (gguf_generate model \"Hello world\" {\"max_tokens\":100 \"temperature\":0.8})"
	@echo ""
	@echo "Optional: Install llama.cpp libraries to /usr/local/lib/lispe/"
ifeq ($(UNAME_S), Darwin)
	@echo "  sudo mkdir -p /usr/local/lib/lispe"
	@echo "  sudo cp $(LLAMA_LIB_DIR)/*.dylib /usr/local/lib/lispe/"
else
	@echo "  sudo mkdir -p /usr/local/lib/lispe"
	@echo "  sudo cp $(LLAMA_LIB_DIR)/*.so /usr/local/lib/lispe/"
endif

# Help
help:
	@echo "Makefile for GGUF with llama.cpp"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Check/build llama.cpp and compile LispE library"
	@echo "  llama-cpp  - Build llama.cpp libraries"
	@echo "  clean      - Clean object files"
	@echo "  clean-all  - Clean everything including llama.cpp build"
	@echo "  install    - Compile and display usage instructions"
	@echo "  help       - Display this help"
	@echo ""
	@echo "Configuration:"
ifeq ($(UNAME_S), Darwin)
	@echo "  Platform: macOS (Metal GPU acceleration)"
else
	@echo "  Platform: Linux (CUDA GPU acceleration if available)"
endif
	@echo "  llama.cpp: $(LLAMA_DIR)"
	@echo ""
	@echo "Features:"
	@echo "  - Automatic llama.cpp download and build"
	@echo "  - GPU acceleration (Metal on macOS, CUDA on Linux)"
	@echo "  - Dynamic library linking"
	@echo "  - Dictionary-based configuration"

