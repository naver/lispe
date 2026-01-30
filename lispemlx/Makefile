# Makefile for MLX library for LispE
# Compatible with macOS (Apple Silicon)
#
# This version uses MLX (Apple) for loading and running MLX-format LLMs

include ../Makefile.in

# Basic configuration
COMPPLUSPLUS = clang++
CXXFLAGS = -std=c++17 -O3 -fPIC -w

# MLX paths
MLX_PATH = /opt/homebrew/Cellar/mlx/0.29.3
MLX_INCLUDE = $(MLX_PATH)/include
MLX_LIB = $(MLX_PATH)/lib

# nlohmann/json - install with: brew install nlohmann-json
JSON_INCLUDE = /opt/homebrew/include

# LispE includes
LISPE_INCLUDE = ../include

INCLUDES = -I$(LISPE_INCLUDE) -I../include -I../src -I$(MLX_INCLUDE) -I$(JSON_INCLUDE)

# Library name
LIB_NAME = liblispe_mlx.so
LBINPATH = ../bin
SRC_DIR = src
LOBJPATH = ../objs

# Linking
LDFLAGS = -shared -undefined dynamic_lookup
MLX_LIBS = -L$(MLX_LIB) -lmlx
MLX_FRAMEWORKS = -framework Metal -framework Foundation -framework Accelerate

# Source files
SOURCES = lispe_methods_mlx.cxx

# Object files
OBJECTS = $(SOURCES:%.cxx=$(LOBJPATH)/lispemlx/%.o)

# Final library
TARGET = $(LBINPATH)/$(LIB_NAME)

.PHONY: all clean install help

all: $(TARGET)

$(TARGET): $(OBJECTS)
	@echo "Linking $(LIB_NAME)..."
	$(COMPPLUSPLUS) $(LDFLAGS) $(OBJECTS) $(MLX_LIBS) $(MLX_FRAMEWORKS) -o $@
	@echo "✓ Library created: $(TARGET)"

$(LOBJPATH)/lispemlx/%.o: $(SRC_DIR)/%.cxx
	@mkdir -p $(LOBJPATH)/lispemlx
	@echo "Compiling $<..."
	$(COMPPLUSPLUS) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -rf $(LOBJPATH)/lispemlx
	rm -f $(TARGET)
	@echo "Cleanup complete"

install: all
	@echo "Library installed in: $(LBINPATH)"
	@echo ""
	@echo "Usage:"
	@echo "  (use 'lispe_mlx)"
	@echo ""
	@echo "  ; Load MLX model"
	@echo "  (setq model (mlx_load \"/path/to/mlx-model-directory\"))"
	@echo ""
	@echo "  ; Enable logging"
	@echo "  (mlx_set_log true)"
	@echo ""
	@echo "  ; Tokenize text"
	@echo "  (mlx_tokenize model \"Hello world\")"
	@echo ""
	@echo "  ; Detokenize tokens"
	@echo "  (mlx_detokenize model '(1 2 3))"
	@echo ""
	@echo "  ; Get model info"
	@echo "  (mlx_info model)"
	@echo ""
	@echo "Optional: Install MLX library to /usr/local/lib/lispe/"
	@echo "  sudo mkdir -p /usr/local/lib/lispe"
	@echo "  sudo cp $(MLX_LIB)/libmlx.dylib /usr/local/lib/lispe/"

help:
	@echo "Makefile for MLX LispE library"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Compile MLX library for LispE"
	@echo "  clean      - Clean object files"
	@echo "  install    - Compile and display usage instructions"
	@echo "  help       - Display this help"
	@echo ""
	@echo "Configuration:"
	@echo "  Platform: macOS (Apple Silicon with Metal)"
	@echo "  MLX: $(MLX_PATH)"
	@echo ""
	@echo "Requirements:"
	@echo "  - MLX: brew install mlx"
	@echo "  - nlohmann-json: brew install nlohmann-json"
