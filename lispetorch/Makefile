include ../Makefile.in
###################################################################
COMPPLUSPLUS = clang++
#------------------------------------------------------------
# Variables libtorch (détection automatique via Python/Conda)
# Détecter l'environnement conda s'il existe
CONDA_PREFIX = $(shell echo $$CONDA_PREFIX)
ifneq ($(CONDA_PREFIX),)
    PYTORCH_PATH = $(shell $(CONDA_PREFIX)/bin/python -c "import torch; import os; print(os.path.dirname(torch.__file__))" 2>/dev/null)
else
    # Essayer différentes commandes Python dans l'ordre de préférence
    PYTORCH_PATH = $(shell python3 -c "import torch; import os; print(os.path.dirname(torch.__file__))" 2>/dev/null || \
                   python -c "import torch; import os; print(os.path.dirname(torch.__file__))" 2>/dev/null || \
                   /opt/homebrew/bin/python3 -c "import torch; import os; print(os.path.dirname(torch.__file__))" 2>/dev/null)
endif
ifeq ($(PYTORCH_PATH),)
    $(error PyTorch not found. Please activate conda environment or install PyTorch: pip install torch or conda install pytorch)
endif
LIBTORCH_INCLUDE = $(PYTORCH_PATH)/include
LIBTORCH_LIB = $(PYTORCH_PATH)/lib


# Variables CUDA (détection automatique)
CUDA_PATH = $(shell find /usr/local/cuda* -maxdepth 0 -type d 2>/dev/null | head -1)
ifeq ($(CUDA_PATH),)
    CUDA_PATH = $(shell find /opt/cuda* -maxdepth 0 -type d 2>/dev/null | head -1)
endif

# Si CUDA est trouvé, ajouter les chemins et flags
ifneq ($(CUDA_PATH),)
    CUDA_INCLUDE = $(CUDA_PATH)/include
    CUDA_LIB = $(CUDA_PATH)/lib64
    CUDA_FLAGS = -DUSE_CUDA
    CUDA_LIBS = -L$(CUDA_LIB) -lcudart -lcublas -lcurand -lcusparse
else
    CUDA_FLAGS = 
    CUDA_LIBS = 
endif

# Variables SentencePiece (détection automatique avec brew)
SENTENCEPIECE_PATH = $(shell /opt/homebrew/bin/brew --prefix sentencepiece 2>/dev/null)
ifneq ($(SENTENCEPIECE_PATH),)
    SENTENCEPIECE_INCLUDE = $(SENTENCEPIECE_PATH)/include
    SENTENCEPIECE_LIB = $(SENTENCEPIECE_PATH)/lib
    SENTENCEPIECE_FLAGS = -DUSE_SENTENCEPIECE
    SENTENCEPIECE_LIBS = -L$(SENTENCEPIECE_LIB) -lsentencepiece -lsentencepiece_train
else
    SENTENCEPIECE_FLAGS = 
    SENTENCEPIECE_LIBS = 
endif

# Variables HuggingFace (utilisation des safetensors via PyTorch)
HF_FLAGS = -DUSE_HUGGINGFACE

# Sources LispETorch (sans MLX)
sourcelispetorch = lispetorch.cxx torchjit.cxx torchtensors.cxx torchextensions.cxx torchdefinitions.cxx lispe_hf_loader.cxx lispe_hf_loader_manual.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectlispetorch = $(sourcelispetorch:%.cxx=$(LOBJPATH)/lispetorch/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++17 -fPIC -w -c $(COPTION) -DUNIX -D_GLIBCXX_USE_CXX11_ABI=0 $(CUDA_FLAGS) $(SENTENCEPIECE_FLAGS) $(HF_FLAGS)
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude -I$(LIBTORCH_INCLUDE) -I$(LIBTORCH_INCLUDE)/torch/csrc/api/include $(if $(CUDA_PATH),-I$(CUDA_INCLUDE),) $(if $(SENTENCEPIECE_PATH),-I$(SENTENCEPIECE_INCLUDE),)
#------------------------------------------------------------
# Modification : utiliser les chemins absolus PyTorch au lieu de @rpath
LIBTORCH_LIBS = -L$(LIBTORCH_LIB) -ltorch -ltorch_cpu -lc10 $(CUDA_LIBS) $(SENTENCEPIECE_LIBS)
# Flags de linkage pour fixer les chemins des bibliothèques PyTorch
PYTORCH_RPATH_FLAGS = -Wl,-rpath,$(LIBTORCH_LIB)
# Flags pour permettre les modifications ultérieures de rpath (installation portable)
RPATH_PADDING_FLAGS = -Wl,-headerpad_max_install_names
# Alternative: forcer les chemins absolus
PYTORCH_ABS_FLAGS = -Wl,-change,@rpath/libtorch.dylib,$(LIBTORCH_LIB)/libtorch.dylib \
                   -Wl,-change,@rpath/libtorch_cpu.dylib,$(LIBTORCH_LIB)/libtorch_cpu.dylib \
                   -Wl,-change,@rpath/libc10.dylib,$(LIBTORCH_LIB)/libc10.dylib
#------------------------------------------------------------
$(LOBJPATH)/lispetorch/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectlispetorch)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_torch.so $(objectlispetorch) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST) $(LIBTORCH_LIBS) $(PYTORCH_RPATH_FLAGS) $(RPATH_PADDING_FLAGS)
	@echo "=== Compilation terminée, installation portable automatique ==="
	$(MAKE) install-portable

# Règle pour compiler uniquement (sans installation portable)
all-build-only: install $(objectlispetorch)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_torch.so $(objectlispetorch) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST) $(LIBTORCH_LIBS) $(PYTORCH_RPATH_FLAGS) $(RPATH_PADDING_FLAGS)
	@echo "=== Compilation terminée (sans installation portable) ==="

# Règle alternative avec chemins absolus (si rpath ne fonctionne pas)
all-abs: install $(objectlispetorch)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_torch.so $(objectlispetorch) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST) $(LIBTORCH_LIBS)
	install_name_tool -change @rpath/libtorch.dylib $(LIBTORCH_LIB)/libtorch.dylib $(LBINPATH)/liblispe_torch.so
	install_name_tool -change @rpath/libtorch_cpu.dylib $(LIBTORCH_LIB)/libtorch_cpu.dylib $(LBINPATH)/liblispe_torch.so
	install_name_tool -change @rpath/libc10.dylib $(LIBTORCH_LIB)/libc10.dylib $(LBINPATH)/liblispe_torch.so

# Règle pour diagnostic
check-deps:
	@echo "=== Environment Detection ==="
	@if [ -n "$(CONDA_PREFIX)" ]; then \
		echo "Conda environment: $(CONDA_PREFIX)"; \
	else \
		echo "No conda environment detected. Consider using: conda activate your-pytorch-env"; \
	fi
	@echo "=== PyTorch Detection ==="
	@echo "Python version: $(shell python --version 2>/dev/null || echo 'Python not found')"
	@echo "PyTorch path: $(PYTORCH_PATH)"
	@echo "PyTorch version: $(shell python -c "import torch; print(torch.__version__)" 2>/dev/null || echo 'PyTorch not found')"
	@echo "=== Checking PyTorch libraries ==="
	@ls -la $(LIBTORCH_LIB)/libtorch*.dylib $(LIBTORCH_LIB)/libc10.dylib 2>/dev/null || echo "PyTorch libraries not found"
	@echo "=== Checking SentencePiece ==="
	@if [ -n "$(SENTENCEPIECE_PATH)" ]; then \
		echo "SentencePiece found at: $(SENTENCEPIECE_PATH)"; \
		ls -la $(SENTENCEPIECE_LIB)/libsentencepiece*.dylib 2>/dev/null || echo "SentencePiece libraries not found"; \
	else \
		echo "SentencePiece not found. Install with: /opt/homebrew/bin/brew install sentencepiece"; \
	fi
	@echo "=== Current library dependencies ==="
	@if [ -f $(LBINPATH)/liblispe_torch.so ]; then otool -L $(LBINPATH)/liblispe_torch.so; else echo "Library not built yet"; fi

# Nouvelle règle pour aider avec la configuration conda
help-conda:
	@echo "=== Conda Setup Help ==="
	@echo "If PyTorch is not found, try:"
	@echo "1. Install conda/miniconda"
	@echo "2. Create environment: conda create -n pytorch python=3.11"
	@echo "3. Activate: conda activate pytorch"
	@echo "4. Install PyTorch: conda install pytorch torchvision torchaudio -c pytorch"
	@echo "5. Build: make all"
	@echo ""
	@echo "For Apple Silicon (M1/M2):"
	@echo "conda install pytorch torchvision torchaudio -c pytorch"


install:
	mkdir -p $(LOBJPATH)/lispetorch

clean:
	rm -f $(LOBJPATH)/lispetorch/*.o

# Règle pour nettoyer et reconstruire avec diagnostic
rebuild: clean all check-deps

# Installation portable dans /usr/local/lib/lispe
PORTABLE_LIB_PATH = /usr/local/lib/lispe

install-portable: all-build-only copy-dependencies fix-rpaths
	@echo "=== Installation portable terminée ==="
	@echo "LispETorch installé avec toutes les dépendances dans $(PORTABLE_LIB_PATH)"
	@echo "La bibliothèque peut maintenant être distribuée de manière portable."

copy-dependencies:
	@echo "=== Copie des dépendances PyTorch dans $(PORTABLE_LIB_PATH) ==="
	@sudo mkdir -p $(PORTABLE_LIB_PATH)
	@# Copie des bibliothèques PyTorch essentielles
	@if [ -f $(LIBTORCH_LIB)/libtorch.dylib ]; then \
		sudo cp $(LIBTORCH_LIB)/libtorch.dylib $(PORTABLE_LIB_PATH)/ && \
		echo "✓ libtorch.dylib copié"; \
	fi
	@if [ -f $(LIBTORCH_LIB)/libtorch_cpu.dylib ]; then \
		sudo cp $(LIBTORCH_LIB)/libtorch_cpu.dylib $(PORTABLE_LIB_PATH)/ && \
		echo "✓ libtorch_cpu.dylib copié"; \
	fi
	@if [ -f $(LIBTORCH_LIB)/libc10.dylib ]; then \
		sudo cp $(LIBTORCH_LIB)/libc10.dylib $(PORTABLE_LIB_PATH)/ && \
		echo "✓ libc10.dylib copié"; \
	fi
	@# Copie des bibliothèques CUDA si disponibles
	@if [ -n "$(CUDA_LIBS)" ] && [ -f $(LIBTORCH_LIB)/libtorch_cuda.dylib ]; then \
		sudo cp $(LIBTORCH_LIB)/libtorch_cuda.dylib $(PORTABLE_LIB_PATH)/ && \
		echo "✓ libtorch_cuda.dylib copié"; \
	fi
	@# Copie de SentencePiece si disponible
	@if [ -n "$(SENTENCEPIECE_PATH)" ]; then \
		if [ -f $(SENTENCEPIECE_LIB)/libsentencepiece.dylib ]; then \
			sudo cp $(SENTENCEPIECE_LIB)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/ && \
			echo "✓ libsentencepiece.dylib copié"; \
		fi; \
		if [ -f $(SENTENCEPIECE_LIB)/libsentencepiece_train.dylib ]; then \
			sudo cp $(SENTENCEPIECE_LIB)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/ && \
			echo "✓ libsentencepiece_train.dylib copié"; \
		fi; \
	fi
	@# Copie de la bibliothèque LispETorch elle-même
	@sudo cp $(LBINPATH)/liblispe_torch.so $(PORTABLE_LIB_PATH)/
	@echo "✓ liblispe_torch.so copié"

fix-rpaths:
	@echo "=== Modification des rpaths pour l'installation portable ==="
	@# Modification du rpath de la bibliothèque principale
	@sudo install_name_tool -add_rpath $(PORTABLE_LIB_PATH) $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || true
	@# Modification des dépendances PyTorch
	@if [ -f $(PORTABLE_LIB_PATH)/libtorch.dylib ]; then \
		sudo install_name_tool -change @rpath/libtorch.dylib $(PORTABLE_LIB_PATH)/libtorch.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change $(LIBTORCH_LIB)/libtorch.dylib $(PORTABLE_LIB_PATH)/libtorch.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so; \
		echo "✓ rpath libtorch.dylib mis à jour"; \
	fi
	@if [ -f $(PORTABLE_LIB_PATH)/libtorch_cpu.dylib ]; then \
		sudo install_name_tool -change @rpath/libtorch_cpu.dylib $(PORTABLE_LIB_PATH)/libtorch_cpu.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change $(LIBTORCH_LIB)/libtorch_cpu.dylib $(PORTABLE_LIB_PATH)/libtorch_cpu.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so; \
		echo "✓ rpath libtorch_cpu.dylib mis à jour"; \
	fi
	@if [ -f $(PORTABLE_LIB_PATH)/libc10.dylib ]; then \
		sudo install_name_tool -change @rpath/libc10.dylib $(PORTABLE_LIB_PATH)/libc10.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change $(LIBTORCH_LIB)/libc10.dylib $(PORTABLE_LIB_PATH)/libc10.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so; \
		echo "✓ rpath libc10.dylib mis à jour"; \
	fi
	@# Modification des dépendances CUDA si présentes
	@if [ -f $(PORTABLE_LIB_PATH)/libtorch_cuda.dylib ]; then \
		sudo install_name_tool -change @rpath/libtorch_cuda.dylib $(PORTABLE_LIB_PATH)/libtorch_cuda.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change $(LIBTORCH_LIB)/libtorch_cuda.dylib $(PORTABLE_LIB_PATH)/libtorch_cuda.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so; \
		echo "✓ rpath libtorch_cuda.dylib mis à jour"; \
	fi
	@# Modification des dépendances SentencePiece si présentes
	@if [ -f $(PORTABLE_LIB_PATH)/libsentencepiece.dylib ]; then \
		echo "Traitement de libsentencepiece.dylib..."; \
		sudo install_name_tool -change @rpath/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change $(SENTENCEPIECE_LIB)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change /opt/homebrew/opt/sentencepiece/lib/libsentencepiece.0.dylib $(PORTABLE_LIB_PATH)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change /usr/local/opt/sentencepiece/lib/libsentencepiece.0.dylib $(PORTABLE_LIB_PATH)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || true; \
		echo "✓ rpath libsentencepiece.dylib mis à jour"; \
	fi
	@if [ -f $(PORTABLE_LIB_PATH)/libsentencepiece_train.dylib ]; then \
		echo "Traitement de libsentencepiece_train.dylib..."; \
		sudo install_name_tool -change @rpath/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change $(SENTENCEPIECE_LIB)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change /opt/homebrew/opt/sentencepiece/lib/libsentencepiece_train.0.dylib $(PORTABLE_LIB_PATH)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || \
		sudo install_name_tool -change /usr/local/opt/sentencepiece/lib/libsentencepiece_train.0.dylib $(PORTABLE_LIB_PATH)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || true; \
		echo "✓ rpath libsentencepiece_train.dylib mis à jour"; \
	fi
	@# Diagnostic et correction spécifique pour les chemins Homebrew détectés
	@echo "Diagnostic des chemins SentencePiece restants..."
	@if otool -L $(PORTABLE_LIB_PATH)/liblispe_torch.so | grep -q "homebrew.*sentencepiece"; then \
		echo "Correction des chemins Homebrew SentencePiece détectés..."; \
		sudo install_name_tool -change /opt/homebrew/opt/sentencepiece/lib/libsentencepiece.0.dylib $(PORTABLE_LIB_PATH)/libsentencepiece.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null && echo "✓ libsentencepiece.0.dylib corrigé" || true; \
		sudo install_name_tool -change /opt/homebrew/opt/sentencepiece/lib/libsentencepiece_train.0.dylib $(PORTABLE_LIB_PATH)/libsentencepiece_train.dylib $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null && echo "✓ libsentencepiece_train.0.dylib corrigé" || true; \
	else \
		echo "✓ Aucun chemin Homebrew SentencePiece détecté"; \
	fi
	@# Correction de l'ID de la bibliothèque principale
	@sudo install_name_tool -id $(PORTABLE_LIB_PATH)/liblispe_torch.so $(PORTABLE_LIB_PATH)/liblispe_torch.so 2>/dev/null || true
	@echo "✓ ID de liblispe_torch.so mis à jour"

# Vérification de l'installation portable
check-portable:
	@echo "=== Vérification de l'installation portable ==="
	@if [ -f $(PORTABLE_LIB_PATH)/liblispe_torch.so ]; then \
		echo "✓ LispETorch installé dans $(PORTABLE_LIB_PATH)"; \
		echo "Dépendances de liblispe_torch.so:"; \
		otool -L $(PORTABLE_LIB_PATH)/liblispe_torch.so; \
		echo ""; \
		echo "Contenu du répertoire $(PORTABLE_LIB_PATH):"; \
		ls -la $(PORTABLE_LIB_PATH)/; \
	else \
		echo "✗ LispETorch non trouvé dans $(PORTABLE_LIB_PATH)"; \
		echo "Exécutez d'abord: make install-portable"; \
	fi

# Désinstallation portable
uninstall-portable:
	@echo "=== Désinstallation de LispETorch portable ==="
	@if [ -d $(PORTABLE_LIB_PATH) ]; then \
		echo "Suppression de $(PORTABLE_LIB_PATH)..."; \
		sudo rm -rf $(PORTABLE_LIB_PATH); \
		echo "✓ Désinstallation terminée"; \
	else \
		echo "Aucune installation portable trouvée"; \
	fi

# Targets disponibles
help:
	@echo "=== LispETorch Makefile Targets ==="
	@echo "all              - Build and automatically install portable version"
	@echo "all-build-only   - Build library only (without portable installation)"
	@echo "all-abs          - Build with absolute paths (if rpath fails)"
	@echo "install-portable - Install LispETorch with all dependencies in /usr/local/lib/lispe"
	@echo "copy-dependencies - Copy PyTorch and SentencePiece libraries to portable location"
	@echo "fix-rpaths       - Fix library paths for portable installation"
	@echo "check-portable   - Verify portable installation"
	@echo "uninstall-portable - Remove portable installation"
	@echo "clean            - Remove object files"
	@echo "check-deps       - Check dependencies and library status"
	@echo "help-conda       - Show conda environment setup instructions"
	@echo "rebuild          - Clean, build, and check dependencies"
	@echo "help             - Show this help message"

.PHONY: all all-build-only all-abs clean check-deps help-conda rebuild help install-portable copy-dependencies fix-rpaths check-portable uninstall-portable

