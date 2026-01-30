#!/bin/bash

echo "=== LispE PyTorch Dependency Fixer ==="

# Configuration
LISPE_TORCH_LIB="../bin/liblispe_torch.so"
PYTORCH_LIB="/Users/clauderoux/miniconda3/envs/entrainement/lib/python3.13/site-packages/torch/lib"

# Vérifier que la bibliothèque existe
if [[ ! -f "$LISPE_TORCH_LIB" ]]; then
    echo "ERROR: $LISPE_TORCH_LIB not found!"
    exit 1
fi

# Vérifier que PyTorch existe
if [[ ! -d "$PYTORCH_LIB" ]]; then
    echo "ERROR: PyTorch library directory not found at $PYTORCH_LIB"
    echo "Please check your conda environment and update the path"
    exit 1
fi

# Diagnostic initial
echo "=== Current dependencies ==="
otool -L "$LISPE_TORCH_LIB"

echo -e "\n=== Checking PyTorch libraries ==="
for lib in libtorch.dylib libtorch_cpu.dylib libc10.dylib; do
    if [[ -f "$PYTORCH_LIB/$lib" ]]; then
        echo "✅ Found: $lib"
    else
        echo "❌ Missing: $lib"
    fi
done

# Fix des dépendances
echo -e "\n=== Fixing dependencies ==="

# Méthode 1: Remplacer @rpath par chemin absolu
install_name_tool -change @rpath/libtorch.dylib "$PYTORCH_LIB/libtorch.dylib" "$LISPE_TORCH_LIB"
install_name_tool -change @rpath/libtorch_cpu.dylib "$PYTORCH_LIB/libtorch_cpu.dylib" "$LISPE_TORCH_LIB"
install_name_tool -change @rpath/libc10.dylib "$PYTORCH_LIB/libc10.dylib" "$LISPE_TORCH_LIB"

echo "Dependencies fixed!"

# Vérification finale
echo -e "\n=== Final verification ==="
otool -L "$LISPE_TORCH_LIB"

# Test de chargement
echo -e "\n=== Testing library loading ==="
cat > test_load.c << 'EOF'
#include <dlfcn.h>
#include <stdio.h>
int main() {
    void* handle = dlopen("../bin/liblispe_torch.so", RTLD_LAZY);
    if (!handle) {
        printf("❌ Error loading library: %s\n", dlerror());
        return 1;
    }
    printf("✅ Library loaded successfully!\n");
    dlclose(handle);
    return 0;
}
EOF

gcc -o test_load test_load.c -ldl
./test_load
rm test_load test_load.c

echo -e "\n=== Setup complete! ==="
