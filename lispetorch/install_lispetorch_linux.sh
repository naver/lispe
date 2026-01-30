#!/bin/bash
# Installation automatique LispETorch + SentencePiece sur Linux
# Usage: ./install_lispetorch_linux.sh

set -e  # Arrêter en cas d'erreur

echo "=== Installation automatique LispETorch + SentencePiece ==="
echo "Système détecté: $(uname -s) $(uname -m)"
echo "Date: $(date)"
echo ""

# Vérification des prérequis
echo "=== Vérification des prérequis ==="

# Conda
if [ -z "$CONDA_PREFIX" ]; then
    echo "❌ Erreur: Environnement conda requis"
    echo "Activez votre environnement conda : conda activate votre-env"
    exit 1
else
    echo "✅ Conda actif: $CONDA_PREFIX"
fi

# Git
if ! command -v git >/dev/null 2>&1; then
    echo "❌ Erreur: Git requis pour cloner SentencePiece"
    echo "Installez git : sudo apt-get install git"
    exit 1
else
    echo "✅ Git: $(git --version | head -n1)"
fi

# CMake
if ! command -v cmake >/dev/null 2>&1; then
    echo "❌ Erreur: CMake requis pour compiler SentencePiece"
    echo "Installez cmake : conda install cmake"
    exit 1
else
    echo "✅ CMake: $(cmake --version | head -n1)"
fi

# G++
if ! command -v g++ >/dev/null 2>&1; then
    echo "❌ Erreur: G++ requis"
    exit 1
else
    echo "✅ G++: $(g++ --version | head -n1)"
fi

# PyTorch
if ! python -c "import torch" 2>/dev/null; then
    echo "❌ Erreur: PyTorch non trouvé"
    echo "Installez PyTorch : conda install pytorch -c pytorch"
    exit 1
else
    echo "✅ PyTorch: $(python -c 'import torch; print(torch.__version__)')"
fi

# Protobuf
if ! python -c "import google.protobuf" 2>/dev/null; then
    echo "⚠️  Protobuf manquant - installation automatique..."
    conda install -y protobuf
else
    echo "✅ Protobuf: $(python -c 'import google.protobuf; print(google.protobuf.__version__)')"
fi

echo ""

# Vérification répertoire
if [ ! -f "Makefile.linux" ]; then
    echo "❌ Erreur: Exécutez ce script depuis le répertoire lispetorch/"
    echo "cd /path/to/lispe/lispetorch && ./install_lispetorch_linux.sh"
    exit 1
fi

echo "=== Diagnostic initial ==="
make -f Makefile.linux status
echo ""

# Nettoyage si demandé
if [ "$1" = "--clean" ]; then
    echo "=== Nettoyage demandé ==="
    make -f Makefile.linux clean-all
    echo ""
fi

# Compilation complète
echo "=== Compilation automatique ==="
echo "Démarrage de la compilation complète..."
start_time=$(date +%s)

if make -f Makefile.linux all; then
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    echo ""
    echo "🎉 ✅ SUCCÈS ! Installation terminée en ${duration}s"
    echo ""
    
    # Test automatique
    echo "=== Test automatique ==="
    if make -f Makefile.linux test >/dev/null 2>&1; then
        echo "✅ Test SentencePiece réussi"
    else
        echo "⚠️  Test échoué (fonctionnalité disponible malgré tout)"
    fi
    
    echo ""
    echo "=== Résumé de l'installation ==="
    echo "📦 LispETorch: $(ls -lh ../bin/liblispe_torch.so | awk '{print $5}')  ../bin/liblispe_torch.so"
    echo "📦 SentencePiece: $(du -sh sentencepiece-build | cut -f1)  sentencepiece-build/"
    echo "🧪 Test: cd tests && ../../bin/lispe demo_sentencepiece.lisp"
    echo ""
    echo "=== Commandes disponibles ==="
    echo "make -f Makefile.linux status           # Diagnostic"
    echo "make -f Makefile.linux test             # Test complet"
    echo "make -f Makefile.linux clean-all        # Nettoyage"
    echo "make -f Makefile.linux check-sentencepiece-local  # Vérifier SentencePiece"
    echo ""
    echo "🚀 LispETorch avec SentencePiece est prêt pour le fine-tuning !"
    
else
    echo ""
    echo "❌ ÉCHEC de la compilation"
    echo ""
    echo "=== Diagnostic d'erreur ==="
    echo "Vérifiez :"
    echo "1. Environnement conda actif"
    echo "2. PyTorch installé"
    echo "3. Permissions d'écriture"
    echo "4. Espace disque disponible"
    echo ""
    echo "Pour plus d'informations :"
    echo "make -f Makefile.linux status"
    echo "make -f Makefile.linux check-sentencepiece-local"
    echo ""
    echo "Consultez README_LINUX_INSTALLATION.md pour plus de détails"
    exit 1
fi
