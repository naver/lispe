#!/bin/bash
#
# Script d'installation portable pour LispETorch
# Ce script installe LispETorch avec toutes ses dépendances dans /usr/local/lib/lispe
# pour une distribution portable sur macOS
#

set -e

echo "=============================================="
echo "Installation portable de LispETorch"
echo "=============================================="

# Vérification des prérequis
echo "1. Vérification des prérequis..."

# Vérifier que nous sommes dans le bon répertoire
if [ ! -f "Makefile" ] || [ ! -f "src/lispe_lispetorch.cxx" ]; then
    echo "❌ Erreur: Ce script doit être exécuté depuis le répertoire lispetorch/"
    exit 1
fi

# Vérifier les permissions sudo
if ! sudo -n true 2>/dev/null; then
    echo "ℹ️  Ce script nécessite des privilèges sudo pour installer dans /usr/local/lib/lispe"
    echo "   Vous serez invité à saisir votre mot de passe."
    sudo -v
fi

# Vérification de l'environnement de développement
echo "2. Vérification de l'environnement..."

# Vérifier make
if ! command -v make >/dev/null 2>&1; then
    echo "❌ make n'est pas installé. Installez les outils de développement Xcode."
    exit 1
fi

# Vérifier g++/clang++
if ! command -v g++ >/dev/null 2>&1 && ! command -v clang++ >/dev/null 2>&1; then
    echo "❌ Aucun compilateur C++ trouvé. Installez les outils de développement Xcode."
    exit 1
fi

# Compilation
echo "3. Compilation de LispETorch..."
make clean
if ! make all; then
    echo "❌ Erreur lors de la compilation"
    echo "Vérifiez que PyTorch est installé dans votre environnement conda/Python"
    echo "Exécutez 'make check-deps' pour diagnostiquer les problèmes"
    exit 1
fi

echo "✅ Compilation réussie"

# Installation portable
echo "4. Installation portable..."
if ! make install-portable; then
    echo "❌ Erreur lors de l'installation portable"
    exit 1
fi

# Vérification
echo "5. Vérification de l'installation..."
make check-portable

# Instructions post-installation
echo ""
echo "=============================================="
echo "✅ Installation portable terminée avec succès!"
echo "=============================================="
echo ""
echo "LispETorch et toutes ses dépendances ont été installées dans:"
echo "  /usr/local/lib/lispe/"
echo ""
echo "Pour utiliser LispETorch depuis LispE:"
echo "  1. Assurez-vous que /usr/local/lib/lispe est dans votre PATH ou LD_LIBRARY_PATH"
echo "  2. Dans LispE, chargez l'extension avec:"
echo "     (use 'torch)"
echo ""
echo "Pour distribuer LispETorch sur d'autres machines macOS:"
echo "  1. Copiez le répertoire /usr/local/lib/lispe/"
echo "  2. Installez-le au même emplacement sur les machines cibles"
echo "  3. Aucune installation PyTorch supplémentaire requise!"
echo ""
echo "Pour désinstaller:"
echo "  make uninstall-portable"
echo ""
echo "Fichiers installés:"
ls -la /usr/local/lib/lispe/ 2>/dev/null || echo "Répertoire non accessible"
