# Guide d'installation LispETorch + SentencePiece sur Linux

## Vue d'ensemble

Ce guide décrit l'installation complète de LispETorch avec support SentencePiece sur Linux, utilisant une compilation locale pour éviter les conflits de dépendances.

## Architecture de la solution

```
lispetorch/
├── Makefile.linux                   # Makefile optimisé Linux
├── sentencepiece/                   # Code source SentencePiece (cloné automatiquement)
├── sentencepiece-build/             # Installation locale SentencePiece
│   ├── lib/libsentencepiece.so      # Bibliothèques principales
│   ├── include/                     # Headers C++
│   └── bin/spm_*                    # Utilitaires (encode, decode, train)
├── src/lispe_lispetorch.cxx         # Code source LispETorch
└── ../bin/liblispe_torch.so         # Bibliothèque finale
```

## Prérequis système

### Environnement obligatoire
- **Linux** (testé sur Red Hat, Ubuntu, CentOS)
- **Conda/Miniconda** activé
- **Git** pour cloner SentencePiece
- **CMake** ≥ 3.12 pour compiler SentencePiece
- **G++** avec support C++17

### Vérification des prérequis
```bash
# Vérifier conda
echo $CONDA_PREFIX

# Vérifier git et cmake
git --version
cmake --version

# Vérifier g++
g++ --version
```

## Installation des dépendances conda

### 1. Créer un environnement conda (optionnel)
```bash
# Créer un nouvel environnement
conda create -n lispetorch python=3.11
conda activate lispetorch
```

### 2. Installer PyTorch
```bash
# Pour GPU avec CUDA
conda install pytorch torchvision torchaudio pytorch-cuda=12.1 -c pytorch -c nvidia

# Pour CPU uniquement
conda install pytorch torchvision torchaudio cpuonly -c pytorch
```

### 3. Installer Protobuf (requis pour SentencePiece)
```bash
conda install protobuf
```

### 4. Vérifier l'installation
```bash
python -c "import torch; print('PyTorch:', torch.__version__)"
python -c "import google.protobuf; print('Protobuf:', google.protobuf.__version__)"
```

## Sources SentencePiece

### Dépôt officiel
- **URL**: https://github.com/google/sentencepiece
- **Licence**: Apache 2.0
- **Clonage automatique**: Le Makefile clone automatiquement la dernière version

### Compilation locale
La compilation se fait automatiquement lors du premier `make`, mais peut être lancée manuellement :

```bash
# Compilation manuelle de SentencePiece
make -f Makefile.linux compile-sentencepiece-local

# Vérification
make -f Makefile.linux check-sentencepiece-local
```

### Configuration cmake de SentencePiece
```cmake
-DCMAKE_INSTALL_PREFIX=./sentencepiece-build  # Installation locale
-DCMAKE_BUILD_TYPE=Release                    # Optimisations
-DSPM_USE_BUILTIN_PROTOBUF=OFF               # Utiliser protobuf système
-DCMAKE_CXX_STANDARD=17                      # Compatibilité C++17
-DCMAKE_CXX_FLAGS="-fPIC"                    # Position Independent Code
-DCMAKE_PREFIX_PATH="$CONDA_PREFIX"          # Trouver protobuf conda
```

## Compilation LispETorch

### 1. Compilation automatique complète
```bash
cd /path/to/lispe/lispetorch
make -f Makefile.linux all
```

Cette commande :
1. Vérifie l'environnement conda
2. Détecte PyTorch et CUDA
3. Clone et compile SentencePiece si nécessaire
4. Compile LispETorch avec toutes les dépendances

### 2. Compilation manuelle étape par étape
```bash
# Vérifier l'environnement
make -f Makefile.linux status

# Compiler SentencePiece seulement
make -f Makefile.linux compile-sentencepiece-local

# Compiler LispETorch seulement
make -f Makefile.linux build-lispetorch
```

### 3. Test de fonctionnement
```bash
# Test automatique avec démonstration SentencePiece
make -f Makefile.linux test

# Test manuel
cd tests
../bin/lispe demo_sentencepiece.lisp
```

## Structure des dépendances

### Bibliothèques liées dans liblispe_torch.so
```
liblispe_torch.so
├── PyTorch (conda)
│   ├── libtorch.so
│   ├── libtorch_cpu.so
│   ├── libc10.so
│   └── libtorch_cuda.so (si CUDA)
├── SentencePiece (local)
│   ├── libsentencepiece.so
│   └── libsentencepiece_train.so
├── CUDA (système)
│   ├── libcudart.so
│   ├── libcublas.so
│   └── ...
└── LispE (../bin/liblispe.so)
```

### RPATH configuré automatiquement
Le Makefile configure automatiquement les RPATH Linux pour :
- PyTorch conda : `$CONDA_PREFIX/lib/python3.x/site-packages/torch/lib`
- SentencePiece local : `./sentencepiece-build/lib`
- CUDA système : `/usr/local/cuda-*/lib64`

## Résolution de problèmes

### Erreur "PyTorch not found"
```bash
# Vérifier conda
echo $CONDA_PREFIX
conda list pytorch

# Réinstaller si nécessaire
conda install pytorch -c pytorch
```

### Erreur "protobuf not found"
```bash
# Installer protobuf
conda install protobuf

# Vérifier version
python -c "import google.protobuf; print(google.protobuf.__version__)"
```

### Erreur "cmake not found"
```bash
# Installer cmake
conda install cmake
# ou
sudo apt-get install cmake  # Ubuntu/Debian
sudo yum install cmake      # Red Hat/CentOS
```

### Problèmes de permissions
```bash
# Nettoyer et recommencer
make -f Makefile.linux clean-all
make -f Makefile.linux all
```

### Conflits de bibliothèques
La compilation locale de SentencePiece évite les conflits avec PyTorch. Si des problèmes persistent :

```bash
# Nettoyage complet
make -f Makefile.linux clean-sentencepiece
rm -rf sentencepiece sentencepiece-build

# Recompilation
make -f Makefile.linux all
```

## Commandes de maintenance

### Diagnostic complet
```bash
make -f Makefile.linux status
```

### Nettoyage
```bash
# Nettoyage partiel (objets)
make -f Makefile.linux clean

# Nettoyage SentencePiece
make -f Makefile.linux clean-sentencepiece

# Nettoyage complet
make -f Makefile.linux clean-all
```

### Vérifications
```bash
# État SentencePiece
make -f Makefile.linux check-sentencepiece-local

# Test des dépendances
ldd ../bin/liblispe_torch.so
```

## Optimisations

### Compilation parallèle
Le Makefile utilise automatiquement tous les cœurs disponibles :
```bash
# Équivalent à make -j$(nproc)
make -f Makefile.linux all
```

### Espace disque
- SentencePiece source : ~50MB
- SentencePiece build : ~7.5MB
- Total overhead : ~60MB

### Performance
- Compilation SentencePiece : 2-5 minutes (selon CPU)
- Compilation LispETorch : 30 secondes
- Total : 3-6 minutes pour installation complète

## Support et versions

### Versions testées
- **PyTorch**: 2.7.1+cu126
- **CUDA**: 12.9
- **Protobuf**: 6.32.0
- **SentencePiece**: 0.2.2 (dernière stable)
- **G++**: 11.5.0
- **CMake**: 3.12+

### Contact et documentation
- Code source LispE : dépôt principal
- SentencePiece : https://github.com/google/sentencepiece
- PyTorch : https://pytorch.org/get-started/locally/

---

*Ce guide correspond à la configuration optimisée pour fine-tuning avec SentencePiece sur Linux.*
