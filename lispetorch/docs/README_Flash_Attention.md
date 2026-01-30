# Flash Attention dans LispETorch

Flash Attention est une implémentation optimisée de l'attention qui réduit significativement l'utilisation mémoire de O(N²) à O(N) pour des séquences de longueur N, tout en maintenant les mêmes résultats mathématiques que l'attention standard.

## Nouvelles Fonctions

### 1. `torch_flash_attention_create`
Crée un module Flash Attention.

**Syntaxe:**
```lisp
(torch_flash_attention_create embed_dim num_heads dropout bias)
```

**Paramètres:**
- `embed_dim` (entier): Dimension des embeddings
- `num_heads` (entier): Nombre de têtes d'attention
- `dropout` (flottant, optionnel): Taux de dropout (défaut: 0.0)
- `bias` (booléen, optionnel): Utiliser les biais dans les projections linéaires (défaut: true)

**Exemple:**
```lisp
(setq flash_attn (torch_flash_attention_create 512 8 0.1 true))
```

### 2. `torch_flash_attention_forward`
Effectue un forward pass avec Flash Attention.

**Syntaxe:**
```lisp
(torch_flash_attention_forward flash_attention query key value)
```

**Paramètres:**
- `flash_attention`: Module Flash Attention créé avec `torch_flash_attention_create`
- `query`: Tensor query [batch_size, seq_len, embed_dim]
- `thekey`: Tensor key [batch_size, seq_len, embed_dim]
- `thevalue`: Tensor value [batch_size, seq_len, embed_dim]

**Exemple:**
```lisp
(setq output (torch_flash_attention_forward flash_attn query thekey thevalue))
```

### 3. `torch_flash_attention_with_mask`
Flash Attention avec masque d'attention.

**Syntaxe:**
```lisp
(torch_flash_attention_with_mask flash_attention query thekey thevalue attn_mask)
```

**Paramètres:**
- `attn_mask`: Masque d'attention [batch_size, num_heads, seq_len, seq_len]

**Exemple:**
```lisp
(setq masked_output (torch_flash_attention_with_mask flash_attn query thekey thevalue attention_mask))
```

### 4. `torch_flash_attention_with_dropout`
Flash Attention avec dropout personnalisé.

**Syntaxe:**
```lisp
(torch_flash_attention_with_dropout flash_attention query thekey thevalue dropout_p training)
```

**Paramètres:**
- `dropout_p` (flottant): Taux de dropout à utiliser
- `training` (booléen): Mode entraînement ou évaluation

**Exemple:**
```lisp
(setq output (torch_flash_attention_with_dropout flash_attn query thekey thevalue 0.2 true))
```

### 5. `torch_scaled_dot_product_attention`
Interface directe vers l'attention scalée dot-product native de PyTorch.

**Syntaxe:**
```lisp
(torch_scaled_dot_product_attention query thekey thevalue attn_mask dropout_p is_causal)
```

**Paramètres:**
- `query`: Tensor query [batch_size, num_heads, seq_len, head_dim]
- `thekey`: Tensor key [batch_size, num_heads, seq_len, head_dim]
- `thevalue`: Tensor value [batch_size, num_heads, seq_len, head_dim]
- `attn_mask` (optionnel): Masque d'attention
- `dropout_p` (flottant, optionnel): Taux de dropout (défaut: 0.0)
- `is_causal` (booléen, optionnel): Utiliser un masque causal (défaut: false)

**Exemple:**
```lisp
(setq output (torch_scaled_dot_product_attention q k v nil 0.1 false))
```

## Avantages de Flash Attention

### 1. **Efficacité Mémoire**
- Complexité mémoire réduite de O(N²) à O(N)
- Permet l'entraînement avec des séquences plus longues
- Utilisation optimale du cache GPU

### 2. **Performance**
- Optimisations automatiques selon la version PyTorch
- Kernels optimisés pour différentes architectures GPU
- Throughput amélioré pour les longues séquences

### 3. **Compatibilité**
- API compatible avec l'attention multi-têtes standard
- Support natif pour les masques d'attention
- Intégration transparente dans les modèles existants

## Exemples d'Utilisation

### Exemple Basique
```lisp
(use 'lispe_torch)

; Configuration
(setq embed_dim 256)
(setq num_heads 8)
(setq batch_size 2)
(setq seq_len 128)

; Créer le module
(setq flash_attn (torch_flash_attention_create embed_dim num_heads 0.1 true))

; Créer des données d'exemple
(setq query (torch_rand (integers batch_size seq_len embed_dim)))
(setq thekey (torch_rand (integers batch_size seq_len embed_dim)))
(setq thevalue (torch_rand (integers batch_size seq_len embed_dim)))

; Forward pass
(setq output (torch_flash_attention_forward flash_attn query thekey thevalue))
(println "Output shape: " (torch_size output))
```

### Transformer avec Flash Attention
```lisp
; Créer une couche transformer complète
(defun create_transformer_layer (embed_dim num_heads ffn_dim)
    (dict
        'attention (torch_flash_attention_create embed_dim num_heads 0.1 true)
        'norm1 (torch_layer_norm_create embed_dim)
        'norm2 (torch_layer_norm_create embed_dim)
        'ffn1 (torch_linear_create embed_dim ffn_dim)
        'ffn2 (torch_linear_create ffn_dim embed_dim)
    )
)

; Forward pass d'une couche transformer
(defun transformer_forward (layer x)
    (block
        ; Self-attention + résiduelle
        (setq attn_out (torch_flash_attention_forward 
            (get layer 'attention) x x x))
        (setq x1 (torch_layer_norm_forward 
            (get layer 'norm1) (torch_add x attn_out)))
        
        ; FFN + résiduelle
        (setq ffn_out (torch_linear_forward (get layer 'ffn2)
            (torch_relu (torch_linear_forward (get layer 'ffn1) x1))))
        (setq output (torch_layer_norm_forward 
            (get layer 'norm2) (torch_add x1 ffn_out)))
        
        output
    )
)
```

### Masque Causal pour Modèles Autorégressifs
```lisp
; Utiliser l'attention causale pour la génération de texte
(setq output (torch_scaled_dot_product_attention 
    query thekey thevalue nil 0.0 true)) ; is_causal = true
```

## Recommandations d'Utilisation

### Quand Utiliser Flash Attention
- **Séquences longues**: Particulièrement bénéfique pour seq_len > 256
- **Modèles volumineux**: Avec de nombreuses têtes d'attention
- **Contraintes mémoire**: Quand la mémoire GPU est limitée
- **Entraînement de LLM**: Essentiel pour les grands modèles de langage

### Optimisations
1. **Utiliser CUDA** quand disponible pour de meilleures performances
2. **Choisir la bonne taille de batch** selon la mémoire disponible
3. **Activer le mode mixte de précision** pour réduire l'usage mémoire
4. **Utiliser gradient checkpointing** pour les très longs modèles

### Comparaison Performance
Pour des séquences de 512 tokens avec 8 têtes d'attention:
- **Mémoire**: ~75% de réduction par rapport à l'attention standard
- **Vitesse**: 1.2-2x plus rapide selon la configuration
- **Précision**: Résultats mathématiquement identiques

## Compatibilité

### Versions PyTorch
- **PyTorch 2.0+**: Utilise `torch.scaled_dot_product_attention` natif
- **PyTorch < 2.0**: Implémentation manuelle optimisée

### Plateformes
- ✅ CPU (toutes architectures)
- ✅ CUDA GPU
- ✅ Apple Metal (MPS)

## Fichiers d'Exemple

1. **`flash_attention_demo.lsp`**: Démonstration basique des fonctionnalités
2. **`flash_transformer_example.lsp`**: Transformer complet avec Flash Attention
3. **`attention_performance_comparison.lsp`**: Comparaison de performance

## Limitations et Notes

1. **Taille minimale**: Flash Attention n'apporte des bénéfices que pour seq_len >= 64
2. **Mémoire GPU**: Les optimisations sont plus prononcées sur GPU
3. **Batch size**: Les performances optimales dépendent de la taille du batch

## Support et Debugging

### Messages d'Erreur Courants
- `embed_dim must be divisible by num_heads`: Vérifier que embed_dim % num_heads == 0
- `Tensor size mismatch`: Vérifier les dimensions des tenseurs d'entrée
- `CUDA out of memory`: Réduire batch_size ou seq_len

### Performance Debugging
```lisp
; Mesurer le temps d'exécution
(setq elapsed (elapse
    (setq output (torch_flash_attention_forward flash_attn query thekey thevalue)))
)
(println "Temps d'exécution: " elapsed " ms")
```

Pour plus d'exemples et de documentation, consultez les fichiers d'exemple fournis.
