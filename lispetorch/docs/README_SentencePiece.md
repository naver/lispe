# SentencePiece Integration for LispE PyTorch

This document describes the SentencePiece tokenization integration in the LispE PyTorch library, providing production-ready subword tokenization for modern language model applications.

## Overview

SentencePiece integration enables:
- **Subword tokenization** compatible with Llama-3.1, GPT, BERT, and other modern LLMs
- **Custom tokenizer training** for domain-specific applications
- **Perfect text reconstruction** through reversible tokenization
- **Multilingual support** without language-specific preprocessing
- **Production deployment** ready for large-scale applications

## Quick Start

### 1. Installation

Ensure SentencePiece is installed on your system:

```bash
# macOS
brew install sentencepiece

# Ubuntu/Debian  
sudo apt-get install libsentencepiece-dev
```

### 2. Basic Usage

```lisp
; Load the library
(use 'liblispe_torch)

; Train a custom tokenizer
(setq training_data "Your training corpus text here...")
(fwrite "corpus.txt" training_data)
(setq result (torch_train_sentencepiece "corpus.txt" "my_model" 8000 "bpe"))

; Load and use the tokenizer
(setq tokenizer (torch_sentencepiece_tokenizer "my_model.model"))
(setq tokens (torch_encode tokenizer "Hello, world!"))
(setq decoded (torch_decode tokenizer tokens))
```

## Functions Reference

### torch_train_sentencepiece

Trains a new SentencePiece model from text data.

**Syntax:**
```lisp
(torch_train_sentencepiece input_file model_prefix vocab_size model_type)
```

**Parameters:**
- `input_file`: Path to training text file
- `model_prefix`: Output model name (creates .model and .vocab files)
- `vocab_size`: Target vocabulary size
- `model_type`: "bpe" (Byte-Pair Encoding) or "unigram"

**Returns:** Success message string

### torch_sentencepiece_tokenizer

Loads a pre-trained SentencePiece model.

**Syntax:**
```lisp
(torch_sentencepiece_tokenizer model_path)
```

**Parameters:**
- `model_path`: Path to .model file

**Returns:** SentencePiece tokenizer object

### Shared Functions

These functions work with both SimpleTokenizer and SentencePiece tokenizers:

- `(torch_encode tokenizer text)` - Encode text to token IDs
- `(torch_decode tokenizer tokens)` - Decode token IDs to text  
- `(torch_vocab_size tokenizer)` - Get vocabulary size

## Training Guidelines

### Vocabulary Size Selection

Choose vocabulary size based on your corpus:

| Corpus Size | Recommended Vocab Size | Use Case |
|-------------|----------------------|----------|
| < 1MB | 1,000 - 5,000 | Small domain, prototyping |
| 1-10MB | 5,000 - 15,000 | Medium domain applications |
| 10-100MB | 15,000 - 30,000 | Large domain corpus |
| > 100MB | 30,000+ | General purpose, production |

### Algorithm Selection

**BPE (Byte-Pair Encoding):**
- Good for most applications
- Deterministic tokenization
- Works well with code and structured text

**Unigram:**
- More flexible subword selection
- Better for morphologically rich languages
- Probabilistic tokenization

## Examples

### Domain-Specific Training

```lisp
; Create domain-specific corpus
(setq tech_corpus (+
    "Machine learning algorithms process data efficiently.\n"
    "Neural networks require substantial computational resources.\n"
    "API endpoints handle HTTP requests and responses.\n"
    "Database optimization improves query performance.\n"))

(fwrite "tech_corpus.txt" tech_corpus)

; Train domain-adapted tokenizer
(setq result (torch_train_sentencepiece "tech_corpus.txt" "tech_model" 3000 "bpe"))
(setq tech_tokenizer (torch_sentencepiece_tokenizer "tech_model.model"))

; Test domain-specific tokenization
(setq test_text "API optimization using machine learning algorithms.")
(setq tokens (torch_encode tech_tokenizer test_text))
(println "Tokens:" tokens)
(println "Token count:" (size tokens))
```

### Comparison with SimpleTokenizer

```lisp
; Create both tokenizer types
(setq simple_tok (torch_simple_tokenizer))
(setq sp_tok (torch_sentencepiece_tokenizer "model.model"))

; Compare tokenization
(setq text "Natural language processing enables sophisticated text analysis.")

(setq simple_tokens (torch_encode simple_tok text))
(setq sp_tokens (torch_encode sp_tok text))

(println "Simple tokenizer: " (size simple_tokens) "tokens")
(println "SentencePiece: " (size sp_tokens) "tokens")

; Test reconstruction quality
(setq simple_decoded (torch_decode simple_tok simple_tokens))
(setq sp_decoded (torch_decode sp_tok sp_tokens))

(println "Simple reconstruction:" (= text simple_decoded))
(println "SentencePiece reconstruction:" (= text sp_decoded))
```

### Multilingual Support

```lisp
; Train multilingual tokenizer
(setq multilingual_corpus (+
    "Hello world in English.\n"
    "Bonjour le monde en français.\n"
    "Hola mundo en español.\n"
    "こんにちは世界 in Japanese.\n"
    "Привет мир in Russian.\n"))

(fwrite "multilingual.txt" multilingual_corpus)
(setq result (torch_train_sentencepiece "multilingual.txt" "multilingual_model" 5000 "bpe"))
(setq multilingual_tok (torch_sentencepiece_tokenizer "multilingual_model.model"))

; Test multilingual tokenization
(setq mixed_text "Hello! Bonjour! ¡Hola! こんにちは!")
(setq tokens (torch_encode multilingual_tok mixed_text))
(setq decoded (torch_decode multilingual_tok tokens))
(println "Perfect multilingual reconstruction:" (= mixed_text decoded))
```

## Production Deployment

### Model Versioning

```lisp
; Version your tokenizer models
(setq version "v1.0")
(setq model_name (+ "production_tokenizer_" version))
(setq result (torch_train_sentencepiece "production_corpus.txt" model_name 32000 "bpe"))

; Load versioned model
(setq prod_tokenizer (torch_sentencepiece_tokenizer (+ model_name ".model")))
```

### Performance Optimization

```lisp
; Pre-load tokenizer for better performance
(setq global_tokenizer (torch_sentencepiece_tokenizer "production.model"))

; Batch processing function
(defun process_texts (texts)
    (setq results (list))
    (loop text texts
        (setq tokens (torch_encode global_tokenizer text))
        (setq results (cons tokens results)))
    (reverse results))

; Use in production
(setq batch_texts (list "Text 1" "Text 2" "Text 3"))
(setq batch_results (process_texts batch_texts))
```

## Integration with Transformers

### Complete Pipeline

```lisp
; Step 1: Tokenization
(setq tokenizer (torch_sentencepiece_tokenizer "model.model"))
(setq texts (list "First sentence." "Second sentence."))
(setq tokenized (list))
(loop text texts
    (setq tokens (torch_encode tokenizer text))
    (setq tokenized (cons tokens tokenized)))

; Step 2: Padding and attention masks
(setq padded (torch_pad_sequences (reverse tokenized) 50))
(setq attention_mask (torch_create_attention_mask padded))

; Step 3: Embedding and transformer processing
(setq vocab_size (torch_vocab_size tokenizer))
(setq embedding (torch_embedding vocab_size 512))
(setq transformer (torch_transformer_block 512 8 1024))

(setq embedded (torch_forward embedding padded))
(setq output (torch_forward transformer embedded))

(println "Pipeline complete - shape:" (torch_shape output))
```

## Troubleshooting

### Common Issues

1. **Vocabulary size too large**
   ```
   Error: Vocabulary size too high (X). Please set it to a value <= Y.
   ```
   **Solution:** Reduce vocab_size parameter based on your corpus size.

2. **Model file not found**
   ```
   Error: Failed to load SentencePiece model: model.model
   ```
   **Solution:** Verify the .model file exists and path is correct.

3. **SentencePiece not available**
   ```
   Error: SentencePiece support not compiled.
   ```
   **Solution:** Install SentencePiece library and recompile.

### Debugging Tips

```lisp
; Check if SentencePiece is available
(setq test_result
  (catch
    (torch_train_sentencepiece "test.txt" "debug_test" 100 "bpe")))

(if (maybe test_result)
    (println "SentencePiece not available:" test_result)
    (println "SentencePiece working correctly"))

; Analyze tokenization patterns
(defun analyze_tokenization (tokenizer text)
    (setq tokens (torch_encode tokenizer text))
    (setq decoded (torch_decode tokenizer tokens))
    (setq compression_ratio (/ (size text) (size tokens)))
    (println "Text: " text)
    (println "Tokens: " (size tokens))
    (println "Compression: " compression_ratio "chars/token")
    (println "Perfect: " (= text decoded)))
```

## Best Practices

1. **Corpus Preparation**
   - Use representative text from your target domain
   - Include diverse sentence structures and vocabulary
   - Minimum 10,000 sentences for quality tokenization

2. **Vocabulary Size**
   - Start with conservative estimates
   - Monitor token-per-word ratios (aim for 1.3-2.0)
   - Adjust based on compression and reconstruction quality

3. **Model Management**
   - Version your tokenizer models
   - Keep training corpora for reproducibility
   - Test on held-out data before deployment

4. **Performance**
   - Pre-load tokenizers in production environments
   - Batch process when possible
   - Cache frequently used tokenizations

## Advanced Features

### Custom Special Tokens

SentencePiece automatically handles these special tokens:
- `<pad>` (ID: 0) - Padding token
- `<unk>` (ID: 1) - Unknown token  
- `<s>` (ID: 2) - Beginning of sequence
- `</s>` (ID: 3) - End of sequence

### Character Coverage

The training automatically optimizes character coverage for your corpus:
- Default: 1.0 (covers all characters)
- Adjustable for domain-specific needs
- Affects handling of rare characters

### Integration Status

✅ **Implemented:**
- Model training with BPE and Unigram
- Model loading and inference
- Perfect encode/decode cycles
- Vocabulary size management
- Error handling and validation

🔄 **In Progress:**
- Custom special token configuration
- Batch tokenization optimization
- Streaming support for large texts

📋 **Planned:**
- Model fine-tuning and vocabulary expansion
- Advanced training parameters
- Performance profiling tools

This comprehensive SentencePiece integration positions LispE PyTorch as a production-ready platform for modern language model development and deployment.
