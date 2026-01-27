# LispE Tiktoken Module

This module provides a BPE (Byte Pair Encoding) tokenizer implementation for LispE, compatible with tiktoken-style vocabularies used by models like GPT.

## Installation

Compile the module using the provided Makefile:

```bash
make
```

## Loading the Module

```lisp
(use 'lispe_tiktoken)
```

## Functions Reference

### tiktoken_create

```lisp
(tiktoken_create vocab special_tokens pattern)
```

Creates a new tiktoken BPE tokenizer object.

**Parameters:**
- `vocab`: A dictionary mapping token strings to their integer IDs
- `special_tokens`: A list of dictionaries, each containing `content` (string) and `id` (integer) keys
- `pattern`: A regex pattern string for tokenization (ICU regex format)

**Returns:** A `tiktoken_` object

**Example:**
```lisp
(setq vocab {"hello" 1 "world" 2 "Ġthe" 3})
(setq special_tokens (list {"content" "<|endoftext|>" "id" 50256}))
(setq pattern "(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\\r\\n\\p{L}\\p{N}]?\\p{L}+|\\p{N}{1,3}| ?[^\\s\\p{L}\\p{N}]+[\\r\\n]*|\\s*[\\r\\n]+|\\s+(?!\\S)|\\s+")
(setq tokenizer (tiktoken_create vocab special_tokens pattern))
```

---

### tiktoken_encode

```lisp
(tiktoken_encode bpe_object text)
```

Encodes a text string into a list of token IDs using BPE.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `text`: The string to encode

**Returns:** A list of integers (token IDs)

**Example:**
```lisp
(setq ids (tiktoken_encode tokenizer "Hello world"))
; Returns: (1234 5678 ...)
```

---

### tiktoken_decode

```lisp
(tiktoken_decode bpe_object ids)
```

Decodes a list of token IDs back into a string, applying meta-character replacements.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `ids`: A list of integers (token IDs)

**Returns:** The decoded string

**Example:**
```lisp
(setq text (tiktoken_decode tokenizer '(1234 5678)))
; Returns: "Hello world"
```

---

### tiktoken_special_encode

```lisp
(tiktoken_special_encode bpe_object text)
```

Returns the token ID for a special token string.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `text`: The special token string to look up

**Returns:** The token ID as an integer, or -1 if not found

**Example:**
```lisp
(setq id (tiktoken_special_encode tokenizer "<|endoftext|>"))
; Returns: 50256
```

---

### tiktoken_vocab_size

```lisp
(tiktoken_vocab_size bpe_object)
```

Returns the size of the tokenizer's vocabulary.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`

**Returns:** The vocabulary size as an integer

**Example:**
```lisp
(setq size (tiktoken_vocab_size tokenizer))
; Returns: 50257
```

---

### tiktoken_set_space

```lisp
(tiktoken_set_space bpe_object str)
```

Sets the meta-character used to represent spaces in tokens. By default, the space character is represented by `Ġ` (U+0120).

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `str`: The new space meta-character string

**Returns:** `true`

**Example:**
```lisp
(tiktoken_set_space tokenizer "Ġ")
```

---

### tiktoken_add_meta

```lisp
(tiktoken_add_meta bpe_object meta_char replacement)
```

Adds a custom meta-character mapping for decoding. Meta-characters are special Unicode characters used by BPE tokenizers to represent common characters like spaces, newlines, and punctuation.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `meta_char`: The meta-character string to map
- `replacement`: The replacement string

**Returns:** `true`

**Example:**
```lisp
; Map a custom meta-character to its replacement
(tiktoken_add_meta tokenizer "Ċ" "\n")
```

---

### tiktoken_remove_meta

```lisp
(tiktoken_remove_meta bpe_object meta_char)
```

Removes a meta-character mapping from the decoder.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `meta_char`: The meta-character string to remove

**Returns:** `true`

**Example:**
```lisp
(tiktoken_remove_meta tokenizer "Ġ")
```

---

### tiktoken_list_meta

```lisp
(tiktoken_list_meta bpe_object [metas])
```

Lists all meta-character mappings, or sets them from a dictionary.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `metas` (optional): A dictionary of meta-character mappings to set

**Returns:** 
- If `metas` is not provided: A dictionary of all current meta-character mappings
- If `metas` is provided: `true` (mappings are updated)

**Example:**
```lisp
; List all meta-character mappings
(setq all_metas (tiktoken_list_meta tokenizer))
; Returns: {"Ġ" " " "Ċ" "\n" ...}

; Set meta-character mappings from a dictionary
(tiktoken_list_meta tokenizer {"Ġ" " " "Ċ" "\n"})
```

---

### tiktoken_decode_bytes

```lisp
(tiktoken_decode_bytes bpe_object ids)
```

Decodes a list of token IDs to raw bytes without meta-character replacement.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `ids`: A list of integers (token IDs)

**Returns:** A list of integers representing raw bytes (0-255)

**Example:**
```lisp
(setq bytes (tiktoken_decode_bytes tokenizer '(1234 5678)))
; Returns: (72 101 108 108 111 ...)
```

---

### tiktoken_decode_single_token_bytes

```lisp
(tiktoken_decode_single_token_bytes bpe_object token)
```

Decodes a single token ID to its raw bytes.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`
- `token`: A single token ID (integer)

**Returns:** A list of integers representing raw bytes (0-255)

**Example:**
```lisp
(setq bytes (tiktoken_decode_single_token_bytes tokenizer 1234))
; Returns: (72 101 108 108 111)
```

---

### tiktoken_token_byte_values

```lisp
(tiktoken_token_byte_values bpe_object)
```

Returns all token byte values from the vocabulary, sorted.

**Parameters:**
- `bpe_object`: A tiktoken object created with `tiktoken_create`

**Returns:** A list of lists, where each inner list contains the byte values for a token

**Example:**
```lisp
(setq all_bytes (tiktoken_token_byte_values tokenizer))
; Returns: ((33) (34) (35) ... (72 101 108 108 111) ...)
```

---

## Built-in Meta-Characters

The module comes with pre-defined meta-character mappings commonly used in GPT-style tokenizers:

| Meta-Character | Unicode | Replacement | Description |
|----------------|---------|-------------|-------------|
| Ġ | U+0120 | ` ` (space) | Space at beginning of token |
| Ċ | U+010A | `\n` | Newline |
| ĉ | U+0109 | `\t` | Tab |
| Ą | U+0104 | `(` | Left parenthesis |
| ą | U+0105 | `)` | Right parenthesis |
| Đ | U+0110 | `[` | Left bracket |
| đ | U+0111 | `]` | Right bracket |
| Ě | U+011A | `{` | Left brace |
| ě | U+011B | `}` | Right brace |
| ... | ... | ... | ... |

The module also handles common contractions (e.g., `'s`, `'t`, `'ll`, `'re`, `'ve`) and various accented characters.

## Complete Example

### TiktokenTokenizer Class for LLM Training

This is a complete example of a reusable Tokenizer class for training or inference with LLMs like Llama:

```lisp
(use 'lispe_tiktoken)

; ==============================================
; TOKENIZER CLASS FOR LLM TRAINING
; ==============================================

(class@ TiktokenTokenizer (tokenizer_path init)
    (defun configure()
        (println "📝 Configuration du tokenizer tiktoken...")
        
        ; Load special tokens map (bos, eos, pad tokens)
        (setq spec_tokens (json_parse (fread (+ tokenizer_path "/special_tokens_map.json"))))
        
        ; Load main tokenizer file
        (setq tok_file (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))
        
        ; Extract special tokens
        (setqi bos_token (@ spec_tokens "bos_token" "content"))
        (setqi eos_token (@ spec_tokens "eos_token" "content"))
        
        ; Extract regex pattern from pre_tokenizer
        (setq pattern (@ tok_file "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

        ; Create the tiktoken tokenizer
        (setqi tokenizer_obj (tiktoken_create
            (@ tok_file "model" "vocab")
            (@ tok_file "added_tokens")
            pattern))

        ; Get special token IDs
        (setqi bos_id (tiktoken_special_encode tokenizer_obj bos_token))
        (setqi eos_id (tiktoken_special_encode tokenizer_obj eos_token))
        (setqi pad_id 0)

        (println "✓ Tokenizer configured - vocab size:" (tiktoken_vocab_size tokenizer_obj))
    )

    ; Format text for chat-style LLM training (Llama 3 format)
    (defun encode_chat_format(instruction response max_seq_length)
        (setq formatted_text (+
            "<|begin_of_text|>"
            "<|start_header_id|>system<|end_header_id|>\n\n"
            "You are a helpful assistant."
            "<|eot_id|>"
            "<|start_header_id|>user<|end_header_id|>\n\n"
            instruction
            "<|eot_id|>"
            "<|start_header_id|>assistant<|end_header_id|>\n\n"
            response
            "<|eot_id|>"
        ))

        (setq tokens (tiktoken_encode tokenizer_obj formatted_text))
        (pushfirst tokens bos_id)

        ; Truncate if needed
        (if (> (size tokens) max_seq_length)
            (setq tokens (@@ tokens 0 max_seq_length)))

        tokens
    )

    ; Pad a sequence to a fixed length
    (defun pad_sequence(token_ids max_length)
        (setq current_length (size token_ids))
        (if (< current_length max_length)
            (block
                (setq padded (copy token_ids))
                (setq padding_needed (- max_length current_length))
                (setq padding (rho padding_needed (integers pad_id)))
                (extend padded padding)
                padded)
            (if (> current_length max_length)
                (@@ token_ids 0 max_length)
                token_ids)))

    ; Decode tokens to string
    (defun decode(token_ids)
        (tiktoken_decode tokenizer_obj token_ids))

    ; Decode tokens to raw bytes (useful for debugging)
    (defun decode_bytes(token_ids)
        (chrbyte (tiktoken_decode_bytes tokenizer_obj token_ids)))
    
    ; Get vocabulary size
    (defun vocab_size()
        (tiktoken_vocab_size tokenizer_obj))
)

; ==============================================
; USAGE
; ==============================================

; Initialize the tokenizer
(setq tokenizer_path "/path/to/your/model/tokenizer")
(setq tiktokenizer (TiktokenTokenizer tokenizer_path (configure)))

; Encode a chat conversation for training
(setq tokens (tiktokenizer TiktokenTokenizer 
    (encode_chat_format 
        "What is the capital of France?" 
        "The capital of France is Paris."
        256)))
(println "Encoded tokens:" tokens)

; Decode back to text
(setq decoded (tiktokenizer TiktokenTokenizer (decode tokens)))
(println "Decoded:" decoded)

; Pad sequence for batching
(setq padded (tiktokenizer TiktokenTokenizer (pad_sequence tokens 512)))
(println "Padded length:" (size padded))
```

### Simple Loading (without class)

For simpler use cases, you can load the tokenizer directly:

```lisp
(use 'lispe_tiktoken)

; Path to your model directory containing tokenizer.json
(setq tokenizer_path "/path/to/your/model")

; Load and parse the tokenizer.json file
(setq tokfile (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))

; Extract the regex pattern
(setq pattern (@ tokfile "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

; Fallback: use a default GPT-style pattern
(check (nullp pattern)
    (setq pattern "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"))

; Create the tokenizer
(setq tokenizer (tiktoken_create 
    (@ tokfile "model" "vocab") 
    (@ tokfile "added_tokens") 
    pattern))

; Use the tokenizer
(println "Vocabulary size:" (tiktoken_vocab_size tokenizer))

; Encode and decode
(setq ids (tiktoken_encode tokenizer "Hello, world!"))
(println "Encoded:" ids)
(println "Decoded:" (tiktoken_decode tokenizer ids))
```

## Error Handling

All functions will throw an error if:
- The `bpe_object` is not a valid tiktoken object
- The BPE object is not properly initialized
- Invalid parameters are provided

Example error messages:
- `"Error: expecting a 'tiktoken_' type"`
- `"Wrong vocabulary format for tiktoken"`
- `"Wrong special tokens format for tiktoken"`
- `"BPE object not initialized"`
