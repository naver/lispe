# LISPE SYNTAX REFERENCE FOR LLMs (V2)

> **⚠️ CRITICAL**: LispE ≠ Common Lisp. Lists are **ARRAYS**, not linked lists.  
> **⚠️ COMMENTS**: Use single `;` only. NOT `;;;` or `;;` (except for multi-line blocks).  
> **💡 NAMING**: Both `under_scores` and `dashes-in-names` are valid. Choose one style consistently.

---

## TABLE OF CONTENTS

1. [Quick Reference](#quick-reference-yaml)
2. [LLM-Specific Gotchas](#llm-specific-gotchas) ⚠️
3. [AI/ML Libraries](#aiml-libraries-key-feature-) 🚀
4. [Wrong → Right](#wrong--right-common-mistakes)
5. [How Do I...?](#how-do-i-patterns)
6. [Function Signatures](#function-signatures-json)
7. [Data Structures](#data-structures-compact)
8. [Control Flow](#control-flow-decision-tree)
9. [Real-World Templates](#real-world-templates)
10. [Migration Guides](#migration-from-other-languages)
11. [Debugging & Performance](#debugging--performance)
12. [Advanced Features](#advanced-features-compact)

---

## LLM-SPECIFIC GOTCHAS

> **🤖 Common LLM hallucinations when generating LispE code:**

```yaml
llm_common_mistakes:
  - mistake: "Using (require 'module) or (import ...)"
    correct: "(use 'module_name)"
    why: "LLMs confuse with Common Lisp/Scheme/Python"
    example: "(use 'lispe_torch)  ; NOT (require 'lispe_torch)"
  
  - mistake: "Multi-line comments with ;;; or multiple ;;"
    correct: ";; comment block ;;"
    why: "Only single ; or ;; ... ;; blocks work"
    example: |
      ; Single line comment
      ;;
      Multi-line comment block
      spans multiple lines
      ;;
  
  - mistake: "(defclass Name ...) for OOP"
    correct: "(class@ Name ...)"
    why: "Not Common Lisp syntax"
    example: "(class@ Person (name age) ...)"
  
  - mistake: "(defvar *global* value) or (setf ...)"
    correct: "(setq global value)"
    why: "No earmuffs convention, no setf"
    example: "(setq counter 0)  ; NOT (defvar *counter* 0)"
  
  - mistake: "(push item list)"
    correct: "(push list item)"
    why: "Argument order is reversed vs Common Lisp"
    example: "(push mylist 42)  ; NOT (push 42 mylist)"
  
  - mistake: "(format t \"~a\" x) or (printf ...)"
    correct: "(println x) or (format \"%1\" x)"
    why: "Different format string syntax"
    example: "(format \"Value: %1, Count: %2\" val cnt)"
  
  - mistake: "(length container) or (len ...)"
    correct: "(size container)"
    why: "Consistent naming across all containers"
    example: "(size mylist)  ; works on lists, dicts, sets, strings"
  
  - mistake: "(nth n list) or (list[n])"
    correct: "(@ list n)"
    why: "Universal access operator for all containers"
    example: "(@ mylist 5)  ; NOT (nth 5 mylist)"
  
  - mistake: "(car nil) without checking"
    correct: "(check (not (nullp x)) (car x))"
    why: "Runtime error on nil access"
    example: |
      (if (not (emptyp mylist))
          (car mylist)
          "default")
  
  - mistake: "Modifying global from function with (setq global val)"
    correct: "(setg global val)"
    why: "setq creates local variable in function scope"
    example: |
      (setq counter 0)
      (defun increment()
          (setg counter (+ counter 1)))  ; Use setg!
  
  - mistake: "Modifying instance field with (setq field val)"
    correct: "(setqi field val)"
    why: "setq creates local, setqi modifies instance field"
    example: |
      (class@ Counter (count)
          (defun inc()
              (setqi count (+ count 1))))  ; Use setqi!
  
  - mistake: "(progn expr1 expr2 ...)"
    correct: "(block expr1 expr2 ...)"
    why: "Different name for sequential execution"
    example: "(block (println \"step 1\") (println \"step 2\") result)"
  
  - mistake: "Assuming lists are linked lists"
    correct: "Lists are arrays by default, use llist for linked"
    why: "Performance characteristics completely different"
    example: |
      (setq arr (list 1 2 3))      ; O(1) random access
      (setq linked (llist 1 2 3))  ; O(n) random access

critical_type_errors:
  - mistake: "Concatenating string + number: (+ \"age: \" 42)"
    correct: "(+ \"age: \" (string 42))"
    why: "No implicit type coercion"
    
  - mistake: "Comparing with = instead of eq for atoms"
    correct: "(eq 'symbol 'other) not (= 'symbol 'other)"
    why: "= is for numbers, eq for identity"

critical_runtime_errors:
  - mistake: "Accessing non-existent dictionary key: (@ dict \"missing\")"
    correct: "(select (@ dict \"key\") \"default\")"
    why: "Throws error if key doesn't exist"
    
  - mistake: "Index out of bounds: (@ list 999)"
    correct: "(if (< idx (size list)) (@ list idx) nil)"
    why: "No bounds checking by default"
```

---

## AI/ML LIBRARIES (KEY FEATURE) 🚀

> **⚡ PERFORMANCE**: LispE's native C++ implementation bypasses Python's heavy API overhead.  
> **Up to 35% faster** LoRA training compared to pure Python pipelines.

### Why LispE for AI/ML?

```
┌─────────────────────────────────────────────────────────────────┐
│  Python Pipeline                 LispE Pipeline                 │
│  ───────────────                 ──────────────                 │
│  Python script                   LispE script                   │
│       ↓                               ↓                         │
│  Python API overhead             Direct C++ binding             │
│       ↓                               ↓                         │
│  PyTorch C++ backend             PyTorch C++ backend            │
│       ↓                               ↓                         │
│  GPU (CUDA/Metal)                GPU (CUDA/Metal)               │
│                                                                 │
│  ❌ GIL contention               ✅ No GIL                      │
│  ❌ Object marshalling           ✅ Direct memory access        │
│  ❌ ~35% overhead                ✅ Native speed                │
└─────────────────────────────────────────────────────────────────┘
```

### Libraries Overview

LispE provides 4 powerful AI/ML libraries for deep learning and LLM inference:

| Library | Directory | Description | Backend |
|---------|-----------|-------------|---------|
| **lispe_torch** | `lispetorch/` | PyTorch integration, Transformers, LoRA, Flash Attention | PyTorch 2.0+ |
| **lispe_tiktoken** | `tiktoken/` | OpenAI tiktoken tokenizer for GPT models | tiktoken |
| **lispe_gguf** | `gguf/` | GGUF model loader, quantized LLM inference | llama.cpp |
| **lispe_mlx** | `lispemlx/` | Apple MLX framework for Apple Silicon | MLX |

### Usage Examples

```lisp
; PyTorch - Deep Learning
(use 'lispe_torch)
(setq model (torch_transformer config))
(setq output (torch_forward model input))

; Tiktoken - GPT tokenization
(use 'lispe_tiktoken)
(setq tokens (tiktoken_encode "Hello world"))

; GGUF - Run quantized LLMs (llama.cpp)
(use 'lispe_gguf)
(setq model (gguf_load "model.gguf"))
(setq response (gguf_generate model prompt))

; MLX - Apple Silicon ML
(use 'lispe_mlx)
(setq model (mlx_load "model"))
(setq output (mlx_generate model input))
```

### Key Features

```yaml
lispe_torch:
  - Flash Attention (O(N) memory)
  - LoRA fine-tuning
  - Transformer architecture (RoPE, Multi-Head Attention)
  - GPU: CUDA + Metal (Apple Silicon)
  - SentencePiece tokenization
  - Text generation (greedy, top-k, top-p, beam search)

lispe_gguf:
  - GGUF quantized models (Q4, Q5, Q8, MXFP4)
  - Zero-configuration GPU (Metal/CUDA)
  - llama.cpp powered
  - Low memory footprint

lispe_tiktoken:
  - OpenAI GPT tokenization
  - BPE encoding/decoding
  - Compatible with GPT-3.5/4 models

lispe_mlx:
  - Apple MLX framework
  - Optimized for Apple Silicon
  - Unified memory architecture

performance_benefits:
  - No Python GIL (Global Interpreter Lock)
  - No object marshalling between Python/C++
  - Direct tensor memory access
  - ~35% faster LoRA training vs Python
  - Lower CPU overhead during GPU operations
```

### LoRA Training Example (Real-world)

```lisp
; Fine-tuning Llama 3.1-8B with LoRA
(use 'lispe_torch)
(use 'lispe_tiktoken)

; Configuration
(setq lora-config (dictionary
    "rank" 16
    "alpha" 32
    "dropout" 0.05
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")))

(setq training-config (dictionary
    "batch_size" 4
    "learning_rate" 2e-4
    "num_epochs" 3
    "device" "mps"))  ; "cuda" for NVIDIA

; Load model with LoRA adapters
(setq model (torch_load_lora model-path lora-config))

; Training loop - 35% faster than Python equivalent
(loop epoch (irange 1 num-epochs 1)
    (loop batch batches
        (setq loss (torch_train_step model batch))
        (println "Loss:" loss)))

; Save LoRA adapters (small file, ~50MB vs 16GB base model)
(torch_save_lora model "my_lora_adapters.pt")
```

---

## QUICK REFERENCE (YAML)

```yaml
# ═══════════════════════════════════════════════════════════════
# CORE SYNTAX AT A GLANCE
# ═══════════════════════════════════════════════════════════════

comments:
  single: "; comment"
  multi: ";; ... ;;"              # Open AND close with ;;

modules: "(use 'module_name)"     # NOT (require ...)

variables:
  local: "(setq var value)"
  global: "(setg var value)"      # From inside functions
  instance: "(setqi field value)" # Create/modify class field
  thread_safe: "(seth var value)" # Inside threadspace block

class:
  define: |
    (class@ Name (field1 (field2 default))
        (defun method() body))
  instance: "(setq obj (Name args))"
  call: "(obj (method args))"
  inherit: "(class@ Child (f) (from@ Parent) ...)"
  field_access: "(@ obj 'field) or (@ obj 0)"
  field_modify: "(set@ obj 'field value)"

containers:
  access: "(@ container key)"
  multi_access: "(@ nested k1 k2 k3)"  # = nested[k1][k2][k3]
  dictionary: '{"key":value}' or '(dictionary "k" v)'
  list_array: "(list a b c)"      # DEFAULT - array-based
  list_linked: "(llist a b c)"    # Linked list (can cycle)

loops:
  counted: "(loopcount n i body)" # i from 0 to n-1
  iterate: "(loop var list body)"
  while: "(while cond body)"
  range: "(loop i (irange 0 10 2) body)"

conditionals:
  simple: "(if cond then else)"
  block_else: "(ife cond then else1 else2 ...)"
  guard: "(check cond action)"    # Execute if true
  guard_neg: "(ncheck cond else-action actions...)"
  first_non_nil: "(select v1 v2 v3)"
  error_handling: "(maybe (try-expr) fallback)"

functions:
  define: "(defun name(args) body)"
  optional: "(defun f(req (opt default)) body)"
  lambda: "(λ (x) body)" or "(\\ (x) body)"
  apply: "(apply 'fn '(args))"
  pattern: "(defpat f ([pattern]) body)"

types_as_converters:
  "(integer \"10\")": "→ 10"
  "(string 42)": "→ \"42\""
  "(float \"3.14\")": "→ 3.14"

typed_lists:
  "(integers 1 2 3)": "int array"
  "(floats 1.0 2.0)": "float array"
  "(strings \"a\" \"b\")": "string array"
  auto_convert: "(integers (strings \"1\" \"2\")) → (1 2)"

push_behavior:
  "(push list item)": "→ END (arrays)"
  "(push llist item)": "→ FRONT (linked)"
  "(pushfirst list item)": "→ FRONT (arrays)"

threading:
  define: "(dethread name(args) body)"
  launch: "(name args)"
  wait: "(wait)"
  safe_vars: "(threadspace (seth var val) ...)"

predicates:
  type_check: "atomp consp numberp integerp floatp stringp listp dictionaryp"
  state_check: "nullp zerop emptyp negativep positivep"
  string_check: "lowerp upperp digitp alphap"

logic:
  operators: "(and a b) (or a b) (not x) (xor a b)"
  equality: "(eq a b) (neq a b) (= a b)"
  comparison: "(< a b) (<= a b) (> a b) (>= a b)"
  three_way: "(>=< a b)"           # Returns -1, 0, or 1

block_expressions:
  sequence: "(block e1 e2 ...)"    # Like progn, returns last
  early_return: "(return value)"   # Exit function early
  local_scope: "(let ((a 1)(b 2)) body)"

special_values:
  nil: "nil"                       # Also () is nil
  true: "true"
  empty_list: "'()" or "(list)"
  empty_string: '""'
```

---

## WRONG → RIGHT (COMMON MISTAKES)

| Category | ❌ Common Lisp / Other | ✅ LispE | Note |
|----------|------------------------|----------|------|
| **module** | `(require 'mod)` | `(use 'mod)` | Different keyword |
| **class** | `(defclass ...)` | `(class@ Name (fields) ...)` | OOP syntax |
| **global** | `(defvar *x* 10)` | `(setq x 10)` | No earmuffs |
| **format** | `(format t "~a" x)` | `(println x)` or `(format "%1" x)` | Different format |
| **comment** | `;;; comment` | `; comment` | Single ; only |
| **push** | `(push item list)` | `(push list item)` | ⚠️ Order reversed! |
| **method** | `(send obj :method)` | `(obj (method))` | Direct call |
| **concat** | `(concatenate ...)` | `(+ str1 str2)` | Simpler |
| **funcall** | `(funcall fn args)` | `(fn args)` | Direct call |
| **naming** | `my-function` | `my_function` or `my-function` | Both valid |
| **lists** | linked by default | **arrays** by default | ⚠️ Use `llist` for linked |
| **nil test** | `(null x)` | `(not x)` or `(nullp x)` | Both work |
| **type check** | `(typep x 'integer)` | `(integerp x)` | Predicates |
| **progn** | `(progn ...)` | `(block ...)` | Returns last expr |
| **return** | `(return-from ...)` | `(return val)` | Early exit |
| **length** | `(length x)` | `(size x)` | ⚠️ All containers |
| **nth** | `(nth n list)` | `(@ list n)` | 0-indexed |
| **aref** | `(aref arr i j)` | `(@ arr i j)` | Multi-dim access |
| **setf** | `(setf x val)` | `(setq x val)` | No setf |
| **index error** | `(nth 999 list)` | `(check (< i (size list)) (@ list i))` | ⚠️ No bounds check |
| **key error** | `(gethash d "k")` | `(select (@ d "k") default)` | ⚠️ Safe access |
| **type coerce** | `"age: " + 42` | `(+ "age: " (string 42))` | ⚠️ Explicit convert |

### Most Critical Errors (Runtime Failures)

```lisp
; ❌ WRONG - Index out of bounds
(@ list 999)

; ✅ RIGHT - Check bounds first
(if (< index (size list))
    (@ list index)
    "default")

; ❌ WRONG - Accessing missing dictionary key
(@ dict "missing_key")

; ✅ RIGHT - Safe access with fallback
(select (@ dict "key") "default_value")

; ❌ WRONG - Concatenating string + number
(+ "Value: " 42)

; ✅ RIGHT - Explicit type conversion
(+ "Value: " (string 42))

; ❌ WRONG - Modifying global from function
(setq global_counter 0)
(defun increment()
    (setq global_counter (+ global_counter 1)))  ; Creates LOCAL!

; ✅ RIGHT - Use setg for globals
(setq global_counter 0)
(defun increment()
    (setg global_counter (+ global_counter 1)))

; ❌ WRONG - Modifying instance field
(class@ Counter (count)
    (defun inc()
        (setq count (+ count 1))))  ; Creates LOCAL!

; ✅ RIGHT - Use setqi for instance fields
(class@ Counter (count)
    (defun inc()
        (setqi count (+ count 1))))

; ❌ WRONG - Accessing nil
(car nil)

; ✅ RIGHT - Check for nil/empty first
(if (not (emptyp mylist))
    (car mylist)
    "list is empty")
```

---

## HOW DO I...? (PATTERNS)

### Create a class with methods?
```lisp
(class@ Person (name (age 0))
    (defun greet()
        (+ "Hello, I'm " name))
    (defun birthday()
        (setqi age (+ age 1)))  ; setqi for instance field
)
(setq p (Person "Alice" 30))
(p (greet))                     ; → "Hello, I'm Alice"
(@ p 'age)                      ; → 30
(set@ p 'age 31)                ; modify from outside
```

### Inherit from a class?
```lisp
(class@ Employee (salary) (from@ Person)
    (defun info()
        (+ (greet) ", salary: " (string salary)))
    (defun parent_greet()
        (from@ Person (greet)))  ; call parent method
)
```

### Iterate?
```lisp
(loopcount 10 i (println i))           ; 0..9
(loop item '(a b c) (println item))    ; over elements
(loop i (irange 0 100 5) (println i))  ; with step
(while (< x 10) (setq x (+ x 1)))      ; while loop
```

### Handle errors?
```lisp
(maybe
    (risky_operation)
    "fallback if error")

; Check before access
(if (not (emptyp list))
    (car list)
    "list is empty")

; Safe dictionary access
(select (@ dict "key") "default_value")
```

### Work with dictionaries?
```lisp
(setq d {"name":"Alice" "age":30})
(@ d "name")                           ; → "Alice"
(setq nested {"a":{"b":{"c":42}}})
(@ nested "a" "b" "c")                 ; → 42

; Safe access with default
(select (@ d "missing") "default")

; Check if key exists
(if (in d "name")
    (@ d "name")
    "not found")
```

### Use pattern matching?
```lisp
(defpat factorial (0) 1)
(defpat factorial (n) (* n (factorial (- n 1))))

(defpat process ([]) "empty")
(defpat process ([head $ rest])        ; $ captures rest
    (cons (transform head) (process rest)))
```

### Use threads?
```lisp
(dethread worker(id)
    (threadstore "result" (compute id)))
(worker 1) (worker 2)
(wait)
(println (threadretrieve "result"))

; Thread-safe variables
(threadspace
    (seth counter 0))
(dethread inc()
    (threadspace (+= counter 1)))
```

### Use block expressions?
```lisp
; Sequence of expressions (like progn)
(block
    (println "step 1")
    (println "step 2")
    (+ 1 2))              ; returns 3

; Early return from function
(defun find_first(list pred)
    (loop item list
        (check (pred item)
            (return item)))   ; exits function immediately
    nil)                      ; default if not found
```

### Check types and values?
```lisp
; Type predicates
(integerp 42)              ; → true
(stringp "hello")          ; → true
(listp '(1 2 3))           ; → true
(dictionaryp {"a":1})      ; → true

; State predicates
(nullp nil)                ; → true
(emptyp '())               ; → true
(zerop 0)                  ; → true
(negativep -5)             ; → true
```

### Choose the right container?
```yaml
choose_container:
  need_random_access: 
    use: "(list ...)"
    why: "Array-based, O(1) access"
    example: "(setq arr (list 1 2 3)) (@ arr 1)"
  
  need_prepend_efficiency:
    use: "(llist ...)"
    why: "Linked list, O(1) prepend"
    example: "(setq ll (llist 1 2 3)) (push ll 0)"
  
  need_unique_elements:
    use: "(set ...) or (seti ...) or (setn ...)"
    why: "Automatic deduplication"
    example: "(setq s (set 1 2 2 3)) ; → {1 2 3}"
  
  need_key_value_pairs:
    use: "(dictionary ...) or {...}"
    why: "Fast lookup by key"
    example: '(setq d {"name":"Alice"})'
  
  need_priority_queue:
    use: "(heap ...)"
    why: "Min/max heap operations"
    example: "(setq h (heap '>=< 5 3 7)) (car h) ; → 3"
  
  need_matrix_operations:
    use: "(rho ...) with irank"
    why: "Multi-dimensional arrays"
    example: "(setq m (rho 3 4 (iota 12)))"
```

---

## FUNCTION SIGNATURES (JSON)

```json
{
  "containers": {
    "@":       {"syntax": "(@ c key...)", "ex": "(@ dict \"a\" \"b\")"},
    "set@":    {"syntax": "(set@ c key val)", "ex": "(set@ obj 'field 10)"},
    "push":    {"syntax": "(push c item)", "note": "END for list, FRONT for llist"},
    "pop":     {"syntax": "(pop c)", "note": "removes from front"},
    "car/cdr": {"syntax": "(car l) (cdr l)", "combos": "cadr caddr caar cdar cddr"},
    "size":    {"syntax": "(size c)", "returns": "integer"},
    "in":      {"syntax": "(in c elem)", "returns": "boolean"}
  },
  "functional": {
    "map":     {"syntax": "(map fn list)", "ex": "(map '+ '(1 2) '(10 20))"},
    "filter":  {"syntax": "(filter pred list)", "ex": "(filter (λ(x)(> x 0)) lst)"},
    "zipwith": {"syntax": "(zipwith fn l1 l2)", "ex": "(zipwith '* '(1 2) '(3 4))"},
    "apply":   {"syntax": "(apply 'fn '(args))", "ex": "(apply '+ '(1 2 3))"}
  },
  "fast_list_ops": {
    "filterlist": "(filterlist cond list)",
    "droplist":   "(droplist cond list)",
    "takewhile":  "(takewhile cond list)",
    "flatten":    "(flatten nested)",
    "reverse":    "(reverse list)"
  },
  "strings": {
    "split":   {"syntax": "(split str delim)", "ex": "(split \"a,b\" \",\")"},
    "trim":    "(trim str)",
    "join":    {"syntax": "(join list sep)", "ex": "(join '(a b) \"/\")"},
    "+":       {"note": "concatenation", "ex": "(+ \"Hello\" \" \" \"World\")"}
  },
  "math": {
    "ops":     "+ - * / % ^^ & | ^ ~ << >>",
    "funcs":   "sqrt exp log sin cos floor ceiling round abs",
    "ranges":  "(iota n) (iota0 n) (irange start end step)",
    "agg":     "(sum c) (min c) (max c) (minmax c)"
  },
  "io": {
    "print":   "(println x) (printerrln x)",
    "file_r":  "(fread path)",
    "file_w":  "(fwrite path data)",
    "json":    "(json_parse str) (json obj) (json_read path)"
  },
  "predicates": {
    "type":    "atomp consp numberp integerp floatp stringp listp dictionaryp",
    "state":   "nullp zerop emptyp negativep positivep",
    "string":  "lowerp upperp digitp alphap"
  },
  "logic": {
    "bool":    "(and a b) (or a b) (not x) (xor a b)",
    "eq":      "(eq a b) (neq a b) (= a b)",
    "cmp":     "< <= > >= >=<"
  },
  "control": {
    "block":   {"syntax": "(block e1 e2 ...)", "note": "returns last expression"},
    "return":  {"syntax": "(return val)", "note": "early exit from function"},
    "break":   {"syntax": "(break)", "note": "exit current loop"}
  }
}
```

---

## DATA STRUCTURES (COMPACT)

| Type | Create | Access | Modify | Notes |
|------|--------|--------|--------|-------|
| **list** (array) | `(list a b)` | `(@ l i)` | `(set@ l i v)` | Default, O(1) random access |
| **llist** (linked) | `(llist a b)` | `(car l)` | `(push l x)` | Can contain cycles |
| **dict** | `{"k":v}` | `(@ d "k")` | `(set@ d "k" v)` | String keys by default |
| **set** | `(set a b)` | `(in s x)` | `(insert s x)` | Also: `sets seti setn` |
| **heap** | `(heap '>=< vals)` | `(car h)` | `(insert h x)` | Priority queue |
| **matrix** | `(rho 3 4 vals)` | `(@ m i j)` | | Use `irank` to transpose |

---

## CONTROL FLOW (DECISION TREE)

```
Need conditional?
├─ Simple if/else → (if cond then else)
├─ Multiple else statements → (ife cond then else1 else2 ...)
├─ Chain of conditions → (cond (c1 a1) (c2 a2) (true default))
├─ Execute only if true → (check cond action)
├─ Execute only if nil → (ncheck cond nil-action true-actions...)
└─ First non-nil value → (select v1 v2 v3)

Need loop?
├─ Fixed count → (loopcount n i body)
├─ Over list → (loop var list body)
├─ With condition → (while cond body)
├─ Over range → (loop i (irange start end step) body)
└─ Exit early → (break)

Need error handling?
└─ Try with fallback → (maybe (expr) fallback)
```

---

## REAL-WORLD TEMPLATES

> **💡 Ready-to-use code patterns for common tasks**

### Template: CLI Argument Parser

```lisp
(defun parse_args(args)
    "Parse command-line arguments into a dictionary"
    (setq opts (dictionary))
    (setq positional (list))
    
    (loop arg args
        (if (eq (@ arg 0) "-")
            ; Flag argument
            (block
                (setq key (trim arg "-"))
                (set@ opts key true))
            ; Positional argument
            (push positional arg)))
    
    (set@ opts "positional" positional)
    opts)

; Usage:
; (parse_args '("-v" "--debug" "input.txt" "output.txt"))
; → {"v":true "debug":true "positional":["input.txt" "output.txt"]}
```

### Template: File Processing Pipeline

```lisp
(defun process_file(path transformer)
    "Read file, transform lines, handle errors"
    (maybe
        (block
            (setq content (fread path))
            (setq lines (split content "\n"))
            (setq transformed (map transformer lines))
            (join transformed "\n"))
        (+ "Error reading file: " path)))

; Usage:
; (defun uppercase_line(line) (upper line))
; (process_file "input.txt" 'uppercase_line)
```

### Template: Simple HTTP-like Response

```lisp
(defun make_response(status body)
    "Create a response dictionary with metadata"
    (dictionary
        "status" status
        "body" body
        "timestamp" (now)
        "content_type" "text/plain"))

(defun success_response(data)
    (make_response 200 data))

(defun error_response(message)
    (make_response 500 message))

; Usage:
; (success_response "Operation completed")
; (error_response "File not found")
```

### Template: Data Validation

```lisp
(defun validate_user(user)
    "Validate user record, return errors or nil"
    (setq errors (list))
    
    ; Check required fields
    (ncheck (in user "name")
        (push errors "Missing required field: name"))
    
    (ncheck (in user "email")
        (push errors "Missing required field: email"))
    
    ; Check email format
    (check (in user "email")
        (ncheck (rgx@ (@ user "email") "[^@]+@[^@]+")
            (push errors "Invalid email format")))
    
    ; Check age if present
    (check (in user "age")
        (check (< (@ user "age") 0)
            (push errors "Age must be positive")))
    
    (if (emptyp errors) nil errors))

; Usage:
; (validate_user {"name":"Alice" "email":"invalid"})
; → ["Invalid email format"]
```

### Template: Retry with Exponential Backoff

```lisp
(defun retry(fn max_attempts delay)
    "Retry function with exponential backoff"
    (setq attempt 0)
    (setq result nil)
    
    (while (and (< attempt max_attempts) (nullp result))
        (setq result
            (maybe
                (fn)
                nil))
        
        (ncheck result
            (block
                (+= attempt 1)
                (check (< attempt max_attempts)
                    (block
                        (println "Retry" attempt "after" delay "ms")
                        (sleep delay)
                        (setq delay (* delay 2)))))))
    
    (select result "All attempts failed"))

; Usage:
; (retry (λ() (fetch_unreliable_api)) 5 100)
```

### Template: Simple State Machine

```lisp
(defun state_machine(initial_state transitions)
    "Create a state machine with transition table"
    (setq current_state initial_state)
    
    (defun process_event(event)
        (setq key (+ (string current_state) ":" (string event)))
        (setq next_state (@ transitions key))
        
        (if (not (nullp next_state))
            (block
                (println "Transition:" current_state "→" next_state)
                (setq current_state next_state)
                current_state)
            (block
                (println "Invalid transition:" current_state event)
                current_state)))
    
    'process_event)

; Usage:
; (setq fsm (state_machine 'idle {
;     "idle:start" 'running
;     "running:pause" 'paused
;     "paused:resume" 'running
;     "running:stop" 'idle
; }))
; (fsm 'start)   ; → running
; (fsm 'pause)   ; → paused
```

### Template: Memoization

```lisp
(defun memoize(fn)
    "Cache function results by arguments"
    (setq cache (dictionary))
    
    (λ (args)
        (setq key (string args))
        (if (in cache key)
            (@ cache key)
            (block
                (setq result (apply fn args))
                (set@ cache key result)
                result))))

; Usage:
; (setq fib_memo (memoize 'fibonacci))
; (fib_memo 100)  ; Fast after first call
```

### Template: JSON API Response Builder

```lisp
(defun json_response(data (status 200) (message "OK"))
    "Build standardized JSON API response"
    (setq response (dictionary
        "status" status
        "message" message
        "data" data
        "timestamp" (now)))
    
    (json response))

(defun paginate(items page size)
    "Paginate a list and return metadata"
    (setq start (* (- page 1) size))
    (setq end (+ start size))
    (setq total (size items))
    (setq page_items (extract items start end))
    
    (dictionary
        "items" page_items
        "page" page
        "size" size
        "total" total
        "pages" (ceiling (/ total size))))

; Usage:
; (json_response (paginate my_data 1 10))
```

---

## MIGRATION FROM OTHER LANGUAGES

### From Python

```yaml
python_to_lispe:
  list_comprehension:
    python: "[x * 2 for x in items]"
    lispe: "(map (λ(x) (* x 2)) items)"
  
  dict_get_default:
    python: "d.get('key', 'default')"
    lispe: "(select (@ d \"key\") \"default\")"
  
  enumerate:
    python: "enumerate(items)"
    lispe: "(zipwith 'list (iota (size items)) items)"
  
  zip:
    python: "zip(list1, list2)"
    lispe: "(zipwith 'list list1 list2)"
  
  join:
    python: "'/'.join(items)"
    lispe: "(join items \"/\")"
  
  split:
    python: "s.split(',')"
    lispe: "(split s \",\")"
  
  any:
    python: "any(pred(x) for x in items)"
    lispe: "(foldl 'or (map pred items) false)"
  
  all:
    python: "all(pred(x) for x in items)"
    lispe: "(foldl 'and (map pred items) true)"
  
  range:
    python: "range(0, 10, 2)"
    lispe: "(irange 0 10 2)"
  
  len:
    python: "len(container)"
    lispe: "(size container)"
```

### From Common Lisp

```yaml
common_lisp_to_lispe:
  defvar:
    cl: "(defvar *global* 10)"
    lispe: "(setq global 10)"
    note: "No earmuffs, use setq"
  
  push:
    cl: "(push item list)"
    lispe: "(push list item)"
    note: "⚠️ Arguments reversed!"
  
  format:
    cl: '(format t "~a ~d" str num)'
    lispe: '(format "%1 %2" str num)'
    note: "Different format strings"
  
  progn:
    cl: "(progn expr1 expr2)"
    lispe: "(block expr1 expr2)"
    note: "Different name"
  
  nth:
    cl: "(nth n list)"
    lispe: "(@ list n)"
    note: "Universal @ operator"
  
  aref:
    cl: "(aref array i j)"
    lispe: "(@ array i j)"
    note: "@ works for all containers"
  
  length:
    cl: "(length seq)"
    lispe: "(size seq)"
    note: "Different name"
  
  setf:
    cl: "(setf x val)"
    lispe: "(setq x val)"
    note: "No setf in LispE"
  
  defclass:
    cl: "(defclass name () (slot1 slot2))"
    lispe: "(class@ name (slot1 slot2) ...)"
    note: "Different class syntax"
  
  null:
    cl: "(null x)"
    lispe: "(nullp x) or (not x)"
    note: "Predicate suffix"
```

### From JavaScript

```yaml
javascript_to_lispe:
  map:
    js: "items.map(x => x * 2)"
    lispe: "(map (λ(x) (* x 2)) items)"
  
  filter:
    js: "items.filter(x => x > 0)"
    lispe: "(filter (λ(x) (> x 0)) items)"
  
  reduce:
    js: "items.reduce((acc, x) => acc + x, 0)"
    lispe: "(foldl '+ items 0)"
  
  forEach:
    js: "items.forEach(x => console.log(x))"
    lispe: "(loop x items (println x))"
  
  object_access:
    js: "obj.field or obj['field']"
    lispe: "(@ obj 'field) or (@ obj \"field\")"
  
  array_length:
    js: "arr.length"
    lispe: "(size arr)"
  
  includes:
    js: "arr.includes(item)"
    lispe: "(in arr item)"
  
  join:
    js: "arr.join('/')"
    lispe: "(join arr \"/\")"
  
  split:
    js: "str.split(',')"
    lispe: "(split str \",\")"
```

---

## DEBUGGING & PERFORMANCE

### Debugging Tips

```yaml
debugging:
  inspect_type:
    function: "(typeof x)"
    example: "(println \"Type:\" (typeof myvar))"
    returns: "Symbol like 'integer, 'string, 'list"
  
  print_debug:
    function: "(println \"DEBUG:\" var)"
    example: "(println \"Value:\" x \"Type:\" (typeof x))"
    note: "Use println for debugging, not print"
  
  trace_execution:
    function: "(trace 'function_name)"
    example: "(trace 'my_function)"
    note: "Shows function calls and returns"
  
  check_size:
    function: "(size container)"
    example: "(println \"List size:\" (size mylist))"
    note: "Works on lists, strings, dicts, sets"
  
  safe_access:
    pattern: "(select (@ container key) default)"
    example: "(select (@ dict \"missing\") \"NOT_FOUND\")"
    note: "Returns default instead of error"
  
  boundary_check:
    pattern: "(if (< index (size list)) (@ list index) default)"
    example: |
      (if (and (>= i 0) (< i (size arr)))
          (@ arr i)
          "out of bounds")
  
  null_check:
    pattern: "(if (not (nullp x)) (use x) fallback)"
    example: "(if (not (emptyp mylist)) (car mylist) nil)"
  
  type_check:
    pattern: "(if (integerp x) (compute x) error)"
    example: "(if (stringp input) (process input) \"Invalid input\")"
  
  debug_macro:
    code: |
      (defmacro debug (expr)
          (list 'block
              (list 'println "DEBUG:" (list 'quote expr) "=>" expr)
              expr))
    usage: "(debug (+ 1 2))  ; prints: DEBUG: (+ 1 2) => 3"
  
  assert_macro:
    code: |
      (defmacro assert (cond msg)
          (list 'if (list 'not cond)
              (list 'println "ASSERTION FAILED:" msg)))
    usage: "(assert (> x 0) \"x must be positive\")"
```

### Performance Hints

```yaml
performance:
  prefer_typed_lists:
    slow: "(list 1 2 3 4 5)"
    fast: "(integers 1 2 3 4 5)"
    benefit: "Faster iteration and arithmetic operations"
    when: "Lists of uniform type (all ints, all floats, all strings)"
  
  use_filterlist:
    slow: "(filter (λ(x) (> x 0)) large_list)"
    fast: "(filterlist (> $ 0) large_list)"
    benefit: "Optimized for large lists, no lambda overhead"
    when: "Simple predicates on large lists"
  
  avoid_nested_access:
    slow: |
      (loop i (irange 0 1000 1)
          (println (@ (@ dict "key") i)))
    fast: |
      (setq inner (@ dict "key"))
      (loop i (irange 0 1000 1)
          (println (@ inner i)))
    benefit: "Avoid repeated dictionary lookups"
  
  prefer_loopcount:
    slow: "(loop i (irange 0 1000 1) body)"
    fast: "(loopcount 1000 i body)"
    benefit: "Direct counter, no range object creation"
    when: "Simple counted loops from 0"
  
  use_block_for_sequence:
    slow: |
      (setq r1 (step1))
      (setq r2 (step2 r1))
      (setq r3 (step3 r2))
      r3
    fast: |
      (block
          (setq r1 (step1))
          (setq r2 (step2 r1))
          (step3 r2))
    benefit: "Clearer intent, potential optimization"
  
  preallocate_lists:
    slow: |
      (setq result (list))
      (loopcount 10000 i (push result i))
    fast: |
      (setq result (integers (irange 0 10000 1)))
    benefit: "Avoid repeated memory allocation"
    when: "Known size in advance"
  
  use_local_variables:
    slow: |
      (defun compute()
          (+ (@ global_dict "a") (@ global_dict "b") (@ global_dict "c")))
    fast: |
      (defun compute()
          (setq a (@ global_dict "a"))
          (setq b (@ global_dict "b"))
          (setq c (@ global_dict "c"))
          (+ a b c))
    benefit: "Faster local variable access"
  
  choose_right_container:
    random_access: "(list ...) - O(1) indexing"
    prepend: "(llist ...) - O(1) prepend"
    unique: "(set ...) - O(1) membership"
    key_value: "(dictionary ...) - O(1) lookup"
    priority: "(heap ...) - O(log n) min/max"
  
  tail_recursion:
    slow: |
      (defun factorial(n)
          (if (= n 0) 1 (* n (factorial (- n 1)))))
    fast: |
      (defun factorial(n (acc 1))
          (if (= n 0) acc (factorial (- n 1) (* n acc))))
    benefit: "Optimized to loop, no stack overflow"
    note: "LispE optimizes tail calls"
```

### Common Performance Pitfalls

```lisp
; ❌ AVOID: Repeated @ access in loop
(loopcount 1000 i
    (println (@ nested "a" "b" "c")))

; ✅ PREFER: Cache the lookup
(setq value (@ nested "a" "b" "c"))
(loopcount 1000 i
    (println value))

; ❌ AVOID: Creating ranges for simple counts
(loop i (irange 0 1000 1)
    (println i))

; ✅ PREFER: loopcount for simple iteration
(loopcount 1000 i
    (println i))

; ❌ AVOID: Generic list for uniform data
(setq numbers (list 1 2 3 4 5 6 7 8 9 10))

; ✅ PREFER: Typed list for better performance
(setq numbers (integers 1 2 3 4 5 6 7 8 9 10))

; ❌ AVOID: filter with lambda for simple predicates
(filter (λ(x) (> x 0)) large_list)

; ✅ PREFER: filterlist with inline predicate
(filterlist (> $ 0) large_list)
```

---

## VARIABLES SCOPE

```
┌─────────────────────────────────────────────────┐
│ GLOBAL SCOPE                                    │
│   (setq x 10)        ; define global            │
│                                                 │
│  ┌────────────────────────────────────────────┐ │
│  │ FUNCTION SCOPE                             │ │
│  │   (defun f()                               │ │
│  │       (setq y 20)    ; local to function   │ │
│  │       (setg x 30)    ; modify global       │ │
│  │   )                                        │ │
│  │  ┌───────────────────────────────────────┐ │ │
│  │  │ CLASS SCOPE                           │ │ │
│  │  │   (class@ C (field)                   │ │ │
│  │  │       (defun m()                      │ │ │
│  │  │           (setq z 1)   ; local        │ │ │
│  │  │           (setqi field 2) ; instance  │ │ │
│  │  │       ))                              │ │ │
│  │  └───────────────────────────────────────┘ │ │
│  │  ┌───────────────────────────────────────┐ │ │
│  │  │ LET SCOPE                             │ │ │
│  │  │   (let ((a 1)(b 2))                   │ │ │
│  │  │       (+ a b))     ; a,b destroyed    │ │ │
│  │  └───────────────────────────────────────┘ │ │
│  └────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────┘
```

---

## ADVANCED FEATURES (COMPACT)

### Pattern Matching with defpat
```lisp
; Type constraints
(defpat f ([integer_ (> x 0)] x) "positive int")
(defpat f ([string_ (> (size x) 5)] x) "long string")
(defpat f (x) "fallback")

; Kleene operators
(defpat f ([(< x 10)+ $ rest]) ...)  ; + one or more
(defpat f ([(> x 0)* $ rest]) ...)   ; * zero or more
(defpat f ([item% $ rest]) ...)      ; % optional
```

### Abstract Data Types (data@)
```lisp
(data@ [Point integer_ integer_] [Circle [Point _ _] float_])
(setq p (Point 10 20))              ; validated creation
; Type constraints: atom_ integer_ float_ string_ list_ ...
```

### Macros
```lisp
(defmacro unless (cond body)
    (list 'if (list 'not cond) body))
(unless (empty list) (process list))
```

### Constructor/Destructor
```lisp
(class@ Resource (handle)
    (defun init()
        (setqi handle (open_resource))
        (toclean 'cleanup))         ; register destructor
    (defun cleanup()
        (close_resource handle)))
(setq r (Resource (init)))          ; call constructor
```

---

## COMPLETE EXAMPLES

### Text Processing Pipeline
```lisp
(defun process_text(text stopwords synonyms)
    (filter (λ(x) (not (in stopwords x)))
        (map (λ(x) (select (@ synonyms x) x))
            (map 'atom
                (split (lower text) " ")))))
```

### Game State Pattern
```lisp
(setg position '(0 0))
(setg inventory (set))

(defpat action ([Move dir])
    (setg position (zipwith '+ position (@ directions dir)))
    "You move")

(defpat action ([Take obj])
    (check (in room_items obj)
        (insert inventory obj)
        (+ "Took " (keystr obj))))

(defun game_loop()
    (while (not game_over)
        (maybe (action (parse (input@)))
               "I don't understand")))
```

### Threaded Computation
```lisp
(threadspace (seth results '()))

(dethread compute(id data)
    (setq r (heavy_computation data))
    (threadspace
        (seth results (cons (list id r) results))))

(loop i (irange 0 10 1)
    (compute i (@ dataset i)))
(wait)
(println results)
```

---

## REMINDERS (COMPACT CHECKLIST)

```
✓ class@ not defclass          ✓ setq not defvar
✓ (use 'mod) not (require ...)  ✓ consistent naming style
✓ (obj (method)) for calls      ✓ + for string concat
✓ @ for container access        ✓ push → END (list)
✓ push → FRONT (llist)          ✓ setqi for instance fields
✓ setg for global from func     ✓ (integer "10") converts
✓ maybe for error handling      ✓ loopcount for counted loops
✓ ; for comments (single!)      ✓ list = array (default)
✓ llist = linked list           ✓ block not progn
✓ size not length               ✓ (return val) for early exit
✓ (break) to exit loops         ✓ nullp/emptyp to check nil
✓ integerp/stringp for types    ✓ select for safe access
✓ (push list item) NOT reversed ✓ bounds checking required
```

---

## EXAMPLES REPOSITORY STRUCTURE

```
examples/
├── AdventOfCode2021-2024/  # Real-world problem solving
├── patterns/               # Pattern matching (defpat, defpred, classes)
│   ├── minizork_en.lisp    # Text adventure game
│   ├── classes.lisp        # Class examples
│   ├── kleene.lisp         # Kleene operators
│   └── prolog.lisp         # Prolog-style predicates
├── threads/                # Threading examples
├── diverse/                # General examples
│   ├── factorial.lisp
│   ├── fibo.lisp
│   ├── macros.lisp
│   └── tail_recursion_pattern.lisp
├── haskellish/             # Haskell-inspired patterns
├── ollama/                 # LLM integration
├── sockets/                # Network programming
└── genetic/                # Genetic algorithms
```

---

## OFFICIAL DOCUMENTATION (WIKI)

> **📚 Full documentation**: https://github.com/naver/lispe/wiki  
> **📁 Examples directory**: `lispe/examples/` (dozens of working examples)

### Quick Links

| Topic | Link |
|-------|------|
| **Index of Functions** | [All functions](https://github.com/naver/lispe/wiki/Index-of-functions) |
| **LispE in a Nutshell** | [Quick overview](https://github.com/naver/lispe/wiki/9.-LispE-in-a-Nutshell) |
| **Classes** | [6.21 Classes](https://github.com/naver/lispe/wiki/6.21-Classes) |
| **Pattern Matching** | [6.1 Pattern Matching](https://github.com/naver/lispe/wiki/6.1-Pattern-Matching-in-LispE) |
| **Threads** | [6.3 Threads](https://github.com/naver/lispe/wiki/6.3-Threads) |
| **Data Structures** | [6.7 Data Structures](https://github.com/naver/lispe/wiki/6.7-Data-Structures) |

### Documentation Index

```yaml
introduction:
  main: "https://github.com/naver/lispe/wiki/1.-Introduction"
  compilation: "https://github.com/naver/lispe/wiki/1.1-Note-on-compilation"
  jag_editor: "https://github.com/naver/lispe/wiki/1.2-Jag:-Terminal-Editor-With-Mouse-Support-and-Colour-Highlighting"
  installers: "https://github.com/naver/lispe/wiki/1.3-Installers-for-Mac-OS-and-Windows"

implementation:
  structure: "https://github.com/naver/lispe/wiki/2.1-The-Structure-of-the-Interpreter"
  how_it_works: "https://github.com/naver/lispe/wiki/2.2-How-the-Interpreter-Works"
  list_impl: "https://github.com/naver/lispe/wiki/2.3-Lists"
  tail_call: "https://github.com/naver/lispe/wiki/2.4-Catch-a-Tail-Call"
  vs_python: "https://github.com/naver/lispe/wiki/2.5--LispE-vs.-Python:-A-Stochastic-Gradient-Descent-Comparison"

extensions:
  enrich: "https://github.com/naver/lispe/wiki/3.1-How-to-enrich-LispE"
  dynamic_libs: "https://github.com/naver/lispe/wiki/3.2-Adding-your-own-Dynamic-Libraries"
  use_in_c: "https://github.com/naver/lispe/wiki/3.3-Use-LispE-in-your-C---programs"

functions_and_operators:
  operators: "https://github.com/naver/lispe/wiki/5.1-Operators"
  functions: "https://github.com/naver/lispe/wiki/5.2-Functions"
  apl_style: "https://github.com/naver/lispe/wiki/5.3-A-la-APL"
  haskell_style: "https://github.com/naver/lispe/wiki/5.4-A-la-Haskell"
  math: "https://github.com/naver/lispe/wiki/5.5-Math-and-Random-Functions"
  system: "https://github.com/naver/lispe/wiki/5.6-System-Functions"
  files: "https://github.com/naver/lispe/wiki/5.7-Files"
  strings: "https://github.com/naver/lispe/wiki/5.8-Strings"
  regex: "https://github.com/naver/lispe/wiki/5.9-Regular-Expressions"

libraries:
  graphics: "https://github.com/naver/lispe/wiki/5.10-Graphic-Instructions"
  sockets: "https://github.com/naver/lispe/wiki/5.12-Sockets"
  xml: "https://github.com/naver/lispe/wiki/5.13-XML"
  sqlite: "https://github.com/naver/lispe/wiki/5.14-SQLite-Library"
  transducer: "https://github.com/naver/lispe/wiki/5.15-Transducer"
  curl: "https://github.com/naver/lispe/wiki/5.16-cURL-Library"
  blas: "https://github.com/naver/lispe/wiki/5.17-BLAS-Library"
  python: "https://github.com/naver/lispe/wiki/5.18-The-Python-Library:-pylispe"

language_features:
  pattern_matching: "https://github.com/naver/lispe/wiki/6.1-Pattern-Matching-in-LispE"
  defpat: "https://github.com/naver/lispe/wiki/6.1.1-Pattern-Functions"
  defpred: "https://github.com/naver/lispe/wiki/6.1.2-Predicate-Functions"
  defmacro: "https://github.com/naver/lispe/wiki/6.1.3-Macro-Functions"
  list_functions: "https://github.com/naver/lispe/wiki/6.2-Functions-on-List"
  threads: "https://github.com/naver/lispe/wiki/6.3-Threads"
  python_bridge: "https://github.com/naver/lispe/wiki/6.5-Bridging-with-Python"
  infix: "https://github.com/naver/lispe/wiki/6.6-Infix-Notation"
  data_structures: "https://github.com/naver/lispe/wiki/6.7-Data-Structures"
  apl_inspired: "https://github.com/naver/lispe/wiki/6.8-Inspiration-from-APL"
  haskell_inspired: "https://github.com/naver/lispe/wiki/6.9-Retrofitting-Haskell-into-Lisp"
  extract: "https://github.com/naver/lispe/wiki/6.10-Stealing-from-Python:-the-'extract'-method"
  fltk_graphics: "https://github.com/naver/lispe/wiki/6.11-Building-your-own-Graphic-Library-based-on-FLTK"
  unicode: "https://github.com/naver/lispe/wiki/6.12-Working-with-UTF8,-UTF16-and-UTF32-characters-in-CPlusPlus,-and-by-the-way-with-emojis-as-well..."
  create_language: "https://github.com/naver/lispe/wiki/6.13-Create-your-own-language-with-a-lot-of-transpiling:-in-LispE"
  text_generation: "https://github.com/naver/lispe/wiki/6.14-Text-Generation-LispE-with-a-Grammar"
  webassembly: "https://github.com/naver/lispe/wiki/6.17-A-WebAssembly-version-of-LispE"
  classes: "https://github.com/naver/lispe/wiki/6.21-Classes"

tutorials:
  shell: "https://github.com/naver/lispe/wiki/7.-Shell"
  minizork: "https://github.com/naver/lispe/wiki/8.-Mini-Zork:-A-functional-adventure"
  nutshell: "https://github.com/naver/lispe/wiki/9.-LispE-in-a-Nutshell"
```

---

## VALIDATION TEST

> **🧪 Test your LLM's understanding with this prompt:**

```
Given this LispE reference, write a function that:
1. Reads a JSON file of user records
2. Filters users over 18 years old
3. Groups them by country using a dictionary
4. Returns a sorted list of (country, count) pairs

Include:
- Error handling with maybe
- Type checking
- Typed lists where appropriate
- Proper variable scoping

The JSON structure is:
[
  {"name": "Alice", "age": 25, "country": "USA"},
  {"name": "Bob", "age": 17, "country": "UK"},
  ...
]
```

**Expected output if LLM understands LispE correctly:**
- Uses `(use 'module)` not `(require ...)`
- Uses `(class@ ...)` if defining classes
- Uses `(setq ...)` not `(defvar ...)`
- Uses `(@ dict key)` for access
- Uses `(size ...)` not `(length ...)`
- Uses `(maybe ...)` for error handling
- Uses `(push list item)` with correct order
- Checks bounds before array access
- Converts types explicitly with `(string ...)` etc.

---

## TYPE HINTS (TypeScript-style Reference)

> **💡 For LLMs that prefer type signatures:**

```typescript
// Core functions
function @(container: any, ...keys: any[]): any
function set@(container: any, key: any, value: any): any
function setq(name: symbol, value: any): any
function setg(name: symbol, value: any): any  // global from function
function setqi(name: symbol, value: any): any // instance field

// Containers
function list(...items: any[]): array
function llist(...items: any[]): linked_list
function dictionary(...pairs: [string, any][]): dict
function set(...items: any[]): set_type

// Type converters
function integer(x: any): int
function float(x: any): float
function string(x: any): string

// Typed lists
function integers(...nums: number[]): int_array
function floats(...nums: number[]): float_array
function strings(...strs: string[]): string_array

// Control flow
function if(cond: bool, then: any, else?: any): any
function loop(var: symbol, list: list, body: any): any
function loopcount(n: int, var: symbol, body: any): any
function block(...exprs: any[]): any  // returns last
function return(value: any): never

// Predicates
function integerp(x: any): bool
function stringp(x: any): bool
function listp(x: any): bool
function nullp(x: any): bool
function emptyp(x: any): bool

// Functional
function map(fn: function, ...lists: list[]): list
function filter(pred: function, list: list): list
function foldl(fn: function, list: list, init: any): any

// Classes
function class@(name: symbol, fields: list, ...methods: any[]): class
```

---

**END OF REFERENCE**