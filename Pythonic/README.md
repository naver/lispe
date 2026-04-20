# Pythonic — A Python-to-LispE Transpiler

This transpiler converts a Python-like language (through **BasAIc**) into **LispE** code. It supports most common Python constructs along with some domain-specific extensions.

## Quick Start

```lisp
(load (+ _current "basic.lisp"))
(load (+ _current "transpiler.lisp"))

(setq code «
def fibonacci(n):
   if n <= 1:
      return n
   return fibonacci(n-1) + fibonacci(n-2)
»)

; Get the compiled LispE code as a string
(println . compilepython code)

; Get the abstract syntax tree
(println . abstract_tree code)

```

## Two Syntax Modes

### Mode A — Explicit closing tags

Uses keywords like `endif`, `endfunction`, `endfor`, `endwhile`, `endtry`, `endclass`, `endswitch`, etc.

```python
function test(x, y)
   if x > y
      return x + y
   else
      return x * y
   endif
endfunction
```

Use `compile` or `transpile` for this mode.

### Mode B — Python-style indentation with colons

Uses `:` at the end of block-opening lines and relies on indentation (like standard Python).

```python
def test(x, y):
   if x > y:
      return x + y
   else:
      return x * y
```

Use `compilepython` for this mode. It automatically injects the closing tags before compiling.

You can also use `injecte_labels` to convert indented Python code into the explicit-tag form.

## Main Functions

| Function | Description |
|---|---|
| `(compile code)` | Compiles code (Mode A) and returns a LispE string |
| `(transpile code)` | Compiles code (Mode A) and returns a LispE S-expression |
| `(abstract_tree code)` | Returns the raw abstract syntax tree |
| `(compilepython code)` | Compiles Python-style code (Mode B) to a LispE string |
| `(injecte_labels code)` | Converts Python indentation into explicit closing tags |

## Supported Syntax

### Control Flow

```python
if condition
   ...
elif other_condition
   ...
else
   ...
endif

while condition
   ...
endwhile

for x in collection
   ...
endfor

switch(a)
case "val1": expr1
case "val2": expr2
endswitch

try
   ...
except(e)
   ...
endtry
```

Inline `then` is also supported: `if x > 10 then x + 1 else x * 2 endif`

### Functions and Lambdas

```python
function name(params)
   ...
endfunction

# Or with def
def name(params)
   ...
enddef

# Lambda expressions
A = [lambda (x, y) x + y](10, 20)
A = [λ (x, y) x + y](10, 20)
```

### Classes

```python
class Test
  def __init__(self, x, y, z)
     self.k = x
     self.v = y
  enddef

  def func(self, x)
     self.u = x
  enddef
endclass
```

- `self.attr = value` translates to `(setqi attr value)`
- `self.attr.method()` translates to method calls on the attribute

### Expressions

- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `**` (power), `^`
- **Bitwise**: `&`, `|`, `<<`, `>>`
- **Comparison**: `==`, `!=`, `<>`, `<`, `>`, `<=`, `>=`, `in`
- **Logical**: `and`, `or`, `xor`, `not`
- **Compound assignment**: `+=`, `-=`, `*=`, `/=`
- **Walrus operator**: `a := expr`
- **Global assignment**: `v =: expr` (uses `setg` instead of `setq`)

### Data Structures

```python
# Lists
b = [1, 2, 3]

# Dictionaries
d = {"a": 2, "b": 3, "c": 24}

# Multiple assignment
[e, r] = [10, 20]
```

### Slicing and Indexing

```python
a[1:]       # from index 1 to end
a[:3]       # from start to index 3
a[2:4]      # from index 2 to 4
a[2:-1]     # from index 2 to last
a[:]        # full copy

a[2, 3] or a[2][3]     # nested access (at a 2 3)
```

### List Comprehensions

```python
B = [x * 2 for x in [1, 2, 3, 4]]
B = [x ** 2 for x in [1, 2, 3, 4] if x % 2 == 0]
```

### Strings

```python
# f-strings
a = f"result: {json b} done"

# Triple-quoted strings
a = """This is a
multi-line string"""
```

### Method Chaining

```python
c = rt.size(209).json()[10]
prompts = prompts.atob().json_parse()
```

### Inline LispE

Embed raw LispE expressions using backticks:

```python
u = `(+ u 20)`
```

### Built-in Aliases

| Python | LispE |
|---|---|
| `append` | `push` |
| `len(x)` | `(size x)` |
| `strip` | `trim` |
| `True` | `true` |
| `False` | `nil` |
| `None` | `nil` |

## Extensions (Beyond Standard Python)

### Rules — Logic predicates with guards

```python
rule execute_tool(chat, msg)
   ("confluence_search" in msg)
   arg = jsonextract(msg, "data")
   call_mcp("confluence", "confluence_search_pages", arg, 'affiche')
endrule
```

The first expression acts as a **guard** (predicate). Compiles to `defpred`.

### Patterns — Pattern matching on arguments

```python
pattern test(prompt, "Chat 2", 'aa', [a, b $ c], e, {"a": "b" $ z})
    println a, b, c
endpattern
```

Supports destructuring of lists and dictionaries with the tail operator `$`. Compiles to `defpat`.

### JS variants — Automatic JSON decoding

`functionjs`, `defjs`, `rulejs`, `patternjs` automatically decode the first parameter from base64 JSON:

```python
defjs entrypoint(chat):
    println "Phrase:", chat[-1, "content"]
```

The first parameter is automatically wrapped with `(json_parse . atob param)`.

### Comments

```python
REM This is a BASIC-style comment
# This is a Python-style comment (in Mode B)
```

## Architecture

| File | Role |
|---|---|
| `basic` | BNF-like grammar definition |
| `compiler.lisp` | Meta-compiler: reads the grammar and generates `basic.lisp` |
| `basic.lisp` | Generated parser (functions `C_xxx` for each grammar rule) |
| `transpiler.lisp` | AST-to-LispE transformer via pattern matching |
| `rules.lisp` | Tokenization rules for the grammar compiler |
| `basic_example.lisp` | Usage examples |

## Example

Input (Python-like):

```python
def fibonacci(n):
   if n <= 1:
      return n
   return fibonacci(n-1) + fibonacci(n-2)
```

Output (LispE):

```lisp
(defun fibonacci (n)
   (if (<= n 1)
      (return n)
   )
   (return (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
)
```
