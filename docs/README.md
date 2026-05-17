# LispE WebAssembly

The following files are the ones needed to use LispE in your Javascript applications.

## index.html
This file contains a complete working example to execute LispE code from within a browser. It demonstrates how to initialise the WASM module, capture `print`/`println` output, and use the JavaScript API to evaluate, reset, and indent code.

## lispe.js

This is the code that creates the bridge between `lispe.wasm` and Javascript. This file has been automatically created at compiling time.

## lispe_functions.js
This is the encapsulation in JavaScript of the WASM library loading.
It exposes the full JavaScript API described below.

## lispe.wasm
This is the WASM library, which is loaded with `lispe.js`.

---

# JavaScript API Reference

All functions that take a `lispe_idx` parameter refer to an interpreter instance index. The default interpreter is index `0`.

## Interpreter Lifecycle

### `callCreateLispE()`
Creates a new LispE interpreter instance. Returns its index (integer).

*Note that there is a default interpreter, which is created with the index `0`, which means that if you intend to use only one interpreter, you don't need to call this function.*

```JavaScript
var idx = callCreateLispE();
```

### `callResetLispE(lispe_idx)`
Resets an existing LispE interpreter, clearing all its state. Returns `"true"`.

```JavaScript
callResetLispE(0); // reset the default interpreter
```

### `callCleanLispE(lispe_idx)`
Deletes a LispE interpreter instance, freeing its resources.

```JavaScript
callCleanLispE(idx);
```

## Code Evaluation

### `callEvalLispE(lispe_idx, code)`
Evaluates a LispE code string and returns the result as a string.

```JavaScript
var result = callEvalLispE(0, '(+ 10 20)'); // "30"
```

### `callEvalLispEAsInt(lispe_idx, code)`
Evaluates code and returns the result as an integer.

```JavaScript
var n = callEvalLispEAsInt(0, '(+ 3 4)'); // 7
```

### `callEvalLispEAsFloat(lispe_idx, code)`
Evaluates code and returns the result as a floating-point number.

```JavaScript
var f = callEvalLispEAsFloat(0, '(/ 22.0 7)'); // 3.142857...
```

### `callEvalLispEAsString(lispe_idx, code)`
Evaluates code and returns the result as a string (similar to `callEvalLispE` but uses a dedicated internal path).

```JavaScript
var s = callEvalLispEAsString(0, '(join (list "a" "b" "c") "-")');
```

### `callEvalLispEToFloats(lispe_idx, code)`
Evaluates code that returns a list of numbers and returns a `Float64Array`.

```JavaScript
var floats = callEvalLispEToFloats(0, '(normal_distribution 100)');
// floats is a Float64Array of 100 values
```

### `callEvalLispEToInts(lispe_idx, code)`
Evaluates code that returns a list of integers and returns an `Int32Array`.

```JavaScript
var ints = callEvalLispEToInts(0, '(range 1 10 1)');
```

### `callEvalLispEToStrings(lispe_idx, code)`
Evaluates code that returns a list of strings and returns a JavaScript array of strings.

```JavaScript
var strs = callEvalLispEToStrings(0, '(list "hello" "world")');
// strs == ["hello", "world"]
```

## Setting Variables from JavaScript

These functions allow you to set LispE variables directly from JavaScript without going through code evaluation.

### `callSetqInt(lispe_idx, variable_name, value)`
Sets a LispE variable to an integer value.

```JavaScript
callSetqInt(0, "x", 42);
callEvalLispE(0, '(println x)'); // prints 42
```

### `callSetqFloat(lispe_idx, variable_name, value)`
Sets a LispE variable to a float value.

```JavaScript
callSetqFloat(0, "pi", 3.14159);
```

### `callSetqString(lispe_idx, variable_name, value)`
Sets a LispE variable to a string value.

```JavaScript
callSetqString(0, "name", "LispE");
```

### `callSetqInts(lispe_idx, variable_name, int32array, length)`
Sets a LispE variable to a list of integers. `int32array` must be an `Int32Array`.

```JavaScript
var vals = new Int32Array([1, 2, 3, 4, 5]);
callSetqInts(0, "mylist", vals, vals.length);
```

### `callSetqFloats(lispe_idx, variable_name, float64array, length)`
Sets a LispE variable to a list of floats. `float64array` must be a `Float64Array`.

```JavaScript
var vals = new Float64Array([1.1, 2.2, 3.3]);
callSetqFloats(0, "data", vals, vals.length);
```

## Code Formatting

### `callIndent(code)`
Indents/formats a LispE code string and returns the formatted version.

```JavaScript
var formatted = callIndent('(defun f(x)(+ x 1))');
// Returns properly indented code
```

---

# Python Transpiler

The `index.html` example demonstrates a Python-to-LispE transpiler. It uses a dedicated secondary interpreter to load transpiler code (`basic_string.js` and `transpiler_string.js`), then transpiles Python code into LispE before evaluating it:

```JavaScript
// Create a dedicated interpreter for the transpiler
var pythonIdx = callCreateLispE();
callEvalLispE(pythonIdx, basic_code);       // from basic_string.js
callEvalLispE(pythonIdx, transpiler_code);  // from transpiler_string.js

// Transpile Python code to LispE
var python_code = 'print(2**10)';
var lispe_code = callEvalLispE(pythonIdx, '(compilepython «' + python_code + '»)');

// Execute the transpiled code in the main interpreter
callResetLispE(0);
var result = callEvalLispE(0, lispe_code);
```

---

# Initialisation & Output Capture

To use the WASM library in an HTML page, you need to configure the `Module` object **before** loading `lispe.js`. This lets you capture LispE `print`/`println` output:

```html
<script type="text/javascript">
    var Module = {
        preRun: [],
        postRun: [],
        print: (function () {
            var element = document.getElementById('output');
            if (element) element.value = '';
            return function (text) {
                if (arguments.length > 1)
                    text = Array.prototype.slice.call(arguments).join(' ');
                if (element) {
                    element.value += text + "\n";
                    element.scrollTop = element.scrollHeight;
                }
            };
        })(),
        printErr: (function () {
            return function (text) {
                console.error(text);
            };
        })()
    };
</script>
<script type="text/javascript" src="lispe.js"></script>
<script type="text/javascript" src="lispe_functions.js"></script>
```

Then you can call the API:

```html
<script type="text/javascript">
    // Evaluate code in the default interpreter
    var result = callEvalLispE(0, '(+ 1 2)');

    // Reset and re-run
    callResetLispE(0);
    result = callEvalLispE(0, '(mapcar (lambda(x) (* x x)) (range 1 10 1))');

    // Create a secondary interpreter
    var idx = callCreateLispE();
    callEvalLispE(idx, '(setq x 100)');
    callCleanLispE(idx);
</script>
```

---

# Error Handling

All API functions throw a JavaScript `Error` when the LispE evaluation fails. Wrap calls in try/catch:

```JavaScript
try {
    var result = callEvalLispE(0, '(/ 1 0)');
} catch (e) {
    console.error("LispE error:", e.message);
}
```

---

# LispE-specific WASM Instructions

**IMPORTANT** _these functions are only available in LispE WASM library._

### asyncjs

Applies an asynchronous function associated with a LispE callback function with arguments:

```
(asyncjs "call_my_js_script(10,20)" lispecallback arg1 arg2)
```

The callback function is optional. `asyncjs` launches a Promise, which once it is executed will call the lispecallback.

```Lisp

(defun recall(val id)
   (setg test val)
   (console_log id)
   (println id ":" val)
)

(setq url "http://localhost:1234")
(setq system "You are a specialist in programming languages.")
(setq prompt "Give me the code to multiply two complex numbers.")
(setq model "qwen2-math-1.5b-instruct@q8_0")

(setq query (f_ `call_lm_studio("{url}", "{model}", "{system}", "{prompt}");`))
(asyncjs query 'recall 1)
```
### evaljs


Applies the JavaScript interpreter on a string. Always returns a string.

```Lisp
(evaljs "10+30") ; returns 40
```


### Console

LispE also provides the following methods to display strings in the console, which can be executed in LispE code:

1. (console_log str) — Outputs a message to the web console.
1. (console_warn str) — Outputs a warning message to the web console.
1. (console_error str) — Outputs an error message to the web console.
1. (console_group str) — Creates a new inline group in the web console.
1. (console_debug str) — Outputs a message to the web console, only if the console's logging level is set to 'debug'.
1. (console_groupEnd) — Ends the most recent inline group in the web console.
1. (console_clear) — Clears the console.
1. (console_table str) — Displays data as a table.
1. (console_time str) — Starts a timer you can use to track how long an operation takes.
1. (console_info str) — Outputs an informational message to the web console.
1. (console_timeEnd str) — Stops a timer that was previously started by console_time.


