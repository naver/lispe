# LispE WebAssembly

The following files are the ones needed to use LispE in your Javascript applications.

## `index.html`

This file contains a complete working example to execute LispE code from within a browser. It demonstrates how to initialise the WASM module, capture `print`/`println` output, and use the JavaScript API to evaluate, reset, and indent code.

## `lispe.js`

This is the code that creates the bridge between `lispe.wasm` and Javascript. This file has been automatically created at compiling time.

## `lispe_functions.js`

This is the encapsulation in JavaScript of the WASM library loading. It exposes the full JavaScript API described below.

## `lispe.wasm`

This is the WASM library, which is loaded with `lispe.js`.

# JavaScript API Reference

All functions that take a `lispe_idx` parameter refer to an interpreter instance index. The default interpreter is index `0`.

## Interpreter Lifecycle

### `callCreateLispE()`

Creates a new LispE interpreter instance. Returns its index (integer).

Note that there is a default interpreter, which is created with the index `0`, which means that if you intend to use only one interpreter, you don't need to call this function.

```javascript
var idx = callCreateLispE();
```

### `callResetLispE(lispe_idx)`

Resets an existing LispE interpreter, clearing all its state. Returns the string `"true"` on success.

Note that the return value is a string, not a boolean. This is consistent with the rest of the API, which exchanges string values across the JS/WASM boundary by default. If you want to test for success, compare explicitly:

```javascript
if (callResetLispE(0) === "true") { /* ... */ }
```

### `callCleanLispE(lispe_idx)`

Deletes a LispE interpreter instance, freeing its resources.

```javascript
callCleanLispE(idx);
```

## Code Evaluation

### `callEvalLispE(lispe_idx, code)`

Evaluates a LispE code string and returns the result as a string. This is the general-purpose entry point: it works for any code, regardless of the type of the returned value, which is serialised to its textual representation.

```javascript
var result = callEvalLispE(0, '(+ 10 20)'); // "30"
```

### `callEvalLispEAsInt(lispe_idx, code)`

Evaluates code and returns the result as an integer. Use this when you know the LispE expression returns an integer and you want to avoid the cost of parsing the string returned by `callEvalLispE`.

```javascript
var n = callEvalLispEAsInt(0, '(+ 3 4)'); // 7
```

### `callEvalLispEAsFloat(lispe_idx, code)`

Evaluates code and returns the result as a floating-point number.

```javascript
var f = callEvalLispEAsFloat(0, '(/ 22.0 7)'); // 3.142857...
```

### `callEvalLispEAsString(lispe_idx, code)`

Evaluates code and returns the result as a string. Unlike `callEvalLispE`, this variant goes through a dedicated internal path optimised for string returns: use it when you know in advance that the LispE expression produces a string value, to avoid the generic serialisation path.

```javascript
var s = callEvalLispEAsString(0, '(join (list "a" "b" "c") "-")'); // "a-b-c"
```

In practice, `callEvalLispE` will work for the same code, but `callEvalLispEAsString` is slightly faster and signals intent in your JS code.

### `callEvalLispEToFloats(lispe_idx, code)`

Evaluates code that returns a list of numbers and returns a `Float64Array`. This is the recommended path for numerical computation: the WASM heap is shared with JavaScript through typed arrays, so no copy is involved.

```javascript
var floats = callEvalLispEToFloats(0, '(normal_distribution 100)');
// floats is a Float64Array of 100 values
```

### `callEvalLispEToInts(lispe_idx, code)`

Evaluates code that returns a list of integers and returns an `Int32Array`.

```javascript
var ints = callEvalLispEToInts(0, '(range 1 10 1)');
```

### `callEvalLispEToStrings(lispe_idx, code)`

Evaluates code that returns a list of strings and returns a JavaScript array of strings.

```javascript
var strs = callEvalLispEToStrings(0, '(list "hello" "world")');
// strs == ["hello", "world"]
```

## Setting Variables from JavaScript

These functions allow you to set LispE variables directly from JavaScript without going through code evaluation. This is the most efficient way to pass data from JS into the LispE interpreter.

### `callSetqInt(lispe_idx, variable_name, value)`

Sets a LispE variable to an integer value.

```javascript
callSetqInt(0, "x", 42);
callEvalLispE(0, '(println x)'); // prints 42
```

### `callSetqFloat(lispe_idx, variable_name, value)`

Sets a LispE variable to a float value.

```javascript
callSetqFloat(0, "pi", 3.14159);
```

### `callSetqString(lispe_idx, variable_name, value)`

Sets a LispE variable to a string value.

```javascript
callSetqString(0, "name", "LispE");
```

### `callSetqInts(lispe_idx, variable_name, int32array, length)`

Sets a LispE variable to a list of integers. `int32array` must be an `Int32Array`.

The `length` parameter is required by the WASM interface, which receives the buffer as a pointer and needs to know how many elements to read. In the common case, pass `int32array.length`. You can pass a smaller value to import only a prefix of the array.

```javascript
var vals = new Int32Array([1, 2, 3, 4, 5]);
callSetqInts(0, "mylist", vals, vals.length);
```

### `callSetqFloats(lispe_idx, variable_name, float64array, length)`

Sets a LispE variable to a list of floats. `float64array` must be a `Float64Array`. Same convention as `callSetqInts` for the `length` parameter.

```javascript
var vals = new Float64Array([1.1, 2.2, 3.3]);
callSetqFloats(0, "data", vals, vals.length);
```

## Code Formatting

### `callIndent(code)`

Indents and formats a LispE code string and returns the formatted version. Useful for editors and REPLs that want to display user code in a normalised form.

```javascript
var formatted = callIndent('(defun f(x)(+ x 1))');
// Returns properly indented code
```

## Initialisation & Output Capture

To use the WASM library in an HTML page, you need to configure the `Module` object before loading `lispe.js`. This lets you capture LispE `print`/`println` output:

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

## Error Handling

All API functions throw a JavaScript `Error` when the LispE evaluation fails. Wrap calls in `try`/`catch`:

```javascript
try {
    var result = callEvalLispE(0, '(/ 1 0)');
} catch (e) {
    console.error("LispE error:", e.message);
}
```

# LispE-specific WASM Instructions

**IMPORTANT:** these functions are only available in the LispE WASM library.

## `asyncjs`

Applies an asynchronous JavaScript function and associates it with a LispE callback function and optional arguments:

```lisp
(asyncjs "call_my_js_script(10,20)" lispecallback arg1 arg2 ...)
```

`asyncjs` launches a JavaScript `Promise`. When the promise resolves, the `lispecallback` function is called with:

- as its **first argument**, the value returned by the asynchronous JavaScript call;
- as its **subsequent arguments**, the values `arg1`, `arg2`, ... passed to `asyncjs` after the callback.

The callback is optional. If omitted, the promise still runs but its result is discarded.

### Example

```lisp
(defun recall(val id)
   (setg test val)
   (console_log id)
   (println id ":" val)
)

(setq url "http://localhost:1234")
(setq system "You are a specialist in programming languages.")
(setq prompt "Give me the code to multiply two complex numbers.")
(setq model "qwen2-math-1.5b-instruct@q8_0")

; f_ is a LispE formatted string: it interpolates {name} expressions
; with the values of the variables in the current scope.
(setq query (f_ `call_lm_studio("{url}", "{model}", "{system}", "{prompt}");`))

(asyncjs query 'recall 1)
```

In this example, `recall` is invoked once `call_lm_studio` resolves: its first argument `val` receives the result of the JS call, and its second argument `id` receives the value `1` passed at the end of the `asyncjs` invocation.

The `f_` prefix denotes a *formatted string*, similar to Python's f-strings: `{variable}` expressions inside the string are replaced by the current value of the variable. This is the recommended way to build dynamic JS calls without manual string concatenation.

## `evaljs`

Applies the JavaScript interpreter on a string. Always returns a string.

```lisp
(evaljs "10+30") ; returns "40"
```

## Console

LispE also provides the following methods to display strings in the console, which can be executed in LispE code:

- `(console_log str)` — Outputs a message to the web console.
- `(console_warn str)` — Outputs a warning message to the web console.
- `(console_error str)` — Outputs an error message to the web console.
- `(console_info str)` — Outputs an informational message to the web console.
- `(console_debug str)` — Outputs a message to the web console, only if the console's logging level is set to `debug`.
- `(console_group str)` — Creates a new inline group in the web console.
- `(console_groupEnd)` — Ends the most recent inline group in the web console.
- `(console_table str)` — Displays data as a table.
- `(console_time str)` — Starts a timer you can use to track how long an operation takes.
- `(console_timeEnd str)` — Stops a timer that was previously started by `console_time`.
- `(console_clear)` — Clears the console.
