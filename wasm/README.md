# LispE WebAssembly

This is the code to compile a WASM version of LispE.

### Some Documentations
1. [C to WASM](https://developer.mozilla.org/en-US/docs/WebAssembly/C_to_wasm)
1. See [Pragmatic compiling C++ to WASM](https://medium.com/@tdeniffel/pragmatic-compiling-from-c-to-webassembly-a-guide-a496cc5954b8) for some nice explanations on how to do it
1. [Calling a method](https://wasmbyexample.dev/examples/strings/strings.c.en-us.html)

### To install it

Do `git clone https://github.com/juj/emsdk.git` to create a new version.

```
./emsdk install latest
./emsdk activate latest
```

### To compile

The Makefile has been modified to be able to use: **emsdk**

`make all`

### To launch the server

Move to the directory where `lispe.wasm` is located:

`emrun --port 8080 .`

You can also use python:

`python -m http.server --directory . 8080`

## Important Files

### index.html
This file contains the minimum code to execute a LispE instruction from within a browser.
The loading and initialisation of the *LispE* interpreter is done in *lispe_run.js*

### lispe_run.js
This is the encapsulation in JavaScript of the WASM libary loading.
It exposes:
```JavaScript
function setMaxSizeInput(v); //which set the maximum size of code sent to Tamgu 
function callEval(code); //which executes the execution of a piece of Tamgu code, it returns a string
function callResetLispE(); which resets the current Tamgu interpreter
```

### lispe.wasm
This is the WASM library, which is loaded with tamgu_run.js.

## New Instructions:

**IMPORTANT** _these functions are only available in LispE WASM library._

### asyncjs

Applies an asynchrone function associated with a LispE callback function with arguments:

```
(asyncjs "call_my_js_script(10,20)" lispecallback arg1 arg2)
```

The callback function is optional. `asyncjs` launches a Promise, which once it is executed will call the lispecallback.

```Lisp

(defun recall(val id)
   (setg test val)
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
(evals "10+30") ; returns 40
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
 
