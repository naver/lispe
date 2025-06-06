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

### Two possibilities

You can either compile the code with exception or without exception handling.

* sh no_exception.sh
* sh exception.sh

_Note_ that no_exception.sh applies a script to remove any exception handling in the code

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

