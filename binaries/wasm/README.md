# LispE WebAssembly

This directory contains the code to execute LispE as a WASM library.

Here is a list of the necessary files to test it (only the last two are necessary to run LispE as a WASM library)

### index.html
This file contains the minimum code to execute a LispE instruction from within a browser.
The loading and initialisation of the *LispE* interpreter are done in *lispe.js* and *lispe_functions.js*.

### lispe.js
This file contains the whole code to initialize the WASM library. It should be used in conjunction with *lispe_function.js*.

### lispe_functions.js
This is the encapsulation in JavaScript of the WASM libary loading.
It exposes many functions that demonstrated in *index.html* such as:

```JavaScript
function callEval(code); //which executes the execution of a piece of Tamgu code, it returns a string
```

### lispe.wasm
This is the WASM library, which is loaded with *lispe.js*.


### To launch the demo server
If you want to test it, you need to run a server in the directory containing the index.html file:

`python -m http.server --directory . 8080`

Then type: `http://localhost:8080` in a browser.
