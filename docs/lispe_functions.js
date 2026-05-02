/** @type {function(*, string=)} */
function assert(condition, text) {
    if (!condition) {
        abort('Assertion failed' + (text ? ': ' + text : ''));
    }
}

var __ATPRERUN__ = []; // functions called before the runtime is initialized
var __ATINIT__ = []; // functions called during startup
var __ATMAIN__ = []; // functions called when main() is to be run
var __ATEXIT__ = []; // functions called during shutdown
var __ATPOSTRUN__ = []; // functions called after the main() is called

// Emscripten 5.x no longer exposes HEAP32/HEAPF64 on Module.
// Create fresh typed array views from the wasm memory buffer each time,
// since the buffer can be detached after memory growth (e.g. after a _malloc).
// IMPORTANT: never cache these views across a call that may grow memory.
function getWasmBuffer() { return (Module["memory"] || Module.wasmMemory).buffer; }
function getHEAP32() { return new Int32Array(getWasmBuffer()); }
function getHEAPF64() { return new Float64Array(getWasmBuffer()); }

//-----------------------------------------------------------------
// Encoding / decoding helpers
//-----------------------------------------------------------------

const encode = function stringToIntegerArray(str, array) {
    for (let i = 0; i < str.length; i++) {
        array[i] = str.charCodeAt(i);
    }
    array[str.length] = 0;
};

// Decode `sz` int32-encoded chars at `array` into a JS string, then free `array`.
const decode = function integerArrayToString(array, sz) {
    let str = "";
    const heap32 = getHEAP32();
    const base = array >> 2;
    for (let i = 0; i < sz; i++) {
        str += String.fromCharCode(heap32[base + i]);
    }
    Module._free(array);
    return str;
};

// Decode `sz` float64-encoded chars (one char per double) at `array` into a JS string.
const decode_float_as_str = function toFloatArrayAsStr(array, sz) {
    let str = "";
    const heapf64 = getHEAPF64();
    const base = array >> 3;
    for (let i = 0; i < sz; i++) {
        str += String.fromCharCode(heapf64[base + i]);
    }
    Module._free(array);
    return str;
};

const decode_float = function toFloatArray(array, sz) {
    const value = new Float64Array(sz);
    const heapf64 = getHEAPF64();
    const base = array >> 3;
    for (let i = 0; i < sz; i++) {
        value[i] = heapf64[base + i];
    }
    Module._free(array);
    return value;
};

const decode_int = function toIntArray(array, sz) {
    const value = new Int32Array(sz);
    const heap32 = getHEAP32();
    const base = array >> 2;
    for (let i = 0; i < sz; i++) {
        value[i] = heap32[base + i];
    }
    Module._free(array);
    return value;
};

const decode_size = function toFirstIntArray(array) {
    const val = getHEAP32()[array >> 2];
    Module._free(array);
    return val;
};

//-----------------------------------------------------------------
// Memory allocation helpers
//-----------------------------------------------------------------

function provideCharactersAsInts(code) {
    // We need some room for error messages
    let nb = code.length + 1;
    nb = Math.max(20, nb);
    const arr = new Int32Array(nb);
    for (let i = 0; i < code.length; i++) {
        arr[i] = code.charCodeAt(i);
    }
    arr[code.length] = 0;
    const a_buffer = Module._malloc(nb * 4);
    // getHEAP32() must be called AFTER the malloc, since malloc may grow memory.
    getHEAP32().set(arr, a_buffer >> 2);
    return a_buffer;
}

// Allocate (nb+1) int32 slots. If `values` is provided, copy them into the buffer.
function provideIntegers(nb, values) {
    const a_buffer = Module._malloc((nb + 1) * 4);
    if (values !== undefined && values !== null) {
        getHEAP32().set(values, a_buffer >> 2);
    }
    return a_buffer;
}

// Allocate (nb+1) float64 slots. If `values` is provided, copy them into the buffer.
function provideFloats(nb, values) {
    const a_buffer = Module._malloc((nb + 1) * 8);
    if (values !== undefined && values !== null) {
        getHEAPF64().set(values, a_buffer >> 3);
    }
    return a_buffer;
}

//-----------------------------------------------------------------
// Eval entry points returning vectors
//-----------------------------------------------------------------

function callEvalLispEToFloats(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_to_floats_lispe'];
    const string_to_array = provideCharactersAsInts(code);
    const the_size = provideIntegers(2);

    let float_result;
    try {
        float_result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
    let sz = decode_size(the_size);
    if (sz < 0) {
        sz *= -1;
        Module._free(string_to_array);
        const str = decode_float_as_str(float_result, sz);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return decode_float(float_result, sz);
}

function callEvalLispEToInts(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_to_ints_lispe'];
    const string_to_array = provideCharactersAsInts(code);
    const the_size = provideIntegers(2);

    let int_result;
    try {
        int_result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
    let sz = decode_size(the_size);
    if (sz < 0) {
        sz *= -1;
        Module._free(string_to_array);
        const str = decode(int_result, sz);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return decode_int(int_result, sz);
}

function callEvalLispEToStrings(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_to_strings_lispe'];
    const string_to_array = provideCharactersAsInts(code);
    const the_size = provideIntegers(2);

    try {
        const string_result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
        let sz = decode_size(the_size);
        if (sz < 0) {
            sz *= -1;
            Module._free(string_to_array);
            const str = decode(string_result, sz);
            throw new Error(str);
        }
        const characters = decode_int(string_result, sz);
        const str_vector = [];
        let s = "";
        for (let i = 0; i < sz; i++) {
            if (characters[i] == 0) {
                str_vector.push(s);
                s = "";
            }
            else {
                s += String.fromCharCode(characters[i]);
            }
        }
        if (s != "") {
            str_vector.push(s);
        }

        Module._free(string_to_array);
        return str_vector;
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
}

//-----------------------------------------------------------------
// Setq variants
//-----------------------------------------------------------------

// Set a LispE variable to an array of integers.
// `vals` must be an Int32Array (or array-like compatible with TypedArray.set).
function callSetqInts(lispe_idx, a_variable, vals, nb) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    if (!(vals instanceof Int32Array)) {
        throw new Error("Expecting an Int32Array");
    }

    const entryFunction = Module['_eval_setq_ints_lispe'];
    const string_to_array = provideCharactersAsInts(a_variable);
    const values = provideIntegers(nb, vals);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, values, nb);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(values);
        return handleException(e);
    }

    Module._free(values);
    if (ret < 0) {
        ret *= -1;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return true;
}

// Set a LispE variable to an array of floats.
// `vals` must be a Float64Array.
function callSetqFloats(lispe_idx, a_variable, vals, nb) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    if (!(vals instanceof Float64Array)) {
        throw new Error("Expecting a Float64Array");
    }

    const entryFunction = Module['_eval_setq_floats_lispe'];
    const string_to_array = provideCharactersAsInts(a_variable);
    const values = provideFloats(nb, vals);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, values, nb);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(values);
        return handleException(e);
    }

    Module._free(values);
    if (ret < 0) {
        ret *= -1;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return true;
}

// Set a LispE variable to a single integer.
function callSetqInt(lispe_idx, a_variable, value) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_setq_int_lispe'];
    const string_to_array = provideCharactersAsInts(a_variable);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, value);
    }
    catch (e) {
        Module._free(string_to_array);
        return handleException(e);
    }

    if (ret < 0) {
        ret *= -1;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return true;
}

// Set a LispE variable to a single float.
function callSetqFloat(lispe_idx, a_variable, value) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_setq_float_lispe'];
    const string_to_array = provideCharactersAsInts(a_variable);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, value);
    }
    catch (e) {
        Module._free(string_to_array);
        return handleException(e);
    }

    if (ret < 0) {
        ret *= -1;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return true;
}

// Set a LispE variable to a string.
function callSetqString(lispe_idx, a_variable, value) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    if (typeof value != "string") {
        throw new Error("Expecting a string as input");
    }

    const entryFunction = Module['_eval_setq_string_lispe'];
    const string_to_array = provideCharactersAsInts(a_variable);
    const value_to_array = provideCharactersAsInts(value);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, value_to_array, value.length);
    }
    catch (e) {
        Module._free(value_to_array);
        Module._free(string_to_array);
        return handleException(e);
    }

    Module._free(value_to_array);
    if (ret < 0) {
        ret *= -1;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return true;
}

//-----------------------------------------------------------------
// Eval entry points returning scalars / strings
//-----------------------------------------------------------------

function callEvalLispEAsInt(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_to_int_lispe'];
    const string_to_array = provideCharactersAsInts(code);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, code.length, Math.max(20, code.length));
    }
    catch (e) {
        Module._free(string_to_array);
        return handleException(e);
    }
    // Sentinel: if first slot was set to 27 (ESC), an error message was written into the buffer.
    const heap32 = getHEAP32();
    if (heap32[string_to_array >> 2] == 27) {
        heap32[string_to_array >> 2] = 32;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return ret;
}

function callEvalLispEAsFloat(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_to_float_lispe'];
    const string_to_array = provideCharactersAsInts(code);

    let ret;
    try {
        ret = entryFunction(lispe_idx, string_to_array, code.length, Math.max(20, code.length));
    }
    catch (e) {
        Module._free(string_to_array);
        return handleException(e);
    }
    const heap32 = getHEAP32();
    if (heap32[string_to_array >> 2] == 27) {
        heap32[string_to_array >> 2] = 32;
        const str = decode(string_to_array, ret);
        throw new Error(str);
    }
    Module._free(string_to_array);
    return ret;
}

function callEvalLispEAsString(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_to_string_lispe'];
    const string_to_array = provideCharactersAsInts(code);
    const the_size = provideIntegers(2);

    try {
        const result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
        Module._free(string_to_array);
        let sz = decode_size(the_size);
        if (sz < 0) {
            sz *= -1;
            const str = decode(result, sz);
            throw new Error(str);
        }
        return decode(result, sz);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
}

function callEvalLispE(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_lispe'];
    const string_to_array = provideCharactersAsInts(code);
    const the_size = provideIntegers(2);

    try {
        const result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
        Module._free(string_to_array);
        let sz = decode_size(the_size);
        if (sz < 0) {
            sz *= -1;
            const str = decode(result, sz);
            throw new Error(str);
        }
        return decode(result, sz);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
}

//---------------------------------------------------------------
// Interpreter lifecycle
// Note that the default LispE is idx=0
// callEvalIdx(0, code) <=> callEval(code)
//---------------------------------------------------------------

// Creates a new LispE interpreter. Returns its index.
function callCreateLispE() {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_create_lispe'];
    try {
        const lispe_idx = entryFunction();
        position_in_buffer = 0;
        return lispe_idx;
    }
    catch (e) {
        return handleException(e);
    }
}

// Delete a LispE interpreter.
function callCleanLispE(lispe_idx) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_clean_lispe'];
    try {
        entryFunction(lispe_idx);
        position_in_buffer = 0;
        return "true";
    }
    catch (e) {
        return handleException(e);
    }
}

// Reset a LispE interpreter.
function callResetLispE(lispe_idx) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_reset_lispe'];
    try {
        entryFunction(lispe_idx);
        position_in_buffer = 0;
        return "true";
    }
    catch (e) {
        return handleException(e);
    }
}

// Indent LispE code.
function callIndent(code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    const entryFunction = Module['_eval_indent_lispe'];
    const string_to_array = provideCharactersAsInts(code);
    const the_size = provideIntegers(2);

    try {
        const string_result = entryFunction(string_to_array, code.length, the_size);
        Module._free(string_to_array);
        const sz = decode_size(the_size);
        return decode(string_result, sz);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
}

//-----------------------------------------------------------------
