/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  mainasm.cxx

//-------------------------------------------------------------------------------------------
#include <unistd.h>
#include "lispe.h"
#include <emscripten/emscripten.h>
//-------------------------------------------------------------------------------------------
void display_error(std::string str) {
    std::cout << str << std::endl;
    EM_ASM({console.error(UTF8ToString($0));}, str.c_str());
}

void display_error(u_ustring s) {
    string str;
    s_unicode_to_utf8(str, s);
    std::cout << str << std::endl;
    EM_ASM({console.error(UTF8ToString($0));}, str.c_str());
}

void display_console(std::string str) {
    std::cout << str << std::endl;
    EM_ASM({console.log(UTF8ToString($0));}, str.c_str());
}

void display_console(u_ustring s) {
    string str;
    s_unicode_to_utf8(str, s);
    std::cout << str << std::endl;
    EM_ASM({console.log(UTF8ToString($0));}, str.c_str());
}

string eval_js(string code, bool& error) {
    // Initialiser error à false
    error = false;
    
    // Exécuter le code JavaScript
    EM_ASM({
        window._lispeJsEvalResult = null;
        window._lispeJsEvalError = null;
        try {
            window._lispeJsEvalResult = eval(UTF8ToString($0));
        } catch(e) {
            window._lispeJsEvalError = e.toString();
        }
    }, code.c_str());

    // Vérifier s'il y a une erreur
    char* errorStr = (char*)EM_ASM_PTR({
        if (window._lispeJsEvalError) {
            var errorStr = window._lispeJsEvalError;
            // UTF-8 needs up to 4 bytes per character: size the buffer in bytes, not in characters
            var size = lengthBytesUTF8(errorStr) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(errorStr, buffer, size);
            return buffer;
        }
        return 0;
    });

    if (errorStr) {
        string result(errorStr);
        free(errorStr);
        error = true;
        return result;  // Retourne le message d'erreur
    }

    // Convertir le résultat en string
    char* resultStr = (char*)EM_ASM_PTR({
        var result = window._lispeJsEvalResult;
        var jsString;
        try {
            jsString = (result === null || result === undefined) ? "nil" : String(result);
        } catch(e) {
            jsString = "";
        }
        // UTF-8 needs up to 4 bytes per character: size the buffer in bytes, not in characters
        var size = lengthBytesUTF8(jsString) + 1;
        var buffer = Module._malloc(size);
        stringToUTF8(jsString, buffer, size);
        return buffer;
    });

    string result(resultStr);
    free(resultStr);
    return result;
}

//------------------------------------------------------------------------------------------
// evaljs_i, evaljs_d, evaljs_b: the JavaScript value comes back as a number or a boolean,
// without going through its text representation.
// Returns false (and the error message in message) if the evaluation failed
static bool run_js(string& code, string& message) {
    EM_ASM({
        window._lispeJsEvalResult = null;
        window._lispeJsEvalError = null;
        try {
            window._lispeJsEvalResult = eval(UTF8ToString($0));
        } catch(e) {
            window._lispeJsEvalError = e.toString();
        }
    }, code.c_str());

    char* errorStr = (char*)EM_ASM_PTR({
        if (window._lispeJsEvalError) {
            var errorStr = window._lispeJsEvalError;
            var size = lengthBytesUTF8(errorStr) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(errorStr, buffer, size);
            return buffer;
        }
        return 0;
    });

    if (errorStr) {
        message = errorStr;
        free(errorStr);
        return false;
    }
    return true;
}

// A double: Number(value), as in JavaScript (true -> 1, "2.5" -> 2.5, null -> 0, undefined -> NaN)
double eval_js_d(string code, bool& error, string& message) {
    error = !run_js(code, message);
    if (error)
        return 0;
    return EM_ASM_DOUBLE({
        return Number(window._lispeJsEvalResult);
    });
}

// An integer: Number(value) truncated toward zero; NaN and infinities give 0.
// The value goes through a double, which holds integers exactly up to 2^53
int64_t eval_js_i(string code, bool& error, string& message) {
    error = !run_js(code, message);
    if (error)
        return 0;
    double d = EM_ASM_DOUBLE({
        var v = Math.trunc(Number(window._lispeJsEvalResult));
        return isFinite(v) ? v : 0;
    });
    return (int64_t)d;
}

// A boolean: the JavaScript truth value (false, 0, "", null, undefined and NaN are false)
bool eval_js_b(string code, bool& error, string& message) {
    error = !run_js(code, message);
    if (error)
        return false;
    return EM_ASM_INT({
        return window._lispeJsEvalResult ? 1 : 0;
    });
}

// evaljs_j: the JavaScript value comes back as a LispE value.
// The returned kind tells what was received:
// 0: null or undefined, 1: false, 2: true, 3: integer (in number), 4: double (in number),
// 5: BigInt (its digits in text), 6: string (in text), 7: array or object (its JSON in text)
// Only arrays and objects go through JSON.stringify, scalar values come back directly
int eval_js_j(string code, bool& error, string& message, string& text, double& number) {
    error = !run_js(code, message);
    if (error)
        return 0;

    int kind = EM_ASM_INT({
        var classify = function(v) {
            if (v === null || v === undefined)
                return 0;
            switch (typeof v) {
                case "boolean":
                    return v ? 2 : 1;
                case "number":
                    window._lispeJsEvalNumber = v;
                    return Number.isSafeInteger(v) ? 3 : 4;
                case "bigint":
                    window._lispeJsEvalText = v.toString();
                    return 5;
                case "string":
                    window._lispeJsEvalText = v;
                    return 6;
                case "function":
                case "symbol":
                    window._lispeJsEvalText = String(v);
                    return 6;
            }
            return -1;
        };
        window._lispeJsEvalText = null;
        var v = window._lispeJsEvalResult;
        var k = classify(v);
        if (k != -1)
            return k;
        var json;
        try {
            json = JSON.stringify(v);
        } catch(e) {
            // a circular structure or a BigInt inside an object
            window._lispeJsEvalText = e.toString();
            return -1;
        }
        if (json === undefined)
            return 0;
        // toJSON can turn an object into a scalar value (a Date becomes a string)
        if (json[0] != "{" && json[0] != "[")
            return classify(JSON.parse(json));
        window._lispeJsEvalText = json;
        return 7;
    });

    if (kind == 3 || kind == 4)
        number = EM_ASM_DOUBLE({ return window._lispeJsEvalNumber; });
    else {
        if (kind >= 5 || kind == -1) {
            char* str = (char*)EM_ASM_PTR({
                var s = window._lispeJsEvalText;
                window._lispeJsEvalText = null;
                var size = lengthBytesUTF8(s) + 1;
                var buffer = Module._malloc(size);
                stringToUTF8(s, buffer, size);
                return buffer;
            });
            text = str;
            free(str);
        }
    }

    if (kind == -1) {
        error = true;
        message = text;
        return 0;
    }
    return kind;
}

// evaljs_li, evaljs_ld, evaljs_lf: an array of numbers (or a typed array) comes back in values,
// copied as a block of doubles into the WASM memory, without any text representation.
// An element that is not a number (string, null, undefined, nested array) is an error, rather than
// a value that would silently disappear and shift the indexes.
// Returns false (and the error message in message) if the evaluation failed
bool eval_js_array(string code, std::vector<double>& values, string& message) {
    if (!run_js(code, message))
        return false;

    int sz = EM_ASM_INT({
        var v = window._lispeJsEvalResult;
        window._lispeJsEvalArray = null;
        window._lispeJsEvalText = null;
        if (v instanceof Float64Array) {
            window._lispeJsEvalArray = v;
            return v.length;
        }
        if (ArrayBuffer.isView(v) && !(v instanceof DataView)) {
            // Int32Array, Uint8Array... and BigInt64Array, whose values are converted with Number
            window._lispeJsEvalArray = Float64Array.from(v, Number);
            return v.length;
        }
        if (!Array.isArray(v)) {
            window._lispeJsEvalText = "Error: expecting an array of numbers, got: " + (v === null ? "null" : typeof v);
            return -1;
        }
        for (var i = 0; i < v.length; i++) {
            if (typeof v[i] != "number") {
                window._lispeJsEvalText = "Error: element " + i + " is not a number: " + (v[i] === null ? "null" : (Array.isArray(v[i]) ? "array" : typeof v[i]));
                return -1;
            }
        }
        window._lispeJsEvalArray = Float64Array.from(v);
        return v.length;
    });

    if (sz < 0) {
        char* str = (char*)EM_ASM_PTR({
            var s = window._lispeJsEvalText;
            window._lispeJsEvalText = null;
            var size = lengthBytesUTF8(s) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(s, buffer, size);
            return buffer;
        });
        message = str;
        free(str);
        return false;
    }

    values.resize(sz);
    if (sz) {
        EM_ASM({
            HEAPF64.set(window._lispeJsEvalArray, $0 >> 3);
            window._lispeJsEvalArray = null;
        }, values.data());
    }
    return true;
}

//------------------------------------------------------------------------------------------
extern "C" {
    EMSCRIPTEN_KEEPALIVE void store_in_lisp(LispE* lisp, List* recall, const char* result, bool error) {
        string r(result);
        if (error) {
            recall->release();
            display_error(r);
            return;
        }

        Element* e = lisp->provideString(r);            
        e->increment();

        recall->liste[1] = e;
#ifdef LISPE_WASM_NO_EXCEPTION
        e = recall->eval(lisp);
        if (e->isError()) {
            display_error(e->toString(lisp));
        }
        e->release();
#else
        try {
            e = recall->eval(lisp);
            if (e->isError()) {
                display_error(e->toString(lisp));
            }
            e->release();
        }
        catch(void* e) {
            if (((Element*)e)->type == t_error) {
                Error* err = (Error*)e;
                r = err->toString(lisp);
                display_error(r);
                err->release();
            }
            else
                display_error("Error occurred in the recall function");
        }        
#endif
        recall->release();
    }
}

void eval_js_async(LispE* lisp, string& code, List* recall) {
    char* resultStr = nullptr;
    bool error = false;

    // Execute JavaScript asynchronously
    EM_ASM({
    const jsCode = UTF8ToString($0);
   
    // Use an immediately invoked async function
    (async function() {
        try {
            // Evaluate the code - await will automatically handle promises
            const response = await eval(jsCode);            
            // Convert to string properly
            let resultString;
            if (typeof response === 'object') {
                resultString = JSON.stringify(response);
            } else {
                resultString = String(response);
            }
            
            // Store result in Lisp
            // UTF-8 needs up to 4 bytes per character: size the buffer in bytes, not in characters
            var size = lengthBytesUTF8(resultString) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(resultString, buffer, size);
            _store_in_lisp($1, $2, buffer, false);
            Module._free(buffer);
        } catch (error) {            
            const errorStr = error.toString();
            // UTF-8 needs up to 4 bytes per character: size the buffer in bytes, not in characters
            var size = lengthBytesUTF8(errorStr) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(errorStr, buffer, size);
            _store_in_lisp($1, $2, buffer, true);
            Module._free(buffer);
        }})();
    }, code.c_str(), lisp, recall);
}

//------------------------------------------------------------------------------------------
// evaljsc and asyncjsc: the JavaScript code is compiled once into a function, kept in
// window._lispeJsCache. The C++ maps below give the index of that function:
//  - js_cache_code: a string, compiled as an expression: new Function("return (" + code + "\n)")
//  - js_cache_call: a function name, called with an array of arguments: new Function("a", "return " + name + "(...a)")
// Code that cannot be compiled as an expression ("var x = 1; x + 2") is recorded with -1,
// it then goes through eval, as with evaljs.
// Note that the compiled functions only see the global scope (window).
#include <unordered_map>
static std::unordered_map<string, int> js_cache_code;
static std::unordered_map<string, int> js_cache_call;
// Strings built on the fly would make the cache grow without end: it is emptied beyond this size
static const size_t js_cache_max = 1024;

static int js_compile(string& code, bool call) {
    std::unordered_map<string, int>& cache = call ? js_cache_call : js_cache_code;
    auto it = cache.find(code);
    if (it != cache.end())
        return it->second;

    if (js_cache_code.size() + js_cache_call.size() >= js_cache_max) {
        js_cache_code.clear();
        js_cache_call.clear();
        EM_ASM({ window._lispeJsCache = []; });
    }

    int id = EM_ASM_INT({
        if (!window._lispeJsCache)
            window._lispeJsCache = [];
        if (!window._lispeJsArgs) {
            // Rebuilds the arguments of a call: kinds has one character per argument,
            // 'd' (100): the next double in the WASM memory, from ptr, otherwise the next value of json
            window._lispeJsArgs = function(kinds, ptr, json) {
                var n = kinds.length;
                var a = new Array(n);
                var values = json ? JSON.parse(json) : null;
                var p = ptr >> 3;
                var k = 0;
                for (var i = 0; i < n; i++)
                    a[i] = (kinds.charCodeAt(i) == 100) ? HEAPF64[p++] : values[k++];
                return a;
            };
        }
        var src = UTF8ToString($0).trim();
        var f;
        try {
            if ($1)
                f = new Function("a", "return " + src + "(...a)");
            else {
                while (src.endsWith(";"))
                    src = src.slice(0, -1).trim();
                f = new Function("return (" + src + "\n)");
            }
        } catch(e) {
            return -1;
        }
        window._lispeJsCache.push(f);
        return window._lispeJsCache.length - 1;
    }, code.c_str(), call);

    cache[code] = id;
    return id;
}

// When the function name could not be compiled as a function ("var x = 1; f"), the call goes through
// eval, which sees the array of arguments a: eval(code + "(...a)")
string eval_jsc(string& code, bool call, string& kinds, string& json, std::vector<double>& numbers, bool& error) {
    error = false;
    int id = js_compile(code, call);

    EM_ASM({
        window._lispeJsEvalResult = null;
        window._lispeJsEvalError = null;
        try {
            var a = $1 ? window._lispeJsArgs(UTF8ToString($2), $3, $4 ? UTF8ToString($4) : null) : null;
            if ($0 != -1)
                window._lispeJsEvalResult = $1 ? window._lispeJsCache[$0](a) : window._lispeJsCache[$0]();
            else
                window._lispeJsEvalResult = eval(UTF8ToString($5) + ($1 ? "(...a)" : ""));
        } catch(e) {
            window._lispeJsEvalError = e.toString();
        }
    }, id, call, kinds.c_str(), numbers.data(), json.empty() ? (const char*)0 : json.c_str(), code.c_str());

    char* errorStr = (char*)EM_ASM_PTR({
        if (window._lispeJsEvalError) {
            var errorStr = window._lispeJsEvalError;
            var size = lengthBytesUTF8(errorStr) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(errorStr, buffer, size);
            return buffer;
        }
        return 0;
    });

    if (errorStr) {
        string result(errorStr);
        free(errorStr);
        error = true;
        return result;
    }

    char* resultStr = (char*)EM_ASM_PTR({
        var result = window._lispeJsEvalResult;
        window._lispeJsEvalResult = null;
        var jsString;
        try {
            jsString = (result === null || result === undefined) ? "nil" : String(result);
        } catch(e) {
            jsString = "";
        }
        var size = lengthBytesUTF8(jsString) + 1;
        var buffer = Module._malloc(size);
        stringToUTF8(jsString, buffer, size);
        return buffer;
    });

    string result(resultStr);
    free(resultStr);
    return result;
}

void eval_jsc_async(LispE* lisp, string& code, bool call, string& kinds, string& json, std::vector<double>& numbers, List* recall) {
    int id = js_compile(code, call);

    EM_ASM({
    // The arguments and the function are read now: the C++ values no longer exist once we are back in LispE
    const f = ($0 == -1) ? null : window._lispeJsCache[$0];
    const jsCode = ($0 == -1) ? UTF8ToString($5) + ($1 ? "(...a)" : "") : null;
    let a = null;
    let argError = null;
    if ($1) {
        try {
            a = window._lispeJsArgs(UTF8ToString($2), $3, $4 ? UTF8ToString($4) : null);
        } catch(e) {
            argError = e;
        }
    }

    (async function() {
        try {
            if (argError)
                throw argError;
            const response = await (f === null ? eval(jsCode) : ($1 ? f(a) : f()));
            let resultString;
            if (typeof response === 'object') {
                resultString = JSON.stringify(response);
            } else {
                resultString = String(response);
            }
            var size = lengthBytesUTF8(resultString) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(resultString, buffer, size);
            _store_in_lisp($6, $7, buffer, false);
            Module._free(buffer);
        } catch (error) {
            const errorStr = error.toString();
            var size = lengthBytesUTF8(errorStr) + 1;
            var buffer = Module._malloc(size);
            stringToUTF8(errorStr, buffer, size);
            _store_in_lisp($6, $7, buffer, true);
            Module._free(buffer);
        }})();
    }, id, call, kinds.c_str(), numbers.data(), json.empty() ? (const char*)0 : json.c_str(), code.c_str(), lisp, recall);
}
