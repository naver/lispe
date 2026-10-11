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
