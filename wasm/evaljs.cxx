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
            var buffer = Module._malloc(errorStr.length + 1);
            stringToUTF8(errorStr, buffer, errorStr.length + 1);
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
        var buffer = Module._malloc(jsString.length + 1);
        stringToUTF8(jsString, buffer, jsString.length + 1);
        return buffer;
    });

    string result(resultStr);
    free(resultStr);
    return result;
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

void eval_js_sync(LispE* lisp, string& code, List* recall) {
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
            var buffer = Module._malloc(resultString.length + 1);
            stringToUTF8(resultString, buffer, resultString.length + 1);
            _store_in_lisp($1, $2, buffer, false);
            Module._free(buffer);
        } catch (error) {            
            const errorStr = error.toString();
            var buffer = Module._malloc(errorStr.length + 1);
            stringToUTF8(errorStr, buffer, errorStr.length + 1);
            _store_in_lisp($1, $2, buffer, true);
            Module._free(buffer);
        }})();
    }, code.c_str(), lisp, recall);
}
