/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  mainasm.cxx

//-------------------------------------------------------------------------------------------
#include "lispe.h"
#include <emscripten/emscripten.h>
//-------------------------------------------------------------------------------------------
static LispE* lisp = NULL;

extern "C" EMSCRIPTEN_KEEPALIVE void clean_lispe() {
    if (lisp != NULL)
        delete lisp;
    lisp = NULL;
}

extern "C" EMSCRIPTEN_KEEPALIVE void reset_lispe() {
    if (lisp != NULL)
        delete lisp;
    lisp = new LispE();
}

extern "C" EMSCRIPTEN_KEEPALIVE int32_t eval_lispe(u_uchar* str, int32_t sz, int32_t mx) {
    u_ustring code;
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str[i] = code[i];
        return sz;
    }
        
    for (long i = 0; i < sz; i++)
        code += str[i];

    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, code);
    Element* executed_code= lisp->execute(cde, ".");
    code = executed_code->asUString(lisp);
    //We clean our result
    executed_code->release();

    //code is encoded in UTF-32
    //We need to convert it back to UTF-16
    //emojis for instance belong to the double UTF-16 family 
    wstring result;
    s_unicode_to_utf16(result, code);
    sz = result.size();
    sz = sz < mx ? sz : mx - 1;

    //We then replace the initial array (str) with our new values
    //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
    for (long i = 0; i < sz; i++)
        str[i] = result[i];
    return sz;
}

extern "C" EMSCRIPTEN_KEEPALIVE int32_t eval_as_int_lispe(u_uchar* str, int32_t sz, int32_t mx) {
    u_ustring code;
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str[i] = code[i];
        return sz;
    }
        
    for (long i = 0; i < sz; i++)
        code += str[i];

    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, code);
    Element* executed_code= lisp->execute(cde, ".");
    sz  = executed_code->asInteger();
    //We clean our result
    executed_code->release();
    return sz;
}

extern "C" EMSCRIPTEN_KEEPALIVE double eval_as_float_lispe(u_uchar* str, int32_t sz, int32_t mx) {
    u_ustring code;
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str[i] = code[i];
        return sz;
    }
        
    for (long i = 0; i < sz; i++)
        code += str[i];

    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, code);
    Element* executed_code= lisp->execute(cde, ".");
    double v  = executed_code->asFloat();
    //We clean our result
    executed_code->release();
    return v;
}

extern "C" EMSCRIPTEN_KEEPALIVE int32_t eval_to_floats_lispe(u_uchar* str, int32_t sz, double* values, int32_t mx) {
    u_ustring code;
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str[i] = code[i];
        return sz;
    }
        
    for (long i = 0; i < sz; i++)
        code += str[i];

    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, code);
    Element* executed_code= lisp->execute(cde, ".");

    switch (executed_code->type) {
        case t_integer:
        case t_short:
        case t_number:
        case t_float:
            values[0] = executed_code->asNumber();
            sz = 1;
            break;
        case t_integers:
        case t_shorts:
        case t_numbers:
        case t_floats:
        case t_list:
            sz = executed_code->size();
            sz = sz < mx ? sz : mx - 1;
            for (long i = 0; i < sz; i++)
                values[i] = executed_code->index(i)->asNumber();
            break;
        case t_llist: {
            sz = 0;
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL && sz < mx; a = a->next())
                values[sz++] = a->value->asNumber();
            break;
        }
        default:
            sz = 0;
    }

    //We clean our result
    return sz;
}

extern "C" EMSCRIPTEN_KEEPALIVE int32_t eval_to_ints_lispe(u_uchar* str, int32_t sz, int32_t* values, int32_t mx) {
    u_ustring code;
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str[i] = code[i];
        return sz;
    }
        
    for (long i = 0; i < sz; i++)
        code += str[i];

    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, code);
    Element* executed_code= lisp->execute(cde, ".");

    switch (executed_code->type) {
        case t_integer:
        case t_short:
        case t_number:
        case t_float:
            values[0] = executed_code->asInteger();
            sz = 1;
            break;
        case t_integers:
        case t_shorts:
        case t_numbers:
        case t_floats:
        case t_list:
            sz = executed_code->size();
            sz = sz < mx ? sz : mx - 1;
            for (long i = 0; i < sz; i++)
                values[i] = executed_code->index(i)->asInteger();
            break;
        case t_llist: {
            sz = 0;
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL && sz < mx; a = a->next())
                values[sz++] = a->value->asInteger();
            break;
        }
        default:
            sz = 0;
    }

    //We clean our result
    return sz;
}

extern "C" EMSCRIPTEN_KEEPALIVE int32_t eval_to_strings_lispe(u_uchar* str, int32_t sz, int32_t* values, int32_t mx) {
    u_ustring code;
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str[i] = code[i];
        return sz;
    }
        
    for (long i = 0; i < sz; i++)
        code += str[i];

    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, code);
    Element* executed_code= lisp->execute(cde, ".");
    wstring result;
    long idx = 0;

    switch (executed_code->type) {
        case t_integers:
        case t_shorts:
        case t_numbers:
        case t_floats:
        case t_strings:
        case t_list: {
            long nb = executed_code->size();
            for (long i = 0; i < nb; i++) {
                code = executed_code->index(i)->asUString(lisp);
                s_unicode_to_utf16(result, code);
                //We then replace the initial array (str) with our new values
                //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
                for (long i = 0; i < result.size() && idx < mx; i++)  {
                    values[idx++] = result[i];
                }
                if (idx < mx)
                    values[idx++] = 0;
                else
                    break;
            }
            break;
        }
        case t_llist: {
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL; a = a->next()) {
                code = a->value->asUString(lisp);
                s_unicode_to_utf16(result, code);
                //We then replace the initial array (str) with our new values
                //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
                for (long i = 0; i < result.size() && idx < mx; i++)  {
                    values[idx++] = result[i];
                }
                
                if (idx < mx)
                    values[idx++] = 0;
                else
                    break;
            }
            break;
        }
        default: {
            code = executed_code->asUString(lisp);
            s_unicode_to_utf16(result, code);
            //We then replace the initial array (str) with our new values
            //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
            for (long i = 0; i < result.size() && idx < mx; i++)  {
                values[idx++] = result[i];
            }
            if (idx < mx)
                values[idx++] = 0;
        }
    }
    //We clean our result
    return idx;
}

//Main initialisation of LispE
int main() {
    lisp = new LispE();
    return 0;
}

