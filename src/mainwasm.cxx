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
static LispE* lispe = NULL;

extern "C" {
EMSCRIPTEN_KEEPALIVE void clean_lispe() {
    if (lispe != NULL)
        delete lispe;
    lispe = NULL;
}

EMSCRIPTEN_KEEPALIVE void reset_lispe() {
    if (lispe != NULL)
        delete lispe;
    lispe = new LispE();
}

EMSCRIPTEN_KEEPALIVE int32_t eval_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, int32_t mx) {
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[start + i] = code[i];
        return sz;
    }
    
    
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int + first , sz);
    Element* executed_code= lispe->execute(cde, ".");
    code = executed_code->asUString(lispe);
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
        str_as_int[start + i] = result[i];
    return sz;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_as_int_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t mx) {
    if (lispe == NULL) {
        u_ustring code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
        
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int + first , sz);
    Element* executed_code= lispe->execute(cde, ".");
    
    if (executed_code->type == t_error) {
        u_ustring code = executed_code->asUString(lispe);
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
            str_as_int[first + i] = result[i];
        return sz*-1;
    }
    
    sz  = executed_code->asInteger();
    //We clean our result
    executed_code->release();
    return sz;
}

EMSCRIPTEN_KEEPALIVE double eval_as_float_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t mx) {
    if (lispe == NULL) {
        u_ustring code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
        
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int + first , sz);
    Element* executed_code= lispe->execute(cde, ".");
    if (executed_code->type == t_error) {
        u_ustring code = executed_code->asUString(lispe);
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
            str_as_int[first + i] = result[i];
        return sz*-1;
    }
    
    double v  = executed_code->asFloat();
    //We clean our result
    executed_code->release();
    return v;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_to_floats_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, double* values, int32_t mx) {
    if (lispe == NULL) {
        u_ustring code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
    
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int + first , sz);
    Element* executed_code= lispe->execute(cde, ".");
    if (executed_code->type == t_error) {
        u_ustring code = executed_code->asUString(lispe);
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
            str_as_int[first + i] = result[i];
        return sz*-1;
    }
    
    switch (executed_code->type) {
        case t_integer:
        case t_short:
        case t_number:
        case t_float:
            values[start] = executed_code->asNumber();
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
                values[i + start] = executed_code->index(i)->asNumber();
            break;
        case t_llist: {
            sz = 0;
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL && sz < mx; a = a->next()) {
                values[start + sz] = a->value->asNumber();
                sz++;
            }
            break;
        }
        default:
            sz = 0;
    }
    
    //We clean our result
    return sz;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_to_ints_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, int32_t mx) {
    if (lispe == NULL) {
        u_ustring code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
        
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int + first , sz);
    Element* executed_code= lispe->execute(cde, ".");
    if (executed_code->type == t_error) {
        u_ustring code = executed_code->asUString(lispe);
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
            str_as_int[first + i] = result[i];
        return sz*-1;
    }
    
    switch (executed_code->type) {
        case t_integer:
        case t_short:
        case t_number:
        case t_float:
            str_as_int[start] = executed_code->asInteger();
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
                str_as_int[start + i] = executed_code->index(i)->asInteger();
            break;
        case t_llist: {
            sz = 0;
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL && sz < mx; a = a->next()) {
                str_as_int[start + sz] = a->value->asInteger();
                sz++;
            }
            break;
        }
        default:
            sz = 0;
    }
    
    //We clean our result
    return sz;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_to_strings_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, int32_t mx) {
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[i + start] = code[i];
        return sz;
    }
        
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int + first , sz);
    Element* executed_code= lispe->execute(cde, ".");
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
                code = executed_code->index(i)->asUString(lispe);
                s_unicode_to_utf16(result, code);
                //We then replace the initial array (str) with our new values
                //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
                for (long i = 0; i < result.size() && idx < mx; i++)  {
                    str_as_int[start + idx] = result[i];
                    idx++;
                }
                if (idx < mx) {
                    str_as_int[start + idx] = 0;
                    idx++;
                }
                else
                    break;
            }
            break;
        }
        case t_llist: {
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL; a = a->next()) {
                code = a->value->asUString(lispe);
                s_unicode_to_utf16(result, code);
                //We then replace the initial array (str) with our new values
                //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
                for (long i = 0; i < result.size() && idx < mx; i++)  {
                    str_as_int[start + idx] = result[i];
                    idx++;
                }
                
                if (idx < mx) {
                    str_as_int[start + idx] = 0;
                    idx++;
                }
                else
                    break;
            }
            break;
        }
        default: {
            code = executed_code->asUString(lispe);
            s_unicode_to_utf16(result, code);
            //We then replace the initial array (str) with our new values
            //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
            for (long i = 0; i < result.size() && idx < mx; i++)  {
                str_as_int[start + idx] = result[i];
                idx++;
            }
            if (idx < mx) {
                str_as_int[start + idx] = 0;
                idx++;
            }
        }
    }
    //We clean our result
    return idx;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_setq_ints_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, int32_t mx) {
    wstring cde;
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[first + i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lispe->provideAtom(code);
    
    //Now our value
    Integers* ints = lispe->provideIntegers();
    long v;
    for (long i = 0; i < mx; i++) {
        v = str_as_int[i + start];
        ints->append(lispe, v);
    }
    
    lispe->storing_variable(ints, variable->label());
    return true;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_setq_floats_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, double* values, int32_t mx) {
    wstring cde;
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[first + i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lispe->provideAtom(code);
    
    //Now our value
    Floats* floats = lispe->provideFloats();
    for (sz = 0; sz < mx; sz++)
        floats->append(lispe, (double)values[start + sz]);
    
    lispe->storing_variable(floats, variable->label());
    return true;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_setq_int_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t value) {
    wstring cde;
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[first + i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lispe->provideAtom(code);
    
    //Now our value
    Integer* a_int = lispe->provideInteger(value);
    lispe->storing_variable(a_int, variable->label());
    return true;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_setq_float_lispe(int32_t* str_as_int, int32_t first, int32_t sz, double value) {
    wstring cde;
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[first + i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lispe->provideAtom(code);
    
    //Now our value
    Float* a_float = lispe->provideFloat(value);
    lispe->storing_variable(a_float, variable->label());
    return true;
}

EMSCRIPTEN_KEEPALIVE int32_t eval_setq_string_lispe(int32_t* str_as_int, int32_t first, int32_t sz, int32_t start, int32_t nb) {
    wstring cde;
    u_ustring code;
    if (lispe == NULL) {
        code = U"LispE was not initialized";
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[first + i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[first + i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lispe->provideAtom(code);
    
    wstring c = L"";
    for (long i  = 0; i < nb; i++)
        c += str_as_int[start + i];
    
    code = U"";
    s_utf16_to_unicode(code, c);
    
    String* a_str = lispe->provideString(code);
    lispe->storing_variable(a_str, variable->label());
    return true;
}
}

//Main initialisation of LispE
int main() {
    lispe = new LispE();
    return 0;
}

