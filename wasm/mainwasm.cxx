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
void display_console(std::string str);
void display_console(u_ustring str);
void moduleconsole(LispE *lisp);
//------------------------------------------------------------------------------------------
extern "C" {
EMSCRIPTEN_KEEPALIVE bool clean_lispe(int32_t idx) {
    return clean_global_lispe(idx);
}

EMSCRIPTEN_KEEPALIVE bool reset_lispe(int32_t idx) {
    if (reset_global_lispe(idx)) {
        moduleconsole(global_lispe(idx));
        return true;
    }
    return false;
}

EMSCRIPTEN_KEEPALIVE int32_t create_lispe() {
    return create_global_lispe();
}

/*
 Indenting a LispE code.
 This function takes four parameters:

 1. str_as_int is the initial string as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. size is the number of values in the return value
 4. The return value is an array of UTF-16 characters
*/
EMSCRIPTEN_KEEPALIVE int32_t* eval_indent_lispe(int32_t* str_as_int, int32_t sz, int32_t* size) {
    int32_t* values;
    u_ustring code;    
    string cde;
    string codeindente;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int, sz);
    IndentCode(cde, codeindente, 3, true,false);
    //We then replace the initial array (str) with our new values
    //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
    wstring result;
    s_utf8_to_utf16(result, codeindente , codeindente.size());

    sz = result.size();
    values = new int32_t[sz];
    size[0] = sz;

    for (long i = 0; i < sz; i++)
        values[i] = result[i];
    return values;
}

EMSCRIPTEN_KEEPALIVE int32_t* eval_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* size) {
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        size[0] = sz * -1;
        int32_t* value_as_int = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            value_as_int[i] = code[i];
        return value_as_int;
    }
    
    string cde;
    //We first convert from UTF-16 into UTF-8
    //JavaScript strings are in UTF-16
    s_utf16_to_utf8(cde, str_as_int, sz);

    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code = lisp->execute(cde, ".");
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();        
        size[0] = sz * -1;
        int32_t* value_as_int = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            value_as_int[i] = result[i];
        return value_as_int;
    }

    code = executed_code->asUString(lisp);

    //We clean our result
    executed_code->release();
    
    //code is encoded in UTF-32
    //We need to convert it back to UTF-16
    //emojis for instance belong to the double UTF-16 family
    wstring result;
    s_unicode_to_utf16(result, code);
    sz = result.size();
    size[0] = sz;

    int32_t* value_as_int = new int32_t[sz];
    //We then replace the initial array (str) with our new values
    //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
    for (long i = 0; i < sz; i++) {
        value_as_int[i] = result[i];
    }

    return value_as_int;
}

/*
Evaluating a LispE code and returning an array of double.
 
 1. str_as_int contains the code as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. size is the number of values in the return value
 4. The return value is an array of double

 Note that we use lisp->eval(code) instead of execute. The _eval function cleans the compiled code from memory
 Note that when size[0] is negative, it means that the return value is an error message.
*/
EMSCRIPTEN_KEEPALIVE double* eval_to_floats_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* size) {
    double* values;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        u_ustring code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        values = new double[sz];
        for (long i = 0; i < sz; i++)
            values[i] = code[i];
        size[0] = sz*-1;
        return values;
    }

    //The main difference with the code above is that the instructions will be cleaned after execution
    u_ustring code;
    s_utf16_to_unicode(code, str_as_int , sz);
    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code= lisp->eval(code);
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();     
        values = new double[sz];
        //We then replace the initial array (str) with our new values
        //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
        for (long i = 0; i < sz; i++)
            values[i] = result[i];
        size[0] = sz*-1;
        return values;
    }
    
    switch (executed_code->type) {
        case t_integer:
        case t_short:
        case t_number:
        case t_float:
            values = new double[1];
            values[0] = executed_code->asNumber();
            size[0] = 1;
            break;
        case t_integers:
        case t_shorts:
        case t_numbers:
        case t_floats:
        case t_list:
            sz = executed_code->size();
            values = new double[sz];
            size[0] = sz;
            for (long i = 0; i < sz; i++)
                values[i] = executed_code->index(i)->asNumber();
            break;
        case t_llist: {
            sz = executed_code->size();
            values = new double[sz];
            size[0] = sz;
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL; a = a->next()) {
                values[sz] = a->value->asNumber();
            }
            break;
        }
        default:
            values = new double[sz];
            size[0] = 0;
    }
    
    //We clean our result
    return values;
}

/*
Evaluating a LispE code and returning an array of int32_t values.
 
 1. str_as_int contains the code as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. size is the number of values in the return value
 4. The return value is an array of int32_t values

 Note that we use lisp->eval(code) instead of execute. The _eval function cleans the compiled code from memory
 Note that when size[0] is negative, it means that the return value is an error message.
*/

EMSCRIPTEN_KEEPALIVE int32_t* eval_to_ints_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* size) {
    int32_t* values;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        u_ustring code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        values = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            values[i] = code[i];
        size[0] = sz*-1;
        return values;
    }
        
    //The main difference with the code above is that the instructions will be cleaned after execution
    u_ustring code;
    s_utf16_to_unicode(code, str_as_int , sz);
    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code= lisp->eval(code);
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();        
        //We then replace the initial array (str) with our new values
        //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
        values = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            values[i] = result[i];
        size[0] = sz*-1;
        return values;
    }
    
    switch (executed_code->type) {
        case t_integer:
        case t_short:
        case t_number:
        case t_float:
            values = new int32_t[1];
            values[0] = executed_code->asInteger();
            size[0] = 1;
            break;
        case t_integers:
        case t_shorts:
        case t_numbers:
        case t_floats:
        case t_list:
            sz = executed_code->size();
            values = new int32_t[sz];
            size[0] = sz;
            for (long i = 0; i < sz; i++)
                values[i] = executed_code->index(i)->asInteger();
            break;
        case t_llist: {
            long nbvalues = 0;
            sz = executed_code->size();
            values = new int32_t[sz];
            size[0] = sz;
            u_link* a = ((LList*)executed_code)->liste.begin();
            for (; a != NULL && nbvalues < sz; a = a->next()) {
                values[sz] = a->value->asInteger();
                nbvalues++;
            }
            break;
        }
        default:
            size[0] = 0;
    }
    
    //We clean our result
    return values;
}

/*
Evaluating a LispE code and returning an array of int32_t values. 
The array is actually composed of successive strings separated with a 0
 
 1. str_as_int contains the code as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. size is the number of values in the return value
 4. The return value is an array of int32_t values

 Note that we use lisp->eval(code) instead of execute. The _eval function cleans the compiled code from memory
 Note that when size[0] is negative, it means that the return value is an error message.
*/

EMSCRIPTEN_KEEPALIVE int32_t* eval_to_strings_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* size) {
    int32_t* values;
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        values = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            values[i] = code[i];
        size[0] = sz * -1;
        return values;
    }
        
    //The main difference with the code above is that the instructions will be cleaned after execution
    s_utf16_to_unicode(code, str_as_int , sz);
    //We call _eval in that case
    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code= lisp->eval(code);
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();        
        //We then replace the initial array (str) with our new values
        //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
        values = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            values[i] = result[i];
        size[0] = sz*-1;
        return values;
    }

    wstring result;

    vector<int32_t> characters;

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
                for (long i = 0; i < result.size(); i++) {
                    characters.push_back(result[i]);
                }
                characters.push_back(0);
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
                for (long i = 0; i < result.size(); i++) {
                    characters.push_back(result[i]);
                }                
                characters.push_back(0);
            }
            break;
        }
        default: {
            code = executed_code->asUString(lisp);
            s_unicode_to_utf16(result, code);
            //We then replace the initial array (str) with our new values
            //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
            for (long i = 0; i < result.size(); i++) {
                characters.push_back(result[i]);
            }
            characters.push_back(0);
        }
    }

    sz = characters.size();
    values = new int32_t[sz];
    for (long i = 0; i < sz; i++)
        values[i] = characters[i];
    size[0] = sz;
    //We clean our result
    return values;
}

/*
Evaluating a LispE code and returning a int32_t value
 
 1. str_as_int contains the code as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. mx is the maximum size of str_as_int, which can then be used to return an error message
 4. The return value is an int32_t value

 Note that the error message always starts with 27
*/

EMSCRIPTEN_KEEPALIVE int32_t eval_to_int_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t mx) {
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        u_ustring code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        str_as_int[0] = 27;
        for (long i = 1; i < sz; i++)
            str_as_int[i] = code[i];
        return sz;
    }
        
    //The main difference with the code above is that the instructions will be cleaned after execution
    u_ustring code;
    s_utf16_to_unicode(code, str_as_int , sz);
    //We call _eval in that case
    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code= lisp->eval(code);
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();
        sz = sz < mx ? sz : mx - 1;
        
        //We then replace the initial array (str) with our new values
        //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
        str_as_int[0] = 27;
        for (long i = 1; i < sz; i++)
            str_as_int[i] = result[i];
        return sz;
    }
    
    sz  = executed_code->asInteger();
    //We clean our result
    executed_code->release();
    return sz;
}

/*
Evaluating a LispE code and returning a double value
 
 1. str_as_int contains the code as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. mx is the maximum size of str_as_int, which can then be used to return an error message
 4. The return value is a double value

 Note that the error message always starts with 27
*/
EMSCRIPTEN_KEEPALIVE double eval_to_float_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t mx) {
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        u_ustring code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        str_as_int[0] = 27;
        for (long i = 1; i < sz; i++)
            str_as_int[i] = code[i];
        return sz;
    }
        
    //The main difference with the code above is that the instructions will be cleaned after execution
    u_ustring code;
    s_utf16_to_unicode(code, str_as_int , sz);
    //We call _eval in that case
    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code= lisp->eval(code);
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();
        sz = sz < mx ? sz : mx - 1;
        
        //We then replace the initial array (str) with our new values
        //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
        str_as_int[0] = 27;
        for (long i = 1; i < sz; i++)
            str_as_int[i] = result[i];
        return sz;
    }
    
    double v  = executed_code->asFloat();
    //We clean our result
    executed_code->release();
    return v;
}

/*
Evaluating a LispE code and returning a string
 
 1. str_as_int contains the code as an array of int32_t values. Note that each of these values is a UTF-16 character
 2. sz is the number of values in str_as_int.
 3. size contains the size of the return value
 4. The return value is an array of int32_t values

*/
EMSCRIPTEN_KEEPALIVE int32_t* eval_to_string_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* size) {
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        size[0] = sz * -1;
        int32_t* value_as_int = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            value_as_int[i] = code[i];
        return value_as_int;
    }
    
    s_utf16_to_unicode(code, str_as_int , sz);
    //We call _eval in that case
    Element* executed_code = lisp->n_null;
    //We call _eval in that case
    try {
        executed_code= lisp->eval(code);
    }
    catch(void* e) {
        wstring result = L"error";
        if (((Element*)e)->type == t_error) {
            code = ((Element*)e)->asUString(lisp);
            display_console(code);
            ((Element*)e)->release();
            //We clean our result
            //code is encoded in UTF-32
            //We need to convert it back to UTF-16
            //emojis for instance belong to the double UTF-16 family
            
            s_unicode_to_utf16(result, code);
        }
        sz = result.size();
        size[0] = sz * -1;
        int32_t* value_as_int = new int32_t[sz];
        for (long i = 0; i < sz; i++)
            value_as_int[i] = result[i];
        return value_as_int;
    }

    code = executed_code->asUString(lisp);

    //We clean our result
    executed_code->release();
    
    //code is encoded in UTF-32
    //We need to convert it back to UTF-16
    //emojis for instance belong to the double UTF-16 family
    wstring result;
    s_unicode_to_utf16(result, code);
    sz = result.size();
    size[0] = sz;

    int32_t* value_as_int = new int32_t[sz];
    //We then replace the initial array (str) with our new values
    //WASM cannot handle strings directly, we have to pass them as arrays of uint_32 elements
    for (long i = 0; i < sz; i++) {
        value_as_int[i] = result[i];
    }

    return value_as_int;
}

/*
Storing an array of ints in a LispE variable
 
 1. str_as_int contains variable name
 2. sz is the number of values in str_as_int.
 3. values is the array of ints
 4. mx the size of the values array

*/
EMSCRIPTEN_KEEPALIVE int32_t eval_setq_ints_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* values, int32_t mx) {
    wstring cde;
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lisp->provideAtom(code);
    
    //Now our value
    Integers* ints = lisp->provideIntegers();
    for (long i = 0; i < mx; i++) {
        ints->append(lisp, (long)values[i]);
    }
    
    lisp->storing_variable(ints, variable->label());
    return true;
}

/*
Storing an array of floats in a LispE variable
 
 1. str_as_int contains variable name
 2. sz is the number of values in str_as_int.
 3. values is the array of double
 4. mx the size of the values array

*/
EMSCRIPTEN_KEEPALIVE int32_t eval_setq_floats_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, double* values, int32_t mx) {
    wstring cde;
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lisp->provideAtom(code);
    
    //Now our value
    Floats* floats = lisp->provideFloats();
    for (sz = 0; sz < mx; sz++)
        floats->append(lisp, (double)values[sz]);
    
    lisp->storing_variable(floats, variable->label());
    return true;
}

/*
Storing an  int in a LispE variable
 
 1. str_as_int contains variable name
 2. sz is the number of values in str_as_int.
 3. value is the value
 
 In case of error, we return the size of the error message in str_as_int as a negative value
*/
EMSCRIPTEN_KEEPALIVE int32_t eval_setq_int_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t value) {
    wstring cde;
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lisp->provideAtom(code);
    
    //Now our value
    Integer* a_int = lisp->provideInteger(value);
    lisp->storing_variable(a_int, variable->label());
    return true;
}

/*
Storing a double in a LispE variable
 
 1. str_as_int contains variable name
 2. sz is the number of values in str_as_int.
 3. value is the value
 
 In case of error, we return the size of the error message in str_as_int as a negative value
*/
EMSCRIPTEN_KEEPALIVE int32_t eval_setq_float_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, double value) {
    wstring cde;
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lisp->provideAtom(code);
    
    //Now our value
    Float* a_float = lisp->provideFloat(value);
    lisp->storing_variable(a_float, variable->label());
    return true;
}

/*
Storing a string in a LispE variable
 
 1. str_as_int contains variable name
 2. sz is the number of values in str_as_int.
 3. value_as_int is the string value
 4. nb is the size of value_as_int
 
 In case of error, we return the size of the error message in str_as_int as a negative value
*/
EMSCRIPTEN_KEEPALIVE int32_t eval_setq_string_lispe(int32_t idx,  int32_t* str_as_int, int32_t sz, int32_t* value_as_int, int32_t nb) {
    wstring cde;
    u_ustring code;
    LispE* lisp = global_lispe(idx);
    if (lisp == NULL) {
        code = U"LispE was not initialized";
        display_console(code);
        sz = code.size();
        for (long i = 0; i < sz; i++)
            str_as_int[i] = code[i];
        return sz*-1;
    }
    
    for (long i = 0; i < sz; i++)
        cde += str_as_int[i];
    
    //This is our variable name
    s_utf16_to_unicode(code, cde);
    Element* variable = lisp->provideAtom(code);
    
    wstring c = L"";
    for (long i  = 0; i < nb; i++)
        c += value_as_int[i];
    
    code = U"";
    s_utf16_to_unicode(code, c);
    
    String* a_str = lisp->provideString(code);
    lisp->storing_variable(a_str, variable->label());
    return true;
}

} //extern "C"


//Main initialisation of LispE
int main() {
    long idx = create_global_lispe();
    moduleconsole(global_lispe(idx));
    return 0;
}

