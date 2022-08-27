/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe.cxx
//
//


#include <stdio.h>
#include "lispe.h"
#include "segmentation.h"
#include "tools.h"

#ifdef UNIX
#include <sys/resource.h>
#endif

//------------------------------------------------------------
static std::string version = "1.2022.8.27.11.51";
string LispVersion() {
    return version;
}

extern "C" {
    const char* lispversion() {
        return version.c_str();
    }
}

//------------------------------------------------------------------------------------------
#ifdef MACDEBUG
    vector<Element*> __indexes;
#endif
//------------------------------------------------------------
wstring Stackelement::asString(LispE* lisp) {
    std::wstringstream message;
    binHash<Element*>::iterator a;
    for (a = variables.begin(); a != variables.end(); a++)
        message << lisp->asString(a->first) << L": " << a->second->stringInList(lisp) << endl;
    return message.str();
}

List* Stackelement::atomes(LispE* lisp) {
    List* liste = lisp->provideList();
    binHash<Element*>::iterator a;
    for (a = variables.begin(); a != variables.end(); a++)
        liste->append(lisp->provideAtom(a->first));
    for (long i = 0; i < lisp->delegation->function_pool.size(); i++) {
        for (a = lisp->delegation->function_pool[i]->begin(); a != lisp->delegation->function_pool[i]->end(); a++)
            liste->append(lisp->provideAtom(a->first));
    }
    return liste;
}
//------------------------------------------------------------
jag_get* get_jag_handler();
void clean_get_handler(jag_get*);
void lispe_readfromkeyboard(string& code, void*);

void lispe_displaystring(string& code, void*) {
    cout << code;
}
//------------------------------------------------------------
//This function hasbeen implemented to correct the fact that Windows cannot handle U"..." correctly
//for large Unicode characters
static u_ustring U(string x) {
    u_ustring u;
    s_utf8_to_unicode(u, USTR(x), x.size());
    return u;
}
//------------------------------------------------------------

Delegation::Delegation() {
    mark = 0;
    input_handler = get_jag_handler();
    reading_string_function = &lispe_readfromkeyboard;
    display_string_function = &lispe_displaystring;
    reading_string_function_object = input_handler;    
    
    id_pool = 1;
    
    error_message = NULL;
    endtrace = false;
    trace_on = false;
    add_to_listing = false;
    stop_execution = 0;
    allfiles["main"] = 0;
    allfiles_names[0] = "main";
    debugfunction = NULL;
    debugobject = NULL;
    next_stop = false;
    i_current_line = -1;
    i_current_file = -1;
#ifdef UNIX
    const rlim_t kStackSize = 32 * 1024 * 1024;   // min stack size = 32 MB
    struct rlimit rl;
    int result;

    result = getrlimit(RLIMIT_STACK, &rl);
    if (result == 0)
    {
        if (rl.rlim_cur < kStackSize)
        {
            rl.rlim_cur = kStackSize;
            result = setrlimit(RLIMIT_STACK, &rl);
            if (result != 0)
            {
                fprintf(stderr, "setrlimit returned result = %d\n", result);
            }
        }
    }
#endif
}

Delegation::~Delegation() {
    clean_get_handler(input_handler);
    function_pool.cleaning();
    for (auto& a : thread_pool)
        a.second.clear();

    for (const auto& a: locks)
        delete a.second;

    binHash<Element*>::iterator a;
	for (a = atom_pool.begin(); a != atom_pool.end(); a++)
        delete a->second;

    for (const auto& a: waitons)
        delete a.second;

    delete _EMPTYLIST;
    delete _EMPTYDICTIONARY;
    delete _BREAK;
    delete _THEEND;
}

//------------------------------------------------------------
// This is a particular method that is implemented in the form of a deflib (see systems.cxx)
void moduleSysteme(LispE* lisp);
void moduleChaines(LispE* lisp);
void moduleMaths(LispE* lisp);
void moduleAleatoire(LispE* lisp);
void moduleRGX(LispE* lisp);
void moduleSocket(LispE* lisp);
void moduleOntology(LispE* lisp);
#ifdef FLTKGUI
void moduleGUI(LispE* lisp);
#endif
//------------------------------------------------------------
//We initialize our structures
void Delegation::initialisation(LispE* lisp) {

    //We initialize all instructions with eval_error
    //Only those for which an instruction exists will be replaced with
    //the proper call...
    //For the others we will throw an exception...
    for (long i = 0; i < l_final; i++) {
        evals[i] = &List::eval_error;
    }
    //These are not exactly instructions, but we need to set their arity nonetheless
    //since they belong to the List::eval method
    evals[t_atom] = &List::eval_call_function;
    evals[t_function] = &List::evalt_function;
    evals[t_library_function] = &List::evalt_library_function;
    evals[t_pattern] = &List::evalt_pattern;
    evals[t_lambda] = &List::evalt_lambda;
    evals[t_thread] = &List::evalt_thread;
    evals[t_data] = &List::evalt_data;
    evals[t_list] = &List::evalt_list;

    lisp->create_name_space(v_mainspace);

    // Here is the predefined list of instructions, with their name and arity
    //Important the instruction is counted into the arity. Hence (consp e) is P_TWO.

    //_max_stack_size can be enabled but represents a potential hazard as
    //too high a value can cause the whole interpreter to crash...
    //By default it is disabled...
#ifdef MAX_STACK_SIZE_ENABLED
    set_instruction(l_set_max_stack_size, "_max_stack_size", P_ONE | P_TWO, &List::evall_set_max_stack_size);
#endif

    set_instruction(l_void, "%__void__%", P_FULL, &List::evall_void);
    set_instruction(l_emptylist, "%__empty__%", P_ONE, &List::evall_emptylist);
    set_instruction(l_quoted, "%__quote__%", P_ONE, &List::evall_quoted);
    set_instruction(l_root, "__root__", P_ATLEASTONE, &List::evall_root);


    set_instruction(l_and, "and", P_ATLEASTTHREE, &List::evall_and);
    set_instruction(l_apply, "apply", P_THREE, &List::evall_apply);
    set_instruction(l_at, "at", P_ATLEASTTHREE, &List::evall_at);
    set_instruction(l_at_shape, "atshape", P_ATLEASTFOUR, &List::evall_at_shape);
    set_instruction(l_atom, "atom", P_TWO, &List::evall_converttoatom);
    set_instruction(l_atomise, "explode", P_TWO, &List::evall_atomise);
    set_instruction(l_atomp, "atomp", P_TWO, &List::evall_atomp);
    set_instruction(l_atoms, "atoms", P_ONE, &List::evall_atoms);
    set_instruction(l_bitand, "&", P_ATLEASTTWO, &List::evall_bitand);
    set_instruction(l_bitandequal, "&=", P_ATLEASTTWO, &List::evall_bitandequal);
    set_instruction(l_bitandnot, "&~", P_ATLEASTTWO, &List::evall_bitandnot);
    set_instruction(l_bitandnotequal, "&~=", P_ATLEASTTWO, &List::evall_bitandnotequal);
    set_instruction(l_bitnot, "~", P_TWO, &List::evall_bitnot);
    set_instruction(l_bitor, "|", P_ATLEASTTWO, &List::evall_bitor);
    set_instruction(l_bitorequal, "|=", P_ATLEASTTWO, &List::evall_bitorequal);
    set_instruction(l_bitxor, "^", P_ATLEASTTWO, &List::evall_bitxor);
    set_instruction(l_bitxorequal, "^=", P_ATLEASTTWO, &List::evall_bitxorequal);
    set_instruction(l_block, "block", P_ATLEASTONE, &List::evall_block);
    set_instruction(l_bodies, "bodies", P_TWO, &List::evall_bodies);
    set_instruction(l_break, "break", P_ONE, &List::evall_break);
    set_instruction(l_cadr, "cadr", P_TWO, &List::evall_cadr);
    set_instruction(l_car, "car", P_TWO, &List::evall_car);
    set_instruction(l_catch, "catch", P_ATLEASTTWO, &List::evall_catch);
    set_instruction(l_cdr, "cdr", P_TWO, &List::evall_cdr);
    set_instruction(l_check, "check", P_ATLEASTTWO, &List::evall_check);
    set_instruction(l_checking, "#checking", P_FOUR, &List::evall_checking);
    set_instruction(l_compose, "#compose", P_FULL, &List::evall_compose);
    set_instruction(l_cond, "cond", P_ATLEASTTWO, &List::evall_cond);
    set_instruction(l_cons, "cons", P_THREE, &List::evall_cons);
    set_instruction(l_consb, "consb", P_THREE, &List::evall_consb);
    set_instruction(l_consp, "consp", P_TWO, &List::evall_consp);
    set_instruction(l_conspoint, "conspoint", P_ATLEASTTWO, &List::evall_conspoint);
    set_instruction(l_count, "count", P_THREE|P_FOUR, &List::evall_count);
    set_instruction(l_cyclic, "cyclicp", P_TWO, &List::evall_cyclicp);
    set_instruction(l_short, "int16_t", P_TWO, &List::evall_converttoshort);
    set_instruction(l_integer, "integer", P_TWO, &List::evall_converttointeger);
    set_instruction(l_float, "float", P_TWO, &List::evall_converttofloat);
    set_instruction(l_number, "number", P_TWO, &List::evall_converttonumber);
    set_instruction(l_string, "string", P_TWO, &List::evall_converttostring);
    set_instruction(l_data, "data", P_ATLEASTTWO, &List::evall_data);
    set_instruction(l_deflib, "deflib", P_THREE, &List::evall_deflib);
    set_instruction(l_deflibpat, "deflibpat", P_THREE, &List::evall_deflibpat);
    set_instruction(l_defmacro, "defmacro", P_FOUR, &List::evall_defmacro);
    set_instruction(l_defpat, "defpat", P_ATLEASTFOUR, &List::evall_defpat);
    set_instruction(l_defspace, "defspace", P_TWO, &List::evall_defspace);
    set_instruction(l_defun, "defun", P_ATLEASTFOUR, &List::evall_defun);
    set_instruction(l_dethread, "dethread", P_ATLEASTFOUR, &List::evall_defun);
    set_instruction(l_dictionary, "dictionary", P_ONE | P_ATLEASTTHREE, &List::evall_dictionary);
    set_instruction(l_dictionaryi, "dictionaryi", P_ONE | P_ATLEASTTHREE, &List::evall_dictionaryi);
    set_instruction(l_dictionaryn, "dictionaryn", P_ONE | P_ATLEASTTHREE, &List::evall_dictionaryn);
    set_instruction(l_different, "!=", P_ATLEASTTHREE, &List::evall_different);
    set_instruction(l_divide, "/", P_ATLEASTTWO, &List::evall_divide);
    set_instruction(l_divideequal, "/=", P_ATLEASTTWO, &List::evall_divideequal);
    set_instruction(l_clone, "clone", P_TWO, &List::evall_clone);
    set_instruction(l_elapse, "elapse", P_ATLEASTONE, &List::evall_elapse);
    set_instruction(l_emptyp, "emptyp", P_TWO, &List::evall_emptyp);
    set_instruction(l_eq, "eq", P_ATLEASTTHREE, &List::evall_eq);
    set_instruction(l_equal, "=", P_THREE, &List::evall_equal);
    set_instruction(l_eval, "eval", P_TWO, &List::evall_eval);
    set_instruction(l_extend, "extend", P_THREE, &List::evall_extend);
    set_instruction(l_extract, "extract", P_THREE|P_FOUR|P_FIVE|P_SIX, &List::evall_extract);
    set_instruction(l_factorial, "!", P_TWO, &List::evall_factorial);
    set_instruction(l_fappend, "fappend", P_THREE, &List::evall_fappend);
    set_instruction(l_filterlist, "filterlist", P_THREE, &List::evall_filterlist);
    set_instruction(l_stringf, "stringf", P_THREE, &List::evall_stringf);
    set_instruction(l_takelist, "takelist", P_THREE, &List::evall_takelist);
    set_instruction(l_droplist, "droplist", P_THREE, &List::evall_droplist);
    set_instruction(l_flatten, "flatten", P_TWO, &List::evall_flatten);
    set_instruction(l_flip, "flip", P_TWO, &List::evall_flip);
    set_instruction(l_folding, "#folding", P_FOUR, &List::evall_folding);
    set_instruction(l_fread, "fread", P_TWO, &List::evall_fread);
    set_instruction(l_fwrite, "fwrite", P_THREE, &List::evall_fwrite);
    set_instruction(l_getchar, "getchar", P_ONE, &List::evall_getchar);
    set_instruction(l_greater, ">", P_ATLEASTTHREE, &List::evall_greater);
    set_instruction(l_greaterorequal, ">=", P_ATLEASTTHREE, &List::evall_greaterorequal);
    set_instruction(l_if, "if", P_THREE | P_FOUR, &List::evall_if);
    set_instruction(l_ife, "ife", P_ATLEASTFOUR, &List::evall_ife);
    set_instruction(l_in, "in", P_THREE, &List::evall_in);
    set_instruction(l_infix, "infix", P_TWO, &List::evall_infix);
    set_instruction(l_input, "input", P_ONE | P_TWO, &List::evall_input);
    set_instruction(l_insert, "insert", P_THREE | P_FOUR, &List::evall_insert);
    set_instruction(l_addr_, "addr_", P_TWO, &List::evall_addr_);
    set_instruction(l_shorts, "shorts", P_ATLEASTONE, &List::evall_shorts);
    set_instruction(l_integers, "integers", P_ATLEASTONE, &List::evall_integers);
    set_instruction(l_irange, "irange", P_THREE | P_FOUR, &List::evall_irange);
    set_instruction(l_irangein, "irangein", P_THREE | P_FOUR, &List::evall_irangein);
    set_instruction(l_join, "join", P_TWO | P_THREE, &List::evall_join);
    set_instruction(l_key, "key", P_ONE|P_ATLEASTTHREE, &List::evall_key);
    set_instruction(l_keyi, "keyi", P_ONE|P_ATLEASTTHREE, &List::evall_keyi);
    set_instruction(l_keyn, "keyn", P_ONE|P_ATLEASTTHREE, &List::evall_keyn);
    set_instruction(l_keys, "keys@", P_TWO, &List::evall_keys);
    set_instruction(l_label, "label", P_THREE, &List::evall_label);
    set_instruction(l_lambda, "λ", P_ATLEASTTHREE, &List::evall_lambda);
    set_instruction(l_last, "last", P_TWO, &List::evall_last);
    set_instruction(l_leftshift, "<<", P_ATLEASTTHREE, &List::evall_leftshift);
    set_instruction(l_leftshiftequal, "<<=", P_ATLEASTTWO, &List::evall_leftshiftequal);
    set_instruction(l_link, "link", P_THREE, &List::evall_link);
    set_instruction(l_list, "list", P_ATLEASTONE, &List::evall_list);
    set_instruction(l_llist, "llist", P_ATLEASTONE, &List::evall_llist);
    set_instruction(l_listand, "&&&", P_THREE, &List::evall_listand);
    set_instruction(l_listor, "|||", P_THREE, &List::evall_listor);
    set_instruction(l_listxor, "^^^", P_THREE, &List::evall_listxor);
    set_instruction(l_to_list, "to_list", P_TWO | P_THREE, &List::evall_to_list);
    set_instruction(l_to_llist, "to_llist", P_TWO, &List::evall_to_llist);
    set_instruction(l_load, "load", P_TWO | P_THREE, &List::evall_load);
    set_instruction(l_lock, "lock", P_ATLEASTTWO, &List::evall_lock);
    set_instruction(l_loop, "loop", P_ATLEASTFOUR, &List::evall_loop);
    set_instruction(l_multiloop, "mloop", P_ATLEASTFOUR, &List::multiloop);
    set_instruction(l_polyloop, "lloop", P_ATLEASTFOUR, &List::polyloop);
    set_instruction(l_loopcount, "loopcount", P_ATLEASTTHREE, &List::evall_loopcount);
    set_instruction(l_compare, ">=<", P_THREE, &List::evall_compare);
    set_instruction(l_lower, "<", P_ATLEASTTHREE, &List::evall_lower);
    set_instruction(l_lowerorequal, "<=", P_ATLEASTTHREE, &List::evall_lowerorequal);
    set_instruction(l_maplist, "maplist", P_THREE | P_FOUR, &List::evall_maplist);
    set_instruction(l_mapping, "#mapping", P_THREE, &List::evall_mapping);
    set_instruction(l_mark, "mark", P_THREE | P_TWO, &List::evall_mark);
    set_instruction(l_matrix, "matrix", P_TWO | P_THREE | P_FOUR, &List::evall_matrix);
    set_instruction(l_matrix_float, "matrix_float", P_TWO | P_THREE | P_FOUR, &List::evall_matrix_float);
    set_instruction(l_minmax, "minmax", P_ATLEASTTWO, &List::evall_minmax);
    set_instruction(l_max, "max", P_ATLEASTTWO, &List::evall_max);
    set_instruction(l_maybe, "maybe", P_ATLEASTTWO, &List::evall_maybe);
    set_instruction(l_min, "min", P_ATLEASTTWO, &List::evall_min);
    set_instruction(l_minus, "-", P_ATLEASTTWO, &List::evall_minus);
    set_instruction(l_minusequal, "-=", P_ATLEASTTWO, &List::evall_minusequal);
    set_instruction(l_mod, "%", P_ATLEASTTWO, &List::evall_mod);
    set_instruction(l_modequal, "%=", P_ATLEASTTWO, &List::evall_modequal);
    set_instruction(l_multiply, "*", P_ATLEASTTWO, &List::evall_multiply);
    set_instruction(l_multiplyequal, "*=", P_ATLEASTTWO, &List::evall_multiplyequal);
    set_instruction(l_ncheck, "ncheck", P_ATLEASTTHREE, &List::evall_ncheck);
    set_instruction(l_nconc, "nconc", P_ATLEASTONE, &List::evall_nconc);
    set_instruction(l_nconcn, "nconcn", P_ATLEASTONE, &List::evall_nconcn);
    set_instruction(l_neq, "neq", P_ATLEASTTHREE, &List::evall_neq);
    set_instruction(l_not, "not", P_TWO, &List::evall_not);
    set_instruction(l_nullp, "nullp", P_TWO, &List::evall_nullp);
    set_instruction(l_numberp, "numberp", P_TWO, &List::evall_numberp);
    set_instruction(l_floats, "floats", P_ATLEASTONE, &List::evall_floats);
    set_instruction(l_numbers, "numbers", P_ATLEASTONE, &List::evall_numbers);
    set_instruction(l_or, "or", P_ATLEASTTHREE, &List::evall_or);
    set_instruction(l_pipe, "pipe", P_ONE, &List::evall_pipe);
    set_instruction(l_plus, "+", P_ATLEASTTWO, &List::evall_plus);
    set_instruction(l_plusequal, "+=", P_ATLEASTTWO, &List::evall_plusequal);
    set_instruction(l_pop, "pop", P_TWO | P_THREE, &List::evall_pop);
    set_instruction(l_popfirst, "popfirst", P_TWO, &List::evall_popfirst);
    set_instruction(l_poplast, "poplast", P_TWO, &List::evall_poplast);
    set_instruction(l_power, "^^", P_ATLEASTTHREE, &List::evall_power);
    set_instruction(l_powerequal, "^^=", P_ATLEASTTHREE, &List::evall_powerequal);
    set_instruction(l_prettify, "prettify", P_TWO | P_THREE, &List::evall_prettify);
    set_instruction(l_print, "print", P_ATLEASTONE, &List::evall_print);
    set_instruction(l_printerr, "printerr", P_ATLEASTONE, &List::evall_printerr);
    set_instruction(l_printerrln, "printerrln", P_ATLEASTONE, &List::evall_printerrln);
    set_instruction(l_println, "println", P_ATLEASTONE, &List::evall_println);
    set_instruction(l_product, "product", P_TWO, &List::evall_product);
    set_instruction(l_push, "push", P_ATLEASTTHREE, &List::evall_push);
    set_instruction(l_pushfirst, "pushfirst", P_ATLEASTTHREE, &List::evall_pushfirst);
    set_instruction(l_pushlast, "pushlast", P_ATLEASTTHREE, &List::evall_pushlast);
    set_instruction(l_quote, "quote", P_TWO, &List::evall_quote);
    set_instruction(l_range, "range", P_FOUR, &List::evall_range);
    set_instruction(l_rangein, "rangein", P_FOUR, &List::evall_rangein);
    set_instruction(l_resetmark, "resetmark", P_TWO, &List::evall_resetmark);
    set_instruction(l_return, "return", P_ONE | P_TWO , &List::evall_return);
    set_instruction(l_replaceall, "replaceall", P_FOUR, &List::evall_replaceall);
    set_instruction(l_reverse, "reverse", P_TWO | P_THREE, &List::evall_reverse);
    set_instruction(l_revertsearch, "rfind", P_THREE | P_FOUR, &List::evall_revertsearch);
    set_instruction(l_rightshift, ">>", P_ATLEASTTHREE, &List::evall_rightshift);
    set_instruction(l_rightshiftequal, ">>=", P_ATLEASTTWO, &List::evall_rightshiftequal);
    set_instruction(l_rotate, "rotate", P_TWO | P_THREE, &List::evall_rotate);
    set_instruction(l_search, "find", P_THREE|P_FOUR, &List::evall_search);
    set_instruction(l_searchall, "findall", P_THREE|P_FOUR, &List::evall_searchall);
    set_instruction(l_select, "select", P_ATLEASTTWO, &List::evall_select);
    set_instruction(l_self, "self", P_ATLEASTONE, &List::eval_call_self);
    set_instruction(l_set_range, "setrange", P_FOUR|P_FIVE|P_SIX|P_SEVEN, &List::evall_set_range);
    set_instruction(l_set, "set", P_ATLEASTONE, &List::evall_set);
    set_instruction(l_sets, "sets", P_ATLEASTONE, &List::evall_sets);
    set_instruction(l_seti, "seti", P_ATLEASTONE, &List::evall_seti);
    set_instruction(l_setn, "setn", P_ATLEASTONE, &List::evall_setn);
    set_instruction(l_set_at, "set@", P_ATLEASTFOUR, &List::evall_set_at);
    set_instruction(l_set_shape, "setshape", P_ATLEASTFIVE, &List::evall_set_shape);
    set_instruction(l_setg, "setg", P_THREE, &List::evall_setg);
    set_instruction(l_setq, "setq", P_THREE, &List::evall_setq);
    set_instruction(l_sign, "sign", P_TWO, &List::evall_sign);
    set_instruction(l_signp, "signp", P_TWO, &List::evall_signp);
    set_instruction(l_size, "size", P_TWO, &List::evall_size);
    set_instruction(l_sleep, "sleep", P_TWO, &List::evall_sleep);
    set_instruction(l_sort, "sort", P_THREE, &List::evall_sort);
    set_instruction(l_space, "space", P_ATLEASTTHREE, &List::evall_space);
    set_instruction(l_stringp, "stringp", P_TWO, &List::evall_stringp);
    set_instruction(l_strings, "strings", P_ATLEASTONE, &List::evall_strings);
    set_instruction(l_switch, "switch", P_ATLEASTTHREE, &List::evall_switch);
    set_instruction(l_sum, "sum", P_TWO, &List::evall_sum);
    set_instruction(l_tensor, "tensor", P_ATLEASTTWO, &List::evall_tensor);
    set_instruction(l_tensor_float, "tensor_float", P_ATLEASTTWO, &List::evall_tensor_float);
    set_instruction(l_threadclear, "threadclear", P_ONE | P_TWO, &List::evall_threadclear);
    set_instruction(l_threadretrieve, "threadretrieve", P_ONE | P_TWO, &List::evall_threadretrieve);
    set_instruction(l_threadstore, "threadstore", P_THREE, &List::evall_threadstore);
    set_instruction(l_throw, "throw", P_TWO, &List::evall_throw);
    set_instruction(l_trace, "trace", P_ONE | P_TWO, &List::evall_trace);
    set_instruction(l_transpose, "transpose", P_TWO, &List::evall_transpose);
    set_instruction(l_heap, "heap", P_ATLEASTONE, &List::evall_heap);
    set_instruction(l_trigger, "trigger", P_TWO, &List::evall_trigger);
    set_instruction(l_type, "type", P_TWO, &List::evall_type);
    set_instruction(l_unique, "unique", P_TWO, &List::evall_unique);
    set_instruction(l_use, "use", P_TWO, &List::evall_use);
    set_instruction(l_values, "values@", P_TWO, &List::evall_values);
    set_instruction(l_wait, "wait", P_ONE, &List::evall_wait);
    set_instruction(l_waiton, "waiton", P_TWO, &List::evall_waiton);
    set_instruction(l_while, "while", P_ATLEASTTHREE, &List::evall_while);
    set_instruction(l_xor, "xor", P_ATLEASTTHREE, &List::evall_xor);
    set_instruction(l_zerop, "zerop", P_TWO, &List::evall_zerop);
    set_instruction(l_zip, "zip", P_ATLEASTTHREE, &List::evall_zip);
    set_instruction(l_zipwith, "zipwith", P_ATLEASTFOUR, &List::evall_zipwith);

#ifdef MACDEBUG
    set_instruction(l_debug_function, "f_debug", P_FULL, &List::List::evall_debug_function);
#endif
    
    // High level functions
    set_instruction(l_composenot, "?", P_FULL, &List::evall_compose);

    set_instruction(l_cycle, "cycle", P_TWO, &List::evall_compose);
    set_instruction(l_repeat, "repeat", P_TWO, &List::evall_compose);
    set_instruction(l_replicate, "replicate", P_THREE, &List::evall_compose);

    set_instruction(l_drop, "drop", P_THREE, &List::evall_compose);
    set_instruction(l_dropwhile, "dropwhile", P_THREE, &List::evall_compose);
    set_instruction(l_filter, "filter", P_THREE, &List::evall_compose);
    set_instruction(l_foldl, "foldl", P_FOUR, &List::evall_compose);
    set_instruction(l_foldl1, "foldl1", P_THREE, &List::evall_compose);
    set_instruction(l_foldr, "foldr", P_FOUR, &List::evall_compose);
    set_instruction(l_foldr1, "foldr1", P_THREE, &List::evall_compose);
    set_instruction(l_map, "map", P_THREE, &List::evall_compose);
    set_instruction(l_scanl, "scanl", P_FOUR, &List::evall_compose);
    set_instruction(l_scanl1, "scanl1", P_THREE, &List::evall_compose);
    set_instruction(l_scanr, "scanr", P_FOUR, &List::evall_compose);
    set_instruction(l_scanr1, "scanr1", P_THREE, &List::evall_compose);
    set_instruction(l_take, "take", P_THREE, &List::evall_compose);
    set_instruction(l_takewhile, "takewhile", P_THREE, &List::evall_compose);
    set_instruction(l_for, "for", P_FOUR|P_FIVE, &List::evall_compose);

    
    //APL-like
    set_instruction(l_innerproduct, ".", P_FIVE, &List::evall_innerproduct);
    set_instruction(l_outerproduct, "°", P_FOUR, &List::evall_outerproduct);
    set_instruction(l_iota, "iota", P_ATLEASTTWO, &List::evall_iota);
    set_instruction(l_invert, "invert", P_TWO | P_THREE, &List::evall_invert);
    set_instruction(l_solve, "solve", P_THREE, &List::evall_solve);
    set_instruction(l_determinant, "determinant", P_TWO, &List::evall_determinant);
    set_instruction(l_ludcmp, "ludcmp", P_TWO, &List::evall_ludcmp);
    set_instruction(l_lubksb, "lubksb", P_FOUR | P_THREE, &List::evall_lubksb);
    set_instruction(l_iota0, "iota0", P_ATLEASTTWO, &List::evall_iota0);
    set_instruction(l_irank, "irank", P_ATLEASTTHREE, &List::evall_irank);
    set_instruction(l_reduce, "reduce", P_TWO | P_THREE, &List::evall_reduce);
    set_instruction(l_scan, "scan", P_THREE, &List::evall_scan);
    set_instruction(l_backreduce, "backreduce", P_TWO | P_THREE, &List::evall_backreduce);
    set_instruction(l_backscan, "backscan", P_THREE, &List::evall_backscan);
    set_instruction(l_rank, "rank", P_ATLEASTTWO, &List::evall_rank);
    set_instruction(l_equalonezero, "==", P_THREE, &List::evall_equalonezero);
    set_instruction(l_rho, "rho", P_ATLEASTTWO, &List::evall_rho);
    set_instruction(l_member, "member", P_THREE, &List::evall_member);
    set_instruction(l_concatenate, ",", P_TWO|P_THREE, &List::evall_concatenate);
    

    //The void function
    lisp->void_function = new Listincode;
    lisp->void_function->liste.push_raw(provideAtomOrInstruction(l_void));
    lisp->garbaging(lisp->void_function);

    //Operators
    operators[l_bitnot] = true;
    operators[l_bitand] = true;
    operators[l_bitandnot] = true;
    operators[l_bitor] = true;
    operators[l_bitxor] = true;
    operators[l_plus] = true;
    operators[l_minus_plus] = true;
    operators[l_minus] = true;
    operators[l_multiply] = true;
    operators[l_divide] = true;
    operators[l_mod] = true;
    operators[l_power] = true;
    operators[l_leftshift] = true;
    operators[l_rightshift] = true;
    operators[l_bitandequal] = true;
    operators[l_bitorequal] = true;
    operators[l_bitxorequal] = true;
    operators[l_plusequal] = true;
    operators[l_minusequal] = true;
    operators[l_multiplyequal] = true;
    operators[l_divideequal] = true;
    operators[l_modequal] = true;
    operators[l_powerequal] = true;
    operators[l_leftshiftequal] = true;
    operators[l_rightshiftequal] = true;
    operators[l_listand] = true;
    operators[l_listor] = true;
    operators[l_listxor] = true;

    math_operators = operators;

    operators[l_equal] = true;
    operators[l_equalonezero] = true;
    operators[l_different] = true;
    operators[l_lower] = true;
    operators[l_compare] = true;
    operators[l_greater] = true;
    operators[l_lowerorequal] = true;
    operators[l_greaterorequal] = true;
    operators[l_concatenate] = true;
    operators[l_compare] = true;

    operators[l_or] = true;
    operators[l_and] = true;
    operators[l_xor] = true;

    operators[l_at] = true;

    comparators[l_equal] = true;
    comparators[l_equalonezero] = true;
    comparators[l_different] = true;
    comparators[l_lower] = true;
    comparators[l_compare] = true;
    comparators[l_greater] = true;
    comparators[l_lowerorequal] = true;
    comparators[l_greaterorequal] = true;
    comparators[l_concatenate] = true;
    comparators[l_compare] = true;

    assignors[l_equal] = true;
    assignors[l_bitandequal] = true;
    assignors[l_bitorequal] = true;
    assignors[l_bitxorequal] = true;
    assignors[l_plusequal] = true;
    assignors[l_minusequal] = true;
    assignors[l_multiplyequal] = true;
    assignors[l_divideequal] = true;
    assignors[l_modequal] = true;
    assignors[l_powerequal] = true;
    assignors[l_leftshiftequal] = true;
    assignors[l_rightshiftequal] = true;

    logicals[l_or] = true;
    logicals[l_and] = true;
    logicals[l_xor] = true;

    Element* e;

    //We record all our operators in advance
    binSetIter ito(operators);
    while (ito.next()) {
        e = new Operator(ito.first, code_to_string[ito.first]);
        e->status = s_constant;
        operator_pool[ito.first] = e;
    }

    code_to_string[t_string] = U"string_";
    code_to_string[t_number] = U"number_";
    code_to_string[t_float] = U"float_";
    code_to_string[t_floats] = U"floats_";
    code_to_string[t_numbers] = U"numbers_";
    code_to_string[t_integer] = U"integer_";
    code_to_string[t_short] = U"short_";
    code_to_string[t_strings] = U"strings_";
    code_to_string[t_integers] = U"integers_";
    code_to_string[t_shorts] = U"shorts_";
    code_to_string[t_list] = U"list_";
    code_to_string[t_llist] = U"llist_";
    code_to_string[t_matrix] = U"matrix_";
    code_to_string[t_matrix_float] = U"matrix_float";
    code_to_string[t_tensor] = U"tensor_";
    code_to_string[t_tensor_float] = U"tensor_float_";
    code_to_string[t_data] = U"data_";
    code_to_string[t_dictionary] = U"dictionary_";
    code_to_string[t_dictionaryn] = U"dictionary_n_";
    code_to_string[t_dictionaryi] = U"dictionary_i_";
    code_to_string[t_sets] = U"set_s_";
    code_to_string[t_setn] = U"set_n_";
    code_to_string[t_seti] = U"set_i_";
    code_to_string[t_set] = U"set_";
    code_to_string[t_atom] = U"atom_";
    code_to_string[t_heap] = U"heap_";

    code_to_string[t_maybe] = U"maybe_";
    code_to_string[t_error] = U"error_";
    
    code_to_string[t_function] = U"function_";
    code_to_string[t_library_function] = U"library_function_";
    code_to_string[t_pattern] = U"pattern_";
    code_to_string[t_lambda] = U"lambda_";
    code_to_string[t_thread] = U"thread_";

    code_to_string[v_null] = U"nil";
    code_to_string[v_true] = U"true";
    
    code_to_string[v_mainspace] = U"mainspace_";

    
    code_to_string[c_opening] = U"(";
    code_to_string[c_closing] = U")";
    code_to_string[c_opening_brace] = U"{";
    code_to_string[c_closing_brace] = U"}";
    code_to_string[c_colon] = U":";
    code_to_string[c_opening_bracket] = U"[";
    code_to_string[c_closing_bracket] = U"]";
    code_to_string[c_opening_data_brace] = U"@{";

    code_to_string[l_minus_plus] = U"-+";

    binHashe<u_ustring>::iterator it;
    for (it = code_to_string.begin(); it != code_to_string.end(); it++)
        string_to_code[it->second] = it->first;

    //We are preparing a recording place for labels and functions .
    code_to_string[v_emptyatom] = U"";


    _NULL = (Atome*)lisp->provideAtomOrInstruction(v_null);
    _ERROR = (Atome*)lisp->provideAtomOrInstruction(t_error);
    _TERMINAL = (Atome*)lisp->provideAtomOrInstruction(l_terminal);
    _COMPOSE = (Atome*)lisp->provideAtomOrInstruction(l_compose);
    _TRUE = (Atome*)lisp->provideAtomOrInstruction(v_true);
    _EMPTYATOM = (Atome*)lisp->provideAtomOrInstruction(v_emptyatom);
    _DEFPAT = (Atome*)lisp->provideAtomOrInstruction(l_defpat);

    _DICO_STRING = (Atome*)lisp->provideAtomOrInstruction(l_dictionary);
    _DICO_INTEGER = (Atome*)lisp->provideAtomOrInstruction(l_dictionaryi);
    _DICO_NUMBER = (Atome*)lisp->provideAtomOrInstruction(l_dictionaryn);

    _SET_STRINGS = (Atome*)lisp->provideAtomOrInstruction(l_sets);
    _SET_NUMBERS = (Atome*)lisp->provideAtomOrInstruction(l_setn);
    _SET_INTEGERS = (Atome*)lisp->provideAtomOrInstruction(l_seti);

    _QUOTE = (Atome*)lisp->provideAtomOrInstruction(l_quote);
    
    _SET_AT = (Atome*)lisp->provideAtomOrInstruction(l_set_at);

    _THEEND = new Error(L"Break Requested", s_constant);

    _EMPTYLIST = new List(s_constant);

    _EMPTYDICTIONARY = new Dictionary(s_constant);

    _BREAK = new Listbreak;

    
    //rest separator in a pattern matching operation
    u_ustring w = U"$";
    _LISTSEPARATOR = (Atome*)lisp->provideAtom(w);

    // Special case, they are now part of the default values
    w = U"";
    _EMPTYSTRING = lisp->provideConststring(w);
    _ZERO = lisp->provideConstinteger(0);
    _ONE = lisp->provideConstinteger(1);
    _TWO = lisp->provideConstinteger(2);
    _MINUSONE = lisp->provideConstinteger(-1);
    
    
    _COMPARE_BOOLEANS[0] = _ONE;
    _COMPARE_BOOLEANS[1] = _ZERO;
    _COMPARE_BOOLEANS[2] = _MINUSONE;
    
    
    _NUMERICAL_BOOLEANS[0] = _ZERO;
    _NUMERICAL_BOOLEANS[1] = _ONE;

    lisp->_BOOLEANS[0] = _NULL;
    lisp->_BOOLEANS[1] = _TRUE;

    //On crée un espace global dans la pile (la fonction associée est _NUL)
    lisp->push(_NULL);

    //We create our constant values
    lisp->recordingunique(_TRUE, v_true);
    lisp->recordingunique(_NULL, v_null);
    lisp->recordingunique(_ERROR, t_error);

    //These types are all basic data structures
    provideAtomOrInstruction(l_minus_plus);

    provideAtomType(t_string);
    provideAtomType(t_number);
    provideAtomType(t_float);
    provideAtomType(t_floats);
    provideAtomType(t_integer);
    provideAtomType(t_short);
    provideAtomType(t_shorts);
    provideAtomType(t_strings);
    provideAtomType(t_numbers);
    provideAtomType(t_integers);
    provideAtomType(t_list);
    provideAtomType(t_heap);
    provideAtomType(t_llist);
    provideAtomType(t_matrix);
    provideAtomType(t_matrix_float);
    provideAtomType(t_tensor);
    provideAtomType(t_tensor_float);
    provideAtomType(t_data);
    provideAtomType(t_maybe);
    provideAtomType(t_dictionary);
    provideAtomType(t_dictionaryn);
    provideAtomType(t_dictionaryi);
    provideAtomType(t_set);
    provideAtomType(t_seti);
    provideAtomType(t_sets);
    provideAtomType(t_setn);
    provideAtomType(t_atom);
    provideAtomType(t_function);
    provideAtomType(t_library_function);
    provideAtomType(t_pattern);
    provideAtomType(t_lambda);
    provideAtomType(t_thread);
    
    recordingData(lisp->create_instruction(t_string, _NULL), t_string, v_null);
    recordingData(lisp->create_instruction(t_float, _NULL), t_float, v_null);
    recordingData(lisp->create_instruction(t_floats, _NULL), t_floats, v_null);
    recordingData(lisp->create_instruction(t_number, _NULL), t_number, v_null);
    recordingData(lisp->create_instruction(t_short, _NULL), t_short, v_null);
    recordingData(lisp->create_instruction(t_integer, _NULL), t_integer, v_null);
    recordingData(lisp->create_instruction(t_numbers, _NULL), t_numbers, v_null);
    recordingData(lisp->create_instruction(t_strings, _NULL), t_strings, v_null);
    recordingData(lisp->create_instruction(t_integers, _NULL), t_integers, v_null);
    recordingData(lisp->create_instruction(t_shorts, _NULL), t_shorts, v_null);
    recordingData(lisp->create_instruction(t_list, _NULL), t_list, v_null);
    recordingData(lisp->create_instruction(t_llist, _NULL), t_llist, v_null);
    recordingData(lisp->create_instruction(t_matrix, _NULL), t_matrix, v_null);
    recordingData(lisp->create_instruction(t_matrix_float, _NULL), t_matrix_float, v_null);
    recordingData(lisp->create_instruction(t_tensor, _NULL), t_tensor, v_null);
    recordingData(lisp->create_instruction(t_tensor_float, _NULL), t_tensor_float, v_null);
    recordingData(lisp->create_instruction(t_data, _NULL), t_data, v_null);
    recordingData(lisp->create_instruction(t_maybe, _NULL), t_maybe, v_null);
    recordingData(lisp->create_instruction(t_dictionary, _NULL), t_dictionary, v_null);
    recordingData(lisp->create_instruction(t_dictionaryn, _NULL), t_dictionaryn, v_null);
    recordingData(lisp->create_instruction(t_dictionaryi, _NULL), t_dictionaryi, v_null);
    recordingData(lisp->create_instruction(t_set, _NULL), t_set, v_null);
    recordingData(lisp->create_instruction(t_seti, _NULL), t_seti, v_null);
    recordingData(lisp->create_instruction(t_sets, _NULL), t_sets, v_null);
    recordingData(lisp->create_instruction(t_setn, _NULL), t_setn, v_null);
    recordingData(lisp->create_instruction(t_heap, _NULL), t_heap, v_null);
    recordingData(lisp->create_instruction(t_atom, _NULL), t_atom, v_null);

    //We introduce _ as a substitute to nil
    w = U"_";
    string_to_code[w] = v_null;

    //We introduce @ as a substitute to at
    w = U"@";
    string_to_code[w] = l_at;
    
    //We introduce @@ as a substitute to extract
    w = U"@@";
    string_to_code[w] = l_extract;

    //We introduce @@@ as a substitute to atshape
    w = U"@@@";
    string_to_code[w] = l_at_shape;

    //We introduce set@@ as a substitute to setrange
    w = U"set@@";
    string_to_code[w] = l_set_range;

    //We introduce set@@@ as a substitute to setshape
    w = U"set@@@";
    string_to_code[w] = l_set_shape;

    //We introduce containerkeys as a substitute to keys@
    w = U"containerkeys";
    string_to_code[w] = l_keys;

    //We introduce containervalues as a substitute to values@
    w = U"containervalues";
    string_to_code[w] = l_values;

    w = U("§");
    string_to_code[w] = l_infix;
    
    w = U("•");
    string_to_code[w] = l_infix;
    
    w = U"/\\";
    string_to_code[w] = l_infix;

    //But also 'false', which is a substitute to nil as well
    w = U"false";
    string_to_code[w] = v_null;

    // We introduce \ and λ (Unicode: 955) as a substitute to lambda...
    w = U"\\";
    string_to_code[w] = l_lambda;
	//( (λ(x) (+ x 10)) 20)
    w = U("lambda");
    string_to_code[w] = l_lambda;
    
    w = U"//";
    string_to_code[w] = l_reduce;
    
    w = U"\\\\";
    string_to_code[w] = l_scan;

    w = U"-//";
    string_to_code[w] = l_backreduce;

    w = U("⌿");
    string_to_code[w] = l_backreduce;

    w = U"-\\\\";
    string_to_code[w] = l_backscan;

    w = U("⍀");
    string_to_code[w] = l_backscan;

    w = U("⍳");
    string_to_code[w] = l_iota;

    w = U("⍳0");
    string_to_code[w] = l_iota0;
    
    w = U("⍴");
    string_to_code[w] = l_rho;
    
    w = U("⍉");
    string_to_code[w] = l_transpose;


    w = U("⌽");
    string_to_code[w] = l_reverse;

    w = U("¬");
    string_to_code[w] = l_not;

    w = U("∑");
    string_to_code[w] = l_sum;

    w = U("∏");
    string_to_code[w] = l_product;

    w = U("⌹");
    string_to_code[w] = l_invert;

    w = U("⍤");
    string_to_code[w] = l_rank;
    
    w = U("∈");
    string_to_code[w] = l_member;
    
    //Small tip, to avoid problems
    // indeed, the instruction cadr is already linked to its own code
    e = new Cadr("cadr");
    e->status = s_constant;
    atom_pool[l_cadr] = e;

    // We create all our atoms and keep them in the stack
    binHashe<string>::iterator its;
    for (its = instructions.begin(); its != instructions.end(); its++) {
        e = lisp->provideAtomOrInstruction(its->first);
        lisp->recordingunique(e, its->first);
    }

    lisp->provideAtom(c_opening);
    lisp->provideAtom(c_closing);
    lisp->provideAtom(c_opening_brace);
    lisp->provideAtom(c_opening_data_brace);
    lisp->provideAtom(c_closing_brace);
    lisp->provideAtom(c_colon);
    lisp->provideAtom(c_opening_bracket);
    lisp->provideAtom(c_closing_bracket);

    
    //We add an extension to the language... see systeme.cxx
    moduleSysteme(lisp);
    moduleChaines(lisp);
    moduleMaths(lisp);
    moduleAleatoire(lisp);
    moduleRGX(lisp);
    moduleSocket(lisp);
    moduleOntology(lisp);
#ifdef FLTKGUI
    moduleGUI(lisp);
#endif
    
    atom_basic_pool.set(atom_pool.base, atom_pool.tsize, atom_pool.indexes);
    number_types.push(t_shorts);
    number_types.push(t_floats);
    number_types.push(t_integers);
    number_types.push(t_numbers);
    number_types.push(t_matrix);
    number_types.push(t_matrix_float);
    number_types.push(t_tensor);
    number_types.push(t_tensor_float);
    number_types.push(t_seti);
    number_types.push(t_setn);
    
}

void LispE::cleaning() {
    if (!isThread) {
        //we force all remaining threads to stop
        stop();
    }

    //Then if some of them are still running
    //we wait for their termination
    while (nbjoined) {}

    clearStack();
    for (long i = 0; i < garbages.size(); i++)
        delete garbages[i];
    garbages.clear();

    return_pool.cleaning();
    string_pool.cleaning();
    float_pool.cleaning();
    number_pool.cleaning();
    integer_pool.cleaning();
    numbers_pool.cleaning();
    floats_pool.cleaning();
    integers_pool.cleaning();
    strings_pool.cleaning();

    list_pool.cleaning();
    
    quoted_pool.cleaning();

    dictionary_pool.cleaning();
    dictionaryi_pool.cleaning();
    dictionaryn_pool.cleaning();

    sets_pool.cleaning();
    setn_pool.cleaning();
    seti_pool.cleaning();
    set_pool.cleaning();

    for (const auto& a : const_string_pool) {
        delete a.second;
    }

    for (const auto& a : const_integer_pool) {
        delete a.second;
    }

    for (const auto& a : const_number_pool) {
        delete a.second;
    }

    for (const auto& a: pools)
        delete a.second;

    for (const auto& a: vpools) {
        if (a != delegation->_NULL && a->status < s_constant)
            delete a;
    }

    if (clean_utf8)
        delete handlingutf8;

    if (!isThread) {
        delete delegation;
#ifdef MACDEBUG
        vector<Element*> errors;
        for (const auto& a: __indexes) {
            if (a != NULL) {
                errors.push_back(a);
            }
        }
        __indexes.clear();
        if (errors.size())
            cerr << "%Errors: " << errors.size() << endl;
#endif
    }
}

LispE::LispE(LispE* lisp, List* function, List* body) {
    current_space = 0;
    void_function = lisp->void_function;
    preparingthread = false;
    check_arity_on_fly = false;
    delegation = lisp->delegation;

    _BOOLEANS[0] = delegation->_NULL;
    _BOOLEANS[1] = delegation->_TRUE;
    id_thread = delegation->id_pool++;

    //For threads, the stack limit is much smaller
    max_stack_size = 1000;


    thread_ancestor = lisp;

    clean_utf8 = false;
    handlingutf8 = lisp->handlingutf8;

    isThread = true;
    trace = lisp->trace;

    lisp->hasThread = true;
    lisp->nbjoined++;

    nbjoined = 0;
    current_thread = function;
    current_body = body;

    //We prepare our stack, with the creation of a local main
    push(delegation->_NULL);
    //we only copy constant elements...
    stack_pool[0]->copy(lisp->stack_pool[0]);

    n_null = delegation->_NULL;
    n_true = delegation->_TRUE;
    n_zero = delegation->_ZERO;
    n_one = delegation->_ONE;
}

/*
 The segmenter is the first step in the compilation of a code.
 We are going to detect the most important characters of the LISP language:
 a) parentheses
 b) atoms
 (c) the quotes
 d) strings (a little extra in our implementation)
 (e) the figures
 f) operators (mathematical and comparator)

 We will fill in a structure that will contain these elements.
 This structure is composed of a list of segments, their type and their position in the
 code (line)
 */

lisp_code LispE::segmenting(string& code, Tokenizer& infos) {
    static uchar stops[172];
    static bool init = false;
    long idx;
    if (!init) {
        init = true;
        memset(stops, 0, 172);
        for (idx = 0; idx <= 32; idx++) {
            stops[idx] = true;
        }

        stops['?'] = true;
        stops['('] = true;
        stops[')'] = true;
        stops[':'] = true;
        stops['"'] = true;
        stops['['] = true;
        stops[']'] = true;
        stops['{'] = true;
        stops['}'] = true;

        stops[39] = true;
        stops[171] = true;
    }

    int16_t string_end;
    long sz = code.size();
    long i, current_i;
    int nb_parentheses = 0;
    int nb_braces = 0;
    int nb_brackets = 0;
    //The first line of the code is 1
    long line_number = 1;
    long culprit = -1;
    u_uchar c, nxt;
    char add = delegation->add_to_listing;
    lisp_code lc;
    string current_line;
    string tampon;
    for (i = 0; i < sz; i++) {
        string_end = 187;
        current_i = i;
        c = getonechar(USTR(code), i);
        switch (c) {
            case 27: {//if it is an escape character
                //We might have a color definition
                idx = i + 1;
                if (code[idx] == '[') {
                    //we are looking for the final 'm'
                    while (idx < sz && code[idx] != 'm')
                        idx++;

                    if (idx != sz)
                        i = idx;
                }
                break;
            }
            case ';':
			case '#':
                idx = i;
                if (code[i+1] == ';') {
                    //we need to find the next line where it appears
                    i+=2;
                    while (i < sz-1 && (code[i] != ';' || code[i+1] != ';')) {
                        if (code[i] == '\n') {
                            if (add == true) {
                                current_line = code.substr(idx, i-idx+1);
                                add_to_listing(line_number, current_line);
                                current_line = "";
                            }
                            else {
                                if (add == 2) {
                                    tampon = code.substr(idx, i-idx+1);
                                    infos.append(tampon, t_comment, line_number, idx, i);
                                }
                            }
                            line_number++;
                            idx = i + 1;
                        }
                        i++;
                    }
                }
                else {
                    while (i < sz && code[i] != '\n') i++;
                    line_number++;
                    if (add == true) {
                        current_line = code.substr(idx, i-idx+1);
                        add_to_listing(line_number, current_line);
                        current_line = "";
                    }
                    else {
                        if (add == 2) {
                            tampon = code.substr(idx, i-idx+1);
                            infos.append(tampon, t_comment, line_number, idx, i);
                        }
                    }
                }
                break;
            case '\n':
                if (add == true) {
                    current_line += c;
                    add_to_listing(line_number, current_line);
                    current_line = "";
                }
                line_number++;
                break;
            case '\t':
            case '\r':
            case ' ':
                if (add == true)
                    current_line += c;
                continue;
            case '"': {
                idx = i + 1;
                tampon = "";
                while (idx < sz && code[idx] != '"') {
                    c = (uchar)code[idx];
                    if (c < 32) {
                        infos.append(tampon, t_emptystring, line_number, i, idx);
                        return e_error_string;
                    }

                    if (c == '\\') {
                        idx++;
                        switch (code[idx]) {
                            case 'n':
                                tampon += '\n';
                                idx++;
                                continue;
                            case 'r':
                                tampon += '\r';
                                idx++;
                                continue;
                            case 't':
                                tampon += '\t';
                                idx++;
                                continue;

                        }
                    }
                    tampon += code[idx];
                    idx++;
                }

                if (tampon == "")
                    infos.append(tampon, t_emptystring, line_number, i, idx);
                else
                    infos.append(tampon, t_string, line_number, i, idx);

                if (add == true) {
                    current_line += "\"";
                    current_line += tampon;
                    current_line += "\"";
                }
                i = idx;
                break;
            }
            case '.':
                if (code[i + 1] == ' ') {
                    tampon = ".";
                    infos.append(tampon, c_point, line_number, i, i + 1);
                    break;
                }
            case '+':
            case '-':
                if (!isdigit(code[i+1])) {
                    idx = i + 1;
                    nxt = 0;
                    while (idx < sz) {
                        nxt = getonechar(USTR(code), idx);
                        if (nxt == 8220 || (nxt < 172 && stops[nxt]))
                            break;
                        i = idx;
                        idx++;
                    }
                    tampon = code.substr(current_i, i - current_i + 1);
                    if (add == true)
                        current_line += tampon;

                    idx = delegation->is_atom(tampon);
                    if (idx >= l_minus_plus && idx <= l_modequal)
                        infos.append(tampon, t_operator, line_number, current_i, i);
                    else
                        infos.append(tampon, t_atom, line_number, current_i, i);
                    break;
                }
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
                double d = convertingfloathexa(code.c_str() + i, idx);
                tampon = code.substr(i, idx);
                if (add == true)
                    current_line += tampon;
                infos.append(d, tampon, t_number, line_number, i, i + idx);
                i += idx - 1;
                break;
            }
            case '\'':
                if (add == true)
                    current_line += c;
                infos.append(c, l_quote, line_number, i, i+1);
                break;
            case '`':
                string_end = '`';
            case 171: { // a string containing what we want... either «» or ``
                idx = i + 1;
                tampon = "";
                long ln = 0;
                while (idx < sz && code[idx] != string_end) {
                    if (code[idx] == '\n')
                        ln++;
                    idx++;
                }
                if (idx == sz && add)
                    return e_error_string;

                tampon = code.substr(i+1, idx-i-1);
                if (tampon == "")
                    infos.append(tampon, t_emptystring, line_number, i, idx);
                else
                    infos.append(tampon, t_string, line_number, i, idx);
                if (add == true) {
                    if (string_end == 187) {
                        current_line += "«";
                        current_line += tampon;
                        current_line += "»";
                    }
                    else {
                        current_line += "`";
                        current_line += tampon;
                        current_line += "`";
                    }
                }
                i = idx;
                line_number += ln;
                break;
            }
            default: {
				if (c < 32)
					continue;

                //If the character is a multi-byte character, we need
                //to position on the beginning of the character again
                //Cases to stop looking for an atom: ()]{}'9 32 10 13" 171 8220
                idx = i + 1;
                nxt = c;
                while (idx <= sz && nxt != 8220 && (nxt > 171 || !stops[nxt])) {
                    i = idx;
                    nxt = getonechar(USTR(code), idx);
                    idx++;
                }

                if ((i - current_i) <= 1) {
                    tampon = c;
                    if (add == true)
                        current_line += c;
                    
                    //if it is a dictionary data structure, it starts with @{..}
                    if (c == '@' && nxt == '{') {
                        tampon += nxt;
                        i++;
                        if (add == true)
                            current_line += '{';
                    }
                }
                else {
                    tampon = code.substr(current_i, i - current_i);
                    if (add == true)
                        current_line += tampon;

                    if (tampon[0] == 'c' && tampon.back() == 'r' && tampon.size() > 3) {
                        // we check if we don't have a variation on car/cdr/cadr/caar etc...
                        idx = 1;
                        bool ok = true;
                        while (tampon[idx] !='r') {
                            if (tampon[idx] != 'a' && tampon[idx] != 'd') {
                                ok = false;
                                break;
                            }
                            idx++;
                        }
                        if (ok) {
                            infos.append(tampon, l_cadr, line_number, current_i, i);
                            i--;
                            break;
                        }
                    }
                }

                lc = (lisp_code)delegation->check_atom(tampon);

                switch (lc) {
                    case l_composenot:
                        infos.append(c, t_atom, line_number, current_i, i);
                        break;
                    case c_opening:
                        nb_parentheses++;
                        infos.append(c, c_opening, line_number, current_i, i);
                        break;
                    case c_closing:
                        nb_parentheses--;
                        if (nb_parentheses <= 0) {
                            if (culprit == -1)
                                culprit = line_number;
                        }
                        infos.append(c, c_closing, line_number, current_i, i);
                        break;
                    case c_opening_brace:
                        nb_braces++;
                        infos.append(c, c_opening_brace, line_number, current_i, i);
                        break;
                    case c_opening_data_brace:
                        nb_braces++;
                        infos.append(c, c_opening_data_brace, line_number, current_i, i);
                        break;
                    case c_closing_brace:
                        nb_braces--;
                        if (nb_braces < 0) {
                            if (culprit == -1)
                                culprit = line_number;
                        }
                        infos.append(c, c_closing_brace, line_number, current_i, i);
                        break;
                    case c_colon:
                        infos.append(c, c_colon, line_number, current_i, i);
                        break;
                    case c_opening_bracket:
                        nb_brackets++;
                        infos.append('(', c_opening, line_number, current_i, i);
                        break;
                    case c_closing_bracket: {
                        if (nb_brackets) {
                            nb_brackets--;
                            infos.append(')', c_closing, line_number, current_i, i);
                        }
                        else {
                            nb_parentheses--;
                            if (nb_parentheses <= 0) {
                                if (culprit == -1)
                                    culprit = line_number;
                            }
                            //We add any number of parentheses to close the gap
                            for (long i = 0; i < nb_parentheses; i++)
                            infos.append(c, c_closing, line_number, current_i, i);
                            nb_parentheses = 1;
                        }
                        break;
                    }
                    default:
                        if (lc >= l_plus && lc <= l_concatenate)
                            infos.append(tampon, t_operator, line_number, current_i, i);
                        else
                            infos.append(tampon, t_atom, line_number, current_i, i);
                }

                if (i != current_i)
                    i--;
                break;
            }
        }
    }

    if (nb_brackets) {
        delegation->i_current_line = line_number;
        return e_error_bracket;
    }

    if (nb_parentheses) {
        delegation->i_current_line = culprit;
        return e_error_parenthesis;
    }

    if (nb_braces) {
        delegation->i_current_line = culprit;
        return e_error_brace;
    }

    if (add && current_line != "")
        add_to_listing(line_number, current_line);

    return e_no_error;
}


Element* LispE::tokenize(wstring& code, bool keepblanks) {
    List* res = provideList();
    long idx;
    long sz = code.size();
    long i;
    wchar_t c;
    wstring tampon;
    for (i = 0; i < sz; i++) {
        c = code[i];
        switch (c) {
            case '\n':
            case '\t':
            case '\r':
            case ' ':
                if (keepblanks) {
                    tampon = c;
                    res->append(provideString(tampon));
                }
                break;
            case ';':
            case ',':
            case '#':
            case '\'':
            case '`':
            case '"':
            case '(':
            case ')':
            case '[':
            case ']':
            case ':':
            case '{':
            case '}':
            case '&':
            case '|':
            case '*':
            case '%':
            case '/':
            case '^':
            case '<':
            case '>':
            case '!':
            case '=':
                tampon = c;
                res->append(provideString(tampon));
                break;
            case '.':
                if (code[i + 1] == ' ') {
                    tampon = c;
                    res->append(provideString(tampon));
                    break;
                }
            case '+':
            case '-':
                if (!isdigit(code[i+1])) {
                    tampon = c;
                    res->append(provideString(tampon));
                    break;
                }
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
                noconvertingfloathexa((wchar_t*)code.c_str() + i, idx);
                tampon = code.substr(i, idx);
                res->append(provideString(tampon));
                i += idx - 1;
                break;
            }
            default: {
                idx = i;
                if (!handlingutf8->is_a_valid_letter(code, idx))  {
                    if (idx == i) {
                        tampon = c;
                        res->append(provideString(tampon));
                    }
                    else {
                        tampon = code.substr(i, idx - i + 1);
                        res->append(provideString(tampon));
                        i = idx;
                    }
                    break;
                }
                idx++;
                while (idx < sz && handlingutf8->is_a_valid_letter(code, idx)) idx++;
                tampon = code.substr(i, idx - i);
                i = idx-1;
                res->append(provideString(tampon));
                break;
            }
        }
    }
    return res;
}

/*
 As far as possible, we will try to avoid the multiplication of objects.
 status == s_constant means that the object is a constant and can never be destroyed...


 In Lisp, the abstract syntax tree and the execution tree are merged...
 We will build a structure in which we will browse the list of segments and for each parenthesis, we will
 build a sub-list...
 */

Element* LispE::abstractSyntaxTree(Element* courant, Tokenizer& parse, long& index, long quoting) {
    Element* e = NULL;
    double value;
    char topfunction = false;
    int16_t lab = -1;
        
    while (index < parse.types.size()) {
        parse.current = index;
        switch (parse.types[index]) {
            case c_opening: {
                if (quoting)
                    quoting++;
                lab = -1;
                index++;
                //Empty list
                if (parse.types[index] == c_closing) {
                    index++;
                    e = delegation->_EMPTYLIST;
                }
                else {
                    bool equal_op_list = false;
                    e = new Listincode(parse.lines[index], delegation->i_current_file);
                    garbaging(e);
                    abstractSyntaxTree(e, parse, index, quoting);
                    if (quoting) {
                        if (e->size() && e->index(0)->label() == l_conspoint) {
                            Element* a = e->eval(this);
                            garbaging(a);
                            removefromgarbage(e);
                            courant->append(a);
                            quoting--;
                            continue;
                        }
                    }
                    else {
                        if (e->size() >= 1) {
                            lab = e->index(0)->label();
                            bool docompose = true;
                            if (lab == l_composenot) {
                                ((List*)e)->liste.erase(0);
                                if (e->size()) {
                                    lab = e->index(0)->label();
                                    docompose = false;
                                }
                            }
                            switch(lab) {
                                    //for defmacro and link, we evaluate these expressions on the fly
                                case l_defspace:
                                    current_space = 0;
                                    continue;
                                case l_lambda:
                                    e->eval(this);
                                    break;
                                case l_defmacro:
                                case l_data:
                                case l_dethread:
                                case l_defun:
                                    e->eval(this);
                                    continue;
                                case l_defpat: {
                                    Element* arguments = e->index(2);
                                    Element* a;
                                    Element* idx;
                                    for (long i = 0; i < arguments->size(); i++) {
                                        idx = arguments->index(i);
                                        a = idx->transformargument(this);
                                        if (a != idx)
                                            ((List*)arguments)->liste.put(i, a);
                                    }
                                    e->eval(this);
                                    continue;
                                }
                                case l_setq:
                                case l_set_at:
                                case l_setg:
                                    if (e->size() > 1 && e->index(1)->label() < l_final) {
                                        wstring msg = L"Error: Invalid variable name: '";
                                        msg += e->index(1)->asString(this);
                                        msg += L"' (keyword)";
                                        throw new Error(msg);
                                    }
                                    break;
                                case l_plusequal:
                                case l_minusequal:
                                case l_multiplyequal:
                                case l_powerequal:
                                case l_leftshiftequal:
                                case l_rightshiftequal:
                                case l_bitandequal:
                                case l_bitandnotequal:
                                case l_bitorequal:
                                case l_bitxorequal:
                                case l_divideequal:
                                case l_modequal:
                                    if (e->size() > 1) {
                                        Element* nxt = e->index(1);
                                        if (nxt->label() < l_final) {
                                            if (nxt->isList()) {
                                                if (nxt->size() < 2 || nxt->index(0)->label() != l_at) {
                                                    wstring msg = L"Error: Expecting an index access with 'at'";
                                                    throw new Error(msg);
                                                }
                                                equal_op_list = true;
                                            }
                                            else {
                                                wstring msg = L"Error: Invalid variable name: '";
                                                msg += e->index(1)->asString(this);
                                                msg += L"' (keyword)";
                                                throw new Error(msg);
                                            }
                                        }
                                    }
                                    break;
                                case l_link:
                                    e->eval(this);
                                    removefromgarbage(e);
                                    continue;
                                case l_if:
                                    if (e->size() == 3)
                                        e->append(void_function);

                                    //The 'terminal' flag helps define if a potential call can be treated as terminal recursion
                                    if (topfunction && topfunction <= courant->size())
                                        e->setterminal();
                                    break;
                                case l_infix: {
                                    Element* inter = ((Listincode*)e)->eval_infix(this);
                                    if (inter != e) {
                                        removefromgarbage(e);
                                        e = inter;
                                        lab = e->index(0)->label();
                                    }
                                    else
                                        break;
                                }
                                default: {
                                    if (lab >= l_map && lab <= l_scanr1) {
                                        Element* inter = e->composing(this, docompose);
                                        if (inter != e) {
                                            removefromgarbage(e);
                                            e = inter;
                                            lab = 0;
                                        }
                                    }
                                    else {
                                        if (topfunction && topfunction <= courant->size()) {
                                            //The 'terminal' flag helps define if a potential call can be treated as terminal recursion
                                            e->setterminal();
                                        }
                                    }
                                }
                            }
                        }

                        e = generate_macro(e);

                        /*
                        We detect if it is an instruction beforehand, in order
                        to limit the call to lisp->delegation->evals during the execution (see Listincode::eval)
                        - List_basic_instruction is used for instructions that do not fail, which means that we do not need to record
                        their position and even trace them back.
                        - List_instruction on the other hand will set the position of the current instruction.
                        */
                        if (delegation->instructions.check(lab)) {
                            Element* lm = NULL;
                            long nbarguments = e->size();
                            switch (lab) {
                                case l_break:
                                    if (nbarguments != 1)
                                        throw new Error("Error: break does not take any arguments");
                                    removefromgarbage(e);
                                    e = &delegation->_BREAKEVAL;
                                    break;
                                case l_if:
                                    lm = new List_if((List*)e);
                                    break;
                                case l_power:
                                    if (nbarguments == 3 && e->index(2)->equalvalue((long)2))
                                        lm = new List_power2((List*)e);
                                    else
                                        lm = new List_execute((Listincode*)e, delegation->evals[lab]);
                                    break;
                                case l_divide:
                                    if (nbarguments == 2)
                                        lm = new List_divide2((List*)e);
                                    else
                                        if (nbarguments == 3)
                                            lm = new List_divide3((List*)e);
                                        else
                                            lm = new List_dividen((List*)e);
                                    break;
                                case l_plus:
                                    if (nbarguments == 2)
                                        lm = new List_plus2((List*)e);
                                    else
                                        if (nbarguments == 3)
                                            lm = new List_plus3((List*)e);
                                        else
                                            lm = new List_plusn((List*)e);
                                    break;
                                case l_minus:
                                    if (nbarguments == 2)
                                        lm = new List_minus2((List*)e);
                                    else
                                        if (nbarguments == 3)
                                            lm = new List_minus3((List*)e);
                                        else
                                            lm = new List_minusn((List*)e);
                                    break;
                                case l_multiply:
                                    if (nbarguments == 2)
                                        lm = new List_multiply2((List*)e);
                                    else
                                        if (nbarguments == 3)
                                            lm = new List_multiply3((List*)e);
                                        else
                                            lm = new List_multiplyn((List*)e);
                                    break;
                                case l_divideequal:
                                    if (equal_op_list)
                                        lm = new List_divideequal_list((List*)e);
                                    else
                                        lm = new List_divideequal_var((List*)e);
                                    break;
                                case l_plusequal:
                                    if (equal_op_list)
                                        lm = new List_plusequal_list((List*)e);
                                    else
                                        lm = new List_plusequal_var((List*)e);
                                    break;
                                case l_minusequal:
                                    if (equal_op_list)
                                        lm = new List_minusequal_list((List*)e);
                                    else
                                        lm = new List_minusequal_var((List*)e);
                                    break;
                                case l_multiplyequal:
                                    if (equal_op_list)
                                        lm = new List_multiplyequal_list((List*)e);
                                    else
                                        lm = new List_multiplyequal_var((List*)e);
                                    break;
                                case l_switch:
                                    lm = new Listswitch((Listincode*)e);
                                    ((Listswitch*)lm)->build(this);
                                    break;
                                case l_return:
                                    if (e->size() == 1)
                                        lm = new Listreturn();
                                    else
                                        lm = new Listreturnelement((Listincode*)e);
                                    break;
                                default:
                                    lm = new List_execute((Listincode*)e, delegation->evals[lab]);
                            }
                            
                            if (lm != NULL) {
                                garbaging(lm);
                                if (!delegation->checkArity(lab, nbarguments)) {
                                    wstring err = L"Error: Wrong number of arguments for: '";
                                    err += delegation->asString(lab);
                                    err += L"'";
                                    throw new Error(err);
                                }
                                removefromgarbage(e);
                                e = lm;
                            }
                        }
                    }
                }
                if (e->size())
                    courant->append(e);
                else {
                    courant->append(delegation->_EMPTYLIST);
                    removefromgarbage(e);
                }
                if (quoting)
                    quoting--;
                break;
            }
            case c_closing:
                index++;
                return delegation->_TRUE;
            case c_opening_brace: {
                index++;
                if (parse.types[index] == c_closing_brace) {
                    index++;
                    e = delegation->_EMPTYDICTIONARY;
                }
                else {
                    Dictionary_as_list dico;
                    abstractSyntaxTree(&dico, parse, index, quoting);
                    e = dico.dictionary(this);
                    garbaging(e);
                    if (e->isList()) {
                        //Then in that case, we need to provide a protection
                        //for each of its elements...
                        for (long i = 0; i < e->size(); i++)
                            control_garbaging(e->index(i));
                    }
                }
                courant->append(e);
                break;
            }
            case c_opening_data_brace: {
                index++;
                if (parse.types[index] == c_closing_brace) {
                    index++;
                    e = delegation->_EMPTYDICTIONARY;
                }
                else {
                    Dictionary_as_buffer dico(this);
                    abstractSyntaxTree(&dico, parse, index, quoting);
                    e = dico.dictionary(this);
                    garbaging(e);
                }
                courant->append(e);
                break;
            }
            case c_closing_brace:
                index++;
                return delegation->_TRUE;
            case t_emptystring:
                courant->append(delegation->_EMPTYSTRING);
                index++;
                break;
            case t_string:
                e = provideConststring(parse.tokens[index]);
                courant->append(e);
                index++;
                break;
            case t_number:
                value = parse.numbers[index];
                if (parse.tokens[index].find(U".") == -1)
                    e = provideConstinteger(value);
                else
                    e = provideConstnumber(value);                
                courant->append(e);
                index++;
                break;
            case t_operator:
                e = provideOperator(encode(parse.tokens[index]));
                courant->append(e);
                index++;
                break;
            case l_cadr:
                e =  provideCADR(parse.tokens[index]);
                courant->append(e);
                index++;
                break;
            case c_colon:
                if (courant->label() == t_dictionary) {
                    courant->reversechoice();
                    index++;
                    if (parse.types[index] == c_colon)
                        throw new Error("Error: wrong key/value separator in a dictionary");
                }
                else {
                    e = provideAtom(encode(parse.tokens[index]));
                    index++;
                    courant->append(e);
                }
                break;
            case c_point:
                if (quoting) {
                    index++;
                    if (courant->size() == 0)
                        throw new Error("Error: Wrong use of '.'");
                    if (parse.types[index] == c_opening) {
                        index++;
                        syntaxTree(courant, parse, index, quoting);
                    }
                    else {
                        syntaxTree(courant, parse, index, true);
                        ((List*)courant)->liste.insert(0, provideAtom(l_conspoint));
                    }
                }
                else {
                    e = provideAtom(encode(parse.tokens[index]));
                    index++;
                    courant->append(e);
                }
                break;
            case t_atom:
                e = provideAtom(encode(parse.tokens[index]));
                index++;
                if (!quoting && e->label() == l_quote) {
                    courant->append(e);
                    abstractSyntaxTree(courant, parse, index, true);
                }
                else {
                    if (!quoting) {
                        if (courant->size() == 0) {
                            if (e->type >= l_lambda && e->type <= l_defpat) {
                                if (e->type == l_lambda)
                                    topfunction = 2;
                                else
                                    topfunction = 3;
                            }
                        }
                        else {
                            if (courant->size() == 1 && courant->index(0)->label() == l_defspace) {
                                //We create a new name space in function_pool
                                courant->append(e);
                                courant->eval(this);
                                break;
                            }
                        }
                    }
                    courant->append(e);
                }
                break;
            case l_quote:
                e = new Listincode(parse.lines[index], delegation->i_current_file);
                index++;
                garbaging(e);
                e->append(provideAtom(l_quote));
                courant->append(e);
                abstractSyntaxTree(e, parse, index, true);
                if (e->size() != 2)
                    throw new Error("Error: Wrong number of arguments for 'quote'");
                break;
            default:
                break;
        }
        if (quoting == 1)
            return delegation->_TRUE;
    }
    return delegation->_TRUE;
}

Element* LispE::syntaxTree(Element* courant, Tokenizer& parse, long& index, long quoting) {
    Element* e = NULL;
    double value;
    char topfunction = false;
    int16_t lab = -1;
        
    while (index < parse.types.size()) {
        parse.current = index;
        switch (parse.types[index]) {
            case c_opening: {
                if (quoting)
                    quoting++;
                lab = -1;
                index++;
                //Empty list
                if (parse.types[index] == c_closing) {
                    index++;
                    e = delegation->_EMPTYLIST;
                }
                else {
                    e = provideList();
                    syntaxTree(e, parse, index, quoting);
                    if (quoting) {
                        if (e->size() && e->index(0)->label() == l_conspoint) {
                            Element* a = e->eval(this);
                            garbaging(a);
                            removefromgarbage(e);
                            e = a;
                        }
                    }
                }
                
                courant->append(e);
                if (quoting)
                    quoting--;
                break;
            }
            case c_closing:
                index++;
                return delegation->_TRUE;
            case c_opening_brace: {
                index++;
                if (parse.types[index] == c_closing_brace) {
                    index++;
                    e = delegation->_EMPTYDICTIONARY;
                }
                else {
                    Dictionary_as_list dico;
                    syntaxTree(&dico, parse, index, quoting);
                    e = dico.dictionary(this);
                }
                courant->append(e);
                break;
            }
            case c_opening_data_brace: {
                index++;
                if (parse.types[index] == c_closing_brace) {
                    index++;
                    e = delegation->_EMPTYDICTIONARY;
                }
                else {
                    Dictionary_as_buffer dico(this);
                    syntaxTree(&dico, parse, index, quoting);
                    e = dico.dictionary(this);
                }
                courant->append(e);
                break;
            }
            case c_closing_brace:
                index++;
                return delegation->_TRUE;
            case t_emptystring:
                courant->append(delegation->_EMPTYSTRING);
                index++;
                break;
            case t_string:
                e = provideConststring(parse.tokens[index]);
                courant->append(e);
                index++;
                break;
            case t_number:
                value = parse.numbers[index];
                if (parse.tokens[index].find(U".") == -1)
                    e = provideConstinteger(value);
                else
                    e = provideConstnumber(value);
                courant->append(e);
                index++;
                break;
            case t_operator:
                e = provideOperator(encode(parse.tokens[index]));
                courant->append(e);
                index++;
                break;
            case l_cadr:
                e =  provideCADR(parse.tokens[index]);
                courant->append(e);
                index++;
                break;
            case c_colon:
                if (courant->label() == t_dictionary) {
                    courant->reversechoice();
                    index++;
                    if (parse.types[index] == c_colon)
                        throw new Error("Error: wrong key/value separator in a dictionary");
                }
                else {
                    e = provideAtom(encode(parse.tokens[index]));
                    index++;
                    courant->append(e);
                }
                break;
            case c_point:
                if (quoting) {
                    index++;
                    if (courant->size() == 0)
                        throw new Error("Error: Wrong use of '.'");
                    if (parse.types[index] == c_opening) {
                        index++;
                        syntaxTree(courant, parse, index, quoting);
                    }
                    else {
                        syntaxTree(courant, parse, index, true);
                        ((List*)courant)->liste.insert(0, provideAtom(l_conspoint));
                    }
                }
                else {
                    e = provideAtom(encode(parse.tokens[index]));
                    index++;
                    courant->append(e);
                }
                break;
            case t_atom:
                e = provideAtom(encode(parse.tokens[index]));
                index++;
                if (!quoting && e->label() == l_quote) {
                    courant->append(e);
                    syntaxTree(courant, parse, index, true);
                }
                else {
                    if (!quoting && courant->size() == 0) {
                        if (e->type >= l_lambda && e->type <= l_defpat) {
                            if (e->type == l_lambda)
                                topfunction = 2;
                            else
                                topfunction = 3;
                        }
                    }
                    courant->append(e);
                }
                break;
            case l_quote:
                e = provideList();
                e->append(provideAtom(l_quote));
                courant->append(e);
                index++;
                syntaxTree(e, parse, index, true);
                if (e->size() != 2)
                    throw new Error("Error: Wrong number of arguments for 'quote'");
                break;
            default:
                break;
        }
        if (quoting == 1)
            return courant;
    }
    return courant;
}

Element* LispE::atomise(u_ustring a) {
    List* l = provideList();
    delegation->atomise(a, l, threaded());
    return l;
}

List* LispE::create_instruction(int16_t label,
                                Element* e1,
                                Element* e2,
                                Element* e3,
                                Element* e4,
                                Element* e5,
                                Element* e6,
                                Element* e7) {
    List* l = provideList();

    garbaging(l);
    l->append(provideAtom(label));
    if (!l->append_not_null(e1))
        return l;
    if (!l->append_not_null(e2))
        return l;
    if (!l->append_not_null(e3))
        return l;
    if (!l->append_not_null(e4))
        return l;
    if (!l->append_not_null(e5))
        return l;
    if (!l->append_not_null(e6))
        return l;
    if (!l->append_not_null(e7))
        return l;
    return l;
}

List* LispE::create_local_instruction(int16_t label,
                                Element* e1,
                                Element* e2,
                                Element* e3,
                                Element* e4,
                                Element* e5,
                                Element* e6,
                                Element* e7) {
    List* l = provideList();

    l->append(provideAtom(label));
    if (!l->append_not_null(e1))
        return l;
    if (!l->append_not_null(e2))
        return l;
    if (!l->append_not_null(e3))
        return l;
    if (!l->append_not_null(e4))
        return l;
    if (!l->append_not_null(e5))
        return l;
    if (!l->append_not_null(e6))
        return l;
    if (!l->append_not_null(e7))
        return l;
    return l;
}

Element* LispE::load(string pathname) {
    pathname = NormalizePathname(pathname);
    try {
        delegation->i_current_file = delegation->allfiles.at(pathname);
        return delegation->entrypoints.at(delegation->i_current_file);
    }
    catch (...) {}

    delegation->i_current_line = 0;
    std::ifstream f(pathname.c_str(),std::ios::in|std::ios::binary);
    if (f.fail()) {
        string err = "Unknown file: ";
        err += pathname;
        throw new Error(err);
    }

    string code;
    string ln;
    while (!f.eof()) {
        getline(f, ln);
        code += ln + "\n";
    }

    delegation->updatepathname(pathname);
    delegation->entrypoints[delegation->i_current_file] = delegation->_NULL;

    try {
        Element* tree = compile_lisp_code(code);
        delegation->entrypoints[delegation->i_current_file] = tree;

        current_path();
        delegation->reset_context();
        return tree->eval(this);
    }
    catch(Error* err) {
        delegation->forceClean();
        throw err;
    }
}

Element* LispE::compile_lisp_code(string& code) {
    clearStop();
    //A little trick to compile code sequences
    code = "(__root__ " + code + ")";
    Tokenizer parse;
    lisp_code retour = segmenting(code, parse);
    List courant;
    long index;
    switch (retour) {
        case e_error_brace:
            throw new Error("Error: Braces do not balance");
        case e_error_bracket:
            throw new Error("Error: brackets do not balance");
        case e_error_parenthesis:
            throw new Error("Error: parentheses do not balance");
        case e_error_string:
            delegation->i_current_line = 1;
            if (parse.current != -1 && parse.current < parse.lines.size())
                delegation->i_current_line = parse.lines[parse.current];
            else
                delegation->i_current_line = parse.lines.back();
            throw new Error("Error: missing end of string");
        default:
            index = 0;
    }

    try {
        abstractSyntaxTree(&courant, parse, index, false);
    }
    catch(Error* err) {
        delegation->i_current_line = 1;
        if (parse.current != -1 && parse.current < parse.lines.size())
            delegation->i_current_line = parse.lines[parse.current];
        else
            if (parse.lines.size())
                delegation->i_current_line = parse.lines.back();
        delegation->forceClean();
        throw err;
    }

    Element* e = courant.liste[0];
    
    if (e->size() == 2) {
        //The block contains only one element that can be evaluated immediately
        return e->index(1);
    }
    //It's really a big block
    return e;
}


//This method allows us to add new features in LispE in the form of
//loading of external libraries. See system.cxx for an example.
Element* LispE::extension(string code, Element* etendre) {
    Listincode* current_list = NULL;

    Tokenizer parse;
    lisp_code retour = segmenting(code, parse);

    switch (retour) {
        case e_error_brace:
            etendre->release();
            throw new Error("Error: Braces do not balance");
        case e_error_bracket:
            etendre->release();
            throw new Error("Error: brackets do not balance");
        case e_error_parenthesis:
            etendre->release();
            throw new Error("Error: parentheses do not balance");
        case e_error_string:
            etendre->release();
            delegation->i_current_line = 1;
            if (parse.current != -1 && parse.current < parse.lines.size())
                delegation->i_current_line = parse.lines[parse.current];
            else
                if (parse.lines.size())
                    delegation->i_current_line = parse.lines.back();
            throw new Error("Error: missing end of string");
        default:
            current_list = new Listincode;
            garbaging(current_list);
    }

    try {
        long index = 0;
        abstractSyntaxTree(current_list, parse, index, false);
        Element* body = current_list->eval(this);
        if (etendre != NULL) {
            body->append(etendre);
            garbaging(etendre);
        }
        return body;
    }
    catch(Error* err) {
        etendre->release();
        delegation->forceClean();
        return err;
    }
}

Element* LispE::execute(string code) {
    delegation->i_current_line = 0;
    delegation->i_current_file = 0;
    clearStop();
    try {
        return compile_lisp_code(code)->eval(this);
    }
    catch(Error* err) {
        delegation->forceClean();
        return err;
    }
}

Element* LispE::execute(string code, string pathname) {
    delegation->i_current_line = 0;
    delegation->i_current_file = 0;
    clearStop();
    if (pathname != "") {
        pathname = NormalizePathname(pathname);
        delegation->updatepathname(pathname);
    }

    try {
        Element* tree = compile_lisp_code(code);
        delegation->entrypoints[delegation->i_current_file] = tree;
        current_path();
        delegation->reset_context();
        return tree->eval(this);
    }
    catch(Error* err) {
        delegation->forceClean();
        return err;
    }
}

void LispE::set_pathname(string pathname) {
    delegation->i_current_line = 0;
    delegation->i_current_file = 0;
    clearStop();
    if (pathname != "") {
        pathname = NormalizePathname(pathname);
        delegation->updatepathname(pathname);
    }
    current_path();
}

void LispE::add_pathname(string pathname) {
    long i_file = delegation->i_current_file;
    if (pathname != "") {
        pathname = NormalizePathname(pathname);
        delegation->updatepathname(pathname);
    }
    delegation->i_current_file = i_file;
}

//------------------------------------------------------------------------------------------
// Macro section
//------------------------------------------------------------------------------------------
//We duplicate our macro into new code that will replace the current call...
void Element::generate_body_from_macro(LispE* lisp, Listincode* code, binHash<Element*>& dico_variables) {
    Element* e;
    for (long i = 0;i < size(); i++) {
        e = index(i);
        if (e->isAtom()) {
            if (dico_variables.check(e->label()))
                code->append(dico_variables[e->label()]);
            else
                code->append(e);
        }
        else {
            if (e->isList()) {
                Listincode* lcode = new Listincode(s_constant);
                lisp->garbaging(lcode);
                lcode->line = ((Listincode*)code)->line;
                lcode->fileidx = ((Listincode*)code)->fileidx;
                code->append(lcode);
                e->generate_body_from_macro(lisp, lcode, dico_variables);
            }
            else
                code->append(e);
        }
    }
}

//This method is systematically called at compiled time.
//It checks if the current instruction is a potential macro.
//If it is the case then:
//We generate the replacement code in two steps.
//First, we associate the macro variables with their replacement code...
//Then we generate a copy of the macro, in which the variables are replaced with
//their local interpretation...
Element* LispE::generate_macro(Element* code) {
    //code is our basis, the first element points to macro
    if (code->size() == 0)
        return code;

    int16_t label = code->index(0)->label();
    
    Element* macro_rule = delegation->macros.search(label);
    if (macro_rule != NULL) {
        Element* macro_parameters = macro_rule->index(2);
        if (macro_parameters->size() != code->size()-1)
            throw new Error("Error: parameter size does not match argument");
        //Now we need to create a place where to store our parameters...
        long i;
        binHash<Element*> dico_variables;
        //Keeping track of the what to replace
        for (i = 0; i < macro_parameters->size(); i++) {
            label = macro_parameters->index(i)->label();
            dico_variables[label] = code->index(i+1);
        }
        
        //We reuse the code that we need to replace with our macro...
        Listincode* lcode = (Listincode*)code;
        //We clear it... We have already saved the important parts of the code
        //within our macro variables...
        lcode->liste.clear();
        macro_rule->index(3)->generate_body_from_macro(this, lcode, dico_variables);
        return lcode;
    }
    return code;
}


//We replace the variables with local macro name...
void Element::replaceVariableNames(LispE* lisp, binHash<Element*>& dico_variables) {
    if (isList()) {
        Element* e;
        for (long i = 0; i < size(); i++) {
            e = index(i);
            if (e->isAtom() && dico_variables.check(e->label()))
                ((List*)this)->liste[i] = dico_variables[e->label()];
            else {
                if (e->isList()) {
                    e->replaceVariableNames(lisp, dico_variables);
                }
            }
        }
    }
}

//We replace the variable names in the code, with local names, that could not occur
//in actual codes. The idea is to leave free the possiblilty to use any variables that the user
//might find informative than to force on a specific variable encoding.
bool Element::replaceVariableNames(LispE* lisp) {
    //First we gather all names
    Element* macro_parameters = index(2);
    int16_t newlabel, varlabel;
    u_ustring lb;
    binHash<Element*> dico_variables;
    for (long i = 0; i < macro_parameters->size(); i++) {
        //Our new name
        lb = U"#macro";
        lb += convertToUString(i);
        //We encode it
        newlabel = lisp->encode(lb);
        varlabel = macro_parameters->index(i)->label();
        if (varlabel == v_null)
            return false;
        dico_variables[varlabel] = lisp->provideAtom(newlabel);
        ((List*)macro_parameters)->liste[i] = dico_variables[varlabel];
    }
    index(3)->replaceVariableNames(lisp, dico_variables);
    return true;
}

//The strings described in the code are kept.
Element* LispE::provideString(string& str) {
    u_ustring s;
    s_utf8_to_unicode(s, USTR(str), str.size());
    return provideString(s);
}

Element* LispE::provideString(wchar_t ch) {
    u_ustring s;
    s = (u_uchar)ch;
    return provideString(s);
}

Element* LispE::provideString(u_uchar ch) {
    u_ustring s;
    s = ch;
    return provideString(s);
}

Element* LispE::provideString(wstring& w) {
    u_pstring s = _w_to_u(w);
    return provideString(s);
}

/*
 Saving arguments in a list: _args
 */
void LispE::arguments(std::vector<string>& args) {
    List* l = provideList();
    for (auto& a: args) {
        l->append(provideString(a));
    }
    string nom = "_args";
    recordingunique(l, encode(nom));
}

void LispE::current_path() {
    if (delegation->allfiles.size() >= 2) {
        u_ustring nom = U"_current";
        string spath = delegation->allfiles_names[1];
        char localpath[4096];
        localpath[0] = 0;

#ifdef WIN32
        _fullpath(localpath, ".", 4096);
#else
        realpath(".", localpath);
#endif


        long pos = spath.rfind("/");
        if (pos == string::npos) {
            spath = localpath;
            if (localpath[spath.size() - 1] != '/')
                spath += "/";
        }
        else
            spath = spath.substr(0, pos + 1);

        Element* e = provideString(spath);
        execution_stack.back()->storing_variable(e, encode(nom));
        e->release();
    }
}











































