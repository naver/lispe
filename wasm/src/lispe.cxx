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
#include "tokens.h"
#ifdef UNIX
#include <sys/resource.h>
#endif
//------------------------------------------------------------
static std::string version = "1.2023.3.29.11.53";
string LispVersion() {
    return version;
}

extern "C" {
    const char* lispversion() {
        return version.c_str();
    }
}

#ifdef LISPE_WASM
static LispE* global_lispe_interpreter = NULL;
void clean_global_lispe() {
    if (global_lispe_interpreter != NULL)
        delete global_lispe_interpreter;
    global_lispe_interpreter = NULL;
}

void reset_global_lispe() {
    if (global_lispe_interpreter != NULL)
        delete global_lispe_interpreter;
    global_lispe_interpreter = new LispE();
}

void create_global_lispe() {
    global_lispe_interpreter = new LispE();
}

LispE* global_lispe() {
    return global_lispe_interpreter;
}

//In this case, errors are no longer treated as exceptions.
Element* Element::eval(LispE* lisp) {
    if (thrown_error)
        return thrown_error;
    return _eval(lisp)->check_if_error(lisp);
}

Element* Error::check_if_error(LispE* lisp) {
    return lisp->delegation->set_error(this);
}

#endif
//------------------------------------------------------------------------------------------
#ifdef MACDEBUG
    vector<Element*> __indexes;
#endif
//------------------------------------------------------------
wstring Stackelement::asString(LispE* lisp) {
    std::wstringstream message;
    binHash<Element*>::iterator a(variables);
    for (; !a.end(); a++)
        message << lisp->asString(a->first) << L": " << a->second->stringInList(lisp) << endl;
    return message.str();
}

List* Stackelement::atomes(LispE* lisp) {
    List* liste = lisp->provideList();
    binHash<Element*>::iterator a(variables);
    for (; !a.end(); a++)
        liste->append(lisp->provideAtom(a->first));
    for (long i = 0; i < lisp->delegation->function_pool.size(); i++) {
        for (a.set(lisp->delegation->function_pool[i]); !a.end(); a++)
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
    s_utf8_to_unicode(u, x, x.size());
    return u;
}

//------------------------------------------------------------
Delegation::Delegation() : main_tokenizer(NULL) {
    mark = 0;
    current_error = NULL;
    
#ifdef LISPE_WASM
    input_handler = NULL;
    reading_string_function = NULL;
    reading_string_function_object = NULL;
#else
    input_handler = get_jag_handler();
    reading_string_function = &lispe_readfromkeyboard;
    reading_string_function_object = input_handler;
#endif
    display_string_function = &lispe_displaystring;
    id_pool = 1;
    
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
#ifndef LISPE_WASM
    clean_get_handler(input_handler);
#endif
    reset_error();
    function_pool.cleaning();
    method_pool.cleaning();
    for (auto& a : thread_pool)
        a.second.clear();
    for (const auto& a: locks)
        delete a.second;
    binHash<Listincode*>::iterator al(straight_eval);
    for (; !al.end(); al++)
        delete al->second;
    binhash<Element*>::iterator h(atom_pool);
	for (; !h.end(); h++)
        delete h->second;
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
#ifndef LISPE_WASM
void moduleSocket(LispE* lisp);
#endif
void moduleOntology(LispE* lisp);
#ifdef FLTKGUI
void moduleGUI(LispE* lisp);
#endif
//------------------------------------------------------------
//We initialize our structures
void Delegation::initialisation(LispE* lisp) {
    main_tokenizer.access = lisp->handlingutf8;
    main_tokenizer.initialize();    
    
    //We initialize all instructions with eval_error
    //Only those for which an instruction exists will be replaced with
    //the proper call...
    //For the others we will Throw an exception...
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
    evals[t_call] = &List::evalt_call;
    evals[t_eval] = &List::eval;
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
    set_instruction(l_next, "%__next__%", P_TWO, &List::evall_next);
    //Note that __root__ is the top function that evaluate the whole code
    //root_ on the other hand is the variable that points to the whole code
    set_instruction(l_root, "__root__", P_ATLEASTONE, &List::evall_root);
    code_to_string[l_code] = U"_root";
    set_instruction(l_and, "and", P_ATLEASTTHREE, &List::evall_and, new List_and_eval());
    set_instruction(l_andvalue, "andvalue", P_ATLEASTTHREE, &List::evall_andvalue, new List_andvalue_eval());
    set_instruction(l_apply, "apply", P_THREE, &List::evall_apply, new List_apply_eval());
    set_instruction(l_at, "at", P_ATLEASTTHREE, &List::evall_at, new List_at_eval());
    set_instruction(l_at_shape, "atshape", P_ATLEASTFOUR, &List::evall_at_shape, new List_at_shape_eval());
    set_instruction(l_atom, "atom", P_TWO, &List::evall_converttoatom, new List_converttoatom_eval());
    set_instruction(l_atomise, "explode", P_TWO, &List::evall_atomise, new List_atomise_eval());
    set_instruction(l_atomp, "atomp", P_TWO, &List::evall_atomp, new List_atomp_eval());
    set_instruction(l_atoms, "atoms", P_ONE, &List::evall_atoms);
    set_instruction(l_bitand, "&", P_ATLEASTTWO, &List::evall_bitand);
    set_instruction(l_bitandequal, "&=", P_ATLEASTTHREE, &List::evall_bitandequal);
    set_instruction(l_bitandnot, "&~", P_ATLEASTTWO, &List::evall_bitandnot);
    set_instruction(l_bitandnotequal, "&~=", P_ATLEASTTHREE, &List::evall_bitandnotequal);
    set_instruction(l_bitnot, "~", P_TWO, &List::evall_bitnot);
    set_instruction(l_bitor, "|", P_ATLEASTTWO, &List::evall_bitor);
    set_instruction(l_bitorequal, "|=", P_ATLEASTTHREE, &List::evall_bitorequal);
    set_instruction(l_bitxor, "^", P_ATLEASTTWO, &List::evall_bitxor);
    set_instruction(l_bitxorequal, "^=", P_ATLEASTTHREE, &List::evall_bitxorequal);
    set_instruction(l_block, "block", P_ATLEASTONE, &List::evall_block, new List_block_eval());
    set_instruction(l_bodies, "bodies", P_TWO, &List::evall_bodies, new List_bodies_eval());
    set_instruction(l_break, "break", P_ONE, &List::evall_break);
    set_instruction(l_cadr, "cadr", P_TWO, &List::evall_cadr, new List_cadr_eval());
    set_instruction(l_car, "car", P_TWO, &List::evall_car, new List_car_eval());
    set_instruction(l_catch, "catch", P_ATLEASTTWO, &List::evall_catch, new List_catch_eval());
    set_instruction(l_cdr, "cdr", P_TWO, &List::evall_cdr, new List_cdr_eval());
    set_instruction(l_check, "check", P_ATLEASTTWO, &List::evall_check, new List_check_eval());
    set_instruction(l_compile, "loadcode", P_TWO | P_THREE, &List::evall_compile);
    set_instruction(l_cond, "cond", P_ATLEASTTWO, &List::evall_cond, new List_cond_eval());
    set_instruction(l_cons, "cons", P_THREE, &List::evall_cons, new List_cons_eval());
    set_instruction(l_consb, "consb", P_THREE, &List::evall_consb, new List_consb_eval());
    set_instruction(l_consp, "consp", P_TWO, &List::evall_consp, new List_consp_eval());
    set_instruction(l_conspoint, "conspoint", P_ATLEASTTWO, &List::evall_conspoint);
    set_instruction(l_count, "count", P_THREE|P_FOUR, &List::evall_count, new List_count_eval());
    set_instruction(l_cutlist, "cutlist", P_THREE, &List::evall_cutlist, new List_cutlist_eval());
    set_instruction(l_cyclic, "cyclicp", P_TWO, &List::evall_cyclicp, new List_cyclicp_eval());
    set_instruction(l_short, "int16_t", P_TWO, &List::evall_converttoshort, new List_converttoshort_eval());
    set_instruction(l_integer, "integer", P_TWO, &List::evall_converttointeger, new List_integer_eval());
    set_instruction(l_float, "float", P_TWO, &List::evall_converttofloat, new List_converttofloat_eval());
    set_instruction(l_complex, "complex", P_THREE, &List::evall_complex, new List_complex_eval());
    set_instruction(l_real, "real", P_TWO, &List::evall_real, new List_real_eval());
    set_instruction(l_imaginary, "imaginary", P_TWO, &List::evall_imaginary, new List_imaginary_eval());
    set_instruction(l_number, "number", P_TWO, &List::evall_converttonumber, new List_number_eval());
    set_instruction(l_string, "string", P_TWO, &List::evall_converttostring, new List_string_eval());
    set_instruction(l_data, "data", P_ATLEASTTWO, &List::evall_data);
    set_instruction(l_deflib, "deflib", P_THREE, &List::evall_deflib);
    set_instruction(l_deflibpat, "deflibpat", P_THREE, &List::evall_deflibpat);
    set_instruction(l_defmacro, "defmacro", P_FOUR, &List::evall_defmacro);
    set_instruction(l_defpat, "defpat", P_ATLEASTFOUR, &List::evall_defpat);
    set_instruction(l_defspace, "defspace", P_TWO, &List::evall_defspace);
    set_instruction(l_defun, "defun", P_ATLEASTFOUR, &List::evall_defun);
    set_instruction(l_dethread, "dethread", P_ATLEASTFOUR, &List::evall_defun);
    set_instruction(l_dictionary, "dictionary", P_ONE | P_ATLEASTTHREE, &List::evall_dictionary, new List_dictionary_eval());
    set_instruction(l_dictionaryi, "dictionaryi", P_ONE | P_ATLEASTTHREE, &List::evall_dictionaryi, new List_dictionaryi_eval());
    set_instruction(l_dictionaryn, "dictionaryn", P_ONE | P_ATLEASTTHREE, &List::evall_dictionaryn, new List_dictionaryn_eval());
    set_instruction(l_different, "!=", P_ATLEASTTHREE, &List::evall_different, new List_different_eval());
    set_instruction(l_divide, "/", P_ATLEASTTWO, &List::evall_divide, new List_dividen());
    set_instruction(l_divideequal, "/=", P_ATLEASTTHREE, &List::evall_divideequal);
    set_instruction(l_clone, "clone", P_TWO, &List::evall_clone, new List_clone_eval());
    set_instruction(l_elapse, "elapse", P_ATLEASTONE, &List::evall_elapse, new List_elapse_eval());
    set_instruction(l_emptyp, "emptyp", P_TWO, &List::evall_emptyp, new List_emptyp_eval());
    set_instruction(l_eq, "eq", P_ATLEASTTHREE, &List::evall_eq, new List_eq_eval());
    set_instruction(l_equal, "=", P_THREE, &List::evall_equal, new List_equal_eval());
    set_instruction(l_eval, "eval", P_TWO, &List::evall_eval, new List_eval_eval());
    set_instruction(l_extend, "extend", P_THREE, &List::evall_extend, new List_extend_eval());
    set_instruction(l_extract, "extract", P_THREE|P_FOUR|P_FIVE|P_SIX, &List::evall_extract, new List_extract_eval());
    set_instruction(l_factorial, "!", P_TWO, &List::evall_factorial, new List_factorial_eval());
    set_instruction(l_fappend, "fappend", P_THREE, &List::evall_fappend, new List_fappend_eval());
    set_instruction(l_stringf, "stringf", P_THREE, &List::evall_stringf, new List_stringf_eval());
    set_instruction(l_filterlist, "filterlist", P_THREE | P_FOUR, &List::evall_filterlist, new List_filterlist_eval());
    set_instruction(l_droplist, "droplist", P_THREE | P_FOUR, &List::evall_droplist, new List_droplist_eval());
    set_instruction(l_takelist, "takelist", P_THREE | P_FOUR, &List::evall_takelist, new List_takelist_eval());
    set_instruction(l_filtercar, "filtercar", P_THREE, &List::evall_filterlist, new List_filterlist_eval());
    set_instruction(l_dropcar, "dropcar", P_THREE, &List::evall_droplist, new List_droplist_eval());
    set_instruction(l_takecar, "takecar", P_THREE, &List::evall_takelist, new List_takelist_eval());
    set_instruction(l_flatten, "flatten", P_TWO, &List::evall_flatten, new List_flatten_eval());
    set_instruction(l_flip, "flip", P_TWO, &List::evall_flip, new List_flip_eval());
    set_instruction(l_fread, "fread", P_TWO, &List::evall_fread, new List_fread_eval());
    set_instruction(l_fwrite, "fwrite", P_THREE, &List::evall_fwrite, new List_fwrite_eval());
    set_instruction(l_greater, ">", P_ATLEASTTHREE, &List::evall_greater, new List_greater_eval());
    set_instruction(l_greaterorequal, ">=", P_ATLEASTTHREE, &List::evall_greaterorequal, new List_greaterorequal_eval());
    set_instruction(l_if, "if", P_THREE | P_FOUR, &List::evall_if, new List_if_eval());
    set_instruction(l_ife, "ife", P_ATLEASTFOUR, &List::evall_ife, new List_ife_eval());
    set_instruction(l_in, "in", P_THREE, &List::evall_in, new List_in_eval());
    set_instruction(l_infix, "infix", P_TWO, &List::evall_infix, new List_infix_eval());
#ifndef LISPE_WASM
    set_instruction(l_getchar, "getchar", P_ONE, &List::evall_getchar);
    set_instruction(l_input, "input", P_ONE | P_TWO, &List::evall_input);
#endif
    set_instruction(l_insert, "insert", P_THREE | P_FOUR, &List::evall_insert, new List_insert_eval());
    set_instruction(l_addr_, "addr_", P_TWO, &List::evall_addr_);
    set_instruction(l_shorts, "shorts", P_ATLEASTONE, &List::evall_shorts, new List_shorts_eval());
    set_instruction(l_integers, "integers", P_ATLEASTONE, &List::evall_integers, new List_integers_eval());
    set_instruction(l_irange, "irange", P_THREE | P_FOUR, &List::evall_irange, new List_irange_eval());
    set_instruction(l_irangein, "irangein", P_THREE | P_FOUR, &List::evall_irangein, new List_irangein_eval());
    set_instruction(l_join, "join", P_TWO | P_THREE, &List::evall_join, new List_join_eval());
    set_instruction(l_key, "key", P_ONE|P_ATLEASTTHREE, &List::evall_key, new List_key_eval());
    set_instruction(l_keyi, "keyi", P_ONE|P_ATLEASTTHREE, &List::evall_keyi, new List_keyi_eval());
    set_instruction(l_keyn, "keyn", P_ONE|P_ATLEASTTHREE, &List::evall_keyn, new List_keyn_eval());
    set_instruction(l_keys, "keys@", P_TWO, &List::evall_keys, new List_keys_eval());
    set_instruction(l_label, "label", P_THREE, &List::evall_label, new List_label_eval());
    set_instruction(l_lambda, "λ", P_ATLEASTTHREE, &List::evall_lambda, new List_lambda_eval());
    set_instruction(l_last, "last", P_TWO, &List::evall_last, new List_last_eval());
    set_instruction(l_leftshift, "<<", P_ATLEASTTWO, &List::evall_leftshift, new List_leftshift_eval());
    set_instruction(l_leftshiftequal, "<<=", P_ATLEASTTHREE, &List::evall_leftshiftequal);
    set_instruction(l_link, "link", P_THREE, &List::evall_link);
    set_instruction(l_list, "list", P_ATLEASTONE, &List::evall_list, new List_list_eval());
    set_instruction(l_llist, "llist", P_ATLEASTONE, &List::evall_llist, new List_llist_eval());
    set_instruction(l_listand, "&&&", P_ATLEASTTWO, &List::evall_listand, new List_listand_eval());
    set_instruction(l_listor, "|||", P_ATLEASTTWO, &List::evall_listor, new List_listor_eval());
    set_instruction(l_listxor, "^^^", P_ATLEASTTWO, &List::evall_listxor, new List_listxor_eval());
    set_instruction(l_to_list, "to_list", P_TWO | P_THREE, &List::evall_to_list, new List_to_list_eval());
    set_instruction(l_to_llist, "to_llist", P_TWO, &List::evall_to_llist, new List_to_llist_eval());
    set_instruction(l_let, "let", P_THREE, &List::evall_let, new List_let_eval());
    set_instruction(l_load, "load", P_TWO | P_THREE, &List::evall_load);
    set_instruction(l_lock, "lock", P_ATLEASTTWO, &List::evall_lock, new List_lock_eval());
    set_instruction(l_loop, "loop", P_ATLEASTFOUR, &List::evall_loop, new List_loop_eval());
    set_instruction(l_mloop, "mloop", P_ATLEASTFOUR, &List::mloop, new List_mloop_eval());
    set_instruction(l_lloop, "lloop", P_ATLEASTFOUR, &List::lloop, new List_lloop_eval());
    set_instruction(l_loopcount, "loopcount", P_ATLEASTTHREE, &List::evall_loopcount, new List_loopcount_eval());
    set_instruction(l_compare, ">=<", P_THREE, &List::evall_compare, new List_compare_eval());
    set_instruction(l_lower, "<", P_ATLEASTTHREE, &List::evall_lower, new List_lower_eval());
    set_instruction(l_lowerorequal, "<=", P_ATLEASTTHREE, &List::evall_lowerorequal, new List_lowerorequal_eval());
    set_instruction(l_maplist, "maplist", P_THREE | P_FOUR, &List::evall_maplist, new List_maplist_eval());
    set_instruction(l_mapcar, "mapcar", P_THREE, &List::evall_maplist, new List_maplist_eval());
    set_instruction(l_mark, "mark", P_THREE | P_TWO, &List::evall_mark, new List_mark_eval());
    set_instruction(l_matrix, "matrix", P_TWO | P_THREE | P_FOUR, &List::evall_matrix, new List_matrix_eval());
    set_instruction(l_matrix_float, "matrix_float", P_TWO | P_THREE | P_FOUR, &List::evall_matrix_float, new List_matrix_float_eval());
    set_instruction(l_minmax, "minmax", P_ATLEASTTWO, &List::evall_minmax, new List_minmax_eval());
    set_instruction(l_max, "max", P_ATLEASTTWO, &List::evall_max, new List_max_eval());
    set_instruction(l_maybe, "maybe", P_ATLEASTTWO, &List::evall_maybe, new List_maybe_eval());
    set_instruction(l_min, "min", P_ATLEASTTWO, &List::evall_min, new List_min_eval());
    set_instruction(l_minus, "-", P_ATLEASTTWO, &List::evall_minus, new List_minusn());
    set_instruction(l_minusequal, "-=", P_ATLEASTTHREE, &List::evall_minusequal);
    set_instruction(l_mod, "%", P_ATLEASTTWO, &List::evall_mod, new List_mod_eval());
    set_instruction(l_modequal, "%=", P_ATLEASTTHREE, &List::evall_modequal);
    set_instruction(l_multiply, "*", P_ATLEASTTWO, &List::evall_multiply, new List_multiplyn());
    set_instruction(l_multiplyequal, "*=", P_ATLEASTTHREE, &List::evall_multiplyequal);
    set_instruction(l_ncheck, "ncheck", P_ATLEASTTHREE, &List::evall_ncheck, new List_ncheck_eval());
    set_instruction(l_nconc, "nconc", P_ATLEASTONE, &List::evall_nconc, new List_nconc_eval());
    set_instruction(l_nconcn, "nconcn", P_ATLEASTONE, &List::evall_nconcn, new List_nconcn_eval());
    set_instruction(l_neq, "neq", P_ATLEASTTHREE, &List::evall_neq, new List_neq_eval());
    set_instruction(l_not, "¬", P_TWO, &List::evall_not, new List_not_eval());
    set_instruction(l_nullp, "nullp", P_TWO, &List::evall_nullp, new List_nullp_eval());
    set_instruction(l_numberp, "numberp", P_TWO, &List::evall_numberp, new List_numberp_eval());
    set_instruction(l_floats, "floats", P_ATLEASTONE, &List::evall_floats, new List_floats_eval());
    set_instruction(l_numbers, "numbers", P_ATLEASTONE, &List::evall_numbers, new List_numbers_eval());
    set_instruction(l_or, "or", P_ATLEASTTHREE, &List::evall_or, new List_or_eval());
    set_instruction(l_pipe, "pipe", P_ONE, &List::evall_pipe, new List_pipe_eval());
    set_instruction(l_plus, "+", P_ATLEASTTWO, &List::evall_plus, new List_plusn());
    set_instruction(l_plusequal, "+=", P_ATLEASTTHREE, &List::evall_plusequal);
    set_instruction(l_pop, "pop", P_TWO | P_THREE, &List::evall_pop, new List_pop_eval());
    set_instruction(l_popfirst, "popfirst", P_TWO, &List::evall_popfirst, new List_popfirst_eval());
    set_instruction(l_poplast, "poplast", P_TWO, &List::evall_poplast, new List_poplast_eval());
    set_instruction(l_power, "^^", P_ATLEASTTWO, &List::evall_power);
    set_instruction(l_powerequal, "^^=", P_ATLEASTTHREE, &List::evall_powerequal);
    set_instruction(l_prettify, "prettify", P_TWO | P_THREE, &List::evall_prettify, new List_prettify_eval());
    set_instruction(l_print, "print", P_ATLEASTONE, &List::evall_print, new List_print_eval());
    set_instruction(l_printerr, "printerr", P_ATLEASTONE, &List::evall_printerr, new List_printerr_eval());
    set_instruction(l_printerrln, "printerrln", P_ATLEASTONE, &List::evall_printerrln, new List_printerrln_eval());
    set_instruction(l_println, "println", P_ATLEASTONE, &List::evall_println, new List_println_eval());
    set_instruction(l_product, "∏", P_TWO, &List::evall_product, new List_product_eval());
    set_instruction(l_push, "push", P_ATLEASTTHREE, &List::evall_push, new List_push_eval());
    set_instruction(l_pushtrue, "pushtrue", P_ATLEASTTHREE, &List::evall_pushtrue, new List_pushtrue_eval());
    set_instruction(l_pushfirst, "pushfirst", P_ATLEASTTHREE, &List::evall_pushfirst, new List_pushfirst_eval());
    set_instruction(l_pushlast, "pushlast", P_ATLEASTTHREE, &List::evall_pushlast, new List_pushlast_eval());
    set_instruction(l_quote, "quote", P_TWO, &List::evall_quote);
    set_instruction(l_range, "range", P_FOUR, &List::evall_range, new List_range_eval());
    set_instruction(l_rangein, "rangein", P_FOUR, &List::evall_rangein, new List_rangein_eval());
    set_instruction(l_resetmark, "resetmark", P_TWO, &List::evall_resetmark, new List_resetmark_eval());
    set_instruction(l_return, "return", P_ONE | P_TWO , &List::evall_return);
    set_instruction(l_replaceall, "replaceall", P_FOUR, &List::evall_replaceall, new List_replaceall_eval());
    set_instruction(l_reverse, "⌽", P_TWO | P_THREE, &List::evall_reverse, new List_reverse_eval());
    set_instruction(l_revertsearch, "rfind", P_THREE | P_FOUR, &List::evall_revertsearch, new List_revertsearch_eval());
    set_instruction(l_rightshift, ">>", P_ATLEASTTWO, &List::evall_rightshift, new List_rightshift_eval());
    set_instruction(l_rightshiftequal, ">>=", P_ATLEASTTHREE, &List::evall_rightshiftequal);
    set_instruction(l_rotate, "rotate", P_TWO | P_THREE, &List::evall_rotate, new List_rotate_eval());
    set_instruction(l_search, "find", P_THREE|P_FOUR, &List::evall_search, new List_search_eval());
    set_instruction(l_searchall, "findall", P_THREE|P_FOUR, &List::evall_searchall, new List_searchall_eval());
    set_instruction(l_select, "select", P_ATLEASTTWO, &List::evall_select, new List_select_eval());
    set_instruction(l_self, "self", P_ATLEASTONE, &List::eval_call_self);
    set_instruction(l_set_range, "setrange", P_FOUR|P_FIVE|P_SIX|P_SEVEN, &List::evall_set_range, new List_set_range_eval());
    set_instruction(l_set, "set", P_ATLEASTONE, &List::evall_set, new List_set_eval());
    set_instruction(l_sets, "sets", P_ATLEASTONE, &List::evall_sets, new List_sets_eval());
    set_instruction(l_seti, "seti", P_ATLEASTONE, &List::evall_seti, new List_seti_eval());
    set_instruction(l_setn, "setn", P_ATLEASTONE, &List::evall_setn, new List_setn_eval());
    set_instruction(l_set_at, "set@", P_ATLEASTTHREE, &List::evall_set_at, new List_set_at_eval());
    set_instruction(l_set_const, "setconst", P_THREE, &List::evall_set_const, new List_set_const_eval());
    set_instruction(l_set_shape, "setshape", P_ATLEASTFIVE, &List::evall_set_shape, new List_set_shape_eval());
    set_instruction(l_setg, "setg", P_THREE, &List::evall_setg, new List_setg_eval());
    set_instruction(l_setq, "setq", P_THREE, &List::evall_setq, new List_setq_eval());
    set_instruction(l_seth, "seth", P_THREE, &List::evall_seth, new List_seth_eval());
    set_instruction(l_sign, "sign", P_TWO, &List::evall_sign, new List_sign_eval());
    set_instruction(l_signp, "signp", P_TWO, &List::evall_signp, new List_signp_eval());
    set_instruction(l_size, "size", P_TWO, &List::evall_size, new List_size_eval());
    set_instruction(l_sleep, "sleep", P_TWO, &List::evall_sleep, new List_sleep_eval());
    set_instruction(l_sort, "sort", P_THREE, &List::evall_sort, new List_sort_eval());
    set_instruction(l_space, "space", P_ATLEASTTHREE, &List::evall_space, new List_space_eval());
    set_instruction(l_stringp, "stringp", P_TWO, &List::evall_stringp, new List_stringp_eval());
    set_instruction(l_strings, "strings", P_ATLEASTONE, &List::evall_strings, new List_strings_eval());
    set_instruction(l_switch, "switch", P_ATLEASTTHREE, &List::evall_switch);
    set_instruction(l_sum, "∑", P_TWO, &List::evall_sum, new List_sum_eval());
    set_instruction(l_tensor, "tensor", P_ATLEASTTWO, &List::evall_tensor, new List_tensor_eval());
    set_instruction(l_tensor_float, "tensor_float", P_ATLEASTTWO, &List::evall_tensor_float, new List_tensor_float_eval());
    set_instruction(l_threadclear, "threadclear", P_ONE | P_TWO, &List::evall_threadclear);
    set_instruction(l_threadretrieve, "threadretrieve", P_ONE | P_TWO, &List::evall_threadretrieve);
    set_instruction(l_threadstore, "threadstore", P_THREE, &List::evall_threadstore);
    set_instruction(l_threadspace, "threadspace", P_ATLEASTONE, &List::evall_threadspace);
    set_instruction(l_throw, "throw", P_TWO, &List::evall_throw);
    set_instruction(l_trace, "trace", P_ONE | P_TWO, &List::evall_trace);
    set_instruction(l_transpose, "⍉", P_TWO, &List::evall_transpose, new List_transpose_eval());
    set_instruction(l_heap, "heap", P_ATLEASTONE, &List::evall_heap, new List_heap_eval());
    set_instruction(l_trigger, "trigger", P_TWO, &List::evall_trigger, new List_trigger_eval());
    set_instruction(l_type, "type", P_TWO, &List::evall_type, new List_type_eval());
    set_instruction(l_unique, "unique", P_TWO, &List::evall_unique, new List_unique_eval());
    set_instruction(l_use, "use", P_TWO | P_THREE, &List::evall_use);
    set_instruction(l_values, "values@", P_TWO, &List::evall_values, new List_values_eval());
    set_instruction(l_wait, "wait", P_ONE, &List::evall_wait, new List_wait_eval());
    set_instruction(l_waiton, "waiton", P_TWO, &List::evall_waiton, new List_waiton_eval());
    set_instruction(l_while, "while", P_ATLEASTTHREE, &List::evall_while, new List_while_eval());
    set_instruction(l_xor, "xor", P_ATLEASTTHREE, &List::evall_xor, new List_xor_eval());
    set_instruction(l_zerop, "zerop", P_TWO, &List::evall_zerop, new List_zerop_eval());
    set_instruction(l_zip, "zip", P_ATLEASTTHREE, &List::evall_zip, new List_zip_eval());
    set_instruction(l_zipwith, "zipwith", P_ATLEASTFOUR, &List::evall_zipwith, new List_zipwith_eval());
#ifdef MACDEBUG
    set_instruction(l_debug_function, "f_debug", P_FULL, &List::List::evall_debug_function);
#endif
    
    // High level functions
    set_instruction(l_cycle, "cycle", P_TWO, &List::evall_cycle_cps);
    set_instruction(l_drop, "drop", P_THREE, &List::evall_drop_cps);
    set_instruction(l_dropwhile, "dropwhile", P_THREE, &List::evall_dropwhile_cps);
    set_instruction(l_filter, "filter", P_THREE, &List::evall_filter_cps);
    set_instruction(l_foldl, "foldl", P_FOUR, &List::evall_foldl_cps);
    set_instruction(l_foldl1, "foldl1", P_THREE, &List::evall_foldl1_cps);
    set_instruction(l_foldr, "foldr", P_FOUR, &List::evall_foldr_cps);
    set_instruction(l_foldr1, "foldr1", P_THREE, &List::evall_foldr1_cps);
    set_instruction(l_for, "for", P_FOUR, &List::evall_for_cps);
    set_instruction(l_map, "map", P_THREE, &List::evall_map_cps);
    set_instruction(l_repeat, "repeat", P_TWO, &List::evall_repeat_cps);
    set_instruction(l_replicate, "replicate", P_THREE, &List::evall_replicate, new List_replicate_eval());
    set_instruction(l_scanl, "scanl", P_FOUR, &List::evall_scanl_cps);
    set_instruction(l_scanl1, "scanl1", P_THREE, &List::evall_scanl1_cps);
    set_instruction(l_scanr, "scanr", P_FOUR, &List::evall_scanr_cps);
    set_instruction(l_scanr1, "scanr1", P_THREE, &List::evall_scanr1_cps);
    set_instruction(l_take, "take", P_THREE, &List::evall_take_cps);
    set_instruction(l_takewhile, "takewhile", P_THREE, &List::evall_takewhile_cps);
    
    //APL-like
    set_instruction(l_innerproduct, ".", P_FIVE, &List::evall_innerproduct, new List_innerproduct_eval());
    set_instruction(l_outerproduct, "°", P_FOUR, &List::evall_outerproduct, new List_outerproduct_eval());
    set_instruction(l_iota, "⍳", P_ATLEASTTWO, &List::evall_iota, new List_iota_eval());
    set_instruction(l_invert, "⌹", P_TWO | P_THREE, &List::evall_invert, new List_invert_eval());
    set_instruction(l_solve, "solve", P_THREE, &List::evall_solve, new List_solve_eval());
    set_instruction(l_determinant, "determinant", P_TWO, &List::evall_determinant, new List_determinant_eval());
    set_instruction(l_ludcmp, "ludcmp", P_TWO, &List::evall_ludcmp, new List_ludcmp_eval());
    set_instruction(l_lubksb, "lubksb", P_FOUR | P_THREE, &List::evall_lubksb, new List_lubksb_eval());
    set_instruction(l_iota0, "⍳0", P_ATLEASTTWO, &List::evall_iota0, new List_iota0_eval());
    set_instruction(l_irank, "irank", P_ATLEASTTHREE, &List::evall_irank, new List_irank_eval());
    set_instruction(l_reduce, "reduce", P_TWO | P_THREE, &List::evall_reduce, new List_reduce_eval());
    set_instruction(l_scan, "scan", P_THREE, &List::evall_scan, new List_scan_eval());
    set_instruction(l_backreduce, "⌿", P_TWO | P_THREE, &List::evall_backreduce, new List_backreduce_eval());
    set_instruction(l_backscan, "⍀", P_THREE, &List::evall_backscan, new List_backscan_eval());
    set_instruction(l_rank, "⍤", P_ATLEASTTWO, &List::evall_rank, new List_rank_eval());
    set_instruction(l_equalonezero, "==", P_THREE, &List::evall_equalonezero, new List_equalonezero_eval());
    set_instruction(l_rho, "⍴", P_ATLEASTTWO, &List::evall_rho, new List_rho_eval());
    set_instruction(l_member, "∈", P_THREE, &List::evall_member, new List_member_eval());
    set_instruction(l_concatenate, ",", P_TWO|P_THREE, &List::evall_concatenate, new List_concatenate_eval());
    //This line is necessary to apply a lambda as an argument (see apply)
    straight_eval[t_call_lambda] = new List_call_lambda();
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
    code_to_string[t_complex] = U"complex_";
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
    
    code_to_string[l_thread] = U"thread";
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
    binHashe<u_ustring>::iterator it(code_to_string);
    for (; !it.end(); it++)
        string_to_code[it->second] = it->first;
    //We are preparing a recording place for labels and functions .
    code_to_string[v_emptyatom] = U"";
    _NULL = (Atome*)lisp->provideAtomOrInstruction(v_null);
    _ERROR = (Atome*)lisp->provideAtomOrInstruction(t_error);
    _TERMINAL = (Atome*)lisp->provideAtomOrInstruction(l_terminal);
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
    _EMPTYLIST = new List_emptylist_eval();
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
    lisp->_BOOLEANS[0][0] = _NULL;
    lisp->_BOOLEANS[0][1] = _TRUE;
    lisp->_BOOLEANS[1][0] = _ZERO;
    lisp->_BOOLEANS[1][1] = _ONE;
    
    lisp->select_bool_as_one = false;
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
    provideAtomType(t_complex);
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
    recordingData(lisp->create_instruction(t_complex, _NULL), t_complex, v_null);
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
    //We introduce nth as a substitute to at (as in Common Lisp)
    w = U"nth";
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
    //We introduce dotimes as a substitute to loopcount (Common Lisp)
    w = U"dotimes";
    string_to_code[w] = l_loopcount;
    
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
    w = U("backreduce");
    string_to_code[w] = l_backreduce;
    w = U"-\\\\";
    string_to_code[w] = l_backscan;
    w = U("backscan");
    string_to_code[w] = l_backscan;
    w = U("iota");
    string_to_code[w] = l_iota;
    w = U("iota0");
    string_to_code[w] = l_iota0;
    
    w = U("rho");
    string_to_code[w] = l_rho;
    
    w = U("transpose");
    string_to_code[w] = l_transpose;
    w = U("reverse");
    string_to_code[w] = l_reverse;
    w = U("not");
    string_to_code[w] = l_not;
    w = U("sum");
    string_to_code[w] = l_sum;
    w = U("product");
    string_to_code[w] = l_product;
    w = U("invert");
    string_to_code[w] = l_invert;
    w = U("rank");
    string_to_code[w] = l_rank;
    
    w = U("member");
    string_to_code[w] = l_member;
    
    //Small tip, to avoid problems
    // indeed, the instruction cadr is already linked to its own code
    e = new Cadr("cadr");
    e->status = s_constant;
    atom_pool[l_cadr] = e;
    // We create all our atoms and keep them in the stack
    binHashe<string>::iterator its(instructions);
    for (; !its.end(); its++) {
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
#ifndef LISPE_WASM
    moduleSocket(lisp);
#endif
    moduleOntology(lisp);
#ifdef FLTKGUI
    moduleGUI(lisp);
#endif
    
    thread_stack.function = null_;
    
    atom_basic_pool.set(0, atom_pool.tsize, atom_pool.indexes);
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
        delegation->thread_stack.clear();
        binHash<Element*>::iterator a;
        for (long i = 0; i < delegation->bodies.size(); i++) {
            for (a.set(delegation->bodies[i]); !a.end(); a++) {
                a->second->decrement();
            }
        }
#ifdef LISPE_WASM
        //In this case, errors are no longer treated as exceptions.
        //We need to clean if an error still lurks around
        delegation->reset_error();
#endif
    }
    //Then if some of them are still running
    //we wait for their termination
    while (nbjoined) {}
    clearStack();
    clean_compositions.clean();
    for (long i = 0; i < garbages.size(); i++)
        delete garbages[i];
    garbages.clear();
    return_pool.cleaning();
    string_pool.cleaning();
    float_pool.cleaning();
    number_pool.cleaning();
    integer_pool.cleaning();
    complex_pool.cleaning();
    rangenumber_pool.cleaning();
    rangeinteger_pool.cleaning();
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
            cerr << "Leaks: " << errors.size() << endl;
#endif
    }
}

LispE::LispE(LispE* lisp, List* function, List* body) : segmenter(lisp->handlingutf8){
    max_size = 25;
    larger_max_size = 100;
    initpoolsize();
	current_path_set = true;
    depth_stack = 0;
    current_space = 0;
    void_function = lisp->void_function;
    preparingthread = false;
    check_arity_on_fly = false;
    delegation = lisp->delegation;
    
    check_thread_stack = false;
    _BOOLEANS[0][0] = delegation->_NULL;
    _BOOLEANS[0][1] = delegation->_TRUE;
    _BOOLEANS[1][0] = delegation->_ZERO;
    _BOOLEANS[1][1] = delegation->_ONE;
    
    select_bool_as_one = false;
    
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
    n_emptylist = delegation->_EMPTYLIST;
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
//we check cadr as a token. It requires a specific processing
//and it must be identified beforehand
static inline bool checkcadr(u_ustring& l, long sz) {
    if (sz > 3 && l[0] == 'c' && l.back() == 'r') {
        sz--;
        for (short i = 1; i < sz; i++) {
            if (l[i] != 'a' && l[i] != 'd')
                return false;
        }
        return true;
    }
    return false;
}

lisp_code LispE::segmenting(u_ustring& code, Tokenizer& infos) {
    u_ustring buffer;
    long line_number = 1;
    long nb_parentheses = 0;
    long nb_braces = 0;
    long nb_brackets = 0;
    long culprit;
    
    int16_t lc = 0;
    int in_quote = 0;
    double d;
    long lg_value = 0;
    long left = 0;
    long right = 0;
    if (delegation->add_to_listing) {
        culprit = 0;
        string s;
        while (lg_value != string::npos) {
            culprit = code.find(U"\n", lg_value);
            if (culprit != string::npos) {
                buffer = code.substr(lg_value, culprit - lg_value + 1);
                if (buffer != U"") {
                    s = "";
                    s_unicode_to_utf8(s, buffer);
                    add_to_listing(line_number, s);
                }
                lg_value = culprit + 1;
                line_number++;
            }
            else
                break;
        }
        
        buffer = code.substr(lg_value, code.size() - lg_value);
        if (buffer != U"") {
            s = "";
            s_unicode_to_utf8(s, buffer);
            add_to_listing(line_number, s);
        }
        
        line_number = 1;
    }
    
    //We then tokenize our code
    //r_parse.stack contains the substrings
    //r_parse.stacktype contains their type
    //r_parse.stackln contains their line number
    long sz = delegation->main_tokenizer.tokenize<u_ustring>(code, r_parse);
    
#ifdef MACDEBUG
    //for debugging
    vector<u_ustring> codes;
    r_parse.stack.to_vector(codes);
    vector<int16_t> icodes;
    r_parse.stacktype.to_vector(icodes);
#endif
            
    culprit = -1;
    long i;
    long ipos = 0;
    
    for (i = 0; i < sz; i++) {
        line_number = r_parse.stackln[i] + 1;
        left = r_parse.positions[ipos++];
        right = r_parse.positions[ipos++];
        
        buffer = r_parse.stack[i];
        //Note that the code in lc are also characters
        //that mimic the type of the element that was identified
        //They do not always correspond to a specific character in the buffer
        lc = r_parse.stacktype[i];
        switch (lc) {
            case '\n': //Carriage return
                continue;
            case '"': //a string
                buffer = buffer.substr(1, buffer.size() - 2);
                lg_value = buffer.find(U"\\");
                if (lg_value != -1) {
                    u_ustring intermediate = buffer.substr(0, lg_value);
                    for (long j = lg_value; j < buffer.size(); j++) {
                        if (buffer[j] == '\\') {
                            switch (buffer[j + 1]) {
                                case 'n':
                                    intermediate += U"\n";
                                    j++;
                                    break;
                                case 'r':
                                    intermediate += U"\r";
                                    j++;
                                    break;
                                case 't':
                                    intermediate += U"\t";
                                    j++;
                                    break;
                                default:
                                    intermediate += buffer[j + 1];
                                    j++;
                            }
                            continue;
                        }
                        intermediate += buffer[j];
                    }
                    buffer = intermediate;
                }
                if (buffer == U"")
                    infos.append(buffer, t_emptystring, line_number, left, right);
                else
                    infos.append(buffer, t_string, line_number, left, right);
                if (in_quote == 1) in_quote = 0;
                break;
            case '`': //a long string
                buffer = buffer.substr(1, buffer.size() - 2);
                if (buffer == U"")
                    infos.append(buffer, t_emptystring, line_number, left, right);
                else
                    infos.append(buffer, t_string, line_number, left, right);
                if (in_quote == 1) in_quote = 0;
                break;
            case '\'': //a quote
                infos.append(buffer, l_quote, line_number, left, right);
                if (!in_quote && !infos.asList)
                    in_quote = 1;
                break;
            case ';': {//a quoted ;, we have a specific rule for this character to handle potential comments
                u_ustring q = U"'";
                infos.append(q, l_quote, line_number, left, left + 1);
                buffer = buffer.substr(1, buffer.size());
                infos.append(buffer, t_atom, line_number, left + 1, right);
                break;
            }
            case '9': //a float: contains a '.'
                lg_value = buffer.find(U",");
                if (lg_value != -1) {
                    //This is a complex number: ddd,ddi
                    u_ustring real = buffer.substr(0, lg_value);
                    u_ustring imaginary = buffer.substr(lg_value+1, buffer.size() - real.size() - 2);
                    d = convertingfloathexa(real);
                    infos.append(d, real, t_complex, line_number, left, right);
                    if (imaginary == U"" || imaginary == U"+")
                        d = 1;
                    else
                        if (imaginary == U"-")
                            d = -1;
                    else
                        d = convertingfloathexa(imaginary);
                    infos.append(d, imaginary, t_complex, line_number, left, right);
                }
                else {
                    if (buffer.find(U".") == -1) {
                        lg_value = convertinginteger(buffer);
                        infos.append(lg_value, buffer, t_integer, line_number, left, right);
                    }
                    else {
                        d = convertingfloathexa((u_uchar*)buffer.c_str(), lg_value);
                        infos.append(d, buffer, t_number, line_number, left, right);
                    }
                }
                if (in_quote == 1) in_quote = 0;
                break;
            case '?': //operators and comparators
                lc = delegation->is_atom(buffer);
                if (lc >= l_minus_plus && lc <= l_modequal)
                    infos.append(buffer, t_operator, line_number, left, right);
                else
                    infos.append(buffer, t_atom, line_number, left, right);
                if (in_quote) in_quote--;
                break;
            case 'A': // a simple token
                if (checkcadr(buffer, buffer.size()))
                    infos.append(buffer, l_cadr, line_number, left, right);
                else
                    infos.append(buffer, t_atom, line_number, left, right);
                if (in_quote == 1) in_quote = 0;
                break;
            case '.':
                if (in_quote == 1) {
                    in_quote = 0;
                    infos.append(buffer, t_atom, line_number, left, right);
                }
                else
                    infos.append(buffer, c_point, line_number, left, right);
                break;
            case ':':
                if (in_quote == 1) {
                    in_quote = 0;
                    infos.append(buffer, t_atom, line_number, left, right);
                }
                else {
                    infos.append(buffer, c_colon, line_number, left, right);
                }
                break;
            case '(': // (
                nb_parentheses++;
                infos.append(buffer, c_opening, line_number, left, right);
                if (in_quote) in_quote++;
                break;
            case ')': // )
                nb_parentheses--;
                if (nb_parentheses <= 0) {
                    if (culprit == -1)
                        culprit = line_number;
                }
                infos.append(buffer, c_closing, line_number, left, right);
                if (in_quote < 3)
                    in_quote = 0;
                else
                    in_quote--;
                break;
            case '[': // [
                nb_brackets++;
                buffer = U'(';
                infos.append(buffer, c_opening, line_number, left, right);
                if (in_quote == 1) in_quote = 0;
                break;
            case ']': // ]
                buffer = U')';
                if (nb_brackets) {
                    nb_brackets--;
                    infos.append(buffer, c_closing, line_number, left, right);
                }
                else {
                    nb_parentheses--;
                    if (nb_parentheses <= 0) {
                        if (culprit == -1)
                            culprit = line_number;
                    }
                    //We add any number of parentheses to close the gap
                    for (long e = 0; e < nb_parentheses; e++)
                        infos.append(buffer, c_closing, line_number, left, right);
                    nb_parentheses = 1;
                }
                if (in_quote < 3)
                    in_quote = 0;
                else
                    in_quote--;
                break;
            case '{': // {
                if (in_quote == 1) {
                    in_quote = 0;
                    infos.append(buffer, t_atom, line_number, left, right);
                }
                else {
                    nb_braces++;
                    infos.append(buffer, c_opening_brace, line_number, left, right);
                }
                break;
            case '}': // }
                if (in_quote == 1) {
                    in_quote = 0;
                    infos.append(buffer, t_atom, line_number, left, right);
                }
                else {
                    nb_braces--;
                    if (nb_braces < 0) {
                        if (culprit == -1)
                            culprit = line_number;
                    }
                    infos.append(buffer, c_closing_brace, line_number, left, right);
                }
                break;
            default:
                infos.append(buffer, t_atom, line_number, left, right);
                if (in_quote == 1) in_quote = 0;
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
    return e_no_error;
}

lisp_code LispE::segmenting(string& s_code, Tokenizer& infos) {
    u_ustring code;
    s_utf8_to_unicode(code, s_code, s_code.size());
    if (delegation->add_to_listing) {
        //since we already have a string version of code
        //no need to convert it back in segmenting(u_ustring&..) above.
        size_t found = 0;
        string localvalue;
        string s;
        long pos = 0;
        long line_number = 0;
        while (pos != string::npos) {
            found = s_code.find("\n", pos);
            if (found != string::npos) {
                localvalue = s_code.substr(pos, found - pos);
                if (localvalue != "") {
                    add_to_listing(line_number, localvalue);
                }
                pos = found + 1;
                line_number++;
            }
            else
                break;
        }
        
        localvalue = s_code.substr(pos, code.size() - pos);
        if (localvalue != "") {
            add_to_listing(line_number, localvalue);
        }
        //We temporary deactivate add_to_listing
        delegation->add_to_listing = false;
        //when calling segmenting again.
        lisp_code lc = segmenting(code, infos);
        delegation->add_to_listing = true;
        return lc;
    }
    return segmenting(code, infos);
}

//We use the segmenter defined in LispE. Note that it records the last call flags
//If they are different, then the system recompiles the rules to take these flags into account
Element* LispE::tokenize(u_ustring& code, bool keepblanks, short decimalseparator) {
    if (!segmenter.loaded) {
        segmenter.setrules();
        segmenter.compile();
    }
    
    segmenter.keepblanks(keepblanks);
    segmenter.setdecimalmode(decimalseparator);
    
    Strings* res = provideStrings();
    tokenizer_result<u_ustring> tokres(&res->liste, false);
    
    segmenter.tokenize<u_ustring>(code, tokres);
    return res;
}

Element* LispE::tokenize(wstring& code, bool keepblanks) {
    Strings* res = provideStrings();
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
                    res->append(tampon);
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
                res->append(tampon);
                break;
            case '.':
                if (code[i + 1] == ' ') {
                    tampon = c;
                    res->append(tampon);
                    break;
                }
            case '+':
            case '-':
                if (!isdigit(code[i+1])) {
                    tampon = c;
                    res->append(tampon);
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
                res->append(tampon);
                i += idx - 1;
                break;
            }
            default: {
                idx = i;
                if (!handlingutf8->is_a_valid_letter(code, idx))  {
                    if (idx == i) {
                        tampon = c;
                        res->append(tampon);
                    }
                    else {
                        tampon = code.substr(i, idx - i + 1);
                        res->append(tampon);
                        i = idx;
                    }
                    break;
                }
                idx++;
                while (idx < sz && handlingutf8->is_a_valid_letter(code, idx)) idx++;
                tampon = code.substr(i, idx - i);
                i = idx-1;
                res->append(tampon);
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
Element* LispE::abstractSyntaxTree(Element* current_program, Tokenizer& parse, long& index, long quoting) {
   Element* element = NULL;
   int16_t lab = -1;
   Element* check_composition_depth = NULL;

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
               element = delegation->_EMPTYLIST;
            }
            else {
               uint16_t currentspace = current_space;
               bool equal_op_list = false;

               //If we are dealing with two compositional elements (map, filter, take etc...)
               //which are at the same level, then we must first compose our composition stack
               if (check_composition_depth == current_program && composition_stack.size())
                  compose(current_program);

               check_composition_depth = NULL;
               element = new Listincode(parse.lines[index], delegation->i_current_file);
               garbaging(element);
               abstractSyntaxTree(element, parse, index, quoting);
               if (quoting) {
                  if (element->size() && element->index(0)->label() == l_conspoint) {
                     Element* a = element->eval(this);
                     garbaging(a);
                     removefromgarbage(element);
                     current_program->append(a);
                     quoting--;
                     continue;
                  }
               }
               else {
                  if (element->size() >= 1) {
                     lab = element->index(0)->label();

                     //if it is a high level function, then we push it into composition_stack
                     //otherwise, we compose the expressions in composition_stack,
                     //beforehand.
                     if (composition_stack.size() && (lab < l_map || lab > l_scanr1))
                        compose(element);

                     switch(lab) {
                        //for defmacro and link, we evaluate these expressions on the fly
                        case l_defspace:
                           current_space = currentspace;
                        continue;
                     case l_set_const:
                        case l_lambda:
                        element->eval(this);
                        break;
                        case t_call_lambda: {
                           //This is a lambda call with arguments
                           Element* lm = new List_call_lambda(this, (Listincode*)element);
                           removefromgarbage(element);
                           garbaging(lm);
                           element = lm;
                           break;
                        }
                        case l_defmacro:
                        case l_data:
                        case l_dethread:
                        case l_defun:
                        element->eval(this);
                        continue;
                        case l_defpat: {
                           Element* arguments = element->index(2);
                           Element* a;
                           Element* idx;
                           for (long i = 0; i < arguments->size(); i++) {
                              idx = arguments->index(i);
                              a = idx->transformargument(this);
                              if (a != idx)
                                 ((List*)arguments)->liste.put(i, a);
                           }
                           element->eval(this);
                           continue;
                        }
                        case l_let:
                        case l_set_range:
                        case l_set_shape:
                        case l_setq:
                        case l_seth:
                        case l_set_at:
                        case l_setg:
                           if (element->size() > 1) {
                           if (element->index(1)->label() < l_final) {
                              wstring msg = L"Error: Invalid variable name: '";
                              msg += element->index(1)->asString(this);
                              msg += L"' (keyword)";
                              return new Error(msg);
                           }
                           else {
                              if (delegation->const_values.check(element->index(1)->label())) {
                                 wstring msg = L"Error: '";
                                 msg += element->index(1)->asString(this);
                                 msg += L"' is a constant value";
                                 return new Error(msg);
                              }
                           }
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
                           if (element->size() > 1) {
                           Element* nxt = element->index(1);
                           if (nxt->label() < l_final) {
                              if (nxt->isList()) {
                                 if (nxt->size() < 2 || nxt->index(0)->label() != l_at) {
                                    wstring msg = L"Error: Expecting an index access with 'at'";
                                    return new Error(msg);
                                 }
                                 equal_op_list = true;
                              }
                              else {
                                 wstring msg = L"Error: Invalid variable name: '";
                                 msg += element->index(1)->asString(this);
                                 msg += L"' (keyword)";
                                 return new Error(msg);
                              }
                           }
                           else {
                              if (delegation->const_values.check(element->index(1)->label())) {
                                 wstring msg = L"Error: '";
                                 msg += element->index(1)->asString(this);
                                 msg += L"' is a constant value";
                                 return new Error(msg);
                              }
                           }
                        }
                        break;
                        case l_link:
                           element->eval(this);
                        removefromgarbage(element);
                        continue;
                     case l_if:
                           if (element->size() == 3)
                              element->append(void_function);
                           break;
                        case l_infix: {
                           Element* inter = ((Listincode*)element)->eval_infix(this);
                           if (inter != element) {
                              removefromgarbage(element);
                              element = inter;
                              lab = element->index(0)->label();
                           }
                           else
                              break;
                        }
                        default: {
                           if (lab >= l_map && lab <= l_scanr1) {
                              if (composition_stack.size() && (composition_stack.back()->label() == t_countertake || composition_stack.back()->label() == t_counterdrop)) {
                                 List lst;
                                 compose(&lst);
                                 composition_stack.push_back(lst.liste[0]);
                              }
                              element = element->eval(this);
                              check_composition_depth = current_program;
                              continue;
                           }
                        }
                     }
                  }

                  element = generate_macro(element);

                  if (lab > l_final) {
                     if (parse.defun_functions.check(lab)) {
                        Element* body = parse.defun_functions[lab];
                        //This is a call to a function: t_atom, a1, a2...
                        if (body->index(0)->label() == l_defun)
                           body = new List_function_eval(this, (Listincode*)element, (List*)body);
                        else
                           body = new List_pattern_eval((Listincode*)element, (List*)body);

                        garbaging(body);
                        removefromgarbage(element);
                        element = body;
                     }
                     else {
                        if (delegation->function_pool[current_space]->check(lab)) {
                           Element* body = (*delegation->function_pool[current_space])[lab];
                           if (body->index(0)->label() == l_deflib) {
                              body = new List_library_eval((Listincode*)element, (List*)body);
                              garbaging(body);
                              removefromgarbage(element);
                              element = body;
                           }
                        }
                     }
                  }
                  /*
                        We detect if it is an instruction beforehand, in order
                        to limit the call to lisp->delegation->evals during the execution (see Listincode::eval)
                        - List_basic_instruction is used for instructions that do not fail, which means that we do not need to record
                        their position and even trace them back.
                        - List_instruction on the other hand will set the position of the current instruction.
                        */
                  if (delegation->instructions.check(lab)) {
                     Element* lm = NULL;
                     long nbarguments = element->size();
                     switch (lab) {
                        case l_break:
                           if (nbarguments != 1)
                              return new Error("Error: break does not take any arguments");
                           removefromgarbage(element);
                        element = &delegation->_BREAKEVAL;
                        break;
                        case l_return:
                           if (element->size() == 1)
                              lm = new Listreturn();
                           else
                              lm = new Listreturnelement((Listincode*)element);
                        break;
                        case l_switch:
                           lm = new List_switch_eval((Listincode*)element);
                        ((List_switch_eval*)lm)->build(this);
                        break;
                     case l_power:
                           if (nbarguments == 3 && element->index(2)->equalvalue((long)2))
                              lm = new List_power2((List*)element);
                           else
                              lm = new List_execute((Listincode*)element, delegation->evals[lab]);
                        break;
                        case l_divide:
                           if (nbarguments == 2)
                              lm = new List_divide2((List*)element);
                           else
                              if (nbarguments == 3)
                              lm = new List_divide3((List*)element);
                           else
                              lm = new List_dividen((List*)element);
                        break;
                        case l_plus:
                           if (nbarguments == 2)
                              lm = new List_plus2((List*)element);
                           else
                              if (nbarguments == 3)
                              lm = new List_plus3((List*)element);
                           else
                              lm = new List_plusn((List*)element);
                        break;
                        case l_minus:
                           if (nbarguments == 2)
                              lm = new List_minus2((List*)element);
                           else
                              if (nbarguments == 3)
                              lm = new List_minus3((List*)element);
                           else
                              lm = new List_minusn((List*)element);
                        break;
                        case l_multiply:
                           if (nbarguments == 2)
                              lm = new List_multiply2((List*)element);
                           else
                              if (nbarguments == 3)
                              lm = new List_multiply3((List*)element);
                           else
                              lm = new List_multiplyn((List*)element);
                        break;
                        case l_divideequal:
                           if (equal_op_list)
                              lm = new List_divideequal_list((List*)element);
                           else
                              lm = new List_divideequal_var((List*)element);
                        break;
                        case l_plusequal:
                           if (equal_op_list)
                              lm = new List_plusequal_list((List*)element);
                           else
                              lm = new List_plusequal_var((List*)element);
                        break;
                        case l_minusequal:
                           if (equal_op_list)
                              lm = new List_minusequal_list((List*)element);
                           else
                              lm = new List_minusequal_var((List*)element);
                        break;
                        case l_multiplyequal:
                           if (equal_op_list)
                              lm = new List_multiplyequal_list((List*)element);
                           else
                              lm = new List_multiplyequal_var((List*)element);
                        break;
                        case l_maplist:
                           if (element->index(1)->isLambda())
                              lm = new List_maplist_lambda_eval((Listincode*)element);
                           else
                              lm = new List_maplist_eval((Listincode*)element);
                        break;
                        case l_zipwith:
                           if (element->index(1)->isLambda())
                              lm = new List_zipwith_lambda_eval((Listincode*)element);
                           else
                              lm = new List_zipwith_eval((Listincode*)element);
                        break;
                        default:
                           lm = cloning((Listincode*)element, lab);                                    
                     }

                     if (lm != NULL) {
                        garbaging(lm);
                        if (!delegation->checkArity(lab, nbarguments)) {
                           wstring err = L"Error: Wrong number of arguments for: '";
                           err += delegation->asString(lab);
                           err += L"'";
                           return new Error(err);
                        }
                        removefromgarbage(element);
                        element = lm;
                     }
                  }
               }
            }
            if (element->size() || element->isComposable())
               current_program->append(element);
            else {
               current_program->append(delegation->_EMPTYLIST);
               removefromgarbage(element);
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
               element = delegation->_EMPTYDICTIONARY;
            }
            else {
               Dictionary_as_list dico;
               abstractSyntaxTree(&dico, parse, index, quoting);
               element = dico.dictionary(this);
               garbaging(element);
               if (element->isList()) {
                  //Then in that case, we need to provide a protection
                  //for each of its elements...
                  for (long i = 0; i < element->size(); i++)
                  control_garbaging(element->index(i));
               }
            }
            current_program->append(element);
            break;
         }
         case c_opening_data_brace: {
            index++;
            if (parse.types[index] == c_closing_brace) {
               index++;
               element = delegation->_EMPTYDICTIONARY;
            }
            else {
               Dictionary_as_buffer dico(this);
               abstractSyntaxTree(&dico, parse, index, quoting);
               element = dico.dictionary(this);
               garbaging(element);
            }
            current_program->append(element);
            break;
         }
         case c_closing_brace:
            index++;
         return delegation->_TRUE;
      case t_emptystring:
         current_program->append(delegation->_EMPTYSTRING);
         index++;
         break;
         case t_string:
         element = provideConststring(parse.tokens[index]);
         current_program->append(element);
         index++;
         break;
         case t_complex: {
            //We need to extract it twice
            double real = parse.numbers[index++];
            double imag = parse.numbers[index++];
            element = provideComplex(real, imag);
            garbaging(element);
            current_program->append(element);
            break;
         }
         case t_integer: {
            long value = parse.integers[index];
            element = provideConstinteger(value);
            current_program->append(element);
            index++;
            break;
         }
         case t_number: {
            double value = parse.numbers[index];
            element = provideConstnumber(value);
            current_program->append(element);
            index++;
            break;
         }
         case t_operator:
            element = provideOperator(encode(parse.tokens[index]));
         current_program->append(element);
         index++;
         break;
      case l_cadr:
         element =  provideCADR(parse.tokens[index]);
         current_program->append(element);
         index++;
         break;
         case c_colon:
            if (current_program->label() == t_dictionary) {
            current_program->reversechoice();
            index++;
            if (parse.types[index] == c_colon)
               return new Error("Error: wrong key/value separator in a dictionary");
         }
         else {
            element = provideAtom(c_colon);
            index++;
            current_program->append(element);
         }
         break;
         case c_point:
            if (quoting) {
            index++;
            if (current_program->size() == 0)
               return new Error("Error: Wrong use of '.'");
            if (parse.types[index] == c_opening) {
               index++;
               syntaxTree(current_program, parse, index, quoting);
            }
            else {
               syntaxTree(current_program, parse, index, true);
               ((List*)current_program)->liste.insert(0, provideAtom(l_conspoint));
            }
         }
         else {
            element = provideAtom(l_innerproduct);
            index++;
            current_program->append(element);
         }
         break;
         case t_atom:
            element = provideAtom(encode(parse.tokens[index]));
         index++;
         if (!quoting && element->label() == l_quote) {
            current_program->append(element);
            abstractSyntaxTree(current_program, parse, index, true);
         }
         else {
            if (!quoting) {
               if (current_program->size()) {
                  if (current_program->size() == 1 && current_program->index(0)->label() == l_defspace) {
                     //We create a new name space in function_pool
                     current_program->append(element);
                     current_program->eval(this);
                     break;
                  }
               }
            }
            if (current_program->size() == 1 &&
                  (current_program->index(0)->label() == l_defun ||
                  current_program->index(0)->label() == l_defpat)) {
               //We are defining a function, we can record it now...
               parse.defun_functions[element->label()] = current_program;
            }
            current_program->append(element);
         }
         break;
         case l_quote:
            element = new List_quote_eval(parse.lines[index], delegation->i_current_file);
         index++;
         garbaging(element);
         element->append(provideAtom(l_quote));
         current_program->append(element);
         abstractSyntaxTree(element, parse, index, true);
         if (element->size() != 2)
               return new Error("Error: Wrong number of arguments for 'quote'");
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
         case t_integer:
         value = parse.numbers[index];
         e = provideConstinteger(value);
         courant->append(e);
         index++;
         break;
         case t_number:
         value = parse.numbers[index];
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
               return new Error("Error: wrong key/value separator in a dictionary");
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
               return new Error("Error: Wrong use of '.'");
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
         else
            courant->append(e);
         break;
         case l_quote:
            e = provideList();
         e->append(provideAtom(l_quote));
         courant->append(e);
         index++;
         syntaxTree(e, parse, index, true);
         if (e->size() != 2)
               return new Error("Error: Wrong number of arguments for 'quote'");
            break;
         default:
            index++;
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

List* LispE::create_instruction(int16_t label,Element* e1,Element* e2,Element* e3) {
    List* l;
    
    switch (label) {
        case l_block:
            l = new List_block_eval();
            break;
        case l_loop:
            l = new List_loop_eval();
            break;
        case l_check:
            l = new List_check_eval();
            break;
        case l_ncheck:
            l = new List_ncheck_eval();
            break;
        case l_if:
            l = new List_if_eval();
            break;
        case l_ife:
            l = new List_ife_eval();
            break;
        case l_not:
            l = new List_not_eval();
            break;
        case l_setq:
            l = new List_setq_eval();
            break;
        case l_push:
            l = new List_push_eval();
            break;
        case l_car:
            l = new List_car_eval();
            break;
        case l_cdr:
            l = new List_cdr_eval();
            break;
        default:
            l = provideList();
    }
    
    garbaging(l);
    l->append(provideAtom(label));
    if (!l->append_not_null(e1))
        return l;
    if (!l->append_not_null(e2))
        return l;
    if (!l->append_not_null(e3))
        return l;
    return l;
}

List* LispE::create_local_instruction(int16_t label,Element* e1,Element* e2,Element* e3) {
    List* l;
    
    switch (label) {
        case l_block:
            l = new List_block_eval();
            break;
        case l_loop:
            l = new List_loop_eval();
            break;
        case l_check:
            l = new List_check_eval();
            break;
        case l_ncheck:
            l = new List_ncheck_eval();
            break;
        case l_if:
            l = new List_if_eval();
            break;
        case l_ife:
            l = new List_ife_eval();
            break;
        case l_not:
            l = new List_not_eval();
            break;
        case l_setq:
            l = new List_setq_eval();
            break;
        case l_push:
            l = new List_push_eval();
            break;
        case l_car:
            l = new List_car_eval();
            break;
        case l_cdr:
            l = new List_cdr_eval();
            break;
        default:
            l = provideList();
    }
    l->append(provideAtom(label));
    if (!l->append_not_null(e1))
        return l;
    if (!l->append_not_null(e2))
        return l;
    if (!l->append_not_null(e3))
        return l;
    return l;
}

Element* LispE::load(string pathname) {
   pathname = NormalizePathname(pathname);
   if (delegation->allfiles.count(pathname)) {
      delegation->i_current_file = delegation->allfiles[pathname];
      const auto& a = delegation->entrypoints.find(delegation->i_current_file);
      if (a != delegation->entrypoints.end())
         return a->second;
   }
   delegation->i_current_line = 0;
   std::ifstream f(pathname.c_str(),std::ios::in|std::ios::binary);
   if (f.fail()) {
      string err = "Unknown file: ";
      err += pathname;
      return new Error(err);
   }
   string code;
   string ln;
   while (!f.eof()) {
      getline(f, ln);
      code += ln + "\n";
   }
   delegation->updatepathname(pathname);
   delegation->entrypoints[delegation->i_current_file] = delegation->_NULL;

   Element* tree = compile_lisp_code(code);
   delegation->entrypoints[delegation->i_current_file] = tree;
   current_path();
   delegation->reset_context();
   depth_stack = 0;
   return tree->eval(this);

   
}


Element* LispE::compile_eval(u_ustring& code) {

   Element* tree = compile_lisp_code(code);
   depth_stack = 0;
   return tree->eval(this);

   
}


Element* LispE::compile_lisp_code(u_ustring& code) {
   clearStop();

   #ifdef LISPE_WASM
   //In this case, errors are no longer treated as exceptions.
   delegation->reset_error();
   #endif

   //A little trick to compile code sequences
   code = U"(__root__ " + code + U")";
   Tokenizer parse;
   lisp_code retour = segmenting(code, parse);

   List courant;
   long index;
   switch (retour) {
      case e_error_brace:
         return new Error("Error: braces do not balance");
   case e_error_bracket:
      return new Error("Error: brackets do not balance");
      case e_error_parenthesis:
      return new Error("Error: parentheses do not balance");
      case e_error_string:
      delegation->i_current_line = 1;
         if (parse.current != -1 && parse.current < parse.lines.size())
            delegation->i_current_line = parse.lines[parse.current];
         else
            delegation->i_current_line = parse.lines.back();
      return new Error("Error: missing end of string");
      default:
         index = 0;
   }

   abstractSyntaxTree(&courant, parse, index, false);

   if (delegation->current_error) {
      delegation->i_current_line = 1;
      if (parse.current != -1 && parse.current < parse.lines.size())
         delegation->i_current_line = parse.lines[parse.current];
      else
         if (parse.lines.size())
         delegation->i_current_line = parse.lines.back();
      delegation->forceClean();
      return delegation->set_error(delegation->current_error);
   }
   Element* e = courant.liste[0];

   if (e->size() == 2) {
      //The block contains only one element that can be evaluated immediately
      e = e->index(1);
   }

   if (!checkvariable(l_code))
      recordingunique(e, l_code);
   return e;
}


Element* LispE::compile_lisp_code(string& code) {
    u_ustring wcode;
    s_utf8_to_unicode(wcode, code, code.size());
    return compile_lisp_code(wcode);
}

//This method allows us to add new features in LispE in the form of
//loading of external libraries. See system.cxx for an example.
Element* LispE::extension(string code, Element* etendre) {
   Listincode* current_list = NULL;
   Tokenizer parse;
   u_ustring wcode;
   s_utf8_to_unicode(wcode, code, code.size());
   lisp_code retour = segmenting(wcode, parse);
   switch (retour) {
      case e_error_brace:
         etendre->release();
      return new Error("Error: braces do not balance");
   case e_error_bracket:
      etendre->release();
      return new Error("Error: brackets do not balance");
      case e_error_parenthesis:
      etendre->release();
      return new Error("Error: parentheses do not balance");
      case e_error_string:
      etendre->release();
      delegation->i_current_line = 1;
         if (parse.current != -1 && parse.current < parse.lines.size())
            delegation->i_current_line = parse.lines[parse.current];
         else
            if (parse.lines.size())
            delegation->i_current_line = parse.lines.back();
         return new Error("Error: missing end of string");
      default:
         current_list = new Listincode;
      garbaging(current_list);
   }

   long index = 0;
   abstractSyntaxTree(current_list, parse, index, false);
   Element* body = current_list->eval(this);
   if (etendre != NULL) {
      body->append(etendre);
      garbaging(etendre);
   }
   return body;

   
}


Element* LispE::execute(string code) {
   delegation->i_current_line = 0;
   delegation->i_current_file = 0;
   clearStop();

   return compile_lisp_code(code)->eval(this);

   
}


Element* LispE::execute(string code, string pathname) {
   delegation->i_current_line = 0;
   delegation->i_current_file = 0;
   clearStop();
   if (pathname != "") {
      pathname = NormalizePathname(pathname);
      delegation->updatepathname(pathname);
   }

   Element* tree = compile_lisp_code(code);
   delegation->entrypoints[delegation->i_current_file] = tree;
   current_path();
   delegation->reset_context();
   depth_stack = 0;
   return tree->eval(this);

   
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
         return new Error("Error: parameter size does not match argument");
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
    s_utf8_to_unicode(s, str, str.size());
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
	if (current_path_set)
		return;
    u_ustring nom = U"_current";
    string spath;
    Element* e;
    
    if (delegation->allfiles.size() >= 2) {
        spath = delegation->allfiles_names[1];
        char localpath[4096];
        localpath[0] = 0;
#ifdef WIN32
        _fullpath(localpath, STR(spath), 4096);
#else
        realpath(STR(spath), localpath);
#endif
#ifdef WIN32
		string end_path = "\\";
#else
		string end_path = "/";
#endif
		long pos = spath.rfind(end_path);
		if (pos == string::npos) {
			spath = localpath;
			if (spath.back() != end_path[0])
                spath += end_path;
		}
		else
			spath = spath.substr(0, pos + 1);
        e = provideString(spath);
        execution_stack.back()->storing_variable(e, encode(nom));
        e->release();
    }
    
    nom = U"_version";
    spath = LispVersion();
    e = provideString(spath);
    execution_stack.back()->storing_variable(e, encode(nom));
    e->release();
	current_path_set = true;
}