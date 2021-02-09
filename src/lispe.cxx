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
static std::string version = "1.2021.2.9.18.4";
string LispVersion() {
    return version;
}

//------------------------------------------------------------
wstring Stackelement::asString(LispE* lisp) {
    std::wstringstream message;
    for (auto& a: variables)
        message << lisp->asString(a.first) << L": " << a.second->stringInList(lisp) << endl;
    return message.str();
}

List* Stackelement::atomes(LispE* lisp) {
    List* liste = new List;
    for (auto& a: variables)
        liste->append(lisp->provideAtom(a.first));
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

Delegation::Delegation() {
    input_handler = get_jag_handler();
    reading_string_function = &lispe_readfromkeyboard;
    display_string_function = &lispe_displaystring;
    reading_string_function_object = input_handler;
    
    endtrace = false;
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
    for (auto& a : thread_pool)
        a.second.clear();
    
    for (auto& a: locks)
        delete a.second;
    
    for (auto& a: atom_pool)
        delete a.second;
    
    for (auto& a: waitons)
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
    evals[t_list] = &List::evalt_list;

    // Here is the predefined list of instructions, with their name and arity
    //Important the instruction is counted into the arity. Hence (consp e) is P_TWO.
    
    //_max_stack_size can be enabled but represents a potential hazard as
    //too high a value can cause the whole interpreter to crash...
    //By default it is disabled...
#ifdef MAX_STACK_SIZE_ENABLED
    set_instruction(l_set_max_stack_size, "_max_stack_size", P_ONE | P_TWO, &List::evall_set_max_stack_size);
#endif
    
    set_instruction(l_and, "and", P_ATLEASTTHREE, &List::evall_and);
    set_instruction(l_apply, "apply", P_THREE, &List::evall_apply);
    set_instruction(l_atomise, "explode", P_TWO, &List::evall_atomise);
    set_instruction(l_atomp, "atomp", P_TWO, &List::evall_atomp);
    set_instruction(l_atoms, "atoms", P_ONE, &List::evall_atoms);
    set_instruction(l_bitand, "&", P_ATLEASTTHREE, &List::evall_bitand);
    set_instruction(l_bitandequal, "&=", P_ATLEASTTHREE, &List::evall_bitandequal);
    set_instruction(l_bitor, "|", P_ATLEASTTHREE, &List::evall_bitor);
    set_instruction(l_bitorequal, "|=", P_ATLEASTTHREE, &List::evall_bitorequal);
    set_instruction(l_bitxor, "^", P_ATLEASTTHREE, &List::evall_bitxor);
    set_instruction(l_bitxorequal, "^=", P_ATLEASTTHREE, &List::evall_bitxorequal);
    set_instruction(l_block, "block", P_ATLEASTONE, &List::evall_block);
    set_instruction(l_break, "break", P_ONE, &List::evall_break);
    set_instruction(l_cadr, "cadr", P_TWO, &List::evall_cadr);
    set_instruction(l_car, "car", P_TWO, &List::evall_car);
    set_instruction(l_catch, "catch", P_TWO, &List::evall_catch);
    set_instruction(l_cdr, "cdr", P_TWO, &List::evall_cdr);
    set_instruction(l_check, "check", P_ATLEASTTWO, &List::evall_check);
    set_instruction(l_checking, "#checking", P_FOUR, &List::evall_checking);
    set_instruction(l_compose, "#compose", P_ATLEASTFOUR, &List::evall_compose);
    set_instruction(l_cond, "cond", P_ATLEASTTWO, &List::evall_cond);
    set_instruction(l_cons, "cons", P_THREE, &List::evall_cons);
    set_instruction(l_consp, "consp", P_TWO, &List::evall_consp);
    set_instruction(l_converttoatom, "atom", P_TWO, &List::evall_converttoatom);
    set_instruction(l_converttointeger, "integer", P_TWO, &List::evall_converttointeger);
    set_instruction(l_converttonumber, "number", P_TWO, &List::evall_converttonumber);
    set_instruction(l_converttostring, "string", P_TWO, &List::evall_converttostring);
    set_instruction(l_data, "data", P_ATLEASTTWO, &List::evall_data);
    set_instruction(l_deflib, "deflib", P_THREE, &List::evall_deflib);
    set_instruction(l_defmacro, "defmacro", P_FOUR, &List::evall_defmacro);
    set_instruction(l_defpat, "defpat", P_ATLEASTFOUR, &List::evall_defpat);
    set_instruction(l_defun, "defun", P_ATLEASTFOUR, &List::evall_defun);
    set_instruction(l_dethread, "dethread", P_ATLEASTFOUR, &List::evall_defun);
    set_instruction(l_bodies, "bodies", P_TWO, &List::evall_bodies);
    set_instruction(l_different, "!=", P_ATLEASTTHREE, &List::evall_different);
    set_instruction(l_divide, "/", P_ATLEASTTHREE, &List::evall_divide);
    set_instruction(l_divideequal, "/=", P_ATLEASTTHREE, &List::evall_divideequal);
    set_instruction(l_eq, "eq", P_ATLEASTTHREE, &List::evall_eq);
    set_instruction(l_equal, "=", P_ATLEASTTHREE, &List::evall_equal);
    set_instruction(l_eval, "eval", P_TWO, &List::evall_eval);
    set_instruction(l_extract, "extract", P_THREE|P_FOUR, &List::evall_extract);
    set_instruction(l_fappend, "fappend", P_THREE, &List::evall_fappend);
    set_instruction(l_flip, "flip", P_TWO, &List::evall_flip);
    set_instruction(l_folding, "#folding", P_FOUR, &List::evall_folding);
    set_instruction(l_fread, "fread", P_TWO, &List::evall_fread);
    set_instruction(l_fwrite, "fwrite", P_THREE, &List::evall_fwrite);
    set_instruction(l_getchar, "getchar", P_ONE, &List::evall_getchar);
    set_instruction(l_greater, ">", P_ATLEASTTHREE, &List::evall_greater);
    set_instruction(l_greaterorequal, ">=", P_ATLEASTTHREE, &List::evall_greaterorequal);
    set_instruction(l_if, "if", P_THREE | P_FOUR, &List::evall_if);
    set_instruction(l_ife, "ife", P_ATLEASTFOUR, &List::evall_ife);
    set_instruction(l_in, "in", P_THREE|P_FOUR, &List::evall_in);
    set_instruction(l_at_index, "at", P_THREE|P_FOUR, &List::evall_at_index);
    set_instruction(l_input, "input", P_ONE | P_TWO, &List::evall_input);
    set_instruction(l_insert, "insert", P_FOUR, &List::evall_insert);
    set_instruction(l_irange, "irange", P_THREE, &List::evall_irange);
    set_instruction(l_join, "join", P_TWO | P_THREE, &List::evall_join);
    set_instruction(l_key, "key", P_ONE|P_THREE|P_FOUR, &List::evall_key);
    set_instruction(l_keyn, "keyn", P_ONE | P_THREE | P_FOUR, &List::evall_keyn);
    set_instruction(l_keys, "keys", P_TWO, &List::evall_keys);
    set_instruction(l_label, "label", P_THREE, &List::evall_label);
    set_instruction(l_lambda, "lambda", P_ATLEASTTHREE, &List::evall_lambda);
    set_instruction(l_last, "last", P_TWO, &List::evall_last);
    set_instruction(l_leftshift, "<<", P_THREE, &List::evall_leftshift);
    set_instruction(l_leftshiftequal, "<<=", P_THREE, &List::evall_leftshiftequal);
    set_instruction(l_list, "list", P_ATLEASTONE, &List::evall_list);
    set_instruction(l_load, "load", P_TWO, &List::evall_load);
    set_instruction(l_lock, "lock", P_ATLEASTTWO, &List::evall_lock);
    set_instruction(l_loop, "loop", P_ATLEASTFOUR, &List::evall_loop);
    set_instruction(l_loopcount, "loopcount", P_ATLEASTTHREE, &List::evall_loopcount);
    set_instruction(l_lower, "<", P_ATLEASTTHREE, &List::evall_lower);
    set_instruction(l_lowerorequal, "<=", P_ATLEASTTHREE, &List::evall_lowerorequal);
    set_instruction(l_mapping, "#mapping", P_THREE, &List::evall_mapping);
    set_instruction(l_mark, "mark", P_THREE | P_TWO, &List::evall_mark);
    set_instruction(l_max, "max", P_ATLEASTTHREE, &List::evall_max);
    set_instruction(l_maybe, "maybe", P_ATLEASTTWO, &List::evall_maybe);
    set_instruction(l_min, "min", P_ATLEASTTHREE, &List::evall_min);
    set_instruction(l_minus, "-", P_ATLEASTTHREE, &List::evall_minus);
    set_instruction(l_minusequal, "-=", P_ATLEASTTHREE, &List::evall_minusequal);
    set_instruction(l_mod, "%", P_ATLEASTTHREE, &List::evall_mod);
    set_instruction(l_modequal, "%=", P_ATLEASTTHREE, &List::evall_modequal);
    set_instruction(l_multiply, "*", P_ATLEASTTHREE, &List::evall_multiply);
    set_instruction(l_multiplyequal, "*=", P_ATLEASTTHREE, &List::evall_multiplyequal);
    set_instruction(l_ncheck, "ncheck", P_ATLEASTTHREE, &List::evall_ncheck);
    set_instruction(l_nconc, "nconc", P_ATLEASTONE, &List::evall_nconc);
    set_instruction(l_neq, "neq", P_ATLEASTTHREE, &List::evall_neq);
    set_instruction(l_not, "not", P_TWO, &List::evall_not);
    set_instruction(l_nullp, "nullp", P_TWO, &List::evall_nullp);
    set_instruction(l_numberp, "numberp", P_TWO, &List::evall_numberp);
    set_instruction(l_or, "or", P_ATLEASTTHREE, &List::evall_or);
    set_instruction(l_pipe, "pipe", P_ONE, &List::evall_pipe);
    set_instruction(l_sum, "sum", P_TWO, &List::evall_sum);
    set_instruction(l_product, "product", P_TWO, &List::evall_product);
    set_instruction(l_plus, "+", P_ATLEASTTHREE, &List::evall_plus);
    set_instruction(l_plusequal, "+=", P_ATLEASTTHREE, &List::evall_plusequal);
    set_instruction(l_pop, "pop", P_TWO | P_THREE, &List::evall_pop);
    set_instruction(l_power, "^^", P_THREE, &List::evall_power);
    set_instruction(l_powerequal, "^^=", P_THREE, &List::evall_powerequal);
    set_instruction(l_prettify, "prettify", P_TWO, &List::evall_prettify);
    set_instruction(l_print, "print", P_ATLEASTONE, &List::evall_print);
    set_instruction(l_printerr, "printerr", P_ATLEASTONE, &List::evall_printerr);
    set_instruction(l_printerrln, "printerrln", P_ATLEASTONE, &List::evall_printerrln);
    set_instruction(l_println, "println", P_ATLEASTONE, &List::evall_println);
    set_instruction(l_push, "push", P_THREE, &List::evall_push);
    set_instruction(l_quote, "quote", P_TWO, &List::evall_quote);
    set_instruction(l_range, "range", P_FOUR, &List::evall_range);
    set_instruction(l_return, "return", P_TWO | P_THREE, &List::evall_return);
    set_instruction(l_resetmark, "resetmark", P_TWO, &List::evall_resetmark);
    set_instruction(l_reverse, "reverse", P_TWO | P_THREE, &List::evall_reverse);
    set_instruction(l_revertsearch, "rfind", P_THREE | P_FOUR, &List::evall_revertsearch);
    set_instruction(l_rightshift, ">>", P_THREE, &List::evall_rightshift);
    set_instruction(l_rightshiftequal, ">>=", P_THREE, &List::evall_rightshiftequal);
    set_instruction(l_search, "find", P_THREE|P_FOUR, &List::evall_search);
    set_instruction(l_searchall, "findall", P_THREE|P_FOUR, &List::evall_searchall);
    set_instruction(l_select, "select", P_ATLEASTTWO, &List::evall_select);
    set_instruction(l_self, "self", P_ATLEASTONE, &List::eval_call_function);
    set_instruction(l_setg, "setg", P_THREE, &List::evall_setg);
    set_instruction(l_setq, "setq", P_THREE, &List::evall_setq);
    set_instruction(l_size, "size", P_TWO, &List::evall_size);
    set_instruction(l_sleep, "sleep", P_TWO, &List::evall_sleep);
    set_instruction(l_sort, "sort", P_THREE, &List::evall_sort);
    set_instruction(l_stringp, "stringp", P_TWO, &List::evall_stringp);
    set_instruction(l_threadclear, "threadclear", P_ONE | P_TWO, &List::evall_threadclear);
    set_instruction(l_threadretrieve, "threadretrieve", P_ONE | P_TWO, &List::evall_threadretrieve);
    set_instruction(l_threadstore, "threadstore", P_THREE, &List::evall_threadstore);
    set_instruction(l_throw, "throw", P_TWO, &List::evall_throw);
    set_instruction(l_trace, "trace", P_ONE | P_TWO, &List::evall_trace);
    set_instruction(l_trigger, "trigger", P_TWO, &List::evall_trigger);
    set_instruction(l_type, "type", P_TWO, &List::evall_type);
    set_instruction(l_unique, "unique", P_TWO, &List::evall_unique);
    set_instruction(l_use, "use", P_TWO, &List::evall_use);
    set_instruction(l_values, "values", P_TWO, &List::evall_values);
    set_instruction(l_wait, "wait", P_ONE, &List::evall_wait);
    set_instruction(l_waiton, "waiton", P_TWO, &List::evall_waiton);
    set_instruction(l_while, "while", P_ATLEASTTHREE, &List::evall_while);
    set_instruction(l_xor, "xor", P_THREE, &List::evall_xor);
    set_instruction(l_zerop, "zerop", P_TWO, &List::evall_zerop);
    set_instruction(l_zip, "zip", P_ATLEASTTHREE, &List::evall_zip);
    set_instruction(l_zipwith, "zipwith", P_ATLEASTFOUR, &List::evall_zipwith);
    
    // High level functions
    set_instruction(l_composenot, "!", P_ATLEASTTHREE, &List::evall_compose);
    set_instruction(l_cycle, "cycle", P_TWO, &List::evall_compose);
    set_instruction(l_drop, "drop", P_THREE, &List::evall_compose);
    set_instruction(l_dropwhile, "dropwhile", P_THREE, &List::evall_compose);
    set_instruction(l_filter, "filter", P_THREE, &List::evall_compose);
    set_instruction(l_foldl, "foldl", P_FOUR, &List::evall_compose);
    set_instruction(l_foldl1, "foldl1", P_THREE, &List::evall_compose);
    set_instruction(l_foldr, "foldr", P_FOUR, &List::evall_compose);
    set_instruction(l_foldr1, "foldr1", P_THREE, &List::evall_compose);
    set_instruction(l_map, "map", P_THREE, &List::evall_compose);
    set_instruction(l_repeat, "repeat", P_TWO, &List::evall_compose);
    set_instruction(l_replicate, "replicate", P_TWO, &List::evall_compose);
    set_instruction(l_scanl, "scanl", P_FOUR, &List::evall_compose);
    set_instruction(l_scanl1, "scanl1", P_THREE, &List::evall_compose);
    set_instruction(l_scanr, "scanr", P_FOUR, &List::evall_compose);
    set_instruction(l_scanr1, "scanr1", P_THREE, &List::evall_compose);
    set_instruction(l_link, "link", P_THREE, &List::evall_link);
    set_instruction(l_take, "take", P_THREE, &List::evall_compose);
    set_instruction(l_takewhile, "takewhile", P_THREE, &List::evall_compose);

    //This 
    operators[l_bitand] = true;
    operators[l_bitor] = true;
    operators[l_bitxor] = true;
    operators[l_plus] = true;
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

    math_operators = operators;

    operators[l_equal] = true;
    operators[l_different] = true;
    operators[l_lower] = true;
    operators[l_greater] = true;
    operators[l_lowerorequal] = true;
    operators[l_greaterorequal] = true;
    
    Element* e;

    //We record all our operators in advance
    for (auto& a: operators) {
        e = new Operator(a.first);
        e->status = s_constant;
        operator_pool[a.first] = e;
    }
    
    wstring w;
    for (auto& a: instructions) {
        w = L"";
        s_utf8_to_unicode(w, USTR(a.second), a.second.size());
        code_to_string[a.first] = w;
    }
    
    code_to_string[t_string] = L"string_";
    code_to_string[t_number] = L"number_";
    code_to_string[t_integer] = L"integer_";
    code_to_string[t_list] = L"list_";
    code_to_string[t_pair] = L"pair_";
    code_to_string[t_data] = L"data_";
    code_to_string[t_maybe] = L"maybe_";
    code_to_string[t_error] = L"error_";
    code_to_string[t_dictionary] = L"dictionary_";
    code_to_string[t_dictionaryn] = L"dictionary_n_";
    code_to_string[t_atom] = L"atom_";
    
    code_to_string[v_null] = L"nil";
    code_to_string[v_true] = L"true";
    
    code_to_string[c_opening] = L"(";
    code_to_string[c_closing] = L")";
    code_to_string[c_opening_brace] = L"{";
    code_to_string[c_closing_brace] = L"}";
    code_to_string[c_colon] = L":";
    code_to_string[c_opening_bracket] = L"[";
    code_to_string[c_closing_bracket] = L"]";
    
    for (auto& a: code_to_string)
        string_to_code[a.second] = a.first;
    
    //We are preparing a recording place for labels and functions .
    code_to_string[v_emptyatom] = L"";
    
    
    _NULL = (Atom*)lisp->provideAtomOrInstruction(v_null);
    _ERROR = (Atom*)lisp->provideAtomOrInstruction(t_error);
    _TERMINAL = (Atom*)lisp->provideAtomOrInstruction(l_terminal);
    _COMPOSE = (Atom*)lisp->provideAtomOrInstruction(l_compose);
    _TRUE = (Atom*)lisp->provideAtomOrInstruction(v_true);
    _EMPTYATOM = (Atom*)lisp->provideAtomOrInstruction(v_emptyatom);
    _DEFPAT = (Atom*)lisp->provideAtomOrInstruction(l_defpat);
    
    _DICO_KEY = (Atom*)lisp->provideAtomOrInstruction(l_key);
    _DICO_KEYN = (Atom*)lisp->provideAtomOrInstruction(l_keyn);
    
    _THEEND = new Error(L"Break Requested", s_constant);
    
    _EMPTYLIST = new List(s_constant);
    
    _EMPTYDICTIONARY = new Dictionary(s_constant);
    
    _BREAK = new Listbreak;
    
    _BOOLEANS[0] = _NULL;
    _BOOLEANS[1] = _TRUE;

    //rest separator in a pattern matching operation
    w = L"$";
    _LISTSEPARATOR = (Atom*)lisp->provideAtom(w);

    // Special case, they are now part of the default values
    w = L"";
    _EMPTYSTRING = (String*)lisp->provideString(w);
    _ZERO = (Integer*)lisp->provideInteger(0);
    _ONE = (Integer*)lisp->provideInteger(1);
    _TWO = (Integer*)lisp->provideInteger(2);
    _MINUSONE = (Integer*)lisp->provideInteger(-1);
    
    //On crée un espace global dans la pile (la fonction associée est _NUL)
    lisp->push(_NULL);
    
    //We create our constant values
    lisp->recordingunique(_TRUE, v_true);
    lisp->recordingunique(_NULL, v_null);
    lisp->recordingunique(_ERROR, t_error);
    
    //These types are all basic data structures

    provideAtomType(t_string);
    provideAtomType(t_number);
    provideAtomType(t_integer);
    provideAtomType(t_list);
    provideAtomType(t_data);
    provideAtomType(t_maybe);
    provideAtomType(t_dictionary);
    provideAtomType(t_dictionaryn);
    provideAtomType(t_atom);
                    
    recordingData(lisp->create_instruction(t_string, _NULL), t_string, v_null);
    recordingData(lisp->create_instruction(t_number, _NULL), t_number, v_null);
    recordingData(lisp->create_instruction(t_integer, _NULL), t_integer, v_null);
    recordingData(lisp->create_instruction(t_list, _NULL), t_list, v_null);
    recordingData(lisp->create_instruction(t_data, _NULL), t_data, v_null);
    recordingData(lisp->create_instruction(t_maybe, _NULL), t_maybe, v_null);
    recordingData(lisp->create_instruction(t_dictionary, _NULL), t_dictionary, v_null);
    recordingData(lisp->create_instruction(t_dictionaryn, _NULL), t_dictionaryn, v_null);
    recordingData(lisp->create_instruction(t_atom, _NULL), t_atom, v_null);
    
    //We introduce _ as a substitute to nil
    w = L"_";
    string_to_code[w] = v_null;

    //But also 'false', which is a substitute to nil as well
    w = L"false";
    string_to_code[w] = v_null;

    // We introduce \ and λ (Unicode: 955) as a substitute to lambda...
    w = L"\\";
    string_to_code[w] = l_lambda;
    w = L"λ";
    string_to_code[w] = l_lambda;
    
    //For compliance with other Lisps
    w = L"t";
    string_to_code[w] = v_true;

    //Small tip, to avoid problems
    // indeed, the instruction cadr is already linked to its own code
    e = new Cadr("cadr");
    e->status = s_constant;
    atom_pool[l_cadr] = e;
    
    // We create all our atoms and keep them in the stack
    for (auto& a: instructions) {
        e = lisp->provideAtomOrInstruction(a.first);
        lisp->recordingunique(e, a.first);
    }
    
    lisp->provideAtom(c_opening);
    lisp->provideAtom(c_closing);
    lisp->provideAtom(c_opening_brace);
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

    //We then clean all our pools...
    for (auto& a: number_pool)
        delete a.second;
    
    for (auto& a: integer_pool)
        delete a.second;
    
    for (auto& a: string_pool)
        delete a.second;
    
    for (long i = 0; i < garbages.size(); i++)
        delete garbages[i];
    
    for (auto& a: pools)
        delete a.second;
    
    if (!isThread) {
        delete delegation;
        delete handlingutf8;
    }
}

std::atomic<long> id_pool(1);

LispE::LispE(LispE* lisp, List* function, Element* body) {

    //For threads, the stack limit is much smaller
    max_stack_size = 1000;

    id_thread = id_pool++;
    
    thread_ancestor = lisp;

    line_error = -1;
    
    delegation = lisp->delegation;
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
/*
 We only copy constant elements to this thread... Basically, only functions
*/
    stack_pool[0]->copy(lisp->stack_pool[0]);
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

        stops['!'] = true;
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

    line_error = -1;
    long sz = code.size();
    long i, current_i;
    int nb_parentheses = 0;
    int nb_braces = 0;
    int nb_brackets = 0;
    //The first line of the code is 1
    long line_number = 1;
    long culprit = -1;
    UWCHAR c, nxt;
    bool add = delegation->add_to_listing;
    lisp_code lc;
    string current_line;
    string tampon;
    for (i = 0; i < sz; i++) {
        current_i = i;
        c = getonechar(USTR(code), i);
        switch (c) {
            case ';':
            case '#': //comments (we accept both with ; and #)
                idx = i;
                while (i < sz && code[i] != '\n') i++;
                if (add) {
                    current_line = code.substr(idx, i-idx+1);
                    add_to_listing(line_number, current_line);
                    current_line = "";
                }
                line_number++;
                break;
            case '\n':
                if (add) {
                    current_line += c;
                    add_to_listing(line_number, current_line);
                    current_line = "";
                }
                line_number++;
                break;
            case '\t':
            case '\r':
            case ' ':
                if (add)
                    current_line += c;
                continue;
            case '\'':
                if (add)
                    current_line += c;
                infos.append(c, l_quote, line_number, i, i+1);
                break;
            case '`': { // a string containing what we want...
                idx = i + 1;
                tampon = "";
                while (idx < sz && code[idx] != '`') {
                    idx++;
                }
                if (idx == sz)
                    return e_error_string;

                tampon = code.substr(i+1, idx-i-1);
                if (tampon == "")
                    infos.append(tampon, t_emptystring, line_number, i, idx);
                else
                    infos.append(tampon, t_string, line_number, i, idx);
                if (add) {
                    current_line += "`";
                    current_line += tampon;
                    current_line += "`";
                }
                i = idx;
                break;
            }
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
                if (add) {
                    current_line += "\"";
                    current_line += tampon;
                    current_line += "\"";
                }
                i = idx;
                break;
            }
            case '+':
            case '-':
                if (!isdigit(code[i+1])) {
                    idx = i + 1;
                    while (idx < sz) {
                        nxt = getonechar(USTR(code), idx);
                        if (nxt == 8220 || (nxt < 172 && stops[nxt]))
                            break;
                        i = idx;
                        idx++;
                    }
                    tampon = code.substr(current_i, i - current_i + 1);
                    if (add)
                        current_line += tampon;
                    idx = delegation->is_atom(tampon);
                    if (idx >= l_plus && idx <= l_modequal)
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
                if (add)
                    current_line += tampon;
                infos.append(d, tampon, t_number, line_number, i, i + idx);
                i += idx - 1;
                break;
            }
            case 171: {
                //This is a «, we look for the next: »
                //(ord «test»)
                idx = i + 1;
                nxt = getonechar(USTR(code), idx);
                idx++;
                while (idx < sz && nxt != 187) {
                    nxt = getonechar(USTR(code), idx);
                    idx++;
                }
                if (idx == sz)
                    return e_error_string;
                
                tampon = code.substr(i+1, idx-i-3);
                if (tampon == "")
                    infos.append(tampon, t_emptystring, line_number, i, idx);
                else
                    infos.append(tampon, t_string, line_number, i, idx);
                if (add) {
                    current_line += "«";
                    current_line += tampon;
                    current_line += "»";
                }
                i = idx-1;
                break;
            }
            case 8220: {
                //(ord “test”)
                idx = i + 1;
                nxt = getonechar(USTR(code), idx);
                idx++;
                while (idx < sz && nxt != 8221) {
                    nxt = getonechar(USTR(code), idx);
                    idx++;
                }
                if (idx == sz)
                    return e_error_string;
                
                tampon = code.substr(i+1, idx-i-4);
                if (tampon == "")
                    infos.append(tampon, t_emptystring, line_number, i, idx);
                else
                    infos.append(tampon, t_string, line_number, i, idx);
                
                if (add) {
                    current_line += "“";
                    current_line += tampon;
                    current_line += "”";
                }
                i = idx-1;
                break;
            }
            default: {
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
                    if (add)
                        current_line += c;
                }
                else {
                    tampon = code.substr(current_i, i - current_i);
                    if (add)
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
                        if (lc >= l_plus && lc <= l_modequal)
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
        line_error = line_number;
        return e_error_bracket;
    }
    
    if (nb_parentheses) {
        line_error = culprit;
        return e_error_parenthesis;
    }
    
    if (nb_braces) {
        line_error = culprit;
        return e_error_brace;
    }
    
    if (add && current_line != "")
        add_to_listing(line_number, current_line);
    
    return e_no_error;
}


Element* LispE::tokenize(wstring& code, bool keepblanks) {
    List* res = new List;
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
 
 */


//We keep the numbers described in the code
Element* LispE::provideNumber(double d) {
    Number* n = number_pool[d];
    if (n == NULL) {
        n = new Number(d, s_constant);
        number_pool[d] = n;
    }
    return n;
}

//We keep the numbers described in the code
Element* LispE::provideInteger(long d) {
    Integer* n = integer_pool[d];
    if (n == NULL) {
        n = new Integer(d, s_constant);
        integer_pool[d] = n;
    }
    return n;
}

//The strings described in the code are kept.
Element* LispE::provideString(string& str) {
    wstring s;
    s_utf8_to_unicode(s, USTR(str), str.size());
    String* c = string_pool[s];
    if (c == NULL) {
        c = new String(s, s_constant);
        string_pool[s] = c;
    }
    return c;
}

Element* LispE::provideString(wchar_t ch) {
    wstring s;
    s = ch;
    String* c = string_pool[s];
    if (c == NULL) {
        c = new String(s, s_constant);
        string_pool[s] = c;
    }
    return c;
}

Element* LispE::provideString(wstring& s) {
    String* c = string_pool[s];
    if (c == NULL) {
        c = new String(s, s_constant);
        string_pool[s] = c;
    }
    return c;
}

/*
 Saving arguments in a list: _args
 */
void LispE::arguments(std::vector<string>& args) {
    List* l = new List;
    for (auto& a: args) {
        l->append(provideString(a));
    }
    string nom = "_args";
    recordingunique(l, encode(nom));
}

void LispE::current_path() {
    if (delegation->allfiles.size() >= 2) {
        wstring nom = L"_current";
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
        execution_stack.top()->recordingunique(e, encode(nom));
    }
}

/*
 In Lisp, the abstract syntax tree and the execution tree are merged...
 We will build a structure in which we will browse the list of segments and for each parenthesis, we will
 build a sub-list...
 */

Element* LispE::abstractSyntaxTree(Element* courant, Tokenizer& parse, long& index) {
    Element* e = NULL;
    Element* quote = NULL;
    double value;
    char topfunction = false;
    while (index < parse.types.size()) {
        switch (parse.types[index]) {
            case c_opening:
                index++;
                //Empty list
                if (parse.types[index] == c_closing) {
                    index++;
                    e = delegation->_EMPTYLIST;
                }
                else {
                    e = new Listincode(parse.lines[index], delegation->i_current_file);
                    garbaging(e);
                    
                    abstractSyntaxTree(e, parse, index);
                    if (e->size() >= 1) {
                        short lab = e->index(0)->label();
                        if (lab == l_defmacro) {
                            e->eval(this);
                            break;
                        }
                        bool docompose = true;
                        if (lab == l_composenot) {
                            ((List*)e)->liste.erase(0);
                            if (e->size()) {
                                lab = e->index(0)->label();
                                docompose = false;
                            }
                        }
                        if (lab >= l_map && lab <= l_scanr1) {
                            Element* inter = e->composing(this, docompose);
                            if (inter != e) {
                                removefromgarbage(e);
                                e = inter;
                            }
                        }
                        else {
                            if (topfunction && topfunction <= courant->size()) {
                                //we detect if and cond at the top level
                                //we use this information to handle terminal recursion on the fly
                                //The 'terminal' flag helps define if a potential call can be treated as terminal recursion
                                if (lab >= l_if && lab <= l_cond)
                                    e->setterminal();
                            }
                        }
                    }
                }
                e = generate_macro(e);
                if (e->size() >= 1) {
                    //these are specialized calls, for which we do not need to go through List::eval
                    short lab = e->index(0)->type;
                    if (lab == l_break) {
                        if (e->size() != 1)
                            throw new Error("Error: break does not take any arguments");
                        removefromgarbage(e);
                        e = &delegation->_BREAKEVAL;
                    }
                }
                
                if (quote == NULL)
                    courant->append(e);
                else {
                    quote->append(e);
                    quote = NULL;
                }
                break;
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
                    abstractSyntaxTree(&dico, parse, index);
                    e = dico.dictionary(this);
                    garbaging(e);
                }
                if (quote == NULL)
                    courant->append(e);
                else {
                    quote->append(e);
                    quote = NULL;
                }
                break;
            }
            case c_closing_brace:
                index++;
                return delegation->_TRUE;
            case t_emptystring:
                if (quote == NULL)
                    courant->append(delegation->_EMPTYSTRING);
                else {
                    quote->append(delegation->_EMPTYSTRING);
                    quote = NULL;
                }
                index++;
                break;
            case t_string:
                e = provideString(parse.tokens[index]);
                if (quote == NULL)
                    courant->append(e);
                else {
                    quote->append(e);
                    quote = NULL;
                }
                index++;
                break;
            case t_number: {
                value = parse.numbers[index];
                if (value == 0)
                    e = delegation->_ZERO;
                else {
                    if (parse.tokens[index].find(".") == -1)
                        e = provideInteger((long)value);
                    else
                        e = provideNumber(value);
                }
                if (quote == NULL)
                    courant->append(e);
                else {
                    quote->append(e);
                    quote = NULL;
                }
                index++;
                break;
            case t_operator:
                e = provideOperator(encode(parse.tokens[index]));
                if (quote == NULL)
                    courant->append(e);
                else {
                    quote->append(e);
                    quote = NULL;
                }
                index++;
                break;
            case l_cadr:
                e =  provideCADR(parse.tokens[index]);
                if (quote == NULL)
                    courant->append(e);
                else {
                    quote->append(e);
                    quote = NULL;
                }
                index++;
                break;
            case c_colon:
                if (courant->label() == t_dictionary) {
                    courant->reversechoice();
                    index++;
                    if (parse.types[index] == c_colon)
                        throw new Error("Error: wrong key/value separator in a dictionary");
                    break;
                }
            case t_atom:
                e = provideAtom(encode(parse.tokens[index]));
                if (quote == NULL) {
                    if (courant->size() == 0) {
                        if (e->type >= l_lambda && e->type <= l_defpat) {
                            if (e->type == l_lambda)
                                topfunction = 2;
                            else
                                topfunction = 3;
                        }
                    }
                    courant->append(e);
                }
                else {
                    quote->append(e);
                    quote = NULL;
                }
                index++;
                break;
            case l_quote:
                if (quote == NULL) {
                    quote = new Listincode(parse.lines[index], delegation->i_current_file);
                    garbaging(quote);
                    courant->append(quote);
                }
                else {
                    e = new Listincode(parse.lines[index], delegation->i_current_file);
                    garbaging(e);
                    quote->append(e);
                    quote = e;
                }
                //The first element of a quote is the quote operator itself
                quote->append(provideAtom(l_quote));
                index++;
                break;
            }
            default:
                break;
        }
    }
    return delegation->_TRUE;
}

Element* LispE::atomise(wstring a) {
    return delegation->atomise(a, checkforLock());
}

List* LispE::create_instruction(short label,
                                Element* e1,
                                Element* e2,
                                Element* e3,
                                Element* e4,
                                Element* e5,
                                Element* e6,
                                Element* e7) {
    List* l = new List;
    
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

Element* LispE::load(string pathname) {
    pathname = NormalizePathname(pathname);
    try {
        delegation->i_current_file = delegation->allfiles.at(pathname);
        return delegation->entrypoints[delegation->i_current_file];
    }
    catch (const std::out_of_range& oor) {}
    
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
    
    try {
        Element* tree = compile(code);
        delegation->entrypoints[delegation->i_current_file] = tree;
        
        current_path();
        
        return tree->eval(this);
    }
    catch(Error* err) {
        return err;
    }
}

Element* LispE::compile(string& code) {
    clearStop();
    //A little trick to compile code sequences
    code = "(block " + code + ")";
    Tokenizer parse;
    lisp_code retour = segmenting(code, parse);
    List courant;
    long index;
    switch (retour) {
        case e_error_brace:
            delegation->i_current_line = line_error;
            throw new Error("Error: Braces do not balance");
        case e_error_bracket:
            delegation->i_current_line = line_error;
            throw new Error("Error: brackets do not balance");
        case e_error_parenthesis:
            delegation->i_current_line = line_error;
            throw new Error("Error: parentheses do not balance");
        case e_error_string:
            delegation->i_current_line = 1;
            if (parse.lines.size())
                delegation->i_current_line = parse.lines.back();
            throw new Error("Error: missing end of string");
        default:
            index = 0;
    }
    
    try {
        abstractSyntaxTree(&courant, parse, index);
    }
    catch(Error* err) {
        delegation->i_current_line = 1;
        if (parse.lines.size())
            delegation->i_current_line = parse.lines.back();
        return err;
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
    if (code.find("deflib ") == -1)
        throw new Error("Error: 'deflib' missing from the code");
    List* current_list = NULL;
    
    Tokenizer parse;
    lisp_code retour = segmenting(code, parse);
    
    switch (retour) {
        case e_error_brace:
            etendre->release();
            delegation->i_current_line = line_error;
            throw new Error("Error: Braces do not balance");
        case e_error_bracket:
            etendre->release();
            delegation->i_current_line = line_error;
            throw new Error("Error: brackets do not balance");
        case e_error_parenthesis:
            etendre->release();
            delegation->i_current_line = line_error;
            throw new Error("Error: parentheses do not balance");
        case e_error_string:
            etendre->release();
            delegation->i_current_line = 1;
            if (parse.lines.size())
                delegation->i_current_line = parse.lines.back();
            throw new Error("Error: missing end of string");
        default:
            current_list = new List;
    }
    
    try {
        long index = 0;
        abstractSyntaxTree(current_list, parse, index);
        Element* body = current_list->eval(this);
        body->append(etendre);
        garbaging(etendre);
        return body;
    }
    catch(Error* err) {
        etendre->release();
        return err;
    }
}

Element* LispE::execute(string code) {
    delegation->i_current_line = 0;
    delegation->i_current_file = 0;
    clearStop();
    try {
        return compile(code)->eval(this);
    }
    catch(Error* err) {
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
        Element* tree = compile(code);
        delegation->entrypoints[delegation->i_current_file] = tree;
        current_path();
        
        return tree->eval(this);
    }
    catch(Error* err) {
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
void Element::generate_body_from_macro(LispE* lisp, Listincode* code, unordered_map<short,Element*>& dico_variables) {
    Element* e;
    for (long i = 0;i < size(); i++) {
        e = index(i);
        if (e->isAtom()) {
            if (dico_variables.find(e->label()) != dico_variables.end())
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
    try {
        short label = code->index(0)->label();
        Element* macro_rule = delegation->macros.at(label);
        Element* macro_parameters = macro_rule->index(2);
        if (macro_parameters->size() != code->size()-1)
            throw new Error("Error: parameter size does not match argument");
        //Now we need to create a place where to store our parameters...
        long i;
        unordered_map<short,Element*> dico_variables;
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
    catch(const std::out_of_range& oor) {
        return code;
    }
}


//We replace the variables with local macro name...
void Element::replaceVariableNames(LispE* lisp, unordered_map<short,Element*>& dico_variables) {
    if (isList()) {
        Element* e;
        for (long i = 0; i < size(); i++) {
            e = index(i);
            if (e->isAtom() && dico_variables.find(e->label()) != dico_variables.end())
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
    short newlabel, varlabel;
    wstring lb;
    unordered_map<short,Element*> dico_variables;
    for (long i = 0; i < macro_parameters->size(); i++) {
        //Our new name
        lb = L"#macro";
        lb += std::to_wstring(i);
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



























































