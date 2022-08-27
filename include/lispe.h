/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  lispe.h
//
//


#ifndef lispe_h
#define lispe_h

#include "segmentation.h"
#include "listes.h"
#include "llistes.h"
#include "tools.h"
#include "stack.h"
#include "delegation.h"
#include <stack>

//------------------------------------------------------------
#define debug_none 0
#define debug_goto 1
#define debug_next 2
#define debug_inside_function 3
//------------------------------------------------------------
typedef int16_t (LispE::*checkEval)(List*, long);
typedef int16_t (LispE::*checkBasicEval)(Listincode*);
//------------------------------------------------------------
string LispVersion();
//------------------------------------------------------------
// The main class to handle the Lisp Interpreter
//------------------------------------------------------------

class LispE {
    vector<Element*> garbages;
    vecte<Stackelement*> stack_pool;

    vecte<Stackelement*> execution_stack;
    long max_stack_size;

public:
    Element* _BOOLEANS[2];
    

    vecte<Floatpool*> float_pool;
    vecte<Numberpool*> number_pool;
    vecte<Integerpool*> integer_pool;
    vecte<Stringpool*> string_pool;

    vecte<Floatspool*> floats_pool;
    vecte<Numberspool*> numbers_pool;
    vecte<Integerspool*> integers_pool;
    vecte<Stringspool*> strings_pool;

    vecte<Set_spool*> sets_pool;
    vecte<Set_npool*> setn_pool;
    vecte<Set_ipool*> seti_pool;
    vecte<Setpool*> set_pool;

    vecte<Dictionarypool*> dictionary_pool;
    vecte<Dictionary_npool*> dictionaryn_pool;
    vecte<Dictionary_ipool*> dictionaryi_pool;
    
    vecte<Listpool*> list_pool;
    
    vecte<Quotedpool*> quoted_pool;
    
    vecte<Returnpool*> return_pool;

    //------------------------------------------
    
    unordered_map<wstring, Element*> pools;
    vector<Element*> vpools;
    
    unordered_map<u_ustring, String*> const_string_pool;
    unordered_map<long, Integer*> const_integer_pool;
    unordered_map<double, Number*> const_number_pool;
    
    unordered_map<short, char> depths;
    
    //Delegation is a class that records any data
    //related to compilation
    Delegation* delegation;
    UTF8_Handler* handlingutf8;

    List* current_thread;
    List* current_body;
    List* void_function;
    LispE* thread_ancestor;
    
    Element* n_null;
    Element* n_true;
    Element* n_zero;
    Element* n_one;
    
    
    std::atomic<int16_t> nbjoined;

    long id_thread;
    int16_t current_space;
    
    char trace;
    bool isThread;
    bool hasThread;
    bool check_arity_on_fly;
    bool preparingthread;
    bool clean_utf8;
    
    LispE(UTF8_Handler* hnd = NULL) {
        current_space = 0;
        initpools();
        preparingthread = false;
        check_arity_on_fly = false;
        id_thread = 0;
        max_stack_size = 10000;
        trace = debug_none;
        delegation = new Delegation;
        isThread = false;
        hasThread = false;
        thread_ancestor = NULL;
        nbjoined = 0;
        current_thread = NULL;
        current_body = NULL;
        if (hnd == NULL) {
            handlingutf8 = new UTF8_Handler;
            clean_utf8 = true;
        }
        else {
            handlingutf8 = hnd;
            clean_utf8 = false;
        }
        delegation->initialisation(this);
        n_null = delegation->_NULL;
        n_true = delegation->_TRUE;
        n_zero = delegation->_ZERO;
        n_one = delegation->_ONE;
    }
    
    inline void initpools() {
        for (long i = 0; i < 100; i++) {
            quoted_pool.push_back(new Quotedpool(this));
            list_pool.push_back(new Listpool(this));
            number_pool.push_back(new Numberpool(this, 0));
            integer_pool.push_back(new Integerpool(this, 0));
            string_pool.push_back(new Stringpool(this));
            strings_pool.push_back(new Stringspool(this));
            numbers_pool.push_back(new Numberspool(this));
            integers_pool.push_back(new Integerspool(this));
        }
    }
    inline bool checkArity(List* l) {
        return (!check_arity_on_fly || delegation->checkArity(l->liste[0]->type, l->size()));
    }
    
    LispE(LispE*, List* function, List* body);
    
    ~LispE() {
        cleaning();
    }
    
    //------------------------------------------

    void set_true_as_true() {
        _BOOLEANS[0] = n_null;
        _BOOLEANS[1] = n_true;
    }

    void set_true_as_one() {
        _BOOLEANS[0] = n_zero;
        _BOOLEANS[1] = n_one;
    }

    inline void push(Element* fonction) {
        execution_stack.push_back(provideStackElement(fonction));
    }
    
    inline void clear_top_stack() {
        execution_stack.back()->clear();
    }

    inline Stackelement* provideStackElement(Element* function) {
        if (execution_stack.last == max_stack_size)
            sendError(U"stack overflow");

        return stack_pool.last?stack_pool.backpop()->setFunction(function):new Stackelement(function);
    }
    
    inline Stackelement* providingStack(Element* function) {
        if (execution_stack.last == max_stack_size)
            sendError(U"stack overflow");

        return stack_pool.last?stack_pool.backpop()->setFunction(function):new Stackelement(function);
    }

    void removeStackElement() {
        Stackelement* s = execution_stack.backpop();
        s->clear();
        stack_pool.push_back(s);
    }

    void removeStackElement(Element* keep) {
        Stackelement* s = execution_stack.backpop();
        s->clear(keep);
        stack_pool.push_back(s);
    }
    
    inline long threadId() {
        return id_thread;
    }
    
    inline bool threaded() {
        return (hasThread | isThread);
    }
    
    void lock() {
        delegation->lock.locking(threaded());
    }
    
    void unlock() {
        delegation->lock.unlocking(threaded());
    }
    
    void stop_trace() {
        delegation->endtrace = true;
        trace = debug_none;
        delegation->next_stop = false;
    }

    bool isEndTrace() {
        return delegation->isEndTrace();
    }
    
    void stop_at_next_line(char tr) {
        if (delegation->endtrace) {
            trace = debug_none;
            delegation->trace_on = false;
            delegation->next_stop = false;
            return;
        }
        trace = tr;
        delegation->trace_on = true;
        delegation->next_stop = true;
    }
    
    void add_pathname(string pathname);
    void set_pathname(string pathname);
    
    void add_to_listing(long l, string& e) {
        delegation->addtolisting(l, e);
    }
    
    List* create_instruction(int16_t label,
                             Element* e = NULL,
                             Element* ee = NULL,
                             Element* eee = NULL,
                             Element* eeee = NULL,
                             Element* eeeee = NULL,
                             Element* eeeeee = NULL,
                             Element* eeeeeee = NULL);

    List* create_local_instruction(int16_t label,
                             Element* e = NULL,
                             Element* ee = NULL,
                             Element* eee = NULL,
                             Element* eeee = NULL,
                             Element* eeeee = NULL,
                             Element* eeeeee = NULL,
                             Element* eeeeeee = NULL);

    //We have an internal pooling for elements that should be shared across
    //threads. We use it in the interpreter for regular expressions for instance
    //These instructions are used to handle elements that should be
    //shared across the interpreter
    inline void pool_in(wstring& key, Element* e) {
        pools[key] = e;
    }

    inline Element* pool_out(wstring& key) {
        try {
            return pools.at(key);
        }
        catch (...) {
            return n_null;
        }
    }
    
    inline long vpool_slot() {
        for (long i = 0; i < vpools.size(); i++) {
            if (vpools[i] == n_null)
                return i;
        }
        vpools.push_back(n_null);
        return (vpools.size() - 1);
    }
    
    inline void vpool_in(Element* e, long idx) {
        vpools[idx] = e;
    }
    
    inline long vpool_add(Element* e) {
        long idx = vpool_slot();
        vpools[idx] = e;
        return idx;
    }
    
    inline bool vpool_check(long idx) {
        return (idx >= 0 && idx < vpools.size() && vpools[idx] != n_null);
    }

    inline bool vpool_check(Element* e, long idx) {
        return (idx >= 0 && idx < vpools.size() && vpools[idx] == e);
    }

    inline Element* vpool_out(long idx) {
        if (idx < 0 || idx >= vpools.size())
            return n_null;
        return vpools[idx];
    }
    
    inline bool vpool_release(long idx) {
        if (idx < 0 || idx >= vpools.size())
            return false;
        Element* e = vpools[idx];
        if (e == n_null)
            return false;
        vpools[idx] = n_null;
        return true;
    }
  
    inline void set_current_line(long l, long f) {
        if (delegation->stop_execution)
            sendError();
        delegation->i_current_line = l;
        delegation->i_current_file = f;
    }
    
    inline void clear_breakpoints() {
        delegation->clear_breakpoints();
    }
    
    long id_file(string pathname) {
        return delegation->id_file(pathname);
    }
    
    inline string name_file(long i) {
        try {
            return delegation->allfiles_names.at(i);
        }
        catch (...) {
            return "";
        }
    }

    inline string current_name_file() {
        try {
            return delegation->allfiles_names.at(delegation->i_current_file);
        }
        catch (...) {
            return "";
        }
    }
    

    inline bool activate_on_breakpoints(List* e) {
        return delegation->activate_on_breakpoints(this, e);
    }
    
    inline bool is_instruction(int16_t c) {
        return delegation->is_instruction(c);
    }
    
    bool is_instruction(string str) {
        wstring s;
        s_utf8_to_unicode(s, USTR(str), str.size());
        return delegation->is_instruction(s);
    }
    
    inline bool is_instruction(u_ustring s) {
        return delegation->is_instruction(s);
    }
    
    inline bool is_operator(int16_t c) {
        return delegation->is_operator(c);
    }
    
    inline bool is_math_operator(int16_t c) {
        return delegation->is_math_operator(c);
    }
    
    int16_t is_atom(u_ustring& s) {
        return delegation->is_atom(s);
    }
    
    int16_t is_atom(string str) {
        u_ustring s;
        s_utf8_to_unicode(s, USTR(str), str.size());
        return delegation->is_atom(s);
    }
    
    Element* load_library(string name);
    Element* extension(string, Element* e = NULL);
    Element* eval(string);
    Element* eval(wstring& w) {
        string s;
        s_unicode_to_utf8(s, w);
        return eval(s);
    }
    
    Element* atomise(u_ustring a);
    void cleaning();
    
    Element* execute(string code);
    Element* execute(string code, string path_name);
    
    Element* compile_lisp_code(string& code);
    Element* load(string chemin);
    lisp_code segmenting(string& code, Tokenizer& s);
    Element* tokenize(wstring& code, bool keepblanks = false);
    Element* abstractSyntaxTree(Element* courant, Tokenizer& s, long& index, long quoting);
    Element* syntaxTree(Element* courant, Tokenizer& s, long& index, long quoting);
    void arguments(std::vector<string>& args);
    void current_path();
    
    void blocking_trace_lock() {
        delegation->blocking();
    }
    
    void releasing_trace_lock() {
        delegation->releasing();
    }
    
    char checking_trace_lock() {
        return delegation->checking();
    }
    
    long stack_size_max() {
        return max_stack_size;
    }
    
    void set_debug_function (lispe_debug_function ldf, void* o) {
        delegation->add_to_listing = true;
        delegation->debugfunction = ldf;
        delegation->debugobject = o;
    }

    void set_stack_max_size(long m) {
        if (m <= 0)
            return;
        max_stack_size = m;
    }
    
    inline void stop() {
        delegation->stop_execution = 0x100;
    }

    inline bool isthreadError() {
        return (delegation->stop_execution & 1);
    }
    
    inline bool hasStopped() {
        return (delegation->stop_execution & 0x100);
    }
    
    inline bool hasOverFlown() {
        return (execution_stack.last >= max_stack_size);
    }
    
    inline void clearStop() {
        delegation->stop_execution = 0;
    }
    
    inline int16_t checkLispState() {
        return (execution_stack.last >= max_stack_size?0x200:delegation->stop_execution);
    }
        
    inline bool sendError() {
        throw delegation->_THEEND;
    }
    
    inline bool sendError(u_ustring msg) {
        throw new Error(msg);
    }
    
    //2
    inline int16_t check_arity(List* l, long sz) {
        int16_t lb = l->liste.get0();
        return (delegation->checkArity(lb, sz))?lb:sendError(U"Error: wrong number of arguments");
    }

    inline int16_t checkState(List* l, long sz) {
        delegation->checkExecution();
        return (!sz)?l_emptylist:(!check_arity_on_fly)?l->liste.get0():check_arity(l, sz);
    }

    //1
    inline int16_t check_basic_trace(Listincode* l) {
        trace_and_context(l);
        return l->liste.get0();
    }

    inline int16_t checkBasicState(Listincode* l) {
        delegation->checkExecution();
        return (!trace)?l->liste.get0():check_basic_trace(l);
    }

    inline void checkPureState(Listincode* l) {
        delegation->checkExecution();
        if (trace) {
            trace_and_context(l);
        }
    }
    
    inline void trace_and_context(Listincode* e) {
        //in the case of a goto, we only take into account breakpoints
        delegation->set_context(e->line, e->fileidx);

        if (trace == debug_goto)
            delegation->next_stop = false;
        
        if (!activate_on_breakpoints(e)) {
            if (delegation->debugfunction == NULL) {
                long nb = stackSize();
                string space(nb, ' ');
                std::cout << "(" << delegation->i_current_line << ") " << nb << ":" << space << e->toString(this) << std::endl;
            }
        }
    }

    inline string toString(int16_t c) {
        return delegation->toString(c);
    }

    inline wstring asString(int16_t c) {
        return delegation->asString(c);
    }

    inline u_ustring asUString(int16_t c) {
        return delegation->asUString(c);
    }

    inline long nbvariables() {
        return execution_stack.back()->size();
    }
    
    inline void atomsOnStack(vector<Element*>& v_atoms) {
        vector<int16_t> labels;
        execution_stack.back()->atoms(labels);
        for (int16_t i = 0; i < labels.size(); i++) {
            if (delegation->is_atom_code(labels[i]))
                v_atoms.push_back(delegation->provideAtom(labels[i]));
        }
    }
    
    string stackImage() {
        string the_stack;
        long sz;
        for (long i = execution_stack.last-1; i >= 0; i--) {
            if (execution_stack[i]->function == NULL || execution_stack[i]->function == n_null)
                continue;
            sz = execution_stack[i]->function->size();
            if (sz > 1) {
                the_stack += execution_stack[i]->function->index(1)->toString(this);
                the_stack += "(";
                if (sz > 2)
                    the_stack += execution_stack[i]->function->index(2)->toString(this);
                the_stack += ")";
                the_stack += "\n";
            }
        }
        return the_stack;
    }
    
    inline long stackSize() {
        return execution_stack.last;
    }
    
    bool globalDeclaration() {
        if (execution_stack.last == 1 && !id_thread)
            return true;
        return false;
    }
    
    wstring stackAsString() {
        return execution_stack.back()->asString(this);
    }
    
    inline Element* called() {
        return execution_stack.back()->called();
    }
    
    inline void popping() {
        execution_stack.pop_back();
    }
    
    inline void pushing(Stackelement* s) {
        execution_stack.push_back(s);
    }
    
    inline List* atomes() {
        return execution_stack.back()->atomes(this);
    }
    
    inline void pop(Stackelement* s) {
        s->clear();
        stack_pool.push_back(s);
    }

    inline void pop() {
        removeStackElement();
    }
    
    inline Element* pop(Element* e) {
        e->increment();
        removeStackElement();
        e->decrementkeep();
        return e;
    }

    //We clear the stack up to upto
    inline Element* pop(Element* e, long upto) {
        e->increment();
        while (execution_stack.last != upto) {
            removeStackElement();
        }
        e->decrementkeep();
        return e;
    }
    
    inline void clearStack() {
        stack_pool.cleaning();
        for (long i = 0; i < execution_stack.last; i++) {
            execution_stack[i]->clear();
            delete execution_stack[i];
        }
    }
    
    inline void cleanTopStack() {
        while (execution_stack.last > 1) {
            removeStackElement();
        }
    }
        
    //It is a little but counter-intuitive, but a dictionary description in a pattern function needs to be replaced
    //with a Dictionary_as_list object... A little under-efficient for sure, but this is done only once at compile time
    //The overall test does not eat so much resources...
    inline void replaceWithDicolist(Element* e, Dictionary_as_list** dico) {
        if (e->isList()) {
            if (!e->size())
                return;
            int16_t label = e->index(0)->label();
            long i = 0;
            if (label == l_key || label == l_keyn) {
                if (*dico == NULL) {
                    *dico = new Dictionary_as_list;
                    garbaging(*dico);
                    if (label == l_key)
                        (*dico)->type = t_dictionary;
                    else
                        (*dico)->type = t_dictionaryn;
                }
                i = 1;
            }
            
            if (*dico != NULL) {
                for (; i < e->size(); i++)
                    replaceWithDicolist(e->index(i),dico);
            }
        }
        else {
            if (*dico != NULL)
                (*dico)->push(e);
        }
    }
    
    inline Element* recordingMethod(Element* e, int16_t label) {
        //We check if we have dictionaries in the list...
        Element* parameters = e->index(2);
        Dictionary_as_list* dico;
        for (long i = 0; i < parameters->size(); i++) {
            dico = NULL;
            replaceWithDicolist(parameters->index(i), &dico);
            //If we have a parameter that is a dico structure,
            //we replace it with a Dico_as_list, which is the
            //original structure with which it was built in the first
            //place.
            if (dico != NULL) {
                if (!dico->verify())
                    throw new Error("Error: When building a pattern for a dictionary, keys with actual values (number of string) should appear first");
                parameters->change(i, dico);
            }
        }
        char depth = 0;
        int16_t sublabel = extractlabel(parameters, depth);
        depths[label] = std::max(depth, depths[label]);
        if (globalDeclaration())
            return delegation->recordingMethod(NULL, e, label, sublabel, current_space);
        return delegation->recordingMethod(execution_stack.back(), e, label, sublabel, current_space);
    }
    
    inline Element* recordingData(Element* e, int16_t label, int16_t ancestor) {
        return delegation->recordingData(e, label, ancestor);
    }
    
    inline bool checkAncestor(Element* ancestor, Element* label) {
        return delegation->checkAncestor(ancestor, label->label());
    }
    
    inline int16_t checkDataStructure(Element* e) {
        return delegation->checkDataStructure(e);
    }

    inline int16_t checkDataStructure(int16_t label) {
        return delegation->checkDataStructure(label);
    }

    //Extract all the values from a list
    inline void extractAllAtoms(Element* e, std::vector<Element*>& atomes) {
        if (e->isList()) {
            for (long i = 0; i < e->size(); i++) {
                extractAllAtoms(e->index(i), atomes);
            }
        }
        else {
            if (e->isDictionary()) {
                Element* l = e->thevalues(this);
                extractAllAtoms(l, atomes);
                l->release();
            }
            else {
                if (e->isPureAtom()) {
                    atomes.push_back(e);
                }
            }
        }
    }

    inline int16_t extractdynamiclabel(Element* e) {
        while (e->isList()) {
            if (!e->size())
                return v_null;
            e = e->index(0);
        }
        return checkDataStructure(e);
    }

    inline int16_t extractdynamiclabel(Element* e, char depth) {
        while (e->isList() && depth) {
            if (!e->size())
                return v_null;
            e = e->index(0);
            depth--;
        }        
        return checkDataStructure(e);
    }

    //We delve into the argument structure to find the first label
    inline int16_t extractlabel(Element* e, char& depth) {
        while (e->isList()) {
            if (!e->size())
                return v_null;
            e = e->index(0);
            depth++;
        }
                
        return checkDataStructure(e->label());
    }
    
    Element* generate_macro(Element* code);

    inline Element* recordingMacro(Element* e, int16_t label) {
        return delegation->recordingMacro(this, e, label);
    }
    
    inline bool recordargument(Element* e, int16_t label) {
        return execution_stack.back()->recordargument(this, e, label);
    }
    
    inline Element* recordingunique(Element* e, int16_t label) {
        if (!execution_stack.back()->recordingunique(e, label)) {
            std::wstringstream w;
            w << "Error: '" << u_to_w(delegation->code_to_string[label]) << "' has been recorded already";
            throw new Error(w.str());
        }
        return e;
    }
    
    inline Element* record_or_replace(Element* e, int16_t label) {
        return execution_stack.back()->record_or_replace(e->duplicate_constant(this), label);
    }

    inline void reset_in_stack(Element* e, int16_t label) {
        execution_stack.back()->reset_in_stack(e, label);
    }

    inline void reset_in_stack(Element* e, int16_t label, Element* keep) {
        execution_stack.back()->reset_in_stack(e, label, keep);
    }

    inline void recording(Element* e, int16_t label) {
        execution_stack.back()->recording(e->duplicate_constant(this), label);
    }

    inline void record_argument(Element* e, int16_t label) {
        execution_stack.back()->record_argument(e, label);
    }

    inline void replacingvalue(Element* e, int16_t label) {
        execution_stack.back()->replacingvalue(e->duplicate_constant(this), label);
    }

    inline Element* recording_variable(Element* e, int16_t label) {
        return execution_stack.back()->recording_variable(e->duplicate_constant(this), label);
    }

    inline void storing_variable(Element* e, int16_t label) {
        execution_stack.back()->storing_variable(e->duplicate_constant(this), label);
    }

    inline void storing_global(Element* e, int16_t label) {
        execution_stack[0]->storing_variable(e->duplicate_constant(this), label);
    }

    inline void storing_global(wstring label, Element* e) {
        execution_stack[0]->storing_variable(e->duplicate_constant(this), delegation->encode(label));
    }

    inline void storing_global(u_ustring label, Element* e) {
        execution_stack[0]->storing_variable(e->duplicate_constant(this), delegation->encode(label));
    }

    inline void removefromstack(int16_t label) {
        execution_stack.back()->remove(label);
    }

    inline void removefromstack(int16_t label, Element* keep) {
        execution_stack.back()->remove(label, keep);
    }

    inline void shareStackElements(Stackelement* s) {
        if (execution_stack.last != 1)
            s->shareElements(execution_stack.back());
        execution_stack.push_back(s);
    }
    
    inline Element* exchangestackfunction(Element* f) {
        return execution_stack.back()->exchange(f);
    }

    inline void setstackfunction(Element* f) {
        execution_stack.back()->function = f;
    }

    inline void savelocal(Element* e, List* l) {
        execution_stack.back()->savelocal(e, l);
    }

    inline bool localsave(Element* e, List* l) {
        return execution_stack.back()->localsave(e, l);
    }

    inline Element* get_variable(string name) {
        return execution_stack.back()->variables.at(encode(name));
    }

    inline Element* get_variable(wstring name) {
        return execution_stack.back()->variables.at(encode(name));
    }

    inline Element* get_variable(u_ustring name) {
        return execution_stack.back()->variables.at(encode(name));
    }

    inline Element* get_variable(int16_t label) {
        return execution_stack.back()->variables.at(label);
    }

    void create_name_space(int16_t label) {
        if (label < l_final && label != v_mainspace)
            throw new Error("Error: Cannot use this label to define a space");
        
        if (delegation->namespaces.check(label))
            current_space = delegation->namespaces[label];
        else {
            current_space = delegation->function_pool.size();
            delegation->function_pool.push_back(new binHash<Element*>());
            delegation->namespaces[label] = current_space;
        }
    }
    
    inline bool unboundAtomError(int16_t label) {
        u_ustring err = U"Error: Unbound atom: '";
        err += delegation->code_to_string[label];
        err += U"'";
        throw new Error(err);
    }
    

    inline Element* getDataStructure(int16_t label) {
        Element* res;
        delegation->data_pool.search(label, &res) ||
        unboundAtomError(label);
        return res;
    }
    
    inline Element* getvalue(int16_t label) {
        Element* res = n_null;
        execution_stack.back()->variables.search(label, &res) ||
        execution_stack.vecteur[0]->variables.search(label, &res);
        return res;
    }

    inline Element* get(u_ustring name) {
        int16_t label = encode(name);
        Element* res;
        execution_stack.back()->variables.search(label, &res) ||
        execution_stack.vecteur[0]->variables.search(label, &res) ||
        (current_space && delegation->function_pool[current_space]->search(label, &res)) ||
        delegation->function_pool[0]->search(label, &res) ||
        unboundAtomError(label);

        return res;
    }

    inline Element* get(string name) {
        int16_t label = encode(name);
        Element* res;
        execution_stack.back()->variables.search(label, &res) ||
        execution_stack.vecteur[0]->variables.search(label, &res) ||
        (current_space && delegation->function_pool[current_space]->search(label, &res)) ||
        delegation->function_pool[0]->search(label, &res) ||
        unboundAtomError(label);

        return res;
    }


    inline Element* get(int16_t label) {
        Element* res;
        execution_stack.back()->variables.search(label, &res) ||
        execution_stack.vecteur[0]->variables.search(label, &res) ||
        (current_space && delegation->function_pool[current_space]->search(label, &res)) ||
        delegation->function_pool[0]->search(label, &res) ||
        delegation->data_pool.search(label, &res) ||
        unboundAtomError(label);
        
        return res;
    }
    
    inline Element* checkLabel(int16_t label) {
        return execution_stack.back()->get(label);
    }

    inline bool checkFunctionLabel(int16_t label) {
        return delegation->function_pool[current_space]->check(label);
    }
    

    // In this way, we keep elements that we want to destroy at the end of execution.
    //such as for example, the objects corresponding to the compilation of a code
    inline void garbaging(Element* e) {
        //This status is used to avoid the destruction of the objects out of the garbage .
        e->status = s_constant;
        garbages.push_back(e);
    }

    inline Element* push_in_garbage(Element* e) {
        //This status is used to avoid the destruction of the objects out of the garbage .
        e->status = s_constant;
        garbages.push_back(e);
        return e;
    }

    inline void control_garbaging(Element* e) {
        //In this case, we set the garbage to a value, which is neither s_protect nor s_constant
        if (!e->is_protected() && e->garbageable()) {
            e->status = s_constant;
            garbages.push_back(e);
        }
    }


    inline void removefromgarbage(Element* e) {
        if (e == garbages.back()) {
            garbages.pop_back();
            delete e;
            return;
        }
        
        for (long i = garbages.size()-2; i >= 0; i--) {
            if (garbages[i] == e) {
                garbages.erase(garbages.begin()+i);
                delete e;
                return;
            }
        }
    }
    
    Element* provideOperator(int16_t identifier) {
        return delegation->provideOperator(identifier);
    }
    
    Element* provideAtom(int16_t identifier) {
        return delegation->provideAtom(identifier);
    }

    Element* provideAtom(u_ustring identifier)  {
           return delegation->provideAtom(identifier);
    }

    Element* provideAtom(wstring idf)  {
        u_pstring identifier = _w_to_u(idf);
        return delegation->provideAtom(identifier);
    }

    Element* provideAtomProtected(u_ustring& identifier)  {
           return delegation->provideAtom(identifier, threaded());
    }
    
    void replaceAtom(u_ustring& identifier, int16_t code) {
        delegation->replaceAtom(identifier, code, threaded());
    }
    
    Element* provideAtomOrInstruction(int16_t identifier) {
        return delegation->provideAtomOrInstruction(identifier);
    }
    Element* provideAtomOrInstruction(string& identifier) {
        return delegation->provideAtomOrInstruction(identifier);
    }
    
    Element* provideNonLabelAtom(int16_t identifier) {
        return delegation->provideNonLabelAtom(identifier);
    }
    Element* provideCADR(u_ustring& c) {
        return delegation->provideCADR(c);
    }

    inline String* provideConststring(u_ustring& u) {
        try {
            return const_string_pool.at(u);
        }
        catch(...) {
            Conststring* c = new Conststring(u);
            const_string_pool[u] = c;
            return c;
        }
    }

    inline Integer* provideConstinteger(long u) {
        try {
            return const_integer_pool.at(u);
        }
        catch(...) {
            Constinteger* c = new Constinteger(u);
            const_integer_pool[u] = c;
            return c;
        }
    }

    inline Number* provideConstnumber(double u) {
        try {
            return const_number_pool.at(u);
        }
        catch(...) {
            Constnumber* c = new Constnumber(u);
            const_number_pool[u] = c;
            return c;
        }
    }

    inline Quoted* quoted() {
        return quoted_pool.last?quoted_pool.backpop()->set(n_null):new Quotedpool(this, n_null);
    }

    inline Quoted* quoted(Element* v) {
        return quoted_pool.last?quoted_pool.backpop()->set(v):new Quotedpool(this, v);
    }

    inline List* provideCall(Element* op, long nb) {
        List* call = list_pool.last?list_pool.backpop(): new Listpool(this);
        call->append(op);
        while (nb) {
            call->append(quoted());
            nb--;
        }
        return call;
    }
    
    inline List* provideList() {
        return list_pool.last?list_pool.backpop(): new Listpool(this);
    }

    inline Dictionary* provideDictionary() {
        return dictionary_pool.last?dictionary_pool.backpop(): new Dictionarypool(this);
    }

    inline Dictionary_n* provideDictionary_n() {
        return dictionaryn_pool.last?dictionaryn_pool.backpop(): new Dictionary_npool(this);
    }

    inline Dictionary_i* provideDictionary_i() {
        return dictionaryi_pool.last?dictionaryi_pool.backpop(): new Dictionary_ipool(this);
    }

    inline Set_s* provideSet_s() {
        return sets_pool.last?sets_pool.backpop(): new Set_spool(this);
    }

    inline Set_s* provideSet_s(Set_s* e) {
        return sets_pool.last?sets_pool.backpop()->set(e): new Set_spool(this, e);
    }

    inline Set_n* provideSet_n() {
        return setn_pool.last?setn_pool.backpop(): new Set_npool(this);
    }

    inline Set_n* provideSet_n(Set_n* e) {
        return setn_pool.last?setn_pool.backpop()->set(e): new Set_npool(this, e);
    }

    inline Set_i* provideSet_i() {
        return seti_pool.last?seti_pool.backpop(): new Set_ipool(this);
    }

    inline Set_i* provideSet_i(Set_i* e) {
        return seti_pool.last?seti_pool.backpop()->set(e): new Set_ipool(this, e);
    }

    inline Set* provideSet() {
        return set_pool.last?set_pool.backpop(): new Setpool(this);
    }

    inline Set* provideSet(Set* e) {
        return set_pool.last?set_pool.backpop()->set(e): new Setpool(this, e);
    }

    inline Floats* provideFloats() {
        return floats_pool.last?floats_pool.backpop():new Floatspool(this);
    }

    inline Floats* provideFloats(long nb, float v) {
        return floats_pool.last?floats_pool.backpop()->set(nb, v):new Floatspool(this, nb, v);
    }

    inline Floats* provideFloats(Floats* n) {
        return floats_pool.last?floats_pool.backpop()->set(n):new Floatspool(this, n);
    }
    
    inline Floats* provideFloats(Floats* n, long pos) {
        return floats_pool.last?floats_pool.backpop()->set(n, pos):new Floatspool(this, n, pos);
    }

    inline Numbers* provideNumbers() {
        return numbers_pool.last?numbers_pool.backpop():new Numberspool(this);
    }

    inline Numbers* provideNumbers(long nb, double v) {
        return numbers_pool.last?numbers_pool.backpop()->set(nb, v):new Numberspool(this, nb, v);
    }

    inline Numbers* provideNumbers(Numbers* n, long pos) {
        return numbers_pool.last?numbers_pool.backpop()->set(n, pos):new Numberspool(this, n, pos);
    }

    inline Integers* provideIntegers(long nb, long v) {
        return integers_pool.last?integers_pool.backpop()->set(nb, v):new Integerspool(this, nb, v);
    }

    inline Integers* provideIntegers(Integers* n, long pos) {
        return integers_pool.last?integers_pool.backpop()->set(n, pos):new Integerspool(this, n, pos);
    }

    inline Element* provideReturn() {
        return return_pool.last?return_pool.backpop()->set(n_null):new Returnpool(this, n_null);
    }

    inline Element* provideReturn(Element* e) {
        return (e == delegation->_TERMINAL)?e:return_pool.last?return_pool.backpop()->set(e):new Returnpool(this, e);
    }
    
    inline Numbers* provideNumbers(Numbers* n) {
        return numbers_pool.last?numbers_pool.backpop()->set(n):new Numberspool(this, n);
    }

    inline Integers* provideIntegers(Integers* n) {
        return integers_pool.last?integers_pool.backpop()->set(n):new Integerspool(this, n);
    }

    inline Integers* provideIntegers() {
        return integers_pool.last?integers_pool.backpop():new Integerspool(this);
    }
    
    inline Strings* provideStrings() {
        return strings_pool.last?strings_pool.backpop():new Stringspool(this);
    }

    inline Strings* provideStrings(Strings* n) {
        return strings_pool.last?strings_pool.backpop()->set(n):new Stringspool(this, n);
    }

    inline Number* provideNumber(double d) {
        return number_pool.last?number_pool.backpop()->set(d):new Numberpool(this, d);
    }

    inline Float* provideFloat(float d) {
        return float_pool.last?float_pool.backpop()->set(d):new Floatpool(this, d);
    }

    inline Integer* provideInteger(long d) {
        return integer_pool.last?integer_pool.backpop()->set(d):new Integerpool(this, d);
    }

    inline String* provideString(u_ustring& c) {
        return string_pool.last?string_pool.backpop()->set(c):new Stringpool(this, c);
    }

    inline String* provideString() {
        return string_pool.last?string_pool.backpop():new Stringpool(this);
    }
    
    Element* provideString(wstring& c);
    Element* provideString(string& c);
    Element* provideString(wchar_t c);
    Element* provideString(u_uchar c);
    
    bool checkencoding(u_ustring str) {
        try {
            delegation->string_to_code.at(str);
            return true;
        }
        catch (...) {
            return false;
        }
    }
    
    int16_t encode(string& str) {
        return delegation->encode(str);
    }
    
    int16_t encode(u_ustring& s) {
        return delegation->encode(s);
    }
    
    int16_t encode(wstring& w) {
        u_pstring s = _w_to_u(w);
        return delegation->encode(s);
    }
    
    int16_t encode(wchar_t c) {
        return delegation->encode(c);
    }
    
};


#endif


