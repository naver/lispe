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

    vecte<Setpool*> set_pool;
    vecte<Set_npool*> setn_pool;

    vecte<Dictionarypool*> dictionary_pool;
    vecte<Dictionary_npool*> dictionaryn_pool;

    vecte<Listpool*> list_pool;
    
    vecte<Returnpool*> return_pool;

    unordered_map<wstring, Element*> pools;
    vector<Element*> vpools;
    
    unordered_map<u_ustring, String*> const_string_pool;
    unordered_map<long, Integer*> const_integer_pool;
    unordered_map<double, Number*> const_number_pool;
    
    //Delegation is a class that records any data
    //related to compilation
    Delegation* delegation;
    Chaine_UTF8* handlingutf8;

    List* current_thread;
    List* current_body;
    LispE* thread_ancestor;
    
    Element* n_null;
    Element* n_true;
    Element* n_zero;
    Element* n_one;
    
    
    std::atomic<short> nbjoined;

    long id_thread;
    
    char trace;
    bool isThread;
    bool hasThread;
    bool evaluating;
    bool preparingthread;
    
    LispE() {
        initpools(this);
        preparingthread = false;
        evaluating = false;
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
        handlingutf8 = new Chaine_UTF8;
        delegation->initialisation(this);
        n_null = delegation->_NULL;
        n_true = delegation->_TRUE;
        n_zero = delegation->_ZERO;
        n_one = delegation->_ONE;
    }
    
    inline void initpools(LispE* lisp) {
        for (long i = 0; i < 50; i++) {
            number_pool.push_back(new Numberpool(lisp, 0));
            integer_pool.push_back(new Integerpool(lisp, 0));
            numbers_pool.push_back(new Numberspool(lisp));
        }
    }
    inline bool checkArity(List* l) {
        return (!evaluating || delegation->checkArity(l->liste[0]->type, l->size()));
    }
    
    LispE(LispE*, List* function, List* body);
    
    ~LispE() {
        cleaning();
    }
    
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
            trace = 0;
            delegation->next_stop = false;
            return;
        }
        trace = tr;
        delegation->next_stop = true;
    }
    
    void add_pathname(string pathname);
    void set_pathname(string pathname);
    
    void add_to_listing(long l, string& e) {
        delegation->addtolisting(l, e);
    }
    
    List* create_instruction(short label,
                             Element* e = NULL,
                             Element* ee = NULL,
                             Element* eee = NULL,
                             Element* eeee = NULL,
                             Element* eeeee = NULL,
                             Element* eeeeee = NULL,
                             Element* eeeeeee = NULL);

    List* create_local_instruction(short label,
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
    
    inline bool is_instruction(short c) {
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
    
    inline bool is_operator(short c) {
        return delegation->is_operator(c);
    }
    
    inline bool is_math_operator(short c) {
        return delegation->is_math_operator(c);
    }
    
    short is_atom(u_ustring& s) {
        return delegation->is_atom(s);
    }
    
    short is_atom(string str) {
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
    
    Element* compile(string& code);
    Element* load(string chemin);
    lisp_code segmenting(string& code, Tokenizer& s);
    Element* tokenize(wstring& code, bool keepblanks = false);
    Element* abstractSyntaxTree(Element* courant, Tokenizer& s, long& index, bool quoting);
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
    
    inline short checkLispState() {
        return (execution_stack.last >= max_stack_size?0x200:delegation->stop_execution);
    }
        
    inline bool sendError() {
        throw delegation->_THEEND;
    }
    
    inline bool sendError(u_ustring msg) {
        throw new Error(msg);
    }
    
    inline short checkState(List* l) {
        (
         (
          l->size() &&
          (
           !evaluating ||
           delegation->checkArity(l->liste.get0(), l->size()) ||
           sendError(U"Error: wrong number of arguments")
           )
          ) ||
         sendError()
         );
        return l->liste.get0();
    }

    inline void trace_and_context(List* e) {
        //in the case of a goto, we only take into account breakpoints
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

    inline string toString(short c) {
        return delegation->toString(c);
    }

    inline wstring asString(short c) {
        return delegation->asString(c);
    }

    inline u_ustring asUString(short c) {
        return delegation->asUString(c);
    }

    inline long nbvariables() {
        return execution_stack.back()->size();
    }
    
    inline void atomsOnStack(vector<Element*>& v_atoms) {
        vector<short> labels;
        execution_stack.back()->atoms(labels);
        for (short i = 0; i < labels.size(); i++) {
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
            short label = e->index(0)->label();
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
    
    inline Element* recordingMethod(Element* e, short label) {
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
        short sublabel = extractlabel(e->index(2));
        if (globalDeclaration())
            return delegation->recordingMethod(NULL, e, label, sublabel);
        return delegation->recordingMethod(execution_stack.back(), e, label, sublabel);
    }
    
    inline Element* recordingData(Element* e, short label, short ancestor) {
        return delegation->recordingData(e, label, ancestor);
    }
    
    inline bool checkAncestor(Element* ancestor, Element* label) {
        return delegation->checkAncestor(ancestor, label->label());
    }
    
    inline Element* getMethod(short label, short sublabel, long i) {
        return delegation->getMethod(label, sublabel, i);
    }
        
    inline short checkDataStructure(short label) {
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

    //We delve into the argument structure to find the first label
    inline short extractlabel(Element* e) {
        while (e->isList()) {
            if (!e->size())
                return v_null;
            e = e->index(0);
        }
        return checkDataStructure(e->label());
    }
    
    inline Element* getDataStructure(short label) {
        if (!delegation->data_pool.check(label)) {
            u_ustring msg = U"Error: Unbounded atom: '";
            msg += delegation->code_to_string[label];
            msg += U"'";
            throw new Error(msg);
        }
        return delegation->data_pool.at(label);
    }
    
    Element* generate_macro(Element* code);

    inline Element* recordingMacro(Element* e, short label) {
        return delegation->recordingMacro(this, e, label);
    }
    
    inline bool recordargument(Element* e, short label) {
        return execution_stack.back()->recordargument(this, e, label);
    }
    
    inline Element* recordingunique(Element* e, short label) {
        if (!execution_stack.back()->recordingunique(e, label)) {
            std::wstringstream w;
            w << "Error: '" << u_to_w(delegation->code_to_string[label]) << "' has been recorded already";
            throw new Error(w.str());
        }
        return e;
    }
    
    inline void recording(Element* e, short label) {
        execution_stack.back()->recording(e, label);
    }

    inline void record_argument(Element* e, short label) {
        execution_stack.back()->record_argument(e, label);
    }

    inline void replacingvalue(Element* e, short label) {
        execution_stack.back()->replacingvalue(e, label);
    }

    inline Element* recording_variable(Element* e, short label) {
        return execution_stack.back()->recording_variable(e, label);
    }

    inline void storing_variable(Element* e, short label) {
        execution_stack.back()->storing_variable(e, label);
    }

    inline void storing_global(Element* e, short label) {
        execution_stack[0]->storing_variable(e, label);
    }

    inline void storing_global(wstring label, Element* e) {
        execution_stack[0]->storing_variable(e, delegation->encode(label));
    }

    inline void storing_global(u_ustring label, Element* e) {
        execution_stack[0]->storing_variable(e, delegation->encode(label));
    }

    inline void removefromstack(short label) {
        execution_stack.back()->remove(label);
    }

    inline void removefromstack(short label, Element* keep) {
        execution_stack.back()->remove(label, keep);
    }

    inline void shareStackElements(Stackelement* s) {
        if (execution_stack.last != 1)
            s->shareElements(execution_stack.back());
        execution_stack.push_back(s);
    }
    
    inline Element* getElementFromStack(Stackelement* current, short label) {
        if (current->variables.check(label))
            return current->variables.at(label);
        return execution_stack.vecteur[0]->variables.search(label);
    }
    
    inline Element* get(u_ustring name) {
        short label = encode(name);
        Element* e = getElementFromStack(execution_stack.back(), label);
        if (e == NULL) {
            if (delegation->function_pool.check(label))
                return delegation->function_pool.at(label);
            
            u_ustring err = U"Error: unknown label: '";
            err += name;
            err += U"'";
            throw new Error(err);
        }
        return e;
    }

    inline Element* get(string name) {
        short label = encode(name);
        Element* e = getElementFromStack(execution_stack.back(), label);
        if (e == NULL) {
            if (delegation->function_pool.check(label))
                return delegation->function_pool.at(label);
            
            string err = "Error: unknown label: '";
            err += name;
            err += "'";
            throw new Error(err);
        }
        return e;
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

    inline Element* get_variable(short label) {
        return execution_stack.back()->variables.at(label);
    }

    inline Element* get(short label) {
        if (execution_stack.back()->variables.check(label))
            return execution_stack.back()->variables.at(label);
        
        if (execution_stack.vecteur[0]->variables.check(label))
            return execution_stack.vecteur[0]->variables.at(label);
        
        if (delegation->function_pool.check(label))
            return delegation->function_pool.at(label);
        
        if (delegation->data_pool.check(label))
            return delegation->data_pool.at(label);
        
        u_ustring name = delegation->code_to_string[label];
        u_ustring err = U"Error: unknown label: '";
        err += name;
        err += U"'";
        throw new Error(err);
    }

    
    inline Element* getvalue(short label) {
        Element* e = getElementFromStack(execution_stack.back(), label);
        if (e == NULL)
            return n_null;
        return e;
    }

    inline Element* checkLabel(short label) {
        return execution_stack.back()->get(label);
    }

    inline bool checkFunctionLabel(short label) {
        return delegation->function_pool.check(label);
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
    
    Element* provideOperator(short identifier) {
        return delegation->provideOperator(identifier);
    }
    
    Element* provideAtom(short identifier) {
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
    
    void replaceAtom(u_ustring& identifier, short code) {
        delegation->replaceAtom(identifier, code, threaded());
    }
    
    Element* provideAtomOrInstruction(short identifier) {
        return delegation->provideAtomOrInstruction(identifier);
    }
    Element* provideAtomOrInstruction(string& identifier) {
        return delegation->provideAtomOrInstruction(identifier);
    }
    
    Element* provideNonLabelAtom(short identifier) {
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

    inline List* provideList() {
        return list_pool.last?list_pool.backpop(): new Listpool(this);
    }

    inline List* provideQuoted(Element* e) {
        List* l = list_pool.last?list_pool.backpop(): new Listpool(this);
        l->append(delegation->_QUOTE);
        l->append(e);
        return l;
    }

    inline Dictionary* provideDictionary() {
        return dictionary_pool.last?dictionary_pool.backpop(): new Dictionarypool(this);
    }

    inline Dictionary_n* provideDictionary_n() {
        return dictionaryn_pool.last?dictionaryn_pool.backpop(): new Dictionary_npool(this);
    }

    inline Set* provideSet() {
        return set_pool.last?set_pool.backpop(): new Setpool(this);
    }

    inline Set* provideSet(Set* e) {
        return set_pool.last?set_pool.backpop()->set(e): new Setpool(this, e);
    }

    inline Set_n* provideSet_n() {
        return setn_pool.last?setn_pool.backpop(): new Set_npool(this);
    }

    inline Set_n* provideSet_n(Set_n* e) {
        return setn_pool.last?setn_pool.backpop()->set(e): new Set_npool(this, e);
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

    inline Numbers* provideNumbers() {
        return numbers_pool.last?numbers_pool.backpop():new Numberspool(this);
    }

    inline Numbers* provideNumbers(long nb, double v) {
        return numbers_pool.last?numbers_pool.backpop()->set(nb, v):new Numberspool(this, nb, v);
    }

    inline Integers* provideIntegers(long nb, long v) {
        return integers_pool.last?integers_pool.backpop()->set(nb, v):new Integerspool(this, nb, v);
    }

    inline Returnpool* provideReturn(Element* e) {
        return return_pool.last?return_pool.backpop()->set(e):new Returnpool(this, e);
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
    
    short encode(string& str) {
        return delegation->encode(str);
    }
    
    short encode(u_ustring& s) {
        return delegation->encode(s);
    }
    
    short encode(wstring& w) {
        u_pstring s = _w_to_u(w);
        return delegation->encode(s);
    }
    
    short encode(wchar_t c) {
        return delegation->encode(c);
    }
    
};


#endif


