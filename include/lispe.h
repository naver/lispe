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
#include "elements.h"
#include "tools.h"
#include "stack.h"
#include "delegation.h"
#include <stack>

//------------------------------------------------------------
string LispVersion();
//------------------------------------------------------------
// The main class to handle the Lisp Interpreter
//------------------------------------------------------------

class LispE {
    /* Methods in LispE
     //check if this string has already been identified with a code
     bool checkencoding(wstring str);
     
     //returns true is str or label is a valid instruction name
     bool is_instruction(short label);
     bool is_instruction(string str);
     bool is_instruction(wstring s);
     
     //returns true is the c is an operator code
     bool is_operator(short c);
     
     //Tokenize the code and stores the result in a Tokenize object.
     e_type segmenting(string& code, Tokenizer& s);
     
     //Build the execution tree
     Element* abstractSyntaxTree(Element* courant, Tokenizer& s, long& index);
     
     //Transforms a string in a list of atoms
     Element* atomise(wstring a);
     
     //Compile the code: calls first segmenting then abstractSyntaxTree
     Element* compile(string& code);
     
     //eval a string. This is a dynamic evaluation of a string while executing a program
     //This method is called when the function eval is called in your code: (eval str)
     Element* eval(string);
     
     //compiles and executes some code
     Element* execute(string code);
     
     //compiles and executes some code from a file
     Element* execute(string code, string path_name);
     
     //records an extension defintion of the form: (deflib (p1...pn) Object)
     Element* extension(string, Element*);
     
     //load a file in memory, compiles it and returns the evaluation
     Element* load(string chemin);
     
     //load an external library
     Element* load_library(string nom);
     
     //These methods are used to handle data pools
     Element* provideAtom(short identifier);
     Element* provideAtom(wstring identifier);
     Element* provideAtomOrInstruction(short identifier);
     Element* provideCADR(string& c);
     Element* provideInteger(Integer* n, long d);
     Element* provideInteger(long d);
     Element* provideNonLabelAtom(short identifier);
     Element* provideNumber(double d);
     Element* provideNumber(Number* n, double d);
     Element* provideOperator(short identifier);
     Element* provideString(string& c);
     Element* provideString(String* c, wstring& w);
     Element* provideString(wchar_t c);
     Element* provideString(wstring& c);
     
     //records an argument in the stack, with unification
     inline bool recordargument(Element* e, short label);
     
     //returns the top function on the stack
     inline Element* called();
     
     //gets the value of a variable
     inline Element* get(short label);
     inline Element* get(string nom);
     inline Element* get(wstring nom);
     
     //returns a data structure element known through its label
     inline Element* getDataStructure(short label);
     
     //Returns list of functions sharing the same name
     //These methods have been recorded using defpat...
     inline Element* getMethod(short label, short sublabel, long i);
     
     //Removes the top of the stack
     inline Element* pop(Element* e);
     
     //Records an element in the stack
     inline Element* recording(Element* e, short label);
     
     //Records a data structure definition 
     inline Element* recordingData(Element* e, short label);
     
     //records a variable as a global variable in Stackelement at position 0
     inline Element* recordingglobal(Element* e, short label);
     
     //Records a new method defined with defpat
     inline Element* recordingMethod(Element* e, short label);
     
     //Records an element in the stack. Fails if an element has already
     //been stored for the same label
     inline Element* recordingunique(Element* e, short label);
     
     //returns the list of all atoms in memory
     inline List* atomes();
     
     //Returns the number of variables recorded on the top of the stack
     inline long nbvariables();
     
     //Returns the size of stack
     inline long stackSize();
     
     //Deletes the top of the stack
     inline void cleanTopStack();
     
     //Delete the current stack
     inline void clearStack();
     
     //Stores 'e' in the garbage list
     inline void garbaging(Element* e);
     
     //Removes the top of the stack
     inline void pop();
     
     //Pushes a new stack element on the top of the stack
     //Called when a new function is executed.
     inline void push(Element* fonction);
     
     //Returns the string associated to a label
     inline wstring asString(short c);
     
     //Creates an new list containing a full instruction of at most four elements
     //This method is used in composing
     List* create_instruction(short label, Element* e = NULL, Element* ee = NULL, Element* eee = NULL, Element* eeee = NULL);
     
     //encode a string and returns its code
     short encode(string& str);
     short encode(wchar_t c);
     short encode(wstring& s);
     
     //stores the arguments on the command line into an _args list
     void arguments(std::vector<string>& args);
     
     //Cleans all the elements recorded in the pools and the garbage
     void cleaning();
     
     //Initialize the _current variable with the path of the top file being executed
     void current_path();
     
     //Displays the current instruction being executed on screen, as a trace to the full execution
     void display_trace(List*);
     
     //Initialisation of all structures and variables to launch a lisp interpreter
     void initialisation();
     
     //Keeps track of the current line in the initial file corresponding to the current instruction
     void set_current_line(List* l);
     
     //initialise an instruction with its label and its arity (the number of arguments it accepts)
     void set_instruction(short instruction_code, string name,  unsigned long arity);
     
     //Called when Ctrl-c has been pressed
     void stop();
     
     //Returns the content of the top of the stack as a string
     wstring stackAsString();
     */
    
    vector<Element*> garbages;
    vector<Stackelement*> stack_pool;

    std::stack<Stackelement*> execution_stack;

public:
    unordered_map<wstring, Element*> pools;

    unordered_map<double, Number*> number_pool;
    unordered_map<long, Integer*> integer_pool;
    unordered_map<wstring, String*> string_pool;
    
    
    //Delegation is a class that records any data
    //related to compilation
    Delegation* delegation;
    Chaine_UTF8* handlingutf8;

    List* current_thread;
    Element* current_body;
    LispE* thread_ancestor;

    std::atomic<short> nbjoined;

    long line_error;
    
    bool isThread;
    bool hasThread;
    bool trace;

    LispE() {
        line_error = -1;
        trace = false;
        delegation = new Delegation;
        isThread = false;
        hasThread = false;
        thread_ancestor = NULL;
        nbjoined = 0;
        current_thread = NULL;
        current_body = NULL;
        handlingutf8 = new Chaine_UTF8;
        delegation->initialisation(this);
    }
    
    LispE(LispE*, List* function, Element* body);
    
    ~LispE() {
        cleaning();
    }
    
    Stackelement* provideStackElement(Element* function) {
        try {
            return stack_pool.at(execution_stack.size())->setFunction(function);
        }
        catch(const std::out_of_range& oor) {
            for (long i = 0; i < 32; i++) {
                stack_pool.push_back(new Stackelement(NULL));
            }
            return stack_pool[execution_stack.size()]->setFunction(function);
        }
    }
    
    void removeStackElement() {
        execution_stack.pop();
        stack_pool[execution_stack.size()]->clear();
    }
    
    inline bool checkforLock() {
        return (hasThread || isThread);
    }
    
    void lock() {
        delegation->lock.locking(checkforLock());
    }
    
    void unlock() {
        delegation->lock.unlocking(checkforLock());
    }
    
    void stop_at_next_line() {
        delegation->next_stop = true;
    }
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
        catch(const std::out_of_range& oor) {
            return delegation->_NULL;
        }
    }
    
    
  
    inline void set_current_line(long l, long f) {
        delegation->current_line = l;
        delegation->i_current_file = f;
    }
    
    inline void clear_breakpoints() {
        delegation->clear_breakpoints();
    }
    
    long id_file(string pathname) {
        return delegation->id_file(pathname);
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
    
    inline bool is_instruction(wstring s) {
        return delegation->is_instruction(s);
    }
    
    inline bool is_operator(short c) {
        return delegation->is_operator(c);
    }
    
    inline bool is_math_operator(short c) {
        return delegation->is_math_operator(c);
    }
    
    short is_atom(wstring& s) {
        return delegation->is_atom(s);
    }
    
    short is_atom(string str) {
        wstring s;
        s_utf8_to_unicode(s, USTR(str), str.size());
        return delegation->is_atom(s);
    }
    
    Element* load_library(string nom);
    Element* extension(string, Element*);
    Element* eval(string);
    Element* eval(wstring& w) {
        string s;
        s_unicode_to_utf8(s, w);
        return eval(s);
    }
    
    Element* atomise(wstring a);
    void cleaning();
    
    Element* execute(string code);
    Element* execute(string code, string path_name);
    
    Element* compile(string& code);
    Element* load(string chemin);
    e_type segmenting(string& code, Tokenizer& s);
    Element* abstractSyntaxTree(Element* courant, Tokenizer& s, long& index);
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
        return delegation->max_stack_size;
    }
    
    void set_debug_function (lispe_debug_function ldf, void* o) {
        delegation->debugfunction = ldf;
        delegation->debugobject = o;
    }

    void set_stack_max_size(long m) {
        if (m <= 0)
            return;
        delegation->max_stack_size = m;
    }
    
    inline void stop() {
        delegation->stop_execution = 0x100;
    }

    inline short hasStopped() {
        return delegation->stop_execution;
    }
    
    inline void clearStop() {
        delegation->stop_execution = 0;
    }
    
    inline short checkLispState() {
        return delegation->checkLispState(execution_stack.size());
    }
        
    void display_trace(List*);
    
    inline wstring asString(short c) {
        return delegation->asString(c);
    }

    inline long nbvariables() {
        return execution_stack.top()->size();
    }
    
    inline long stackSize() {
        return execution_stack.size();
    }
    
    wstring stackAsString() {
        return (execution_stack.size() == 1?L"":execution_stack.top()->asString(this));
    }
    
    inline Element* called() {
        return execution_stack.top()->called();
    }
    
    inline void push(Element* fonction) {
        execution_stack.push(provideStackElement(fonction));
    }
    
    inline List* atomes() {
        return execution_stack.top()->atomes(this);
    }
    
    inline void pop() {
        removeStackElement();
    }
    
    inline Element* pop(Element* e) {
        e->incrementstatus(1, true);
        removeStackElement();
        e->decrementstatusraw(1);
        return e;
    }

    //We clear the stack up to upto
    inline Element* pop(Element* e, long upto) {
        e->incrementstatus(1, true);
        while (execution_stack.size() != upto) {
            removeStackElement();
        }
        e->decrementstatusraw(1);
        return e;
    }
    
    inline void clearStack() {
        for (auto& a: stack_pool)
            delete a;
    }
    
    inline void cleanTopStack() {
        while (execution_stack.size() > 1) {
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
        return delegation->recordingMethod(execution_stack.top(), e, label, sublabel);
    }
    
    inline Element* recordingData(Element* e, short label, short ancestor) {
        return delegation->recordingData(e, label, ancestor);
    }
    
    inline bool checkAncestor(Element* ancestor, Element* label) {
        return delegation->checkAncestor(ancestor, label);
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
        Element* e = delegation->getDataStructure(label);
        if (e == NULL) {
            wstring msg = L"Error: Unbounded atom: '";
            msg += delegation->code_to_string[label];
            msg += L"'";
            throw new Error(msg);
        }
        return e;
    }
    
    Element* generate_macro(Element* code);

    inline Element* recordingMacro(Element* e, short label) {
        return delegation->recordingMacro(this, e, label);
    }
    
    inline bool recordargument(Element* e, short label) {
        return execution_stack.top()->recordargument(this, e, label);
    }
    
    inline Element* recordingunique(Element* e, short label) {
        if (!execution_stack.top()->recordingunique(e, label)) {
            std::wstringstream w;
            w << "Error: '" << delegation->code_to_string[label] << "' has been recorded already";
            throw new Error(w.str());
        }
        return e;
    }
    
    inline Element* recording(Element* e, short label) {
        return execution_stack.top()->recording(e, label);
    }
    
    inline Element* recordingglobal(Element* e, short label) {
        return stack_pool[0]->recording(e, label);
    }
    
    inline void removefromstack(short label) {
        execution_stack.top()->remove(label);
    }
    
    inline Element* get(wstring nom) {
        short label = encode(nom);
        Element* e = execution_stack.top()->get(label);
        if (e == NULL) {
            //Is it a global variable?
            if (execution_stack.size() == 1) {
                wstring err = L"Error: unknown label: '";
                err += nom;
                err += L"'";
                throw new Error(err);
            }
            
            e = stack_pool[0]->get(label);
            if (e == NULL) {
                wstring err = L"Error: unknown label: '";
                err += nom;
                err += L"'";
                throw new Error(err);
            }
        }
        return e;
    }
    
    inline Element* get(string nom) {
        short label = encode(nom);
        Element* e = execution_stack.top()->get(label);
        if (e == NULL) {
            //Is it a global variable?
            if (execution_stack.size() == 1) {
                string err = "Error: unknown label: '";
                err += nom;
                err += "'";
                throw new Error(err);
            }
            
            e = stack_pool[0]->get(label);
            if (e == NULL) {
                string err = "Error: unknown label: '";
                err += nom;
                err += "'";
                throw new Error(err);
            }
        }
        return e;
    }
    
    inline Element* get(short label) {
        Element* e = execution_stack.top()->get(label);
        if (e == NULL) {
            //Is it a global variable?
            if (execution_stack.size() == 1) {
                try {
                    return delegation->data_pool.at(label);
                }
                catch(const std::out_of_range& oor) {
                    wstring nom = delegation->code_to_string[label];
                    wstring err = L"Error: unknown label: '";
                    err += nom;
                    err += L"'";
                    throw new Error(err);
                }
            }
            
            e = stack_pool[0]->get(label);
            if (e == NULL) {
                try {
                    return delegation->data_pool.at(label);
                }
                catch(const std::out_of_range& oor) {
                    wstring nom = delegation->code_to_string[label];
                    wstring err = L"Error: unknown label: '";
                    err += nom;
                    err += L"'";
                    throw new Error(err);
                }
            }
        }
        return e;
    }
    
    inline Element* getvalue(short label) {
        Element* e = execution_stack.top()->get(label);
        if (e == NULL) {
            if (execution_stack.size() == 1)
                return delegation->_NULL;            
            e = stack_pool[0]->get(label);
            if (e == NULL)
                return delegation->_NULL;
        }
        return e;
    }
    
    inline Element* checkLabel(short label) {
        return execution_stack.top()->get(label);
    }

    inline bool checkFunctionLabel(short label) {
        Element* func = stack_pool[0]->get(label);
        return (func != NULL && func->isFunction());
    }
    

    // In this way, we keep elements that we want to destroy at the end of execution.
    //such as for example, the objects corresponding to the compilation of a code
    inline void garbaging(Element* e) {
        //This status is used to avoid the destruction of the objects out of the garbage .
        e->status = s_constant;
        garbages.push_back(e);
    }

    inline Element* regarbaging(Element* e, Element* v) {
        if (v == e)
            return v;
        //This status is used to avoid the destruction of the objects out of the garbage .
        v->status = s_constant;
        for (long i = garbages.size()-1; i >= 0; i--) {
            if (garbages[i] == e) {
                garbages[i] = v;
                delete e;
                return v;
            }
        }
        garbages.push_back(v);
        return v;
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

    Element* provideAtom(wstring identifier)  {
           return delegation->provideAtom(identifier);
    }
    
    Element* provideAtomProtected(wstring& identifier)  {
           return delegation->provideAtom(identifier, checkforLock());
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
    Element* provideCADR(string& c) {
        return delegation->provideCADR(c);
    }
    
    Element* provideNumber(double d);
    Element* provideInteger(long d);
    Element* provideString(wstring& c);
    Element* provideString(string& c);
    Element* provideString(wchar_t c);
    
    bool checkencoding(wstring str) {
        try {
            delegation->string_to_code.at(str);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }
    
    short encode(string& str) {
        return delegation->encode(str);
    }
    
    short encode(wstring& s) {
        return delegation->encode(s);
    }
    
    short encode(wchar_t c) {
        return delegation->encode(c);
    }
    
};


#endif


