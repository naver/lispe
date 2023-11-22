// LispE
// Copyright 2020-present NAVER Corp.
// The 3-Clause BSD License
//
//  delegation.h
//
//

#ifndef delegation_h
#define delegation_h

#include <thread>
#include <mutex>
#include <atomic>
#include <condition_variable>
#include "mapbin.h"
#include "tokens.h"

//------------------------------------------------------------
class LispE;
class jag_get;
typedef bool (*lispe_debug_function)(LispE*, List* e, void*);
//------------------------------------------------------------
class BlockThread {
public:
    std::condition_variable lock;
    std::mutex mtx;
    std::atomic<char> used;

    BlockThread() {
        used = 0;
    }

    char check() {
        return used;
    }
    
    void blocked() {
        std::unique_lock<std::mutex> lck(mtx);
        used = 1;
        lock.wait(lck);
        used = 2;
    }

    void released() {
        if (used == 1)
            lock.notify_all();
    }

    ~BlockThread() {
        if (used == 1)
            lock.notify_all();
    }
};

class ThreadLock {
public:
    std::recursive_mutex* lock;

    inline ThreadLock() {
        lock = new std::recursive_mutex;
    }

    inline void locking(bool check) {
        if (check)
            lock->lock();
    }
    
    inline void unlocking(bool check) {
        if (check)
            lock->unlock();
    }

    inline void locking() {
        lock->lock();
    }
    
    inline void unlocking() {
        lock->unlock();
    }
    
    ~ThreadLock() {
        delete lock;
    }
    
};

//------------------------------------------------------------
class ThreadElement {
public:
    std::atomic<bool> cleaning;
    std::thread* thid;
    LispE* thread_lisp;
    
    ThreadElement(LispE* l) {
        thread_lisp = l;
        cleaning = false;
    }
    ~ThreadElement() {
        if (cleaning == true) {
            thid->join();
            delete thid;
        }
    }
};

//------------------------------------------------------------
//Delegation is common to all threads
//It basically stores everything that is common to all threads
//and won't budge after loading or compilation
//The only exception is when creating new atoms
typedef void (*reading_string)(string&, void*);

class Delegation {
public:
    BlockThread trace_lock;
    ThreadLock lock;
    ThreadLock lock_thread;

    methodEval evals[l_final];
    Listincode* straight_eval[l_final];
    
    binHashe<u_ustring> code_to_string;
    binHashe<string> instructions;
    
    binHash<int16_t> data_ancestor;
    vecte<binHash<Element*>* > function_pool;
    vecte<binHash<Element*>* > bodies;
    vecte<unordered_map<int16_t, unordered_map<int16_t, vector<Element*> > >* > method_pool;

    std::vector<ThreadElement*> currentthreads;
    
    binHash<Element*> data_pool;
    vecte<long> idxinfos;
    
    binSet assignors;
    binSet operators;
    binSet math_operators;
    binSet comparators;
    binSet logicals;
    binSet const_values;
    
    binhash<Element*> atom_pool;

    binHash<Element*> operator_pool;
    binSet atom_basic_pool;
    binHash<unsigned long> arities;
    binHash<Element*> macros;

    binSet number_types;
    
    Stackelement thread_stack;
    lispe_tokenizer main_tokenizer;
    
    //------------------------------------------
    binHash<uint16_t> namespaces;
    unordered_map<u_ustring, int16_t> string_to_code;
    unordered_map<int16_t, vector<int16_t> > data_descendant;

    unordered_map<string, bool> libraries;
    unordered_map<string, long> allfiles;
    unordered_map<long, Element*> entrypoints;

    unordered_map<u_ustring, ThreadLock*> locks;
    unordered_map<u_ustring, BlockThread*> waitons;
    
    unordered_map<u_ustring, List> thread_pool;

    //this is an all-purpose pool for internal usage

    unordered_map<long, string> allfiles_names;
    unordered_map<long, unordered_map<long, bool> > breakpoints;
    map<long, map<long, string> > listing;
    vector<Element*> force_clean;

    std::atomic<long> id_pool;

    Error* current_error;
    
    reading_string reading_string_function;
    reading_string display_string_function;
    void* reading_string_function_object;
    
    Element* _NUMERICAL_BOOLEANS[2];
    Element* _COMPARE_BOOLEANS[3];
    
    Atome* _ERROR;
    Atome* _TERMINAL;
    Atome* _TRUE;
    Atome* _NULL;
    Atome* _EMPTYATOM;
    Atome* _LISTSEPARATOR;
    Atome* _DEFPAT;
    Atome* _DICO_INTEGER;
    Atome* _DICO_NUMBER;
    Atome* _DICO_STRING;
    Atome* _SET_NUMBERS;
    Atome* _SET_INTEGERS;
    Atome* _SET_STRINGS;
    Atome* _QUOTE;
    Atome* _SET_AT;
    Element* _BREAK;
    Listbreak _BREAKEVAL;
    
    List* _EMPTYLIST;
    
    Dictionary* _EMPTYDICTIONARY;
    
    Integer* _ZERO;
    Integer* _ONE;
    Integer* _TWO;
    Integer* _MINUSONE;
    
    String* _EMPTYSTRING;
    
    Error* _THEEND;

    jag_get* input_handler;
    
    lispe_debug_function debugfunction;
    void* debugobject;

    long i_current_line;
    long i_current_file;
    int16_t stop_execution;

    bool next_stop;
    char add_to_listing;
    uint32_t mark;
    
    std::atomic<bool> endtrace;
    bool trace_on;
    
    Delegation();
    
    void initialisation(LispE*);
    
    void blocking() {
        trace_lock.blocked();
    }
    
    void releasing() {
        trace_lock.released();
    }
    
    char checking() {
        return trace_lock.check();
    }
    
    void clean_threads(ThreadElement* the = NULL) {
        lock_thread.locking(true);
        long sz = currentthreads.size() - 1;
        for (long i = sz; i >= 0; i--) {
            if (currentthreads[i] != NULL && currentthreads[i]->cleaning) {
                ThreadElement* te =  currentthreads[i];
                if (i == sz) {
                    currentthreads.pop_back();
                    sz--;
                }
                else
                    currentthreads.erase(currentthreads.begin()+i);
                delete te;
            }
        }
        if (the != NULL)
            currentthreads.push_back(the);
        lock_thread.unlocking(true);
    }
    
    void toBeCleanedOnError(Element* e, bool tobelocked) {
        lock.locking(tobelocked);
        force_clean.push_back(e);
        lock.unlocking(tobelocked);
    }
    
    void forceClean() {
        for (const auto& e: force_clean) {
            if (e != NULL)
                e->clean();
        }
    }
    
    void removeFromForceClean(Element* e, bool tobelocked) {
        lock.locking(tobelocked);
        for (long i = 0; i < force_clean.size(); i++) {
            if (force_clean[i] == e) {
                force_clean[i] = NULL;
            }
        }
        lock.unlocking(tobelocked);
    }
    
    char checkComparator(int16_t type, int16_t root) {
        if (root == type) {
            if (operators.check(type))
                return -1;
            return 0;
        }
        
        if (assignors.check(root))
            return 0;
        
        if (logicals.check(root))
            return logicals.check(type);
        
        if (comparators.check(root)) {
            return (comparators.check(type) || logicals.check(type));
        }
        
        return (type == l_plus || type == l_minus || comparators.check(type) || logicals.check(type));
    }

    int set_idx_info(long line) {
        int idx = (int)idxinfos.size();
        if (idx) {
            if (idxinfos[idx - 2] == line && idxinfos[idx - 1] == i_current_file)
                return idx;
        }
        
        idxinfos.push_back(line);
        idxinfos.push_back(i_current_file);
        return idx;
    }
    
    bool check_straight(int16_t type) {
        return (type < l_final && straight_eval[type] != NULL);
    }
    
    bool isComparator(int16_t type) {
        return comparators.check(type);
    }

    inline string toString(int16_t c) {
        string s;
        if (code_to_string.check(c)) {
            u_ustring w = code_to_string.at(c);
            s_unicode_to_utf8(s, w);
            return s;
        }
        return "nil";
    }

    inline wstring asString(int16_t c) {
        if (code_to_string.check(c))
            return u_to_w(code_to_string.at(c));
        return L"nil";
    }

    inline u_ustring asUString(int16_t c) {
        if (code_to_string.check(c))
            return code_to_string.at(c);
        return U"nil";
    }

    bool isNumberType(int16_t u) {
        return number_types.check(u);
    }
    
    void updatepathname(string& pathname) {
        const auto& a = allfiles.find(pathname);
        if (a == allfiles.end()) {
            i_current_file = allfiles.size();
            allfiles[pathname] = i_current_file;
            allfiles_names[i_current_file] = pathname;
        }
        else
            i_current_file = a->second;
    }
    
    void addtolisting(long l, string& e) {
        if (!listing.count(i_current_file) || !listing[i_current_file].count(l))
            listing[i_current_file][l] = e;
    }
    
    bool isEndTrace() {
        return endtrace;
    }
        
    int16_t encode(string& str) {
        u_ustring s;
        s_utf8_to_unicode(s, str, str.size());
        const auto& e = string_to_code.find(s);
        if (e == string_to_code.end()) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
        return e->second;
    }

    int16_t encode(wstring& w) {
        u_pstring s = _w_to_u(w);
        const auto& e = string_to_code.find(s);
        if (e == string_to_code.end()) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
        return e->second;
    }

    int16_t encode(u_ustring& s) {
        const auto& e = string_to_code.find(s);
        if (e == string_to_code.end()) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
        return e->second;
    }
    
    int16_t encode(wchar_t c) {
        u_ustring s;
        s = (u_uchar)c;
        const auto& e = string_to_code.find(s);
        if (e == string_to_code.end()) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
        return e->second;
    }

    int16_t encode(u_uchar c) {
        u_ustring s;
        s = c;
        const auto& e = string_to_code.find(s);
        if (e == string_to_code.end()) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
        return e->second;
    }

    void replaceAtom(u_ustring& name, int16_t code, bool tobelocked) {
        lock.locking(tobelocked);
        string_to_code[name] = code;
        lock.unlocking(tobelocked);
    }

    inline int16_t get_code(u_ustring& s) {
        const auto& a = string_to_code.find(s);
        return a == string_to_code.end()?-1:a->second;
    }

    inline int16_t get_code(wstring& s) {
        const auto& a = string_to_code.find(_w_to_u(s));
        return a == string_to_code.end()?-1:a->second;
    }

    lisp_code check_atom(string& w) {
        u_ustring s;
        s_utf8_to_unicode(s, w, w.size());
        int16_t code = atom_pool.checkvalue(get_code(s));
        return code==-1?l_final:(lisp_code)code;
    }

    int16_t is_atom(string& w) {
        u_ustring s;
        s_utf8_to_unicode(s, w, w.size());
        return atom_pool.checkvalue(get_code(s));
    }

    int16_t is_atom(u_ustring& s) {
        return atom_pool.checkvalue(get_code(s));
    }

    int16_t is_atom(wstring& w) {
        return atom_pool.checkvalue(get_code(w));
    }

    int16_t is_basic_atom(wstring& w) {
        return atom_basic_pool.checkvalue(get_code(w));
    }

    int16_t is_basic_atom(u16string& e) {
        u_ustring w;
        for (long i = 0; i < e.size(); i++)
            w += (u_uchar)e[i];
        
        return atom_basic_pool.checkvalue(get_code(w));
    }


    bool is_instruction(int16_t c) {
        return instructions.check(c);
    }

    bool is_instruction(u_ustring& s) {
        return (s == U"true" || s == U"nil" || instructions.check(get_code(s)));
    }

    bool is_instruction(wstring& s) {
        return (s == L"true" || s == L"nil" || instructions.check(get_code(s)));
    }

    bool is_math_operator(int16_t c) {
        return math_operators.check(c);
    }

    bool is_operator(int16_t c) {
        return operators.check(c);
    }

    bool is_atom_code(int16_t code) {
        return (code > l_final && atom_pool.check(code));
    }


    long id_file(string& pathname) {
        const auto& a = allfiles.find(pathname);
        if (a == allfiles.end()) {
            long i = allfiles.size();
            allfiles[pathname] = i;
            allfiles_names[i] = pathname;
            return i;
        }
        return a->second;
    }

    Element* provideOperator(int16_t code) {
        return operator_pool.search(code);
    }

    bool sameArity(int16_t instruction_code, unsigned long arity) {
        return (arities.check(instruction_code) && arities.at(instruction_code) == arity);
    }

    bool arity_check(int16_t instruction_code, unsigned long sz) {
        return (arities.check(instruction_code))?(arities.at(instruction_code)&sz):false;
    }

    bool checkArity(int16_t instruction_code, unsigned long sz) {
        if (arities.check(instruction_code)) {
            sz = 1 << ((sz < 16)?sz:15);
            return ((arities.at(instruction_code)&sz) == sz);
        }
        return true;
    }

    inline void set_instruction(lisp_code instruction_code, string name,  unsigned long arity, methodEval m) {
        straight_eval[instruction_code] = new Listincode();
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        evals[instruction_code] = m;
        u_ustring n;
        s_utf8_to_unicode(n, name, name.size());
        code_to_string[instruction_code] = n;
    }

    inline void set_instruction(lisp_code instruction_code,
                                string name,
                                unsigned long arity,
                                methodEval m,
                                Listincode* cln) {
        straight_eval[instruction_code] = cln;
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        evals[instruction_code] = m;
        u_ustring n;
        s_utf8_to_unicode(n, name, name.size());
        code_to_string[instruction_code] = n;
        cln->multiple = (P_ATLEASTFIVE == (P_ATLEASTFIVE & arity));
    }

    inline void set_instruction(lisp_code instruction_code,
                                string name,
                                unsigned long arity,
                                Listincode* cln) {
        straight_eval[instruction_code] = cln;
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        evals[instruction_code] = &List::eval_list_instruction;
        u_ustring n;
        s_utf8_to_unicode(n, name, name.size());
        code_to_string[instruction_code] = n;
        cln->multiple = (P_ATLEASTFIVE == (P_ATLEASTFIVE & arity));
    }

    inline void set_pure_instruction(lisp_code instruction_code, string name,  unsigned long arity) {
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        u_ustring n;
        s_utf8_to_unicode(n, name, name.size());
        code_to_string[instruction_code] = n;
    }

    inline void clear_breakpoints() {
        breakpoints.clear();
    }

    inline bool check_breakpoints(long i_file, long i_line) {
        return (breakpoints.count(i_file) && breakpoints[i_file].count(i_line));
    }

    inline bool activate_on_breakpoints(LispE* lisp, List* e) {
        if (next_stop) {
            if (debugfunction != NULL) {
                (*debugfunction)(lisp, e, debugobject);
                return true;
            }
        }
        
        if (check_breakpoints(i_current_file, i_current_line)) {
            next_stop = false;
            if (debugfunction != NULL) {
                (*debugfunction)(lisp, e, debugobject);
                return true;
            }
        }
        return false;
    }
    
    
    bool recordingFunction(Element* e, int16_t label, uint16_t space) {
        if (function_pool[space]->check(label))
            return false;
        
        (*function_pool[space])[label] = e;
        return true;
    }

    bool replaceFunction(Element* e, int16_t label, uint16_t space) {
        if (function_pool[space]->check(label)) {
            if (bodies[space]->check(label))
                (*bodies[space])[label]->decrement();
            
            (*bodies[space])[label] = e;
            e->increment();
            (*function_pool[space])[label] = e;
            return true;
        }
        return false;
    }

    inline Element* recordingMethod(Stackelement* stack, Element* e, int16_t label, int16_t sublabel, uint16_t space) {
        //If the first element of e is a data structure: ( (L x x x))
        //We need to extract the second label...
        
        if (!method_pool[space]->count(label)) {
            //We record the first instance of a defpat declaration
            if (stack == NULL)
                recordingFunction(e, label, space);
            else
                stack->recording(e, label);
        }
        
        (*method_pool[space])[label][sublabel].push_back(e);
        
        const auto& ad = data_descendant.find(sublabel);
        if (ad != data_descendant.end()) {
            for (const auto& a: ad->second)
                (*method_pool[space])[label][a].push_back(e);
        }
        
        return e;
    }
    
    inline Element* recordingData(Element* e, int16_t label, int16_t ancestor) {
        if (data_pool.check(label))
            throw new Error("Error: data structure has already been recorded");
        
        data_pool[label] = e;
        e->type = t_data;
        if (ancestor != v_null) {
            data_ancestor[label] = ancestor;
            data_descendant[ancestor].push_back(label);
            data_pool[ancestor] = _TRUE;
        }
        return e;
    }
    
    inline bool checkAncestor(Element* ancestor, int16_t label) {
        return (data_ancestor.check(label) && (data_ancestor.at(label) == ancestor->label()));
    }
    
    inline int16_t checkDataStructure(Element* e) {
        if (data_pool.check(e->label()))
            return e->label();
        //Atoms have a label which is different from their type
        //For the other elements, type_atom returns v_null.
        //So if the label is not part of the data_pool, an atom will still return its type
        //The rationale behind this complicated structure is to ensure that pattern rules
        //that are indexed on atom_ will still be chosen and triggered.
        return e->type_atom();
    }

    inline int16_t checkDataStructure(int16_t label) {
        if (data_pool.check(label))
            return label;
        return v_null;
    }

    inline Element* getDataStructure(int16_t label) {
        return data_pool.search(label);
    }
    
    inline Element* recordingMacro(LispE* lisp, Element* e, int16_t label) {
        if (macros.check(label))
            throw new Error("Error: This macro has already been recorded");
        
        if (!e->replaceVariableNames(lisp))
            throw new Error("Error: The definition of a parameter should not be 'nil'");
        macros[label] = e;
        return e;
    }
    
    void setError(Error* err) {
        lock.locking(true);
        if ((stop_execution & 1) == 1) {
            current_error->message += L"\n";
            current_error->message += err->message;
            err->release();
        }
        else {
            stop_execution |= 1;
            current_error = err;
            current_error->increment();
        }
        lock.unlocking(true);
    }
    
#ifdef LISPE_WASM
    void throwError() {
        //We need to  check if the error has not be thrown yet
        stop_execution &= 0xFFFE;
    }

    inline void checkExecution() {
        if (stop_execution) {
            if (current_error != NULL)
                current_error->decrement();
            current_error = _THEEND;
        }
    }

#else
    void throwError() {
        //We need to  check if the error has not be thrown yet
        stop_execution &= 0xFFFE;
        Error* err = current_error;
        current_error = NULL;
        throw err;
    }
    
    inline void checkExecution() {
        if (stop_execution)
            throw _THEEND;
    }
#endif
    
    void atomise(u_ustring& a, List* liste, bool tobelocked) {
        Element* e;
        lock.locking(tobelocked);
        for (long i = 0; i < a.size(); i++) {
            e = provideAtom(encode(a[i]));
            liste->append(e);
        }
        lock.unlocking(tobelocked);
    }

    ThreadLock* getlock(u_ustring& w) {
        lock.locking();
        ThreadLock* l = locks[w];
        if (l == NULL) {
            l = new ThreadLock;
            locks[w] = l;
        }
        lock.unlocking();
        return l;
    }
    
    void waiton(u_ustring& w) {
        lock.locking();
        BlockThread* b = waitons[w];
        if (b == NULL) {
            b = new BlockThread;
            waitons[w] = b;
        }
        lock.unlocking();
        b->blocked();
    }
    
    bool trigger(u_ustring& w) {
        lock.locking();
        const auto& a = waitons.find(w);
        if (a != waitons.end()) {
            lock.unlocking();
            a->second->released();
            return true;
        }
        
        lock.unlocking();
        return false;
    }
    
    //Storage for within threads
    void thread_store(u_ustring& key, Element* e) {
        e = e->fullcopy();
        lock.locking();
        thread_pool[key].status = 1;
        thread_pool[key].append(e);
        lock.unlocking();
    }
    
    Element* thread_retrieve_all() {
        Dictionary* d = new Dictionary;
        u_ustring key;
        lock.locking();
        for (auto& a: thread_pool) {
            key = a.first;
            d->recording(key, a.second.copying(true));
        }
        lock.unlocking();
        return d;
    }
    
    Element* thread_retrieve(u_ustring& key) {
        Element* v = NULL;
        lock.locking();
        const auto& a = thread_pool.find(key);
        if (a != thread_pool.end())
            v = a->second.copying(true);
        lock.unlocking();
        return v;
    }
    
    void thread_clear_all() {
        lock.locking();
        for (auto& a: thread_pool) {
            a.second.clear();
        }
        thread_pool.clear();
        lock.unlocking();
    }
    
    bool thread_clear(u_ustring& key) {
        lock.locking();
        const auto& a = thread_pool.find(key);
        if (a != thread_pool.end()) {
            a->second.clear();
            lock.unlocking();
            return true;
        }
        lock.unlocking();
        return false;
    }
    
    Element* provideAtom(int16_t code) {
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atome(code, code_to_string.at(code));
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideAtom(u_ustring& name) {
        int16_t code = encode(name);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atome(code, name);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    void provideAtomType(int16_t code) {
        if (!atom_pool.check(code)) {
            Element* e = new Atomtype(code, code_to_string.at(code));
            e->status = s_constant;
            atom_pool[code] = e;
        }
    }


    Element* provideAtom(u_ustring& name, bool tobelocked) {
        lock.locking(tobelocked);
        int16_t code = encode(name);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atome(code, name);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        lock.unlocking(tobelocked);
        return e;
    }

    Element* provideAtomOrInstruction(int16_t code) {
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            if (operator_pool.check(code))
                e = operator_pool[code];
            else {
                if (instructions.check(code))
                    e = new Instruction(code, code_to_string.at(code));
                else
                    e = new Atome(code, code_to_string.at(code));
                e->status = s_constant;
            }
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideAtomOrInstruction(string& identifier) {
        int16_t code = encode(identifier);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            if (operator_pool.check(code))
                e = operator_pool[code];
            else {
                if (instructions.check(code))
                    e = new Instruction(code, code_to_string.at(code));
                else
                    e = new Atome(code, code_to_string.at(code));
                e->status = s_constant;
            }
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideCADR(u_ustring& strvalue) {
        int16_t code = encode(strvalue);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Cadr(strvalue);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    inline void reset_error() {
        if (current_error != NULL)
            current_error->decrement();
        current_error = NULL;
    }

    inline void reset_context() {
        if (current_error != NULL)
            current_error->decrement();
        current_error = NULL;
        i_current_line = -1;
        i_current_file = -1;
    }
    
    
    inline void set_context(int idxinfo) {
        if (!current_error && idxinfo < idxinfos.size()) {
            i_current_line = idxinfos[idxinfo];
            i_current_file = idxinfos[idxinfo + 1];
        }
    }

    inline Element* set_error(Error* err) {
        if (!current_error) {
            current_error = err;
            current_error->increment();
        }
        else {
            if (current_error != err)
                err->release();
        }
        return current_error;
    }
    
    inline void set_end() {
        if (current_error != NULL)
            current_error->decrement();
        current_error = _THEEND;
    }
    
    inline void set_error_context(Error* err, int idx_info) {
        if (!current_error && idx_info < idxinfos.size()) {
            current_error = err;
            current_error->increment();
            i_current_line = idxinfos[idx_info];
            i_current_file = idxinfos[idx_info + 1];
        }
    }

    ~Delegation();
};

#endif /* delegation_h */
