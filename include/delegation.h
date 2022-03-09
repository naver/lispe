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
//Delegation is common to all threads
//It basically stores everything that is common to all threads
//and won't budge after loading or compilation
//The only exception is when creating new atoms
typedef void (*reading_string)(string&, void*);

class Delegation {
public:
    BlockThread trace_lock;
    ThreadLock lock;

    methodEval evals[l_final];


    binHashe<u_ustring> code_to_string;
    binHashe<string> instructions;
    
    binHash<short> data_ancestor;
    binHash<Element*> function_pool;
    binHash<Element*> data_pool;
    
    binSet assignors;
    binSet operators;
    binSet math_operators;
    binSet comparators;
    binSet logicals;

    binHash<Element*> operator_pool;
    binHash<Element*> atom_pool;
    binSet atom_basic_pool;
    binHash<unsigned long> arities;
    binHash<Element*> macros;

    binSet number_types;
    
    //------------------------------------------
    unordered_map<u_ustring, short> string_to_code;
    unordered_map<short, vector<short> > data_descendant;
    unordered_map<short, unordered_map<short, vector<Element*> > > method_pool;

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

    Error* error_message;
    
    reading_string reading_string_function;
    reading_string display_string_function;
    void* reading_string_function_object;
    
    Element* _NUMERICAL_BOOLEANS[2];
    Element* _COMPARE_BOOLEANS[3];
    
    Atome* _ERROR;
    Atome* _TERMINAL;
    Atome* _COMPOSE;
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
    bool context_is_error;
    short stop_execution;

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
    
    char checkComparator(short type, short root) {
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


    inline string toString(short c) {
        string s;
        if (code_to_string.check(c)) {
            u_ustring w = code_to_string.at(c);
            s_unicode_to_utf8(s, w);
            return s;
        }
        return "nil";
    }

    inline wstring asString(short c) {
        if (code_to_string.check(c))
            return u_to_w(code_to_string.at(c));
        return L"nil";
    }

    inline u_ustring asUString(short c) {
        if (code_to_string.check(c))
            return code_to_string.at(c);
        return U"nil";
    }

    bool isNumberType(short u) {
        return number_types.check(u);
    }
    
    void updatepathname(string& pathname) {
        try {
            i_current_file = allfiles.at(pathname);
        }
        catch (...) {
            i_current_file = allfiles.size();
            allfiles[pathname] = i_current_file;
            allfiles_names[i_current_file] = pathname;
        }
    }
    
    void addtolisting(long l, string& e) {
        try {
            listing.at(i_current_file).at(l);
        }
        catch (...) {
            listing[i_current_file][l] = e;
        }
    }
    
    bool isEndTrace() {
        return endtrace;
    }
    
    void updatecreator();
    
    short encode(string& str) {
        u_ustring s;
        s_utf8_to_unicode(s, USTR(str), str.size());
        try {
            return string_to_code.at(s);
        }
        catch (...) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }

    short encode(wstring& w) {
        u_pstring s = _w_to_u(w);
        try {
            return string_to_code.at(s);
        }
        catch (...) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }

    short encode(u_ustring& s) {
        try {
            return string_to_code.at(s);
        }
        catch (...) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }
    
    short encode(wchar_t c) {
        u_ustring s;
        s = (u_uchar)c;
        try {
            return string_to_code.at(s);
        }
        catch (...) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }

    short encode(u_uchar c) {
        u_ustring s;
        s = c;
        try {
            return string_to_code.at(s);
        }
        catch (...) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }

    void replaceAtom(u_ustring& name, short code, bool tobelocked) {
        lock.locking(tobelocked);
        string_to_code[name] = code;
        lock.unlocking(tobelocked);
    }


    lisp_code check_atom(string& w) {
        u_ustring s;
        s_utf8_to_unicode(s, USTR(w), w.size());
        try {
            short code = string_to_code.at(s);
            if (atom_pool.check(code))
                return (lisp_code)code;
        }
        catch (...) {
        }
        return l_final;
    }

    short is_atom(string& w) {
        u_ustring s;
        s_utf8_to_unicode(s, USTR(w), w.size());
        try {
            short code = string_to_code.at(s);
            if (atom_pool.check(code))
                return code;
        }
        catch (...) {
        }
        return -1;
    }

    short is_atom(u_ustring& s) {
        try {
            short code = string_to_code.at(s);
            if (atom_pool.check(code))
                return code;
        }
        catch (...) {
        }
        return -1;
    }

    short is_atom(wstring& w) {
        try {
            short code = string_to_code.at(_w_to_u(w));
            if (atom_pool.check(code))
                return code;
        }
        catch (...) {
        }
        return -1;
    }

    short is_basic_atom(wstring& w) {
        try {
            short code = string_to_code.at(_w_to_u(w));
            if (atom_basic_pool.check(code))
                return code;
        }
        catch (...) {
        }
        return -1;
    }


    bool is_instruction(short c) {
        return instructions.check(c);
    }

    bool is_instruction(u_ustring& s) {
        try {
            return (s == U"true" || s == U"nil" || instructions.check(string_to_code.at(s)));
        }
        catch (...) {
            return false;
        }
    }

    bool is_instruction(wstring& s) {
        try {
            return (s == L"true" || s == L"nil" || instructions.check(string_to_code.at(_w_to_u(s))));
        }
        catch (...) {
            return false;
        }
    }

    bool is_math_operator(short c) {
        return math_operators.check(c);
    }

    bool is_operator(short c) {
        return operators.check(c);
    }

    bool is_atom_code(short code) {
        return (code > l_final && atom_pool.check(code));
    }


    long id_file(string& pathname) {
        try {
            return allfiles.at(pathname);
        }
        catch (...) {
            return -1;
        }
    }

    Element* provideOperator(short code) {
        return operator_pool.search(code);
    }

    bool sameArity(short instruction_code, unsigned long arity) {
        try {
            return (arities.at(instruction_code) == arity);
        }
        catch (...) {
            return false;
        }
        return false;
    }
    
    bool checkArity(short instruction_code, unsigned long sz) {
        if (arities.check(instruction_code)) {
            sz = 1 << ((sz < 16)?sz:15);
            return ((arities.at(instruction_code)&sz) == sz);
        }
        return true;
    }

    inline void set_instruction(short instruction_code, string name,  unsigned long arity, methodEval m) {
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        evals[instruction_code] = m;
        u_ustring n;
        s_utf8_to_unicode(n, USTR(name), name.size());
        code_to_string[instruction_code] = n;
    }

    inline void set_pure_instruction(short instruction_code, string name,  unsigned long arity) {
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        u_ustring n;
        s_utf8_to_unicode(n, USTR(name), name.size());
        code_to_string[instruction_code] = n;
    }

    inline void clear_breakpoints() {
        breakpoints.clear();
    }

    inline bool activate_on_breakpoints(LispE* lisp, List* e) {
        if (next_stop) {
            if (debugfunction != NULL) {
                (*debugfunction)(lisp, e, debugobject);
                return true;
            }
        }
        try {
            if (breakpoints.at(i_current_file).at(i_current_line) == true) {
                next_stop = false;
                if (debugfunction != NULL) {
                    (*debugfunction)(lisp, e, debugobject);
                    return true;
                }
            }
        }
        catch (...) {
            return false;
        }
        return false;
    }

    bool check_breakpoints(long i_file, long i_line) {
        try {
            return breakpoints.at(i_file).at(i_line);
        }
        catch (...) {
            return false;
        }
    }
    
    bool recordingFunction(Element* e, short label) {
        if (function_pool.check(label))
            return false;
        
        function_pool[label] = e;
        return true;
    }
    
    inline Element* recordingMethod(Stackelement* stack, Element* e, short label, short sublabel) {
        //If the first element of e is a data structure: ( (L x x x))
        //We need to extract the second label...
        
        try {
            method_pool.at(label);
        }
        catch (...) {
            //We record the first instance of a defpat declaration
            if (stack == NULL)
                recordingFunction(e, label);
            else
                stack->recording(e, label);
        }
        
        method_pool[label][sublabel].push_back(e);
        
        try {
            for (const auto& a: data_descendant.at(sublabel))
                method_pool[label][a].push_back(e);
        }
        catch (...) {}
        
        return e;
    }
    
    inline Element* recordingData(Element* e, short label, short ancestor) {
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
    
    inline bool checkAncestor(Element* ancestor, short label) {
        return (data_ancestor.check(label) && (data_ancestor.at(label) == ancestor->label()));
    }
    
    inline Element* getMethod(short label, short sublabel, long i) {
        try {
            return method_pool.at(label).at(sublabel).at(i);
        }
        catch (...) {
            return _NULL;
        }
    }
        
    inline short checkDataStructure(short label) {
        if (data_pool.check(label))
            return label;
        return v_null;
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
        return data_pool.search(label);
    }
    
    inline Element* recordingMacro(LispE* lisp, Element* e, short label) {
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
            error_message->message += L"\n";
            error_message->message += err->message;
            err->release();
        }
        else {
            stop_execution |= 1;
            error_message = err;
        }
        lock.unlocking(true);
    }
    
    void throwError() {
        //We need to  check if the error has not be thrown yet
        stop_execution &= 0xFFFE;
        Error* err = error_message;
        error_message = NULL;
        throw err;
    }
    
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
        try {
            BlockThread* b = waitons.at(w);
            lock.unlocking();
            b->released();
            return true;
        }
        catch (...) {
            lock.unlocking();
        }
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
        try {
            v = thread_pool.at(key).copying(true);
        }
        catch (...) {}
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
        try {
            thread_pool.at(key).clear();
            lock.unlocking();
            return true;
        }
        catch (...) {}
        lock.unlocking();
        return false;
    }
    
    Element* provideAtom(short code) {
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atome(code, code_to_string.at(code));
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideAtom(u_ustring& name) {
        short code = encode(name);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atome(code, name);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    void provideAtomType(short code) {
        if (!atom_pool.check(code)) {
            Element* e = new Atomtype(code, code_to_string.at(code));
            e->status = s_constant;
            atom_pool[code] = e;
        }
    }


    Element* provideAtom(u_ustring& name, bool tobelocked) {
        lock.locking(tobelocked);
        short code = encode(name);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atome(code, name);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        lock.unlocking(tobelocked);
        return e;
    }

    Element* provideAtomOrInstruction(short code) {
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
        short code = encode(identifier);
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


    Element* provideNonLabelAtom(short code) {
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Atomnotlabel(code, code_to_string.at(code));
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideCADR(u_ustring& strvalue) {
        short code = encode(strvalue);
        Element* e =  atom_pool.search(code);
        if (e == NULL) {
            e = new Cadr(strvalue);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    inline void checkExecution() {
        if (stop_execution)
            throw _THEEND;
    }
    
    inline void reset_context() {
        context_is_error = false;
        i_current_line = -1;
        i_current_file = -1;
    }
    
    
    inline void set_context(long l, long f) {
        if (!context_is_error) {
            i_current_line = l;
            i_current_file = f;
        }
    }

    inline void set_error_context(long l, long f) {
        if (!context_is_error) {
            context_is_error = true;
            i_current_line = l;
            i_current_file = f;
        }
    }

    ~Delegation();
};

#endif /* delegation_h */
