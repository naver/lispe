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

//------------------------------------------------------------
class LispE;
class jag_get;
typedef void (*lispe_debug_function)(LispE*, List* e, void*);
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
typedef Element* (List::*methodEval)(LispE*);

class Delegation {
public:
    BlockThread trace_lock;
    ThreadLock lock;

    unordered_map<wstring, short> string_to_code;
    unordered_map<short, wstring> code_to_string;

    unordered_map<short, string> instructions;
    unordered_map<short, short> data_ancestor;
    unordered_map<short, vector<short> > data_descendant;
    unordered_map<short, bool> operators;
    unordered_map<short, bool> math_operators;
    unordered_map<short, Element*> operator_pool;
    unordered_map<short, Element*> atom_pool;
    unordered_map<short, Element*> data_pool;
    unordered_map<short, unordered_map<short, vector<Element*> > > method_pool;
    unordered_map<long, unsigned long> arities;
    unordered_map<short, Element*> macros;

    methodEval evals[l_final];
    
    unordered_map<List*, long> in_file;
    unordered_map<string, bool> libraries;
    unordered_map<string, long> allfiles;
    unordered_map<long, Element*> entrypoints;

    unordered_map<wstring, ThreadLock*> locks;
    unordered_map<wstring, BlockThread*> waitons;
    
    unordered_map<wstring, List> thread_pool;

    //this is an all-purpose pool for internal usage

    unordered_map<long, string> allfiles_names;
    unordered_map<long, unordered_map<long, bool> > breakpoints;
    map<long, map<long, string> > listing;

    reading_string reading_string_function;
    reading_string display_string_function;
    void* reading_string_function_object;
    
    Element* _BOOLEANS[2];
    
    Atom* _ERROR;
    Atom* _TERMINAL;
    Atom* _COMPOSE;
    Atom* _TRUE;
    Atom* _NULL;
    Atom* _EMPTYATOM;
    Atom* _LISTSEPARATOR;
    Atom* _DEFPAT;
    Atom* _DICO_KEYN;
    Atom* _DICO_KEY;
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
    short stop_execution;

    bool next_stop;
    bool add_to_listing;
    std::atomic<bool> endtrace;
    
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
    
    inline wstring asString(short c) {
        try {
            return code_to_string.at(c);
        }
        catch(const std::out_of_range& oor) {
            return L"nil";
        }
    }

    void updatepathname(string& pathname) {
        try {
            i_current_file = allfiles.at(pathname);
        }
        catch (const std::out_of_range& oor) {
            i_current_file = allfiles.size();
            allfiles[pathname] = i_current_file;
            allfiles_names[i_current_file] = pathname;
        }
    }
    
    void addtolisting(long l, string& e) {
        try {
            listing.at(i_current_file).at(l);
        }
        catch(const std::out_of_range& oor) {
            listing[i_current_file][l] = e;
        }
    }
    
    bool isEndTrace() {
        return endtrace;
    }
    
    short encode(string& str) {
        wstring s;
        s_utf8_to_unicode(s, USTR(str), str.size());
        try {
            return string_to_code.at(s);
        }
        catch(const std::out_of_range& oor) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }

    short encode(wstring& s) {
        try {
            return string_to_code.at(s);
        }
        catch(const std::out_of_range& oor) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }
    
    short encode(wchar_t c) {
        wstring s;
        s = c;
        try {
            return string_to_code.at(s);
        }
        catch(const std::out_of_range& oor) {
            long idx = string_to_code.size() + l_final;
            code_to_string[idx] = s;
            string_to_code[s] = idx;
            return idx;
        }
    }

    void replaceAtom(wstring& name, short code, bool tobelocked) {
        lock.locking(tobelocked);
        string_to_code[name] = code;
        lock.unlocking(tobelocked);
    }


    lisp_code check_atom(string& w) {
        wstring s;
        s_utf8_to_unicode(s, USTR(w), w.size());
        try {
            short code = string_to_code.at(s);
            atom_pool.at(code);
            return (lisp_code)code;
        }
        catch(const std::out_of_range& oor) {
            return l_final;
        }
    }

    short is_atom(string& w) {
        wstring s;
        s_utf8_to_unicode(s, USTR(w), w.size());
        try {
            short code = string_to_code.at(s);
            atom_pool.at(code);
            return code;
        }
        catch(const std::out_of_range& oor) {
            return -1;
        }
    }

    short is_atom(wstring& s) {
        try {
            short code = string_to_code.at(s);
            atom_pool.at(code);
            return code;
        }
        catch(const std::out_of_range& oor) {
            return -1;
        }
    }

    bool is_instruction(short c) {
        try {
            instructions.at(c);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }

    bool is_instruction(wstring& s) {
        try {
            instructions.at(string_to_code.at(s));
            return true;
        }
        catch(const std::out_of_range& oor) {
            if (s == L"true" || s == L"nil")
                return true;
            return false;
        }
    }
    
    bool is_math_operator(short c) {
        try {
            math_operators.at(c);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }

    bool is_operator(short c) {
        try {
            operators.at(c);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }

    bool is_atom_code(short code) {
        if (code < l_final)
            return false;
        try {
            atom_pool.at(code);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }


    long id_file(string& pathname) {
        try {
            return allfiles.at(pathname);
        }
        catch (const std::out_of_range& oor) {
            return -1;
        }
    }

    Element* provideOperator(short code) {
        return operator_pool[code];
    }


    inline void set_instruction(short instruction_code, string name,  unsigned long arity, methodEval m) {
        instructions[instruction_code] = name;
        arities[instruction_code] = arity;
        evals[instruction_code] = m;
    }

    inline void clear_breakpoints() {
        breakpoints.clear();
    }

    inline bool activate_on_breakpoints(LispE* lisp, List* e) {
        if (next_stop) {
            next_stop = false;
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
        catch(const std::out_of_range& oor) {
            return false;
        }
        return false;
    }

    bool check_breakpoints(long i_file, long i_line) {
        try {
            return breakpoints.at(i_file).at(i_line);
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }
    
    inline Element* recordingMethod(Stackelement* stack, Element* e, short label, short sublabel) {
        //If the first element of e is a data structure: ( (L x x x))
        //We need to extract the second label...
        
        try {
            method_pool.at(label);
        }
        catch(const std::out_of_range& oor) {
            //We record the first instance of a defpat declaration
            stack->recording(e, label);
        }
        
        e->status = s_constant;
        method_pool[label][sublabel].push_back(e);
        
        try {
            for (auto& a: data_descendant.at(sublabel))
                method_pool[label][a].push_back(e);
        }
        catch(const std::out_of_range& oor) {}
        
        return e;
    }
    
    inline Element* recordingData(Element* e, short label, short ancestor) {
        try {
            data_pool.at(label);
            throw new Error("Error: data structure has already been recorded");
        }
        catch(const std::out_of_range& oor) {
            data_pool[label] = e;
            e->status = s_constant;
            e->type = t_data;
            if (ancestor != v_null) {
                data_ancestor[label] = ancestor;
                data_descendant[ancestor].push_back(label);
                data_pool[ancestor] = _TRUE;
            }
            return e;
        }
    }
    
    inline bool checkAncestor(Element* ancestor, Element* label) {
        try {
            return (data_ancestor.at(label->label()) == ancestor->label());
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }
    
    inline Element* getMethod(short label, short sublabel, long i) {
        try {
            return method_pool.at(label).at(sublabel).at(i);
        }
        catch(const std::out_of_range& oor) {
            return _NULL;
        }
    }
        
    inline short checkDataStructure(short label) {
        try {
            data_pool.at(label);
            return label;
        }
        catch(const std::out_of_range& oor) {
            return v_null;
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
        try {
            return data_pool.at(label);
        }
        catch(const std::out_of_range& oor) {
            return NULL;
        }
    }
    
    inline Element* recordingMacro(LispE* lisp, Element* e, short label) {
        try {
            macros.at(label);
            throw new Error("Error: This macro has already been recorded");
        }
        catch(const std::out_of_range& oor) {
            macros[label] = e;
            if (!e->replaceVariableNames(lisp))
                throw new Error("Error: The definition of a parameter should not be 'nil'");
            return e;
        }
    }
    
    Element* atomise(wstring& a, bool tobelocked) {
        List* liste = new List;
        Element* e;
        lock.locking(tobelocked);
        for (long i = 0; i < a.size(); i++) {
            e = provideAtom(encode(a[i]));
            liste->append(e);
        }
        lock.unlocking(tobelocked);
        return liste;
    }

    ThreadLock* getlock(wstring& w) {
        lock.locking();
        ThreadLock* l = locks[w];
        if (l == NULL) {
            l = new ThreadLock;
            locks[w] = l;
        }
        lock.unlocking();
        return l;
    }
    
    void waiton(wstring& w) {
        lock.locking();
        BlockThread* b = waitons[w];
        if (b == NULL) {
            b = new BlockThread;
            waitons[w] = b;
        }
        lock.unlocking();
        b->blocked();
    }
    
    bool trigger(wstring& w) {
        lock.locking();
        try {
            BlockThread* b = waitons.at(w);
            lock.unlocking();
            b->released();
            return true;
        }
        catch(const std::out_of_range& oor) {
            lock.unlocking();
        }
        return false;
    }
    
    //Storage for within threads
    void thread_store(wstring& key, Element* e) {
        e = e->fullcopy();
        lock.locking();
        thread_pool[key].status = 1;
        thread_pool[key].append(e);
        lock.unlocking();
    }
    
    Element* thread_retrieve_all() {
        Dictionary* d = new Dictionary;
        wstring key;
        lock.locking();
        for (auto& a: thread_pool) {
            key = a.first;
            d->recording(key, a.second.copying(true));
        }
        lock.unlocking();
        return d;
    }
    
    Element* thread_retrieve(wstring& key) {
        Element* v = NULL;
        lock.locking();
        try {
            v = thread_pool.at(key).copying(true);
        }
        catch(const std::out_of_range& oor) {}
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
    
    bool thread_clear(wstring& key) {
        lock.locking();
        try {
            thread_pool.at(key).clear();
            lock.unlocking();
            return true;
        }
        catch(const std::out_of_range& oor) {}
        lock.unlocking();
        return false;
    }
    
    Element* provideAtom(short code) {
        Element* e = atom_pool[code];
        if (e == NULL) {
            e = new Atom(code);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideAtom(wstring& name) {
        short code = encode(name);
        Element* e = atom_pool[code];
        if (e == NULL) {
            e = new Atom(code);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideAtom(wstring& name, bool tobelocked) {
        lock.locking(tobelocked);
        short code = encode(name);
        Element* e = atom_pool[code];
        if (e == NULL) {
            e = new Atom(code);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        lock.unlocking(tobelocked);
        return e;
    }

    Element* provideAtomOrInstruction(short code) {
        Element* e = atom_pool[code];
        if (e == NULL) {
            try {
                instructions.at(code);
                e = new Instruction(code);
            }
            catch(const std::out_of_range& oor) {
                e = new Atom(code);
            }
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideAtomOrInstruction(string& identifier) {
        short code = encode(identifier);
        Element* e = atom_pool[code];
        if (e == NULL) {
            try {
                instructions.at(code);
                e = new Instruction(code);
            }
            catch(const std::out_of_range& oor) {
                e = new Atom(code);
            }
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }


    Element* provideNonLabelAtom(short code) {
        Element* e = atom_pool[code];
        if (e == NULL) {
            e = new Atomnotlabel(code);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }

    Element* provideCADR(string& strvalue) {
        short code = encode(strvalue);
        Element* e = atom_pool[code];
        if (e == NULL) {
            e = new Cadr(strvalue);
            e->status = s_constant;
            atom_pool[code] = e;
        }
        return e;
    }
    
    ~Delegation();
};

#endif /* delegation_h */
