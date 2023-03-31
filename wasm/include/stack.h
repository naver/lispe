/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  stack.h
//
//
#ifndef stack_h
#define stack_h
class LispE;
#include "vecte.h"
#include "mapbin.h"
class Stackelement {
public:
    
    binHash<Element*> variables;
    vecte<int16_t> status;
    binSet names;
    binSetIter itn;
    Element* function;
    Stackelement() {
        function = NULL;
    }

    Stackelement(Element* f) {
        function = f;
    }

    Element* called() {
        return function;
    }

    
    long size() {
        return variables.size();
    }

    
    bool recordargument(LispE* lisp, Element* e, int16_t label) {
        if (variables.check(label))
            return variables.at(label)->unify(lisp,e, false);
        
        e = e->duplicate_constant(lisp);
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
        return true;
    }

    
    bool recordingunique(Element* e, int16_t label) {
        if (variables.check(label))
            return false;
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
        return true;
    }

    void savelocal(Element* e, List* l) {
        int16_t label = e->label();
        if (variables.check(label)) {
            l->append(e);
            l->append(variables.at(label));
        }
    }

    bool localsave(Element* e, List* l) {
        int16_t label = e->label();
        if (variables.check(label)) {
            l->append(e);
            l->append(variables.at(label));
            return true;
        }
        return false;
    }

    //record a function argument in the stack
    void record_argument(Element* e, int16_t label) {
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
    }

    
    void recording(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return;
            
            if (names.check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names.erase(label);
                v->decrement();
                variables.put(label, e);
                return;
            }
            
            variables.put(label, e);
        }
        else {
            variables[label] = e;
        }
        
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
    }

    Element* recording_variable(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return e;
            
            if (names.check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names.erase(label);
                v->decrement();
                variables.put(label, e);
                return e;
            }
            variables.put(label, e);
        }
        else {
            variables[label] = e;
        }
        
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
        return e;
    }

    void storing_variable(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return;
            
            if (names.check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names.erase(label);
                v->decrement();
                variables.put(label, e);
                return;
            }
            variables.put(label, e);
        }
        else {
            variables[label] = e;
        }
        
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
    }

    void reset_in_stack(Element* e, int16_t label) {
        if (e == NULL)
            remove(label);
        else {
            e->status = status.backpop();
            if (variables.at(label) != e) {
                variables.at(label)->decrement();
                variables.put(label, e);
            }
        }
    }

    
    void reset_in_stack(Element* e, int16_t label, Element* keep) {
        Element* last = variables.at(label);
        if (e == NULL) {
            if (last == keep)
                last->decrementkeep();
            else
                last->decrement();
            removeonly(label);
        }
        else {
            e->status = status.backpop();
            if (last != e) {
                if (last == keep)
                    last->decrementkeep();
                else
                    last->decrement();
                variables.put(label, e);
            }
        }
    }

    
    Element* record_or_replace(Element* e, int16_t label) {
        Element* ret = variables.search(label);
        if (ret != NULL) {
            //we keep track of the current status...
            status.push_back(ret->status);
            ret->increment();
            if (ret == e)
                return e;
            
            if (e->status != s_constant)
                e->increment();
            else
                names.erase(label);
            variables.put(label, e);
            return ret;
        }
        variables[label] = e;
        
        if (e->status != s_constant) {
            e->increment();
            names.push(label);
        }
        return NULL;
    }

    void replacingvalue(Element* e, int16_t label) {
        if (names.check(label)) {
            Element* v = variables.at(label);
            if (v == e)
                return;
            if (e->status != s_constant)
                e->increment();
            else
                names.erase(label);
            v->decrement();
        }
        else {
            if (e->status != s_constant) {
                e->increment();
                names.push(label);
            }
        }
        variables.put(label, e);
    }

    inline Element* get(int16_t label) {
        return variables.search(label);
    }

    inline void removeonly(int16_t label) {
        names.erase(label);
        variables.erase(label);
    }

    inline void remove(int16_t label) {
        if (names.checkanderase(label)) {
            variables.at(label)->decrement();
        }
        variables.erase(label);
    }

    void remove(int16_t label, Element* keep) {
        if (names.checkanderase(label)) {
            Element* local = variables.at(label);
            if (local == keep)
                local->decrementkeep();
            else
                local->decrement();
        }
        variables.erase(label);
    }

    u_ustring asUString(LispE*);
    wstring asString(LispE*);
    List* atomes(LispE*);
    
    void copy(Stackelement* stack) {
        //We only copy constant values...
        if (stack != NULL) {
            binHash<Element*>::iterator a(stack->variables);
            for (; !a.end(); a++) {
                if (a->second->status == s_constant && a->second->type <= t_error)
                    variables[a->first] = a.second;
            }
        }
    }

    
    void clear() {
        itn.set(names);
        while (itn.next())
            variables.at(itn.first)->decrement();
        names.clear();
        variables.clear();
        status.clear();
        function = NULL;
    }

    void clear(Element* keep) {
        Element* e;
        itn.set(names);
        while (itn.next()) {
            e = variables.at(itn.first);
            if (e != keep)
                e->decrement();
        }
        names.clear();
        variables.clear();
        status.clear();
        function = NULL;
    }

    Element* exchange(Element* f) {
        Element* e = function;
        function = f;
        return e;
    }

    
    Stackelement* setFunction(Element* f) {
        function = f;
        return this;
    }

    
    //We only copy unknown values
    //used for lambdas to keep track of values from the previous stack
    void shareElements(Stackelement* stack) {
        stack->variables.andnot(variables);
    }

    
    void atoms(vector<int16_t>& v_atoms) {
        binHash<Element*>::iterator a(variables);
        for (; !a.end(); a++) {
            v_atoms.push_back(a->first);
        }
    }

    
    ~Stackelement() {
        itn.set(names);
        while (itn.next())
            variables.at(itn.first)->decrement();
    }

};
#endif /* stack_h */