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
    vecte<short> names;
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
    
    bool recordargument(LispE* lisp, Element* e, short label) {
        if (variables.check(label))
            return variables.at(label)->unify(lisp,e, false);
        
        e = e->duplicate_constant();
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
        return true;
    }
    
    bool recordingunique(Element* e, short label) {
        if (variables.check(label))
            return false;
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
        return true;
    }

    void savelocal(Element* e, List* l) {
        short label = e->label();
        if (variables.check(label)) {
            l->append(e);
            l->append(variables.at(label));
        }
    }

    bool localsave(Element* e, List* l) {
        short label = e->label();
        if (variables.check(label)) {
            l->append(e);
            l->append(variables.at(label));
            return true;
        }
        return false;
    }


    //record a function argument in the stack
    void record_argument(Element* e, short label) {
        e = e->duplicate_constant();
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
    }
    
    void recording(Element* e, short label) {
        if (variables.check(label)) {
            if (variables.at(label) == e)
                return;
            
            e = e->duplicate_constant();

            if (names.check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names.erase(label);
                variables.at(label)->decrement();
                variables.put(label, e);
                return;
            }
            
            variables.put(label, e);
        }
        else {
            e = e->duplicate_constant();
            variables[label] = e;
        }
        
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
    }

    Element* recording_variable(Element* e, short label) {
        if (variables.check(label)) {
            if (variables.at(label) == e)
                return e;
            
            e = e->duplicate_constant();

            if (names.check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names.erase(label);
                variables.at(label)->decrement();
                variables.put(label, e);
                return e;
            }

            variables.put(label, e);
        }
        else {
            e = e->duplicate_constant();
            variables[label] = e;
        }
        
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
        return e;
    }

    void storing_variable(Element* e, short label) {
        if (variables.check(label)) {
            if (variables.at(label) == e)
                return;
            
            e = e->duplicate_constant();

            if (names.check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names.erase(label);
                variables.at(label)->decrement();
                variables.put(label, e);
                return;
            }

            variables.put(label, e);
        }
        else {
            e = e->duplicate_constant();
            variables[label] = e;
        }
        
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
    }


    void replacingvalue(Element* e, short label) {
        if (variables.at(label) == e)
            return;
        
        e = e->duplicate_constant();
        
        if (names.check(label)) {
            if (e->status != s_constant)
                e->increment();
            else
                names.erase(label);
            variables.at(label)->decrement();
            variables.put(label, e);
            return;
        }

        variables.put(label, e);
        
        if (e->status != s_constant) {
            e->increment();
            names.push_back(label);
        }
    }

    Element* get(short label) {
        return variables.search(label);
    }
    
    void remove(short label) {
        if (names.checkanderase(label)) {
            variables.at(label)->decrement();
        }
        variables.erase(label);
    }

    void remove(short label, Element* keep) {
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
        binHash<Element*>::iterator a;
        for (a = stack->variables.begin(); a != stack->variables.end(); a++) {
            if (a->second->status == s_constant && a->second->type <= t_error)
                variables[a->first] = a.second;
        }
    }
    
    void clear() {
        for (short i = 0; i < names.last; i++) {
            variables.at(names.vecteur[i])->decrement();
        }
        variables.clear();
        names.clear();
        function = NULL;
    }

    void clear(Element* keep) {
        Element* e;
        for (short i = 0; i < names.last; i++) {
            e = variables.at(names.vecteur[i]);
            if (e != keep)
                e->decrement();
        }
        variables.clear();
        names.clear();
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
    
    void atoms(vector<short>& v_atoms) {
        binHash<Element*>::iterator a;
        for (a = variables.begin(); a != variables.end(); a++) {
            v_atoms.push_back(a->first);
        }
    }
    
    ~Stackelement() {
        for (short i = 0; i < names.last; i++)
            variables.at(names.vecteur[i])->decrement();
    }
};

#endif /* stack_h */
