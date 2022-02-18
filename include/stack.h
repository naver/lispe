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

const short n_variables = 4;
const short f_variables = n_variables - 1;

class Stackelement {
public:
    
    binHash<Element*> variables;
    vecte<short> names[n_variables];
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
        
        e = e->duplicate_constant(lisp);
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names[label&f_variables].push_back(label);
        }
        return true;
    }
    
    bool recordingunique(Element* e, short label) {
        if (variables.check(label))
            return false;
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names[label&f_variables].push_back(label);
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
        variables[label] = e;
        if (e->status != s_constant) {
            e->increment();
            names[label&f_variables].push_back(label);
        }
    }
    
    void recording(Element* e, short label) {
        if (variables.check(label)) {
            if (variables.at(label) == e)
                return;
            
            if (names[label&f_variables].check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names[label&f_variables].erase(label);
                variables.at(label)->decrement();
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
            names[label&f_variables].push_back(label);
        }
    }

    Element* recording_variable(Element* e, short label) {
        if (variables.check(label)) {
            if (variables.at(label) == e)
                return e;
            
            if (names[label&f_variables].check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names[label&f_variables].erase(label);
                variables.at(label)->decrement();
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
            names[label&f_variables].push_back(label);
        }
        return e;
    }

    void storing_variable(Element* e, short label) {
        if (variables.check(label)) {
            if (variables.at(label) == e)
                return;
            
            if (names[label&f_variables].check(label)) {
                if (e->status != s_constant)
                    e->increment();
                else
                    names[label&f_variables].erase(label);
                variables.at(label)->decrement();
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
            names[label&f_variables].push_back(label);
        }
    }


    void replacingvalue(Element* e, short label) {
        if (variables.at(label) == e)
            return;
                
        if (names[label&f_variables].check(label)) {
            if (e->status != s_constant)
                e->increment();
            else
                names[label&f_variables].erase(label);
            variables.at(label)->decrement();
            variables.put(label, e);
            return;
        }

        variables.put(label, e);
        
        if (e->status != s_constant) {
            e->increment();
            names[label&f_variables].push_back(label);
        }
    }

    Element* get(short label) {
        return variables.search(label);
    }
    
    void remove(short label) {
        if (names[label&f_variables].checkanderase(label)) {
            variables.at(label)->decrement();
        }
        variables.erase(label);
    }

    void remove(short label, Element* keep) {
        if (names[label&f_variables].checkanderase(label)) {
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
        for (short label = 0; label < n_variables; label++) {
            for (short i = 0; i < names[label&f_variables].last; i++) {
                variables.at(names[label&f_variables].vecteur[i])->decrement();
            }
            names[label&f_variables].clear();
        }
        variables.clear();
        function = NULL;
    }

    void clear(Element* keep) {
        Element* e;
        for (short label = 0; label < n_variables; label++) {
            for (short i = 0; i < names[label&f_variables].last; i++) {
                e = variables.at(names[label&f_variables].vecteur[i]);
                if (e != keep)
                    e->decrement();
            }
            names[label&f_variables].clear();
        }
        
        variables.clear();
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
        for (short label = 0; label < n_variables; label++) {
            for (short i = 0; i < names[label&f_variables].last; i++)
                variables.at(names[label&f_variables].vecteur[i])->decrement();
            names[label&f_variables].clear();
        }
    }
};

#endif /* stack_h */
