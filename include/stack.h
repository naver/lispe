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
    
    Element* function;
    VECTE<short> names;
    binHash<Element*> variables;
    
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
        
        variables[label] = e;
        if (!e->status) {
            e->incrementstatus(1, true);
            names.push_back(label);
        }
        return true;
    }
    
    bool recordingunique(Element* e, short label) {
        if (variables.check(label))
            return false;
        variables[label] = e;
        if (!e->status) {
            e->incrementstatus(1, true);
            names.push_back(label);
        }
        return true;
    }
    
    Element* recording(Element* e, short label) {
        if (variables.check(label)) {
            Element* previous = variables.at(label);
            if (previous == e)
                return e;
            if (names.checkanderase(label))
                previous->decrementstatus(1, true);
        }
        
        e = e->duplicate_constant_container();
        variables[label] = e;
        if (!e->status) {
            e->incrementstatus(1, true);
            names.push_back(label);
        }
        return e;
    }

    Element* recordingvalue(Element* e, short label) {
        if (variables.check(label)) {
            Element* previous = variables.at(label);
            if (previous == e)
                return e;
            if (names.checkanderase(label))
                previous->decrementstatus(1, true);
        }
        
        e = e->duplicate_constant_container();
        variables[label] = e;
        e->incrementstatus(1, true);
        names.push_back(label);
        return e;
    }

    Element* get(short label) {
        return variables.search(label);
    }
    
    void remove(short label) {
        if (names.checkanderase(label)) {
            variables.at(label)->decrementstatus(1,true);
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
            variables.at(names.vecteur[i])->decrementstatus(1, true);
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
                e->decrementstatus(1, true);
        }
        variables.clear();
        names.clear();
        function = NULL;
    }

    Stackelement* setFunction(Element* f) {
        function = f;
        return this;
    }
    
    //We only copy unknown values
    //used for lambdas to keep track of values from the previous stack
    void setElements(Stackelement* stack) {
        binHash<Element*>::iterator a;
        for (a = stack->variables.begin(); a != stack->variables.end(); a++) {
            if (!variables.check(a->first)) {
                variables[a->first] = a.second;
            }
        }
    }
    
    void atoms(vector<short>& v_atoms) {
        binHash<Element*>::iterator a;
        for (a = variables.begin(); a != variables.end(); a++) {
            v_atoms.push_back(a->first);
        }
    }
    
    ~Stackelement() {
        for (short i = 0; i < names.last; i++)
            variables.at(names.vecteur[i])->decrementstatus(1, true);
    }
};

#endif /* stack_h */
