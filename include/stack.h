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
    Element* function;
    List_instance* instance;

    Stackelement() {
        function = NULL;
        instance = NULL;
    }

    Stackelement(Element* f) {
        function = f;
        instance = NULL;
    }

    inline void set(Element* f, List_instance* i) {
        function = f;
        instance = i;
    }
    
    inline void check_space(int16_t space_function, int16_t currentspace) {
        if (space_function != currentspace)
            instance = NULL;
    }
    
    inline void set(Element* f, List_instance* i, int16_t space_function, int16_t currentspace) {
        function = f;
        instance = (space_function == currentspace)?i:NULL;
    }

    Element* called() {
        return function;
    }
    
    long size() {
        return variables.size();
    }
    
    bool recordargument(LispE* lisp, Element* e, int16_t label);
    
    bool recordingunique(Element* e, int16_t label) {
        if (variables.check(label))
            return false;
        e->increment();
        variables[label] = e;
        return true;
    }

    void savelocal(Element* e, List* l) {
        int16_t label = e->label();
        if (variables.check(label)) {
            l->append(e);
            l->append(variables.at(label));
        }
    }
    
    bool get_instance_variable(int16_t label, Element** res) {
        return (instance && instance->get_variable(label, res));
    }

    inline bool check_instance_variable(int16_t label) {
        return (instance && instance->check_variable(label));
    }
    
    Element* store_instance_variable(int16_t label, Element* e) {
        return instance->store_variable(label, e);
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
        e->increment();
    }
    
    void recording(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return;
            
            e->increment();
            v->decrement();
            variables.put(label, e);
        }
        else {
            e->increment();
            variables[label] = e;
        }

    }

    inline bool check_with_instance(int16_t label) {
        return (variables.check(label) || check_instance_variable(label));
    }
    
    Element* recording_variable_with_instance(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return e;
            
            e->increment();
            v->decrement();
            variables.put(label, e);
        }
        else {
            if (check_instance_variable(label))
                return store_instance_variable(label, e);
            
            e->increment();
            variables[label] = e;
        }
        
        return e;
    }

    Element* recording_variable(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return e;
            
            e->increment();
            v->decrement();
            variables.put(label, e);
        }
        else {
            e->increment();
            variables[label] = e;
        }
        
        return e;
    }

    void storing_variable(Element* e, int16_t label) {
        Element* v = variables.search(label);
        if (v != NULL) {
            if (v == e)
                return;

            e->increment();
            v->decrement();
            variables.put(label, e);
        }
        else {
            e->increment();
            variables[label] = e;
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
            if (last != e) {
                e->status = status.backpop();
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
            if (ret == e)
                return e;
            
            //we keep track of the current status...
            status.push_back(ret->status);
            ret->increment();
            
            e->increment();
            variables.put(label, e);
            return ret;
        }

        variables[label] = e;
        e->increment();
        return NULL;
    }

    void replace_stack_value(Element* e, int16_t label) {
        Element* v = variables.at(label);
        if (v == e)
            return;
        
        e->increment();
        v->decrement();
        variables.put(label, e);
    }

    void put_and_keep(Element* v, Element* keep, int16_t label) {
        Element* e = variables.at(label);
        if (e == keep)
            e->decrementkeep();
        else
            e->decrement();
        if (v == NULL)
            variables.erase(label);
        else {
            v->decrement();
            variables.put(label, v);
        }
    }
    
    inline Element* get(int16_t label) {
        return variables.search(label);
    }

    inline void removeonly(int16_t label) {
        variables.erase(label);
    }

    inline void remove(int16_t label) {
        if (variables.check(label)) {
            variables.at(label)->decrement();
            variables.erase(label);
        }
    }

    void remove(int16_t label, Element* keep) {
        if (variables.check(label)) {
            Element* local = variables.at(label);
            if (local == keep)
                local->decrementkeep();
            else
                local->decrement();
            variables.erase(label);
        }
    }

    u_ustring asUString(LispE*);
    wstring asString(LispE*);
    string toString(LispE*);
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
        binHash<Element*>::iterator itn(variables);
        for (;!itn.end(); itn++)
            itn.second->decrement();
        variables.clear();
        status.clear();
        function = NULL;
        instance = NULL;
    }

    void clear(Element* keep) {
        binHash<Element*>::iterator itn(variables);
        for (;!itn.end(); itn++) {
            if (itn.second != keep)
                itn.second->decrement();
        }
        variables.clear();
        status.clear();
        function = NULL;
        instance = NULL;
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
        binHash<Element*>::iterator itn(variables);
        for (;!itn.end(); itn++)
            itn.second->decrement();
    }
};

#endif /* stack_h */
