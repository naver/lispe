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

class Stackelement {
public:
    
    Element* function;
    unordered_map<short, Element*> variables;
    
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
        try {
            return variables.at(label)->unify(lisp,e, false);
        }
        catch(const std::out_of_range& oor) {
            variables[label] = e;
            e->incrementstatus(1, true);
        }
        return true;
    }
    
    bool recordingunique(Element* e, short label) {
        try {
            variables.at(label);
            return false;
        }
        catch(const std::out_of_range& oor) {
            variables[label] = e;
            e->incrementstatus(1, true);
        }
        return true;
    }
    
    Element* recording(Element* e, short label) {
        Element* previous = variables[label];
        if (previous != NULL) {
            if (previous == e)
                return e;
            previous->decrementstatus(1, true);
        }
        
        e = e->duplicate_constant_container();
        variables[label] = e;
        e->incrementstatus(1, true);
        return e;
    }

    Element* get(short label) {
        try {
            return variables.at(label);
        }
        catch(const std::out_of_range& oor) {
            return NULL;
        }
    }
    
    void remove(short label) {
        try {
            variables.at(label)->decrementstatus(1,true);
            variables.erase(label);
        }
        catch(const std::out_of_range& oor) {}
    }
    
    wstring asString(LispE*);
    List* atomes(LispE*);
    
    void copy(Stackelement* stack) {
        //We only copy constant values...
        for (auto& a: stack->variables) {
            if (a.second->status == s_constant && a.second->type <= t_error)
                variables[a.first] = a.second;
        }
    }
    
    void clear() {
        for (auto& a: variables)
            a.second->decrementstatus(1, true);
        variables.clear();
        function = NULL;
    }

    Stackelement* setFunction(Element* f) {
        function = f;
        return this;
    }
    
    //We only copy unknown values
    //used for lambdas to keep track of values from the previous stack
    void setElements(Stackelement* stack) {
        for (auto& a: stack->variables) {
            if (variables.find(a.first) == variables.end()) {
                variables[a.first] = a.second;
                a.second->incrementstatus(1, true);
            }
        }
    }
    
    void atoms(vector<short>& v_atoms) {
        for (auto& a: variables)
            v_atoms.push_back(a.first);
    }
    
    ~Stackelement() {
        for (auto& a: variables)
            a.second->decrementstatus(1, true);
    }
};

#endif /* stack_h */
