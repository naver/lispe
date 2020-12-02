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
    /* Methods in Stackelements
     
     //record an argument in the stack and stop_execution  is now in delegationcheck if it has been stored already
     //this method is used to check for unification. For isntance, if a variable 'x' has been
     //recorded and an attempt is done to record 'x' again, then if it is the same value, it is not
     //a failure.
     bool recordargument(LispE* lisp, Element* e, short label);
     
     //record a unique version of an atom in the stack
     //fails if the value has already been recorded
     bool recordingunique(Element* e, short label);
     
     //returns the function that triggered the creation of that
     //stack element. Used to execute 'self'
     Element* called()
     
     //returns the element in the stack corresponding to a variable
     //given as label
     Element* get(short label);
     
     //records a variable in memory. If the variable is alreday present, then
     //it is discarded and replaced
     Element* recording(Element* e, short label);
     
     //returns the list of variables stored in this stack element
     List* atomes(LispE*);
     
     //Number of variables stored in this stack element
     long size();
     
     //Returns the variable values as a string
     wstring asString(LispE*);
     */
    
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
            if (a.second->status == s_constant)
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
