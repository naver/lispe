/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  composing.cxx
//
//

#include "lispe.h"

/*
 Implementation Notes
 --------------------
 
 Basically, every single high level instruction is transformed into a loop.
 
 We distinguish two sorts of instructions:
 
 filtering: filter, l_take, l_drop, l_takewhile and l_dropwhile
 mapping: map, scanl, foldl, scanr, foldr, scanl1, foldl1, scanr1, foldr1
 
 The filtering methods checks on intermediate values to only keep the ones that match their conditions
 The mapping methods apply computing on each value from the initial list
 
 Mapping
 -------
 
 For instance: (map '+ '(1 2 3)) is transformed into:
 
 (setq #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 #i is the iterator variable
 #accu the accumulator in which different operations are computed
 #recipient a list in which every single #accu is stored
 
 Filtering
 ----------
 
 (filter '(< 10) '(1 2 3)) is implemented as:
 
 (setq #recipient ()) (loop #i '(1 2 3) (check (< #i 10) (push #recipient #i)))
 
 Combining
 ---------
 When these different instructions are composed together (combined together) such as:
 
 (filter '(< 20) (map '+ '(1 3 5 9 6 4)))
 -----------------------------------------
 
 Then, we keep only one single loop, in which we add our conditions:
 ----------------------------------
 
 First the map: (setq #recipient ()) (loop #i '(1 3 5 9 6 4) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 Then the filtering, whose condition now encapsulates '(push #recipient #)'
 
 (setq #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (check (< #accu 10) (push #recipient #accu)))
 
 Note that the 'check' now takes #accu as the variable to check against
 
 An other example is applying a mapping onto a mapping:
 
 (mapping '* (mapping '+ '(1 2 3 4)))
 ------------------------------------
 
 In this case, we simply modify the operation twice:
 
 The first map: (setq #recipient ()) (loop #i '(1 2 3 4) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 The second map:
 
 (setq #recipient ()) (loop #i '(1 3 5 9 6 4) (setq #accu (* (+ #i #i) (+ #i #i))) (push #recipient #accu))


 If we take the following example:
 
 (map '* (filter '(< 20) (map '+ '(1 3 5 9 6 4))))
 -------------------------------------------------
 
 a) First we create our initial loop:
 (setq #recipient ()) (loop #i '(1 3 5 9 6 4) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 b) Second, we introduce our filter:
 (setq #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (check (< #accu 10) (push #recipient #accu)))
 
 c) Third, we add the final map:
 (setq #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (check (< #accu 10) (setq ##accu (* #accu0 #accu0)) (push #recipient ##accu)))
 
 Note that we have introduced an new accumulator: ##accu, since we cannot modify the initial #accu, on which the test depends...
  
 (filter '(!= 100) (map '* (filter '(< 20) (map '+ '(1 3 5 9 6 4)))))
 --------------------------------------------------------------------
 
 The loop is now:
 
 (setq %content0 ())
 (loop %i0
    '(1 3 5 9 6 4)
    (setq %v0 (+ %i0 %i0))
    (check (< %v0 20)
       (setq %v0_ (* %v0 %v0))
       (check (!= %v0_ 100)
          (push %content0 %v0_)
       )
    )
 )
 
 Note how the last 'check' in the structure now bears on ##accu and not on #accu...

 This method is used at compile time to evaluate take, takewhile, repeat, map, filter etc..
 In order to compose them when they are embedded...
 case l_cycle
 case l_repeat
 case l_take
 case l_drop
 case l_takewhile
 case l_dropwhile
 case l_filter
 case l_map
 case l_scan(l|r)(1)
 case l_fold(l|r)(1)
 
 This section is akin to a complex macro, which transforms these instructions into LispE loops.
 Hence, we build a single loop, in which we introduce our tests and our intermediate computations.
 
 Examples:
 
 //(map '* (map '+ '(1 2 3 4 5 6 7 8 9)))
 //(map '* (filter '(< 10) '(1 3 9 10 12 34 5 7)))
 //(filter '(< 100) (map '* '(1 3 9 10 12 34 5 7)))
 //(filter '(< 100) (map '* (filter '(< 10) (map '+ '(1 2 3 4 5 6 7 8)))))
 //(map '* (take 3 (filter '(> 2) (map '+ '(1 2 3 4 5)))))
 //(map '+ (map '* (filter '(> 2) '(1 2 3 4 5))))
 //(scanl '+ 10 (take 3 '(1 2 3 4)))
 //(scanl '+ 10 (take 3 (map '* '(1 2 3 4))))
 //(scanl '+ 10 (drop 3 (map '* '(1 2 3 4 5 6 7 8 9 10))))
 //(map '* (takewhile '(< 100) '(1 2 3 4 5 10 102 12 32)))
 //(map '+ (filter '(< 100) (map '* (takewhile '(< 10) (map '+ '(1 2 3 4 5 12 34 4 6 7 8))))))
 //(map '+ (filter '(< 1000) (map '* (dropwhile '(< 10) (map '+ '(1 2 3 4 5 12 21 25 34 44 6 7 8))))))
 //(map 'eval (map 'atom (filter (\(x) (in x "prgx")) (map 'string (atoms)))))
 //(scanl1 '+ (map '* '(1 2 3 4)))
 //(scanl1 '+ (filter '(> 2) '(1 2 3 4)))
 //(scanl1 '+ (filter '(< 20) (map '* '(1 2 3 4))))
 //(scanl1 '+ (take 3 (map '* '(1 2 3 4))))
 //(scanl1 '+ (map '* (filter '(< 20) '(1 2 3 4))))
 //(defun x(v) (for i (map '+ (range 1 10 1)) (+ i 10)))
 //(defun xx(v) (for i (map '+ (range 1 10 1)) (+ i 10) (> i 3)))
 */

#define C_INS create_instruction

class Code : public Element {
public:
    Element* code;
    
    Code(LispE* lisp, Element* c) : Element(t_code) {
        code = c;
        lisp->clean_compositions.push_back(this);
    }

    bool isComposable() {
        return true;
    }

    Element* next_element() {
        return code->next_element();
    }

    Element* index(long i) {
        return code->index(i);
    }

    string toString(LispE* lisp) {
        return code->toString(lisp);
    }

    u_ustring asUString(LispE* lisp) {
        return code->asUString(lisp);
    }
};


class Action : public Element {
public:
    Element* action;
    
    Action(LispE* lisp, Element* a) : Element(t_action){
        if (a->index(0)->type == l_quote)
            a = a->index(1);
        action = a;
        lisp->clean_compositions.push_back(this);
    }
    
    bool isComposable() {
        return true;
    }

    string toString(LispE* lisp) {
        return action->toString(lisp);
    }
    
    u_ustring asUString(LispE* lisp) {
        return action->asUString(lisp);
    }

    Element* index(long i) {
        return action->index(i);
    }
    
    bool hasAction() {
        return true;
    }
    
    Element* to_code(LispE* lisp, Element* var, Element* c) {
        c = lisp->create_instruction(l_setq, var, action);
        return new Code(lisp, c);
    }
    
    Element* next_element() {
        return action;
    }
    
    Element* compose(LispE* lisp, Element* var, Element* e) {
        if (e != NULL)
            var =  ((Action*)e)->action;
        
        if (action->isList()) {
            if (action->isLambda()) {
                List* a = new Listincode();
                lisp->garbaging(a);
                a->append(action);
                a->append(var);
                action = a;
                return this;
            }
            if (action->size() == 2) {
                if (action->index(0)->isOperator()) {
                    List* a = new Listincode();
                    lisp->garbaging(a);
                    a->append(action->index(0));
                    a->append(var);
                    a->append(action->index(1));
                    action = a;
                    return this;
                }
                if (action->index(1)->isOperator()) {
                    List* a = new Listincode();
                    lisp->garbaging(a);
                    a->append(action->index(1));
                    a->append(action->index(0));
                    a->append(var);
                    action = a;
                    return this;
                }
            }
            throw new Error("Error: missing operator");
        }
        List* a = new Listincode();
        lisp->garbaging(a);
        a->append(action);
        a->append(var);
        if (action->isOperator())
            a->append(var);
        action = a;
        return this;
    }

};

class Condition : public Element {
public:
    Element* condition;
    
    Condition(LispE* lisp, Element* c, int16_t lb = t_condition) : Element(lb){
        if (c->index(0)->type == l_quote)
            c = c->index(1);
        condition = c;
        lisp->clean_compositions.push_back(this);
    }
    
    bool isComposable() {
        return true;
    }

    Element* index(long i) {
        return condition->index(i);
    }

    Element* next_element() {
        return condition;
    }

    virtual Element* to_code(LispE* lisp, Element* var, Element* c) {
        condition = lisp->create_instruction(l_check, condition);
        return this;
    }

    string toString(LispE* lisp) {
        return condition->toString(lisp);
    }
    
    u_ustring asUString(LispE* lisp) {
        return condition->asUString(lisp);
    }

    Element* compose(LispE* lisp, Element* var, Element* e) {
        Element* c = condition;
        if (condition->isList()) {
            if (condition->isLambda()) {
                List* a = new Listincode();
                lisp->garbaging(a);
                a->append(condition);
                a->append(var);
                c = a;
            }
            else {
                if (condition->size() == 2) {
                    if (lisp->isComparator(condition->index(0))) {
                        List* a = new Listincode();
                        lisp->garbaging(a);
                        a->append(condition->index(0));
                        a->append(var);
                        a->append(condition->index(1));
                        c = a;
                    }
                    else {
                        if (lisp->isComparator(condition->index(1))) {
                            List* a = new Listincode();
                            lisp->garbaging(a);
                            a->append(condition->index(1));
                            a->append(condition->index(0));
                            a->append(var);
                            c = a;
                        }
                        else
                            throw new Error("Error: missing operator");
                    }
                }
            }
        }
        else {
            List* a = new Listincode();
            lisp->garbaging(a);
            a->append(condition);
            a->append(var);
            c = a;
        }
        
        
        if (e == NULL) {
            condition = c;
            return this;
        }
        
        Element* cnd = ((Condition*)e)->condition;
        if (cnd->index(0)->label() == l_and) {
            cnd->append(c);
            return e;
        }
        
        condition = lisp->create_instruction(l_and, c, cnd);
        return this;
    }
};

class Conditiontake : public Condition {
public:
    Conditiontake(LispE* lisp, Element* c) : Condition(lisp, c, t_conditiontake) {}

    Element* to_code(LispE* lisp, Element* var, Element* c) {
        return this;
    }
    
};

class Conditiondrop : public Condition {
public:
    
    Conditiondrop(LispE* lisp, Element* c) : Condition(lisp, c, t_conditiondrop) {}

    Element* to_code(LispE* lisp, Element* var, Element* c) {
        return this;
    }
    
};

class Counter : public Element {
public:
    Element* counter;
    
    Counter(LispE* lisp, Element* c, int16_t lb) : Element(lb) {
        counter = c;
        lisp->clean_compositions.push_back(this);
    }
    
    bool isComposable() {
        return true;
    }

    Element* next_element() {
        return counter;
    }

    string toString(LispE* lisp) {
        return counter->toString(lisp);
    }

    u_ustring asUString(LispE* lisp) {
        return counter->asUString(lisp);
    }

    Element* to_code(LispE* lisp, Element* var, Element* c) {
        return this;
    }
    
    Element* compose(LispE* lisp, Element* var, Element* e) {
        if (e == NULL || counter->more(lisp,e) == false_)
            return this;
        return e;
    }
};


class Forlist : public Element {
public:
    Element* variable;
    Element* action;
    
    Forlist(LispE* lisp, Element* v, Element* a) : Element(l_for) {
        variable = v;
        action = a;
        lisp->clean_compositions.push_back(this);
    }
    
    bool isComposable() {
        return true;
    }

    string toString(LispE* lisp) {
        return variable->toString(lisp);
    }
    
    u_ustring asUString(LispE* lisp) {
        return variable->asUString(lisp);
    }


    Element* next_element() {
        return variable;
    }

    Element* to_code(LispE* lisp, Element* var, Element* c) {
        return new Action(lisp, action);
    }
    
    Element* compose(LispE* lisp, Element* var, Element* e) {
        return this;
    }
};

class Fold : public Element {
public:
    Element* initial;
    Element* action;
    Element* counter;
    Element* variable;
    Element* recipient;
    
    Fold(LispE* lisp, Element* i, Element* a, int16_t lb) : Element(lb) {
        initial = i;
        if (a->index(0)->type == l_quote)
            a = a->index(1);
        action = a;
        counter = NULL;
        lisp->clean_compositions.push_back(this);
    }
    
    bool isComposable() {
        return true;
    }

    string toString(LispE* lisp) {
        return action->toString(lisp);
    }
    
    u_ustring asUString(LispE* lisp) {
        return action->asUString(lisp);
    }

    bool hasAction() {
        return true;
    }

    Element* next_element() {
        return action;
    }

    //var is our accumulator
    Element* to_code(LispE* lisp, Element* rec_var, Element* c) {
        counter = c;
        recipient = rec_var;
        List* a = new Listincode;
        lisp->garbaging(a);
        a->append(action);
        switch (type) {
            case l_foldl:
            case l_foldl1:
            case l_scanl:
            case l_scanl1:
                a->append(counter);
                a->append(variable);
                break;
            default:
                a->append(variable);
                a->append(counter);
        }
        action = a;
        return this;
    }
    
    Element* compose(LispE* lisp, Element* var, Element* e) {
        variable = var;
        return this;
    }
    
};

//-------------------------------------------------------------------------------------------
//Methods associated with high level functions
//-------------------------------------------------------------------------------------------

Element* List::evall_repeat_cps(LispE* lisp) {
    /*
     (repeat value)
     */
    if (!lisp->composition_stack.empty())
         throw new Error("Error: cannot apply 'repeat' with a context");
    
    Element* e = new Infinitelist(lisp);
    e->append(liste[1]);
    lisp->garbaging(e);
    lisp->composition_stack.push_back(e);
    return e;
}

Element* List::evall_cycle_cps(LispE* lisp) {
    /*
     (cycle value)
     */
    if (!lisp->composition_stack.empty())
         throw new Error("Error: cannot apply 'cycle' with a context");
    Element* c = new Cyclelist(lisp);
    c->append(liste[1]);
    lisp->garbaging(c);
    lisp->composition_stack.push_back(c);
    return c;
}

Element* List::evall_map_cps(LispE* lisp) {
    /*
     (map action list)
     Five kind of actions:
     'function
     'operator
     '(lambda)
     '(operator v)
     '(v operator)
    */
    //First we push the list into the composition
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'map'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'map'");
    lisp->composition_stack.push_back(new Action(lisp, liste[1]));
    return lisp->composition_stack.back();
}

Element* List::evall_filter_cps(LispE* lisp) {
    /*
     (filter boolean list)
     Five kind of actions:
     'function
     'operator
     '(lambda)
     '(operator v)
     '(v operator)
    */
    //First we push the list into the composition
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'filter'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'filter'");

    lisp->composition_stack.push_back(new Condition(lisp, liste[1]));
    return lisp->composition_stack.back();
}

Element* List::evall_take_cps(LispE* lisp) {
    /*
     (take nb list)
     */
    
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'take'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'take'");

    lisp->composition_stack.push_back(new Counter(lisp, liste[1], t_countertake));
    return lisp->composition_stack.back();
}

Element* List::evall_drop_cps(LispE* lisp) {
    /*
     (drop nb list)
     */
    
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'drop'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'drop'");

    lisp->composition_stack.push_back(new Counter(lisp, liste[1], t_counterdrop));
    return lisp->composition_stack.back();
}

Element* List::evall_takewhile_cps(LispE* lisp) {
    /*
     (takewhile condition list)
     */
    
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'takewhile'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'takewhile'");

    lisp->composition_stack.push_back(new Conditiontake(lisp, liste[1]));
    return lisp->composition_stack.back();
}

Element* List::evall_dropwhile_cps(LispE* lisp) {
    /*
     (dropwhile condition list)
     */
    
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'dropwhile'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'dropwhile'");

    lisp->composition_stack.push_back(new Conditiondrop(lisp, liste[1]));
    return lisp->composition_stack.back();
}

Element* List::evall_for_cps(LispE* lisp) {
    if (size() < 4)
        throw new Error("Error: Wrong number of arguments for 'for'");

    if (lisp->composition_stack.empty())
        lisp->composition_stack.push_back(liste[2]);

    lisp->composition_stack.push_back(new Forlist(lisp, liste[1], liste[3]));
    return lisp->composition_stack.back();
}

Element* List::evall_foldl_cps(LispE* lisp) {
    /*
     (foldl action initial list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 4)
            throw new Error("Error: Wrong number of arguments for 'foldl'");
        lisp->composition_stack.push_back(liste[3]);
    }
    else
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'foldl'");

    lisp->composition_stack.push_back(new Fold(lisp, liste[2], liste[1], l_foldl));
    return lisp->composition_stack.back();
}

Element* List::evall_scanl_cps(LispE* lisp) {
    /*
     (foldl action initial list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 4)
            throw new Error("Error: Wrong number of arguments for 'scanl'");
        lisp->composition_stack.push_back(liste[3]);
    }
    else
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'scanl'");
    lisp->composition_stack.push_back(new Fold(lisp, liste[2], liste[1], l_scanl));
    return lisp->composition_stack.back();
}

Element* List::evall_foldr_cps(LispE* lisp) {
    /*
     (foldl action initial list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 4)
            throw new Error("Error: Wrong number of arguments for 'foldr'");
        lisp->composition_stack.push_back(liste[3]);
    }
    else
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'foldr'");
    
    lisp->composition_stack.push_back(new Fold(lisp, liste[2], liste[1], l_foldr));
    return lisp->composition_stack.back();
}

Element* List::evall_scanr_cps(LispE* lisp) {
    /*
     (foldl action initial list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 4)
            throw new Error("Error: Wrong number of arguments for 'scanr'");
        lisp->composition_stack.push_back(liste[3]);
    }
    else
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'scanr'");
    lisp->composition_stack.push_back(new Fold(lisp, liste[2], liste[1], l_scanr));
    return lisp->composition_stack.back();
}

Element* List::evall_foldl1_cps(LispE* lisp) {
    /*
     (foldl action list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'foldl1'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'foldl1'");
    lisp->composition_stack.push_back(new Fold(lisp, null_, liste[1], l_foldl1));
    return lisp->composition_stack.back();
}

Element* List::evall_scanl1_cps(LispE* lisp) {
    /*
     (foldl action list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'scanl1'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'scanl1'");
    lisp->composition_stack.push_back(new Fold(lisp, null_, liste[1], l_scanl1));
    return lisp->composition_stack.back();
}

Element* List::evall_foldr1_cps(LispE* lisp) {
    /*
     (foldl action list)
     */
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'foldr1'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'foldr1'");
    lisp->composition_stack.push_back(new Fold(lisp, null_, liste[1], l_foldr1));
    return lisp->composition_stack.back();
}

Element* List::evall_scanr1_cps(LispE* lisp) {
    if (lisp->composition_stack.empty()) {
        if (size() < 3)
            throw new Error("Error: Wrong number of arguments for 'scanr1'");
        lisp->composition_stack.push_back(liste[2]);
    }
    else
        if (size() < 2)
            throw new Error("Error: Wrong number of arguments for 'scanr1'");
    lisp->composition_stack.push_back(new Fold(lisp, null_, liste[1], l_scanr1));
    return lisp->composition_stack.back();
}


//The compose method itself

Element* LispE::compose(Element* fin) {
    static u_uchar idx_var = 48;
    
    
    u_ustring s_idx(U"%i");
    s_idx += idx_var;

    u_ustring s_recipient(U"%v");
    s_recipient += idx_var;

    u_ustring s_counter(U"%c");
    s_counter += idx_var;
    
    u_ustring s_content(U"%content");
    s_content += idx_var;
    
    Element* idx = provideAtom(s_idx);
    Element* basic_idx = idx;
    Element* recipient = provideAtom(s_recipient);
    Element* current_recipient = recipient;
    Element* content = provideAtom(s_content);
    Element* counter = provideAtom(s_counter);
    
    Element* e;
    char usedvar = false;

    
    vector<Element*> composed;
    composed.push_back(composition_stack[1]->compose(this, idx, NULL));
    Element* last = composed.back();
    if (last->label() == l_for) {
        idx = last->next_element();
        basic_idx = idx;
        e = last->to_code(this, recipient, NULL);
        composed.pop_back();
        composed.push_back(e);
        usedvar = true;
    }
    
    int16_t act;
    long i;
    for (i = 2; i < composition_stack.size(); i++) {
        last = composed.back();
        act = composition_stack[i]->label();
        if (act == last->label() && last->label() != t_code && act < l_for) {
            e = composition_stack[i]->compose(this, idx, last);
            if (e != last) {
                composed.pop_back();
                composed.push_back(e);
            }
        }
        else {
            if (act == l_for) {
                idx = composition_stack[i]->next_element();
                basic_idx = idx;
            }

            e = last->to_code(this, recipient, counter);
            act = last->label();

            if (last->hasAction()) {
                usedvar = true;
                idx = recipient;
                s_recipient += U"_";
                current_recipient = recipient;
                recipient = provideAtom(s_recipient);
            }

            if (e == last) {
                if (act >= l_foldl && act <= l_scanr1) {
                    //We use a counter then
                    s_counter += U"_";
                    counter = provideAtom(s_counter);
                }
            }
            else {
                composed.pop_back();
                composed.push_back(e);
            }
            composed.push_back(composition_stack[i]->compose(this, idx, NULL));
        }
    }
    
    last = composed.back();
     
    act = last->label();
    e = last->to_code(this, recipient, counter);

    if (last->hasAction()) {
        current_recipient = recipient;
        usedvar = true;
    }
    
    if (e == last) {
        if (act >= l_foldl && act <= l_scanr1) {
            //We use a counter then
            s_counter += U"_";
            counter = provideAtom(s_counter);
        }
    }
    else {
        composed.pop_back();
        composed.push_back(e);
    }
    
    List* initialisations = C_INS(l_block);
    List* default_init = C_INS(l_setq, content, n_emptylist);
    
    Element* loop_on_list = composition_stack[0];

    Element* return_result = content;
    Element* current = new Listincode;
    garbaging(current);
    Element* base = current;
    Element* root = NULL;
    Fold* fold;
    
    u_ustring s_to_loop_on(U"#l");
    s_to_loop_on += idx_var;
    Element* to_loop_on = provideAtom(s_to_loop_on);
    
    bool content_initialisation = false;

    for (i= 0; i < composed.size(); i++) {
        last = composed[i];
        act = last->label();
        switch (act) {
            case t_condition:
                last = last->next_element();
                current->append(last);
                current = last;
                break;
            case t_countertake:
                last = last->next_element();
                e = C_INS(l_ife, C_INS(l_eq, C_INS(l_size,content), last), C_INS(l_break));
                if (root != NULL)
                    ((List*)e)->extend((List*)root);
                root = e;
                break;
            case t_counterdrop:
                last = last->next_element();
                return_result = C_INS(l_extract, return_result, last, n_zero);
                break;
            case t_conditiontake:
                last = last->next_element();
                e = C_INS(l_ife, C_INS(l_not, last), C_INS(l_break));
                current->append(e);
                current = e;
                break;
            case t_conditiondrop:
                last = last->next_element();
                initialisations->append(C_INS(l_setq, counter, n_null));
                e = C_INS(l_ncheck, counter, C_INS(l_if, (C_INS(l_not, last)), C_INS(l_setq, counter, n_true)));
                current->append(e);
                current = e;
                s_counter += U"_";
                counter = provideAtom(s_counter);
                break;
                
            case l_foldr:
                loop_on_list = C_INS(l_reverse, loop_on_list);
            case l_foldl:
                fold = (Fold*)last;
                usedvar = 2;
                default_init = C_INS(l_setq, content, fold->counter);
                initialisations->append(C_INS(l_setq, fold->counter, fold->initial));
                current->append(C_INS(l_setq, fold->counter, fold->action));
                current->append(C_INS(l_setq, fold->recipient, fold->counter));
                break;
            case l_scanr:
                loop_on_list = C_INS(l_reverse, loop_on_list);
            case l_scanl:
                fold = (Fold*)last;
                initialisations->append(C_INS(l_setq, fold->counter, fold->initial));
                current->append(C_INS(l_setq, fold->counter, fold->action));
                current->append(C_INS(l_setq, fold->recipient, fold->counter));
                default_init->release();
                if (act == l_scanl)
                    default_init = C_INS(l_setq, content, C_INS(l_list, fold->counter));
                else
                    default_init = C_INS(l_setq, content, C_INS(l_llist, fold->counter));
                content_initialisation = true;
                break;
            case l_foldr1:
                initialisations->append(C_INS(l_setq, to_loop_on, C_INS(l_reverse, loop_on_list)));
            case l_foldl1:
                fold = (Fold*)last;
                usedvar = 2;
                default_init->release();
                default_init = C_INS(l_setq, content, fold->counter);
                if (act == l_foldl1)
                    initialisations->append(C_INS(l_setq, to_loop_on, loop_on_list));
                loop_on_list = C_INS(l_cdr, to_loop_on);
                initialisations->append(C_INS(l_setq, fold->counter, C_INS(l_car, to_loop_on)));
                current->append(C_INS(l_setq, fold->counter, fold->action));
                current->append(C_INS(l_setq, fold->recipient, fold->counter));
                break;
            case l_scanr1:
                initialisations->append(C_INS(l_setq, to_loop_on, C_INS(l_reverse, loop_on_list)));
            case l_scanl1:
                fold = (Fold*)last;
                if (act == l_scanl1)
                    initialisations->append(C_INS(l_setq, to_loop_on, loop_on_list));
                loop_on_list = C_INS(l_cdr, to_loop_on);
                initialisations->append(C_INS(l_setq, fold->counter, C_INS(l_car, to_loop_on)));
                current->append(C_INS(l_setq, fold->counter, fold->action));
                current->append(C_INS(l_setq, fold->recipient, fold->counter));
                default_init->release();
                if (act == l_scanl1)
                    default_init = C_INS(l_setq, content, C_INS(l_list, fold->counter));
                else
                    default_init = C_INS(l_setq, content, C_INS(l_llist, fold->counter));
                content_initialisation = true;
                break;
            default:
                last = last->next_element();
                current->append(last);
        }
    }
    
    List* code = C_INS(l_loop, basic_idx, loop_on_list);
    initialisations->append(default_init);
    
    if (usedvar) {
        if (usedvar == 2)
            e = C_INS(l_setq, content, current_recipient);
        else
            e = C_INS(l_push, content, current_recipient);
    }
    else
        e = C_INS(l_push, content, idx);
                            
    current->append(e);
    
    if (root == NULL)
        code->extend((List*)base);
    else {
        ((List*)root)->extend((List*)base);
        code->append((List*)root);
    }
    
    initialisations->append(code);
    code = initialisations;
    
    //Final value that is returned
    code->append(return_result);
    //cerr << code->toString(this) << endl;
    idx_var++;
    if (idx_var == 58)
        idx_var = 48;
    
    clean_compositions.clean();
    composition_stack.clear();
    fin->append(code);
    return fin;
}

//------------------------------------------------------------
void List_switch_eval::build(LispE* lisp) {
    u_ustring key;
    Element* e;
    Element* v;
    for (long i = 2; i < size(); i++) {
        e = liste[i];
        if (e->type != t_list || !e->size())
            throw new Error("Error: wrong 'switch statement'");
        
        v = e->index(0);
        if (v == true_)
            default_value = (List*)e;
        else {
            if (v->isString() || v->isNumber()) {
                key = v->asUString(lisp);
                cases[key] = (List*)e;
            }
            else
                throw new Error("Error: Unknown statement");
        }
    }
}

//--------------------------------------------------------------------------------
Element* Atome::transformargument(LispE* lisp) {
    if (name.back() == '+' || name.back() == '*' || name.back() == '%') {
        char a = name.back();
        u_ustring bare_name = name.substr(0, name.size()-1);
        int16_t l_name = lisp->encode(bare_name);
        Element* e = new Atomekleene(l_name, bare_name, a);
        return lisp->push_in_garbage(e);
    }
    return this;
}
//--------------------------------------------------------------------------------
Element* List::transformargument(LispE* lisp) {
    long sz = liste.size();

    if (!sz)
        return this;

    Element* element;
    int16_t label = liste[0]->label();

    if (label == l_quote) {
        element = new Listargumentquote(this);
        label = element->label();
        if (label >= l_void && !lisp->delegation->data_pool.check(label))
            lisp->delegation->data_pool[label] = element;
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }

    bool kleene = false;
    //We first evaluate all the next elements in the list
    for (long i = 1; i < sz; i++) {
        element = liste[i]->transformargument(lisp);
        if (element->label() == l_plus || element->label() == l_multiply || element->label() == l_mod) {
            if (liste[i-1]->argumentvalue() != NULL) {
                sz--;
                element = new Listkleene(liste[i-1], liste[i-1]->argumentvalue(), element->label());
                lisp->garbaging(element);
                liste[i-1] = element;
                liste.erase(i--);
            }
            else {
                if (i > 1)
                    throw new Error("Error: The Kleene operators (*+%) can only apply to a function call");
                else //In this case, the first element was not yet evaluated...
                    kleene = true;
            }
        }
        else
            liste[i] = element;
    }
    
    //Then we evaluate the initial label
    //This is a set description
    if (label >= l_set && label <= l_sets) {
        element = new Listargumentset(this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }

    if (label >= l_dictionary && label <= l_dictionaryn) {
        element = new Argumentdictionary(lisp, this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }

    bool sep = false;
    if (sz > 1 && liste[sz-2] == separator_) {
        sep = true;
    }


    //If it is a list, then we need to evaluate this element
    //We do not test; label == t_list, because in this case it could
    //be a constraint on the object type...
    if (liste[0]->isAtom()) {
        //This is either a data structure or a type constraint
        //It has to be integrated into a list... [string_ xxx]
        if (lisp->checkDataStructure(label)) {
            if (label >= t_atom && label <= t_maybe)
                element = new Listargumentlabel(this, label);
            else
                element = new Listargumentdata(this);
            
            lisp->removefromgarbage(this);
            return lisp->push_in_garbage(element);
        }
        
        //This is an executable...
        //In this case, it could be an embedded list of calls...
        if (liste[0]->isExecutable(lisp)) {
            element = liste.back();

            //If the last element is also an argument function, then eval returns its argument
            if (element->isArgumentFunction())
                element = new Listargumentfunction(this, element->argumentvalue());
            else {
                //We find the last atom in the sequence
                if (!element->isAtom()) {
                    for (long i = sz - 2; i > 0; i--) {
                        if (liste[i]->isAtom()) {
                            element = liste[i];
                            break;
                        }
                    }
                    if (!element->isAtom())
                        throw new Error("Error: Missing argument in defpat function");
                }
                element = new Listargumentfunction(this, element);
            }
            
            lisp->removefromgarbage(this);
            return lisp->push_in_garbage(element);
        }
    }
    
    //When liste[0] is also a liste, we need to evaluate it as well
    if (kleene) {
        element = liste[0]->transformargument(lisp);
        if (element->argumentvalue() == NULL)
            throw new Error("Error: The Kleene operators (*+%) can only apply to a function call");
        sz--;
        element = new Listkleene(element, element->argumentvalue(), element->label());
        lisp->garbaging(element);
        liste[0] = element;
        liste.erase(1);
    }
    else
        liste[0] = liste[0]->transformargument(lisp);

    if (sep) {
        element = new Listseparator(this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }
    return this;
}
