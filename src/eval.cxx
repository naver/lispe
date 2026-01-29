/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  eval.cxx
//
//

#include "lispe.h"
#include"avl.h"
#include <math.h>
#include <algorithm>
#include <thread>
#include <chrono>

//------------------------------------------------------------------------------------------
string get_char(jag_get* h);
//------------------------------------------------------------------------------------------

void List::evalAsUString(long i, LispE* lisp, u_ustring& w) {
    Element* e = liste[i]->eval(lisp);
    w = e->asUString(lisp);
    e->release();
}

void List::evalAsNumber(long i, LispE* lisp, double& d) {
    Element* e = liste[i]->eval(lisp);
    d = e->asNumber();
    e->release();
}

void List::evalAsInteger(long i, LispE* lisp, long& d) {
    Element* e = liste[i]->eval(lisp);
    d = e->asInteger();
    e->release();
}
//------------------------------------------------------------------------------------------
char List::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] == null_)
            continue;
        if (liste[i]->check_match(lisp, value->index(i)) != check_ok)
            return i;
    }
    return check_ok;
}

char List_instance::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size() || value->label() != type)
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] == null_)
            continue;
        if (liste[i]->check_match(lisp, value->index(i)) != check_ok)
            return i;
    }
    return check_ok;
}


char LList::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    char i = 0;
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        //In this case, we skip it, no constraints...
        if (a->value == null_)
            continue;
        if (a->value->check_match(lisp, value->index(i)) != check_ok)
            return i;
        i++;
    }
    return check_ok;
}

char Floats::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] != value->index(i)->asFloat())
            return i;
    }
    return check_ok;
}

char Numbers::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] != value->index(i)->asNumber())
            return i;
    }
    return check_ok;
}

char Shorts::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] != value->index(i)->asShort())
            return i;
    }
    return check_ok;
}

char Integers::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] != value->index(i)->asInteger())
            return i;
    }
    return check_ok;
}

char Strings::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] != value->index(i)->asUString(lisp))
            return i;
    }
    return check_ok;
}

char Stringbytes::check_match(LispE* lisp, Element* value) {
    if (!value->isList() || liste.size() != value->size())
        return check_mismatch;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    for (long i = 0; i < liste.size(); i++) {
        //In this case, we skip it, no constraints...
        if (liste[i] != value->index(i)->toString(lisp))
            return i;
    }
    return check_ok;
}


//------------------------------------------------------------------------------------------

bool List::isExecutable(LispE* lisp) {
    if (liste.size())
        return (liste[0]->isExecutable(lisp));
    return false;
}

bool Atome::isExecutable(LispE* lisp) {
    return lisp->checkFunctionLabel(atome);
}

Element* List::evalthreadspace(LispE* lisp, long listsize, long i) {
    lisp->delegation->lock.locking();
    bool check = lisp->check_thread_stack;
    lisp->check_thread_stack = true;
    
    //We might need to mark the last element as being terminal
    //the block might belong to an if
    liste.back()->setterminal(terminal);
    bool previous_context = lisp->create_no_pool_element;
    lisp->create_no_pool_element = true;
        
    Element* element = null_;
    
    try {
        for (; i < listsize && element->type != l_return; i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        lisp->create_no_pool_element = previous_context;
        lisp->check_thread_stack = false;
        lisp->delegation->lock.unlocking();
        throw err;
    }
    lisp->create_no_pool_element = previous_context;
    lisp->check_thread_stack = check;
    lisp->delegation->lock.unlocking();
    return element;
}

//------------------------------------------------------------------------------------------
bool Element::isequal(LispE* lisp, Element* value) {
    return (this == null_ || value == this);
}

bool List::isequal(LispE* lisp, Element* value) {
    if (value == this)
        return true;

    if (value->isValueList())
        return value->isequal(lisp, this);

    if (!value->isList())
        return false;
        
    long sz = liste.size();
    if (!sz)
        return (value->isEmpty());
    
    long i = 0;
    if (value->type == t_llist) {
        LList* l = (LList*)value;
        u_link* u = l->liste.begin();
        
        for (; u!=NULL && i < sz; i++, u = u->next()) {
            if (!u->value->isequal(lisp, liste[i])) {
                return false;
            }
        }
        return (u == NULL && i == sz);
    }
    
    if (sz != value->size())
        return false;
    
    for (; i < sz; i++) {
        if (!liste[i]->isequal(lisp, value->index(i))) {
            return false;
        }
    }
    return true;
}

bool LList::isequal(LispE* lisp, Element* value) {
    if (value == this)
        return true;
    if (!value->isList())
        return false;
    
    if (liste.empty())
        return (value->isEmpty());

    if (value->type == t_llist) {
        LList* l = (LList*)value;
        u_link* u;
        u_link* u_l;
        for (u = liste.begin(), u_l = l->liste.begin(); u != NULL && u_l != NULL; u = u->next(), u_l = u_l->next()) {
            if (!u->value->isequal(lisp, u_l->value))
                return false;
        }
        return (u == NULL && u_l == NULL);
    }
    
    long szvalue = value->size();
    long ivalue = 0;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    u_link* i_rule = liste.begin();
    for (; i_rule != NULL && ivalue < szvalue; ivalue++, i_rule = i_rule->next()) {
        if (!i_rule->value->isequal(lisp, value->index(ivalue)))
            return false;
    }
    return (i_rule == NULL && ivalue == szvalue);
}

/*
 This is the reason why we have a 'record' Boolean.
 When we use unify in the context of a pattern function, then record is true, as Atom is then a variable name
 When we use unify to compare structures, then record is false, and if there is no match, it is an error
 */
bool Atome::isequal(LispE* lisp, Element* value) {
    //This is a case, when we record our value into the stack
    return (value == this || lisp->checkAncestor(this, value));
}

bool Floats::isequal(LispE* lisp, Element* value) {
    if (value->type == type)
        return (liste == ((Floats*)value)->liste);

    long sz = liste.size();
    if (sz != value->size())
        return false;

    if (!sz)
        return true;
    

    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        if (liste[i] != value->index(i)->asFloat()) {
            return false;
        }
    }
    return true;
}

bool Numbers::isequal(LispE* lisp, Element* value) {
    if (value->type == type)
        return (liste == ((Numbers*)value)->liste);

    long sz = liste.size();
    if (sz != value->size())
        return false;

    if (!sz)
        return true;
    

    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        if (liste[i] != value->index(i)->asNumber()) {
            return false;
        }
    }
    return true;
}

bool Shorts::isequal(LispE* lisp, Element* value) {
    if (value->type == type)
        return (liste == ((Shorts*)value)->liste);

    long sz = liste.size();

    if (sz != value->size())
        return false;

    if (!sz)
        return true;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        if (liste[i] != value->index(i)->asShort()) {
            return false;
        }
    }
    return true;
}

bool Integers::isequal(LispE* lisp, Element* value) {
    if (value->type == type)
        return (liste == ((Integers*)value)->liste);

    long sz = liste.size();
    
    if (sz != value->size())
        return false;

    if (!sz)
        return true;
    
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        if (liste[i] != value->index(i)->asInteger()) {
            return false;
        }
    }
    return true;
}

bool Strings::isequal(LispE* lisp, Element* value) {
    if (value->type == type)
        return (liste == ((Strings*)value)->liste);

    long sz = liste.size();
    if (sz != value->size())
        return false;

    if (!sz)
        return true;
    

    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        if (liste[i] != value->index(i)->asUString(lisp)) {
            return false;
        }
    }
    return true;
}

bool Stringbytes::isequal(LispE* lisp, Element* value) {
    if (value->type == type)
        return (liste == ((Stringbytes*)value)->liste);

    long sz = liste.size();
    if (sz != value->size())
        return false;

    if (!sz)
        return true;
    

    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        if (liste[i] != value->index(i)->toString(lisp)) {
            return false;
        }
    }
    return true;
}

//------------------------------------------------------------------------------------------
//In this specific case, it is a variable
Element* Atome::eval(LispE* lisp) {
    return lisp->get(atome);
}

//------------------------------------------------------------------------------
// A LispE instruction always starts with an operator or an instruction
//The evaluation function: eval section
//------------------------------------------------------------------------------

Element* Instruction::eval(LispE* lisp) {
    wstring msg =L"Error: cannot evaluate this instruction: '";
    msg += lisp->asString(label());
    msg += L"'";
    throw new Error(msg);
}
//------------------------------------------------------------------------------
// This function is called when the 'eval' instruction is executed on a string
// We need to clean the garbage after the compiling
//------------------------------------------------------------------------------
#ifdef LISPE_WASM_NO_EXCEPTION
Element* LispE::EVAL(u_ustring& code) {
    return _eval(code);
}
#endif


#ifdef LISPE_WASM
string eval_js(string code, bool& error);
void eval_js_sync(LispE* lisp, string& code, List* recall);

Element* List::evall_js(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string code;
    if (element->isList()) {
        Element* e;
        if (!element->size()) {
            element->release();
            throw new Error ("Error: empty list");
        }
        code = element->index(0)->toString(lisp);
        code += "(";
        try {
            for (long i = 1; i < element->size();i++) {
                if (i != 1)
                    code += ", ";
                e = element->index(i)->eval(lisp);
                code += e->jsonify(lisp);
                e->release();
            }
        }
        catch(Error* err) {
            element->release();
            throw err;
        }
        code += ");";
    }
    else
        code = element->toString(lisp);
    element->release();
    bool err = false;
    code = eval_js(code, err);
    if (err)
        return new Error(code);
    return lisp->provideString(code);
}

Element* List::evall_js_sync(LispE* lisp) {
    static int i = 0;
    Element* element = liste[1]->eval(lisp);
    string code;
    if (element->isList()) {
        Element* e;
        if (!element->size()) {
            element->release();
            throw new Error ("Error: empty list");
        }
        code = element->index(0)->toString(lisp);
        code += "(";
        try {
            for (long i = 1; i < element->size();i++) {
                if (i != 1)
                    code += ", ";
                e = element->index(i)->eval(lisp);
                code += e->jsonify(lisp);
                e->release();
            }
        }
        catch(Error* err) {
            element->release();
            throw err;
        }
        code += ");";
    }
    else
        code = element->toString(lisp);

    element->release();
    long sz = liste.size();
    List* recall = lisp->provideList();
    element = liste[2]->eval(lisp);
    recall->append(element);
    recall->append(null_);
    for (long i = 3; i < sz; i++) {
        element = liste[i]->eval(lisp);
        recall->append(element->quoting());
    }
    eval_js_sync(lisp, code, recall);
    return true_;
}

Element* LispE::eval(u_ustring& code) {
    long garbage_size = garbages.size();
    bool add = delegation->add_to_listing;
    delegation->add_to_listing = false;
    
    //First we compile it, some elements will be stored in the garbage
    //essentially lists and dictionaries
    //eval can be called when threads are active, we need then to protect
    //the access to the inner dictionaries
    Element* e = NULL;
    Element* element = NULL;
    try {
        element = compile_string(code);
        e = e->eval(this);
    }
    catch (Error* err) {
        e = err;
    }

    delegation->add_to_listing = add;

    //If nothing has been added to the garbage collector, we return the obtained value.
    if (garbage_size == garbages.size())
        return e;
    
    //We protect our new value, it can be saved in the garbage.
    //It's better not to lose it... We protect these elements recursively
    //Their status will be s_protect (status == 254).
    //We will put their value back to 0 at the end of the process
    e->protecting(true, this);
    
    //temporary keeps track of the elements that will be stored back in the
    //garbage...

    vector<Element*> temporary;
    while (garbages.size() != garbage_size) {
        element = garbages.back();
        // Unprotected elements are destroyed
        if (element->status == s_constant) {
            delete element;
        }
        else //This is the returned value, we want to be able to destroy it later.
            if (element->status != s_protect)
                temporary.push_back(element);
        garbages.pop_back();
    }
    
    //Then we add temporary, this way we avoid holes in the structure.
    for (const auto& a: temporary)
        garbages.push_back(a);
    
    //We de-protect... Note that we now give the status s_destructible in this case
    e->protecting(false, this);
    return e;
}
#else
Element* LispE::eval(u_ustring& code) {
    long garbage_size = garbages.size();
    bool add = delegation->add_to_listing;
    delegation->add_to_listing = false;
    
    //First we compile it, some elements will be stored in the garbage
    //essentially lists and dictionaries
    //eval can be called when threads are active, we need then to protect
    //the access to the inner dictionaries
    Element* e = NULL;
    Element* element = NULL;
    bool locked = false;
    try {
        lock();
        element = compile_string(code);
        locked = true;
        unlock();
        e = element->eval(this);
    }
    catch (Error* err) {
        if (!locked)
            unlock();
        e = err;
    }

    delegation->add_to_listing = add;

    //If nothing has been added to the garbage collector, we return the obtained value.
    if (garbage_size == garbages.size())
        return e;
    
    //We protect our new value, it can be saved in the garbage.
    //It's better not to lose it... We protect these elements recursively
    //Their status will be s_protect (status == 254).
    //We will put their value back to 0 at the end of the process
    e->protecting(true, this);
    
    //temporary keeps track of the elements that will be stored back in the
    //garbage...
    vector<Element*> temporary;
    long nb = garbages.size();
    
    while (nb != garbage_size) {
        element = garbages.back();
        // Unprotected elements are destroyed
        if (element->status == s_constant) {
            delete element;
        }
        else //This is the returned value, we want to be able to destroy it later.
            if (element->status != s_protect)
                temporary.push_back(element);
        garbages.pop_back();
        nb--;
    }
    
    //Delete section from pick-up
    //Then we add temporary, this way we avoid holes in the structure.
    for (const auto& a: temporary)
        garbages.push_back(a);
    
    //We de-protect... Note that we now give the status s_destructible in this case
    e->protecting(false, this);
    return e;
}
#endif
//--------------------------------------------------------------------------------

Element* List::evall_break(LispE* lisp) {
    int idx = infoIdx();
    if (idx) {
        lisp->checkState((Listincode*)this);
        lisp->resetStack();
    }
    return break_;
}

Element* List::eval_error(LispE* lisp) {
    if (liste.is_not_empty()) {
        wstring msg = L"Error: unknown instruction: '";
        msg += lisp->asString(liste[0]->type);
        msg += L"'";
        throw new Error(msg);
    }
    return this;
}

Element* List::evall_quote(LispE* lisp) {
    return liste[1];
}

Element* Listreturn::eval(LispE* lisp) {
    return lisp->provideReturn();
}

Element* Listreturnelement::eval(LispE* lisp) {
    return lisp->provideReturn(action->eval_terminal(lisp, terminal));
}

Element* List::evall_toclean(LispE* lisp) {
    if (lisp->current_instance == NULL)
        throw new Error("Error: this function can only be called from within a class instance");
    Element* e = liste[1]->eval(lisp);
    if (lisp->current_instance->clean != NULL)
        delete lisp->current_instance->clean;
    
    lisp->current_instance->clean = new group_instance_clean(lisp, e, lisp->current_instance->space);
    return True_;
}


Element* List::evall_return(LispE* lisp) {
    if (liste.size() == 1)
        return lisp->provideReturn(null_);
    
    return lisp->provideReturn(liste[1]->eval_terminal(lisp, terminal));
}

Element* List::evall_atomise(LispE* lisp) {
    Element* values = liste[1]->eval(lisp);
    Element* theatoms = lisp->atomise(values->asUString(lisp));
    values->release();
    return theatoms;
}

Element* List::evall_atomp(LispE* lisp) {
    Element* atome = liste[1]->eval(lisp);
    if (atome == emptylist_ || atome->isAtom())
        return True_;
    atome->release();
    return False_;
}

Element* List::evall_atoms(LispE* lisp) {
    return lisp->atomes();
}

Element* List::evall_addr_(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    uint64_t addr = (uint64_t)element;
    element->release();
    return lisp->provideInteger(addr);
}


#ifdef LISPE_WASM_NO_EXCEPTION

Element* List::evall_root(LispE* lisp) {
    size_t listsize = liste.size();

    lisp->delegation->reset_context();
    
    //We might need to mark the last element as being terminal
    //the block might belong to an if
    liste.back()->setterminal(terminal);
    
    Element* element = null_;
    
    for (size_t i = 1; i < listsize && element->type != l_return && thrown_error == NULL; i++) {
        element->release();
        element = liste[i]->eval(lisp);
    }
    
    if (element->type == l_return) {
        if (element->type == l_break)
            return null_;
        
        Element* value = element->eval(lisp);
        element->release();
        return value;
    }

    if (thrown_error != NULL) {
        element->release();
        return thrown_error;
    }
    
    return element;
}
#else
Element* List::evall_root(LispE* lisp) {
    size_t listsize = liste.size();

    lisp->delegation->reset_context();
    
    //We might need to mark the last element as being terminal
    //the block might belong to an if
    liste.back()->setterminal(terminal);
    
    Element* element = null_;
    
    for (size_t i = 1; i < listsize && element->type != l_return; i++) {
        element->release();
        element = liste[i]->eval(lisp);
    }
    
    if (element->type == l_return) {
        if (element->type == l_break)
            return null_;
        
        Element* value = element->eval(lisp);
        element->release();
        return value;
    }

    return element;
}
#endif

Element* List::evall_block(LispE* lisp) {
    long listsize = liste.size();
    
    //We might need to mark the last element as being terminal
    //the block might belong to an if
    liste.back()->setterminal(terminal);
    
    if (listsize == 2)
        return liste[1]->eval(lisp);
    
    Element* element = null_;

    for (long i = 1; i < listsize && element->type != l_return; i++) {
        element->release();
        element = liste[i]->eval(lisp);
    }

    return element;
}


Element* List::evall_emptyp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool b = element->isEmpty();
    element->release();
    return booleans_[b];
}

Element* List::evall_maplist(LispE* lisp) {
    if (liste[1]->isLambda()) {
        List_maplist_lambda_eval m(this);
        return m.eval(lisp);
    }
    else {
        List_maplist_eval m(this);
        return m.eval(lisp);
    }
}


Element* apply_op1_op2(LispE* lisp, Element* op1, Element* op2, Element* l1, Element* l2) {
    List* call = lisp->provideList();
    call->append(op2);

    Element* res;

    methodEval met = lisp->delegation->evals[op2->type];

    if (op2->isOperator()) {
        //We are applying the second operator between l1 and l2
        call->append(lisp->quoted(l1));
        call->append(lisp->quoted(l2));
        
        try {
            res = (call->*met)(lisp);
        }
        catch (Error* err) {
            call->release();
            throw err;
        }
        
        call->release();
    }
    else {
        //We need to do the job ourselves...
        res = lisp->provideList();
        call->append(lisp->quoted());
        call->append(lisp->quoted());
        
        call->in_quote(1, l1->index(0));
        call->in_quote(2, l2->index(0));
        try {
            res->append((call->*met)(lisp));
            for (long i = 1; i < l1->size(); i++) {
                call->in_quote(1, l1->index(i));
                call->in_quote(2, l2->index(i));
                res->append((call->*met)(lisp));
            }
        }
        catch (Error* err) {
            call->release();
            res->release();
            throw err;
        }
        
        call->release();
    }

    call = lisp->provideList();
    call->append(op1);
    met = lisp->delegation->evals[op1->type];

    if (op1->isOperator()) {
        //Then we do a reduce on this list with the first operator
        call->append(res->quoting());
        try {
            op2 = (call->*met)(lisp);
        }
        catch (Error* err) {
            call->release();
            res->release();
            throw err;
        }
        
        call->release();
        return op2;
    }
    
    if (!res->size()) {
        call->release();
        return res;
    }
    
    call->append(lisp->quoted());
    call->append(lisp->quoted());
    
    call->in_quote(1, res->value_on_index(lisp, (long)0));
    call->in_quote(2, res->index(1));
    
    Element* e = null_;
    try {
        e = (call->*met)(lisp);
        call->in_quote(1, e);
        for (long i = 2; i < res->size(); i++) {
            call->in_quote(2, res->index(i));
            e = (call->*met)(lisp);
            call->in_quote(1, e);
        }
    }
    catch (Error* err) {
        call->release();
        res->release();
        throw err;
    }
        
    call->release(e);
    res->release();
    return e->release_but_last();
}

//(transpose (rho 2 4 '(1 3 9 10 12 34)))
Element* List::evall_transpose(LispE* lisp) {
    Element* matrix = liste[1]->eval(lisp);
    Element* transposed_matrix = matrix->transposed(lisp);
    matrix->release();
    return transposed_matrix;
}


inline long mmin(long x, long y) {
    return (x <= y?x:y);
}


Element* List::evall_cdr(LispE* lisp) {
    Element* lst = liste[1]->eval(lisp);
    Element* c;
    
    try {
        c = lst->cdr(lisp);
        if (!lst->element_container(c))
            lst->release();
    }
    catch (Error* err) {
        lst->release();
        throw err;
    }
    
    return c;
}

Element* List::evall_conspoint(LispE* lisp) {
    LList* lst = new LList(&lisp->delegation->mark);
    long i;
    long sz = size() - 1;
    for (i = 1; i < sz; i++)
        lst->append(liste[i]);
    lst->append_as_last(lisp, liste[i]);
    return lst;
    
}

Element* List::evall_consp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool b = element->isList();
    element->release();
    return booleans_[b];
}


Element* List::evall_cyclicp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool b = element->is_cyclic();
    element->release();
    return booleans_[b];
}

Element* List::evall_clone(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    Element* copie = element->fullcopy();
    element->release();
    return copie;
}


Element* List::evall_converttoshort(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = new Short(value->asShort());
    value->release();
    return element;
}

Element* List::evall_complex(LispE* lisp) {
    double d;
    evalAsNumber(1, lisp, d);
    double i;
    evalAsNumber(2, lisp, i);
    return lisp->provideComplex(d, i);
}

Element* List::evall_real(LispE* lisp) {
    Element* e = liste[1]->eval(lisp);
    double d = e->asNumber();
    e->release();
    return lisp->provideNumber(d);
}

Element* List::evall_imaginary(LispE* lisp) {
    Element* e = liste[1]->eval(lisp);
    if (e->type == t_complex) {
        double d = ((Complexe*)e)->content.imag();
        e->release();
        return lisp->provideNumber(d);
    }
    throw new Error("Error: expecting a complex value");
}

Element* List::evall_converttointeger(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = lisp->provideInteger(value->asInteger());
    value->release();
    return element;
}


Element* List::evall_converttofloat(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = lisp->provideFloat(value->asFloat());
    value->release();
    return element;
}

Element* List::evall_converttonumber(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = lisp->provideNumber(value->asNumber());
    value->release();
    return element;
}

Element* List::evall_converttostring(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    u_ustring strvalue = value->asUString(lisp);
    Element* element = lisp->provideString(strvalue);
    value->release();
    return element;
}

Element* List::evall_converttostringbyte(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    string strvalue = value->toString(lisp);
    Element* element = new Stringbyte(strvalue);
    value->release();
    return element;
}

Element* List::evall_data(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->storeforgarbage(this);
        garbaging_values(lisp);
    }

    long listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    try {
        //We record a data structure of the form: (data (Atom x y z) (Atom x y))
        int16_t lab;
        long i = 1;
        int16_t ancestor = v_null;
        if (liste[1]->isAtom()) {
            ancestor = liste[1]->label();
            i = 2;
        }
        for (; i < listsize; i++) {
            second_element = liste[i];
            if (!second_element->isList() || !second_element->size())
                throw new Error(L"Error: A data structure can only contain non empty lists");
            lab = second_element->index(0)->label();
            if (lab == v_null)
                throw new Error(L"Error: Missing definition name in data structure");
            lisp->recordingData(second_element, lab, ancestor);
        }
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return True_;
}


Element* List::evall_deflib(LispE* lisp) {
    // we manage extensions to the language with deflib (see systeme.cxx for an example)
    if (liste._size() != 3)
        throw new Error("Error: wrong number of arguments");
    
    int16_t label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    if (lisp->globalDeclaration()) {
        if (!lisp->delegation->recordingFunction(this, label, lisp->current_space)) {
            wstring nm =L"Error: Function '";
            nm += lisp->asString(label);
            nm += L"' already declared";
            throw new Error(nm);
        }
        return this;
    }
    return lisp->recordingunique(this, label);
}

Element* List::evall_deflibpat(LispE* lisp) {
    if (liste._size() != 3)
        throw new Error("Error: wrong number of arguments");
    
    int16_t label;
    
    //We declare a function
    label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    
    Element* arguments = liste[2];
    Element* a;
    Element* idx;
    for (long i = 0; i < arguments->size(); i++) {
        idx = arguments->index(i);
        a = idx->transformargument(lisp);
        if (a != idx)
            ((List*)arguments)->liste.put(i, a);
    }

    return lisp->recordingMethod(this, label);
}

Element* List::evall_defmacro(LispE* lisp) {
    if (liste._size() < 4)
        throw new Error("Error: wrong number of arguments");
    //We declare a function
    int16_t label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    return lisp->recordingMacro(this, label);
}

Element* List::evall_defpat(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->storeforgarbage(this);
        garbaging_values(lisp);
    }

    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");
    
    int16_t label;
    
    //We declare a function
    label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    last(lisp)->setterminal();
    return lisp->recordingMethod(this, label);
}

Element* List::evall_defpred(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->storeforgarbage(this);
        garbaging_values(lisp);
    }

    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");
    
    int16_t label;
    
    //We declare a function
    label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    last(lisp)->setterminal();
    return lisp->recordingMethod(this, label);
}

Element* List::evall_defprol(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->storeforgarbage(this);
        garbaging_values(lisp);
    }

    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");
    
    int16_t label;
    
    //We declare a function
    label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    last(lisp)->setterminal();
    return lisp->recordingMethod(this, label);
}

Element* List::evall_defspace(LispE* lisp) {
    int16_t label = liste[1]->label();
    if (label == l_thread)
        throw new Error("Error: 'thread' is a reserved space name");
    
    lisp->create_name_space(label);
    return True_;
}

//(class name (x y z)
Element* List::evall_class(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    int16_t label = liste[1]->label();
    if (liste.size() == 2) {
        if (label == l_thread)
            throw new Error("Error: 'thread' is a reserved space name");
        
        lisp->create_name_space(label);
        return True_;
    }
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");

    if (lisp->globalDeclaration()) {
        if (lisp->delegation->class_pool.check(label)) {
            wstring nm =L"Error: Function '";
            nm += lisp->asString(label);
            nm += L"' already declared";
            throw new Error(nm);
        }
        return this;
    }
    return lisp->recordingunique(this, label);
}


Element* List::evall_defun(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->storeforgarbage(this);
        garbaging_values(lisp);
    }

    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");

    //We declare a function
    int16_t label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");

    if (lisp->globalDeclaration()) {
        if (!lisp->delegation->recordingFunction(this, label, lisp->current_space)) {
            wstring nm =L"Error: Function '";
            nm += lisp->asString(label);
            nm += L"' already declared";
            throw new Error(nm);
        }
        last(lisp)->setterminal();
        return this;
    }
    last(lisp)->setterminal();
    return lisp->recordingunique(this, label);
}

Element* List::evall_getchar(LispE* lisp) {
    string code = get_char(lisp->delegation->input_handler);
    return lisp->provideString(code);
}

Element* List::evall_if(LispE* lisp) {
    Element* condition = liste[1]->eval(lisp);
    char test = 3 - condition->Boolean();
    condition->release();
    if (test >= liste.size())
        return null_;
    
    return liste[test]->eval_terminal(lisp, terminal);
}

Element* List::evall_ife(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);

    if (element->Boolean()) {
        element->release();
        return liste[2]->eval_terminal(lisp, terminal);
    }

    long listsize = liste.size();
    liste.back()->setterminal(terminal);
    _releasing(element);
    
    for (long i = 3; i < listsize && element->type != l_return; i++) {
        element->release();
        element = liste[i]->eval(lisp);
    }
    return element;
}

Element* List::evall_index_zero(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* result = container;

    long listsize = liste.size() - 1;
    Element* value;
    long i = 2;

    try {
        //The user might have provided a list of indexes
        //which we use to traverse a complex hierarchical structure...
        for (i = 2; i < listsize; i++) {
            value = liste[i]->eval(lisp);
            result = result->protected_index(lisp, value);
            value->release();
        }
        
        value = liste[i]->eval(lisp);
        int16_t the_type = result->type;
        result = result->value_on_index(lisp, value);
        value->release();
        
        if (result == null_) {
            container->release();
            if (the_type == t_string || the_type == t_strings || the_type == t_sets)
                return emptystring_;
            return zero_value;
        }
        
        if (!container->element_container(result))
            container->release();
    }
    catch (Error* err) {
        container->release();
        if (container != result)
            result->release();
        throw err;
    }

    return result;
}

Element* List::evall_set_at(LispE* lisp) {
    List_set_at_eval m(this);
    return m.eval(lisp);
}

//Infix Expressions: (• x op y op z op u)
Element* List::eval_infix(LispE* lisp) {
    long listsize = liste.size();
    if (!listsize)
        return this;

    Element* e;
    Element* oper = NULL;
    Listincode* root;

    long beg = 1;
    if (liste[0]->label() != l_infix) {
        if (liste[0]->isExecutable(lisp)) {
            //we need to check for lists in the arguments, we are still under the infix hood...
            for (long i = 1; i < listsize; i++) {
                e = liste[i];
                if (e->isList()) {
                    oper = ((List*)e)->eval_infix(lisp);
                    if (oper != e) {
                        liste[i] = oper;
                        lisp->removefromgarbage(e);
                    }
                }
            }
            return this;
        }
        beg = 0;
    }

    int16_t label;
    Listincode* operations = new Listincode(((Listincode*)this)->idxinfo);
    lisp->storeforgarbage(operations);
    bool addtolast = false;
    bool checkoperator = false;
    for (long i = beg; i < listsize; i++) {
        oper = liste[i];
        if (oper->isOperator()) { //10 + -> first operator
            if (!checkoperator)
                throw new Error("Error: Infix expression is malformed");

            if (operations->size() == 1) {
                e = operations->liste[0];
                operations->liste[0] = oper;
                operations->append(e);
                addtolast = false;
            }
            else {
                label = oper->label();
                if (label == operations->liste[0]->label()) {
                    addtolast = false;
                }
                else {
                    if (label == l_plus || label == l_minus || label == l_and || label == l_xor || label == l_or) { // go up; this is how the operator priority is maintained.
                        root = new Listincode(((Listincode*)this)->idxinfo);
                        lisp->storeforgarbage(root);
                        root->append(oper);
                        root->append(operations);
                        operations = root;
                        addtolast = false;
                    }
                    else {
                        e = operations->last(lisp);
                        addtolast = true;
                        if (!e->isList() || e->index(0)->label() != label || e->size() == 2) { //go in next time
                            root = new Listincode(((Listincode*)this)->idxinfo);
                            lisp->storeforgarbage(root);
                            root->append(oper);
                            root->append(e);
                            operations->liste[operations->size() - 1] = root;
                        }
                        else {
                            oper = e->change_to_n();
                            if (oper != e) {
                                lisp->removefromgarbage(e);
                                operations->liste[operations->size() - 1] = oper;
                            }
                        }
                    }
                }
            }
            checkoperator = false;
            continue;
        }
    
        if (checkoperator)
            throw new Error("Error: Infix expression is malformed");
    
        checkoperator = true;
        e = oper->eval_infix(lisp);
        if (e != oper)
            lisp->removefromgarbage(oper);
        
        if (addtolast) {
            operations->last(lisp)->append(e);
        }
        else
            operations->append(e);
        //cout << operations->toString(lisp) << endl;
    }

    if (!checkoperator)
        throw new Error("Error: Infix expression is malformed");

    return operations;
}

Element* List::evall_input(LispE* lisp) {

    string code;
    if (liste.size() == 2) {
        u_ustring wcode;
        evalAsUString(1, lisp, wcode);
        s_unicode_to_utf8(code, wcode);
    }
    
    lisp->delegation->reading_string_function(code, lisp->delegation->reading_string_function_object);
#ifdef WIN32
    cout << std::endl;
#endif
    return lisp->provideString(code);
}



Element* List::evall_lambda(LispE* lisp) {
    if (liste.size() < 3)
        throw new Error("Error: wrong number of arguments");

    if (!liste[1]->isList())
        throw new Error(L"Error: Missing parameter list in a lambda declaration");
    last(lisp)->setterminal();
    return this;
}


Element* List::evall_last(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* value = container->last_element(lisp);
    container->release();
    return value;
}

Element* List::evall_load(LispE* lisp) {
    Element* filename = liste[1]->eval(lisp);
    string name = filename->toString(lisp);
    filename->release();
    if (liste.size() == 2)
        return lisp->load(name);
    
    uint16_t current = lisp->current_space;
    int16_t label = liste[2]->label();
    lisp->create_name_space(label);
    filename = lisp->load(name);
    lisp->current_space = current;
    return filename;
}

Element* List::evall_compile(LispE* lisp) {
    Element* the_code = liste[1]->eval(lisp);
    u_ustring code = the_code->asUString(lisp);
    the_code->release();
    return lisp->compile_eval(code);
}

Element* List::evall_next(LispE* lisp) {
    return liste[1]->eval(lisp)->next_element();
}

Element* List::evall_not(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    Element* negated = element->negate(lisp);
    element->release();
    return negated;
}

Element* List::evall_nullp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool test = element->isNULL();
    element->release();
    return booleans_[test];
}

Element* List::evall_numberp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool b = element->isNumber();
    element->release();
    return booleans_[b];
}

#ifdef MAX_STACK_SIZE_ENABLED
Element* List::evall_set_max_stack_size(LispE* lisp) {

    long m;
    if (liste.size() == 1)
        return lisp->provideInteger(lisp->stack_size_max());
    evalAsInteger(1, lisp, m);
    lisp->set_stack_max_size(m);
    return True_;
}
#endif


Element* List::evall_set_const(LispE* lisp) {
    int16_t label = liste[1]->label();
    Element* element = liste[2]->eval(lisp);
    lisp->delegation->const_values[label] = true;
    lisp->storing_global(element, label);
    if (element->status != s_constant)
        lisp->storeforgarbage(element);
    return True_;
}

Element* List::evall_setg(LispE* lisp) {
    int16_t label = liste[1]->label();
    Element* element = liste[2]->eval(lisp);
    if (!lisp->delegation->replaceFunction(element, label, lisp->current_space))
        lisp->storing_global(element, label);
    return True_;
}

#ifdef LISPE_WASM_NO_EXCEPTION
Element* List::evall_let(LispE* lisp) {
    long i;
    vector<uint16_t> labels;
    vector<Element*> values;
    long sz = size();
    Element* e;
    Element* current;

    List* variable_list = (List*)liste[1];
    long sz_varlist = variable_list->size();
    for (i = 0; i < sz_varlist; i++) {
        current = variable_list->liste[i];
        e = current->index(1)->eval(lisp);
        if (thrown_error) {
            for (i = 0; i < values.size(); i++)
                lisp->put_and_keep(values[i], NULL, labels[i]);
            return e;
        }
        labels.push_back(current->index(0)->label());
        current = lisp->record_or_replace(e, labels.back());
        values.push_back(current);
    }

    e = null_;
    for (i = 2; i < sz && !thrown_error; i++) {
        e->release();
        e = liste[i]->eval(lisp);
    }

    e->increment();
    for (i = 0; i < values.size(); i++)
        lisp->put_and_keep(values[i], NULL, labels[i]);
    e->decrementkeep();
    return e;
}
#else
Element* List::evall_let(LispE* lisp) {
    long i;
    vector<uint16_t> labels;
    vector<Element*> values;
    long sz = size();
    Element* e;
    Element* current;
    try {
        List* variable_list = (List*)liste[1];
        long sz_varlist = variable_list->size();
        for (i = 0; i < sz_varlist; i++) {
            current = variable_list->liste[i];
            e = current->index(1)->eval(lisp);
            labels.push_back(current->index(0)->label());
            current = lisp->record_or_replace(e, labels.back());
            values.push_back(current);
        }
    }
    catch(Error* err) {
        for (i = 0; i < values.size(); i++)
            lisp->put_and_keep(values[i], NULL, labels[i]);
        throw err;
    }
    
    e = null_;
    try {
        for (i = 2; i < sz; i++) {
            e->release();
            e = liste[i]->eval(lisp);
        }
        e->increment();
        for (i = 0; i < values.size(); i++)
            lisp->put_and_keep(values[i], NULL, labels[i]);
        e->decrementkeep();
        return e;
    }
    catch (Error* err) {
        for (i = 0; i < values.size(); i++)
            lisp->put_and_keep(values[i], NULL, labels[i]);
        throw err;
    }

    return True_;
}
#endif

Element* List::evall_setq(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->storing_variable(element, liste[1]->label());
    return True_;
}

Element* List::evall_setqv(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->storing_variable(element, liste[1]->label());
    return element;
}

Element* List::evall_setqi(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    int16_t label = liste[1]->label();
    List_instance* instance = lisp->current_instance;
    if (instance != NULL) {
        long i = instance->names.search(label);
        if (i != -1) {
            //In this case, we force the value onto the existing variable...
            if (element != instance->liste[i]) {
                instance->liste[i]->decrement();
                instance->liste[i] = element;
                element->increment();
            }
        }
        else {
            instance->names.push_back(label);
            instance->append(element);
        }
    }
    lisp->storing_variable(element, liste[1]->label());
    return element;
}

Element* List::evall_setqequal(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    int16_t label = liste[1]->label();
    return lisp->recording_back(element, label);
}

Element* List::evall_seth(LispE* lisp) {
    if (lisp->check_thread_stack) {
        Element* element = liste[2]->eval(lisp);
        lisp->delegation->thread_stack.storing_variable(element->duplicate_constant(lisp), liste[1]->label());
        return True_;
    }
    throw new Error("Error: this instruction can only be used in a 'threadspace' block");
}


Element* List::evall_sign(LispE* lisp) {
    return liste[1]->eval(lisp)->invert_sign(lisp);
}

Element* List::evall_size(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    long sz = element->size();
    element->release();
    return lisp->provideInteger(sz);
}

Element* List::evall_tally(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    long sz = element->tally();
    element->release();
    return lisp->provideInteger(sz);
}

Element* List::evall_sleep(LispE* lisp) {
    long tm;
    evalAsInteger(1, lisp, tm);
    std::this_thread::sleep_for(std::chrono::milliseconds(tm));
    return True_;
}


Element* List::evall_stringp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool test = element->isString();
    element->release();
    return booleans_[test];
}

//This version does handle an internal dictionary, we need
//to check each key against our current value
//This version is called when an eval is applied to a list.
Element* List::evall_switch(LispE* lisp) {
    u_ustring key;
    Element* e;
    List* code = NULL;
    long i;
    
    e = liste[1]->eval(lisp);
    key = e->asUString(lisp);
    _releasing(e);
    
    for (i = 2; i < size(); i++) {
        if (!liste[i]->isList() || !liste[i]->size())
            throw new Error("Error: wrong 'switch statement'");
        
        if (liste[i]->index(0)->isString() || liste[i]->index(0)->isNumber()) {
            if (key == liste[i]->index(0)->asUString(lisp)) {
                code = (List*)liste[i];
                break;
            }
        }
        else {
            if (liste[i]->index(0) == true_) {
                code = (List*)liste[i];
                break;
            }
        }
    }
    
    if (code == NULL) {
        u_ustring msg = U"Error: Unknown 'switch' key: '";
        msg += key;
        msg += U"'";
        throw new Error(msg);
    }

    long sz = code->liste.size();
    for (long i = 1; i < sz; i++) {
        _releasing(e);
        e = code->liste[i]->eval(lisp);
    }

    return e;
}
    
Element* List::evall_threadclear(LispE* lisp) {

    if (liste.size() == 1) {
        lisp->delegation->thread_clear_all();
        return True_;
    }
    
    u_ustring key;
    evalAsUString(1, lisp, key);
    return booleans_[lisp->delegation->thread_clear(key)];
}


Element* List::evall_threadretrieve(LispE* lisp) {
    Element* dictionary_retrieve = liste[0];
    
    
    if (liste.size() == 1) {
        //We return all as a dictionary
        return lisp->delegation->thread_retrieve_all();
    }
    
    u_ustring key;
    evalAsUString(1, lisp, key);
    dictionary_retrieve = lisp->delegation->thread_retrieve(key);
    if (dictionary_retrieve == NULL)
        return null_;
    return dictionary_retrieve;
}


Element* List::evall_threadstore(LispE* lisp) {
    u_ustring key;
    evalAsUString(1, lisp, key);
    
    Element* value = liste[2]->eval(lisp);

    bool previous_context = lisp->create_no_pool_element;
    lisp->create_no_pool_element = true;
    lisp->delegation->thread_store(key, value);
    lisp->create_no_pool_element = previous_context;

    value->release();

    return True_;
}

Element* List::evall_threadspace(LispE* lisp) {
    return evalthreadspace(lisp, liste.size(), 1);
}


Element* List::evall_throw(LispE* lisp) {
    u_ustring msg;
    evalAsUString(1, lisp, msg);
    throw new Error(msg);
}


Element* List::evall_trace(LispE* lisp) {
    if (liste.size() == 1) {
        if (lisp->trace)
            return True_;
        return False_;
    }
    
    Element* activate = liste[1]->eval(lisp);
    lisp->trace  = activate->Boolean();
    activate->release();
    return booleans_[lisp->trace];
}


Element* List::evall_trigger(LispE* lisp) {
    u_ustring key;
    evalAsUString(1, lisp, key);
    return booleans_[lisp->delegation->trigger(key)];
}


Element* List::evall_type(LispE* lisp) {
    Element* element  = liste[1]->eval(lisp);
    Element* atom_type = lisp->provideAtom(element->type);
    element->release();
    
    return atom_type;
}

Element* List::evall_unique(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* value = container->unique(lisp);
    container->release();
    return value;
}


Element* List::evall_use(LispE* lisp) {
    Element* lib_name = liste[1]->eval(lisp);
    string nom_bib = lib_name->toString(lisp);
    lib_name->release();
    if (liste.size() == 2)
        return lisp->load_library(nom_bib);

    uint16_t current = lisp->current_space;
    int16_t label = liste[2]->label();
    lisp->create_name_space(label);
    lib_name = lisp->load_library(nom_bib);
    lisp->current_space = current;
    return lib_name;
}

Element* List::evall_keys(LispE* lisp) {
    Element* dictionary = liste[1]->eval(lisp);
    Element* keys = dictionary->thekeys(lisp);
    dictionary->release();
    return keys;
}

Element* List::evall_force(LispE* lisp) {
    if (!lisp->create_no_pool_element)
        throw new Error("Error: 'force' can only be used as a thread argument");
    Element* e = liste[1]->eval(lisp);
    return new Force(e);
}

Element* List::evall_values(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* values = container->thevalues(lisp);
    container->release();
    return values;
}

Element* List::evall_wait(LispE* lisp) {
    //We wait for all threads to be finished
    while (lisp->nbjoined) {}
    return True_;
}


Element* List::evall_waiton(LispE* lisp) {
    u_ustring key;
    evalAsUString(1, lisp, key);
    lisp->delegation->waiton(key);
    return True_;
}

Element* List::evall_resetmark(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    container->resetusermark();
    container->release();
    return True_;
}

Element* List::evall_zerop(LispE* lisp) {
    long n;
    evalAsInteger(1, lisp, n);
    return booleans_[!n];
}

Element* List::evall_link(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    
    Element* atome = liste[2]->eval(lisp);
    if (!atome->isAtom()) {
        atome->release();
        throw new Error("Error: the second argument must be an atom");
    }
    
    u_ustring identifier;
    evalAsUString(1, lisp, identifier);

    //The first atom is replaced with the second atom code...
    lisp->replaceAtom(identifier, atome->label());
    return True_;
}

Element* List::evall_zipwith(LispE* lisp) {
    if (liste[1]->isLambda()) {
        List_zipwith_lambda_eval m(this);
        return m.eval(lisp);
    }
    else {
        List_zipwith_eval m(this);
        return m.eval(lisp);
    }
}

Element* Enumlist::eval(LispE* lisp) {
    if (lisp != NULL) {
        List* l = lisp->provideList();
        List* sub;
        for (long  i = 0; i < lst->size(); i++) {
            sub = lisp->provideList();
            sub->append(lisp->provideInteger(i + init));
            sub->append(lst->index(i)->copying(true));
            l->append(sub);
        }
        return l;
    }
    List* l = new List();
    List* sub;
    for (long  i = 0; i < lst->size(); i++) {
        sub = new List();
        sub->append(new Integer(i + init));
        sub->append(lst->index(i)->copying(true));
        l->append(sub);
    }
    return l;
}

Element* List::evall_this(LispE* lisp) {
    if (lisp->current_instance != NULL)
        return lisp->current_instance;
    return null_;
}

#ifdef MACDEBUG
//This is a stub function, which is used to focus on specific function debugging
Element* List::evall_debug_function(LispE* lisp) {
    long occupation = 0;
    for (const auto& a: __indexes) {
        if (a != NULL) {
            occupation++;
        }
    }
    return lisp->provideInteger(occupation);
}
#endif



