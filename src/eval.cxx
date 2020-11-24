/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  elements.cxx
//
//

#include "lispe.h"
#include <math.h>
#include <algorithm>
#include <thread>
#include <chrono>

//------------------------------------------------------------------------------------------
#define _releasing(f) f->release();f=null_
//------------------------------------------------------------------------------------------
class Comparison {
public:
    List compare;
    LispE* lisp;
    
    Comparison(LispE* l, Element* comp) {
        lisp = l;
        compare.append(comp);
        compare.append(null_);
        compare.append(null_);
        
    }
    
    bool eval(Element* i, Element* j) {
        compare.liste[1] = i;
        compare.liste[2] = j;
        return compare.eval(lisp)->Boolean();
        
    }
    
    bool checkComparison(Element* i) {
        return eval(i,i);
    }
    
    bool operator() (Element* i, Element* j) {
        return eval(i, j);
    }
};

//------------------------------------------------------------------------------------------
Element* range(LispE* lisp, double init, double limit, double inc) {
    
    List* range_list = emptylist_;
    
    double d = (limit - init) / inc;
    if (d<0)
        d *= -1;
    
    if (init > limit && inc > 0)
        inc *= -1;
    
    if (d <= 100000) {
        if (inc == 0)
            return range_list;
        
        range_list = new List;
        range_list->liste.reserve((long)d);
        //Integers ?
        if (inc == (long)inc && init == (long)init && limit == (long)limit) {
            if (inc > 0) {
                for (long i = init; i <= limit; i += inc) {
                    range_list->append(lisp->provideInteger(i));
                }
            }
            else {
                for (long i = init; i >= limit; i += inc)
                range_list->append(lisp->provideInteger(i));
            }
        }
        else {
            if (inc > 0) {
                for (double i = init; i <= limit; i += inc) {
                    range_list->append(lisp->provideNumber(i));
                }
            }
            else {
                for (double i = init; i >= limit; i += inc)
                range_list->append(lisp->provideNumber(i));
            }
        }
        return range_list;
    }
    throw new Error("Error: Exceeding range");
}

//------------------------------------------------------------------------------------------
void List::evalAsString(long i, LispE* lisp, wstring& w) {
    Element* e = liste[i]->eval(lisp);
    w = e->asString(lisp);
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

//------------------------------------------------------------------------------------------

bool List::isExecutable(LispE* lisp) {
    if (liste.size())
        return (liste[0]->isExecutable(lisp));
    return false;
}

bool Atom::isExecutable(LispE* lisp) {
    return lisp->checkFunctionLabel(atome);
}
//------------------------------------------------------------------------------------------
//null_ unifies with all
bool Element::unify(LispE* lisp, Element* value, bool record) {
    return (this == null_ || value == this);
}

//We recursively traverse the structure to find a match for value variables that complies with the keys...
//We want to handle cases such as: pattern ( {x:v y:v}) with a constraint on v being the same value
bool Dictionary_as_list::traverse(LispE* lisp, Dictionary* d, vector<wstring>& keys, vector<long>& consummed, long di, long i, bool record) {
    if (i == keyvalues.size()) {
        return (consummed.size() == keys.size());
    }
    
    Element* k = keyvalues[i];
    Element* v = valuevalues[i];
    if (k == separator_) {
        if (v == null_)
            return true;
        if (consummed.size() == keys.size())
            return v->unify(lisp, emptydictionary_, record);
        long ii,jj;
        bool found = false;
        Dictionary* dico = new Dictionary;
        for (ii = 0; ii < keys.size(); ii++) {
            found = false;
            for (jj = 0; jj < consummed.size(); jj++) {
                if (consummed[jj] == ii) {
                    found = true;
                    break;
                }
            }
            if (found)
                continue;
            dico->recording(keys[ii], d->dictionary[keys[ii]]);
        }
        if (!v->unify(lisp, dico, record)) {
            dico->release();
            return false;
        }
        return true;
    }
    
    if (di == keys.size())
        return false;
    
    
    bool removekey = false;
    bool removevalue = false;
    if (record) {
        removekey = (k != null_ && k->isAtom() && !lisp->checkLabel(k->label()));
        removevalue = (v != null_ && v->isAtom() && !lisp->checkLabel(v->label()));
    }
    if (v->unify(lisp, d->dictionary[keys[di]], record) && k->unify(lisp,lisp->provideString(keys[di]), record)) {
        consummed.push_back(di);
        if (traverse(lisp, d, keys, consummed, di+1, i+1, record))
            return true;
        consummed.pop_back();
    }
    
    if (removekey)
        lisp->removefromstack(k->label());
    if (removevalue)
        lisp->removefromstack(v->label());
    return traverse(lisp, d, keys, consummed, di+1, i, record);
}

bool Dictionary_as_list::traverse(LispE* lisp, Dictionary_n* d, vector<double>& keys, vector<long>& consummed, long di, long i, bool record) {
    if (i == keyvalues.size()) {
        return (consummed.size() == keys.size());
    }
    
    Element* k = keyvalues[i];
    Element* v = valuevalues[i];
    if (k == separator_) {
        if (v == null_)
            return true;
        if (consummed.size() == keys.size())
            return v->unify(lisp, emptydictionary_, record);
        long ii,jj;
        bool found = false;
        Dictionary_n* dico = new Dictionary_n;
        for (ii = 0; ii < keys.size(); ii++) {
            found = false;
            for (jj = 0; jj < consummed.size(); jj++) {
                if (consummed[jj] == ii) {
                    found = true;
                    break;
                }
            }
            if (found)
                continue;
            dico->recording(keys[ii], d->dictionary[keys[ii]]);
        }
        if (!v->unify(lisp, dico, record)) {
            dico->release();
            return false;
        }
        return true;
    }
    
    if (di == keys.size())
        return false;
    
    bool removekey = false;
    bool removevalue = false;
    if (record) {
        removekey = (k != null_ && k->isAtom() && !lisp->checkLabel(k->label()));
        removevalue = (v != null_ && v->isAtom() && !lisp->checkLabel(v->label()));
    }
    if (v->unify(lisp, d->dictionary[keys[di]], record) && k->unify(lisp,lisp->provideNumber(keys[di]) , record)) {
        consummed.push_back(di);
        if (traverse(lisp, d, keys, consummed, di+1, i+1, record))
            return true;
        consummed.pop_back();
    }
    
    if (removekey)
        lisp->removefromstack(k->label());
    if (removevalue)
        lisp->removefromstack(v->label());
    return traverse(lisp, d, keys, consummed, di+1, i, record);
}

bool Dictionary_as_list::unify(LispE* lisp, Element* value, bool record) {
    if (!value->isDictionary())
        return false;
    
    long ksz = keyvalues.size();
    if (ksz == 0)
        return (value->size() == 0);
    
    
    /*
     We have different cases:
     
     a) purekeys means that all keys in the pattern are actual values: string or number
     b) mxkeyvalue defines the position of the last true value in the list of keys: (k1 k2 k3 var1 var2...)
     c) the rest of the patterns after mxkeyvalue are variables that must be instantiated with the arguments (var1 var2...)
     d) the last element in the pattern might be the rest of the dictionary operator: (k1:v k2:v $:var)
     */
    
    long i = 0;
    Element* e;
    if (purekeys) {
        //it is a perfect match
        if (value->size() != ksz)
            return false;
        
        //Each key is a value, not an atom...
        for (; i < ksz; i++) {
            e = value->checkkey(lisp, keyvalues[i]);
            if (e == null_ || !valuevalues[i]->unify(lisp, e, record))
                return false;
        }
        return true;
    }
    
    if (keyvalues.back() == separator_) {
        if (value->size() < ksz-1)
            return false;
    }
    else {
        if (value->size() != ksz)
            return false;
    }
    
    if (mxkeyvalue) {
        for (i = 0; i < mxkeyvalue; i++) {
            e = value->checkkey(lisp, keyvalues[i]);
            if (e == null_ || !valuevalues[i]->unify(lisp, e, record))
                return false;
        }
    }
    
    bool found;
    long j;
    vector<long> consummed;
    if (value->type == t_dictionary) {
        vector<wstring> keys;
        wstring k;
        for (auto& a : ((Dictionary*)value)->dictionary) {
            k = a.first;
            if (mxkeyvalue) {
                found = false;
                for (j = 0; j < mxkeyvalue; j++) {
                    if (keyvalues[j]->equalvalue(k)) {
                        found = true;
                        break;
                    }
                }
                if (found)
                    continue;
            }
            keys.push_back(a.first);
        }
        
        return traverse(lisp, (Dictionary*)value, keys, consummed, 0, mxkeyvalue, record);
    }
    
    vector<double> keys;
    for (auto& a : ((Dictionary_n*)value)->dictionary) {
        if (mxkeyvalue) {
            found = false;
            for (j = 0; j < mxkeyvalue; j++) {
                if (keyvalues[j]->equalvalue(a.first)) {
                    found = true;
                    break;
                }
            }
            if (found)
                continue;
        }
        keys.push_back(a.first);
    }
    return traverse(lisp, (Dictionary_n*)value, keys, consummed, 0, mxkeyvalue, record);
}

bool List::unify(LispE* lisp, Element* value, bool record) {
    long sz = liste.size();
    if (!sz)
        return (value->size() == 0);
    
    bool rec = true;
    short ilabel = v_null;
    if (record) {
        if (liste[0]->label() == l_quote)
            return liste[1]->unify(lisp, value, false);
        
        ilabel = lisp->extractlabel(this);
        
        //First case, this is one of the basic types
        if (value->type == ilabel && ilabel >= t_atom && ilabel <= t_maybe) {
            return (liste.back() != null_ && liste.back()->unify(lisp, value, record));
        }
        
        rec = (ilabel == v_null);
        
        /*
         Second, it is not a data structure, but it is either a function or an instruction.
         If there is a variable to unify with, it is ALWAYS the last one in the list, whatever the depth.
         If the order is wrong, then you can use 'flip' to re-order local function arguments.
         For instance:
         (defpat mod3 ((eq 0 (flip (% 3 x)))) ...)
         
         In this case, 'x' is the last element in the list, however since the arguments for 'mod'
         are in the wrong order (we want: (% x 3)), we use 'flip' to invert the arguments.
         
         If it does not make the trick, then implement a function, which you will call instead.
         
         isExecutable returns true is the element is an instruction or a function...
         */
        
        if (rec && isExecutable(lisp)) {
            //If it is a function embedding: (flip (in 'str x))
            Element* exec = liste.back();
            while (exec->isExecutable(lisp))
                exec = exec->last();
            if (exec == null_ || !exec->unify(lisp, value, record))
                return false;
            try {
                value = eval(lisp);
            }
            catch(Error* err) {
                err->release();
                return false;
            }
            return value->Boolean();
        }
    }
    else {
        if (value == this)
            return true;
        rec = (lisp->extractlabel(liste[0]) == v_null);
    }
    
    if (!value->isList())
        return false;
    
    long szvalue = value->size();
    if (sz > 1 && liste[sz-2] == separator_) {
        if (szvalue < sz-2)
            return false;
    }
    else {
        if (szvalue != sz)
            return false;
    }
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        //In this case, we skip it, no constraints...
        //This is a case of a match with list division
        if (liste[i] == separator_) {
            //The parameter list should only contain one last element:
            if (i != sz - 2)
                return false;
            //We do not care about the rest of the list
            if (liste[i+1] == null_)
                return true;
            //we need to build a sublist out of value...
            List* sublist;
            if (i == szvalue)
                sublist = emptylist_;
            else {
                sublist = new List;
                for (sz = i; sz < szvalue; sz++)
                    sublist->append(value->index(sz));
            }
            if (!liste[i+1]->unify(lisp, sublist, rec)) {
                sublist->release();
                return false;
            }
            return true;
        }
        if (liste[i] != null_ && !liste[i]->unify(lisp, value->index(i), rec))
            return false;
        rec = record;
    }
    return true;
}

/*
 This is the reason why we have a 'record' Boolean.
 When we use unify in the context of a pattern function, then record is true, as Atom is then a variable name
 When we use unify to compare structures, then record is false, and if there is no match, it is an error
 */
bool Atom::unify(LispE* lisp, Element* value, bool record) {
    //This is a case, when we record our value into the stack
    return (value == this || lisp->checkAncestor(this, value) || (record && (this == null_ || lisp->recordargument(value, atome))));
}
//------------------------------------------------------------------------------------------

Element* List::evalpattern(LispE* lisp, short function_label) {
    List arguments;
    Element* element;
    Element* body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    short ilabel = -1;
    short sublabel = -1;
    char match;

    try {
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractlabel(element);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    arguments.clear();
                    if (match == check_mismatch)
                        throw new Error(L"Error: Size mismatch between argument list and data structure definition");
                    else {
                        std::wstringstream message;
                        message << L"Error: Mismatch on argument: " << match;
                        message << " (" << lisp->asString(lisp->getDataStructure(ilabel)->index(match)->label()) << " required)";
                        throw new Error(message.str());
                    }
                }
            }
            //We keep the track of the first element as it used as an index to gather pattern methods
            if (i == 1)
                sublabel = ilabel;
            arguments.append(element->duplicate_constant_container());
        }
    }
    catch(Error* err) {
        arguments.clear();
        throw err;
    }


    ilabel = 0;
    match = 0;
    body = lisp->getMethod(function_label,sublabel,ilabel);
    while (!match) {
        while (body != null_) {
            element = body->index(2);
            if (element->size() == nbarguments) {
                match = true;
                lisp->push(body);
                for (i = 0; i < element->size(); i++) {
                    if (!element->index(i)->unify(lisp, arguments.liste[i], true)) {
                        match = false;
                        break;
                    }
                }
                if (match)
                    break;
                
                lisp->pop();
            }
            body = lisp->getMethod(function_label,sublabel,++ilabel);
        }
        if (!match && sublabel != v_null) {
            sublabel = v_null;
            ilabel = 0;
            //We check, if we have a roolback function
            body = lisp->getMethod(function_label,v_null,ilabel);
        }
        else
            break;
    }
    
    if (body == null_) {
        arguments.clear();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        throw new Error(message);
    }
    
    element = null_;
    try {
        for (i = 3; i < body->size() && element->type != l_return; i++) {
            element->release();
            element = body->index(i)->eval(lisp);
        }
        
        if (element->type == l_return) {
            Element* ret = element;
            element = ret->eval(lisp);
            ret->release();
            //This version protects 'e' from being destroyed in the stack.
            arguments.clear();
            return lisp->pop(element);
        }
    }
    catch(Error* err) {
        arguments.clear();
        lisp->pop();
        throw err;
    }

    arguments.clear();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}
//------------------------------------------------------------------------------------------
void List::evalthread(Element* body, LispE* lisp) {
    Element* element = lisp->delegation->_TERMINAL;
    
    try {
        while (element == lisp->delegation->_TERMINAL) {
            element = null_;
            for (long i = 3; i < body->size() && element != lisp->delegation->_TERMINAL && element->type != l_return; i++) {
                element->release();
                element = body->index(i)->eval(lisp);
            }
            
            if (element->type == l_return) {
                element->eval(lisp)->release();
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                return;
            }
        }
    }
    catch(Error* err) {
        err->release();
        return;
    }
    element->release();
}

void launchthread(LispE* call) {
    call->current_thread->evalthread(call->current_body, call);
    LispE* lisp = call->thread_ancestor;
    delete call;
    lisp->nbjoined--;
}

//This function is only used to compare the number of
//parameters of a function and its arguments
long List::argumentsize(LispE* lisp, long sz) {
    if (liste.size() == sz) {
        while (sz > 0 && liste[sz-1]->isList())
            sz--;
        return sz;
    }
    if (liste.size() < sz) {
        //If the last argument is a list
        if (liste.back()->isNotEmptyList() && liste.back()->index(0) == emptylist_)
            return liste.size()-1;
        return -1;
    }
    //In this case, the difference in size should only be made up of listings.
    for (long i = liste.size() -1; i >= sz; i--) {
        if (!liste[i]->isList())
            return -1;
    }
    return sz;
}

void List::sameSizeNoTerminalArguments(LispE* lisp, LispE* thread_lisp, Element* data, Element* parameters, bool threading) {
    List temporaryargs;
    long i;
    bool pushed = false;
    try {
        for (i = 1; i <= parameters->size(); i++)
            temporaryargs.liste.push_back(liste[i]->eval(lisp));
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        thread_lisp->push(data);
        pushed = true;
        // For each of the parameters we record in the stack
        short label;
        for (i = 0; i < parameters->size(); i++) {
            label = parameters->index(i)->label();
            if (label == v_null) {
                while (i < parameters->size()) {
                    temporaryargs.liste[i++]->release();
                }
                thread_lisp->pop();
                throw new Error(L"Error: Wrong parameter description");
            }

            data = temporaryargs.liste[i];

            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            if (threading) {
                //containers should be duplicated...
                data = data->copying(false);
            }
            
            thread_lisp->recording(data, label);
        }
    }
    catch (Error* err) {
        while (i < temporaryargs.liste.size()) {
            temporaryargs.liste[i++]->release();
        }
        if (pushed)
            thread_lisp->pop();
        throw err;
    }
}

void List::sameSizeTerminalArguments(LispE* lisp, Element* parameters) {
    Element* data;
    long i;
    short label;

    try {
        // For each of the parameters we record in the stack
        for (i = 0; i < parameters->size(); i++) {
            label = parameters->index(i)->label();
            data = liste[i+1]->eval(lisp);
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            lisp->recording(data, label);
        }
    }
    catch (Error* err) {
        throw err;
    }
}

void List::differentSizeNoTerminalArguments(LispE* lisp, LispE* thread_lisp, Element* data, Element* parameters,
                                   long nbarguments, long defaultarguments, bool threading) {
    long i;
    List temporaryargs;
    bool pushed = false;
    try {
        //The stack is increased by one element to keep track of the calls.
        for (i = 0; i < nbarguments; i++) {
            temporaryargs.liste.push_back(liste[i+1]->eval(lisp));
        }
        for (;i < parameters->size(); i++) {
            temporaryargs.liste.push_back(NULL);
        }
        
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        thread_lisp->push(data);
        pushed = true;
        // For each of the parameters we record in the stack
        short label;
        Element* element;
        for (i = 0; i < parameters->size(); i++) {
            data = temporaryargs.liste[i];
            element = parameters->index(i);
            
            if (element->isList()) {
                if (element->index(0) == emptylist_) {
                    //This is a specific case, when we store the rest of the arguments
                    //in a list
                    if (element->size() == 2) {
                        //We have our variable:
                        label = element->index(1)->label();
                        //We create a list:
                        List* l = new List;
                        //We store the rest of the arguments in it...
                        if (data != NULL) {
                            l->append(data);
                            i++;
                            while (i < nbarguments) {
                                l->append(temporaryargs.liste[i]);
                                i++;
                            }
                        }
                        data = l;
                        i = parameters->size();
                    }
                    else {
                        while (i < nbarguments) {
                            temporaryargs.liste[i++]->release();
                        }
                        thread_lisp->pop();
                        throw new Error(L"Error: Wrong parameter description");
                    }
                }
                else {
                    label = element->size();
                    if (!label) {
                        while (i < nbarguments) {
                            temporaryargs.liste[i++]->release();
                        }
                        thread_lisp->pop();
                        throw new Error(L"Error: Wrong parameter description");
                    }
                    //If the argument was not provided, then we rely on the default value
                    //if it is available
                    if (data == NULL) {
                        if (label == 2)
                            data = element->index(1)->eval(lisp); //default value
                        else
                            data = null_;
                    }
                    label = element->index(0)->label();
                }
            }
            else
                label = element->label();
            
            if (label == v_null) {
                while (i < nbarguments) {
                    temporaryargs.liste[i++]->release();
                }
                thread_lisp->pop();
                throw new Error(L"Error: Wrong parameter description");
            }


            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            if (threading) {
                //containers should be duplicated...
                data = data->copying(false);
            }
            
            thread_lisp->recording(data, label);
        }
    }
    catch (Error* err) {
        long mn = (temporaryargs.size() < nbarguments) ? temporaryargs.size() : nbarguments;
        while (i < mn) {
            temporaryargs.liste[i++]->release();
        }
        if (pushed)
            thread_lisp->pop();
        throw err;
    }
}

void List::differentSizeTerminalArguments(LispE* lisp, Element* parameters, long nbarguments, long defaultarguments) {
    try {
        //The stack is increased by one element to keep track of the calls.
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        
        // For each of the parameters we record in the stack
        short label;
        Element* element;
        Element* data;
        
        for (long i = 0; i < parameters->size(); i++) {
            data = (i < nbarguments) ? liste[i+1]->eval(lisp) : NULL;
            
            element = parameters->index(i);
            //This is the zone when arguments can be implicit
            if (element->isList()) {
                if (element->index(0) == emptylist_) {
                    //This is a specific case, when we store the rest of the arguments
                    //in a list
                    if (element->size() == 2) {
                        //We have our variable:
                        label = element->index(1)->label();
                        //We create a list:
                        List* l = new List;
                        //We store the rest of the arguments in it...
                        if (data != NULL) {
                            l->append(data);
                            i++;
                            while (i < nbarguments) {
                                l->append(liste[i+1]->eval(lisp));
                                i++;
                            }
                        }
                        data = l;
                        i = parameters->size();
                    }
                    else {
                        throw new Error(L"Error: Wrong parameter description");
                    }
                }
                else {
                    label = element->size();
                    if (!label)
                        throw new Error(L"Error: Wrong parameter description");
                    //If the argument was not provided, then we rely on the default value
                    //if it is available
                    if (data == NULL) {
                        if (label == 2)
                            data = element->index(1)->eval(lisp); //default value
                        else
                            data = null_;
                    }
                    label = element->index(0)->label();
                }
            }
            else
                label = element->label();
            
            if (label == v_null) {
                throw new Error(L"Error: Wrong parameter description");
            }
            lisp->recording(data, label);
        }
    }
    catch (Error* err) {
        throw err;
    }
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::evalfunction(Element* body, LispE* lisp) {
    // It is either a lambda or a function
    //otherwise it's an error
    LispE* thread_lisp = lisp;
    Element* parameters;
    Element* element;
    
    long nbarguments = liste.size()-1;
    long defaultarguments = 0;
    long i;
    int bodystart = 3;
    short function_type = body->index(0)->type;
    bool threading = false;
    
    switch (function_type) {
        case l_dethread:
            //The first call to the execution of a thread triggers
            //the creation of a new LispE environment, copied from the current one...
            //current_thread corresponds to our current List, which is being executed...
            //We keep a track of it, in order to avoid creating unlimited threads
            //if this function is called recursively...
            if (thread_lisp->current_body != body) {
                thread_lisp = new LispE(lisp, this, body);
                threading = true;
            }
        case l_deflib:
        case l_defun:
            //the third element is the argument list .
            //we need our body to be the same number
            parameters = body->index(2);
            defaultarguments = parameters->argumentsize(lisp, nbarguments);
            if (defaultarguments == -1) {
                wstring message = L"Error: Wrong number of arguments in call to: '(";
                message += body->index(1)->asString(lisp);
                message += L" ";
                message += body->index(2)->asString(lisp);
                message += L"...)'";
                throw new Error(message);
            }
            break;
        case l_lambda:
            parameters = body->index(1);
            defaultarguments = parameters->argumentsize(lisp, nbarguments);
            if (defaultarguments == -1) {
                wstring message = L"Error: Wrong number of arguments in: '(lambda ";
                message += body->index(1)->asString(lisp);
                message += L"...)'";
                throw new Error(message);
            }
            bodystart = 2;
            break;
        default: {
            //It could be a data structure...
            function_type = body->index(0)->label();
            Element* data = lisp->getDataStructure(function_type);
            List* values = new List;
            values->append(data->index(0));
            try {
                for (i = 1; i <= nbarguments; i++) {
                    element = liste[i]->eval(lisp);
                    values->append(element->duplicate_constant_container());
                }
            }
            catch(Error* err) {
                values->clear();
                delete values;
                throw err;
            }
            
            char res = data->check_match(lisp,values);
            if (res != check_ok) {
                values->clear();
                delete values;
                if (res == check_mismatch)
                    throw new Error(L"Error: Size mismatch between argument list and data structure definition");
                else {
                    std::wstringstream message;
                    message << L"Error: Mismatch on argument: " << (int)res;
                    message << " (" << lisp->asString(data->index((int)res)->label()) << " required)";
                    throw new Error(message.str());
                }
            }
            values->type = t_data;
            return values;
        }
    }
    
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    
    //((lambda (x y) (if (eq x 0) (* 2 y) (self (- x 1) (+ y 2)))) 20 1)
    //( (lambda (x) (if (eq x 1) 1 (+ x (self (- x 1))))) 20)
    
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    if (terminal && lisp->called() == body) {
        if (defaultarguments == parameters->size())
            sameSizeTerminalArguments(lisp, parameters);
        else
            differentSizeTerminalArguments(lisp, parameters, nbarguments, defaultarguments);
        return lisp->delegation->_TERMINAL;
    }
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments(lisp, thread_lisp, body, parameters, threading);
    else
        differentSizeNoTerminalArguments(lisp, thread_lisp, body, parameters, nbarguments, defaultarguments, threading);
    
    //Checking if this is the first call to the thread
    if (threading) {
        //The thread can be of course recursive, but we do not want this recursivity
        //to trigger a new thread at each call...
        //We only create a thread once... The next calls will be executed
        //as regular functions
        std::thread* tid = new std::thread(launchthread, thread_lisp);
        if (tid == NULL) {
            delete thread_lisp;
            throw new Error("Error: Too many threads created. Cannot execute it...");
        }
        return true_;
    }
            
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    element = lisp->delegation->_TERMINAL;
    //We do not try to execute the code again, we simply returns back to the original loop.

    try {
        while (element == lisp->delegation->_TERMINAL) {
            element = null_;
            for (i = bodystart; i < body->size() && element != lisp->delegation->_TERMINAL && element->type != l_return; i++) {
                element->release();
                element = body->index(i)->eval(lisp);
            }
            if (element->type == l_return) {
                Element* ret = element;
                element = ret->eval(lisp);
                ret->release();
                //This version protects 'e' from being destroyed in the stack.
                return lisp->pop(element);
            }
        }
    }
    catch (Error* err) {
        lisp->pop();
        throw err;
    }
    
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}
//------------------------------------------------------------------------------------------
//In this specific case, it is a variable
Element* Atom::eval(LispE* lisp) {
    return lisp->get(atome);
}
//------------------------------------------------------------------------------
// A LispE instruction always starts with an operator or an instruction
//------------------------------------------------------------------------------
void LispE::display_trace(List* e) {
    if (trace) {
        if (!activate_on_breakpoints(e)) {
            long nb = stackSize();
            string space(nb, ' ');
            cout << "(" << delegation->current_line << ") " << nb << ":" << space << e->toString(this) << endl;
        }
    }
}

//------------------------------------------------------------------------------
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
Element* LispE::eval(string code) {
    long garbage_size = garbages.size();
    delegation->add_to_listing = false;
    
    //First we compile it, some elements will be stored in the garbage
    //essentially lists and dictionaries
    //eval can be called when threads are active, we need then to protect
    //the access to the inner dictionaries
    Element* e = NULL;
    bool locked = false;
    try {
        lock();
        e = compile(code);
        locked = true;
        unlock();
        e = e->eval(this);
    }
    catch (Error* err) {
        if (!locked)
            unlock();
        e = err;        
    }
    
    //If nothing has been added to the garbage collector, we return the obtained value.
    if (garbage_size == garbages.size())
        return e;
    
    //We protect our new value, it can be saved in the garbage.
    //It's better not to lose it... We protect these elements recursively
    //Their status will be s_protect (status == 254).
    //We will put their value back to 0 at the end of the process
    e->protecting(true);
    
    //temporary keeps track of the elements that will be stored back in the
    //garbage...
    Element* element;
    vector<Element*> temporary;
    for (long index = garbage_size; index < garbages.size(); index++) {
        element = garbages[index];
        // Unprotected elements are destroyed
        if (garbages[index]->status == s_constant) {
            delete garbages[index];
        }
        else //This is the returned value, we want to be able to destroy it later.
            if (garbages[index]->status != s_protect)
                temporary.push_back(garbages[index]);
    }
    
    //Delete section from pick-up
    garbages.erase(garbages.begin()+garbage_size, garbages.end());
    //Then we add temporary, this way we avoid holes in the structure.
    for (auto& a: temporary)
        garbages.push_back(a);
    
    //We de-protect... Note that we now give the status s_destructible in this case
    e->protecting(false);
    return e;
}
//------------------------------------------------------------------------------

inline short Arity(const unsigned long arity, long sz) {
    static short check[2] = {0x1000, 0};
    unsigned long a = 1 << ((sz < 16)?sz:15);
    return check[((arity&a) == a)];
}

void Listincode::set_current_line(LispE* lisp) {
    lisp->set_current_line(line, fileidx);
}

//--------------------------------------------------------------------------------
// The main evaluation function, the one that evaluates instructions or functions
//--------------------------------------------------------------------------------

Element* List::eval(LispE* lisp) {
    Element* first_element = null_;
    Element* element = null_;
    Element* a = null_;
    long listsize;
    short label;
    bool test = true;

    
    try {
        first_element = liste.at(0);
        set_current_line(lisp);
        lisp->display_trace(this);
        listsize = liste.size();
        label = lisp->checkLispState() | first_element->type;
        label |=  Arity(lisp->delegation->arities.at(label), listsize) ;
    }
    catch(const std::out_of_range& oor) {
        if (lisp->hasStopped())
            throw lisp->delegation->_THEEND;
        
        if (liste.size() == 0)
            return this;
        
        if ((label & 0x200) == 0x200)
            throw new Error("Error: stack overflow");
        
        wstring msg = L"Error: unknown instruction: '";
        msg += lisp->asString(first_element->type);
        msg += L"'";
        throw new Error(msg);
        
    }
    
#ifdef DEBUG
    //(map 'eval (map 'atom (filter (\(x) (in x "socket")) (map 'string '(extract socket_read socket_create)))))
    //(map 'eval (map 'atom (map 'string '(extract socket_read socket_create))))
    //(map '* (take 4 (filter '(< 10) '(1 3 10 23 45 67 1 2 3 5 9))))
    //(take 4 (filter '(< 10) (map '* '(1 3 10 23 45 67 1 2 3 5 9))))
    //(take 4 (filter '(< 10) (map '+ (map '* '(1 3 10 23 45 67 1 2 3 5 9)))))
    //(map '(- 5) (take 4 (filter '(< 10) (map '* '(1 3 10 23 45 67 1 2 3 5 9]
    cout << toString(lisp) << endl;
    std::vector<Element*> atomes;
    lisp->extractAllAtoms(this, atomes);
    unordered_map<short, bool> uniques;
    for (auto& a: atomes) {
        if (uniques.find(a->label()) != uniques.end())
            continue;
        uniques[a->label()] = true;
        Element* value = lisp->getvalue(a->label());
        cout << a->toString(lisp) << ":" << value->toString(lisp) << endl;
    }
#endif
    
    //The first element tells us what to do...
    //It must be an operator or a function
    
    try {
        switch (label) {
            case l_quote:
                return liste[1];
            case l_return: {
                first_element = new Return(null_);
                if (listsize == 2) {
                    element = liste[1]->eval(lisp);
                    ((Return*)first_element)->value = element;
                }
                return first_element;
            }
            case l_break:
                return break_;
            case l_while: {
                first_element = liste[1]->eval(lisp);
                element = null_;
                test = lisp->trace;
                while (first_element->Boolean()) {
                    first_element->release();
                    for (long i = 2; i < listsize && element->type != l_return; i++) {
                        _releasing(element);
                        element = liste[i]->eval(lisp);
                    }
                    
                    //In case a 'return' has been placed in the code
                    if (element->type == l_return) {
                        lisp->stop_at_next_line(test);
                        if (element->isBreak())
                            return null_;
                        //this is a return, it goes back to the function call
                        return element;
                    }
                    first_element = liste[1]->eval(lisp);
                }
                first_element->release();
                lisp->stop_at_next_line(test);
                return element;
            }
            case l_set_max_stack_size: {
                long m;
                if (listsize == 1)
                    return lisp->provideInteger(lisp->stack_size_max());
                evalAsInteger(1, lisp, m);
                lisp->set_stack_max_size(m);
                return true_;
            }
            case l_plus: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->plus(lisp, element);
                }

                element->release();
                return first_element;
            }
            case l_minus: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->minus(lisp, element);
                }

                element->release();
                return first_element;
            }
            case l_multiply: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->multiply(lisp, element);
                }

                element->release();
                return first_element;
            }
            case l_power: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->power(lisp, element);
                }

                element->release();
                return first_element;
            }
            case l_leftshift: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->leftshift(lisp, element);
                }

                element->release();
                return first_element;
            }
            case l_rightshift: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->rightshift(lisp, element);
                }

                element->release();
                return first_element;
            }
            case l_bitand: {
                   first_element = liste[1]->eval(lisp);
                   for (short i = 2; i < listsize; i++) {
                       _releasing(element);
                       element = liste[i]->eval(lisp);
                       first_element = first_element->bit_and(lisp, element);
                   }

                   element->release();
                   return first_element;
               }
            case l_bitor: {
                   first_element = liste[1]->eval(lisp);
                   for (short i = 2; i < listsize; i++) {
                       _releasing(element);
                       element = liste[i]->eval(lisp);
                       first_element = first_element->bit_or(lisp, element);
                   }

                   element->release();
                   return first_element;
               }
            case l_bitxor: {
                   first_element = liste[1]->eval(lisp);
                   for (short i = 2; i < listsize; i++) {
                       _releasing(element);
                       element = liste[i]->eval(lisp);
                       first_element = first_element->bit_xor(lisp, element);
                   }

                   element->release();
                   return first_element;
               }
            case l_plusequal:{
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->plus(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_minusequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->minus(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_multiplyequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->multiply(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;

            }
            case l_powerequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->power(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_leftshiftequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->leftshift(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_rightshiftequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->rightshift(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_bitandequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_and(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_bitorequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_or(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_bitxorequal: {
                first_element = liste[1]->eval(lisp);
                for (short i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_xor(lisp, element);
                }

                element->release();
                label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_divide:
                return divide(lisp,false);
            case l_mod:
                return mod(lisp,false);
            case l_divideequal:
                return divide(lisp,true);
            case l_modequal:
                return mod(lisp,true);
            case l_eq: {
                for (long i = 1; i < listsize-1 && test; i++) {
                    first_element = liste[i]->eval(lisp);
                    element = liste[i+1]->eval(lisp);
                    test = ( (first_element == element) || first_element->equal(lisp, element)->Boolean());
                    _releasing(first_element);
                    _releasing(element);
                }
                return booleans_[test];
            }
            case l_neq: {
                test = false;
                for (long i = 1; i < listsize-1 && !test; i++) {
                    first_element = liste[i]->eval(lisp);
                    element = liste[i+1]->eval(lisp);
                    test = ( (first_element == element) || first_element->equal(lisp, element)->Boolean());
                    _releasing(first_element);
                    _releasing(element);
                }
                return booleans_[!test];
            }
            case l_throw: {
                wstring msg;
                throw new Error(msg);
            }
            case l_catch: {
                try {
                    first_element = liste[1]->eval(lisp);
                }
                catch (Error* err) {
                    //This error is converted into a non-blocking error message .
                    element = new Maybe(lisp, err);
                    err->release();
                    return element;
                }
                return first_element;
            }
            case l_maybe: {
                if (listsize==2) {
                    first_element = liste[1]->eval(lisp);
                    return booleans_[(first_element->label() == t_maybe)];
                }
                
                //Otherwise, we test each value for error and then we send the last one back
                first_element = null_;
                try {
                    for (long i = 1; i < listsize-1; i++) {
                        first_element->release();
                        first_element = liste[i]->eval(lisp);
                    }
                }
                catch(Error* err) {
                    err->release();
                    return liste.back()->eval(lisp);
                }
                return first_element;
            }
            case l_equal: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                test = first_element->unify(lisp, element, false);
                first_element->release();
                element->release();
                return booleans_[test];
            }
            case l_different: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                test = first_element->unify(lisp, element, false);
                first_element->release();
                element->release();
                return booleans_[!test];
            }
            case l_lower: {
                for (long i = 1; i < listsize-1 && test; i++) {
                    first_element = liste[i]->eval(lisp);
                    element = liste[i+1]->eval(lisp);
                    test = first_element->less(lisp, element)->Boolean();
                    _releasing(first_element);
                    _releasing(element);
                }
                return booleans_[test];
            }
            case l_greater: {
                for (long i = 1; i < listsize-1 && test; i++) {
                    first_element = liste[i]->eval(lisp);
                    element = liste[i+1]->eval(lisp);
                    test = first_element->more(lisp, element)->Boolean();
                    _releasing(first_element);
                    _releasing(element);
                }
                return booleans_[test];
            }
            case l_lowerorequal: {
                for (long i = 1; i < listsize-1 && test; i++) {
                    first_element = liste[i]->eval(lisp);
                    element = liste[i+1]->eval(lisp);
                    test = first_element->lessorequal(lisp, element)->Boolean();
                    _releasing(first_element);
                    _releasing(element);
                }
                return booleans_[test];
            }
            case l_greaterorequal: {
                for (long i = 1; i < listsize-1 && test; i++) {
                    first_element = liste[i]->eval(lisp);
                    element = liste[i+1]->eval(lisp);
                    test = first_element->moreorequal(lisp, element)->Boolean();
                    _releasing(first_element);
                    _releasing(element);
                }
                return booleans_[test];
            }
            case l_max: {
                if (listsize == 2) {
                    //In this case, the argument must be a list
                    first_element = liste[1]->eval(lisp);
                    if (!first_element->isList())
                        throw new Error("Error: the first argument must be a list");
                    if (first_element->size() == 0)
                        return null_;
                    a = first_element->index(0);
                    if (first_element->size() == 1)
                        return a->copying(false);
                    for (long i = 2; i < first_element->size(); i++) {
                        element = first_element->index(i);
                        if (a->less(lisp, element)->Boolean())
                            a = element;
                    }
                    return a->copying(false);
                }
                
                first_element = liste[1]->eval(lisp);
                for (long i = 2; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    if (first_element->less(lisp, element)->Boolean()) {
                        first_element->release();
                        first_element=element;
                    }
                    else {
                        _releasing(element);
                    }
                }
                return first_element;
            }
            case l_min: {
                if (listsize == 2) {
                    //In this case, the argument must be a list
                    first_element = liste[1]->eval(lisp);
                    if (!first_element->isList())
                        throw new Error("Error: the first argument must be a list");
                    if (first_element->size() == 0)
                        return null_;
                    a = first_element->index(0);
                    if (first_element->size() == 1)
                        return a->copying(false);
                    for (long i = 2; i < first_element->size(); i++) {
                        element = first_element->index(i);
                        if (a->more(lisp, element)->Boolean())
                            a = element;
                    }
                    return a->copying(false);
                }
                
                first_element = liste[1]->eval(lisp);
                for (long i = 2; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    if (first_element->more(lisp, element)->Boolean()) {
                        first_element->release();
                        first_element=element;
                    }
                    else {
                        _releasing(element);
                    }
                }
                return first_element;
            }
            case l_atomp: {
                element = liste[1]->eval(lisp);
                test = element->isAtom();
                element->release();
                return booleans_[test];
            }
            case l_numberp: {
                element = liste[1]->eval(lisp);
                test = element->isNumber();
                element->release();
                return booleans_[test];
            }
            case l_stringp: {
                element = liste[1]->eval(lisp);
                test = element->isString();
                element->release();
                return booleans_[test];
            }
            case l_consp: {
                element = liste[1]->eval(lisp);
                test = element->isList();
                element->release();
                return booleans_[test];
            }
            case l_zerop: {
                element = liste[1]->eval(lisp);
                double n = element->asNumber();
                element->release();
                return booleans_[!n];
            }
            case l_nullp: {
                element = liste[1]->eval(lisp);
                if (element == null_)
                    return true_;
                element->release();
                return false_;
            }
            case l_data: {
                //We record a data structure of the form: (data (Atom x y z) (Atom x y))
                short lab;
                long i = 1;
                short ancestor = v_null;
                if (liste[1]->isAtom()) {
                    ancestor = liste[1]->label();
                    i = 2;
                }
                for (; i < listsize; i++) {
                    element = liste[i];
                    if (!element->isList() || !element->size())
                        throw new Error(L"Error: A data structure can only contain non empty lists");
                    lab = element->index(0)->label();
                    if (lab == v_null)
                        throw new Error(L"Error: Missing definition name in data structure");
                    lisp->recordingData(element, lab, ancestor);
                }
                return true_;
            }
            case l_flip: {
                if (liste[1]->isList() && liste[1]->size() >= 3) {
                    List* l = (List*)liste[1];
                    //We reverse the two first arguments
                    List call;
                    call.liste.push_back(l->liste[0]);
                    call.liste.push_back(l->liste[2]);
                    call.liste.push_back(l->liste[1]);
                    for (long i = 3; i < liste[1]->size(); i++)
                        call.liste.push_back(l->liste[i]);
                    return call.eval(lisp);
                }
                first_element = liste[1]->eval(lisp);
                if (first_element->isDictionary()) {
                    element = first_element->reverse(lisp, true);
                    first_element->release();
                    return element;
                }
                first_element->release();
                throw new Error("Error: Cannot apply flip on this structure");
            }
            case l_select: { //we return the first non null value
                element = null_;
                for (long i = 1; i < listsize && element == null_; i++)
                element = liste[i]->eval(lisp);
                return element;
            }
            case l_compose: {
                
                /*
                 loop = liste[5]...
                 */
                long i = 4;
                if (liste[4]->isList()) {
                    label = liste[4]->index(1)->label();
                }
                else {
                    label = liste[4]->label();
                    i = 5;
                }
                for (; i < listsize-1; i++)
                    liste[i]->eval(lisp);
                element = liste.back()->eval(lisp);
                return lisp->get(label);
            }
            case l_loop: {
                //We loop in a list
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing label for 'loop'");
                test = lisp->trace;
                element = liste[2]->eval(lisp);
                first_element = element->loop(lisp, label, this);
                element->release();
                lisp->stop_at_next_line(test);
                return first_element;
            }
            case l_loopcount: {
                long counter;
                evalAsInteger(1, lisp, counter);
                element = null_;
                while (counter > 0) {
                    for (long i = 2; i < listsize && element->type != l_return; i++) {
                        _releasing(element);
                        element = liste[i]->eval(lisp);
                    }
                    //In case a 'return' or a 'break' has been placed in the code
                    if (element->type == l_return) {
                        if (element->isBreak())
                            return null_;
                        //this is a return, it goes back to the function call
                        return element;
                    }
                    counter--;
                }
                return element;
            }
            case l_or:
            {
                element = null_;
                for (long i = 1; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    if (element->Boolean()) {
                        return true_;
                    }
                }
                element->release();
                return false_;
            }
            case l_and:
            {
                element = null_;
                for (long i = 1; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    if (!element->Boolean()) {
                        return false_;
                    }
                }
                element->release();
                return true_;
            }
            case l_xor: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                
                if (first_element->Boolean() == element->Boolean())
                    a = false_;
                else
                    a = true_;
                
                first_element->release();
                element->release();
                return a;
            }
            case l_ncheck: {
                //A ncheck is like a check, but when the test fails
                //it executes the second element, otherwise
                //the actual list of instructions starts at 3...
                first_element = liste[1]->eval(lisp);
                test = first_element->Boolean();
                first_element->release();
                if (!test)
                    return liste[2]->eval(lisp);
                
                element = null_;
                for (long i = 3; i < listsize && element->type != l_return; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                }
                return element;
            }
            case l_check: {
                first_element = liste[1]->eval(lisp);
                test = first_element->Boolean();
                first_element->release();
                if (!test)
                    return null_;
                
                element = null_;
                for (long i = 2; i < listsize && element->type != l_return; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                }
                return element;
            }
            case l_ife: {
                first_element = liste[1]->eval(lisp);
                test = first_element->Boolean();
                first_element->release();
                if (test)
                    return liste[2]->eval(lisp);
                
                element = null_;
                for (long i = 3; i < listsize && element->type != l_return; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                }
                return element;
            }
            case l_block:
                if (listsize == 2)
                    return liste[1]->eval(lisp);
                element = null_;
                for (long i = 1; i < listsize && element->type != l_return; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                }
                return element;
            case l_converttoatom: {
                wstring a;
                evalAsString(1,lisp,a);
                return lisp->provideAtomProtected(a);
            }
            case l_converttointeger:
                element = liste[1]->eval(lisp);
                first_element = lisp->provideInteger(element->asInteger());
                element->release();
                return first_element;
            case l_converttonumber:
                element = liste[1]->eval(lisp);
                first_element = lisp->provideNumber(element->asNumber());
                element->release();
                return first_element;
            case l_converttostring: {
                element = liste[1]->eval(lisp);
                wstring strvalue = element->asString(lisp);
                first_element = lisp->provideString(strvalue);
                element->release();
                return first_element;
            }
            case l_list:
                first_element = new List();
                if (listsize == 1)
                    return first_element;
                element = emptylist_;
                for (long i = 1; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    first_element->append(element->copying());
                }
                return first_element;
            case l_reverse:
                element = liste[1]->eval(lisp);
                first_element = element->reverse(lisp, true);
                element->release();
                return first_element;
            case l_cons: {
                //merging an element into the next list
                first_element = liste[1]->eval(lisp);
                
                element = liste[2]->eval(lisp);
                if (element == null_) {
                    a = new List();
                    a->append(first_element);
                    return a;
                }
                
                if (!element->isList()) {
                    a = new Pair();
                    a->append(first_element);
                    a->append(element);
                    return a;
                }
                                
                if (element->type == t_pair)
                    a = new Pair();
                else
                    a = new List();
                a->append(first_element);
                for (long i = 0; i < element->size(); i++)
                    a->append(element->value_on_index(lisp, i));
                element->release();
                return a;
                
            }
            case l_trace:
                if (listsize == 1) {
                    if (lisp->trace)
                        return true_;
                    return false_;
                }
                first_element = liste[1]->eval(lisp);
                lisp->trace  = first_element->Boolean();
                first_element->release();
                if (lisp->trace)
                    return true_;
                return false_;
            case l_key: {
                if (listsize == 1) {
                    //We create an empty dictionary
                    return new Dictionary;
                }
                if (listsize == 3) {
                    //The second element is an a_key
                    first_element = liste[1]->eval(lisp);
                    if (first_element->type == t_dictionary) {
                        wstring a_key;
                        evalAsString(2, lisp, a_key);
                        element = first_element->value_on_index(a_key, lisp);
                        first_element->release();
                        return element;
                    }
                    if (first_element->type == t_dictionaryn) {
                        double a_key;
                        evalAsNumber(2, lisp, a_key);
                        element = first_element->value_on_index(a_key, lisp);
                        first_element->release();
                        return element;
                    }
                    throw new Error(L"Error: the first argument must be a dictionary");
                }
                
                if (listsize == 4) {
                    //We store a value
                    first_element = liste[1]->eval(lisp);
                    if (first_element->type == t_dictionary) {
                        wstring a_key;
                        evalAsString(2, lisp, a_key);
                        element = liste[3]->eval(lisp);
                        // It is out of question to manipulate a dictionary declared in the code
                        first_element = first_element->duplicate_constant_container();
                        first_element->recording(a_key, element->copying(false));
                        return first_element;
                    }
                    if (first_element->type == t_dictionaryn) {
                        double a_key;
                        evalAsNumber(2, lisp, a_key);
                        element = liste[3]->eval(lisp);
                        // It is out of question to manipulate a dictionary declared in the code
                        first_element = first_element->duplicate_constant_container();
                        first_element->recording(a_key, element->copying(false));
                        return first_element;
                    }
                    throw new Error(L"Error: the first argument must be a dictionary");
                }
                throw new Error(L"Error: Incorrect number of arguments for 'key'");
            }
            case l_keyn: {
                if (listsize == 1) {
                    //We create an empty dictionary
                    return new Dictionary_n;
                }
                if (listsize == 3) {
                    //The second element is an a_key
                    first_element = liste[1]->eval(lisp);
                    if (first_element->type != t_dictionaryn) {
                        first_element->release();
                        throw new Error(L"Error: the first argument must be a dictionary indexed on numbers");
                    }
                    double a_key;
                    evalAsNumber(2, lisp, a_key);
                    element = first_element->value_on_index(a_key, lisp);
                    first_element->release();
                    return element;
                }
                
                if (listsize == 4) {
                    //We store a value
                    first_element = liste[1]->eval(lisp);
                    if (first_element->type != t_dictionaryn) {
                        first_element->release();
                        throw new Error(L"Error: the first argument must be a dictionary indexed on numbers");
                    }
                    double a_key;
                    evalAsNumber(2, lisp, a_key);
                    element = liste[3]->eval(lisp);
                    // It is out of question to manipulate a dictionary declared in the code
                    first_element = first_element->duplicate_constant_container();
                    first_element->recording(a_key, element->copying(false));
                    return first_element;
                }
                throw new Error(L"Error: Incorrect number of arguments for 'keyn'");
            }
            case l_last: {
                first_element = liste[1]->eval(lisp);
                return first_element->last_element(lisp);
            }
            case l_push: {
                //We store a value in a list
                first_element = liste[1]->eval(lisp);
                if (!first_element->isList()) {
                    first_element->release();
                    throw new Error(L"Error: missing list in 'push'");
                }
                first_element = first_element->duplicate_constant_container();
                element = liste[2]->eval(lisp);
                first_element->append(element->copying(false));
                return first_element;
            }
            case l_insert: {
                //We insert a value in a list
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                long idx;
                evalAsInteger(3, lisp, idx);
                a = first_element->insert(lisp, element, idx);
                
                element->release();
                if (a != first_element) {
                    first_element->release();
                    return a;
                }
                return first_element;
            }
            case l_pop: {
                first_element = liste[1]->eval(lisp);
                if (first_element->type == t_dictionary) {
                    element = liste[2]->eval(lisp);
                    wstring keyvalue = element->asString(lisp);
                    element->release();
                    if (first_element->remove(keyvalue))
                        return first_element;
                    first_element->release();
                    return null_;
                }
                if (first_element->type == t_dictionaryn) {
                    element = liste[2]->eval(lisp);
                    double keyvalue = element->asNumber();
                    element->release();
                    if (first_element->remove(keyvalue))
                        return first_element;
                    first_element->release();
                    return null_;
                }
                
                if (first_element->isList()) {
                    long keyvalue;
                    if (listsize == 3) {
                        element = liste[2]->eval(lisp);
                        keyvalue = element->asInteger();
                        element->release();
                    }
                    else
                        keyvalue = first_element->size();
                    if (first_element->remove(keyvalue))
                        return first_element;
                    first_element->release();
                    return emptylist_;
                }
                
                //In the case of a string, we return a copy, we do not modify the string.
                //itself
                if (first_element->type == t_string) {
                    long keyvalue;
                    wstring strvalue = first_element->asString(lisp);
                    if (listsize == 3) {
                        element = liste[2]->eval(lisp);
                        keyvalue = element->asInteger();
                        element->release();
                    }
                    else {
                        if (!strvalue.size())
                            return emptystring_;
                        strvalue.pop_back();
                        return lisp->provideString(strvalue);
                    }
                    
                    if (keyvalue < 0 || keyvalue >= strvalue.size())
                        return emptystring_;
                    strvalue.erase(strvalue.begin()+keyvalue);
                    first_element->release();
                    return lisp->provideString(strvalue);
                }
                first_element->release();
                throw new Error(L"Error: the first argument must be a dictionary");
            }
            case l_keys:
                first_element = liste[1]->eval(lisp);
                if (first_element->type != t_dictionary && first_element->type != t_dictionaryn) {
                    first_element->release();
                    throw new Error(L"Error: the first argument must be a dictionary");
                }
                element = first_element->thekeys(lisp);
                first_element->release();
                return element;
            case l_values:
                first_element = liste[1]->eval(lisp);
                if (first_element->type != t_dictionary && first_element->type != t_dictionaryn) {
                    first_element->release();
                    throw new Error(L"Error: the first argument must be a dictionary");
                }
                element = first_element->thevalues(lisp);
                first_element->release();
                return element;
            case l_cond: {
                long szv;
                for (long i = 1; i < listsize; i++) {
                    element = liste[i];
                    szv = element->size();
                    
                    if (!element->isList() || szv <= 1)
                        throw new Error(L"Error: in a 'cond' the first element must be a list");
                    
                    List* code = (List*)element;
                    a = code->liste[0]->eval(lisp);
                    if (a->Boolean()) {
                        _releasing(a);
                        first_element = null_;
                        code->liste.back()->setterminal(terminal);
                        for (long int j = 1; j < szv; j++) {
                            _releasing(first_element);
                            first_element = code->liste[j]->eval(lisp);
                        }
                        return first_element;
                    }
                    _releasing(a);
                }
                return null_;
            }
            case l_not: {
                first_element = liste[1]->eval(lisp);
                test = first_element->Boolean();
                first_element->release();
                return booleans_[!test];
            }
            case l_if: {
                first_element = liste[1]->eval(lisp);
                test = first_element->Boolean();
                first_element->release();
                
                if (test) {
                    liste[2]->setterminal(terminal);
                    return liste[2]->eval(lisp);
                }
                if (listsize == 4) {
                    liste[3]->setterminal(terminal);
                    return liste[3]->eval(lisp);
                }
                return null_;
            }
            case l_car:
                element = liste[1]->eval(lisp);
                first_element = element->car(lisp);
                element->release();
                return first_element;
            case l_cdr:
                element = liste[1]->eval(lisp);
                first_element = element->cdr(lisp);
                element->release();
                return first_element;
            case l_cadr:
                element = liste[1]->eval(lisp);
                first_element = first_element->cadr(lisp, element);
                element->release();
                return first_element;
            case l_label: {
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing label for 'label'");
                element = liste[2]->eval(lisp);
                //the difference with 'setq' is that only one version is recorded.
                //of the variable... Without duplication
                return lisp->recordingunique(element, label);
            }
            case l_setq: {
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing label for 'setq'");
                element = liste[2]->eval(lisp);
                lisp->recording(element, label);
                return true_;
            }
            case l_setg: {
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing label for 'setg'");
                element = liste[2]->eval(lisp);
                lisp->recordingglobal(element, label);
                return true_;
            }
            case l_self:
                //We recall our top function
                return evalfunction(lisp->called(), lisp);
            case l_deflib: { // we manage extensions to the language with deflib (see systeme.cxx for an example)
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing name in the declaration of a function");
                if (!liste[2]->isList())
                    throw new Error(L"Error: List of missing parameters in a function declaration");
                return lisp->recordingunique(this, label);
            }
            case l_defmacro: {
                //We declare a function
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing name in the declaration of a function");
                if (!liste[2]->isList())
                    throw new Error(L"Error: List of missing parameters in a function declaration");
                return lisp->recordingMacro(this, label);
            }
            case l_sleep: {
                long tm;
                evalAsInteger(1, lisp, tm);
                std::this_thread::sleep_for(std::chrono::milliseconds(tm));
                return true_;
            }
            case l_wait: {
                //We wait for all threads to be finished
                while (lisp->nbjoined) {}
                return true_;
            }
            case l_dethread:
            case l_defun: {
                //We declare a function
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing name in the declaration of a function");
                if (!liste[2]->isList())
                    throw new Error(L"Error: List of missing parameters in a function declaration");
                return lisp->recordingunique(this, label);
            }
            case l_defpat: {
                //We declare a function
                label = liste[1]->label();
                if (label == v_null)
                    throw new Error(L"Error: Missing name in the declaration of a function");
                if (!liste[2]->isList())
                    throw new Error(L"Error: List of missing parameters in a function declaration");
                return lisp->recordingMethod(this, label);
            }
            case l_lambda:
                if (!liste[1]->isList())
                    throw new Error(L"Error: Missing parameter list in a lambda declaration");
                return this;
            case t_atom: {
                //In this case, it must be a function
                first_element = liste[0]->eval(lisp);
                if (first_element->index(0)->label() == l_defpat)
                    return evalpattern(lisp, first_element->index(1)->label());
                return evalfunction(first_element, lisp);
            case t_list:
                first_element = first_element->eval(lisp);
                //Execution of a lambda a priori, otherwise it's an error
                element = evalfunction(first_element,lisp);
                first_element->release();
                return element;
            }
            case l_lock: {
                element = null_;
                wstring key;
                evalAsString(1, lisp, key);
                ThreadLock* _lock = lisp->delegation->getlock(key);
                test = lisp->checkforLock();
                _lock->locking(test);
                for (long i = 2; i < listsize && element->type != l_return; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                }
                _lock->unlocking(test);
                return element;
            }
            case l_waiton: {
                wstring key;
                evalAsString(1, lisp, key);
                lisp->delegation->waiton(key);
                return true_;
            }
            case l_trigger: {
                wstring key;
                evalAsString(1, lisp, key);
                return booleans_[lisp->delegation->trigger(key)];
            }
            case l_threadstore: {
                wstring key;
                evalAsString(1, lisp, key);
                first_element = liste[2]->eval(lisp);
                lisp->delegation->thread_store(key, first_element);
                first_element->release();
                return true_;
            }
            case l_threadclear: {
                if (listsize == 1) {
                    lisp->delegation->thread_clear_all();
                    return true_;
                }

                wstring key;
                evalAsString(1, lisp, key);
                return booleans_[lisp->delegation->thread_clear(key)];
            }
            case l_threadretrieve: {
                if (listsize == 1) {
                    //We return all as a dictionary
                    return lisp->delegation->thread_retrieve_all();
                }
                wstring key;
                evalAsString(1, lisp, key);
                first_element = lisp->delegation->thread_retrieve(key);
                if (first_element == NULL)
                    return null_;
                return first_element;
            }
            case l_print: {
                for (long i = 1; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    std::cout << element->toString(lisp);
                    _releasing(element);
                }
                if (lisp->isThread)
                    std::cout.flush();
                return emptyatom_;
            }
            case l_println: {
                for (long i = 1; i < listsize; i++) {
                    if (i != 1)
                        std::cout << " ";
                    element = liste[i]->eval(lisp);
                    std::cout << element->toString(lisp);
                    _releasing(element);
                }
                std::cout << std::endl;
                if (lisp->isThread)
                    std::cout.flush();
                return emptyatom_;
            }
            case l_printerr: {
                for (long i = 1; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    std::cerr << element->toString(lisp);
                    _releasing(element);
                }
                if (lisp->isThread)
                    std::cerr.flush();
                return emptyatom_;
            }
            case l_printerrln: {
                for (long i = 1; i < listsize; i++) {
                    if (i != 1)
                        std::cerr << " ";
                    element = liste[i]->eval(lisp);
                    std::cerr << element->toString(lisp);
                    _releasing(element);
                }
                std::cerr << std::endl;
                if (lisp->isThread)
                    std::cerr.flush();
                return emptyatom_;
            }
            case l_prettify: {
                first_element = liste[1]->eval(lisp);
                string s = first_element->prettify(lisp);
                first_element->release();
                return new String(s);
            }
            case l_atoms: {
                return lisp->atomes();
            }
            case l_atomise:
                element = liste[1]->eval(lisp);
                first_element = lisp->atomise(element->asString(lisp));
                element->release();
                return first_element;
            case l_join: {
                first_element = liste[1]->eval(lisp);
                wstring sep;
                if (listsize == 3)
                    evalAsString(2,lisp,sep);
                element = first_element->join_in_list(lisp, sep);
                first_element->release();
                return element;
            }
            case l_eval: {
                element = liste[1]->eval(lisp);
                
                //This is a specific case, when the element is a string
                //we need then to call the a meta-eval, the one that
                //comes with Lisp itself
                if (element->isString()) {
                    first_element = lisp->eval(element->toString(lisp));
                    element->release();
                    if (first_element->isError())
                        throw first_element;
                    return first_element;
                }
                //We just need to evaluate this element...
                first_element = element->eval(lisp);
                if (first_element != element)
                    element->release();
                return first_element;
            }
            case l_type: {
                element = liste[1]->eval(lisp);
                first_element = lisp->provideAtom(element->type);
                element->release();
                return first_element;
            }
            case l_load:
                element = liste[1]->eval(lisp);
                
                first_element = lisp->load(element->toString(lisp));
                element->release();
                return first_element;
            case l_input: {
                string code;
                getline(std::cin, code);
                return lisp->provideString(code);
            }
            case l_pipe:
                if (std::cin.eof())
                    return false_;
                return true_;
            case l_fread: {
                first_element = liste[1]->eval(lisp);
                
                element = new String("");
                string nom = first_element->toString(lisp);
                first_element->release();
                first_element = element->charge(lisp, nom);
                return element;
            }
            case l_fappend: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                string chemin = first_element->toString(lisp);
                //We put ourselves in append mode
                std::ofstream o(chemin.c_str(), std::ios::binary|std::ios::app);
                if (o.fail()) {
                    element->release();
                    first_element->release();
                    string erreur = "Error: Cannot write in file: ";
                    erreur += chemin;
                    throw new Error(erreur);
                }
                chemin = element->toString(lisp);
                o << chemin;
                first_element->release();
                element->release();
                return true_;
            }
            case l_fwrite: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                string chemin = first_element->toString(lisp);
                std::ofstream o(chemin.c_str(), std::ios::binary);
                if (o.fail()) {
                    element->release();
                    first_element->release();
                    string erreur = "Error: Cannot write in file: ";
                    erreur += chemin;
                    throw new Error(erreur);
                }
                chemin = element->toString(lisp);
                o << chemin;
                first_element->release();
                element->release();
                return true_;
            }
            case l_size: {
                element = liste[1]->eval(lisp);
                first_element = lisp->provideNumber(element->size());
                element->release();
                return first_element;
                
            }
            case l_use: {
                element = liste[1]->eval(lisp);
                string nom_bib = element->toString(lisp);
                element->release();
                return lisp->load_library(nom_bib);
            }
            case l_index: {
                first_element = liste[1]->eval(lisp);
                if (listsize == 4) {
                    long idx;
                    evalAsInteger(2, lisp, idx);
                    element = liste[3]->eval(lisp);
                    a = first_element->replace(lisp, idx, element);
                    element->release();
                    return a;
                }
                element = liste[2]->eval(lisp);
                a = first_element->value_on_index(lisp, element);
                element->release();
                first_element->release();
                return a;
            }
            case l_extract: {
                element = liste[1]->eval(lisp);
                first_element = element->extraction(lisp, this);
                element->release();
                return first_element;
            }
            case l_in: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                long idx = 0;
                if (listsize == 4)
                    evalAsInteger(3,lisp, idx);
                a = first_element->search_element(lisp, element, idx);
                first_element->release();
                element->release();
                if (a != null_) {
                    a->release();
                    return true_;
                }
                return a;
            }
            case l_search: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                long idx = 0;
                if (listsize == 4)
                    evalAsInteger(3,lisp, idx);
                a = first_element->search_element(lisp, element, idx);
                first_element->release();
                element->release();
                return a;
            }
            case l_searchall: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                long idx = 0;
                if (listsize == 4) {
                    evalAsInteger(3,lisp, idx);
                }
                a = first_element->search_all_elements(lisp, element, idx);
                first_element->release();
                element->release();
                return a;
            }
            case l_revertsearch: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                long idx = 0;
                if (listsize == 4) {
                    evalAsInteger(3,lisp, idx);
                }
                a = first_element->search_reverse(lisp, element, idx);
                first_element->release();
                element->release();
                return a;
            }
            case l_irange: {
                double init, inc;
                evalAsNumber(1, lisp, init);
                evalAsNumber(2, lisp, inc);
                if (init == (long)init && inc == (long)inc)
                    return new InfiniterangeInteger(init, inc);
                return new Infiniterange(init, inc);
            }
            case l_range: {
                double init, limit, inc;
                evalAsNumber(1, lisp, init);
                evalAsNumber(2, lisp, limit);
                evalAsNumber(3, lisp, inc);
                return range(lisp, init, limit, inc);
            }
            case l_mapping: { //abus de langage: I agree
                //We detect the type of the instruction on the fly
                first_element = liste[1]->eval(lisp);
                //This is a variable that was evaluated on the fly
                List call;
                call.liste.push_back(first_element);
                call.liste.push_back(liste[2]);
                if (lisp->is_math_operator(first_element->type)) {
                    //Then we must duplicate the second element:
                    call.liste.push_back(liste[2]);
                }
                element = call.eval(lisp);
                first_element->release();
                return element;
            }
            case l_checking: {
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                List call;
                if (first_element->isInstruction()) {
                    call.liste.push_back(first_element);
                    call.liste.push_back(liste[3]);
                    call.liste.push_back(element);
                }
                else {
                    if (element->isInstruction()) {
                        call.liste.push_back(element);
                        call.liste.push_back(first_element);
                        call.liste.push_back(liste[3]);
                    }
                    else {
                        first_element->release();
                        element->release();
                        throw new Error("Error: condition or operation cannot be evaluated: missing instruction");
                    }
                }
                a = call.eval(lisp);
                first_element->release();
                element->release();
                return a;
            }
            case l_folding: { //abus de langage: I agree
                //We detect the type of the instruction on the fly
                first_element = liste[1]->eval(lisp);
                if (liste[1]->isList()) {
                    //This is a quoted expression, we can safely replace it
                    liste[0] = first_element;
                    liste[1] = liste[2];
                    liste[2] = liste[3];
                    liste.pop_back();
                    return eval(lisp);
                }
                //This is a variable that was evaluated on the fly
                List call;
                call.liste.push_back(first_element);
                call.liste.push_back(liste[2]);
                call.liste.push_back(liste[3]);
                element = call.eval(lisp);
                first_element->release();
                return element;
            }
            case l_apply:  { //(apply func l)
                element = liste[2]->eval(lisp);
                if (!element->isList()) {
                    element->release();
                    throw new Error("Error: arguments to 'apply' should be given as a list");
                }
                
                first_element = liste[1]->eval(lisp);
                
                if (element->status == s_constant) {
                    List l;
                    l.liste = ((List*)element)->liste;
                    l.liste.insert(l.liste.begin(), first_element);
                    a = l.eval(lisp);
                }
                else {
                    //We insert this element in our list
                    List* l = (List*)element;
                    l->liste.insert(l->liste.begin(), first_element);
                    
                    a = l->eval(lisp);
                    
                    //We remove this first element
                    if (l->status)
                        l->liste.erase(l->liste.begin());
                    else
                        l->release();
                }
                first_element->release();
                return a;
            }
            case l_sort: {
                //First element is the comparison function OR an operator
                first_element = liste[1]->eval(lisp);
                element = liste[2]->eval(lisp);
                if (!element->isList()) {
                    first_element->release();
                    element->release();
                    throw new Error(L"Error: the second argument should be a list for 'sort'");
                }
                
                if (first_element->isList()) {
                    //It is inevitably a lambda
                    if (first_element->size() == 1)
                        first_element = first_element->index(0);
                }
                else {
                    //C Is either an atom or an operator
                    if (!first_element->isAtom()) {
                        first_element->release();
                        element->release();
                        throw new Error(L"Error: incorrect comparison function in 'sort'");
                    }
                }
                
                Comparison comp(lisp, first_element);
                List* l = (List*)element;
                if (comp.checkComparison(l->liste[0])) {
                    first_element->release();
                    element->release();
                    throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
                }
                sort(l->liste.begin(), l->liste.end(), comp);
                first_element->release();
                return element;
            }
            case l_zip: {
                //We combine different lists together...
                vector<Element*> lists;
                long szl = -1;
                long i;
                for (i = 1; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    if (!element->isList()) {
                        element->release();
                        a = new Error(L"Error: 'zip' only accepts lists as arguments");
                        break;
                    }
                    if (i == 1)
                        szl = element->size();
                    else {
                        if (szl != element->size()) {
                            element->release();
                            a = new Error(L"Error: Lists should all have the same size in 'zip'");
                            break;
                        }
                    }
                    lists.push_back(element);
                    element = null_;
                }
                
                if (a->isError()) {
                    for (auto& x: lists)
                        x->release();
                    throw a;
                }
                
                a = new List;
                List* sub;
                for (long j = 0; j < szl; j++) {
                    sub = new List;
                    a->append(sub);
                    for (i = 0; i <lists.size(); i++)
                        sub->append(lists[i]->index(j));
                }
                for (i = 0; i < lists.size(); i++)
                    lists[i]->release();
                return a;
            }
            case l_zipwith: {
                //We combine different lists together with an operator
                //First element is the operation
                first_element = liste[1]->eval(lisp);
                
                vector<Element*> lists;
                long szl = -1;
                long i;
                for (i = 2; i < listsize; i++) {
                    element = liste[i]->eval(lisp);
                    if (!element->isList()) {
                        element->release();
                        a = new Error(L"Error: 'zipwith' only accepts lists as arguments");
                        break;
                    }
                    if (i == 2)
                        szl = element->size();
                    else {
                        if (szl != element->size()) {
                            element->release();
                            a = new Error(L"Error: Lists should all have the same size in 'zipwith'");
                            break;
                        }
                    }
                    lists.push_back(element);
                    element->incrementstatus(2,false);
                    element = null_;
                }
                
                if (a->isError()) {
                    for (auto& e: lists) {
                        e->decrementstatus(2, false);
                    }
                    throw a;
                }
                
                //First element is the operation, second element the list
                first_element = liste[1]->eval(lisp);
                
                long j;
                List lsp;
                long sz;
                long lsz = lists.size();
                if (first_element->isList()) {
                    sz = first_element->size();
                    //lambda or function
                    if (sz == 1)
                        lsp.append(((List*)first_element)->liste[0]);
                    else
                        lsp.append(first_element);
                    for (j = 0; j < lsz; j++)
                    lsp.append(null_);
                }
                else {
                    lsp.append(first_element);
                    for (j = 0; j < lsz; j++)
                    lsp.append(null_);
                }
                
                element = new List;
                
                for (j = 0; j < szl; j++) {
                    for (i = 0; i < lsz; i++)
                        lsp.liste[i+1] = lists[i]->index(j);
                    a = lsp.eval(lisp);
                    element->append(a);
                    a = null_;
                }
                
                for (i = 0; i < lsz; i++)
                    lists[i]->decrementstatus(2, false);
                return element;
            }
            default: {
                if ((label & 0x1000) == 0x1000) {
                    wstring msg = L"Error: wrong number of arguments for: '";
                    msg += lisp->asString(first_element->type);
                    msg += L"'";
                    throw new Error(msg);
                }
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        element->release();
        a->release();
        throw err;
    }
    return null_;
}

Element* Listcallfunction::eval(LispE* lisp) {
    if (lisp->checkLispState()) {
        if (lisp->hasStopped())
            throw lisp->delegation->_THEEND;
        throw new Error("Error: stack overflow");
    }
    Element* function = liste[0]->eval(lisp);
    
    set_current_line(lisp);
    if (lisp->trace) {
        short label = function->index(0)->label();
        if (label == l_defun || label == l_defpat) {
            if (lisp->trace == 2) {
                lisp->display_trace(this);
                lisp->stop_at_next_line(1);
            }
            else
                lisp->trace = 0;
        }
        
        if (label == l_defpat)
            function = evalpattern(lisp, function->index(1)->label());
        else
            function = evalfunction(function, lisp);

        lisp->stop_at_next_line(1);
        return function;
    }


    //In this case, it must be a function
    if (function->index(0)->label() == l_defpat)
        return evalpattern(lisp, function->index(1)->label());
    return evalfunction(function, lisp);
}


Element* Listbreak::eval(LispE* lisp) {
    return break_;
}

Element* Listoperation::eval(LispE* lisp) {
    if (lisp->checkLispState()) {
        if (lisp->hasStopped())
            throw lisp->delegation->_THEEND;
        throw new Error("Error: stack overflow");
    }
    
    set_current_line(lisp);
    lisp->display_trace(this);

    Element* first_element = liste[1]->eval(lisp);
    Element* element = null_;
    short listsize = liste.size();
    short i;

    try {
        switch (liste[0]->type) {
            case l_plus: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->plus(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_minus: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->minus(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_multiply: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->multiply(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_power: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->power(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_leftshift: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->leftshift(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_rightshift: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->rightshift(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_bitand: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_and(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_bitor: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_or(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_bitxor: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_xor(lisp, element);
                }
                
                element->release();
                return first_element;
            }
            case l_plusequal:{
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->plus(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_minusequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->minus(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_multiplyequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->multiply(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
                
            }
            case l_powerequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->power(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_leftshiftequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->leftshift(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_rightshiftequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->rightshift(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_bitandequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_and(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_bitorequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_or(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_bitxorequal: {
                for (i = 2; i < listsize; i++) {
                    _releasing(element);
                    element = liste[i]->eval(lisp);
                    first_element = first_element->bit_xor(lisp, element);
                }
                
                element->release();
                short label = liste[1]->label();
                if (label > l_final)
                    return lisp->recording(first_element, label);
                return first_element;
            }
            case l_divide:
                return divide(lisp,false);
            case l_mod:
                return mod(lisp,false);
            case l_divideequal:
                return divide(lisp,true);
            case l_modequal:
                return mod(lisp,true);
        }
    }
    catch (Error* err) {
        first_element->release();
        element->release();
        throw err;
    }

    return zero_;
}

Element* Listcompose::eval(LispE* lisp) {
    if (lisp->checkLispState()) {
        if (lisp->hasStopped())
            throw lisp->delegation->_THEEND;
        throw new Error("Error: stack overflow");
    }

    List* loop = (List*)liste.back();

    short i = 4;
    short listsize = liste.size()-1;
    short label;

    //When liste[4], the recipient variable is not a list
    //Then we are dealing with a fold
    if (liste[4]->isList())
        label = liste[4]->index(1)->label();
    else {
        //this is a fold, and liste[4] != (setq recipient ())
        //hence no need to execute it...
        label = liste[4]->label();
        i = 5;
    }
    
    for (; i < listsize; i++)
        liste[i]->eval(lisp);

    loop->liste[2]->eval(lisp)->loop(lisp, loop->liste[1]->label(), loop);
    return lisp->get(label);
}
