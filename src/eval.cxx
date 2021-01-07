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
string get_char(jag_get* h);
//------------------------------------------------------------------------------------------
#define _releasing(f) f->release();f=null_
//------------------------------------------------------------------------------------------
struct Comparison {
public:
    List* compare;
    LispE* lisp;
    
    Comparison(LispE* l, List* lst, Element* comp) {
        lisp = l;
        compare = lst;
        compare->append(comp);
        compare->append(null_);
        compare->append(null_);
    }
        
    inline bool eval(Element* i, Element* j) {
        compare->liste[1] = i;
        compare->liste[2] = j;
        return compare->eval(lisp)->Boolean();
        
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
    
    if (mark())
        return (value == liste.object);
    
    setmark(true);
    liste.object = value;
    
    bool rec = true;
    short ilabel = v_null;
    if (record) {
        if (liste[0]->label() == l_quote) {
            setmark(false);
            return liste[1]->unify(lisp, value, false);
        }
        
        ilabel = lisp->extractlabel(this);
        
        //First case, this is one of the basic types
        if (value->type == ilabel && ilabel >= t_atom && ilabel <= t_maybe) {
            setmark(false);
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
            if (exec == null_ || !exec->unify(lisp, value, record)) {
                setmark(false);
                return false;
            }
            try {
                value = eval(lisp);
            }
            catch(Error* err) {
                setmark(false);
                err->release();
                return false;
            }
            setmark(false);
            return value->Boolean();
        }
    }
    else {
        if (value == this) {
            setmark(false);
            return true;
        }
        rec = (lisp->extractlabel(liste[0]) == v_null);
    }
    
    if (!value->isList()) {
        setmark(false);
        return false;
    }
    
    long szvalue = value->size();
    if (sz > 1 && liste[sz-2] == separator_) {
        if (szvalue < sz-2) {
            setmark(false);
            return false;
        }
    }
    else {
        if (szvalue != sz) {
            setmark(false);
            return false;
        }
    }
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    for (long i = 0; i < sz; i++) {
        //In this case, we skip it, no constraints...
        //This is a case of a match with list division
        if (liste[i] == separator_) {
            //The parameter list should only contain one last element:
            if (i != sz - 2) {
                setmark(false);
                return false;
            }
            //We do not care about the rest of the list
            if (liste[i+1] == null_) {
                setmark(false);
                return true;
            }
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
                setmark(false);
                return false;
            }
            setmark(false);
            return true;
        }
        if (liste[i] != null_ && !liste[i]->unify(lisp, value->index(i), rec)) {
            setmark(false);
            return false;
        }
        rec = record;
    }
    setmark(false);
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
    vector<Element*> temporaryargs;
    long i;
    bool pushed = false;
    try {
        for (i = 1; i <= parameters->size(); i++)
            temporaryargs.push_back(liste[i]->eval(lisp));
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
                    temporaryargs[i++]->release();
                }
                thread_lisp->pop();
                throw new Error(L"Error: Wrong parameter description");
            }

            data = temporaryargs[i];

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
        while (i < temporaryargs.size()) {
            temporaryargs[i++]->release();
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
    vector<Element*> temporaryargs;
    bool pushed = false;
    try {
        //The stack is increased by one element to keep track of the calls.
        for (i = 0; i < nbarguments; i++) {
            temporaryargs.push_back(liste[i+1]->eval(lisp));
        }
        for (;i < parameters->size(); i++) {
            temporaryargs.push_back(NULL);
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
            data = temporaryargs[i];
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
                                l->append(temporaryargs[i]);
                                i++;
                            }
                        }
                        data = l;
                        i = parameters->size();
                    }
                    else {
                        while (i < nbarguments) {
                            temporaryargs[i++]->release();
                        }
                        thread_lisp->pop();
                        throw new Error(L"Error: Wrong parameter description");
                    }
                }
                else {
                    label = element->size();
                    if (!label) {
                        while (i < nbarguments) {
                            temporaryargs[i++]->release();
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
                    temporaryargs[i++]->release();
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
            temporaryargs[i++]->release();
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
        //in the case of a goto, we only take into account breakpoints
        if (trace == debug_goto)
            delegation->next_stop = false;
        
        if (!activate_on_breakpoints(e)) {
            if (delegation->debugfunction == NULL) {
                long nb = stackSize();
                string space(nb, ' ');
                cout << "(" << delegation->i_current_line << ") " << nb << ":" << space << e->toString(this) << endl;
            }
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
    bool add = delegation->add_to_listing;
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

    delegation->add_to_listing = add;

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
inline bool checkArity(const unsigned long arity, long sz) {
    unsigned long a = 1 << ((sz < 16)?sz:15);
    return ((arity&a) == a);
}

void Listincode::set_current_line(LispE* lisp) {
    lisp->set_current_line(line, fileidx);
}

//--------------------------------------------------------------------------------
// The main evaluation function, the one that evaluates instructions or functions
//--------------------------------------------------------------------------------

Element* List::eval(LispE* lisp) {
    try {
        short label = liste.at(0)->type | lisp->checkLispState();
        set_current_line(lisp);
        return (this->*lisp->delegation->evals.at(label))(lisp);
    }
    catch (const std::out_of_range& oor) {
        if (!liste.size())
            return this;
        
        if (lisp->checkLispState()) {
            if (lisp->hasStopped())
                throw lisp->delegation->_THEEND;
            throw new Error("Error: stack overflow");
        }

        wstring msg = L"Error: unknown instruction: '";
        msg += lisp->asString(liste[0]->type);
        msg += L"'";
        throw new Error(msg);
    }
}

Element* List::evall_break(LispE* lisp) {
    return break_;
}

Element* List::evall_quote(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    return liste[1];
}


Element* List::evall_return(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 2)
        throw new Error("Error: wrong number of arguments");
    
    Element* first_element = new Return(null_);
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        if (listsize == 2) {
            second_element = liste[1]->eval(lisp);
            ((Return*)first_element)->value = second_element;
        }
        return first_element;

    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }
    return null_;
}

Element* List::evall_and(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = null_;
        for (long i = 1; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            if (!second_element->Boolean()) {
                return false_;
            }
        }
        second_element->release();
        return true_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_apply(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        //(apply func l)
        second_element = liste[2]->eval(lisp);
        if (!second_element->isList()) {
            second_element->release();
            throw new Error("Error: arguments to 'apply' should be given as a list");
        }

        first_element = liste[1]->eval(lisp);

        if (second_element->status == s_constant) {
            List l;
            l.liste = ((List*)second_element)->liste;
            l.liste.insert(0, first_element);
            third_element = l.eval(lisp);
        }
        else {
            //We insert this element in our list
            List* l = (List*)second_element;
            l->liste.insert(0, first_element);

            third_element = l->eval(lisp);

            //We remove this first element
            if (l->status)
                l->liste.erase(0);
            else
                l->release();
        }
        first_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_atomise(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->atomise(second_element->asString(lisp));
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_atomp(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        if (second_element == emptylist_)
            return true_;
        test = second_element->isAtom();
        second_element->release();
        return booleans_[test];
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_atoms(LispE* lisp) {
    if (liste.size() != 1)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    return lisp->atomes();
}


Element* List::evall_bitand(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->bit_and(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_bitandequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->bit_and(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->bit_and(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_bitor(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->bit_or(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }
    return null_;
}


Element* List::evall_bitorequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->bit_or(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->bit_or(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_bitxor(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->bit_xor(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }
    return null_;
}


Element* List::evall_bitxorequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->bit_xor(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->bit_xor(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_block(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        if (listsize == 2)
            return liste[1]->eval(lisp);
        second_element = null_;
        for (long i = 1; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        return second_element;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_cadr(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = first_element->cadr(lisp, second_element);
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_car(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = second_element->car(lisp);
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_catch(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    
    Element* element = null_;

    lisp->display_trace(this);
    
    try {
        for (short i = 1; i < listsize; i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        //This error is converted into a non-blocking error message .
        element = new Maybe(lisp, err);
        err->release();
    }
    return element;
}


Element* List::evall_cdr(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    return liste[1]->eval(lisp)->cdr(lisp);
}


Element* List::evall_check(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        first_element->release();
        if (!test)
            return null_;

        second_element = null_;
        liste.back()->setterminal(terminal);
        for (long i = 2; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_checking(LispE* lisp) {
    if (liste.size() != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        List call;
        if (first_element->isInstruction()) {
            call.liste.push_back(first_element);
            call.liste.push_back(liste[3]);
            call.liste.push_back(second_element);
        }
        else {
            if (second_element->isInstruction()) {
                call.liste.push_back(second_element);
                call.liste.push_back(first_element);
                call.liste.push_back(liste[3]);
            }
            else {
                first_element->release();
                second_element->release();
                throw new Error("Error: condition or operation cannot be evaluated: missing instruction");
            }
        }
        third_element = call.eval(lisp);
        first_element->release();
        second_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_compose(LispE* lisp) {
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


Element* List::evall_cond(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        long szv;
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i];
            szv = second_element->size();

            if (!second_element->isList() || szv <= 1)
                throw new Error(L"Error: in a 'cond' the first element must be a list");

            List* code = (List*)second_element;
            third_element = code->liste[0]->eval(lisp);
            if (third_element->Boolean()) {
                _releasing(third_element);
                first_element = null_;
                code->liste.back()->setterminal(terminal);
                for (long int j = 1; j < szv; j++) {
                    _releasing(first_element);
                    first_element = code->liste[j]->eval(lisp);
                }
                return first_element;
            }
            _releasing(third_element);
        }
        return null_;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}

Element* List::evall_cons(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        //merging an element into the next list
        first_element = liste[1]->eval(lisp);
        if (first_element == emptylist_)
            first_element = null_;

        second_element = liste[2]->eval(lisp);
        if (second_element == null_ || second_element == emptylist_) {
            third_element = new List();
            third_element->append(first_element);
            return third_element;
        }

        if (!second_element->isList()) {
            third_element = new Pair();
            third_element->append(first_element);
            third_element->append(second_element);
            return third_element;
        }

        if (second_element->type == t_pair)
            third_element = new Pair();
        else
            third_element = new List();
        third_element->append(first_element);
        for (long i = 0; i < second_element->size(); i++) {
            third_element->append(second_element->value_on_index(lisp, i));
        }
        second_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_consp(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        test = second_element->isList();
        second_element->release();
        return booleans_[test];
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_converttoatom(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    wstring a;
    evalAsString(1,lisp,a);
    return lisp->provideAtomProtected(a);
}


Element* List::evall_converttointeger(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideInteger(second_element->asInteger());
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_converttonumber(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideNumber(second_element->asNumber());
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_converttostring(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        wstring strvalue = second_element->asString(lisp);
        first_element = lisp->provideString(strvalue);
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_data(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        //We record a data structure of the form: (data (Atom x y z) (Atom x y))
        short lab;
        long i = 1;
        short ancestor = v_null;
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
        return true_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_deflib(LispE* lisp) {
    // we manage extensions to the language with deflib (see systeme.cxx for an example)
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    
    short label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    return lisp->recordingunique(this, label);
}


Element* List::evall_defmacro(LispE* lisp) {
    if (liste.size() != 4)
        throw new Error("Error: wrong number of arguments");
    //We declare a function
    short label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    return lisp->recordingMacro(this, label);
}

Element* List::evall_defpat(LispE* lisp) {
    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");
    
    short label;
    lisp->display_trace(this);
    
    //We declare a function
    label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    return lisp->recordingMethod(this, label);
}

Element* List::evall_defun(LispE* lisp) {
    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");

    //We declare a function
    short label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    return lisp->recordingunique(this, label);
}


Element* List::evall_different(LispE* lisp) {
    if (liste.size() < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        test = first_element->unify(lisp, second_element, false);
        first_element->release();
        second_element->release();
        return booleans_[!test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_divide(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->divide(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }
    return null_;
}


Element* List::evall_divideequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->divide(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->divide(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }


    return null_;
}


Element* List::evall_eq(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize-1 && test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = ( (first_element == second_element) || first_element->equal(lisp, second_element)->Boolean());
            _releasing(first_element);
            _releasing(second_element);
        }
        return booleans_[test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_equal(LispE* lisp) {
    if (liste.size() < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        test = first_element->unify(lisp, second_element, false);
        first_element->release();
        second_element->release();
        return booleans_[test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_eval(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);

        //This is a specific case, when the element is a string
        //we need then to call the a meta-eval, the one that
        //comes with Lisp itself
        if (second_element->isString()) {
            first_element = lisp->eval(second_element->toString(lisp));
            second_element->release();
            if (first_element->isError())
                throw first_element;
            return first_element;
        }
        //We just need to evaluate this element...
        first_element = second_element->eval(lisp);
        if (first_element != second_element)
            second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_extract(LispE* lisp) {
    if (!checkArity(P_THREE|P_FOUR, liste.size()))
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = second_element->extraction(lisp, this);
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_fappend(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        string chemin = first_element->toString(lisp);
        //We put ourselves in append mode
        std::ofstream o(chemin.c_str(), std::ios::binary|std::ios::app);
        if (o.fail()) {
            second_element->release();
            first_element->release();
            string erreur = "Error: Cannot write in file: ";
            erreur += chemin;
            throw new Error(erreur);
        }
        chemin = second_element->toString(lisp);
        o << chemin;
        first_element->release();
        second_element->release();
        return true_;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_flip(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        if (liste[1]->isList() && liste[1]->size() >= 3) {
            List* l = (List*)liste[1];
            //We reverse the two first arguments
            List call;
            call.liste.push_back(l->liste[0]);
            call.liste.push_back(l->liste[2]);
            call.liste.push_back(l->liste[1]);
            for (long i = 3; i < liste[1]->size(); i++) {
                call.liste.push_back(l->liste[i]);
            }
            return call.eval(lisp);
        }
        first_element = liste[1]->eval(lisp);
        if (first_element->isList()) {
            long listsize = first_element->size();
            if (listsize < 2)
                return first_element;
            second_element = new List;
            second_element->append(first_element->index(1));
            second_element->append(first_element->index(0));
            for (long i = 2; i < listsize; i++) {
                second_element->append(first_element->index(i));
            }
            first_element->release();
            return second_element;
        }
        
        if (first_element->isDictionary()) {
            second_element = first_element->reverse(lisp, true);
            first_element->release();
            return second_element;
        }
        first_element->release();
        throw new Error("Error: Cannot apply flip on this structure");
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_folding(LispE* lisp) {
    if (liste.size() != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        //abus de langage: I agree
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
        second_element = call.eval(lisp);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_fread(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);

        second_element = new String("");
        string nom = first_element->toString(lisp);
        first_element->release();
        first_element = second_element->charge(lisp, nom);
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_fwrite(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        string chemin = first_element->toString(lisp);
        std::ofstream o(chemin.c_str(), std::ios::binary);
        if (o.fail()) {
            second_element->release();
            first_element->release();
            string erreur = "Error: Cannot write in file: ";
            erreur += chemin;
            throw new Error(erreur);
        }
        chemin = second_element->toString(lisp);
        o << chemin;
        first_element->release();
        second_element->release();
        return true_;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_getchar(LispE* lisp) {
    if (liste.size() != 1)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    string code = get_char(lisp->delegation->input_handler);
    return lisp->provideString(code);
}


Element* List::evall_greater(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize-1 && test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->more(lisp, second_element)->Boolean();
            _releasing(first_element);
            _releasing(second_element);
        }
        return booleans_[test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_greaterorequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize-1 && test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->moreorequal(lisp, second_element)->Boolean();
            _releasing(first_element);
            _releasing(second_element);
        }
        return booleans_[test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_if(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    char test = true;

    lisp->display_trace(this);

    try {
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
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_ife(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);
    
    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        first_element->release();
        if (test) {
            liste[2]->setterminal(terminal);
            return liste[2]->eval(lisp);
        }

        second_element = null_;
        liste.back()->setterminal(terminal);
        for (long i = 3; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_in(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long idx = 0;
        if (listsize == 4)
            evalAsInteger(3,lisp, idx);
        third_element = first_element->search_element(lisp, second_element, idx);
        first_element->release();
        second_element->release();
        if (third_element != null_) {
            third_element->release();
            return true_;
        }
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_at_index(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        if (listsize == 4) {
            long idx;
            evalAsInteger(2, lisp, idx);
            second_element = liste[3]->eval(lisp);
            third_element = first_element->replace(lisp, idx, second_element);
            second_element->release();
            return third_element;
        }
        second_element = liste[2]->eval(lisp);
        third_element = first_element->protected_index(lisp, second_element);
        second_element->release();
        first_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_input(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    string code;
    if (listsize == 2) {
        wstring wcode;
        evalAsString(1, lisp, wcode);
        s_unicode_to_utf8(code, wcode);
    }
    
    lisp->delegation->reading_string_function(code, lisp->delegation->reading_string_function_object);
    return lisp->provideString(code);
}


Element* List::evall_insert(LispE* lisp) {
    if (liste.size() != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        //We insert a value in a list
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long idx;
        evalAsInteger(3, lisp, idx);
        third_element = first_element->insert(lisp, second_element, idx);
        second_element->release();
        if (third_element != first_element) {
            first_element->release();
            return third_element;
        }
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_irange(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);
    
    double init, inc;
    evalAsNumber(1, lisp, init);
    evalAsNumber(2, lisp, inc);
    if (init == (long)init && inc == (long)inc)
        return new InfiniterangeInteger(init, inc);
    return new Infiniterange(init, inc);
}


Element* List::evall_join(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        wstring sep;
        if (listsize == 3)
            evalAsString(2,lisp,sep);
        second_element = first_element->join_in_list(lisp, sep);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_key(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
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
                second_element = first_element->protected_index(lisp, a_key);
                first_element->release();
                return second_element;
            }
            if (first_element->type == t_dictionaryn) {
                double a_key;
                evalAsNumber(2, lisp, a_key);
                second_element = first_element->protected_index(lisp, a_key);
                first_element->release();
                return second_element;
            }
            throw new Error(L"Error: the first argument must be a dictionary");
        }

        if (listsize == 4) {
            //We store a value
            first_element = liste[1]->eval(lisp);
            if (first_element->type == t_dictionary) {
                wstring a_key;
                evalAsString(2, lisp, a_key);
                second_element = liste[3]->eval(lisp);
                // It is out of question to manipulate a dictionary declared in the code
                first_element = first_element->duplicate_constant_container();
                first_element->recording(a_key, second_element->copying(false));
                return first_element;
            }
            if (first_element->type == t_dictionaryn) {
                double a_key;
                evalAsNumber(2, lisp, a_key);
                second_element = liste[3]->eval(lisp);
                // It is out of question to manipulate a dictionary declared in the code
                first_element = first_element->duplicate_constant_container();
                first_element->recording(a_key, second_element->copying(false));
                return first_element;
            }
            throw new Error(L"Error: the first argument must be a dictionary");
        }
        throw new Error(L"Error: Incorrect number of arguments for 'key'");
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_keyn(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
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
            second_element = first_element->protected_index(lisp, a_key);
            first_element->release();
            return second_element;
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
            second_element = liste[3]->eval(lisp);
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant_container();
            first_element->recording(a_key, second_element->copying(false));
            return first_element;
        }
        throw new Error(L"Error: Incorrect number of arguments for 'keyn'");
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_keys(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        if (first_element->type != t_dictionary && first_element->type != t_dictionaryn) {
            first_element->release();
            throw new Error(L"Error: the first argument must be a dictionary");
        }
        second_element = first_element->thekeys(lisp);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_label(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    short label;

    lisp->display_trace(this);

    try {
        label = liste[1]->label();
        if (label == v_null)
            throw new Error(L"Error: Missing label for 'label'");
        second_element = liste[2]->eval(lisp);
        //the difference with 'setq' is that only one version is recorded.
        //of the variable... Without duplication
        return lisp->recordingunique(second_element, label);
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_lambda(LispE* lisp) {
    if (liste.size() < 3)
        throw new Error("Error: wrong number of arguments");

    if (!liste[1]->isList())
        throw new Error(L"Error: Missing parameter list in a lambda declaration");
    return this;
}


Element* List::evall_last(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    lisp->display_trace(this);
    return liste[1]->eval(lisp)->last_element(lisp);
}


Element* List::evall_leftshift(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(1);
        for (short i = 2; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            first_element = first_element->leftshift(lisp, second_element);
        }

        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_leftshiftequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    short label;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(s_constant);
        for (short i = 2; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            first_element = first_element->leftshift(lisp, second_element);
        }

        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_list(LispE* lisp) {
    short listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = new List();
        second_element = emptylist_;
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element->append(second_element->copying(false));
        }
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}

Element* List::evall_nconc(LispE* lisp) {
    short listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* first_element = null_;
    Element* second_element = null_;
    Element* last = null_;

    lisp->display_trace(this);
    long i, l;
    bool pair = false;

    try {
        last =  liste.back()->eval(lisp);
        pair = !last->isList();
        second_element = liste[1]->eval(lisp);
        if (second_element == emptylist_ || second_element == null_) {
            if (pair) {
                if (listsize == 3)
                    return last;
                first_element = new Pair();
            }
            else
                first_element = new List();
        }
        else {
            if (second_element->isList())
                first_element = second_element->duplicate_constant_container(pair);
            else
                throw new Error("Error: first element is not a list");
        }

        second_element = emptylist_;
        listsize--;
        for (i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (second_element->isList()) {
                for (l = 0; l < second_element->size(); l++) {
                    first_element->append(second_element->value_on_index(lisp, l));
                }
                _releasing(second_element);
            }
            else {
                std::wstringstream st;
                if (i == 2)
                    st << "Error: second argument is not a list";
                else
                    if (i == 3)
                        st << "Error: third argument is not a list";
                    else
                        st << "Error: " << i << "th argument is not a list";
                throw new Error(st.str());
            }
        }
        if (pair)
            first_element->append(last);
        else {
            for (l = 0; l < last->size(); l++) {
                first_element->append(last->value_on_index(lisp, l));
            }
            last->release();
        }
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        last->release();
        throw err;
    }

    return null_;
}


Element* List::evall_load(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->load(second_element->toString(lisp));
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_lock(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        second_element = null_;
        wstring key;
        evalAsString(1, lisp, key);
        ThreadLock* _lock = lisp->delegation->getlock(key);
        test = lisp->checkforLock();
        _lock->locking(test);
        for (long i = 2; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        _lock->unlocking(test);
        return second_element;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_loop(LispE* lisp) {
    if (liste.size() < 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    short label;
    char test = true;

    lisp->display_trace(this);

    try {
        //We loop in a list
        label = liste[1]->label();
        if (label == v_null)
            throw new Error(L"Error: Missing label for 'loop'");
        test = lisp->trace;
        second_element = liste[2]->eval(lisp);
        first_element = second_element->loop(lisp, label, this);
        second_element->release();
        if (test && lisp->trace != debug_goto)
            lisp->stop_at_next_line(debug_next);
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_loopcount(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        long counter;
        evalAsInteger(1, lisp, counter);
        second_element = null_;
        while (counter > 0) {
            for (long i = 2; i < listsize && second_element->type != l_return; i++) {
                _releasing(second_element);
                second_element = liste[i]->eval(lisp);
            }
            //If a 'return' or a 'break' has been placed in the code
            if (second_element->type == l_return) {
                if (second_element->isBreak())
                    return null_;
                //this is a return, it goes back to the function call
                return second_element;
            }
            counter--;
        }
        return second_element;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_lower(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize-1 && test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->less(lisp, second_element)->Boolean();
            _releasing(first_element);
            _releasing(second_element);
        }
        return booleans_[test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_lowerorequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize-1 && test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->lessorequal(lisp, second_element)->Boolean();
            _releasing(first_element);
            _releasing(second_element);
        }
        return booleans_[test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_mapping(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    try {
        //abus de langage: I agree
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
        second_element = call.eval(lisp);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_max(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    try {
        if (listsize == 2) {
            //In this case, the argument must be a list
            first_element = liste[1]->eval(lisp);
            if (!first_element->isList())
                throw new Error("Error: the first argument must be a list");
            if (first_element->size() == 0)
                return null_;
            third_element = first_element->index(0);
            if (first_element->size() == 1)
                return third_element->copying(false);
            for (long i = 2; i < first_element->size(); i++) {
                second_element = first_element->index(i);
                if (third_element->less(lisp, second_element)->Boolean())
                    third_element = second_element;
            }
            return third_element->copying(false);
        }

        first_element = liste[1]->eval(lisp);
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (first_element->less(lisp, second_element)->Boolean()) {
                first_element->release();
                first_element=second_element;
            }
            else {
                _releasing(second_element);
            }
        }
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_maybe(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];

    lisp->display_trace(this);
    
    if (listsize==2) {
        first_element = liste[1]->eval(lisp);
        bool val = (first_element->label() == t_maybe);
        first_element->release();
        return booleans_[val];
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


Element* List::evall_min(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    try {
        if (listsize == 2) {
            //In this case, the argument must be a list
            first_element = liste[1]->eval(lisp);
            if (!first_element->isList())
                throw new Error("Error: the first argument must be a list");
            if (first_element->size() == 0)
                return null_;
            third_element = first_element->index(0);
            if (first_element->size() == 1)
                return third_element->copying(false);
            for (long i = 2; i < first_element->size(); i++) {
                second_element = first_element->index(i);
                if (third_element->more(lisp, second_element)->Boolean())
                    third_element = second_element;
            }
            return third_element->copying(false);
        }

        first_element = liste[1]->eval(lisp);
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (first_element->more(lisp, second_element)->Boolean()) {
                first_element->release();
                first_element=second_element;
            }
            else {
                _releasing(second_element);
            }
        }
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_minus(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->minus(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }


    return null_;
}

Element* List::evall_minusequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->minus(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->minus(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }


    return null_;
}

Element* List::evall_mod(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->mod(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_modequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->mod(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->mod(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_multiply(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->multiply(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_multiplyequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->multiply(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->multiply(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_ncheck(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        //A ncheck is like a check, but when the test fails
        //it executes the second element, otherwise
        //the actual list of instructions starts at 3...
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        first_element->release();
        if (!test) {
            liste[2]->setterminal(terminal);
            return liste[2]->eval(lisp);
        }

        second_element = null_;
        liste.back()->setterminal(terminal);
        for (long i = 3; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_neq(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        test = false;
        for (long i = 1; i < listsize-1 && !test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = ( (first_element == second_element) || first_element->equal(lisp, second_element)->Boolean());
            _releasing(first_element);
            _releasing(second_element);
        }
        return booleans_[!test];
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_not(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    char test = true;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        first_element->release();
        return booleans_[!test];
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_nullp(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        if (second_element == null_)
            return true_;
        second_element->release();
        return false_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_numberp(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        test = second_element->isNumber();
        second_element->release();
        return booleans_[test];
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_or(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = null_;
        for (long i = 1; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            if (second_element->Boolean()) {
                return true_;
            }
        }
        second_element->release();
        return false_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_pipe(LispE* lisp) {
    if (liste.size() != 1)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    //pipe returns a line read on std::cin
    //It returns nil, if the stream is closed...
    string code;
    getline(std::cin, code);
    if (std::cin.eof())
        return null_;
    return lisp->provideString(code);
}


Element* List::evall_plus(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (first_element->isList() && listsize == 2) {
            lst = (List*)first_element;
            listsize = lst->size();
            if (listsize == 0)
                return zero_;
            first_element = lst->liste[0]->copyatom(1);
            i = 1;
        }
        else {
            i = 2;
            first_element = first_element->copyatom(1);
        }
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->plus(lisp, second_element);
        }
        if (lst != this)
            lst->release();
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_plusequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    List* lst = this;
    long i;
    Element* first_element = liste[1];
    Element* second_element = liste[2];
    short label;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);
        second_element = second_element->eval(lisp);
        if (second_element->isList() && listsize == 3) {
            lst = (List*)second_element;
            listsize = lst->size();
            if (!listsize)
                return zero_;
            second_element = null_;
            i = 0;
        }
        else {
            i = 3;
            first_element = first_element->plus(lisp, second_element);
        }
            
        for (; i < listsize; i++) {
            _releasing(second_element);
            second_element = lst->liste[i]->eval(lisp);
            first_element = first_element->plus(lisp, second_element);
        }

        if (lst != this)
            lst->release();
        
        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_pop(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        //If it is a string, we return a copy, we do not modify the string.
        //itself
        if (first_element->isString()) {
            long keyvalue;
            wstring strvalue = first_element->asString(lisp);
            if (listsize == 3)
                evalAsInteger(2, lisp, keyvalue);
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

        if (listsize == 3) {
            second_element = liste[2]->eval(lisp);
            if (first_element->remove(lisp, second_element))
                return first_element;
        }
        else {
            if (first_element->removelast())
                return first_element;
        }
        
        first_element->release();
        return null_;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_power(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(1);
        for (short i = 2; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            first_element = first_element->power(lisp, second_element);
        }

        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_powerequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    short label;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(s_constant);
        for (short i = 2; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            first_element = first_element->power(lisp, second_element);
        }

        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_prettify(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        string s = first_element->prettify(lisp);
        first_element->release();
        return new String(s);
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_print(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        string val;
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            val = second_element->toString(lisp);
            lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
            _releasing(second_element);
        }
        if (lisp->isThread)
            std::cout.flush();
        return emptyatom_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_printerr(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            std::cerr << second_element->toString(lisp);
            _releasing(second_element);
        }
        if (lisp->isThread)
            std::cerr.flush();
        return emptyatom_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_printerrln(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        for (long i = 1; i < listsize; i++) {
            if (i != 1)
                std::cerr << " ";
            second_element = liste[i]->eval(lisp);
            std::cerr << second_element->toString(lisp);
            _releasing(second_element);
        }
        std::cerr << std::endl;
        if (lisp->isThread)
            std::cerr.flush();
        return emptyatom_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_println(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        string val;
        for (long i = 1; i < listsize; i++) {
            if (i != 1)
                val = " ";
            second_element = liste[i]->eval(lisp);
            val += second_element->toString(lisp);
            lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
            _releasing(second_element);
        }
        #ifdef WIN32
        val = "\r";
        #else
            val = "\n";
        #endif
        lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
        if (lisp->isThread)
            std::cout.flush();
        return emptyatom_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_push(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        //We store a value in a list
        first_element = liste[1]->eval(lisp);
        if (!first_element->isList()) {
            first_element->release();
            throw new Error(L"Error: missing list in 'push'");
        }
        first_element = first_element->duplicate_constant_container();
        second_element = liste[2]->eval(lisp);
        first_element->append(second_element->copying(false));
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_range(LispE* lisp) {
    if (liste.size() != 4)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    double init, limit, inc;
    evalAsNumber(1, lisp, init);
    evalAsNumber(2, lisp, limit);
    evalAsNumber(3, lisp, inc);
    return range(lisp, init, limit, inc);
}


Element* List::evall_reverse(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 2 && listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);
    bool duplicate = true;

    try {
        second_element = liste[1]->eval(lisp);
        if (listsize == 3) {
            first_element = liste[2]->eval(lisp);
            duplicate = first_element->Boolean();
            first_element->release();
        }
        first_element = second_element->reverse(lisp, duplicate);
        if (second_element != first_element)
            second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_revertsearch(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long idx = 0;
        if (listsize == 4) {
            evalAsInteger(3,lisp, idx);
        }
        third_element = first_element->search_reverse(lisp, second_element, idx);
        first_element->release();
        second_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_rightshift(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(1);
        for (short i = 2; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            first_element = first_element->rightshift(lisp, second_element);
        }

        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_rightshiftequal(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    short label;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(s_constant);
        for (short i = 2; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            first_element = first_element->rightshift(lisp, second_element);
        }

        second_element->release();
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(first_element, label);
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_search(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long idx = 0;
        if (listsize == 4)
            evalAsInteger(3,lisp, idx);
        third_element = first_element->search_element(lisp, second_element, idx);
        _releasing(first_element);
        _releasing(second_element);
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_searchall(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 3 && listsize != 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long idx = 0;
        if (listsize == 4) {
            evalAsInteger(3,lisp, idx);
        }
        third_element = first_element->search_all_elements(lisp, second_element, idx);
        first_element->release();
        second_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_select(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        //we return the first non null value
        second_element = null_;
        for (long i = 1; i < listsize && second_element == null_; i++)
            second_element = liste[i]->eval(lisp);
        return second_element;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}

#ifdef MAX_STACK_SIZE_ENABLED
Element* List::evall_set_max_stack_size(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    long m;
    if (listsize == 1)
        return lisp->provideInteger(lisp->stack_size_max());
    evalAsInteger(1, lisp, m);
    lisp->set_stack_max_size(m);
    return true_;
}
#endif

Element* List::evall_setg(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    short label;

    lisp->display_trace(this);

    try {
        label = liste[1]->label();
        if (label == v_null)
            throw new Error(L"Error: Missing label for 'setg'");
        second_element = liste[2]->eval(lisp);
        lisp->recordingglobal(second_element, label);
        return true_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_setq(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    short label;

    lisp->display_trace(this);

    try {
        label = liste[1]->label();
        if (label == v_null)
            throw new Error(L"Error: Missing label for 'setq'");
        second_element = liste[2]->eval(lisp);
        lisp->recording(second_element, label);
        return true_;
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_size(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideNumber(second_element->size());
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_sleep(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    long tm;
    evalAsInteger(1, lisp, tm);
    std::this_thread::sleep_for(std::chrono::milliseconds(tm));
    return true_;
}


Element* List::evall_sort(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        //First element is the comparison function OR an operator
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        if (!second_element->isList()) {
            first_element->release();
            second_element->release();
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
                second_element->release();
                throw new Error(L"Error: incorrect comparison function in 'sort'");
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    List complist;
    Comparison comp(lisp, &complist, first_element);
    List* l = (List*)second_element;
    if (comp.checkComparison(l->liste[0])) {
        first_element->release();
        second_element->release();
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
    }
    vector<Element*> v;
    l->liste.get(v);
    sort(v.begin(), v.end(), comp);
    l->liste.set(v);
    first_element->release();
    return second_element;
}


Element* List::evall_stringp(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        test = second_element->isString();
        second_element->release();
        return booleans_[test];
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_threadclear(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    if (listsize == 1) {
        lisp->delegation->thread_clear_all();
        return true_;
    }
    
    wstring key;
    evalAsString(1, lisp, key);
    return booleans_[lisp->delegation->thread_clear(key)];
}


Element* List::evall_threadretrieve(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];

    lisp->display_trace(this);

    try {
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
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_threadstore(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];

    lisp->display_trace(this);

    try {
        wstring key;
        evalAsString(1, lisp, key);
        first_element = liste[2]->eval(lisp);
        lisp->delegation->thread_store(key, first_element);
        first_element->release();
        return true_;
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_throw(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);
    
    wstring msg;
    evalAsString(1, lisp, msg);
    throw new Error(msg);
}


Element* List::evall_trace(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 1 && listsize != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];

    lisp->display_trace(this);

    try {
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
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_trigger(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    wstring key;
    evalAsString(1, lisp, key);
    return booleans_[lisp->delegation->trigger(key)];
}


Element* List::evall_type(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideAtom(second_element->type);
        second_element->release();
        return first_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_unique(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = first_element->unique(lisp);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_use(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        second_element = liste[1]->eval(lisp);
        string nom_bib = second_element->toString(lisp);
        second_element->release();
        return lisp->load_library(nom_bib);
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_values(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = first_element->thevalues(lisp);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_wait(LispE* lisp) {
    if (liste.size() != 1)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);
    
    //We wait for all threads to be finished
    while (lisp->nbjoined) {}
    return true_;
}


Element* List::evall_waiton(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");

    lisp->display_trace(this);

    wstring key;
    evalAsString(1, lisp, key);
    lisp->delegation->waiton(key);
    return true_;
}


Element* List::evall_while(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = null_;
        test = lisp->trace;
        while (first_element->Boolean()) {
            first_element->release();
            for (long i = 2; i < listsize && second_element->type != l_return; i++) {
                _releasing(second_element);
                second_element = liste[i]->eval(lisp);
            }

            //if a 'return' has been placed in the code
            if (second_element->type == l_return) {
                lisp->stop_at_next_line(test);
                if (second_element->isBreak())
                    return null_;
                //this is a return, it goes back to the function call
                return second_element;
            }
            first_element = liste[1]->eval(lisp);
        }
        first_element->release();
        if (test && lisp->trace != debug_goto)
            lisp->stop_at_next_line(debug_next);

        if (second_element->type == l_return) {
            if (second_element->isBreak())
                return null_;
            return second_element;
        }

        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_xor(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);

        if (first_element->Boolean() == second_element->Boolean())
            third_element = false_;
        else
            third_element = true_;

        first_element->release();
        second_element->release();
        return third_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}

Element* List::evall_mark(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 2 && listsize != 3)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = null_;

    lisp->display_trace(this);
    bool test;
    
    try {
        first_element = liste[1]->eval(lisp);
        if (listsize == 2) {
            test = first_element->usermark();
            first_element->release();
            return booleans_[test];
        }
        
        bool test = liste[2]->eval(lisp)->Boolean();
        first_element->setusermark(test);
        first_element->release();
        return true_;
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}

Element* List::evall_resetmark(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = null_;

    lisp->display_trace(this);
    
    try {
        first_element = liste[1]->eval(lisp);
        first_element->resetusermark();
        first_element->release();
        return true_;
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_zerop(LispE* lisp) {
    if (liste.size() != 2)
        throw new Error("Error: wrong number of arguments");
    
    lisp->display_trace(this);
    
    long n;
    evalAsInteger(1, lisp, n);
    return booleans_[!n];
}

Element* List::evall_link(LispE* lisp) {
    if (liste.size() != 3)
        throw new Error("Error: wrong number of arguments");
    
    Element* first_element = null_;
    Element* second_element = null_;
    
    try {
        second_element = liste[2]->eval(lisp);
        if (!second_element->isAtom())
            throw new Error("Error: the second argument must be an atom");
        
        //The first atom is replaced with the second atom code...
        wstring identifier;
        evalAsString(1, lisp, identifier);
        lisp->replaceAtom(identifier, second_element->label());
        return true_;
    }
    catch (Error* err) {
        second_element->release();
        first_element->release();
        throw err;
    }
    
    return null_;
}

Element* List::evall_zip(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 3)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        //We combine different lists together...
        vector<Element*> lists;
        long szl = -1;
        long i;
        for (i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (!second_element->isList()) {
                second_element->release();
                third_element = new Error(L"Error: 'zip' only accepts lists as arguments");
                break;
            }
            if (i == 1)
                szl = second_element->size();
            else {
                if (szl != second_element->size()) {
                    second_element->release();
                    third_element = new Error(L"Error: Lists should all have the same size in 'zip'");
                    break;
                }
            }
            lists.push_back(second_element);
            second_element = null_;
        }

        if (third_element->isError()) {
            for (auto& x: lists)
                x->release();
            throw third_element;
        }

        third_element = new List;
        List* sub;
        for (long j = 0; j < szl; j++) {
            sub = new List;
            third_element->append(sub);
            for (i = 0; i <lists.size(); i++)
            sub->append(lists[i]->index(j));
        }
        for (i = 0; i < lists.size(); i++)
        lists[i]->release();
        return third_element;
    }
    catch (Error* err) {
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_zipwith(LispE* lisp) {
    short listsize = liste.size();
    if (listsize < 4)
        throw new Error("Error: wrong number of arguments");
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    lisp->display_trace(this);

    try {
        //We combine different lists together with an operator
        //First element is the operation
        first_element = liste[1]->eval(lisp);

        vector<Element*> lists;
        long szl = -1;
        long i;
        for (i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (!second_element->isList()) {
                second_element->release();
                third_element = new Error(L"Error: 'zipwith' only accepts lists as arguments");
                break;
            }
            if (i == 2)
                szl = second_element->size();
            else {
                if (szl != second_element->size()) {
                    second_element->release();
                    third_element = new Error(L"Error: Lists should all have the same size in 'zipwith'");
                    break;
                }
            }
            lists.push_back(second_element);
            second_element->incrementstatus(2,false);
            second_element = null_;
        }

        if (third_element->isError()) {
            for (auto& e: lists) {
                e->decrementstatus(2, false);
            }
            throw third_element;
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

        second_element = new List;

        for (j = 0; j < szl; j++) {
            for (i = 0; i < lsz; i++)
            lsp.liste[i+1] = lists[i]->index(j);
            third_element = lsp.eval(lisp);
            second_element->append(third_element);
            third_element = null_;
        }

        for (i = 0; i < lsz; i++)
        lists[i]->decrementstatus(2, false);
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return null_;
}


Element* List::eval_call_function(LispE* lisp) {
    Element* function;
    
    if (liste[0]->label() == l_self) {
        function = lisp->called();
        if (!lisp->trace)
            return evalfunction(function, lisp);
    }
    else
        function = liste[0]->eval(lisp);

    short label = function->index(0)->label();
        
    if (lisp->trace) {
        char tr = debug_next;
        if (label == l_defun || label == l_defpat || label == l_lambda) {
            lisp->display_trace(this);
            if (lisp->trace == debug_inside_function)
                lisp->stop_at_next_line(debug_next);
            else {
                if (lisp->trace == debug_next) {
                    lisp->trace = debug_none;
                }
            }
        }
        
        if (label == l_defpat)
            function = evalpattern(lisp, function->index(1)->label());
        else
            function = evalfunction(function, lisp);
        
        if (lisp->trace != debug_goto)
            lisp->stop_at_next_line(tr);
        return function;
    }

    //In this case, it must be a function
    if (label == l_defpat)
        return evalpattern(lisp, function->index(1)->label());
    return evalfunction(function, lisp);
}

Element* List::evalt_list(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    
    lisp->display_trace(this);
    
    try {
        first_element = first_element->eval(lisp);
        //Execution of a lambda a priori, otherwise it's an error
        second_element = evalfunction(first_element,lisp);
        first_element->release();
        return second_element;
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }
    return null_;
}

Element* List::evall_sum(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 2)
        throw new Error("Error: wrong number of arguments");

    List* lst = NULL;
    Element* first_element = liste[1];
    double v = 0;
    
    try {
        first_element = first_element->eval(lisp);
        if (!first_element->isList())
            throw new Error("Error: expecting a list as argument");
        
        lst = (List*)first_element;
        listsize = lst->size();
        for (long i = 0; i < listsize; i++)
            v += lst->liste[i]->checkNumber(lisp);
        lst->release();
        first_element->release();
        return lisp->provideNumber(v);
    }
    catch (Error* err) {
        if (lst != NULL)
            lst->release();
        first_element->release();
        throw err;
    }
    return null_;
}

Element* List::evall_product(LispE* lisp) {
    short listsize = liste.size();
    if (listsize != 2)
        throw new Error("Error: wrong number of arguments");

    List* lst = NULL;
    Element* first_element = liste[1];
    double v = 1;
    
    try {
        first_element = first_element->eval(lisp);
        if (!first_element->isList())
            throw new Error("Error: expecting a list as argument");
        
        lst = (List*)first_element;
        listsize = lst->size();
        for (long i = 0; i < listsize; i++)
            v *= lst->liste[i]->checkNumber(lisp);
        lst->release();
        first_element->release();
        return lisp->provideNumber(v);
    }
    catch (Error* err) {
        if (lst != NULL)
            lst->release();
        first_element->release();
        throw err;
    }

    return null_;
}
