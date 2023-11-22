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
    bool prev_prepare = lisp->preparingthread;
    lisp->preparingthread = true;
        
    Element* element = null_;
    
    try {
        for (; i < listsize && element->type != l_return; i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        lisp->preparingthread = prev_prepare;
        lisp->check_thread_stack = false;
        lisp->delegation->lock.unlocking();
        throw err;
    }
    lisp->preparingthread = prev_prepare;
    lisp->check_thread_stack = check;
    lisp->delegation->lock.unlocking();
    return element;
}

//------------------------------------------------------------------------------------------
//null_ unifies with all
bool Element::unify(LispE* lisp, Element* value, bool record) {
    return (this == null_ || value == this);
}

bool Element::isequal(LispE* lisp, Element* value) {
    return (this == null_ || value == this);
}

//We recursively traverse the structure to find a match for value variables that complies with the keys...
//We want to handle cases such as: pattern ( {x:v y:v}) with a constraint on v being the same value
bool Dictionary_as_list::traverse(LispE* lisp, Dictionary* d, Strings* keys, Integers* consummed, long di, long i, bool record) {
    if (i == keyvalues.size()) {
        return (consummed->size() == keys->size());
    }
    
    Element* k = keyvalues[i];
    Element* v = valuevalues[i];
    if (k == separator_) {
        if (v == null_)
            return true;
        if (consummed->size() == keys->size())
            return v->unify(lisp, emptydictionary_, record);
        long ii,jj;
        bool found = false;
        Dictionary* dico = lisp->provideDictionary();
        for (ii = 0; ii < keys->size(); ii++) {
            found = false;
            for (jj = 0; jj < consummed->size(); jj++) {
                if (consummed->liste[jj] == ii) {
                    found = true;
                    break;
                }
            }
            if (found)
                continue;
            dico->recording(keys->liste[ii], d->dictionary[keys->liste[ii]]);
        }
        if (!v->unify(lisp, dico, record)) {
            dico->release();
            return false;
        }
        dico->release();
        return true;
    }
    
    if (di == keys->size())
        return false;
    
    
    bool removekey = false;
    bool removevalue = false;
    if (record) {
        removekey = (k != null_ && k->isAtom() && !lisp->checkLabel(k->label()));
        removevalue = (v != null_ && v->isAtom() && !lisp->checkLabel(v->label()));
    }
    Element* e = lisp->provideString(keys->liste[di]);
    if (v->unify(lisp, d->dictionary[keys->liste[di]], record) && k->unify(lisp,e, record)) {
        consummed->liste.push_back(di);
        e->release();
        if (traverse(lisp, d, keys, consummed, di+1, i+1, record))
            return true;
        consummed->liste.pop_back();
    }
    else
        e->release();
    if (removekey)
        lisp->removefromstack(k->label());
    if (removevalue)
        lisp->removefromstack(v->label());
    return traverse(lisp, d, keys, consummed, di+1, i, record);
}

bool Dictionary_as_list::traverse(LispE* lisp, Dictionary_n* d, Numbers* keys, Integers* consummed, long di, long i, bool record) {
    if (i == keyvalues.size()) {
        return (consummed->size() == keys->size());
    }
    
    Element* k = keyvalues[i];
    Element* v = valuevalues[i];
    if (k == separator_) {
        if (v == null_)
            return true;
        if (consummed->size() == keys->size())
            return v->unify(lisp, emptydictionary_, record);
        long ii,jj;
        bool found = false;
        Dictionary_n* dico = lisp->provideDictionary_n();
        for (ii = 0; ii < keys->size(); ii++) {
            found = false;
            for (jj = 0; jj < consummed->size(); jj++) {
                if (consummed->liste[jj] == ii) {
                    found = true;
                    break;
                }
            }
            if (found)
                continue;
            dico->recording(keys->liste[ii], d->dictionary[keys->liste[ii]]);
        }
        if (!v->unify(lisp, dico, record)) {
            dico->release();
            return false;
        }
        return true;
    }
    
    if (di == keys->size())
        return false;
    
    bool removekey = false;
    bool removevalue = false;
    if (record) {
        removekey = (k != null_ && k->isAtom() && !lisp->checkLabel(k->label()));
        removevalue = (v != null_ && v->isAtom() && !lisp->checkLabel(v->label()));
    }
    Element* e = lisp->provideNumber(keys->liste[di]);
    if (v->unify(lisp, d->dictionary[keys->liste[di]], record) && k->unify(lisp, e, record)) {
        consummed->liste.push_back(di);
        e->release();
        if (traverse(lisp, d, keys, consummed, di+1, i+1, record))
            return true;
        consummed->liste.pop_back();
    }
    else
        e->release();
    if (removekey)
        lisp->removefromstack(k->label());
    if (removevalue)
        lisp->removefromstack(v->label());
    return traverse(lisp, d, keys, consummed, di+1, i, record);
}


bool Dictionary_as_list::traverse(LispE* lisp, Dictionary_i* d, Integers* keys, Integers* consummed, long di, long i, bool record) {
    if (i == keyvalues.size()) {
        return (consummed->size() == keys->size());
    }
    
    Element* k = keyvalues[i];
    Element* v = valuevalues[i];
    if (k == separator_) {
        if (v == null_)
            return true;
        if (consummed->size() == keys->size())
            return v->unify(lisp, emptydictionary_, record);
        long ii,jj;
        bool found = false;
        Dictionary_i* dico = lisp->provideDictionary_i();
        for (ii = 0; ii < keys->size(); ii++) {
            found = false;
            for (jj = 0; jj < consummed->size(); jj++) {
                if (consummed->liste[jj] == ii) {
                    found = true;
                    break;
                }
            }
            if (found)
                continue;
            dico->recording(keys->liste[ii], d->dictionary[keys->liste[ii]]);
        }
        if (!v->unify(lisp, dico, record)) {
            dico->release();
            return false;
        }
        return true;
    }
    
    if (di == keys->size())
        return false;
    
    bool removekey = false;
    bool removevalue = false;
    if (record) {
        removekey = (k != null_ && k->isAtom() && !lisp->checkLabel(k->label()));
        removevalue = (v != null_ && v->isAtom() && !lisp->checkLabel(v->label()));
    }
    Element* e = lisp->provideInteger(keys->liste[di]);
    if (v->unify(lisp, d->dictionary[keys->liste[di]], record) && k->unify(lisp, e, record)) {
        consummed->liste.push_back(di);
        e->release();
        if (traverse(lisp, d, keys, consummed, di+1, i+1, record))
            return true;
        consummed->liste.pop_back();
    }
    else
        e->release();
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
        return value->isEmpty();
    
    
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
    Integers* consummed = lisp->provideIntegers();
    if (value->type == t_dictionary) {
        Strings* keys = lisp->provideStrings();
        u_ustring k;
        for (const auto& a : ((Dictionary*)value)->dictionary) {
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
            keys->liste.push_back(k);
        }
        
        found = traverse(lisp, (Dictionary*)value, keys, consummed, 0, mxkeyvalue, record);
        keys->release();
        consummed->release();
        return found;
    }
    
    if (value->type == t_dictionaryn) {
        Numbers* keys = lisp->provideNumbers();
        for (const auto& a : ((Dictionary_n*)value)->dictionary) {
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
            keys->liste.push_back(a.first);
        }
        found = traverse(lisp, (Dictionary_n*)value, keys, consummed, 0, mxkeyvalue, record);
        keys->release();
        consummed->release();
        return found;
    }
    Integers* keys = lisp->provideIntegers();
    for (const auto& a : ((Dictionary_i*)value)->dictionary) {
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
        keys->liste.push_back(a.first);
    }
    found = traverse(lisp, (Dictionary_i*)value, keys, consummed, 0, mxkeyvalue, record);
    keys->release();
    consummed->release();
    return found;
}

bool Listargumentlabel::unify(LispE* lisp, Element* value, bool record) {
    liste.object = value;
    if (value->type == ilabel) {
        return (liste.back()->unify(lisp, value, record));
    }
    return false;
}

bool Listargumentquote::unify(LispE* lisp, Element* value, bool record) {
    liste.object = value;
    return liste[1]->unify(lisp, value, false);
}

#ifdef LISPE_WASM
bool Listargumentfunction::unify(LispE* lisp, Element* value, bool record) {
    //If it is a function embedding: (flip (in 'str x))
    liste.object = value;
    if (!argument->unify(lisp, value, record)) {
        return false;
    }
    value = eval(lisp);
    if (lisp->delegation->current_error) {
        lisp->delegation->reset_context();
        return false;
    }
    bool test = value->Boolean();
    value->release();
    return test;
}
#else
bool Listargumentfunction::unify(LispE* lisp, Element* value, bool record) {
    //If it is a function embedding: (flip (in 'str x))
    liste.object = value;
    if (!argument->unify(lisp, value, record)) {
        return false;
    }
    try {
        value = eval(lisp);
    }
    catch (Error* err) {
        err->release();
        lisp->delegation->reset_context();
        return false;
    }
    bool test = value->Boolean();
    value->release();
    return test;
}
#endif

bool Listargumentdata::unify(LispE* lisp, Element* value, bool record) {
    liste.object = value;
    long szvalue = value->size();
    long sz = liste.size();
    if (szvalue != sz)
        return false;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    bool test = true;
    for (long i = 0; i < sz && test; i++) {
        test = (liste[i] == null_ || liste[i]->unify(lisp, value->index(i), record));
    }
    return test;
}

bool Listargumentset::unify(LispE* lisp, Element* value, bool record) {
    if (!value->isSet())
        return false;

    liste.object = value;
    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue < sz-3) {
        return false;
    }
    
    //The first element is l_set or l_setn
    bool test = true;
    long i;
    List flat;

    void* iter = value->begin_iter();
    Element* next_value;
    for (i = 1; i < sz && test; i++) {
        if (!liste[i]->isAtom()) {
            if (value->checkkey(lisp, liste[i]) == null_) {
                value->clean_iter(iter);
                return false;
            }
            flat.appendraw(liste[i]);
            continue;
        }
        next_value = value->next_iter(lisp, iter);
        while (next_value != emptyatom_ && flat.find_element(lisp, next_value, 0) != -1) {
            next_value->release();
            next_value = value->next_iter(lisp, iter);
        }
        
        //In this case, we skip it, no constraints...
        //This is a case of a match with list division
        if (liste[i] == separator_) {
            //The parameter list should only contain one last element:
            if (i != sz - 2) {
                value->clean_iter(iter);
                return false;
            }
            //We do not care about the rest of the list
            if (liste[i+1] == null_) {
                value->clean_iter(iter);
                return true;
            }
            //we need to build a sublist out of value...
            Element* sublist;
            switch (value->type) {
                case t_sets:
                    sublist = lisp->provideSet_s();
                    break;
                case t_seti:
                    sublist = lisp->provideSet_i();
                    break;
                case t_setn:
                    sublist = lisp->provideSet_n();
                    break;
                default:
                    sublist = lisp->provideSet();
            }
            
            while (next_value != emptyatom_) {
                if (flat.find_element(lisp, next_value, 0) == -1)
                    sublist->append(next_value);
                next_value->release();
                next_value = value->next_iter(lisp, iter);
            }
            value->clean_iter(iter);
            if (!liste[i+1]->unify(lisp, sublist, record)) {
                sublist->release();
                return false;
            }
            return true;
        }
        
        test = (next_value != emptyatom_ && (liste[i] == null_ || liste[i]->unify(lisp, next_value, record)));
    }
    value->clean_iter(iter);
    return test;
}

bool Atomekleene::unify_kleene(LispE* lisp, Element* value, Element* current, long& i, long& r, bool record) {
    if (r >= current->size())
        return false;

    Element* next = null_;
    bool not_the_end = true;
    if (r == current->size() -1)
        not_the_end =  false;
    else
        next = current->index(++r);
    
    long first = i;
    long sz = value->size();
    Element* l;
    if (action == '%') {
        //We do not accept the last element of a rule to be optional
        if (!not_the_end)
            return false;
        
        if (next->unify(lisp, value->index(i), false))
            return unify(lisp, null_, record);
        
        if (i < sz - 1 && next->unify(lisp, value->index(i+1), false)) {
            if (unify(lisp, value->index(i++), record))
                return true;
        }
        i = first;
        return false;
    }

    l = lisp->provideList();
    if (action == '+') {
        if (i >=  sz) {
            l->release();
            return false;
        }
        l->append(value->index(i++)->copying(false));
    }
    
    for (; i < sz; i++) {
        if (not_the_end && next->unify(lisp, value->index(i), false)) {
            if (!unify(lisp, l, record)) {
                i = first;
                l->release();
                return false;
            }
            return true;
        }
        l->append(value->index(i)->copying(false));
    }
    
    if (!not_the_end) {
        if (!unify(lisp, l, record)) {
            i = first;
            l->release();
            return false;
        }
        return true;
    }
    
    i = first;
    l->release();
    return false;
}

bool Listkleene::unify_kleene(LispE* lisp, Element* value, Element* current, long& i, long& r, bool record) {
    if (action == l_mod) {
        if (argument->unify(lisp, value->index(i), record))
            return true;
        lisp->storing_variable(null_, variable->label());
        i--;
        return true;
    }

    long sz = value->size();
    List* l = lisp->provideList();
    
    if (action == l_plus) {
        if (i >=  sz) {
            l->release();
            return false;
        }
        
        if (!argument->unify(lisp, value->index(i++), true)) {
            l->release();
            return false;
        }
        
        l->append(variable->eval(lisp));
        lisp->removefromstack(variable->label());
    }
    
    for (; i < sz; i++) {
        if (!argument->unify(lisp, value->index(i), true))
            break;
        
        l->append(variable->eval(lisp));
        lisp->removefromstack(variable->label());
    }
    i--;
    lisp->storing_variable(l, variable->label());
    return true;
}


bool Listseparator::unify(LispE* lisp, Element* value, bool record) {
    if (!value->isList()) {
        return false;
    }

    liste.object = value;
    long sz = liste.size();
    long szvalue = value->size();
    long ivalue = 0;
    
    Element* e;
    
    bool test = true;
    long i;
    for (i = 0; i < sz && test; i++, ivalue++) {
        e = liste[i];
        //In this case, we skip it, no constraints...
        //This is a case of a match with list division
        if (e == separator_) {
            //We do not care about the rest of the list
            if (liste[i+1] == null_)
                return true;
            
            Element* sublist;
            if (ivalue >= szvalue)
                sublist = emptylist_;
            else {
                //We store the remainder of the list in a variable
                switch (value->type) {
                    case t_list:
                        sublist = new List((List*)value, ivalue);
                        break;
                    case t_llist:
                        sublist = new LList((LList*)value, ivalue);
                        break;
                    case t_floats:
                        sublist = new Floats((Floats*)value, ivalue);
                        break;
                    case t_numbers:
                        sublist = new Numbers((Numbers*)value, ivalue);
                        break;
                    case t_shorts:
                        sublist = new Shorts((Shorts*)value, ivalue);
                        break;
                    case t_integers:
                        sublist = new Integers((Integers*)value, ivalue);
                        break;
                    case t_strings:
                        sublist = new Strings((Strings*)value, ivalue);
                        break;
                    case t_stringbytes:
                        sublist = new Stringbytes((Stringbytes*)value, ivalue);
                        break;
                    default:
                        sublist = value->newInstance();
                        for (; ivalue < szvalue; ivalue++)
                            sublist->append(value->index(ivalue)->copying(false));
                        
                }
            }
            
            if (!liste[i+1]->unify(lisp, sublist, record)) {
                sublist->release();
                return false;
            }
            return true;
        }
        test = (e == null_ || e->unify_kleene(lisp, value, this, ivalue, i, record));
    }
    
    return false;
}

bool List::unify(LispE* lisp, Element* value, bool record) {
    long szrules = liste.size();
    if (!szrules)
        return value->isEmpty();

    if (mark())
        return (value == liste.object);
    
    setmark(true);
    liste.object = value;
    
    bool rec = true;

    if (!record) {
        if (value == this) {
            setmark(false);
            return true;
        }
        rec = (lisp->extractdynamiclabel(liste[0]) == v_null);
    }
    
    if (!value->isList()) {
        setmark(false);
        return false;
    }
    
    long szvalue = value->size();
    long ivalue = 0;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    bool test = true;
    Element* e;
    long irule = 0;
    for (; irule < szrules && test; irule++, ivalue++) {
        e = liste[irule];
        test = (e == null_ || e->unify_kleene(lisp, value, this, ivalue, irule, rec));
        rec = record;
    }
    setmark(false);
    return (test && (irule == szrules) && (ivalue >= szvalue));
}

bool LList::unify(LispE* lisp, Element* value, bool record) {
    if (liste.empty())
        return value->isEmpty();

    bool rec = true;

    if (!record) {
        if (value == this) {
            return true;
        }
        rec = (lisp->extractdynamiclabel(liste.front()) == v_null);
    }
    
    if (!value->isList()) {
        return false;
    }
    
    long szvalue = value->size();
    long ivalue = 0;
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    bool test = true;
    Element* e;
    u_link* i_rule = liste.begin();
    long irule = 0;
    for (; i_rule != NULL && test; i_rule = i_rule->next(), irule++, ivalue++) {
        e = i_rule->value;
        test = (e == null_ || e->unify_kleene(lisp, value, this, ivalue, irule, rec));
        rec = record;
    }
    return (test && (i_rule == NULL) && (ivalue >= szvalue));
}

/*
 This is the reason why we have a 'record' Boolean.
 When we use unify in the context of a pattern function, then record is true, as Atom is then a variable name
 When we use unify to compare structures, then record is false, and if there is no match, it is an error
 */
bool Atome::unify(LispE* lisp, Element* value, bool record) {
    //This is a case, when we record our value into the stack
    return ((record && (this == null_ || lisp->recordargument(value, atome))) || value == this || lisp->checkAncestor(this, value));
}

bool Floats::unify(LispE* lisp, Element* value, bool record) {
    if (value == this)
        return true;

    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue != sz || !value->isList())
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

bool Numbers::unify(LispE* lisp, Element* value, bool record) {
    if (value == this)
        return true;

    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue != sz || !value->isList())
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

bool Shorts::unify(LispE* lisp, Element* value, bool record) {
    if (value == this)
        return true;

    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue != sz || !value->isList())
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

bool Integers::unify(LispE* lisp, Element* value, bool record) {
    if (value == this)
        return true;

    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue != sz || !value->isList())
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

bool Strings::unify(LispE* lisp, Element* value, bool record) {
    if (value == this)
        return true;

    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue != sz || !value->isList())
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

bool Stringbytes::unify(LispE* lisp, Element* value, bool record) {
    if (value == this)
        return true;

    long sz = liste.size();
    long szvalue = value->size();
    
    if (szvalue != sz || !value->isList())
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


Element* List::eval_pattern(LispE* lisp, int16_t function_label) {
    List* arguments = lisp->provideList();
    Element* element;
    Element* body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    int16_t ilabel = -1;
    int16_t sublabel = -1;
    char match;
    char depth = lisp->depths[function_label] - 1;

    try {
        lisp->setStack();
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractdynamiclabel(element, depth);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    arguments->clear();
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
            arguments->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        arguments->release();
        lisp->resetStack();
        throw err;
    }

    body = NULL;
    auto& functions = lisp->delegation->method_pool[lisp->current_space]->at(function_label);
    auto subfunction = functions.find(sublabel);
    if (subfunction == functions.end()) {
        sublabel = v_null;
        //We check, if we have a rollback function
        subfunction = functions.find(sublabel);
        if (subfunction == functions.end()) {
            arguments->release();
            wstring message = L"Error: Could not find a match for function: '";
            message += lisp->asString(function_label);
            message += L"'";
            lisp->resetStack();
            throw new Error(message);
        }
    }

    body = subfunction->second[0];

    ilabel = 1;
    match = 0;
    long sz = subfunction->second.size();
    lisp->push(body);

    while (body != NULL) {
        lisp->setstackfunction(body);
        element = body->index(2);
        if (element->size() == nbarguments) {
            match = true;
            for (i = 0; i < nbarguments && match; i++) {
                match = element->index(i)->unify(lisp, arguments->liste[i], true);
            }
            if (match)
                break;
            
            lisp->clear_top_stack();
        }
        body = NULL;
        if (ilabel < sz)
            body = subfunction->second[ilabel++];
        else {
            if (sublabel != v_null) {
                sublabel = v_null;
                ilabel = 1;
                //We check, if we have a rollback function
                subfunction = functions.find(sublabel);
                if (subfunction != functions.end()) {
                    body = subfunction->second[0];
                    sz = subfunction->second.size();
                }
            }
        }
    }

    if (!match) {
        lisp->pop();
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        lisp->resetStack();
        throw new Error(message);
    }
        
    try {
        nbarguments = body->size();
        if (nbarguments == 4)
            element = body->index(3)->eval(lisp);
        else {
            element = null_;
            for (i = 3; i < nbarguments && element->type != l_return; i++) {
                element->release();
                element = body->index(i)->eval(lisp);
            }
        }
        if (element->type == l_return) {
            body = element->eval(lisp);
            element->release();
            //This version protects 'e' from being destroyed in the stack.
            arguments->release(body);
            lisp->resetStack();
            return lisp->pop(body);
        }
    }
    catch (Error* err) {
        arguments->release();
        lisp->pop();
        lisp->resetStack();
        throw err;
    }

    arguments->release(element);
    lisp->resetStack();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}
//------------------------------------------------------------------------------------------
#ifdef LISPE_WASM
void List::evalthread(LispE* lisp, List* body) {
    Element* element = terminal_;
    
    long nbarguments = body->size();
    
    while (element == terminal_) {
        if (nbarguments == 4)
            element = body->liste[3]->eval(lisp);
        else {
            element = null_;
            for (long i = 3; i < nbarguments && element != terminal_ && element->type != l_return && !element->isError(); i++) {
                element->release();
                element = body->liste[i]->eval(lisp);
            }
        }
        if (element->isError()) {
            lisp->delegation->setError((Error*)element);
            return;
        }
        if (element->type == l_return) {
            element->eval(lisp)->release();
            element->release();
            //This version protects 'e' from being destroyed in the stack.
            return;
        }
    }
    element->release();
}
#else
void List::evalthread(LispE* lisp, List* body) {
    Element* element = terminal_;
    
    try {
        long nbarguments = body->size();
        
        while (element == terminal_) {
            if (nbarguments == 4)
                element = body->liste[3]->eval(lisp);
            else {
                element = null_;
                for (long i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                element->eval(lisp)->release();
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                return;
            }
        }
    }
    catch (Error* err) {
        lisp->delegation->setError(err);
    }
    element->release();
}
#endif

void launchthread(ThreadElement* the) {
    LispE* thread_lisp = the->thread_lisp;
    thread_lisp->current_thread->evalthread(thread_lisp, thread_lisp->current_body);
    thread_lisp->thread_ancestor->nbjoined--;
    //We clean some trailing threads
    thread_lisp->delegation->clean_threads();
    the->cleaning = true;
    delete thread_lisp;
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::eval_thread(LispE* lisp, List* body) {
    //We are already running this thread, it is a recursive call
    //we treat as a regular function
    if (lisp->current_body == body)
        return eval_function(lisp, body);

    //We create a new lisp container for the thread
    LispE* thread_lisp = new LispE(lisp, this, body);
    Element* parameters;
    
    long nbarguments = liste.size()-1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
    if (defaultarguments == -1) {
        wstring message = L"Error: Wrong number of arguments in call to: '(";
        message += body->liste[1]->asString(lisp);
        message += L" ";
        message += body->liste[2]->asString(lisp);
        message += L"...)'";
        throw new Error(message);
    }
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments_thread(lisp, thread_lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments_thread(lisp, thread_lisp, body, (List*)parameters, nbarguments, defaultarguments);
    
    //The thread can be of course recursive, but we do not want this recursivity
    //to trigger a new thread at each call...
    //We only create a thread once... The next calls will be executed
    //as regular functions
    ThreadElement* the = new ThreadElement(thread_lisp);
    the->thid = new std::thread(launchthread, the);
    if (the->thid == NULL) {
        delete the;
        throw new Error("Error: Too many threads created. Cannot execute it...");
    }
    
    lisp->delegation->clean_threads(the);
    return True_;
}

//------------------------------------------------------------------------
//This function is only used to compare the number of
//parameters of a function and its arguments
long List::argumentsize(long sz) {
    long listsz = liste.size();
    if (listsz == sz) {
        while (sz > 0 && liste[sz-1]->isList())
            sz--;
        return sz;
    }
    if (listsz < sz) {
        //If the last argument is a list
        Element* last = liste.back();
        if (listsz && last->isNotEmptyList() && last->index(0)->isEmpty())
            return listsz-1;
        return -1;
    }
    //In this case, the difference in size should only be made up of listings.
    for (long i = listsz -1; i >= sz; i--) {
        if (!liste[i]->isList())
            return -1;
    }
    return sz;
}

void List::sameSizeNoTerminalArguments(LispE* lisp, Element* data, List* parameters) {
    Stackelement* s = lisp->providingStack(data);
    // For each of the parameters we record in the stack
    int16_t label;

    long sz = parameters->size();
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        for (long i = 0; i < sz; i++) {
            label = parameters->liste[i]->label();
            //The evaluation must be done on the previous stage of the stack
            data = liste[i+1]->eval(lisp)->duplicate_constant(lisp);
            s->record_argument(data, label);
        }
    }
    catch (Error* err) {
        lisp->pop(s);
        throw err;
    }
    lisp->pushing(s);
}

void List::sameSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* data, List* parameters) {
    //We then push a new stack element...
    //We cannot push it before, or the system will not be able to resolve
    //the argument variables...
    //Note that if it is a new thread creation, the body is pushed onto the stack
    //of this new thread environment...
    thread_lisp->push(data);
    bool preparethread = lisp->preparingthread;
    lisp->preparingthread = true;
    long sz = parameters->liste.size();
    // For each of the parameters we record in the stack
    try {
        for (long i = 0; i < sz; i++) {
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            //containers should be duplicated...
            data = liste[i+1]->eval(lisp)->duplicate();
            thread_lisp->record_argument(data, parameters->liste[i]->label());
        }
    }
    catch (Error* err) {
        lisp->preparingthread = preparethread;
        thread_lisp->pop();
        throw err;
    }
    lisp->preparingthread = preparethread;
}

void List::sameSizeTerminalArguments(LispE* lisp, List* parameters) {
    Element* data;
    long sz = parameters->size();
    try {
        // For each of the parameters we record in the stack
        for (long i = 0; i < sz; i++) {
            data = liste[i+1]->eval(lisp);
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            lisp->replacingvalue(data, parameters->liste[i]->label());
        }
    }
    catch (Error* err) {
        throw err;
    }
}

void List::differentSizeNoTerminalArguments(LispE* lisp, Element* data, List* parameters,long nbarguments, long defaultarguments) {
    Stackelement* s = lisp->providingStack(data);
    long sz = parameters->liste.size();
    long i;
    List* l = NULL;
    
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        // For each of the parameters we record in the stack
        int16_t label;
        Element* element;
        for (i = 0; i < sz; i++) {
            if (i < nbarguments)
                data = liste[i+1]->eval(lisp);
            else
                data = NULL;
            element = parameters->liste[i];
            
            if (element->isList()) {
                if (element->index(0) == emptylist_) {
                    //This is a specific case, when we store the rest of the arguments
                    //in a list
                    if (element->size() == 2) {
                        //We have our variable:
                        label = element->index(1)->label();
                        //We create a list:
                        l = lisp->provideList();
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
                        i = parameters->liste.size();
                    }
                    else
                        throw new Error(L"Error: Wrong parameter description");
                }
                else {
                    label = element->size();
                    if (!label)
                        throw new Error(L"Error: Wrong parameter description");
                    //If the argument was not provided, then we rely on the default value
                    //if it is available
                    if (data == NULL) {
                        if (label == 2) {
                            data = element->index(1)->eval(lisp); //default value
                        }
                        else
                            data = null_;
                    }
                    label = element->index(0)->label();
                }
            }
            else
                label = element->label();
            
            if (label == v_null || data == NULL)
                throw new Error(L"Error: Wrong parameter description");


            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            data = data->duplicate_constant(lisp);
            s->record_argument(data, label);
        }
    }
    catch (Error* err) {
        lisp->pop(s);
        if (l != NULL)
            l->release();
        throw err;
    }
    lisp->pushing(s);
}

void List::differentSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* data, List* parameters,long nbarguments, long defaultarguments) {
    
    //We create a new stage in the local stack for the new thread
    thread_lisp->push(data);
    
    bool preparethread = lisp->preparingthread;
    lisp->preparingthread = true;

    long i;
    List* l = NULL;
    long sz = parameters->liste.size();
    
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        // For each of the parameters we record in the stack
        int16_t label;
        Element* element;
        for (i = 0; i < sz; i++) {
            if (i < nbarguments)
                data = liste[i+1]->eval(lisp);
            else
                data = NULL;
            
            element = parameters->liste[i];
            
            if (element->isList()) {
                if (element->index(0) == emptylist_) {
                    //This is a specific case, when we store the rest of the arguments
                    //in a list
                    if (element->size() == 2) {
                        //We have our variable:
                        label = element->index(1)->label();
                        //We create a list:
                        l = lisp->provideList();
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
                        i = parameters->liste.size();
                    }
                    else
                        throw new Error(L"Error: Wrong parameter description");
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
            
            if (label == v_null || data == NULL)
                throw new Error(L"Error: Wrong parameter description");


            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
                //containers should be duplicated...
            data = data->duplicate();
            thread_lisp->record_argument(data, label);
        }
    }
    catch (Error* err) {
        lisp->preparingthread = preparethread;
        if (l != NULL)
            l->release();
        thread_lisp->pop();
        throw err;
    }
    lisp->preparingthread = preparethread;
}

//In this case, we record in the current stack
void List::differentSizeTerminalArguments(LispE* lisp, List* parameters, long nbarguments, long defaultarguments) {
    List* l = NULL;
    int16_t label;
    Element* element;
    Element* data;
    long sz = parameters->liste.size();
    
    try {
        
        for (long i = 0; i < sz; i++) {
            data = (i < nbarguments) ? liste[i+1]->eval(lisp) : NULL;
            
            element = parameters->liste[i];
            //This is the zone when arguments can be implicit
            if (element->isList()) {
                if (element->index(0) == emptylist_) {
                    //This is a specific case, when we store the rest of the arguments
                    //in a list
                    if (element->size() == 2) {
                        //We have our variable:
                        label = element->index(1)->label();
                        //We create a list:
                        l = lisp->provideList();
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
                        i = parameters->liste.size();
                    }
                    else
                        throw new Error(L"Error: Wrong parameter description");
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
            
            if (label == v_null || data == NULL)
                throw new Error(L"Error: Wrong parameter description");
            
            lisp->replacingvalue(data, label);
        }
    }
    catch (Error* err) {
        if (l != NULL)
            l->release();
        throw err;
    }
}

Element* List::eval_function(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    Element* parameters;
    Element* element;
    
    long nbarguments = liste.size()-1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
    if (defaultarguments == -1) {
        wstring message = L"Error: Wrong number of arguments in call to: '(";
        message += body->liste[1]->asString(lisp);
        message += L" ";
        message += body->liste[2]->asString(lisp);
        message += L"...)'";
        throw new Error(message);
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
            sameSizeTerminalArguments(lisp, (List*)parameters);
        else
            differentSizeTerminalArguments(lisp, (List*)parameters, nbarguments, defaultarguments);
        return terminal_;
    }
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
        
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    long i;
    nbarguments = body->size();
    try {
        lisp->setStack();
        do {
            if (nbarguments == 4)
                element = body->liste[3]->eval(lisp);
            else {
                element = null_;
                for (i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                parameters = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                lisp->resetStack();
                return lisp->pop(parameters);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->pop();
        throw err;
    }
    
    //This version protects 'e' from being destroyed in the stack.
    lisp->resetStack();
    return lisp->pop(element);
}
    
Element* List::eval_library_function(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    Element* parameters;
    Element* element;
    
    long nbarguments = liste.size() - 1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
        
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
        
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.
    try {
        lisp->setStack();
        element = body->liste[3]->eval(lisp);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->pop();
        throw err;
    }
    
    lisp->resetStack();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}
    

//Execution of a function as well as the shift of parameters with arguments
//For this specific lambda, we do not create a stack
Element* List::eval_lambda_min(LispE* lisp) {
    Element* stackfunction = lisp->exchangestackfunction(this);
    Element* element;
    int16_t nb_instructions = size();
    
    try {
        if (nb_instructions == 3)
            element = liste[2]->eval(lisp);
        else {
            element = null_;
            for (int16_t i = 2; i < nb_instructions && element->type != l_return; i++) {
                element->release();
                element = liste[i]->eval(lisp);
            }
        }
        if (element->type == l_return) {
            Element* e = element->eval(lisp);
            element->release();
            lisp->setstackfunction(stackfunction);
            //This version protects 'e' from being destroyed in the stack.
            return e;
        }
    }
    catch (Error* err) {
        lisp->setstackfunction(stackfunction);
        throw err;
    }
    
    lisp->setstackfunction(stackfunction);
    //This version protects 'e' from being destroyed in the stack.
    return element;
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::eval_lambda(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    List* parameters;
    Element* element;
    
    long nbarguments = liste.size() - 1;
    
    parameters = (List*)body->liste[1];
    if (nbarguments != parameters->size()) {
        wstring message = L"Error: Wrong number of arguments in: '(lambda ";
        message += parameters->asString(lisp);
        message += L"...)'";
        throw new Error(message);
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
        sameSizeTerminalArguments(lisp, parameters);
        return terminal_;
    }
    
    // For each of the parameters we record in the stack
    int16_t label;
    long i;
    vecte<Element*> recorded(nbarguments);
    vecte<int16_t> labels(nbarguments);
    
    try {
        lisp->setStack();
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        for (i = 0; i < nbarguments; i++) {
            label = parameters->liste[i]->label();
            //The evaluation must be done on the previous stage of the stack
            element = liste[i+1]->eval(lisp);
            recorded.push_raw(lisp->record_or_replace(element, label));
            labels.push_raw(label);
        }
    }
    catch (Error* err) {
        lisp->resetStack();
        //We must call reset_in_stack from end to begin
        //the status should popped out from Stack::status
        for (i = labels.size() - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], labels[i]);
        throw err;
    }

    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    nbarguments = body->size();
    Element* stackfunction = lisp->exchangestackfunction(body);
    try {
        do {
            if (nbarguments == 3)
                element = body->liste[2]->eval(lisp);
            else {
                element = null_;
                for (i = 2; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                Element* e = element->eval(lisp);
                element->release();
                //We must call reset_in_stack from end to begin
                //the status should popped out from Stack::status
                for (i = labels.size() - 1; i >= 0; i--)
                    lisp->reset_in_stack(recorded[i], labels[i], e);
                lisp->resetStack();
                return e;
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->setstackfunction(stackfunction);
        for (i = labels.size() - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], labels[i]);
        throw err;
    }
    
    for (i = labels.size() - 1; i >= 0; i--)
        lisp->reset_in_stack(recorded[i], labels[i], element);
    lisp->setstackfunction(stackfunction);
    lisp->resetStack();
    return element;
}


//Execution of a function as well as the shift of parameters with arguments
Element* List::eval_data(LispE* lisp, Element* data) {
    List* values = lisp->provideList();
    values->append(data->index(0));
    Element* element;
    long nbarguments = liste.size()-1;
    try {
        for (long i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            values->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
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


//Execution of a function as well as the shift of parameters with arguments
Element* List::evalfunction(LispE* lisp, Element* body) {
    int16_t label = body->function_label(lisp);
    switch(label) {
        case l_defpat:
            return eval_pattern(lisp, ((List*)body)->liste[1]->label());
        case l_dethread:
            return eval_thread(lisp, (List*)body);
        case l_deflib:
            return eval_library_function(lisp, (List*)body);
        case l_defun:
            return eval_function(lisp, (List*)body);
        case l_lambda:
            return eval_lambda(lisp, (List*)body);
        default:
            return eval_data(lisp, lisp->getDataStructure(label));
    }
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
#ifdef LISPE_WASM
Element* LispE::EVAL(u_ustring& code) {
    return _eval(code);
}
#endif

Element* LispE::eval(u_ustring& code) {
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
        e = compile_lisp_code(code);
        locked = true;
        unlock();
        e = e->eval(this);
    }
    catch (Error* err) {
        if (!locked)
            unlock();
        if (e != NULL)
            e->release();
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
    Element* element;
    temporary.clear();
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
    for (const auto& a: temporary)
        garbages.push_back(a);
    
    //We de-protect... Note that we now give the status s_destructible in this case
    e->protecting(false, this);
    return e;
}
//--------------------------------------------------------------------------------
// The main evaluation function, the one that evaluates instructions or functions
//--------------------------------------------------------------------------------
bool List::eval_Boolean(LispE* lisp, int16_t instruction) {
    return (this->*lisp->delegation->evals[instruction])(lisp)->Boolean();
}

Element* List::evall_void(LispE* lisp) {
    return null_;
}


Element* List::evall_emptylist(LispE* lisp) {
    return emptylist_;
}

Element* List::eval_no_fail(LispE* lisp) {
    try {
        Element* e = (this->*lisp->delegation->evals[lisp->checkState(this, liste.size())])(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return err;
    }
}

Element* List::eval(LispE* lisp) {
    try {
        Element* e = (this->*lisp->delegation->evals[lisp->checkState(this, liste.size())])(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, 0);
    }
}

Element* List_eval::eval(LispE* lisp) {
    return (this->*met)(lisp);
}

//This the function that is called when an eval is done on a list of elements where a function is present
//(eval '(cons 'a 'b)), we first create a List_cons_eval object that will "borrow" the current list content
//We then evaluate.
Element* List::eval_list_instruction(LispE* lisp) {
    List* l = lisp->borrowing((Listincode*)this, size());
    Element* e;
    try {
        e = l->eval(lisp);
    }
    catch(Error* err) {
        delete l;
        return lisp->check_error(this, err, 0);
    }
    delete l;
    return e;
}

Element* LispE::check_error(List* l, Error* err, int idxinfo) {
    delegation->set_error_context(err, idxinfo);
    if (err != delegation->_THEEND) {
        resetStack();
        throw err;
    }

    if (checkLispState()) {
        if (isthreadError()) {
            if (!isThread)
                delegation->throwError();
            return n_null;
        }
        if (hasStopped())
            throw delegation->_THEEND;
        return n_null;
    }
    return l->eval_error(this);
}

Element* Listincode::eval(LispE* lisp) {
    try {
        Element* e = (this->*lisp->delegation->evals[lisp->checkBasicState(this)])(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}

Element* List_execute::eval(LispE* lisp) {
    try {
        lisp->checkPureState(this);
        Element* e = (this->*method)(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}
//--------------------------------------------------------------------------------

Element* List::evall_break(LispE* lisp) {
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
    action->setterminal(terminal);
    return lisp->provideReturn(action->eval(lisp));
}

Element* List::evall_return(LispE* lisp) {
    if (liste.size() == 1)
        return lisp->provideReturn(null_);
    
    liste[1]->setterminal(terminal);
    return lisp->provideReturn(liste[1]->eval(lisp));
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


#ifdef LISPE_WASM
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
        if (lst->element_container()) {
            c->increment();
            lst->release();
            c->decrementkeep();
        }
        else
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
        double d = ((Complex*)e)->content.imag();
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
        lisp->garbaging(this);
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
    return lisp->recordingMethod(this, label);
}

Element* List::evall_defmacro(LispE* lisp) {
    if (liste._size() != 4)
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
        lisp->garbaging(this);
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
    last()->setterminal();
    return lisp->recordingMethod(this, label);
}

Element* List::evall_defspace(LispE* lisp) {
    int16_t label = liste[1]->label();
    if (label == l_thread)
        throw new Error("Error: 'thread' is a reserved space name");
    
    lisp->create_name_space(label);
    return True_;
}

Element* List::evall_defun(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->garbaging(this);
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
        last()->setterminal();
        return this;
    }
    last()->setterminal();
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
    
    liste[test]->setterminal(terminal);
    return liste[test]->eval(lisp);
}

Element* List::evall_ife(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);

    if (element->Boolean()) {
        element->release();
        liste[2]->setterminal(terminal);
        return liste[2]->eval(lisp);
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
            return zero_;
        }
        
        if (container->element_container()) {
            result->increment();
            container->release();
            result->decrementkeep();
        }
        else
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

//Infix Expressions: x op y op z op u
Element* Listincode::eval_infix(LispE* lisp) {
    long listsize = liste.size();
    if (listsize % 2)
        throw new Error("Error: Infix expression is malformed");
    
    //In this case, we will evaluate this expression in the code itself...
    if (listsize == 2)
        return this;
    
    //We use this instruction to read infix structures such as:
    Listincode* operations = new Listincode(idxinfo);
    Listincode* root = operations;
    Listincode* inter;
    Element* op;

    lisp->garbaging(operations);
    
    long iop = 4;
    long ival = 1;

    //First we gather all our ops, there should one be every two elements
    op = liste[2];
    operations->append(op);
    
    while (ival < listsize) {
        
        //We check the sequence of operators of the same kind
        for (; iop < listsize - 1; iop+=2) {
            if (!liste[iop]->isAtom())
                throw new Error("Error: Expecting an operator in this infix expression");
            if (op != liste[iop]) {
                op = liste[iop];
                break;
            }
        }
        
        //we then push all the values up to the operator that is different
        for (;ival < iop; ival += 2)
            operations->append(liste[ival]);
        
        if (iop < listsize -1) {
            //If this operator is + or -, we create on level up
            char comp = lisp->delegation->checkComparator(op->type, root->index(0)->type);
            if (comp == -1) {
                operations = root;
                iop += 2;
                continue;
            }
            
            if (comp) {
                inter = new Listincode(idxinfo);
                lisp->garbaging(inter);
                inter->append(op);
                inter->append(root);
                root = inter;
                //In all cases, operations points to the top level
                operations = root;
            }
            else {
                //We create one level down.
                //We feed this new List with the last element of the current list
                inter = new Listincode(idxinfo);
                lisp->garbaging(inter);
                inter->append(op);
                Element* last = operations->liste.back();
                inter->append(last);
                operations->pop();
                operations->append(inter);
                operations = inter;
            }
        }
        iop += 2;
    }
    return root;
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
    last()->setterminal();
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
        lisp->garbaging(element);
    return True_;
}

Element* List::evall_setg(LispE* lisp) {
    int16_t label = liste[1]->label();
    Element* element = liste[2]->eval(lisp);
    if (!lisp->delegation->replaceFunction(element, label, lisp->current_space))
        lisp->storing_global(element, label);
    return True_;
}

Element* List::evall_let(LispE* lisp) {
    Element* e = liste[2]->eval(lisp);
    lisp->storing_variable(e, liste[1]->label());
    
    long sz = size();
    if (sz > 3) {
        e = null_;
        try {
            for (long i = 3; i < sz; i++) {
                e->release();
                e = liste[i]->eval(lisp);
            }
            lisp->removefromstack(liste[1]->label(), e);
            return e;
        }
        catch (Error* err) {
            lisp->removefromstack(liste[1]->label());
            throw err;
        }
    }

    return True_;
}

Element* List::evall_setq(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->storing_variable(element, liste[1]->label());
    return True_;
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

    bool preparethread = lisp->preparingthread;
    lisp->preparingthread = true;
    lisp->delegation->thread_store(key, value);
    lisp->preparingthread = preparethread;

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

Element* List::eval_call_function(LispE* lisp) {
    if (lisp->trace) {
        Element* body = liste[0]->eval(lisp);

        //We also retrieve its label (which is l_defun or l_defpat or...)
        int16_t label = body->index(0)->label();
        char tr = debug_next;
        if (label == l_defun || label == l_defpat || label == l_lambda) {
            if (lisp->trace == debug_inside_function)
                lisp->stop_at_next_line(debug_next);
            else {
                if (lisp->trace == debug_next) {
                    lisp->trace = debug_none;
                }
            }
        }
        
        body = evalfunction(lisp, body);
        
        if (lisp->trace != debug_goto)
            lisp->stop_at_next_line(tr);
        return body;
    }
    return evalfunction(lisp, liste[0]->eval(lisp));
}


//eval_call_function is usually called once, except in the case of a trace
//In other cases, we promote the call to a specific: t_pattern, t_self or t_function call
//which do not check anything else anymore...
Element* Listincode::eval_call_function(LispE* lisp) {
    Element* body = liste[0]->eval(lisp);

    if (lisp->delegation->trace_on) {
        if (lisp->trace) {
            //We also retrieve its label (which is l_defun or l_defpat or...)
            int16_t label = body->index(0)->label();
            char tr = debug_next;
            if (label == l_defun || label == l_defpat || label == l_lambda) {
                if (lisp->trace == debug_inside_function)
                    lisp->stop_at_next_line(debug_next);
                else {
                    if (lisp->trace == debug_next) {
                        lisp->trace = debug_none;
                    }
                }
            }
            
            body = evalfunction(lisp, body);
            
            if (lisp->trace != debug_goto)
                lisp->stop_at_next_line(tr);
            return body;
        }
        else
            return evalfunction(lisp, body);
    }

    if (lisp->threaded())
        return evalfunction(lisp, body);
    
    int16_t label = body->function_label(lisp);
    switch(label) {
        case l_defpat:
            liste[0] = new List_pattern_eval(this, (List*)body);
            lisp->garbaging(liste[0]);
            return liste[0]->eval(lisp);
        case l_defun:
            liste[0] = new List_function_eval(lisp, this, (List*)body);
            lisp->garbaging(liste[0]);
            return liste[0]->eval(lisp);
        case l_deflib:
            liste[0] = new List_library_eval(this, (List*)body);
            lisp->garbaging(liste[0]);
            return liste[0]->eval(lisp);
        case l_dethread:
            return eval_thread(lisp, (List*)body);
        case l_lambda:
            liste[0] = new Atomefonction(body, t_lambda);
            lisp->garbaging(liste[0]);
            return eval_lambda(lisp, (List*)body);
        default:
            body = lisp->getDataStructure(label);
            liste[0] = new Atomefonction(body, t_data);
            lisp->garbaging(liste[0]);
            return eval_data(lisp, body);
    }
}

Element* Listincode::eval_call_self(LispE* lisp) {
    if (lisp->delegation->trace_on) {
        Element* body = lisp->called();
        if (lisp->trace) {
            //We also retrieve its label (which is l_defun or l_defpat or...)
            int16_t label = body->index(0)->label();
            char tr = debug_next;
            if (label == l_defun || label == l_defpat || label == l_lambda) {
                if (lisp->trace == debug_inside_function)
                    lisp->stop_at_next_line(debug_next);
                else {
                    if (lisp->trace == debug_next) {
                        lisp->trace = debug_none;
                    }
                }
            }
            
            body = evalfunction(lisp, body);
            
            if (lisp->trace != debug_goto)
                lisp->stop_at_next_line(tr);
            return body;
        }
        else
            return evalfunction(lisp, body);
    }

    return evalfunction(lisp, lisp->called());
}

Element* List::eval_call_self(LispE* lisp) {
    return evalfunction(lisp,lisp->called());
}

Element* List::evalt_list(LispE* lisp) {
    Element* first_element = liste[0];
        

    Element* second_element = null_;

    try {
        if (first_element->isInstruction())
            return eval(lisp);
        
        first_element = first_element->eval(lisp);
        if (!first_element->size())
            return first_element;
        
        //Execution of a lambda a priori, otherwise it's an error
        second_element = evalfunction(lisp, first_element);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }
    return second_element;
}

#ifdef MACDEBUG
//This is a stub function, which is used to focus on specific function debugging
Element* List::evall_debug_function(LispE* lisp) {
    return true_;
}
#endif



