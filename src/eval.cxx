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


//------------------------------------------------------------------------------------------

bool List::isExecutable(LispE* lisp) {
    if (liste.size())
        return (liste[0]->isExecutable(lisp));
    return false;
}

bool Atome::isExecutable(LispE* lisp) {
    return lisp->checkFunctionLabel(atome);
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
            keys->liste.push_back(k);
        }
        
        found = traverse(lisp, (Dictionary*)value, keys, consummed, 0, mxkeyvalue, record);
        keys->release();
        consummed->release();
        return found;
    }
    
    Numbers* keys = lisp->provideNumbers();
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
        keys->liste.push_back(a.first);
    }
    found = traverse(lisp, (Dictionary_n*)value, keys, consummed, 0, mxkeyvalue, record);
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

bool Listargumentfunction::unify(LispE* lisp, Element* value, bool record) {
    //If it is a function embedding: (flip (in 'str x))
    liste.object = value;
    if (!argument->unify(lisp, value, record)) {
        return false;
    }
    try {
        value = eval(lisp);
    }
    catch(Error* err) {
        err->release();
        return false;
    }
    bool test = value->Boolean();
    value->release();
    return test;
}

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
        rec = (lisp->extractlabel(liste[0]) == v_null);
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
        rec = (lisp->extractlabel(liste.front()) == v_null);
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
//------------------------------------------------------------------------------------------
bool List::isequal(LispE* lisp, Element* value) {
    if (value == this)
        return true;
    
    if (!value->isList())
        return false;
    
    long szrules = liste.size();
    if (!szrules)
        return (value->isEmpty());

    if (mark())
        return (value == liste.object);
    
    setmark(true);
    liste.object = value;
    
    long i = 0;

    if (value->type == t_llist) {
        LList* l = (LList*)value;
        u_link* u = l->liste.begin();
        
        for (; u!=NULL && i < szrules; i++, u = u->next()) {
            if (!u->value->isequal(lisp, liste[i])) {
                setmark(false);
                return false;
            }
        }
        setmark(false);
        return (u == NULL && i == szrules);
    }
        
    long szvalue = value->size();
    for (; i < szrules && i < szvalue; i++) {
        if (!liste[i]->isequal(lisp, value->index(i))) {
            setmark(false);
            return false;
        }
    }
    setmark(false);
    return (i == szrules && i == szvalue);
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

bool Numbers::isequal(LispE* lisp, Element* value) {
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

bool Shorts::isequal(LispE* lisp, Element* value) {
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

bool Integers::isequal(LispE* lisp, Element* value) {
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

bool Strings::isequal(LispE* lisp, Element* value) {
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
//------------------------------------------------------------------------------------------

Element* List::eval_pattern(LispE* lisp, short function_label) {
    List* arguments = lisp->provideList();
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
            arguments->append(element->duplicate_constant());
        }
    }
    catch(Error* err) {
        arguments->release();
        throw err;
    }

    ilabel = 0;
    match = 0;
    body = lisp->getMethod(function_label,sublabel,ilabel);
    if (body == null_) {
        sublabel = v_null;
        ilabel = 0;
        //We check, if we have a roolback function
        body = lisp->getMethod(function_label,v_null,0);
    }

    lisp->push(body);
    while (body != null_) {
        lisp->setstackfunction(body);
        element = body->index(2);
        if (element->size() == nbarguments) {
            match = true;
            for (i = 0; i < element->size() && match; i++) {
                match = element->index(i)->unify(lisp, arguments->liste[i], true);
            }
            if (match)
                break;
            
            lisp->clear_top_stack();
        }
        body = lisp->getMethod(function_label,sublabel,++ilabel);
        if (body == null_ && sublabel != v_null) {
            sublabel = v_null;
            ilabel = 0;
            //We check, if we have a roolback function
            body = lisp->getMethod(function_label,v_null,ilabel);
        }
    }
    
    if (!match) {
        lisp->pop();
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        throw new Error(message);
    }
    
    element = null_;
    try {
        nbarguments = body->size();
        for (i = 3; i < nbarguments && element->type != l_return; i++) {
            element->release();
            element = body->index(i)->eval(lisp);
        }
        
        if (element->type == l_return) {
            body = element->eval(lisp);
            element->release();
            //This version protects 'e' from being destroyed in the stack.
            arguments->release(body);
            return lisp->pop(body);
        }
    }
    catch(Error* err) {
        arguments->release();
        lisp->pop();
        throw err;
    }

    arguments->release(element);
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}
//------------------------------------------------------------------------------------------
void List::evalthread(LispE* lisp, List* body) {
    Element* element = terminal_;
    
    try {
        long nbarguments = body->size();
        
        while (element == terminal_) {
            element = null_;
            for (long i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                element->release();
                element = body->liste[i]->eval(lisp);
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
        lisp->delegation->setError(err);
    }
    element->release();
}

void launchthread(LispE* call) {
    call->current_thread->evalthread(call, call->current_body);
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

void List::sameSizeNoTerminalArguments(LispE* lisp, Element* data, List* parameters) {
    Stackelement* s = lisp->providingStack(data);
    // For each of the parameters we record in the stack
    short label;

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
            data = liste[i+1]->eval(lisp);
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
        lisp->preparingthread = false;
        thread_lisp->pop();
        throw err;
    }
    lisp->preparingthread = false;
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

void List::differentSizeNoTerminalArguments(LispE* lisp, Element* data, List* parameters,
                                   long nbarguments, long defaultarguments) {
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
        short label;
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

void List::differentSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* data, List* parameters,
                                   long nbarguments, long defaultarguments) {
    
    //We create a new stage in the local stack for the new thread
    thread_lisp->push(data);
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
        short label;
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
        lisp->preparingthread = false;
        if (l != NULL)
            l->release();
        thread_lisp->pop();
        throw err;
    }
    lisp->preparingthread = false;
}

//In this case, we record in the current stack
void List::differentSizeTerminalArguments(LispE* lisp, List* parameters, long nbarguments, long defaultarguments) {
    List* l = NULL;
    short label;
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
    long defaultarguments = parameters->argumentsize(lisp, nbarguments);
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
        do  {
            element = null_;
            for (i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                element->release();
                element = body->liste[i]->eval(lisp);
            }
            if (element->type == l_return) {
                parameters = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                return lisp->pop(parameters);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->pop();
        throw err;
    }
    
    //This version protects 'e' from being destroyed in the stack.
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
    long defaultarguments = parameters->argumentsize(lisp, nbarguments);
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
        
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    try {
        element = body->liste[3]->eval(lisp);
    }
    catch (Error* err) {
        lisp->pop();
        throw err;
    }
    
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
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
    long defaultarguments = parameters->argumentsize(lisp, nbarguments);
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
    std::thread* tid = new std::thread(launchthread, thread_lisp);
    if (tid == NULL) {
        delete thread_lisp;
        throw new Error("Error: Too many threads created. Cannot execute it...");
    }
    return true_;
}

//Execution of a function as well as the shift of parameters with arguments
//For this specific lambda, we do not create a stack
Element* List::eval_lambda_min(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    long i;
    Element* element;
    long nbarguments = body->size();
    
    Element* stackfunction = lisp->exchangestackfunction(body);
    
    try {
        element = null_;
        for (i = 2; i < nbarguments && element->type != l_return; i++) {
            element->release();
            element = body->liste[i]->eval(lisp);
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
    short label;
    long i;
    List* call = lisp->provideList();
    Integers* labels = lisp->provideIntegers();
    
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        for (i = 0; i < nbarguments; i++) {
            label = parameters->liste[i]->label();
            //The evaluation must be done on the previous stage of the stack
            element = liste[i+1]->eval(lisp);
            if (lisp->localsave(parameters->index(i), call))
                lisp->replacingvalue(element, label);
            else {
                lisp->recording(element, label);
                labels->liste.push_back(label);
            }
        }
    }
    catch (Error* err) {
        for (i = 0; i < call->size(); i+=2) {
            lisp->replacingvalue(call->liste[i+1], call->liste[i]->label());
        }
        for (i = 0; i < labels->size(); i++)
            lisp->removefromstack(labels->liste[i]);
        call->release();
        labels->release();
        throw err;
    }

    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    nbarguments = body->size();
    Element* stackfunction = lisp->exchangestackfunction(body);
    
    try {
        do  {
            element = null_;
            for (i = 2; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                element->release();
                element = body->liste[i]->eval(lisp);
            }
            if (element->type == l_return) {
                Element* e = element->eval(lisp);
                element->release();
                for (i = 0; i < call->size(); i+=2) {
                    lisp->replacingvalue(call->liste[i+1], call->liste[i]->label());
                }
                //This version protects 'e' from being destroyed in the stack.
                for (i = 0; i < labels->size(); i++)
                    lisp->removefromstack(labels->liste[i], e);
                labels->release();
                call->release();
                return e;
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->setstackfunction(stackfunction);
        for (i = 0; i < call->size(); i+=2) {
            lisp->replacingvalue(call->liste[i+1], call->liste[i]->label());
        }
        for (i = 0; i < labels->size(); i++)
            lisp->removefromstack(labels->liste[i]);
        labels->release();
        call->release();
        throw err;
    }
    
    for (i = 0; i < call->size(); i+=2) {
        lisp->replacingvalue(call->liste[i+1], call->liste[i]->label());
    }

    for (i = 0; i < labels->size(); i++)
        lisp->removefromstack(labels->liste[i], element);
    labels->release();
    call->release();
    lisp->setstackfunction(stackfunction);
    
    //This version protects 'e' from being destroyed in the stack.
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
            values->append(element->duplicate_constant());
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


//Execution of a function as well as the shift of parameters with arguments
Element* List::evalfunction(LispE* lisp, Element* body) {
    short label = body->function_label();
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
    e->protecting(false, this);
    return e;
}
//--------------------------------------------------------------------------------
// The main evaluation function, the one that evaluates instructions or functions
//--------------------------------------------------------------------------------
bool List::eval_Boolean(LispE* lisp, short instruction) {
    return (this->*lisp->delegation->evals[instruction])(lisp)->Boolean();
}

Element* List::eval(LispE* lisp) {
    try {
        return (this->*lisp->delegation->evals[lisp->checkState(this)])(lisp);
    }
    catch (Error* err) {
        if (err != lisp->delegation->_THEEND)
            throw err;
        
        if (lisp->checkLispState()) {
            if (lisp->isthreadError()) {
                if (!lisp->isThread)
                    lisp->delegation->throwError();
                return null_;
            }
            if (lisp->hasStopped())
                throw lisp->delegation->_THEEND;
            return null_;
        }
        return eval_error(lisp);
    }
}

Element* Listincode::eval(LispE* lisp) {
    try {
        lisp->delegation->set_context(line, fileidx);
        if (lisp->trace)
            lisp->trace_and_context(this);
        return (this->*lisp->delegation->evals[liste.get0()])(lisp);
    }
    catch (Error* err) {
        if (err != lisp->delegation->_THEEND)
            throw err;

        if (lisp->checkLispState()) {
            if (lisp->isthreadError()) {
                if (!lisp->isThread)
                    lisp->delegation->throwError();
                return null_;
            }
            if (lisp->hasStopped())
                throw lisp->delegation->_THEEND;
            return null_;
        }
        return eval_error(lisp);
    }
}

Element* List_basic_execute::eval(LispE* lisp) {
    try {
        if (lisp->trace) {
            lisp->delegation->set_context(line, fileidx);
            lisp->trace_and_context(this);
        }
        return (this->*method)(lisp);
    }
    catch(Error* err) {
        if (err != lisp->delegation->_THEEND)
            throw err;

        if (lisp->checkLispState()) {
            if (lisp->isthreadError()) {
                if (!lisp->isThread)
                    lisp->delegation->throwError();
                return null_;
            }
            if (lisp->hasStopped())
                throw lisp->delegation->_THEEND;
            return null_;
        }
        return eval_error(lisp);
    }
}


Element* List_execute::eval(LispE* lisp) {
    try {
        lisp->delegation->set_context(line, fileidx);
        if (lisp->trace)
            lisp->trace_and_context(this);
        return (this->*method)(lisp);
    }
    catch(Error* err) {
        if (err != lisp->delegation->_THEEND)
            throw err;

        if (lisp->checkLispState()) {
            if (lisp->isthreadError()) {
                if (!lisp->isThread)
                    lisp->delegation->throwError();
                return null_;
            }
            if (lisp->hasStopped())
                throw lisp->delegation->_THEEND;
            return null_;
        }
        return eval_error(lisp);
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


Element* List::evall_return(LispE* lisp) {
    if (liste.size() == 1)
        return lisp->provideReturn(null_);

    try {
        return lisp->provideReturn(liste[1]->eval(lisp));
    }
    catch (Error* err) {
        throw err;
    }
}

Element* List::evall_and(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;
    bool test = true;


    try {
        second_element = null_;
        for (long i = 1; i < listsize && test; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            test = second_element->Boolean();
        }
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}


Element* List::evall_apply(LispE* lisp) {
    Element* function = liste[1];
    Element* arguments = null_;
    Element* result = null_;


    try {
        //(apply func l)
        arguments = liste[2]->eval(lisp);
        if (arguments->type != t_list)
            throw new Error("Error: arguments to 'apply' should be given as a list");

        function = function->eval(lisp);
        List* l = lisp->provideList();
        l->append(function);
        l->extend((List*)arguments);
        
        result = l->eval(lisp);
        function->release();
        arguments->release();
        l->release();
    }
    catch (Error* err) {
        function->release();
        arguments->release();
        result->release();
        throw err;
    }

    return result;
}


Element* List::evall_atomise(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->atomise(second_element->asUString(lisp));
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_atomp(LispE* lisp) {
    Element* second_element = null_;
    bool test = true;


    try {
        second_element = liste[1]->eval(lisp);
        if (second_element == emptylist_)
            return true_;
        test = second_element->isAtom();
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}


Element* List::evall_atoms(LispE* lisp) {

    return lisp->atomes();
}

Element* List::evall_bitnot(LispE* lisp) {
    Element* e = null_;
    Element* first_element = liste[1];
        
    try {
        first_element = first_element->eval(lisp);
        first_element = first_element->copyatom(1);
        e = first_element->bit_not(lisp);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return e;
}

Element* List::evall_addr_(LispE* lisp) {
    Element* element = liste[1];
    uint64_t addr;
        
    try {
        element = element->eval(lisp);
        addr = (uint64_t)element;
        element->release();
        element = lisp->provideInteger(addr);
    }
    catch (Error* err) {
        element->release();
        throw err;
    }

    return element;

}



Element* List::evall_block(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;

    
    //We might need to mark the last element as being terminal
    //the block might belong to an if
    if (listsize)
        liste.back()->setterminal(terminal);

    try {
        if (listsize == 2)
            return liste[1]->eval(lisp);
        second_element = null_;
        for (long i = 1; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        throw err;
    }

    return second_element;
}


Element* List::evall_elapse(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;
    double diff = 0;
    
    try {
        std::chrono::high_resolution_clock::time_point chrono_beg = std::chrono::high_resolution_clock::now();
        second_element = null_;
        for (long i = 1; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        std::chrono::high_resolution_clock::time_point chrono_end = std::chrono::high_resolution_clock::now();
        
        if (second_element->type == l_return) {
            Element* body = second_element->eval(lisp);
            _releasing(second_element);
            body->release();
        }
        
        diff = std::chrono::duration_cast<std::chrono::milliseconds>( chrono_end - chrono_beg).count();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return lisp->provideNumber(diff);
}

Element* List::evall_emptyp(LispE* lisp) {
    
    Element* element = liste[1]->eval(lisp);
    bool b = element->isEmpty();
    element->release();
    return booleans_[b];
}

Element* List::evall_cadr(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = first_element->cadr(lisp, second_element);
        if (second_element->element_container()) {
            first_element->increment();
            second_element->release();
            first_element->decrementkeep();
        }
        else
            second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_car(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = second_element->car(lisp);
        if (second_element->element_container()) {
            first_element->increment();
            second_element->release();
            first_element->decrementkeep();
        }
        else
            second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_maplist(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* element = null_;
    Element* op = null_;
    Element* container = NULL;
    
    lisp->set_true_as_one();
    List* call = lisp->provideList();
    short label = -1;
    void* iter = NULL;
    
    try {
        element = liste[2]->eval(lisp);
        
        op = liste[1];
        if (!op->isLambda())
            op = liste[1]->eval(lisp);

        listsz = element->size();
        if (!listsz) {
            call->release();
            element->release();
            lisp->set_true_as_true();
            return emptylist_;
        }
        
        Element* e;
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");
            
            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);

            if (lisp->localsave(op->index(1)->index(0), call))
                lisp->replacingvalue(nxt, label);
            else
                lisp->recording(nxt, label);
            
            e = eval_lambda_min(lisp, (List*)op);
            switch (e->type) {
                case t_string:
                    container = lisp->provideStrings();
                    break;
                case t_integer:
                    container = lisp->provideIntegers();
                    break;
                case t_float:
                    container = lisp->provideFloats();
                    break;
                case t_number:
                    container = lisp->provideNumbers();
                    break;
                default:
                    container = lisp->provideList();
            }
            e = e->copying(false);
            container->append(e);
            e->release();
            nxt = element->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                lisp->replacingvalue(nxt, label);
                e = eval_lambda_min(lisp, (List*)op);
                e = e->copying(false);
                container->append(e);
                e->release();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            if (call->size())
                lisp->replacingvalue(call->liste[1], label);
            else
                lisp->removefromstack(label);
            call->release();
        }
        else {
            call->append(op);
            call->append(null_);

            methodEval met = lisp->delegation->evals[op->type];
            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);
            call->liste[1] = nxt->quoting(lisp);
            call->liste[1]->increment();
            e = (call->*met)(lisp);
            switch (e->type) {
                case t_string:
                    container = lisp->provideStrings();
                    break;
                case t_integer:
                    container = lisp->provideIntegers();
                    break;
                case t_float:
                    container = lisp->provideFloats();
                    break;
                case t_number:
                    container = lisp->provideNumbers();
                    break;
                default:
                    container = lisp->provideList();
            }
            e = e->copying(false);
            container->append(e);
            if (e != call->liste[1])
                call->liste[1]->decrement();
            e->release();
            nxt = element->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                call->liste[1] = nxt->quoting(lisp);
                call->liste[1]->increment();
                e = (call->*met)(lisp)->copying(false);
                container->append(e);
                if (e != call->liste[1])
                    call->liste[1]->decrement();
                e->release();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            call->rawrelease();
            element->clean_iter(iter);
        }
        element->release();
        op->release();
        lisp->set_true_as_true();
        return container;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
        }
        if (iter != NULL)
            element->clean_iter(iter);
        call->release();
        lisp->set_true_as_true();
        element->release();
        op->release();
        if (container != NULL)
            container->release();
        throw err;
    }

    return emptylist_;
}

Element* List::evall_filterlist(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* element = null_;
    Element* op = null_;
    Element* result = null_;
    
    lisp->set_true_as_one();
    short ps = 1;
    List* call = lisp->provideList();
    short label = -1;
    void* iter =  NULL;
    
    try {
        element = liste[2]->eval(lisp);
        
        switch (element->type) {
            case t_floats:
                result = lisp->provideFloats();
                break;
            case t_numbers:
                result = lisp->provideNumbers();
                break;
            case t_shorts:
                result = new Shorts();
                break;
            case t_integers:
                result = lisp->provideIntegers();
                break;
            case t_strings:
                result = lisp->provideStrings();
                break;
            case t_string:
                result = lisp->provideString();
                break;
            case t_llist:
                result = new LList(&lisp->delegation->mark);
                break;
            default:
                result = lisp->provideList();
        }

        listsz = element->size();
        if (!listsz) {
            call->release();
            element->release();
            lisp->set_true_as_true();
            return result;
        }
        
        op = liste[1];
        if (!op->isLambda())
            op = liste[1]->eval(lisp);
        
        Element* e;

        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);

            if (lisp->localsave(op->index(1)->index(0), call))
                lisp->replacingvalue(nxt, label);
            else
                lisp->recording(nxt, label);
            
            e = eval_lambda_min(lisp, (List*)op);
            if (e->Boolean()) {
                e->release();
                e = element->index(0)->copying(false);
                result->append(e);
            }
            e->release();
            nxt = element->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                lisp->replacingvalue(nxt, label);
                e = call->eval_lambda_min(lisp, (List*)op);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    result->append(e);
                }
                e->release();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
            call->release();
        }
        else {
            if (!op->isList() || !op->size())
                throw new Error("Error: Wrong filter");
            
            for (ps = 0; ps < op->size(); ps++) {
                call->append(op->index(ps));
            }
            call->append(null_);
            methodEval met = lisp->delegation->evals[op->type];
            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                call->liste[ps] = nxt->quoting(lisp);
                call->liste[ps]->increment();
                e = (call->*met)(lisp);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    result->append(e);
                }
                e->release();
                call->liste[ps]->decrement();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            call->rawrelease();
        }
        element->release();
        op->release();
        lisp->set_true_as_true();
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
        }
        
        call->release();
        lisp->set_true_as_true();
        if (iter != NULL)
            element->clean_iter(iter);
        element->release();
        op->release();
        result->release();
        throw err;
    }

    return emptylist_;
}

Element* List::evall_takelist(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* element = null_;
    Element* op = null_;
    Element* result = null_;
    
    lisp->set_true_as_one();
    short ps = 1;
    List* call = lisp->provideList();
    short label = -1;
    void* iter = NULL;
    
    try {
        element = liste[2]->eval(lisp);
        
        switch (element->type) {
            case t_floats:
                result = lisp->provideFloats();
                break;
            case t_numbers:
                result = lisp->provideNumbers();
                break;
            case t_shorts:
                result = new Shorts();
                break;
            case t_integers:
                result = lisp->provideIntegers();
                break;
            case t_strings:
                result = lisp->provideStrings();
                break;
            case t_string:
                result = lisp->provideString();
                break;
            case t_llist:
                result = new LList(&lisp->delegation->mark);
                break;
            default:
                result = lisp->provideList();
        }

        listsz = element->size();
        if (!listsz) {
            call->release();
            element->release();
            lisp->set_true_as_true();
            return result;
        }
        
        op = liste[1];
        if (!op->isLambda())
            op = liste[1]->eval(lisp);
        
        Element* e;

        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);

            if (lisp->localsave(op->index(1)->index(0), call))
                lisp->replacingvalue(nxt, label);
            else
                lisp->recording(nxt, label);
            
            e = eval_lambda_min(lisp, (List*)op);
            if (e->Boolean()) {
                e->release();
                e = element->index(0)->copying(false);
                result->append(e);
            }
            else
                listsz = 0; //we force to stop now...
            
            e->release();
            nxt = element->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                lisp->replacingvalue(nxt, label);
                e = call->eval_lambda_min(lisp, (List*)op);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    result->append(e);
                }
                else {
                    e->release();
                    break;
                }
                e->release();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
            call->release();
        }
        else {
            if (!op->isList() || !op->size())
                throw new Error("Error: Wrong filter");
            
            for (ps = 0; ps < op->size(); ps++) {
                call->append(op->index(ps));
            }
            call->append(null_);
            methodEval met = lisp->delegation->evals[op->type];
            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->liste[ps] = nxt->quoting(lisp);
                call->liste[ps]->increment();
                e = (call->*met)(lisp);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    result->append(e);
                    e->release();
                }
                else {
                    e->release();
                    call->liste[ps]->decrement();
                    break;
                }
                call->liste[ps]->decrement();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            call->rawrelease();
        }
        element->release();
        op->release();
        lisp->set_true_as_true();
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
        }
        
        if (iter != NULL)
            element->clean_iter(iter);
        call->release();
        lisp->set_true_as_true();
        element->release();
        op->release();
        result->release();
        throw err;
    }

    return emptylist_;
}

Element* List::evall_droplist(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* element = null_;
    Element* op = null_;
    Element* result = null_;
    
    lisp->set_true_as_one();
    short ps = 1;
    List* call = lisp->provideList();
    short label = -1;
    void* iter = NULL;
    
    try {
        element = liste[2]->eval(lisp);
        
        switch (element->type) {
            case t_floats:
                result = lisp->provideFloats();
                break;
            case t_numbers:
                result = lisp->provideNumbers();
                break;
            case t_shorts:
                result = new Shorts();
                break;
            case t_integers:
                result = lisp->provideIntegers();
                break;
            case t_strings:
                result = lisp->provideStrings();
                break;
            case t_string:
                result = lisp->provideString();
                break;
            case t_llist:
                result = new LList(&lisp->delegation->mark);
                break;
            default:
                result = lisp->provideList();
        }

        listsz = element->size();
        if (!listsz) {
            call->release();
            element->release();
            lisp->set_true_as_true();
            return result;
        }
        
        op = liste[1];
        if (!op->isLambda())
            op = liste[1]->eval(lisp);
        
        Element* e;
        bool add = false;

        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);

            if (lisp->localsave(op->index(1)->index(0), call))
                lisp->replacingvalue(nxt, label);
            else
                lisp->recording(nxt, label);
            
            e = eval_lambda_min(lisp, (List*)op);
            if (e->Boolean()) {
                e->release();
                e = element->index(0)->copying(false);
                result->append(e);
                add = true;
            }
            e->release();
            nxt = element->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                lisp->replacingvalue(nxt, label);
                e = call->eval_lambda_min(lisp, (List*)op);
                if (add || e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    result->append(e);
                    add = true;
                }
                e->release();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
            call->release();
        }
        else {
            if (!op->isList() || !op->size())
                throw new Error("Error: Wrong filter");
            
            for (ps = 0; ps < op->size(); ps++) {
                call->append(op->index(ps));
            }
            call->append(null_);
            methodEval met = lisp->delegation->evals[op->type];
            iter = element->begin_iter();
            Element* nxt = element->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->liste[ps] = nxt->quoting(lisp);
                call->liste[ps]->increment();
                e = (call->*met)(lisp);
                if (add || e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    result->append(e);
                    add = true;
                }
                e->release();
                call->liste[ps]->decrement();
                nxt = element->next_iter_exchange(lisp, iter);
            }
            element->clean_iter(iter);
            call->rawrelease();
        }
        element->release();
        op->release();
        lisp->set_true_as_true();
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (call->size())
                lisp->replacingvalue(call->liste[1], call->liste[0]->label());
            else
                lisp->removefromstack(label);
        }
        
        if (iter != NULL)
            element->clean_iter(iter);

        call->release();
        lisp->set_true_as_true();
        element->release();
        op->release();
        result->release();
        throw err;
    }

    return emptylist_;
}

Element* List::evall_stringf(LispE* lisp) {
    char* buffer = NULL;
    Element* e = liste[2];
    string format;
    long sz = 0;
    
    try {
        e = e->eval(lisp);
        format = e->toString(lisp);
        e->release();
        e = liste[1]->eval(lisp);
        sz = format.size() + 100;
        buffer = new char[sz];
        switch (e->type) {
            case t_float:
                sprintf_s(buffer, sz, format.c_str(), e->asFloat());
                break;
            case t_number:
                sprintf_s(buffer, sz, format.c_str(), e->asNumber());
                break;
            case t_integer:
                sprintf_s(buffer, sz, format.c_str(), e->asInteger());
                break;
            default:
                throw new Error("Error: the first argument should be a number");
        }
        e->release();
    }
    catch(Error* err) {
        if (buffer != NULL)
            delete[] buffer;
        e->release();
        throw err;
    }
    
    format = buffer;
    delete[] buffer;
    return lisp->provideString(format);
}

Element* List::evall_factorial(LispE* lisp) {
    static unsigned long factorials[] = {1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880L, 3628800L, 39916800L, 479001600L, 6227020800L, 87178291200L, 1307674368000L, 20922789888000L, 355687428096000L, 6402373705728000L, 121645100408832000L, 2432902008176640000L};
    
    Element* e = null_;
    Element* r = null_;
    long value = 0;
    unsigned long res = 1;

    try {
        e = liste[1]->eval(lisp);
        if (e->isList()) {
            long listsize = e->size();
            r = lisp->provideNumbers();
            for (long j = 0; j < listsize; j++) {
                res = 1;
                value = e->index(j)->asInteger();
                if (value < 0)
                    throw new Error("Error: factorial of a negative number does not exists");
                if (value <= 20)
                    ((Numbers*)r)->liste.push_back(factorials[value]);
                else {
                    res = (long)factorials[20];
                    for (long i = 21; i <= value; i++) {
                        res *= i;
                    }
                    ((Numbers*)r)->liste.push_back(factorials[value]);
                }
            }
            e->release();
            return r;
        }
        
        value = e->asInteger();
        if (value < 0)
            throw new Error("Error: factorial of a negative number does not exists");
        e->release();
        if (value <= 20)
            return lisp->provideInteger((long)factorials[value]);
        
        res = (long)factorials[20];
        for (long i = 21; i <= value; i++)
            res *= i;
    }
    catch (Error* err) {
        e->release();
        r->release();
        throw err;
    }
    
    return lisp->provideInteger(res);
}

Element* List::evall_iota(LispE* lisp) {
    long sz = size();
    Element* res = null_;
    Element* e = null_;
    Element* sub;
    try {
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_integer) {
                res = lisp->provideIntegers();
                for (long j = 1; j <= e->asInteger(); j++) {
                    ((Integers*)res)->liste.push_back(j);
                }
            }
            else {
                res = lisp->provideNumbers();
                double v = e->asNumber();
                long u = v;
                
                for (double j = 1 + (v-u); j <= v; j++) {
                    ((Numbers*)res)->liste.push_back(j);
                }
            }
            e->release();
            return res;
        }
        
        res = lisp->provideList();
        for (long i = 1; i < sz; i++) {
            e = liste[i]->eval(lisp);
            if (e->type == t_integer) {
                sub = lisp->provideIntegers();
                res->append(sub);
                for (long j = 1; j <= e->asInteger(); j++) {
                    ((Integers*)sub)->liste.push_back(j);
                }
            }
            else {
                sub = lisp->provideNumbers();
                res->append(sub);
                double v = e->asNumber();
                long u = v;
                for (double j = 1 + (v-u); j <= v; j++) {
                    ((Numbers*)sub)->liste.push_back(j);
                }
            }
            e->release();
        }
    }
    catch (Error* err) {
        e->release();
        res->release();
        throw err;
    }
    
    return res;
}

Element* List::evall_iota0(LispE* lisp) {
    long sz = size();
    Element* res = null_;
    Element* e = null_;
    Element* sub;
    try {
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_integer) {
                res = lisp->provideIntegers();
                for (long j = 0; j < e->asInteger(); j++) {
                    ((Integers*)res)->liste.push_back(j);
                }
            }
            else {
                res = lisp->provideNumbers();
                double v = e->asNumber();
                long u = v;
                
                for (double j = (v-u); j < v; j++) {
                    ((Numbers*)res)->liste.push_back(j);
                }
            }
            e->release();
            return res;
        }
        
        res = lisp->provideList();
        for (long i = 1; i < sz; i++) {
            e = liste[i]->eval(lisp);
            if (e->type == t_integer) {
                sub = lisp->provideIntegers();
                res->append(sub);
                for (long j = 0; j < e->asInteger(); j++) {
                    ((Integers*)sub)->liste.push_back(j);
                }
            }
            else {
                sub = lisp->provideNumbers();
                res->append(sub);
                double v = e->asNumber();
                long u = v;
                
                for (double j = (v-u); j < v; j++) {
                    ((Numbers*)sub)->liste.push_back(j);
                }
            }
            e->release();
        }
    }
    catch (Error* err) {
        res->release();
        throw err;
    }
    
    return res;
}

Element* apply_op1_op2(LispE* lisp, Element* op1, Element* op2, Element* l1, Element* l2) {
    List* call = lisp->provideList();
    call->append(op2);

    Element* res;

    methodEval met = lisp->delegation->evals[op2->type];

    if (op2->isOperator()) {
        List* L1 = lisp->provideQuoted(l1);
        List* L2 = lisp->provideQuoted(l2);
        
        //We are applying the second operator between l1 and l2
        call->append(L1);
        call->append(L2);
        
        
        try {
            res = (call->*met)(lisp);
        }
        catch(Error* err) {
            call->release();
            throw err;
        }
        
        call->release();
    }
    else {
        //We need to do the job ourselves...
        res = lisp->provideList();
        call->append(l1->index(0));
        call->append(l2->index(0));
        try {
            res->append((call->*met)(lisp));
            for (long i = 1; i < l1->size(); i++) {
                call->replacing(1, l1->index(i)->quoting(lisp));
                call->replacing(2, l2->index(i)->quoting(lisp));
                res->append((call->*met)(lisp));
            }
        }
        catch(Error* err) {
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
        List* L1 = lisp->provideQuoted(res);
        
        //Then we do a reduce on this list with the first operator
        call->append(L1);
        try {
            op2 = (call->*met)(lisp);
        }
        catch(Error* err) {
            call->release();
            L1->release();
            throw err;
        }
        
        call->release();
        return op2;
    }
    
    if (!res->size()) {
        call->release();
        return res;
    }
    
    call->append(res->value_on_index(lisp, (long)0)->quoting(lisp));
    call->append(res->index(1)->quoting(lisp));
    
    Element* e = null_;
    try {
        e = (call->*met)(lisp)->quoting(lisp);
        call->replacing(1, e);
        for (long i = 2; i < res->size(); i++) {
            call->replacing(2, res->index(i)->quoting(lisp));
            e = (call->*met)(lisp)->quoting(lisp);
            call->replacing(1, e);
        }
    }
    catch(Error* err) {
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
    Element* matrix= null_;
    
    Element* transposed_matrix = null_;
    
    try {
        matrix = liste[1]->eval(lisp);
        transposed_matrix = matrix->transposed(lisp);
    }
    catch (Error* err) {
        matrix->release();
        transposed_matrix->release();
        throw err;
    }
    
    matrix->release();
    return transposed_matrix;
}

// (.  '((1 2) (5 4) (3 0)) '+ '* '((6 2 3 4) (7 0 1 8)))
// (. (iota 10) '+ '* (iota 10.0))
//(setq m1 (rho 3 2 '(1 2 5 4 3 0)))
//(setq m2 (rho 2 4 '(6 2 3 4 7 0 1 8)))
Element* List::evall_innerproduct(LispE* lisp) {
    Element* l1 = null_;
    Element* l2 = null_;
    Element* op1 = null_;
    Element* op2 = null_;
    Element* e = null_;
    lisp->set_true_as_one();

    try {
        l1 = liste[1]->eval(lisp);
        l2 = liste[4]->eval(lisp);
        
        long sx_1, sy_1;
        char t1 = l1->isPureList(sx_1, sy_1);

        long sx_2, sy_2;
        char t2 = l2->isPureList(sx_2, sy_2);

        if (!t1 || !t2 || t1 != t2)
            throw new Error("Error: arguments for '.' must be compatible lists or matrices");
        
        op1 = liste[2]->eval(lisp);
        if (op1->type == l_equal)
            op1 = lisp->provideAtom(l_equalonezero);
        op2 = liste[3]->eval(lisp);
        if (op2->type == l_equal)
            op2 = lisp->provideAtom(l_equalonezero);

        if (t1 == 2) {
            if (sx_1 != sx_2)
                throw new Error("Error: lists should have the same size for '.'");
                                
            l1->increment();
            l2->increment();
            e = apply_op1_op2(lisp, op1, op2, l1, l2);
            l1->decrement();
            l2->decrement();
            op1->release();
            op2->release();
            lisp->set_true_as_true();
            return e;
        }

        if (t1 == 3) {
            if (sx_1 != sx_2)
                throw new Error("Error: lists should have the same size for '.'");
                                
            if (l1 == l2) {
                if (l2->type == t_numbers) {
                    l2 = lisp->provideNumbers();
                    ((Numbers*)l2)->liste = ((Numbers*)l1)->liste;
                }
                else {
                    l2 = lisp->provideIntegers();
                    ((Integers*)l2)->liste = ((Integers*)l1)->liste;
                }
            }
            
            l1->increment();
            l2->increment();
            e = apply_op1_op2(lisp, op1, op2, l1, l2);
            l1->decrement();
            l2->decrement();
            op1->release();
            op2->release();
            lisp->set_true_as_true();
            return e;
        }

        if (sy_1 != sx_2)
            throw new Error("Error: incompatible matrices");

        Element* l2_transposed;
        long i, j = 0;
        
        l2_transposed = l2->transposed(lisp);
        
        Element* row;
        Matrice* res = new Matrice(lisp, sx_1, sy_2, 0.0);
        //We are dealing with matrices...
        for (i = 0; i < sx_1; i++) {
            row = l1->index(i);
            for (j = 0; j < sy_2; j++) {
                e = apply_op1_op2(lisp, op1, op2, row, l2_transposed->index(j));
                res->index(i)->replacing(j, e);
                e->release();
            }
        }
        l1->release();
        l2->release();
        op1->release();
        op2->release();
        l2_transposed->release();
        lisp->set_true_as_true();
        return res;
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        l1->release();
        l2->release();
        op1->release();
        op2->release();
        throw err;
    }

    return null_;
}

// (° '(2 3 4) '* '(1 2 3 4))
// (° (rho 2 3 '(4 5 6 9)) '* (rho 3 3 (iota 10)))
Element* List::evall_outerproduct(LispE* lisp) {
    //Operation is: (° operation l1 l2)    
    Element* l1 = null_;
    Element* l2 = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    lisp->set_true_as_one();

    try {
        l1 = liste[1]->eval(lisp);
        l2 = liste[3]->eval(lisp);
        
        if (!l1->isList() || !l2->isList())
            throw new Error("Error: arguments for '°' are lists");
        
        if (l1 == l2) {
            switch (l2->type) {
                case t_floats:
                    l2 = lisp->provideFloats();
                    ((Floats*)l2)->liste = ((Floats*)l1)->liste;
                    break;
                case t_numbers:
                    l2 = lisp->provideNumbers();
                    ((Numbers*)l2)->liste = ((Numbers*)l1)->liste;
                    break;
                case t_shorts:
                    l2 = new Shorts();
                    ((Shorts*)l2)->liste = ((Shorts*)l1)->liste;
                    break;
                case t_integers:
                    l2 = lisp->provideIntegers();
                    ((Integers*)l2)->liste = ((Integers*)l1)->liste;
                    break;
                case t_strings:
                    l2 = lisp->provideStrings();
                    ((Strings*)l2)->liste = ((Strings*)l1)->liste;
                    break;
            }
        }

        op = liste[2]->eval(lisp);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        call = lisp->provideList();
        call->append(op);
        call->append(null_);
        call->append(null_);
        
        vecte<long> size;
        l1->getShape(size);
        l2->getShape(size);
        if (lisp->delegation->isNumberType(l1->type) && lisp->delegation->isNumberType(l2->type)) {
            if (size.size() == 2) {
                if ((l1->type == t_floats && l2->type == t_floats) ||
                    (l1->type == t_shorts && l2->type == t_shorts)) {
                    res = new Matrice_float(lisp, size[0], size[1], 0.0);
                    ((Matrice_float*)res)->combine(lisp, l1, l2, call);
                }
                else {
                    res = new Matrice(lisp, size[0], size[1], 0.0);
                    ((Matrice*)res)->combine(lisp, l1, l2, call);
                }
            }
            else {
                if ((l1->type == t_floats && l2->type == t_floats) ||
                    (l1->type == t_shorts && l2->type == t_shorts)) {
                    res = new Tenseur_float(lisp, size, zero_);
                    ((Tenseur_float*)res)->combine(lisp, l1, l2, call);
                }
                else {
                    res = new Tenseur(lisp, size, zero_);
                    ((Tenseur*)res)->combine(lisp, l1, l2, call);
                }
            }
        }
        else {
            res = lisp->provideList();
            vecte<long> shape;
            l1->getShape(shape);
            l2->getShape(shape);
            long idx = 0;
            ((List*)res)->build(lisp, shape, 0, res, l1, idx);
            ((List*)res)->combine(lisp, l1, l2, call);
        }
        
        call->rawrelease();
        l1->release();
        l2->release();
        op->release();
        lisp->set_true_as_true();
        return res;
    }
    catch (Error* err) {
        if (call != NULL)
            call->rawrelease();
        lisp->set_true_as_true();
        l1->release();
        l2->release();
        op->release();
        res->release();
        throw err;
    }

    return null_;
}

Element* List::evall_rank(LispE* lisp) {
    long listsz = liste.size();
    
    Element* element = null_;
    Element* e = null_;
    Element* lst = null_;
    vecte<long> positions;
    long i, v;
    
    try {
        element = liste[1]->eval(lisp);
        for (i = 2; i < listsz; i++) {
            e = liste[i]->eval(lisp);
            if (e->isNumber()) {
                v = e->asInteger();
                positions.push_back(v);
            }
            else
                positions.push_back(-1);
            _releasing(e);
        }
        
        lst = element->rank(lisp,positions);
        element->release();
        if (lst == NULL)
            return emptylist_;
    }
    catch (Error* err) {
        lst->release();
        element->release();
        e->release();
        throw err;
    }
    return lst;
}

Element* List::evall_irank(LispE* lisp) {
    long listsz = liste.size();
    
    Element* element = null_;
    Element* e = null_;
    Element* lst = null_;
    Rankloop* r = NULL;
    long i, v;
    
    try {
        element = liste[1]->eval(lisp);
        r = new Rankloop(lisp, (List*)element);
        
        for (i = 2; i < listsz; i++) {
            e = liste[i]->eval(lisp);
            if (e->isNumber()) {
                v = e->asInteger();
                r->positions.push_back(v);
            }
            else
                r->positions.push_back(-1);
            _releasing(e);
        }
        vecte<long> shape;
        element->getShape(shape);
        
        if (shape.size() < r->positions.size())
            throw new Error("Error: cannot loop with 'irank' with these indexes");
        
        if (shape.size() == r->positions.size()) {
            r->max_iterator = 1;
            r->last = true;
        }
        else
            r->max_iterator = shape[r->positions.size()];
    }
    catch (Error* err) {
        if (r != NULL)
            r->release();
        lst->release();
        element->release();
        e->release();
        throw err;
    }
    return r;
}

Element* List::evall_reduce(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* l1 = null_;
    Element* op = null_;
    long sz = 0;
    
    lisp->set_true_as_one();
    
    try {
        if (listsz == 2) {
            //This is a copy
            l1 = liste[1]->eval(lisp);
            op = l1->fullcopy();
            if (op != l1)
                l1->release();
            return op;
        }
        
        l1 = liste[2]->eval(lisp);
        
        if (!l1->isList())
            throw new Error("Error: arguments for '//' is a list");
        
        op = liste[1]->eval(lisp);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);

        sz = l1->size();
        if (!sz) {
            lisp->set_true_as_true();
            return null_;
        }
                
        if (op->isList() && op->size() && op->index(0)->type != l_lambda) {
            //this is a filter, the first list
            long j = 0;
            long nb;
            switch (l1->type) {
                case t_floats: {
                    Floats* res = lisp->provideFloats();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j++;
                            continue;
                        }
                        if (j == sz) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Floats*)l1)->liste[j]);
                            nb--;
                        }
                        j++;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_numbers: {
                    Numbers* res = lisp->provideNumbers();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j++;
                            continue;
                        }
                        if (j == sz) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Numbers*)l1)->liste[j]);
                            nb--;
                        }
                        j++;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_shorts: {
                    Shorts* res = new Shorts();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asShort();
                        if (!nb) {
                            j++;
                            continue;
                        }
                        if (j == sz) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Shorts*)l1)->liste[j]);
                            nb--;
                        }
                        j++;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_integers: {
                    Integers* res = lisp->provideIntegers();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j++;
                            continue;
                        }
                        if (j == sz) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Integers*)l1)->liste[j]);
                            nb--;
                        }
                        j++;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_strings: {
                    Strings* res = lisp->provideStrings();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j++;
                            continue;
                        }
                        if (j == sz) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Strings*)l1)->liste[j]);
                            nb--;
                        }
                        j++;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                default: {
                    List* res = lisp->provideList();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j++;
                            continue;
                        }
                        
                        if (j == sz) {
                            res->release();
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->append(l1->index(j));
                            nb--;
                        }
                        j++;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
            }
        }
        
        if (sz == 1)
            return l1->value_on_index(lisp, (long)0);
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        l1->release();
        op->release();
        throw err;
    }

    List* call = lisp->provideList();
    call->append(op);
    methodEval met = lisp->delegation->evals[op->type];

    if (op->isOperator()) {
        call->append(lisp->provideQuoted(l1));
        
        try {
            l1 = (call->*met)(lisp);
        }
        catch(Error* err) {
            lisp->set_true_as_true();
            call->release();
            throw err;
        }
        
        lisp->set_true_as_true();
        call->release();
        return l1;
    }
    
    
    call->append(l1->value_on_index(lisp, (long)0)->quoting(lisp));
    call->append(l1->index(1)->quoting(lisp));
    Element* e = null_;
    try {
        e = (call->*met)(lisp)->quoting(lisp);
        call->replacing(1, e);
        for (long i = 2; i < l1->size(); i++) {
            call->replacing(2, l1->index(i)->quoting(lisp));
            e = (call->*met)(lisp)->quoting(lisp);
            call->replacing(1, e);
        }
    }
    catch(Error* err) {
        call->release();
        l1->release();
        throw err;
    }
    
    call->release(e);
    l1->release();
    return e->release_but_last();
}


inline long mmin(long x, long y) {
    return (x <= y?x:y);
}

Element* List::evall_backreduce(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (//- operation l1)
    
    Element* l1 = null_;
    Element* op = null_;
    long sz = 0;
    
    lisp->set_true_as_one();
    
    try {
        if (listsz == 2) {
            //This is a copy
            l1 = liste[1]->eval(lisp);
            op = l1->reverse(lisp);
            if (op != l1)
                l1->release();
            return op;
        }
        
        l1 = liste[2]->eval(lisp);
        
        if (!l1->isList())
            throw new Error("Error: arguments for '//-' is a list");
        
        op = liste[1]->eval(lisp);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);

        sz = l1->size();
        if (!sz) {
            lisp->set_true_as_true();
            return null_;
        }
                

        if (op->isList() && op->size() && op->index(0)->type != l_lambda) {
            //this is a filter, the first list
            long j = sz - 1;
            long nb;
            switch (l1->type) {
                case t_floats: {
                    Floats* res = lisp->provideFloats();
                    for (long i = op->size() - 1; i >= 0; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j--;
                            continue;
                        }
                        if (j == -1) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Floats*)l1)->liste[j]);
                            nb--;
                        }
                        j--;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_numbers: {
                    Numbers* res = lisp->provideNumbers();
                    for (long i = op->size() - 1; i >= 0; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j--;
                            continue;
                        }
                        if (j == -1) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Numbers*)l1)->liste[j]);
                            nb--;
                        }
                        j--;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_shorts: {
                    Shorts* res = new Shorts();
                    for (long i = op->size() - 1; i >= 0; i--) {
                        nb = op->index(i)->asShort();
                        if (!nb) {
                            j--;
                            continue;
                        }
                        if (j == -1) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Shorts*)l1)->liste[j]);
                            nb--;
                        }
                        j--;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_integers: {
                    Integers* res = lisp->provideIntegers();
                    for (long i = op->size() - 1; i >= 0; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j--;
                            continue;
                        }
                        if (j == -1) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Integers*)l1)->liste[j]);
                            nb--;
                        }
                        j--;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                case t_strings: {
                    Strings* res = lisp->provideStrings();
                    for (long i = op->size() - 1; i >= 0; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j--;
                            continue;
                        }
                        if (j == -1) {
                            delete res;
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->liste.push_back(((Strings*)l1)->liste[j]);
                            nb--;
                        }
                        j--;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
                default: {
                    List* res = lisp->provideList();
                    for (long i = op->size() - 1; i >= 0; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            j--;
                            continue;
                        }
                        if (j == -1) {
                            res->release();
                            throw new Error("Error: List size mismatch");
                        }
                        while (nb) {
                            res->append(l1->index(j));
                            nb--;
                        }
                        j--;
                    }
                    lisp->set_true_as_true();
                    op->release();
                    l1->release();
                    return res;
                }
            }
        }
        
        if (sz == 1)
            return l1->value_on_index(lisp, (long)0);
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        l1->release();
        op->release();
        throw err;
    }

    List* call = lisp->provideList();
    call->append(op);
    methodEval met = lisp->delegation->evals[op->type];

    if (op->isOperator()) {
        call->append(lisp->provideQuoted(l1->reverse(lisp)));
        Element* e;
        try {
            e = (call->*met)(lisp);
        }
        catch(Error* err) {
            lisp->set_true_as_true();
            call->release();
            throw err;
        }
        
        lisp->set_true_as_true();
        call->release();
        l1->release();
        return e;
    }

    Element* e = l1->reverse(lisp);
    l1->release();
    l1 = e;
    call->append(l1->value_on_index(lisp, (long)0)->quoting(lisp));
    call->append(l1->index(1)->quoting(lisp));
    e = null_;
    try {
        e = (call->*met)(lisp)->quoting(lisp);
        call->replacing(1, e);
        for (long i = 2; i < l1->size(); i++) {
            call->replacing(2, l1->index(i)->quoting(lisp));
            e = (call->*met)(lisp)->quoting(lisp);
            call->replacing(1, e);
        }
    }
    catch(Error* err) {
        call->release();
        l1->release();
        throw err;
    }
    
    call->release(e);
    l1->release();
    return e->release_but_last();
}

// (, (rho 2 3 '(4 5 6 9)) (rho 2 3 (iota 10)))
// (, (rho 3 3 3 (iota 90)) -1)
// (, (rho 3 3 3 (iota 90)) (* (iota 3) -1))
// (, (rho 3 3 3 (iota 90)) (* (rho 3 3 (iota 10)) -1))

Element* List::evall_concatenate(LispE* lisp) {
    long listsize = size();
    Element* first_element = null_;
    Element* second_element = null_;
    Element* res = null_;
    lisp->set_true_as_one();
    
    try {
        first_element = liste[1]->eval(lisp);
        if (listsize == 2) {
            lisp->set_true_as_true();
            
            if (first_element->isValueList()) {
                lisp->set_true_as_true();
                return first_element;
            }
            switch (first_element->type) {
                case t_matrix:
                case t_tensor: {
                    Numbers* l = lisp->provideNumbers();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->set_true_as_true();
                    return l;
                }
                case t_matrix_float:
                case t_tensor_float: {
                    Floats* l = lisp->provideFloats();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->set_true_as_true();
                    return l;
                }
                default: {
                    res = lisp->provideList();
                    first_element->flatten(lisp,(List*)res);
                    first_element->release();
                    lisp->set_true_as_true();
                    return res;
                }
            }
        }

        if (!first_element->isList()) {
            switch (first_element->type) {
                case t_integer:
                    second_element = lisp->provideIntegers();
                    break;
                case t_float:
                    second_element = lisp->provideFloats();
                    break;
                case t_number:
                    second_element = lisp->provideNumbers();
                    break;
                case t_string:
                    second_element = lisp->provideStrings();
                    break;
                default:
                    second_element = lisp->provideList();
            }
            second_element->append(first_element);
            first_element->release();
            first_element = second_element;
        }
        
        second_element = liste[2]->eval(lisp);
        
        vecte<long> sz1;
        vecte<long> sz2;
        switch (first_element->type) {
            case t_matrix: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice(lisp, sz1[0], sz1[1], 0.0);
                ((Matrice*)res)->setvalue((Matrice*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_matrix_float: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_float(lisp, sz1[0], sz1[1], 0.0);
                ((Matrice_float*)res)->setvalue((Matrice_float*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_float*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_tensor: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur(lisp, sz1, zero_);
                ((Tenseur*)res)->setvalue((Tenseur*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            case t_tensor_float: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_float(lisp, sz1, zero_);
                ((Tenseur_float*)res)->setvalue((Tenseur_float*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_float*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_float*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            default:
                res = first_element->duplicate();
                res->concatenate(lisp, second_element);
                if (res != first_element)
                    first_element->release();
        }

        lisp->set_true_as_true();
        second_element->release();
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        res->release();
        second_element->release();
        first_element->release();
        throw err;
    }
    
    return res;
}

Element* List::evall_member(LispE* lisp) {
    Element* element = liste[1];
    Element* the_set = liste[1];
    Element* result = liste[1];
    lisp->set_true_as_one();
    
    try {
        element = element->eval(lisp);
        if (!element->isList())
            throw new Error("Error: expecting a list as first element");
        the_set = liste[2]->eval(lisp);
        if (!the_set->isList())
            throw new Error("Error: expecting a list as first element");
        result = element->check_member(lisp, the_set);
        the_set->release();
        element->release();
        lisp->set_true_as_true();
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        element->release();
        the_set->release();
        throw err;
    }
    return result;
}

Element* List::evall_rho(LispE* lisp) {
    long listsize =  size();
    
    Element* e =  null_;
    Element* res = null_;
    lisp->set_true_as_one();

    try {
        if (listsize == 2) {
            //In this case we return the shape of our list
            e = liste[1]->eval(lisp);
            switch (e->type) {
                case t_matrix: {
                    res = lisp->provideIntegers();
                    ((Integers*)res)->liste.push_back(((Matrice*)e)->size_x);
                    ((Integers*)res)->liste.push_back(((Matrice*)e)->size_y);
                    e->release();
                    lisp->set_true_as_true();
                    return res;
                }
                case t_matrix_float: {
                    res = lisp->provideIntegers();
                    ((Integers*)res)->liste.push_back(((Matrice_float*)e)->size_x);
                    ((Integers*)res)->liste.push_back(((Matrice_float*)e)->size_y);
                    e->release();
                    lisp->set_true_as_true();
                    return res;
                }
                case t_tensor: {
                    res = lisp->provideIntegers();
                    ((Integers*)res)->liste = ((Tenseur*)e)->shape;
                    e->release();
                    lisp->set_true_as_true();
                    return res;
                }
                case t_tensor_float: {
                    res = lisp->provideIntegers();
                    ((Integers*)res)->liste = ((Tenseur_float*)e)->shape;
                    e->release();
                    lisp->set_true_as_true();
                    return res;
                }
                case t_strings:
                case t_floats:
                case t_numbers:
                case t_shorts:
                case t_integers:
                    listsize = e->size();
                    e->release();
                    lisp->set_true_as_true();
                    return lisp->provideInteger(listsize);
                case t_list:
                case t_llist: {
                    vecte<long> sizes;
                    e->getShape(sizes);
                    if (sizes.size() == 1) {
                        lisp->set_true_as_true();
                        return lisp->provideInteger(sizes[0]);
                    }
                    if (e->checkShape(0, sizes)) {
                        res = lisp->provideIntegers();
                        for (long i = 0; i < sizes.size(); i++) {
                            ((Integers*)res)->liste.push_back(sizes[i]);
                        }
                        lisp->set_true_as_true();
                        return res;
                    }
                    break;
                }
                default:
                    listsize = e->size();
                    e->release();
                    lisp->set_true_as_true();
                    return lisp->provideInteger(listsize);
            }
        }
        long ei = 0;
        long sz1;

        if (listsize == 3) {
            e = liste[2]->eval(lisp);
            if (!e->isList())
                throw new Error("Error: Second argument should be a list");
            evalAsInteger(1, lisp, sz1);
            switch (e->type) {
                case t_floats: {
                    listsize = e->size();
                    res = lisp->provideFloats();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        float v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asFloat();
                        for (long i = 0; i < sz1; i++) {
                            ((Floats*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Floats*)res)->liste.push_back(e->index(ei++)->asFloat());
                    }
                    break;
                }
                case t_numbers: {
                    listsize = e->size();
                    res = lisp->provideNumbers();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        double v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asNumber();
                        for (long i = 0; i < sz1; i++) {
                            ((Numbers*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Numbers*)res)->liste.push_back(e->index(ei++)->asNumber());
                    }
                    break;
                }
                case t_shorts: {
                    listsize = e->size();
                    res = new Shorts();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        short v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asShort();
                        for (long i = 0; i < sz1; i++) {
                            ((Shorts*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Shorts*)res)->liste.push_back(e->index(ei++)->asShort());
                    }
                    break;
                }
                case t_integers: {
                    listsize = e->size();
                    res = lisp->provideIntegers();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        long v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asInteger();
                        for (long i = 0; i < sz1; i++) {
                            ((Integers*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Integers*)res)->liste.push_back(e->index(ei++)->asInteger());
                    }
                    break;
                }
                case t_strings: {
                    listsize = e->size();
                    res = lisp->provideStrings();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        u_ustring v;
                        if (listsize == 1)
                            v = e->index(0)->asUString(lisp);
                        for (long i = 0; i < sz1; i++) {
                            ((Strings*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Strings*)res)->liste.push_back(e->index(ei++)->asUString(lisp));
                    }
                    break;
                }
                case t_list: {
                    listsize = e->size();
                    res = lisp->provideList();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        Element* v = zero_;
                        if (listsize == 1)
                            v = e->index(0);
                        for (long i = 0; i < sz1; i++)
                            res->append(v);
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        res->append(e->index(ei++));
                    }
                    break;
                }
                case t_llist: {
                    res = new LList(&lisp->delegation->mark);
                    u_link* u = ((LList*)e)->liste.begin();
                    if (u == NULL) {
                        Element* v = zero_;
                        for (long i = 0; i < sz1; i++)
                            ((LList*)res)->push_front(v);
                        break;
                    }
                    else {
                        if (u->_next == NULL) {
                            Element* v = u->value->copying(false);
                            for (long i = 0; i < sz1; i++)
                                ((LList*)res)->push_front(v);
                        }
                        else {
                            for (long i = 0; i < sz1; i++) {
                                if (u == NULL)
                                    u = ((LList*)e)->liste.begin();
                                ((LList*)res)->push_front(u->value->copying(false));
                                u = u->next();
                            }
                        }
                    }
                }
            }
            e->release();
            lisp->set_true_as_true();
            return res;
        }
        
        if (listsize == 4) {
            long sz2;
            e = liste[3]->eval(lisp);
            if (!e->isList())
                throw new Error("Error: third argument should be a list");
            
            evalAsInteger(1, lisp, sz1);
            evalAsInteger(2, lisp, sz2);
            switch (e->type) {
                case t_shorts:
                case t_floats:
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideFloats(1, 0);
                    }
                    res = new Matrice_float(lisp, e, sz1, sz2);
                    break;
                case t_numbers:
                case t_integers:
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideIntegers(1, 0);
                    }
                    res = new Matrice(lisp, e, sz1, sz2);
                    break;
                case t_strings: {
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideStrings();
                        e->append(emptystring_);
                    }
                    vecte<long> shape;
                    shape.push_back(sz1);
                    shape.push_back(sz2);
                    res = new List;
                    sz1 = 0;
                    ((List*)res)->build(lisp,shape, 0,res, e, sz1);
                    break;
                }
                case t_list: {
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideIntegers(1, 0);
                        res = new Matrice(lisp, e, sz1, sz2);
                        break;
                    }
                    else {
                        vecte<long> shape;
                        shape.push_back(sz1);
                        shape.push_back(sz2);
                        res = new List;
                        sz1 = 0;
                        ((List*)res)->build(lisp,shape, 0,res, e, sz1);
                    }
                    break;
                }
                case t_llist: {
                    vecte<long> shape;
                    shape.push_back(sz1);
                    shape.push_back(sz2);
                    res = new LList(&lisp->delegation->mark);
                    u_link* u = ((LList*)e)->liste.begin();
                    if (u == NULL) {
                        e->release();
                        e = new LList(&lisp->delegation->mark);
                        e->append(zero_);
                        u = ((LList*)e)->liste.begin();
                    }
                    ((LList*)res)->build(lisp,shape, 0, (LList*)res, (LList*)e, &u);
                    break;
                }
            }
            
            e->release();
            lisp->set_true_as_true();
            return res;
        }

        vecte<long> shape;
        long idx;
        listsize--;
        for (long i = 1; i < listsize; i++) {
            evalAsInteger(i, lisp, idx);
            shape.push_back(idx);
        }
        e = liste[listsize]->eval(lisp);
        if (!e->isList())
            throw new Error("Error: last argument should be a list");

        switch (e->type) {
            case t_shorts:
            case t_floats:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideFloats(1, 0);
                }
                res = new Tenseur_float(lisp, e, shape);
                break;
            case t_numbers:
            case t_integers:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideIntegers(1, 0);
                }
                res = new Tenseur(lisp, e, shape);
                break;
            case t_strings:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideStrings();
                    e->append(emptystring_);
                }
                else {
                    res = new List;
                    idx = 0;
                    ((List*)res)->build(lisp,shape, 0,res, e, idx);
                }
                break;
            case t_list: {
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideIntegers(1, 0);
                    res = new Tenseur(lisp, e, shape);
                }
                else {
                    res = new List;
                    idx = 0;
                    ((List*)res)->build(lisp,shape, 0,res, e, idx);
                }
                break;
            }
            case t_llist: {
                res = new LList(&lisp->delegation->mark);
                u_link* u = ((LList*)e)->liste.begin();
                if (u == NULL) {
                    e->release();
                    e = new LList(&lisp->delegation->mark);
                    e->append(zero_);
                    u = ((LList*)e)->liste.begin();
                }
                ((LList*)res)->build(lisp,shape, 0, (LList*)res, (LList*)e, &u);
                break;
            }
        }

        e->release();
        lisp->set_true_as_true();
    }
    catch (Error* err) {
        e->release();
        res->release();
        lisp->set_true_as_true();
        throw err;
    }
    return res;
}

Element* List::evall_equalonezero(LispE* lisp) {
    Element* l1 = null_;
    Element* l2 = null_;
    Integers* res = NULL;
    
    try {
        l1 = liste[1]->eval(lisp);
        l2 = liste[2]->eval(lisp);
        
        if (!l1->isList() || !l2->isList()) {
            bool test = l1->isequal(lisp, l2);
            l1->release();
            l2->release();
            return numbools_[test];
        }
        
        res = lisp->provideIntegers();
        for (long i = 0; i < l1->size() && i < l2->size(); i++) {
            if (l1->index(i)->isequal(lisp, l2->index(i)))
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        
        l1->release();
        l2->release();
    }
    catch (Error* err) {
        l1->release();
        l2->release();
        if (res != NULL)
            res->release();
        throw err;
    }
    
    return res;
}

Element* List::evall_scan(LispE* lisp) {
    //Operation is: (° operation l1 l2)
    
    Element* l1 = null_;
    Element* op = null_;
    long sz = 0;
    
    lisp->set_true_as_one();

    try {
        l1 = liste[2]->eval(lisp);
        
        if (!l1->isList())
            throw new Error("Error: arguments for '\\\\' is a list");
        
        op = liste[1]->eval(lisp);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);

        sz = l1->size();
        if (!sz) {
            lisp->set_true_as_true();
            return null_;
        }
                
        if (op->isList() && op->size() && op->index(0)->type != l_lambda) {
            //this is a filter, the first list
            long j = 0;
            long nb;
            switch (l1->type) {
                case t_floats: {
                    Floats* res = lisp->provideFloats();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == sz) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Floats*)l1)->liste[j]);
                                nb--;
                            }
                            j++;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != sz) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                 return res;
                }
                case t_numbers: {
                    Numbers* res = lisp->provideNumbers();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == sz) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Numbers*)l1)->liste[j]);
                                nb--;
                            }
                            j++;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != sz) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                 return res;
                }
                case t_shorts: {
                    Shorts* res = new Shorts();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asShort();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == sz) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Shorts*)l1)->liste[j]);
                                nb--;
                            }
                            j++;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != sz) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                case t_integers: {
                    Integers* res = lisp->provideIntegers();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == sz) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Integers*)l1)->liste[j]);
                                nb--;
                            }
                            j++;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != sz) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                case t_strings: {
                    Strings* res = lisp->provideStrings();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(U"");
                        }
                        else {
                            if (j == sz) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Strings*)l1)->liste[j]);
                                nb--;
                            }
                            j++;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != sz) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                default: {
                    List* res = lisp->provideList();
                    for (long i = 0; i < op->size(); i++) {
                        nb = op->index(i)->asInteger();
                        if (!nb)
                            res->append(zero_);
                        else {
                            if (j == sz) {
                                res->release();
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->append(l1->index(j));
                                nb--;
                            }
                            j++;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != sz) {
                        res->release();
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                  return res;
                }
            }
        }
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        l1->release();
        op->release();
        throw err;
    }

    List* res = lisp->provideList();
    List* call = lisp->provideList();

    try {
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        bool monadic = op->check_arity(lisp, P_TWO);
        
        call->append(op);
        call->append(null_);
        Element* e;
        call->liste[1] = l1->value_on_index(lisp, (long)0);
        res->append(call->liste[1]);
        methodEval met = lisp->delegation->evals[op->type];
        if (!monadic) {
            call->append(null_);
            for (long i = 1; i < sz; i++) {
                call->liste[2] = l1->index(i);
                e = (call->*met)(lisp);
                res->append(e);
                call->liste[1] = e;
            }
        }
        else {
            for (long i = 1; i < sz; i++) {
                call->liste[1] = l1->index(i);
                e = (call->*met)(lisp);
                res->append(e);
            }
        }
        
        call->rawrelease();
        l1->release();
        op->release();
        lisp->set_true_as_true();
    }
    catch (Error* err) {
        call->rawrelease();
        lisp->set_true_as_true();
        res->release();
        l1->release();
        op->release();
        throw err;
    }
    
    return res;
}

Element* List::evall_backscan(LispE* lisp) {
    //Operation is: (° operation l1 l2)
    
    Element* l1 = null_;
    Element* op = null_;
    long sz = 0;
    
    lisp->set_true_as_one();

    try {
        l1 = liste[2]->eval(lisp);
        
        if (!l1->isList())
            throw new Error("Error: arguments for '\\\\' is a list");
        
        op = liste[1]->eval(lisp);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);

        sz = l1->size();
        if (!sz) {
            lisp->set_true_as_true();
            return null_;
        }
                
        if (op->isList() && op->size() && op->index(0)->type != l_lambda) {
            //this is a filter, the first list
            long j = sz - 1;
            long nb;
            switch (l1->type) {
                case t_floats: {
                    Floats* res = lisp->provideFloats();
                    for (long i = op->size() - 1; i >= 0 ; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == -1) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Floats*)l1)->liste[j]);
                                nb--;
                            }
                            j--;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != -1) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                case t_numbers: {
                    Numbers* res = lisp->provideNumbers();
                    for (long i = op->size() - 1; i >= 0 ; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == -1) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Numbers*)l1)->liste[j]);
                                nb--;
                            }
                            j--;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != -1) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                case t_shorts: {
                    Shorts* res = new Shorts();
                    for (long i = op->size() - 1; i >= 0 ; i--) {
                        nb = op->index(i)->asShort();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == -1) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Shorts*)l1)->liste[j]);
                                nb--;
                            }
                            j--;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != -1) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                case t_integers: {
                    Integers* res = lisp->provideIntegers();
                    for (long i = op->size() - 1; i >= 0 ; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(0);
                        }
                        else {
                            if (j == -1) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Integers*)l1)->liste[j]);
                                nb--;
                            }
                            j--;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != -1) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                    return res;
                }
                case t_strings: {
                    Strings* res = lisp->provideStrings();
                    for (long i = op->size() - 1; i >= 0 ; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb) {
                            res->liste.push_back(U"");
                        }
                        else {
                            if (j == -1) {
                                delete res;
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->liste.push_back(((Strings*)l1)->liste[j]);
                                nb--;
                            }
                            j--;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != -1) {
                        delete res;
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                  return res;
                }
                default: {
                    List* res = lisp->provideList();
                    for (long i = op->size() - 1; i >= 0 ; i--) {
                        nb = op->index(i)->asInteger();
                        if (!nb)
                            res->append(zero_);
                        else {
                            if (j == -1) {
                                res->release();
                                throw new Error("Error: List size mismatch");
                            }
                            while (nb) {
                                res->append(l1->index(j));
                                nb--;
                            }
                            j--;
                        }
                    }
                    lisp->set_true_as_true();
                    if (j != -1) {
                        res->release();
                        throw new Error("Error: List size mismatch");
                    }
                    op->release();
                    l1->release();
                 return res;
                }
            }
        }
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        l1->release();
        op->release();
        throw err;
    }

    List* call = lisp->provideList();
    call->append(op);
    call->append(null_);
    List* res = lisp->provideList();

    try {
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        bool monadic = op->check_arity(lisp, P_TWO);

        Element* e;
        sz--;
        call->liste[1] = l1->value_on_index(lisp, sz);
        res->append(call->liste[1]);
        methodEval met = lisp->delegation->evals[op->type];
        if (!monadic) {
            call->append(null_);
            for (long i = sz-1; i >= 0; i--) {
                call->liste[2] = l1->index(i);
                e = (call->*met)(lisp);
                res->append(e);
                call->liste[1] = e;
            }
        }
        else {
            for (long i = sz-1; i >= 0; i--) {
                call->liste[1] = l1->index(i);
                e = (call->*met)(lisp);
                res->append(e);
            }
        }
        
        call->rawrelease();
        l1->release();
        op->release();
        lisp->set_true_as_true();
    }
    catch (Error* err) {
        call->rawrelease();
        res->release();
        lisp->set_true_as_true();
        l1->release();
        op->release();
        throw err;
    }
    return res;
}

Element* List::evall_catch(LispE* lisp) {
    short listsize = liste.size();
    Element* element = null_;

    
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
    Element* lst = null_;
    Element* c = null_;
    
    try {
        lst = liste[1]->eval(lisp);
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
        c->release();
        throw err;
    }
    return c;
}

Element* List::evall_check(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    bool test = true;


    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        _releasing(first_element);
        
        if (!test)
            return null_;

        second_element = null_;
        liste.back()->setterminal(terminal);
        for (long i = 2; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_checking(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;

    List* call = lisp->provideList();

    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        if (first_element->isInstruction()) {
            call->liste.push_element(first_element);
            call->liste.push_element(liste[3]);
            call->liste.push_element(second_element);
        }
        else {
            if (second_element->isInstruction()) {
                call->liste.push_element(second_element);
                call->liste.push_element(first_element);
                call->liste.push_element(liste[3]);
            }
            else
                throw new Error("Error: condition or operation cannot be evaluated: missing instruction");
        }
        third_element = call->eval(lisp);
        call->rawrelease();
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        call->rawrelease();
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return third_element;
}


Element* List::evall_compose(LispE* lisp) {
    Element* values = null_;
    List* loop = (List*)liste.back();
    short i = 4;
    short listsize = liste.size()-1;
    short label;
    
	bool nxt = lisp->delegation->next_stop;

    lisp->set_true_as_one();
    
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
        
    try {
        for (; i < listsize; i++) {
            liste[i]->eval(lisp);
        }
        
        values = loop->liste[2]->eval(lisp);
        values->loop(lisp, loop->liste[1]->label(), loop);
    }
    catch (Error* err) {
        lisp->set_true_as_true();
        throw err;
    }

    lisp->set_true_as_true();
	lisp->delegation->next_stop = nxt;
    values->release();
    return lisp->get(label);
}


Element* List::evall_cond(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


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
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        //merging an element into the next list
        first_element = liste[1]->eval(lisp);
        if (first_element == emptylist_)
            first_element = null_;

        second_element = liste[2]->eval(lisp);
        if (second_element == null_ || second_element == emptylist_) {
            third_element = lisp->provideList();
            third_element->append(first_element);
            return third_element;
        }

        if (!second_element->isList()) {
            third_element = new Pair();
            third_element->append(first_element);
            third_element->append(second_element);
            return third_element;
        }

        switch (second_element->type) {
            case t_pair: {
                third_element = new Pair();
                third_element->append(first_element);
                for (long i = 0; i < second_element->size(); i++)
                    third_element->append(second_element->value_on_index(lisp, i));
                break;
            }
            case t_llist: {
                third_element = new LList(&lisp->delegation->mark);
                LList* lst = (LList*)second_element;
                for (u_link* e = lst->liste.last(); e != NULL; e = e->previous())
                    ((LList*)third_element)->push_front(e->value->copying(false));
                ((LList*)third_element)->push_front(first_element);
                break;
            }
            default: {
                third_element = lisp->provideList();
                third_element->append(first_element);
                for (long i = 0; i < second_element->size(); i++)
                    third_element->append(second_element->value_on_index(lisp, i));
            }
        }
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return third_element;
}


Element* List::evall_consp(LispE* lisp) {
    Element* second_element = null_;
    bool test = true;


    try {
        second_element = liste[1]->eval(lisp);
        test = second_element->isList();
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}

Element* List::evall_cyclicp(LispE* lisp) {
    Element* second_element = null_;
    bool test = true;


    try {
        second_element = liste[1]->eval(lisp);
        test = second_element->is_cyclic();
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}


Element* List::evall_clone(LispE* lisp) {
    Element* element = null_;
    Element* res;


    try {
        element = liste[1]->eval(lisp);
        //This is to be able to use lists from provideList
        if (element->type == t_list)
            res = element->asList(lisp);
        else
            res = element->fullcopy();
        element->release();
    }
    catch (Error* err) {
        element->release();
        throw err;
    }
    return res;
}

Element* List::evall_converttoatom(LispE* lisp) {

    u_ustring tampon;
    evalAsUString(1,lisp,tampon);
    long sz = tampon.size();
    if (tampon[0] == 'c' && tampon.back() == 'r' && sz > 3) {
        // we check if we don't have a variation on car/cdr/cadr/caar etc...
        sz-=2;
        while (sz != 1) {
            if (tampon[sz] != 'a' && tampon[sz] != 'd')
                return lisp->provideAtomProtected(tampon);
            sz--;
        }
        return  lisp->provideCADR(tampon);
    }

    return lisp->provideAtomProtected(tampon);
}


Element* List::evall_converttoshort(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = new Short(second_element->asShort());
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_converttointeger(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideInteger(second_element->asInteger());
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_converttofloat(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideFloat(second_element->asFloat());
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_converttonumber(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideNumber(second_element->asNumber());
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_converttostring(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        u_ustring strvalue = second_element->asUString(lisp);
        first_element = lisp->provideString(strvalue);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_data(LispE* lisp) {
    //if the function was created on the fly, we need to store its contents
    //in the garbage
    if (!is_protected()) {
        lisp->garbaging(this);
        garbaging_values(lisp);
    }

    short listsize = liste.size();
    if (listsize < 2)
        throw new Error("Error: wrong number of arguments");
    Element* second_element = null_;

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
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return true_;
}


Element* List::evall_deflib(LispE* lisp) {
    // we manage extensions to the language with deflib (see systeme.cxx for an example)
    if (liste._size() != 3)
        throw new Error("Error: wrong number of arguments");
    
    short label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    if (lisp->globalDeclaration()) {
        if (!lisp->delegation->recordingFunction(this, label)) {
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
    
    short label;
    
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
    short label = liste[1]->label();
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
    
    short label;
    
    //We declare a function
    label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");
    return lisp->recordingMethod(this, label);
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
    short label = liste[1]->label();
    if (label == v_null)
        throw new Error(L"Error: Missing name in the declaration of a function");
    if (!liste[2]->isList())
        throw new Error(L"Error: List of missing parameters in a function declaration");

    if (lisp->globalDeclaration()) {
        if (!lisp->delegation->recordingFunction(this, label)) {
            wstring nm =L"Error: Function '";
            nm += lisp->asString(label);
            nm += L"' already declared";
            throw new Error(nm);
        }
        return this;
    }
    return lisp->recordingunique(this, label);
}

Element* List::evall_bodies(LispE* lisp) {
    Element* function = liste[1]->eval(lisp);
    if (function->protected_index(lisp, (long)0)->type == l_defpat) {
        List* functions =  lisp->provideList();
        short label = function->protected_index(lisp, (long)1)->label();
        try {
            for (auto& a: lisp->delegation->method_pool.at(label)) {
                for (auto& b : a.second) {
                    functions->append(b);
                }
            }
            return functions;
        }
        catch (...) {
            return function;
        }
    }
    return function;
}


Element* List::evall_different(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        test = first_element->isequal(lisp, second_element);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return booleans_[!test];
}

Element* List::evall_eq(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    bool test = true;


    try {
        if (listsize == 3) {
            first_element = liste[1]->eval(lisp);
            second_element = liste[2]->eval(lisp);
            test = ( (first_element == second_element) || first_element->egal(second_element));
            first_element->release();
            second_element->release();
            return booleans_[test];
        }
        
        listsize--;
        for (long i = 1; i < listsize && test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = ( (first_element == second_element) || first_element->egal(second_element));
            _releasing(first_element);
            _releasing(second_element);
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return booleans_[test];
}


Element* List::evall_equal(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    char test = true;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        test = first_element->isequal(lisp, second_element);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return booleans_[test];
}


Element* List::evall_eval(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);

        //This is a specific case, when the element is a string
        //we need then to call the a meta-eval, the one that
        //comes with Lisp itself
        if (second_element->isString()) {
            first_element = lisp->eval(second_element->toString(lisp));
            _releasing(second_element);
            if (first_element->isError())
                throw new Error(first_element->asUString(lisp));
            return first_element;
        }
        
        if (!second_element->size())
            return second_element;
        
        //We just need to evaluate this element...
        lisp->evaluating = true;
        first_element = second_element->eval(lisp);
        lisp->evaluating = false;
        if (first_element != second_element) {
            if (second_element->element_container()) {
                first_element->increment();
                second_element->release();
                first_element->decrementkeep();
            }
            else
                second_element->release();
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        lisp->evaluating = false;
        throw err;
    }

    return first_element;
}


Element* List::evall_extract(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = second_element->extraction(lisp, this);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_set_range(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    short label = liste[1]->label();


    try {
        second_element = liste[1]->eval(lisp);
        first_element = second_element->replace_in(lisp, this);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    if (label > l_final)
        return lisp->recording_variable(first_element, label);
    return first_element;
}


Element* List::evall_flatten(LispE* lisp) {
    Element* element = null_;
    
    try {
        element = liste[1]->eval(lisp);
        if (element->isValueList())
            return element;
        
        if (element->type == t_matrix || element->type == t_tensor) {
            Numbers* l = lisp->provideNumbers();
            element->flatten(lisp, l);
            element->release();
            return l;
        }

        if (element->type == t_matrix_float || element->type == t_tensor_float) {
            Floats* l = lisp->provideFloats();
            element->flatten(lisp, l);
            element->release();
            return l;
        }

        List* l = lisp->provideList();
        element->flatten(lisp,l);
        element->release();
        return l;
    }
    catch (Error* err) {
        element->release();
        throw err;
    }
    
    return null_;
}

Element* List::evall_fappend(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        string chemin = first_element->toString(lisp);
        //We put ourselves in append mode
        std::ofstream o(chemin.c_str(), std::ios::binary|std::ios::app);
        if (o.fail()) {
            string erreur = "Error: Cannot write in file: ";
            erreur += chemin;
            throw new Error(erreur);
        }
        chemin = second_element->toString(lisp);
        o << chemin;
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return true_;
}


Element* List::evall_flip(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;

    List* call = NULL;

    try {
        if (liste[1]->isList() && liste[1]->size() >= 3) {
            call = lisp->provideList();
            List* l = (List*)liste[1];
            //We reverse the two first arguments
            call->liste.push_element(l->liste[0]);
            call->liste.push_element(l->liste[2]);
            call->liste.push_element(l->liste[1]);
            for (long i = 3; i < liste[1]->size(); i++) {
                call->liste.push_element(l->liste[i]);
            }
            first_element =  call->eval(lisp);
            call->rawrelease();
            return first_element;
        }
        first_element = liste[1]->eval(lisp);
        switch (first_element->type) {
            case t_floats: {
                long listsize = first_element->size();
                if (listsize < 2)
                    return first_element;
                second_element = lisp->provideFloats();
                ((Floats*)second_element)->liste = ((Floats*)first_element)->liste;
                ((Floats*)second_element)->liste[0] = ((Floats*)first_element)->liste[1];
                ((Floats*)second_element)->liste[1] = ((Floats*)first_element)->liste[0];
                first_element->release();
                return second_element;
            }
            case t_numbers: {
                long listsize = first_element->size();
                if (listsize < 2)
                    return first_element;
                second_element = lisp->provideNumbers();
                ((Numbers*)second_element)->liste = ((Numbers*)first_element)->liste;
                ((Numbers*)second_element)->liste[0] = ((Numbers*)first_element)->liste[1];
                ((Numbers*)second_element)->liste[1] = ((Numbers*)first_element)->liste[0];
                first_element->release();
                return second_element;
            }
            case t_shorts: {
                long listsize = first_element->size();
                if (listsize < 2)
                    return first_element;
                second_element = new Shorts();
                ((Shorts*)second_element)->liste = ((Shorts*)first_element)->liste;
                ((Shorts*)second_element)->liste[0] = ((Shorts*)first_element)->liste[1];
                ((Shorts*)second_element)->liste[1] = ((Shorts*)first_element)->liste[0];
                first_element->release();
                return second_element;
            }
            case t_integers: {
                long listsize = first_element->size();
                if (listsize < 2)
                    return first_element;
                second_element = lisp->provideIntegers();
                ((Integers*)second_element)->liste = ((Integers*)first_element)->liste;
                ((Integers*)second_element)->liste[0] = ((Integers*)first_element)->liste[1];
                ((Integers*)second_element)->liste[1] = ((Integers*)first_element)->liste[0];
                first_element->release();
                return second_element;
            }
            case t_strings: {
                long listsize = first_element->size();
                if (listsize < 2)
                    return first_element;
                second_element = lisp->provideStrings();
                ((Strings*)second_element)->liste = ((Strings*)first_element)->liste;
                ((Strings*)second_element)->liste[0] = ((Strings*)first_element)->liste[1];
                ((Strings*)second_element)->liste[1] = ((Strings*)first_element)->liste[0];
                first_element->release();
                return second_element;
            }
            case t_list: {
                long listsize = first_element->size();
                if (listsize < 2)
                    return first_element;
                second_element = lisp->provideList();
                second_element->append(first_element->index(1));
                second_element->append(first_element->index(0));
                for (long i = 2; i < listsize; i++) {
                    second_element->append(first_element->index(i));
                }
                first_element->release();
                return second_element;
            }
            case t_llist: {
                LList* elements = (LList*)first_element;
                if (!elements->atleast2())
                    return first_element;
                LList* l = (LList*)elements->copying(false);
                second_element = l->liste.first->value;
                l->liste.first->value = l->liste.first->_next->value;
                l->liste.first->_next->value = second_element;
                return l;
            }
            case t_dictionary:
            case t_dictionaryi:
            case t_dictionaryn: {
                second_element = first_element->reverse(lisp, true);
                first_element->release();
                return second_element;
            }
            default:
                if (first_element->isList()) {
                    long listsize = first_element->size();
                    if (listsize < 2)
                        return first_element;
                    second_element = lisp->provideList();
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
        }
    }
    catch (Error* err) {
        if (call != NULL) {
            call->rawrelease();
        }
        first_element->release();
        second_element->release();
        throw err;
    }

    throw new Error("Error: Cannot apply flip on this structure");
}


Element* List::evall_folding(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;

    List* call = NULL;
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
        call = lisp->provideList();
        call->liste.push_element(first_element);
        call->liste.push_element(liste[2]);
        call->liste.push_element(liste[3]);
        second_element = call->eval(lisp);
        call->rawrelease();
        first_element->release();
    }
    catch (Error* err) {
        if (call != NULL) {
            call->rawrelease();
        }
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_fread(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        u_ustring u;
        second_element = lisp->provideString(u);
        string nom = first_element->toString(lisp);
        _releasing(first_element);
        first_element = second_element->charge(lisp, nom);
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_fwrite(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        string chemin = first_element->toString(lisp);
        std::ofstream o(chemin.c_str(), std::ios::binary);
        if (o.fail()) {
            string erreur = "Error: Cannot write in file: ";
            erreur += chemin;
            throw new Error(erreur);
        }
        chemin = second_element->toString(lisp);
        o << chemin;
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return true_;
}


Element* List::evall_getchar(LispE* lisp) {

    string code = get_char(lisp->delegation->input_handler);
    return lisp->provideString(code);
}


Element* List::evall_greater(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    Integers* res = NULL;
    Element* test = true_;


    try {
        if (listsize == 3) {
            first_element = liste[1]->eval(lisp);
            second_element = liste[2]->eval(lisp);
            if (booleans_[0] == zero_ && first_element->isList() && second_element->isList()) {
                res = lisp->provideIntegers();
                for (long i = 0; i < first_element->size() && i < second_element->size(); i++) {
                    if (first_element->index(i)->more(lisp, second_element->index(i))->Boolean())
                        res->liste.push_back(1);
                    else
                        res->liste.push_back(0);
                }
                first_element->release();
                second_element->release();
                return res;
            }
            test = first_element->more(lisp, second_element);
            first_element->release();
            second_element->release();
            return test;
        }

        listsize--;
        for (long i = 1; i < listsize && test == true_ ; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->more(lisp, second_element);
            _releasing(first_element);
            _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (res != NULL)
            res->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return test;
}


Element* List::evall_greaterorequal(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    Integers* res = NULL;
    Element* test = true_;


    try {
        if (listsize == 3) {
            first_element = liste[1]->eval(lisp);
            second_element = liste[2]->eval(lisp);
            if (booleans_[0] == zero_ && first_element->isList() && second_element->isList()) {
                res = lisp->provideIntegers();
                for (long i = 0; i < first_element->size() && i < second_element->size(); i++) {
                    if (first_element->index(i)->moreorequal(lisp, second_element->index(i))->Boolean())
                        res->liste.push_back(1);
                    else
                        res->liste.push_back(0);
                }
                first_element->release();
                second_element->release();
                return res;
            }
            test = first_element->moreorequal(lisp, second_element);
            first_element->release();
            second_element->release();
            return test;
        }

        listsize--;
        for (long i = 1; i < listsize && test == true_; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->moreorequal(lisp, second_element);
            _releasing(first_element);
            _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (res != NULL)
            res->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return test;
}


Element* List::evall_if(LispE* lisp) {
    Element* first_element = liste[0];
    bool test = true;


    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        _releasing(first_element);

        if (test) {
            liste[2]->setterminal(terminal);
            return liste[2]->eval(lisp);
        }
        if (liste.size() == 4) {
            liste[3]->setterminal(terminal);
            return liste[3]->eval(lisp);
        }
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return null_;
}


Element* List::evall_ife(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    bool test = true;

    
    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        _releasing(first_element);
        
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
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_in(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    bool res = false;
    
    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        res = first_element->check_element(lisp, second_element);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return booleans_[res];
}

Element* List::evall_index(LispE* lisp) {
    short listsize = liste.size();
    Element* container = liste[1];
    Element* value = null_;
    Element* result = null_;


    try {
        container = container->eval(lisp);
        result = container;
        //The user might have provided a list of indexes
        //which we use to traverse a complex hierarchical structure...
        for (long i = 2; i < listsize; i++) {
            value = liste[i]->eval(lisp);
            result = result->protected_index(lisp, value);
            value->release();
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
        value->release();
        result->release();
        throw err;
    }

    return result;
}

Element* List::evall_index_zero(LispE* lisp) {
    short listsize = liste.size();
    Element* container = liste[1];
    Element* value = null_;
    Element* result = null_;
    long i = 2;

    try {
        listsize--;
        container = container->eval(lisp);
        result = container;
        //The user might have provided a list of indexes
        //which we use to traverse a complex hierarchical structure...
        for (i = 2; i < listsize; i++) {
            value = liste[i]->eval(lisp);
            result = result->protected_index(lisp, value);
            value->release();
        }
        
        value = liste[i]->eval(lisp);
        short the_type = result->type;
        result = result->value_on_index(lisp, value);
        value->release();
        if (result == null_) {
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
        value->release();
        result->release();
        throw err;
    }

    return result;
}


Element* List::evall_set_at(LispE* lisp) {
    short listsize = liste.size();
    Element* container = liste[1];
    Element* ix = null_;
    Element* value = null_;
    Element* result = null_;


    try {
        container = container->eval(lisp);
        result = container;
        for (long i = 2; i < listsize - 2; i++) {
            ix = liste[i]->eval(lisp);
            result = result->protected_index(lisp, ix);
            ix->release();
        }
        ix = liste[listsize-2]->eval(lisp);
        value = liste[listsize-1]->eval(lisp)->copying(false);
        result->replace(lisp, ix, value);
        value->release();
        ix->release();
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
        value->release();
        ix->release();
        result->release();
        throw err;
    }

    return result;
}


//Infix Expressions: x op y op z op u
Element* List::evall_infix(LispE* lisp) {
    Element* expression = liste[1]->eval(lisp);
    short listsize = expression->size();

    if (expression->type != t_list || !listsize)
        return expression;
    
    expression = expression->duplicate_constant();
    //We need first to detect the atom LIST structure
    long ival;
    Element* e = null_;
    Element* eprec;
    List* inter = lisp->provideList();
    //We use sub to execute infix on sub-list within the current list
    List* sub = lisp->provideList();
    sub->append(lisp->provideAtom(l_infix));
    List* root;
    
    if (listsize == 2 && expression->index(0)->isNonOperatorAtom() && expression->index(1)->type == t_list) {
        //this is a function call: test(i j k) --> (test i j k l)
        e = expression->index(1);
        sub->append(e->quoting(lisp));
        e = sub->evall_infix(lisp);
        inter->append(expression->index(0));
        if (e->index(0)->label() == l_concatenate) {
            for (ival = 1; ival < e->size(); ival++)
                inter->append(e->index(ival));
            e->release();
        }
        else {
            if (e->size() == 1) {
                inter->append(e->index(0));
                e->release();
            }
            else
                inter->append(e);
        }
        sub->release();
        expression->release();
        return inter;
    }
    
    bool modified = false;
    for (ival = 0; ival < listsize; ival++) {
        eprec = e;
        e = expression->index(ival);
        if (e->type == t_list) {
            sub->append(e->quoting(lisp));
            e = sub->evall_infix(lisp);
            if (eprec->isPureAtom() && ival) {
                root = lisp->provideList();
                root->append(eprec);
                root->append(e);
                e = root;
                inter->pop();
            }
            inter->append(e);
            sub->pop();
            modified = true;
        }
        else
            inter->append(e);
    }
    
    if (modified) {
        expression->release();
        expression = inter;
        listsize = expression->size();
    }
    else
        inter->release();
    
    if (listsize <= 1 || !(listsize & 1)) {
        sub->release();
        return expression;
    }
    
    List* operations = lisp->provideList();
    root = operations;

    //First we gather all our ops, there should one be every two elements
    Element* op = expression->index(1);
    operations->append(op);
    
    long iop = 3;
    ival = 0;

    try {
        while (ival < listsize) {
            
            //We check the sequence of operators of the same kind
            for (; iop < listsize - 1; iop++) {
                if (expression->index(iop)->isAtom() && op != expression->index(iop)) {
                    op = expression->index(iop);
                    break;
                }
            }
            
            //we then push all the values up to the operator that is different
            for (;ival < iop; ival += 2)
                operations->append(expression->index(ival));
            
            if (iop < listsize -1) {
                //On this case, the current operator becomes the new head
                char comp = lisp->delegation->checkComparator(op->type, root->index(0)->type);
                if (comp == -1) {
                    operations = root;
                    iop += 2;
                    continue;
                }
                
                if (comp) {
                    inter = lisp->provideList();
                    inter->append(op);
                    inter->append(root);
                    root = inter;
                    operations = root;
                }
                else {
                    //We create one level down.
                    //We feed this new List with the last element of the current list
                    inter = lisp->provideList();
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
    }
    catch(Error* err) {
        expression->release();
        root->release();
        sub->rawrelease();
        throw err;
    }
    expression->release();
    sub->rawrelease();
    return root;
}

//Infix Expressions: x op y op z op u
Element* Listincode::eval_infix(LispE* lisp) {
    short listsize = liste.size();
    if (listsize % 2)
        throw new Error("Error: Infix expression is malformed");
    
    //In this case, we will evaluate this expression in the code itself...
    if (listsize == 2)
        return this;
    
    //We use this instruction to read infix structures such as:
    Listincode* operations = new Listincode(line, fileidx);
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
                inter = new Listincode(line, fileidx);
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
                inter = new Listincode(line, fileidx);
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


Element* List::evall_insert(LispE* lisp) {
    short lstsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        //We insert a value in a list
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long ix;
        if (lstsize == 3) {
            ix = first_element->default_insertion();
        }
        else
            evalAsInteger(3, lisp, ix);
        third_element = first_element->insert(lisp, second_element, ix);
        second_element->release();
        if (third_element != first_element) {
            first_element->release();
            return third_element;
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_join(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        u_ustring sep;
        if (liste.size() == 3)
            evalAsUString(2,lisp,sep);
        second_element = first_element->join_in_list(lisp, sep);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_key(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        if (listsize == 1) {
            //We create an empty dictionary
            return lisp->provideDictionary();
        }

        first_element = liste[1]->eval(lisp);
        
        if (listsize == 3 && first_element->isDictionary()) {
            //The second element is an a_key
            switch (first_element->type) {
                case t_dictionary: {
                    u_ustring a_key;
                    evalAsUString(2, lisp, a_key);
                    second_element = first_element->protected_index(lisp, a_key);
                    first_element->release();
                    return second_element;
                }
                case t_dictionaryi: {
                    long a_key;
                    evalAsInteger(2, lisp, a_key);
                    second_element = first_element->protected_index(lisp, a_key);
                    first_element->release();
                    return second_element;
                }
                case t_dictionaryn: {
                    double a_key;
                    evalAsNumber(2, lisp, a_key);
                    second_element = first_element->protected_index(lisp, a_key);
                    first_element->release();
                    return second_element;
                }
            }
        }

        long first = 2;
        if (first_element->isDictionary()) {
            if ((listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'key'");
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant();
        }
        else {
            if (!(listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'key'");
            first = 3;
            u_ustring a_key = first_element->asUString(lisp);
            first_element->release();
            first_element = lisp->provideDictionary();
            second_element = liste[2]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }

        //We store values
        switch (first_element->type) {
            case t_dictionary: {
                u_ustring a_key;
                for (long i = first; i < listsize; i+=2) {
                    evalAsUString(i, lisp, a_key);
                    second_element = liste[i+1]->eval(lisp);
                    first_element->recording(a_key, second_element->copying(false));
                }
                break;
            }
            case t_dictionaryi: {
                long a_key;
                for (long i = 2; i < listsize; i+=2) {
                    evalAsInteger(i, lisp, a_key);
                    second_element = liste[i+1]->eval(lisp);
                    first_element->recording(a_key, second_element->copying(false));
                }
                break;
            }
            case t_dictionaryn: {
                double a_key;
                for (long i = 2; i < listsize; i+=2) {
                    evalAsNumber(i, lisp, a_key);
                    second_element = liste[i+1]->eval(lisp);
                    first_element->recording(a_key, second_element->copying(false));
                }
                break;
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_keyi(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        if (listsize == 1) {
            //We create an empty dictionary
            return lisp->provideDictionary_i();
        }
        first_element = liste[1]->eval(lisp);

        long a_key;
        if (listsize == 3 && first_element->isDictionary()) {
            if (first_element->type != t_dictionaryi)
                throw new Error("Error: wrong dictionary type for 'keyi'");

            evalAsInteger(2, lisp, a_key);
            second_element = first_element->protected_index(lisp, a_key);
            first_element->release();
            return second_element;
        }

        long first;
        if (first_element->isDictionary()) {
			if (first_element->type != t_dictionaryi)
				throw new Error("Error: wrong dictionary type for 'keyi'");

            if ((listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyi'");
            first = 2;
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant();
        }
        else {
            if (!(listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyi'");
            first = 3;
            a_key = first_element->asInteger();
            first_element->release();
            first_element = lisp->provideDictionary_i();
            second_element = liste[2]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }

        //We store the values
        for (long i = first; i < listsize; i+=2) {
            evalAsInteger(i, lisp, a_key);
            second_element = liste[i+1]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_keyn(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        if (listsize == 1) {
            //We create an empty dictionary
            return lisp->provideDictionary_n();
        }
        first_element = liste[1]->eval(lisp);

        double a_key;
        if (listsize == 3 && first_element->isDictionary()) {
            if (first_element->type != t_dictionaryn)
                throw new Error("Error: wrong dictionary type for 'keyn'");

            evalAsNumber(2, lisp, a_key);
            second_element = first_element->protected_index(lisp, a_key);
            first_element->release();
            return second_element;
        }

        long first;
		if (first_element->isDictionary()) {
			if (first_element->type != t_dictionaryn)
				throw new Error("Error: wrong dictionary type for 'keyn'");

			if ((listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyn'");
            first = 2;
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant();
        }
        else {
            if (!(listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyn'");
            first = 3;
            a_key = first_element->asNumber();
            first_element->release();
            first_element = lisp->provideDictionary_n();
            second_element = liste[2]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }

        //We store the values
        for (long i = first; i < listsize; i+=2) {
            evalAsNumber(i, lisp, a_key);
            second_element = liste[i+1]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_keys(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        if (first_element->type != t_dictionary && first_element->type != t_dictionaryn && first_element->type != t_dictionaryi)
            throw new Error(L"Error: the first argument must be a dictionary");
        second_element = first_element->thekeys(lisp);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_label(LispE* lisp) {
    Element* second_element = null_;
    short label;


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
    return liste[1]->eval(lisp)->last_element(lisp);
}

Element* List::evall_list(LispE* lisp) {
    short listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = lisp->provideList();
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element->append(second_element->copying(false));
        }
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_heap(LispE* lisp) {
    short listsize = liste.size();
    Element* oper = null_;
    List* compare = lisp->provideList();
    Heap* tas = NULL;
    Element* second_element = null_;


    try {
        if (listsize != 1)
            oper = liste[1]->eval(lisp);
        
        if (oper == null_ || oper->size() == 0)
            oper = lisp->provideAtom(l_compare);
        
        compare->append(oper);
        compare->append(null_);
        compare->append(null_);
        tas =  new Heap(compare);
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            tas->insert(lisp, second_element->copying(false));
        }
    }
    catch (Error* err) {
        compare->release();
        if (tas != NULL)
            tas->release();
        throw err;
    }

    return tas;
}


Element* List::evall_llist(LispE* lisp) {
    short listsize = liste.size();
    if (listsize == 1)
        return new LList(&lisp->delegation->mark);

    LList* first_element = NULL;
    Element* second_element = null_;

    try {
        first_element = new LList(&lisp->delegation->mark);
        for (long i = listsize - 1; i > 0; i--) {
            second_element = liste[i]->eval(lisp);
            first_element->push_front(second_element->copying(false));
        }
    }
    catch (Error* err) {
        if (first_element != NULL)
            first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_to_list(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = liste[1];


    try {
        second_element = second_element->eval(lisp);
        first_element = second_element->asList(lisp);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_to_llist(LispE* lisp) {
    Element* second_element = liste[1];
    LList* first_element = NULL;

    try {
        second_element = liste[1]->eval(lisp);
        if (second_element->type == t_heap) {
            first_element = (LList*)((Heap*)second_element)->asLList(lisp);
        }
        else {
            if (second_element->isSet()) {
                first_element = new LList(&lisp->delegation->mark);
                void* iter = second_element->begin_iter();
                Element* next_value = second_element->next_iter(lisp, iter);
                while (next_value != emptyatom_) {
                    if (second_element->check_element(lisp, next_value))
                        first_element->push_front(next_value);
                    next_value = second_element->next_iter(lisp, iter);
                }
                second_element->clean_iter(iter);
            }
            else {
                if (second_element->isList()) {
                    if (second_element->type == l_llist)
                        return second_element;
                    
                    first_element = new LList(&lisp->delegation->mark);
                    for (long i = second_element->size() - 1; i >= 0; i--) {
                        first_element->push_front(second_element->index(i)->copying(false));
                    }
                }
                else {
                    first_element = new LList(&lisp->delegation->mark);
                    first_element->push_front(second_element->copying(false));
                }
            }
        }
        second_element->release();
    }
    catch (Error* err) {
        if (first_element != NULL)
            first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_nconc(LispE* lisp) {
    short listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* first_element = null_;
    Element* second_element = null_;
    Element* last = null_;
    Element* e;

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
                first_element = lisp->provideList();
        }
        else {
            if (second_element->isList())
                first_element = second_element->duplicate_constant(pair);
            else
                throw new Error("Error: first element is not a list");
        }

        second_element = emptylist_;
        listsize--;
        for (i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (second_element->isList()) {
                for (l = 0; l < second_element->size(); l++) {
                    e = second_element->value_on_index(lisp, l);
                    first_element->append(e);
                    e->release();
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
                e = last->value_on_index(lisp, l);
                first_element->append(e);
                e->release();
            }
            _releasing(last);
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        last->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_nconcn(LispE* lisp) {
    short listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* first_element = null_;
    Element* second_element = null_;
    Element* last = null_;
    Element* e;

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
                first_element = lisp->provideList();
        }
        else {
            if (second_element->isList())
                first_element = second_element->fullcopy();
            else
                throw new Error("Error: first element is not a list");
        }

        second_element = emptylist_;
        listsize--;
        for (i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (second_element->isList()) {
                for (l = 0; l < second_element->size(); l++) {
                    e = second_element->value_on_index(lisp, l);
                    first_element->append(e);
                    e->release();
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
                e = last->value_on_index(lisp, l);
                first_element->append(e);
                e->release();
            }
            _releasing(last);
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        last->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_load(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->load(second_element->toString(lisp));
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_lock(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;
    bool test = true;


    try {
        second_element = null_;
        u_ustring key;
        evalAsUString(1, lisp, key);
        ThreadLock* _lock = lisp->delegation->getlock(key);
        test = lisp->threaded();
        _lock->locking(test);
        for (long i = 2; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        _lock->unlocking(test);
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_loop(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    short label;


    try {
        //We loop in a list
        label = liste[1]->label();
        if (label == v_null)
            throw new Error(L"Error: Missing label for 'loop'");
        second_element = liste[2]->eval(lisp);
        first_element = second_element->loop(lisp, label, this);
        second_element->release();
        if (lisp->trace > debug_goto)
            lisp->stop_at_next_line(debug_next);
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_loopcount(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;


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
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return second_element;
}

Element* List::evall_compare(LispE* lisp) {
    Element* first_element = null_;
    Element* second_element = null_;
    Element* test = true_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        if (first_element->isequal(lisp, second_element))
            test = zero_;
        else {
            test = first_element->less(lisp, second_element);
            if (test->Boolean())
                test = minusone_;
            else
                test = one_;
        }
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return test;
}

Element* List::evall_lower(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = null_;
    Element* second_element = null_;
    Integers* res = NULL;
    Element* test = true_;


    try {
        if (listsize == 3) {
            first_element = liste[1]->eval(lisp);
            second_element = liste[2]->eval(lisp);
            if (booleans_[0] == zero_ && first_element->isList() && second_element->isList()) {
                res = lisp->provideIntegers();
                for (long i = 0; i < first_element->size() && i < second_element->size(); i++) {
                    if (first_element->index(i)->less(lisp, second_element->index(i))->Boolean())
                        res->liste.push_back(1);
                    else
                        res->liste.push_back(0);
                }
                first_element->release();
                second_element->release();
                return res;
            }
            test = first_element->less(lisp, second_element);
            first_element->release();
            second_element->release();
            return test;
        }

        listsize--;
        for (long i = 1; i < listsize && test == true_; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->less(lisp, second_element);
            _releasing(first_element);
            _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (res != NULL)
            res->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return test;
}


Element* List::evall_lowerorequal(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* test = true_;
    Integers* res = NULL;


    try {
        if (listsize == 3) {
            first_element = liste[1]->eval(lisp);
            second_element = liste[2]->eval(lisp);
            if (booleans_[0] == zero_ && first_element->isList() && second_element->isList()) {
                res = lisp->provideIntegers();
                for (long i = 0; i < first_element->size() && i < second_element->size(); i++) {
                    if (first_element->index(i)->lessorequal(lisp, second_element->index(i))->Boolean())
                        res->liste.push_back(1);
                    else
                        res->liste.push_back(0);
                }
                first_element->release();
                second_element->release();
                return res;
            }
            test = first_element->lessorequal(lisp, second_element);
            first_element->release();
            second_element->release();
            return test;
        }

        listsize--;
        for (long i = 1; i < listsize && test == true_ ; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = first_element->lessorequal(lisp, second_element);
            _releasing(first_element);
            _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (res != NULL)
            res->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return test;
}


Element* List::evall_mapping(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    
    List* call = lisp->provideList();
    
    try {
        //abus de langage: I agree
        //We detect the type of the instruction on the fly
        first_element = liste[1]->eval(lisp);
        //This is a variable that was evaluated on the fly
        call->liste.push_element(first_element);
        call->liste.push_element(liste[2]);
        if (lisp->is_math_operator(first_element->type)) {
            //Then we must duplicate the second element:
            call->liste.push_element(liste[2]);
        }
        second_element = call->eval(lisp);
        call->rawrelease();
        first_element->release();
    }
    catch (Error* err) {
        call->rawrelease();
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_minmax(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* max_element = null_;
    Element* min_element = null_;
    Element* res = null_;

    try {
        if (listsize == 2) {
            //In this case, the argument must be a list
            first_element = liste[1]->eval(lisp);
            res = first_element->minmax(lisp);
            first_element->release();
            return res;
        }

        max_element = liste[1]->eval(lisp);
        min_element = max_element;
        
        for (long i = 2; i < listsize; i++) {
            first_element = liste[i]->eval(lisp);
            if (first_element->less(lisp, min_element)->Boolean()) {
                if (max_element != min_element)
                    min_element->release();
                min_element = first_element;
            }
            else {
                if (first_element->more(lisp, max_element)->Boolean()) {
                    if (max_element != min_element)
                        max_element->release();
                    max_element = first_element;
                }
                else {
                    _releasing(first_element);
                }
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        max_element->release();
        min_element->release();
        throw err;
    }
    
    res = lisp->provideList();
    res->append(min_element);
    res->append(max_element);
    min_element->release();
    max_element->release();
    return res;
}

Element* List::evall_max(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;

    try {
        if (listsize == 2) {
            //In this case, the argument must be a list
            first_element = liste[1]->eval(lisp);
            second_element = first_element->maximum(lisp);
            first_element->release();
            return second_element;
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
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_maybe(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];

    
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
    Element* first_element = liste[0];
    Element* second_element = null_;

    try {
        if (listsize == 2) {
            //In this case, the argument must be a list
            first_element = liste[1]->eval(lisp);
            second_element = first_element->minimum(lisp);
            first_element->release();
            return second_element;
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
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}



Element* List::evall_ncheck(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    bool test = true;


    try {
        //A ncheck is like a check, but when the test fails
        //it executes the second element, otherwise
        //the actual list of instructions starts at 3...
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        _releasing(first_element);
        
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
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_neq(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    bool test = true;


    try {
        test = false;
        for (long i = 1; i < listsize-1 && !test; i++) {
            first_element = liste[i]->eval(lisp);
            second_element = liste[i+1]->eval(lisp);
            test = ( (first_element == second_element) || first_element->egal(second_element));
            _releasing(first_element);
            _releasing(second_element);
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return booleans_[!test];
}


Element* List::evall_not(LispE* lisp) {
    Element* first_element = liste[0];
    bool test = true;


    try {
        first_element = liste[1]->eval(lisp);
        test = first_element->Boolean();
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return booleans_[!test];
}


Element* List::evall_nullp(LispE* lisp) {
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        if (second_element == null_)
            return true_;
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return false_;
}


Element* List::evall_numberp(LispE* lisp) {
    Element* second_element = null_;
    bool test = true;


    try {
        second_element = liste[1]->eval(lisp);
        test = second_element->isNumber();
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}

Element* List::evall_tensor(LispE* lisp) {
    long sz = size();
    Element* e = zero_;


    vecte<long> shape;
    long s;
    try {
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor) {
                Tenseur* ts = new Tenseur(lisp, (List*)e);
                e->release();
                return ts;
            }

            if (!e->isList())
                throw new Error("Error: The first element should be a list");
                        
            if (e->type == t_tensor_float)
                ((Tenseur_float*)e)->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isNumber())
                    throw new Error("Error: this list should contain numbers");
            }
            
            Numbers l;
            e->flatten(lisp,&l);
            Tenseur* ts = new Tenseur(lisp, &l, shape);
            e->release();
            return ts;
        }
        
        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        throw err;
    }

    return new Tenseur(lisp, shape, zero_);
}

Element* List::evall_tensor_float(LispE* lisp) {
    long sz = size();
    Element* e = zero_;


    vecte<long> shape;
    long s;
    try {
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_float) {
                Tenseur_float* ts = new Tenseur_float(lisp, (Tenseur_float*)e);
                e->release();
                return ts;
            }

            if (!e->isList())
                throw new Error("Error: The first element should be a list");

            if (e->type == t_tensor)
                ((Tenseur*)e)->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isNumber())
                    throw new Error("Error: this list should contain numbers");
            }

            Floats l;
            e->flatten(lisp,&l);
            Tenseur_float* ts = new Tenseur_float(lisp, &l, shape);
            e->release();
            return ts;
        }
        
        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        throw err;
    }

    return new Tenseur_float(lisp, shape, zero_);
}

Element* List::evall_matrix(LispE* lisp) {
    long sz = size();
    Element* e = zero_;

    long sx, sy;
    try {
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->type == t_matrix) {
                Matrice* m = new Matrice(lisp, (Matrice*)e);
                e->release();
                return m;
            }

            if (e->type == t_matrix_float) {
                Matrice* m = new Matrice(lisp, (Matrice_float*)e);
                e->release();
                return m;
            }

            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");
            
            long size_x = e->size();
            long size_y = e->index(0)->size();
            Numbers l;
            e->flatten(lisp, &l);
            Matrice* m = new Matrice(lisp, &l, size_x, size_y);
            e->release();
            return m;
        }
        
        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);
        
    }
    catch (Error* err) {
        e->release();
        throw err;
    }

    return new Matrice(sx, sy, e);
}

Element* List::evall_matrix_float(LispE* lisp) {
    long sz = size();
    Element* e = zero_;


    long sx, sy;
    try {
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->type == t_matrix_float) {
                Matrice_float* m = new Matrice_float(lisp, (Matrice_float*)e);
                e->release();
                return m;
            }

            if (e->type == t_matrix) {
                Matrice_float* m = new Matrice_float(lisp, (Matrice*)e);
                e->release();
                return m;
            }

            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");
            
            long size_x = e->size();
            long size_y = e->index(0)->size();
            Floats l;
            e->flatten(lisp, &l);
            Matrice_float* m = new Matrice_float(lisp, &l, size_x, size_y);
            e->release();
            return m;
        }
        
        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);
        
    }
    catch (Error* err) {
        e->release();
        throw err;
    }

    return new Matrice_float(sx, sy, e);
}

Element* List::evall_floats(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideFloats();

    Floats* n = lisp->provideFloats();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->liste.push_back(values->index(i)->asFloat());
                }
            }
            else
                n->liste.push_back(values->asFloat());
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_numbers(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideNumbers();

    Numbers* n = lisp->provideNumbers();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->liste.push_back(values->index(i)->asNumber());
                }
            }
            else
                n->liste.push_back(values->asNumber());
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_shorts(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return new Shorts();

    Shorts* n = new Shorts();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->liste.push_back(values->index(i)->asShort());
                }
            }
            else
                n->liste.push_back(values->asShort());
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_integers(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideIntegers();

    Integers* n = lisp->provideIntegers();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->liste.push_back(values->index(i)->asInteger());
                }
            }
            else
                n->liste.push_back(values->asInteger());
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_set(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet();

    Set* n = lisp->provideSet();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            n->add(lisp, values);
            _releasing(values);
        }
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_sets(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_s();

    Set_s* n = lisp->provideSet_s();
    Element* values = null_;

    try {
        values = liste[1]->eval(lisp);
        if (values->isSet()) {
            if (values->type == t_sets)
                n->ensemble = ((Set_s*)values)->ensemble;
            else {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asUString(lisp));
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
        }
        else {
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->add(values->index(i)->asUString(lisp));
                }
            }
            else
                n->add(values->asUString(lisp));
        }
        _releasing(values);

        for (long e = 2; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isSet()) {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asUString(lisp));
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asUString(lisp));
                    }
                }
                else
                    n->add(values->asUString(lisp));
            }
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_seti(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_i();

    Set_i* n = lisp->provideSet_i();
    Element* values = null_;

    try {
        values = liste[1]->eval(lisp);
        if (values->isSet()) {
            if (values->type == t_seti)
                n->ensemble = ((Set_i*)values)->ensemble;
            else {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asInteger());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
        }
        else {
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->add(values->index(i)->asInteger());
                }
            }
            else
                n->add(values->asNumber());
        }
        _releasing(values);

        for (long e = 2; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isSet()) {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asInteger());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asInteger());
                    }
                }
                else
                    n->add(values->asInteger());
            }
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_setn(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_n();

    Set_n* n = lisp->provideSet_n();
    Element* values = null_;

    try {
        values = liste[1]->eval(lisp);
        if (values->isSet()) {
            if (values->type == t_setn)
                n->ensemble = ((Set_n*)values)->ensemble;
            else {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asNumber());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
        }
        else {
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->add(values->index(i)->asNumber());
                }
            }
            else
                n->add(values->asNumber());
        }
        _releasing(values);

        for (long e = 2; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isSet()) {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asNumber());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asNumber());
                    }
                }
                else
                    n->add(values->asNumber());
            }
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* List::evall_strings(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideStrings();

    Strings* n = lisp->provideStrings();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->liste.push_back(values->index(i)->asUString(lisp));
                }
            }
            else
                n->liste.push_back(values->asUString(lisp));
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}


Element* List::evall_or(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;
    bool test = false;


    try {
        second_element = null_;
        for (long i = 1; i < listsize && !test; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            test = second_element->Boolean();
        }
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}


Element* List::evall_pipe(LispE* lisp) {

    //pipe returns a line read on std::cin
    //It returns nil, if the stream is closed...
    string code;
    getline(std::cin, code);
    if (std::cin.eof())
        return null_;
    return lisp->provideString(code);
}




Element* List::evall_pop(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        //If it is a string, we return a copy, we do not modify the string.
        //itself
        if (first_element->isString()) {
            long keyvalue;
            u_ustring strvalue = first_element->asUString(lisp);
            if (liste.size() == 3)
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

        if (liste.size() == 3) {
            second_element = liste[2]->eval(lisp);
            if (first_element->remove(lisp, second_element))
                return first_element;
        }
        else {
            if (first_element->type == t_llist) {
                if (first_element->removefirst())
                    return first_element;
            }
            else {
                if (first_element->removelast())
                    return first_element;
            }
        }
        
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}

Element* List::evall_popfirst(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        //If it is a string, we return a copy, we do not modify the string.
        //itself
        if (first_element->isString()) {
            u_ustring strvalue = first_element->asUString(lisp);
            if (!strvalue.size())
                return emptystring_;
            strvalue = strvalue.substr(1, strvalue.size());
            return lisp->provideString(strvalue);
        }

        if (first_element->removefirst())
            return first_element;
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}

Element* List::evall_poplast(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        //If it is a string, we return a copy, we do not modify the string.
        //itself
        if (first_element->isString()) {
            u_ustring strvalue = first_element->asUString(lisp);
            if (!strvalue.size())
                return emptystring_;
            strvalue.pop_back();
            return lisp->provideString(strvalue);
        }

        if (first_element->removelast())
            return first_element;
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return null_;
}



Element* List::evall_prettify(LispE* lisp) {
    Element* first_element = liste[0];


    try {
        first_element = liste[1]->eval(lisp);
        string s = first_element->prettify(lisp);
        first_element->release();
        return lisp->provideString(s);
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


    try {
        string val;
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            val += second_element->toString(lisp);
            _releasing(second_element);
        }
        
        lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
        if (lisp->isThread)
            std::cout.flush();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return emptyatom_;
}


Element* List::evall_printerr(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;


    try {
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            std::cerr << second_element->toString(lisp);
            _releasing(second_element);
        }
        if (lisp->isThread)
            std::cerr.flush();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return emptyatom_;
}


Element* List::evall_printerrln(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;


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
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return emptyatom_;
}


Element* List::evall_println(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;


    try {
        string val;
        for (long i = 1; i < listsize; i++) {
            if (i != 1)
                val += " ";
            second_element = liste[i]->eval(lisp);
            val += second_element->toString(lisp);
            _releasing(second_element);
        }
 #ifdef WIN32
		val += "\r\n";
 #else
        val += "\n";
 #endif
        lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
        if (lisp->isThread)
            std::cout.flush();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return emptyatom_;
}


Element* List::evall_extend(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* e;


    try {
        //We store a value in a list
        first_element = liste[1]->eval(lisp);
        if (!first_element->isList())
            throw new Error(L"Error: missing list in 'extend'");
        first_element = first_element->duplicate_constant();
        second_element = liste[2]->eval(lisp);
        if (second_element->isList()) {
            if (first_element->type == t_llist) {
                first_element->concatenate(lisp, second_element);
            }
            else {
                for (long i = 0; i < second_element->size(); i++) {
                    e = second_element->index(i)->copying(false);
                    first_element->append(e);
                    e->release();
                }
            }
        }
        else {
            second_element = second_element->copying(false);
            first_element->append(second_element);
        }
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_push(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* e;

    try {
        //We store a value in a list
        first_element = first_element->eval(lisp);
        if (!first_element->isList() && !first_element->isSet())
            throw new Error(L"Error: missing list in 'push'");
        first_element = first_element->duplicate_constant();
        if (first_element->type == t_llist) {
            for (long i = 2; i < size(); i++) {
                second_element = liste[i]->eval(lisp);
                e = second_element->copying(false);
                if (e != second_element) {
                    _releasing(second_element);
                }
                ((LList*)first_element)->push_front(e);
            }
        }
        else {
            for (long i = 2; i < size(); i++) {
                second_element = liste[i]->eval(lisp);
                e = second_element->copying(false);
                first_element->append(e);
                if (e != second_element) {
                    _releasing(second_element);
                }
                e->release();
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }
    return first_element;
}

Element* List::evall_pushfirst(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* e;

    try {
        //We store a value in a list
        first_element = first_element->eval(lisp);
        if (!first_element->isList())
            throw new Error(L"Error: missing list in 'push'");
        first_element = first_element->duplicate_constant();
        if (first_element->type == t_llist) {
            for (long i = 2; i < size(); i++) {
                second_element = liste[i]->eval(lisp);
                e = second_element->copying(false);
                if (e != second_element) {
                    _releasing(second_element);
                }
                ((LList*)first_element)->push_front(e);
            }
        }
        else {
            for (long i = 2; i < size(); i++) {
                second_element = liste[i]->eval(lisp);
                e = second_element->copying(false);
                first_element->insert(lisp, e, 0);
                if (e != second_element) {
                    _releasing(second_element);
                }
                e->release();
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_pushlast(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* e;

    try {
        //We store a value in a list
        first_element = first_element->eval(lisp);
        if (!first_element->isList())
            throw new Error(L"Error: missing list in 'push'");
        first_element = first_element->duplicate_constant();
        if (first_element->type == t_llist) {
            for (long i = 2; i < size(); i++) {
                second_element = liste[i]->eval(lisp);
                e = second_element->copying(false);
                if (e != second_element) {
                    _releasing(second_element);
                }
                ((LList*)first_element)->append(e);
            }
        }
        else {
            for (long i = 2; i < size(); i++) {
                second_element = liste[i]->eval(lisp);
                e = second_element->copying(false);
                first_element->append(e);
                if (e != second_element) {
                    _releasing(second_element);
                }
                e->release();
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_reverse(LispE* lisp) {
    Element* result = liste[0];
    Element* matrix = null_;

    bool duplicate = true;
    
    try {
        matrix = liste[1]->eval(lisp);
        if (liste.size() == 3) {
            result = liste[2]->eval(lisp);
            if (result != true_) {
                long axis = result->asInteger();
                if (axis < 0 || axis >= matrix->shapesize())
                    throw new Error("Error: cannot reverse along this axis");
                result = matrix->rotate(lisp, axis);
                matrix->release();
                return result;
            }
            duplicate = false;
        }
        
        result = matrix->reverse(lisp, duplicate);
        if (matrix != result)
            matrix->release();
    }
    catch (Error* err) {
        result->release();
        matrix->release();
        throw err;
    }

    return result;
}


Element* List::evall_revertsearch(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4) {
            evalAsInteger(3,lisp, ix);
        }
        third_element = first_element->search_reverse(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return third_element;
}

Element* List::evall_search(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4)
            evalAsInteger(3,lisp, ix);
        third_element = first_element->search_element(lisp, second_element, ix);
        _releasing(first_element);
        _releasing(second_element);
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return third_element;
}


Element* List::evall_count(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4) {
            evalAsInteger(3,lisp, ix);
        }
        third_element = first_element->count_all_elements(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return third_element;
}

Element* List::evall_searchall(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4) {
            evalAsInteger(3,lisp, ix);
        }
        third_element = first_element->search_all_elements(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        throw err;
    }

    return third_element;
}

Element* List::evall_replaceall(LispE* lisp) {
    Element* object = liste[0];
    Element* search = null_;
    Element* replace = null_;
    Element* e = null_;
    
    try {
        object = liste[1]->eval(lisp);
        search = liste[2]->eval(lisp);
        replace = liste[3]->eval(lisp);
        e = object->replace_all_elements(lisp, search, replace);
        object->release();
        search->release();
        replace->release();
    }
    catch (Error* err) {
        object->release();
        search->release();
        replace->release();
        throw err;
    }

    return e;
}


Element* List::evall_select(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;


    try {
        //we return the first non null value
        second_element = null_;
        for (long i = 1; i < listsize && second_element == null_; i++) {
            liste[i]->setterminal(terminal);
            second_element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return second_element;
}

#ifdef MAX_STACK_SIZE_ENABLED
Element* List::evall_set_max_stack_size(LispE* lisp) {

    long m;
    if (liste.size() == 1)
        return lisp->provideInteger(lisp->stack_size_max());
    evalAsInteger(1, lisp, m);
    lisp->set_stack_max_size(m);
    return true_;
}
#endif

Element* List::evall_setg(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->storing_global(element, liste[1]->label());
    return true_;
}


Element* List::evall_setq(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->storing_variable(element, liste[1]->label());
    return true_;
}

Element* List::evall_signp(LispE* lisp) {
    double v = 0;

    evalAsNumber(1, lisp, v);
    if (v == 0)
        return zero_;
    if (v < 0)
        return minusone_;
    return one_;
}

Element* List::evall_sign(LispE* lisp) {
    Element* e = liste[1]->eval(lisp);
    Element* ue = e->invert_sign(lisp);
    e->release();
    return ue;
}


Element* List::evall_size(LispE* lisp) {

    Element* element = liste[1]->eval(lisp);
    long sz = element->size();
    element->release();
    return lisp->provideInteger(sz);
}

Element* List::evall_sleep(LispE* lisp) {

    long tm;
    evalAsInteger(1, lisp, tm);
    std::this_thread::sleep_for(std::chrono::milliseconds(tm));
    return true_;
}


Element* List::evall_sort(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        //First element is the comparison function OR an operator
        first_element = liste[1]->eval(lisp);
        second_element = liste[2]->eval(lisp);
        if (!second_element->isList())
            throw new Error(L"Error: the second argument should be a list for 'sort'");

        if (first_element->isList()) {
            //It is inevitably a lambda
            if (first_element->size() == 1)
                first_element = first_element->index(0);
        }
        else {
            //C Is either an atom or an operator
            if (!first_element->isAtom())
                throw new Error(L"Error: incorrect comparison function in 'sort'");
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    switch (second_element->type) {
        case t_floats: {
            List complist;
            complist.append(first_element);
            complist.append(null_);
            complist.append(null_);
            try {
                ((Floats*)second_element)->sorting(lisp, &complist);
                first_element->release();
                return second_element;
            }
            catch (Error* err) {
                first_element->release();
                second_element->release();
                throw err;
            }
        }
        case t_numbers: {
            List complist;
            complist.append(first_element);
            complist.append(null_);
            complist.append(null_);
            try {
                ((Numbers*)second_element)->sorting(lisp, &complist);
                first_element->release();
                return second_element;
            }
            catch (Error* err) {
                first_element->release();
                second_element->release();
                throw err;
            }
        }
        case t_shorts: {
            List complist;
            complist.append(first_element);
            complist.append(null_);
            complist.append(null_);
            try {
                ((Shorts*)second_element)->sorting(lisp, &complist);
                first_element->release();
                return second_element;
            }
            catch (Error* err) {
                first_element->release();
                second_element->release();
                throw err;
            }
        }
        case t_integers: {
            List complist;
            complist.append(first_element);
            complist.append(null_);
            complist.append(null_);
            try {
                ((Integers*)second_element)->sorting(lisp, &complist);
                first_element->release();
                return second_element;
            }
            catch (Error* err) {
                first_element->release();
                second_element->release();
                throw err;
            }
        }
        case t_strings: {
            List complist;
            complist.append(first_element);
            complist.append(null_);
            complist.append(null_);
            try {
                ((Strings*)second_element)->sorting(lisp, &complist);
                first_element->release();
                return second_element;
            }
            catch (Error* err) {
                first_element->release();
                second_element->release();
                throw err;
            }
        }
        case t_llist: {
            List* l = (List*)second_element->asList(lisp);
            if (l->size() <= 1) {
                l->release();
                return second_element;
            }
            
            List complist;
            complist.append(first_element);
            complist.append(l->index(0)->quoting(lisp));
            complist.append(l->index(0)->quoting(lisp));
            if (complist.eval(lisp)->Boolean()) {
                first_element->release();
                second_element->release();
                throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
            }
            complist.replacing(1, null_);
            complist.replacing(2, null_);

            l->liste.sorting(lisp, &complist);
            first_element->release();
            u_link* it = ((LList*)second_element)->liste.begin();
            for (long i = 0; i < l->size(); i++) {
                it->value = l->index(i);
                it = it->next();
            }
            
            l->release();
            return second_element;
        }
        default: {
            List* l = (List*)second_element;
            if (l->size() <= 1)
                return second_element;
            
            List complist;
            complist.append(first_element);
            complist.append(l->index(0)->quoting(lisp));
            complist.append(l->index(0)->quoting(lisp));
            if (complist.eval(lisp)->Boolean()) {
                first_element->release();
                second_element->release();
                throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
            }
            complist.replacing(1, null_);
            complist.replacing(2, null_);
            l->liste.sorting(lisp, &complist);
            first_element->release();
            return second_element;
        }
    }
}

Element* List::evall_stringp(LispE* lisp) {
    
    Element* element = liste[1]->eval(lisp);
    bool test = element->isString();
    element->release();
    return booleans_[test];
}


Element* List::evall_threadclear(LispE* lisp) {

    if (liste.size() == 1) {
        lisp->delegation->thread_clear_all();
        return true_;
    }
    
    u_ustring key;
    evalAsUString(1, lisp, key);
    return booleans_[lisp->delegation->thread_clear(key)];
}


Element* List::evall_threadretrieve(LispE* lisp) {
    Element* first_element = liste[0];


    try {
        if (liste.size() == 1) {
            //We return all as a dictionary
            return lisp->delegation->thread_retrieve_all();
        }
        u_ustring key;
        evalAsUString(1, lisp, key);
        first_element = lisp->delegation->thread_retrieve(key);
        if (first_element == NULL)
            return null_;
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_threadstore(LispE* lisp) {
    Element* first_element = liste[0];

    lisp->preparingthread = true;
    try {
        u_ustring key;
        evalAsUString(1, lisp, key);
        first_element = liste[2]->eval(lisp);
        lisp->delegation->thread_store(key, first_element);
        first_element->release();
        lisp->preparingthread = false;
    }
    catch (Error* err) {
        lisp->preparingthread = false;
        first_element->release();
        throw err;
    }

    return true_;
}


Element* List::evall_throw(LispE* lisp) {
    
    u_ustring msg;
    evalAsUString(1, lisp, msg);
    throw new Error(msg);
}


Element* List::evall_trace(LispE* lisp) {
    Element* first_element = liste[0];


    try {
        if (liste.size() == 1) {
            if (lisp->trace)
                return true_;
            return false_;
        }
        first_element = liste[1]->eval(lisp);
        lisp->trace  = first_element->Boolean();
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return booleans_[lisp->trace];
}


Element* List::evall_trigger(LispE* lisp) {

    u_ustring key;
    evalAsUString(1, lisp, key);
    return booleans_[lisp->delegation->trigger(key)];
}


Element* List::evall_type(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        second_element = liste[1]->eval(lisp);
        first_element = lisp->provideAtom(second_element->type);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_rotate(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;

    bool left = false;

    try {
        if (liste.size() == 3) {
            second_element = liste[2]->eval(lisp);
            left = second_element->Boolean();
            _releasing(second_element);
        }
        first_element = liste[1]->eval(lisp);
        second_element = first_element->rotate(left);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}

Element* List::evall_unique(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = first_element->unique(lisp);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_use(LispE* lisp) {
    Element* second_element = null_;


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
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = first_element->thevalues(lisp);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_wait(LispE* lisp) {
    
    //We wait for all threads to be finished
    while (lisp->nbjoined) {}
    return true_;
}


Element* List::evall_waiton(LispE* lisp) {

    u_ustring key;
    evalAsUString(1, lisp, key);
    lisp->delegation->waiton(key);
    return true_;
}


Element* List::evall_while(LispE* lisp) {
    short listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;


    try {
        first_element = liste[1]->eval(lisp);
        second_element = null_;
        while (first_element->Boolean()) {
            first_element->release();
            for (long i = 2; i < listsize && second_element->type != l_return; i++) {
                _releasing(second_element);
                second_element = liste[i]->eval(lisp);
            }

            //if a 'return' has been placed in the code
            if (second_element->type == l_return) {
                lisp->stop_at_next_line(lisp->trace);
                if (second_element->isBreak())
                    return null_;
                //this is a return, it goes back to the function call
                return second_element;
            }
            first_element = liste[1]->eval(lisp);
        }
        first_element->release();
        if (lisp->trace > debug_goto)
            lisp->stop_at_next_line(debug_next);

        if (second_element->type == l_return) {
            if (second_element->isBreak())
                return null_;
            return second_element;
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }

    return second_element;
}


Element* List::evall_xor(LispE* lisp) {
    short listsize = liste.size();
    Element* second_element = null_;
    char test = true;
    char check = true;


    try {
        second_element = liste[1]->eval(lisp);
        check = (char)second_element->Boolean();
        for (long i = 2; i < listsize && test; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            test = check;
            check = (char)second_element->Boolean();
            test ^= check;
        }
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        throw err;
    }

    return booleans_[test];
}

Element* List::evall_mark(LispE* lisp) {
    Element* first_element = null_;

    bool test;
    
    try {
        first_element = liste[1]->eval(lisp);
        if (liste.size() == 2) {
            test = first_element->usermark();
            first_element->release();
            return booleans_[test];
        }
        
        bool test = liste[2]->eval(lisp)->Boolean();
        first_element->setusermark(test);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return true_;
}

Element* List::evall_resetmark(LispE* lisp) {
    Element* first_element = null_;

    
    try {
        first_element = liste[1]->eval(lisp);
        first_element->resetusermark();
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return true_;
}


Element* List::evall_zerop(LispE* lisp) {
    
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
        u_ustring identifier;
        evalAsUString(1, lisp, identifier);
        lisp->replaceAtom(identifier, second_element->label());
    }
    catch (Error* err) {
        second_element->release();
        first_element->release();
        throw err;
    }
    
    return true_;
}

Element* List::evall_zip(LispE* lisp) {
    short listsize = liste.size();
    List* lists = lisp->provideList();
    Element* container = null_;
    Element* result = null_;
    long szl = -1;
    long i;
    long j = 0;
    short thetype = 0;

    try {
        //We combine different lists together...
        for (i = 1; i < listsize; i++) {
            container = liste[i]->eval(lisp);
            if (!container->isList())
                throw new Error(L"Error: 'zip' only accepts lists as arguments");
            if (i == 1)
                szl = container->size();
            else {
                if (szl != container->size())
                    throw new Error(L"Error: Lists should all have the same size in 'zip'");
            }
            lists->append(container);
            if (!thetype)
                thetype = container->type;
            else {
                if (thetype != container->type) {
                    if (thetype == t_integers && container->type == t_numbers)
                        thetype = t_numbers;
                    else
                        thetype = t_list;
                }
            }
            container = null_;
        }
        
        result = lisp->provideList();
        Element* sub;
        Element* e;
        switch (thetype) {
            case t_strings:
                sub = lisp->provideStrings();
                break;
            case t_shorts:
                sub = new Shorts();
                break;
            case t_integers:
                sub = lisp->provideIntegers();
                break;
            case t_floats:
                sub = lisp->provideFloats();
                break;
            case t_numbers:
                sub = lisp->provideNumbers();
                break;
            default:
                sub = lisp->provideList();
        }
        for (j = 0; j < szl; j++) {
            if (j)
                sub = sub->newInstance();
            result->append(sub);
            for (i = 0; i <lists->size(); i++) {
                e = lists->liste[i]->value_on_index(lisp, j);
                sub->append(e);
                e->release();
            }
        }
        lists->release();
    }
    catch (Error* err) {
        container->release();
        result->release();
        lists->release();
        throw err;
    }

    return result;
}


Element* List::evall_zipwith(LispE* lisp) {
    List* lists = lisp->provideList();
    List* call = lisp->provideList();

    long szl;
    long lsz;
    long i, j = 0;
    short listsize = liste.size();
    Element* function = liste[0];
    Element* container = null_;
    Element* value = null_;
    List* params = NULL;
    Integers* labels = NULL;

    
    try {
        //We combine different lists together with an operator
        //First element is the operation
        container = liste[2]->eval(lisp);
        if (!container->isList())
            throw new Error(L"Error: 'zipwith' only accepts lists as arguments");
        szl = container->size();
        lists->append(container);
        
        for (i = 3; i < listsize; i++) {
            container = liste[i]->eval(lisp);
            if (container->isList() && szl == container->size()) {
                lists->append(container);
                container = null_;
            }
            else
                throw new Error(L"Error: 'zipwith' only accepts lists of same size as arguments");
        }

        if (!szl) {
            lists->release();
            call->release();
            return emptylist_;
        }

        lsz = lists->size();

        //First element is the operation, second element the list
        function = liste[1];
        if (!function->isLambda())
            function = liste[1]->eval(lisp);
        
        if (function->isLambda()) {
            labels = lisp->provideIntegers();
            params = (List*)function->index(1);
            if (params->size() < lsz)
                throw new Error("Error: Wrong number of arguments");
            
            for (i = 0; i < lsz; i++) {
                if (lisp->localsave(params->index(i), call))
                    lisp->replacingvalue(lists->liste[i]->index(0), params->liste[i]->label());
                else {
                    lisp->recording(lists->liste[i]->index(0), params->liste[i]->label());
                    labels->liste.push_back(params->index(i)->label());
                }
            }
            
            value = eval_lambda_min(lisp, (List*)function);
            switch (value->type) {
                case t_string:
                    container = lisp->provideStrings();
                    break;
                case t_integer:
                    container = lisp->provideIntegers();
                    break;
                case t_float:
                    container = lisp->provideFloats();
                    break;
                case t_number:
                    container = lisp->provideNumbers();
                    break;
                default:
                    container = lisp->provideList();
            }
            container->append(value);
            _releasing(value);

            for (j = 1; j < szl; j++) {
                for (i = 0; i < lsz; i++)
                    lisp->replacingvalue(lists->liste[i]->index(j), params->liste[i]->label());
                value = eval_lambda_min(lisp, (List*)function);
                container->append(value);
                _releasing(value);
            }
            
            for (i = 0; i < call->size(); i+=2) {
                lisp->replacingvalue(call->liste[i+1], call->liste[i]->label());
            }
            call->release();
            for (i = 0; i < labels->size(); i++)
                lisp->removefromstack(labels->liste[i]);
            labels->release();
        }
        else {
            call->append(function);
            for (j = 0; j < lsz; j++)
                call->append(null_);

            methodEval met = lisp->delegation->evals[function->type];
            for (i = 0; i < lsz; i++)
                call->liste[i+1] = lists->liste[i]->index(0);
            value = (call->*met)(lisp);
            switch (value->type) {
                case t_string:
                    container = lisp->provideStrings();
                    break;
                case t_integer:
                    container = lisp->provideIntegers();
                    break;
                case t_float:
                    container = lisp->provideFloats();
                    break;
                case t_number:
                    container = lisp->provideNumbers();
                    break;
                default:
                    container = lisp->provideList();
            }
            container->append(value);
            _releasing(value);
            for (j = 1; j < szl; j++) {
                for (i = 0; i < lsz; i++)
                    call->liste[i+1] = lists->liste[i]->index(j);
                value = (call->*met)(lisp);
                container->append(value);
                _releasing(value);
            }
            call->rawrelease();
        }
        
        function->release();
        lists->release();
    }
    catch (Error* err) {
        container->increment();
        if (params != NULL) {
            for (i = 0; i < call->size(); i+=2) {
                lisp->replacingvalue(call->liste[i+1], call->liste[i]->label());
            }
            call->release();
            for (i = 0; i < labels->size(); i++)
                lisp->removefromstack(labels->liste[i]);
            labels->release();
        }
        else
            call->rawrelease();
        lists->release();
        function->release();
        container->decrement();
        value->release();
        throw err;
    }

    return container;
}

Element* List::eval_call_function(LispE* lisp) {
    if (lisp->trace) {
        Element* body = liste[0]->eval(lisp);

        //We also retrieve its label (which is l_defun or l_defpat or...)
        short label = body->index(0)->label();
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

    if (lisp->trace) {
        //We also retrieve its label (which is l_defun or l_defpat or...)
        short label = body->index(0)->label();
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

    if (lisp->threaded())
        return evalfunction(lisp, body);
    
    short label = body->function_label();
    switch(label) {
        case l_defpat:
            liste[0] = new Atomefonction(body, t_pattern);
            lisp->garbaging(liste[0]);
            return eval_pattern(lisp, body->index(1)->label());
        case l_dethread:
            return eval_thread(lisp, (List*)body);
        case l_deflib:
            liste[0] = new Atomefonction(body, t_library_function);
            lisp->garbaging(liste[0]);
            return eval_library_function(lisp, (List*)body);
        case l_defun:
            liste[0] = new Atomefonction(body, t_function);
            lisp->garbaging(liste[0]);
            return eval_function(lisp, (List*)body);
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
    Element* body = lisp->called();

    if (lisp->trace) {
        //We also retrieve its label (which is l_defun or l_defpat or...)
        short label = body->index(0)->label();
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

    if (lisp->threaded())
        return evalfunction(lisp, body);
    
    short label = body->function_label();
    switch(label) {
        case l_defpat:
            liste[0] = new Atomefonction(body, t_pattern);
            lisp->garbaging(liste[0]);
            return eval_pattern(lisp, body->index(1)->label());
        case l_dethread:
            return eval_thread(lisp, (List*)body);
        case l_deflib:
            liste[0] = new Atomefonction(body, t_library_function);
            lisp->garbaging(liste[0]);
            return eval_library_function(lisp, (List*)body);
        case l_defun:
            liste[0] = new Atomefonction(body, t_function);
            lisp->garbaging(liste[0]);
            return eval_function(lisp, (List*)body);
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
    return evall_set_at(lisp);
}
#endif
