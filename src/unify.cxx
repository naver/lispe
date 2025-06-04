/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  unify.cxx
//
//


#include <stdio.h>
#include "lispe.h"
#include "segmentation.h"
#include "tools.h"
#include "tokens.h"

#ifdef UNIX
#include <sys/resource.h>
#endif

//null_ unifies with all
bool Element::unify(LispE* lisp, Element* value, bool record) {
    return (this == null_ || value == this);
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

#ifdef LISPE_WASM_NO_EXCEPTION
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

