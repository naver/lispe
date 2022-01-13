/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  elements.cxx
//
//

#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>

//------------------------------------------------------------------------------------------
binSetIter::binSetIter(binSet& b) {
    indexes = b.indexes;
    tsize = b.tsize;
    base = b.base;
    idx = 0;
    nb = 0;
    filter = indexes[0];
}

void binSetIter::set(binSet& b) {
    indexes = b.indexes;
    tsize = b.tsize;
    base = b.base;
    idx = 0;
    nb = 0;
    filter = indexes[0];
}

//------------------------------------------------------------------------------------------
Element* Element::quoting(LispE* lisp) {
    List* l = lisp->provideList();
    l->append(quote_);
    l->append(this);
    return l;
}
//------------------------------------------------------------------------------------------
Rankloop::Rankloop(LispE* lp, List* l) : List(l,0) {
    last = false;
    type = l_irank;
    lisp = lp;
    index_value = null_;
    lst = l;
    max_iterator = 0;
}
//------------------------------------------------------------------------------------------
short Element::function_label() {
    throw new Error("Error: Not a function or a data structure");
}
//------------------------------------------------------------------------------------------
void Listpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_back(this);
    }
}

void Listpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_back(this);
    }
}

void Listpool::release() {
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_back(this);
    }
}

void Listpool::release(Element* e) {
    if (!status) {
        liste.decrement(e);
        liste.clear();
        lisp->list_pool.push_back(this);
    }
}

void Listpool::rawrelease() {
    if (!status) {
        liste.clear();
        liste.decrement();
        lisp->list_pool.push_back(this);
    }
}

void Floatpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status)
        lisp->float_pool.push_back(this);
}


void Floatpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status)
        lisp->float_pool.push_back(this);
}

void Floatpool::release() {
    if (!status)
        lisp->float_pool.push_back(this);
}

void Numberpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status)
        lisp->number_pool.push_back(this);
}


void Numberpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status)
        lisp->number_pool.push_back(this);
}

void Numberpool::release() {
    if (!status)
        lisp->number_pool.push_back(this);
}

void Returnpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status)
        lisp->return_pool.push_back(this);
}

void Returnpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status)
        lisp->return_pool.push_back(this);
}


void Returnpool::release() {
    if (!status)
        lisp->return_pool.push_back(this);
}

void Integerpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        lisp->integer_pool.push_back(this);
    }
}


void Integerpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        lisp->integer_pool.push_back(this);
    }
}

void Integerpool::release() {
    if (!status) {
        lisp->integer_pool.push_back(this);
    }
}

void Stringpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}


void Stringpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

void Stringpool::release() {
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

void Floatspool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        liste.clear();
        lisp->floats_pool.push_back(this);
    }
}


void Floatspool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        liste.clear();
        lisp->floats_pool.push_back(this);
    }
}

void Floatspool::release() {
    if (!status) {
        liste.clear();
        lisp->floats_pool.push_back(this);
    }
}

void Numberspool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_back(this);
    }
}


void Numberspool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_back(this);
    }
}

void Numberspool::release() {
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_back(this);
    }
}

void Integerspool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        lisp->integers_pool.push_back(this);
        liste.clear();
    }
}


void Integerspool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        lisp->integers_pool.push_back(this);
        liste.clear();
    }
}

void Integerspool::release() {
    if (!status) {
        lisp->integers_pool.push_back(this);
        liste.clear();
    }
}

void Stringspool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        lisp->strings_pool.push_back(this);
        liste.clear();
    }
}


void Stringspool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status -= nb;
    if (!status) {
        lisp->strings_pool.push_back(this);
        liste.clear();
    }
}

void Stringspool::release() {
    if (!status) {
        lisp->strings_pool.push_back(this);
        liste.clear();
    }
}

Element* Floatspool::newInstance() {
    return lisp->provideFloats();
}

Element* Floatspool::newInstance(Element* v) {
    return lisp->provideFloats(liste.size(), v->asFloat());
}


Element* Numberspool::newInstance() {
    return lisp->provideNumbers();
}

Element* Numberspool::newInstance(Element* v) {
    return lisp->provideNumbers(liste.size(), v->asNumber());
}

Element* Integerspool::newInstance() {
    return lisp->provideIntegers();
}

Element* Integerspool::newInstance(Element* v) {
    return lisp->provideIntegers(liste.size(), v->asInteger());
}

Element* Stringspool::newInstance() {
    return lisp->provideStrings();
}

Element* Listpool::newInstance() {
    return lisp->provideList();
}

Element* Floatpool::fullcopy() {
    if (lisp->preparingthread)
        return new Number(number);
    return lisp->provideFloat(number);
}

Element* Floatpool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    return lisp->provideFloat(number);
}

Element* Floatpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Float(number);
    
    if (!status)
        return this;
    return lisp->provideFloat(number);
}

Element* Numberpool::fullcopy() {
    if (lisp->preparingthread)
        return new Number(number);
    return lisp->provideNumber(number);
}

Element* Numberpool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    return lisp->provideNumber(number);
}

Element* Numberpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Number(number);
    
    if (!status)
        return this;
    return lisp->provideNumber(number);
}

Element* Integerpool::fullcopy() {
    if (lisp->preparingthread)
        return new Integer(integer);
    return lisp->provideInteger(integer);
}

Element* Integerpool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    return lisp->provideInteger(integer);
}

Element* Integerpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Integer(integer);
    
    if (!status)
        return this;
    
    return lisp->provideInteger(integer);
}

Element* Constfloat::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new Number(number);
    return lisp->provideFloat(number);
}

Element* Constfloat::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new Number(number);
    return lisp->provideFloat(number);
}

Element* Constfloat::copyatom(uint16_t s) {
    return (provide)?lisp->provideFloat(number):new Float(number);
}

Element* Constfloat::duplicate_constant(bool pair) {
    return (provide)?lisp->provideFloat(number):new Float(number);
}

Element* Constnumber::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new Number(number);
    return lisp->provideNumber(number);
}

Element* Constnumber::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new Number(number);
    return lisp->provideNumber(number);
}

Element* Constnumber::copyatom(uint16_t s) {
    return (provide)?lisp->provideNumber(number):new Number(number);
}

Element* Constnumber::duplicate_constant(bool pair) {
    return (provide)?lisp->provideNumber(number):new Number(number);
}

Element* Constinteger::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new Integer(integer);
    return lisp->provideInteger(integer);
}

Element* Constinteger::copyatom(uint16_t s) {
    return (provide)?lisp->provideInteger(integer):new Integer(integer);
}

Element* Constinteger::duplicate_constant(bool pair) {
    return (provide)?lisp->provideInteger(integer):new Integer(integer);
}

Element* Constinteger::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new Integer(integer);
    return lisp->provideInteger(integer);
}

Element* Constshort::fullcopy() {
    return new Short(integer);
}

Element* Constshort::copyatom(uint16_t s) {
    return new Short(integer);
}

Element* Constshort::duplicate_constant(bool pair) {
    return new Short(integer);
}

Element* Constshort::copying(bool duplicate) {
    return new Short(integer);
}


Element* Conststring::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Conststring::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Conststring::copyatom(uint16_t s) {
    return (provide)?lisp->provideString(content):new String(content);
}

Element* Conststring::duplicate_constant(bool pair) {
    return (provide)?lisp->provideString(content):new String(content);
}

Element* Stringpool::fullcopy() {
    if (lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Stringpool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    return lisp->provideString(content);
}

Element* Stringpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new String(content);
    
    if (!status)
        return this;
    
    return lisp->provideString(content);
}

Element* Listpool::fullcopy() {
    if (liste.marking)
        return liste.object;
    liste.marking = true;
    if (lisp->preparingthread)
        liste.object = new List;
    else
        liste.object = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        liste.object->append(liste[i]->fullcopy());
    }
    liste.marking = false;
    return liste.object;
}

Element* Listpool::copyatom(uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    List* l = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copyatom(s));
    }
    release();
    return l;
}

Element* Listpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    List* l;
    if (lisp->preparingthread)
        l = new List;
    else {
        if (!is_protected() && liste.nocdr() && !duplicate)
            return this;
        
        l = lisp->provideList();
    }
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copying(false));
    }
    return l;
}

Element* Floatspool::fullcopy() {
    if (lisp->preparingthread)
        return new Floats(this);
    
    return lisp->provideFloats(this);
}

Element* Floatspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Floats(this);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideFloats(this);
}

Element* Floatspool::copyatom(uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Floats* f = lisp->provideFloats(this);
    release();
    return f;
}

Element* Numberspool::fullcopy() {
    if (lisp->preparingthread)
        return new Numbers(this);
    
    return lisp->provideNumbers(this);
}

Element* Numberspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Numbers(this);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideNumbers(this);
}

Element* Numberspool::copyatom(uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Numbers* n = lisp->provideNumbers(this);
    release();
    return n;
}

Element* Integerspool::fullcopy() {
    if (lisp->preparingthread)
        return new Integers(this);
    
    return lisp->provideIntegers(this);
}

Element* Integerspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Integers(this);
    
    //If it is a CDR, we need to copy it...
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideIntegers(this);
}

Element* Integerspool::copyatom(uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Integers* i = lisp->provideIntegers(this);
    release();
    return i;
}

Element* Stringspool::fullcopy() {
    if (lisp->preparingthread)
        return new Strings(this);
    
    return lisp->provideStrings(this);
}

Element* Stringspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Strings(this);
    
    //If it is a CDR, we need to copy it...
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideStrings(this);
}

Element* Stringspool::copyatom(uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    return lisp->provideStrings(this);
}

Element* Setpool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet(this);
}

Element* Set::fullcopy() {
    return new Set(ensemble);
}

Element* Set::copying(bool duplicate) {
    if (exchange_value.provide && exchange_value.lisp->preparingthread)
        return new Set(ensemble);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return new Set(ensemble);
}

Element* Set_n::fullcopy() {
    return new Set_n(ensemble);
}

Element* Set_n::copying(bool duplicate) {
    if (exchange_value.provide && exchange_value.lisp->preparingthread)
        return new Set_n(ensemble);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return new Set_n(ensemble);
}

Element* Set_npool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_n(this);
}

Element* Dictionarypool::newInstance() {
    return lisp->provideDictionary();
}

Element* Dictionarypool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Dictionary* d;
    if (lisp->preparingthread)
        d = new Dictionary;
    else
        d = lisp->provideDictionary();
    object = d;
    Element* e;
    for (auto& a: dictionary) {
        e = a.second->fullcopy();
        d->dictionary[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Dictionarypool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary* d = lisp->provideDictionary();
    Element* e;
    for (auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}


Element* Dictionarypool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->preparingthread && !is_protected() && !duplicate)
        return this;
    
    Dictionary* d;
    if (lisp->preparingthread)
        d = new Dictionary;
    else
        d = lisp->provideDictionary();
    Element* e;
    for (auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

void Dictionarypool::decrement() {
    if (is_protected() || marking)
        return;
    
    marking = true;
    
    status--;
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionary_pool.push_back(this);
    }
    marking = false;
}


void Dictionarypool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    
    status-=nb;
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionary_pool.push_back(this);
    }
    marking = false;
}

void Dictionarypool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (auto& a: dictionary)
            a.second->decrement();
        marking = false;
        dictionary.clear();
        lisp->dictionary_pool.push_back(this);
    }
}

Element* Dictionary_npool::newInstance() {
    return lisp->provideDictionary_n();
}

Element* Dictionary_npool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Dictionary_n* d;
    if (lisp->preparingthread)
        d = new Dictionary_n;
    else
        d = lisp->provideDictionary_n();
    object = d;
    Element* e;
    for (auto& a: dictionary) {
        e = a.second->fullcopy();
        d->dictionary[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Dictionary_npool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->preparingthread && !is_protected() && !duplicate)
        return this;
    Dictionary_n* d;
    if (lisp->preparingthread)
        d = new Dictionary_n;
    else
        d = lisp->provideDictionary_n();
    Element* e;
    for (auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Dictionary_npool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary_n* d = lisp->provideDictionary_n();
    Element* e;
    for (auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

void Dictionary_npool::decrement() {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status--;
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionaryn_pool.push_back(this);
    }
    marking = false;
}


void Dictionary_npool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status-=nb;
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionaryn_pool.push_back(this);
    }
    marking = false;
}

void Dictionary_npool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (auto& a: dictionary)
            a.second->decrement();
        marking = false;
        dictionary.clear();
        lisp->dictionaryn_pool.push_back(this);
    }
}

Element* Setpool::newInstance() {
    return lisp->provideSet();
}

Element* Setpool::fullcopy() {
    if (lisp->preparingthread)
        return new Set(ensemble);
    else
        return lisp->provideSet(this);
}

Element* Setpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->preparingthread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->preparingthread)
        return new Set(ensemble);
    else
        return lisp->provideSet(this);
}

void Setpool::decrement() {
    if (is_protected())
        return;

    status--;
    if (!status) {
        ensemble.clear();
        lisp->set_pool.push_back(this);
    }
}


void Setpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;

    status-=nb;
    if (!status) {
        ensemble.clear();
        lisp->set_pool.push_back(this);
    }
}

void Setpool::release() {
    if (!status) {
        ensemble.clear();
        lisp->set_pool.push_back(this);
    }
}

Element* Set_npool::newInstance() {
    return lisp->provideSet_n();
}

Element* Set_npool::fullcopy() {
    if (lisp->preparingthread)
        return new Set_n(ensemble);
    else
        return lisp->provideSet_n(this);
}

Element* Set_npool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->preparingthread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->preparingthread)
        return new Set_n(ensemble);
    else
        return lisp->provideSet_n(this);
}

void Set_npool::decrement() {
    if (is_protected())
        return;

    status--;
    if (!status) {
        ensemble.clear();
        lisp->setn_pool.push_back(this);
    }
}


void Set_npool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;

    status-=nb;
    if (!status) {
        ensemble.clear();
        lisp->setn_pool.push_back(this);
    }
}

void Set_npool::release() {
    if (!status) {
        ensemble.clear();
        lisp->setn_pool.push_back(this);
    }
}

//------------------------------------------------------------------------------------------
Infinitelist::Infinitelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

Cyclelist::Cyclelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

Dictionary_as_buffer::Dictionary_as_buffer(LispE* lisp) : Element(t_dictionary) {
    dico = lisp->provideDictionary();
    choice = true;
}

//------------------------------------------------------------------------------------------
Element* Floats::check_member(LispE* lisp, Element* the_set) {
    Floats* n = lisp->provideFloats();
    float v;
    long i, j;
    long sz = the_set->size();
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            v = the_set->index(i)->asFloat();
            if (liste[j] == v) {
                n->liste.push_back(1);
                break;
            }
        }
        if (i == sz)
            n->liste.push_back(0);
    }
    return n;
}

Element* Numbers::check_member(LispE* lisp, Element* the_set) {
    Numbers* n = lisp->provideNumbers();
    double v;
    long i, j;
    long sz = the_set->size();
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            v = the_set->index(i)->asNumber();
            if (liste[j] == v) {
                n->liste.push_back(1);
                break;
            }
        }
        if (i == sz)
            n->liste.push_back(0);
    }
    return n;
}

Element* Shorts::check_member(LispE* lisp, Element* the_set) {
    Shorts* n = new Shorts();
    long v;
    long i, j;
    long sz = the_set->size();
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            v = the_set->index(i)->asShort();
            if (liste[j] == v) {
                n->liste.push_back(1);
                break;
            }
        }
        if (i == sz)
            n->liste.push_back(0);
    }
    return n;
}

Element* Integers::check_member(LispE* lisp, Element* the_set) {
    Integers* n = lisp->provideIntegers();
    long v;
    long i, j;
    long sz = the_set->size();
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            v = the_set->index(i)->asInteger();
            if (liste[j] == v) {
                n->liste.push_back(1);
                break;
            }
        }
        if (i == sz)
            n->liste.push_back(0);
    }
    return n;
}

Element* Strings::check_member(LispE* lisp, Element* the_set) {
    Strings* n = lisp->provideStrings();
    long sz = the_set->size();
    std::vector<u_ustring> v;
    long i, j;
    for (i = 0; i < sz; i++)
        v.push_back(the_set->index(i)->asUString(lisp));
    
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            if (liste[j] == v[i]) {
                n->liste.push_back(v[i]);
                break;
            }
        }
        if (i == sz)
            n->liste.push_back(U"");
    }
    return n;
}

Element* List::check_member(LispE* lisp, Element* the_set) {
    List* r = lisp->provideList();
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp,the_set);
        r->append(e);
    }
    return r;
}

Element* LList::check_member(LispE* lisp, Element* the_set) {
    LList* r = new LList(liste.mark);
    Element* e;
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        e = a->value->check_member(lisp,the_set);
        r->append(e);
    }
    return r;
}

Element* Matrice::check_member(LispE* lisp,Element* the_set) {
    Matrice* r = new Matrice;
    r->size_x = size_x;
    r->size_y = size_y;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Matrice_float::check_member(LispE* lisp, Element* the_set) {
    Matrice_float* r = new Matrice_float;
    r->size_x = size_x;
    r->size_y = size_y;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Tenseur::check_member(LispE* lisp, Element* the_set) {
    Tenseur* r = new Tenseur;
    r->shape = shape;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Tenseur_float::check_member(LispE* lisp, Element* the_set) {
    Tenseur_float* r = new Tenseur_float;
    r->shape = shape;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

//------------------------------------------------------------------------------------------
inline bool LIST::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    comparison->liste[1] = item->buffer[i];
    comparison->liste[2] = item->buffer[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void LIST::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                item->swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                item->swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            item->swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            item->swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    item->swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        item->swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    comparison->liste[2] = item->buffer[rmax];
    for (j = rmin; j < rmax; j++) {
        comparison->liste[1] = item->buffer[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            item->swap(pivot,j);
        }
    }
    pivot++;
    item->swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void LIST::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = item->last - home;
    if (sz <= 1)
        return;
    
    sorting(lisp, comparison, comparison->liste[0]->type, home, item->last - 1);
}

bool Floats::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((Float*)comparison->liste[1])->number = liste[i];
    ((Float*)comparison->liste[2])->number = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Floats::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    ((Float*)comparison->liste[2])->number = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((Float*)comparison->liste[1])->number = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void Floats::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = size();
    if (sz <= 1)
        return;
    
    Constfloat n1(0);
    Constfloat n2(0);
    comparison->liste[1] = &n1;
    comparison->liste[2] = &n2;
    
    n1.number = liste[0];
    n2.number = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
    
    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}

bool Numbers::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((Number*)comparison->liste[1])->number = liste[i];
    ((Number*)comparison->liste[2])->number = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Numbers::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    ((Number*)comparison->liste[2])->number = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((Number*)comparison->liste[1])->number = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void Numbers::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = size();
    if (sz <= 1)
        return;
    
    Constnumber n1(0);
    Constnumber n2(0);
    comparison->liste[1] = &n1;
    comparison->liste[2] = &n2;
    
    n1.number = liste[0];
    n2.number = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
    
    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}

bool Integers::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((Integer*)comparison->liste[1])->integer = liste[i];
    ((Integer*)comparison->liste[2])->integer = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Integers::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    ((Integer*)comparison->liste[2])->integer = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((Integer*)comparison->liste[1])->integer = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void Integers::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = size();
    if (sz <= 1)
        return;
    
    Constinteger n1(0);
    Constinteger n2(0);
    comparison->liste[1] = &n1;
    comparison->liste[2] = &n2;
    n1.integer = liste[0];
    n2.integer = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
    
    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}

bool Shorts::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((Short*)comparison->liste[1])->integer = liste[i];
    ((Short*)comparison->liste[2])->integer = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Shorts::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    ((Short*)comparison->liste[2])->integer = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((Short*)comparison->liste[1])->integer = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void Shorts::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = size();
    if (sz <= 1)
        return;
    
    Constshort n1(0);
    Constshort n2(0);
    comparison->liste[1] = &n1;
    comparison->liste[2] = &n2;
    n1.integer = liste[0];
    n2.integer = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
    
    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}

bool Strings::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((String*)comparison->liste[1])->content = liste[i];
    ((String*)comparison->liste[2])->content = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Strings::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    ((String*)comparison->liste[2])->content = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((String*)comparison->liste[1])->content = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void Strings::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = size();
    if (sz <= 1)
        return;
    
    Conststring n1(U"");
    Conststring n2(U"");
    comparison->liste[1] = &n1;
    comparison->liste[2] = &n2;
    n1.content = liste[0];
    n2.content = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
    
    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}
//------------------------------------------------------------------------------------------
Element* Element::minimum(LispE* lisp) {
    throw new Error("Error: cannot find the minimum for this object");
}

Element* List::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v = index(0);
    for (long i = 1; i < liste.size(); i++) {
        if (v->more(lisp, liste[i]) == true_)
            v = liste[i];
    }
    return v->copying(false);
}

Element* LList::minimum(LispE* lisp) {
    if (liste.empty())
        return null_;
    Element* v = liste.front();
    u_link* it = liste.begin();
    it = it->next();
    for (;it != NULL; it = it->next()) {
        if (v->more(lisp, it->value) == true_)
            v = it->value;
    }
    return v->copying(false);
}

Element* Floats::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    float v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideFloat(v);
}

Element* Numbers::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    double v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideNumber(v);
}

Element* Shorts::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    short v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return new Short(v);
}

Element* Integers::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    long v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideInteger(v);
}

Element* Strings::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideString(v);
}

Element* Set::minimum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    u_ustring w;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            w = a;
            first = false;
        }
        else
            if (w < a)
                w = a;
    }
    
    return lisp->provideString(w);
}

Element* Set_n::minimum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    double w = 0;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            w = a;
            first = false;
        }
        else
            if (w < a)
                w = a;
    }
    
    return lisp->provideNumber(w);
}


Element* Dictionary::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_n::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}
//------------------------------------------------------------------------------------------
Element* Element::maximum(LispE* lisp) {
    throw new Error("Error: cannot find the maximum for this object");
}

Element* List::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v = index(0);
    for (long i = 1; i < liste.size(); i++) {
        if (v->less(lisp, liste[i]) == true_)
            v = liste[i];
    }
    return v->copying(false);
}

Element* LList::maximum(LispE* lisp) {
    if (liste.empty())
        return null_;
    Element* v = liste.front();
    u_link* it = liste.begin();
    it = it->next();
    for (;it != NULL; it = it->next()) {
        if (v->less(lisp, it->value) == true_)
            v = it->value;
    }
    return v->copying(false);
}

Element* Floats::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    float v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideFloat(v);
}

Element* Numbers::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    double v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideNumber(v);
}

Element* Shorts::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    short v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return new Short(v);
}

Element* Integers::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    long v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideInteger(v);
}

Element* Strings::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideString(v);
}

Element* Set::maximum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    u_ustring w;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            w = a;
            first = false;
        }
        else
            if (w > a)
                w = a;
    }
    
    return lisp->provideString(w);
}

Element* Set_n::maximum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    double w = 0;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            w = a;
            first = false;
        }
        else
            if (w > a)
                w = a;
    }
    
    return lisp->provideNumber(w);
}

Element* Dictionary::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_n::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}
//------------------------------------------------------------------------------------------

Element* Element::minmax(LispE* lisp) {
    throw new Error("Error: cannot find the minmax for this object");
}

Element* List::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v_min = index(0);
    Element* v_max = v_min;
    for (long i = 1; i < liste.size(); i++) {
        if (v_min->more(lisp, liste[i]) == true_)
            v_min = liste[i];
        else {
            if (v_max->less(lisp, liste[i]) == true_)
                v_max = liste[i];
        }
    }
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

Element* LList::minmax(LispE* lisp) {
    if (liste.empty())
        return null_;
    Element* v_min = liste.front();
    Element* v_max = v_min;
    u_link* it = liste.begin();
    it = it->next();
    for (; it != NULL; it = it->next()) {
        if (v_min->more(lisp, it->value) == true_)
            v_min = it->value;
        else {
            if (v_max->less(lisp, it->value) == true_)
                v_max = it->value;
        }
    }
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

Element* Floats::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    float v_min = liste[0];
    float v_max = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v_min > liste[i])
            v_min = liste[i];
        else {
            if (v_max < liste[i])
                v_max = liste[i];
        }
    }
    Floats* f = lisp->provideFloats();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Numbers::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    double v_min = liste[0];
    double v_max = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v_min > liste[i])
            v_min = liste[i];
        else {
            if (v_max < liste[i])
                v_max = liste[i];
        }
    }
    Numbers* f = lisp->provideNumbers();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Shorts::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    short v_min = liste[0];
    short v_max = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v_min > liste[i])
            v_min = liste[i];
        else {
            if (v_max < liste[i])
                v_max = liste[i];
        }
    }
    Shorts* f = new Shorts();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Integers::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    long v_min = liste[0];
    long v_max = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v_min > liste[i])
            v_min = liste[i];
        else {
            if (v_max < liste[i])
                v_max = liste[i];
        }
    }
    Floats* f = lisp->provideFloats();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Strings::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring v_min = liste[0];
    u_ustring v_max = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v_min > liste[i])
            v_min = liste[i];
        else {
            if (v_max < liste[i])
                v_max = liste[i];
        }
    }
    Strings* f = lisp->provideStrings();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Set::minmax(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    u_ustring v_min;
    u_ustring v_max;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            v_min = a;
            v_max = a;
            first = false;
        }
        else {
            if (v_max < a)
                v_max = a;
            else
                if (v_min > a)
                    v_min = a;
        }
    }
    Strings* f = lisp->provideStrings();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Set_n::minmax(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    double v_min = 0;
    double v_max = 0;
    bool first = true;
    
    for (auto& a : ensemble) {
        if (first) {
            v_min = a;
            v_max = a;
            first = false;
        }
        else {
            if (v_max < a)
                v_max = a;
            else
                if (v_min > a)
                    v_min = a;
        }
    }
    Numbers* f = lisp->provideNumbers();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

Element* Dictionary::minmax(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (auto& a : dictionary) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second) == true_)
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second) == true_)
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

Element* Dictionary_n::minmax(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (auto& a : dictionary) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second) == true_)
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second) == true_)
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}
//------------------------------------------------------------------------------------------
void Element::flatten(LispE* lisp, List* l) {
    l->append(this);
}

void Element::flatten(LispE* lisp, Numbers* l) {
    l->append(this);
}

void Element::flatten(LispE* lisp, Floats* l) {
    l->append(this);
}

void List::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        liste[i]->flatten(lisp, l);
    }
}

void List::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        liste[i]->flatten(lisp, l);
    }
}

void List::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        liste[i]->flatten(lisp, l);
    }
}

void LList::flatten(LispE* lisp, List* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        a->value->flatten(lisp, l);
    }
}

void LList::flatten(LispE* lisp, Numbers* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        a->value->flatten(lisp, l);
    }
}

void LList::flatten(LispE* lisp, Floats* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        a->value->flatten(lisp, l);
    }
}

void Floats::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideFloat(liste[i]));
    }
}

void Floats::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Floats::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}


void Numbers::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideNumber(liste[i]));
    }
}

void Numbers::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Numbers::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Integers::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideInteger(liste[i]));
    }
}

void Integers::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Integers::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Shorts::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(new Short(liste[i]));
    }
}

void Shorts::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Shorts::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Strings::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideString(liste[i]));
    }
}

void Strings::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(convertingfloathexa(liste[i].c_str()));
    }
}

void Strings::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(convertingfloathexa(liste[i].c_str()));
    }
}

void Set::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    u_ustring k;
    for (auto& a : ensemble) {
        k = a;
        l->append(lisp->provideString(k));
    }
}

void Set_n::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    for (auto& a : ensemble) {
        l->append(lisp->provideNumber(a));
    }
}


void Dictionary::flatten(LispE* lisp, List* l) {
    u_ustring k;
    for (auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideString(k));
        a.second->flatten(lisp, l);
    }
}

void Dictionary_n::flatten(LispE* lisp, List* l) {
    double k;
    for (auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideNumber(k));
        a.second->flatten(lisp, l);
    }
}

//------------------------------------------------------------------------------------------
Element* List::transposed(LispE* lisp) {
    vecte<long> sz;
    getShape(sz);
    if (sz.size() <= 1)
        return this;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    Element* tenseur;
    if (sz.size() == 2)
        tenseur = new Matrice(lisp, sz[0], sz[1], 0.0);
    else
        tenseur = new Tenseur(lisp, sz, zero_);
    
    Element* e;
    for (i = 0; i < sz[1]; i++) {
        e = liste[i];
        for (long j = 0; j < sz[0]; j++) {
            tenseur->index(j)->replacing(i, e->index(j)->copying(false));
        }
    }
    
    return tenseur;
}

Element* Matrice::transposed(LispE* lisp) {
    Matrice* transposed_matrix = new Matrice(lisp, size_y, size_x, 0.0);
    long i, j = 0;
    
    Element* e;
    for (i = 0; i < size_x; i++) {
        e = liste[i];
        for (j = 0; j < size_y; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Matrice_float::transposed(LispE* lisp) {
    Matrice_float* transposed_matrix = new Matrice_float(lisp, size_y, size_x, 0.0);
    long i, j = 0;
    
    Element* e;
    for (i = 0; i < size_x; i++) {
        e = liste[i];
        for (j = 0; j < size_y; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Tenseur::transposed(LispE* lisp) {
    vecte<long> sz;
    sz = shape;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    
    Tenseur* transposed_matrix = new Tenseur(lisp, sz, zero_);
    long j = 0;
    
    Element* e;
    for (i = 0; i < shape[0]; i++) {
        e = liste[i];
        for (j = 0; j < shape[1]; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Tenseur_float::transposed(LispE* lisp) {
    vecte<long> sz;
    sz = shape;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    
    Tenseur_float* transposed_matrix = new Tenseur_float(lisp, sz, zero_);
    long j = 0;
    
    Element* e;
    for (i = 0; i < shape[0]; i++) {
        e = liste[i];
        for (j = 0; j < shape[1]; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}
//------------------------------------------------------------------------------------------

void List::storevalue(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void List::storevalue(LispE* lisp,long v) {
    append(lisp->provideInteger(v));
}

void List::storevalue(LispE* lisp, u_ustring& v) {
    append(lisp->provideString(v));
}

void Floats::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Floats::storevalue(LispE* lisp, float v) {
    liste.push_back(v);
}

void Floats::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Floats::storevalue(LispE* lisp, u_ustring& s) {
    long l;
    float v = convertingfloathexa((u_uchar*)s.c_str(), l);
    liste.push_back(v);
}

void Numbers::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Numbers::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Numbers::storevalue(LispE* lisp, u_ustring& s) {
    long l;
    double v = convertingfloathexa((u_uchar*)s.c_str(), l);
    liste.push_back(v);
}

void Integers::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Integers::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Integers::storevalue(LispE* lisp, u_ustring& s) {
    long v = convertinginteger(s);
    liste.push_back(v);
}

void Shorts::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Shorts::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Shorts::storevalue(LispE* lisp, u_ustring& s) {
    long v = convertinginteger(s);
    liste.push_back(v);
}


void Strings::storevalue(LispE* lisp, double v) {
    liste.push_back(convertToUString(v));
}

void Strings::storevalue(LispE* lisp,long v) {
    liste.push_back(convertToUString(v));
}

void Strings::storevalue(LispE* lisp, u_ustring& s) {
    liste.push_back(s);
}

//------------------------------------------------------------------------------------------


void Element::prettyfying(LispE* lisp, string& code) {
    if (isList()) {
        if (size() == 0) {
            code += " ()";
            return;
        }
        
        short type = index(0)->type;
        if (type == l_lambda) {
            code += " ";
            code += toString(lisp);
            return;
        }
        
        Element* params;
        
        if (type == l_defun || type == l_defpat || type == l_deflib) {
            code += "(";
            code += lisp->toString(type);
            code += " ";
            code += index(1)->toString(lisp);
            code += " ";
            params = index(2);
            if (type == l_defpat) {
                code += "(";
                string local;
                for (long i = 0; i < params->size(); i++) {
                    if (i)
                        code += " ";
                    local = index(i)->toString(lisp);
                    if (local[0] == '(') {
                        local[0] = '[';
                        local.back() = ']';
                    }
                    code += local;
                }
                code += ")";
            }
            else
                code += params->toString(lisp);
            code += "\n";
            for (long i = 3; i < size(); i++) {
                index(i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }

        if (type == l_loop || type == l_multiloop || type == l_polyloop) {
            code += "(";
            //The command name
            code += lisp->toString(type);
            code += " ";
            //Iterative variables
            code += index(1)->toString(lisp);
            code += " ";
            params = index(2);
            if (params->isList()) {
                code += "\n";
                params->prettyfying(lisp, code);
            }
            else {
                code += params->toString(lisp);
                code += "\n";
            }
            for (long i = 3; i < size(); i++) {
                index(i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }

        long i = 0;
        
        
        if (type == l_if || type == l_check || type == l_ncheck || type == l_ife) {
            code += "(";
            code += index(i++)->toString(lisp);
            code += " ";
            code += index(i++)->toString(lisp);
            code += "\n";
            for (; i < size(); i++) {
                index(i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }
        
        string local = toString(lisp);
        if (local.size() < 50) {
            code += local;
            code += "\n";
            return;
        }
        
        code += "(";
        
        if (type == l_while || type == l_setq || type == l_setg || type == l_loopcount || type == l_key || type == l_keyn) {
            code += index(i++)->toString(lisp);
            code += " ";
            code += index(i++)->toString(lisp);
            code += "\n";
        }
        else {
            if (type > t_error && type < l_final) {
                code += index(i++)->toString(lisp);
                i = 1;
            }
            code += "\n";
        }
        
        for (; i < size(); i++) {
            index(i)->prettyfying(lisp, code);
            if (code.back() != '\n')
                code += "\n";
        }
        code += ")\n";
        return;
    }
    if (isString())
        code += jsonstring(toString(lisp));
    else {
        if (isDictionary()) {
            string local = toString(lisp);
            if (local.size() < 50) {
                code += local;
                return;
            }
            code += "{\n";
            if (type == t_dictionary) {
                map<u_ustring, Element*>& dico = ((Dictionary*)this)->dictionary;
                u_ustring key;
                for (auto& a: dico) {
                    local = "";
                    key = a.first;
                    s_unicode_to_utf8(local, key);
                    code += local;
                    code += ":";
                    a.second->prettyfying(lisp, code);
                    if (code.back() != '\n')
                        code += "\n";
                }
                code += "}\n";
                return;
            }
            unordered_map<double, Element*>& dico = ((Dictionary_n*)this)->dictionary;
            for (auto& a: dico) {
                local = convertToString(a.first);
                code += local;
                code += ":";
                a.second->prettyfying(lisp, code);
                if (code.back() != '\n')
                    code += "\n";
            }
            code += "}\n";
            return;
        }
        else
            code += toString(lisp);
    }
}

//(defpat action ( [Take 'x] [Take y] )(if (check_object position x) (block (push belongings x) (println "Ok we have picked up" x)) (println "Cannot pick up the" x)))
//(prettify '((12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10)))

string Element::prettify(LispE* lisp) {
    string code;
    prettyfying(lisp, code);
    string body;
    IndentCode(code, body, GetBlankSize(), true, false);
    return body;
}

//------------------------------------------------------------------------------------------
Element* Element::invert_sign(LispE* lisp) {
    throw new Error("Error: Cannot invert this object");
}

Element* Float::invert_sign(LispE* lisp) {
    if (!status) {
        number *= -1;
        return this;
    }
    return lisp->provideFloat(number * -1);
}

Element* Number::invert_sign(LispE* lisp) {
    if (!status) {
        number *= -1;
        return this;
    }
    return lisp->provideNumber(number * -1);
}

Element* Short::invert_sign(LispE* lisp) {
    if (!status) {
        integer *= -1;
        return this;
    }
    return new Short(integer * -1);
}

Element* Integer::invert_sign(LispE* lisp) {
    if (!status) {
        integer *= -1;
        return this;
    }
    return lisp->provideInteger(integer * -1);
}

Element* Floats::invert_sign(LispE* lisp) {
    Floats* n = this;
    if (status)
        n = lisp->provideFloats(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

Element* Numbers::invert_sign(LispE* lisp) {
    Numbers* n = this;
    if (status)
        n = lisp->provideNumbers(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

Element* Shorts::invert_sign(LispE* lisp) {
    Shorts* n = this;
    if (status)
        n = new Shorts(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

Element* Integers::invert_sign(LispE* lisp) {
    Integers* n = this;
    if (status)
        n = lisp->provideIntegers(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

//------------------------------------------------------------------------------------------
//This method returns the needed instructions to build the dictionary
Element* Dictionary_as_list::dictionary(LispE* lisp) {
    Element* keycmd;
    List* last_element = lisp->provideList();
    
    if (!choice || keyvalues.size() != valuevalues.size()) {
        if (valuevalues.size() == 0) {
            //We create a set...
            if (type == t_float || type == t_number)
                keycmd = lisp->delegation->_DICO_SETN;
            else {
                if (type == t_short || type == t_integer)
                    keycmd = lisp->delegation->_DICO_SETI;
                else
                    keycmd = lisp->delegation->_DICO_SET;
            }
            //We generate: (set k v k' v' k" v"...)
            last_element->append(keycmd);
            for (long i = 0; i < keyvalues.size(); i++)
                last_element->append(keyvalues[i]);
        }
        else {
            last_element->release();
            throw new Error("Error: dictionary has a different number of key/value");
        }
    }
    else {
        if (type >= t_float && type <= t_number)
            keycmd = lisp->delegation->_DICO_KEYN;
        else
            keycmd = lisp->delegation->_DICO_KEY;
        
        //We generate: (key (key) k v k' v' k" v"...)
        last_element->append(keycmd);
        last_element->append(lisp->provideList());
        last_element->liste[1]->append(keycmd);
        for (long i = 0; i < keyvalues.size(); i++) {
            last_element->append(keyvalues[i]);
            last_element->append(valuevalues[i]);
        }
    }
    
    try {
        //See below for some explanations
        keycmd = last_element->eval(lisp);
        last_element->release();
        return keycmd;
    }
    catch(Error* err) {
        err->release();
        return last_element;
    }
}

//A little bit of explanation here
//When a dictionary is created with actual values in its description.
//It has no variables for instance, then we create the actual dictionary by calling eval on (key ...)
//However, the values in it can be regular values that must be stored in the garbage.
//However, there is a catch, when using eval on a string, we will store these values here in the garbage.
//We do not want these values to be protected... However, they will be removed from the garbage in eval and
//never be deleted. We set their status with a specific value: s_inside to avoid this problem.
void Dictionary::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (auto& a : dictionary) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

void Dictionary_n::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (auto& a : dictionary) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

void List::garbaging_values(LispE* lisp) {
    if (liste.marking)
        return;
    liste.marking = true;
    for (long i = 0; i < liste.size(); i++) {
        lisp->control_garbaging(liste[i]);
        liste[i]->garbaging_values(lisp);
    }
    liste.marking = false;
}

void LList::garbaging_values(LispE* lisp) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        lisp->control_garbaging(a->value);
        a->value->garbaging_values(lisp);
    }
}

//------------------------------------------------------------------------------------------
void List::append(LispE* lisp, u_ustring& k) {
    append(lisp->provideString(k));
}

void LList::append(LispE* lisp, u_ustring& k) {
    append(lisp->provideString(k));
}

void Floats::append(LispE* lisp, u_ustring& k) {
    long l;
    float d = convertingfloathexa((u_uchar*)k.c_str(), l);
    liste.push_back(d);
}

void Numbers::append(LispE* lisp, u_ustring& k) {
    long l;
    double d = convertingfloathexa((u_uchar*)k.c_str(), l);
    liste.push_back(d);
}

void Integers::append(LispE* lisp, u_ustring& k) {
    long d = convertinginteger(k);
    liste.push_back(d);
}

void Shorts::append(LispE* lisp, u_ustring& k) {
    short d = (short)convertinginteger(k);
    liste.push_back(d);
}

void Strings::append(LispE* lisp, u_ustring& k) {
    liste.push_back(k);
}

void List::append(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void LList::append(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void Floats::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Floats::append(LispE* lisp, float v) {
    liste.push_back(v);
}

void Numbers::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Integers::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Shorts::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Strings::append(LispE* lisp, double v) {
    liste.push_back(convertToUString(v));
}

void Set::append(LispE* lisp, double v) {
    ensemble.insert(convertToUString(v));
}

void List::append(LispE* lisp, long v) {
    append(lisp->provideInteger(v));
}

void LList::append(LispE* lisp, long v) {
    append(lisp->provideInteger(v));
}

void Floats::append(LispE* lisp, long v) {
    liste.push_back(v);
}

void Numbers::append(LispE* lisp, long v) {
    liste.push_back(v);
}

void Integers::append(LispE* lisp, long v) {
    liste.push_back(v);
}

void Shorts::append(LispE* lisp, long v) {
    liste.push_back(v);
}

void Strings::append(LispE* lisp, long v) {
    liste.push_back(convertToUString(v));
}

void Set::append(LispE* lisp, long v) {
    ensemble.insert(convertToUString(v));
}

void Dictionary_as_list::append(LispE* lisp, u_ustring& k) {
    append(lisp->provideString(k));
}

void Dictionary_as_list::append(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void Dictionary_as_list::append(LispE* lisp, long v) {
    append(lisp->provideInteger(v));
}

void Dictionary_as_buffer::append(LispE* lisp, u_ustring& k) {
    if (choice)
        key = k;
    else {
        dico->dictionary[key] = lisp->provideString(k);
        reversechoice();
    }
}

void Dictionary_as_buffer::append(LispE* lisp, double v) {
    if (choice)
        key = convertToUString(v);
    else {
        dico->dictionary[key] = lisp->provideNumber(v);
        reversechoice();
    }
}

void Dictionary_as_buffer::append(LispE* lisp, long v) {
    if (choice)
        key = convertToUString(v);
    else {
        dico->dictionary[key] = lisp->provideInteger(v);
        reversechoice();
    }
}

void Dictionarypool::append(LispE* lisp, u_ustring& k) {
    if (choice)
        u_key = k;
    else {
        Element* e = lisp->provideString(k);
        dictionary[u_key] = e;
        e->increment();
        reversechoice();
    }
}

void Dictionarypool::append(LispE* lisp, double v) {
    if (choice)
        u_key = convertToUString(v);
    else {
        Element* e = lisp->provideNumber(v);
        dictionary[u_key] = e;
        e->increment();
        reversechoice();
    }
}

void Dictionarypool::append(LispE* lisp, long v) {
    if (choice)
        u_key = convertToUString(v);
    else {
        Element* e = lisp->provideInteger(v);
        dictionary[u_key] = e;
        e->increment();
        reversechoice();
    }
}

//------------------------------------------------------------------------------------------
Element* Element::rank(LispE* lisp, vecte<long>& positions) {
    throw new Error("Error: cannot apply 'rank' to this element");
}

Element* Matrice_float::rank(LispE* lisp, vecte<long>& positions) {
    short sz = positions.size();
    if (!sz || sz > 2)
        throw new Error("Error: index mismatch");
        
    if (positions[0] == -1) {
        //We return all columns
        if (sz == 1 || positions[1] == -1) {
            Matrice_float* m = new Matrice_float();
            Floats* result;
            
            for (long j = 0; j < size_y; j++) {
                result = lisp->provideFloats();
                for (long i = 0; i < size_x; i++) {
                    result->liste.push_back(val(i,j));
                }
                m->append(result);
            }
            m->size_x = size_y;
            m->size_y = size_x;
            return m;
        }
        else {
            if (positions[1] >= size_y)
                throw new Error("Error: indexes out of bounds");
        }
    }
    else {
        if (sz == 2 && positions[1] != -1) {
            if (positions[1] >= size_y)
                throw new Error("Error: indexes out of bounds");
            return lisp->provideFloat(val(positions[0], positions[1]));
        }
        
        if (positions[0] >= size_x)
            throw new Error("Error: indexes out of bounds");
        
        return lisp->provideFloats(((Floats*)liste[positions[0]]));
    }

    Floats* result = lisp->provideFloats();
    for (long i = 0; i < size_x; i++) {
        result->liste.push_back(val(i,positions[1]));
    }
    return result;
}

Element* Matrice::rank(LispE* lisp, vecte<long>& positions) {
    short sz = positions.size();
    if (!sz || sz > 2)
        throw new Error("Error: index mismatch");
        
    if (positions[0] == -1) {
        //We return all columns
        if (sz == 1 || positions[1] == -1) {
            Matrice* m = new Matrice();
            Numbers* result;
            
            for (long j = 0; j < size_y; j++) {
                result = lisp->provideNumbers();
                for (long i = 0; i < size_x; i++) {
                    result->liste.push_back(val(i,j));
                }
                m->append(result);
            }
            m->size_x = size_y;
            m->size_y = size_x;
            return m;
        }
        else {
            if (positions[1] >= size_y)
                throw new Error("Error: indexes out of bounds");
        }
    }
    else {
        if (sz == 2 && positions[1] != -1) {
            if (positions[1] >= size_y)
                throw new Error("Error: indexes out of bounds");
            return lisp->provideNumber(val(positions[0], positions[1]));
        }
        
        if (positions[0] >= size_x)
            throw new Error("Error: indexes out of bounds");
        
        return lisp->provideNumbers(((Numbers*)liste[positions[0]]));
    }

    Numbers* result = lisp->provideNumbers();
    for (long i = 0; i < size_x; i++) {
        result->liste.push_back(val(i,positions[1]));
    }
    return result;
}

Element* Tenseur::storeRank(LispE* lisp, Element* current, vecte<long>& positions, long idx) {
    long nb = shape.size() - idx;
    long p_idx = -1;
    if (idx < positions.size())
        p_idx = positions[idx];
    
    long i;
    if (p_idx == -1) {
        //If nb == 2, then it is a matrix
        //If nb == 1, then it is a vector
        if (nb == 1)
            return lisp->provideNumbers((Numbers*)current);
        
        List* m = lisp->provideList();

        if (nb == 2) {
            Numbers* result;
            for (long j = 0; j < shape[idx+1]; j++) {
                result = lisp->provideNumbers();
                for (i = 0; i < shape[idx]; i++) {
                    result->liste.push_back(current->index(i)->index(j)->asNumber());
                }
                m->append(result);
            }
        }
        else {
            for (i = 0; i < shape[idx]; i++) {
                m->append(storeRank(lisp, current->index(i), positions, idx+1));
            }
        }
        return m;
    }
    
    if (nb == 1)
        return current->index(p_idx);
    
    return storeRank(lisp, current->index(p_idx), positions, idx+1);
}

Element* Tenseur_float::storeRank(LispE* lisp, Element* current, vecte<long>& positions, long idx) {
    long nb = shape.size() - idx;
    long p_idx = -1;
    if (idx < positions.size())
        p_idx = positions[idx];
    
    long i;
    if (p_idx == -1) {
        //If nb == 2, then it is a matrix
        //If nb == 1, then it is a vector
        if (nb == 1)
            return lisp->provideFloats((Floats*)current);
        
        List* m = lisp->provideList();

        if (nb == 2) {
            Floats* result;
            for (long j = 0; j < shape[idx+1]; j++) {
                result = lisp->provideFloats();
                for (i = 0; i < shape[idx]; i++) {
                    result->liste.push_back(current->index(i)->index(j)->asFloat());
                }
                m->append(result);
            }
        }
        else {
            for (i = 0; i < shape[idx]; i++) {
                m->append(storeRank(lisp, current->index(i), positions, idx+1));
            }
        }
        return m;
    }
    
    if (nb == 1)
        return current->index(p_idx);
    
    return storeRank(lisp, current->index(p_idx), positions, idx+1);}


Element* Tenseur::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    short sz = positions.size();
    if (!sz || sz > shape.size())
        throw new Error("Error: index mismatch");

    while (positions.size() > 1 && positions.back() < 0)
        positions.pop_back();
    
    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            throw new Error("Error: indexes out of bounds");
    }
    
    Element* res = storeRank(lisp, this, positions, 0);
    if (res->type == t_numbers)
        return res;
    
    if (res->type == t_number)
        return lisp->provideFloat(res->asNumber());
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_floats) {
        Matrice* m = new Matrice((List*)res);
        res->release();
        return m;
    }
    Tenseur* ts = new Tenseur(lisp, (List*)res);
    res->release();
    return ts;
}

Element* Tenseur_float::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    short sz = positions.size();
    if (!sz || sz > shape.size())
        throw new Error("Error: index mismatch");

    while (positions.size() > 1 && positions.back() < 0)
        positions.pop_back();

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            throw new Error("Error: indexes out of bounds");
    }
    
    Element* res = storeRank(lisp, this, positions, 0);
    if (res->type == t_floats)
        return res;
    
    if (res->type == t_float)
        return lisp->provideFloat(res->asFloat());
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_floats) {
        Matrice_float* m = new Matrice_float((List*)res);
        res->release();
        return m;
    }
    Tenseur_float* ts = new Tenseur_float(lisp, (List*)res);
    res->release();
    return ts;
}

Element* List::storeRank(LispE* lisp, Element* current, vecte<long>& shape, vecte<long>& positions, long idx) {
    long nb = shape.size() - idx;
    long p_idx = -1;
    if (idx < positions.size())
        p_idx = positions[idx];
    
    long i;
    if (p_idx == -1) {
        //If nb == 2, then it is a matrix
        //If nb == 1, then it is a vector
        if (nb == 1)
            return current;
        
        List* m = lisp->provideList();

        if (nb == 2) {
            Element* result;
            Element* ref = current->index(0);
            if (ref->isValueList()) {
                for (long j = 0; j < shape[idx+1]; j++) {
                    result = ref->newInstance();
                    for (i = 0; i < shape[idx]; i++) {
                        result->append(current->index(i)->index(j));
                    }
                    m->append(result);
                }
            }
            else {
                for (long j = 0; j < shape[idx+1]; j++) {
                    result = ref->newInstance();
                    for (i = 0; i < shape[idx]; i++) {
                        result->append(current->index(i)->index(j)->copying(false));
                    }
                    m->append(result);
                }
            }
        }
        else {
            for (i = 0; i < shape[idx]; i++) {
                m->append(storeRank(lisp, current->index(i), shape, positions, idx+1));
            }
        }
        return m;
    }
    
    if (nb == 1)
        return current->index(p_idx)->copying(false);
    
    return storeRank(lisp, current->index(p_idx), shape, positions, idx+1);
}

Element* List::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    while (positions.size() > 1 && positions.back() < 0)
        positions.pop_back();
    
    vecte<long> shape;
    getShape(shape);
    if (!checkShape(0, shape))
        throw new Error("Error: unregular matrix: some sub-lists have different sizes");
    
    short sz = positions.size();
    if (!sz || sz > shape.size())
        throw new Error("Error: index mismatch");

    while (positions.size() > 1 && positions.back() < 0)
        positions.pop_back();

    return storeRank(lisp, this, shape, positions, 0);
}

//------------------------------------------------------------------------------------------

Element* Element::loop(LispE* lisp, short label,  List* code) {
    return null_;
}

Element* String::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    long i = 0;
    u_ustring localvalue;
    long szc = content.size();
    while (i < szc) {
        lisp->handlingutf8->getchar(content, localvalue, i, szc);
        element = lisp->provideString(localvalue);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            //the break is local, the return is global to a function
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* LList::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        element = a->value->copying(false);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* List::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = liste[i]->copying(false);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Matrice::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < size_x; i++) {
        lisp->replacingvalue(liste[i], label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Matrice_float::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < size_x; i++) {
        lisp->replacingvalue(liste[i], label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Tenseur::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < shape[0]; i++) {
        lisp->replacingvalue(liste[i], label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Tenseur_float::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < shape[0]; i++) {
        lisp->replacingvalue(liste[i], label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Rankloop::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* ranks = null_;
    
    for (long i = 0; i < max_iterator; i++) {
        if (last)
            ranks = lst->rank(lisp, positions);
        else {
            positions.push_back(i);
            ranks = lst->rank(lisp, positions);
            positions.pop_back();
        }
        lisp->replacingvalue(ranks, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Floats::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Float* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideFloat(liste[i]);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}


Element* Numbers::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Number* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideNumber(liste[i]);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Shorts::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Integer* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideInteger(liste[i]);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Integers::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Integer* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideInteger(liste[i]);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Set::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    String* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    u_ustring u;
    for (auto & a: ensemble) {
        u = a;
        element = lisp->provideString(u);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Set_n::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Number* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        element = lisp->provideNumber(a);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Strings::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    String* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideString(liste[i]);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* InfiniterangeNumber::loop(LispE* lisp, short label, List* code) {
    if (!increment)
        throw new Error("Error: increment cannot be 0");
    long i_loop;
    Element* e = null_;
    long sz = code->liste.size();
    double value = initial_value;
    
    Number* element;
    lisp->recording(null_, label);
    
    char check = 0;
    if (!infinite_loop) {
        if (increment < 0)
            check = -1;
        else
            check = 1;
    }
    
    while (!lisp->hasStopped() && compare(check, value)) {
        element = lisp->provideNumber(value);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        value += increment;
    }
    return e;
}

Element* InfiniterangeInteger::loop(LispE* lisp, short label, List* code) {
    if (!increment)
        throw new Error("Error: increment cannot be 0");
    
    long i_loop;
    Element* e = null_;
    long sz = code->liste.size();
    long value = initial_value;
    Integer* element;
    lisp->recording(null_, label);
    char check = 0;
    if (!infinite_loop) {
        if (increment < 0)
            check = -1;
        else
            check = 1;
    }
    
    while (!lisp->hasStopped() && compare(check, value)) {
        element = lisp->provideInteger(value);
        lisp->replacingvalue(element, label);
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        value += increment;
    }
    return e;
}


Element* Infinitelist::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    lisp->recording(element, label);
    
    while (!lisp->hasStopped()) {
        _releasing(e);
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Cyclelist::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    if (!element->isList()) {
        element->release();
        throw new Error("Error: we can only cycle on list");
    }
    List* values = (List*)element;
    long sze = values->liste.size();
    long i = 0;
    //We then execute our instructions
    while (!lisp->hasStopped()) {
        element = values->liste[i]->copying(false);
        lisp->replacingvalue(element, label);
        _releasing(e);
        if ((++i) >= sze)
            i = 0;
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}



Element* Dictionary::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    
    String* element;
    lisp->recording(null_, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    Strings* _keys = lisp->provideStrings();
    for (auto& a: dictionary)
        _keys->liste.push_back(a.first);
    try {
        for (long i = 0; i < _keys->size(); i++) {
            element = lisp->provideString(_keys->liste[i]);
            lisp->replacingvalue(element, label);
            _releasing(e);
            //We then execute our instructions
            for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
                e->release();
                e = code->liste[i_loop]->eval(lisp);
            }
            if (e->type == l_return) {
                _keys->release();
                if (e->isBreak())
                    return null_;
                return e;
            }
        }
    }
    catch(Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
}

Element* Dictionary_n::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    
    lisp->recording(null_, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    Numbers* _keys = lisp->provideNumbers();
    for (auto& a: dictionary)
        _keys->liste.push_back(a.first);
    try {
        for (long a_key = 0; a_key < _keys->liste.size(); a_key++) {
            lisp->replacingvalue(lisp->provideNumber(_keys->liste[a_key]), label);
            _releasing(e);
            //We then execute our instructions
            for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
                e->release();
                e = code->liste[i_loop]->eval(lisp);
            }
            if (e->type == l_return) {
                _keys->release();
                if (e->isBreak())
                    return null_;
                return e;
            }
        }
    }
    catch(Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
    
}

/*
 In this case variables are in a list...
 (loop (x y z) lx ly lz ...)
 
 (loop [x y z] '(1 3 4) '(5 6 7) '(9 10 13) (println (+ x y z)))
 */
Element* List::multiloop(LispE* lisp) {
    List* values = lisp->provideList();
    List* indexes = lisp->provideList();
    Element* e = null_;
    Element* ix;
    long sz = size();
    long var;
    long indexe = 0;
    long nbvars = liste[1]->size();
    
    short label;
    
    try {
        for (var = 0; var < nbvars; var++) {
            label = liste[1]->index(var)->label();
            lisp->recording(null_, label);
            e = liste[var+2]->eval(lisp);
            values->append(e);
            indexes->append(e->thekeys(lisp));
        }
        
        long nb = values->liste[0]->size();
        e = null_;
        while (indexe < nb) {
            _releasing(e);
            for (var = 0; var < nbvars; var++) {
                ix = indexes->liste[var]->index(indexe);
                e = values->liste[var]->value_on_index(lisp, ix);
                label = liste[1]->index(var)->label();
                lisp->replacingvalue(e, label);
            }
            //We then execute our instructions
            e = null_;
            for (var = nbvars + 2; var < sz && e->type != l_return; var++) {
                e->release();
                e = liste[var]->eval(lisp);
            }
            if (e->type == l_return) {
                indexes->release();
                if (e->isBreak()) {
                    values->release();
                    return null_;
                }
                values->release(e->eval(lisp));
                return e;
            }
            indexe++;
        }
    }
    catch(Error* err) {
        values->release();
        indexes->release();
        throw err;
    }
    
    values->release(e);
    indexes->release();
    return e;
}

Element* List::polyloop(LispE* lisp) {
    List* values = lisp->provideList();
    Element* e = null_;
    long sz = size();
    long var;
    long indexe = 0;
    long nbvars = liste[1]->size();
    
    short label;
    
    try {
        for (var = 0; var < nbvars; var++) {
            label = liste[1]->index(var)->label();
            lisp->recording(null_, label);
            values->append(liste[var + 2]->eval(lisp));
        }
        
        long nb = values->liste[0]->size();
        
        while (indexe < nb) {
            _releasing(e);
            for (var = 0; var < nbvars; var++) {
                e = values->liste[var]->value_from_index(lisp, indexe);
                label = liste[1]->index(var)->label();
                lisp->replacingvalue(e, label);
            }
            e = null_;
            //We then execute our instructions
            for (var = nbvars + 2; var < sz && e->type != l_return; var++) {
                e->release();
                e = liste[var]->eval(lisp);
            }
            if (e->type == l_return) {
                if (e->isBreak()) {
                    values->release();
                    return null_;
                }
                values->release(e->eval(lisp));
                return e;
            }
            indexe++;
        }
    }
    catch(Error* err) {
        values->release();
        throw err;
    }
    
    values->release(e);
    return e;
}

//------------------------------------------------------------------------------------------
Element* s_findall(LispE* lisp, wstring& s, wstring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return emptylist_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return emptylist_;
    Integers* liste = lisp->provideIntegers();
    while (pos != -1) {
        liste->liste.push_back(pos);
        pos=s.find(sub,pos+sz);
    }
    return liste;
}

Element* s_findall(LispE* lisp, u_ustring& s, u_ustring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return emptylist_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return emptylist_;
    Integers* liste = lisp->provideIntegers();
    while (pos != -1) {
        liste->liste.push_back(pos);
        pos=s.find(sub,pos+sz);
    }
    return liste;
}

Element* s_count(LispE* lisp, u_ustring& s, u_ustring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return zero_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return zero_;
    long nb = 0;
    while (pos != -1) {
        nb++;
        pos=s.find(sub,pos+sz);
    }
    return lisp->provideInteger(nb);
}


//------------------------------------------------------------------------------------------
Element* Element::insert(LispE* lisp, Element* e, long ix) {
    return null_;
}

Element* String::insert(LispE* lisp, Element* e, long ix) {
    u_ustring res;
    if (ix < 0)
        res = lisp->handlingutf8->u_insert_sep(content, e->asUString(lisp));
    else {
        if (ix >= content.size())
            res = content + e->asUString(lisp);
        else {
            res = content;
            res.insert(ix, e->asUString(lisp));
        }
    }
    return lisp->provideString(res);
}

Element* List::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    e = e->copying(false);
    List* l = (List*)duplicate_constant();
    l->liste.insert(ix, e);
    return l;
}

Element* LList::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    e = e->copying(false);
    LList* l = (LList*)duplicate_constant();
    l->insertion(e, ix);
    return l;
}

Element* List::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    List* l = (List*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->append(liste[i]->copying(false));
        l->append(liste[0]->copying(false));
        return l;
    }
    
    l->append(liste.back()->copying(false));
    for (long i = 0; i < liste.size() - 1; i ++)
        l->append(liste[i]->copying(false));
    return l;
}

Element* LList::rotate(bool left) {
    if (!liste.atleast2())
        return this;
    
    LList* l = new LList(liste.mark);
    u_link* it = liste.last();
    bool cyclic = (it->_next != NULL);
    u_link* tail = NULL;
    
    if (left) {
        l->append(liste.first->value->copying(false));
        if (cyclic)
            tail = l->liste.first;

        while (it != liste.first) {
            l->push_front(it->value->copying(false));
            it = it->previous();
        }
        
        if (cyclic) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }
        return l;
    }
    Element* last = it->value->copying(false);
    it = it->previous();
    while (it != NULL) {
        l->push_front(it->value->copying(false));
        if (cyclic) {
            tail = l->liste.first;
            cyclic = false;
        }
        it = it->previous();
    }
    l->push_front(last);
    if (tail != NULL) {
        //there is a cycle
        //we need to reproduce it...
        l->liste.first->_previous = tail;
        tail->_next = l->liste.first;
    }
    return l;
}

Element* List::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    List* list = lisp->provideList();
    long i, j;
    bool found;
    list->append(liste[0]->copying(false));
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < list->liste.size(); j++) {
            if (liste[i]->unify(lisp, list->liste[j], false)) {
                found = true;
                break;
            }
        }
        if (!found)
            list->append(liste[i]->copying(false));
    }
    return list;
}

Element* LList::unique(LispE* lisp) {
    if (liste.empty())
        return this;
    
    List* list = lisp->provideList();
    bool found;
    list->append(liste.front()->copying(false));
    u_link* it = liste.begin();
    it = it->next();
    for (;it != NULL; it = it->next()) {
        found = false;
        for (long j = 0; j < list->liste.size(); j++) {
            if (it->value->unify(lisp, list->liste[j], false)) {
                found = true;
                break;
            }
        }
        if (!found)
            list->append(it->value->copying(false));
    }
    return list;
}

Element* Floats::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    Floats* l = (Floats*)duplicate_constant();
    l->liste.insert(ix, e->asFloat());
    return l;
}

Element* Floats::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Floats* l = (Floats*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Floats::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Floats* nb = lisp->provideFloats();
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Numbers::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    Numbers* l = (Numbers*)duplicate_constant();
    l->liste.insert(ix, e->asNumber());
    return l;
}

Element* Numbers::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Numbers* l = (Numbers*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Numbers::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Numbers* nb = lisp->provideNumbers();
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Integers::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    Integers* l = (Integers*)duplicate_constant();
    l->liste.insert(ix, e->asInteger());
    return l;
}

Element* Integers::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Integers* l = (Integers*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Integers::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Integers* nb = lisp->provideIntegers();
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Shorts::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    Shorts* l = (Shorts*)duplicate_constant();
    l->liste.insert(ix, e->asInteger());
    return l;
}

Element* Shorts::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Shorts* l = (Shorts*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Shorts::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Shorts* nb = new Shorts();
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}


Element* Strings::insert(LispE* lisp, Element* e, long ix) {
    if (ix < 0)
        throw new Error("Error: Wrong index in 'insert'");
    
    Strings* l = (Strings*)duplicate_constant();
    l->liste.insert(ix, e->asUString(lisp));
    return l;
}

Element* String::rotate(bool left) {
    if (content.size() <= 1)
        return this;
    
    String* s = new String(L"");
    if (left) {
        s->content = content.substr(1,content.size());
        s->content += content[0];
        return s;
    }
    s->content = content.back();
    s->content += content.substr(0, content.size()-1);
    return s;
}

Element* Strings::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Strings* l = (Strings*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Strings::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Strings* nb = lisp->provideStrings();
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

//------------------------------------------------------------------------------------------

Element* Element::thekeys(LispE* lisp) {
    return emptylist_;
}

Element* String::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* List::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* LList::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    long sz = size();
    for (long i = 0; i < sz; i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Floats::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Numbers::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Integers::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Shorts::thekeys(LispE* lisp) {
    Shorts* keys = new Shorts();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Strings::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Set::thekeys(LispE* lisp) {
    Strings* keys = lisp->provideStrings();
    if (ensemble.empty())
        return keys;
    u_ustring k;
    for (auto& a : ensemble) {
        k = a;
        keys->append(lisp->provideString(k));
    }
    return keys;
}

Element* Set_n::thekeys(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideNumber(a));
    }
    return keys;
}


Element* Dictionary::thekeys(LispE* lisp) {
    Strings* dkeys = lisp->provideStrings();
    u_ustring keyvalue;
    for (auto& a: dictionary) {
        keyvalue = a.first;
        dkeys->append(keyvalue);
    }
    return dkeys;
}

Element* Dictionary_n::thekeys(LispE* lisp) {
    Numbers* dkeys = lisp->provideNumbers();
    for (auto& a: dictionary) {
        dkeys->liste.push_back(a.first);
    }
    return dkeys;
}

Element* Element::thevalues(LispE* lisp) {
    return emptylist_;
}

Element* Set::thevalues(LispE* lisp) {
    Strings* keys = lisp->provideStrings();
    if (ensemble.empty())
        return keys;
    u_ustring k;
    for (auto& a : ensemble) {
        k = a;
        keys->append(lisp->provideString(k));
    }
    return keys;
}

Element* Set_n::thevalues(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideNumber(a));
    }
    return keys;
}

Element* Dictionary::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Dictionary_n::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

//------------------------------------------------------------------------------------------
Element* Element::Boolean(LispE* lisp) {
    return booleans_[Boolean()];
}

//------------------------------------------------------------------------------------------
Element* Element::next_iter(LispE* lisp, void* it) {
    return emptyatom_;
}

Element* Set::next_iter(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    u_ustring s = **n;
    Element* r = lisp->provideString(s);
    (*n)++;
    return r;
}

Element* Set_n::next_iter(LispE* lisp, void* it) {
    std::set<double>::iterator* n = (std::set<double>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    Element* r = lisp->provideNumber(**n);
    (*n)++;
    return r;
}

Element* Set_i::next_iter(LispE* lisp, void* it) {
    std::set<long>::iterator* n = (std::set<long>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    Element* r = lisp->provideInteger(**n);
    (*n)++;
    return r;
}

Element* Element::next_iter_exchange(LispE* lisp, void* it) {
    return emptyatom_;
}

Element* Set::next_iter_exchange(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.content = **n;
    (*n)++;
    return &exchange_value;
}

Element* Set_n::next_iter_exchange(LispE* lisp, void* it) {
    std::set<double>::iterator* n = (std::set<double>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.number = **n;
    (*n)++;
    return &exchange_value;
}

Element* Set_i::next_iter_exchange(LispE* lisp, void* it) {
    std::set<long>::iterator* n = (std::set<long>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.integer = **n;
    (*n)++;
    return &exchange_value;
}

//------------------------------------------------------------------------------------------
long List::find_element(LispE* lisp, Element* valeur, long ix) {
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return i;
    }
    return -1;
}
//------------------------------------------------------------------------------------------
Element* Element::search_element(LispE* lisp, Element* valeur, long ix) {
    return null_;
}

Element* String::search_element(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.find(val, ix);
    if (ix == -1)
        return null_;
    return lisp->provideInteger(ix);
}

Element* List::search_element(LispE* lisp, Element* valeur, long ix) {
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* LList::search_element(LispE* lisp, Element* valeur, long ix) {
    u_link*  it = at(ix);
    long i = 0;
    for (;it != NULL; it = it->next()) {
        if (it->value->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
        i++;
    }
    return null_;
}

Element* Floats::search_element(LispE* lisp, Element* valeur, long ix) {
    float v = valeur->asFloat();
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Numbers::search_element(LispE* lisp, Element* valeur, long ix) {
    double v = valeur->asNumber();
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Integers::search_element(LispE* lisp, Element* valeur, long ix) {
    long v = valeur->asInteger();
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Shorts::search_element(LispE* lisp, Element* valeur, long ix) {
    short v = valeur->asShort();
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Strings::search_element(LispE* lisp, Element* valeur, long ix) {
    u_ustring v = valeur->asUString(lisp);
    for (long i = ix; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Set::search_element(LispE* lisp, Element* valeur, long ix) {
    u_ustring k = valeur->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_n::search_element(LispE* lisp, Element* valeur, long ix) {
    double k = valeur->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

Element* Dictionary::search_element(LispE* lisp, Element* valeur, long ix) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            u_ustring keyvalue = a.first;
            return lisp->provideString(keyvalue);
        }
    }
    return null_;
}


Element* Dictionary_n::search_element(LispE* lisp, Element* valeur, long ix) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_)
            return lisp->provideNumber(a.first);
    }
    return null_;
}

//------------------------------------------------------------------------------------------
bool Element::check_element(LispE* lisp, Element* valeur) {
    return false;
}

bool String::check_element(LispE* lisp, Element* valeur) {
    u_ustring val = valeur->asUString(lisp);
    return (content.find(val, 0) != -1);
}

bool List::check_element(LispE* lisp, Element* valeur) {
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return true;
    }
    return false;
}

bool LList::check_element(LispE* lisp, Element* valeur) {
    u_link*  it = liste.begin();
    long i = 0;
    for (;it != NULL; it = it->next()) {
        if (it->value->equal(lisp, valeur) == true_)
            return true;
        i++;
    }
    return false;
}

bool Floats::check_element(LispE* lisp, Element* valeur) {
    float v = valeur->asFloat();
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i] == v)
            return true;
    }
    return false;
}

bool Numbers::check_element(LispE* lisp, Element* valeur) {
    double v = valeur->asNumber();
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i] == v)
            return true;
    }
    return false;
}

bool Integers::check_element(LispE* lisp, Element* valeur) {
    long v = valeur->asInteger();
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i] == v)
            return true;
    }
    return false;
}

bool Shorts::check_element(LispE* lisp, Element* valeur) {
    short v = valeur->asShort();
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i] == v)
            return true;
    }
    return false;
}

bool Strings::check_element(LispE* lisp, Element* valeur) {
    u_ustring v = valeur->asUString(lisp);
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i] == v)
            return true;
    }
    return false;
}

bool Set::check_element(LispE* lisp, Element* valeur) {
    u_ustring k = valeur->asUString(lisp);
    return (ensemble.find(k) != ensemble.end());
}

bool Set_i::check_element(LispE* lisp, Element* valeur) {
    long k = valeur->asInteger();
    return (ensemble.find(k) != ensemble.end());
}

bool Set_n::check_element(LispE* lisp, Element* valeur) {
    double k = valeur->asNumber();
    return (ensemble.find(k) != ensemble.end());
}

bool Dictionary::check_element(LispE* lisp, Element* valeur) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            u_ustring keyvalue = a.first;
            return true;
        }
    }
    return false;
}


bool Dictionary_n::check_element(LispE* lisp, Element* valeur) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_)
            return true;
    }
    return false;
}

//------------------------------------------------------------------------------------------

Element* Element::checkkey(LispE* lisp, Element* e) {
    return null_;
}

Element* Set::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asUString(lisp)) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_n::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asNumber()) == ensemble.end())
        return null_;
    return true_;
}


Element* Dictionary::checkkey(LispE* lisp, Element* e) {
    try {
        return dictionary.at(e->asUString(lisp));
    }
    catch (...) {
        return null_;
    }
}

Element* Dictionary_n::checkkey(LispE* lisp, Element* e) {
    try {
        return dictionary.at(e->asNumber());
    }
    catch (...) {
        return null_;
    }
}


//------------------------------------------------------------------------------------------
Element* Element::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    return zero_;
}

Element* String::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    u_ustring cherche = valeur->asUString(lisp);
    u_ustring remplacement = remp->asUString(lisp);
    long nb = nb_ureplacestring(content,cherche, remplacement);
    return lisp->provideInteger(nb);
}

Element* List::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_) {
            replacing(i, remp->copying(false));
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* LList::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    u_link* a = liste.begin();
    long nb = 0;
    for (; a != NULL; a = a->next()) {
        if (a->value->equal(lisp, valeur) == true_) {
            if (a->value != remp) {
                a->value->decrement();
                a->value = remp->copying(false);
                a->value->increment();
                nb++;
            }
        }
    }
    return lisp->provideInteger(nb);
}

Element* Floats::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    float r = remp->asFloat();
    float v = valeur->asFloat();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (liste[i] == v) {
            liste[i] = r;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Numbers::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    double v = valeur->asNumber();
    double r = remp->asNumber();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (liste[i] == v) {
            liste[i] = r;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Shorts::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    short v = valeur->asShort();
    short r = remp->asShort();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (liste[i] == v) {
            liste[i] = r;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Integers::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    long v = valeur->asInteger();
    long r = remp->asInteger();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (liste[i] == v) {
            liste[i] = r;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Strings::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    u_ustring v = valeur->asUString(lisp);
    u_ustring r = remp->asUString(lisp);
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (liste[i] == v) {
            liste[i] = r;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Set::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) != ensemble.end()) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asUString(lisp));
        return one_;
    }
    return zero_;
}

Element* Set_n::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) != ensemble.end()) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asNumber());
        return one_;
    }
    return zero_;
}


Element* Dictionary::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            if (a.second != remp) {
                a.second->decrement();
                a.second = remp->copying(false);
                a.second->increment();
                nb++;
            }
        }
    }
    return lisp->provideInteger(nb);
}

Element* Dictionary_n::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long nb = 0;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            if (a.second != remp) {
                a.second->decrement();
                a.second = remp->copying(false);
                a.second->increment();
                nb++;
            }
        }
    }
    return lisp->provideInteger(nb);
}

//------------------------------------------------------------------------------------------
Element* Element::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    return emptylist_;
}

Element* String::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.find(val, ix);
    return s_findall(lisp,content, val, ix);
}

Element* List::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i]->equal(lisp, valeur) == true_) {
            l->liste.push_back(i);
        }
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* LList::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    u_link* a = at(ix);
    for (; a != NULL; a = a->next()) {
        if (a->value->equal(lisp, valeur) == true_) {
            l->liste.push_back(ix);
        }
        ix++;
    }
    if (l->liste.empty()) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Floats::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    float v = valeur->asFloat();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Numbers::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    double v = valeur->asNumber();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Shorts::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    short v = valeur->asShort();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Integers::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    long v = valeur->asInteger();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Strings::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    u_ustring v = valeur->asUString(lisp);
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Set::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_n::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}


Element* Dictionary::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            keyvalue = a.first;
            l->append(keyvalue);
        }
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Dictionary_n::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_)
            l->liste.push_back(a.first);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

//------------------------------------------------------------------------------------------
Element* Element::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    return zero_;
}

Element* String::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.find(val, ix);
    return s_count(lisp,content, val, ix);
}

Element* List::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i]->equal(lisp, valeur) == true_) {
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* LList::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    u_link* a = at(ix);
    for (; a != NULL; a = a->next()) {
        if (a->value->equal(lisp, valeur) == true_) {
            nb++;
        }
        ix++;
    }
    return lisp->provideInteger(nb);
}

Element* Floats::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    float v = valeur->asFloat();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Numbers::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    double v = valeur->asNumber();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Shorts::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    short v = valeur->asShort();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Integers::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    long v = valeur->asInteger();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i] == v)
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Strings::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    u_ustring v = valeur->asUString(lisp);
    for (long i = ix; i <
         liste.size(); i++) {
        if (liste[i] == v)
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Set::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return zero_;
    return one_;
}

Element* Set_n::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return zero_;
    return one_;
}

Element* Dictionary::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    u_ustring keyvalue;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            keyvalue = a.first;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Dictionary_n::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_)
            nb++;
    }
    return lisp->provideInteger(nb);
}

//------------------------------------------------------------------------------------------
Element* Element::list_and(LispE* lisp, Element* value) {
    return this;
}

Element* String::list_and(LispE* lisp, Element* value) {
    String* result = lisp->provideString();
    u_ustring val = value->asUString(lisp);
    for (long i = 0; i < content.size(); i++) {
        if (val.find(content[i]) != -1 && result->content.find(content[i]) == -1)
            result->content += content[i];
    }
    return result;
}

Element* List::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    List* result = lisp->provideList();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (value->check_element(lisp, liste[i]) && !result->check_element(lisp, liste[i]))
            result->append(liste[i]->copying(false));
    }
    if (result->isEmpty()) {
        result->release();
        return emptylist_;
    }
    return result;
}

Element* LList::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    LList* result = new LList(liste.mark);
    u_link* a = liste.begin();
    for (; a != NULL; a = a->next()) {
        if (value->check_element(lisp, a->value) && !result->check_element(lisp, a->value))
            result->append(a->value->copying(false));
    }
    if (result->isEmpty()) {
        result->release();
        return emptylist_;
    }
    return result;
}

Element* Floats::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Floats* l = lisp->provideFloats();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
            l->liste.push_back(liste[i]);
    }

    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Numbers::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Numbers* l = lisp->provideNumbers();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
            l->liste.push_back(liste[i]);
    }
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Shorts::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Shorts* l = new Shorts();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
            l->liste.push_back(liste[i]);
    }

    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Integers::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Integers* l = lisp->provideIntegers();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
            l->liste.push_back(liste[i]);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Strings::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Strings* l = lisp->provideStrings();
    long sz = liste.size();
    for (long i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
            l->liste.push_back(liste[i]);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Set::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set* s = lisp->provideSet();
    for (auto& a: ensemble) {
        exchange_value.content = a;
        if (value->check_element(lisp, &exchange_value))
            s->add(a);
    }
    return s;
}

Element* Set_n::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set_n* s = lisp->provideSet_n();
    for (auto& a: ensemble) {
        exchange_value.number = a;
        if (value->check_element(lisp, &exchange_value))
            s->add(a);
    }
    return s;
}

Element* Set_i::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set_i* s = lisp->provideSet_i();
    for (auto& a: ensemble) {
        exchange_value.integer = a;
        if (value->check_element(lisp, &exchange_value))
            s->add(a);
    }
    return s;
}

//------------------------------------------------------------------------------------------
Element* Element::list_or(LispE* lisp, Element* value) {
    return this;
}

Element* String::list_or(LispE* lisp, Element* value) {
    String* result = lisp->provideString();
    u_ustring val = value->asUString(lisp);
    long i;
    for (i = 0; i < content.size(); i++) {
        if (result->content.find(content[i]) == -1)
            result->content += content[i];
    }

    for (i = 0; i < val.size(); i++) {
        if (result->content.find(val[i]) == -1)
            result->content += val[i];
    }

    return result;
}

Element* List::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    List* result = lisp->provideList();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (result->check_element(lisp, liste[i]) == false)
            result->append(liste[i]->copying(false));
    }
    
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            if (result->check_element(lisp, a->value) == false)
                result->append(a->value->copying(false));
        }
    }
    else {
        if (value->isList()) {
            sz = value->size();
            for (i = 0; i < sz; i++) {
                if (result->check_element(lisp, value->index(i)) == false)
                    result->append(value->index(i)->copying(false));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter(lisp, iter);
                while (next_value != emptyatom_) {
                    if (result->check_element(lisp, next_value) == false)
                        result->append(next_value);
                    next_value = value->next_iter(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    
    if (result->isEmpty()) {
        result->release();
        return emptylist_;
    }
    return result;
}

Element* LList::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    LList* result = new LList(liste.mark);
    u_link* a = liste.begin();
    for (; a != NULL; a = a->next()) {
        if (result->check_element(lisp, a->value) == false)
            result->append(a->value->copying(false));
    }
    
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            if (result->check_element(lisp, a->value) == false)
                result->append(a->value->copying(false));
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                if (result->check_element(lisp, value->index(i)) == false)
                    result->append(value->index(i)->copying(false));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter(lisp, iter);
                while (next_value != emptyatom_) {
                    if (result->check_element(lisp, next_value) == false)
                        result->append(next_value);
                    next_value = value->next_iter(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    
    if (result->isEmpty()) {
        result->release();
        return emptylist_;
    }
    return result;
}

Element* Floats::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Floats* l = lisp->provideFloats();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            for (i = 0; i < l->size(); i++) {
                if (l->liste[i] == a->value->asFloat())
                    break;
            }
            if (i == l->size())
                l->liste.push_back(a->value->asFloat());
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                for (i = 0; i < l->size(); i++) {
                    if (l->liste[i] == value->index(j)->asFloat())
                        break;
                }
                if (i == l->size())
                    l->liste.push_back(value->index(j)->asFloat());
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    for (i = 0; i < l->size(); i++) {
                        if (l->liste[i] == next_value->asFloat())
                            break;
                    }
                    if (i == l->size())
                        l->liste.push_back(next_value->asFloat());
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Numbers::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Numbers* l = lisp->provideNumbers();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            for (i = 0; i < l->size(); i++) {
                if (l->liste[i] == a->value->asNumber())
                    break;
            }
            if (i == l->size())
                l->liste.push_back(a->value->asNumber());
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                for (i = 0; i < l->size(); i++) {
                    if (l->liste[i] == value->index(j)->asNumber())
                        break;
                }
                if (i == l->size())
                    l->liste.push_back(value->index(j)->asNumber());
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    for (i = 0; i < l->size(); i++) {
                        if (l->liste[i] == next_value->asNumber())
                            break;
                    }
                    if (i == l->size())
                        l->liste.push_back(next_value->asNumber());
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Shorts::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Shorts* l = new Shorts();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            for (i = 0; i < l->size(); i++) {
                if (l->liste[i] == a->value->asShort())
                    break;
            }
            if (i == l->size())
                l->liste.push_back(a->value->asShort());
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                for (i = 0; i < l->size(); i++) {
                    if (l->liste[i] == value->index(j)->asShort())
                        break;
                }
                if (i == l->size())
                    l->liste.push_back(value->index(j)->asShort());
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    for (i = 0; i < l->size(); i++) {
                        if (l->liste[i] == next_value->asShort())
                            break;
                    }
                    if (i == l->size())
                        l->liste.push_back(next_value->asShort());
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Integers::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Integers* l = lisp->provideIntegers();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            for (i = 0; i < l->size(); i++) {
                if (l->liste[i] == a->value->asInteger())
                    break;
            }
            if (i == l->size())
                l->liste.push_back(a->value->asInteger());
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                for (i = 0; i < l->size(); i++) {
                    if (l->liste[i] == value->index(j)->asInteger())
                        break;
                }
                if (i == l->size())
                    l->liste.push_back(value->index(j)->asInteger());
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    for (i = 0; i < l->size(); i++) {
                        if (l->liste[i] == next_value->asInteger())
                            break;
                    }
                    if (i == l->size())
                        l->liste.push_back(next_value->asInteger());
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Strings::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Strings* l = lisp->provideStrings();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            for (i = 0; i < l->size(); i++) {
                if (l->liste[i] == a->value->asUString(lisp))
                    break;
            }
            if (i == l->size())
                l->liste.push_back(a->value->asUString(lisp));
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                for (i = 0; i < l->size(); i++) {
                    if (l->liste[i] == value->index(j)->asUString(lisp))
                        break;
                }
                if (i == l->size())
                    l->liste.push_back(value->index(j)->asUString(lisp));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    for (i = 0; i < l->size(); i++) {
                        if (l->liste[i] == next_value->asUString(lisp))
                            break;
                    }
                    if (i == l->size())
                        l->liste.push_back(next_value->asUString(lisp));
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Set::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Set* s = lisp->provideSet();
    s->ensemble = ensemble;
    
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            s->add(a->value->asUString(lisp));
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                s->add(value->index(j)->asUString(lisp));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    s->add(next_value->asUString(lisp));
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    return s;
}

Element* Set_n::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Set_n* s = lisp->provideSet_n();
    s->ensemble = ensemble;
    
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            s->add(a->value->asNumber());
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                s->add(value->index(j)->asNumber());
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    s->add(next_value->asNumber());
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    return s;
}

Element* Set_i::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Set_i* s = lisp->provideSet_i();
    s->ensemble = ensemble;
    
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            s->add(a->value->asInteger());
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                s->add(value->index(j)->asInteger());
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    s->add(next_value->asInteger());
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    return s;
}

//------------------------------------------------------------------------------------------
Element* Element::list_xor(LispE* lisp, Element* value) {
    return this;
}

Element* String::list_xor(LispE* lisp, Element* value) {
    String* intersection = (String*)list_and(lisp, value);
    
    String* result = lisp->provideString();
    u_ustring val = value->asUString(lisp);
    long i;
    for (i = 0; i < content.size(); i++) {
        if (intersection->content.find(content[i]) == -1 && result->content.find(content[i]) == -1)
            result->content += content[i];
    }

    for (i = 0; i < val.size(); i++) {
        if (intersection->content.find(val[i]) == -1 && result->content.find(val[i]) == -1)
            result->content += val[i];
    }
    intersection->release();
    return result;
}

Element* List::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    List* intersection = (List*)list_and(lisp, value);
    
    List* result = lisp->provideList();
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!intersection->check_element(lisp, liste[i]) && !result->check_element(lisp, liste[i]))
            result->append(liste[i]->copying(false));
    }
    
    Element* v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value;
            if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                result->append(v->copying(false));
        }
    }
    else {
        if (value->isList()) {
            sz = value->size();
            for (i = 0; i < sz; i++) {
                v = value->index(i);
                if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                    result->append(v->copying(false));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                v = value->next_iter(lisp, iter);
                while (v != emptyatom_) {
                    if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                        result->append(v);
                    v = value->next_iter(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    intersection->release();
    if (result->isEmpty()) {
        result->release();
        return emptylist_;
    }
    return result;
}

Element* LList::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    LList* intersection = (LList*)list_and(lisp, value);
    
    LList* result = new LList(liste.mark);
    u_link* a = liste.begin();
    for (; a != NULL; a = a->next()) {
        if (!intersection->check_element(lisp, a->value) && !result->check_element(lisp, a->value))
            result->append(a->value->copying(false));
    }
    
    Element* v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value;
            if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                result->append(v->copying(false));
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i);
                if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                    result->append(v->copying(false));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                v = value->next_iter(lisp, iter);
                while (v != emptyatom_) {
                    if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                        result->append(v);
                   v = value->next_iter(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    intersection->release();
    if (result->isEmpty()) {
        result->release();
        return emptylist_;
    }
    return result;
}

Element* Floats::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    
    Floats* l = lisp->provideFloats();

    Floats* intersection = (Floats*)list_and(lisp, value);
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    float v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asFloat();
            if (!intersection->liste.check(v) && !l->liste.check(v))
                l->liste.push_back(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (i = 0; i < sz; i++) {
                v = value->index(i)->asFloat();
                if (!intersection->liste.check(v) && !l->liste.check(v))
                    l->liste.push_back(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asFloat();
                    if (!intersection->liste.check(v) && !l->liste.check(v))
                        l->liste.push_back(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    intersection->release();
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Numbers::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Numbers* l = lisp->provideNumbers();
    Numbers* intersection = (Numbers*)list_and(lisp, value);
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    double v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asNumber();
            if (!intersection->liste.check(v) && !l->liste.check(v))
                l->liste.push_back(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (i = 0; i < sz; i++) {
                v = value->index(i)->asNumber();
                if (!intersection->liste.check(v) && !l->liste.check(v))
                    l->liste.push_back(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asNumber();
                    if (!intersection->liste.check(v) && !l->liste.check(v))
                        l->liste.push_back(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    intersection->release();
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Shorts::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Shorts* l = new Shorts();
    Shorts* intersection = (Shorts*)list_and(lisp, value);
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    short v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asShort();
            if (!intersection->liste.check(v) && !l->liste.check(v))
                l->liste.push_back(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (i = 0; i < sz; i++) {
                v = value->index(i)->asShort();
                if (!intersection->liste.check(v) && !l->liste.check(v))
                    l->liste.push_back(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asShort();
                    if (!intersection->liste.check(v) && !l->liste.check(v))
                        l->liste.push_back(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    intersection->release();
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Integers::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Integers* l = lisp->provideIntegers();
    Integers* intersection = (Integers*)list_and(lisp, value);
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    long v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asInteger();
            if (!intersection->liste.check(v) && !l->liste.check(v))
                l->liste.push_back(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (i = 0; i < sz; i++) {
                v = value->index(i)->asInteger();
                if (!intersection->liste.check(v) && !l->liste.check(v))
                    l->liste.push_back(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asInteger();
                    if (!intersection->liste.check(v) && !l->liste.check(v))
                        l->liste.push_back(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    intersection->release();
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Strings::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Strings* l = lisp->provideStrings();
    Strings* intersection = (Strings*)list_and(lisp, value);
    long sz = liste.size();
    long i;
    for (i = 0; i < sz; i++) {
        if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
            l->liste.push_back(liste[i]);
    }

    u_ustring v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asUString(lisp);
            if (!intersection->liste.check(v) && !l->liste.check(v))
                l->liste.push_back(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (i = 0; i < sz; i++) {
                v = value->index(i)->asUString(lisp);
                if (!intersection->liste.check(v) && !l->liste.check(v))
                    l->liste.push_back(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asUString(lisp);
                    if (!intersection->liste.check(v) && !l->liste.check(v))
                        l->liste.push_back(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }
    
    intersection->release();
    
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Set::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Set* s = lisp->provideSet();
    Set* intersection = (Set*)list_and(lisp, value);
    
    for (auto & a : ensemble) {
        if (intersection->ensemble.find(a) == intersection->ensemble.end())
            s->add(a);
    }
    
    u_ustring v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asUString(lisp);
            if (intersection->ensemble.find(v) == intersection->ensemble.end())
                s->add(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asUString(lisp);
                if (intersection->ensemble.find(v) == intersection->ensemble.end())
                    s->add(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asUString(lisp);
                    if (intersection->ensemble.find(v) == intersection->ensemble.end())
                        s->add(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    intersection->release();
    return s;
}

Element* Set_n::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Set_n* s = lisp->provideSet_n();
    Set_n* intersection = (Set_n*)list_and(lisp, value);
    
    for (auto & a : ensemble) {
        if (intersection->ensemble.find(a) == intersection->ensemble.end())
            s->add(a);
    }

    double v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asNumber();
            if (intersection->ensemble.find(v) == intersection->ensemble.end())
                s->add(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asNumber();
                if (intersection->ensemble.find(v) == intersection->ensemble.end())
                    s->add(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asNumber();
                    if (intersection->ensemble.find(v) == intersection->ensemble.end())
                        s->add(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    intersection->release();
    return s;
}

Element* Set_i::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Set_i* s = lisp->provideSet_i();
    Set_i* intersection = (Set_i*)list_and(lisp, value);
    
    for (auto & a : ensemble) {
        if (intersection->ensemble.find(a) == intersection->ensemble.end())
            s->add(a);
    }

    long v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asInteger();
            if (intersection->ensemble.find(v) == intersection->ensemble.end())
                s->add(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asInteger();
                if (intersection->ensemble.find(v) == intersection->ensemble.end())
                    s->add(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asInteger();
                    if (intersection->ensemble.find(v) == intersection->ensemble.end())
                        s->add(v);
                    next_value = value->next_iter_exchange(lisp, iter);
                }
                value->clean_iter(iter);
            }
        }
    }

    intersection->release();
    return s;
}

//------------------------------------------------------------------------------------------
Element* Element::search_reverse(LispE* lisp, Element* valeur, long ix) {
    return minusone_;
}

Element* String::search_reverse(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.rfind(val, content.size() - ix);
    return lisp->provideInteger(ix);
}

Element* List::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* LList::search_reverse(LispE* lisp, Element* valeur, long ix) {
    u_link* it = liste.b_at(ix);
    for (; it != NULL; it = it->previous()) {
        if (it->value->equal(lisp, valeur) == true_)
            return lisp->provideInteger(ix);
        if (!ix)
            break;
        ix--;
    }
    
    return minusone_;
}


Element* Floats::search_reverse(LispE* lisp, Element* valeur, long ix) {
    float v = valeur->asFloat();
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Numbers::search_reverse(LispE* lisp, Element* valeur, long ix) {
    double v = valeur->asNumber();
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Shorts::search_reverse(LispE* lisp, Element* valeur, long ix) {
    short v = valeur->asShort();
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Integers::search_reverse(LispE* lisp, Element* valeur, long ix) {
    long v = valeur->asInteger();
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Strings::search_reverse(LispE* lisp, Element* valeur, long ix) {
    u_ustring v = valeur->asUString(lisp);
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Set::search_reverse(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_n::search_reverse(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Dictionary::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            u_ustring keyvalue = a.first;
            return lisp->provideString(keyvalue);
        }
    }
    return emptystring_;
}

Element* Dictionary_n::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_)
            return lisp->provideNumber(a.first);
    }
    return minusone_;
}


//------------------------------------------------------------------------------------------
Element* Element::reverse(LispE* lisp, bool duplicate) {
    return emptylist_;
}

Element* Float::reverse(LispE* lisp, bool duplicate) {
    return lisp->provideFloat(number*-1);
}

Element* Number::reverse(LispE* lisp, bool duplicate) {
    return lisp->provideNumber(number*-1);
}

Element* Short::reverse(LispE* lisp, bool duplicate) {
    return new Short(integer*-1);
}

Element* Integer::reverse(LispE* lisp, bool duplicate) {
    return lisp->provideInteger(integer*-1);
}

Element* String::reverse(LispE* lisp, bool duplicate) {
    u_ustring resultat;
    for (long i = content.size()-1; i >= 0; i--)
        resultat += content[i];
    
    if (duplicate)
        return lisp->provideString(resultat);
    
    content = resultat;
    return this;
}

Element* List::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        List* l = lisp->provideList();
        for (long i = liste.size()-1; i >= 0; i--)
            l->append(liste[i]->copying(false));
        return l;
    }
    
    liste.reverse();
    return this;
}

// duplicate is not taking into account for LList
// there are two many cases where it creates dangling structures...
Element* LList::reverse(LispE* lisp, bool duplicate) {
    if (duplicate) {
        LList* l = new LList(liste.mark);
        u_link*  it = liste.begin();
        if (it == NULL)
            return l;
        
        u_link* p = it;
        u_link* tail = NULL;
        for (; it != NULL; it = it->next()) {
            p = it;
            l->push_front(it->value->copying(false));
            if (tail == NULL)
                tail = l->liste.first;
        }
        if (p->_next != NULL) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Floats::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Floats* l = lisp->provideFloats();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Numbers::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Numbers* l = lisp->provideNumbers();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Integers::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Integers* l = lisp->provideIntegers();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Shorts::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Shorts* l = new Shorts();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Strings::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Strings* l = lisp->provideStrings();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}


Element* Dictionary::reverse(LispE* lisp, bool duplicate) {
    Dictionary* dico = lisp->provideDictionary();
    
    u_ustring k;
    Element* e;
    for (auto& a: dictionary) {
        k = a.second->asUString(lisp);
        e = dico->dictionary[k];
        if (e == NULL) {
            e = lisp->provideStrings();
            dico->dictionary[k] = e;
            e->increment();
        }
        k = a.first;
        ((Strings*)e)->append(k);
    }
    return dico;
}

Element* Dictionary_n::reverse(LispE* lisp, bool duplicate) {
    Dictionary_n* dico = lisp->provideDictionary_n();
    
    double k;
    Element* e;
    for (auto& a: dictionary) {
        k = a.second->asNumber();
        e = dico->dictionary[k];
        if (e == NULL) {
            e = lisp->provideNumbers();
            dico->dictionary[k] = e;
            e->increment();
        }
        ((Numbers*)e)->liste.push_back(a.first);
    }
    return dico;
}

Element* List::rotate(LispE* lisp, long axis) {
    return reverse(lisp, true);
}


Element* Matrice_float::rotate(LispE* lisp, long axis) {
    Matrice_float* revert_matrix = new Matrice_float;
    revert_matrix->size_x = size_x;
    revert_matrix->size_y = size_y;
    
    long i;
    
    if (axis == 1) {
        for (i = 0; i < size_x; i++) {
            revert_matrix->append(liste[i]->rotate(lisp,0));
        }
        return revert_matrix;
    }
    
    Element* e;
    for (i = size_x-1; i>= 0;  i--) {
        e = lisp->provideFloats((Floats*)liste[i]);
        revert_matrix->append(e);
    }
    return revert_matrix;
}

Element* Matrice::rotate(LispE* lisp, long axis) {
    Matrice* revert_matrix = new Matrice;
    revert_matrix->size_x = size_x;
    revert_matrix->size_y = size_y;
    
    long i;
    
    if (axis == 1) {
        for (i = 0; i < size_x; i++) {
            revert_matrix->append(liste[i]->rotate(lisp,0));
        }
        return revert_matrix;
    }
    
    Element* e;
    for (i = size_x-1; i>= 0;  i--) {
        e = lisp->provideNumbers((Numbers*)liste[i]);
        revert_matrix->append(e);
    }
    return revert_matrix;
}

Element* Tenseur::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return lisp->provideNumbers((Numbers*)value);
    
    Element* r;
    if (init) {
        r = new Tenseur;
        ((Tenseur*)r)->shape = shape;
    }
    else
        r = lisp->provideList();
    
    Element* e;
    for (long i = 0; i < shape[pos]; i++) {
        e = reversion(lisp, value->index(i), pos+1, axis, false);
        r->append(e);
    }
    return r;
}

Element* Tenseur::rotate(LispE* lisp, long axis) {
    return reversion(lisp, this, 0, axis, true);
}

Element* Tenseur_float::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return lisp->provideNumbers((Numbers*)value);
    
    Element* r;
    if (init) {
        r = new Tenseur_float;
        ((Tenseur_float*)r)->shape = shape;
    }
    else
        r = lisp->provideList();
    
    Element* e;
    for (long i = 0; i < shape[pos]; i++) {
        e = reversion(lisp, value->index(i), pos+1, axis, false);
        r->append(e);
    }
    return r;
}

Element* Tenseur_float::rotate(LispE* lisp, long axis) {
    return reversion(lisp, this, 0, axis, true);
}

//------------------------------------------------------------------------------------------
Element* Element::protected_index(LispE* lisp,long i) {
    return null_;
}

Element* Element::protected_index(LispE* lisp, u_ustring&) {
    return null_;
}

Element* Element::protected_index(LispE* lisp, double k) {
    return null_;
}

Element* String::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* List::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return liste[i];
    return null_;
}

Element* LList::protected_index(LispE* lisp,long i) {
    Element* e = null_;
    if (i >= 0) {
        e = at_e(i)->copying(false);
        if (e == NULL)
            return null_;
    }
    return e;
}

Element* Floats::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    return null_;
}

Element* Numbers::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    return null_;
}

Element* Shorts::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    return null_;
}

Element* Integers::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    return null_;
}

Element* Strings::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    return null_;
}

Element* Set::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (auto& a: ensemble) {
            if (!i) {
                exchange_value.content = a;
                return lisp->provideString(exchange_value.content);
            }
            i--;
        }
    }
    return null_;
}

Element* Set_n::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (auto& a: ensemble) {
            if (!i) {
                return lisp->provideNumber(a);
            }
            i--;
        }
    }
    return null_;
}


//------------------------------------------------------------------------------------------
Element* Element::last_element(LispE* lisp) {
    return null_;
}

Element* String::last_element(LispE* lisp) {
    if (!content.size())
        return null_;
    wchar_t c = content.back();
    return lisp->provideString(c);
}

Element* List::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return liste.back();
}

Element* LList::last_element(LispE* lisp) {
    if (liste.empty())
        return null_;
    return liste.back();
}

Element* Floats::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideFloat(liste.back());
}

Element* Numbers::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideNumber(liste.back());
}

Element* Shorts::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return new Short(liste.back());
}

Element* Integers::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideInteger(liste.back());
}

Element* Strings::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideString(liste.back());
}


//------------------------------------------------------------------------------------------
Element* Element::value_on_index(LispE* lisp, long i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    return null_;
}

Element* LList::value_on_index(LispE* lisp, long i) {
    Element* e = null_;
    if (i >= 0) {
        e = at_e(i)->copying(false);
        if (e == NULL)
            return null_;
    }
    return e;
}

Element* Floats::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    return null_;
}

Element* Numbers::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    return null_;
}

Element* Shorts::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    return null_;
}

Element* Integers::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    return null_;
}

Element* Strings::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    return null_;
}

Element* String::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* Set::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (auto& a: ensemble) {
            if (!i) {
                exchange_value.content = a;
                return lisp->provideString(exchange_value.content);
            }
            i--;
        }
    }
    return null_;
}

Element* Set_n::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (auto& a: ensemble) {
            if (!i) {
                return lisp->provideNumber(a);
            }
            i--;
        }
    }
    return null_;
}

Element* Element::value_from_index(LispE* lisp, long i) {
    return null_;
}

Element* List::value_from_index(LispE* lisp, long i) {
    return liste[i]->copying(false);
}

Element* LList::value_from_index(LispE* lisp, long i) {
    return at_e(i)->copying(false);
}

Element* Floats::value_from_index(LispE* lisp, long i) {
    return lisp->provideFloat(liste[i]);
}

Element* Numbers::value_from_index(LispE* lisp, long i) {
    return lisp->provideNumber(liste[i]);
}

Element* Shorts::value_from_index(LispE* lisp, long i) {
    return new Short(liste[i]);
}

Element* Integers::value_from_index(LispE* lisp, long i) {
    return lisp->provideInteger(liste[i]);
}

Element* Strings::value_from_index(LispE* lisp, long i) {
    return lisp->provideString(liste[i]);
}

Element* String::value_from_index(LispE* lisp, long i) {
    return lisp->provideString(content[i]);
}

Element* Set::value_from_index(LispE* lisp, long i) {
    for (auto& a: ensemble) {
        if (!i) {
            exchange_value.content = a;
            return lisp->provideString(exchange_value.content);
        }
        i--;
    }
    return null_;
}

Element* Set_n::value_from_index(LispE* lisp, long i) {
    for (auto& a: ensemble) {
        if (!i) {
            return lisp->provideNumber(a);
        }
        i--;
    }
    return null_;
}
//------------------------------------------------------------------------------------------

Element* Element::value_on_index(wstring& k, LispE* lisp) {
    return null_;
}

Element* Element::value_on_index(u_ustring& k, LispE* lisp) {
    return null_;
}

Element* Set::value_on_index(wstring& w, LispE* lisp) {
    u_pstring k = _w_to_u(w);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set::value_on_index(u_ustring& k, LispE* lisp) {
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}


Element* Dictionary::value_on_index(u_ustring& k, LispE* lisp) {
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (...) {
        return null_;
    }
}

Element* Dictionary::value_on_index(wstring& u, LispE* lisp) {
    u_pstring k = _w_to_u(u);
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (...) {
        return null_;
    }
}


Element* Set::protected_index(LispE* lisp, u_ustring& k) {
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Dictionary::protected_index(LispE* lisp, u_ustring& k) {
    try {
        return dictionary.at(k);
    }
    catch (...) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(double k, LispE* lisp) {
    return null_;
}

Element* Dictionary_n::value_on_index(double k, LispE* lisp) {
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (...) {
        return null_;
    }
}

Element* Dictionary_n::protected_index(LispE* lisp, double k) {
    try {
        return dictionary.at(k);
    }
    catch (...) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(LispE* lisp, Element* i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    
    return null_;
}

Element* LList::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    ix = null_;
    if (i >= 0) {
        ix = at_e(i)->copying(false);
        if (ix == NULL)
            return null_;
    }
    return ix;
}

Element* Floats::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    
    return null_;
}

Element* Numbers::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    
    return null_;
}

Element* Shorts::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    
    return null_;
}

Element* Integers::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    
    return null_;
}

Element* Strings::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    
    return null_;
}

Element* String::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* Set::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_n::value_on_index(LispE* lisp, Element* ix) {
    double k = ix->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideNumber(k);
}


Element* Dictionary::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (...) {
        return null_;
    }
}

Element* Dictionary_n::value_on_index(LispE* lisp, Element* ix) {
    try {
        return dictionary.at(ix->checkNumber(lisp))->copying(false);
    }
    catch (...) {
        return null_;
    }
}
//------------------------------------------------------------------------------------------
Element* Element::protected_index(LispE* lisp,Element*) {
    throw new Error("Error: value cannot be access through index");
}

Element* String::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    
    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    throw new Error("Error: index out of bounds");
}

Element* List::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i];
    
    throw new Error("Error: index out of bounds");
}

Element* LList::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0) {
        ix = at_e(i)->copying(false);
        if (ix == NULL)
            throw new Error("Error: index out of bounds");
    }
    else
        throw new Error("Error: index out of bounds");
    return ix;
}

Element* Floats::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Numbers::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Integers::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Shorts::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Strings::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Set::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_n::protected_index(LispE* lisp, Element* ix) {
    double k = ix->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideNumber(k);
}


Element* Dictionary::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    try {
        return dictionary.at(k);
    }
    catch (...) {
        throw new Error("Error: index out of bounds");
    }
}

Element* Dictionary_n::protected_index(LispE* lisp, Element* ix) {
    try {
        return dictionary.at(ix->checkNumber(lisp));
    }
    catch (...) {
        throw new Error("Error: index out of bounds");
    }
}

//------------------------------------------------------------------------------------------
Element* Element::join_in_list(LispE* lisp, u_ustring& sep) {
    throw new Error("Error: 'join' can only be used for lists");
}

Element* List::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += liste[i]->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* LList::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        str += beg;
        beg = sep;
        str += a->value->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Floats::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString(liste[i]);
    }
    return lisp->provideString(str);
}

Element* Numbers::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString(liste[i]);
    }
    return lisp->provideString(str);
}

Element* Shorts::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString((long)liste[i]);
    }
    return lisp->provideString(str);
}

Element* Integers::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString(liste[i]);
    }
    return lisp->provideString(str);
}

Element* Strings::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += liste[i];
    }
    return lisp->provideString(str);
}

Element* Set::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: ensemble) {
        str += beg;
        beg = sep;
        str += a;
    }
    return lisp->provideString(str);
}


Element* Set_n::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep == U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: ensemble) {
        str += beg;
        beg = sep;
        str += convertToUString(a);
    }
    return lisp->provideString(str);
}

Element* Dictionary::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary_n::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}


//------------------------------------------------------------------------------------------
Element* Element::charge(LispE* lisp, string chemin) {
    return emptyatom_;
}
//------------------------------------------------------------------------------------------
Element* Element::replace(LispE* lisp, long i, Element* e) {
    throw new Error("Error: cannot modify this element");
}

Element* String::replace(LispE* lisp, long i, Element* e) {
    if (i < 0 || i >= content.size())
        throw new Error("Error: cannot modify at this position");
    u_ustring c = content.substr(0, i);
    c += e->asUString(lisp);
    c += content.substr(i+1, content.size());
    return lisp->provideString(c);
}
//------------------------------------------------------------------------------------------

wstring Infinitelist::asString(LispE* lisp) {
    wstring tampon(L"(repeat ");
    tampon += value->asString(lisp);
    tampon += L")";
    return tampon;
}

wstring Cyclelist::asString(LispE* lisp) {
    wstring tampon(L"(cycle ");
    tampon += value->asString(lisp);
    tampon += L")";
    return tampon;
}

wstring InfiniterangeNumber::asString(LispE* lisp) {
    std::wstringstream str;
    str << L"(irange " << initial_value << " " << increment << L")";
    return str.str();
}

wstring InfiniterangeInteger::asString(LispE* lisp) {
    std::wstringstream str;
    str << L"(irange " << initial_value << " " << increment << L")";
    return str.str();
}

wstring Error::asString(LispE* lisp) {
    if (lisp != NULL && lisp->delegation->i_current_file != -1) {
        string s = lisp->delegation->allfiles_names[lisp->delegation->i_current_file];
        wstring w;
        s_utf8_to_unicode(w, USTR(s), s.size());
        std::wstringstream msg;
        msg << message << L", line: " << lisp->delegation->i_current_line << L" in: " << w;
        return msg.str();
    }
    else
        return message;
}
//------------------------------------------------------------------------------------------
u_ustring Atomefonction::asUString(LispE* lisp) {
    return lisp->asUString(function_label);
}

u_ustring Infinitelist::asUString(LispE* lisp) {
    u_ustring tampon(U"(repeat ");
    tampon += value->asUString(lisp);
    tampon += U")";
    return tampon;
}

u_ustring Cyclelist::asUString(LispE* lisp) {
    u_ustring tampon(U"(cycle ");
    tampon += value->asUString(lisp);
    tampon += U")";
    return tampon;
}

//------------------------------------------------------------------------------------------
bool Element::check_arity(LispE* lisp, unsigned long arity) {
    if (type == t_atom)
        return eval(lisp)->check_arity(lisp, arity);
    if (type < l_final)
        return lisp->delegation->sameArity(type, arity);
    return false;
}

//------------------------------------------------------------------------------------------
Element* List::newInstance(Element* e) {
    long i;
    long sz = size();
    for (i = 0; i < sz; i++) {
        if (liste[i]->isList())
            break;
    }
    
    if (i == sz) {
        switch(e->type) {
            case t_float:
                return new Floats(sz, e->asNumber());
            case t_number:
                return new Numbers(sz, e->asNumber());
            case t_short:
                return new Shorts(sz, e->asShort());
            case t_integer:
                return new Integers(sz, e->asInteger());
            case t_string:
                return new Strings(sz, e->asString(NULL));
        }
    }
    
    List* lst = (List*)newInstance();
    for (i = 0; i < sz; i++) {
        if (liste[i]->isList())
            lst->append(liste[i]->newInstance(e));
        else
            lst->append(e);
    }
    return lst;
}

//------------------------------------------------------------------------------------------
Element* Element::equal(LispE* lisp, Element* e) {
    return booleans_[(e==this)];
}

Element* Atome::equal(LispE* lisp, Element* e) {
    return booleans_[(e->label() == atome)];
}

Element* Maybe::equal(LispE* lisp, Element* e) {
    return booleans_[(e->label() == t_error)];
}

Element* List::equal(LispE* lisp, Element* e) {
    return booleans_[e->isList() && liste.equal(((List*)e)->liste)];
}

Element* LList::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_llist && e->size() == 0 && liste.empty()) || e == this)];
}

Element* Dictionary::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0) || e == this)];
}

Element* Dictionary_n::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0) || e== this)];
}

Element* String::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content == e->asUString(lisp))];
}

Element* Number::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && number == e->asNumber())];
}

Element* Float::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && number == e->asFloat())];
}

Element* Short::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && integer == e->asShort())];
}

Element* Integer::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && integer == e->asInteger())];
}

Element* Floats::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_floats && liste == ((Floats*)e)->liste)];
}

Element* Numbers::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_numbers && liste == ((Numbers*)e)->liste)];
}

Element* Shorts::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_shorts && liste == ((Shorts*)e)->liste)];
}

Element* Integers::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_integers && liste == ((Integers*)e)->liste)];
}

Element* Strings::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_strings && liste == ((Strings*)e)->liste)];
}

Element* Set::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_set && ensemble == ((Set*)e)->ensemble)];
}

Element* Set_n::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_setn && ensemble == ((Set_n*)e)->ensemble)];
}

bool Element::egal(Element* e) {
    return (e==this);
}

bool Atome::egal(Element* e) {
    return (e->label() == atome);
}

bool Maybe::egal(Element* e) {
    return (e->label() == t_error);
}

bool List::egal(Element* e) {
    return e->isList() && liste.equal(((List*)e)->liste);
}

bool LList::egal(Element* e) {
    return ((e->type == t_llist && e->size() == 0 && liste.empty()) || e == this);
}

bool Dictionary::egal(Element* e) {
    return ((e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0) || e == this);
}

bool Dictionary_n::egal(Element* e) {
    return ((e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0) || e== this);
}

bool String::egal(Element* e) {
    return (e->type == t_string && content == e->asUString(NULL));
}

bool Number::egal(Element* e) {
    return (e->isNumber() && number == e->asNumber());
}

bool Float::egal(Element* e) {
    return (e->isNumber() && number == e->asFloat());
}

bool Short::egal(Element* e) {
    return (e->isNumber() && integer == e->asShort());
}

bool Integer::egal(Element* e) {
    return (e->isNumber() && integer == e->asInteger());
}

bool Floats::egal(Element* e) {
    return (e->type == t_floats && liste == ((Floats*)e)->liste);
}

bool Numbers::egal(Element* e) {
    return (e->type == t_numbers && liste == ((Numbers*)e)->liste);
}

bool Shorts::egal(Element* e) {
    return (e->type == t_shorts && liste == ((Shorts*)e)->liste);
}

bool Integers::egal(Element* e) {
    return (e->type == t_integers && liste == ((Integers*)e)->liste);
}

bool Strings::egal(Element* e) {
    return (e->type == t_strings && liste == ((Strings*)e)->liste);
}

bool Set::egal(Element* e) {
    return (e->type == t_set && ensemble == ((Set*)e)->ensemble);
}

bool Set_n::egal(Element* e) {
    return (e->type == t_setn && ensemble == ((Set_n*)e)->ensemble);
}

//------------------------------------------------------------------------------------------
Element* Element::less(LispE* lisp, Element* e) {
    return false_;
}

Element* Element::lessorequal(LispE* lisp, Element* e){
    return false_;
}

Element* Element::more(LispE* lisp, Element* e) {
    return false_;
}

Element* Element::moreorequal(LispE* lisp, Element* e) {
    return false_;
}

Element* String::less(LispE* lisp, Element* e) {
    return booleans_[content < e->asUString(lisp)];
}

Element* String::lessorequal(LispE* lisp, Element* e){
    return booleans_[content <= e->asUString(lisp)];
}

Element* String::more(LispE* lisp, Element* e) {
    return booleans_[content > e->asUString(lisp)];
}

Element* String::moreorequal(LispE* lisp, Element* e) {
    return booleans_[content >= e->asUString(lisp)];
}

Element* Number::less(LispE* lisp, Element* e) {
    return booleans_[number < e->asNumber()];
}

Element* Float::less(LispE* lisp, Element* e) {
    return booleans_[number < e->asFloat()];
}

Element* Float::lessorequal(LispE* lisp, Element* e){
    return booleans_[number <= e->asFloat()];
}

Element* Number::lessorequal(LispE* lisp, Element* e){
    return booleans_[number <= e->asNumber()];
}

Element* Float::more(LispE* lisp, Element* e) {
    return booleans_[number > e->asFloat()];
}

Element* Number::more(LispE* lisp, Element* e) {
    return booleans_[number > e->asNumber()];
}

Element* Float::moreorequal(LispE* lisp, Element* e) {
    return booleans_[number >= e->asFloat()];
}

Element* Number::moreorequal(LispE* lisp, Element* e) {
    return booleans_[number >= e->asNumber()];
}

Element* Integer::less(LispE* lisp, Element* e) {
    return booleans_[integer < e->asInteger()];
}

Element* Short::less(LispE* lisp, Element* e) {
    return booleans_[integer < e->asShort()];
}

Element* Short::lessorequal(LispE* lisp, Element* e) {
    return booleans_[integer <= e->asShort()];
}

Element* Integer::lessorequal(LispE* lisp, Element* e){
    return booleans_[integer <= e->asInteger()];
}

Element* Integer::more(LispE* lisp, Element* e) {
    return booleans_[integer > e->asInteger()];
}

Element* Integer::moreorequal(LispE* lisp, Element* e) {
    return booleans_[integer >= e->asInteger()];
}

Element* Short::more(LispE* lisp, Element* e) {
    return booleans_[integer > e->asShort()];
}

Element* Short::moreorequal(LispE* lisp, Element* e) {
    return booleans_[integer >= e->asShort()];
}


//------------------------------------------------------------------------------------------
Element* Element::plus(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '+' with these operands");
}

Element* Element::bit_not(LispE* lisp) {
    throw new Error("Error: cannot evaluate '~' for this operand");
}

Element* Element::bit_and(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '&' with these operands");
}

Element* Element::bit_and_not(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '&' with these operands");
}

Element* Element::bit_or(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '|' with these operands");
}

Element* Element::bit_xor(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '^' with these operands");
}

Element* Element::minus(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '-' with these operands");
}

Element* Element::multiply(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '*' with these operands");
}

Element* Element::divide(LispE* lisp, Element* e)  {
    throw new Error("Error: cannot evaluate '/' with these operands");
}

Element* Element::mod(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '%' with these operands");
}

Element* Element::power(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '^^' with these operands");
}

Element* Element::leftshift(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '<<' with these operands");
}

Element* Element::rightshift(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '>>' with these operands");
}

Element* String::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->plus(lisp, e);
    }
    
    if (status != s_constant) {
        content += e->asUString(lisp);
        return this;
    }
    u_ustring c = content + e->asUString(lisp);
    return lisp->provideString(c);
}

Element* Strings::plus(LispE* lisp, Element* e) {
    //Two cases either e is a string or it is a list...
    if (e == NULL) {
        u_ustring d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste[i];
        }
        return lisp->provideString(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asUString(lisp);
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] += e->asUString(lisp);
    }
    return this;
}

Element* Set::plus(LispE* lisp, Element* e) {
    //Two cases either e is a string or it is a list...
    u_ustring d;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d += a;
        }
        return lisp->provideString(d);
    }
    Set* res = lisp->provideSet();
    if (e->type == t_set) {
        auto nxt = ((Set*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set*)e)->ensemble.end())
                return res;
            d = a + *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size())
                return res;
            d = a + e->index(i)->asUString(lisp);
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    u_ustring w = e->asUString(lisp);
    for (auto& a: ensemble) {
        d = a + w;
        res->add(d);
    }
    release();
    return res;
}

//------------------------------------------------------------------------------------------
Element* Element::extraction(LispE* lisp, List* l) {
    return null_;
}

Element* List::extraction(LispE* lisp, List* l) {
    Element* e_from = l->liste[2];
    Element* e;
    
    long from = 0;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }
    
    e = null_;
    switch (ty) {
        case t_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            from = e->asInteger();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            //We skip the first characters
            from = e->asInteger() + 1;
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            if (from < 0)
                from = size() + from;
            break;
        default:
            e->release();
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e->release();
    e_from->release();
    
    if (from < 0 || from >= size())
        return emptylist_;
    
    if (nxt == l->size()) {
        //Only one element is returned
        return liste[from]->copying(false);
    }
    
    Element* e_upto = l->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }
    
    long upto;
    e = null_;
    switch (ty) {
        case t_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            //All characters are integrated
            upto = e->asInteger() + 1;
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger() - 1;
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = size() + upto;
                }
            }
            break;
        default:
            e->release();
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }
    
    e->release();
    e_upto->release();
    if (upto <= from)
        return emptylist_;
    
    if (upto > size())
        upto = size();
    l = lisp->provideList();
    for (;from < upto; from++)
        l->append(liste[from]->copying(false));
    return l;
}

Element* LList::extraction(LispE* lisp, List* l) {
    Element* e_from = l->liste[2];
    Element* e;
    
    long from = 0;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }
    
    e = null_;
    switch (ty) {
        case t_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            from = e->asInteger();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            //We skip the first characters
            from = e->asInteger() + 1;
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            break;
        default:
            e->release();
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e->release();
    e_from->release();
    
    if (nxt == l->size()) {
        //Only one element is returned
        if (from < 0)
            e = liste.b_at_e(from*-1);
        else
            e = at_e(from);
        
        if (e == NULL)
            return emptylist_;
        
        return e->copying(false);
    }
    
    Element* e_upto = l->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }
    
    long upto;
    e = null_;
    long sz = size();
    switch (ty) {
        case t_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            //All characters are integrated
            upto = e->asInteger() + 1;
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger() - 1;
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = sz + upto;
                }
            }
            break;
        default:
            e->release();
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }
    
    e->release();
    e_upto->release();

    if (upto <= from)
        return emptylist_;
    
    if (upto > sz)
        upto = sz;
    
    LList* ll = new LList(liste.mark);
    u_link*  it = liste.at(upto - 1);
    for (;it != NULL && upto != from; it = it->previous(), upto--) {
        ll->push_front(it->value->copying(false));
    }
    return ll;
}

Element* Floats::extraction(LispE* lisp, List* l) {
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideFloat(liste[depuis]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Floats* n = lisp->provideFloats();
    for (;depuis < upto; depuis++) {
        n->liste.push_back(liste[depuis]);
    }
    return n;
}

Element* Numbers::extraction(LispE* lisp, List* l) {
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideNumber(liste[depuis]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Numbers* n = lisp->provideNumbers();
    for (;depuis < upto; depuis++) {
        n->liste.push_back(liste[depuis]);
    }
    return n;
}

Element* Shorts::extraction(LispE* lisp, List* l) {
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return new Short(liste[depuis]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Shorts* n = new Shorts();
    for (;depuis < upto; depuis++) {
        n->liste.push_back(liste[depuis]);
    }
    return n;
}

Element* Integers::extraction(LispE* lisp, List* l) {
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideInteger(liste[depuis]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Integers* n = lisp->provideIntegers();
    for (;depuis < upto; depuis++) {
        n->liste.push_back(liste[depuis]);
    }
    return n;
}

Element* Strings::extraction(LispE* lisp, List* l) {
    
    Element* e_from = l->liste[2];
    Element* e;
    
    long from = 0;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }
    
    e = null_;
    switch (ty) {
        case t_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            from = e->asInteger();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            //We skip the first characters
            from = e->asInteger() + 1;
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            if (from < 0)
                from = size() + from;
            break;
        default:
            e->release();
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e->release();
    e_from->release();
    
    if (from < 0 || from >= size())
        return emptylist_;
    
    if (nxt == l->size()) {
        //Only one element is returned
        return lisp->provideString(liste[from]);
    }
    
    Element* e_upto = l->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }
    
    long upto;
    e = null_;
    switch (ty) {
        case t_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            //All characters are integrated
            upto = e->asInteger() + 1;
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger() - 1;
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = size() + upto;
                }
            }
            break;
        default:
            e->release();
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }
    
    e->release();
    e_upto->release();
    if (upto <= from)
        return emptylist_;
    
    if (upto > size())
        upto = size();
    Strings* n = lisp->provideStrings();
    for (;from < upto; from++) {
        n->liste.push_back(liste[from]);
    }
    return n;
}

Element* String::extraction(LispE* lisp, List* liste) {
    Element* e_from = liste->liste[2];
    
    long from;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }
    
    switch (ty) {
        case t_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.find(ch);
            if (from == -1)
                return emptystring_;
            from += ch.size();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.find(ch);
            if (from == -1)
                return emptystring_;
            firstisString = ch.size();
            break;
        }
        case t_minus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.rfind(ch, content.size());
            if (from == -1)
                return emptystring_;
            //We skip the first characters
            from += ch.size();
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.rfind(ch, content.size());
            if (from == -1)
                return emptystring_;
            firstisString = ch.size();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            if (from < 0)
                from = content.size() + from;
            break;
        default:
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e_from->release();
    
    if (from < 0 || from >= content.size())
        return emptystring_;
    
    if (nxt == liste->size()) {
        //Only one element is returned
        return lisp->provideString(content[from]);
    }
    
    Element* e_upto = liste->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }
    
    long upto;
    
    switch (ty) {
        case t_string: {
            if (firstisString == -1) firstisString = 0;
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            //All characters are integrated
            upto += ch.size();
            break;
        }
        case t_minus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.rfind(ch, content.size());
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_minus_plus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.rfind(ch, content.size());
            if (upto == -1)
                return emptystring_;
            //All characters are integrated
            upto += ch.size();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = content.size() + upto;
                }
            }
            break;
        default:
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }
    
    e_upto->release();
    if (upto <= from)
        return emptystring_;
    
    if (upto > content.size())
        upto = content.size();
    u_ustring remplace = content.substr(from, upto - from);
    return lisp->provideString(remplace);
}
//------------------------------------------------------------------------------------------
Element* Element::replace_in(LispE* lisp, List* l) {
    return null_;
}

Element* List::replace_in(LispE* lisp, List* l) {
    Element* e_from = l->liste[2];
    Element* e;
    Element* last = l->liste.back();
    last = last->eval(lisp);
    Element* e_upto;
    
    long from = 0;
    long upto;

    long firstisString = -1;
    short nxt = 3;
    short ty;
    try {
        switch (e_from->label()) {
            case l_minus:
                e_from = l->liste[3]->eval(lisp);
                nxt = 4;
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_minus_string;
                else
                    throw new Error("Error: Wrong value after first operator: '-'");
                break;
            case l_plus:
                e_from = l->liste[3]->eval(lisp);
                nxt = 4;
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_plus_string;
                else
                    throw new Error("Error: Wrong value after first operator: '+'");
                break;
            case l_minus_plus:
                e_from = l->liste[3]->eval(lisp);
                nxt = 4;
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_minus_plus_string;
                else
                    throw new Error("Error: Wrong value after first operator: '-+'");
                break;
            default:
                e_from = e_from->eval(lisp);
                ty = e_from->type;
        }
        
        e = null_;
        switch (ty) {
            case t_string: {
                e = search_element(lisp, e_from, 0);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                from = e->asInteger();
                firstisString = 0;
                break;
            }
            case t_plus_string: {
                e = search_element(lisp, e_from, 0);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                firstisString = e->asInteger();
                break;
            }
            case t_minus_string: {
                e = search_reverse(lisp, e_from, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                //We skip the first characters
                from = e->asInteger() + 1;
                firstisString = 0;
                break;
            }
            case t_minus_plus_string: {
                e = search_reverse(lisp, e_from, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                firstisString = e->asInteger();
                break;
            }
            case t_float:
            case t_short:
            case t_integer:
            case t_number:
                from = e_from->asInteger();
                if (from < 0)
                    from = size() + from;
                break;
            default:
                e->release();
                e_from->release();
                throw new Error("Error: cannot use the first position in 'setrange'");
        }
        
        e->release();
        e_from->release();
        
        if (from < 0 || from >= size()) {
            last->release();
            return this;
        }
        
        if (nxt == l->size() - 1) {
            //We replace our element in place at e_from
            List* l = (List*)fullcopy();
            return l->replace(lisp, from, last->copying(false));
        }
        
        e_upto = l->liste[nxt];
        switch (e_upto->label()) {
            case l_minus:
                e_upto = l->liste[nxt+1]->eval(lisp);
                ty = e_upto->type;
                if (ty == t_string)
                    ty = t_minus_string;
                else
                    throw new Error("Error: Wrong value after second operator: '-'");
                break;
            case l_plus:
                e_upto = l->liste[nxt+1]->eval(lisp);
                ty = e_upto->type;
                if (ty == t_string)
                    ty = t_plus_string;
                else
                    throw new Error("Error: Wrong value after second operator: '+'");
                break;
            case l_minus_plus:
                e_upto = l->liste[nxt+1]->eval(lisp);
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_minus_plus_string;
                else
                    throw new Error("Error: Wrong value after second operator: '-+'");
                break;
            default:
                e_upto = e_upto->eval(lisp);
                ty = e_upto->type;
        }
        
        e = null_;
        switch (ty) {
            case t_string: {
                if (firstisString == -1) firstisString = 0;
                e = search_element(lisp, e_upto, from + firstisString);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                upto = e->asInteger();
                break;
            }
            case t_plus_string: {
                if (firstisString == -1) firstisString = 0;
                e = search_element(lisp, e_upto, from + firstisString);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                //All characters are integrated
                upto = e->asInteger() + 1;
                break;
            }
            case t_minus_string: {
                e = search_reverse(lisp, e_upto, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                upto = e->asInteger();
                break;
            }
            case t_minus_plus_string: {
                e = search_reverse(lisp, e_upto, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                upto = e->asInteger() - 1;
                break;
            }
            case t_float:
            case t_short:
            case t_integer:
            case t_number:
                upto = e_upto->asInteger();
                if (firstisString != -1 && upto > 0) {
                    //in this case upto is a number of characters, not a position
                    upto += from + firstisString;
                }
                else {
                    if (upto <= 0) {
                        //We start from the end...
                        upto = size() + upto;
                    }
                }
                break;
            default:
                e->release();
                e_upto->release();
                throw new Error("Error: cannot use the second position in 'setrange'");
        }
    }
    catch (Error* err) {
        last->release();
        throw err;
    }
    
    e->release();
    e_upto->release();
    
    if (upto <= from) {
        last->release();
        return this;
    }
    
    if (upto > size())
        upto = size();
    l = lisp->provideList();
    long i = 0;
    for (i = 0; i < from; i++)
        l->append(liste[i]->copying(false));
    l->append(last->copying(false));
    for (i = upto; i < size(); i++)
        l->append(liste[i]->copying(false));
    return l;
}

Element* LList::replace_in(LispE* lisp, List* l) {
    Element* e_from = l->liste[2];
    Element* e;
    Element* last = l->liste.back();
    last = last->eval(lisp);
    Element* e_upto;
    
    long from = 0;
    long upto;

    long firstisString = -1;
    short nxt = 3;
    short ty;
    long sz = size();
    try {
        switch (e_from->label()) {
            case l_minus:
                e_from = l->liste[3]->eval(lisp);
                nxt = 4;
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_minus_string;
                else
                    throw new Error("Error: Wrong value after first operator: '-'");
                break;
            case l_plus:
                e_from = l->liste[3]->eval(lisp);
                nxt = 4;
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_plus_string;
                else
                    throw new Error("Error: Wrong value after first operator: '+'");
                break;
            case l_minus_plus:
                e_from = l->liste[3]->eval(lisp);
                nxt = 4;
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_minus_plus_string;
                else
                    throw new Error("Error: Wrong value after first operator: '-+'");
                break;
            default:
                e_from = e_from->eval(lisp);
                ty = e_from->type;
        }
        
        e = null_;
        switch (ty) {
            case t_string: {
                e = search_element(lisp, e_from, 0);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                from = e->asInteger();
                firstisString = 0;
                break;
            }
            case t_plus_string: {
                e = search_element(lisp, e_from, 0);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                firstisString = e->asInteger();
                break;
            }
            case t_minus_string: {
                e = search_reverse(lisp, e_from, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                //We skip the first characters
                from = e->asInteger() + 1;
                firstisString = 0;
                break;
            }
            case t_minus_plus_string: {
                e = search_reverse(lisp, e_from, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                firstisString = e->asInteger();
                break;
            }
            case t_float:
            case t_short:
            case t_integer:
            case t_number:
                from = e_from->asInteger();
                if (from < 0)
                    from = sz + from;
                break;
            default:
                e->release();
                e_from->release();
                throw new Error("Error: cannot use the first position in 'setrange'");
        }
        
        e->release();
        e_from->release();
        
        if (from < 0 || from >= sz) {
            last->release();
            return this;
        }
        
        if (nxt == l->size() - 1) {
            //We replace our element in place at e_from
            LList* l = (LList*)fullcopy();
            return l->replace(lisp, from, last->copying(false));
        }
        
        e_upto = l->liste[nxt];
        switch (e_upto->label()) {
            case l_minus:
                e_upto = l->liste[nxt+1]->eval(lisp);
                ty = e_upto->type;
                if (ty == t_string)
                    ty = t_minus_string;
                else
                    throw new Error("Error: Wrong value after second operator: '-'");
                break;
            case l_plus:
                e_upto = l->liste[nxt+1]->eval(lisp);
                ty = e_upto->type;
                if (ty == t_string)
                    ty = t_plus_string;
                else
                    throw new Error("Error: Wrong value after second operator: '+'");
                break;
            case l_minus_plus:
                e_upto = l->liste[nxt+1]->eval(lisp);
                ty = e_from->type;
                if (ty == t_string)
                    ty = t_minus_plus_string;
                else
                    throw new Error("Error: Wrong value after second operator: '-+'");
                break;
            default:
                e_upto = e_upto->eval(lisp);
                ty = e_upto->type;
        }
        
        e = null_;
        switch (ty) {
            case t_string: {
                if (firstisString == -1) firstisString = 0;
                e = search_element(lisp, e_upto, from + firstisString);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                upto = e->asInteger();
                break;
            }
            case t_plus_string: {
                if (firstisString == -1) firstisString = 0;
                e = search_element(lisp, e_upto, from + firstisString);
                if (e == null_) {
                    last->release();
                    return emptylist_;
                }
                //All characters are integrated
                upto = e->asInteger() + 1;
                break;
            }
            case t_minus_string: {
                e = search_reverse(lisp, e_upto, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                upto = e->asInteger();
                break;
            }
            case t_minus_plus_string: {
                e = search_reverse(lisp, e_upto, 0);
                if (e == minusone_) {
                    last->release();
                    return emptylist_;
                }
                upto = e->asInteger() - 1;
                break;
            }
            case t_float:
            case t_short:
            case t_integer:
            case t_number:
                upto = e_upto->asInteger();
                if (firstisString != -1 && upto > 0) {
                    //in this case upto is a number of characters, not a position
                    upto += from + firstisString;
                }
                else {
                    if (upto <= 0) {
                        //We start from the end...
                        upto = sz + upto;
                    }
                }
                break;
            default:
                e->release();
                e_upto->release();
                throw new Error("Error: cannot use the second position in 'setrange'");
        }
    }
    catch (Error* err) {
        last->release();
        throw err;
    }
    
    e->release();
    e_upto->release();
    
    if (upto <= from) {
        last->release();
        return this;
    }
    
    LList* ll = new LList(liste.mark);
    u_link*  it;
    it = liste.last();
    for (; sz != upto && it != NULL; it = it->previous(), sz--) {
        ll->push_front(it->value->copying(false));
    }
    ll->push_front(last->copying(false));

    while (it != NULL && sz != from) {
        sz--;
        it = it->previous();
    }
    
    for (; it != NULL; it = it->previous())
        ll->push_front(it->value->copying(false));
    return ll;
}

Element* Floats::replace_in(LispE* lisp, List* l) {
    Element* e_last = l->liste.back()->eval(lisp);
    float last = e_last->asFloat();
    e_last->release();
    
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Floats* l = (Floats*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Floats* n = lisp->provideFloats();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Numbers::replace_in(LispE* lisp, List* l) {
    Element* e_last = l->liste.back()->eval(lisp);
    double last = e_last->asNumber();
    e_last->release();

    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Numbers* l = (Numbers*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Numbers* n = lisp->provideNumbers();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Shorts::replace_in(LispE* lisp, List* l) {
    Element* elast = l->liste.back()->eval(lisp);
    long last = elast->asInteger();
    elast->release();

    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Shorts* l = (Shorts*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Shorts* n = new Shorts();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Integers::replace_in(LispE* lisp, List* l) {
    Element* elast = l->liste.back()->eval(lisp);
    long last = elast->asInteger();
    elast->release();

    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Integers* l = (Integers*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Integers* n = lisp->provideIntegers();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}


Element* Strings::replace_in(LispE* lisp, List* l) {
    Element* e = l->liste.back()->eval(lisp);
    u_ustring last = e->asUString(lisp);
    e->release();

    Element* e_from = l->liste[2];
    
    long from = 0;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }
    
    e = null_;
    switch (ty) {
        case t_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            from = e->asInteger();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            e = search_element(lisp, e_from, 0);
            if (e == null_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            //We skip the first characters
            from = e->asInteger() + 1;
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            if (from < 0)
                from = size() + from;
            break;
        default:
            e->release();
            e_from->release();
            throw new Error("Error: cannot use the first position in 'setrange'");
    }
    
    e->release();
    e_from->release();
    
    if (from < 0 || from >= size())
        return this;
    
    if (nxt == l->size() - 1) {
        //Only one element is returned
        //On returns only one element
        Strings* l = (Strings*)fullcopy();
        l->liste[from] = last;
        return l;
    }
    
    Element* e_upto = l->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }
    
    long upto;
    e = null_;
    switch (ty) {
        case t_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            //All characters are integrated
            upto = e->asInteger() + 1;
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger() - 1;
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = size() + upto;
                }
            }
            break;
        default:
            e->release();
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'setrange'");
    }
    
    e->release();
    e_upto->release();
    if (upto <= from)
        return emptylist_;
    
    if (upto > size())
        upto = size();
    Strings* n = lisp->provideStrings();
    long i;
    for (i = 0; i < from; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* String::replace_in(LispE* lisp, List* liste) {
    Element* e_from = liste->liste.back()->eval(lisp);
    u_ustring last = e_from->asUString(lisp);
    e_from->release();

    e_from = liste->liste[2];
    
    long from;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }
    
    switch (ty) {
        case t_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.find(ch);
            if (from == -1)
                return emptystring_;
            from += ch.size();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.find(ch);
            if (from == -1)
                return emptystring_;
            firstisString = ch.size();
            break;
        }
        case t_minus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.rfind(ch, content.size());
            if (from == -1)
                return emptystring_;
            //We skip the first characters
            from += ch.size();
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.rfind(ch, content.size());
            if (from == -1)
                return emptystring_;
            firstisString = ch.size();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            if (from < 0)
                from = content.size() + from;
            break;
        default:
            e_from->release();
            throw new Error("Error: cannot use the first position in 'setrange'");
    }
    
    e_from->release();
    
    if (from < 0 || from >= content.size())
        return this;
    
    if (nxt == liste->size() - 1) {
        //Only one element is returned
        u_ustring result = content.substr(0, from);
        result += last;
        result += content.substr(from+1, content.size());
        return lisp->provideString(result);
    }
    
    Element* e_upto = liste->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }
    
    long upto;
    
    switch (ty) {
        case t_string: {
            if (firstisString == -1) firstisString = 0;
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            //All characters are integrated
            upto += ch.size();
            break;
        }
        case t_minus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.rfind(ch, content.size());
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_minus_plus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.rfind(ch, content.size());
            if (upto == -1)
                return emptystring_;
            //All characters are integrated
            upto += ch.size();
            break;
        }
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = content.size() + upto;
                }
            }
            break;
        default:
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'setrange'");
    }
    
    e_upto->release();
    if (upto <= from)
        return emptystring_;
    
    if (upto > content.size())
        upto = content.size();
    u_ustring result = content.substr(0, from);
    result += last;
    result += content.substr(upto, content.size());
    return lisp->provideString(result);
}
//------------------------------------------------------------------------------------------
void Listincode::protecting(bool protection, LispE* lisp) {    
    if (protection) {
        if (status == s_constant)
            status = s_protect;
    }
    else {
        if (status == s_protect) {
            status = s_constant;
            lisp->garbaging(this);
        }
    }
    
    for (long i = 0; i < liste.size(); i++)
        liste[i]->protecting(protection, lisp);
}
//------------------------------------------------------------------------------------------

Element* List::duplicate_constant(bool pair) {
    if (status == s_constant) {
        List* l;
        if (pair)
            l =  new Pair();
        else
            l = (List*)newInstance();
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copying(false));
        }
        return l;
    }
    return this;
}

Element* LList::duplicate_constant(bool pair) {
    if (status == s_constant)
        return back_duplicate();
    return this;
}

Element* Dictionary::duplicate_constant(bool pair) {
    if (status == s_constant) {
        Dictionary* d = (Dictionary*)newInstance();
        Element* e;
        for (auto& a: dictionary) {
            e = a.second->copying(false);
            d->dictionary[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}

Element* Floats::duplicate_constant(bool pair) {
    if (status == s_constant) {
        Floats* l = (Floats*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Numbers::duplicate_constant(bool pair) {
    if (status == s_constant) {
        Numbers* l = (Numbers*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Shorts::duplicate_constant(bool pair) {
    if (status == s_constant) {
        Shorts* l = new Shorts;
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Integers::duplicate_constant(bool pair) {
    if (status == s_constant) {
        Integers* l = (Integers*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Strings::duplicate_constant(bool pair) {
    if (status == s_constant) {
        Strings* l = (Strings*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}
//------------------------------------------------------------------------------------------
Element* Element::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    l->append(copying(false));
    return l;
}

Element* Floats::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideFloat(liste[i]));
    return l;
}

Element* Numbers::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideNumber(liste[i]));
    return l;
}

Element* Shorts::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    for (long i = 0; i < liste.size(); i++)
        l->append(new Short(liste[i]));
    return l;
}

Element* Integers::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideInteger(liste[i]));
    return l;
}

Element* Strings::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideString(liste[i]));
    return l;
}

Element* LList::asList(LispE* lisp) {
    List* l =  lisp->provideList();
    for (u_link* a = liste.begin(); a != NULL; a = a->next())
        l->append(a->value);
    return l;
}

Element* Set::asList(LispE* lisp) {
    List* l = lisp->provideList();
    u_ustring v;
    for (auto& a: ensemble) {
        v = a;
        l->append(lisp->provideString(v));
    }
    return l;
}

Element* Set_n::asList(LispE* lisp) {
    List* l = lisp->provideList();
    for (auto& a: ensemble) {
        l->append(lisp->provideNumber(a));
    }
    return l;
}

Element* Set_i::asList(LispE* lisp) {
    List* l = lisp->provideList();
    for (auto& a: ensemble) {
        l->append(lisp->provideInteger(a));
    }
    return l;
}


//------------------------------------------------------------------------------------------
//For running car/cdr, everything that is not List is an error
Element* Element::cadr(LispE* lisp, Element*) {
    throw new Error("Error: You reached the list size limit");
}

Element* Element::car(LispE*) {
    throw new Error("Error: You can only apply 'car' to a list or a string");
}

Element* Element::cdr(LispE*) {
    throw new Error("Error: 'cdr' can only be applied to a list or a string");
}

Element* String::car(LispE* lisp) {
    if (content.size() == 0)
        throw new Error("Error: Empty string");
    return lisp->provideString(content[0]);
}

Element* String::cdr(LispE* lisp) {
    if (content.size() == 0)
        throw new Error("Error: Empty string");
    u_ustring w = content.substr(1, content.size()-1);
    return lisp->provideString(w);
}

Element* Element::cadr(LispE*, u_ustring& actions) {
    throw new Error("Error: You reached the list size limit");
}

Element* List::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    Element* e = this;
    bool pair = (e->type == t_pair);
    
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            e = e->protected_index(lisp, pos);
            if (e == null_)
                throw new Error("Error: You reached the list size limit");
            
            pair = (e->type == t_pair);
            sz = e->size();
            pos = 0;
        }
        else {
            if (pos == sz)
                throw new Error("Error: You reached the list size limit");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        if (pair) {
            //The last one...
            if (pos == sz - 1)
                return e->index(pos);
            return new Pair((Pair*)e, pos);
        }
        else {
            return new Listpool(lisp, (List*)e, pos);
        }
    }
    
    return e;
}

//cadr and cdr do not take into account cycles
//They are bounded by the number of times cdr is called...
//Beware that in the case of recursive calls, you cannot
//expect cadr to return an empty list if there is a cycle within
//This is the main difference with loop, which will detect a cycle and stop
Element* LList::cadr(LispE* lisp, u_ustring& action) {
    u_link* it = liste.first;
    Element* e = this;
    
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            if (it == NULL)
                throw new Error("Error: You reached the list size limit");

            e = it->value;
            if (i == 0)
                return e;
            u_ustring act = action.substr(0, i);
            return e->cadr(lisp, act);
        }
        else {
            if (it == NULL)
                throw new Error("Error: You reached the list size limit");
            //We do not try to detect a cycle here...
            it = it->_next;
        }
    }
    
    if (it != NULL)
        return new LList(this, it);
    
    return e;
}

Element* Floats::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    
    for (i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            if (i)
                throw new Error("Error: the elements of a list of values are no lists");
            return lisp->provideFloat(liste[pos]);
        }
        if (pos == sz)
            throw new Error("Error: You reached the list size limit");
        pos++;
    }

    if (pos) {
        if (pos == sz)
            return null_;
        return lisp->provideFloats(this, pos);
    }
    
    return null_;
}

Element* Numbers::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    
    for (i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            if (i)
                throw new Error("Error: the elements of a list of values are no lists");
            return lisp->provideNumber(liste[pos]);
        }
        if (pos == sz)
            throw new Error("Error: You reached the list size limit");
        pos++;
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        return lisp->provideNumbers(this, pos);
    }
    
    return null_;
}

Element* String::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    u_ustring u;

    for (i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            if (i)
                throw new Error("Error: cannot apply 'car/cdr' to one character");
            u = content[pos];
            return lisp->provideString(u);
        }
        else {
            if (pos == sz)
                throw new Error("Error: You reached the list size limit");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        u = content.substr(pos, sz);
        return lisp->provideString(u);
    }
    
    return null_;
}

Element* Strings::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    
    for (i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            if (!i)
                return lisp->provideString(liste[pos]);
            exchange_value.content = liste[pos];
            u_ustring nxt = action.substr(0, i);
            return exchange_value.cadr(lisp, nxt);
        }
        
        if (pos == sz)
            throw new Error("Error: You reached the list size limit");
        pos++;
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        return new Strings(this, pos);
    }
    
    return null_;
}

Element* Shorts::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    
    for (i = action.size() - 1; i>= 0; i--) {
        for (i = action.size() - 1; i>= 0; i--) {
            if (action[i] == 'a') {
                if (i)
                    throw new Error("Error: the elements of a list of values are no lists");
                return new Short(liste[pos]);
            }
            if (pos == sz)
                throw new Error("Error: You reached the list size limit");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        return new Shorts(this, pos);
    }
    
    return null_;
}


Element* Integers::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    
    for (i = action.size() - 1; i>= 0; i--) {
        for (i = action.size() - 1; i>= 0; i--) {
            if (action[i] == 'a') {
                if (i)
                    throw new Error("Error: the elements of a list of values are no lists");
                return lisp->provideInteger(liste[pos]);
            }
            if (pos == sz)
                throw new Error("Error: You reached the list size limit");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        return lisp->provideIntegers(this, pos);
    }
    
    return null_;
}


Element* List::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return liste[0];
}

Element* List::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new Listpool(lisp, this, 1);
}

Element* LList::car(LispE* lisp) {
    if (liste.empty())
        return null_;
    return liste.front();
}

//Note that cycles are not taken into account here
//If the next element points inside the list again
//then cdr will return the list at this point...
//There is potentially a infinite potential loop
//in the list...
Element* LList::cdr(LispE* lisp) {
    u_link* it = liste.first;
    if (it == NULL || it->_next == NULL)
        return null_;
    it = it->_next;
    return new LList(this, it);
}

Element* Floats::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideFloat(liste[0]);
}

Element* Floats::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return lisp->provideFloats(this, 1);
}

Element* Numbers::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideNumber(liste[0]);
}

Element* Numbers::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return lisp->provideNumbers(this, 1);
}

Element* Shorts::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return new Short(liste[0]);
}

Element* Integers::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideInteger(liste[0]);
}

Element* Shorts::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new Shorts(this, 1);
}

Element* Integers::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return lisp->provideIntegers(this, 1);
}

Element* Strings::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideString(liste[0]);
}

Element* Strings::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new Strings(this, 1);
}


Element* Pair::cdr(LispE* lisp) {
    long sz = liste.size();
    if (!sz)
        return null_;
    
    if (sz == 2)
        return liste.back();
    
    return new Pair(this, 1);
}

Element* InfiniterangeNumber::car(LispE* lisp) {
    return lisp->provideNumber(initial_value);
}

Element* InfiniterangeNumber::cdr(LispE* lisp) {
    return new InfiniterangeNumber(initial_value+increment, increment);
}

Element* InfiniterangeInteger::car(LispE* lisp) {
    return lisp->provideInteger(initial_value);
}

Element* InfiniterangeInteger::cdr(LispE* lisp) {
    return new InfiniterangeInteger(initial_value+increment, increment);
}
//------------------------------------------------------------------------------------------

Element* Set_i::fullcopy() {
    return new Set_i(ensemble);
}

Element* Set_i::copying(bool duplicate) {
    if (exchange_value.provide && exchange_value.lisp->preparingthread)
        return new Set_i(ensemble);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return new Set_i(ensemble);
}

Element* Set_ipool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_i(this);
}

Element* Set_ipool::newInstance() {
    return lisp->provideSet_i();
}

Element* Set_ipool::fullcopy() {
    if (lisp->preparingthread)
        return new Set_i(ensemble);
    else
        return lisp->provideSet_i(this);
}

Element* Set_ipool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->preparingthread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->preparingthread)
        return new Set_i(ensemble);
    else
        return lisp->provideSet_i(this);
}

void Set_ipool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        ensemble.clear();
        lisp->seti_pool.push_back(this);
    }
}


void Set_ipool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status-=nb;
    if (!status) {
        ensemble.clear();
        lisp->seti_pool.push_back(this);
    }
}

void Set_ipool::release() {
    if (!status) {
        ensemble.clear();
        lisp->seti_pool.push_back(this);
    }
}

Element* Set_i::minimum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    long w = 0;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            w = a;
            first = false;
        }
        else
            if (w < a)
                w = a;
    }
    
    return lisp->provideInteger(w);
}

Element* Set_i::maximum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    long w = 0;
    bool first = true;
    for (auto& a : ensemble) {
        if (first) {
            w = a;
            first = false;
        }
        else
            if (w > a)
                w = a;
    }
    
    return lisp->provideInteger(w);
}

Element* Set_i::minmax(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    long v_min = 0;
    long v_max = 0;
    bool first = true;
    
    for (auto& a : ensemble) {
        if (first) {
            v_min = a;
            v_max = a;
            first = false;
        }
        else {
            if (v_max < a)
                v_max = a;
            else
                if (v_min > a)
                    v_min = a;
        }
    }
    Numbers* f = lisp->provideNumbers();
    f->liste.push_back(v_min);
    f->liste.push_back(v_max);
    return f;
}

void Set_i::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    for (auto& a : ensemble) {
        l->append(lisp->provideInteger(a));
    }
}

Element* Set_i::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Integer* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        element = lisp->provideInteger(a);
        lisp->replacingvalue(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Set_i::thekeys(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideInteger(a));
    }
    return keys;
}

Element* Set_i::thevalues(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideInteger(a));
    }
    return keys;
}

Element* Set_i::search_element(LispE* lisp, Element* valeur, long ix) {
    long k = valeur->asInteger();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_i::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asInteger()) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_i::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    long keyvalue = valeur->asInteger();
    if (ensemble.find(keyvalue) != ensemble.end()) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asInteger());
        return one_;
    }
    return zero_;
}

Element* Set_i::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    long keyvalue = valeur->asInteger();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_i::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long keyvalue = valeur->asInteger();
    if (ensemble.find(keyvalue) == ensemble.end())
        return zero_;
    return one_;
}

Element* Set_i::search_reverse(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    long keyvalue = valeur->asInteger();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_i::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (auto& a: ensemble) {
            if (!i) {
                return lisp->provideInteger(a);
            }
            i--;
        }
    }
    return null_;
}

Element* Set_i::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (auto& a: ensemble) {
            if (!i) {
                return lisp->provideInteger(a);
            }
            i--;
        }
    }
    return null_;
}

Element* Set_i::value_from_index(LispE* lisp, long i) {
    for (auto& a: ensemble) {
        if (!i) {
            return lisp->provideInteger(a);
        }
        i--;
    }
    return null_;
}

Element* Set_i::value_on_index(LispE* lisp, Element* ix) {
    long k = ix->asInteger();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideInteger(k);
}

Element* Set_i::protected_index(LispE* lisp, Element* ix) {
    long k = ix->asInteger();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideInteger(k);
}

Element* Set_i::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep == U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: ensemble) {
        str += beg;
        beg = sep;
        str += convertToUString(a);
    }
    return lisp->provideString(str);
}

Element* Set_i::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_seti && ensemble == ((Set_i*)e)->ensemble)];
}

bool Set_i::egal(Element* e) {
    return (e->type == t_seti && ensemble == ((Set_i*)e)->ensemble);
}
