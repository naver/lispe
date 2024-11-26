/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  dictionaries.cxx
//
//


#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>


Dictionary_as_list::Dictionary_as_list(LispE* lisp, List* l) : Element(t_dictionary) {
    choice = true;
    purekeys = true;
    select = true;
    
    //To the bits are set to zeo
    keyvalue = 0;
    mxkeyvalue = 0;
    Element* e;
    for (long  i = 1; i < l->size(); i += 2) {
        e = l->index(i);
        if (!e->isString() && !e->isNumber())
            purekeys = false;
        else {
            //We record its position in bit vector
            mxkeyvalue = keyvalues.size();
            keyvalue |= ((uint64_t)1 << mxkeyvalue++);
        }
        keyvalues.push_back(e);
        valuevalues.push_back(l->index(i + 1));
    }
}


Element* Dictionarypool::newInstance() {
    return lisp->provideDictionary();
}

Element* Dictionarypool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Dictionary* d;
    if (lisp->create_in_thread)
        d = new Dictionary;
    else
        d = lisp->provideDictionary();
    object = d;
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->fullcopy();
        d->dictionary[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Dictionarypool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary* d = lisp->provideDictionary();
    Element* e;
    for (const auto& a: dictionary) {
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
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    
    Dictionary* d;
    if (lisp->create_in_thread)
        d = new Dictionary;
    else
        d = lisp->provideDictionary();
    Element* e;
    for (const auto& a: dictionary) {
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
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionary_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Dictionarypool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    
    status-=nb;
    if (!status) {
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionary_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Dictionarypool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (const auto& a: dictionary)
            a.second->decrement();
        marking = false;
        dictionary.clear();
        lisp->dictionary_pool.push_max(lisp->max_size, this);
    }
}

void Dictionarypool::append(Element* e) {
    if (choice)
        u_key = e->asUString(NULL);
    else {
        dictionary[u_key] = e;
        e->increment();
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


Element* Dictionary_npool::newInstance() {
    return lisp->provideDictionary_n();
}

Element* Dictionary_npool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Dictionary_n* d;
    if (lisp->create_in_thread)
        d = new Dictionary_n;
    else
        d = lisp->provideDictionary_n();
    object = d;
    Element* e;
    for (const auto& a: dictionary) {
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
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    Dictionary_n* d;
    if (lisp->create_in_thread)
        d = new Dictionary_n;
    else
        d = lisp->provideDictionary_n();
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Dictionary_npool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary_n* d = lisp->provideDictionary_n();
    Element* e;
    for (const auto& a: dictionary) {
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
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionaryn_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Dictionary_npool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status-=nb;
    if (!status) {
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionaryn_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Dictionary_npool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (const auto& a: dictionary)
            a.second->decrement();
        marking = false;
        dictionary.clear();
        lisp->dictionaryn_pool.push_max(lisp->max_size, this);
    }
}

Element* Dictionary_ipool::newInstance() {
    return lisp->provideDictionary_i();
}

Element* Dictionary_ipool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Dictionary_i* d;
    if (lisp->create_in_thread)
        d = new Dictionary_i;
    else
        d = lisp->provideDictionary_i();
    object = d;
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->fullcopy();
        d->dictionary[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Dictionary_ipool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    Dictionary_i* d;
    if (lisp->create_in_thread)
        d = new Dictionary_i;
    else
        d = lisp->provideDictionary_i();
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Dictionary_ipool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary_i* d = lisp->provideDictionary_i();
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

void Dictionary_ipool::decrement() {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status--;
    if (!status) {
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionaryi_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Dictionary_ipool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status-=nb;
    if (!status) {
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->dictionaryi_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Dictionary_ipool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (const auto& a: dictionary)
            a.second->decrement();
        marking = false;
        dictionary.clear();
        lisp->dictionaryi_pool.push_max(lisp->max_size, this);
    }
}

Element* Dictionary::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary::minmax(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (const auto& a : dictionary) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second)->Boolean())
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second)->Boolean())
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

void Dictionary::flatten(LispE* lisp, List* l) {
    u_ustring k;
    for (const auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideString(k));
        a.second->flatten(lisp, l);
    }
}

void Dictionary::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (const auto& a : dictionary) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

Element* Dictionary::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    
    String* element = lisp->provideString();
    lisp->recording(element, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    Strings* _keys = lisp->provideStrings();
    for (const auto& a: dictionary)
        _keys->liste.push_back(a.first);
    try {
        for (long i = 0; i < _keys->size(); i++) {
            e->release();
            e = lisp->get_variable(label);
            if (e != element) {
                if (e->type != t_string) {
                    e = lisp->provideString(_keys->liste[i]);
                    lisp->recording(e, label);
                }
                else
                    ((String*)e)->content = _keys->liste[i];
                element = (String*)e;
            }
            else
                element->content = _keys->liste[i];
            e = null_;
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
    catch (Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
}

Element* Dictionary::thekeys(LispE* lisp) {
    Strings* dkeys = lisp->provideStrings();
    u_ustring keyvalue;
    for (const auto& a: dictionary) {
        keyvalue = a.first;
        dkeys->append(keyvalue);
    }
    return dkeys;
}

Element* Dictionary::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (const auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Dictionary::search_element(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            u_ustring keyvalue = a.first;
            return lisp->provideString(keyvalue);
        }
    }
    return null_;
}

bool Dictionary::check_element(LispE* lisp, Element* valeur) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            u_ustring keyvalue = a.first;
            return true;
        }
    }
    return false;
}

Element* Dictionary::checkkey(LispE* lisp, Element* e) {
    auto it = dictionary.find(e->asUString(lisp));
    return (it == dictionary.end())?null_:it->second;
}

Element* Dictionary::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    if (remp->equal(lisp, valeur))
        return zero_;

    long nb = 0;
    Element* novel = remp->copying(false);
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            a.second->decrement();
            a.second = novel;
            novel->increment();
            nb++;
        }
    }
    if (novel != remp)
        novel->release();
    return lisp->provideInteger(nb);
}

Element* Dictionary::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue;
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
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

Element* Dictionary::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    u_ustring keyvalue;
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            keyvalue = a.first;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Dictionary::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            u_ustring keyvalue = a.first;
            return lisp->provideString(keyvalue);
        }
    }
    return null_;
}

Element* Dictionary::reverse(LispE* lisp, bool duplicate) {
    Dictionary* dico = lisp->provideDictionary();
    
    u_ustring k;
    Element* e;
    for (const auto& a: dictionary) {
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

Element* Dictionary::value_on_index(u_ustring& k, LispE* lisp) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary::value_on_index(wstring& u, LispE* lisp) {
    u_pstring k = _w_to_u(u);
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary::protected_index(LispE* lisp, u_ustring& k) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Dictionary::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    auto it = dictionary.find(k);
    if (it == dictionary.end())
        throw new Error("Error: index out of bounds");
    return it->second;
}

Element* Dictionary::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0) || e == this)];
}

bool Dictionary::egal(Element* e) {
    return ((e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0) || e == this);
}

Element* Dictionary::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Dictionary* d = lisp->provideDictionary();
        Element* e;
        for (const auto& a: dictionary) {
            e = a.second->copying(true);
            d->dictionary[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}


Element* Dictionary_i::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Dictionary_i* d = lisp->provideDictionary_i();
        Element* e;
        for (const auto& a: dictionary) {
            e = a.second->copying(true);
            d->dictionary[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}

Element* Dictionary_n::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Dictionary_n* d = lisp->provideDictionary_n();
        Element* e;
        for (const auto& a: dictionary) {
            e = a.second->copying(true);
            d->dictionary[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}

Element* Dictionary_i::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_i::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_i::minmax(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (const auto& a : dictionary) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second)->Boolean())
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second)->Boolean())
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

void Dictionary_i::flatten(LispE* lisp, List* l) {
    long k;
    for (const auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideNumber(k));
        a.second->flatten(lisp, l);
    }
}

void Dictionary_i::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (const auto& a : dictionary) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

Element* Dictionary_i::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Integer* element = lisp->provideInteger(0);
    lisp->recording(element, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    Integers* _keys = lisp->provideIntegers();
    for (const auto& a: dictionary)
        _keys->liste.push_back(a.first);
    try {
        for (long a_key = 0; a_key < _keys->liste.size(); a_key++) {
            e->release();
            e = lisp->get_variable(label);
            if (e != element) {
                if (e->type != t_integer) {
                    e = lisp->provideInteger(_keys->liste[a_key]);
                    lisp->recording(e, label);
                }
                else
                    ((Integer*)e)->content = _keys->liste[a_key];
                element = (Integer*)e;
            }
            else
                element->content = _keys->liste[a_key];
            e = null_;
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
    catch (Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
    
}

Element* Dictionary_i::thekeys(LispE* lisp) {
    Integers* dkeys = lisp->provideIntegers();
    for (const auto& a: dictionary) {
        dkeys->liste.push_back(a.first);
    }
    return dkeys;
}

Element* Dictionary_i::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (const auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Dictionary_i::search_element(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

bool Dictionary_i::check_element(LispE* lisp, Element* valeur) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return true;
    }
    return false;
}

Element* Dictionary_i::checkkey(LispE* lisp, Element* e) {
    auto it = dictionary.find(e->asInteger());
    return (it == dictionary.end())?null_:it->second;
}

Element* Dictionary_i::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    if (remp->equal(lisp, valeur))
        return zero_;
    
    long nb = 0;
    Element* novel = remp->copying(false);
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            a.second->decrement();
            a.second = novel;
            novel->increment();
            nb++;
        }
    }
    if (novel != remp)
        novel->release();
    return lisp->provideInteger(nb);
}

Element* Dictionary_i::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            l->liste.push_back(a.first);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Dictionary_i::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Dictionary_i::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

Element* Dictionary_i::reverse(LispE* lisp, bool duplicate) {
    Dictionary_i* dico = lisp->provideDictionary_i();
    
    long k;
    Element* e;
    for (const auto& a: dictionary) {
        k = a.second->asInteger();
        e = dico->dictionary[k];
        if (e == NULL) {
            e = lisp->provideIntegers();
            dico->dictionary[k] = e;
            e->increment();
        }
        ((Integers*)e)->liste.push_back(a.first);
    }
    return dico;
}

Element* Dictionary_i::value_on_index(long k, LispE* lisp) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary_i::protected_index(LispE* lisp, long k) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Dictionary_i::value_on_index(LispE* lisp, Element* ix) {
    long v = ix->checkInteger(lisp);
    auto it = dictionary.find(v);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary_i::protected_index(LispE* lisp, Element* ix) {
    long v = ix->checkInteger(lisp);
    auto it = dictionary.find(v);
    if (it == dictionary.end())
        throw new Error("Error: index out of bounds");
    return it->second;
}

Element* Dictionary_i::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += convertToUString(a.first);
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary_i::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionaryi && e->size() == 0 && dictionary.size() == 0) || e== this)];
}

bool Dictionary_i::egal(Element* e) {
    return ((e->type == t_dictionaryi && e->size() == 0 && dictionary.size() == 0) || e== this);
}

Element* Dictionary_n::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_n::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_n::minmax(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (const auto& a : dictionary) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second)->Boolean())
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second)->Boolean())
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

void Dictionary_n::flatten(LispE* lisp, List* l) {
    double k;
    for (const auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideNumber(k));
        a.second->flatten(lisp, l);
    }
}

void Dictionary_n::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (const auto& a : dictionary) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

Element* Dictionary_n::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Number* element = lisp->provideNumber(0);
    lisp->recording(element, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    Numbers* _keys = lisp->provideNumbers();
    for (const auto& a: dictionary)
        _keys->liste.push_back(a.first);
    try {
        for (long a_key = 0; a_key < _keys->liste.size(); a_key++) {
            e->release();
            e = lisp->get_variable(label);
            if (e != element) {
                if (e->type != t_number) {
                    e = lisp->provideNumber(_keys->liste[a_key]);
                    lisp->recording(e, label);
                }
                else
                    ((Number*)e)->content = _keys->liste[a_key];
                element = (Number*)e;
            }
            else
                element->content = _keys->liste[a_key];
            e = null_;
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
    catch (Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
    
}

Element* Dictionary_n::thekeys(LispE* lisp) {
    Numbers* dkeys = lisp->provideNumbers();
    for (const auto& a: dictionary) {
        dkeys->liste.push_back(a.first);
    }
    return dkeys;
}

Element* Dictionary_n::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (const auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Dictionary_n::search_element(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

bool Dictionary_n::check_element(LispE* lisp, Element* valeur) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return true;
    }
    return false;
}

Element* Dictionary_n::checkkey(LispE* lisp, Element* e) {
    auto it = dictionary.find(e->asNumber());
    return (it == dictionary.end())?null_:it->second;
}

Element* Dictionary_n::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    if (remp->equal(lisp, valeur))
        return zero_;

    long nb = 0;
    Element* novel = remp->copying(false);
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            a.second->decrement();
            a.second = novel;
            novel->increment();
            nb++;
        }
    }
    if (novel != remp)
        novel->release();
    return lisp->provideInteger(nb);
}

Element* Dictionary_n::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            l->liste.push_back(a.first);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Dictionary_n::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Dictionary_n::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : dictionary) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

Element* Dictionary_n::reverse(LispE* lisp, bool duplicate) {
    Dictionary_n* dico = lisp->provideDictionary_n();
    
    double k;
    Element* e;
    for (const auto& a: dictionary) {
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

Element* Dictionary_n::value_on_index(double k, LispE* lisp) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary_n::protected_index(LispE* lisp, double k) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Dictionary_n::value_on_index(LispE* lisp, Element* ix) {
    double v = ix->checkNumber(lisp);
    auto it = dictionary.find(v);
    return (it == dictionary.end())?null_:it->second->copying(false);
}

Element* Dictionary_n::protected_index(LispE* lisp, Element* ix) {
    double v = ix->checkNumber(lisp);
    auto it = dictionary.find(v);
    if (it == dictionary.end())
        throw new Error("Error: index out of bounds");
    return it->second;
}

Element* Dictionary_n::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += convertToUString(a.first);
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary_n::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0) || e== this)];
}

bool Dictionary_n::egal(Element* e) {
    return ((e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0) || e== this);
}

Element* Dictionary::next_iter(LispE* lisp, void* it) {
    std::unordered_map<u_ustring, Element*>::iterator* n = (std::unordered_map<u_ustring, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;
    
    u_ustring u = (*n)->first;
    (*n)++;
    return lisp->provideString(u);
}

Element* Dictionary::next_iter_exchange(LispE* lisp, void* it) {
    std::unordered_map<u_ustring, Element*>::iterator* n = (std::unordered_map<u_ustring, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;
    
    u_ustring u = (*n)->first;
    (*n)++;
    return lisp->provideString(u);
}

Element* Dictionary_i::next_iter(LispE* lisp, void* it) {
    std::unordered_map<long, Element*>::iterator* n = (std::unordered_map<long, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;

    long u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

Element* Dictionary_i::next_iter_exchange(LispE* lisp, void* it) {
    std::unordered_map<long, Element*>::iterator* n = (std::unordered_map<long, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;

    long u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

Element* Dictionary_n::next_iter(LispE* lisp, void* it) {
    std::unordered_map<double, Element*>::iterator* n = (std::unordered_map<double, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;
    double u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

Element* Dictionary_n::next_iter_exchange(LispE* lisp, void* it) {
    std::unordered_map<double, Element*>::iterator* n = (std::unordered_map<double, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;
    double u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

//------- Trees----
Element* Treepool::newInstance() {
    return lisp->provideTree();
}

Element* Treepool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Tree* d;
    if (lisp->create_in_thread)
        d = new Tree;
    else
        d = lisp->provideTree();
    object = d;
    Element* e;
    for (const auto& a: tree) {
        e = a.second->fullcopy();
        d->tree[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Treepool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    Tree* d = lisp->provideTree();
    Element* e;
    for (const auto& a: tree) {
        e = a.second->copying(false);
        d->tree[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Treepool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    
    Tree* d;
    if (lisp->create_in_thread)
        d = new Tree;
    else
        d = lisp->provideTree();
    Element* e;
    for (const auto& a: tree) {
        e = a.second->copying(false);
        d->tree[a.first] = e;
        e->increment();
    }
    return d;
}

void Treepool::decrement() {
    if (is_protected() || marking)
        return;
    
    marking = true;
    
    status--;
    if (!status) {
        for (const auto& a : tree)
            a.second->decrement();
        tree.clear();
        lisp->tree_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Treepool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    
    status-=nb;
    if (!status) {
        for (const auto& a : tree)
            a.second->decrement();
        tree.clear();
        lisp->tree_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Treepool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (const auto& a: tree)
            a.second->decrement();
        marking = false;
        tree.clear();
        lisp->tree_pool.push_max(lisp->max_size, this);
    }
}

void Treepool::append(LispE* lisp, u_ustring& k) {
    if (choice)
        u_key = k;
    else {
        Element* e = lisp->provideString(k);
        tree[u_key] = e;
        e->increment();
        reversechoice();
    }
}

void Treepool::append(LispE* lisp, double v) {
    if (choice)
        u_key = convertToUString(v);
    else {
        Element* e = lisp->provideNumber(v);
        tree[u_key] = e;
        e->increment();
        reversechoice();
    }
}

void Treepool::append(LispE* lisp, long v) {
    if (choice)
        u_key = convertToUString(v);
    else {
        Element* e = lisp->provideInteger(v);
        tree[u_key] = e;
        e->increment();
        reversechoice();
    }
}


Element* Tree_npool::newInstance() {
    return lisp->provideTree_n();
}

Element* Tree_npool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Tree_n* d;
    if (lisp->create_in_thread)
        d = new Tree_n;
    else
        d = lisp->provideTree_n();
    object = d;
    Element* e;
    for (const auto& a: tree) {
        e = a.second->fullcopy();
        d->tree[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Tree_npool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    Tree_n* d;
    if (lisp->create_in_thread)
        d = new Tree_n;
    else
        d = lisp->provideTree_n();
    Element* e;
    for (const auto& a: tree) {
        e = a.second->copying(false);
        d->tree[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Tree_npool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    Tree_n* d = lisp->provideTree_n();
    Element* e;
    for (const auto& a: tree) {
        e = a.second->copying(false);
        d->tree[a.first] = e;
        e->increment();
    }
    return d;
}

void Tree_npool::decrement() {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status--;
    if (!status) {
        for (const auto& a : tree)
            a.second->decrement();
        tree.clear();
        lisp->treen_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Tree_npool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status-=nb;
    if (!status) {
        for (const auto& a : tree)
            a.second->decrement();
        tree.clear();
        lisp->treen_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Tree_npool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (const auto& a: tree)
            a.second->decrement();
        marking = false;
        tree.clear();
        lisp->treen_pool.push_max(lisp->max_size, this);
    }
}

Element* Tree_ipool::newInstance() {
    return lisp->provideTree_i();
}

Element* Tree_ipool::fullcopy() {
    if (marking)
        return object;
    
    marking = true;
    Tree_i* d;
    if (lisp->create_in_thread)
        d = new Tree_i;
    else
        d = lisp->provideTree_i();
    object = d;
    Element* e;
    for (const auto& a: tree) {
        e = a.second->fullcopy();
        d->tree[a.first] = e;
        e->increment();
    }
    marking = false;
    return d;
}

Element* Tree_ipool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    Tree_i* d;
    if (lisp->create_in_thread)
        d = new Tree_i;
    else
        d = lisp->provideTree_i();
    Element* e;
    for (const auto& a: tree) {
        e = a.second->copying(false);
        d->tree[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Tree_ipool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    Tree_i* d = lisp->provideTree_i();
    Element* e;
    for (const auto& a: tree) {
        e = a.second->copying(false);
        d->tree[a.first] = e;
        e->increment();
    }
    return d;
}

void Tree_ipool::decrement() {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status--;
    if (!status) {
        for (const auto& a : tree)
            a.second->decrement();
        tree.clear();
        lisp->treei_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Tree_ipool::decrementstatus(uint16_t nb) {
    if (is_protected() || marking)
        return;
    
    marking = true;
    status-=nb;
    if (!status) {
        for (const auto& a : tree)
            a.second->decrement();
        tree.clear();
        lisp->treei_pool.push_max(lisp->max_size, this);
    }
    marking = false;
}

void Tree_ipool::release() {
    if (!status) {
        if (marking)
            return;
        marking = true;
        for (const auto& a: tree)
            a.second->decrement();
        marking = false;
        tree.clear();
        lisp->treei_pool.push_max(lisp->max_size, this);
    }
}

Element* Tree::minimum(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : tree) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Tree::maximum(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : tree) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Tree::minmax(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (const auto& a : tree) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second)->Boolean())
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second)->Boolean())
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

void Tree::flatten(LispE* lisp, List* l) {
    u_ustring k;
    for (const auto& a: tree) {
        k = a.first;
        l->append(lisp->provideString(k));
        a.second->flatten(lisp, l);
    }
}

void Tree::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (const auto& a : tree) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

Element* Tree::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    
    String* element = lisp->provideString();
    lisp->recording(element, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the tree is changed
    //in the following instructions
    Strings* _keys = lisp->provideStrings();
    for (const auto& a: tree)
        _keys->liste.push_back(a.first);
    try {
        for (long i = 0; i < _keys->size(); i++) {
            e->release();
            e = lisp->get_variable(label);
            if (e != element) {
                if (e->type != t_string) {
                    e = lisp->provideString(_keys->liste[i]);
                    lisp->recording(e, label);
                }
                else
                    ((String*)e)->content = _keys->liste[i];
                element = (String*)e;
            }
            else
                element->content = _keys->liste[i];
            e = null_;
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
    catch (Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
}

Element* Tree::thekeys(LispE* lisp) {
    Strings* dkeys = lisp->provideStrings();
    u_ustring keyvalue;
    for (const auto& a: tree) {
        keyvalue = a.first;
        dkeys->append(keyvalue);
    }
    return dkeys;
}

Element* Tree::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (const auto& a: tree) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Tree::search_element(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            u_ustring keyvalue = a.first;
            return lisp->provideString(keyvalue);
        }
    }
    return null_;
}

bool Tree::check_element(LispE* lisp, Element* valeur) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            u_ustring keyvalue = a.first;
            return true;
        }
    }
    return false;
}

Element* Tree::checkkey(LispE* lisp, Element* e) {
    auto it = tree.find(e->asUString(lisp));
    return (it == tree.end())?null_:it->second;
}

Element* Tree::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    if (remp->equal(lisp, valeur))
        return zero_;

    long nb = 0;
    Element* novel = remp->copying(false);
    for (auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            a.second->decrement();
            a.second = novel;
            novel->increment();
            nb++;
        }
    }
    if (novel != remp)
        novel->release();
    return lisp->provideInteger(nb);
}

Element* Tree::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue;
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
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

Element* Tree::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    u_ustring keyvalue;
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            keyvalue = a.first;
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* Tree::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            u_ustring keyvalue = a.first;
            return lisp->provideString(keyvalue);
        }
    }
    return null_;
}

Element* Tree::reverse(LispE* lisp, bool duplicate) {
    Tree* dico = lisp->provideTree();
    
    u_ustring k;
    Element* e;
    for (const auto& a: tree) {
        k = a.second->asUString(lisp);
        e = dico->tree[k];
        if (e == NULL) {
            e = lisp->provideStrings();
            dico->tree[k] = e;
            e->increment();
        }
        k = a.first;
        ((Strings*)e)->append(k);
    }
    return dico;
}

Element* Tree::value_on_index(u_ustring& k, LispE* lisp) {
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree::value_on_index(wstring& u, LispE* lisp) {
    u_pstring k = _w_to_u(u);
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree::protected_index(LispE* lisp, u_ustring& k) {
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second;
}

Element* Tree::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    auto it = tree.find(k);
    if (it == tree.end())
        throw new Error("Error: index out of bounds");
    return it->second;
}

Element* Tree::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: tree) {
        str += beg;
        beg = sep;
        str += a.first;
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Tree::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_tree && e->size() == 0 && tree.size() == 0) || e == this)];
}

bool Tree::egal(Element* e) {
    return ((e->type == t_tree && e->size() == 0 && tree.size() == 0) || e == this);
}

Element* Tree::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Tree* d = lisp->provideTree();
        Element* e;
        for (const auto& a: tree) {
            e = a.second->copying(true);
            d->tree[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}


Element* Tree_i::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Tree_i* d = lisp->provideTree_i();
        Element* e;
        for (const auto& a: tree) {
            e = a.second->copying(true);
            d->tree[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}

Element* Tree_n::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Tree_n* d = lisp->provideTree_n();
        Element* e;
        for (const auto& a: tree) {
            e = a.second->copying(true);
            d->tree[a.first] = e;
            e->increment();
        }
        return d;
    }
    return this;
}

Element* Tree_i::minimum(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : tree) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Tree_i::maximum(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : tree) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Tree_i::minmax(LispE* lisp) {
    if (!tree.size())
        return null_;
    
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (const auto& a : tree) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second)->Boolean())
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second)->Boolean())
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

void Tree_i::flatten(LispE* lisp, List* l) {
    long k;
    for (const auto& a: tree) {
        k = a.first;
        l->append(lisp->provideNumber(k));
        a.second->flatten(lisp, l);
    }
}

void Tree_i::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (const auto& a : tree) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

Element* Tree_i::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Integer* element = lisp->provideInteger(0);
    lisp->recording(element, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the tree is changed
    //in the following instructions
    Integers* _keys = lisp->provideIntegers();
    for (const auto& a: tree)
        _keys->liste.push_back(a.first);
    try {
        for (long a_key = 0; a_key < _keys->liste.size(); a_key++) {
            e->release();
            e = lisp->get_variable(label);
            if (e != element) {
                if (e->type != t_integer) {
                    e = lisp->provideInteger(_keys->liste[a_key]);
                    lisp->recording(e, label);
                }
                else
                    ((Integer*)e)->content = _keys->liste[a_key];
                element = (Integer*)e;
            }
            else
                element->content = _keys->liste[a_key];
            e = null_;
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
    catch (Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
    
}

Element* Tree_i::thekeys(LispE* lisp) {
    Integers* dkeys = lisp->provideIntegers();
    for (const auto& a: tree) {
        dkeys->liste.push_back(a.first);
    }
    return dkeys;
}

Element* Tree_i::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (const auto& a: tree) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Tree_i::search_element(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

bool Tree_i::check_element(LispE* lisp, Element* valeur) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return true;
    }
    return false;
}

Element* Tree_i::checkkey(LispE* lisp, Element* e) {
    auto it = tree.find(e->asInteger());
    return (it == tree.end())?null_:it->second;
}

Element* Tree_i::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    if (remp->equal(lisp, valeur))
        return zero_;
    
    long nb = 0;
    Element* novel = remp->copying(false);
    for (auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            a.second->decrement();
            a.second = novel;
            novel->increment();
            nb++;
        }
    }
    if (novel != remp)
        novel->release();
    return lisp->provideInteger(nb);
}

Element* Tree_i::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Integers* l = lisp->provideIntegers();
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            l->liste.push_back(a.first);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Tree_i::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Tree_i::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

Element* Tree_i::reverse(LispE* lisp, bool duplicate) {
    Tree_i* dico = lisp->provideTree_i();
    
    long k;
    Element* e;
    for (const auto& a: tree) {
        k = a.second->asInteger();
        e = dico->tree[k];
        if (e == NULL) {
            e = lisp->provideIntegers();
            dico->tree[k] = e;
            e->increment();
        }
        ((Integers*)e)->liste.push_back(a.first);
    }
    return dico;
}

Element* Tree_i::value_on_index(long k, LispE* lisp) {
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree_i::protected_index(LispE* lisp, long k) {
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second;
}

Element* Tree_i::value_on_index(LispE* lisp, Element* ix) {
    long v = ix->checkInteger(lisp);
    auto it = tree.find(v);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree_i::protected_index(LispE* lisp, Element* ix) {
    long v = ix->checkInteger(lisp);
    auto it = tree.find(v);
    if (it == tree.end())
        throw new Error("Error: index out of bounds");
    return it->second;
}

Element* Tree_i::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: tree) {
        str += beg;
        beg = sep;
        str += convertToUString(a.first);
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Tree_i::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_treei && e->size() == 0 && tree.size() == 0) || e== this)];
}

bool Tree_i::egal(Element* e) {
    return ((e->type == t_treei && e->size() == 0 && tree.size() == 0) || e== this);
}

Element* Tree_n::minimum(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : tree) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Tree_n::maximum(LispE* lisp) {
    if (!tree.size())
        return null_;
    Element* e = NULL;
    
    for (const auto& a : tree) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Tree_n::minmax(LispE* lisp) {
    if (!tree.size())
        return null_;
    
    Element* v_min = NULL;
    Element* v_max = NULL;
    for (const auto& a : tree) {
        if (v_min == NULL) {
            v_min = a.second;
            v_max = a.second;
        }
        else {
            if (v_max->less(lisp, a.second)->Boolean())
                v_max = a.second;
            else
                if (v_min->more(lisp, a.second)->Boolean())
                    v_min = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

void Tree_n::flatten(LispE* lisp, List* l) {
    double k;
    for (const auto& a: tree) {
        k = a.first;
        l->append(lisp->provideNumber(k));
        a.second->flatten(lisp, l);
    }
}

void Tree_n::garbaging_values(LispE* lisp) {
    if (marking)
        return;
    marking = true;
    for (const auto& a : tree) {
        if (!a.second->is_protected()) {
            lisp->control_garbaging(a.second);
            a.second->garbaging_values(lisp);
        }
    }
    marking = false;
}

Element* Tree_n::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Number* element = lisp->provideNumber(0);
    lisp->recording(element, label);
    
    long sz = code->liste.size();
    //We record the keys first, in  case the tree is changed
    //in the following instructions
    Numbers* _keys = lisp->provideNumbers();
    for (const auto& a: tree)
        _keys->liste.push_back(a.first);
    try {
        for (long a_key = 0; a_key < _keys->liste.size(); a_key++) {
            e->release();
            e = lisp->get_variable(label);
            if (e != element) {
                if (e->type != t_number) {
                    e = lisp->provideNumber(_keys->liste[a_key]);
                    lisp->recording(e, label);
                }
                else
                    ((Number*)e)->content = _keys->liste[a_key];
                element = (Number*)e;
            }
            else
                element->content = _keys->liste[a_key];
            e = null_;
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
    catch (Error* err) {
        _keys->release();
        throw err;
    }
    _keys->release();
    return e;
    
}

Element* Tree_n::thekeys(LispE* lisp) {
    Numbers* dkeys = lisp->provideNumbers();
    for (const auto& a: tree) {
        dkeys->liste.push_back(a.first);
    }
    return dkeys;
}

Element* Tree_n::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (const auto& a: tree) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Tree_n::search_element(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

bool Tree_n::check_element(LispE* lisp, Element* valeur) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return true;
    }
    return false;
}

Element* Tree_n::checkkey(LispE* lisp, Element* e) {
    auto it = tree.find(e->asNumber());
    return (it == tree.end())?null_:it->second;
}

Element* Tree_n::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    if (remp->equal(lisp, valeur))
        return zero_;

    long nb = 0;
    Element* novel = remp->copying(false);
    for (auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean()) {
            a.second->decrement();
            a.second = novel;
            novel->increment();
            nb++;
        }
    }
    if (novel != remp)
        novel->release();
    return lisp->provideInteger(nb);
}

Element* Tree_n::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            l->liste.push_back(a.first);
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Tree_n::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    long nb = 0;
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            nb++;
    }
    return lisp->provideInteger(nb);
}

Element* Tree_n::search_reverse(LispE* lisp, Element* valeur, long ix) {
    for (const auto& a : tree) {
        if (a.second->equal(lisp, valeur)->Boolean())
            return lisp->provideNumber(a.first);
    }
    return null_;
}

Element* Tree_n::reverse(LispE* lisp, bool duplicate) {
    Tree_n* dico = lisp->provideTree_n();
    
    double k;
    Element* e;
    for (const auto& a: tree) {
        k = a.second->asNumber();
        e = dico->tree[k];
        if (e == NULL) {
            e = lisp->provideNumbers();
            dico->tree[k] = e;
            e->increment();
        }
        ((Numbers*)e)->liste.push_back(a.first);
    }
    return dico;
}

Element* Tree_n::value_on_index(double k, LispE* lisp) {
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree_n::protected_index(LispE* lisp, double k) {
    auto it = tree.find(k);
    return (it == tree.end())?null_:it->second;
}

Element* Tree_n::value_on_index(LispE* lisp, Element* ix) {
    double v = ix->checkNumber(lisp);
    auto it = tree.find(v);
    return (it == tree.end())?null_:it->second->copying(false);
}

Element* Tree_n::protected_index(LispE* lisp, Element* ix) {
    double v = ix->checkNumber(lisp);
    auto it = tree.find(v);
    if (it == tree.end())
        throw new Error("Error: index out of bounds");
    return it->second;
}

Element* Tree_n::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: tree) {
        str += beg;
        beg = sep;
        str += convertToUString(a.first);
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Tree_n::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_treen && e->size() == 0 && tree.size() == 0) || e== this)];
}

bool Tree_n::egal(Element* e) {
    return ((e->type == t_treen && e->size() == 0 && tree.size() == 0) || e== this);
}

Element* Tree::next_iter(LispE* lisp, void* it) {
    std::map<u_ustring, Element*>::iterator* n = (std::map<u_ustring, Element*>::iterator*)it;
    if (*n == tree.end())
        return emptyatom_;
    
    u_ustring u = (*n)->first;
    (*n)++;
    return lisp->provideString(u);
}

Element* Tree::next_iter_exchange(LispE* lisp, void* it) {
    std::map<u_ustring, Element*>::iterator* n = (std::map<u_ustring, Element*>::iterator*)it;
    if (*n == tree.end())
        return emptyatom_;
    
    u_ustring u = (*n)->first;
    (*n)++;
    return lisp->provideString(u);
}

Element* Tree_i::next_iter(LispE* lisp, void* it) {
    std::map<long, Element*>::iterator* n = (std::map<long, Element*>::iterator*)it;
    if (*n == tree.end())
        return emptyatom_;

    long u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

Element* Tree_i::next_iter_exchange(LispE* lisp, void* it) {
    std::map<long, Element*>::iterator* n = (std::map<long, Element*>::iterator*)it;
    if (*n == tree.end())
        return emptyatom_;

    long u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

Element* Tree_n::next_iter(LispE* lisp, void* it) {
    std::map<double, Element*>::iterator* n = (std::map<double, Element*>::iterator*)it;
    if (*n == tree.end())
        return emptyatom_;
    double u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

Element* Tree_n::next_iter_exchange(LispE* lisp, void* it) {
    std::map<double, Element*>::iterator* n = (std::map<double, Element*>::iterator*)it;
    if (*n == tree.end())
        return emptyatom_;
    double u = (*n)->first;
    (*n)++;
    return lisp->provideInteger(u);
}

