/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  sets.cxx
//
//


#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>

Element* Setpool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet(this);
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

Element* Set_npool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_n(this);
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

void Set::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    u_ustring k;
    for (auto& a : ensemble) {
        k = a;
        l->append(lisp->provideString(k));
    }
}

void Set::append(LispE* lisp, double v) {
    ensemble.insert(convertToUString(v));
}

void Set::append(LispE* lisp, long v) {
    ensemble.insert(convertToUString(v));
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

Element* Set::next_iter(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    u_ustring s = **n;
    Element* r = lisp->provideString(s);
    (*n)++;
    return r;
}

Element* Set::next_iter_exchange(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.content = **n;
    (*n)++;
    return &exchange_value;
}

Element* Set::search_element(LispE* lisp, Element* valeur, long ix) {
    u_ustring k = valeur->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

bool Set::check_element(LispE* lisp, Element* valeur) {
    u_ustring k = valeur->asUString(lisp);
    return (ensemble.find(k) != ensemble.end());
}

Element* Set::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asUString(lisp)) == ensemble.end())
        return null_;
    return true_;
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

Element* Set::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return zero_;
    return one_;
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

Element* Set::search_reverse(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
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

Element* Set::protected_index(LispE* lisp, u_ustring& k) {
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
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

Element* Set::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_set && ensemble == ((Set*)e)->ensemble)];
}

bool Set::egal(Element* e) {
    return (e->type == t_set && ensemble == ((Set*)e)->ensemble);
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

Element* Set::asList(LispE* lisp) {
    List* l = lisp->provideList();
    u_ustring v;
    for (auto& a: ensemble) {
        v = a;
        l->append(lisp->provideString(v));
    }
    return l;
}

Element* Set_i::next_iter(LispE* lisp, void* it) {
    std::set<long>::iterator* n = (std::set<long>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    Element* r = lisp->provideInteger(**n);
    (*n)++;
    return r;
}

Element* Set_i::next_iter_exchange(LispE* lisp, void* it) {
    std::set<long>::iterator* n = (std::set<long>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.integer = **n;
    (*n)++;
    return &exchange_value;
}

bool Set_i::check_element(LispE* lisp, Element* valeur) {
    long k = valeur->asInteger();
    return (ensemble.find(k) != ensemble.end());
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

Element* Set_i::asList(LispE* lisp) {
    List* l = lisp->provideList();
    for (auto& a: ensemble) {
        l->append(lisp->provideInteger(a));
    }
    return l;
}

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

void Set_n::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    for (auto& a : ensemble) {
        l->append(lisp->provideNumber(a));
    }
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

Element* Set_n::thekeys(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideNumber(a));
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

Element* Set_n::next_iter(LispE* lisp, void* it) {
    std::set<double>::iterator* n = (std::set<double>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    Element* r = lisp->provideNumber(**n);
    (*n)++;
    return r;
}

Element* Set_n::next_iter_exchange(LispE* lisp, void* it) {
    std::set<double>::iterator* n = (std::set<double>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.number = **n;
    (*n)++;
    return &exchange_value;
}

Element* Set_n::search_element(LispE* lisp, Element* valeur, long ix) {
    double k = valeur->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

bool Set_n::check_element(LispE* lisp, Element* valeur) {
    double k = valeur->asNumber();
    return (ensemble.find(k) != ensemble.end());
}

Element* Set_n::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asNumber()) == ensemble.end())
        return null_;
    return true_;
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

Element* Set_n::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_n::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return zero_;
    return one_;
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

Element* Set_n::search_reverse(LispE* lisp, Element* valeur, long ix) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
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

Element* Set_n::value_from_index(LispE* lisp, long i) {
    for (auto& a: ensemble) {
        if (!i) {
            return lisp->provideNumber(a);
        }
        i--;
    }
    return null_;
}

Element* Set_n::value_on_index(LispE* lisp, Element* ix) {
    double k = ix->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideNumber(k);
}

Element* Set_n::protected_index(LispE* lisp, Element* ix) {
    double k = ix->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideNumber(k);
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

Element* Set_n::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_setn && ensemble == ((Set_n*)e)->ensemble)];
}

bool Set_n::egal(Element* e) {
    return (e->type == t_setn && ensemble == ((Set_n*)e)->ensemble);
}

Element* Set_n::asList(LispE* lisp) {
    List* l = lisp->provideList();
    for (auto& a: ensemble) {
        l->append(lisp->provideNumber(a));
    }
    return l;
}

