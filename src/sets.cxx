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

Element* Set_spool::copyatom(uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_s(this);
}

Element* Set_spool::newInstance() {
    return lisp->provideSet_s();
}

Element* Set_spool::fullcopy() {
    if (lisp->preparingthread)
        return new Set_s(ensemble);
    else
        return lisp->provideSet_s(this);
}

Element* Set_spool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->preparingthread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->preparingthread)
        return new Set_s(ensemble);
    else
        return lisp->provideSet_s(this);
}

void Set_spool::decrement() {
    if (is_protected())
        return;

    status--;
    if (!status) {
        ensemble.clear();
        lisp->sets_pool.push_back(this);
    }
}

void Set_spool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;

    status-=nb;
    if (!status) {
        ensemble.clear();
        lisp->sets_pool.push_back(this);
    }
}

void Set_spool::release() {
    if (!status) {
        ensemble.clear();
        lisp->sets_pool.push_back(this);
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

Element* Set_s::fullcopy() {
    return new Set_s(ensemble);
}

Element* Set_s::copying(bool duplicate) {
    if (exchange_value.provide && exchange_value.lisp->preparingthread)
        return new Set_s(ensemble);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return new Set_s(ensemble);
}

Element* Set_s::minimum(LispE* lisp) {
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

Element* Set_s::maximum(LispE* lisp) {
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

Element* Set_s::minmax(LispE* lisp) {
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

void Set_s::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    u_ustring k;
    for (auto& a : ensemble) {
        k = a;
        l->append(lisp->provideString(k));
    }
}

void Set_s::append(LispE* lisp, double v) {
    ensemble.insert(convertToUString(v));
}

void Set_s::append(LispE* lisp, long v) {
    ensemble.insert(convertToUString(v));
}

Element* Set_s::loop(LispE* lisp, short label, List* code) {
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

Element* Set_s::thekeys(LispE* lisp) {
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

Element* Set_s::thevalues(LispE* lisp) {
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

Element* Set_s::next_iter(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    u_ustring s = **n;
    Element* r = lisp->provideString(s);
    (*n)++;
    return r;
}

Element* Set_s::next_iter_exchange(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    exchange_value.content = **n;
    (*n)++;
    return &exchange_value;
}

Element* Set_s::search_element(LispE* lisp, Element* valeur, long ix) {
    u_ustring k = valeur->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

bool Set_s::check_element(LispE* lisp, Element* valeur) {
    return (ensemble.find(valeur->asUString(lisp)) != ensemble.end());
}

Element* Set_s::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asUString(lisp)) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_s::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) != ensemble.end()) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asUString(lisp));
        return one_;
    }
    return zero_;
}

Element* Set_s::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_s::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return zero_;
    return one_;
}

Element* Set_s::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set_s* s = lisp->provideSet_s();
    for (auto& a: ensemble) {
        exchange_value.content = a;
        if (value->check_element(lisp, &exchange_value))
            s->add(a);
    }
    return s;
}

Element* Set_s::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Set_s* s = lisp->provideSet_s();
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

Element* Set_s::list_xor(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '^^^' to strings, lists or sets");
    
    Set_s* s = lisp->provideSet_s();
    Set_s* intersection = (Set_s*)list_and(lisp, value);
    
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

Element* Set_s::search_reverse(LispE* lisp, Element* valeur, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_s::protected_index(LispE* lisp, long i) {
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

Element* Set_s::value_on_index(LispE* lisp, long i) {
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

Element* Set_s::value_from_index(LispE* lisp, long i) {
    for (auto& a: ensemble) {
        if (!i) {
            exchange_value.content = a;
            return lisp->provideString(exchange_value.content);
        }
        i--;
    }
    return null_;
}

Element* Set_s::value_on_index(wstring& w, LispE* lisp) {
    u_pstring k = _w_to_u(w);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::value_on_index(u_ustring& k, LispE* lisp) {
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::protected_index(LispE* lisp, u_ustring& k) {
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        throw new Error("Error: index out of bounds");
    
    return lisp->provideString(k);
}

Element* Set_s::join_in_list(LispE* lisp, u_ustring& sep) {
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

Element* Set_s::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_sets && ensemble == ((Set_s*)e)->ensemble)];
}

bool Set_s::egal(Element* e) {
    return (e->type == t_sets && ensemble == ((Set_s*)e)->ensemble);
}

Element* Set_s::plus(LispE* lisp, Element* e) {
    //Two cases either e is a string or it is a list...
    u_ustring d;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d += a;
        }
        return lisp->provideString(d);
    }
    Set_s* res = lisp->provideSet_s();
    if (e->type == t_sets) {
        auto nxt = ((Set_s*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_s*)e)->ensemble.end())
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

Element* Set_s::asList(LispE* lisp) {
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
    return (ensemble.find(valeur->asInteger()) != ensemble.end());
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
        throw new Error("Error: index out of bounds");
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
    return (ensemble.find(valeur->asNumber()) != ensemble.end());
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
        throw new Error("Error: index out of bounds");
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

Element* Set::fullcopy() {
    return new Set(dictionary, true);
}

Element* Set::copying(bool duplicate) {
    if (!is_protected() && !duplicate)
        return this;
    
    return new Set(dictionary);
}

Element* Set::minimum(LispE* lisp) {
    if (dictionary.empty())
        return null_;
    Element* e = null_;
    bool first = true;
    for (auto& a : dictionary) {
        if (first) {
            e = a.second;
            first = false;
        }
        else
            if (e->less(lisp, a.second)->Boolean())
                e = a.second;
    }
    
    return e->copying(false);
}

Element* Set::maximum(LispE* lisp) {
    if (dictionary.empty())
        return null_;
    Element* e = null_;
    bool first = true;
    for (auto& a : dictionary) {
        if (first) {
            e = a.second;
            first = false;
        }
        else
            if (e->more(lisp, a.second)->Boolean())
                e = a.second;
    }
    
    return e->copying(false);
}

Element* Set::minmax(LispE* lisp) {
    if (dictionary.empty())
        return null_;
    Element* em = null_;
    Element* eM = null_;
    bool first = true;
    for (auto& a : dictionary) {
        if (first) {
            em = a.second;
            eM = a.second;
            first = false;
        }
        else {
            if (em->less(lisp, a.second)->Boolean())
                em = a.second;
            else
                if (eM->more(lisp, a.second)->Boolean())
                    eM = a.second;
        }
    }
    
    List* l = lisp->provideList();
    l->append(em->copying(false));
    l->append(eM->copying(false));
    return l;
}

void Set::flatten(LispE* lisp, List* l) {
    if (dictionary.empty())
        return;
    for (auto& a : dictionary) {
        l->append(a.second);
    }
}

Element* Set::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    Element* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (auto& a: dictionary) {
        element = a.second;
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
    if (dictionary.empty())
        return keys;
    u_ustring k;
    for (auto& a : dictionary) {
        k = a.first;
        keys->append(lisp->provideString(k));
    }
    return keys;
}

Element* Set::thevalues(LispE* lisp) {
    List* keys = lisp->provideList();
    if (dictionary.empty())
        return keys;
    for (auto& a : dictionary) {
        keys->append(a.second->copying(false));
    }
    return keys;
}

Element* Set::next_iter(LispE* lisp, void* it) {
    std::unordered_map<u_ustring, Element*>::iterator* n = (std::unordered_map<u_ustring, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;
    
    u_ustring u = (*n)->first;
    Element* r = dictionary[u];
    (*n)++;
    return r->copying(false);
}

Element* Set::next_iter_exchange(LispE* lisp, void* it) {
    std::unordered_map<u_ustring, Element*>::iterator* n = (std::unordered_map<u_ustring, Element*>::iterator*)it;
    if (*n == dictionary.end())
        return emptyatom_;
    
    u_ustring u = (*n)->first;
    Element* r = dictionary[u];
    (*n)++;
    return r;
}

Element* Set::search_element(LispE* lisp, Element* valeur, long ix) {
    if (dictionary.find(valeur->asUString(lisp)) == dictionary.end())
        return null_;
    return true_;
}

bool Set::check_element(LispE* lisp, Element* valeur) {
    return (dictionary.find(valeur->asUString(lisp)) != dictionary.end());
}

Element* Set::checkkey(LispE* lisp, Element* e) {
    if (dictionary.find(e->asUString(lisp)) == dictionary.end())
        return null_;
    return true_;
}

Element* Set::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (dictionary.find(keyvalue) != dictionary.end()) {
        dictionary[keyvalue]->decrement();
        dictionary.erase(keyvalue);
        dictionary[keyvalue] = remp;
        remp->increment();
        return one_;
    }
    return zero_;
}

Element* Set::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    List* l = lisp->provideList();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (dictionary.find(keyvalue) == dictionary.end())
        return emptylist_;
    l->append(dictionary[keyvalue]->copying(false));
    return l;
}

Element* Set::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring keyvalue = valeur->asUString(lisp);
    if (dictionary.find(keyvalue) == dictionary.end())
        return zero_;
    return one_;
}

Element* Set::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set* s = lisp->provideSet();
    for (auto& a: dictionary) {
        if (value->check_element(lisp, a.second)) {
            s->dictionary[a.first] = a.second;
            a.second->increment();
        }
    }
    return s;
}

Element* Set::list_or(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '|||' to strings, lists or sets");
    
    Set* s = lisp->provideSet();
    s->dictionary = dictionary;
    
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            s->add(lisp, a->value);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long j = 0; j < sz; j++) {
                s->add(lisp, value->index(j));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    s->add(lisp, next_value);
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
    
    for (auto & a : dictionary) {
        if (intersection->dictionary.find(a.first) == intersection->dictionary.end())
            s->add(lisp, a.second);
    }
    
    u_ustring v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asUString(lisp);
            if (intersection->dictionary.find(v) == intersection->dictionary.end())
                s->add(lisp, a->value);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asUString(lisp);
                if (intersection->dictionary.find(v) == intersection->dictionary.end())
                    s->add(lisp, value->index(i));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asUString(lisp);
                    if (intersection->dictionary.find(v) == intersection->dictionary.end())
                        s->add(lisp, next_value);
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
    List* l = lisp->provideList();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (dictionary.find(keyvalue) == dictionary.end())
        return emptylist_;
    l->append(dictionary[keyvalue]->copying(false));
    return l;
}

Element* Set::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < dictionary.size()) {
        for (auto& a: dictionary) {
            if (!i) {
                return a.second;
            }
            i--;
        }
    }
    return null_;
}

Element* Set::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < dictionary.size()) {
        for (auto& a: dictionary) {
            if (!i) {
                return a.second;
            }
            i--;
        }
    }
    return null_;
}

Element* Set::value_from_index(LispE* lisp, long i) {
    for (auto& a: dictionary) {
        if (!i) {
            return a.second;
        }
        i--;
    }
    return null_;
}

Element* Set::value_on_index(wstring& w, LispE* lisp) {
    u_pstring k = _w_to_u(w);
    if (dictionary.find(k) == dictionary.end())
        return null_;
    return dictionary[k];
}

Element* Set::value_on_index(u_ustring& k, LispE* lisp) {
    if (dictionary.find(k) == dictionary.end())
        return null_;
    return dictionary[k];
}

Element* Set::protected_index(LispE* lisp, u_ustring& k) {
    if (dictionary.find(k) == dictionary.end())
        return null_;
    return dictionary[k];
}

Element* Set::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (dictionary.find(k) == dictionary.end())
        return null_;
    return dictionary[k];
}

Element* Set::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (dictionary.find(k) == dictionary.end())
        throw new Error("Error: index out of bounds");
    
    return dictionary[k];
}

Element* Set::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
    }
    return lisp->provideString(str);
}

Element* Set::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_sets && dictionary == ((Set*)e)->dictionary)];
}

bool Set::egal(Element* e) {
    return (e->type == t_sets && dictionary == ((Set*)e)->dictionary);
}

Element* Set::asList(LispE* lisp) {
    List* l = lisp->provideList();
    for (auto& a: dictionary) {
        l->append(a.second->copying(false));
    }
    return l;
}

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
        return new Set(dictionary, true);
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
        return new Set(dictionary, true);
    else
        return lisp->provideSet(this);
}

void Setpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->set_pool.push_back(this);
    }
}

void Setpool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status-=nb;
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->set_pool.push_back(this);
    }
}

void Setpool::release() {
    if (!status) {
        for (auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->set_pool.push_back(this);
    }
}
