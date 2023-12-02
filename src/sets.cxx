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
#include "avl.h"
#include <math.h>
#include <algorithm>

Element* Set_s::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        return lisp->provideSet_s(this);
    }
    return this;
}

Element* Set_i::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        return lisp->provideSet_i(this);
    }
    return this;
}

Element* Set_n::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        return lisp->provideSet_n(this);
    }
    return this;
}

Element* Set::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        return lisp->provideSet(this);
    }
    return this;
}


Element* Set_spool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_s(this);
}

Element* Set_spool::newInstance() {
    return lisp->provideSet_s();
}

Element* Set_spool::fullcopy() {
    if (lisp->create_in_thread)
        return new Set_s(ensemble);
    else
        return lisp->provideSet_s(this);
}

Element* Set_spool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->create_in_thread)
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

Element* Set_ipool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_i(this);
}

Element* Set_ipool::newInstance() {
    return lisp->provideSet_i();
}

Element* Set_ipool::fullcopy() {
    if (lisp->create_in_thread)
        return new Set_i(ensemble);
    else
        return lisp->provideSet_i(this);
}

Element* Set_ipool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->create_in_thread)
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

Element* Set_npool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet_n(this);
}

Element* Set_npool::newInstance() {
    return lisp->provideSet_n();
}

Element* Set_npool::fullcopy() {
    if (lisp->create_in_thread)
        return new Set_n(ensemble);
    else
        return lisp->provideSet_n(this);
}

Element* Set_npool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->create_in_thread)
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
    if (exchange_value.provide && exchange_value.lisp->create_in_thread)
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
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
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

Element* Set_s::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    String* element = lisp->provideString();
    lisp->recording(element, label);
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        element->content = a;
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
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
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

Element* Set_s::search_element(LispE* lisp, Element* a_value, long ix) {
    return !ensemble.count(a_value->asUString(lisp))?null_:a_value;
}

bool Set_s::check_element(LispE* lisp, Element* a_value) {
    return ensemble.count(a_value->asUString(lisp));
}

Element* Set_s::checkkey(LispE* lisp, Element* e) {
    return !ensemble.count(e->asUString(lisp))?null_:true_;
}

Element* Set_s::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    u_ustring keyvalue = a_value->asUString(lisp);
    if (ensemble.count(keyvalue)) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asUString(lisp));
        return one_;
    }
    return zero_;
}

Element* Set_s::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = a_value->asUString(lisp);
    if (!ensemble.count(keyvalue))
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_s::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    u_ustring keyvalue = a_value->asUString(lisp);
    if (!ensemble.count(keyvalue))
        return zero_;
    return one_;
}

Element* Set_s::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set_s* s = lisp->provideSet_s();
    for (const auto& a: ensemble) {
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
        if (!intersection->ensemble.count(a))
            s->add(a);
    }
    
    u_ustring v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asUString(lisp);
            if (!intersection->ensemble.count(v))
                s->add(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asUString(lisp);
                if (!intersection->ensemble.count(v))
                    s->add(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asUString(lisp);
                    if (!intersection->ensemble.count(v))
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

Element* Set_s::search_reverse(LispE* lisp, Element* a_value, long ix) {
    return !ensemble.count(a_value->asUString(lisp))?null_:a_value;
}

Element* Set_s::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    for (const auto& a: ensemble) {
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
    if (!ensemble.count(k))
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::value_on_index(u_ustring& k, LispE* lisp) {
    if (!ensemble.count(k))
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::protected_index(LispE* lisp, u_ustring& k) {
    if (!ensemble.count(k))
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (!ensemble.count(k))
        return null_;
    return lisp->provideString(k);
}

Element* Set_s::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    if (!ensemble.count(k))
        throw new Error("Error: index out of bounds");
    return lisp->provideString(k);
}

Element* Set_s::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            d += a;
        }
        return lisp->provideString(d);
    }
    Set_s* res = lisp->provideSet_s();
    if (e->type == t_sets) {
        auto nxt = ((Set_s*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d = a + w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_s::asList(LispE* lisp, List* l) {
    u_ustring v;
    for (const auto& a: ensemble) {
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
    exchange_value.content = **n;
    (*n)++;
    return &exchange_value;
}

bool Set_i::check_element(LispE* lisp, Element* a_value) {
    return ensemble.count(a_value->asInteger());
}

Element* Set_i::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set_i* s = lisp->provideSet_i();
    for (const auto& a: ensemble) {
        exchange_value.content = a;
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
        if (!intersection->ensemble.count(a))
            s->add(a);
    }

    long v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asInteger();
            if (!intersection->ensemble.count(v))
                s->add(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asInteger();
                if (!intersection->ensemble.count(v))
                    s->add(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asInteger();
                    if (!intersection->ensemble.count(v))
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

Element* Set_i::asList(LispE* lisp, List* l) {
    for (const auto& a: ensemble) {
        l->append(lisp->provideInteger(a));
    }
    return l;
}

Element* Set_i::fullcopy() {
    return new Set_i(ensemble);
}

Element* Set_i::copying(bool duplicate) {
    if (exchange_value.provide && exchange_value.lisp->create_in_thread)
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
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
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
    
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
        l->append(lisp->provideInteger(a));
    }
}

Element* Set_i::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Integer* element = lisp->provideInteger(0);
    lisp->recording(element, label);
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        element->content = a;
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

Element* Set_i::thekeys(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (const auto& a : ensemble) {
        keys->append(lisp->provideInteger(a));
    }
    return keys;
}

Element* Set_i::thevalues(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (const auto& a : ensemble) {
        keys->append(lisp->provideInteger(a));
    }
    return keys;
}

Element* Set_i::search_element(LispE* lisp, Element* a_value, long ix) {
    return !ensemble.count(a_value->asInteger())?null_:a_value;
}

Element* Set_i::checkkey(LispE* lisp, Element* e) {
    return !ensemble.count(e->asInteger())?null_:true_;
}

Element* Set_i::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    long keyvalue = a_value->asInteger();
    if (ensemble.count(keyvalue)) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asInteger());
        return one_;
    }
    return zero_;
}

Element* Set_i::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Numbers* l = lisp->provideNumbers();
    long keyvalue = a_value->asInteger();
    if (!ensemble.count(keyvalue))
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_i::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    long keyvalue = a_value->asInteger();
    if (!ensemble.count(keyvalue))
        return zero_;
    return one_;
}

Element* Set_i::search_reverse(LispE* lisp, Element* a_value, long ix) {
    return !ensemble.count(a_value->asInteger())?null_:a_value;
}

Element* Set_i::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            if (!i) {
                return lisp->provideInteger(a);
            }
            i--;
        }
    }
    return null_;
}

Element* Set_i::value_from_index(LispE* lisp, long i) {
    for (const auto& a: ensemble) {
        if (!i) {
            return lisp->provideInteger(a);
        }
        i--;
    }
    return null_;
}

Element* Set_i::value_on_index(LispE* lisp, Element* ix) {
    long k = ix->asInteger();
    if (!ensemble.count(k))
        return null_;
    return lisp->provideInteger(k);
}

Element* Set_i::protected_index(LispE* lisp, Element* ix) {
    long k = ix->asInteger();
    if (!ensemble.count(k))
        throw new Error("Error: index out of bounds");
    return lisp->provideInteger(k);
}

Element* Set_i::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep == U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: ensemble) {
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
    if (exchange_value.provide && exchange_value.lisp->create_in_thread)
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
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
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
    
    for (const auto& a : ensemble) {
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
    for (const auto& a : ensemble) {
        l->append(lisp->provideNumber(a));
    }
}

Element* Set_n::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Number* element = lisp->provideNumber(0);
    lisp->recording(element, label);
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        _releasing(e);
        element->content = a;
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
    for (const auto& a : ensemble) {
        keys->append(lisp->provideNumber(a));
    }
    return keys;
}

Element* Set_n::thevalues(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (const auto& a : ensemble) {
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
    exchange_value.content = **n;
    (*n)++;
    return &exchange_value;
}

Element* Set_n::search_element(LispE* lisp, Element* a_value, long ix) {
    return !ensemble.count(a_value->asNumber())?null_:a_value;
}

bool Set_n::check_element(LispE* lisp, Element* a_value) {
    return ensemble.count(a_value->asNumber());
}

Element* Set_n::checkkey(LispE* lisp, Element* e) {
    return !ensemble.count(e->asNumber())?null_:true_;
}

Element* Set_n::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    double keyvalue = a_value->asNumber();
    if (ensemble.count(keyvalue)) {
        ensemble.erase(keyvalue);
        ensemble.insert(remp->asNumber());
        return one_;
    }
    return zero_;
}

Element* Set_n::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = a_value->asNumber();
    if (!ensemble.count(keyvalue))
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_n::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    double keyvalue = a_value->asNumber();
    if (!ensemble.count(keyvalue))
        return zero_;
    return one_;
}

Element* Set_n::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set_n* s = lisp->provideSet_n();
    for (const auto& a: ensemble) {
        exchange_value.content = a;
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
        if (!intersection->ensemble.count(a))
            s->add(a);
    }

    double v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asNumber();
            if (!intersection->ensemble.count(v))
                s->add(v);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asNumber();
                if (!intersection->ensemble.count(v))
                    s->add(v);
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asNumber();
                    if (!intersection->ensemble.count(v))
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

Element* Set_n::search_reverse(LispE* lisp, Element* a_value, long ix) {
    return !ensemble.count(a_value->asNumber())?null_:a_value;
}

Element* Set_n::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < ensemble.size()) {
        for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            if (!i) {
                return lisp->provideNumber(a);
            }
            i--;
        }
    }
    return null_;
}

Element* Set_n::value_from_index(LispE* lisp, long i) {
    for (const auto& a: ensemble) {
        if (!i) {
            return lisp->provideNumber(a);
        }
        i--;
    }
    return null_;
}

Element* Set_n::value_on_index(LispE* lisp, Element* ix) {
    double k = ix->asNumber();
    if (!ensemble.count(k))
        return null_;
    return lisp->provideNumber(k);
}

Element* Set_n::protected_index(LispE* lisp, Element* ix) {
    double k = ix->asNumber();
    if (!ensemble.count(k))
        throw new Error("Error: index out of bounds");
    return lisp->provideNumber(k);
}

Element* Set_n::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep == U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: ensemble) {
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

Element* Set_n::asList(LispE* lisp, List* l) {
    for (const auto& a: ensemble) {
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
    for (const auto& a : dictionary) {
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
    for (const auto& a : dictionary) {
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
    for (const auto& a : dictionary) {
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
    for (const auto& a : dictionary) {
        l->append(a.second);
    }
}

Element* Set::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    Element* element;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (const auto& a: dictionary) {
        element = a.second;
        lisp->replacestackvalue(element, label);
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
    for (const auto& a : dictionary) {
        k = a.first;
        keys->append(lisp->provideString(k));
    }
    return keys;
}

Element* Set::thevalues(LispE* lisp) {
    List* keys = lisp->provideList();
    if (dictionary.empty())
        return keys;
    for (const auto& a : dictionary) {
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

Element* Set::search_element(LispE* lisp, Element* a_value, long ix) {
    u_ustring keyvalue = a_value->asUString(lisp);
    auto it = dictionary.find(keyvalue);
    return (it == dictionary.end())?null_:it->second;
}

bool Set::check_element(LispE* lisp, Element* a_value) {
    return (dictionary.count(a_value->asUString(lisp)));
}

Element* Set::checkkey(LispE* lisp, Element* e) {
    auto it = dictionary.find(e->asUString(lisp));
    return (it == dictionary.end())?null_:it->second;
}

Element* Set::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    u_ustring keyvalue = a_value->asUString(lisp);
    auto it = dictionary.find(keyvalue);
    if (it != dictionary.end()) {
        it->second->decrement();
        dictionary[keyvalue] = remp;
        remp->increment();
        return one_;
    }
    return zero_;
}

Element* Set::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    List* l = lisp->provideList();
    u_ustring keyvalue = a_value->asUString(lisp);
    auto it = dictionary.find(keyvalue);
    if (it == dictionary.end())
        return emptylist_;
    l->append(it->second->copying(false));
    return l;
}

Element* Set::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    u_ustring keyvalue = a_value->asUString(lisp);
    return (dictionary.count(keyvalue))?one_:zero_;
}

Element* Set::list_and(LispE* lisp, Element* value) {
    if (!value->isList() && !value->isSet())
        throw new Error("Error: Can only apply '&&&' to strings, lists or sets");
    
    Set* s = lisp->provideSet();
    for (const auto& a: dictionary) {
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
        if (!intersection->dictionary.count(a.first))
            s->add(lisp, a.second);
    }
    
    u_ustring v;
    if (value->type == t_llist) {
        u_link* a = ((LList*)value)->liste.begin();
        for (; a != NULL; a = a->next()) {
            v = a->value->asUString(lisp);
            if (!intersection->dictionary.count(v))
                s->add(lisp, a->value);
        }
    }
    else {
        if (value->isList()) {
            long sz = value->size();
            for (long i = 0; i < sz; i++) {
                v = value->index(i)->asUString(lisp);
                if (!intersection->dictionary.count(v))
                    s->add(lisp, value->index(i));
            }
        }
        else {
            if (value->isSet()) {
                void* iter = value->begin_iter();
                Element* next_value = value->next_iter_exchange(lisp, iter);
                while (next_value != emptyatom_) {
                    v = next_value->asUString(lisp);
                    if (!intersection->dictionary.count(v))
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

Element* Set::search_reverse(LispE* lisp, Element* a_value, long ix) {
    u_ustring keyvalue = a_value->asUString(lisp);
    auto it = dictionary.find(keyvalue);
    return (it == dictionary.end())?null_:it->second;
}

Element* Set::protected_index(LispE* lisp, long i) {
    if (i >= 0 && i < dictionary.size()) {
        for (const auto& a: dictionary) {
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
        for (const auto& a: dictionary) {
            if (!i) {
                return a.second;
            }
            i--;
        }
    }
    return null_;
}

Element* Set::value_from_index(LispE* lisp, long i) {
    for (const auto& a: dictionary) {
        if (!i) {
            return a.second;
        }
        i--;
    }
    return null_;
}

Element* Set::value_on_index(wstring& w, LispE* lisp) {
    u_pstring k = _w_to_u(w);
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Set::value_on_index(u_ustring& k, LispE* lisp) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Set::protected_index(LispE* lisp, u_ustring& k) {
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Set::value_on_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    auto it = dictionary.find(k);
    return (it == dictionary.end())?null_:it->second;
}

Element* Set::protected_index(LispE* lisp, Element* ix) {
    u_ustring k = ix->asUString(lisp);
    auto it = dictionary.find(k);
    if (it == dictionary.end())
        throw new Error("Error: index out of bounds");
    
    return it->second;
}

Element* Set::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (const auto& a: dictionary) {
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

Element* Set::asList(LispE* lisp, List* l) {
    for (const auto& a: dictionary) {
        l->append(a.second->copying(false));
    }
    return l;
}

Element* Setpool::copyatom(LispE* lsp, uint16_t s) {
    if (status < s)
        return this;
    
    return lisp->provideSet(this);
}

Element* Setpool::newInstance() {
    return lisp->provideSet();
}

Element* Setpool::fullcopy() {
    if (lisp->create_in_thread)
        return new Set(dictionary, true);
    else
        return lisp->provideSet(this);
}

Element* Setpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (!lisp->create_in_thread && !is_protected() && !duplicate)
        return this;
    
    if (lisp->create_in_thread)
        return new Set(dictionary, true);
    else
        return lisp->provideSet(this);
}

void Setpool::decrement() {
    if (is_protected())
        return;
    
    status--;
    if (!status) {
        for (const auto& a : dictionary)
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
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->set_pool.push_back(this);
    }
}

void Setpool::release() {
    if (!status) {
        for (const auto& a : dictionary)
            a.second->decrement();
        dictionary.clear();
        lisp->set_pool.push_back(this);
    }
}

void Set::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        append(value->copying(false));
        value->release();
    }
}

void Set::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            append(value->copying(false));
            value->release();
        }
    }
}

void Set::push_element_front(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        append(value->copying(false));
        value->release();
    }
}

void Set::push_element_back(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        append(value->copying(false));
        value->release();
    }
}

void Set_s::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asUString(lisp));
        value->release();
    }
}

void Set_s::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            ensemble.insert(value->asUString(lisp));
            value->release();
        }
    }
}

void Set_s::push_element_front(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asUString(lisp));
        value->release();
    }
}

void Set_s::push_element_back(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asUString(lisp));
        value->release();
    }
}

void Set_i::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asInteger());
        value->release();
    }
}

void Set_i::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            ensemble.insert(value->asInteger());
            value->release();
        }
    }
}

void Set_i::push_element_front(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asInteger());
        value->release();
    }
}

void Set_i::push_element_back(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asInteger());
        value->release();
    }
}

void Set_n::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asNumber());
        value->release();
    }
}

void Set_n::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            ensemble.insert(value->asNumber());
            value->release();
        }
    }
}

void Set_n::push_element_front(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asNumber());
        value->release();
    }
}

void Set_n::push_element_back(LispE* lisp, List* l)  {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        ensemble.insert(value->asNumber());
        value->release();
    }
}


Avl::Avl(Element* e) {
    value = e;
    value->increment();
    same=NULL;
    left=NULL;
    right=NULL;
    height=0;
}

void Avl::tree_shape(Avl** courant,long& type) {
    long hg,hd;
    Avl* element;

    element=*courant;
    if (type<11 && element !=NULL) {
        if (element->left !=NULL)
            hg=(element->left)->height;
        else
            hg=-1;
        if (element->right !=NULL)
            hd=(element->right)->height;
        else
            hd=-1;
      if (hg>hd) {
          type=type*10+1;
          tree_shape(&element->left,type);
      }
      else
          if (hg<hd || (hg==hd && hd!=-1)) {
              type=type*10+2;
              tree_shape(&element->right,type);
          }
    }
}

void Avl::check_height(Avl** courant) {
    long hd,hg,type;
    Avl* element;

    element=*courant;
    if (element->left !=NULL)
        hg=1+(element->left)->height;
    else
        hg=0;
    if (element->right !=NULL)
        hd=1+(element->right)->height;
    else
        hd=0;
    if (hg==hd || hg==hd+1)
        element->height=hg;
    else
        if (hd==hg+1)
            element->height=hd;
        else {
            type=0;
            if (hg>hd)
                element->height=hg;
        else
                element->height=hd;
            tree_shape(courant,type);
            rebuilding(courant,type);
        }
}


void Avl::rebuilding(Avl** courant,long type) {
    Avl* element;
    Avl* e;
    Avl* eg;
    Avl* ed;

    element=*courant;
    if (type>10) {
        switch(type) {
        case 11:
            e=(element->left)->right;
            (element->left)->right=element;
            *courant=element->left;
            ((*courant)->right)->left=e;
            check_height(&(*courant)->right);
            check_height(courant);
            break;
        case 12:
            eg=((element->left)->right)->left;
            ed=((element->left)->right)->right;
            ((element->left)->right)->left=element->left;
            ((element->left)->right)->right=element;
            *courant=(element->left)->right;
            ((*courant)->left)->right=eg;
            ((*courant)->right)->left=ed;
            check_height(&(*courant)->left);
            check_height(&(*courant)->right);
            check_height(courant);
            break;
        case 21:
            eg=((element->right)->left)->left;
            ed=((element->right)->left)->right;
            ((element->right)->left)->right=element->right;
            ((element->right)->left)->left=element;
            *courant=(element->right)->left;
            ((*courant)->left)->right=eg;
            ((*courant)->right)->left=ed;
            check_height(&(*courant)->left);
            check_height(&(*courant)->right);
            check_height(courant);
            break;
        case 22:
            e=(element->right)->left;
            (element->right)->left=element;
            *courant=element->right;
            ((*courant)->left)->right=e;
            check_height(&(*courant)->left);
            check_height(courant);
        }
    }
}

void Avl::insertion(LispE* lisp, Avl** courant,Element* ajout, List* compare) {
    long test;
    if (*courant==NULL)
        *courant= new Avl(ajout);
    else {
        compare->in_quote(2, (*courant)->value);
        test = compare->eval(lisp)->asInteger();
        if (!test) {
            Avl* dernier=*courant;
            //On va chercher le dernier element de la liste
            while (dernier->same!=NULL)
                dernier=dernier->same;
            dernier->same= new Avl(ajout);
        }
        else {
            if (test < 0)
                insertion(lisp, &(*courant)->left,ajout,compare);
            else
                insertion(lisp, &(*courant)->right,ajout,compare);
            check_height(courant);
        }
    }
}

Element* Avl::search(LispE* lisp, Element* element, List* compare) {
    Avl* e = this;
    int16_t test;
    while (e != NULL) {
        compare->in_quote(2, e->value);
        test = compare->eval(lisp)->asShort();
        if (!test) {
            while (e != NULL) {
                if (e->value->isequal(lisp, element))
                    return e->value;
                e = e->same;
            }
            return NULL;
        }
        
        if (test < 0)
            e = e->left;
        else
            e = e->right;
    }
    return NULL;
}

bool Avl::check(LispE* lisp, Element* element, List* compare) {
    Avl* e = this;
    int16_t test;
    while (e != NULL) {
        compare->in_quote(2, e->value);
        test = compare->eval(lisp)->asShort();
        if (!test) {
            while (e != NULL) {
                if (e->value->isequal(lisp, element))
                    return true;
                e = e->same;
            }
            return false;
        }
        
        if (test < 0)
            e = e->left;
        else
            e = e->right;
    }
    return false;
}

bool Avl::erase(LispE* lisp, Avl** root, Element* current, List* compare) {
    Avl* parent = this;
    Avl* e=this;
    int16_t test;
    char origin = 0;
    while (e != NULL) {
        compare->in_quote(2, e->value);
        test = compare->eval(lisp)->asShort();
        if (!test) {
            Avl* clean = e;
            Avl* moving = e;
            Avl* prec = NULL;
            while (moving != NULL) {
                if (current->isequal(lisp, moving->value)) {
                    break;
                }
                prec = moving;
                moving = moving->same;
            }
            if (moving == NULL)
                return false;
            bool change = false;
            if (e->same == NULL) {
                change = true;
                if (e->left != NULL)
                    moving = e->left;
                else {
                    if (e->right != NULL)
                        moving = e->right;
                    else
                        moving = NULL;
                }
            }
            else {
                if (prec == NULL) {
                    clean = e;
                    moving = e->same;
                    moving->left = e->left;
                    moving->right = e->right;
                }
                else {
                    prec->same = moving->same;
                    clean = moving;
                    moving = e;
                }
            }
            
            //we replace the current element with moving...
            if (!origin) {
                //This is the root of our tree
                *root = moving;
            }
            else {
                if (origin == -1)
                    //It was the left node
                    parent->left = moving;
                else
                    parent->right = moving;
            }

            if (change)
                check_height(root);
            
            clean->value->decrement();
            delete clean;
            return true;
        }
        parent= e;
        if (test < 0) {
            origin = -1;
            e=e->left;
        }
        else {
            origin = 1;
            e=e->right;
        }
    }
    return false;

}

void Avl::insertion(LispE* lisp, Avl** courant, Element* ajout, List* compare, short label) {
    int16_t test;
    if (*courant==NULL)
        *courant= new Avl(ajout);
    else {
        lisp->replacestackvalue((*courant)->value, label);
        test = compare->eval_lambda_min(lisp)->asShort();
        if (!test) {
            Avl* dernier=*courant;
            //On va chercher le dernier element de la liste
            while (dernier->same!=NULL)
                dernier=dernier->same;
            dernier->same= new Avl(ajout);
        }
        else {
            if (test < 0)
                insertion(lisp, &(*courant)->left,ajout,compare, label);
            else
                insertion(lisp, &(*courant)->right,ajout,compare, label);
            check_height(courant);
        }
    }
}

Element* Avl::search(LispE* lisp, Element* element, List* compare, short label) {
    Avl* e = this;
    int16_t test;
    while (e != NULL) {
        lisp->replacestackvalue(e->value, label);
        test = compare->eval_lambda_min(lisp)->asShort();
        if (!test) {
            while (e != NULL) {
                if (e->value->isequal(lisp, element))
                    return e->value;
                e = e->same;
            }
            return NULL;
        }
        
        if (test < 0)
            e = e->left;
        else
            e = e->right;
    }
    return NULL;
}

bool Avl::check(LispE* lisp, Element* element, List* compare, short label) {
    Avl* e = this;
    int16_t test;
    while (e != NULL) {
        lisp->replacestackvalue(e->value, label);
        test = compare->eval_lambda_min(lisp)->asShort();
        if (!test) {
            while (e != NULL) {
                if (e->value->isequal(lisp, element))
                    return true;
                e = e->same;
            }
            return false;
        }
        
        if (test < 0)
            e = e->left;
        else
            e = e->right;
    }
    return false;
}

bool Avl::erase(LispE* lisp, Avl** root, Element* current, List* compare, short label) {
    Avl* parent = this;
    Avl* e=this;
    int16_t test;
    char origin = 0;
    while (e != NULL) {
        lisp->replacestackvalue(e->value, label);
        test = compare->eval_lambda_min(lisp)->asShort();
        if (!test) {
            Avl* clean = e;
            Avl* moving = e;
            Avl* prec = NULL;
            while (moving != NULL) {
                if (current->isequal(lisp, moving->value)) {
                    break;
                }
                prec = moving;
                moving = moving->same;
            }
            if (moving == NULL)
                return false;
            bool change = false;
            if (e->same == NULL) {
                change = true;
                if (e->left != NULL)
                    moving = e->left;
                else {
                    if (e->right != NULL)
                        moving = e->right;
                    else
                        moving = NULL;
                }
            }
            else {
                if (prec == NULL) {
                    clean = e;
                    moving = e->same;
                    moving->left = e->left;
                    moving->right = e->right;
                }
                else {
                    prec->same = moving->same;
                    clean = moving;
                    moving = e;
                }
            }
            
            //we replace the current element with moving...
            if (!origin) {
                //This is the root of our tree
                *root = moving;
            }
            else {
                if (origin == -1)
                    //It was the left node
                    parent->left = moving;
                else
                    parent->right = moving;
            }

            if (change)
                check_height(root);
            
            clean->value->decrement();
            delete clean;
            return true;
        }
        parent= e;
        if (test < 0) {
            origin = -1;
            e=e->left;
        }
        else {
            origin = 1;
            e=e->right;
        }
    }
    return false;

}


void Avl::pop_front(Avl** root) {
    Avl* e = this;
    if (e->left == NULL) {
        if (same == NULL)
            *root = right;
        else {
            *root = same;
            same->right = right;
        }

        if (*root != NULL)
            check_height(root);
        e->value->decrement();
        delete e;
        return;
    }
    
    while (e->left->left != NULL)
        e = e->left;
    Avl* a  = e->left;
    
    if (a->same != NULL) {
        e->left = a->same;
        a->same->right = a->right;
    }
    else {
        e->left = a->right;
        check_height(root);
    }
    
    a->value->decrement();
    delete a;
}

void Avl::pop_last(Avl** root) {
    Avl* e = this;
    if (e->right == NULL) {
        if (same == NULL)
            *root = left;
        else {
            *root = same;
            same->left = left;
        }
        if (*root != NULL)
            check_height(root);
        e->value->decrement();
        delete e;
        return;
    }
    
    while (e->right->right != NULL)
        e = e->right;
    
    Avl* a  = e->right;
    if (a->same != NULL) {
        Avl* prec = NULL;
        while (a->same != NULL) {
            prec = a;
            a = a->same;
        }
        prec->same = NULL;
    }
    else {
        e->right = a->left;
        check_height(root);
    }

    a->value->decrement();
    delete a;
}

bool Avl::equal(LispE* lisp, Avl* avl, bool record) {
    if (avl == NULL)
        return false;
    Avl* e = this;
    Avl* a = avl;
    
    while (e != NULL && a!= NULL) {
        if (e->value->unify(lisp, a->value, record) == false)
            return false;
        e = e->same;
        a = a->same;
    }
    if (e != a)
        return false;
    if (left != NULL) {
        if (!left->equal(lisp, avl->left, record))
            return false;
    }
    else {
        if (avl->left != NULL)
            return false;
    }
    if (right != NULL)
        return right->equal(lisp, avl->right, record);
    if (avl->right == NULL)
        return true;
    return false;
}

bool Avl::isequal(LispE* lisp, Avl* avl) {
    if (avl == NULL)
        return false;
    Avl* e = this;
    Avl* a = avl;
    
    while (e != NULL && a!= NULL) {
        if (e->value->isequal(lisp, a->value) == false)
            return false;
        e = e->same;
        a = a->same;
    }
    if (e != a)
        return false;
    if (left != NULL) {
        if (!left->isequal(lisp, avl->left))
            return false;
    }
    else {
        if (avl->left != NULL)
            return false;
    }
    if (right != NULL)
        return right->isequal(lisp, avl->right);
    if (avl->right == NULL)
        return true;
    return false;
}

Element* Avl::element(LispE* lisp) {
    if (same == NULL)
        return value;
    
    List* l = lisp->provideList();
    Avl* e = this;
    while (e != NULL) {
        l->append(e->value);
        e = e->same;
    }
    return l;
}

Element* Avl::element() {
    if (same == NULL)
        return value;
    List* l = new List();
    Avl* e = this;
    while (e != NULL) {
        l->append(e->value);
        e = e->same;
    }
    return l;
}

void Avl::flatten(List* l) {
    if (left != NULL)
        left->flatten(l);
    element(l);
    if (right != NULL)
        right->flatten(l);

}

void Avl::flatten(List& l) {
    if (left != NULL)
        left->flatten(l);
    element(l);
    if (right != NULL)
        right->flatten(l);
}

void Avl::to_list(LispE* lisp, List* l) {
    if (left != NULL)
        left->to_list(lisp, l);
    element(l);
    if (right != NULL)
        right->to_list(lisp, l);
}

void Avl::to_llist(LispE* lisp, LList* l) {
    if (right != NULL)
        right->to_llist(lisp, l);
    element(l);
    if (left != NULL)
        left->to_llist(lisp, l);
}

void Avl::jsonString(LispE* lisp, wstring& w) {
    if (left != NULL) {
        left->jsonString(lisp, w);
        w += ',';
    }
    Avl* e = this;
    while (e != NULL) {
        if (e != this)
            w += ',';
        w += e->value->jsonString(lisp);
        e = e->same;
    }
    if (right != NULL) {
        w += ',';
        right->jsonString(lisp, w);
    }
}

void Avl::asString(LispE* lisp, wstring& w) {
    if (left != NULL) {
        left->asString(lisp, w);
        w += ' ';
    }
    Avl* e = this;
    while (e != NULL) {
        if (e != this)
            w += ' ';
        w += e->value->asString(lisp);
        e = e->same;
    }
    if (right != NULL) {
        w += ' ';
        right->asString(lisp, w);
    }
}

void Avl::asUString(LispE* lisp, u_ustring& w) {
    if (left != NULL) {
        left->asUString(lisp, w);
        w += ' ';
    }
    Avl* e = this;
    while (e != NULL) {
        if (e != this)
            w += ' ';
        w += e->value->asUString(lisp);
        e = e->same;
    }
    if (right != NULL) {
        w += ' ';
        right->asUString(lisp, w);
    }
}


Element* Avl::front(LispE* lisp) {
    Avl* e = this;
    while (e->left != NULL)
        e = e->left;
    return e->value;
}

Element* Avl::back(LispE* lisp) {
    Avl* e = this;
    while (e->right != NULL)
        e = e->right;
    if (lisp == NULL)
        return e->element();
    while (e->same != NULL)
        e = e->same;
    return e->value;
}

bool Heap::check_element(LispE* lisp, Element* element_value) {
    if (root == NULL)
        return false;
    compare->in_quote(1, element_value);
    bool res = root->check(lisp, element_value, compare);
    compare->in_quote(1, null_);
    compare->in_quote(2, null_);
    return res;
}

Element* Heap::search_element(LispE* lisp, Element* element_value, long idx) {
    if (root == NULL)
        return null_;
    compare->in_quote(1, element_value);
    Element* res = root->search(lisp, element_value, compare);
    compare->in_quote(1, null_);
    compare->in_quote(2, null_);
    return (res == NULL)?null_:res;
}

Element* Heap::insert(LispE* lisp, Element* element, long idx) {
    if (root == NULL) {
        root = new Avl(element);
        return this;
    }
    
    compare->in_quote(1, element);
    root->insertion(lisp, &root, element, compare);
    compare->in_quote(1, null_);
    compare->in_quote(2, null_);
    return this;
}

Element* Heap::insert(LispE* lisp, Element* element) {
    if (root == NULL) {
        root = new Avl(element);
        return this;
    }
    
    compare->in_quote(1, element);
    root->insertion(lisp, &root, element, compare);
    compare->in_quote(1, null_);
    compare->in_quote(2, null_);
    return this;
}

bool Heap::remove(LispE* lisp, Element* element) {
    if (root == NULL)
        return false;
    
    compare->in_quote(1, element);
    bool del = root->erase(lisp, &root, element, compare);
    compare->in_quote(1, null_);
    compare->in_quote(2, null_);
    return del;
}

void Heap::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        insert(lisp, value->copying(false));
        value->release();
    }
}

void Heap::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            insert(lisp, value->copying(false));
            value->release();
        }
    }
}

void Heap::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        insert(lisp, value->copying(false));
        value->release();
    }
}

void Heap::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        insert(lisp, value->copying(false));
        value->release();
    }
}

Element* Heap::car(LispE* lisp) {
    if (root == NULL)
        return null_;
    return root->front(lisp);
}

Element* Heap::index(long i) {
    if (root == NULL)
        return NULL;
    if (i == -1)
        return root->back(NULL);
    return root->traverse(i);
}

Element* Heap::protected_index(LispE* lisp, long i) {
    if (root == NULL)
        return null_;
    
    if (i == -1)
        return root->back(lisp);
    
    Element* e = root->traverse(lisp, i);
    if (e == NULL)
        return null_;
    return e;
}

Element* Heap::value_from_index(LispE* lisp, long i) {
    if (i == -1)
        return root->back(lisp);
    return root->traverse(lisp, i);
}

Element* Heap::value_on_index(LispE* lisp, long i) {
    if (root == NULL)
        return null_;
    if (i == -1)
        return root->back(lisp);
    Element* e = root->traverse(lisp, i);
    if (e == NULL)
        return null_;
    return e;
}

Element* Heap::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->asInteger();
    if (root == NULL)
        return null_;
    if (i == -1)
        return root->back(lisp);
    Element* e = root->traverse(lisp, i);
    if (e == NULL)
        return null_;
    return e;
}

Element* Heap::protected_index(LispE* lisp, Element* k) {
    long i = k->asInteger();
    if (root == NULL)
        throw new Error("Error: index out of bounds");
    if (i == -1)
        return root->back(lisp);
    Element* e = root->traverse(lisp, i);
    if (e == NULL)
        throw new Error("Error: index out of bounds");
    return e;
}

Element* Heap::asList(LispE* lisp, List* l) {
    if (root == NULL)
        return l;
    root->to_list(lisp, l);
    return l;
}

Element* Heap::asLList(LispE* lisp) {
    LList* l = new LList(&lisp->delegation->mark);
    if (root == NULL)
        return l;
    root->to_llist(lisp, l);
    return l;
}

wstring Heap::jsonString(LispE* lisp) {
    wstring r;
    if (root == NULL) {
        r = L"[]";
    }
    else {
        r = '[';
        root->jsonString(lisp, r);
        r += ']';
    }
    return r;
}

wstring Heap::asString(LispE* lisp) {
    wstring r;
    if (root == NULL) {
        r = L"()";
    }
    else {
        r = '(';
        root->asString(lisp, r);
        r = ')';
    }
    return r;
}

u_ustring Heap::asUString(LispE* lisp) {
    u_ustring r;
    if (root == NULL) {
        r = U"()";
    }
    else {
        r = '(';
        root->asUString(lisp, r);
        r += ')';
    }
    return r;
}

Element* Heap::next_iter(LispE* lisp, void* it) {
    Element* e = ((Iter_heap*)it)->next();
    if (e == NULL)
        return emptyatom_;
    return e->copying(false);
}

Element* Heap::next_iter_exchange(LispE* lisp, void* it) {
    Element* e = ((Iter_heap*)it)->next();
    if (e == NULL)
        return emptyatom_;
    return e;
}


bool Heap::egal(Element* e) {
    return (e->type == t_heap && root == ((Heap*)e)->root);
}

Element* Heap::loop(LispE* lisp, int16_t label, List* code) {
    if (root == NULL)
        return null_;
    
    long i_loop;
    Iter_heap iter(root);
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* element = iter.next();
    while (element != NULL) {
        lisp->replacestackvalue(element, label);
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
        element = iter.next();
    }
    return e;
}


bool Heap::unify(LispE* lisp, Element* value, bool record) {
    if (value->type != t_heap)
        return false;

    if (root == NULL)
        return (value->isEmpty());

    bool rec = true;

    if (!record) {
        if (value == this) {
            return true;
        }
        rec = (lisp->extractdynamiclabel(car(lisp)) == v_null);
    }
    
    Iter_heap irule(root);
    Iter_heap ivalue(((Heap*)value)->root);
    
    //this contains a data structure definition
    //This method is used to check if value matches the data structure in 'this'
    //rec==false, if the first element is a data structure name...
    bool test = true;
    Element* r = irule.next();
    Element* v = ivalue.next();
    while (r != NULL && v != NULL) {
        test = (r == null_ || r->unify(lisp, v, rec));
        rec = record;
        r = irule.next();
        v = ivalue.next();
    }
    return (test && r == NULL && v == NULL);
}


bool Heaplambda::check_element(LispE* lisp, Element* element_value) {
    if (root == NULL)
        return false;
    
    short lab1 = compare->index(1)->index(0)->label();
    short lab2 = compare->index(1)->index(1)->label();
    
    Element* e1 = lisp->record_or_replace(element_value, lab1);
    Element* e2 = lisp->record_or_replace(null_, lab2);

    bool res = false;
    try {
        res = root->check(lisp, element_value, compare, lab2);
    }
    catch (Error* err) {
        lisp->reset_in_stack(e2, lab2);
        lisp->reset_in_stack(e1, lab1);
        throw err;
    }

    lisp->reset_in_stack(e2, lab2);
    lisp->reset_in_stack(e1, lab1);
    
    return res;
}

Element* Heaplambda::search_element(LispE* lisp, Element* element_value, long idx) {
    if (root == NULL)
        return null_;

    short lab1 = compare->index(1)->index(0)->label();
    short lab2 = compare->index(1)->index(1)->label();
    
    Element* e1 = lisp->record_or_replace(element_value, lab1);
    Element* e2 = lisp->record_or_replace(null_, lab2);

    Element* res = NULL;
    try {
        res = root->search(lisp, element_value, compare, lab2);
    }
    catch (Error* err) {
        lisp->reset_in_stack(e2, lab2);
        lisp->reset_in_stack(e1, lab1);
        throw err;
    }
    
    lisp->reset_in_stack(e2, lab2);
    lisp->reset_in_stack(e1, lab1);

    return (res == NULL)?null_:res;
}

Element* Heaplambda::insert(LispE* lisp, Element* element, long idx) {
    if (root == NULL) {
        root = new Avl(element);
        return this;
    }
    
    short lab1 = compare->index(1)->index(0)->label();
    short lab2 = compare->index(1)->index(1)->label();
    
    Element* e1 = lisp->record_or_replace(element, lab1);
    Element* e2 = lisp->record_or_replace(null_, lab2);
    
    try {
        root->insertion(lisp, &root, element, compare, lab2);
    }
    catch (Error* err) {
        lisp->reset_in_stack(e2, lab2);
        lisp->reset_in_stack(e1, lab1);
        throw err;
    }

    lisp->reset_in_stack(e2, lab2);
    lisp->reset_in_stack(e1, lab1);
    return this;
}

Element* Heaplambda::insert(LispE* lisp, Element* element) {
    if (root == NULL) {
        root = new Avl(element);
        return this;
    }
    
    short lab1 = compare->index(1)->index(0)->label();
    short lab2 = compare->index(1)->index(1)->label();
    
    Element* e1 = lisp->record_or_replace(element, lab1);
    Element* e2 = lisp->record_or_replace(null_, lab2);

    try {
        root->insertion(lisp, &root, element, compare, lab2);
    }
    catch (Error* err) {
        lisp->reset_in_stack(e2, lab2);
        lisp->reset_in_stack(e1, lab1);
        throw err;
    }

    lisp->reset_in_stack(e2, lab2);
    lisp->reset_in_stack(e1, lab1);
    return this;
}

bool Heaplambda::remove(LispE* lisp, Element* element) {
    if (root == NULL)
        return false;
    
    short lab1 = compare->index(1)->index(0)->label();
    short lab2 = compare->index(1)->index(1)->label();
    
    Element* e1 = lisp->record_or_replace(element, lab1);
    Element* e2 = lisp->record_or_replace(null_, lab2);

    bool del = false;

    try {
        del = root->erase(lisp, &root, element, compare, lab2);
    }
    catch (Error* err) {
        lisp->reset_in_stack(e2, lab2);
        lisp->reset_in_stack(e1, lab1);
        throw err;
    }

    lisp->reset_in_stack(e2, lab2);
    lisp->reset_in_stack(e1, lab1);

    return del;
}
