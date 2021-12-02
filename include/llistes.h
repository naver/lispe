/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  listes.h
//
//

//This is the implementation of the List object

#ifndef llistes_h
#define llistes_h

#include "lispe.h"


class u_link {
public:
    
    Element* value;
    u_link* _next;
    u_link* _previous;
    short status;
    uint16_t mark;
    
    u_link(Element* v) {
        mark = 0;
        status = 0;
        value = v;
        _next = NULL;
        _previous = NULL;
    }
    
    void dec() {
        status--;
        if (!status) {
            value->decrement();
            delete this;
        }
    }

    void inc(long s) {
        status += s;
    }
    
    bool check_next() {
        return (_next != NULL && mark != _next->mark);
    }
    
    u_link* next() {
        if (_next != NULL && mark != _next->mark) {
            _next->mark = mark;
            return _next;
        }
        return NULL;
    }

    u_link* previous() {
        if (_previous != NULL && mark != _previous->mark) {
            _previous->mark = mark;
            return _previous;
        }
        return NULL;
    }

    void erase() {
        if (_previous)
            _previous->_next = _next;
        if (_next)
            _next->_previous = _previous;
        dec();
    }
    
    //we insert it before...
    void insert(u_link* e) {
        if (_previous) {
            _previous->_next = e;
            e->_previous = _previous;
        }
        e->_next = this;
        _previous = e;
        e->inc(status);
    }
  
};

class u_links {
public:
    
    u_link* first;
    uint16_t* mark;
    
    u_links(uint16_t* m) : mark(m) {
        first = NULL;
    }
    
    u_links(u_links& l, long from) {
        mark = l.mark;
        u_link* e = l.at(from);
        first = e;
        while (e != NULL) {
            e->inc(1);
            e = e->next();
        }
    }

    u_links(u_links& l, u_link* e) {
        mark = l.mark;
        l.initialize();
        first = e;
        while (e != NULL) {
            e->inc(1);
            e = e->next();
        }
    }

    bool empty() {
        return (first == NULL);
    }

    void clear() {
        u_link* e = begin();
        while (e != NULL) {
            first = e->next();
            e->dec();
            e = first;
        }
        first = NULL;
    }
    
    u_link* last() {
        u_link* e = begin();
        u_link* prev = NULL;
        while (e) {
            prev = e;
            e = e->next();
        }
        return prev;
    }
    
    void insertbeforelast(Element* v) {
        u_link* e = last();
        if (e == NULL) {
            first = new u_link(v);
            first->inc(1);
            return;
        }
        e->insert(new u_link(v));
    }
    
    void insert(long i, Element* v) {
        if (!i) {
            push_front(v);
            return;
        }
        
        u_link* c = begin();
        
        while (c->check_next() && i) {
            i--;
            c = c->next();
        }

        if (c->_next)
            c->insert(new u_link(v));
        else {
            c->_next = new u_link(v);
            c->_next->_previous = c;
            c->_next->inc(c->status);
        }
    }
    
    void push_front(Element* v) {
        u_link* e = new u_link(v);
        if (first == NULL)
            e->inc(1);
        else
            first->insert(e);
        first = e;
    }
    
    void push_back(Element* v) {
        u_link* e = last();
        if (e == NULL) {
            first = new u_link(v);
            first->inc(1);
            return;
        }
        e->_next = new u_link(v);
        e->_next->_previous = e;
        e->_next->inc(e->status);
    }

    void pop_front() {
        u_link* u = first;
        first = first->_next;
        u->erase();
    }

    void pop_back() {
        if (first) {
            u_link* e = last();
            if (first == e)
                first = NULL;
            e->erase();
        }
    }
    
    inline void initialize() {
        if (first != NULL) {
            ++*mark;
            if (first->mark == *mark)
                ++*mark;
            first->mark = *mark;
        }
    }
    
    u_link* begin() {
        initialize();
        return first;
    }
    
    u_link* end() {
        return NULL;
    }
    
    Element* front() {
        return first->value;
    }

    Element* back() {
        if (!first)
            return NULL;
        return last()->value;
    }

    u_link* b_at(long i) {
        u_link* e = last();
        if (!i)
            return e;
        
        while (e != NULL && i) {
            e = e->previous();
            i--;
        }
        return e;
    }


    u_link* at(long i) {
        if (!i)
            return first;
        
        u_link* c = begin();
        while (c != NULL && i) {
            c = c->next();
            i--;
        }
        return c;
    }

    Element* at_e(long i) {
        if (!i)
            return first->value;
        
        u_link* c = begin();
        while (c != NULL && i) {
            c = c->_next;
            i--;
        }
        if (c != NULL)
            return c->value;
        return NULL;
    }

    void erase(u_link* e) {
        if (e->_next == e)
            e->_next = NULL;
        if (first == e)
            first = first->_next;
        e->erase();
    }
    
    void decrement() {
        vector<u_link*> toclean;
        u_link* u = begin();
        while (u != NULL) {
            if (u->status == 1)
                toclean.push_back(u);
            else
                u->status--;
            u = u->next();
        }
        for (auto& a : toclean)
            a->dec();
    }
    
    long size() {
        long sz = 0;
        u_link* e = begin();
        while (e != NULL) {
            e = e->next();
            sz++;
        }
        return sz;
    }

    void reverse() {
        if (first != NULL && first->_next != NULL) {
            u_link* e = begin();
            //We invert the directions
            while (e->check_next()) {
                first = e->next();
                e->_next = e->_previous;
                e->_previous = first;
                e = first;
            }
            first->_next = first->_previous;
            first->_previous = NULL;
        }
    }

    void connect(u_links& l) {
        u_link* u;
        if (first == NULL) {
            first = l.first;
            u = begin();
            while (u != NULL) {
                u->inc(1);
                u =  u->next();
            }
        }
        else {
            u = last();
            u->_next = l.first;
            l.first->_previous = u;
            //if there is no cycle
            //No element in the current list is in l
            if (l.first->mark != u->mark) {
                short status = u->status;
                u = l.begin();
                while (u != NULL) {
                    u->inc(status);
                    u = u->next();
                }
            }
        }
    }
};

class LList : public Element {
public:

    u_links liste;
    char terminal;
    
    LList(uint16_t* m) : liste(m), terminal(0), Element(t_llist) {}
    LList(uint16_t* m, uint16_t s) : liste(m), terminal(0), Element(t_llist, s) {}
    LList(LList* l, long idx) : terminal(0), liste(l->liste, idx), Element(t_llist) {}
    LList(LList* l, u_link* it) : terminal(0), liste(l->liste, it), Element(t_llist) {}
    
    bool isContainer() {
        return true;
    }
    
    void setterminal(char v = 1) {
        terminal |= v;
    }
    
    bool element_container() {
        return true;
    }

    Element* asList(LispE* lisp);
    
    Element* loop(LispE* lisp, short label,  List* code);

    long find_element(LispE*, Element* element_value, long idx);
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* last_element(LispE* lisp);
    
    Element* last() {
        return liste.back();
    }

    inline u_link* at(long idx) {
        return liste.at(idx);
    }

    inline Element* at_e(long idx) {
        return liste.at_e(idx);
    }

    void insertion(Element* e, long idx) {
        e->increment();
        liste.insert(idx, e);
    }
    
    void front(Element* e) {
        liste.push_front(e);
    }
    
    void beforelast(Element* e) {
        liste.insertbeforelast(e);
        e->increment();
    }

    short label(long i) {
        return ((Atome*)at_e(i))->atome;
    }
    
    Element* thekeys(LispE* lisp);

    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    
    Element* fullcopy() {
        LList* l = new LList(liste.mark);
        for (u_link* a = liste.last(); a != liste.end(); a = a->previous()) {
            l->liste.push_front(a->value->fullcopy());
        }
        return l;
    }
    
    Element* copyatom(uint16_t s) {
        if (status < s)
            return this;

        LList* l = new LList(liste.mark);
        for (u_link* a = liste.last(); a != liste.end(); a = a->previous()) {
            l->liste.push_front(a->value->copyatom(s));
        }
        release();
        return l;
    }

    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        LList* l = new LList(liste.mark);
        for (u_link* a = liste.last(); a != liste.end(); a = a->previous()) {
            l->liste.push_front(a->value->copying(false));
        }
        return l;
    }
    

    Element* quoted(LispE*);
    Element* unique(LispE* lisp);
    Element* rotate(bool left);

    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(bool pair = false);
    
    bool isList() {
        return true;
    }
    
    bool isNotEmptyList() {
        return (!liste.empty());
    }
    
    void decrement() {
        if (is_protected())
            return;
        
        status--;
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    

    void incrementstatus(uint16_t nb) {
        if (!is_protected())
            status += nb;
    }

    void decrementstatus(uint16_t nb) {
        if (is_protected())
            return;
 
        status -= nb;
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    //The status is decremented without destroying the element.
    void decrementkeep() {
        if (is_protected())
            return;
        status--;
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long idx) {
        if (!idx)
            return liste.front();
        if (idx == liste.size() - 1)
            return liste.back();
        
        u_link*  it = liste.begin();
        for (;it != liste.end(); it++) {
            if (!idx) {
                return it->value;
            }
            idx--;
        }
        return NULL;
    }
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);

    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            liste.decrement();
            delete this;
        }
    }

    void rawrelease() {
        if (!status) {
            liste.decrement();
            liste.clear();
            delete this;
        }
    }
    
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    
    long size() {
        return liste.size();
    }
    
    long default_insertion() {
        return 0;
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    void garbaging_values(LispE*);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
        
        for (u_link* a = liste.begin(); a != liste.end(); a = a->next())
            a->value->protecting(protection, lisp);
    }
    
    wstring jsonString(LispE* lisp) {
        if (liste.empty())
            return L"[]";

        long sz = liste.size() - 1;
        
        wstring buffer(L"[");
        long i = 0;
        for (u_link* a = liste.begin(); a != liste.end(); a = a->next()) {
            if (i && i <= sz)
                buffer += L",";
            buffer += a->value->jsonString(lisp);
            i++;
        }
        buffer += L"]";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        if (liste.empty())
            return L"()";
        
        long sz = liste.size() - 1;
        
        wstring buffer(L"(");
        
        long i = 0;
        u_link* prev = liste.begin();
        for (u_link* a = prev; a != liste.end(); a = a->next()) {
            prev = a;
            if (i && i <= sz)
                buffer += L" ";
            buffer += a->value->stringInList(lisp);
            i++;
        }
        if (prev->_next)
            buffer += L"...";
        buffer += L")";
        return buffer;
    }

    u_ustring asUString(LispE* lisp) {
        if (liste.empty())
            return U"()";
        
        long sz = liste.size() - 1;

        u_ustring buffer(U"(");
        
        long i = 0;
        u_link* prev = liste.begin();
        for (u_link* a = prev; a != liste.end(); a = a->next()) {
            prev = a;
            if (i && i <= sz)
                buffer += U" ";
            buffer += a->value->stringInUList(lisp);
            i++;
        }
        if (prev->_next)
            buffer += U"...";
        buffer += U")";
        return buffer;
    }
    
    bool atleast2() {
        return (liste.first != NULL && liste.first->_next != NULL);
    }
    
    bool compare(LispE* lisp, List* comparison, short instruction, long i, long j);
    void sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax);
    void sorting(LispE* lisp, List* comparison);

    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);

    void push_front(Element* e) {
        liste.push_front(e);
        e->increment();
    }

    void append(Element* e) {
        liste.push_back(e);
        e->increment();
    }

    bool append_not_null(Element* e) {
        if (e != NULL) {
            liste.push_back(e);
            e->increment();
            return true;
        }
        return false;
    }

    void appendraw(Element* e) {
        liste.push_back(e);
    }

    void change(long i, Element* e) {
        u_link*  it = at(i);
        it->value->decrement();
        it->value = e;
        e->increment();
    }

    void changelast(Element* e) {
        liste.back()->decrement();
        liste.pop_back();
        liste.push_back(e);
        e->increment();
    }
    
    void replacing(long i, Element* e) {
        u_link*  it = at(i);
        if (e != it->value) {
            it->value->decrement();
            it->value = e;
            e->increment();
        }
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0)
            throw new Error("Error: position does not exist");
        u_link*  it = at(i);
        if (it == NULL)
            liste.push_back(e);
        else {
            it->value->decrement();
            it->value = e;
            e->increment();
        }
        return this;
    }
    
    bool Boolean() {
        return (!liste.empty());
    }
    
    
    Element* reverse(LispE*, bool duplique = true);
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
    void pop() {
        Element* e = liste.back();
        e->decrement();
        liste.pop_back();
    }

    bool removefirst() {
        if (!liste.size())
            return false;
        Element* e = liste.front();
        e->decrement();
        liste.pop_front();
        return true;
    }
    
    bool removelast() {
        if (!liste.size())
            return false;
        Element* e = liste.back();
        e->decrement();
        liste.pop_back();
        return true;
    }
    
    bool remove(LispE*, Element* e) {
        long d =  e->asInteger();
        return remove(d);
    }

    bool remove(long d) {
        if (liste.empty())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        
        u_link*  it = at(d);
        liste.erase(it);
        return true;
    }
        
    void concatenate(LispE* lisp, Element* e) {
        if (e->isList()) {
            if (e->type == t_llist) {
                liste.connect(((LList*)e)->liste);
            }
            else {
                for (long i = 0; i < e->size(); i++) {
                    append(e->value_on_index(lisp, i));
                }
            }
        }
        else
            append(e->copying(false));
    }
    
    Element* check_member(LispE*, Element* the_set);
    
    Element* insert(LispE* lisp, Element* e, long idx);

    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected()) {
            liste.decrement();
            liste.clear();
        }
    }

    void clear(Element* e) {
        if (!is_protected()) {
            for (u_link* a = liste.begin(); a != liste.end(); a = a->next()) {
                if (e != a->value)
                    a->value->decrement();
            }
            liste.clear();
        }
    }

    void clean() {
        if (!is_protected()) {
            liste.decrement();
            liste.clear();
        }
    }
    

    Element* newInstance() {
        return new List;
    }

    Element* bit_not(LispE* l);
    Element* bit_and(LispE* l, Element* e);
    Element* bit_and_not(LispE* l, Element* e);
    Element* bit_or(LispE* l, Element* e);
    Element* bit_xor(LispE* l, Element* e);
    Element* plus(LispE* l, Element* e);
    Element* minus(LispE* l, Element* e);
    Element* multiply(LispE* l, Element* e);
    Element* divide(LispE* l, Element* e);
    Element* mod(LispE* l, Element* e);
    Element* power(LispE* l, Element* e);
    Element* leftshift(LispE* l, Element* e);
    Element* rightshift(LispE* l, Element* e);

};

#endif


