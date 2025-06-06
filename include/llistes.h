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
    uint32_t mark;
    uint16_t status;
#ifdef LISPE_WASM_NO_EXCEPTION
    bool error;
#endif

    u_link(Element* v) {
#ifdef LISPE_WASM_NO_EXCEPTION
        error = false;
#endif
        status = 0;
        mark = 0;
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

    void inc(uint16_t s) {
        status += s;
    }
    
    void release() {
        if (!status) {
            value->release();
            delete this;
        }
    }
    
    virtual bool check_cycle() {
        return (_next != NULL && mark == _next->mark);
    }
    
    virtual bool check_next() {
        return (_next != NULL && mark != _next->mark);
    }
    
    virtual u_link* next() {
        if (_next != NULL && mark != _next->mark) {
            _next->mark = mark;
            return _next;
        }
        return NULL;
    }

    u_link* next_mark() {
        _next->mark = mark;
        return _next;
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

    //we insert it after...
    virtual void u_push(u_link* e) {
        e->_previous = this;
        _next = e;
        e->inc(status);
    }

    virtual bool isFinal() {
        return false;
    }
    
    virtual void u_connect(Element* v) {
        _next = new u_link(v);
        _next->_previous = this;
        _next->inc(status);
    }

    virtual void connect_as_last(Element* v);

};

class u_link_last : public u_link {
public:
    u_link_last(Element* v) : u_link(v) {}

#ifdef LISPE_WASM_NO_EXCEPTION
    void u_push(u_link* e) {
        error = true;
    }

    void u_connect(Element* v) {
        error = true;
    }

    void connect_as_last(Element* v) {
        error = true;
    }
#else
    void u_push(u_link* e) {
        throw new Error("Error: cannot add an element to this list");
    }

    void u_connect(Element* v) {
        throw new Error("Error: cannot add an element to this list");
    }

    void connect_as_last(Element* v) {
        throw new Error("Error: cannot add an element to this list");
    }
#endif
    bool isFinal() {
        return true;
    }
    
    bool check_cycle() {
        return false;
    }
    
    bool check_next() {
        return false;
    }
    
    u_link* next() {
        return NULL;
    }

};


class u_links {
public:
    
    u_link* first;
    uint32_t* mark;
    
    u_links(uint32_t* m) : mark(m) {
        first = NULL;
    }

    u_links(u_links& l, long from) {
        mark = l.mark;
        u_link* e = l.at(from);
        first = e;
        initialize(e);
        while (e != NULL) {
            e->inc(1);
            e = e->next();
        }
    }

    u_links(u_links& l, u_link* e) {
        mark = l.mark;
        first = e;
        if (e != NULL) {
            initialize(e);
            while (e != NULL) {
                e->inc(1);
                e = e->next();
            }
        }
    }

    bool isone() {
        return (first != NULL && first->_next == NULL);
    }
    
    bool atleast2() {
        return (first != NULL && first->_next != NULL && first->_next != first);
    }
    
    inline void initialize(u_link* e) {
        e->mark = ++*mark;
    }
    
    inline void initialize() {
        if (first != NULL) {
            first->mark = ++*mark;
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
    
    bool check_cycle() {
        u_link* e = begin();
        while (e != NULL) {
            if (e->check_cycle())
                return true;
            e = e->next();
        }
        return false;
    }
    
    u_link* last() {
        if (!first)
            return NULL;
        u_link* e = begin();
        while (e->check_next()) {
            e = e->next_mark();
        }
        initialize(e);
        return e;
    }

    u_link* last(long& sz) {
        sz = 0;
        if (!first)
            return NULL;
        u_link* e = begin();
        while (e->check_next()) {
            sz++;
            e = e->next_mark();
        }
        initialize(e);
        return e;
    }
    
    void reset_head() {
        if (first == NULL)
            return;
        while (first->_previous != NULL)
            first = first->_previous;
    }

    //Here we do not reset the marks at the end
    //of the loop.
    u_link* last_raw() {
        if (!first)
            return NULL;
        u_link* e = begin();
        while (e->check_next()) {
            e = e->next_mark();
        }
        return e;
    }

    void insertbeforelast(Element* v) {
        u_link* e = last_raw();
        if (e == NULL) {
            first = new u_link(v);
            first->inc(1);
            return;
        }
        e->insert(new u_link(v));
    }
    
    bool insert(long i, Element* v) {
        if (!i) {
            push_front(v);
            return true;
        }
        
        u_link* c = begin();
        
        while (c->check_next() && i) {
            i--;
            c = c->next();
        }

        if (c->_next)
            c->insert(new u_link(v));
        else {
            c->u_connect(v);
#ifdef LISPE_WASM_NO_EXCEPTION
            if (c->error) {
                c->error = false;
                return false;
            }
#endif
        }
        return true;
    }
    
    void push_front(Element* v) {
        u_link* e = new u_link(v);
        if (first == NULL)
            e->inc(1);
        else
            first->insert(e);
        first = e;
    }

    void push_front(Element* v, bool is_final);

#ifdef LISP_WASM
    bool push_back(Element* v) {
        u_link* e = last_raw();
        if (e == NULL) {
            first = new u_link(v);
            first->inc(1);
            return true;
        }

        e->u_connect(v);
        if (e->error) {
            e->error = false;
            return false;
        }
        return true;
    }
#else
    void push_back(Element* v) {
        u_link* e = last_raw();
        if (e == NULL) {
            first = new u_link(v);
            first->inc(1);
            return;
        }

        e->u_connect(v);
    }
#endif
    
    void push_back_as_last(LispE* lisp, Element* v);
    
    void pop_front() {
        u_link* u = first;
        first = first->_next;
        u->erase();
    }

    void pop_back() {
        if (first) {
            u_link* e = last_raw();
            if (first == e)
                first = NULL;
            e->erase();
        }
    }
    
    u_link* begin() {
        initialize();
        return first;
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
        if (e != NULL)
            initialize(e);
        return e;
    }

    u_link* at(long i) {
        u_link* c = begin();
        if (!i)
            return c;
                
        while (c != NULL && i) {
            c = c->next();
            i--;
        }
        if (c != NULL)
            initialize(c);
        return c;
    }

    Element* b_at_e(long i) {
        u_link* e = last();
        if (e == NULL)
            return NULL;
        
        if (!i)
            return e->value;
        
        while (e != NULL && i) {
            e = e->previous();
            i--;
        }
        return (e == NULL)?NULL:e->value;
    }

    Element* at_e(long i) {
        if (first == NULL)
            return NULL;
        
        if (!i)
            return first->value;
        
        u_link* c = begin();
        while (c != NULL && i) {
            c = c->next();
            i--;
        }
        return (c == NULL)?NULL:c->value;
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
        for (const auto& a : toclean)
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

    void reverse(LispE* lisp);
    void connect(LispE* lisp, u_links& l);
    
    inline void to_vector(std::vector<Element*>& v) {
        u_link* u = begin();
        while (u != NULL) {
            v.push_back(u->value);
            u = u->next();
        }
    }

};

class LList : public Element {
public:

    u_links liste;
    
    LList(uint32_t* m) : liste(m), Element(t_llist) {}
    LList(uint32_t* m, uint16_t s) : liste(m), Element(t_llist, s) {}
    LList(LList* l, long idx) : liste(l->liste, idx), Element(t_llist) {}
    LList(LList* l, u_link* it) : liste(l->liste, it), Element(t_llist) {}
    
    bool isContainer() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    bool element_container(Element* e) {
        if (!status) {
            e->increment();
            release();
            e->decrementkeep();
        }
        return true;
    }

    Element* asList(LispE* lisp, List* l);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);

    long find_element(LispE*, Element* element_value, long idx);
    bool check_element(LispE* lisp, Element* element_value);
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* replace_all_elements(LispE*, Element* element_value, Element* remp);
    Element* count_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* list_and(LispE*, Element* value);
    Element* list_xor(LispE*, Element* value);
    Element* list_or(LispE*, Element* value);

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

    bool insertion(Element* e, long idx) {
        e->increment();
        return liste.insert(idx, e);
    }
    
    void front(Element* e) {
        liste.push_front(e);
    }
    
    void copyfrom(Element* x) {
        liste.clear();
        LList* l = (LList*)x;
        u_link* a = l->liste.last();
        if (a == NULL)
            return;
        
        u_link* tail = NULL;
        bool cyclic = (a->_next != NULL);

        for (; a != NULL; a = a->previous()) {
            push_front(a->value->fullcopy(), a->isFinal());
            if (cyclic) {
                tail = liste.first;
                cyclic = false;
            }
        }
        
        if (tail != NULL) {
            //there is a cycle
            //we need to reproduce it...
            liste.first->_previous = tail;
            tail->_next = liste.first;
        }
    }

    void* begin_iter() {
        u_links* u = new u_links(liste.mark);
        u->first = liste.begin();
        return u;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);

    void clean_iter(void* it) {
        delete (u_links*)it;
    }

    void beforelast(Element* e) {
        liste.insertbeforelast(e);
        e->increment();
    }

    int16_t label(long i) {
        return ((Atome*)at_e(i))->atome;
    }
    
    Element* thekeys(LispE* lisp);

    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);

    Element* fullcopy() {
        LList* l = new LList(liste.mark);

        u_link* a = liste.last();
        if (a == NULL)
            return l;
        
        u_link* tail = NULL;
        bool cyclic = (a->_next != NULL);

        for (; a != NULL; a = a->previous()) {
            l->push_front(a->value->fullcopy(), a->isFinal());
            if (cyclic) {
                tail = l->liste.first;
                cyclic = false;
            }
        }
        if (tail != NULL) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }

        return l;
    }
    
    Element* copyatom(LispE* lisp, uint16_t s) {
        if (status < s)
            return this;

        LList* l = new LList(liste.mark);
        u_link* a = liste.last();
        if (a == NULL) {
            release();
            return l;
        }

        bool cyclic = (a->_next != NULL);
        u_link* tail = NULL;

        for (; a != NULL; a = a->previous()) {
            l->push_front(a->value->copyatom(lisp, s), a->isFinal());
            if (cyclic) {
                tail = l->liste.first;
                cyclic = false;
            }
        }
        if (tail != NULL) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }
        release();
        return l;
    }

    void build(LispE* lisp, vecte<long>& shape, long isz, LList* res, LList* lst, u_link** idx);
    
    LList* back_duplicate() {
        LList* l = new LList(liste.mark);
        u_link* a = liste.last();
        if (a == NULL)
            return l;

        bool cyclic = (a->_next != NULL);
        u_link* tail = NULL;

        for (; a != NULL; a = a->previous()) {
            l->push_front(a->value->copying(true), a->isFinal());
            if (cyclic) {
                tail = l->liste.first;
                cyclic = false;
            }
        }
        if (tail != NULL) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }
        return l;
    }
    
    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        return back_duplicate();
    }    

    Element* quoted(LispE*);
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);

    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool checkShape(long depth, vecte<long>& sz) {
        if (size() != sz[depth])
            return false;
        if (depth == sz.size()-1) {
            for (u_link* u = liste.begin(); u != NULL; u = u->next()) {
                if (u->value->isList())
                    return false;
            }
            return true;
        }
        
        for (u_link* u = liste.begin(); u != NULL; u = u->next()) {
            if (!u->value->isList())
                return false;
            if (!u->value->checkShape(depth+1,sz))
                return false;
        }
        return true;
    }

    void getShape(vecte<long>& sz) {
        long s;
        Element* l = this;
        while (l->type != v_null) {
            s = l->size();
            if (!s)
                return;
            sz.push_back(s);
            l = l->index(0);
        }
    }

    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }

    bool isNotEmptyList() {
        return (!liste.empty());
    }
    
    void decrement() {
        status -= not_protected();
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    void decrementstatus(uint16_t nb) {
        status -= nb * not_protected();
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long idx) {
        if (!idx)
            return liste.front();
        u_link*  it = liste.begin();
        for (;it != NULL; it = it->next()) {
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
    
    bool is_cyclic() {
        return liste.check_cycle();
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
        
        for (u_link* a = liste.begin(); a != NULL; a = a->next())
            a->value->protecting(protection, lisp);
    }
    
    wstring jsonString(LispE* lisp) {
        if (liste.empty())
            return L"[]";

        long sz = liste.size() - 1;
        
        wstring buffer(L"[");
        long i = 0;
        for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
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
                
        wstring buffer(L"(");
        
        bool first = true;
        u_link* prev = liste.begin();
        for (u_link* a = prev; a != NULL; a = a->next()) {
            prev = a;
            if (!first)
                buffer += L" ";
            else
                first = false;
            if (a->isFinal())
                buffer += L". ";
            buffer += a->value->stringInList(lisp);
        }
        if (prev->_next)
            buffer += L" ...";
        buffer += L")";
        return buffer;
    }

    u_ustring asUString(LispE* lisp) {
        if (liste.empty())
            return U"()";
        
        u_ustring buffer(U"(");
        
        bool first = true;
        u_link* prev = liste.begin();
        for (u_link* a = prev; a != NULL; a = a->next()) {
            prev = a;
            if (!first)
                buffer += U" ";
            else
                first = false;
            if (a->isFinal())
                buffer += U". ";
          buffer += a->value->stringInUList(lisp);
        }
        if (prev->_next)
            buffer += U" ...";
        buffer += U")";
        return buffer;
    }
    
    bool isone() {
        return liste.isone();
    }
    
    bool atleast2() {
        return liste.atleast2();
    }
    
    //bool compare(LispE* lisp, List* comparison, int16_t instruction, long i, long j);
    //void sorting(LispE* lisp, List* comparison, int16_t instruction, long rmin, long rmax);
    //void sorting(LispE* lisp, List* comparison);

    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);

    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);

    void push_front(Element* e) {
        liste.push_front(e);
        e->increment();
    }

    void push_front(Element* e, bool is_final) {
        liste.push_front(e, is_final);
        e->increment();
    }

    void append(Element* e) {
        liste.push_back(e);
        e->increment();
    }

    void append_as_last(LispE* lisp, Element* e) {
        liste.push_back_as_last(lisp, e);
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
        liste.pop_back();
    }

    bool removefirst() {
        if (!liste.size())
            return false;
        liste.pop_front();
        return true;
    }
    
    bool removelast() {
        if (!liste.size())
            return false;
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
                liste.connect(lisp, ((LList*)e)->liste);
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
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
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
            for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
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
        return new LList(liste.mark);
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

class Iter_llist : public Element {
public:
    vector<Element*> elements;
    LList* reference;
    Iter_llist(LList* l, long sz) : Element(t_llist) {
        elements.reserve(sz);
        reference = l;
        u_link* e = l->liste.begin();
        while (e != NULL) {
            elements.push_back(e->value);
            e = e->next();
        }
    }
    
    Element* index(long idx) {
        return elements[idx];
    }

    Element* value_on_index(LispE* lisp, long i);

    ~Iter_llist() {
        reference->release();
    }
};


#endif


