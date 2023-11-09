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


#ifndef listes_h
#define listes_h

#include "vecte.h"
#include <list>

typedef Element* (List::*methodEval)(LispE*);
typedef enum {a_various_list, a_flat_list, a_valuelist, a_structured_list, a_tensor} liste_type;

class Matrice_number;
class Shorts;

class Atomefonction : public Element {
public:
    Element* body;
    int16_t function_label;
    
    Atomefonction(Element* b, int16_t a) : body(b), Element(a) {
        function_label = b->index(1)->label();
    }
    
    u_ustring asUString(LispE* lisp);
    
    Element* eval(LispE* lisp) {
        return body;
    }
    
    int16_t label() {
        return function_label;
    }
};

class ITEM {
public:
    Element** buffer;
    uint64_t last;
    uint64_t sz;
    uint64_t status;
    
    ITEM(long t) {
        status = 0; //this is the reference counter
        last = 0; //this is the last element
        sz = t; //this is the size
        //We always create one more element
        //to handle some room for exchange
        buffer = (Element**)malloc(sizeof(Element*)*(sz + 1));
        //hence buffer[sz] does exist even though
        //it cannot be accessed through normal means
    }
    
    inline void reserve(long t) {
        if (t > sz) {
            sz = t;
            //We reallocate our vecteur
            buffer = (Element**)realloc(buffer, sizeof(Element*)*(sz + 1));
        }
    }
    
    inline void resize(long t) {
        if (t >= sz) {
            sz = t << 1;
            //We reallocate our vecteur
            buffer = (Element**)realloc(buffer, sizeof(Element*)*(sz + 1));
        }
    }
    
    inline Element* operator[](long pos) {
        return buffer[pos];
    }

    inline void swap(long left, long right) {
        //We use the fact that the last element exists
        //but cannot be accessed...
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
    }
    
    inline bool reverse(long left, long right) {
        if (left >= right)
            return false;
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
        return true;
    }
    
    inline void erase(long i) {
        if (i >= 0 && i < last) {
            buffer[i]->decrement();
            last--;
            for (;i < last; i++)
                buffer[i] = buffer[i+1];
        }
    }
    
    inline void pop_back() {
        if (last) {
            last--;
            buffer[last]->decrement();
        }
    }
    
    inline void insert(long pos, Element* val) {
        resize(last);
        
        val->increment();
        if (pos >= last) {
            buffer[last++] = val;
            return;
        }
        
        //All elements are moved to the right.
        for (long i = last; i > pos; i--)
            buffer[i] = buffer[i - 1];
        buffer[pos] = val;
        last++;
    }
    
    inline void extend(ITEM* val, long val_home, long home) {
        resize(last+val->last);
        Element* e;
        for (long i = val_home; i < val->last; i++) {
            e = val->buffer[i];
            e->incrementstatus(!e->equal_item(this, home));
            buffer[last++] = e;
        }
    }
    
    inline void push_back(Element* val, long home) {
        resize(last);
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
        val->incrementstatus(!val->equal_item(this, home));
    }
    
    inline void push_raw(Element* val) {
        resize(last);
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
    }
    
    inline void increment() {
        if (!status) {
            for (long i = 0; i < last; i++) {
                buffer[i]->increment();
            }
        }
    }
    
    inline void increment(uint16_t nb) {
        if (!status) {
            for (long i = 0; i < last; i++) {
                buffer[i]->incrementstatus(nb);
            }
        }
    }
    
    inline void decrement() {
        if (!status) {
            for (long i = 0; i < last; i++) {
                buffer[i]->decrement();
            }
        }
    }
    
    inline void decrement(uint16_t nb) {
        if (!status) {
            for (long i = 0; i < last; i++) {
                buffer[i]->decrementstatus(nb);
            }
        }
    }
    
    inline void decrement(Element* e) {
        if (!status) {
            for (long i = 0; i < last; i++) {
                if (e == buffer[i])
                    e->decrementkeep();
                else
                    buffer[i]->decrement();
            }
        }
    }

    inline void clean(long i) {
        for (; i < last; i++) {
            buffer[i]->decrement();
        }
    }

    inline void setAndQuote(long i, Element* v) {
        ((Quoted*)buffer[i])->requoting(v);
    }
    
    
    ~ITEM() {
        free(buffer);
    }
    
};

class LIST {
public:
    
    //It is always the same
    ITEM* item;
    Element* object;
    long home;
    bool marking;
    bool usermarking;
    
    LIST(LIST& l, long pos) {
        object = NULL;
        marking = false;
        usermarking = false;
        home  = pos + l.home;
        item = l.item;
        //We modify the common reference counter
        item->status++;
    }
    
    LIST(long t) {
        object = NULL;
        marking = false;
        usermarking = false;
        home = 0;
        item = new ITEM(t);
    }
    
    ~LIST() {
        if (!item->status)
            delete item;
        else
            item->status--;
    }
    
    inline bool equal(LIST& l) {
        return ((item->last == 0 && l.item->last == 0) || (l.item == item && home == l.home));
    }
    
    inline void reserve(long sz) {
        item->reserve(sz);
    }
    
    inline void swap(long i, long j) {
        item->swap(i + home, j + home);
    }
    
    inline void put(long pos, Element* val) {
        item->buffer[pos + home] = val;
    }
    
    inline Element* exchange(long pos, Element* val) {
        Element* e = item->buffer[pos + home];
        item->buffer[pos + home] = val;
        return e;
    }
    
    inline Element* exchangelast(Element* val) {
        Element* e = item->buffer[item->last - 1];
        item->buffer[item->last - 1] = val;
        return e;
    }
        
    uint16_t shared(uint16_t status) {
        return status + (item->status != 0);
    }
    
    inline void setmark(bool v) {
        marking = v;
    }
    
    inline bool mark() {
        return marking;
    }
    
    inline bool cleanfromhome() {
        item->clean(home);
        item->last = home;
        home--;
        return (home >= 0);
    }

    inline void setusermark(bool v) {
        usermarking = v;
    }
    
    inline bool usermark() {
        return usermarking;
    }
    
    long _size() {
        return item == NULL?0:item->last - home;
    }
    
    long size() {
        return item->last - home;
    }
    
    void clear() {
        home = 0;
        if (item->status) {
            //In this case it is a shared buffer
            //We have no right to it anymore
            //We need to provide a new one
            item->status--;
            item = new ITEM(8);
        }
        else
            item->last = 0;
    }
    
    void clean() {
        item->decrement();
        item->last = home;
    }
    
    inline void pop_back() {
        item->pop_back();
    }
    
    inline void insert(long pos, Element* val) {
        item->insert(pos + home, val);
    }
    
    inline Element* back() {
        return item->buffer[item->last - 1];
    }
    
    inline Element* popback() {
        return item->buffer[--item->last];
    }
    
    inline void push_element(Element* val) {
        item->push_back(val, home);
    }
    
    inline void extend(LIST* val) {
        item->extend(val->item, val->home, home);
    }
    
    inline void push_raw(Element* val) {
        item->push_raw(val);
    }
    
    inline Element*& operator[](long pos) {
        return item->buffer[pos+home];
    }
    
    inline int16_t get0() {
        return item->buffer[home]->type;
    }
    
    void erase(long pos) {
        item->erase(pos +home);
    }
    
    void reverse() {
        long sz = item->last - 1;
        for (long i = home; i < sz && item->reverse(i,sz); i++) {
            sz--;
        }
    }
    
    void display(LispE* lisp, long rmin, long rmax) {
        cout << "(";
        for (long i = rmin; i <= rmax; i++) {
            if (i != rmin)
                cout << " ";
            cout << item->buffer[i]->toString(lisp);
        }
        cout << ")" << endl;
    }
    
    inline void setAndQuote(long i, Element* v) {
        item->setAndQuote(home + i, v);
    }
    
    bool compare(LispE*, List* compare, int16_t instruction, long i, long j);
    void sorting(LispE*, List* f, int16_t instruction, long b, long e);
    void sorting(LispE*, List* f);
    
    void operator =(LIST& z) {
        item->last = home;
        for (long i = 0; i < z.size(); i++)
            push_element(z[i]);
    }
    
    inline void increment() {
        if (!marking) {
            marking = true;
            item->increment(home);
            marking = false;
        }
    }
    
    inline void decrement() {
        if (!marking) {
            marking = true;
            item->decrement();
            marking = false;
        }
    }
    
    inline void increment(uint16_t nb) {
        if (!marking) {
            marking = true;
            item->increment(nb);
            marking = false;
        }
    }
    
    inline void decrement(uint16_t nb) {
        if (!marking) {
            marking = true;
            item->decrement(nb);
            marking = false;
        }
    }
    
    inline void decrement(Element* e) {
        if (!marking) {
            marking = true;
            item->decrement(e);
            marking = false;
        }
    }
    
    inline void decrement_and_clear() {
        //If there is some kind of sharing
        //we re-create a new item, and release the grip
        //on the current one...
        //otherwise, we clean the current structure
        //since, this is the only access...
        if (item->status) {
            item->status--;
            item = new ITEM(item->sz);
            home = 0;
        }
        else {
            //We clean the whole structure
            //and reset last to its first element
            item->decrement();
            item->last = home;
        }
    }
    
    inline void decrement_and_clear(Element* e) {
        //If there is some kind of sharing
        //we re-create a new item, and release the grip
        //on the current one...
        //otherwise, we clean the current structure
        //since, this is the only access...
        if (item->status) {
            item->status--;
            item = new ITEM(item->sz);
            home = 0;
        }
        else {
            //We clean the whole structure
            //and reset last to its first element
            item->decrement(e);
            item->last = home;
        }
    }
    
    inline Element* at(long i) {
        i += home;
        if (i < 0 || i >= item->last)
            throw new Error("LIST error");
        return item->buffer[i];
    }
    
    inline bool is_not_empty() {
        return (home != item->last);
    }
    
    inline bool empty() {
        return (home == item->last);
    }
    
    inline bool checkType() {
        return (home != item->last && item->buffer[0]->type < l_final);
    }
    
    void get(std::vector<Element*>& v) {
        for (long i = home; i < item->last; i++)
            v.push_back(item->buffer[i]);
    }
    
    void set(std::vector<Element*>& v) {
        item->last = home;
        for (long i = 0; i < v.size(); i++) {
            item->buffer[item->last++] = v[i];
        }
    }
    
    inline void to_vector(std::vector<Element*>& v) {
        for (long i = home; i < item->last; i++)
            v.push_back(item->buffer[i]);
    }
    
};

class List : public Element {
public:
    
    LIST liste;
    char terminal;
    
    List() : terminal(0), liste(8), Element(t_list) {}
    List(uint16_t s) : terminal(0), liste(1), Element(t_list, s) {}
    
    //In all other case, we "borrow" the ITEM object to create a LIST object that will
    //share the same content. No copy or duplication is necessary.
    //ITEM exposes a "status" value that is used to count the number of times an object has been borrowed
    //to correctly assess when it can be safely deleted.
    //When a CDR is called, it will share this list's item, but with a different "home" value.
    //The "home value" in a LIST object defines where it starts in the internal buffer of ITEM
    List(List* l, long p) : terminal(0), liste(l->liste, p), Element(t_list) {}
    
    bool isContainer() {
        return true;
    }
    
    virtual Element* negate(LispE* lisp);
    
    Element* quoting() {
        return new Quoted(this);
    }
    
    void setterminal(char v = 1) {
        terminal |= v;
    }
    
    Element* asList(LispE* lisp, List* l) {
        l->release();
        return this;
    }
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    bool is_quote() {
        return (!liste.empty() && liste[0]->type == l_quote);
        
    }
    bool equal_item(ITEM* i, long home) {
        return (liste.home == home && liste.item == i);
    }
    
    virtual Element* loop(LispE* lisp, int16_t label,  List* code);
    Element* mloop(LispE* lisp);
    Element* lloop(LispE* lisp);
    
    void set_from(Element* e, long i) {
        append(e->index(i));
    }
    
    void set_from(Element* e, long i, long j) {
        while (i != j) {
            append(e->index(i++));
        }
    }
    
    void set_in(LispE* lisp, Element* e, long i) {
        liste[i]->decrement();
        liste[i] = e;
        e->increment();
    }
    
    bool check_element(LispE* lisp, Element* element_value);
    long find_element(LispE*, Element* element_value, long idx);
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* replace_all_elements(LispE*, Element* element_value, Element* remp);
    Element* count_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* list_and(LispE*, Element* value);
    Element* list_xor(LispE*, Element* value);
    Element* list_or(LispE*, Element* value);
    
    virtual Element* last_element(LispE* lisp);
    
    Element* last() {
        return liste.back();
    }
    
    void swap(long i, long j) {
        liste.swap(i, j);
    }
        
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e);
        return true;
    }
    
    void front(Element* e) {
        liste.insert(0, e);
    }
    
    void beforelast(Element* e) {
        long sz = liste.size();
        if (!sz)
            liste.push_element(e);
        else
            liste.insert(sz-1, e);
    }
    
    void buildList(LispE* lisp, Element* result, Element* current, vecte<long>& shape, vecte<long>& positions, long idx, long axis);
    Element* storeRank(LispE* lisp, Element* result, Element* current, vecte<long>& shape, vecte<long>& positions, long idx);
    Element* rank(LispE* lisp, vecte<long>& positions);
    
    int16_t label(long i) {
        return ((Atome*)liste.item->buffer[i])->atome;
    }
    
    Element* thekeys(LispE* lisp);
    
    bool isFunction() {
        return (liste.size() > 1 && liste[0]->label() >= l_lambda && liste[0]->label() <= l_defpat);
    }
    
    virtual int infoIdx() {
        return 0;
    }
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
        
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    virtual void clean_iter(void* it) {
        delete (long*)it;
    }
    
    bool check_arity(LispE* lisp, unsigned long arity) {
        if (isFunction()) {
            unsigned long arity_function;
            if (liste[0]->label() == l_lambda) {
                arity_function = 1 << (1 + liste[1]->size());
            }
            else
                arity_function = 1 << (1 + liste[2]->size());
            return (arity == arity_function);
        }
        return false;
    }
    
    void build(LispE* lisp, vecte<long>& shape, long isz, Element* res, Element* lst, long& idx);
    
    bool isExecutable(LispE*);
    
    virtual char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    virtual Element* fullcopy() {
        if (liste.marking)
            return liste.object;
        liste.marking = true;
        liste.object = new List;
        for (long i = 0; i < liste.size(); i++) {
            liste.object->append(liste[i]->fullcopy());
        }
        liste.marking = false;
        return liste.object;
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s);
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        List* l = new List;
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copying(false));
        }
        return l;
    }
    
    
    Element* release_but_last() {
        Element* e = liste.back();
        liste.item->last--;
        release();
        e->decrementkeep();
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Stringbytes* l);
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    virtual Element* duplicate_constant(LispE* lisp);
    
    virtual bool isList() {
        return true;
    }
    
    bool element_container() {
        return true;
    }
    
    bool isLambda() {
        return (liste.size() && liste.item->buffer[0]->type == l_lambda);
    }
    
    bool isNULL() {
        return liste.empty();
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    bool isNotEmptyList() {
        return (!liste.empty());
    }
    
    virtual void decrement() {
        status -= not_protected();
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    
    virtual void decrementstatus(uint16_t nb) {
        status -= nb * not_protected();
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    virtual Element* index(long i) {
        return liste[i];
    }
    
    Element* index(vecte<long> idx) {
        Element* e = this;
        for (short i = 0; i < idx.size(); i++)
            e = e->index(idx[i]);
        return e;
    }
    
    //The element in position i is in a quote
    //We replace it with the new element v
    inline void in_quote(long i, Element* v) {
        liste.setAndQuote(i, v);
    }
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void combine(LispE* lisp, Element* result, Element* l1, Element* l2, List* action, char& char_tensor);
    
    virtual void release() {
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    virtual void rawrelease() {
        if (!status) {
            liste.clear();
            liste.decrement();
            delete this;
        }
    }
    
    virtual void release(Element* e) {
        if (!status) {
            liste.decrement(e);
            e->decrementkeep();
            delete this;
        }
    }
    
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    
    virtual long size() {
        return liste.size();
    }
    
    virtual long shapesize() {
        return liste.size();
    }
    
    virtual void dimensions(long& v) {
        for (long i = 0; i < size(); i++) {
            liste[i]->dimensions(v);
        }
    }
    
    //This function is only used to compare the number of
    //parameters of a function and its arguments
    long argumentsize(long sz);
    
    Element* car(LispE* lisp);
    virtual Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    void garbaging_values(LispE*);
    
    virtual void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
        
        for (long i = 0; i < liste.size(); i++)
            liste[i]->protecting(protection, lisp);
    }
    
    void setmark(bool v) {
        liste.setmark(v);
    }
    
    bool mark() {
        return liste.mark();
    }
    
    void setusermark(bool v) {
        liste.setusermark(v);
    }
    
    bool usermark() {
        return  liste.usermark();
    }
    
    void resetusermark() {
        if (!liste.marking) {
            liste.marking = true;
            liste.usermarking = false;
            for (long i = 0; i < size(); i++) {
                liste[i]->resetusermark();
            }
            liste.marking = false;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        if (liste.mark())
            return L"#inf";
        
        liste.setmark(true);
        
        sz -= 1;
        
        wstring buffer(L"[");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            buffer += liste[i]->jsonString(lisp);
        }
        buffer += L"]";
        liste.setmark(true);
        return buffer;
    }
    
    virtual wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        if (liste[0]->type == l_quote && sz == 2) {
            wstring buffer(L"'");
            buffer += liste[1]->stringInList(lisp);
            return buffer;
        }
        
        if (liste.mark())
            return L"...";
        
        liste.setmark(true);
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            buffer += liste[i]->stringInList(lisp);
        }
        buffer += L")";
        liste.setmark(false);
        return buffer;
    }
    
    virtual u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        if (liste[0]->type == l_quote && sz == 2) {
            u_ustring buffer(U"'");
            buffer += liste[1]->stringInUList(lisp);
            return buffer;
        }
        
        if (liste.mark())
            return U"...";
        
        liste.setmark(true);
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            buffer += liste[i]->stringInUList(lisp);
        }
        buffer += U")";
        liste.setmark(false);
        return buffer;
    }
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_element(e);
    }
    
    virtual void extend(List* l) {
        liste.extend(&l->liste);
    }
    
    bool append_not_null(Element* e) {
        if (e != NULL) {
            liste.push_element(e);
            return true;
        }
        return false;
    }
    
    void appendraw(Element* e) {
        liste.push_raw(e);
    }
    
    void change(long i, Element* e) {
        liste[i]->decrement();
        liste[i] = e;
        e->increment();
    }
    
    void changelast(Element* e) {
        liste.back()->decrement();
        liste[liste.size()-1] = e;
        e->increment();
    }
    
    void replacing(long i, Element* e) {
        if (e != liste[i]) {
            liste[i]->decrement();
            liste[i] = e;
            e->increment();
        }
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_element(e);
        else {
            liste[i]->decrement();
            liste[i] = e;
            e->increment();
        }
        return this;
    }
    
    virtual Element* eval(LispE*);
    Element* eval_no_fail(LispE*);
    
    bool Boolean() {
        return (liste.size());
    }
    
    Element* eval_pattern(LispE* lisp, int16_t function_name);
    
    void evalthread(LispE*, List* body);
    Element* evalfunction(LispE*, Element* body);
    Element* eval_function(LispE*, List* body);
    Element* eval_library_function(LispE*, List* body);
    Element* eval_thread(LispE*, List* body);
    Element* eval_data(LispE*, Element* body);
    Element* eval_lambda(LispE*, List* body);
    Element* eval_lambda_min(LispE*);
    
    void evalAsUString(long i, LispE* lisp, u_ustring& w);
    void evalAsNumber(long i, LispE* lisp, double& d);
    void evalAsInteger(long i, LispE* lisp, long& d);
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    virtual int16_t label() {
        return (liste.is_not_empty()?t_list:v_null);
    }
    
    int16_t label0() {
        return liste.get0();
    }
    
    int16_t function_label(LispE* lisp) {
        return liste[0]->label();
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
        liste.erase(0);
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
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    virtual char isPureList() {
        for (long i = 0; i < liste.size(); i++) {
            if (liste[i]->isList())
                return a_structured_list;
        }
        return a_flat_list;
    }
    
    bool checkListofTensor() {
        for (long i = 0; i < liste.size(); i++) {
            if (!liste[i]->isTensor())
                return false;
        }
        return true;
    }
    
    virtual Element* newTensor(bool nb, LispE* lisp, List* l);
    
    virtual char isPureList(long& x, long& y) {
        x = liste.size();
        if (x) {
            if (liste[0]->isList()) {
                y = liste[0]->size();
                for (long i = 1; i < x; i++) {
                    if (!liste[i]->isList() || y != liste[i]->size())
                        return a_various_list;
                }
                return a_structured_list;
            }
            y = 1;
            for (long i = 1; i < x; i++) {
                if (liste[i]->isList())
                    return a_various_list;
            }
        }
        return a_flat_list;
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (e->isList()) {
            for (long i = 0; i < e->size(); i++) {
                append(e->value_on_index(lisp, i));
            }
        }
        else
            append(e->copying(false));
    }
    
    virtual Element* transposed(LispE* lisp);
    virtual Element* rotate(LispE* lisp, long axis);
    
    
    bool structuredList(long depth, vecte<long>& sz, short& element_type) {
        if (size() != sz[depth])
            return false;
        
        if (depth == sz.size()-1) {
            Element* e;
            for (long i = 0; i < size(); i++) {
                e = liste[i];
                if (!e->isNumber())
                    return false;
                element_type = (e->type > element_type)?e->type:element_type;
            }
            return true;
        }
        
        for (long i = 0; i < size(); i++) {
            if (!liste[i]->isList())
                return false;
            if (!liste[i]->structuredList(depth+1,sz, element_type))
                return false;
        }
        return true;
    }
    
    bool checkShape(long depth, vecte<long>& sz) {
        if (size() != sz[depth])
            return false;
        if (depth == sz.size()-1) {
            for (long i = 0; i < size(); i++) {
                if (liste[i]->isList())
                    return false;
            }
            return true;
        }
        
        for (long i = 0; i < size(); i++) {
            if (!liste[i]->isList())
                return false;
            if (!liste[i]->checkShape(depth+1,sz))
                return false;
        }
        return true;
    }
    
    void getShape(vecte<long>& sz) {
        Element* l = this;
        while (l->isList()) {
            sz.push_back(l->size());
            l = l->index(0);
        }
    }
            
    virtual Element* check_member(LispE*, Element* the_set);
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    void sameSizeNoTerminalArguments(LispE* lisp, Element* body, List* parameters);
    void differentSizeNoTerminalArguments(LispE* lisp, Element* body, List* parameters, long nbarguments, long defaultarguments);
    
    void sameSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* body, List* parameters);
    void differentSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* body, List* parameters, long nbarguments, long defaultarguments);
    
    void differentSizeTerminalArguments(LispE* lisp, List* parameters, long nbarguments,  long defaultarguments);
    void sameSizeTerminalArguments(LispE* lisp, List* parameters);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.decrement_and_clear();
    }
    
    void clear(Element* e) {
        if (!is_protected())
            liste.decrement_and_clear(e);
    }
    
    void clean() {
        liste.clean();
    }
    
    void copyfrom(Element* x) {
        liste.clean();
        List* l = (List*)x;
        for (long i = 0; i < l->size(); i++)
            append(l->liste[i]);
    }
    
    Element* transformargument(LispE*);
    
#ifdef MAX_STACK_SIZE_ENABLED
    Element* evall_set_max_stack_size(LispE* lisp);
#endif
    
    virtual Element* reduce(LispE* lisp, Element* l1, long sz);
    virtual Element* backreduce(LispE* lisp, Element* l1, long sz);    
    virtual Element* scan(LispE* lisp, Element* l1, long sz);
    virtual Element* backscan(LispE* lisp, Element* l1, long sz);
        
    virtual Element* eval_call_function(LispE* lisp);
    
    virtual Element* eval_call_self(LispE* lisp);
    
    Element* eval_error(LispE* lisp);
    
    Element* evalthreadspace(LispE* lisp, long listsize, long first);
    
    Element* evall_addr_(LispE* lisp);
    Element* evall_and(LispE* lisp);
    Element* evall_andvalue(LispE* lisp);
    Element* evall_apply(LispE* lisp);
    Element* evall_atomise(LispE* lisp);
    Element* evall_atomp(LispE* lisp);
    Element* evall_atoms(LispE* lisp);
    Element* evall_backreduce(LispE* lisp);
    Element* evall_backscan(LispE* lisp);
    Element* evall_bitand(LispE* lisp);
    Element* evall_bitandequal(LispE* lisp);
    Element* evall_bitandnot(LispE* lisp);
    Element* evall_bitandnotequal(LispE* lisp);
    Element* evall_bitnot(LispE* lisp);
    Element* evall_bitor(LispE* lisp);
    Element* evall_bitorequal(LispE* lisp);
    Element* evall_bitxor(LispE* lisp);
    Element* evall_bitxorequal(LispE* lisp);
    Element* evall_block(LispE* lisp);
    Element* evall_bodies(LispE* lisp);
    Element* evall_break(LispE* lisp);
    Element* evall_bytes(LispE* lisp);
    Element* evall_cadr(LispE* lisp);
    Element* evall_car(LispE* lisp);
    Element* evall_catch(LispE* lisp);
    Element* evall_cdr(LispE* lisp);
    Element* evall_check(LispE* lisp);
    Element* evall_compile(LispE* lisp);
    Element* evall_concatenate(LispE* lisp);
    Element* evall_cond(LispE* lisp);
    Element* evall_cons(LispE* lisp);
    Element* evall_conspoint(LispE* lisp);
    Element* evall_consb(LispE* lisp);
    Element* evall_consp(LispE* lisp);
    Element* evall_complex(LispE* lisp);
    Element* evall_real(LispE* lisp);
    Element* evall_imaginary(LispE* lisp);
    Element* evall_converttoatom(LispE* lisp);
    Element* evall_converttointeger(LispE* lisp);
    Element* evall_converttoshort(LispE* lisp);
    Element* evall_converttonumber(LispE* lisp);
    Element* evall_converttofloat(LispE* lisp);
    Element* evall_converttostring(LispE* lisp);
    Element* evall_converttostringbyte(LispE* lisp);
    Element* evall_count(LispE* lisp);
    Element* evall_slice(LispE* lisp);
    Element* evall_cyclicp(LispE* lisp);
    Element* evall_data(LispE* lisp);
    Element* evall_deflib(LispE* lisp);
    Element* evall_deflibpat(LispE* lisp);
    Element* evall_defmacro(LispE* lisp);
    Element* evall_defpat(LispE* lisp);
    Element* evall_defspace(LispE* lisp);
    Element* evall_defun(LispE* lisp);
    Element* evall_determinant(LispE* lisp);
    Element* evall_dictionary(LispE* lisp);
    Element* evall_dictionaryi(LispE* lisp);
    Element* evall_dictionaryn(LispE* lisp);
    Element* evall_different(LispE* lisp);
    Element* evall_divide(LispE* lisp);
    Element* evall_divideequal(LispE* lisp);
    Element* evall_clone(LispE* lisp);
    Element* evall_elapse(LispE* lisp);
    Element* evall_emptyp(LispE* lisp);
    Element* evall_emptylist(LispE* lisp);
    Element* evall_eq(LispE* lisp);
    Element* evall_equal(LispE* lisp);
    Element* evall_equalonezero(LispE* lisp);
    Element* evall_eval(LispE* lisp);
    Element* evall_extend(LispE* lisp);
    Element* evall_extract(LispE* lisp);
    Element* evall_factorial(LispE* lisp);
    Element* evall_bappend(LispE* lisp);
    Element* evall_fappend(LispE* lisp);
    Element* evall_filterlist(LispE* lisp);
    Element* evall_droplist(LispE* lisp);
    Element* evall_takelist(LispE* lisp);
    Element* evall_takenb(LispE* lisp);
    Element* evall_flatten(LispE* lisp);
    Element* evall_flip(LispE* lisp);
    Element* evall_stringf(LispE* lisp);
    Element* evall_bread(LispE* lisp);
    Element* evall_bwrite(LispE* lisp);
    Element* evall_fread(LispE* lisp);
    Element* evall_fwrite(LispE* lisp);
    Element* evall_getchar(LispE* lisp);
    Element* evall_greater(LispE* lisp);
    Element* evall_greaterorequal(LispE* lisp);
    Element* evall_if(LispE* lisp);
    Element* evall_ife(LispE* lisp);
    Element* evall_in(LispE* lisp);
    Element* evall_at_shape(LispE* lisp);
    Element* evall_at(LispE* lisp);
    Element* evall_index_zero(LispE* lisp);
    Element* evall_infix(LispE* lisp);
    Element* evall_innerproduct(LispE* lisp);
    Element* evall_input(LispE* lisp);
    Element* evall_irank(LispE* lisp);
    Element* evall_insert(LispE* lisp);
    Element* evall_integers(LispE* lisp);
    Element* evall_shorts(LispE* lisp);
    Element* evall_invert(LispE* lisp);
    Element* evall_iota(LispE* lisp);
    Element* evall_iota0(LispE* lisp);
    Element* evall_irange(LispE* lisp);
    Element* evall_irangein(LispE* lisp);
    Element* evall_join(LispE* lisp);
    Element* evall_key(LispE* lisp);
    Element* evall_keyi(LispE* lisp);
    Element* evall_keyn(LispE* lisp);
    Element* evall_keys(LispE* lisp);
    Element* evall_label(LispE* lisp);
    Element* evall_lambda(LispE* lisp);
    Element* evall_last(LispE* lisp);
    Element* evall_leftshift(LispE* lisp);
    Element* evall_leftshiftequal(LispE* lisp);
    Element* evall_link(LispE* lisp);
    Element* evall_list(LispE* lisp);
    Element* evall_llist(LispE* lisp);
    Element* evall_listand(LispE* lisp);
    Element* evall_listor(LispE* lisp);
    Element* evall_listxor(LispE* lisp);
    Element* evall_to_list(LispE* lisp);
    Element* evall_to_tensor(LispE* lisp);
    Element* evall_to_llist(LispE* lisp);
    Element* evall_load(LispE* lisp);
    Element* evall_let(LispE* lisp);
    Element* evall_lock(LispE* lisp);
    Element* evall_loop(LispE* lisp);
    Element* evall_loopcount(LispE* lisp);
    Element* evall_compare(LispE* lisp);
    Element* evall_lower(LispE* lisp);
    Element* evall_lowerorequal(LispE* lisp);
    Element* evall_lubksb(LispE* lisp);
    Element* evall_ludcmp(LispE* lisp);
    Element* evall_maplist(LispE* lisp);
    Element* evall_mark(LispE* lisp);
    Element* evall_matrix_string(LispE* lisp);
    Element* evall_matrix_stringbyte(LispE* lisp);
    Element* evall_matrix_short(LispE* lisp);
    Element* evall_matrix_integer(LispE* lisp);
    Element* evall_matrix_number(LispE* lisp);
    Element* evall_matrix_float(LispE* lisp);
    Element* evall_minmax(LispE* lisp);
    Element* evall_max(LispE* lisp);
    Element* evall_maybe(LispE* lisp);
    Element* evall_member(LispE* lisp);
    Element* evall_min(LispE* lisp);
    Element* evall_minus(LispE* lisp);
    Element* evall_minusequal(LispE* lisp);
    Element* evall_mod(LispE* lisp);
    Element* evall_modequal(LispE* lisp);
    Element* evall_multiply(LispE* lisp);
    Element* evall_multiplyequal(LispE* lisp);
    Element* evall_ncheck(LispE* lisp);
    Element* evall_nconc(LispE* lisp);
    Element* evall_nconcn(LispE* lisp);
    Element* evall_neq(LispE* lisp);
    Element* evall_next(LispE* lisp);
    Element* evall_not(LispE* lisp);
    Element* evall_nullp(LispE* lisp);
    Element* evall_numberp(LispE* lisp);
    Element* evall_numbers(LispE* lisp);
    Element* evall_floats(LispE* lisp);
    Element* evall_or(LispE* lisp);
    Element* evall_outerproduct(LispE* lisp);
    Element* evall_over(LispE* lisp);
    Element* evall_pipe(LispE* lisp);
    Element* evall_plus(LispE* lisp);
    Element* evall_plusequal(LispE* lisp);
    Element* evall_plusmultiply(LispE* lisp);
    Element* evall_pop(LispE* lisp);
    Element* evall_popfirst(LispE* lisp);
    Element* evall_poplast(LispE* lisp);
    Element* evall_power(LispE* lisp);
    Element* evall_powerequal(LispE* lisp);
    Element* evall_powerequal2(LispE* lisp);
    Element* evall_prettify(LispE* lisp);
    Element* evall_print(LispE* lisp);
    Element* evall_printerr(LispE* lisp);
    Element* evall_printerrln(LispE* lisp);
    Element* evall_println(LispE* lisp);
    Element* evall_product(LispE* lisp);
    Element* evall_pushtrue(LispE* lisp);
    Element* evall_push(LispE* lisp);
    Element* evall_pushfirst(LispE* lisp);
    Element* evall_pushlast(LispE* lisp);
    Element* evall_quote(LispE* lisp);
    Element* evall_quoted(LispE* lisp) {
        return this;
    }
    Element* evall_range(LispE* lisp);
    Element* evall_rangein(LispE* lisp);
    Element* evall_rank(LispE* lisp);
    Element* evall_reduce(LispE* lisp);
    Element* evall_replaceall(LispE* lisp);
    Element* evall_replicate(LispE*);
    Element* evall_resetmark(LispE* lisp);
    Element* evall_return(LispE* lisp);
    Element* evall_reverse(LispE* lisp);
    Element* evall_revertsearch(LispE* lisp);
    Element* evall_rho(LispE* lisp);
    Element* evall_rightshift(LispE* lisp);
    Element* evall_rightshiftequal(LispE* lisp);
    Element* evall_root(LispE* lisp);
    Element* evall_rotate(LispE* lisp);
    Element* evall_scan(LispE* lisp);
    Element* evall_search(LispE* lisp);
    Element* evall_searchall(LispE* lisp);
    Element* evall_select(LispE* lisp);
    Element* evall_sets(LispE* lisp);
    Element* evall_set(LispE* lisp);
    Element* evall_set_at(LispE* lisp);
    Element* evall_set_const(LispE* lisp);
    Element* evall_set_range(LispE* lisp);
    Element* evall_set_shape(LispE* lisp);
    Element* evall_setg(LispE* lisp);
    Element* evall_seti(LispE* lisp);
    Element* evall_setn(LispE* lisp);
    Element* evall_setq(LispE* lisp);
    Element* evall_seth(LispE* lisp);
    Element* evall_sign(LispE* lisp);
    Element* evall_signp(LispE* lisp);
    Element* evall_size(LispE* lisp);
    Element* evall_sleep(LispE* lisp);
    Element* evall_solve(LispE* lisp);
    Element* evall_sort(LispE* lisp);
    Element* evall_stringp(LispE* lisp);
    Element* evall_strings(LispE* lisp);
    Element* evall_stringbytes(LispE* lisp);
    Element* evall_space(LispE* lisp);
    Element* evall_sum(LispE* lisp);
    Element* evall_switch(LispE* lisp);
    Element* evall_tally(LispE* lisp);
    Element* evall_tensor_string(LispE* lisp);
    Element* evall_tensor_stringbyte(LispE* lisp);
    Element* evall_tensor_short(LispE* lisp);
    Element* evall_tensor_integer(LispE* lisp);
    Element* evall_tensor_number(LispE* lisp);
    Element* evall_tensor_float(LispE* lisp);
    Element* evall_threadclear(LispE* lisp);
    Element* evall_threadretrieve(LispE* lisp);
    Element* evall_threadstore(LispE* lisp);
    Element* evall_threadspace(LispE* lisp);
    Element* evall_heap(LispE* lisp);
    Element* evall_throw(LispE* lisp);
    Element* evall_trace(LispE* lisp);
    Element* evall_transpose(LispE* lisp);
    Element* evall_trigger(LispE* lisp);
    Element* evall_type(LispE* lisp);
    Element* evall_unique(LispE* lisp);
    Element* evall_use(LispE* lisp);
    Element* evall_values(LispE* lisp);
    Element* evall_void(LispE* lisp);
    Element* evall_wait(LispE* lisp);
    Element* evall_waiton(LispE* lisp);
    Element* evall_while(LispE* lisp);
    Element* evall_xor(LispE* lisp);
    Element* evall_zerop(LispE* lisp);
    Element* evall_zip(LispE* lisp);
    Element* evall_zipwith(LispE* lisp);
    Element* evalt_list(LispE* lisp);
    
    Element* evalt_call(LispE* lisp) {
        return liste[0]->eval(lisp);
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
    
    
    //Composable functions
    Element* evall_map_cps(LispE*);
    Element* evall_filter_cps(LispE*);
    Element* evall_take_cps(LispE*);
    Element* evall_repeat_cps(LispE*);
    Element* evall_cycle_cps(LispE*);
    Element* evall_drop_cps(LispE*);
    Element* evall_takewhile_cps(LispE*);
    Element* evall_dropwhile_cps(LispE*);
    Element* evall_for_cps(LispE*);
    Element* evall_foldl_cps(LispE*);
    Element* evall_scanl_cps(LispE*);
    Element* evall_foldr_cps(LispE*);
    Element* evall_scanr_cps(LispE*);
    Element* evall_foldl1_cps(LispE*);
    Element* evall_scanl1_cps(LispE*);
    Element* evall_foldr1_cps(LispE*);
    Element* evall_scanr1_cps(LispE*);
    
    
#ifdef MACDEBUG
    Element* evall_debug_function(LispE* lisp);
#endif
    
    bool eval_Boolean(LispE* lisp, int16_t instruction);
    
    inline Element* evalt_function(LispE* lisp) {
        //In this case, it must be a function call (t_function)
        return eval_function(lisp, (List*)((Atomefonction*)liste[0])->body);
    }
    
    inline Element* evalt_library_function(LispE* lisp) {
        //In this case, it must be a function call (t_function)
        return eval_library_function(lisp, (List*)((Atomefonction*)liste[0])->body);
    }
    
    inline Element* evalt_pattern(LispE* lisp) {
        //In this case, it must be a pattern function call (t_pattern)
        return eval_pattern(lisp, ((Atomefonction*)liste[0])->function_label);
    }
    
    inline Element* evalt_lambda(LispE* lisp) {
        //In this case, it must be a self call (t_self)
        return eval_lambda(lisp, (List*)((Atomefonction*)liste[0])->body);
    }
    
    inline Element* evalt_thread(LispE* lisp) {
        //In this case, it must be a self call (t_self)
        return eval_thread(lisp, (List*)((Atomefonction*)liste[0])->body);
    }
    
    inline Element* evalt_data(LispE* lisp) {
        //In this case, it must be a self call (t_self)
        return eval_data(lisp, ((Atomefonction*)liste[0])->body);
    }
    
    
    virtual Element* newInstance() {
        return new List;
    }
    
    virtual Element* newInstance(Element* v);
    virtual bool incode() {
        return false;
    }
    
};

//-------------------------------------------------------
// Pool of lists
//-------------------------------------------------------

class Listpool : public List {
public:
    LispE* lisp;
    
    Listpool(LispE* l) : lisp(l) {}
    Listpool(LispE* lsp, List* l, long p) : lisp(lsp), List(l, p) {}
    
    void decrementstatus(uint16_t nb);
    void decrement();
    
    void release();
    void release(Element* e);
    void rawrelease();
    Element* newInstance();
    Element* fullcopy();
    Element* copyatom(LispE* lisp, uint16_t s);
    Element* copying(bool duplicate = true);
    
};

//-------------------------------------------------------
// Pattern function arguments
//-------------------------------------------------------

class Listargumentquote : public List {
public:
    Listargumentquote(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    
    int16_t label() {
        return liste[1]->label();
    }
    
    char check_match(LispE* lisp, Element* value) {
        return check_ok;
    }
    
    bool isList() {
        return false;
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listkleene : public List {
public:
    Element* argument;
    Element* variable;
    int16_t action;
    
    Listkleene(Element* l, Element* v, int16_t a) : action(a), argument(l), variable(v) {}
    
    bool unify_kleene(LispE* lisp, Element* value, Element* current, long& i, long& r, bool record);
};

class Listargumentdata : public List {
public:
    
    Listargumentdata(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listseparator : public List {
public:
    
    Listseparator(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listargumentset : public List {
public:
    
    Listargumentset(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    
    bool isList() {
        return false;
    }
    int16_t label() {
        switch (liste[0]->type) {
            case l_set:
                return t_set;
            case l_sets:
                return t_sets;
            case l_setn:
                return t_setn;
            default:
                return t_seti;
        }
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Argumentdictionary : public Element {
public:
    Dictionary_as_list d;
    Argumentdictionary(LispE* lisp, List* l) : d(lisp, l), Element(t_dictionary) {}
    
    bool unify(LispE* lisp, Element* value, bool record) {
        return d.unify(lisp, value, record);
    }
};


class Listargumentlabel : public List {
public:
    int16_t ilabel;
    
    Listargumentlabel(List* l, int16_t lab) : ilabel(lab), List(l, 0) {}
    
    bool unify(LispE* lisp, Element* value, bool record);
    
    bool isList() {
        return false;
    }
    
    int16_t label() {
        return ilabel;
    }
    
};

class Listargumentfunction : public List {
public:
    Element* argument;
    
    Listargumentfunction(List* l, Element* e) : argument(e), List(l, 0) {}
    bool unify(LispE* lisp, Element* value, bool record);
    bool isArgumentFunction() {
        return true;
    }
    
    Element* argumentvalue() {
        return argument;
    }
};

//-------------------------------------------------------
// Compiling code into Listincode objects
//-------------------------------------------------------

class Listincode : public List {
public:
    int idxinfo;
    bool multiple;
    
    Listincode(Listincode* l) : List(l, 0) {
        terminal = l->terminal;
        status = s_constant;
        idxinfo = l->idxinfo;
        multiple = l->multiple;
    }

    Listincode(Listincode* l, long i) : List(l, i) {
        terminal = l->terminal;
        status = s_constant;
        idxinfo = l->idxinfo;
        multiple = l->multiple;
    }

    Listincode(List* l) : List(l, 0) {
        terminal = l->terminal;
        status = s_constant;
        idxinfo = -1;
        multiple = l->isMultiple();
    }
    
    Listincode(int idx) : List(s_constant) {
        idxinfo = idx;
        multiple = false;
    }
    
    Listincode(uint16_t s) : List(s) {
        idxinfo = 0;
        multiple = false;
    }
    
    Listincode() : List(s_constant) {
        idxinfo = 0;
        multiple = false;
    }
    
    void copyfrom(Element* l) {
        throw new Error("Error: You cannot modify a static list");
    }
    
    bool isMultiple() {
        return multiple;
    }
    
    Element* eval(LispE*);
    Element* eval_infix(LispE* lisp);
    Element* eval_call_self(LispE* lisp);
    Element* eval_call_function(LispE* lisp);
    
    void incrementstatus(uint16_t nb) {}
    void decrementstatus(uint16_t nb) {}
    void increment() {}
    void decrement() {}
    
    void release() {}
    
    void force_release() {
        liste.decrement();
        delete this;
    }
    
    void protecting(bool protection, LispE* lisp);
    
    bool incode() {
        return true;
    }
    virtual List* cloning(Listincode* e, methodEval m);
    virtual List* cloning() {
        return new Listincode();
    }
    
    int infoIdx() {
        return idxinfo;
    }
};

class List_switch_eval : public Listincode {
public:
    std::unordered_map<u_ustring, List*> cases;
    List* default_value;
    List_switch_eval(Listincode* l) : Listincode(l) {
        default_value = NULL;
    }
    
    List_switch_eval() {}
    List_switch_eval(bool m)  {multiple = m;}
    
    //We traverse the structure, which should be of the form:
    //(switch action
    //("str" code)
    //(true code)
    void build(LispE* lisp);
    Element* eval(LispE*);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_switch_eval(e);
    }
    
    List* cloning() {
        return new List_switch_eval(multiple);
    }
};

class Listbreak : public Element {
public:
    
    Listbreak() : Element(l_return, s_constant) {}
    
    bool isBreak() {
        return true;
    }
    
    long size() {
        return 1;
    }
    
    Element* eval(LispE*) {
        return this;
    }
};


class Listreturn : public Element {
public:
    
    Listreturn() : Element(l_return, s_constant) {}
    
    long size() {
        return 1;
    }
    
    Element* eval(LispE* lisp);
};


class Listreturnelement : public Element {
public:
    Element* action;
    char terminal;
    
    Listreturnelement(List* l) : Element(l_return, s_constant) {
        terminal = l->terminal;
        action = l->liste[1];
    }
    
    void setterminal(char v = 1) {
        terminal |= v;
    }
    
    long size() {
        return 2;
    }
    
    Element* eval(LispE* lisp);
};

class Listlambda : public List {
public:
    
    Listlambda() : List() {}
    
    Element* eval(LispE* lisp) {
        return evalfunction(lisp, liste[0]->eval(lisp));
    }
};

class List_emptylist_eval : public Listincode {
public:
    
    List_emptylist_eval() {
        status = s_constant;
    }
    
    Element* eval(LispE* lisp);
    Element* duplicate_constant(LispE* lisp);
};


//-------------------------------------------------------
// Direct call to basic operations
//-------------------------------------------------------
class List_execute : public Listincode {
public:
    methodEval method;
    
    List_execute(Listincode* l, methodEval m) : method(m), Listincode(l) {}
    Element* eval(LispE* lisp);
};

class List_eval : public Listincode {
public:
    methodEval met;
    
    List_eval(LispE* lisp, Element* a);
    
    Element* eval(LispE* lisp);
};

class List_at_eval : public Listincode {
public:
    
    List_at_eval(Listincode* l) : Listincode(l) {}
    List_at_eval(List* l) : Listincode(l) {}
    List_at_eval() {}
    List_at_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_at_eval(e);
    }
    
    List* cloning() {
        return new List_at_eval(multiple);
    }
};

class List_over_eval : public Listincode {
public:
    
    List_over_eval(Listincode* l) : Listincode(l) {}
    List_over_eval(List* l) : Listincode(l) {}
    List_over_eval() {}
    List_over_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_over_eval(e);
    }
    
    List* cloning() {
        return new List_over_eval(multiple);
    }
};

class List_join_eval : public Listincode {
public:
    
    List_join_eval(Listincode* l) : Listincode(l) {}
    List_join_eval(List* l) : Listincode(l) {}
    
    List_join_eval() {}
    List_join_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_join_eval(e);
    }
    
    List* cloning() {
        return new List_join_eval(multiple);
    }
};

class List_insert_eval : public Listincode {
public:
    
    List_insert_eval(Listincode* l) : Listincode(l) {}
    List_insert_eval(List* l) : Listincode(l) {}
    List_insert_eval() {}
    List_insert_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_insert_eval(e);
    }
    
    List* cloning() {
        return new List_insert_eval(multiple);
    }
};

class List_keys_eval : public Listincode {
public:
    
    List_keys_eval(Listincode* l) : Listincode(l) {}
    List_keys_eval(List* l) : Listincode(l) {}
    List_keys_eval() {}
    List_keys_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_keys_eval(e);
    }
    
    List* cloning() {
        return new List_keys_eval(multiple);
    }
    
};

class List_zip_eval : public Listincode {
public:
    
    List_zip_eval(Listincode* l) : Listincode(l) {}
    List_zip_eval(List* l) : Listincode(l) {}
    List_zip_eval() {}
    List_zip_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_zip_eval(e);
    }
    
    List* cloning() {
        return new List_zip_eval(multiple);
    }
    
};



class List_label_eval : public Listincode {
public:
    
    List_label_eval(Listincode* l) : Listincode(l) {}
    List_label_eval(List* l) : Listincode(l) {}
    List_label_eval() {}
    List_label_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_label_eval(e);
    }
    
    List* cloning() {
        return new List_label_eval(multiple);
    }
    
};



class List_infix_eval : public Listincode {
public:
    
    List_infix_eval(Listincode* l) : Listincode(l) {}
    List_infix_eval(List* l) : Listincode(l) {}
    List_infix_eval() {}
    List_infix_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_infix_eval(e);
    }
    
    List* cloning() {
        return new List_infix_eval(multiple);
    }
    
};

class List_extract_eval : public Listincode {
public:
    
    List_extract_eval(Listincode* l) : Listincode(l) {}
    List_extract_eval(List* l) : Listincode(l) {}
    List_extract_eval() {}
    List_extract_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_extract_eval(e);
    }
    
    List* cloning() {
        return new List_extract_eval(multiple);
    }
    
};

class List_set_shape_eval : public Listincode {
public:
    
    List_set_shape_eval(Listincode* l) : Listincode(l) {}
    List_set_shape_eval(List* l) : Listincode(l) {}
    List_set_shape_eval() {}
    List_set_shape_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_set_shape_eval(e);
    }
    
    List* cloning() {
        return new List_set_shape_eval(multiple);
    }
    
};

class List_at_shape_eval : public Listincode {
public:
    
    List_at_shape_eval(Listincode* l) : Listincode(l) {}
    List_at_shape_eval(List* l) : Listincode(l) {}
    List_at_shape_eval() {}
    List_at_shape_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_at_shape_eval(e);
    }
    
    List* cloning() {
        return new List_at_shape_eval(multiple);
    }
    
};

class List_set_range_eval : public Listincode {
public:
    
    List_set_range_eval(Listincode* l) : Listincode(l) {}
    List_set_range_eval() {}
    List_set_range_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_set_range_eval(e);
    }
    
    List* cloning() {
        return new List_set_range_eval(multiple);
    }
    
};

class List_flatten_eval : public Listincode {
public:
    
    List_flatten_eval(Listincode* l) : Listincode(l) {}
    List_flatten_eval(List* l) : Listincode(l) {}
    List_flatten_eval() {}
    List_flatten_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_flatten_eval(e);
    }
    
    List* cloning() {
        return new List_flatten_eval(multiple);
    }
    
};

class List_different_eval : public Listincode {
public:
    
    List_different_eval(Listincode* l) : Listincode(l) {}
    List_different_eval(List* l) : Listincode(l) {}
    List_different_eval() {}
    List_different_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_different_eval(e);
    }
    
    List* cloning() {
        return new List_different_eval(multiple);
    }
    
};

class List_equal_eval : public Listincode {
public:
    
    List_equal_eval(Listincode* l) : Listincode(l) {}
    List_equal_eval(List* l) : Listincode(l) {}
    List_equal_eval() {}
    List_equal_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_equal_eval(e);
    }
    
    List* cloning() {
        return new List_equal_eval(multiple);
    }
    
};

class List_flip_eval : public Listincode {
public:
    
    List_flip_eval(Listincode* l) : Listincode(l) {}
    List_flip_eval(List* l) : Listincode(l) {}
    List_flip_eval() {}
    List_flip_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_flip_eval(e);
    }
    
    List* cloning() {
        return new List_flip_eval(multiple);
    }
    
};

class List_block_eval : public Listincode {
public:
    List_block_eval(Listincode* l) : Listincode(l) {
    }
    
    List_block_eval() {}
    List_block_eval(bool m)  {multiple = m;}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_block_eval(e);
    }
    
    List* cloning() {
        return new List_block_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_set_at_eval : public Listincode {
public:
    
    List_set_at_eval(Listincode* l) : Listincode(l) {}
    List_set_at_eval(List* l) : Listincode(l) {}
    
    List_set_at_eval() {}
    List_set_at_eval(bool m)  {multiple = m;}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_set_at_eval(e);
    }
    
    List* cloning() {
        return new List_set_at_eval(multiple);
    }
    
    
    Element* eval(LispE* lisp);
};

class List_car_eval : public Listincode {
public:
    
    List_car_eval(Listincode* l) : Listincode(l) {}
    List_car_eval(List* l) : Listincode(l) {}
    List_car_eval() {}
    List_car_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_car_eval(e);
    }
    
    List* cloning() {
        return new List_car_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_cdr_eval : public Listincode {
public:
    
    List_cdr_eval(Listincode* l) : Listincode(l) {}
    List_cdr_eval(List* l) : Listincode(l) {}
    List_cdr_eval() {}
    List_cdr_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_cdr_eval(e);
    }
    
    List* cloning() {
        return new List_cdr_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_cadr_eval : public Listincode {
public:
    
    List_cadr_eval(Listincode* l) : Listincode(l) {}
    List_cadr_eval(List* l) : Listincode(l) {}
    List_cadr_eval() {}
    List_cadr_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_cadr_eval(e);
    }
    
    List* cloning() {
        return new List_cadr_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_bytes_eval : public Listincode {
public:
    
    List_bytes_eval(Listincode* l) : Listincode(l) {}
    List_bytes_eval(List* l) : Listincode(l) {}
    List_bytes_eval() {}
    List_bytes_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bytes_eval(e);
    }
    
    List* cloning() {
        return new List_bytes_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_atomp_eval : public Listincode {
public:
    List_atomp_eval(Listincode* l) : Listincode(l) {}
    List_atomp_eval() {}
    List_atomp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_atomp_eval(e);
    }
    
    List* cloning() {
        return new List_atomp_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_numberp_eval : public Listincode {
public:
    List_numberp_eval(Listincode* l) : Listincode(l) {}
    List_numberp_eval() {}
    List_numberp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_numberp_eval(e);
    }
    
    List* cloning() {
        return new List_numberp_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_consp_eval : public Listincode {
public:
    List_consp_eval(Listincode* l) : Listincode(l) {}
    List_consp_eval() {}
    List_consp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_consp_eval(e);
    }
    
    List* cloning() {
        return new List_consp_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_emptyp_eval : public Listincode {
public:
    List_emptyp_eval(Listincode* l) : Listincode(l) {}
    List_emptyp_eval() {}
    List_emptyp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_emptyp_eval(e);
    }
    
    List* cloning() {
        return new List_emptyp_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_zerop_eval : public Listincode {
public:
    List_zerop_eval(Listincode* l) : Listincode(l) {}
    List_zerop_eval() {}
    List_zerop_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_zerop_eval(e);
    }
    
    List* cloning() {
        return new List_zerop_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_nullp_eval : public Listincode {
public:
    List_nullp_eval(Listincode* l) : Listincode(l) {}
    List_nullp_eval() {}
    List_nullp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_nullp_eval(e);
    }
    
    List* cloning() {
        return new List_nullp_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_stringp_eval : public Listincode {
public:
    List_stringp_eval(Listincode* l) : Listincode(l) {}
    List_stringp_eval() {}
    List_stringp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_stringp_eval(e);
    }
    
    List* cloning() {
        return new List_stringp_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_push_eval : public Listincode {
public:
    
    List_push_eval(Listincode* l) : Listincode(l) {}
    List_push_eval(List* l) : Listincode(l) {}
    List_push_eval() {}
    List_push_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_push_eval(e);
    }
    
    List* cloning() {
        return new List_push_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_pushtrue_eval : public Listincode {
public:
    
    List_pushtrue_eval(Listincode* l) : Listincode(l) {}
    List_pushtrue_eval(List* l) : Listincode(l) {}
    List_pushtrue_eval() {}
    List_pushtrue_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_pushtrue_eval(e);
    }
    
    List* cloning() {
        return new List_pushtrue_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_pushfirst_eval : public Listincode {
public:
    
    List_pushfirst_eval(Listincode* l) : Listincode(l) {}
    List_pushfirst_eval(List* l) : Listincode(l) {}
    List_pushfirst_eval() {}
    List_pushfirst_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_pushfirst_eval(e);
    }
    
    List* cloning() {
        return new List_pushfirst_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_pushlast_eval : public Listincode {
public:
    
    List_pushlast_eval(Listincode* l) : Listincode(l) {}
    List_pushlast_eval(List* l) : Listincode(l) {}
    List_pushlast_eval() {}
    List_pushlast_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_pushlast_eval(e);
    }
    
    List* cloning() {
        return new List_pushlast_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_print_eval : public Listincode {
public:
    
    List_print_eval(Listincode* l) : Listincode(l) {}
    List_print_eval(List* l) : Listincode(l) {}
    List_print_eval() {}
    List_print_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_print_eval(e);
    }
    
    List* cloning() {
        return new List_print_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_printerr_eval : public Listincode {
public:
    
    List_printerr_eval(Listincode* l) : Listincode(l) {}
    List_printerr_eval(List* l) : Listincode(l) {}
    List_printerr_eval() {}
    List_printerr_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_printerr_eval(e);
    }
    
    List* cloning() {
        return new List_printerr_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_println_eval : public Listincode {
public:
    
    List_println_eval(Listincode* l) : Listincode(l) {}
    List_println_eval(List* l) : Listincode(l) {}
    List_println_eval() {}
    List_println_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_println_eval(e);
    }
    
    List* cloning() {
        return new List_println_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_printerrln_eval : public Listincode {
public:
    
    List_printerrln_eval(Listincode* l) : Listincode(l) {}
    List_printerrln_eval(List* l) : Listincode(l) {}
    List_printerrln_eval() {}
    List_printerrln_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_printerrln_eval(e);
    }
    
    List* cloning() {
        return new List_printerrln_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_pop_eval : public Listincode {
public:
    
    List_pop_eval(Listincode* l) : Listincode(l) {}
    List_pop_eval(List* l) : Listincode(l) {}
    List_pop_eval() {}
    List_pop_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_pop_eval(e);
    }
    
    List* cloning() {
        return new List_pop_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_popfirst_eval : public Listincode {
public:
    
    List_popfirst_eval(Listincode* l) : Listincode(l) {}
    List_popfirst_eval(List* l) : Listincode(l) {}
    List_popfirst_eval() {}
    List_popfirst_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_popfirst_eval(e);
    }
    
    List* cloning() {
        return new List_popfirst_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_poplast_eval : public Listincode {
public:
    
    List_poplast_eval(Listincode* l) : Listincode(l) {}
    List_poplast_eval(List* l) : Listincode(l) {}
    List_poplast_eval() {}
    List_poplast_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_poplast_eval(e);
    }
    
    List* cloning() {
        return new List_poplast_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_last_eval : public Listincode {
public:
    
    List_last_eval(Listincode* l) : Listincode(l) {}
    List_last_eval() {}
    List_last_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_last_eval(e);
    }
    
    List* cloning() {
        return new List_last_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_in_eval : public Listincode {
public:
    
    List_in_eval(Listincode* l) : Listincode(l) {}
    List_in_eval(List* l) : Listincode(l) {}
    List_in_eval() {}
    List_in_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_in_eval(e);
    }
    
    List* cloning() {
        return new List_in_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_and_eval : public Listincode {
public:
    
    List_and_eval(Listincode* l) : Listincode(l) {
    }
    
    List_and_eval(List* l) : Listincode(l) {
    }
    
    List_and_eval() {}
    List_and_eval(bool m) {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_and_eval(e);
    }
    
    List* cloning() {
        return new List_and_eval(multiple);
    }
    
    
    Element* eval(LispE* lisp);
};

class List_or_eval : public Listincode {
public:
    
    List_or_eval(Listincode* l) : Listincode(l) {
    }
    List_or_eval(List* l) : Listincode(l) {
    }
    List_or_eval() {}
    List_or_eval(bool m) {multiple = m;}

    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_or_eval(e);
    }
    
    List* cloning() {
        return new List_or_eval(multiple);
    }
    

    Element* eval(LispE* lisp);
};

class List_set_eval : public Listincode {
public:
    
    List_set_eval(Listincode* l) : Listincode(l) {}
    List_set_eval(List* l) : Listincode(l) {}
    List_set_eval() {}
    List_set_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_set_eval(e);
    }
    
    List* cloning() {
        return new List_set_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_sets_eval : public Listincode {
public:
    
    List_sets_eval(Listincode* l) : Listincode(l) {}
    List_sets_eval(List* l) : Listincode(l) {}
    List_sets_eval() {}
    List_sets_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_sets_eval(e);
    }
    
    List* cloning() {
        return new List_sets_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_seti_eval : public Listincode {
public:
    
    List_seti_eval(Listincode* l) : Listincode(l) {}
    List_seti_eval(List* l) : Listincode(l) {}
    List_seti_eval() {}
    List_seti_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_seti_eval(e);
    }
    
    List* cloning() {
        return new List_seti_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_setn_eval : public Listincode {
public:
    
    List_setn_eval(Listincode* l) : Listincode(l) {}
    List_setn_eval(List* l) : Listincode(l) {}
    List_setn_eval() {}
    List_setn_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_setn_eval(e);
    }
    
    List* cloning() {
        return new List_setn_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_check_eval : public Listincode {
public:
    
    List_check_eval(Listincode* l) : Listincode(l) {}
    List_check_eval(List* l) : Listincode(l) {}
    List_check_eval() {}
    List_check_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_check_eval(e);
    }
    
    List* cloning() {
        return new List_check_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_ncheck_eval : public Listincode {
public:
    
    List_ncheck_eval(Listincode* l) : Listincode(l) {}
    List_ncheck_eval(List* l) : Listincode(l) {}
    List_ncheck_eval() {}
    List_ncheck_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_ncheck_eval(e);
    }
    
    List* cloning() {
        return new List_ncheck_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_cons_eval : public Listincode {
public:
    
    List_cons_eval(Listincode* l) : Listincode(l) {}
    List_cons_eval(List* l) : Listincode(l) {}
    List_cons_eval() {}
    List_cons_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_cons_eval(e);
    }
    
    List* cloning() {
        return new List_cons_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_consb_eval : public Listincode {
public:
    
    List_consb_eval(Listincode* l) : Listincode(l) {}
    List_consb_eval(List* l) : Listincode(l) {}
    List_consb_eval() {}
    List_consb_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_consb_eval(e);
    }
    
    List* cloning() {
        return new List_consb_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_clone_eval : public Listincode {
public:
    
    List_clone_eval(Listincode* l) : Listincode(l) {}
    List_clone_eval() {}
    List_clone_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_clone_eval(e);
    }
    
    List* cloning() {
        return new List_clone_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_list_eval : public Listincode {
public:
    
    List_list_eval(Listincode* l) : Listincode(l) {}
    List_list_eval(List* l) : Listincode(l) {}
    List_list_eval() {}
    List_list_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_list_eval(e);
    }
    
    List* cloning() {
        return new List_list_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_cond_eval : public Listincode {
public:
    List_cond_eval(Listincode* l) : Listincode(l) {}
    List_cond_eval(List* l) : Listincode(l) {}
    List_cond_eval() {}
    List_cond_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_cond_eval(e);
    }
    
    List* cloning() {
        return new List_cond_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_integer_eval : public Listincode {
public:
    
    List_integer_eval(Listincode* l) : Listincode(l) {}
    List_integer_eval() {}
    List_integer_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_integer_eval(e);
    }
    
    List* cloning() {
        return new List_integer_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_complex_eval : public Listincode {
public:
    
    List_complex_eval(Listincode* l) : Listincode(l) {}
    List_complex_eval() {}
    List_complex_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_complex_eval(e);
    }
    
    List* cloning() {
        return new List_complex_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_real_eval : public Listincode {
public:
    
    List_real_eval(Listincode* l) : Listincode(l) {}
    List_real_eval() {}
    List_real_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_real_eval(e);
    }
    
    List* cloning() {
        return new List_real_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_imaginary_eval : public Listincode {
public:
    
    List_imaginary_eval(Listincode* l) : Listincode(l) {}
    List_imaginary_eval() {}
    List_imaginary_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_imaginary_eval(e);
    }
    
    List* cloning() {
        return new List_imaginary_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_number_eval : public Listincode {
public:
    
    List_number_eval(Listincode* l) : Listincode(l) {}
    List_number_eval() {}
    List_number_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_number_eval(e);
    }
    
    List* cloning() {
        return new List_number_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_string_eval : public Listincode {
public:
    
    List_string_eval(Listincode* l) : Listincode(l) {}
    List_string_eval() {}
    List_string_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_string_eval(e);
    }
    
    List* cloning() {
        return new List_string_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_stringbyte_eval : public Listincode {
public:
    
    List_stringbyte_eval(Listincode* l) : Listincode(l) {}
    List_stringbyte_eval() {}
    List_stringbyte_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_stringbyte_eval(e);
    }
    
    List* cloning() {
        return new List_stringbyte_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_nconc_eval : public Listincode {
public:
    
    List_nconc_eval(Listincode* l) : Listincode(l) {}
    List_nconc_eval(List* l) : Listincode(l) {}
    List_nconc_eval() {}
    List_nconc_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_nconc_eval(e);
    }
    
    List* cloning() {
        return new List_nconc_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_nconcn_eval : public Listincode {
public:
    
    List_nconcn_eval(Listincode* l) : Listincode(l) {}
    List_nconcn_eval(List* l) : Listincode(l) {}
    List_nconcn_eval() {}
    List_nconcn_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_nconcn_eval(e);
    }
    
    List* cloning() {
        return new List_nconcn_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_min_eval : public Listincode {
public:
    
    List_min_eval(Listincode* l) : Listincode(l) {}
    List_min_eval(List* l) : Listincode(l) {}
    List_min_eval() {}
    List_min_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_min_eval(e);
    }
    
    List* cloning() {
        return new List_min_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_minmax_eval : public Listincode {
public:
    
    List_minmax_eval(Listincode* l) : Listincode(l) {}
    List_minmax_eval(List* l) : Listincode(l) {}
    List_minmax_eval() {}
    List_minmax_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_minmax_eval(e);
    }
    
    List* cloning() {
        return new List_minmax_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_max_eval : public Listincode {
public:
    
    List_max_eval(Listincode* l) : Listincode(l) {}
    List_max_eval(List* l) : Listincode(l) {}
    List_max_eval() {}
    List_max_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_max_eval(e);
    }
    
    List* cloning() {
        return new List_max_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_maybe_eval : public Listincode {
public:
    
    List_maybe_eval(Listincode* l) : Listincode(l) {}
    List_maybe_eval(List* l) : Listincode(l) {}
    List_maybe_eval() {}
    List_maybe_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_maybe_eval(e);
    }
    
    List* cloning() {
        return new List_maybe_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_integers_eval : public Listincode {
public:
    List_integers_eval(Listincode* l) : Listincode(l) {}
    List_integers_eval(List* l) : Listincode(l) {}
    List_integers_eval() {}
    List_integers_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_integers_eval(e);
    }
    
    List* cloning() {
        return new List_integers_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_numbers_eval : public Listincode {
public:
    
    List_numbers_eval(Listincode* l) : Listincode(l) {}
    List_numbers_eval(List* l) : Listincode(l) {}
    List_numbers_eval() {}
    List_numbers_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_numbers_eval(e);
    }
    
    List* cloning() {
        return new List_numbers_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_strings_eval : public Listincode {
public:
    
    List_strings_eval(Listincode* l) : Listincode(l) {}
    List_strings_eval(List* l) : Listincode(l) {}
    List_strings_eval() {}
    List_strings_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_strings_eval(e);
    }
    
    List* cloning() {
        return new List_strings_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_stringbytes_eval : public Listincode {
public:
    
    List_stringbytes_eval(Listincode* l) : Listincode(l) {}
    List_stringbytes_eval(List* l) : Listincode(l) {}
    List_stringbytes_eval() {}
    List_stringbytes_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_stringbytes_eval(e);
    }
    
    List* cloning() {
        return new List_stringbytes_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_key_eval : public Listincode {
public:
    
    List_key_eval(Listincode* l) : Listincode(l) {}
    List_key_eval(List* l) : Listincode(l) {}
    List_key_eval() {}
    List_key_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_key_eval(e);
    }
    
    List* cloning() {
        return new List_key_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_keyi_eval : public Listincode {
public:
    
    List_keyi_eval(Listincode* l) : Listincode(l) {}
    List_keyi_eval(List* l) : Listincode(l) {}
    List_keyi_eval() {}
    List_keyi_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_keyi_eval(e);
    }
    
    List* cloning() {
        return new List_keyi_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_keyn_eval : public Listincode {
public:
    
    List_keyn_eval(Listincode* l) : Listincode(l) {}
    List_keyn_eval(List* l) : Listincode(l) {}
    List_keyn_eval() {}
    List_keyn_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_keyn_eval(e);
    }
    
    List* cloning() {
        return new List_keyn_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_compare_eval : public Listincode {
public:
    
    List_compare_eval(Listincode* l) : Listincode(l) {}
    List_compare_eval(List* l) : Listincode(l) {}
    List_compare_eval() {}
    List_compare_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_compare_eval(e);
    }
    
    List* cloning() {
        return new List_compare_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_sort_eval : public Listincode {
public:
    
    List_sort_eval(Listincode* l) : Listincode(l) {}
    List_sort_eval(List* l) : Listincode(l) {}
    List_sort_eval() {}
    List_sort_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_sort_eval(e);
    }
    
    List* cloning() {
        return new List_sort_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_xor_eval : public Listincode {
public:
    
    List_xor_eval(Listincode* l) : Listincode(l) {}
    List_xor_eval(List* l) : Listincode(l) {}
    List_xor_eval() {}
    List_xor_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_xor_eval(e);
    }
    
    List* cloning() {
        return new List_xor_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_type_eval : public Listincode {
public:
    
    List_type_eval(Listincode* l) : Listincode(l) {}
    List_type_eval(List* l) : Listincode(l) {}
    List_type_eval() {}
    List_type_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_type_eval(e);
    }
    
    List* cloning() {
        return new List_type_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_rotate_eval : public Listincode {
public:
    
    List_rotate_eval(Listincode* l) : Listincode(l) {}
    List_rotate_eval(List* l) : Listincode(l) {}
    List_rotate_eval() {}
    List_rotate_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_rotate_eval(e);
    }
    
    List* cloning() {
        return new List_rotate_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_unique_eval : public Listincode {
public:
    
    List_unique_eval(Listincode* l) : Listincode(l) {}
    List_unique_eval(List* l) : Listincode(l) {}
    List_unique_eval() {}
    List_unique_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_unique_eval(e);
    }
    
    List* cloning() {
        return new List_unique_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_values_eval : public Listincode {
public:
    
    List_values_eval(Listincode* l) : Listincode(l) {}
    List_values_eval(List* l) : Listincode(l) {}
    List_values_eval() {}
    List_values_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_values_eval(e);
    }
    
    List* cloning() {
        return new List_values_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};



class List_extend_eval : public Listincode {
public:
    
    List_extend_eval(Listincode* l) : Listincode(l) {}
    List_extend_eval(List* l) : Listincode(l) {}
    List_extend_eval() {}
    List_extend_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_extend_eval(e);
    }
    
    List* cloning() {
        return new List_extend_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_lambda_eval : public Listincode {
public:
    List* parameters;
    vecte<int16_t> labels;
    int16_t nbinstructions;
    
    List_lambda_eval(Listincode* l) : Listincode(l) {
        type = t_call_lambda;
        status = s_constant;
        //the third element is the argument list .
        //we need our body to be the same number
        parameters = (List*)liste[1];
        nbinstructions = size();
        for (long i = 0; i < parameters->size(); i++) {
            labels.push_back(parameters->liste[i]->label());
        }
    }

    List_lambda_eval(List* l) : Listincode(l) {
        type = t_call_lambda;
        status = s_constant;
        //the third element is the argument list .
        //we need our body to be the same number
        parameters = (List*)liste[1];
        nbinstructions = size();
        for (long i = 0; i < parameters->size(); i++) {
            labels.push_back(parameters->liste[i]->label());
        }
    }

    Element* eval_lambda_min(LispE*);
    
    bool is_straight_eval() {
        return true;
    }

    Element* duplicate_constant(LispE* lisp) {
        return this;
    }
    
    Element* eval(LispE* lisp) {
        return this;
    }
    
    List_lambda_eval() {
        parameters = NULL;
    }

    List_lambda_eval(bool m) {
        parameters = NULL;
        multiple = m;
    }

    int16_t label() {
        return t_call_lambda;
    }

    List* cloning() {
        return new List_lambda_eval(multiple);
    }

    List* cloning(Listincode* e, methodEval m) {
        return new List_lambda_eval(e);
    }

    Element* reduce(LispE* lisp, Element* l1, long sz);
    Element* backreduce(LispE* lisp, Element* l1, long sz);
    Element* scan(LispE* lisp, Element* l1, long sz);
    Element* backscan(LispE* lisp, Element* l1, long sz);


};

class List_call_lambda : public Listincode {
public:
    List_lambda_eval* body;
    long nbarguments;

    List_call_lambda()  {
        body = NULL;
        nbarguments = 0;
    }

    List_call_lambda(LispE* lisp, Listincode* l);
    
    bool is_straight_eval() {
        return true;
    }

    List* cloning() {
        return new List_call_lambda();
    }

    Element* eval(LispE*);
    
    //We define these methods to handle the creation of a full-fledge List_call_lambda
    //The next two methods are actually called in apply
    void append(Element* b) {
        //The first element is the lambda itself
        if (body == NULL)
            body = (List_lambda_eval*)b;
        else
            nbarguments++;
        List::append(b);
    }
    
    void extend(List* l) {
        nbarguments = l->size();
        if (nbarguments != body->parameters->size()) {
            wstring message = L"Error: Wrong number of arguments in: 'lambda'";
            throw new Error(message);
        }
        List::extend(l);
    }
};

class List_zipwith_lambda_eval : public Listincode {
public:
    List_lambda_eval* lambda_e;
    vecte<int16_t> params;
    bool choose;
    bool del;
    
    List_zipwith_lambda_eval(Listincode* l) : Listincode(l) {
        choose = true;
        if (liste.back()->type == v_null) {
            choose = liste.back()->Boolean();
        }
        Element* function = liste[1];
        if (function->type == t_call_lambda) {
            lambda_e = (List_lambda_eval*)function;
            del = false;
        }
        else {
            lambda_e = new List_lambda_eval((List*)function);
            del = true;
        }
         params = lambda_e->labels;
    }
    
    List_zipwith_lambda_eval(List* l) : Listincode(l) {
        choose = true;
        if (liste.back()->type == v_null) {
            choose = liste.back()->Boolean();
        }
        Element* function = liste[1];
        if (function->type == t_call_lambda) {
            lambda_e = (List_lambda_eval*)function;
            del = false;
        }
        else {
            lambda_e = new List_lambda_eval((List*)function);
            del = true;
        }
         params = lambda_e->labels;
    }
    
    List_zipwith_lambda_eval() {
        del = false;
    }

    List_zipwith_lambda_eval(bool m) {
        del = false;
        multiple = m;
    }

    ~List_zipwith_lambda_eval() {
        if (del)
            delete lambda_e;
    }
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_zipwith_lambda_eval(e);
    }
    
    List* cloning() {
        return new List_zipwith_lambda_eval(multiple);
    }
  

};

class List_zipwith_eval : public Listincode {
public:
    bool choose;
    
    List_zipwith_eval(Listincode* l) : Listincode(l) {
        choose = true;
        if (liste.back()->type == v_null) {
            choose = liste.back()->Boolean();
        }
    }
    List_zipwith_eval(List* l) : Listincode(l) {
        choose = true;
        if (liste.back()->type == v_null) {
            choose = liste.back()->Boolean();
        }
    }
    
    List_zipwith_eval() {}
    List_zipwith_eval(bool m)  {multiple = m;}
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_zipwith_eval(e);
    }
    
    List* cloning() {
        return new List_zipwith_eval(multiple);
    }
    

};

class List_function_eval : public Listincode {
public:
    List* body;
    Element* parameters;
    long nbarguments, defaultarguments;
    bool same;
    
    List_function_eval(LispE* lisp, Listincode* l, List* b);
    List_function_eval(LispE* lisp, List* b);
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }

    int16_t label() {
        return t_call;
    }
    
};

class List_pattern_eval : public Listincode {
public:
    List* body;
    int16_t function_label;
    
    List_pattern_eval(Listincode* l, List* b) : body(b), Listincode(l) {
        type = t_call;
        status = s_constant;
        function_label = body->liste[1]->label();
    }
    
    List_pattern_eval(List* b) : body(b) {
        liste.push_element(b);
        type = t_eval;
        status = s_constant;
        function_label = body->liste[1]->label();
    }
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }

    int16_t label() {
        return t_call;
    }
    
};

class List_library_eval : public Listincode {
public:
    List* body;
    List* parameters;
    long nbarguments;
    long defaultarguments;
    bool same;
    
    //the third element is the argument list .
    //we need our body to be the same number
    List_library_eval(Listincode* l, List* b) : body(b), Listincode(l) {
        type = t_call;
        status = s_constant;
        nbarguments = liste.size() - 1;
        parameters = (List*)body->liste[2];
        defaultarguments = parameters->argumentsize(nbarguments);
        same = (defaultarguments == parameters->size());
    }
    
    List_library_eval(List* b) : body(b), Listincode() {
        liste.push_element(b);
        type = t_eval;
        status = s_constant;
        nbarguments = 1;
        parameters = (List*)body->liste[2];
        defaultarguments = parameters->argumentsize(nbarguments);
        same = (defaultarguments == parameters->size());
    }
    
    Element* eval(LispE* lisp);
    
    bool is_straight_eval() {
        return true;
    }

    int16_t label() {
        return t_call;
    }
    
};

class List_not_eval : public Listincode {
public:
    
    List_not_eval(Listincode* l) : Listincode(l) {}
    List_not_eval(List* l) : Listincode(l) {}
    List_not_eval() {}
    List_not_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_not_eval(e);
    }
    
    List* cloning() {
        return new List_not_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_setq_eval : public Listincode {
public:
    
    List_setq_eval(Listincode* l) : Listincode(l) {}
    List_setq_eval() {}
    List_setq_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_setq_eval(e);
    }
    
    List* cloning() {
        return new List_setq_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_setg_eval : public Listincode {
public:
    
    List_setg_eval(Listincode* l) : Listincode(l) {}
    List_setg_eval() {}
    List_setg_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_setg_eval(e);
    }
    
    List* cloning() {
        return new List_setg_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_seth_eval : public Listincode {
public:
    
    List_seth_eval(Listincode* l) : Listincode(l) {}
    List_seth_eval() {}
    List_seth_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_seth_eval(e);
    }
    
    List* cloning() {
        return new List_seth_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_set_const_eval : public Listincode {
public:
    
    List_set_const_eval(Listincode* l) : Listincode(l) {}
    List_set_const_eval() {}
    List_set_const_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_set_const_eval(e);
    }
    
    List* cloning() {
        return new List_set_const_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_let_eval : public Listincode {
public:
    
    List_let_eval(Listincode* l) : Listincode(l) {}
    List_let_eval() {}
    List_let_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_let_eval(e);
    }
    
    List* cloning() {
        return new List_let_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_select_eval : public Listincode {
public:
    
    List_select_eval(Listincode* l) : Listincode(l) {}
    List_select_eval(List* l) : Listincode(l) {}
    List_select_eval() {}
    List_select_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_select_eval(e);
    }
    
    List* cloning() {
        return new List_select_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_replicate_eval : public Listincode {
public:
    
    List_replicate_eval(Listincode* l) : Listincode(l) {}
    List_replicate_eval(List* l) : Listincode(l) {}
    List_replicate_eval() {}
    List_replicate_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_replicate_eval(e);
    }
    
    List* cloning() {
        return new List_replicate_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_reverse_eval : public Listincode {
public:
    
    List_reverse_eval(Listincode* l) : Listincode(l) {}
    List_reverse_eval(List* l) : Listincode(l) {}
    List_reverse_eval() {}
    List_reverse_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_reverse_eval(e);
    }
    
    List* cloning() {
        return new List_reverse_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_revertsearch_eval : public Listincode {
public:
    
    List_revertsearch_eval(Listincode* l) : Listincode(l) {}
    List_revertsearch_eval(List* l) : Listincode(l) {}
    List_revertsearch_eval() {}
    List_revertsearch_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_revertsearch_eval(e);
    }
    
    List* cloning() {
        return new List_revertsearch_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};



class List_search_eval : public Listincode {
public:
    
    List_search_eval(Listincode* l) : Listincode(l) {}
    List_search_eval(List* l) : Listincode(l) {}
    List_search_eval() {}
    List_search_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_search_eval(e);
    }
    
    List* cloning() {
        return new List_search_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_count_eval : public Listincode {
public:
    
    List_count_eval(Listincode* l) : Listincode(l) {}
    List_count_eval(List* l) : Listincode(l) {}
    List_count_eval() {}
    List_count_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_count_eval(e);
    }
    
    List* cloning() {
        return new List_count_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_searchall_eval : public Listincode {
public:
    
    List_searchall_eval(Listincode* l) : Listincode(l) {}
    List_searchall_eval(List* l) : Listincode(l) {}
    List_searchall_eval() {}
    List_searchall_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_searchall_eval(e);
    }
    
    List* cloning() {
        return new List_searchall_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_replaceall_eval : public Listincode {
public:
    
    List_replaceall_eval(Listincode* l) : Listincode(l) {}
    List_replaceall_eval(List* l) : Listincode(l) {}
    List_replaceall_eval() {}
    List_replaceall_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_replaceall_eval(e);
    }
    
    List* cloning() {
        return new List_replaceall_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_if_eval : public Listincode {
public:
    
    List_if_eval(Listincode* l) : Listincode(l) {}
    List_if_eval() {}
    List_if_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_if_eval(e);
    }
    
    List* cloning() {
        return new List_if_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_ife_eval : public Listincode {
public:
    
    List_ife_eval(List* l) : Listincode(l) {}
    List_ife_eval() {}
    List_ife_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_ife_eval(e);
    }
    
    List* cloning() {
        return new List_ife_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_dictionary_eval : public Listincode {
public:
    
    List_dictionary_eval(Listincode* l) : Listincode(l) {}
    List_dictionary_eval(List* l) : Listincode(l) {}
    List_dictionary_eval() {}
    List_dictionary_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_dictionary_eval(e);
    }
    
    List* cloning() {
        return new List_dictionary_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_dictionaryi_eval : public Listincode {
public:
    
    List_dictionaryi_eval(Listincode* l) : Listincode(l) {}
    List_dictionaryi_eval(List* l) : Listincode(l) {}
    List_dictionaryi_eval() {}
    List_dictionaryi_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_dictionaryi_eval(e);
    }
    
    List* cloning() {
        return new List_dictionaryi_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_dictionaryn_eval : public Listincode {
public:
    
    List_dictionaryn_eval(Listincode* l) : Listincode(l) {}
    List_dictionaryn_eval(List* l) : Listincode(l) {}
    List_dictionaryn_eval() {}
    List_dictionaryn_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_dictionaryn_eval(e);
    }
    
    List* cloning() {
        return new List_dictionaryn_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_loop_eval : public Listincode {
public:
    
    List_loop_eval(Listincode* l) : Listincode(l) {}
    List_loop_eval(List* l) : Listincode(l) {}
    List_loop_eval() {}
    List_loop_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_loop_eval(e);
    }
    
    List* cloning() {
        return new List_loop_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_quote_eval : public Listincode {
public:
    
    List_quote_eval(int idx_info) : Listincode(idx_info) {}
    
    Element* eval(LispE* lisp);
};

class List_maplist_lambda_eval : public Listincode {
public:
    long listesize;
    int16_t label;
    bool choice;
    
    List_maplist_lambda_eval(Listincode* l) : Listincode(l) {
        listesize = size();
        if (!liste[1]->index(1)->size())
            throw new Error("Error: Wrong number of arguments");
        label = liste[1]->index(1)->index(0)->label();
        if (label < l_final)
            throw new Error("Error: Wrong argument");
        choice = (liste[0]->label() == l_maplist);
    }
    
    List_maplist_lambda_eval(List* l) : Listincode(l) {
        listesize = size();
        if (!liste[1]->index(1)->size())
            throw new Error("Error: Wrong number of arguments");
        label = liste[1]->index(1)->index(0)->label();
        if (label < l_final)
            throw new Error("Error: Wrong argument");
        choice = (liste[0]->label() == l_maplist);
    }
    
    List_maplist_lambda_eval() {}
    List_maplist_lambda_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_maplist_lambda_eval(e);
    }
    
    List* cloning() {
        return new List_maplist_lambda_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_maplist_eval : public Listincode {
public:
    long listesize;
    bool choice;

    List_maplist_eval(Listincode* l) : Listincode(l) {
        listesize = size();
        choice = (liste[0]->label() == l_maplist);
    }
    
    List_maplist_eval(List* l) : Listincode(l) {
        listesize = size();
        choice = (liste[0]->label() == l_maplist);
    }
    
    List_maplist_eval() {}
    List_maplist_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_maplist_eval(e);
    }
    
    List* cloning() {
        return new List_maplist_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_droplist_eval : public Listincode {
public:
    
    List_droplist_eval(Listincode* l) : Listincode(l) {}
    List_droplist_eval(List* l) : Listincode(l) {}
    List_droplist_eval() {}
    List_droplist_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_droplist_eval(e);
    }
    
    List* cloning() {
        return new List_droplist_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_filterlist_eval : public Listincode {
public:
    
    List_filterlist_eval(Listincode* l) : Listincode(l) {}
    List_filterlist_eval(List* l) : Listincode(l) {}
    List_filterlist_eval() {}
    List_filterlist_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_filterlist_eval(e);
    }
    
    List* cloning() {
        return new List_filterlist_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_takelist_eval : public Listincode {
public:
    
    List_takelist_eval(Listincode* l) : Listincode(l) {}
    List_takelist_eval(List* l) : Listincode(l) {}
    List_takelist_eval() {}
    List_takelist_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_takelist_eval(e);
    }
    
    List* cloning() {
        return new List_takelist_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_takenb_eval : public Listincode {
public:
    
    List_takenb_eval(Listincode* l) : Listincode(l) {}
    List_takenb_eval(List* l) : Listincode(l) {}
    List_takenb_eval() {}
    List_takenb_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_takenb_eval(e);
    }
    
    List* cloning() {
        return new List_takenb_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_greater_eval : public Listincode {
public:
    
    List_greater_eval(List* l) : Listincode(l) {
    }
    List_greater_eval() {}
    List_greater_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_greater_eval(e);
    }
    
    List* cloning() {
        return new List_greater_eval(multiple);
    }
    

    Element* eval(LispE* lisp);
};

class List_greaterorequal_eval : public Listincode {
public:
    
    List_greaterorequal_eval(List* l): Listincode(l) {
    }
    List_greaterorequal_eval() {}
    List_greaterorequal_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_greaterorequal_eval(e);
    }
    
    List* cloning() {
        return new List_greaterorequal_eval(multiple);
    }
    

    Element* eval(LispE* lisp);
};

class List_lower_eval : public Listincode {
public:
    
    List_lower_eval(List* l) : Listincode(l) {
    }
    
    List_lower_eval() {}
    List_lower_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_lower_eval(e);
    }
    
    List* cloning() {
        return new List_lower_eval(multiple);
    }
    

    Element* eval(LispE* lisp);
};

class List_lowerorequal_eval : public Listincode {
public:
    
    List_lowerorequal_eval(List* l): Listincode(l) {
    }
    List_lowerorequal_eval() {}
    List_lowerorequal_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_lowerorequal_eval(e);
    }
    
    List* cloning() {
        return new List_lowerorequal_eval(multiple);
    }
    
    
    Element* eval(LispE* lisp);
};

class List_eq_eval : public Listincode {
public:
    
    List_eq_eval(List* l): Listincode(l) {}
    List_eq_eval() {}
    List_eq_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_eq_eval(e);
    }
    
    List* cloning() {
        return new List_eq_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_neq_eval : public Listincode {
public:
    
    List_neq_eval(List* l) : Listincode(l) {}
    List_neq_eval() {}
    List_neq_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_neq_eval(e);
    }
    
    List* cloning() {
        return new List_neq_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};

class List_while_eval : public Listincode {
public:
    
    List_while_eval(Listincode* l) : Listincode(l) {}
    List_while_eval(List* l) : Listincode(l) {}
    List_while_eval() {}
    List_while_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_while_eval(e);
    }
    
    List* cloning() {
        return new List_while_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_loopcount_eval : public Listincode {
public:
    
    List_loopcount_eval(Listincode* l) : Listincode(l) {}
    List_loopcount_eval(List* l) : Listincode(l) {}
    List_loopcount_eval() {}
    List_loopcount_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_loopcount_eval(e);
    }
    
    List* cloning() {
        return new List_loopcount_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_equalonezero_eval : public Listincode {
public:
    List_equalonezero_eval(Listincode* l) : Listincode(l) {}
    List_equalonezero_eval(List* l) : Listincode(l) {}
    List_equalonezero_eval() {}
    List_equalonezero_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_equalonezero_eval(e);
    }
    
    List* cloning() {
        return new List_equalonezero_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_scan_eval : public Listincode {
public:
    List_scan_eval(Listincode* l) : Listincode(l) {}
    List_scan_eval(List* l) : Listincode(l) {}
    List_scan_eval() {}
    List_scan_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_scan_eval(e);
    }
    
    List* cloning() {
        return new List_scan_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_backscan_eval : public Listincode {
public:
    List_backscan_eval(Listincode* l) : Listincode(l) {}
    List_backscan_eval(List* l) : Listincode(l) {}
    List_backscan_eval() {}
    List_backscan_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_backscan_eval(e);
    }
    
    List* cloning() {
        return new List_backscan_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_rho_eval : public Listincode {
public:
    List_rho_eval(Listincode* l) : Listincode(l) {}
    List_rho_eval(List* l) : Listincode(l) {}
    List_rho_eval() {}
    List_rho_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_rho_eval(e);
    }
    
    List* cloning() {
        return new List_rho_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_member_eval : public Listincode {
public:
    List_member_eval(Listincode* l) : Listincode(l) {}
    List_member_eval(List* l) : Listincode(l) {}
    List_member_eval() {}
    List_member_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_member_eval(e);
    }
    
    List* cloning() {
        return new List_member_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_backreduce_eval : public Listincode {
public:
    List_backreduce_eval(Listincode* l) : Listincode(l) {}
    List_backreduce_eval(List* l) : Listincode(l) {}
    List_backreduce_eval() {}
    List_backreduce_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_backreduce_eval(e);
    }
    
    List* cloning() {
        return new List_backreduce_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_concatenate_eval : public Listincode {
public:
    List_concatenate_eval(Listincode* l) : Listincode(l) {}
    List_concatenate_eval(List* l) : Listincode(l) {}
    List_concatenate_eval() {}
    List_concatenate_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_concatenate_eval(e);
    }
    
    List* cloning() {
        return new List_concatenate_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_reduce_eval : public Listincode {
public:
    List_reduce_eval(Listincode* l) : Listincode(l) {}
    List_reduce_eval(List* l) : Listincode(l) {}
    
    List_reduce_eval() {}
    List_reduce_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_reduce_eval(e);
    }
    
    List* cloning() {
        return new List_reduce_eval(multiple);
    }
    Element* eval(LispE*);
};


class List_rank_eval : public Listincode {
public:
    List_rank_eval(Listincode* l) : Listincode(l) {}
    List_rank_eval(List* l) : Listincode(l) {}
    List_rank_eval() {}
    List_rank_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_rank_eval(e);
    }
    
    List* cloning() {
        return new List_rank_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_irank_eval : public Listincode {
public:
    List_irank_eval(Listincode* l) : Listincode(l) {}
    List_irank_eval(List* l) : Listincode(l) {}
    List_irank_eval() {}
    List_irank_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_irank_eval(e);
    }
    
    List* cloning() {
        return new List_irank_eval(multiple);
    }
    Element* eval(LispE*);
};


class List_innerproduct_eval : public Listincode {
public:
    List_innerproduct_eval(Listincode* l) : Listincode(l) {}
    List_innerproduct_eval(List* l) : Listincode(l) {}
    List_innerproduct_eval() {}
    List_innerproduct_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_innerproduct_eval(e);
    }
    
    List* cloning() {
        return new List_innerproduct_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_outerproduct_eval : public Listincode {
public:
    List_outerproduct_eval(Listincode* l) : Listincode(l) {}
    List_outerproduct_eval(List* l) : Listincode(l) {}
    List_outerproduct_eval() {}
    List_outerproduct_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_outerproduct_eval(e);
    }
    
    List* cloning() {
        return new List_outerproduct_eval(multiple);
    }
    Element* eval(LispE*);
};


class List_stringf_eval : public Listincode {
public:
    List_stringf_eval(Listincode* l) : Listincode(l) {}
    List_stringf_eval(List* l) : Listincode(l) {}
    List_stringf_eval() {}
    List_stringf_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_stringf_eval(e);
    }
    
    List* cloning() {
        return new List_stringf_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_iota_eval : public Listincode {
public:
    List_iota_eval(Listincode* l) : Listincode(l) {}
    List_iota_eval(List* l) : Listincode(l) {}
    List_iota_eval() {}
    List_iota_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_iota_eval(e);
    }
    
    List* cloning() {
        return new List_iota_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_iota0_eval : public Listincode {
public:
    List_iota0_eval(Listincode* l) : Listincode(l) {}
    List_iota0_eval(List* l) : Listincode(l) {}
    List_iota0_eval() {}
    List_iota0_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_iota0_eval(e);
    }
    
    List* cloning() {
        return new List_iota0_eval(multiple);
    }
    Element* eval(LispE*);
};

class List_andvalue_eval : public Listincode {
public:
    
    List_andvalue_eval(Listincode* l) : Listincode(l) {
    }
    List_andvalue_eval(List* l) : Listincode(l) {
    }
    List_andvalue_eval() {}
    List_andvalue_eval(bool m) {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_andvalue_eval(e);
    }
    
    List* cloning() {
        return new List_andvalue_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_apply_eval : public Listincode {
public:
    
    List_apply_eval(Listincode* l) : Listincode(l) {}
    List_apply_eval(List* l) : Listincode(l) {}
    List_apply_eval() {}
    List_apply_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_apply_eval(e);
    }
    
    List* cloning() {
        return new List_apply_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_atomise_eval : public Listincode {
public:
    
    List_atomise_eval(Listincode* l) : Listincode(l) {}
    List_atomise_eval(List* l) : Listincode(l) {}
    List_atomise_eval() {}
    List_atomise_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_atomise_eval(e);
    }
    
    List* cloning() {
        return new List_atomise_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_converttoatom_eval : public Listincode {
public:
    
    List_converttoatom_eval(Listincode* l) : Listincode(l) {}
    List_converttoatom_eval(List* l) : Listincode(l) {}
    List_converttoatom_eval() {}
    List_converttoatom_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_converttoatom_eval(e);
    }
    
    List* cloning() {
        return new List_converttoatom_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_bodies_eval : public Listincode {
public:
    
    List_bodies_eval(Listincode* l) : Listincode(l) {}
    List_bodies_eval(List* l) : Listincode(l) {}
    List_bodies_eval() {}
    List_bodies_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bodies_eval(e);
    }
    
    List* cloning() {
        return new List_bodies_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_catch_eval : public Listincode {
public:
    
    List_catch_eval(Listincode* l) : Listincode(l) {}
    List_catch_eval(List* l) : Listincode(l) {}
    List_catch_eval() {}
    List_catch_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_catch_eval(e);
    }
    
    List* cloning() {
        return new List_catch_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_cyclicp_eval : public Listincode {
public:
    
    List_cyclicp_eval(Listincode* l) : Listincode(l) {}
    List_cyclicp_eval(List* l) : Listincode(l) {}
    List_cyclicp_eval() {}
    List_cyclicp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_cyclicp_eval(e);
    }
    
    List* cloning() {
        return new List_cyclicp_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_converttoshort_eval : public Listincode {
public:
    
    List_converttoshort_eval(Listincode* l) : Listincode(l) {}
    List_converttoshort_eval(List* l) : Listincode(l) {}
    List_converttoshort_eval() {}
    List_converttoshort_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_converttoshort_eval(e);
    }
    
    List* cloning() {
        return new List_converttoshort_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_converttofloat_eval : public Listincode {
public:
    
    List_converttofloat_eval(Listincode* l) : Listincode(l) {}
    List_converttofloat_eval(List* l) : Listincode(l) {}
    List_converttofloat_eval() {}
    List_converttofloat_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_converttofloat_eval(e);
    }
    
    List* cloning() {
        return new List_converttofloat_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_elapse_eval : public Listincode {
public:
    
    List_elapse_eval(Listincode* l) : Listincode(l) {}
    List_elapse_eval(List* l) : Listincode(l) {}
    List_elapse_eval() {}
    List_elapse_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_elapse_eval(e);
    }
    
    List* cloning() {
        return new List_elapse_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_eval_eval : public Listincode {
public:
    
    List_eval_eval(Listincode* l) : Listincode(l) {}
    List_eval_eval(List* l) : Listincode(l) {}
    List_eval_eval() {}
    List_eval_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_eval_eval(e);
    }
    
    List* cloning() {
        return new List_eval_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_slice_eval : public Listincode {
public:
    
    List_slice_eval(Listincode* l) : Listincode(l) {}
    List_slice_eval(List* l) : Listincode(l) {}
    List_slice_eval() {}
    List_slice_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_slice_eval(e);
    }
    
    List* cloning() {
        return new List_slice_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_factorial_eval : public Listincode {
public:
    
    List_factorial_eval(Listincode* l) : Listincode(l) {}
    List_factorial_eval(List* l) : Listincode(l) {}
    List_factorial_eval() {}
    List_factorial_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_factorial_eval(e);
    }
    
    List* cloning() {
        return new List_factorial_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_bappend_eval : public Listincode {
public:
    
    List_bappend_eval(Listincode* l) : Listincode(l) {}
    List_bappend_eval(List* l) : Listincode(l) {}
    List_bappend_eval() {}
    List_bappend_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bappend_eval(e);
    }
    
    List* cloning() {
        return new List_bappend_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_bread_eval : public Listincode {
public:
    
    List_bread_eval(Listincode* l) : Listincode(l) {}
    List_bread_eval(List* l) : Listincode(l) {}
    List_bread_eval() {}
    List_bread_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bread_eval(e);
    }
    
    List* cloning() {
        return new List_bread_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_bwrite_eval : public Listincode {
public:
    
    List_bwrite_eval(Listincode* l) : Listincode(l) {}
    List_bwrite_eval(List* l) : Listincode(l) {}
    List_bwrite_eval() {}
    List_bwrite_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bwrite_eval(e);
    }
    
    List* cloning() {
        return new List_bwrite_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_fappend_eval : public Listincode {
public:
    
    List_fappend_eval(Listincode* l) : Listincode(l) {}
    List_fappend_eval(List* l) : Listincode(l) {}
    List_fappend_eval() {}
    List_fappend_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_fappend_eval(e);
    }
    
    List* cloning() {
        return new List_fappend_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_fread_eval : public Listincode {
public:
    
    List_fread_eval(Listincode* l) : Listincode(l) {}
    List_fread_eval(List* l) : Listincode(l) {}
    List_fread_eval() {}
    List_fread_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_fread_eval(e);
    }
    
    List* cloning() {
        return new List_fread_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_fwrite_eval : public Listincode {
public:
    
    List_fwrite_eval(Listincode* l) : Listincode(l) {}
    List_fwrite_eval(List* l) : Listincode(l) {}
    List_fwrite_eval() {}
    List_fwrite_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_fwrite_eval(e);
    }
    
    List* cloning() {
        return new List_fwrite_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_shorts_eval : public Listincode {
public:
    
    List_shorts_eval(Listincode* l) : Listincode(l) {}
    List_shorts_eval(List* l) : Listincode(l) {}
    List_shorts_eval() {}
    List_shorts_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_shorts_eval(e);
    }
    
    List* cloning() {
        return new List_shorts_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_irange_eval : public Listincode {
public:
    
    List_irange_eval(Listincode* l) : Listincode(l) {}
    List_irange_eval(List* l) : Listincode(l) {}
    List_irange_eval() {}
    List_irange_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_irange_eval(e);
    }
    
    List* cloning() {
        return new List_irange_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_irangein_eval : public Listincode {
public:
    
    List_irangein_eval(Listincode* l) : Listincode(l) {}
    List_irangein_eval(List* l) : Listincode(l) {}
    List_irangein_eval() {}
    List_irangein_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_irangein_eval(e);
    }
    
    List* cloning() {
        return new List_irangein_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_llist_eval : public Listincode {
public:
    
    List_llist_eval(Listincode* l) : Listincode(l) {}
    List_llist_eval(List* l) : Listincode(l) {}
    List_llist_eval() {}
    List_llist_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_llist_eval(e);
    }
    
    List* cloning() {
        return new List_llist_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_to_list_eval : public Listincode {
public:
    
    List_to_list_eval(Listincode* l) : Listincode(l) {}
    List_to_list_eval(List* l) : Listincode(l) {}
    List_to_list_eval() {}
    List_to_list_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_to_list_eval(e);
    }
    
    List* cloning() {
        return new List_to_list_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_to_tensor_eval : public Listincode {
public:
    
    List_to_tensor_eval(Listincode* l) : Listincode(l) {}
    List_to_tensor_eval(List* l) : Listincode(l) {}
    List_to_tensor_eval() {}
    List_to_tensor_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_to_tensor_eval(e);
    }
    
    List* cloning() {
        return new List_to_tensor_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_to_llist_eval : public Listincode {
public:
    
    List_to_llist_eval(Listincode* l) : Listincode(l) {}
    List_to_llist_eval(List* l) : Listincode(l) {}
    List_to_llist_eval() {}
    List_to_llist_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_to_llist_eval(e);
    }
    
    List* cloning() {
        return new List_to_llist_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_lock_eval : public Listincode {
public:
    
    List_lock_eval(Listincode* l) : Listincode(l) {}
    List_lock_eval(List* l) : Listincode(l) {}
    List_lock_eval() {}
    List_lock_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_lock_eval(e);
    }
    
    List* cloning() {
        return new List_lock_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_mloop_eval : public Listincode {
public:
    
    List_mloop_eval(Listincode* l) : Listincode(l) {}
    List_mloop_eval(List* l) : Listincode(l) {}
    List_mloop_eval() {}
    List_mloop_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_mloop_eval(e);
    }
    
    List* cloning() {
        return new List_mloop_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_lloop_eval : public Listincode {
public:
    
    List_lloop_eval(Listincode* l) : Listincode(l) {}
    List_lloop_eval(List* l) : Listincode(l) {}
    List_lloop_eval() {}
    List_lloop_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_lloop_eval(e);
    }
    
    List* cloning() {
        return new List_lloop_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_mark_eval : public Listincode {
public:
    
    List_mark_eval(Listincode* l) : Listincode(l) {}
    List_mark_eval(List* l) : Listincode(l) {}
    List_mark_eval() {}
    List_mark_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_mark_eval(e);
    }
    
    List* cloning() {
        return new List_mark_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_resetmark_eval : public Listincode {
public:
    
    List_resetmark_eval(Listincode* l) : Listincode(l) {}
    List_resetmark_eval(List* l) : Listincode(l) {}
    List_resetmark_eval() {}
    List_resetmark_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_resetmark_eval(e);
    }
    
    List* cloning() {
        return new List_resetmark_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_tensor_string_eval : public Listincode {
public:
    
    List_tensor_string_eval(Listincode* l) : Listincode(l) {}
    List_tensor_string_eval(List* l) : Listincode(l) {}
    List_tensor_string_eval() {}
    List_tensor_string_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tensor_string_eval(e);
    }
    
    List* cloning() {
        return new List_tensor_string_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_tensor_stringbyte_eval : public Listincode {
public:
    
    List_tensor_stringbyte_eval(Listincode* l) : Listincode(l) {}
    List_tensor_stringbyte_eval(List* l) : Listincode(l) {}
    List_tensor_stringbyte_eval() {}
    List_tensor_stringbyte_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tensor_stringbyte_eval(e);
    }
    
    List* cloning() {
        return new List_tensor_stringbyte_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_tensor_number_eval : public Listincode {
public:
    
    List_tensor_number_eval(Listincode* l) : Listincode(l) {}
    List_tensor_number_eval(List* l) : Listincode(l) {}
    List_tensor_number_eval() {}
    List_tensor_number_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tensor_number_eval(e);
    }
    
    List* cloning() {
        return new List_tensor_number_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_tensor_float_eval : public Listincode {
public:
    
    List_tensor_float_eval(Listincode* l) : Listincode(l) {}
    List_tensor_float_eval(List* l) : Listincode(l) {}
    List_tensor_float_eval() {}
    List_tensor_float_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tensor_float_eval(e);
    }
    
    List* cloning() {
        return new List_tensor_float_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_tensor_short_eval : public Listincode {
public:
    
    List_tensor_short_eval(Listincode* l) : Listincode(l) {}
    List_tensor_short_eval(List* l) : Listincode(l) {}
    List_tensor_short_eval() {}
    List_tensor_short_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tensor_short_eval(e);
    }
    
    List* cloning() {
        return new List_tensor_short_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_tensor_integer_eval : public Listincode {
public:
    
    List_tensor_integer_eval(Listincode* l) : Listincode(l) {}
    List_tensor_integer_eval(List* l) : Listincode(l) {}
    List_tensor_integer_eval() {}
    List_tensor_integer_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tensor_integer_eval(e);
    }
    
    List* cloning() {
        return new List_tensor_integer_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_matrix_number_eval : public Listincode {
public:
    
    List_matrix_number_eval(Listincode* l) : Listincode(l) {}
    List_matrix_number_eval(List* l) : Listincode(l) {}
    List_matrix_number_eval() {}
    List_matrix_number_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_matrix_number_eval(e);
    }
    
    List* cloning() {
        return new List_matrix_number_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_matrix_string_eval : public Listincode {
public:
    
    List_matrix_string_eval(Listincode* l) : Listincode(l) {}
    List_matrix_string_eval(List* l) : Listincode(l) {}
    List_matrix_string_eval() {}
    List_matrix_string_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_matrix_string_eval(e);
    }
    
    List* cloning() {
        return new List_matrix_string_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_matrix_stringbyte_eval : public Listincode {
public:
    
    List_matrix_stringbyte_eval(Listincode* l) : Listincode(l) {}
    List_matrix_stringbyte_eval(List* l) : Listincode(l) {}
    List_matrix_stringbyte_eval() {}
    List_matrix_stringbyte_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_matrix_stringbyte_eval(e);
    }
    
    List* cloning() {
        return new List_matrix_stringbyte_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_matrix_short_eval : public Listincode {
public:
    
    List_matrix_short_eval(Listincode* l) : Listincode(l) {}
    List_matrix_short_eval(List* l) : Listincode(l) {}
    List_matrix_short_eval() {}
    List_matrix_short_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_matrix_short_eval(e);
    }
    
    List* cloning() {
        return new List_matrix_short_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_matrix_integer_eval : public Listincode {
public:
    
    List_matrix_integer_eval(Listincode* l) : Listincode(l) {}
    List_matrix_integer_eval(List* l) : Listincode(l) {}
    List_matrix_integer_eval() {}
    List_matrix_integer_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_matrix_integer_eval(e);
    }
    
    List* cloning() {
        return new List_matrix_integer_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_matrix_float_eval : public Listincode {
public:
    
    List_matrix_float_eval(Listincode* l) : Listincode(l) {}
    List_matrix_float_eval(List* l) : Listincode(l) {}
    List_matrix_float_eval() {}
    List_matrix_float_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_matrix_float_eval(e);
    }
    
    List* cloning() {
        return new List_matrix_float_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_floats_eval : public Listincode {
public:
    
    List_floats_eval(Listincode* l) : Listincode(l) {}
    List_floats_eval(List* l) : Listincode(l) {}
    List_floats_eval() {}
    List_floats_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_floats_eval(e);
    }
    
    List* cloning() {
        return new List_floats_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_pipe_eval : public Listincode {
public:
    
    List_pipe_eval(Listincode* l) : Listincode(l) {}
    List_pipe_eval(List* l) : Listincode(l) {}
    List_pipe_eval() {}
    List_pipe_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_pipe_eval(e);
    }
    
    List* cloning() {
        return new List_pipe_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_prettify_eval : public Listincode {
public:
    
    List_prettify_eval(Listincode* l) : Listincode(l) {}
    List_prettify_eval(List* l) : Listincode(l) {}
    List_prettify_eval() {}
    List_prettify_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_prettify_eval(e);
    }
    
    List* cloning() {
        return new List_prettify_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_range_eval : public Listincode {
public:
    
    List_range_eval(Listincode* l) : Listincode(l) {}
    List_range_eval(List* l) : Listincode(l) {}
    List_range_eval() {}
    List_range_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_range_eval(e);
    }
    
    List* cloning() {
        return new List_range_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_rangein_eval : public Listincode {
public:
    
    List_rangein_eval(Listincode* l) : Listincode(l) {}
    List_rangein_eval(List* l) : Listincode(l) {}
    List_rangein_eval() {}
    List_rangein_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_rangein_eval(e);
    }
    
    List* cloning() {
        return new List_rangein_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_sign_eval : public Listincode {
public:
    
    List_sign_eval(Listincode* l) : Listincode(l) {}
    List_sign_eval(List* l) : Listincode(l) {}
    List_sign_eval() {}
    List_sign_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_sign_eval(e);
    }
    
    List* cloning() {
        return new List_sign_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_size_eval : public Listincode {
public:
    
    List_size_eval(Listincode* l) : Listincode(l) {}
    List_size_eval(List* l) : Listincode(l) {}
    List_size_eval() {}
    List_size_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_size_eval(e);
    }
    
    List* cloning() {
        return new List_size_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_tally_eval : public Listincode {
public:
    
    List_tally_eval(Listincode* l) : Listincode(l) {}
    List_tally_eval(List* l) : Listincode(l) {}
    List_tally_eval() {}
    List_tally_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_tally_eval(e);
    }
    
    List* cloning() {
        return new List_tally_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_signp_eval : public Listincode {
public:
    
    List_signp_eval(Listincode* l) : Listincode(l) {}
    List_signp_eval(List* l) : Listincode(l) {}
    List_signp_eval() {}
    List_signp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_signp_eval(e);
    }
    
    List* cloning() {
        return new List_signp_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_sleep_eval : public Listincode {
public:
    
    List_sleep_eval(Listincode* l) : Listincode(l) {}
    List_sleep_eval(List* l) : Listincode(l) {}
    List_sleep_eval() {}
    List_sleep_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_sleep_eval(e);
    }
    
    List* cloning() {
        return new List_sleep_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_space_eval : public Listincode {
public:
    
    List_space_eval(Listincode* l) : Listincode(l) {}
    List_space_eval(List* l) : Listincode(l) {}
    List_space_eval() {}
    List_space_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_space_eval(e);
    }
    
    List* cloning() {
        return new List_space_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_sum_eval : public Listincode {
public:
    
    List_sum_eval(Listincode* l) : Listincode(l) {}
    List_sum_eval(List* l) : Listincode(l) {}
    List_sum_eval() {}
    List_sum_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_sum_eval(e);
    }
    
    List* cloning() {
        return new List_sum_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_product_eval : public Listincode {
public:
    
    List_product_eval(Listincode* l) : Listincode(l) {}
    List_product_eval(List* l) : Listincode(l) {}
    List_product_eval() {}
    List_product_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_product_eval(e);
    }
    
    List* cloning() {
        return new List_product_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_transpose_eval : public Listincode {
public:
    
    List_transpose_eval(Listincode* l) : Listincode(l) {}
    List_transpose_eval(List* l) : Listincode(l) {}
    List_transpose_eval() {}
    List_transpose_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_transpose_eval(e);
    }
    
    List* cloning() {
        return new List_transpose_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_heap_eval : public Listincode {
public:
    
    List_heap_eval(Listincode* l) : Listincode(l) {}
    List_heap_eval(List* l) : Listincode(l) {}
    List_heap_eval() {}
    List_heap_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_heap_eval(e);
    }
    
    List* cloning() {
        return new List_heap_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_trigger_eval : public Listincode {
public:
    
    List_trigger_eval(Listincode* l) : Listincode(l) {}
    List_trigger_eval(List* l) : Listincode(l) {}
    List_trigger_eval() {}
    List_trigger_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_trigger_eval(e);
    }
    
    List* cloning() {
        return new List_trigger_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_wait_eval : public Listincode {
public:
    
    List_wait_eval(Listincode* l) : Listincode(l) {}
    List_wait_eval(List* l) : Listincode(l) {}
    List_wait_eval() {}
    List_wait_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_wait_eval(e);
    }
    
    List* cloning() {
        return new List_wait_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_waiton_eval : public Listincode {
public:
    
    List_waiton_eval(Listincode* l) : Listincode(l) {}
    List_waiton_eval(List* l) : Listincode(l) {}
    List_waiton_eval() {}
    List_waiton_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_waiton_eval(e);
    }
    
    List* cloning() {
        return new List_waiton_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_solve_eval : public Listincode {
public:
    
    List_solve_eval(Listincode* l) : Listincode(l) {}
    List_solve_eval(List* l) : Listincode(l) {}
    List_solve_eval() {}
    List_solve_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_solve_eval(e);
    }
    
    List* cloning() {
        return new List_solve_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_determinant_eval : public Listincode {
public:
    
    List_determinant_eval(Listincode* l) : Listincode(l) {}
    List_determinant_eval(List* l) : Listincode(l) {}
    List_determinant_eval() {}
    List_determinant_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_determinant_eval(e);
    }
    
    List* cloning() {
        return new List_determinant_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_ludcmp_eval : public Listincode {
public:
    
    List_ludcmp_eval(Listincode* l) : Listincode(l) {}
    List_ludcmp_eval(List* l) : Listincode(l) {}
    List_ludcmp_eval() {}
    List_ludcmp_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_ludcmp_eval(e);
    }
    
    List* cloning() {
        return new List_ludcmp_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_lubksb_eval : public Listincode {
public:
    
    List_lubksb_eval(Listincode* l) : Listincode(l) {}
    List_lubksb_eval(List* l) : Listincode(l) {}
    List_lubksb_eval() {}
    List_lubksb_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_lubksb_eval(e);
    }
    
    List* cloning() {
        return new List_lubksb_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_invert_eval : public Listincode {
public:
    
    List_invert_eval(Listincode* l) : Listincode(l) {}
    List_invert_eval(List* l) : Listincode(l) {}
    List_invert_eval() {}
    List_invert_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_invert_eval(e);
    }
    
    List* cloning() {
        return new List_invert_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_listand_eval : public Listincode {
public:
    
    List_listand_eval(Listincode* l) : Listincode(l) {}
    List_listand_eval(List* l) : Listincode(l) {}
    List_listand_eval() {}
    List_listand_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_listand_eval(e);
    }
    
    List* cloning() {
        return new List_listand_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_listor_eval : public Listincode {
public:
    
    List_listor_eval(Listincode* l) : Listincode(l) {}
    List_listor_eval(List* l) : Listincode(l) {}
    List_listor_eval() {}
    List_listor_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_listor_eval(e);
    }
    
    List* cloning() {
        return new List_listor_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_listxor_eval : public Listincode {
public:
    
    List_listxor_eval(Listincode* l) : Listincode(l) {}
    List_listxor_eval(List* l) : Listincode(l) {}
    List_listxor_eval() {}
    List_listxor_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_listxor_eval(e);
    }
    
    List* cloning() {
        return new List_listxor_eval(multiple);
    }
    Element* eval(LispE* lisp);
};

class List_plusmultiply : public Listincode {
public:
    
    List_plusmultiply(Listincode* l) : Listincode(l) {}
    List_plusmultiply(List* l) : Listincode(l) {}
    List_plusmultiply() {}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_plusmultiply(e);
    }
    
    List* cloning() {
        return new List_plusmultiply();
    }
    
    Element* eval(LispE* lisp);
};

class List_divide2 : public List {
public:
    List_divide2(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_plus2 : public List {
public:
    List_plus2(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_minus2 : public List {
public:
    List_minus2(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_multiply2 : public List {
public:
    List_multiply2(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_divide3 : public List {
public:
    List_divide3(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_plus3 : public List {
public:
    List_plus3(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_minus3 : public List {
public:
    List_minus3(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_multiply3 : public List {
public:
    List_multiply3(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_dividen : public Listincode {
public:
    List_dividen(Listincode* l) : Listincode(l) {}
    List_dividen(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_dividen() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_dividen(e);
    }
    
    List* cloning() {
        return new List_dividen();
    }
    
    Element* eval(LispE*);
};

class List_plusn : public Listincode {
public:
    List_plusn(Listincode* l) : Listincode(l) {}
    List_plusn(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_plusn() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_plusn(e);
    }
    
    List* cloning() {
        return new List_plusn();
    }
    Element* eval(LispE*);
};

class List_minusn : public Listincode {
public:
    List_minusn(Listincode* l) : Listincode(l) {}
    List_minusn(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_minusn() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_minusn(e);
    }
    
    List* cloning() {
        return new List_minusn();
    }
    Element* eval(LispE*);
};

class List_multiplyn : public Listincode {
public:
    List_multiplyn(Listincode* l) : Listincode(l) {}
    List_multiplyn(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_multiplyn() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_multiplyn(e);
    }
    
    List* cloning() {
        return new List_multiplyn();
    }
    Element* eval(LispE*);
};

class List_mod_eval : public Listincode {
public:
    
    List_mod_eval(Listincode* l) : Listincode(l) {}
    List_mod_eval(List* l) : Listincode(l) {}
    List_mod_eval() {}
    List_mod_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_mod_eval(e);
    }
    
    List* cloning() {
        return new List_mod_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_leftshift_eval : public Listincode {
public:
    
    List_leftshift_eval(Listincode* l) : Listincode(l) {}
    List_leftshift_eval(List* l) : Listincode(l) {}
    List_leftshift_eval() {}
    List_leftshift_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_leftshift_eval(e);
    }
    
    List* cloning() {
        return new List_leftshift_eval(multiple);
    }
    Element* eval(LispE* lisp);
};


class List_rightshift_eval : public Listincode {
public:
    
    List_rightshift_eval(Listincode* l) : Listincode(l) {}
    List_rightshift_eval(List* l) : Listincode(l) {}
    List_rightshift_eval() {}
    List_rightshift_eval(bool m)  {multiple = m;}
    
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_rightshift_eval(e);
    }
    
    List* cloning() {
        return new List_rightshift_eval(multiple);
    }
    
    Element* eval(LispE* lisp);
};


class List_power2 : public List {
public:
    List_power2(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_powern : public List {
public:
    Element* eval(LispE* lisp) {
        return evall_power(lisp);
    }
};


class List_divideequal_var : public List {
public:
    int16_t label;
    
    List_divideequal_var(List* l) : List(l, 0) {
        label = liste[1]->label();
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_plusequal_var : public List {
public:
    int16_t label;

    List_plusequal_var(List* l) : List(l, 0) {
        label = liste[1]->label();
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_minusequal_var : public List {
public:
    int16_t label;

    List_minusequal_var(List* l) : List(l, 0) {
        label = liste[1]->label();
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_multiplyequal_var : public List {
public:
    int16_t label;

    List_multiplyequal_var(List* l) : List(l, 0) {
        label = liste[1]->label();
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_divideequal_list : public List {
public:
    List_divideequal_list(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_plusequal_list : public List {
public:
    List_plusequal_list(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_minusequal_list : public List {
public:
    List_minusequal_list(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_multiplyequal_list : public List {
public:
    List_multiplyequal_list(List* l) : List(l, 0) {
        terminal = l->terminal;
    }
    Element* eval(LispE*);
};

class List_bitand : public Listincode {
public:
    List_bitand(Listincode* l) : Listincode(l) {}
    List_bitand(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_bitand() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bitand(e);
    }
    
    List* cloning() {
        return new List_bitand();
    }
    Element* eval(LispE*);
};

class List_bitandnot : public Listincode {
public:
    List_bitandnot(Listincode* l) : Listincode(l) {}
    List_bitandnot(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_bitandnot() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bitandnot(e);
    }
    
    List* cloning() {
        return new List_bitandnot();
    }
    Element* eval(LispE*);
};

class List_bitnot : public Listincode {
public:
    List_bitnot(Listincode* l) : Listincode(l) {}
    List_bitnot(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_bitnot() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bitnot(e);
    }
    
    List* cloning() {
        return new List_bitnot();
    }
    Element* eval(LispE*);
};

class List_bitor : public Listincode {
public:
    List_bitor(Listincode* l) : Listincode(l) {}
    List_bitor(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_bitor() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bitor(e);
    }
    
    List* cloning() {
        return new List_bitor();
    }
    Element* eval(LispE*);
};

class List_bitxor : public Listincode {
public:
    List_bitxor(Listincode* l) : Listincode(l) {}
    List_bitxor(List* l) : Listincode(l) {
        terminal = l->terminal;
    }
    List_bitxor() {}
    bool is_straight_eval() {
        return true;
    }
    
    List* cloning(Listincode* e, methodEval m) {
        return new List_bitxor(e);
    }
    
    List* cloning() {
        return new List_bitxor();
    }
    Element* eval(LispE*);
};


class Floats : public Element {
public:
    
    Constfloat exchange_value;
    vecte_a<float> liste;
    
    Floats() : Element(t_floats), exchange_value(0) {}
    Floats(Floats* n) : liste(n->liste), Element(t_floats), exchange_value(0) {}
    Floats(uint16_t s) : Element(t_floats, s), exchange_value(0) {}
    Floats(long nb, float v) : liste(nb, v), Element(t_floats), exchange_value(0) {}
    Floats(Floats* f, long pos) : liste(f->liste, pos), Element(t_floats), exchange_value(0) {}
    
    Element* matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21);
    
    virtual Element* newInstance() {
        return new Floats;
    }
    
    bool is_same_tensor(Element* a) {
        return (a->type == type && liste.size() == a->size());
    }

    Element* newTensor(LispE* lisp, List* l);
    
    Element* asList(LispE* lisp, List* l);
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    inline void reset() {
        liste.reset();
    }

    virtual void copyfrom(Element* l) {
        liste = ((Floats*)l)->liste;
    }

    Element* newInstance(Element* v) {
        return new Floats(liste.size(), v->asFloat());
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (!e->isList())
            liste.push_back(e->asFloat());
        else {
            for (long i = 0; i < e->size(); i++) {
                liste.push_back(e->index(i)->asFloat());
            }
        }
    }
    
    bool checkShape(long depth, vecte<long>& sz) {
        return (depth < sz.size() && sz[depth] == size());
    }
    
    void set_from(Element* c, long i) {
        liste.push_back(((Floats*)c)->liste[i]);
    }
    
    void set_from(Element* c, long i, long j) {
        while (i != j) {
            liste.push_back(((Floats*)c)->liste[i++]);
        }
    }
    
    void set_in(LispE* lisp, Element* c, long i) {
        liste[i] = c->asFloat();
    }
    
    Element* invert_sign(LispE* lisp);
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    void clean_iter(void* it) {
        delete (long*)it;
    }
    
    Element* rotate(LispE* lisp, long nb);
    
    Element* check_member(LispE* lisp, Element* the_set);
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
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
    
    void swap(long i, long j) {
        liste.swap(i,j);
    }
    
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e->asFloat());
        return true;
    }
    
    void front(Element* e) {
        liste.insert(0, e->asFloat());
    }
    
    void beforelast(Element* e) {
        liste.beforelast(e->asFloat());
    }
    
    Element* thekeys(LispE* lisp);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    virtual Element* fullcopy() {
        Floats* e = new Floats;
        e->liste = liste;
        return e;
    }
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        return new Floats(this);
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s);
    
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    
    bool isNotEmptyList() {
        return !liste.empty();
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* last() {
        exchange_value.content = liste.back();
        return &exchange_value;
    }
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            delete this;
        }
    }
    
    
    long size() {
        return liste.size();
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        sz -= 1;
        
        wstring buffer(L"[");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            buffer += convertToWString(liste[i]);
        }
        buffer += L"]";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            buffer += convertToWString(liste[i]);
        }
        buffer += L")";
        return buffer;
    }
    
    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            buffer += convertToUString(liste[i]);
        }
        buffer += U")";
        return buffer;
    }
    
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, float v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_back(e->asFloat());
    }
    
    void appendraw(Element* e) {
        liste.push_back(e->asFloat());
    }
    
    void change(long i, Element* e) {
        liste.at(i, e->asFloat());
    }
    
    void changelast(Element* e) {
        liste.atlast(e->asFloat());
    }
    
    void replacing(long i, Element* e) {
        liste.at(i, e->asFloat());
    }
    
    void replacingandclean(long i, Element* e) {
        liste.at(i, e->asFloat());
        e->release();
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_back(e->asFloat());
        else {
            liste.at(i, e->asFloat());
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    int16_t label() {
        return (liste.empty()?t_floats:v_null);
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, float v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
    bool removefirst() {
        if (!liste.size())
            return false;
        liste.erase(0);
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
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    void getShape(vecte<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return a_valuelist;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return a_valuelist;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.clear();
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
    
    Element* plus_direct(LispE* lisp, Element* e);
    Element* minus_direct(LispE* lisp, Element* e);
    Element* multiply_direct(LispE* lisp, Element* e);
    Element* divide_direct(LispE* lisp, Element* e);
    
    
    void sorting(LispE* lisp, List* comparison);
};


class Floatspool : public Floats {
public:
    LispE* lisp;
    Floatspool(LispE* l) : lisp(l) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Floatspool(LispE* l, long nb, float v) : lisp(l), Floats(nb, v) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Floatspool(LispE* l, Floats* n) : lisp(l), Floats(n) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Floatspool(LispE* l, Floats* f, long pos) : lisp(l), Floats(f, pos) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    inline Floatspool* set(Floats* nb, long pos) {
        liste.clear();
        for (long i = pos; i < nb->liste.size(); i++)
            liste.push_back(nb->liste[i]);
        return this;
    }
    
    inline Floatspool* set(long nb, float v) {
        liste.clear();
        while (nb != 0) {
            liste.push_back(v);
            nb--;
        }
        return this;
    }
    
    inline Floatspool* set(Floats* n) {
        liste = n->liste;
        return this;
    }
    
    
    Element* newInstance();
    Element* newInstance(Element* v);
    
    
    void decrementstatus(uint16_t nb);
    void decrement();
    
    void release();
    Element* fullcopy();
    Element* copyatom(LispE* lisp, uint16_t s);
    Element* copying(bool duplicate = true);
};

class Numbers : public Element {
public:
    
    Constnumber exchange_value;
    vecte_a<double> liste;
    
    Numbers() : Element(t_numbers), exchange_value(0) {}
    Numbers(Numbers* n) : liste(n->liste), Element(t_numbers), exchange_value(0) {}
    Numbers(Numbers* n, long pos) : liste(n->liste, pos), Element(t_numbers), exchange_value(0) {}
    Numbers(uint16_t s) : Element(t_numbers, s), exchange_value(0) {}
    Numbers(long nb, double v) : liste(nb,v), Element(t_numbers), exchange_value(0) {}
    
    Element* matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21);
    
    bool is_same_tensor(Element* a) {
        return (a->type == type && liste.size() == a->size());
    }

    Element* newTensor(LispE* lisp, List* l);
    
    virtual Element* newInstance() {
        return new Numbers;
    }
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    inline void reset() {
        liste.reset();
    }

    void copyfrom(Element* l) {
        liste = ((Numbers*)l)->liste;
    }

    bool checkShape(long depth, vecte<long>& sz) {
        return (depth < sz.size() && sz[depth] == size());
    }
    
    void set_from(Element* c, long i) {
        liste.push_back(((Numbers*)c)->liste[i]);
    }
    
    void set_from(Element* c, long i, long j) {
        while (i != j) {
            liste.push_back(((Numbers*)c)->liste[i++]);
        }
    }
    
    void set_in(LispE* lisp, Element* c, long i) {
        liste[i] = c->asNumber();
    }
    
    Element* asList(LispE* lisp, List* l);
    
    Element* newInstance(Element* v) {
        return new Numbers(liste.size(), v->asNumber());
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (!e->isList())
            liste.push_back(e->asNumber());
        else {
            for (long i = 0; i < e->size(); i++) {
                liste.push_back(e->index(i)->asNumber());
            }
        }
    }
    
    Element* invert_sign(LispE* lisp);
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    void clean_iter(void* it) {
        delete (long*)it;
    }
    
    Element* rotate(LispE* lisp, long nb);
    
    Element* check_member(LispE* lisp, Element* the_set);
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
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
    
    void swap(long i, long j) {
        liste.swap(i,j);
    }
    
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e->asNumber());
        return true;
    }
    
    void front(Element* e) {
        liste.insert(0, e->asNumber());
    }
    
    void beforelast(Element* e) {
        liste.beforelast(e->asNumber());
    }
    
    Element* thekeys(LispE* lisp);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    
    virtual Element* fullcopy() {
        Numbers* e = new Numbers;
        e->liste = liste;
        return e;
    }
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        return new Numbers(this);
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s);
    
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    
    bool isNotEmptyList() {
        return !liste.empty();
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* last() {
        exchange_value.content = liste.back();
        return &exchange_value;
    }
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            delete this;
        }
    }
    
    
    long size() {
        return liste.size();
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        sz -= 1;
        
        wstring buffer(L"[");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            buffer += convertToWString(liste[i]);
        }
        buffer += L"]";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            buffer += convertToWString(liste[i]);
        }
        buffer += L")";
        return buffer;
    }
    
    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            buffer += convertToUString(liste[i]);
        }
        buffer += U")";
        return buffer;
    }
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_back(e->asNumber());
    }
    void appendraw(Element* e) {
        liste.push_back(e->asNumber());
    }
    
    void change(long i, Element* e) {
        liste.at(i, e->asNumber());
    }
    
    void changelast(Element* e) {
        liste.atlast(e->asNumber());
    }
    
    void replacing(long i, Element* e) {
        liste.at(i, e->asNumber());
    }
    
    void replacingandclean(long i, Element* e) {
        liste.at(i, e->asFloat());
        e->release();
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_back(e->asNumber());
        else {
            liste.at(i, e->asNumber());
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    int16_t label() {
        return (liste.empty()?t_numbers:v_null);
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
    bool removefirst() {
        if (!liste.size())
            return false;
        liste.erase(0);
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
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    void getShape(vecte<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return a_valuelist;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return a_valuelist;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.clear();
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
    
    Element* plus_direct(LispE* lisp, Element* e);
    Element* minus_direct(LispE* lisp, Element* e);
    Element* multiply_direct(LispE* lisp, Element* e);
    Element* divide_direct(LispE* lisp, Element* e);
    
    
    void sorting(LispE* lisp, List* comparison);
};

class Numberspool : public Numbers {
public:
    LispE* lisp;
    Numberspool(LispE* l) : lisp(l) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Numberspool(LispE* l, long nb, double v) : lisp(l), Numbers(nb, v) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Numberspool(LispE* l, Numbers* n) : lisp(l), Numbers(n) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Numberspool(LispE* l, Numbers* f, long pos) : lisp(l), Numbers(f, pos) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    
    inline Numberspool* set(Numbers* nb, long pos) {
        liste.clear();
        for (long i = pos; i < nb->liste.size(); i++)
            liste.push_back(nb->liste[i]);
        return this;
    }
    
    inline Numberspool* set(long nb, double v) {
        liste.clear();
        while (nb != 0) {
            liste.push_back(v);
            nb--;
        }
        return this;
    }
    
    inline Numberspool* set(Numbers* n) {
        liste = n->liste;
        return this;
    }
    
    
    Element* newInstance();
    Element* newInstance(Element* v);
    
    
    void decrementstatus(uint16_t nb);
    void decrement();
    
    void release();
    Element* fullcopy();
    Element* copyatom(LispE* lisp, uint16_t s);
    Element* copying(bool duplicate = true);
    
};

class Shorts : public Element {
public:
    
    Constshort exchange_value;
    vecte_a<int16_t> liste;
    
    Shorts() : Element(t_shorts), exchange_value(0) {}
    Shorts(uint16_t s) : Element(t_shorts, s), exchange_value(0) {}
    Shorts(long nb, long v) : liste(nb, v), Element(t_shorts), exchange_value(0) {}
    Shorts(Shorts* i) : liste(i->liste), Element(t_shorts), exchange_value(0) {}
    Shorts(Shorts* i, long pos) : liste(i->liste, pos), Element(t_shorts), exchange_value(0) {}
    
    Element* matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21);
    
    virtual Element* newInstance() {
        return new Shorts;
    }
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    inline void reset() {
        liste.reset();
    }

    void copyfrom(Element* l) {
        liste = ((Shorts*)l)->liste;
    }

    bool checkShape(long depth, vecte<long>& sz) {
        return (depth < sz.size() && sz[depth] == size());
    }
    
    Element* asList(LispE* lisp, List* l);
    Element* invert_sign(LispE* lisp);
    Element* newInstance(Element* v) {
        return new Shorts(liste.size(), v->asShort());
    }
    
    void set_from(Element* c, long i) {
        liste.push_back(((Shorts*)c)->liste[i]);
    }
    
    void set_from(Element* c, long i, long j) {
        while (i != j) {
            liste.push_back(((Shorts*)c)->liste[i++]);
        }
    }
    
    void set_in(LispE* lisp, Element* c, long i) {
        liste[i] = c->asShort();
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (!e->isList())
            liste.push_back(e->asShort());
        else {
            for (long i = 0; i < e->size(); i++) {
                liste.push_back(e->index(i)->asShort());
            }
        }
    }
    
    Element* newTensor(LispE* lisp, List* l);
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    void clean_iter(void* it) {
        delete (long*)it;
    }
    
    Element* check_member(LispE* lisp, Element* the_set);
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
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
    
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e->asShort());
        return true;
    }
    
    void swap(long i, long j) {
        liste.swap(i,j);
    }
    
    void front(Element* e) {
        liste.insert(0, e->asShort());
    }
    
    void beforelast(Element* e) {
        liste.beforelast(e->asShort());
    }
    
    Element* thekeys(LispE* lisp);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    
    virtual Element* fullcopy() {
        Shorts* e = new Shorts;
        e->liste = liste;
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        Shorts* e = new Shorts;
        e->liste = liste;
        return e;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    
    bool isNotEmptyList() {
        return !liste.empty();
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* last() {
        exchange_value.content = liste.back();
        return &exchange_value;
    }
    
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    bool is_same_tensor(Element* a) {
        return (a->type == type && liste.size() == a->size());
    }

    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            delete this;
        }
    }
    
    
    long size() {
        return liste.size();
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        sz -= 1;
        
        wstring buffer(L"[");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            buffer += convertToWString((long)liste[i]);
        }
        buffer += L"]";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            buffer += convertToWString((long)liste[i]);
        }
        buffer += L")";
        return buffer;
    }
    
    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            buffer += convertToUString((long)liste[i]);
        }
        buffer += U")";
        return buffer;
    }
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_back(e->asShort());
    }
    void appendraw(Element* e) {
        liste.push_back(e->asShort());
    }
    
    void change(long i, Element* e) {
        liste.at(i, e->asShort());
    }
    
    void changelast(Element* e) {
        liste.atlast( e->asShort());
    }
    
    void replacing(long i, Element* e) {
        liste.at(i, e->asShort());
    }
    
    void replacingandclean(long i, Element* e) {
        liste.at(i, e->asShort());
        e->release();
    }

    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_back(e->asShort());
        else {
            liste.at(i, e->asShort());
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    int16_t label() {
        return (liste.empty()?t_integers:v_null);
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    Element* rotate(LispE* lisp, long nb);
    
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
    bool removefirst() {
        if (!liste.size())
            return false;
        liste.erase(0);
        return true;
    }
    
    bool removelast() {
        if (!liste.size())
            return false;
        liste.pop_back();
        return true;
    }
    
    bool remove(LispE*, Element* e) {
        long d =  e->asShort();
        return remove(d);
    }
    
    bool remove(long d) {
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    void getShape(vecte<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return a_valuelist;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return a_valuelist;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.clear();
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s) {
        if (liste.shared(status) < s)
            return this;
        
        Shorts* i = new Shorts(this);
        release();
        return i;
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
    
    
    Element* plus_direct(LispE* lisp, Element* e);
    Element* minus_direct(LispE* lisp, Element* e);
    Element* multiply_direct(LispE* lisp, Element* e);
    Element* divide_direct(LispE* lisp, Element* e);
    
    void sorting(LispE* lisp, List* comparison);
};

class Integers : public Element {
public:
    
    Constinteger exchange_value;
    vecte_a<long> liste;
    
    Integers() : Element(t_integers), exchange_value(0) {}
    Integers(uint16_t s) : Element(t_integers, s), exchange_value(0) {}
    Integers(long nb, long v) : liste(nb, v), Element(t_integers), exchange_value(0) {}
    Integers(Integers* i) : liste(i->liste), Element(t_integers), exchange_value(0) {}
    Integers(Integers* i, long pos) : liste(i->liste, pos), Element(t_integers), exchange_value(0) {}
    
    Element* matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21);
    
    virtual Element* newInstance() {
        return new Integers;
    }
    
    bool is_same_tensor(Element* a) {
        return (a->type == type && liste.size() == a->size());
    }

    Element* newTensor(LispE* lisp, List* l);
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    inline void reset() {
        liste.reset();
    }

    void copyfrom(Element* l) {
        liste = ((Integers*)l)->liste;
    }

    bool checkShape(long depth, vecte<long>& sz) {
        return (depth < sz.size() && sz[depth] == size());
    }
    
    void set_from(Element* c, long i) {
        liste.push_back(((Integers*)c)->liste[i]);
    }
    
    void set_from(Element* c, long i, long j) {
        while (i != j) {
            liste.push_back(((Integers*)c)->liste[i++]);
        }
    }
    
    void set_in(LispE* lisp, Element* c, long i) {
        liste[i] = c->asInteger();
    }
    
    Element* asList(LispE* lisp, List* l);
    Element* invert_sign(LispE* lisp);
    Element* newInstance(Element* v) {
        return new Integers(liste.size(), v->asInteger());
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (!e->isList())
            liste.push_back(e->asInteger());
        else {
            for (long i = 0; i < e->size(); i++) {
                liste.push_back(e->index(i)->asInteger());
            }
        }
    }
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    void clean_iter(void* it) {
        delete (long*)it;
    }
    
    Element* check_member(LispE* lisp, Element* the_set);
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
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
    
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e->asInteger());
        return true;
    }
    
    void swap(long i, long j) {
        liste.swap(i,j);
    }
    
    void front(Element* e) {
        liste.insert(0, e->asInteger());
    }
    
    void beforelast(Element* e) {
        liste.beforelast(e->asInteger());
    }
    
    Element* thekeys(LispE* lisp);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    
    virtual Element* fullcopy() {
        Integers* e = new Integers;
        e->liste = liste;
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        Integers* e = new Integers;
        e->liste = liste;
        return e;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    
    bool isNotEmptyList() {
        return !liste.empty();
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* last() {
        exchange_value.content = liste.back();
        return &exchange_value;
    }
    
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            delete this;
        }
    }
    
    
    long size() {
        return liste.size();
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        sz -= 1;
        
        wstring buffer(L"[");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            buffer += convertToWString(liste[i]);
        }
        buffer += L"]";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            buffer += convertToWString(liste[i]);
        }
        buffer += L")";
        return buffer;
    }
    
    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            buffer += convertToUString(liste[i]);
        }
        buffer += U")";
        return buffer;
    }
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_back(e->asInteger());
    }
    void appendraw(Element* e) {
        liste.push_back(e->asInteger());
    }
    
    void change(long i, Element* e) {
        liste.at(i, e->asInteger());
    }
    
    void changelast(Element* e) {
        liste.atlast( e->asInteger());
    }
    
    void replacing(long i, Element* e) {
        liste.at(i, e->asInteger());
    }

    void replacingandclean(long i, Element* e) {
        liste.at(i, e->asInteger());
        e->release();
    }

    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_back(e->asInteger());
        else {
            liste.at(i, e->asInteger());
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    int16_t label() {
        return (liste.empty()?t_integers:v_null);
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    Element* rotate(LispE* lisp, long nb);
        
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
    bool removefirst() {
        if (!liste.size())
            return false;
        liste.erase(0);
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
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    void getShape(vecte<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return a_valuelist;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return a_valuelist;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.clear();
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s);
    
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
    
    
    Element* plus_direct(LispE* lisp, Element* e);
    Element* minus_direct(LispE* lisp, Element* e);
    Element* multiply_direct(LispE* lisp, Element* e);
    Element* divide_direct(LispE* lisp, Element* e);
    
    void sorting(LispE* lisp, List* comparison);
};

class Integerspool : public Integers {
public:
    LispE* lisp;
    Integerspool(LispE* l) : lisp(l) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Integerspool(LispE* l, long sz, long v) : lisp(l), Integers(sz, v) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Integerspool(LispE* l, Integers* n) : lisp(l), Integers(n) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Integerspool(LispE* l, Integers* f, long pos) : lisp(l), Integers(f, pos) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    inline Integerspool* set(Integers* nb, long pos) {
        liste.clear();
        for (long i = pos; i < nb->liste.size(); i++)
            liste.push_back(nb->liste[i]);
        return this;
    }
    
    inline Integerspool* set(long nb, long v) {
        liste.clear();
        while (nb != 0) {
            liste.push_back(v);
            nb--;
        }
        return this;
    }
    
    inline Integerspool* set(Integers* i) {
        liste = i->liste;
        return this;
    }
    
    
    Element* newInstance();
    Element* newInstance(Element* v);
    void decrementstatus(uint16_t nb);
    void decrement();
    
    void release();
    Element* fullcopy();
    Element* copyatom(LispE* lisp, uint16_t s);
    Element* copying(bool duplicate = true);
    
};


class Strings : public Element {
public:
    
    vecte_n<u_ustring> liste;
    Conststring exchange_value;
    
    Strings() : exchange_value(U""), Element(t_strings) {}
    Strings(Strings* n, long pos) : exchange_value(U""), liste(n->liste, pos), Element(t_strings) {}
    
    Strings(long nb, wstring w) : exchange_value(U""), liste(nb, _w_to_u(w)), Element(t_strings) {}
    Strings(long nb, u_ustring w) : exchange_value(U""), liste(nb, w), Element(t_strings) {}
    
    Strings(Strings* s) : liste(s->liste), exchange_value(U""), Element(t_strings) {}
    
    Element* asList(LispE* lisp, List* l);
    
    virtual Element* newInstance() {
        return new Strings;
    }
    
    void copyfrom(Element* l) {
        liste = ((Strings*)l)->liste;
    }

    bool checkShape(long depth, vecte<long>& sz) {
        return (depth < sz.size() && sz[depth] == size());
    }
    
    void set_from(Element* c, long i) {
        liste.push_back(((Strings*)c)->liste[i]);
    }
    
    void set_from(Element* c, long i, long j) {
        while (i != j) {
            liste.push_back(((Strings*)c)->liste[i++]);
        }
    }
    
    void set_in(LispE* lisp, Element* c, long i) {
        liste[i] = c->asUString(lisp);
    }
    
    bool is_same_tensor(Element* a) {
        return (a->type == type && liste.size() == a->size());
    }

    Element* newTensor(LispE* lisp, List* l);

    Element* newInstance(Element* v) {
        return new Strings(liste.size(), v->asString(NULL));
    }
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    Element* check_member(LispE* lisp, Element* the_set);
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    void clean_iter(void* it) {
        delete (long*)it;
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (!e->isList())
            liste.push_back(e->asUString(lisp));
        else {
            for (long i = 0; i < e->size(); i++) {
                liste.push_back(e->index(i)->asUString(lisp));
            }
        }
    }
    
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
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
    
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e->asUString(NULL));
        return true;
    }
    
    void swap(long i, long j) {
        liste.swap(i,j);
    }
    
    void front(Element* e) {
        liste.insert(0, e->asUString(NULL));
    }
    
    void beforelast(Element* e) {
        liste.beforelast(e->asUString(NULL));
    }
    
    Element* thekeys(LispE* lisp);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    virtual Element* fullcopy() {
        Strings* e = new Strings();
        e->liste = liste;
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    Element* rotate(LispE*, long nb);
    
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        Strings* e = new Strings();
        e->liste = liste;
        return e;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    
    bool isNotEmptyList() {
        return !liste.empty();
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* last() {
        exchange_value.content = liste.back();
        return &exchange_value;
    }
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Strings* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            delete this;
        }
    }
    
    
    long size() {
        return liste.size();
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        sz -= 1;
        
        wstring buffer(L"[");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            buffer += wjsonstring(liste[i]);
        }
        buffer += L"]";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            buffer += wjsonstring(liste[i]);
        }
        buffer += L")";
        return buffer;
    }
    
    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            buffer += ujsonstring(liste[i]);
        }
        buffer += U")";
        return buffer;
    }
    
    
    void append(wstring& w) {
        u_pstring k = _w_to_u(w);
        liste.push_back(k);
    }
    
    void append(u_ustring& k) {
        liste.push_back(k);
    }
    
    void append(string& k) {
        u_ustring w;
        s_utf8_to_unicode(w, k, k.size());
        liste.push_back(w);
    }
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_back(e->asUString(NULL));
    }
    void appendraw(Element* e) {
        liste.push_back(e->asUString(NULL));
    }
    
    void change(long i, Element* e) {
        liste[i] = e->asUString(NULL);
    }
    
    void changelast(Element* e) {
        liste[liste.size()-1] = e->asUString(NULL);
    }
    
    void replacing(long i, Element* e) {
        liste[i] = e->asUString(NULL);
    }
    
    void replacingandclean(long i, Element* e) {
        liste.at(i, e->asUString(NULL));
        e->release();
    }

    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_back(e->asUString(NULL));
        else {
            liste[i] = e->asUString(NULL);
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    int16_t label() {
        return (liste.empty()?t_strings:v_null);
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
    bool removefirst() {
        if (!liste.size())
            return false;
        liste.erase(0);
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
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    void getShape(vecte<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return a_valuelist;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return a_valuelist;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.clear();
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s);
    
    Element* plus(LispE* l, Element* e);
    
    void sorting(LispE* lisp, List* comparison);
    
};

class Stringspool : public Strings {
public:
    LispE* lisp;
    Stringspool(LispE* l) : lisp(l) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Stringspool(LispE* l, Strings* s) : lisp(l), Strings(s) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }
    
    Stringspool(LispE* l, Strings* s, long pos) : lisp(l), Strings(s, pos) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }

    Stringspool(LispE* l, long nb, u_ustring v) : lisp(l), Strings(nb, v) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }

    inline Stringspool* set(Strings* i) {
        liste = i->liste;
        return this;
    }

    inline Stringspool* set(long nb, u_ustring v) {
        liste.clear();
        while (nb != 0) {
            liste.push_back(v);
            nb--;
        }
        return this;
    }

    Element* newInstance();
    
    void decrementstatus(uint16_t nb);
    void decrement();
    
    void release();
    Element* fullcopy();
    Element* copyatom(LispE* lisp, uint16_t s);
    Element* copying(bool duplicate = true);
    
};

class Stringbytes : public Element {
public:
    
    vecte_n<string> liste;
    Conststringbyte exchange_value;
    
    Stringbytes() : exchange_value(""), Element(t_stringbytes) {}
    Stringbytes(Stringbytes* n, long pos) : exchange_value(""), liste(n->liste, pos), Element(t_stringbytes) {}
    Stringbytes(long nb, string w) : exchange_value(""), liste(nb, w), Element(t_stringbytes) {}
    Stringbytes(Stringbytes* s) : liste(s->liste), exchange_value(""), Element(t_stringbytes) {}
    Stringbytes(Strings* s) : exchange_value(""), Element(t_stringbytes) {
        string v;
        for (long i = 0; i < s->size(); i++) {
            s_unicode_to_utf8(v,s->liste[i]);
            liste.push_back(v);
        }
    }
    
    Element* asList(LispE* lisp, List* l);
    
    virtual Element* newInstance() {
        return new Stringbytes;
    }
    
    void copyfrom(Element* l) {
        liste = ((Stringbytes*)l)->liste;
    }

    bool checkShape(long depth, vecte<long>& sz) {
        return (depth < sz.size() && sz[depth] == size());
    }
    
    void set_from(Element* c, long i) {
        liste.push_back(((Stringbytes*)c)->liste[i]);
    }
    
    void set_from(Element* c, long i, long j) {
        while (i != j) {
            liste.push_back(((Stringbytes*)c)->liste[i++]);
        }
    }
    
    void set_in(LispE* lisp, Element* c, long i) {
        liste[i] = c->toString(lisp);
    }
    
    bool is_same_tensor(Element* a) {
        return (a->type == type && liste.size() == a->size());
    }

    Element* newTensor(LispE* lisp, List* l);

    Element* newInstance(Element* v) {
        return new Stringbytes(liste.size(), v->toString(NULL));
    }
    
    inline void reserve(long sz) {
        liste.reserve(sz);
    }
    
    Element* check_member(LispE* lisp, Element* the_set);
    
    void* begin_iter() {
        long* n = new long[1];
        n[0] = 0;
        return n;
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);
    
    void clean_iter(void* it) {
        delete (long*)it;
    }
    
    void concatenate(LispE* lisp, Element* e) {
        if (!e->isList())
            liste.push_back(e->toString(lisp));
        else {
            for (long i = 0; i < e->size(); i++) {
                liste.push_back(e->index(i)->toString(lisp));
            }
        }
    }
    
    Element* equal(LispE* lisp, Element* e);
    bool egal(Element* e);
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }
    
    Element* negate(LispE* lisp);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
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
    
    bool insertion(Element* e, long idx) {
        liste.insert(idx, e->toString(NULL));
        return true;
    }
    
    void swap(long i, long j) {
        liste.swap(i,j);
    }
    
    void front(Element* e) {
        liste.insert(0, e->toString(NULL));
    }
    
    void beforelast(Element* e) {
        liste.beforelast(e->toString(NULL));
    }
    
    Element* thekeys(LispE* lisp);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isequal(LispE* lisp, Element* value);
    Element* comparison(LispE* lisp, Element* value);
    
    virtual Element* fullcopy() {
        Stringbytes* e = new Stringbytes();
        e->liste = liste;
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotating(LispE* lisp, bool left);
    Element* rotate(LispE*, long nb);
    
    
    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        Stringbytes* e = new Stringbytes();
        e->liste = liste;
        return e;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp);
    
    bool isList() {
        return true;
    }
    
    bool isEmpty() {
        return liste.empty();
    }
    
    
    bool isNotEmptyList() {
        return !liste.empty();
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    Element* replace_in(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* last() {
        exchange_value.content = liste.back();
        return &exchange_value;
    }
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    void flatten(LispE*, Floats* l);
    void flatten(LispE*, Integers* l);
    void flatten(LispE*, Shorts* l);
    void flatten(LispE*, Stringbytes* l);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            delete this;
        }
    }
    
    
    long size() {
        return liste.size();
    }
    
    Element* car(LispE* lisp);
    Element* cdr(LispE* lisp);
    Element* cadr(LispE*, u_ustring& actions);
    
    void protecting(bool protection, LispE* lisp) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
    }
    
    wstring jsonString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"[]";
        
        sz -= 1;
        
        wstring buffer(L"[");
        wstring u;
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L",";
            u = L"";
            s_utf8_to_unicode(u, liste[i], liste[i].size());
            buffer += wjsonstring(u);
        }
        buffer += L"]";
        return buffer;
    }
    
    string toString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return "()";
        
        sz -= 1;
        
        string buffer("(");

        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += " ";
            buffer += jsonstring(liste[i]);
        }
        buffer += ")";
        return buffer;
    }

    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        wstring u;
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += L" ";
            u = L"";
            s_utf8_to_unicode(u, liste[i], liste[i].size());
            buffer += wjsonstring(u);
        }
        buffer += L")";
        return buffer;
    }

    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        u_ustring u;
        for (long i = 0; i <= sz; i++) {
            if (i && i <= sz)
                buffer += U" ";
            s_utf8_to_unicode(u, liste[i], liste[i].size());
            buffer += ujsonstring(u);
        }
        buffer += U")";
        return buffer;
    }
    
    
    void append(wstring& w) {
        string u;
        s_unicode_to_utf8(u, w);
        liste.push_back(u);
    }
    
    void append(u_ustring& k) {
        string u;
        s_unicode_to_utf8(u, k);
        liste.push_back(u);
    }
    
    void append(string& k) {
        liste.push_back(k);
    }
    
    void push_element(LispE* lisp, List* l);
    void push_element_true(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);
    
    void append(Element* e) {
        liste.push_back(e->toString(NULL));
    }
    void appendraw(Element* e) {
        liste.push_back(e->toString(NULL));
    }
    
    void change(long i, Element* e) {
        liste[i] = e->toString(NULL);
    }
    
    void changelast(Element* e) {
        liste[liste.size()-1] = e->toString(NULL);
    }
    
    void replacing(long i, Element* e) {
        liste[i] = e->toString(NULL);
    }
    
    void replacingandclean(long i, Element* e) {
        liste.at(i, e->toString(NULL));
        e->release();
    }

    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0) {
            i += liste.size();
            if (i < 0)
                throw new Error("Error: index out of bounds");
        }
        
        if (i >= liste.size())
            liste.push_back(e->toString(NULL));
        else {
            liste[i] = e->toString(NULL);
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    int16_t label() {
        return (liste.empty()?t_stringbytes:v_null);
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    void storevalue(LispE*, string& v) {
        liste.push_back(v);
    }
    
    bool removefirst() {
        if (!liste.size())
            return false;
        liste.erase(0);
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
        if (!liste.size())
            return false;
        
        if (d == liste.size() || d == -1) {
            liste.pop_back();
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        liste.erase(d);
        return true;
    }
    
    Element* takenb(LispE* lisp, long nb, bool direction);
    
    void getShape(vecte<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return a_valuelist;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return a_valuelist;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert_with_compare(LispE*, Element* e, List& comparison);
    
    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (!is_protected())
            liste.clear();
    }
    
    virtual Element* copyatom(LispE* lisp, uint16_t s);
    
    Element* plus(LispE* l, Element* e);
    
    void sorting(LispE* lisp, List* comparison);
    
};

class Rankloop : public List {
public:
    LispE* lisp;
    Element* lst;
    vecte<long> positions;
    long max_iterator;
    Element* index_value;
    bool last;
    long i_nxt;
    
    Rankloop(LispE* lp, List* l);
    
    ~Rankloop() {
        index_value->decrement();
    }
    
    bool isContainer() {
        return true;
    }
        
    bool isList() {
        return true;
    }
    
    long size() {
        return max_iterator;
    }
    
    void* begin_iter();
    void clean_iter(void* it) {}
    Element* next_iter(LispE* lisp, void* iter);
    Element* next_iter_exchange(LispE* lisp, void* iter);
    
    Element* index(long i) {
        index_value->decrement();
        if (last)
            index_value = lst->rank(lisp, positions);
        else {
            positions.push_back(i);
            index_value = lst->rank(lisp, positions);
            positions.pop_back();
        }
        index_value->increment();
        return index_value;
    }
    
    Element* protected_index(LispE* lisp,long i) {
        Element* r;
        if (last)
            r = lst->rank(lisp, positions);
        else {
            positions.push_back(i);
            r = lst->rank(lisp, positions);
            positions.pop_back();
        }
        return r;
    }
    
    Element* value_from_index(LispE* lisp, long i) {
        Element* r;
        if (last)
            r = lst->rank(lisp, positions);
        else {
            positions.push_back(i);
            r = lst->rank(lisp, positions);
            positions.pop_back();
        }
        return r;
    }
    
    Element* value_on_index(LispE*, long i) {
        Element* r;
        if (last)
            r = lst->rank(lisp, positions);
        else {
            positions.push_back(i);
            r = lst->rank(lisp, positions);
            positions.pop_back();
        }
        return r;
    }
    
    Element* value_on_index(LispE*, Element* idx) {
        Element* r;
        if (last)
            r = lst->rank(lisp, positions);
        else {
            positions.push_back(idx->asInteger());
            r = lst->rank(lisp, positions);
            positions.pop_back();
        }
        return r;
    }
    
    Element* protected_index(LispE*, Element* k)  {
        Element* r;
        if (last)
            r = lst->rank(lisp, positions);
        else {
            positions.push_back(k->asInteger());
            r = lst->rank(lisp, positions);
            positions.pop_back();
        }
        return r;
    }
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
};

//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
// Tensor Template
//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------


#define Tenseur_short Tenseur<short, t_tensor_short, Shorts*>
#define Tenseur_integer Tenseur<long, t_tensor_integer, Integers*>
#define Tenseur_float Tenseur<float, t_tensor_float, Floats*>
#define Tenseur_number Tenseur<double, t_tensor_number, Numbers*>
#define Tenseur_string Tenseur<u_ustring, t_tensor_string, Strings*>
#define Tenseur_stringbyte Tenseur<string, t_tensor_stringbyte, Stringbytes*>


template <typename A, lisp_code T, typename C> class Tenseur : public List {
public:
    vecte<long> shape;
    
    Tenseur<A,T,C>() {
        type = T;
    }
    
    Tenseur<A,T,C>(long nb, vecte<long>& sz) {
        type = T;
        for (long i = 1; i < sz.size(); i++)
            shape.push_back(sz[i]);
    }

    Tenseur<A,T,C>(vecte<long>& sz, Element* n);
    Tenseur<A,T,C>(vecte<long>& sz, A n);
    Tenseur<A,T,C>(vecte<long>& sz, C n);
    Tenseur<A,T,C>(LispE* lisp, vecte<long>& sz, Element* n);
    
    Tenseur<A,T,C>(LispE* lisp, Element* lst, vecte<long>& sz);
    Tenseur<A,T,C>(Tenseur<A,T,C>* tensor);
    
    //We steal the ITEM structure of this list
    Tenseur<A,T,C>(LispE* lisp, List* l);
    
    A zeroValue();
    A asValue(Element* e);
    C provide();
    C provide(LispE* lisp, C n);
    C provide(LispE* lisp);
    C provide(long nb, A v);
    C provide(LispE* lisp, long nb, A v);

    long shapesize() {
        return shape.size();
    }
    
    void dimensions(long& v) {
        long nb = 1;
        for (long i = 0; i < shape.size(); i++)
            nb *= shape[i];
        v += nb;
    }

    Element* check_member(LispE*, Element* the_set);
    
    long nbelements() {
        long nb = 1;
        for (int16_t i = 0; i < shape.size(); i++)
            nb*=shape[i];
        return nb;
    }
    
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
    Element* duplicate_constant(LispE* lisp, bool pair = false) {
        if (status == s_constant)
            return new Tenseur<A,T,C>(this);
        return this;
    }
    
    Element* fullcopy() {
        return new Tenseur<A,T,C>(this);
    }
    
    long tally() {
        long ta = 1;
        for (long i = 0; i < shape.size(); i++)
            ta *= shape[i];
        return ta;
    }

    void setShape() {
        shape.clear();
        Element* e = this;
        while (e->isList()) {
            shape.push_back(e->size());
            e = e->index(0);
        }
    }

    Element* storeRank(LispE* lisp, Element* result, Element* current, vecte<long>& positions, long idx);
    Element* rank(LispE* lisp, vecte<long>& positions);
    
    void build(LispE* lisp, long isz, Element* res, A n);
    void build(LispE* lisp, long isz, Element* res, Element* lst, long& idx);
    void build(LispE* lisp, long isz, Element* res);
    
    
    void build(long isz, Element* res, A n) {
        if (isz == shape.size()-2) {
            C lst;
            for (long i = 0; i < shape[isz]; i++) {
                lst = provide(shape[isz+1], n);
                res->append(lst);
            }
        }
        else {
            Tenseur<A,T,C>* lst;
            for (long i = 0; i < shape[isz]; i++) {
                lst = new Tenseur<A,T,C>(isz+1, shape);
                res->append(lst);
                build(isz+1, lst, n);
            }
        }
    }
    
    void build(long isz, Element* res, Element* lst, long& idx) {
        if (isz == shape.size()-2) {
            C l;
            long i,j;
            for (i = 0; i < shape[isz]; i++) {
                l = provide();
                res->append(l);
                for (j = 0; j < shape[isz+1]; j++) {
                    if (idx == lst->size())
                        idx = 0;
                    l->liste.push_back(asValue(lst->index(idx++)));
                }
            }
        }
        else {
            Tenseur<A,T,C>* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = new Tenseur<A,T,C>(isz+1, shape);
                res->append(l);
                build(isz+1, l, lst, idx);
            }
        }
    }

    void build(long isz, long& idx, Element* res, C lst) {
        if (isz == shape.size()-2) {
            C l;
            long i,j;
            for (i = 0; i < shape[isz]; i++) {
                l = provide();
                res->append(l);
                for (j = 0; j < shape[isz+1]; j++) {
                    if (idx == lst->size())
                        idx = 0;
                    l->liste.push_back(lst->liste[idx++]);
                }
            }
        }
        else {
            Tenseur<A,T,C>* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = new Tenseur<A,T,C>(isz+1, shape);
                res->append(l);
                build(isz+1, idx, l, lst);
            }
        }
    }

    void build(long isz, Element* res) {
        if (isz == shape.size()-2) {
            C l;
            for (long i = 0; i < shape[isz]; i++) {
                l = provide();
                res->append(l);
                l->liste = ((C)liste[i])->liste;
            }
        }
        else {
            Tenseur<A,T,C>* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = new Tenseur<A,T,C>(isz+1, shape);
                res->append(l);
                build(isz+1,l);
            }
        }
    }
    
    char isPureList(long& x, long& y) {
        x = shape[0];
        y = shape[1];
        return a_tensor;
    }
    
    void getShape(vecte<long>& sz) {
        for (long i = 0; i < shape.size(); i++)
            sz.push_back(shape[i]);
    }
    
    char isPureList() {
        return a_tensor;
    }
    
    bool isTensor() {
        return true;
    }
    
    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        return new Tenseur<A,T,C>(this);
    }
    
    Element* transposed(LispE* lisp);
    Element* rotate(LispE* lisp, long axis);
    Element* rotating(LispE* lisp, bool left);
    Element* reversion(LispE* lisp, Element* value, long pos, long axis, bool init);
    Element* reverse(LispE* lisp, bool duplique = true) {
        return rotate(lisp, 1);
    }
    
    void concatenate(LispE* lisp, long isz, Element* res, Element* e) {
        if (res->isValueList()) {
            res->concatenate(lisp, e);
        }
        else {
            for (long i = 0; i < shape[isz]; i++) {
                if (e->isList())
                    concatenate(lisp, isz+1, res->index(i), e->index(i));
                else
                    concatenate(lisp, isz+1, res->index(i), e);
            }
        }
    }
    
    void concatenate(LispE* lisp, Element* e);
    
    
    void setvalue(Element* res, Element* lst);
    
    void setvalue(Tenseur<A,T,C>* lst) {
        setvalue(this, lst);
    }
    
    Element* negate(LispE* lisp);
    
    Element* newInstance() {
        return new Tenseur<A,T,C>(shape, zeroValue());
    }
    
    Element* newInstance(Element* e) {
        return new Tenseur<A,T,C>(shape, e);
    }
    
    Element* pureInstance() {
        return new Tenseur<A,T,C>(0, shape);
    }
    
    bool is_same_tensor(Element* a) {
        if (a->type == type) {
            vecte<long> sa;
            a->getShape(sa);
            return (shape == sa);
        }
        return false;
    }

    Element* newTensor(LispE* lisp, List* l) {
        return new Tenseur<A,T,C>(lisp, l);
    }

    Element* newTensor(bool nb, LispE* lisp, List* l = NULL);

};

//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
// Matrice Template
//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------

class Matrix : public List {
public:
    long size_x, size_y;

    Matrix() {}
    Matrix(List* l) : List(l, 0) {}
    
    bool isTensor() {
        return true;
    }

    bool isMatrix() {
        return true;
    }

    char isPureList() {
        return a_tensor;
    }
    
    char isPureList(long& x, long& y) {
        x = size_x;
        y = size_y;
        return a_tensor;
    }
};

#define Matrice_short Matrice<short, t_matrix_short, t_tensor_short, Shorts*>
#define Matrice_integer Matrice<long, t_matrix_integer, t_tensor_integer, Integers*>
#define Matrice_float Matrice<float, t_matrix_float, t_tensor_float, Floats*>
#define Matrice_number Matrice<double, t_matrix_number, t_tensor_number, Numbers*>
#define Matrice_string Matrice<u_ustring, t_matrix_string, t_tensor_string, Strings*>
#define Matrice_stringbyte Matrice<string, t_matrix_stringbyte, t_tensor_stringbyte, Stringbytes*>

template <typename A, lisp_code T, lisp_code TT, typename C> class Matrice : public Matrix {
public:
    Matrice<A,T,TT,C>() {
        type = T;
    }
    
    Matrice<A,T,TT,C>(long x, long y) {
        type = T;
        size_x = x;
        size_y = y;
    }

    //We steal the ITEM structure of this list
    Matrice<A,T,TT,C>(List* l) : Matrix(l) {
        type = T;
        size_x = l->size();
        size_y = l->index(0)->size();
    }

    Matrice<A,T,TT,C>(long x, long y, Element* n);
    Matrice<A,T,TT,C>(C n, long x, long y);
    Matrice<A,T,TT,C>(LispE* lisp, Element* lst, long x, long y);
    Matrice<A,T,TT,C>(LispE* lisp, long x, C lst, long y);
    Matrice<A,T,TT,C>(LispE* lisp, long x, long y, A n);
    Matrice<A,T,TT,C>(long x, long y, A n);
    Matrice<A,T,TT,C>(LispE* lisp, Matrix* m);
    
    long shapesize() {
        return 2;
    }
    
    void dimensions(long& v) {
        v += size_x*size_y;
    }

    void getShape(vecte<long>& sz) {
        sz.push_back(size_x);
        sz.push_back(size_y);
    }

    void setShape() {
        size_x = size();
        size_y = index(0)->size();
    }

    long tally() {
        return (size_x * size_y);
    }

    Element* check_member(LispE*, Element* the_set);
    
    Element* loop(LispE* lisp, int16_t label,  List* code);
    
    inline A val(long i, long j) {
        return ((C)liste[i])->liste[j];
    }
    
    inline Element* indexe(long i, long j) {
        return liste[i]->index(j);
    }
    
    inline void set(long i, long j, A v) {
        ((C)liste[i])->liste.at(j,v);
    }
    
    inline void mult(long i, long j, A v) {
        ((C)liste[i])->liste[j] *= v;
    }
    
    A zeroValue();
    A asValue(Element* e);
    Element* provideValue(LispE* lisp, A v);
    C provide();
    C provide(long nb, A v);
    C provide(LispE* lisp);
    C provide(LispE* lisp, long nb, A v);
    C provide(LispE* lisp, C v);

    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && !duplicate)
            return this;
        
        return new Matrice<A,T,TT,C>(this);
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(LispE* lisp, bool pair = false) {
        if (status == s_constant)
            return new Matrice<A,T,TT,C>(this);
        return this;
    }
    
    Element* fullcopy() {
        return new Matrice<A,T,TT,C>(this);
    }
    
    Element* inversion(LispE* lisp);
    Element* solve(LispE* lisp, Matrice<A,T,TT,C>* Y);
    double determinant(LispE* lisp);
    Element* ludcmp(LispE* lisp);
    Element* lubksb(LispE* lisp, Integers* indexes, Matrice<A,T,TT,C>* Y = NULL);
    
    void build(LispE* lisp, Element* lst);
    void buildfromvalues(LispE* lisp, C lst);
    
    void setvalue(Matrice<A,T,TT,C>* lst) {
        for (long i = 0; i < lst->size_x; i++) {
            for (long j = 0; j < lst->size_y; j++) {
                liste[i]->replacing(j, lst->index(i)->index(j));
            }
        }
    }
    
    Element* transposed(LispE* lisp);
    Element* rotate(LispE* lisp, long axis);
    Element* rotating(LispE* lisp, bool left);
    Element* reverse(LispE* lisp, bool duplique = true) {
        return rotate(lisp, 1);
    }
    
    void concatenate(LispE* lisp, Element* e);
    
    Element* rank(LispE* lisp, vecte<long>& positions);
    
    Element* negate(LispE* lisp);
    
    Element* newInstance(Element* e) {
        return new Matrice<A,T,TT,C>(size_x, size_y, e);
    }
    
    Element* newInstance() {
        return new Matrice<A,T,TT,C>(size_x, size_y, zeroValue());
    }

    Element* pureInstance() {
        return new Matrice<A,T,TT,C>(size_x, size_y);
    }
    
    bool is_same_tensor(Element* a) {
        if (a->type == type) {
            vecte<long> sa;
            a->getShape(sa);
            return (sa.size() == 2 && size_x == sa[0] && size_y == sa[1]);
        }
        return false;
    }

    Element* newTensor(LispE* lisp, List* l) {
        return new Tenseur<A,TT,C>(lisp, l);
    }

    Element* newTensor(bool nb, LispE* lisp, List* l) {
        if (l == NULL)
            return provide();
        C v = provide();
        for (long i = 0; i < l->size(); i++)
            v->append(l->index(i));
        return v;
    }
};


#endif
