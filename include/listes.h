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

class ITEM {
public:
    Element** buffer;
    long last;
    long sz;
    short status;

    ITEM(long t) {
        status = 0; //this is the reference counter
        last = 0; //this is the last element
        sz = t; //this is the size
        //We always create one more element
        //to handle some room for exchange
        buffer = new Element*[t+1];
        //hence buffer[sz] does exist even though
        //it cannot be accessed through normal means
    }
    
    void reserve(long t) {
        if (t <= sz)
            return;

        Element** tfs;

        //we reallocate our structure
        tfs = new Element*[t+1];

        for (long i = 0; i< last; i++) {
            tfs[i] = buffer[i];
        }

        delete[] buffer;
        buffer = tfs;
        sz = t;
    }

    void reserveforward(long t, long pos) {
        Element** tfs;

        //we reallocate our structure
        tfs = new Element*[t+1];

        long inc = 0;
        //In this case, we skip the pos position
        for (long i = 0; i< last; i++) {
            inc += (i == pos);
            tfs[i+inc] = buffer[i];
        }

        delete[] buffer;
        buffer = tfs;
        sz = t;
    }

    void swap(long left, long right) {
        //We use the fact that the last element exists
        //but cannot be accessed...
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
    }

    bool reverse(long left, long right) {
        if (left >= right)
            return false;
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
        return true;
    }
    
    void erase(long i) {
        if (i < 0 || i >= last)
            return;

        buffer[i]->decrementstatus(1, false);
        
        last--;
        while (i < last) {
            buffer[i] = buffer[i+1];
            i++;
        }
    }

    inline void insert(long pos, Element* val) {
        val->incrementstatus(1, false);

        //If pos is beyond last, it becomes a push
        if (pos > last)
            pos = last;
        
        if (last >= sz) {
            reserveforward(sz<<1, pos);
            buffer[pos] = val;
            last++;
            return;
        }
        
        if (pos == last) {
            buffer[last++] = val;
            return;
        }
        
        // the element is then added in its place
        // if the box is empty we place it at this place
        //If not, all elements are moved to the right.
        //sinon, on deplace tous les elements d'une case vers la droite
        for (long i = last; i > pos; i--) {
            buffer[i] = buffer[i-1];
        }
        buffer[pos] = val;
        last++;
    }

    inline void push_back(Element* val) {
        if (last >= sz)
            reserve(sz<<1);

        val->incrementstatus(1, false);
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
    }
    
    void decrement() {
        if (status)
            return;
        for (long i = 0; i < last; i++) {
            buffer[i]->decrementstatus(1, false);
        }
    }

    ~ITEM() {
        delete[] buffer;
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

    //if this a list, which is not duplicated through CDR calls
    bool nocdr() {
        return !item->status;
    }
    
    inline void setmark(bool v) {
        marking = v;
    }
    
    inline bool mark() {
        return marking;
    }
    
    inline void setusermark(bool v) {
        usermarking = v;
    }
    
    inline bool usermark() {
        return usermarking;
    }
        
    long size() {
        return item->last - home;
    }

    void clear() {
        item->last = home;
    }

    inline void pop_back() {
        item->last--;
    }

    inline void insert(long pos, Element* val) {
        item->insert(pos + home, val);
    }

    inline Element* back() {
        return item->buffer[item->last - 1];
    }

    inline void push_back(Element* val) {
        item->push_back(val);
    }


    inline Element*& operator [](long pos) {
        return item->buffer[pos+home];
    }

    void erase(long i) {
        item->erase(i+home);
    }

    void reserve(long t) {
        item->reserve(t);
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
    
    bool compare(LispE*, List* compare, short instruction, long i, long j);
    void sorting(LispE*, List* f, short instruction, long b, long e);
    void sorting(LispE*, List* f);
    
    void operator =(LIST& z) {
        item->last = home;
        for (long i = 0; i < z.size(); i++)
            push_back(z[i]);
    }

    inline void decrement() {
        if (marking)
            return;
        marking = true;
        item->decrement();
        marking = false;
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
    
    inline Element* at(long i) {
        i += home;
        if (i < 0 || i >= item->last)
            throw std::out_of_range("LIST error");
        return item->buffer[i];
    }
    
    inline bool is_not_empty() {
        return (home != item->last);
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

};

class List : public Element {
public:
    
    /* Method in List
     bool argumentsize(long sz);
     bool Boolean();
     bool isContainer();
     bool isList();
     bool remove(long d);
     bool unify(LispE* lisp, Element* value, bool record);
     char check_match(LispE* lisp, Element* value);
     Element* bit_and(LispE* l, bool local);
     Element* bit_or(LispE* l, bool local);
     Element* bit_xor(LispE* l, bool local);
     Element* car(LispE* lisp);
     Element* composing(LispE*);
     Element* divide(LispE* l, bool local);
     Element* duplicate_constant_container();
     Element* equal(LispE* lisp, Element* e);
     Element* eval(LispE*);
     Element* evalAsInteger(long i, LispE* lisp, long& d);
     Element* evalAsNumber(long i, LispE* lisp, double& d);
     Element* evalAsString(long i, LispE* lisp, wstring& w);
     Element* execute(Element* corps, LispE*);
     Element* executepattern(LispE* lisp, short function_name);
     Element* extraction(LispE* lisp, List*);
     Element* index(long i);
     Element* insert(LispE* lisp, Element* e, long idx);
     Element* reverse(LispE*, bool duplique = true);
     Element* join_in_list(LispE* lisp, wstring& sep);
     Element* leftshift(LispE* l, bool local);
     Element* loop(LispE* lisp, short label,  List* code);
     Element* minus(LispE* l, bool local);
     Element* mod(LispE* l, bool local);
     Element* multiply(LispE* l, bool local);
     Element* plus(LispE* l, bool local);
     Element* power(LispE* l, bool local);
     Element* rightshift(LispE* l, bool local);
     Element* search_all_elements(LispE*, Element* element_value, long idx);
     Element* search_element(LispE*, Element* element_value, long idx);
     Element* search_reverse(LispE*, Element* element_value, long idx);
     Element* value_on_index(LispE*, Element* idx);
     Element* value_on_index(LispE*, long i);
     virtual Element* cdr(LispE* lisp);
     virtual Element* copying(bool duplicate = true);
     virtual Element* last_element(LispE* lisp);
     Element* last();
     virtual long size();
     virtual wstring asString(LispE* lisp);
     void append(Element* e);
     void appendraw(Element* e);
     void clear();
     void decrementstatus(uchar nb, bool top);
     void decrementstatusraw(uchar nb);
     void incrementstatus(uchar nb, bool top);
     void protecting(bool protection);
     void release();
     Element* replace(LispE* lisp, long i, Element* e);
     wstring jsonString(LispE* lisp);
     
     */
    LIST liste;
    bool terminal;
    
    List() : terminal(false), liste(8), Element(t_list) {}
    List(uchar s) : terminal(false), liste(1), Element(t_list, s) {}
    
    //When a CDR is called, we create a copy of LIST, which shared the same ITEM object
    //but with a different home...
    List(List* l, long p) : terminal(false), liste(l->liste, p), Element(t_list) {}

    bool isContainer() {
        return true;
    }
    
    void setterminal(bool v = true) {
        terminal = v;
    }
    
    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    virtual Element* last_element(LispE* lisp);
    
    Element* last() {
        return liste.back();
    }

    void insertion(Element* e, long idx) {
        liste.insert(idx, e);
    }
    
    void front(Element* e) {
        liste.insert(0, e);
    }
    
    void beforelast(Element* e) {
        long sz = liste.size();
        if (!sz)
            liste.push_back(e);
        else
            liste.insert(sz-1, e);
    }

    bool isFunction() {
        return (liste.size() > 1 && liste[0]->label() >= l_lambda && liste[0]->label() <= l_defpat);
    }

    bool isExecutable(LispE*);
    
    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    
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
    
    Element* unique(LispE* lisp);

    void flatten(LispE*, List* l);
    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (status < s_protect && liste.nocdr() && !duplicate)
            return this;
        
        List* l = new List;
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copying(false));
        }
        return l;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant_container(bool pair = false);
    
    bool isList() {
        return true;
    }
    
    bool isNotEmptyList() {
        return (liste.size());
    }
    
    void incrementstatus(uchar nb, bool top) {
        if (status < s_protect)
            status += nb;
    }
    
    void decrementstatus(uchar nb, bool top) {
        if (status > s_destructible && status < s_protect) {
            status -= nb;
        }
        
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    //The status is decremented without destroying the element.
    void decrementstatusraw(uchar nb) {
        if (status > s_destructible && status < s_protect)
            status -= nb;
    }
    
    Element* join_in_list(LispE* lisp, wstring& sep);
    
    Element* extraction(LispE* lisp, List*);
    
    Element* index(long i) {
        return liste[i];
    }
    Element* protected_index(LispE*,long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void release() {
        if (!status) {
            liste.decrement();
            delete this;
        }
    }
    
    Element* equal(LispE* lisp, Element* e);
    
    virtual long size() {
        return liste.size();
    }
    
    //This function is only used to compare the number of
    //parameters of a function and its arguments
    long argumentsize(LispE* lisp, long sz);
    
    Element* car(LispE* lisp);
    virtual Element* cdr(LispE* lisp);
    
    void protecting(bool protection) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
        
        for (long i = 0; i < liste.size(); i++)
            liste[i]->protecting(protection);
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
        if (liste.marking)
            return;
        liste.marking = true;
        liste.usermarking = false;
        for (long i = 0; i < size(); i++) {
            liste[i]->resetusermark();
        }
        liste.marking = false;
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
    
    void append(Element* e) {
        liste.push_back(e);
    }

    bool append_not_null(Element* e) {
        if (e != NULL) {
            liste.push_back(e);
            return true;
        }
        return false;
    }

    void appendraw(Element* e) {
        liste.push_back(e);
    }

    void change(long i, Element* e) {
        liste[i]->decrementstatus(1,false);
        liste[i] = e;
        e->incrementstatus(1,false);
    }

    void changelast(Element* e) {
        liste.back()->decrementstatus(1,false);
        liste[liste.size()-1] = e;
        e->incrementstatus(1,false);
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0)
            throw new Error("Error: position does not exist");
        if (i >= liste.size())
            liste.push_back(e);
        else {
            liste[i]->decrementstatus(1,false);
            liste[i] = e;
        }
        e->incrementstatus(1,false);
        return this;
    }
    
    Element* composing(LispE*, bool compose);
    Element* eval(LispE*);
        
    bool Boolean() {
        return (liste.size());
    }
    
    void evalthread(Element* corps, LispE*);
    Element* evalfunction(Element* corps, LispE*);
    Element* evalpattern(LispE* lisp, short function_name);
    
    void evalAsString(long i, LispE* lisp, wstring& w);
    void evalAsNumber(long i, LispE* lisp, double& d);
    void evalAsInteger(long i, LispE* lisp, long& d);
    
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    short label() {
        return (liste.is_not_empty()?t_list:v_null);
    }

    Element* reverse(LispE*, bool duplique = true);
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, wstring& v);
    
    bool removelast() {
        if (!liste.size())
            return false;
        Element* e = liste.back();
        liste.pop_back();
        e->decrementstatus(1, false);
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
            Element* e = liste.back();
            liste.pop_back();
            e->decrementstatus(1, false);
            return true;
        }
        if (d < 0 || d > liste.size())
            return false;
        Element* e = liste[d];
        liste.erase(d);
        e->decrementstatus(1, false);
        return true;
    }
    
    Element* insert(LispE* lisp, Element* e, long idx);

    virtual void set_current_line(LispE*) {}

    void sameSizeNoTerminalArguments(LispE* lisp, LispE* thread_lisp, Element* body, Element* parameters, bool threading);
    void sameSizeTerminalArguments(LispE* lisp, Element* parameters);
    void differentSizeNoTerminalArguments(LispE* lisp, LispE* thread_lisp, Element* body, Element* parameters, long nbarguments, long defaultarguments, bool threading);
    void differentSizeTerminalArguments(LispE* lisp, Element* parameters, long nbarguments,  long defaultarguments);

    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (status < s_protect)
            liste.decrement_and_clear();
    }

#ifdef MAX_STACK_SIZE_ENABLED
    Element* evall_set_max_stack_size(LispE* lisp);
#endif

    Element* eval_error(LispE* lisp);
    Element* evall_quote(LispE* lisp);
    Element* evall_return(LispE* lisp);
    Element* evall_break(LispE* lisp);
    Element* evall_while(LispE* lisp);
    Element* evall_link(LispE* lisp);
    Element* evall_plus(LispE* lisp);
    Element* evall_minus(LispE* lisp);
    Element* evall_multiply(LispE* lisp);
    Element* evall_power(LispE* lisp);
    Element* evall_leftshift(LispE* lisp);
    Element* evall_rightshift(LispE* lisp);
    Element* evall_divide(LispE* lisp);
    Element* evall_mod(LispE* lisp);
    Element* evall_bitand(LispE* lisp);
    Element* evall_bitor(LispE* lisp);
    Element* evall_bitxor(LispE* lisp);
    Element* evall_plusequal(LispE* lisp);
    Element* evall_minusequal(LispE* lisp);
    Element* evall_multiplyequal(LispE* lisp);
    Element* evall_powerequal(LispE* lisp);
    Element* evall_leftshiftequal(LispE* lisp);
    Element* evall_rightshiftequal(LispE* lisp);
    Element* evall_bitandequal(LispE* lisp);
    Element* evall_bitorequal(LispE* lisp);
    Element* evall_bitxorequal(LispE* lisp);
    Element* evall_divideequal(LispE* lisp);
    Element* evall_modequal(LispE* lisp);
    Element* evall_eq(LispE* lisp);
    Element* evall_neq(LispE* lisp);
    Element* evall_throw(LispE* lisp);
    Element* evall_catch(LispE* lisp);
    Element* evall_maybe(LispE* lisp);
    Element* evall_equal(LispE* lisp);
    Element* evall_different(LispE* lisp);
    Element* evall_lower(LispE* lisp);
    Element* evall_greater(LispE* lisp);
    Element* evall_lowerorequal(LispE* lisp);
    Element* evall_greaterorequal(LispE* lisp);
    Element* evall_max(LispE* lisp);
    Element* evall_min(LispE* lisp);
    Element* evall_atomp(LispE* lisp);
    Element* evall_numberp(LispE* lisp);
    Element* evall_stringp(LispE* lisp);
    Element* evall_consp(LispE* lisp);
    Element* evall_zerop(LispE* lisp);
    Element* evall_nullp(LispE* lisp);
    Element* evall_data(LispE* lisp);
    Element* evall_flip(LispE* lisp);
    Element* evall_select(LispE* lisp);
    Element* evall_compose(LispE* lisp);
    Element* evall_loop(LispE* lisp);
    Element* evall_loopcount(LispE* lisp);
    Element* evall_or(LispE* lisp);
    Element* evall_and(LispE* lisp);
    Element* evall_xor(LispE* lisp);
    Element* evall_ncheck(LispE* lisp);
    Element* evall_check(LispE* lisp);
    Element* evall_ife(LispE* lisp);
    Element* evall_block(LispE* lisp);
    Element* evall_converttoatom(LispE* lisp);
    Element* evall_converttointeger(LispE* lisp);
    Element* evall_converttonumber(LispE* lisp);
    Element* evall_converttostring(LispE* lisp);
    Element* evall_list(LispE* lisp);
    Element* evall_reverse(LispE* lisp);
    Element* evall_cons(LispE* lisp);
    Element* evall_trace(LispE* lisp);
    Element* evall_key(LispE* lisp);
    Element* evall_keyn(LispE* lisp);
    Element* evall_last(LispE* lisp);
    Element* evall_push(LispE* lisp);
    Element* evall_insert(LispE* lisp);
    Element* evall_unique(LispE* lisp);
    Element* evall_pop(LispE* lisp);
    Element* evall_keys(LispE* lisp);
    Element* evall_values(LispE* lisp);
    Element* evall_cond(LispE* lisp);
    Element* evall_not(LispE* lisp);
    Element* evall_nconc(LispE* lisp);
    Element* evall_if(LispE* lisp);
    Element* evall_car(LispE* lisp);
    Element* evall_cdr(LispE* lisp);
    Element* evall_cadr(LispE* lisp);
    Element* evall_label(LispE* lisp);
    Element* evall_setq(LispE* lisp);
    Element* evall_setg(LispE* lisp);
    Element* evall_deflib(LispE* lisp);
    Element* evall_defmacro(LispE* lisp);
    Element* evall_sleep(LispE* lisp);
    Element* evall_wait(LispE* lisp);
    Element* evall_defpat(LispE* lisp);
    Element* evall_defun(LispE* lisp);
    Element* evall_bodies(LispE* lisp);
    Element* evall_lambda(LispE* lisp);
    Element* eval_call_function(LispE* lisp);
    Element* evalt_list(LispE* lisp);
    Element* evall_lock(LispE* lisp);
    Element* evall_sum(LispE* lisp);
    Element* evall_product(LispE* lisp);
    Element* evall_waiton(LispE* lisp);
    Element* evall_trigger(LispE* lisp);
    Element* evall_threadstore(LispE* lisp);
    Element* evall_threadclear(LispE* lisp);
    Element* evall_threadretrieve(LispE* lisp);
    Element* evall_print(LispE* lisp);
    Element* evall_println(LispE* lisp);
    Element* evall_printerr(LispE* lisp);
    Element* evall_printerrln(LispE* lisp);
    Element* evall_prettify(LispE* lisp);
    Element* evall_mark(LispE* lisp);
    Element* evall_resetmark(LispE* lisp);
    Element* evall_atoms(LispE* lisp);
    Element* evall_atomise(LispE* lisp);
    Element* evall_join(LispE* lisp);
    Element* evall_eval(LispE* lisp);
    Element* evall_type(LispE* lisp);
    Element* evall_load(LispE* lisp);
    Element* evall_input(LispE* lisp);
    Element* evall_getchar(LispE* lisp);
    Element* evall_pipe(LispE* lisp);
    Element* evall_flatten(LispE* lisp);
    Element* evall_fread(LispE* lisp);
    Element* evall_fappend(LispE* lisp);
    Element* evall_fwrite(LispE* lisp);
    Element* evall_size(LispE* lisp);
    Element* evall_use(LispE* lisp);
    Element* evall_at_index(LispE* lisp);
    Element* evall_extract(LispE* lisp);
    Element* evall_in(LispE* lisp);
    Element* evall_search(LispE* lisp);
    Element* evall_searchall(LispE* lisp);
    Element* evall_revertsearch(LispE* lisp);
    Element* evall_irange(LispE* lisp);
    Element* evall_range(LispE* lisp);
    Element* evall_mapping(LispE* lisp);
    Element* evall_checking(LispE* lisp);
    Element* evall_folding(LispE* lisp);
    Element* evall_apply(LispE* lisp);
    Element* evall_sort(LispE* lisp);
    Element* evall_zip(LispE* lisp);
    Element* evall_zipwith(LispE* lisp);
    
    bool eval_Boolean(LispE* lisp, short instruction);

};

class Listincode : public List {
public:
    long line;
    long fileidx;
    
    Listincode(long l, long f) : line(l), fileidx(f), List(s_constant) {}
    Listincode(uchar s) : List(s) {}
    Listincode() {}
    
    void set_current_line(LispE*);

};

class Listbreak : public Element {
public:
    
    Listbreak() : Element(l_return, s_constant) {}
  
    bool isBreak() {
        return true;
    }
    
    Element* eval(LispE*) {
        return this;
    }
};

class Listlambda : public List {
public:

    Listlambda() : List() {}
    
    Element* eval(LispE* lisp) {
        return evalfunction(liste[0]->eval(lisp), lisp);
    }
};

class Pair : public List {
public:
    
    Pair() : List() {
        type = t_pair;
    }

    //When a CDR is called, we create a copy of LIST, which shared the same ITEM object
    //but with a different home...
    Pair(Pair* l, long p) : List((List*)l, p) {}

    Element* fullcopy() {
        if (liste.marking)
            return liste.object;
        
        liste.marking = true;
        liste.object = new Pair;
        for (long i = 0; i < liste.size(); i++) {
            liste.object->append(liste[i]->fullcopy());
        }
        
        liste.marking = false;
        return liste.object;
    }

    Element* copying(bool duplicate = true) {
        if (status < s_protect && liste.nocdr() && !duplicate)
            return this;
        
        Pair* l = new Pair;
        for (long i = 0; i < liste.size(); i++)
            l->append(liste[i]->copying(false));
        return l;
    }
    
    Element* cdr(LispE* lisp);
    
    wstring asString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return L"()";
        
        sz -= 1;
        
        wstring buffer(L"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i == sz)
                buffer += L" . ";
            else
                if (i && i < sz)
                    buffer+= L" ";
            
            buffer += liste[i]->stringInList(lisp);
        }
        buffer += L")";
        return buffer;
    }
};

#endif
