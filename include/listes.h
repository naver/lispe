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

typedef Element* (List::*methodEval)(LispE*);

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

    inline void push_raw(Element* val) {
        if (last >= sz)
            reserve(sz<<1);

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

    void decrement(Element* e) {
        if (status)
            return;
        for (long i = 0; i < last; i++) {
            if (e != buffer[i])
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
        
    long _size() {
        return item == NULL?0:item->last - home;
    }

    long size() {
        return item->last - home;
    }

    void clear() {
        item->last = home;
    }

    void clean() {
        item->decrement();
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

    inline void push_raw(Element* val) {
        item->push_raw(val);
    }
    
    inline Element*& operator[](long pos) {
        return item->buffer[pos+home];
    }
    
    inline short get0() {
        return item->buffer[home]->type;
    }

    void erase(long pos) {
        item->erase(pos +home);
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

    LIST liste;
    bool terminal;
    
    List() : terminal(false), liste(8), Element(t_list) {}
    List(uchar s) : terminal(false), liste(1), Element(t_list, s) {}
    
    //In all other case, we "borrow" the ITEM object to create a LIST object that will
    //share the same content. No copy or duplication is necessary.
    //ITEM exposes a "status" value that is used to count the number of times an object has been borrowed
    //to correctly assess when it can be safely deleted.
    //When a CDR is called, it will share this list's item, but with a different "home" value.
    //The "home value" in a LIST object defines where it starts in the internal buffer of ITEM
    List(List* l, long p) : terminal(false), liste(l->liste, p), Element(t_list) {}

    bool isContainer() {
        return true;
    }
    
    void setterminal(bool v = true) {
        terminal = v;
    }
    
    virtual Element* loop(LispE* lisp, short label,  List* code);
    Element* multiloop(LispE* lisp);
    Element* polyloop(LispE* lisp);
    
    long find_element(LispE*, Element* element_value, long idx);
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

    Element* thekeys(LispE* lisp);

    bool isFunction() {
        return (liste.size() > 1 && liste[0]->label() >= l_lambda && liste[0]->label() <= l_defpat);
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
    
    virtual Element* copyatom(uchar s) {
        if (status < s)
            return this;

        List* l = new List;
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copyatom(s));
        }
        return l;
    }

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
    

    Element* quoted(LispE*);
    Element* unique(LispE* lisp);
    Element* rotate(bool left);

    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    virtual Element* duplicate_constant_container(bool pair = false);
    
    virtual bool isList() {
        return true;
    }
    
    bool isLambda() {
        return (liste.size() && liste.item->buffer[0]->type == l_lambda);
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
    void decrementSansDelete(uchar nb) {
        if (status > s_destructible && status < s_protect)
            status -= nb;
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    
    Element* index(long i) {
        return liste[i];
    }
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);

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
    
    virtual void rawrelease() {
        if (!status) {
            for (long i = 0; i < liste.size(); i++)
                liste[i]->release();
            liste.clear();
            liste.decrement();
            delete this;
        }
    }
    
    Element* equal(LispE* lisp, Element* e);
    
    virtual long size() {
        return liste.size();
    }
    
    virtual long shapesize() {
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
    
    void append(LispE* lisp, u_ustring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);

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
        liste.push_raw(e);
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
    
    void replacing(long i, Element* e) {
        if (e == liste[i])
            return;
        
        liste[i]->decrementstatus(1,false);
        liste[i] = e;
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
    virtual Element* eval(LispE*);
        
    bool Boolean() {
        return (liste.size());
    }
    
    void evalthread(LispE*, Element* corps);
    Element* evalfunction(LispE*, Element* corps);
    
    Element* eval_pattern(LispE* lisp, short function_name);
    Element* eval_function(LispE*, Element* corps);
    Element* eval_thread(LispE*, Element* corps);
    Element* eval_data(LispE*, Element* corps);
    Element* eval_lambda(LispE*, Element* corps);
    
    void evalAsUString(long i, LispE* lisp, u_ustring& w);
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
    void storevalue(LispE*, u_ustring& v);
    
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
    
    virtual char isPureList() {
        for (long i = 0; i < liste.size(); i++) {
            if (liste[i]->isList())
                return 1;
        }
        return 2;
    }

    virtual char isPureList(long& x, long& y) {
        x = liste.size();
        if (x) {
            if (liste[0]->isList()) {
                y = liste[0]->size();
                for (long i = 1; i < x; i++) {
                    if (!liste[i]->isList() || y != liste[i]->size())
                        return 0;
                }
                return 1;
            }
            y = 1;
            for (long i = 1; i < x; i++) {
                if (liste[i]->isList())
                    return 0;
            }
        }
        return 2;
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

    bool checkShape(long depth, vector<long>& sz) {
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
    
    void getShape(vector<long>& sz) {
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

    virtual Element* check_member(LispE*, Element* the_set);
    
    Element* insert(LispE* lisp, Element* e, long idx);

    void sameSizeNoTerminalArguments(LispE* lisp, Element* body, Element* parameters);
    void differentSizeNoTerminalArguments(LispE* lisp, Element* body, Element* parameters, long nbarguments, long defaultarguments);

    void sameSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* body, Element* parameters);
    void differentSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* body, Element* parameters, long nbarguments, long defaultarguments);

    void differentSizeTerminalArguments(LispE* lisp, Element* parameters, long nbarguments,  long defaultarguments);
    void sameSizeTerminalArguments(LispE* lisp, Element* parameters);

    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (status < s_protect)
            liste.decrement_and_clear();
    }

    void clear(Element* e) {
        if (status < s_protect)
            liste.decrement_and_clear(e);
    }

    void clean() {
        liste.clean();
    }
    
    Element* transformargument(LispE*);
    
#ifdef MAX_STACK_SIZE_ENABLED
    Element* evall_set_max_stack_size(LispE* lisp);
#endif

    Element* eval_call_function(LispE* lisp);
    Element* eval_error(LispE* lisp);
    
    Element* evall_and(LispE* lisp);
    Element* evall_apply(LispE* lisp);
    Element* evall_at_index(LispE* lisp);
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
    Element* evall_cadr(LispE* lisp);
    Element* evall_car(LispE* lisp);
    Element* evall_catch(LispE* lisp);
    Element* evall_cdr(LispE* lisp);
    Element* evall_check(LispE* lisp);
    Element* evall_checking(LispE* lisp);
    Element* evall_compose(LispE* lisp);
    Element* evall_concatenate(LispE* lisp);
    Element* evall_cond(LispE* lisp);
    Element* evall_cons(LispE* lisp);
    Element* evall_consp(LispE* lisp);
    Element* evall_converttoatom(LispE* lisp);
    Element* evall_converttointeger(LispE* lisp);
    Element* evall_converttonumber(LispE* lisp);
    Element* evall_converttostring(LispE* lisp);
    Element* evall_data(LispE* lisp);
    Element* evall_deflib(LispE* lisp);
    Element* evall_deflibpat(LispE* lisp);
    Element* evall_defmacro(LispE* lisp);
    Element* evall_defpat(LispE* lisp);
    Element* evall_defun(LispE* lisp);
    Element* evall_determinant(LispE* lisp);
    Element* evall_different(LispE* lisp);
    Element* evall_divide(LispE* lisp);
    Element* evall_divideequal(LispE* lisp);
    Element* evall_duplicate(LispE* lisp);
    Element* evall_elapse(LispE* lisp);
    Element* evall_eq(LispE* lisp);
    Element* evall_equal(LispE* lisp);
    Element* evall_equalonezero(LispE* lisp);
    Element* evall_eval(LispE* lisp);
    Element* evall_extend(LispE* lisp);
    Element* evall_extract(LispE* lisp);
    Element* evall_factorial(LispE* lisp);
    Element* evall_fappend(LispE* lisp);
    Element* evall_filterlist(LispE* lisp);
    Element* evall_flatten(LispE* lisp);
    Element* evall_flip(LispE* lisp);
    Element* evall_folding(LispE* lisp);
    Element* evall_fread(LispE* lisp);
    Element* evall_fwrite(LispE* lisp);
    Element* evall_getchar(LispE* lisp);
    Element* evall_greater(LispE* lisp);
    Element* evall_greaterorequal(LispE* lisp);
    Element* evall_if(LispE* lisp);
    Element* evall_ife(LispE* lisp);
    Element* evall_in(LispE* lisp);
    Element* evall_index(LispE* lisp);
    Element* evall_infix(LispE* lisp);
    Element* evall_innerproduct(LispE* lisp);
    Element* evall_input(LispE* lisp);
    Element* evall_insert(LispE* lisp);
    Element* evall_integers(LispE* lisp);
    Element* evall_invert(LispE* lisp);
    Element* evall_iota(LispE* lisp);
    Element* evall_iota0(LispE* lisp);
    Element* evall_irange(LispE* lisp);
    Element* evall_join(LispE* lisp);
    Element* evall_key(LispE* lisp);
    Element* evall_keyn(LispE* lisp);
    Element* evall_keys(LispE* lisp);
    Element* evall_label(LispE* lisp);
    Element* evall_lambda(LispE* lisp);
    Element* evall_last(LispE* lisp);
    Element* evall_leftshift(LispE* lisp);
    Element* evall_leftshiftequal(LispE* lisp);
    Element* evall_link(LispE* lisp);
    Element* evall_list(LispE* lisp);
    Element* evall_load(LispE* lisp);
    Element* evall_lock(LispE* lisp);
    Element* evall_loop(LispE* lisp);
    Element* evall_loopcount(LispE* lisp);
    Element* evall_lower(LispE* lisp);
    Element* evall_lowerorequal(LispE* lisp);
    Element* evall_lubksb(LispE* lisp);
    Element* evall_ludcmp(LispE* lisp);
    Element* evall_maplist(LispE* lisp);
    Element* evall_mapping(LispE* lisp);
    Element* evall_mark(LispE* lisp);
    Element* evall_matrix(LispE* lisp);
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
    Element* evall_neq(LispE* lisp);
    Element* evall_not(LispE* lisp);
    Element* evall_nullp(LispE* lisp);
    Element* evall_numberp(LispE* lisp);
    Element* evall_numbers(LispE* lisp);
    Element* evall_or(LispE* lisp);
    Element* evall_outerproduct(LispE* lisp);
    Element* evall_pipe(LispE* lisp);
    Element* evall_plus(LispE* lisp);
    Element* evall_plusequal(LispE* lisp);
    Element* evall_pop(LispE* lisp);
    Element* evall_power(LispE* lisp);
    Element* evall_powerequal(LispE* lisp);
    Element* evall_prettify(LispE* lisp);
    Element* evall_print(LispE* lisp);
    Element* evall_printerr(LispE* lisp);
    Element* evall_printerrln(LispE* lisp);
    Element* evall_println(LispE* lisp);
    Element* evall_product(LispE* lisp);
    Element* evall_push(LispE* lisp);
    Element* evall_quote(LispE* lisp);
    Element* evall_range(LispE* lisp);
    Element* evall_rank(LispE* lisp);
    Element* evall_reduce(LispE* lisp);
    Element* evall_resetmark(LispE* lisp);
    Element* evall_return(LispE* lisp);
    Element* evall_reverse(LispE* lisp);
    Element* evall_revertsearch(LispE* lisp);
    Element* evall_rho(LispE* lisp);
    Element* evall_rightshift(LispE* lisp);
    Element* evall_rightshiftequal(LispE* lisp);
    Element* evall_rotate(LispE* lisp);
    Element* evall_scan(LispE* lisp);
    Element* evall_search(LispE* lisp);
    Element* evall_searchall(LispE* lisp);
    Element* evall_select(LispE* lisp);
    Element* evall_set(LispE* lisp);
    Element* evall_set_at(LispE* lisp);
    Element* evall_setg(LispE* lisp);
    Element* evall_setn(LispE* lisp);
    Element* evall_setq(LispE* lisp);
    Element* evall_sign(LispE* lisp);
    Element* evall_size(LispE* lisp);
    Element* evall_sleep(LispE* lisp);
    Element* evall_solve(LispE* lisp);
    Element* evall_sort(LispE* lisp);
    Element* evall_stringp(LispE* lisp);
    Element* evall_strings(LispE* lisp);
    Element* evall_sum(LispE* lisp);
    Element* evall_tensor(LispE* lisp);
    Element* evall_threadclear(LispE* lisp);
    Element* evall_threadretrieve(LispE* lisp);
    Element* evall_threadstore(LispE* lisp);
    Element* evall_throw(LispE* lisp);
    Element* evall_trace(LispE* lisp);
    Element* evall_transpose(LispE* lisp);
    Element* evall_trigger(LispE* lisp);
    Element* evall_type(LispE* lisp);
    Element* evall_unique(LispE* lisp);
    Element* evall_use(LispE* lisp);
    Element* evall_values(LispE* lisp);
    Element* evall_wait(LispE* lisp);
    Element* evall_waiton(LispE* lisp);
    Element* evall_while(LispE* lisp);
    Element* evall_xor(LispE* lisp);
    Element* evall_zerop(LispE* lisp);
    Element* evall_zip(LispE* lisp);
    Element* evall_zipwith(LispE* lisp);
    
    Element* evalt_data(LispE* lisp);
    Element* evalt_function(LispE* lisp);
    Element* evalt_lambda(LispE* lisp);
    Element* evalt_list(LispE* lisp);
    Element* evalt_pattern(LispE* lisp);
    Element* evalt_thread(LispE* lisp);

    bool eval_Boolean(LispE* lisp, short instruction);

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
    

    virtual Element* newInstance() {
        return new List;
    }
    
    virtual Element* newInstance(Element* v);
    virtual bool incode() {
        return false;
    }

};

class Listpool : public List {
public:
    LispE* lisp;
    Listpool(LispE* l) : lisp(l) {}

    void decrementstatus(uchar nb, bool top);
    void release();
    void rawrelease();
    Element* newInstance();
    Element* fullcopy();
    Element* copyatom(uchar s);
    Element* copying(bool duplicate = true);

};

class Listargumentquote : public List {
public:
    
    Listargumentquote(List* l) : List(l, 0) {}
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listargumentdata : public List {
public:
    
    Listargumentdata(List* l) : List(l, 0) {}
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listargumentseparator : public List {
public:
    
    Listargumentseparator(List* l) : List(l, 0) {}
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listargumentset : public List {
public:
    
    Listargumentset(List* l) : List(l, 0) {}
    
    bool isList() {
        return false;
    }
    short label() {
        if (liste[0]->type == l_set)
            return t_set;
        return t_setn;
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
};



class Listargumentlabel : public List {
public:
    short ilabel;
    
    Listargumentlabel(List* l, short lab) : ilabel(lab), List(l, 0) {}
    
    bool unify(LispE* lisp, Element* value, bool record);
};

class Listargumentfunction : public List {
public:
    Element* argument;
    
    Listargumentfunction(List* l, Element* e) : argument(e), List(l, 0) {}
    bool unify(LispE* lisp, Element* value, bool record);
};


class Listincode : public List {
public:
    long line;
    long fileidx;
    
    Listincode(long l, long f) : List(s_constant) {
		line = l;
		fileidx = f;
	}
    Listincode(uchar s) : List(s) {
		line = 0;
		fileidx = 0;
	}
    Listincode() {
		line = 0;
		fileidx = 0;
	}
    
    Element* eval(LispE*);
    Element* evall_infix(LispE* lisp);
    bool incode() {
        return true;
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

class Listlambda : public List {
public:

    Listlambda() : List() {}
    
    Element* eval(LispE* lisp) {
        return evalfunction(lisp, liste[0]->eval(lisp));
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

    u_ustring asUString(LispE* lisp) {
        long sz = liste.size();
        if (!sz)
            return U"()";
        
        sz -= 1;
        
        u_ustring buffer(U"(");
        
        for (long i = 0; i <= sz; i++) {
            if (i == sz)
                buffer += U" . ";
            else
                if (i && i < sz)
                    buffer+= U" ";
            
            buffer += liste[i]->stringInUList(lisp);
        }
        buffer += U")";
        return buffer;
    }

};

class Numbers : public Element {
public:
    
    Constnumber exchange_value;
    vector<double> liste;
    
    Numbers() : Element(t_numbers), exchange_value(0) {}
    Numbers(Numbers* n) : liste(n->liste), Element(t_numbers), exchange_value(0) {}
    Numbers(uchar s) : Element(t_numbers, s), exchange_value(0) {}
    Numbers(long nb, double v) : liste(nb,v), Element(t_numbers), exchange_value(0) {}

    virtual Element* newInstance() {
        return new Numbers;
    }

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

    Element* inversion(LispE* lisp);
    Element* equal(LispE* lisp, Element* e);
    Element* minimum(LispE*);
    Element* maximum(LispE*);

    Element* rotate(LispE* lisp, long axis) {
        Numbers* n = new Numbers;
        for (long i = liste.size()-1; i >= 0; i--)
            n->liste.push_back(liste[i]);
        return n;
    }

    Element* check_member(LispE*, Element* the_set) {
        Numbers* n = new Numbers;
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

    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }

    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* last_element(LispE* lisp);
    
    void swap(long i, long j) {
        double v = liste[i];
        liste[i] = liste[j];
        liste[j] = v;
    }

    void insertion(Element* e, long idx) {
        liste.insert(liste.begin()+idx, e->asNumber());
    }
    
    void front(Element* e) {
        liste.insert(liste.begin(), e->asNumber());
    }
    
    void beforelast(Element* e) {
        long sz = liste.size();
        if (!sz)
            liste.push_back(e->asNumber());
        else
            liste.insert(liste.begin()+sz-1, e->asNumber());
    }

    Element* thekeys(LispE* lisp);

    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    
    virtual Element* fullcopy() {
        Numbers* e = new Numbers;
        e->liste = liste;
        return e;
    }

    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (status < s_protect && !duplicate)
            return this;

        return new Numbers(this);
    }
    
    virtual Element* copyatom(uchar s) {
        if (status < s)
            return this;

        return new Numbers(this);
    }


    Element* unique(LispE* lisp);
    Element* rotate(bool left);

    
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
            delete this;
        }
    }
    
    //The status is decremented without destroying the element.
    void decrementSansDelete(uchar nb) {
        if (status > s_destructible && status < s_protect)
            status -= nb;
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.number = liste[i];
        return &exchange_value;
    }
    
    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    
    Element* protected_index(LispE*,long i);
    
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
    
    void protecting(bool protection) {
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
        liste[i] = e->asNumber();
    }

    void changelast(Element* e) {
        liste[liste.size()-1] = e->asNumber();
    }
    
    void replacing(long i, Element* e) {
        liste[i] = e->asNumber();
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0)
            throw new Error("Error: position does not exist");
        if (i >= liste.size())
            liste.push_back(e->asNumber());
        else {
            liste[i] = e->asNumber();
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
        
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    short label() {
        return (liste.empty()?t_numbers:v_null);
    }

    Element* reverse(LispE*, bool duplique = true);
    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
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
        liste.erase(liste.begin()+d);
        return true;
    }
    
    void getShape(vector<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return 3;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return 3;
    }

    Element* insert(LispE* lisp, Element* e, long idx);


    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (status < s_protect)
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


    bool compare(LispE* lisp, List* comparison, short instruction, long i, long j);
    void sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax);
    void sorting(LispE* lisp, List* comparison);
};

class Numberspool : public Numbers {
public:
    LispE* lisp;
    Numberspool(LispE* l) : lisp(l) {}

    Element* newInstance();
    
    void decrementstatus(uchar nb, bool top);
    void release();
    Element* fullcopy();
    Element* copyatom(uchar s);
    Element* copying(bool duplicate = true);

};

class Integers : public Element {
public:
    
    Constinteger exchange_value;
    vector<long> liste;
    
    Integers() : Element(t_integers), exchange_value(0) {}
    Integers(uchar s) : Element(t_integers, s), exchange_value(0) {}
    Integers(long nb, long v) : liste(nb, v), Element(t_integers), exchange_value(0) {}
    Integers(Integers* i) : liste(i->liste), Element(t_integers), exchange_value(0) {}

    virtual Element* newInstance() {
        return new Integers;
    }

    Element* inversion(LispE* lisp);
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

    Element* check_member(Element* the_set) {
        Integers* n = new Integers;
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
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }

    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* last_element(LispE* lisp);
    
    void insertion(Element* e, long idx) {
        liste.insert(liste.begin()+idx, e->asInteger());
    }
    
    void swap(long i, long j) {
        long v = liste[i];
        liste[i] = liste[j];
        liste[j] = v;
    }
    
    void front(Element* e) {
        liste.insert(liste.begin(), e->asInteger());
    }
    
    void beforelast(Element* e) {
        long sz = liste.size();
        if (!sz)
            liste.push_back(e->asInteger());
        else
            liste.insert(liste.begin()+sz-1, e->asInteger());
    }

    Element* thekeys(LispE* lisp);

    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    
    virtual Element* fullcopy() {
        Integers* e = new Integers;
        e->liste = liste;
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotate(bool left);

    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (status < s_protect && !duplicate)
            return this;

        Integers* e = new Integers;
        e->liste = liste;
        return e;
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
            delete this;
        }
    }
    
    //The status is decremented without destroying the element.
    void decrementSansDelete(uchar nb) {
        if (status > s_destructible && status < s_protect)
            status -= nb;
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.integer = liste[i];
        return &exchange_value;
    }
    
    Element* equal(LispE* lisp, Element* e);
    Element* minimum(LispE*);
    Element* maximum(LispE*);

    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    
    Element* protected_index(LispE*,long i);
    
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
    
    void protecting(bool protection) {
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
        liste[i] = e->asInteger();
    }

    void changelast(Element* e) {
        liste[liste.size()-1] = e->asInteger();
    }
    
    void replacing(long i, Element* e) {
        liste[i] = e->asInteger();
    }
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0)
            throw new Error("Error: position does not exist");
        if (i >= liste.size())
            liste.push_back(e->asInteger());
        else {
            liste[i] = e->asInteger();
        }
        return this;
    }
    
    bool Boolean() {
        return (liste.size());
    }
        
    //The label of _EMPTYLIST is v_null
    //We can then compare with () as if it was nil
    short label() {
        return (liste.empty()?t_integers:v_null);
    }

    Element* reverse(LispE*, bool duplique = true);
    
    Element* rotate(LispE* lisp, long axis) {
        Integers* n = new Integers;
        for (long i = liste.size()-1; i >= 0; i--)
            n->liste.push_back(liste[i]);
        return n;
    }

    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
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
        liste.erase(liste.begin()+d);
        return true;
    }
    
    void getShape(vector<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return 3;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return 3;
    }

    Element* insert(LispE* lisp, Element* e, long idx);

    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (status < s_protect)
            liste.clear();
    }
    
    virtual Element* copyatom(uchar s) {
        if (status < s)
            return this;

        return new Integers(this);
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

    bool compare(LispE* lisp, List* comparison, short instruction, long i, long j);
    void sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax);
    void sorting(LispE* lisp, List* comparison);
};

class Integerspool : public Integers {
public:
    LispE* lisp;
    Integerspool(LispE* l) : lisp(l) {}

    Element* newInstance();
    void decrementstatus(uchar nb, bool top);
    void release();
    Element* fullcopy();
    Element* copyatom(uchar s);
    Element* copying(bool duplicate = true);

};


class Matrice : public List {
public:
    long size_x, size_y;

    Matrice() {
        type = t_matrix;
    }
    
    Matrice(long x, long y, Element* n) {
        type = t_matrix;
        size_x = x;
        size_y = y;
        Numbers* l;
        double v = n->asNumber();
        for (long i = 0; i < size_x; i++) {
            l = new Numbers(size_y, v);
            liste.push_back(l);
        }
    }

    Matrice(Element* lst, long x, long y) {
        type = t_matrix;
        size_x = x;
        size_y = y;
        build(lst);
    }

    Matrice(long x, long y, double n) {
        type = t_matrix;
        size_x = x;
        size_y = y;
        Numbers* l;

        for (long i = 0; i < size_x; i++) {
            l = new Numbers(size_y, n);
            liste.push_back(l);
        }
    }

    Matrice(Matrice* m) {
        type = t_matrix;
        size_x = m->size_x;
        size_y = m->size_y;
        Numbers* l;
        for (long i = 0; i < size_x; i++) {
            l = new Numbers;
            l->liste = ((Numbers*)m->liste[i])->liste;
            liste.push_back(l);
        }
    }
    
    //We steal the ITEM structure of this list
    Matrice(List* l) : List(l,0) {
        type = t_matrix;
        size_x = l->size();
        size_y = l->index(0)->size();
    }

    long shapesize() {
        return 2;
    }

    Element* check_member(LispE*, Element* the_set);
    
    Element* loop(LispE* lisp, short label,  List* code);
    
    inline double val(long i, long j) {
        return ((Numbers*)liste[i])->liste[j];
    }

    inline Element* indexe(long i, long j) {
        return liste[i]->index(j);
    }
    
    inline void set(long i, long j, double v) {
        ((Numbers*)liste[i])->liste[j] = v;
    }
    
    inline void mult(long i, long j, double v) {
        ((Numbers*)liste[i])->liste[j] *= v;
    }

    char isPureList(long& x, long& y) {
        x = size_x;
        y = size_y;
        return 1;
    }

    char isPureList() {
        return 4;
    }
    
    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (status < s_protect && liste.nocdr() && !duplicate)
            return this;
        
        return new Matrice(this);
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant_container(bool pair = false) {
        if (status == s_constant)
            return new Matrice(this);
        return this;
    }

    Element* fullcopy() {
        return new Matrice(this);
    }

    Element* inversion(LispE* lisp);
    Element* solve(LispE* lisp, Matrice* Y);
    double determinant();
    Element* ludcmp(LispE* lisp);
    Element* lubksb(LispE* lisp, Integers* indexes, Matrice* Y = NULL);

    void build(Element* lst) {
        Numbers* l;
        long idx = 0;
        for (long x = 0; x < size_x; x++) {
            l = new Numbers;
            liste.push_back(l);
            for (long y = 0; y < size_y; y++) {
                if (idx == lst->size())
                    idx = 0;
                l->liste.push_back(lst->index(idx++)->asNumber());
            }
        }
    }

    void combine(LispE* lisp, long isz1, long isz2, Element* l1, Element* l2, List* action) {
        if (!l1->isList() && !l2->isList()) {
            action->liste[1] = l1;
            action->liste[2] = l2;
            Element* e = action->eval(lisp);
            liste[isz1]->replacing(isz2, e);
            e->release();
            return;
        }
        
        if (l1->isList()) {
            for (long i1 = 0; i1 < l1->size(); i1++) {
                combine(lisp, i1, isz2, l1->index(i1), l2, action);
            }
        }
        if (l2->isList()) {
            for (long i2 = 0; i2 < l2->size(); i2++) {
                combine(lisp, isz1, i2, l1, l2->index(i2), action);
            }
        }
    }
    
    void combine(LispE* lisp, Element* l1, Element* l2, List* action) {
        combine(lisp, 0, 0, l1, l2, action);
    }
    
    void setvalue(Matrice* lst) {
        for (long i = 0; i < lst->size_x; i++) {
            for (long j = 0; j < lst->size_y; j++) {
                liste[i]->replacing(j, lst->index(i)->index(j));
            }
        }
    }
    
    Element* transposed(LispE* lisp);
    Element* rotate(LispE* lisp, long axis);
    Element* reverse(LispE* lisp, bool duplique = true) {
        return rotate(lisp, 1);
    }

    void concatenate(LispE* lisp, Element* e) {
        if (e->isList()) {
            if (e->size() != size_x)
                throw new Error("Error: Length error");
            for (long i = 0; i < size_x; i++) {
                liste[i]->concatenate(lisp, e->index(i));
            }
        }
        else {
            for (long i = 0; i < size_x; i++) {
                liste[i]->concatenate(lisp, e);
            }
        }
    }
    
    Element* rank(LispE* lisp, vector<long>& positions);
    
    Element* newInstance(Element* e) {
        return new Matrice(size_x, size_y, e);
    }
    
    Element* newInstance() {
        return new Matrice(size_x, size_y, 0.0);
    }

};

class Tenseur : public List {
public:
    vector<long> shape;

    Tenseur() {
        type = t_tensor;
    }
    
    Tenseur(std::vector<long>& sz, Element* n) {
        type = t_tensor;
        shape = sz;
        if (shape.size())
            build(0,this, n->asNumber());
    }
    
    Tenseur(std::vector<long>& sz, double n) {
        type = t_tensor;
        shape = sz;
        if (shape.size())
            build(0,this, n);
    }
    
    Tenseur(Element* lst, std::vector<long>& sz) {
        type = t_tensor;
        shape = sz;
        if (shape.size()) {
            long idx = 0;
            build(0,this, lst, idx);
        }
    }

    Tenseur(Tenseur* tensor) {
        type = t_tensor;
        shape = tensor->shape;
        tensor->build(0, this);
    }

    //We steal the ITEM structure of this list
    Tenseur(List* l) : List(l, 0) {
        type = t_tensor;
        Element* e = l;
        while (e->isList()) {
            shape.push_back(e->size());
            e = e->index(0);
        }
    }
    
    long shapesize() {
        return shape.size();
    }

    Element* check_member(LispE*, Element* the_set);
    
    long nbelements() {
        long nb = 1;
        for (short i = 0; i < shape.size(); i++)
            nb*=shape[i];
        return nb;
    }
    
    
    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* duplicate_constant_container(bool pair = false) {
        if (status == s_constant)
            return new Tenseur(this);
        return this;
    }
    
    Element* fullcopy() {
        return new Tenseur(this);
    }

    Element* storeRank(Element* current, vector<long>& positions, long idx) {
        bool last = false;
        if (idx == shape.size() - 1) {
            last = true;
        }
        
        long p_idx = -1;
        if (idx < positions.size())
            p_idx = positions[idx];
        
        if (p_idx == -1) {
            if (last)
                return new Numbers((Numbers*)current);
            
            Element* result;
            Element* e = storeRank(current->index(0), positions, idx+1);
            if (e->type == t_number)
                result = new Numbers;
            else
                result = new List;
            result->append(e);
            for (p_idx = 1; p_idx < shape[idx]; p_idx++) {
                result->append(storeRank(current->index(p_idx), positions, idx+1));
            }
            return result;
        }

        if (last)
            return current->index(p_idx);
        return storeRank(current->index(p_idx), positions, idx+1);
    }
    
    Element* rank(LispE* lisp, vector<long>& positions);
    
    void build(long isz, Element* res, double n) {
        if (isz == shape.size()-2) {
            Numbers* lst;
            for (long i = 0; i < shape[isz]; i++) {
                lst = new Numbers(shape[isz+1], n);
                res->append(lst);
            }
        }
        else {
            List* lst;
            for (long i = 0; i < shape[isz]; i++) {
                lst = new List;
                res->append(lst);
                build(isz+1, lst, n);
            }
        }
    }

    void build(long isz, Element* res, Element* lst, long& idx) {
        if (isz == shape.size()-2) {
            Numbers* l;
            long i,j;
            for (i = 0; i < shape[isz]; i++) {
                l = new Numbers;
                res->append(l);
                for (j = 0; j < shape[isz+1]; j++) {
                    if (idx == lst->size())
                        idx = 0;
                    l->liste.push_back(lst->index(idx++)->asNumber());
                }
            }
        }
        else {
            List* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = new List;
                res->append(l);
                build(isz+1, l, lst, idx);
            }
        }
    }

    void build(long isz, Element* res) {
        if (isz == shape.size()-2) {
            Numbers* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = new Numbers;
                res->append(l);
                l->liste = ((Numbers*)liste[i])->liste;
            }
        }
        else {
            List* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = new List;
                res->append(l);
                build(isz+1,l);
            }
        }
    }

    void combine(LispE* lisp, vector<long>& isz1,vector<long>& isz2, Element* l1, Element* l2, List* action) {
        if (!l1->isList() && !l2->isList()) {
            if (!isz1.size() || !isz2.size())
                return;
            
            action->liste[1] = l1;
            action->liste[2] = l2;
            Element* e = action->eval(lisp);
            Element* r = this;
            long i;
            for (i = 0; i < isz1.size(); i++) {
                r = r->index(isz1[i]);
            }
            for (i = 0; i < isz2.size()-1; i++) {
                r = r->index(isz2[i]);
            }
            r->replacing(isz2.back(), e);
            e->release();
            return;
        }
        
        if (l1->isList()) {
            for (long i1 = 0; i1 < l1->size(); i1++) {
                isz1.push_back(i1);
                combine(lisp, isz1, isz2, l1->index(i1), l2, action);
                isz1.pop_back();
            }
        }
        if (l2->isList()) {
            for (long i2 = 0; i2 < l2->size(); i2++) {
                isz2.push_back(i2);
                combine(lisp, isz1, isz2, l1, l2->index(i2), action);
                isz2.pop_back();
            }
        }
    }
    
    void combine(LispE* lisp, Element* l1, Element* l2, List* action) {
        vector<long> isz1;
        vector<long> isz2;
        combine(lisp, isz1, isz2, l1, l2, action);
    }
    
    char isPureList(long& x, long& y) {
        x = shape[0];
        y = shape[1];
        return 1;
    }

    void getShape(vector<long>& sz) {
        sz = shape;
    }
    
    char isPureList() {
        return 4;
    }
    
    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (status < s_protect && liste.nocdr() && !duplicate)
            return this;
        
        return new Tenseur(this);
    }

    Element* transposed(LispE* lisp);
    Element* rotate(LispE* lisp, long axis);
    Element* reversion(LispE* lisp, Element* value, long pos, long axis, bool init);
    Element* reverse(LispE* lisp, bool duplique = true) {
        return rotate(lisp, shape.size()-1);
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
    
    void concatenate(LispE* lisp, Element* e) {
        if (e->isList()) {
            vector<long> sz;
            e->getShape(sz);
            for (long i = 0; i < sz.size()-1; i++) {
                if (sz[i] != shape[i])
                    throw new Error("Error: Incompatible dimensions");
            }
        }
        
        concatenate(lisp, 0, this, e);
    }

    
    void setvalue(Element* res, Element* lst) {
        if (lst->type == t_numbers) {
            for (long i = 0; i < lst->size(); i++) {
                res->replacing(i, lst->index(i));
            }
        }
        else {
            for (long i = 0; i < lst->size(); i++) {
                setvalue(res->index(i), lst->index(i));
            }
        }            
    }
    
    void setvalue(Tenseur* lst) {
        setvalue(this, lst);
    }

    Element* newInstance() {
        return new Tenseur(shape, 0.0);
    }

    Element* newInstance(Element* e) {
        return new Tenseur(shape, e);
    }
};



class Strings : public Element {
public:
    
    vector<u_ustring> liste;
    Conststring exchange_value;
    
    Strings() : exchange_value(U""), Element(t_strings) {}
    Strings(long nb, wstring w) : exchange_value(U""), Element(t_strings) {
        u_pstring u = _w_to_u(w);
        while (nb) {
            liste.push_back(u);
            nb--;
        }
    }
    Strings(long nb, u_ustring v) : exchange_value(U""), Element(t_strings) {
        while (nb) {
            liste.push_back(v);
            nb--;
        }
    }

    Strings(Strings* s) : liste(s->liste), exchange_value(U""), Element(t_strings) {}
    
    virtual Element* newInstance() {
        return new Strings;
    }

    Element* newInstance(Element* v) {
        return new Strings(liste.size(), v->asString(NULL));
    }

    Element* check_member(LispE* lisp, Element* the_set) {
        Strings* n = new Strings;
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
    
    bool isContainer() {
        return true;
    }
    
    bool isValueList() {
        return true;
    }

    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* last_element(LispE* lisp);
    
    void insertion(Element* e, long idx) {
        liste.insert(liste.begin()+idx, e->asUString(NULL));
    }
    
    void swap(long i, long j) {
        u_ustring v = liste[i];
        liste[i] = liste[j];
        liste[j] = v;
    }
    
    void front(Element* e) {
        liste.insert(liste.begin(), e->asUString(NULL));
    }
    
    void beforelast(Element* e) {
        long sz = liste.size();
        if (!sz)
            liste.push_back(e->asUString(NULL));
        else
            liste.insert(liste.begin()+sz-1, e->asUString(NULL));
    }

    Element* thekeys(LispE* lisp);

    char check_match(LispE* lisp, Element* value);
    
    bool unify(LispE* lisp, Element* value, bool record);
    
    virtual Element* fullcopy() {
        Strings* e = new Strings();
        e->liste = liste;
        return e;
    }
    
    Element* unique(LispE* lisp);
    Element* rotate(bool left);

    
    virtual Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (status < s_protect && !duplicate)
            return this;

        Strings* e = new Strings();
        e->liste = liste;
        return e;
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
    
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    Element* extraction(LispE* lisp, List*);
    
    Element* index(long i) {
        exchange_value.content = liste[i];
        return &exchange_value;
    }
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);

    void flatten(LispE*, List* l);
    void flatten(LispE*, Numbers* l);
    
    Element* protected_index(LispE*,long i);
    
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
    
    void protecting(bool protection) {
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
        s_utf8_to_unicode(w, USTR(k), k.size());
        liste.push_back(w);
    }
    
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
    
    Element* replace(LispE* lisp, long i, Element* e) {
        if (i < 0)
            throw new Error("Error: position does not exist");
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
    short label() {
        return (liste.empty()?t_strings:v_null);
    }

    Element* reverse(LispE*, bool duplique = true);
    Element* rotate(LispE* lisp, long axis) {
        Strings* n = new Strings;
        for (long i = liste.size()-1; i >= 0; i--)
            n->liste.push_back(liste[i]);
        return n;
    }

    
    void storevalue(LispE*, double v);
    void storevalue(LispE*, long v);
    void storevalue(LispE*, u_ustring& v);
    
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
        liste.erase(liste.begin()+d);
        return true;
    }
    
    void getShape(vector<long>& sz) {
        sz.push_back(liste.size());
    }
    
    char isPureList() {
        return 3;
    }
    
    char isPureList(long& x, long& y) {
        x = size();
        y = 1;
        return 3;
    }

    Element* insert(LispE* lisp, Element* e, long idx);

    //There is a big difference between clean and clear
    //clear assumes that elements have been appended to the
    //list...
    void clear() {
        if (status < s_protect)
            liste.clear();
    }
    
    virtual Element* copyatom(uchar s) {
        if (status < s)
            return this;

        return new Strings(this);
    }

    Element* plus(LispE* l, Element* e);

    bool compare(LispE* lisp, List* comparison, short instruction, long i, long j);
    void sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax);
    void sorting(LispE* lisp, List* comparison);

};

class Stringspool : public Strings {
public:
    LispE* lisp;
    Stringspool(LispE* l) : lisp(l) {}
    Stringspool(long nb, u_ustring v) : Strings(nb, v) {}

    Element* newInstance();

    void decrementstatus(uchar nb, bool top);
    void release();
    Element* fullcopy();
    Element* copyatom(uchar s);
    Element* copying(bool duplicate = true);

};


#endif
