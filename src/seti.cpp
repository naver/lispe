class Set_i : public Element {
public:
    
    std::set<long> ensemble;
    Constinteger exchange_value;
    
    Set_i() : exchange_value(0), Element(t_seti) {}
    
    Set_i(std::set<long>& e) : exchange_value(0), ensemble(e), Element(t_seti) {}
    
    Set_i(uint16_t s) : exchange_value(0), Element(t_seti, s) {}
    
    bool isContainer() {
        return true;
    }
        
    Element* newInstance() {
        return new Set_i();
    }

    void* begin_iter() {
        return new std::set<long>::iterator(ensemble.begin());
    }
    
    virtual Element* copyatom(uint16_t s) {
        if (status < s)
            return this;

        return new Set_i(ensemble);
    }

    Element* next_iter(LispE* lisp, void* it);

    void clean_iter(void* it) {
        delete (std::set<long>::iterator*)it;
    }

    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* minimum(LispE*);
    Element* maximum(LispE*);
    Element* minmax(LispE*);
    
    void flatten(LispE*, List* l);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* replace_all_elements(LispE*, Element* element_value, Element* remp);
    Element* count_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    Element* checkkey(LispE* lisp, Element* e);

    Element* fullcopy();
    Element* copying(bool duplicate = true);
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(bool pair = false) {
        if (status == s_constant) {
            return new Set_i(ensemble);
        }
        return this;
    }
    
    Element* join_in_list(LispE* lisp, u_ustring& sep);
    
    bool unify(LispE* lisp, Element* e, bool record) {
        if (e == this)
            return true;
        
        if (e->type != t_set)
            return false;

        return ensemble == ((Set_i*)e)->ensemble;
    }
     
    bool egal(Element* e);
    Element* equal(LispE* lisp, Element* e);
    
    long size() {
        return ensemble.size();
    }
            
    wstring jsonString(LispE* lisp) {
        if (ensemble.empty())
            return L"[]";
        
        wstring buffer(L"[");
        bool first = true;
        for (auto& a: ensemble) {
            if (first)
                first = false;
            else
                buffer += L",";
            buffer += convertToWString(a);
        }
        buffer += L"]";
        return buffer;
    }
    
    u_ustring asUString(LispE* lisp) {
        if (ensemble.empty())
            return U"{}";
        
        u_ustring buffer(U"{");
        bool first = true;
        for (auto& a: ensemble) {
            if (first)
                first = false;
            else
                buffer += U" ";
            buffer += convertToUString(a);
        }
        buffer += U"}";
        return buffer;
    }
    
    wstring asString(LispE* lisp) {
        if (ensemble.empty())
            return L"{}";
        
        wstring buffer(L"{");
        bool first = true;
        for (auto& a: ensemble) {
            if (first)
                first = false;
            else
                buffer += L" ";
            buffer += convertToWString(a);
        }
        buffer += L"}";
        return buffer;
    }
    
    bool Boolean() {
        return ensemble.empty();
    }
    
    Element* index(long i) {
        for (auto& a: ensemble) {
            if (i <= 0) {
                exchange_value.number = a;
                return &exchange_value;
            }
            i--;
        }
        exchange_value.number = 0;
        return &exchange_value;
    }

    Element* value_from_index(LispE*, long i);
    
    Element* value_on_index(LispE*, long i);
    Element* protected_index(LispE*,long i);
    
    Element* protected_index(LispE*, long&);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    void storevalue(LispE*, long k) {
        ensemble.insert(k);
    }

    
    void add(long k) {
        ensemble.insert(k);
    }

    void append(LispE* lisp, long v) {
        ensemble.insert(v);
    }
    void append(LispE* lisp, long v) {
        ensemble.insert((long)v);
    }

    void append(Element* e) {
        ensemble.insert(e->asInteger());
    }
    
    void appendraw(Element* e) {
        ensemble.insert(e->asInteger());
    }

    Element* insert(LispE* lisp, Element* e, long idx) {
        ensemble.insert(e->asInteger());
        return this;
    }
    
    Element* replace(LispE* lisp, Element* i, Element* e) {
        long k = i->asInteger();
        if (ensemble.find(k) != ensemble.end()) {
            ensemble.erase(k);
        }
        ensemble.insert(e->asInteger());
        return this;
    }

    Element* thekeys(LispE* lisp);
    Element* thevalues(LispE* lisp);

    bool remove(LispE* lisp, Element* e) {
        long k = e->asInteger();
        if (ensemble.find(k) == ensemble.end()) {
            return false;
        }
        else {
            ensemble.erase(k);
            return true;
        }
    }

    bool remove(long k) {
        if (ensemble.find(k) == ensemble.end()) {
            return false;
        }
        else {
            ensemble.erase(k);
            return true;
        }
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

class Set_ipool : public Set_i {
public:
    LispE* lisp;
    
    Set_ipool(LispE* l) : lisp(l) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }

    Set_ipool(LispE* l, Set_i* e) : lisp(l), Set_i(e->ensemble) {
        exchange_value.lisp = l;
        exchange_value.provide = true;
    }

    Set_ipool* set(Set_i* s) {
        ensemble = s->ensemble;
        return this;
    }
    
    void decrementstatus(uint16_t nb);
    void decrement();
    
    void release();
    Element* fullcopy();
    Element* copying(bool duplicate = true);
    Element* copyatom(uint16_t s);
    Element* newInstance();

};


//-------Elements.cxx---------------------------------------------------------------
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
        lisp->setn_pool.push_back(this);
    }
}


void Set_ipool::decrementstatus(uint16_t nb) {
    if (is_protected())
        return;
    
    status-=nb;
    if (!status) {
        ensemble.clear();
        lisp->setn_pool.push_back(this);
    }
}

void Set_ipool::release() {
    if (!status) {
        ensemble.clear();
        lisp->setn_pool.push_back(this);
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
    Number* element;
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

Element* Set_i::next_iter(LispE* lisp, void* it) {
    std::set<long>::iterator* n = (std::set<long>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    Element* r = lisp->provideInteger(**n);
    (*n)++;
    return r;
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

Element* List::evall_seti(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_i();

    Set_i* n = lisp->provideSet_i();
    Element* values = null_;


    try {
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->type == t_seti)
                n->ensemble = ((Set_i*)values)->ensemble;
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asInteger());
                    }
                }
                else
                    n->add(values->asInteger());
            }
            _releasing(values);
        }
        
    }
    catch (Error* err) {
        delete n;
        values->release();
        throw err;
    }

    return n;
}

Element* Set_i::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d += a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
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
            if (i == e->size()) {
                release();
                return res;
            }
            d = a + e->index(i)->asInteger();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    long w = e->asInteger();
    for (auto& a: ensemble) {
        d = a + w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d -= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a - *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            d = a - e->index(i)->asInteger();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    long w = e->asInteger();
    for (auto& a: ensemble) {
        d = a - w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 1;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d *= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a * *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            d = a * e->index(i)->asInteger();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    long w = e->asInteger();
    for (auto& a: ensemble) {
        d = a * w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else {
                if (!a)
                    throw new Error("Error: division by zero");
                d /= a;
            }
        }
        return lisp->provideNumber(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            if (!*nxt) {
                delete res;
                throw new Error("Error: division by zero");
            }

            d = a / *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    double w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asNumber();
            if (!w) {
                delete res;
                throw new Error("Error: division by zero");
            }
            d = a / w ;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asNumber();
    if (!w) {
        delete res;
        throw new Error("Error: division by zero");
    }

    for (auto& a: ensemble) {
        d = a / w;
        res->add(d);
    }
    release();
    return res;
}


Element* Set_i::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        bool first = true;
        long d = 0;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else {
                if (!a)
                    throw new Error("Error: division by zero");
                d %= (long)a;
            }
        }
        return lisp->provideNumber(d);
    }

    double d;

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            if (!*nxt) {
                delete res;
                throw new Error("Error: division by zero");
            }

            d = (long)a % (long)*nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    long w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asInteger();
            if (!w) {
                delete res;
                throw new Error("Error: division by zero");
            }
            d = (long)a % w ;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    if (!w) {
        delete res;
        throw new Error("Error: division by zero");
    }

    for (auto& a: ensemble) {
        d = (long)a % w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    //Two cases either e is a number or it is a list...
    double d = 1;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d = pow(d, (double)a);
        }
        return lisp->provideNumber(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = pow((double)a,*nxt);
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    double w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asNumber();
            d = pow((double)a, w) ;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asNumber();
    if (!w) {
        delete res;
        throw new Error("Error: division by zero");
    }

    for (auto& a: ensemble) {
        d = pow((double)a, w);
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d &= ~a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            res->add(a & ~*nxt);
            nxt++;
        }
        release();
        return res;
    }

    long r;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r = e->index(i)->asInteger();
            d = a & ~r;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    r = ~e->asInteger();
    for (auto& a: ensemble) {
        d = a & r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_and(LispE* lisp, Element* e) {
    long d;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d &= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            res->add(a & *nxt);
            nxt++;
        }
        release();
        return res;
    }

    long r;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r = e->index(i)->asInteger();
            d = a & r;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    r = e->asInteger();
    for (auto& a: ensemble) {
        d = a & r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_or(LispE* lisp, Element* e) {
    long d = 0;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d |= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            res->add(a | *nxt);
            nxt++;
        }
        release();
        return res;
    }

    long r;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r = e->index(i)->asInteger();
            d = a | r;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    r = e->asInteger();
    for (auto& a: ensemble) {
        d = a | r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d ^= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            res->add(a ^ *nxt);
            nxt++;
        }
        release();
        return res;
    }

    long r;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r = e->index(i)->asInteger();
            d = a ^ r;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    r = e->asInteger();
    for (auto& a: ensemble) {
        d = a ^ r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_not(LispE* lisp) {
    //Two cases either e is a number or it is a list...
    Set_i* res = lisp->provideSet_i();

    for (auto& a: ensemble) {
        res->add(~a);
    }
    release();
    return res;
}

Element* Set_i::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d <<= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a << *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    
    long w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asInteger();
            d = a << w;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    for (auto& a: ensemble) {
        d = a << w;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_i::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d >>= a;
        }
        return lisp->provideInteger(d);
    }

    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_i*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a >> *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    
    long w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asInteger();
            d = a >> w;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    for (auto& a: ensemble) {
        d = a >> w;
        res->add(d.v);
    }
    release();
    return res;
}

