/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lists.cxx
//
//
#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>
//--------------------------------------------------------------------------------
List* Listincode::cloning(Listincode* e, methodEval m) {
    return new List_execute(e, m);
}

//--------------------------------------------------------------------------------
void u_links::push_front(Element* v, bool is_final) {
    u_link* e;
    if (is_final)
        e = new u_link_last(v);
    else
        e = new u_link(v);
    
    if (first == NULL)
        e->inc(1);
    else
        first->insert(e);
    first = e;
}

void u_link::connect_as_last(Element* v) {
    _next = new u_link_last(v);
    _next->_previous = this;
    _next->inc(status);
}

void u_links::push_back_as_last(LispE* lisp, Element* v) {
    u_link* e = last_raw();
    if (e == NULL) {
        first = new u_link_last(v);
        first->inc(1);
        return;
    }
    e->connect_as_last(v);
#ifdef LISPE_WASM
    if (e->error) {
        e->error = false;
        lisp->delegation->set_error(new Error("Error: Cannot add an element to this linked list"));
    }
#endif
}

void u_links::reverse(LispE* lisp) {
   if (atleast2()) {
      //First, if it in the middle of a longer list
      //we cut it from it...
      u_link* e = last();
      if (e->isFinal())
         {
         lisp->delegation->set_error(new Error("Error: cannot reverse a linked list with a final '.' element"));
         return;
      }

      u_link* p = e->_next;

      //p != NULL => cycle
      if (p == NULL) {
         //no cycle, but "first"
         //might be inside a list, with some elements before
         //such as returned by a cdr...
         p = first->_previous;
         if (p != NULL) {
            p->_next = e;
            //we cut this element temporary from the list
            first->_previous = NULL;
         }
      }

      u_link* n;
      while (first != e) {
         n = first->_next;
         first->_next = first->_previous;
         first->_previous = n;
         first = n;
      }
      first->_next = first->_previous;
      first->_previous = p;
   }
}


void u_links::connect(LispE* lisp, u_links& l) {
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
      u = last_raw();
      if (u->isFinal())
         {
         lisp->delegation->set_error(new Error("Error: Cannot add an element to this linked list"));
         return;
      }

      u->_next = l.first;
      l.first->_previous = u;
      //if there is no cycle
      //No element in the current list is in l
      if (l.first->mark != u->mark) {
         int16_t status = u->status;
         u = l.begin();
         while (u != NULL) {
            u->inc(status);
            u = u->next();
         }
      }
   }
}


//--------------------------------------------------------------------------------
List_call_lambda::List_call_lambda(LispE* lisp, Listincode* l) : Listincode(l) {
   body = (List_lambda_eval*)liste[0];
   nbarguments = l->size() - 1;
   if (nbarguments != body->parameters->size()) {
      wstring inside_class = L"Error: Wrong number of arguments in: '(lambda ";
      inside_class += body->parameters->asString(lisp);
      inside_class += L"...)'";
      {
         lisp->delegation->set_error(new Error(inside_class));
         return;
      }
   }
}


List_function_eval::List_function_eval(LispE* lisp, Listincode* l, List* b) : body(b), Listincode(l) {
   type = t_call;
   status = s_constant;
   nbarguments = liste.size() - 1;
   //the third element is the argument list .
   //we need our body to be the same number
   parameters = body->liste[2];
   defaultarguments = parameters->argumentsize(nbarguments);
   if (defaultarguments == -1) {
      wstring inside_class = L"Error: Wrong number of arguments in call to: '(";
      inside_class += body->liste[1]->asString(lisp);
      inside_class += L" ";
      inside_class += body->liste[2]->asString(lisp);
      inside_class += L"...)'";
      {
         lisp->delegation->set_error(new Error(inside_class));
         return;
      }
   }
   same = (defaultarguments == parameters->size());
}


List_function_eval::List_function_eval(LispE* lisp, List* b) : body(b) {
   liste.push_element(b);
   type = t_eval;
   status = s_constant;
   nbarguments = 1;
   //the third element is the argument list .
   //we need our body to be the same number
   parameters = body->liste[2];
   defaultarguments = parameters->argumentsize(nbarguments);
   if (defaultarguments == -1) {
      wstring inside_class = L"Error: Wrong number of arguments in call to: '(";
      inside_class += body->liste[1]->asString(lisp);
      inside_class += L" ";
      inside_class += body->liste[2]->asString(lisp);
      inside_class += L"...)'";
      {
         lisp->delegation->set_error(new Error(inside_class));
         return;
      }
   }
   same = (defaultarguments == parameters->size());
}


//--------------------------------------------------------------------------------
//Pools methods
//--------------------------------------------------------------------------------
void Listpool::decrement() {
    status -= not_protected();
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_max(lisp->larger_max_size, this);
    }
}

void Listpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_max(lisp->larger_max_size, this);
    }
}

void Listpool::release() {
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_max(lisp->larger_max_size, this);
    }
}

void Listpool::release(Element* e) {
    if (!status) {
        liste.decrement(e);
        liste.clear();
        lisp->list_pool.push_max(lisp->larger_max_size, this);
    }
}

void Listpool::rawrelease() {
    if (!status) {
        liste.clear();
        liste.decrement();
        lisp->list_pool.push_max(lisp->larger_max_size, this);
    }
}

Element* Listpool::newInstance() {
    return lisp->provideList();
}

Element* Listpool::fullcopy() {
    if (liste.marking)
        return liste.object;
    liste.marking = true;
    if (lisp->preparingthread)
        liste.object = new List;
    else
        liste.object = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        liste.object->append(liste[i]->fullcopy());
    }
    liste.marking = false;
    return liste.object;
}

Element* Listpool::copyatom(LispE* lsp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    List* l = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copyatom(lisp, s));
    }
    return l;
}

Element* Listpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    List* l;
    if (lisp->preparingthread)
        l = new List;
    else {
        if (!is_protected() && !duplicate)
            return this;
        
        l = lisp->provideList();
    }
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copying(false));
    }
    return l;
}

void Numberspool::decrement() {
    status -= not_protected();
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_max(lisp->max_size, this);
    }
}

void Floatspool::decrement() {
    status -= not_protected();
    if (!status) {
        liste.clear();
        lisp->floats_pool.push_max(lisp->max_size, this);
    }
}

void Floatspool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        liste.clear();
        lisp->floats_pool.push_max(lisp->max_size, this);
    }
}

void Floatspool::release() {
    if (!status) {
        liste.clear();
        lisp->floats_pool.push_max(lisp->max_size, this);
    }
}

Element* Floatspool::newInstance() {
    return lisp->provideFloats();
}

Element* Floatspool::newInstance(Element* v) {
    return lisp->provideFloats(liste.size(), v->asFloat());
}

Element* Floatspool::fullcopy() {
    if (lisp->preparingthread)
        return new Floats(this);
    
    return lisp->provideFloats(this);
}

Element* Floatspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Floats(this);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideFloats(this);
}

Element* Floatspool::copyatom(LispE* lsp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    return lisp->provideFloats(this);
}

void Numberspool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_max(lisp->max_size, this);
    }
}

void Numberspool::release() {
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_max(lisp->max_size, this);
    }
}

Element* Numberspool::newInstance() {
    return lisp->provideNumbers();
}

Element* Numberspool::newInstance(Element* v) {
    return lisp->provideNumbers(liste.size(), v->asNumber());
}

Element* Numberspool::fullcopy() {
    if (lisp->preparingthread)
        return new Numbers(this);
    
    return lisp->provideNumbers(this);
}

Element* Numberspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Numbers(this);
    
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideNumbers(this);
}

Element* Numberspool::copyatom(LispE* lsp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    return lisp->provideNumbers(this);
}

void Integerspool::decrement() {
    status -= not_protected();
    if (!status) {
        lisp->integers_pool.push_max(lisp->max_size, this);
        liste.clear();
    }
}

void Integerspool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        lisp->integers_pool.push_max(lisp->max_size, this);
        liste.clear();
    }
}

void Integerspool::release() {
    if (!status) {
        lisp->integers_pool.push_max(lisp->max_size, this);
        liste.clear();
    }
}

Element* Integerspool::newInstance() {
    return lisp->provideIntegers();
}

Element* Integerspool::newInstance(Element* v) {
    return lisp->provideIntegers(liste.size(), v->asInteger());
}

Element* Integerspool::fullcopy() {
    if (lisp->preparingthread)
        return new Integers(this);
    
    return lisp->provideIntegers(this);
}

Element* Integerspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Integers(this);
    
    //If it is a CDR, we need to copy it...
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideIntegers(this);
}

Element* Integerspool::copyatom(LispE* lsp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    return lisp->provideIntegers(this);
}

void Stringspool::decrement() {
    status -= not_protected();
    if (!status) {
        lisp->strings_pool.push_max(lisp->max_size, this);
        liste.clear();
    }
}

void Stringspool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        lisp->strings_pool.push_max(lisp->max_size, this);
        liste.clear();
    }
}

void Stringspool::release() {
    if (!status) {
        lisp->strings_pool.push_max(lisp->max_size, this);
        liste.clear();
    }
}

Element* Stringspool::newInstance() {
    return lisp->provideStrings();
}

Element* Stringspool::fullcopy() {
    if (lisp->preparingthread)
        return new Strings(this);
    
    return lisp->provideStrings(this);
}

Element* Stringspool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Strings(this);
    
    //If it is a CDR, we need to copy it...
    if (!is_protected() && !duplicate)
        return this;
    
    return lisp->provideStrings(this);
}

Element* Stringspool::copyatom(LispE* lsp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    return lisp->provideStrings(this);
}

//--------------------------------------------------------------------------------
//List methods
//--------------------------------------------------------------------------------
inline bool LIST::compare(LispE* lisp, List* comparison, int16_t instruction, long i, long j) {
    comparison->in_quote(1, item->buffer[i]);
    comparison->in_quote(2, item->buffer[j]);
    return comparison->eval_Boolean(lisp, instruction);
}

void LIST::sorting(LispE* lisp, List* comparison, int16_t instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                item->swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                item->swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            item->swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            item->swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    item->swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        item->swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }
    
    pivot = rmin - 1;
    comparison->in_quote(2, item->buffer[rmax]);
    for (j = rmin; j < rmax; j++) {
        comparison->in_quote(1, item->buffer[j]);
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            item->swap(pivot,j);
        }
    }
    pivot++;
    item->swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void LIST::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = item->last - home;
    if (sz <= 1)
        return;
    
    sorting(lisp, comparison, comparison->liste[0]->type, home, item->last - 1);
}

//------------------------------------------------------------------------------------------
Element* List::check_member(LispE* lisp, Element* the_set) {
    List* r = lisp->provideList();
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp,the_set);
        r->append(e);
    }
    return r;
}

Element* LList::check_member(LispE* lisp, Element* the_set) {
    LList* r = new LList(liste.mark);
    Element* e;
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        e = a->value->check_member(lisp,the_set);
        r->append(e);
    }
    return r;
}

Element* List::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v = index(0);
    for (long i = 1; i < liste.size(); i++) {
        if (v->more(lisp, liste[i])->Boolean())
            v = liste[i];
    }
    return v->copying(false);
}

Element* LList::minimum(LispE* lisp) {
    if (liste.empty())
        return null_;
    Element* v = liste.front();
    u_link* it = liste.begin();
    it = it->next();
    for (;it != NULL; it = it->next()) {
        if (v->more(lisp, it->value)->Boolean())
            v = it->value;
    }
    return v->copying(false);
}

Element* List::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v = index(0);
    for (long i = 1; i < liste.size(); i++) {
        if (v->less(lisp, liste[i])->Boolean())
            v = liste[i];
    }
    return v->copying(false);
}

Element* LList::maximum(LispE* lisp) {
    if (liste.empty())
        return null_;
    Element* v = liste.front();
    u_link* it = liste.begin();
    it = it->next();
    for (;it != NULL; it = it->next()) {
        if (v->less(lisp, it->value)->Boolean())
            v = it->value;
    }
    return v->copying(false);
}

Element* List::minmax(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v_min = index(0);
    Element* v_max = v_min;
    for (long i = 1; i < liste.size(); i++) {
        if (v_min->more(lisp, liste[i])->Boolean())
            v_min = liste[i];
        else {
            if (v_max->less(lisp, liste[i])->Boolean())
                v_max = liste[i];
        }
    }
    List* l = lisp->provideList();
    l->append(v_min);
    l->append(v_max);
    return l;
}

Element* LList::minmax(LispE* lisp) {
    if (liste.empty())
        return null_;
    Element* v_min = liste.front();
    Element* v_max = v_min;
    u_link* it = liste.begin();
    it = it->next();
    for (; it != NULL; it = it->next()) {
        if (v_min->more(lisp, it->value)->Boolean())
            v_min = it->value;
        else {
            if (v_max->less(lisp, it->value)->Boolean())
                v_max = it->value;
        }
    }
    LList* l = new LList(liste.mark);
    l->append(v_min);
    l->append(v_max);
    return l;
}

void List::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        liste[i]->flatten(lisp, l);
    }
}

void List::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        liste[i]->flatten(lisp, l);
    }
}

void List::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        liste[i]->flatten(lisp, l);
    }
}

void LList::flatten(LispE* lisp, List* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        a->value->flatten(lisp, l);
    }
}

void LList::flatten(LispE* lisp, Numbers* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        a->value->flatten(lisp, l);
    }
}

void LList::flatten(LispE* lisp, Floats* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        a->value->flatten(lisp, l);
    }
}

Element* List::transposed(LispE* lisp) {
    vecte<long> sz;
    getShape(sz);
    if (sz.size() <= 1)
        return this;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    Element* tenseur;
    if (sz.size() == 2)
        tenseur = new Matrice(lisp, sz[0], sz[1], 0.0);
    else
        tenseur = new Tenseur(lisp, sz, zero_);
    
    Element* e;
    for (i = 0; i < sz[1]; i++) {
        e = liste[i];
        for (long j = 0; j < sz[0]; j++) {
            tenseur->index(j)->replacing(i, e->index(j));
        }
    }
    
    return tenseur;
}

void List::storevalue(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void List::storevalue(LispE* lisp,long v) {
    append(lisp->provideInteger(v));
}

void List::storevalue(LispE* lisp, u_ustring& v) {
    append(lisp->provideString(v));
}

void List::garbaging_values(LispE* lisp) {
    if (liste.marking)
        return;
    liste.marking = true;
    for (long i = 0; i < liste.size(); i++) {
        lisp->control_garbaging(liste[i]);
        liste[i]->garbaging_values(lisp);
    }
    liste.marking = false;
}

void LList::garbaging_values(LispE* lisp) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        lisp->control_garbaging(a->value);
        a->value->garbaging_values(lisp);
    }
}

void List::append(LispE* lisp, u_ustring& k) {
    append(lisp->provideString(k));
}

void LList::append(LispE* lisp, u_ustring& k) {
    append(lisp->provideString(k));
}

void List::append(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void LList::append(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void List::append(LispE* lisp, long v) {
    append(lisp->provideInteger(v));
}

void LList::append(LispE* lisp, long v) {
    append(lisp->provideInteger(v));
}

void List::buildList(LispE* lisp, Element* result, Element* current, vecte<long>& shape, vecte<long>& positions, long idx, long axis) {
    if (axis < idx) {
        for (idx = positions.last-1; idx >= 0; idx--) {
            current = current->index(positions[idx]);
        }
        result->append(current->duplicate_constant(lisp));
        return;
    }
    
    positions.push_back(0);
    for (long i = 0; i < shape[axis]; i++) {
        positions.setlast(i);
        buildList(lisp, result, current, shape, positions, idx, axis - 1);
    }
    positions.pop_back();
}

Element* List::storeRank(LispE* lisp, Element* result, Element* current, vecte<long>& shape, vecte<long>& positions, long idx) {
    long axis = idx;
    //first we search for our first actual axis...
    while (axis < positions.size() && positions[axis] == -1) axis++;
    if (axis == idx) {
        //It is a direct value...
        if (idx == positions.size() - 1)
            return current->index(positions[idx])->duplicate_constant(lisp);
        
        return storeRank(lisp, result, current->index(positions[idx]), shape, positions, idx+1);
    }
    
    //otherwise, this is an axis
    vecte<long> paths;
    if (axis < positions.size()) {
        long p_idx = positions[axis];
        paths.push_back(p_idx);
        buildList(lisp, result, current, shape, paths, idx, axis - 1);
        return result;
    }
    
    paths.push_back(0);
    Element* r;
    for (long i = 0; i < shape[axis]; i++) {
        paths.setlast(i);
        r = lisp->provideList();
        result->append(r);
        buildList(lisp, r, current, shape, paths, idx, axis - 1);
    }
    return result;
}

Element* List::rank(LispE* lisp, vecte<long>& positions) {
   vecte<long> shape;
   getShape(shape);
   if (!checkShape(0, shape))
      return new Error("Error: unregular matrix: some sub-lists have different sizes");

   int16_t sz = positions.size();
   if (!sz || sz > shape.size())
      return new Error("Error: index mismatch");
   Element* result = lisp->provideList();
   Element* res = storeRank(lisp, result, this, shape, positions, 0);
   if (res != result)
      result->release();

   return res;
}


Element* LList::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   lisp->recording(null_, label);
   Element* element;
   long sz = code->liste.size();
   for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
      element = a->value->copying(false);
      lisp->replacingvalue(element, label);
      _releasing(e);
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* List::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   lisp->recording(null_, label);
   Element* element;
   long sz = code->liste.size();
   for (long i = 0; i < liste.size(); i++) {
      element = liste[i]->copyatom(lisp, 1);
      lisp->replacingvalue(element, label);
      _releasing(e);
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* List::mloop(LispE* lisp) {
     List_mloop_eval m(this);
     return m.eval(lisp);
}

Element* List::lloop(LispE* lisp) {
     List_lloop_eval m(this);
     return m.eval(lisp);
}

Element* List::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   e = e->copying(false);
   List* l = (List*)duplicate_constant(lisp);
   l->liste.insert(ix, e);
   return l;
}


Element* LList::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   e = e->copying(false);
   LList* l = (LList*)duplicate_constant(lisp);
   if (!l->insertion(e, ix))
      return new Error("Error: cannot insert this element in this list");
   return l;
}


Element* List::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    List* l = (List*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->append(liste[i]->copying(false));
        l->append(liste[0]->copying(false));
        return l;
    }
    
    l->append(liste.back()->copying(false));
    for (long i = 0; i < liste.size() - 1; i ++)
        l->append(liste[i]->copying(false));
    return l;
}

Element* LList::rotate(bool left) {
    if (!liste.atleast2())
        return this;
    
    LList* l = new LList(liste.mark);
    u_link* it = liste.last();
    bool cyclic = (it->_next != NULL);
    u_link* tail = NULL;
    
    if (left) {
        l->append(liste.first->value->copying(false));
        if (cyclic)
            tail = l->liste.first;
        while (it != liste.first) {
            l->push_front(it->value->copying(false));
            it = it->previous();
        }
        
        if (cyclic) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }
        return l;
    }
    Element* last = it->value->copying(false);
    it = it->previous();
    while (it != NULL) {
        l->push_front(it->value->copying(false));
        if (cyclic) {
            tail = l->liste.first;
            cyclic = false;
        }
        it = it->previous();
    }
    l->push_front(last);
    if (tail != NULL) {
        //there is a cycle
        //we need to reproduce it...
        l->liste.first->_previous = tail;
        tail->_next = l->liste.first;
    }
    return l;
}

Element* List::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    List* list = lisp->provideList();
    long i;
    std::set<u_ustring> values;
    for (i = 0; i < liste.size(); i++) {
        if (values.insert(liste[i]->asUString(lisp)).second)
            list->append(liste[i]->copying(false));
    }
    return list;
}

Element* LList::unique(LispE* lisp) {
    if (liste.empty())
        return this;
    
    LList* list = new LList(liste.mark);
    u_link* it = liste.begin();
    std::set<u_ustring> values;
    for (;it != NULL; it = it->next()) {
        if (values.insert(it->value->asUString(lisp)).second)
            list->push_front(it->value->copying(false));
    }
    return list;
}

Element* List::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* LList::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    long sz = size();
    for (long i = 0; i < sz; i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

long List::find_element(LispE* lisp, Element* a_value, long ix) {
    long i = ix;
    long sz = liste.size();
    for (; i < sz && !liste[i]->equal(lisp, a_value)->Boolean(); i++) {}
    return (i == sz)?-1:i;
}

Element* List::search_element(LispE* lisp, Element* a_value, long ix) {
    long i = ix;
    long sz = liste.size();
    for (; i < sz && !liste[i]->equal(lisp, a_value)->Boolean(); i++) {}
    return (i == sz)?null_:lisp->provideInteger(i);
}

Element* LList::search_element(LispE* lisp, Element* a_value, long ix) {
    u_link*  it = at(ix);
    for (;it != NULL && !it->value->equal(lisp, a_value)->Boolean(); it = it->next()) {}
    
    return (it == NULL)?null_:it->value;
}

bool List::check_element(LispE* lisp, Element* a_value) {
    long i = 0;
    long sz = liste.size();
    for (; i < sz  && !liste[i]->equal(lisp, a_value)->Boolean(); i++) {}
    return (i != sz);
}

bool LList::check_element(LispE* lisp, Element* a_value) {
    u_link*  it = liste.begin();
    for (;it != NULL && !it->value->equal(lisp, a_value)->Boolean(); it = it->next()) {}
    return (it != NULL);
}

Element* List::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    long nb = 0;
    for (long i = 0; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, a_value)->Boolean()) {
            replacing(i, remp->copying(false));
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* LList::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    u_link* a = liste.begin();
    long nb = 0;
    for (; a != NULL; a = a->next()) {
        if (a->value->equal(lisp, a_value)->Boolean()) {
            if (a->value != remp) {
                a->value->decrement();
                a->value = remp->copying(false);
                a->value->increment();
                nb++;
            }
        }
    }
    return lisp->provideInteger(nb);
}

Element* List::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i]->equal(lisp, a_value)->Boolean()) {
            l->liste.push_back(i);
        }
    }
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* LList::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    u_link* a = at(ix);
    for (; a != NULL; a = a->next()) {
        if (a->value->equal(lisp, a_value)->Boolean()) {
            l->liste.push_back(ix);
        }
        ix++;
    }
    if (l->liste.empty()) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* List::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    long nb = 0;
    long sz = liste.size();
    for (long i = ix; i < sz; i++) {
        if (liste[i]->equal(lisp, a_value)->Boolean()) {
            nb++;
        }
    }
    return lisp->provideInteger(nb);
}

Element* LList::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    long nb = 0;
    u_link* a = at(ix);
    for (; a != NULL; a = a->next()) {
        if (a->value->equal(lisp, a_value)->Boolean()) {
            nb++;
        }
        ix++;
    }
    return lisp->provideInteger(nb);
}

Element* List::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   List* result = lisp->provideList();
   long sz = liste.size();
   for (long i = 0; i < sz; i++) {
      if (value->check_element(lisp, liste[i]) && !result->check_element(lisp, liste[i]))
         result->append(liste[i]->copying(false));
   }
   if (result->isEmpty()) {
      result->release();
      return emptylist_;
   }
   return result;
}


Element* LList::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   LList* result = new LList(liste.mark);
   u_link* a = liste.begin();
   for (; a != NULL; a = a->next()) {
      if (value->check_element(lisp, a->value) && !result->check_element(lisp, a->value))
         result->append(a->value->copying(false));
   }
   if (result->isEmpty()) {
      result->release();
      return emptylist_;
   }
   return result;
}


Element* List::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   List* result = lisp->provideList();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (result->check_element(lisp, liste[i]) == false)
         result->append(liste[i]->copying(false));
   }

   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         if (result->check_element(lisp, a->value) == false)
            result->append(a->value->copying(false));
      }
   }
   else {
      if (value->isList()) {
         sz = value->size();
         for (i = 0; i < sz; i++) {
            if (result->check_element(lisp, value->index(i)) == false)
               result->append(value->index(i)->copying(false));
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter(lisp, iter);
            while (next_value != emptyatom_) {
               if (result->check_element(lisp, next_value) == false)
                  result->append(next_value);
               next_value = value->next_iter(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }

   if (result->isEmpty()) {
      result->release();
      return emptylist_;
   }
   return result;
}


Element* LList::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   LList* result = new LList(liste.mark);
   u_link* a = liste.begin();
   for (; a != NULL; a = a->next()) {
      if (result->check_element(lisp, a->value) == false)
         result->append(a->value->copying(false));
   }

   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         if (result->check_element(lisp, a->value) == false)
            result->append(a->value->copying(false));
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long i = 0; i < sz; i++) {
            if (result->check_element(lisp, value->index(i)) == false)
               result->append(value->index(i)->copying(false));
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter(lisp, iter);
            while (next_value != emptyatom_) {
               if (result->check_element(lisp, next_value) == false)
                  result->append(next_value);
               next_value = value->next_iter(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }

   if (result->isEmpty()) {
      result->release();
      return emptylist_;
   }
   return result;
}


Element* List::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   List* intersection = (List*)list_and(lisp, value);

   List* result = lisp->provideList();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!intersection->check_element(lisp, liste[i]) && !result->check_element(lisp, liste[i]))
         result->append(liste[i]->copying(false));
   }

   Element* v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value;
         if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
            result->append(v->copying(false));
      }
   }
   else {
      if (value->isList()) {
         sz = value->size();
         for (i = 0; i < sz; i++) {
            v = value->index(i);
            if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
               result->append(v->copying(false));
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            v = value->next_iter(lisp, iter);
            while (v != emptyatom_) {
               if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                  result->append(v);
               v = value->next_iter(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   intersection->release();
   if (result->isEmpty()) {
      result->release();
      return emptylist_;
   }
   return result;
}


Element* LList::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   LList* intersection = (LList*)list_and(lisp, value);

   LList* result = new LList(liste.mark);
   u_link* a = liste.begin();
   for (; a != NULL; a = a->next()) {
      if (!intersection->check_element(lisp, a->value) && !result->check_element(lisp, a->value))
         result->append(a->value->copying(false));
   }

   Element* v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value;
         if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
            result->append(v->copying(false));
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long i = 0; i < sz; i++) {
            v = value->index(i);
            if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
               result->append(v->copying(false));
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            v = value->next_iter(lisp, iter);
            while (v != emptyatom_) {
               if (!intersection->check_element(lisp, v) && !result->check_element(lisp, v))
                  result->append(v);
               v = value->next_iter(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   intersection->release();
   if (result->isEmpty()) {
      result->release();
      return emptylist_;
   }
   return result;
}


Element* List::search_reverse(LispE* lisp, Element* a_value, long ix) {
    for (long i = liste.size() - 1; i >= ix; i--) {
        if (liste[i]->equal(lisp, a_value)->Boolean())
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* LList::search_reverse(LispE* lisp, Element* a_value, long ix) {
    u_link* it = liste.b_at(ix);
    for (; it != NULL; it = it->previous()) {
        if (it->value->equal(lisp, a_value)->Boolean())
            return lisp->provideInteger(ix);
        if (!ix)
            break;
        ix--;
    }
    
    return null_;
}

Element* List::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        List* l = lisp->provideList();
        for (long i = liste.size()-1; i >= 0; i--)
            l->append(liste[i]->copying(false));
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* LList::reverse(LispE* lisp, bool duplicate) {
    if (duplicate) {
        LList* l = new LList(liste.mark);
        u_link*  it = liste.begin();
        if (it == NULL)
            return l;
        
        u_link* p = it;
        u_link* tail = NULL;
        for (; it != NULL; it = it->next()) {
            p = it;
            l->push_front(it->value->copying(false));
            if (tail == NULL)
                tail = l->liste.first;
        }
        if (p->_next != NULL) {
            //there is a cycle
            //we need to reproduce it...
            l->liste.first->_previous = tail;
            tail->_next = l->liste.first;
        }
        return l;
    }
    
    liste.reverse(lisp);
    return this;
}

Element* List::rotate(LispE* lisp, long axis) {
    return reverse(lisp, true);
}

Element* List::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return liste[i];
    return null_;
}

Element* LList::protected_index(LispE* lisp,long i) {
    Element* e = null_;
    if (i >= 0) {
        e = at_e(i)->copying(false);
        if (e == NULL)
            return null_;
    }
    return e;
}

Element* List::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return liste.back();
}

Element* LList::last_element(LispE* lisp) {
    if (liste.empty())
        return null_;
    return liste.back();
}

Element* Infiniterangenumber::value_on_index(LispE* lisp, long i) {
    double v = initial_value + increment*i;
    if (infinite_loop) {
        exchange_value.content = v;
        return &exchange_value;
    }
    
    if (increment < 0) {
        if (v > bound) {
            exchange_value.content = v;
            return &exchange_value;
        }
    }
    else {
        if (v < bound) {
            exchange_value.content = v;
            return &exchange_value;
        }
    }
    return null_;
}

Element* Infiniterangeinteger::value_on_index(LispE* lisp, long i) {
    long v = initial_value + increment*i;
    if (infinite_loop) {
        exchange_value.content = v;
        return &exchange_value;
    }
    
    if (increment < 0) {
        if (v > bound) {
            exchange_value.content = v;
            return &exchange_value;
        }
    }
    else {
        if (v < bound) {
            exchange_value.content = v;
            return &exchange_value;
        }
    }
    return null_;
}

Element* List::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    return null_;
}

Element* LList::value_on_index(LispE* lisp, long i) {
    if (i >= 0) {
        Element* e = at_e(i)->copying(false);
        if (e == NULL)
            return null_;
        return e;
    }
    return null_;
}

Element* Iter_llist::value_on_index(LispE* lisp, long i) {
    if (i >= 0) {
        Element* e = elements[i]->copying(false);
        if (e == NULL)
            return null_;
        return e;
    }
    return null_;
}

Element* List::value_from_index(LispE* lisp, long i) {
    return liste[i]->copying(false);
}

Element* LList::value_from_index(LispE* lisp, long i) {
    return at_e(i)->copying(false);
}

Element* List::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    
    return null_;
}

Element* LList::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    ix = null_;
    if (i >= 0) {
        ix = at_e(i)->copying(false);
        if (ix == NULL)
            return null_;
    }
    return ix;
}

Element* List::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0 && i < liste.size())
      return liste[i];

   return new Error("Error: index out of bounds");
}


Element* LList::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0) {
      ix = at_e(i);
      if (ix == NULL)
         return new Error("Error: index out of bounds");
   }
   else
      return new Error("Error: index out of bounds");
   return ix;
}


Element* List::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += liste[i]->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* LList::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (u_link* a = liste.begin(); a != NULL; a = a->next()) {
        str += beg;
        beg = sep;
        str += a->value->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* List::newInstance(Element* e) {
    long i;
    long sz = size();
    for (i = 0; i < sz; i++) {
        if (liste[i]->isList())
            break;
    }
    
    if (i == sz) {
        switch(e->type) {
            case t_float:
                return new Floats(sz, e->asNumber());
            case t_number:
                return new Numbers(sz, e->asNumber());
            case t_short:
                return new Shorts(sz, e->asShort());
            case t_integer:
                return new Integers(sz, e->asInteger());
            case t_string:
                return new Strings(sz, e->asString(NULL));
        }
    }
    
    List* lst = (List*)newInstance();
    for (i = 0; i < sz; i++) {
        if (liste[i]->isList())
            lst->append(liste[i]->newInstance(e));
        else
            lst->append(e);
    }
    return lst;
}

Element* List::equal(LispE* lisp, Element* e) {
    return booleans_[e->isList() && liste.equal(((List*)e)->liste)];
}

Element* LList::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_llist && e->size() == 0 && liste.empty()) || e == this)];
}

bool List::egal(Element* e) {
    return e->isList() && liste.equal(((List*)e)->liste);
}

bool LList::egal(Element* e) {
    return ((e->type == t_llist && e->size() == 0 && liste.empty()) || e == this);
}

Element* List::extraction(LispE* lisp, List* l) {
   Element* e_from = l->liste[2];
   Element* e;

   long sz = size();
   long from = 0;
   long firstisString = -1;
   int16_t nxt = 3;
   int16_t ty;
   switch (e_from->label()) {
      case l_minus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after first operator: '-'");
      break;
      case l_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '+'");
      break;
      case l_minus_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '-+'");
      break;
      default:
         e_from = e_from->eval(lisp);
      ty = e_from->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         from = e->asInteger();
         firstisString = 0;
         break;
      }
      case t_plus_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         //We skip the first characters
         from = e->asInteger() + 1;
         firstisString = 0;
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      from = e_from->asInteger();
         if (from < 0)
            from = sz + from;
         break;
      default:
         e->release();
      e_from->release();
      return new Error("Error: cannot use the first position in 'extract'");
   }

   e->release();
   e_from->release();

   if (from < 0 || from >= sz)
      return emptylist_;

   if (nxt == l->size()) {
      //Only one element is returned
      return liste[from]->copying(false);
   }

   Element* e_upto = l->liste[nxt];
   switch (e_upto->label()) {
      case l_minus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after second operator: '-'");
      break;
      case l_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '+'");
      break;
      case l_minus_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '-+'");
      break;
      default:
         e_upto = e_upto->eval(lisp);
      ty = e_upto->type;
   }

   long upto;
   e = null_;
   switch (ty) {
      case t_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_plus_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         //All characters are integrated
         upto = e->asInteger() + 1;
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger() - 1;
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      upto = e_upto->asInteger();
         if (firstisString != -1 && upto > 0) {
         //in this case upto is a number of characters, not a position
         upto += from + firstisString;
      }
      else {
         if (upto <= 0) {
            //We start from the end...
            upto = sz + upto;
         }
      }
      break;
      default:
         e->release();
      e_upto->release();
      return new Error("Error: cannot use the second position in 'extract'");
   }

   e->release();
   e_upto->release();
   if (upto <= from)
      return emptylist_;

   //In this case, we use extraction as a multiple cdr...
   if (upto >= sz)
      return new Listpool(lisp, this, from);
   l = lisp->provideList();
   for (;from < upto; from++)
   l->append(liste[from]->copying(false));
   return l;
}


Element* LList::extraction(LispE* lisp, List* l) {
   Element* e_from = l->liste[2];
   Element* e;

   long from = 0;
   long firstisString = -1;
   int16_t nxt = 3;
   int16_t ty;
   switch (e_from->label()) {
      case l_minus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after first operator: '-'");
      break;
      case l_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '+'");
      break;
      case l_minus_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '-+'");
      break;
      default:
         e_from = e_from->eval(lisp);
      ty = e_from->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         from = e->asInteger();
         firstisString = 0;
         break;
      }
      case t_plus_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         //We skip the first characters
         from = e->asInteger() + 1;
         firstisString = 0;
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      from = e_from->asInteger();
      break;
         default:
            e->release();
         e_from->release();
      return new Error("Error: cannot use the first position in 'extract'");
   }

   e->release();
   e_from->release();

   if (nxt == l->size()) {
      //Only one element is returned
      if (from < 0)
         e = liste.b_at_e(from*-1);
      else
         e = at_e(from);

      if (e == NULL)
         return emptylist_;

      return e->copying(false);
   }

   Element* e_upto = l->liste[nxt];
   switch (e_upto->label()) {
      case l_minus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after second operator: '-'");
      break;
      case l_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '+'");
      break;
      case l_minus_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '-+'");
      break;
      default:
         e_upto = e_upto->eval(lisp);
      ty = e_upto->type;
   }

   long upto;
   e = null_;
   long sz = size();
   switch (ty) {
      case t_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_plus_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         //All characters are integrated
         upto = e->asInteger() + 1;
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger() - 1;
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      upto = e_upto->asInteger();
         if (firstisString != -1 && upto > 0) {
         //in this case upto is a number of characters, not a position
         upto += from + firstisString;
      }
      else {
         if (upto <= 0) {
            //We start from the end...
            upto = sz + upto;
         }
      }
      break;
      default:
         e->release();
      e_upto->release();
      return new Error("Error: cannot use the second position in 'extract'");
   }

   e->release();
   e_upto->release();
   if (upto <= from)
      return emptylist_;

   //Equivalent to multiple cdr
   if (upto >= sz) {
      u_link* it = liste.at(from);
      if (it->isFinal())
         return it->value;
      return new LList(this, it);
   }


   LList* ll = new LList(liste.mark);
   u_link* it = liste.at(upto - 1);
   for (;it != NULL && upto != from; it = it->previous(), upto--) {
      ll->push_front(it->value->copying(false));
   }
   return ll;
}


Element* List::replace_in(LispE* lisp, List* l) {
   Element* e_from = l->liste[2];
   Element* e;
   Element* last = l->liste.back();
   last = last->eval(lisp);
   Element* e_upto;

   long from = 0;
   long upto;
   long firstisString = -1;
   int16_t nxt = 3;
   int16_t ty;

   switch (e_from->label()) {
      case l_minus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after first operator: '-'"));
      }
      break;
      case l_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after first operator: '+'"));
      }
      break;
      case l_minus_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after first operator: '-+'"));
      }
      break;
      default:
         e_from = e_from->eval(lisp);
      ty = e_from->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         from = e->asInteger();
         firstisString = 0;
         break;
      }
      case t_plus_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         firstisString = e->asInteger();
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         //We skip the first characters
         from = e->asInteger() + 1;
         firstisString = 0;
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         firstisString = e->asInteger();
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      from = e_from->asInteger();
         if (from < 0)
            from = size() + from;
         break;
      default:
         e->release();
      e_from->release();
      {
         last->release();
         return lisp->delegation->set_error(new Error("Error: cannot use the first position in 'setrange'"));
      }
   }

   e->release();
   e_from->release();

   if (from < 0 || from >= size()) {
      last->release();
      return this;
   }

   if (nxt == l->size() - 1) {
      //We replace our element in place at e_from
      List* l = (List*)fullcopy();
      return l->replace(lisp, from, last->copying(false));
   }

   e_upto = l->liste[nxt];
   switch (e_upto->label()) {
      case l_minus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after second operator: '-'"));
      }
      break;
      case l_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after second operator: '+'"));
      }
      break;
      case l_minus_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after second operator: '-+'"));
      }
      break;
      default:
         e_upto = e_upto->eval(lisp);
      ty = e_upto->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         upto = e->asInteger();
         break;
      }
      case t_plus_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         //All characters are integrated
         upto = e->asInteger() + 1;
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         upto = e->asInteger();
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         upto = e->asInteger() - 1;
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      upto = e_upto->asInteger();
         if (firstisString != -1 && upto > 0) {
         //in this case upto is a number of characters, not a position
         upto += from + firstisString;
      }
      else {
         if (upto <= 0) {
            //We start from the end...
            upto = size() + upto;
         }
      }
      break;
      default:
         e->release();
      e_upto->release();
      {
         last->release();
         return lisp->delegation->set_error(new Error("Error: cannot use the second position in 'setrange'"));
      }
   }

   if (thrown_error) {
      last->release();
      return lisp->delegation->set_error(thrown_error);
   }

   e->release();
   e_upto->release();

   if (upto <= from) {
      last->release();
      return this;
   }

   if (upto > size())
      upto = size();
   l = lisp->provideList();
   long i = 0;
   for (i = 0; i < from; i++)
   l->append(liste[i]->copying(false));
   l->append(last->copying(false));
   for (i = upto; i < size(); i++)
   l->append(liste[i]->copying(false));
   return l;
}


Element* LList::replace_in(LispE* lisp, List* l) {
   Element* e_from = l->liste[2];
   Element* e;
   Element* last = l->liste.back();
   last = last->eval(lisp);
   Element* e_upto;

   long from = 0;
   long upto;
   long firstisString = -1;
   int16_t nxt = 3;
   int16_t ty;
   long sz = size();

   switch (e_from->label()) {
      case l_minus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after first operator: '-'"));
      }
      break;
      case l_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after first operator: '+'"));
      }
      break;
      case l_minus_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after first operator: '-+'"));
      }
      break;
      default:
         e_from = e_from->eval(lisp);
      ty = e_from->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         from = e->asInteger();
         firstisString = 0;
         break;
      }
      case t_plus_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         firstisString = e->asInteger();
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         //We skip the first characters
         from = e->asInteger() + 1;
         firstisString = 0;
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         firstisString = e->asInteger();
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      from = e_from->asInteger();
         if (from < 0)
            from = sz + from;
         break;
      default:
         e->release();
      e_from->release();
      {
         last->release();
         return lisp->delegation->set_error(new Error("Error: cannot use the first position in 'setrange'"));
      }
   }

   e->release();
   e_from->release();

   if (from < 0 || from >= sz) {
      last->release();
      return this;
   }

   if (nxt == l->size() - 1) {
      //We replace our element in place at e_from
      LList* l = (LList*)fullcopy();
      return l->replace(lisp, from, last->copying(false));
   }

   e_upto = l->liste[nxt];
   switch (e_upto->label()) {
      case l_minus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after second operator: '-'"));
      }
      break;
      case l_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after second operator: '+'"));
      }
      break;
      case l_minus_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            {
         last->release();
         return lisp->delegation->set_error(new Error("Error: Wrong value after second operator: '-+'"));
      }
      break;
      default:
         e_upto = e_upto->eval(lisp);
      ty = e_upto->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         upto = e->asInteger();
         break;
      }
      case t_plus_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         //All characters are integrated
         upto = e->asInteger() + 1;
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         upto = e->asInteger();
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_) {
            last->release();
            return emptylist_;
         }
         upto = e->asInteger() - 1;
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      upto = e_upto->asInteger();
         if (firstisString != -1 && upto > 0) {
         //in this case upto is a number of characters, not a position
         upto += from + firstisString;
      }
      else {
         if (upto <= 0) {
            //We start from the end...
            upto = sz + upto;
         }
      }
      break;
      default:
         e->release();
      e_upto->release();
      {
         last->release();
         return lisp->delegation->set_error(new Error("Error: cannot use the second position in 'setrange'"));
      }
   }

   if (thrown_error) {
      last->release();
      return lisp->delegation->set_error(thrown_error);
   }

   e->release();
   e_upto->release();

   if (upto <= from) {
      last->release();
      return this;
   }

   LList* ll = new LList(liste.mark);
   u_link*  it;
   it = liste.last();
   for (; sz != upto && it != NULL; it = it->previous(), sz--) {
      ll->push_front(it->value->copying(false));
   }
   ll->push_front(last->copying(false));
   while (it != NULL && sz != from) {
      sz--;
      it = it->previous();
   }

   for (; it != NULL; it = it->previous())
   ll->push_front(it->value->copying(false));
   return ll;
}


Element* List::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        List* l;
        l = lisp->provideList();
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copying(false));
        }
        return l;
    }
    return this;
}

Element* LList::duplicate_constant(LispE* lisp) {
    if (status == s_constant)
        return back_duplicate();
    return this;
}

Element* List_emptylist_eval::duplicate_constant(LispE* lisp) {
    return lisp->provideList();
}

Element* LList::asList(LispE* lisp, List* l) {
    for (u_link* a = liste.begin(); a != NULL; a = a->next())
        l->append(a->value);
    return l;
}

Element* List::cadr(LispE* lisp, u_ustring& action) {
   long pos = 0;
   long sz = size();
   Element* e = this;

   for (long i = action.size() - 1; i>= 0; i--) {
      if (action[i] == 'a') {
         e = e->protected_index(lisp, pos);
         if (e == null_)
            return new Error("Error: No more elements to traverse with 'cad..r'");

         sz = e->size();
         pos = 0;
      }
      else {
         if (pos == sz)
            return new Error("Error: No more elements to traverse with 'cad..r'");
         pos++;
      }
   }

   if (pos) {
      if (pos == sz)
         return null_;
      return new Listpool(lisp, (List*)e, pos);
   }

   return e;
}


Element* LList::cadr(LispE* lisp, u_ustring& action) {
   u_link* it = liste.first;
   Element* e = this;

   for (long i = action.size() - 1; i>= 0; i--) {
      if (action[i] == 'a') {
         if (it == NULL)
            return new Error("Error: No more elements to traverse with 'cad..r'");
         e = it->value;
         if (i == 0)
            return e;
         u_ustring act = action.substr(0, i);
         return e->cadr(lisp, act);
      }
      else {
         if (it == NULL)
            return new Error("Error: No more elements to traverse with 'cad..r'");
         //We do not try to detect a cycle here...
         it = it->_next;
      }
   }

   if (it != NULL) {
      if (it->isFinal())
         return it->value;

      return new LList(this, it);
   }

   return null_;
}


Element* List::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return liste[0];
}

Element* List::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new Listpool(lisp, this, 1);
}

Element* LList::car(LispE* lisp) {
    if (liste.empty())
        return null_;
    return liste.front();
}

Element* LList::cdr(LispE* lisp) {
    u_link* it = liste.first;
    if (it == NULL || it->_next == NULL)
        return null_;
    it = it->_next;
    
    if (it->isFinal())
        return it->value;
    return new LList(this, it);
}

//--------------------------------------------------------------------------------
//Template methods for sorting
//--------------------------------------------------------------------------------
/*
template<> Element* vecte_a<long>::provide(LispE* lisp, long v) {
    return lisp->provideInteger(v);
}

*/
template <class Z> bool vecte_a<Z>::compare(LispE* lisp, List* comparison, int16_t instruction, long i, long j) {
    comparison->liste[1]->setvalue(items->buffer[i]);
    comparison->liste[2]->setvalue(items->buffer[j]);
    return comparison->eval_Boolean(lisp, instruction);
}
template <class Z>void vecte_a<Z>::values_sorting(LispE* lisp, List* comparison, int16_t instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }

    
    pivot = rmin - 1;
    comparison->liste[2]->setvalue(items->buffer[rmax]);
    for (j = rmin; j < rmax; j++) {
        comparison->liste[1]->setvalue(items->buffer[j]);
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }

    pivot++;
    swap(pivot, rmax);
    
    values_sorting(lisp, comparison, instruction, rmin, pivot-1);
    values_sorting(lisp, comparison, instruction, pivot+1, rmax);
}
//--------------------------------------------------------------------------------
template <class Z> bool vecte_n<Z>::compare(LispE* lisp, List* comparison, int16_t instruction, long i, long j) {
    comparison->liste[1]->setvalue(items->buffer[i]);
    comparison->liste[2]->setvalue(items->buffer[j]);
    return comparison->eval_Boolean(lisp, instruction);
}
template <class Z> void vecte_n<Z>::values_sorting(LispE* lisp, List* comparison, int16_t instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))
    
    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
            sz = rmin;
            for (j = rmin; j < rmax; j++) {
                if (compare(lisp, comparison, instruction, j + 1, j)) {
                    swap(j, j + 1);
                    sz = j;
                    pivot = j;
                    while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                        swap(pivot, pivot - 1);
                }
            }
            rmax = sz;
        }
        return;
    }

    
    pivot = rmin - 1;
    comparison->liste[2]->setvalue(items->buffer[rmax]);
    for (j = rmin; j < rmax; j++) {
        comparison->liste[1]->setvalue(items->buffer[j]);
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }

    pivot++;
    swap(pivot, rmax);
    
    values_sorting(lisp, comparison, instruction, rmin, pivot-1);
    values_sorting(lisp, comparison, instruction, pivot+1, rmax);
}
//--------------------------------------------------------------------------------
//Numbers methods
//--------------------------------------------------------------------------------
Element* Numbers::check_member(LispE* lisp, Element* the_set) {
    Numbers* n = lisp->provideNumbers();
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

void Numbers::sorting(LispE* lisp, List* comparison) {
   //We sort between home and last...
   long sz = size();
   if (sz <= 1)
      return;

   Constnumber n1(0);
   Constnumber n2(0);
   comparison->liste[1] = &n1;
   comparison->liste[2] = &n2;

   n1.content = liste[0];
   n2.content = liste[0];
   if (comparison->eval(lisp)->Boolean())
      {
      lisp->delegation->set_error(new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'."));
      return;
   }

   liste.values_sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}


Element* Numbers::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideNumber(liste.mini());
}

Element* Numbers::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    if (!liste.size())
        return null_;
    return lisp->provideNumber(liste.maxi());
}

Element* Numbers::minmax(LispE* lisp) {
    double v_min;
    double v_max;
    if (liste.minmax(v_min, v_max)) {
        Numbers* f = lisp->provideNumbers();
        f->liste.push_back(v_min);
        f->liste.push_back(v_max);
        return f;
    }
    return null_;
}

void Numbers::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideNumber(liste[i]));
    }
}

void Numbers::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Numbers::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Numbers::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Numbers::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Numbers::storevalue(LispE* lisp, u_ustring& s) {
    long l;
    double v = convertingfloathexa((u_uchar*)s.c_str(), l);
    liste.push_back(v);
}

Element* Numbers::invert_sign(LispE* lisp) {
    Numbers* n = this;
    if (status)
        n = lisp->provideNumbers(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

void Numbers::append(LispE* lisp, u_ustring& k) {
    long l;
    double d = convertingfloathexa((u_uchar*)k.c_str(), l);
    liste.push_back(d);
}

void Numbers::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Numbers::append(LispE* lisp, long v) {
    liste.push_back(v);
}

Element* Numbers::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   Number* element = lisp->provideNumber(0);
   lisp->recording(element, label);
   long sz = code->liste.size();
   for (long i = 0; i < liste.size(); i++) {
      _releasing(e);
      element->content = liste[i];
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Numbers::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   Numbers* l = (Numbers*)duplicate_constant(lisp);
   l->liste.insert(ix, e->asNumber());
   return l;
}


Element* Numbers::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Numbers* l = (Numbers*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Numbers::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Numbers* nb = lisp->provideNumbers();
    long i;
    std::set<double> values;
    for (i = 0; i < liste.size(); i++) {
        if (values.insert(liste[i]).second)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Numbers::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Numbers::search_element(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search(a_value->asNumber(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

bool Numbers::check_element(LispE* lisp, Element* a_value) {
    return liste.check(a_value->asNumber());
}

Element* Numbers::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    return lisp->provideInteger(liste.replaceall(a_value->asNumber(), remp->asNumber()));
}

Element* Numbers::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    liste.searchall(l->liste, a_value->asNumber(), ix);
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Numbers::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    return lisp->provideInteger(liste.count(a_value->asNumber()));
}

Element* Numbers::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   Numbers* l = lisp->provideNumbers();
   long sz = liste.size();
   for (long i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
         l->liste.push_back(liste[i]);
   }

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Numbers::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   Numbers* l = lisp->provideNumbers();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         for (i = 0; i < l->size(); i++) {
            if (l->liste[i] == a->value->asNumber())
               break;
         }
         if (i == l->size())
            l->liste.push_back(a->value->asNumber());
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long j = 0; j < sz; j++) {
            for (i = 0; i < l->size(); i++) {
               if (l->liste[i] == value->index(j)->asNumber())
                  break;
            }
            if (i == l->size())
               l->liste.push_back(value->index(j)->asNumber());
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               for (i = 0; i < l->size(); i++) {
                  if (l->liste[i] == next_value->asNumber())
                     break;
               }
               if (i == l->size())
                  l->liste.push_back(next_value->asNumber());
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Numbers::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   Numbers* l = lisp->provideNumbers();
   Numbers* intersection = (Numbers*)list_and(lisp, value);
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   double v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value->asNumber();
         if (!intersection->liste.check(v) && !l->liste.check(v))
            l->liste.push_back(v);
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (i = 0; i < sz; i++) {
            v = value->index(i)->asNumber();
            if (!intersection->liste.check(v) && !l->liste.check(v))
               l->liste.push_back(v);
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               v = next_value->asNumber();
               if (!intersection->liste.check(v) && !l->liste.check(v))
                  l->liste.push_back(v);
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   intersection->release();

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Numbers::search_reverse(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search_back(a_value->asNumber(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

Element* Numbers::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Numbers* l = lisp->provideNumbers();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Numbers::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    return null_;
}

Element* Numbers::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideNumber(liste.back());
}

Element* Numbers::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    return null_;
}

Element* Numbers::value_from_index(LispE* lisp, long i) {
    return lisp->provideNumber(liste[i]);
}

Element* Numbers::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    
    return null_;
}

Element* Numbers::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0 && i < liste.size())
      return lisp->provideNumber(liste[i]);

   return new Error("Error: index out of bounds");
}


Element* Numbers::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString(liste[i]);
    }
    return lisp->provideString(str);
}

Element* Numbers::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_numbers && liste == ((Numbers*)e)->liste)];
}

bool Numbers::egal(Element* e) {
    return (e->type == t_numbers && liste == ((Numbers*)e)->liste);
}

Element* Numbers::extraction(LispE* lisp, List* l) {
    long from;
    long sz = liste.size();
    l->evalAsInteger(2, lisp, from);
    if (from >= 0) {
        if (from >= sz)
            return emptylist_;
    }
    else {
        //We start from the end...
        from = sz + from;
        if (from < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideNumber(liste[from]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= sz)
            upto = sz;
    }
    else {
        //We start from the end...
        upto = sz + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < from) {
        return emptylist_;
    }
    
    if (upto == sz)
        return lisp->provideNumbers(this, from);
    
    Numbers* n = lisp->provideNumbers();
    for (;from < upto; from++) {
        n->liste.push_back(liste[from]);
    }
    return n;
}

Element* Numbers::replace_in(LispE* lisp, List* l) {
    Element* e_last = l->liste.back()->eval(lisp);
    double last = e_last->asNumber();
    e_last->release();
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Numbers* l = (Numbers*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Numbers* n = lisp->provideNumbers();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Numbers::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Numbers* l = lisp->provideNumbers();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Numbers::asList(LispE* lisp, List* l) {
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideNumber(liste[i]));
    return l;
}

Element* Numbers::cadr(LispE* lisp, u_ustring& action) {
   long pos = 0;
   long sz = size();
   long i;

   for (i = action.size() - 1; i>= 0; i--) {
      if (action[i] == 'a') {
         if (i) {
            u_ustring err = U"Error: You cannot apply 'cad..r' to: '";
            err += index(pos)->asUString(lisp);
            err += U"'";
            return new Error(err);
         }
         return lisp->provideNumber(liste[pos]);
      }
      if (pos == sz)
         return new Error("Error: No more elements to traverse with 'cad..r'");
      pos++;
   }

   if (pos) {
      if (pos == sz)
         return null_;
      return lisp->provideNumbers(this, pos);
   }

   return null_;
}


Element* Numbers::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideNumber(liste[0]);
}

Element* Numbers::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return lisp->provideNumbers(this, 1);
}

//--------------------------------------------------------------------------------
//Integers methods
//--------------------------------------------------------------------------------
Element* Integers::check_member(LispE* lisp, Element* the_set) {
    Integers* n = lisp->provideIntegers();
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

void Integers::sorting(LispE* lisp, List* comparison) {
   //We sort between home and last...
   long sz = size();
   if (sz <= 1)
      return;

   Constinteger n1(0);
   Constinteger n2(0);
   comparison->liste[1] = &n1;
   comparison->liste[2] = &n2;
   n1.content = liste[0];
   n2.content = liste[0];
   if (comparison->eval(lisp)->Boolean())
      {
      lisp->delegation->set_error(new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'."));
      return;
   }

   liste.values_sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}


Element* Integers::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideInteger(liste.mini());
}

Element* Integers::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideInteger(liste.maxi());
}

Element* Integers::minmax(LispE* lisp) {
    long v_min;
    long v_max;
    if (liste.minmax(v_min, v_max)) {
        Integers* f = lisp->provideIntegers();
        f->liste.push_back(v_min);
        f->liste.push_back(v_max);
        return f;
    }
    return null_;
}

void Integers::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideInteger(liste[i]));
    }
}

void Integers::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Integers::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Integers::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Integers::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Integers::storevalue(LispE* lisp, u_ustring& s) {
    long v = convertinginteger(s);
    liste.push_back(v);
}

Element* Integers::invert_sign(LispE* lisp) {
    Integers* n = this;
    if (status)
        n = lisp->provideIntegers(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

void Integers::append(LispE* lisp, u_ustring& k) {
    long d = convertinginteger(k);
    liste.push_back(d);
}

void Integers::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Integers::append(LispE* lisp, long v) {
    liste.push_back(v);
}

Element* Integers::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   Integer* element = lisp->provideInteger(0);
   lisp->recording(element, label);
   long sz = code->liste.size();
   for (long i = 0; i < liste.size(); i++) {
      _releasing(e);
      element->content = liste[i];
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Integers::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   Integers* l = (Integers*)duplicate_constant(lisp);
   l->liste.insert(ix, e->asInteger());
   return l;
}


Element* Integers::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Integers* l = (Integers*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Integers::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Integers* nb = lisp->provideIntegers();
    long i;
    std::set<long> values;
    for (i = 0; i < liste.size(); i++) {
        if (values.insert(liste[i]).second)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Integers::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Integers::search_element(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search(a_value->asInteger(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

bool Integers::check_element(LispE* lisp, Element* a_value) {
    return liste.check(a_value->asInteger());
}

Element* Integers::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    return lisp->provideInteger(liste.replaceall(a_value->asInteger(), remp->asInteger()));
}

Element* Integers::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    liste.searchall(l->liste, a_value->asInteger(), ix);
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Integers::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    return lisp->provideInteger(liste.count(a_value->asInteger()));
}

Element* Integers::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   Integers* l = lisp->provideIntegers();
   long sz = liste.size();
   for (long i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
         l->liste.push_back(liste[i]);
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Integers::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   Integers* l = lisp->provideIntegers();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         for (i = 0; i < l->size(); i++) {
            if (l->liste[i] == a->value->asInteger())
               break;
         }
         if (i == l->size())
            l->liste.push_back(a->value->asInteger());
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long j = 0; j < sz; j++) {
            for (i = 0; i < l->size(); i++) {
               if (l->liste[i] == value->index(j)->asInteger())
                  break;
            }
            if (i == l->size())
               l->liste.push_back(value->index(j)->asInteger());
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               for (i = 0; i < l->size(); i++) {
                  if (l->liste[i] == next_value->asInteger())
                     break;
               }
               if (i == l->size())
                  l->liste.push_back(next_value->asInteger());
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Integers::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   Integers* l = lisp->provideIntegers();
   Integers* intersection = (Integers*)list_and(lisp, value);
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   long v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value->asInteger();
         if (!intersection->liste.check(v) && !l->liste.check(v))
            l->liste.push_back(v);
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (i = 0; i < sz; i++) {
            v = value->index(i)->asInteger();
            if (!intersection->liste.check(v) && !l->liste.check(v))
               l->liste.push_back(v);
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               v = next_value->asInteger();
               if (!intersection->liste.check(v) && !l->liste.check(v))
                  l->liste.push_back(v);
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   intersection->release();

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Integers::search_reverse(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search_back(a_value->asInteger(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

Element* Integers::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Integers* l = lisp->provideIntegers();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Integers::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    return null_;
}

Element* Integers::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideInteger(liste.back());
}

Element* Integers::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    return null_;
}

Element* Integers::value_from_index(LispE* lisp, long i) {
    return lisp->provideInteger(liste[i]);
}

Element* Integers::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    
    return null_;
}

Element* Integers::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0 && i < liste.size())
      return lisp->provideInteger(liste[i]);

   return new Error("Error: index out of bounds");
}


Element* Integers::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString(liste[i]);
    }
    return lisp->provideString(str);
}

Element* Integers::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_integers && liste == ((Integers*)e)->liste)];
}

bool Integers::egal(Element* e) {
    return (e->type == t_integers && liste == ((Integers*)e)->liste);
}

Element* Integers::extraction(LispE* lisp, List* l) {
    long from;
    long sz = liste.size();
    
    l->evalAsInteger(2, lisp, from);
    if (from >= 0) {
        if (from >= sz)
            return emptylist_;
    }
    else {
        //We start from the end...
        from = sz + from;
        if (from < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideInteger(liste[from]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= sz)
            upto = sz;
    }
    else {
        //We start from the end...
        upto = sz + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < from) {
        return emptylist_;
    }
    
    //In this case we use it as a multiple cdr
    if (upto == sz)
        return lisp->provideIntegers(this, from);
    
    Integers* n = lisp->provideIntegers();
    for (;from < upto; from++) {
        n->liste.push_back(liste[from]);
    }
    return n;
}

Element* Integers::replace_in(LispE* lisp, List* l) {
    Element* elast = l->liste.back()->eval(lisp);
    long last = elast->asInteger();
    elast->release();
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Integers* l = (Integers*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Integers* n = lisp->provideIntegers();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Integers::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Integers* l = lisp->provideIntegers();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Integers::asList(LispE* lisp, List* l) {
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideInteger(liste[i]));
    return l;
}

Element* Integers::cadr(LispE* lisp, u_ustring& action) {
   long pos = 0;
   long sz = size();
   long i;

   for (i = action.size() - 1; i>= 0; i--) {
      for (i = action.size() - 1; i>= 0; i--) {
         if (action[i] == 'a') {
            if (i) {
               u_ustring err = U"Error: You cannot apply 'cad..r' to: '";
               err += index(pos)->asUString(lisp);
               err += U"'";
               return new Error(err);
            }
            return lisp->provideInteger(liste[pos]);
         }
         if (pos == sz)
            return new Error("Error: No more elements to traverse with 'cad..r'");
         pos++;
      }
   }

   if (pos) {
      if (pos == sz)
         return null_;
      return lisp->provideIntegers(this, pos);
   }

   return null_;
}


Element* Integers::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideInteger(liste[0]);
}

Element* Integers::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return lisp->provideIntegers(this, 1);
}

//--------------------------------------------------------------------------------
//Strings methods
//--------------------------------------------------------------------------------
Element* Strings::check_member(LispE* lisp, Element* the_set) {
    Strings* n = lisp->provideStrings();
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

void Strings::sorting(LispE* lisp, List* comparison) {
   //We sort between home and last...
   long sz = size();
   if (sz <= 1)
      return;

   Conststring n1(U"");
   Conststring n2(U"");
   comparison->liste[1] = &n1;
   comparison->liste[2] = &n2;
   n1.content = liste[0];
   n2.content = liste[0];
   if (comparison->eval(lisp)->Boolean())
      {
      lisp->delegation->set_error(new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'."));
      return;
   }

   liste.values_sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}


Element* Strings::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring u = liste.mini();
    return lisp->provideString(u);
}

Element* Strings::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring u = liste.maxi();
    return lisp->provideString(u);
}

Element* Strings::minmax(LispE* lisp) {
    u_ustring v_min;
    u_ustring v_max;
    if (liste.minmax(v_min, v_max)) {
        Strings* f = lisp->provideStrings();
        f->liste.push_back(v_min);
        f->liste.push_back(v_max);
        return f;
    }
    return null_;
}

void Strings::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideString(liste[i]));
    }
}

void Strings::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(convertingfloathexa(liste[i].c_str()));
    }
}

void Strings::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(convertingfloathexa(liste[i].c_str()));
    }
}

void Strings::storevalue(LispE* lisp, double v) {
    liste.push_back(convertToUString(v));
}

void Strings::storevalue(LispE* lisp,long v) {
    liste.push_back(convertToUString(v));
}

void Strings::storevalue(LispE* lisp, u_ustring& s) {
    liste.push_back(s);
}

void Strings::append(LispE* lisp, u_ustring& k) {
    liste.push_back(k);
}

void Strings::append(LispE* lisp, double v) {
    liste.push_back(convertToUString(v));
}

void Strings::append(LispE* lisp, long v) {
    liste.push_back(convertToUString(v));
}

Element* Strings::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   String* element = lisp->provideString();
   lisp->recording(element, label);

   long sz = code->liste.size();
   for (long i = 0; i < liste.size(); i++) {
      _releasing(e);
      element->content = liste[i];
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Strings::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   Strings* l = (Strings*)duplicate_constant(lisp);
   l->liste.insert(ix, e->asUString(lisp));
   return l;
}


Element* Strings::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Strings* l = (Strings*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Strings::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Strings* nb = lisp->provideStrings();
    long i;
    std::set<u_ustring> values;
    for (i = 0; i < liste.size(); i++) {
        if (values.insert(liste[i]).second)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Strings::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Strings::search_element(LispE* lisp, Element* a_value, long ix) {
    u_ustring v = a_value->asUString(lisp);
    long pos = liste.search(v, ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

bool Strings::check_element(LispE* lisp, Element* a_value) {
    u_ustring v = a_value->asUString(lisp);
    return liste.check(v);
}

Element* Strings::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    u_ustring v = a_value->asUString(lisp);
    u_ustring r = remp->asUString(lisp);
    return lisp->provideInteger(liste.replaceall(v, r));
}

Element* Strings::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    u_ustring v = a_value->asUString(lisp);
    liste.searchall(l->liste, v, ix);
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Strings::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    u_ustring str = a_value->asUString(lisp);
    return lisp->provideInteger(liste.count(str));
}

Element* Strings::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   Strings* l = lisp->provideStrings();
   long sz = liste.size();
   for (long i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
         l->liste.push_back(liste[i]);
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Strings::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   Strings* l = lisp->provideStrings();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         for (i = 0; i < l->size(); i++) {
            if (l->liste[i] == a->value->asUString(lisp))
               break;
         }
         if (i == l->size())
            l->liste.push_back(a->value->asUString(lisp));
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long j = 0; j < sz; j++) {
            for (i = 0; i < l->size(); i++) {
               if (l->liste[i] == value->index(j)->asUString(lisp))
                  break;
            }
            if (i == l->size())
               l->liste.push_back(value->index(j)->asUString(lisp));
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               for (i = 0; i < l->size(); i++) {
                  if (l->liste[i] == next_value->asUString(lisp))
                     break;
               }
               if (i == l->size())
                  l->liste.push_back(next_value->asUString(lisp));
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Strings::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   Strings* l = lisp->provideStrings();
   Strings* intersection = (Strings*)list_and(lisp, value);
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   u_ustring v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value->asUString(lisp);
         if (!intersection->liste.check(v) && !l->liste.check(v))
            l->liste.push_back(v);
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (i = 0; i < sz; i++) {
            v = value->index(i)->asUString(lisp);
            if (!intersection->liste.check(v) && !l->liste.check(v))
               l->liste.push_back(v);
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               v = next_value->asUString(lisp);
               if (!intersection->liste.check(v) && !l->liste.check(v))
                  l->liste.push_back(v);
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }

   intersection->release();

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Strings::search_reverse(LispE* lisp, Element* a_value, long ix) {
    u_ustring v = a_value->asUString(lisp);
    long pos = liste.search_back(v, ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

Element* Strings::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Strings* l = lisp->provideStrings();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Strings::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    return null_;
}

Element* Strings::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideString(liste.back());
}

Element* Strings::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    return null_;
}

Element* Strings::value_from_index(LispE* lisp, long i) {
    return lisp->provideString(liste[i]);
}

Element* Strings::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    
    return null_;
}

Element* Strings::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0 && i < liste.size())
      return lisp->provideString(liste[i]);

   return new Error("Error: index out of bounds");
}


Element* Strings::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += liste[i];
    }
    return lisp->provideString(str);
}

Element* Strings::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_strings && liste == ((Strings*)e)->liste)];
}

bool Strings::egal(Element* e) {
    return (e->type == t_strings && liste == ((Strings*)e)->liste);
}

Element* Strings::plus(LispE* lisp, Element* e) {
    //Two cases either e is a string or it is a list...
    if (e == NULL) {
        u_ustring d = liste.sum();
        return lisp->provideString(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asUString(lisp);
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] += e->asUString(lisp);
    }
    return this;
}

Element* Strings::extraction(LispE* lisp, List* l) {

   Element* e_from = l->liste[2];
   Element* e;
   long sz = liste.size();

   long from = 0;
   long firstisString = -1;
   int16_t nxt = 3;
   int16_t ty;
   switch (e_from->label()) {
      case l_minus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after first operator: '-'");
      break;
      case l_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '+'");
      break;
      case l_minus_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '-+'");
      break;
      default:
         e_from = e_from->eval(lisp);
      ty = e_from->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         from = e->asInteger();
         firstisString = 0;
         break;
      }
      case t_plus_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         //We skip the first characters
         from = e->asInteger() + 1;
         firstisString = 0;
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      from = e_from->asInteger();
         if (from < 0)
            from = sz + from;
         break;
      default:
         e->release();
      e_from->release();
      return new Error("Error: cannot use the first position in 'extract'");
   }

   e->release();
   e_from->release();

   if (from < 0 || from >= sz)
      return emptylist_;

   if (nxt == l->size()) {
      //Only one element is returned
      return lisp->provideString(liste[from]);
   }

   Element* e_upto = l->liste[nxt];
   switch (e_upto->label()) {
      case l_minus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after second operator: '-'");
      break;
      case l_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '+'");
      break;
      case l_minus_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '-+'");
      break;
      default:
         e_upto = e_upto->eval(lisp);
      ty = e_upto->type;
   }

   long upto;
   e = null_;
   switch (ty) {
      case t_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_plus_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         //All characters are integrated
         upto = e->asInteger() + 1;
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger() - 1;
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      upto = e_upto->asInteger();
         if (firstisString != -1 && upto > 0) {
         //in this case upto is a number of characters, not a position
         upto += from + firstisString;
      }
      else {
         if (upto <= 0) {
            //We start from the end...
            upto = sz + upto;
         }
      }
      break;
      default:
         e->release();
      e_upto->release();
      return new Error("Error: cannot use the second position in 'extract'");
   }

   e->release();
   e_upto->release();
   if (upto <= from)
      return emptylist_;

   if (upto >= sz)
      return new Stringspool(lisp, this, from);

   Strings* n = lisp->provideStrings();
   for (;from < upto; from++) {
      n->liste.push_back(liste[from]);
   }
   return n;
}


Element* Strings::replace_in(LispE* lisp, List* l) {
   Element* e = l->liste.back()->eval(lisp);
   u_ustring last = e->asUString(lisp);
   e->release();
   Element* e_from = l->liste[2];

   long from = 0;
   long firstisString = -1;
   int16_t nxt = 3;
   int16_t ty;
   switch (e_from->label()) {
      case l_minus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after first operator: '-'");
      break;
      case l_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '+'");
      break;
      case l_minus_plus:
         e_from = l->liste[3]->eval(lisp);
      nxt = 4;
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after first operator: '-+'");
      break;
      default:
         e_from = e_from->eval(lisp);
      ty = e_from->type;
   }

   e = null_;
   switch (ty) {
      case t_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         from = e->asInteger();
         firstisString = 0;
         break;
      }
      case t_plus_string: {
         e = search_element(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         //We skip the first characters
         from = e->asInteger() + 1;
         firstisString = 0;
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_from, 0);
         if (e == null_)
            return emptylist_;
         firstisString = e->asInteger();
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      from = e_from->asInteger();
         if (from < 0)
            from = size() + from;
         break;
      default:
         e->release();
      e_from->release();
      return new Error("Error: cannot use the first position in 'setrange'");
   }

   e->release();
   e_from->release();

   if (from < 0 || from >= size())
      return this;

   if (nxt == l->size() - 1) {
      //Only one element is returned
      //On returns only one element
      Strings* l = (Strings*)fullcopy();
      l->liste[from] = last;
      return l;
   }

   Element* e_upto = l->liste[nxt];
   switch (e_upto->label()) {
      case l_minus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_minus_string;
         else
            return new Error("Error: Wrong value after second operator: '-'");
      break;
      case l_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_upto->type;
      if (ty == t_string)
            ty = t_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '+'");
      break;
      case l_minus_plus:
         e_upto = l->liste[nxt+1]->eval(lisp);
      ty = e_from->type;
      if (ty == t_string)
            ty = t_minus_plus_string;
         else
            return new Error("Error: Wrong value after second operator: '-+'");
      break;
      default:
         e_upto = e_upto->eval(lisp);
      ty = e_upto->type;
   }

   long upto;
   e = null_;
   switch (ty) {
      case t_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_plus_string: {
         if (firstisString == -1) firstisString = 0;
         e = search_element(lisp, e_upto, from + firstisString);
         if (e == null_)
            return emptylist_;
         //All characters are integrated
         upto = e->asInteger() + 1;
         break;
      }
      case t_minus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger();
         break;
      }
      case t_minus_plus_string: {
         e = search_reverse(lisp, e_upto, 0);
         if (e == null_)
            return emptylist_;
         upto = e->asInteger() - 1;
         break;
      }
      case t_float:
      case t_short:
      case t_integer:
      case t_number:
      upto = e_upto->asInteger();
         if (firstisString != -1 && upto > 0) {
         //in this case upto is a number of characters, not a position
         upto += from + firstisString;
      }
      else {
         if (upto <= 0) {
            //We start from the end...
            upto = size() + upto;
         }
      }
      break;
      default:
         e->release();
      e_upto->release();
      return new Error("Error: cannot use the second position in 'setrange'");
   }

   e->release();
   e_upto->release();
   if (upto <= from)
      return emptylist_;

   if (upto > size())
      upto = size();
   Strings* n = lisp->provideStrings();
   long i;
   for (i = 0; i < from; i++)
   n->liste.push_back(liste[i]);
   n->liste.push_back(last);
   for (i = upto; i < size(); i++)
   n->liste.push_back(liste[i]);
   return n;
}


Element* Strings::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Strings* l = lisp->provideStrings();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Strings::asList(LispE* lisp, List* l) {
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideString(liste[i]));
    return l;
}

Element* Strings::cadr(LispE* lisp, u_ustring& action) {
   long pos = 0;
   long sz = size();
   long i;

   for (i = action.size() - 1; i>= 0; i--) {
      if (action[i] == 'a') {
         if (!i)
            return lisp->provideString(liste[pos]);
         exchange_value.content = liste[pos];
         u_ustring nxt = action.substr(0, i);
         return exchange_value.cadr(lisp, nxt);
      }

      if (pos == sz)
         return new Error("Error: No more elements to traverse with 'cad..r'");
      pos++;
   }

   if (pos) {
      if (pos == sz)
         return null_;
      return new Strings(this, pos);
   }

   return null_;
}


Element* Strings::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideString(liste[0]);
}

Element* Strings::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new Strings(this, 1);
}

//--------------------------------------------------------------------------------
//Shorts methods
//--------------------------------------------------------------------------------
Element* Shorts::check_member(LispE* lisp, Element* the_set) {
    Shorts* n = new Shorts();
    long v;
    long i, j;
    long sz = the_set->size();
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            v = the_set->index(i)->asShort();
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

void Shorts::sorting(LispE* lisp, List* comparison) {
   //We sort between home and last...
   long sz = size();
   if (sz <= 1)
      return;

   Constshort n1(0);
   Constshort n2(0);
   comparison->liste[1] = &n1;
   comparison->liste[2] = &n2;
   n1.content = liste[0];
   n2.content = liste[0];
   if (comparison->eval(lisp)->Boolean())
      {
      lisp->delegation->set_error(new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'."));
      return;
   }

   liste.values_sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}


Element* Shorts::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return new Short(liste.mini());
}

Element* Shorts::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return new Short(liste.maxi());
}

Element* Shorts::minmax(LispE* lisp) {
    int16_t v_min;
    int16_t v_max;
    if (liste.minmax(v_min, v_max)) {
        Shorts* f = new Shorts();
        f->liste.push_back(v_min);
        f->liste.push_back(v_max);
        return f;
    }
    return null_;
}

void Shorts::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(new Short(liste[i]));
    }
}

void Shorts::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Shorts::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Shorts::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Shorts::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Shorts::storevalue(LispE* lisp, u_ustring& s) {
    long v = convertinginteger(s);
    liste.push_back(v);
}

Element* Shorts::invert_sign(LispE* lisp) {
    Shorts* n = this;
    if (status)
        n = new Shorts(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

void Shorts::append(LispE* lisp, u_ustring& k) {
    int16_t d = (int16_t)convertinginteger(k);
    liste.push_back(d);
}

void Shorts::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Shorts::append(LispE* lisp, long v) {
    liste.push_back(v);
}

Element* Shorts::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   Short* element = new Short(0);
   lisp->recording(element, label);
   long sz = code->liste.size();
   for (long i = 0; i < liste.size(); i++) {
      _releasing(e);
      element->content = liste[i];
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Shorts::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   Shorts* l = (Shorts*)duplicate_constant(lisp);
   l->liste.insert(ix, e->asInteger());
   return l;
}


Element* Shorts::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Shorts* l = (Shorts*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Shorts::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Shorts* nb = new Shorts();
    long i;
    std::set<int16_t> values;
    for (i = 0; i < liste.size(); i++) {
        if (values.insert(liste[i]).second)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Shorts::thekeys(LispE* lisp) {
    Shorts* keys = new Shorts();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Shorts::search_element(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search(a_value->asShort(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

bool Shorts::check_element(LispE* lisp, Element* a_value) {
    return liste.check(a_value->asShort());
}

Element* Shorts::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    return lisp->provideInteger(liste.replaceall(a_value->asShort(), remp->asShort()));
}

Element* Shorts::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    liste.searchall(l->liste, a_value->asShort(), ix);
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Shorts::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    return lisp->provideInteger(liste.count(a_value->asShort()));
}

Element* Shorts::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   Shorts* l = new Shorts();
   long sz = liste.size();
   for (long i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
         l->liste.push_back(liste[i]);
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Shorts::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   Shorts* l = new Shorts();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         for (i = 0; i < l->size(); i++) {
            if (l->liste[i] == a->value->asShort())
               break;
         }
         if (i == l->size())
            l->liste.push_back(a->value->asShort());
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long j = 0; j < sz; j++) {
            for (i = 0; i < l->size(); i++) {
               if (l->liste[i] == value->index(j)->asShort())
                  break;
            }
            if (i == l->size())
               l->liste.push_back(value->index(j)->asShort());
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               for (i = 0; i < l->size(); i++) {
                  if (l->liste[i] == next_value->asShort())
                     break;
               }
               if (i == l->size())
                  l->liste.push_back(next_value->asShort());
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Shorts::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   Shorts* l = new Shorts();
   Shorts* intersection = (Shorts*)list_and(lisp, value);
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   int16_t v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value->asShort();
         if (!intersection->liste.check(v) && !l->liste.check(v))
            l->liste.push_back(v);
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (i = 0; i < sz; i++) {
            v = value->index(i)->asShort();
            if (!intersection->liste.check(v) && !l->liste.check(v))
               l->liste.push_back(v);
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               v = next_value->asShort();
               if (!intersection->liste.check(v) && !l->liste.check(v))
                  l->liste.push_back(v);
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   intersection->release();

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Shorts::search_reverse(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search_back(a_value->asShort(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

Element* Shorts::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Shorts* l = new Shorts();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Shorts::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    return null_;
}

Element* Shorts::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return new Short(liste.back());
}

Element* Shorts::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    return null_;
}

Element* Shorts::value_from_index(LispE* lisp, long i) {
    return new Short(liste[i]);
}

Element* Shorts::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return new Short(liste[i]);
    
    return null_;
}

Element* Shorts::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0 && i < liste.size())
      return new Short(liste[i]);

   return new Error("Error: index out of bounds");
}


Element* Shorts::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString((long)liste[i]);
    }
    return lisp->provideString(str);
}

Element* Shorts::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_shorts && liste == ((Shorts*)e)->liste)];
}

bool Shorts::egal(Element* e) {
    return (e->type == t_shorts && liste == ((Shorts*)e)->liste);
}

Element* Shorts::extraction(LispE* lisp, List* l) {
    long from;
    long sz = liste.size();
    
    l->evalAsInteger(2, lisp, from);
    if (from >= 0) {
        if (from >= sz)
            return emptylist_;
    }
    else {
        //We start from the end...
        from = sz + from;
        if (from < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return new Short(liste[from]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= sz)
            upto = sz;
    }
    else {
        //We start from the end...
        upto = sz + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < from) {
        return emptylist_;
    }
    
    if (upto == sz)
        return new Shorts(this, from);
    
    Shorts* n = new Shorts();
    for (;from < upto; from++) {
        n->liste.push_back(liste[from]);
    }
    return n;
}

Element* Shorts::replace_in(LispE* lisp, List* l) {
    Element* elast = l->liste.back()->eval(lisp);
    long last = elast->asInteger();
    elast->release();
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Shorts* l = (Shorts*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Shorts* n = new Shorts();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Shorts::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Shorts* l = new Shorts;
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Shorts::asList(LispE* lisp, List* l) {
    for (long i = 0; i < liste.size(); i++)
        l->append(new Short(liste[i]));
    return l;
}

Element* Shorts::cadr(LispE* lisp, u_ustring& action) {
   long pos = 0;
   long sz = size();
   long i;

   for (i = action.size() - 1; i>= 0; i--) {
      for (i = action.size() - 1; i>= 0; i--) {
         if (action[i] == 'a') {
            if (i) {
               u_ustring err = U"Error: You cannot apply 'cad..r' to: '";
               err += index(pos)->asUString(lisp);
               err += U"'";
               return new Error(err);
            }
            return new Short(liste[pos]);
         }
         if (pos == sz)
            return new Error("Error: No more elements to traverse with 'cad..r'");
         pos++;
      }
   }

   if (pos) {
      if (pos == sz)
         return null_;
      return new Shorts(this, pos);
   }

   return null_;
}


Element* Shorts::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return new Short(liste[0]);
}

Element* Shorts::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new Shorts(this, 1);
}

//--------------------------------------------------------------------------------
//Floats methods
//--------------------------------------------------------------------------------
Element* Floats::check_member(LispE* lisp, Element* the_set) {
    Floats* n = lisp->provideFloats();
    float v;
    long i, j;
    long sz = the_set->size();
    for (j = 0; j < size(); j++) {
        for (i = 0; i < sz; i++) {
            v = the_set->index(i)->asFloat();
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

void Floats::sorting(LispE* lisp, List* comparison) {
   //We sort between home and last...
   long sz = size();
   if (sz <= 1)
      return;

   Constfloat n1(0);
   Constfloat n2(0);
   comparison->liste[1] = &n1;
   comparison->liste[2] = &n2;

   n1.content = liste[0];
   n2.content = liste[0];
   if (comparison->eval(lisp)->Boolean())
      {
      lisp->delegation->set_error(new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'."));
      return;
   }

   liste.values_sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}


Element* Floats::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideFloat(liste.mini());
}

Element* Floats::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideFloat(liste.maxi());
}

Element* Floats::minmax(LispE* lisp) {
    float v_min;
    float v_max;
    if (liste.minmax(v_min, v_max)) {
        Floats* f = lisp->provideFloats();
        f->liste.push_back(v_min);
        f->liste.push_back(v_max);
        return f;
    }
    return null_;
}

void Element::flatten(LispE* lisp, Floats* l) {
    l->append(this);
}

void Floats::flatten(LispE* lisp, List* l) {
    for (long i = 0; i < size(); i++) {
        l->append(lisp->provideFloat(liste[i]));
    }
}

void Floats::flatten(LispE* lisp, Numbers* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Floats::flatten(LispE* lisp, Floats* l) {
    for (long i = 0; i < size(); i++) {
        l->liste.push_back(liste[i]);
    }
}

void Floats::storevalue(LispE* lisp, double v) {
    liste.push_back(v);
}

void Floats::storevalue(LispE* lisp, float v) {
    liste.push_back(v);
}

void Floats::storevalue(LispE* lisp,long v) {
    liste.push_back(v);
}

void Floats::storevalue(LispE* lisp, u_ustring& s) {
    long l;
    float v = convertingfloathexa((u_uchar*)s.c_str(), l);
    liste.push_back(v);
}

Element* Floats::invert_sign(LispE* lisp) {
    Floats* n = this;
    if (status)
        n = lisp->provideFloats(this);
    
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

void Floats::append(LispE* lisp, u_ustring& k) {
    long l;
    float d = convertingfloathexa((u_uchar*)k.c_str(), l);
    liste.push_back(d);
}

void Floats::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Floats::append(LispE* lisp, float v) {
    liste.push_back(v);
}

void Floats::append(LispE* lisp, long v) {
    liste.push_back(v);
}

Element* Floats::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   Float* element = lisp->provideFloat(0);
   lisp->recording(element, label);
   long sz = code->liste.size();
   for (long i = 0; i < liste.size(); i++) {
      _releasing(e);
      element->content = liste[i];
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Floats::insert(LispE* lisp, Element* e, long ix) {
   if (ix < 0)
      return new Error("Error: Wrong index in 'insert'");

   Floats* l = (Floats*)duplicate_constant(lisp);
   l->liste.insert(ix, e->asFloat());
   return l;
}


Element* Floats::rotate(bool left) {
    if (liste.size() <= 1)
        return this;
    
    Floats* l = (Floats*)newInstance();
    if (left) {
        for (long i = 1; i < liste.size(); i++)
            l->liste.push_back(liste[i]);
        l->liste.push_back(liste[0]);
        return l;
    }
    
    l->liste.push_back(liste.back());
    for (long i = 0; i < liste.size() - 1; i ++)
        l->liste.push_back(liste[i]);
    return l;
}

Element* Floats::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    Floats* nb = lisp->provideFloats();
    long i;
    std::set<float> values;
    for (i = 0; i < liste.size(); i++) {
        if (values.insert(liste[i]).second)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Floats::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Floats::search_element(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search(a_value->asFloat(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

bool Floats::check_element(LispE* lisp, Element* a_value) {
    return liste.check(a_value->asFloat());
}

Element* Floats::replace_all_elements(LispE* lisp, Element* a_value, Element* remp) {
    return lisp->provideInteger(liste.replaceall(a_value->asFloat(), remp->asFloat()));
}

Element* Floats::search_all_elements(LispE* lisp, Element* a_value, long ix) {
    Integers* l = lisp->provideIntegers();
    liste.searchall(l->liste, a_value->asFloat(), ix);
    if (l->liste.size() == 0) {
        l->release();
        return emptylist_;
    }
    return l;
}

Element* Floats::count_all_elements(LispE* lisp, Element* a_value, long ix) {
    return lisp->provideInteger(liste.count(a_value->asFloat()));
}

Element* Floats::list_and(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '&&&' to strings, lists or sets");

   Floats* l = lisp->provideFloats();
   long sz = liste.size();
   for (long i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]) && value->check_element(lisp, index(i)))
         l->liste.push_back(liste[i]);
   }
   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Floats::list_or(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '|||' to strings, lists or sets");

   Floats* l = lisp->provideFloats();
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         for (i = 0; i < l->size(); i++) {
            if (l->liste[i] == a->value->asFloat())
               break;
         }
         if (i == l->size())
            l->liste.push_back(a->value->asFloat());
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (long j = 0; j < sz; j++) {
            for (i = 0; i < l->size(); i++) {
               if (l->liste[i] == value->index(j)->asFloat())
                  break;
            }
            if (i == l->size())
               l->liste.push_back(value->index(j)->asFloat());
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               for (i = 0; i < l->size(); i++) {
                  if (l->liste[i] == next_value->asFloat())
                     break;
               }
               if (i == l->size())
                  l->liste.push_back(next_value->asFloat());
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Floats::list_xor(LispE* lisp, Element* value) {
   if (!value->isList() && !value->isSet())
      return new Error("Error: Can only apply '^^^' to strings, lists or sets");

   Floats* l = lisp->provideFloats();
   Floats* intersection = (Floats*)list_and(lisp, value);
   long sz = liste.size();
   long i;
   for (i = 0; i < sz; i++) {
      if (!intersection->liste.check(liste[i]) && !l->liste.check(liste[i]))
         l->liste.push_back(liste[i]);
   }
   float v;
   if (value->type == t_llist) {
      u_link* a = ((LList*)value)->liste.begin();
      for (; a != NULL; a = a->next()) {
         v = a->value->asFloat();
         if (!intersection->liste.check(v) && !l->liste.check(v))
            l->liste.push_back(v);
      }
   }
   else {
      if (value->isList()) {
         long sz = value->size();
         for (i = 0; i < sz; i++) {
            v = value->index(i)->asFloat();
            if (!intersection->liste.check(v) && !l->liste.check(v))
               l->liste.push_back(v);
         }
      }
      else {
         if (value->isSet()) {
            void* iter = value->begin_iter();
            Element* next_value = value->next_iter_exchange(lisp, iter);
            while (next_value != emptyatom_) {
               v = next_value->asFloat();
               if (!intersection->liste.check(v) && !l->liste.check(v))
                  l->liste.push_back(v);
               next_value = value->next_iter_exchange(lisp, iter);
            }
            value->clean_iter(iter);
         }
      }
   }
   intersection->release();

   if (l->liste.size() == 0) {
      l->release();
      return emptylist_;
   }
   return l;
}


Element* Floats::search_reverse(LispE* lisp, Element* a_value, long ix) {
    long pos = liste.search_back(a_value->asFloat(), ix);
    return (pos == -1)?null_:lisp->provideInteger(pos);
}

Element* Floats::reverse(LispE* lisp, bool duplicate) {
    if (liste.size() <= 1)
        return this;
    
    if (duplicate) {
        Floats* l = lisp->provideFloats();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }
    
    liste.reverse();
    return this;
}

Element* Floats::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    return null_;
}

Element* Floats::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideFloat(liste.back());
}

Element* Floats::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    return null_;
}

Element* Floats::value_from_index(LispE* lisp, long i) {
    return lisp->provideFloat(liste[i]);
}

Element* Floats::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideFloat(liste[i]);
    
    return null_;
}

Element* Floats::protected_index(LispE* lisp, Element* ix) {
   long i = ix->checkInteger(lisp);
   if (i < 0)
      i = liste.size() + i;

   if (i >= 0 && i < liste.size())
      return lisp->provideFloat(liste[i]);

   return new Error("Error: index out of bounds");
}


Element* Floats::join_in_list(LispE* lisp, u_ustring& sep) {
    u_ustring str;
    u_ustring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += convertToUString(liste[i]);
    }
    return lisp->provideString(str);
}

Element* Floats::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_floats && liste == ((Floats*)e)->liste)];
}

bool Floats::egal(Element* e) {
    return (e->type == t_floats && liste == ((Floats*)e)->liste);
}

Element* Floats::extraction(LispE* lisp, List* l) {
    long sz = liste.size();
    long from;
    l->evalAsInteger(2, lisp, from);
    if (from >= 0) {
        if (from >= sz)
            return emptylist_;
    }
    else {
        //We start from the end...
        from = sz + from;
        if (from < 0)
            return emptylist_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideFloat(liste[from]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= sz)
            upto = sz;
    }
    else {
        //We start from the end...
        upto = sz + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < from) {
        return emptylist_;
    }
    
    if (upto == sz)
        return lisp->provideFloats(this, from);
    
    Floats* n = lisp->provideFloats();
    for (;from < upto; from++) {
        n->liste.push_back(liste[from]);
    }
    return n;
}

Element* Floats::replace_in(LispE* lisp, List* l) {
    Element* e_last = l->liste.back()->eval(lisp);
    float last = e_last->asFloat();
    e_last->release();
    
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return emptylist_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return emptylist_;
    }
    if (l->size() == 4) {
        //On returns only one element
        Floats* l = (Floats*)fullcopy();
        l->liste[depuis] = last;
        return l;
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto > 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return emptylist_;
    }
    if (upto < depuis) {
        return emptylist_;
    }
    
    Floats* n = lisp->provideFloats();
    long i;
    for (i = 0; i < depuis; i++)
        n->liste.push_back(liste[i]);
    n->liste.push_back(last);
    for (i = upto; i < size(); i++)
        n->liste.push_back(liste[i]);
    return n;
}

Element* Floats::duplicate_constant(LispE* lisp) {
    if (status == s_constant) {
        Floats* l = lisp->provideFloats();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Floats::asList(LispE* lisp, List* l) {
    for (long i = 0; i < liste.size(); i++)
        l->append(lisp->provideFloat(liste[i]));
    return l;
}

Element* Floats::cadr(LispE* lisp, u_ustring& action) {
   long pos = 0;
   long sz = size();
   long i;

   for (i = action.size() - 1; i>= 0; i--) {
      if (action[i] == 'a') {
         if (i) {
            u_ustring err = U"Error: You cannot apply 'cad..r' to: '";
            err += index(pos)->asUString(lisp);
            err += U"'";
            return new Error(err);
         }
         return lisp->provideFloat(liste[pos]);
      }
      if (pos == sz)
         return new Error("Error: No more elements to traverse with 'cad..r'");
      pos++;
   }
   if (pos) {
      if (pos == sz)
         return null_;
      return lisp->provideFloats(this, pos);
   }

   return null_;
}


Element* Floats::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideFloat(liste[0]);
}

Element* Floats::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return lisp->provideFloats(this, 1);
}

//--------------------------------------------------------------------------------
//Matrice methods
//--------------------------------------------------------------------------------
Element* Matrice::check_member(LispE* lisp,Element* the_set) {
    Matrice* r = new Matrice;
    r->size_x = size_x;
    r->size_y = size_y;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Matrice::transposed(LispE* lisp) {
    Matrice* transposed_matrix = new Matrice(lisp, size_y, size_x, 0.0);
    long i, j = 0;
    
    Element* e;
    for (i = 0; i < size_x; i++) {
        e = liste[i];
        for (j = 0; j < size_y; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Matrice::rank(LispE* lisp, vecte<long>& positions) {
   int16_t sz = positions.size();
   if (!sz || sz > 2)
      return new Error("Error: index mismatch");

   if (positions[0] == -1) {
      //We return all columns
      if (sz == 1 || positions[1] == -1) {
         Matrice* m = new Matrice();
         Numbers* result;

         for (long j = 0; j < size_y; j++) {
            result = lisp->provideNumbers();
            for (long i = 0; i < size_x; i++) {
               result->liste.push_back(val(i,j));
            }
            m->append(result);
         }
         m->size_x = size_y;
         m->size_y = size_x;
         return m;
      }
      else {
         if (positions[1] >= size_y)
            return new Error("Error: indexes out of bounds");
      }
   }
   else {
      if (sz == 2 && positions[1] != -1) {
         if (positions[1] >= size_y)
            return new Error("Error: indexes out of bounds");
         return lisp->provideNumber(val(positions[0], positions[1]));
      }

      if (positions[0] >= size_x)
         return new Error("Error: indexes out of bounds");

      return lisp->provideNumbers(((Numbers*)liste[positions[0]]));
   }
   Numbers* result = lisp->provideNumbers();
   for (long i = 0; i < size_x; i++) {
      result->liste.push_back(val(i,positions[1]));
   }
   return result;
}


Element* Matrice::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   lisp->recording(null_, label);
   long sz = code->liste.size();
   for (long i = 0; i < size_x; i++) {
      lisp->replacingvalue(liste[i], label);
      _releasing(e);
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Matrice::rotate(LispE* lisp, long axis) {
    Matrice* revert_matrix = new Matrice;
    revert_matrix->size_x = size_x;
    revert_matrix->size_y = size_y;
    
    long i;
    
    if (axis == 1) {
        for (i = 0; i < size_x; i++) {
            revert_matrix->append(liste[i]->rotate(lisp,0));
        }
        return revert_matrix;
    }
    
    Element* e;
    for (i = size_x-1; i>= 0;  i--) {
        e = lisp->provideNumbers((Numbers*)liste[i]);
        revert_matrix->append(e);
    }
    return revert_matrix;
}

Element* Matrice_float::check_member(LispE* lisp, Element* the_set) {
    Matrice_float* r = new Matrice_float;
    r->size_x = size_x;
    r->size_y = size_y;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Matrice_float::transposed(LispE* lisp) {
    Matrice_float* transposed_matrix = new Matrice_float(lisp, size_y, size_x, 0.0);
    long i, j = 0;
    
    Element* e;
    for (i = 0; i < size_x; i++) {
        e = liste[i];
        for (j = 0; j < size_y; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Matrice_float::rank(LispE* lisp, vecte<long>& positions) {
   int16_t sz = positions.size();
   if (!sz || sz > 2)
      return new Error("Error: index mismatch");

   if (positions[0] == -1) {
      //We return all columns
      if (sz == 1 || positions[1] == -1) {
         Matrice_float* m = new Matrice_float();
         Floats* result;

         for (long j = 0; j < size_y; j++) {
            result = lisp->provideFloats();
            for (long i = 0; i < size_x; i++) {
               result->liste.push_back(val(i,j));
            }
            m->append(result);
         }
         m->size_x = size_y;
         m->size_y = size_x;
         return m;
      }
      else {
         if (positions[1] >= size_y)
            return new Error("Error: indexes out of bounds");
      }
   }
   else {
      if (sz == 2 && positions[1] != -1) {
         if (positions[1] >= size_y)
            return new Error("Error: indexes out of bounds");
         return lisp->provideFloat(val(positions[0], positions[1]));
      }

      if (positions[0] >= size_x)
         return new Error("Error: indexes out of bounds");

      return lisp->provideFloats(((Floats*)liste[positions[0]]));
   }
   Floats* result = lisp->provideFloats();
   for (long i = 0; i < size_x; i++) {
      result->liste.push_back(val(i,positions[1]));
   }
   return result;
}


Element* Matrice_float::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   lisp->recording(null_, label);
   long sz = code->liste.size();
   for (long i = 0; i < size_x; i++) {
      lisp->replacingvalue(liste[i], label);
      _releasing(e);
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Matrice_float::rotate(LispE* lisp, long axis) {
    Matrice_float* revert_matrix = new Matrice_float;
    revert_matrix->size_x = size_x;
    revert_matrix->size_y = size_y;
    
    long i;
    
    if (axis == 1) {
        for (i = 0; i < size_x; i++) {
            revert_matrix->append(liste[i]->rotate(lisp,0));
        }
        return revert_matrix;
    }
    
    Element* e;
    for (i = size_x-1; i>= 0;  i--) {
        e = lisp->provideFloats((Floats*)liste[i]);
        revert_matrix->append(e);
    }
    return revert_matrix;
}

//--------------------------------------------------------------------------------
//Tenseur methods
//--------------------------------------------------------------------------------
Element* Tenseur::check_member(LispE* lisp, Element* the_set) {
    Tenseur* r = new Tenseur;
    r->shape = shape;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Tenseur::transposed(LispE* lisp) {
    vecte<long> sz;
    sz = shape;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    
    Tenseur* transposed_matrix = new Tenseur(lisp, sz, zero_);
    long j = 0;
    
    Element* e;
    for (i = 0; i < shape[0]; i++) {
        e = liste[i];
        for (j = 0; j < shape[1]; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Tenseur::storeRank(LispE* lisp, Element* result, Element* current, vecte<long>& positions, long idx) {
    long axis = idx;
    //first we search for our first actual axis...
    while (axis < positions.size() && positions[axis] == -1) axis++;
    if (axis == idx) {
        //It is a direct value...
        if (idx == positions.size() - 1)
            return current->index(positions[idx])->duplicate_constant(lisp);
        
        return storeRank(lisp, result, current->index(positions[idx]), positions, idx+1);
    }
    
    //otherwise, this is an axis
    vecte<long> paths;
    if (axis < positions.size()) {
        long p_idx = positions[axis];
        paths.push_back(p_idx);
        buildList(lisp, result, current, shape, paths, idx, axis - 1);
        return result;
    }
    
    paths.push_back(0);
    Element* r;
    for (long i = 0; i < shape[axis]; i++) {
        paths.setlast(i);
        r = lisp->provideList();
        result->append(r);
        buildList(lisp, r, current, shape, paths, idx, axis - 1);
    }
    return result;
}

Element* Tenseur::rank(LispE* lisp, vecte<long>& positions) {
   //We get rid of the final negative values (useless)
   int16_t sz = positions.size();
   if (!sz || sz > shape.size())
      return new Error("Error: index mismatch");
   //Check positions
   for (long i = 0; i < sz; i++) {
      if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
         return new Error("Error: indexes out of bounds");
   }

   Element* result = lisp->provideList();
   Element* res = storeRank(lisp, result, this, positions, 0);
   if (result != res)
      result->release();

   if (res->type == t_numbers || res->type == t_number)
      return res;

   //We steal the ITEM structure of res
   //which is a very fast operation
   //Since its internal values are not copied but borrowed
   if (res->index(0)->type == t_floats) {
      Matrice* m = new Matrice((List*)res);
      res->release();
      return m;
   }
   Tenseur* ts = new Tenseur(lisp, (List*)res);
   res->release();
   return ts;
}


Element* Tenseur::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   lisp->recording(null_, label);
   long sz = code->liste.size();
   for (long i = 0; i < shape[0]; i++) {
      lisp->replacingvalue(liste[i], label);
      _releasing(e);
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Tenseur::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return lisp->provideNumbers((Numbers*)value);
    
    Element* r;
    if (init) {
        r = new Tenseur;
        ((Tenseur*)r)->shape = shape;
    }
    else
        r = lisp->provideList();
    
    Element* e;
    for (long i = 0; i < shape[pos]; i++) {
        e = reversion(lisp, value->index(i), pos+1, axis, false);
        r->append(e);
    }
    return r;
}

Element* Tenseur::rotate(LispE* lisp, long axis) {
    return reversion(lisp, this, 0, axis, true);
}

Element* Tenseur_float::check_member(LispE* lisp, Element* the_set) {
    Tenseur_float* r = new Tenseur_float;
    r->shape = shape;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Tenseur_float::transposed(LispE* lisp) {
    vecte<long> sz;
    sz = shape;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    
    Tenseur_float* transposed_matrix = new Tenseur_float(lisp, sz, zero_);
    long j = 0;
    
    Element* e;
    for (i = 0; i < shape[0]; i++) {
        e = liste[i];
        for (j = 0; j < shape[1]; j++) {
            transposed_matrix->index(j)->replacing(i, e->index(j));
        }
    }
    return transposed_matrix;
}

Element* Tenseur_float::storeRank(LispE* lisp, Element* result, Element* current, vecte<long>& positions, long idx) {
    long axis = idx;
    //first we search for our first actual axis...
    while (axis < positions.size() && positions[axis] == -1) axis++;
    if (axis == idx) {
        //It is a direct value...
        if (idx == positions.size() - 1)
            return current->index(positions[idx])->duplicate_constant(lisp);
        
        return storeRank(lisp, result, current->index(positions[idx]), positions, idx+1);
    }
    
    //otherwise, this is an axis
    vecte<long> paths;
    if (axis < positions.size()) {
        long p_idx = positions[axis];
        paths.push_back(p_idx);
        buildList(lisp, result, current, shape, paths, idx, axis - 1);
        return result;
    }
    
    paths.push_back(0);
    Element* r;
    for (long i = 0; i < shape[axis]; i++) {
        paths.setlast(i);
        r = lisp->provideList();
        result->append(r);
        buildList(lisp, r, current, shape, paths, idx, axis - 1);
    }
    return result;
}

Element* Tenseur_float::rank(LispE* lisp, vecte<long>& positions) {
   //We get rid of the final negative values (useless)
   int16_t sz = positions.size();
   if (!sz || sz > shape.size())
      return new Error("Error: index mismatch");
   //Check positions
   for (long i = 0; i < sz; i++) {
      if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
         return new Error("Error: indexes out of bounds");
   }
   Element* result = lisp->provideList();
   Element* res = storeRank(lisp, result, this, positions, 0);
   if (res != result)
      result->release();

   if (res->type == t_floats || res->type == t_float)
      return res;

   //We steal the ITEM structure of res
   //which is a very fast operation
   //Since its internal values are not copied but borrowed
   if (res->index(0)->type == t_floats) {
      Matrice_float* m = new Matrice_float((List*)res);
      res->release();
      return m;
   }
   Tenseur_float* ts = new Tenseur_float(lisp, (List*)res);
   res->release();
   return ts;
}


Element* Tenseur_float::loop(LispE* lisp, int16_t label, List* code) {
   long i_loop;
   Element* e = null_;
   lisp->recording(null_, label);
   long sz = code->liste.size();
   for (long i = 0; i < shape[0]; i++) {
      lisp->replacingvalue(liste[i], label);
      _releasing(e);
      //We then execute our instructions
      for (i_loop = 3; i_loop < sz && thrown_error == NULL && e->type != l_return; i_loop++) {
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


Element* Tenseur_float::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return lisp->provideNumbers((Numbers*)value);
    
    Element* r;
    if (init) {
        r = new Tenseur_float;
        ((Tenseur_float*)r)->shape = shape;
    }
    else
        r = lisp->provideList();
    
    Element* e;
    for (long i = 0; i < shape[pos]; i++) {
        e = reversion(lisp, value->index(i), pos+1, axis, false);
        r->append(e);
    }
    return r;
}

Element* Tenseur_float::rotate(LispE* lisp, long axis) {
    return reversion(lisp, this, 0, axis, true);
}

Element* LList::next_iter(LispE* lisp, void* it) {
    u_links* a = (u_links*)it;
    if (a->first == NULL)
        return emptyatom_;
    Element* e = a->first->value;
    a->first = a->first->next();
    return e;
}

Element* LList::next_iter_exchange(LispE* lisp, void* it) {
    u_links* a = (u_links*)it;
    if (a->first == NULL)
        return emptyatom_;
    Element* e = a->first->value;
    a->first = a->first->next();
    return e;
}

Element* List::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    Element* e = liste[n[0]];
    n[0]++;
    return e;
}

Element* List::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    Element* e = liste[n[0]];
    n[0]++;
    return e;
}

Element* Shorts::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    int16_t v = liste[n[0]];
    n[0]++;
    return new Short(v);
}

Element* Shorts::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    exchange_value.content = liste[n[0]];
    n[0]++;
    return &exchange_value;
}

Element* Integers::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    long v = liste[n[0]];
    n[0]++;
    return lisp->provideInteger(v);
}

Element* Integers::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    exchange_value.content = liste[n[0]];
    n[0]++;
    return &exchange_value;
}

Element* Floats::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    float v = liste[n[0]];
    n[0]++;
    return lisp->provideFloat(v);
}

Element* Floats::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    exchange_value.content = liste[n[0]];
    n[0]++;
    return &exchange_value;
}

Element* Numbers::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    double v = liste[n[0]];
    n[0]++;
    return lisp->provideNumber(v);
}

Element* Numbers::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    exchange_value.content = liste[n[0]];
    n[0]++;
    return &exchange_value;
}

Element* Strings::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    u_ustring v = liste[n[0]];
    n[0]++;
    return lisp->provideString(v);
}

Element* Strings::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == liste.size())
        return emptyatom_;
    exchange_value.content = liste[n[0]];
    n[0]++;
    return &exchange_value;
}

void Element::push_element(LispE* lisp, List* l) {
   {
      lisp->delegation->set_error(new Error("Error: cannot push in this kind of elements"));
      return;
   }
}


void Element::push_element_true(LispE* lisp, List* l) {
   {
      lisp->delegation->set_error(new Error("Error: cannot push in this kind of elements"));
      return;
   }
}


void Element::push_element_front(LispE* lisp, List* l) {
   {
      lisp->delegation->set_error(new Error("Error: cannot push in this kind of elements"));
      return;
   }
}


void Element::push_element_back(LispE* lisp, List* l) {
   {
      lisp->delegation->set_error(new Error("Error: cannot push in this kind of elements"));
      return;
   }
}


void List::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        append(value->copying(false));
        value->release();
    }
}

void List::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            append(value->copying(false));
            value->release();
        }
    }
}

void List::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        insert(lisp, value->copying(false), 0);
        value->release();
    }
}

void List::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        append(value->copying(false));
        value->release();
    }
}

void LList::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        push_front(value->copying(false));
        value->release();
    }
}

void LList::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            push_front(value->copying(false));
            value->release();
        }
    }
}

void LList::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        push_front(value->copying(false));
        value->release();
    }
}

void LList::push_element_back(LispE* lisp, List* l) {
   Element* value = l->liste[2]->eval(lisp);

   u_link* current = liste.last_raw();
   u_link* u = new u_link(value->copying(false));

   if (current == NULL) {
      liste.first = u;
      u->inc(1);
   }
   else {
      current->u_push(u);
      #ifdef LISPE_WASM
      if (current->error) {
         current->error = false;
         lisp->delegation->set_error(new Error("Error: Cannot add an element to this linked list"));
         return;
      }
      #endif
   }
   u->value->incrementstatus(u->status);

   for (long i = 3; i < l->size(); i++) {
      value = l->liste[i]->eval(lisp);
      current = u;
      u = new u_link(value->copying(false));
      current->u_push(u);
      #ifdef LISPE_WASM
      if (current->error) {
         current->error = false;
         lisp->delegation->set_error(new Error("Error: Cannot add an element to this linked list"));
         return;
      }
      #endif
      u->value->incrementstatus(u->status);
   }

   if (thrown_error) {
      u->release();
      {
         lisp->delegation->set_error(thrown_error);
         return;
      }
   }
}


void Integers::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asInteger());
        value->release();
    }
}

void Integers::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            liste.push_back(value->asInteger());
            value->release();
        }
    }
}

void Integers::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.insert(0, value->asInteger());
        value->release();
    }
}

void Integers::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asInteger());
        value->release();
    }
}

void Numbers::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asNumber());
        value->release();
    }
}

void Numbers::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            liste.push_back(value->asNumber());
            value->release();
        }
    }
}

void Numbers::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.insert(0, value->asNumber());
        value->release();
    }
}

void Numbers::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asNumber());
        value->release();
    }
}

void Floats::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asFloat());
        value->release();
    }
}

void Floats::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            liste.push_back(value->asFloat());
            value->release();
        }
    }
}

void Floats::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.insert(0, value->asFloat());
        value->release();
    }
}

void Floats::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asFloat());
        value->release();
    }
}

void Shorts::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asShort());
        value->release();
    }
}

void Shorts::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            liste.push_back(value->asShort());
            value->release();
        }
    }
}

void Shorts::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.insert(0, value->asShort());
        value->release();
    }
}

void Shorts::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asShort());
        value->release();
    }
}

void Strings::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asUString(lisp));
        value->release();
    }
}

void Strings::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            liste.push_back(value->asUString(lisp));
            value->release();
        }
    }
}

void Strings::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.insert(0, value->asUString(lisp));
        value->release();
    }
}

void Strings::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        liste.push_back(value->asUString(lisp));
        value->release();
    }
}

//------------------------------------------------------------------------------------------------------------
Element* List::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        append(e);
        return this;
    }
            
    Element* test = NULL;
    if (end < 3) {
        comparison.in_quote(2, liste[0]);
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                comparison.in_quote(2, liste[1]);
                test = comparison.eval(lisp);
                if (test->Boolean()) {
                    insertion(e , 1);
                    return this;
                }
            }
            append(e);
        }
        return this;
    }
    
    end--;
    long begin = 0;
    long i = 0;
    
    //then we compare by dichotomy
    while ((end-begin) > 1) {
        i = begin + ((end - begin) >> 1);
        comparison.in_quote(2, liste[i]);
        test = comparison.eval(lisp);
        if (test->Boolean())
            end = i;
        else
            begin = i;
    }
    if (test->Boolean()) {
        if (i == begin)
            insertion(e, i);
        else {
            comparison.in_quote(2, liste[begin]);
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, begin);
            else
                insertion(e, end);
        }
    }
    else {
        if (i == end)
            insertion(e, i + 1);
        else {
            comparison.in_quote(2, liste[end]);
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
}

Element* Numbers::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        append(e);
        return this;
    }
        
    Element* test = NULL;
    if (end < 3) {
        comparison.in_quote(2, index(0));
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                comparison.in_quote(2, index(1));
                test = comparison.eval(lisp);
                if (test->Boolean()) {
                    insertion(e , 1);
                    return this;
                }
            }
            append(e);
        }
        return this;
    }
    
    end--;
    long begin = 0;
    long i = 0;
    
    //then we compare by dichotomy
    while ((end-begin) > 1) {
        i = begin + ((end - begin) >> 1);
        comparison.in_quote(2, index(i));
        test = comparison.eval(lisp);
        if (test->Boolean())
            end = i;
        else
            begin = i;
    }
    if (test->Boolean()) {
        if (i == begin)
            insertion(e, i);
        else {
            comparison.in_quote(2, index(begin));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, begin);
            else
                insertion(e, end);
        }
    }
    else {
        if (i == end)
            insertion(e, i + 1);
        else {
            comparison.in_quote(2, index(end));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
}

Element* Floats::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        append(e);
        return this;
    }
        
    Element* test = NULL;
    if (end < 3) {
        comparison.in_quote(2, index(0));
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                comparison.in_quote(2, index(1));
                test = comparison.eval(lisp);
                if (test->Boolean()) {
                    insertion(e , 1);
                    return this;
                }
            }
            append(e);
        }
        return this;
    }
    
    end--;
    long begin = 0;
    long i = 0;
    
    //then we compare by dichotomy
    while ((end-begin) > 1) {
        i = begin + ((end - begin) >> 1);
        comparison.in_quote(2, index(i));
        test = comparison.eval(lisp);
        if (test->Boolean())
            end = i;
        else
            begin = i;
    }
    if (test->Boolean()) {
        if (i == begin)
            insertion(e, i);
        else {
            comparison.in_quote(2, index(begin));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, begin);
            else
                insertion(e, end);
        }
    }
    else {
        if (i == end)
            insertion(e, i + 1);
        else {
            comparison.in_quote(2, index(end));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
}

Element* Integers::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        append(e);
        return this;
    }
        
    
    Element* test = NULL;
    if (end < 3) {
        comparison.in_quote(2, index(0));
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                comparison.in_quote(2, index(1));
                test = comparison.eval(lisp);
                if (test->Boolean()) {
                    insertion(e , 1);
                    return this;
                }
            }
            append(e);
        }
        return this;
    }
    
    end--;
    long begin = 0;
    long i = 0;
    
    //then we compare by dichotomy
    while ((end-begin) > 1) {
        i = begin + ((end - begin) >> 1);
        comparison.in_quote(2, index(i));
        test = comparison.eval(lisp);
        if (test->Boolean())
            end = i;
        else
            begin = i;
    }
    if (test->Boolean()) {
        if (i == begin)
            insertion(e, i);
        else {
            comparison.in_quote(2, index(begin));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, begin);
            else
                insertion(e, end);
        }
    }
    else {
        if (i == end)
            insertion(e, i + 1);
        else {
            comparison.in_quote(2, index(end));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
}

Element* Shorts::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        append(e);
        return this;
    }
        
    Element* test = NULL;
    if (end < 3) {
        comparison.in_quote(2, index(0));
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                comparison.in_quote(2, index(1));
                test = comparison.eval(lisp);
                if (test->Boolean()) {
                    insertion(e , 1);
                    return this;
                }
            }
            append(e);
        }
        return this;
    }
    
    end--;
    long begin = 0;
    long i = 0;
    
    //then we compare by dichotomy
    while ((end-begin) > 1) {
        i = begin + ((end - begin) >> 1);
        comparison.in_quote(2, index(i));
        test = comparison.eval(lisp);
        if (test->Boolean())
            end = i;
        else
            begin = i;
    }
    if (test->Boolean()) {
        if (i == begin)
            insertion(e, i);
        else {
            comparison.in_quote(2, index(begin));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, begin);
            else
                insertion(e, end);
        }
    }
    else {
        if (i == end)
            insertion(e, i + 1);
        else {
            comparison.in_quote(2, index(end));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
}

Element* Strings::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        append(e);
        return this;
    }
        
    Element* test = NULL;
    if (end < 3) {
        comparison.in_quote(2, index(0));
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                comparison.in_quote(2, index(1));
                test = comparison.eval(lisp);
                if (test->Boolean()) {
                    insertion(e , 1);
                    return this;
                }
            }
            append(e);
        }
        return this;
    }
    
    end--;
    long begin = 0;
    long i = 0;
    
    //then we compare by dichotomy
    while ((end-begin) > 1) {
        i = begin + ((end - begin) >> 1);
        comparison.in_quote(2, index(i));
        test = comparison.eval(lisp);
        if (test->Boolean())
            end = i;
        else
            begin = i;
    }
    if (test->Boolean()) {
        if (i == begin)
            insertion(e, i);
        else {
            comparison.in_quote(2, index(begin));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, begin);
            else
                insertion(e, end);
        }
    }
    else {
        if (i == end)
            insertion(e, i + 1);
        else {
            comparison.in_quote(2, index(end));
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
}

Element* LList::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    Element* test;
    u_link* last = NULL;
    u_link* u;
    
    for (u = liste.begin(); u != NULL; u = u->next()) {
        last = u;
        comparison.in_quote(2, u->value);
        test = comparison.eval(lisp);
        if (test->Boolean()) {
            u_link* l = new u_link(e);
            u->insert(l);
            return this;
        }
    }
        
    u = new u_link(e);
    if (last == NULL) {
        liste.first = u;
        u->inc(1);
    }
    else {
        last->u_push(u);
#ifdef LISPE_WASM
            if (last->error) {
                last->error = false;
                return lisp->delegation->set_error(new Error("Error: Cannot add an element to this linked list"));
            }
#endif
    }
        
    return this;
}