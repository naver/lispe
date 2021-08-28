/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  elements.cxx
//
//

#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>

//------------------------------------------------------------------------------------------
Element* List::quoted(LispE* lisp) {
    List* l = lisp->provideList();
    l->liste.push_raw(lisp->delegation->_QUOTE);
    l->append(this);
    return l;
}

Element* Element::quoting(Element* quote) {
    List* l = (List*)newInstance();
    l->append(quote);
    l->append(this);
    return l;
}
//------------------------------------------------------------------------------------------
union double64 {
public:
    
    uint64_t bits;
    double v;
    
    double64(double d) {
        v = d;
    }
};

//------------------------------------------------------------------------------------------
void Listpool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect) {
        status -= nb;
    }
    
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_back(this);
    }
}

void Listpool::release() {
    if (!status) {
        liste.decrement();
        liste.clear();
        lisp->list_pool.push_back(this);
    }
}

void Numberpool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect)
        status -= nb;
    if (!status)
        lisp->number_pool.push_back(this);
}

void Numberpool::release() {
    if (status == s_destructible)
        lisp->number_pool.push_back(this);
}

void Integerpool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect)
        status -= nb;
    if (!status)
        lisp->integer_pool.push_back(this);
}

void Integerpool::release() {
    if (status == s_destructible)
        lisp->integer_pool.push_back(this);
}

void Stringpool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect)
        status -= nb;
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

void Stringpool::release() {
    if (status == s_destructible) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

void Numberspool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect)
        status -= nb;
    if (!status) {
        liste.clear();
        lisp->numbers_pool.push_back(this);
    }
}

void Numberspool::release() {
    if (status == s_destructible) {
        liste.clear();
        lisp->numbers_pool.push_back(this);
    }
}

void Integerspool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect)
        status -= nb;
    if (!status) {
        lisp->integers_pool.push_back(this);
        liste.clear();
    }
}

void Integerspool::release() {
    if (status == s_destructible) {
        lisp->integers_pool.push_back(this);
        liste.clear();
    }
}

void Stringspool::decrementstatus(uchar nb, bool top) {
    if (status > s_destructible && status < s_protect)
        status -= nb;
    if (!status) {
        lisp->strings_pool.push_back(this);
        liste.clear();
    }
}

void Stringspool::release() {
    if (status == s_destructible) {
        lisp->strings_pool.push_back(this);
        liste.clear();
    }
}

Element* Numberspool::newInstance() {
    return lisp->provideNumbers();
}

Element* Integerspool::newInstance() {
    return lisp->provideIntegers();
}

Element* Stringspool::newInstance() {
    return lisp->provideStrings();
}

Element* Listpool::newInstance() {
    return lisp->provideList();
}

Element* Numberpool::fullcopy() {
    if (lisp->preparingthread)
        return new Number(number);
    return lisp->provideNumber(number);
}

Element* Numberpool::copyatom(uchar s) {
    if (status < s)
        return this;
    return lisp->provideNumber(number);
}

Element* Numberpool::copying(bool duplicate) {
    if (status < s_protect || !duplicate)
        return this;
    if (lisp->preparingthread)
        return new Number(number);
    return lisp->provideNumber(number);
}

Element* Integerpool::fullcopy() {
    if (lisp->preparingthread)
        return new Integer(integer);
    return lisp->provideInteger(integer);
}

Element* Integerpool::copyatom(uchar s) {
    if (status < s)
        return this;
    return lisp->provideInteger(integer);
}

Element* Integerpool::copying(bool duplicate) {
    if (status < s_protect || !duplicate)
        return this;
    
    if (lisp->preparingthread)
        return new Integer(integer);

    return lisp->provideInteger(integer);
}

Element* Stringpool::fullcopy() {
    if (lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Stringpool::copyatom(uchar s) {
    if (status < s)
        return this;
    return lisp->provideString(content);
}

Element* Stringpool::copying(bool duplicate) {
    if (status < s_protect || !duplicate)
        return this;
    
    if (lisp->preparingthread)
        return new String(content);

    return lisp->provideString(content);
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

Element* Listpool::copyatom(uchar s) {
    if (status < s)
        return this;

    List* l = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copyatom(s));
    }
    return l;
}

Element* Listpool::copying(bool duplicate) {
    //If it is a CDR, we need to copy it...
    if (status < s_protect && liste.nocdr() && !duplicate)
        return this;
    
    List* l;
    if (lisp->preparingthread)
        l = new List;
    else
        l = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copying(false));
    }
    return l;
}

Element* Numberspool::fullcopy() {
    if (lisp->preparingthread)
        return new Numbers(this);

    Numbers* e = lisp->provideNumbers();
    e->liste = liste;
    return e;
}

Element* Numberspool::copying(bool duplicate) {
    //If it is a CDR, we need to copy it...
    if (status < s_protect && !duplicate)
        return this;

    if (lisp->preparingthread)
        return new Numbers(this);
    
    Numbers* e = lisp->provideNumbers();
    e->liste = liste;
    return e;
}

Element* Numberspool::copyatom(uchar s) {
    if (status < s)
        return this;

    Numbers* e = lisp->provideNumbers();
    e->liste = liste;
    return e;
}

Element* Integerspool::fullcopy() {
    if (lisp->preparingthread)
        return new Integers(this);

    Integers* e = lisp->provideIntegers();
    e->liste = liste;
    return e;
}

Element* Integerspool::copying(bool duplicate) {
    //If it is a CDR, we need to copy it...
    if (status < s_protect && !duplicate)
        return this;

    if (lisp->preparingthread)
        return new Integers(this);

    Integers* e = lisp->provideIntegers();
    e->liste = liste;
    return e;
}

Element* Integerspool::copyatom(uchar s) {
    if (status < s)
        return this;

    Integers* e = lisp->provideIntegers();
    e->liste = liste;
    return e;
}

Element* Stringspool::fullcopy() {
    if (lisp->preparingthread)
        return new Strings(this);

    Strings* e = lisp->provideStrings();
    e->liste = liste;
    return e;
}

Element* Stringspool::copying(bool duplicate) {
    //If it is a CDR, we need to copy it...
    if (status < s_protect && !duplicate)
        return this;

    if (lisp->preparingthread)
        return new Strings(this);

    Strings* e = lisp->provideStrings();
    e->liste = liste;
    return e;
}

Element* Stringspool::copyatom(uchar s) {
    if (status < s)
        return this;

    Strings* e = lisp->provideStrings();
    e->liste = liste;
    return e;
}

//------------------------------------------------------------------------------------------
Infinitelist::Infinitelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

Cyclelist::Cyclelist(LispE* lisp) : Element(t_list) {
    value = null_;
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

//------------------------------------------------------------------------------------------
inline bool LIST::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    comparison->liste[1] = item->buffer[i];
    comparison->liste[2] = item->buffer[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void LIST::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
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
    comparison->liste[2] = item->buffer[rmax];
    for (j = rmin; j < rmax; j++) {
        comparison->liste[1] = item->buffer[j];
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


bool Numbers::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((Number*)comparison->liste[1])->number = liste[i];
    ((Number*)comparison->liste[2])->number = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Numbers::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
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
    ((Number*)comparison->liste[2])->number = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((Number*)comparison->liste[1])->number = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
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
    
    n1.number = liste[0];
    n2.number = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");

    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}

bool Integers::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((Integer*)comparison->liste[1])->integer = liste[i];
    ((Integer*)comparison->liste[2])->integer = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Integers::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
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
    ((Integer*)comparison->liste[2])->integer = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((Integer*)comparison->liste[1])->integer = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
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
    n1.integer = liste[0];
    n2.integer = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");

    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}

bool Strings::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    ((String*)comparison->liste[1])->content = liste[i];
    ((String*)comparison->liste[2])->content = liste[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void Strings::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
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
    ((String*)comparison->liste[2])->content = liste[rmax];
    for (j = rmin; j < rmax; j++) {
        ((String*)comparison->liste[1])->content = liste[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            swap(pivot,j);
        }
    }
    pivot++;
    swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void Strings::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = size();
    if (sz <= 1)
        return;
    
    Conststring n1(L"");
    Conststring n2(L"");
    comparison->liste[1] = &n1;
    comparison->liste[2] = &n2;
    n1.content = liste[0];
    n2.content = liste[0];
    if (comparison->eval(lisp)->Boolean())
        throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");

    sorting(lisp, comparison, comparison->liste[0]->type, 0, sz-1);
}
//------------------------------------------------------------------------------------------
Element* Element::minimum(LispE* lisp) {
    throw new Error("Error: cannot find the minimum for this object");
}

Element* List::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v = index(0);
    for (long i = 1; i < liste.size(); i++) {
        if (v->more(lisp, liste[i]) == true_)
            v = liste[i];
    }
    return v->copying(false);
}

Element* Numbers::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    double v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideNumber(v);
}

Element* Integers::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    long v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideInteger(v);
}

Element* Strings::minimum(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v > liste[i])
            v = liste[i];
    }
    return lisp->provideString(v);
}

Element* Set::minimum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    u_ustring w;
    bool first = true;
    for (auto& a : ensemble) {
        if (first)
            w = a;
        else
            if (w < a)
                w = a;
    }
    
    return lisp->provideString(w);
}

Element* Set_n::minimum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    double w = 0;
    bool first = true;
    for (auto& a : ensemble) {
        if (first)
            w = a;
        else
            if (w < a)
                w = a;
    }
    
    return lisp->provideNumber(w);
}


Element* Dictionary::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_n::minimum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->more(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}
//------------------------------------------------------------------------------------------
Element* Element::maximum(LispE* lisp) {
    throw new Error("Error: cannot find the maximum for this object");
}

Element* List::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    Element* v = index(0);
    for (long i = 1; i < liste.size(); i++) {
        if (v->less(lisp, liste[i]) == true_)
            v = liste[i];
    }
    return v->copying(false);
}

Element* Numbers::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    double v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideNumber(v);
}

Element* Integers::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    long v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideInteger(v);
}

Element* Strings::maximum(LispE* lisp) {
    if (!liste.size())
        return null_;
    u_ustring v = liste[0];
    for (long i = 1; i < liste.size(); i++) {
        if (v < liste[i])
            v = liste[i];
    }
    return lisp->provideString(v);
}

Element* Set::maximum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    u_ustring w;
    bool first = true;
    for (auto& a : ensemble) {
        if (first)
            w = a;
        else
            if (w > a)
                w = a;
    }
    
    return lisp->provideString(w);
}

Element* Set_n::maximum(LispE* lisp) {
    if (ensemble.empty())
        return null_;
    double w = 0;
    bool first = true;
    for (auto& a : ensemble) {
        if (first)
            w = a;
        else
            if (w > a)
                w = a;
    }
    
    return lisp->provideNumber(w);
}
Element* Dictionary::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}

Element* Dictionary_n::maximum(LispE* lisp) {
    if (!dictionary.size())
        return null_;
    Element* e = NULL;
    
    for (auto& a : dictionary) {
        if (e == NULL)
            e = a.second;
        else {
            if (e->less(lisp, a.second))
                e = a.second;
        }
    }
    
    return e->copying(false);
}
//------------------------------------------------------------------------------------------
void Element::flatten(LispE* lisp, List* l) {
    l->append(this);
}

void Element::flatten(LispE* lisp, Numbers* l) {
    l->append(this);
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

void Set::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    u_ustring k;
    for (auto& a : ensemble) {
        k = a;
        l->append(lisp->provideString(k));
    }
}

void Set_n::flatten(LispE* lisp, List* l) {
    if (ensemble.empty())
        return;
    for (auto& a : ensemble) {
        l->append(lisp->provideNumber(a));
    }
}


void Dictionary::flatten(LispE* lisp, List* l) {
    u_ustring k;
    for (auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideString(k));
        a.second->flatten(lisp, l);
    }
}

void Dictionary_n::flatten(LispE* lisp, List* l) {
    double k;
    for (auto& a: dictionary) {
        k = a.first;
        l->append(lisp->provideNumber(k));
        a.second->flatten(lisp, l);
    }
}

//------------------------------------------------------------------------------------------
Element* List::transposed(LispE* lisp) {
    vector<long> sz;
    getShape(sz);
    if (sz.size() <= 1)
        return this;
    long i = sz[0];
    sz[0] = sz[1];
    sz[1] = i;
    Element* tenseur;
    if (sz.size() == 2)
        tenseur = new Matrice(sz[0], sz[1], 0.0);
    else
        tenseur = new Tenseur(sz, zero_);
    
    Element* e;
    for (i = 0; i < sz[1]; i++) {
        e = liste[i];
        for (long j = 0; j < sz[0]; j++) {
            tenseur->index(j)->replacing(i, e->index(j)->copying(false));
        }
    }

    return tenseur;
}

Element* Matrice::transposed(LispE* lisp) {
    Matrice* transposed_matrix = new Matrice(size_y, size_x, 0.0);
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

Element* Tenseur::transposed(LispE* lisp) {
    vector<long> sz = shape;
    long i = sz[0];
    sz[0] = sz[1];
    sz[1] = i;

    Tenseur* transposed_matrix = new Tenseur(sz, zero_);
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

//------------------------------------------------------------------------------------------

void List::storevalue(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void List::storevalue(LispE* lisp,long v) {
    append(lisp->provideInteger(v));
}

void List::storevalue(LispE* lisp, u_ustring& v) {
    append(lisp->provideString(v));
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

void Strings::storevalue(LispE* lisp, double v) {
    liste.push_back(convertToUString(v));
}

void Strings::storevalue(LispE* lisp,long v) {
    liste.push_back(convertToUString(v));
}

void Strings::storevalue(LispE* lisp, u_ustring& s) {
    liste.push_back(s);
}

//------------------------------------------------------------------------------------------


void Element::prettyfying(LispE* lisp, string& code) {
    if (isList()) {
        if (size() == 0) {
            code += " ()";
            return;
        }
                
        short type = index(0)->type;
        if (type == l_lambda) {
            code += " ";
            code += toString(lisp);
            return;
        }
        
        Element* params;
        
        if (type == l_defun || type == l_defpat || type == l_deflib || type == l_loop) {
            code += "(";
            code += lisp->toString(type);
            code += " ";
            code += protected_index(lisp, (long)1)->toString(lisp);
            code += " ";
            params = protected_index(lisp, (long)2);
            if (type == l_defpat) {
                code += "(";
                string local;
                for (long i = 0; i < params->size(); i++) {
                    if (i)
                        code += " ";
                    local = params->protected_index(lisp, i)->toString(lisp);
                    if (local[0] == '(') {
                        local[0] = '[';
                        local.back() = ']';
                    }
                    code += local;
                }
                code += ")";
            }
            else
                code += params->toString(lisp);
            code += "\n";
            for (long i = 3; i < size(); i++) {
                protected_index(lisp, i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }

        long i = 0;


        if (type == l_if || type == l_check || type == l_ncheck || type == l_ife) {
            code += "(";
            code += protected_index(lisp, i++)->toString(lisp);
            code += " ";
            code += protected_index(lisp, i++)->toString(lisp);
            code += "\n";
            for (; i < size(); i++) {
                protected_index(lisp, i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }

        string local = toString(lisp);
        if (local.size() < 50) {
            code += local;
            code += "\n";
            return;
        }
        
        code += "(";

        if (type == l_while || type == l_setq || type == l_setg || type == l_loopcount || type == l_key || type == l_keyn) {
            code += protected_index(lisp, i++)->toString(lisp);
            code += " ";
            code += protected_index(lisp, i++)->toString(lisp);
            code += "\n";
        }
        else {
            if (type > t_error && type < l_final) {
                code += protected_index(lisp, i++)->toString(lisp);
                i = 1;
            }
            code += "\n";
        }
                
        for (; i < size(); i++) {
            params = protected_index(lisp, i);
            params->prettyfying(lisp, code);
            if (code.back() != '\n')
                code += "\n";
        }
        code += ")\n";
        return;
    }
    if (isString())
        code += jsonstring(toString(lisp));
    else {
        if (isDictionary()) {
            string local = toString(lisp);
            if (local.size() < 50) {
                code += local;
                return;
            }
            code += "{\n";
            if (type == t_dictionary) {
                map<u_ustring, Element*>& dico = ((Dictionary*)this)->dictionary;
                u_ustring key;
                for (auto& a: dico) {
                    local = "";
                    key = a.first;
                    s_unicode_to_utf8(local, key);
                    code += local;
                    code += ":";
                    a.second->prettyfying(lisp, code);
                    if (code.back() != '\n')
                        code += "\n";
                }
                code += "}\n";
                return;
            }
            unordered_map<double, Element*>& dico = ((Dictionary_n*)this)->dictionary;
            for (auto& a: dico) {
                local = convertToString(a.first);
                code += local;
                code += ":";
                a.second->prettyfying(lisp, code);
                if (code.back() != '\n')
                    code += "\n";
            }
            code += "}\n";
            return;
        }
        else
            code += toString(lisp);
    }
}

//(defpat action ( [Take 'x] [Take y] )(if (check_object position x) (block (push belongings x) (println "Ok we have picked up" x)) (println "Cannot pick up the" x)))
//(prettify '((12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10)))

string Element::prettify(LispE* lisp) {
    string code;
    prettyfying(lisp, code);
    string body;
    IndentCode(code, body, GetBlankSize(), true, false);
    return body;
}

//------------------------------------------------------------------------------------------
Element* Element::inversion(LispE* lisp) {
    throw new Error("Error: Cannot invert this object");
}

Element* Number::inversion(LispE* lisp) {
    return lisp->provideNumber(number * -1);
}

Element* Integer::inversion(LispE* lisp) {
    return lisp->provideNumber(integer * -1);
}

Element* Numbers::inversion(LispE* lisp) {
    Numbers* n = lisp->provideNumbers();
    n->liste = liste;
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

Element* Integers::inversion(LispE* lisp) {
    Integers* n =  lisp->provideIntegers();
    n->liste = liste;
    for (long i = 0; i < n->liste.size(); i++)
        n->liste[i] *= -1;
    return n;
}

//------------------------------------------------------------------------------------------
//This method returns the needed instructions to build the dictionary
Element* Dictionary_as_list::dictionary(LispE* lisp) {
    Element* keycmd;
    List* last_element = lisp->provideList();

    if (!choice || keyvalues.size() != valuevalues.size()) {
        if (valuevalues.size() == 0) {
            //We create a set...
            if (type == t_number || type == t_integer)
                keycmd = lisp->delegation->_DICO_SETN;
            else
                keycmd = lisp->delegation->_DICO_SET;
            //We generate: (set k v k' v' k" v"...)
            last_element->append(keycmd);
            for (long i = 0; i < keyvalues.size(); i++)
                last_element->append(keyvalues[i]);
        }
        else {
            last_element->release();
            throw new Error("Error: dictionary has a different number of key/value");
        }
    }
    else {
        if (type == t_number || type == t_integer)
            keycmd = lisp->delegation->_DICO_KEYN;
        else
            keycmd = lisp->delegation->_DICO_KEY;
        
        //We generate: (key (key) k v k' v' k" v"...)
        last_element->append(keycmd);
        last_element->append(new List);
        last_element->liste[1]->append(keycmd);
        for (long i = 0; i < keyvalues.size(); i++) {
            last_element->append(keyvalues[i]);
            last_element->append(valuevalues[i]);
        }
    }
    
    try {
        keycmd = last_element->eval(lisp);
        last_element->release();
        return keycmd;
    }
    catch(Error* err) {
        err->release();
        return last_element;
    }
}

//------------------------------------------------------------------------------------------
void List::append(LispE* lisp, u_ustring& k) {
    liste.push_back(lisp->provideString(k));
}

void Numbers::append(LispE* lisp, u_ustring& k) {
    long l;
    double d = convertingfloathexa((u_uchar*)k.c_str(), l);
    liste.push_back(d);
}

void Integers::append(LispE* lisp, u_ustring& k) {
    long d = convertinginteger(k);
    liste.push_back(d);
}

void Strings::append(LispE* lisp, u_ustring& k) {
    liste.push_back(k);
}

void List::append(LispE* lisp, double v) {
    liste.push_back(lisp->provideNumber(v));
}

void Numbers::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Integers::append(LispE* lisp, double v) {
    liste.push_back(v);
}

void Strings::append(LispE* lisp, double v) {
    liste.push_back(convertToUString(v));
}

void Set::append(LispE* lisp, double v) {
    ensemble.insert(convertToUString(v));
}

void List::append(LispE* lisp, long v) {
    liste.push_back(lisp->provideInteger(v));
}

void Numbers::append(LispE* lisp, long v) {
    liste.push_back(v);
}

void Integers::append(LispE* lisp, long v) {
    liste.push_back(v);
}

void Strings::append(LispE* lisp, long v) {
    liste.push_back(convertToUString(v));
}

void Set::append(LispE* lisp, long v) {
    ensemble.insert(convertToUString(v));
}

void Dictionary_as_list::append(LispE* lisp, u_ustring& k) {
    append(lisp->provideString(k));
}

void Dictionary_as_list::append(LispE* lisp, double v) {
    append(lisp->provideNumber(v));
}

void Dictionary_as_list::append(LispE* lisp, long v) {
    append(lisp->provideInteger(v));
}

void Dictionary_as_buffer::append(LispE* lisp, u_ustring& k) {
    if (choice)
        key = k;
    else {
        dico->dictionary[key] = lisp->provideString(k);
        reversechoice();
    }
}

void Dictionary_as_buffer::append(LispE* lisp, double v) {
    if (choice)
        key = convertToUString(v);
    else {
        dico->dictionary[key] = lisp->provideNumber(v);
        reversechoice();
    }
}

void Dictionary_as_buffer::append(LispE* lisp, long v) {
    if (choice)
        key = convertToUString(v);
    else {
        dico->dictionary[key] = lisp->provideInteger(v);
        reversechoice();
    }
}


//------------------------------------------------------------------------------------------
Element* Matrice::rank(LispE* lisp, vector<long>& positions) {
    while (positions.back() < 0)
        positions.pop_back();
    
    short sz = positions.size();
    if (!sz || sz > 2)
        throw new Error("Error: index mismatch");
    
    if (positions[0] != -1) {
        if (sz == 2) {
            if (positions[1] >= size_y)
                throw new Error("Error: indexes out of bounds");
            return lisp->provideNumber(val(positions[0], positions[1]));
        }
        
        if (positions[0] < 0 || positions[0] >= size_x)
            throw new Error("Error: indexes out of bounds");
        
        return new Numbers(((Numbers*)liste[positions[0]]));
    }
    
    if (sz == 1 || positions[1] >= size_y)
        throw new Error("Error: indexes out of bounds");
    
    Numbers* result = lisp->provideNumbers();
    for (long i = 0; i < size_x; i++) {
        result->liste.push_back(val(i,positions[1]));
    }
    return result;
}

Element* Tenseur::rank(LispE* lisp, vector<long>& positions) {
    //We get rid of the final negative values (useless)
    while (positions.back() < 0)
        positions.pop_back();

    short sz = positions.size();
    if (!sz || sz > shape.size())
        throw new Error("Error: index mismatch");
    
    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            throw new Error("Error: indexes out of bounds");
    }
    
    Element* res = storeRank(this, positions, 0);
    if (res->type == t_numbers)
        return res;
    
    if (res->type == t_number)
        return lisp->provideNumber(res->asNumber());
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_numbers) {
        Matrice* m = new Matrice((List*)res);
        res->release();
        return m;
    }
    Tenseur* ts = new Tenseur((List*)res);
    res->release();
    return ts;
}
//------------------------------------------------------------------------------------------

Element* Element::loop(LispE* lisp, short label,  List* code) {
    return null_;
}

Element* String::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    long i = 0;
    u_ustring localvalue;
    long szc = content.size();
    while (i < szc) {
        lisp->handlingutf8->getchar(content, localvalue, i, szc);
        element = lisp->provideString(localvalue);
        lisp->recordingvalue(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            //the break is local, the return is global to a function
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* List::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = liste[i]->copying(false);
        lisp->recordingvalue(element, label);
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

Element* Matrice::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < size_x; i++) {
        for (long j = 0; j < size_y; j++) {
            element = lisp->provideNumber(liste[i]->index(j)->asNumber());
            lisp->recordingvalue(element, label);
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
    }
    return e;
}

Element* Tenseur::loop(LispE* lisp, short label, List* code) {
    Numbers* n = lisp->provideNumbers();
    flatten(lisp, n);
    Element* e = n->loop(lisp, label, code);
    delete n;
    return e;
}

Element* Numbers::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideNumber(liste[i]);
        lisp->recordingvalue(element, label);
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

Element* Integers::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideInteger(liste[i]);
        lisp->recordingvalue(element, label);
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

Element* Set::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    u_ustring w;
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        w = a;
        element = lisp->provideString(w);
        lisp->recordingvalue(element, label);
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

Element* Set_n::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (auto & a: ensemble) {
        element = lisp->provideNumber(a);
        lisp->recordingvalue(element, label);
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

Element* Strings::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = lisp->provideString(liste[i]);
        lisp->recordingvalue(element, label);
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

Element* InfiniterangeNumber::loop(LispE* lisp, short label, List* code) {
    if (!increment)
        throw new Error("Error: increment cannot be 0");
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    long sz = code->liste.size();
    double value = initial_value;
    Element* element;
    char check = 0;
    if (!infinite_loop) {
        if (increment < 0)
            check = 1;
        else
            check = 2;
    }

    while (!lisp->hasStopped()) {        
        if (check == 1 && value <= bound)
            break;
        if (check == 2 && value >= bound)
            break;

        element = lisp->provideNumber(value);
        lisp->recordingvalue(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        value += increment;
    }
    return e;
}

Element* InfiniterangeInteger::loop(LispE* lisp, short label, List* code) {
    if (!increment)
        throw new Error("Error: increment cannot be 0");

    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    long sz = code->liste.size();
    long value = initial_value;
    Element* element;
    char check = 0;
    if (!infinite_loop) {
        if (increment < 0)
            check = 1;
        else
            check = 2;
    }

    while (!lisp->hasStopped()) {
        if (check == 1 && value <= bound)
            break;
        if (check == 2 && value >= bound)
            break;

        element = lisp->provideInteger(value);
        lisp->recordingvalue(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        value += increment;
    }
    return e;
}


Element* Infinitelist::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    lisp->recordingvalue(element, label);
    
    while (!lisp->hasStopped()) {
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
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

Element* Cyclelist::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    if (!element->isList()) {
        element->release();
        throw new Error("Error: we can only cycle on list");
    }
    List* values = (List*)element;
    long sze = values->liste.size();
    long i = 0;
    //We then execute our instructions
    while (!lisp->hasStopped()) {
        element = values->liste[i]->copying(false);
        lisp->recordingvalue(element, label);
        if ((++i) >= sze)
            i = 0;
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



Element* Dictionary::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    vector<u_ustring> _keys;
    for (auto& a: dictionary)
        _keys.push_back(a.first);
    for (auto& a_key : _keys) {
        element = lisp->provideString(a_key);
        lisp->recordingvalue(element, label);
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

Element* Dictionary_n::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recordingvalue(null_, label);
    Element* element;
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    vector<double> _keys;
    for (auto& a: dictionary)
        _keys.push_back(a.first);
    for (auto& a_key : _keys) {
        element = lisp->provideNumber(a_key);
        lisp->recordingvalue(element, label);
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

/*
 In this case variables are in a list...
 (loop (x y z) lx ly lz ...)
 
 (loop [x y z] '(1 3 4) '(5 6 7) '(9 10 13) (println (+ x y z)))
*/
Element* List::multiloop(LispE* lisp) {
    List* values = lisp->provideList();
    List* indexes = lisp->provideList();
    Element* e = null_;
    Element* idx;
    long sz = size();
    long var;
    long indexe = 0, i_loop;
    //this is where the actual code to be executed starts
    long b_loop = 2 + liste[1]->size();
    
    short label;
    bool looping = true;
    
    try {
        for (var = 0; var < liste[1]->size(); var++) {
            label = liste[1]->index(var)->label();
            if (label == v_null)
                throw new Error("Missing variable in 'loop'");
            lisp->recordingvalue(null_, label);
        }
        
        //The next elements are the one we want to loop on...
        //We should have as many lists as variables
        //The actual code will start at b_loop...
        bool multiloop = false;
        for (var = 2; var < b_loop ; var++) {
            e = index(var)->eval(lisp);
            if (!e->isList())
                multiloop = true;
            values->appendraw(e);
            indexes->appendraw(e->thekeys(lisp));
        }
        
        if (!multiloop && !lisp->checkforLock())
            liste[0] = lisp->provideAtom(l_polyloop);
        
        while (looping) {
            for (var = 0; var < liste[1]->size(); var++) {
                idx = indexes->liste[var]->value_on_index(lisp, indexe);
                if (idx == null_) {
                    looping = false;
                    break;
                }
                e = values->liste[var]->value_on_index(lisp, idx);
                label = liste[1]->index(var)->label();
                lisp->recordingvalue(e, label);
            }
            if (!looping)
                break;
            e = null_;
            //We then execute our instructions
            for (i_loop = b_loop; i_loop < sz && e->type != l_return; i_loop++) {
                e->release();
                e = liste[i_loop]->eval(lisp);
            }
            if (e->type == l_return) {
                values->rawrelease();
                indexes->rawrelease();
                if (e->isBreak())
                    return null_;
                return e;
            }
            indexe++;
        }
    }
    catch(Error* err) {
        values->rawrelease();
        indexes->rawrelease();
        throw err;
    }
    
    values->rawrelease();
    indexes->rawrelease();
    return e;
}

Element* List::polyloop(LispE* lisp) {
    List* values = lisp->provideList();
    Element* e = null_;
    long sz = size();
    long var;
    long indexe = 0, i_loop;
    //this is where the actual code to be executed starts
    long b_loop = 2 + liste[1]->size();
    
    short label;
    bool looping = true;
    
    try {
        for (var = 0; var < liste[1]->size(); var++) {
            label = liste[1]->index(var)->label();
            if (label == v_null)
                throw new Error("Missing variable in 'loop'");
            lisp->recordingvalue(null_, label);
        }
        
        //The next elements are the one we want to loop on...
        //We should have as many lists as variables
        //The actual code will start at b_loop...
        for (var = 2; var < b_loop ; var++) {
            e = index(var)->eval(lisp);
            values->appendraw(e);
        }
        
        while (looping) {
            for (var = 0; var < liste[1]->size(); var++) {
                e = values->liste[var]->value_on_index(lisp, indexe);
                if (e == null_) {
                    looping = false;
                    break;
                }
                label = liste[1]->index(var)->label();
                lisp->recordingvalue(e, label);
            }
            if (!looping)
                break;
            e = null_;
            //We then execute our instructions
            for (i_loop = b_loop; i_loop < sz && e->type != l_return; i_loop++) {
                e->release();
                e = liste[i_loop]->eval(lisp);
            }
            if (e->type == l_return) {
                values->rawrelease();
                if (e->isBreak())
                    return null_;
                return e;
            }
            indexe++;
        }
    }
    catch(Error* err) {
        values->rawrelease();
        throw err;
    }
    
    values->rawrelease();
    return e;
}

//------------------------------------------------------------------------------------------
Element* s_findall(LispE* lisp, wstring& s, wstring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return emptylist_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return emptylist_;
    Integers* liste = lisp->provideIntegers();
    while (pos != -1) {
        liste->liste.push_back(pos);
        pos=s.find(sub,pos+sz);
    }
    return liste;
}

Element* s_findall(LispE* lisp, u_ustring& s, u_ustring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return emptylist_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return emptylist_;
    Integers* liste = lisp->provideIntegers();
    while (pos != -1) {
        liste->liste.push_back(pos);
        pos=s.find(sub,pos+sz);
    }
    return liste;
}


//------------------------------------------------------------------------------------------
Element* Element::insert(LispE* lisp, Element* e, long idx) {
    return null_;
}

Element* String::insert(LispE* lisp, Element* e, long idx) {
    u_ustring res;
    if (idx < 0)
        res = lisp->handlingutf8->u_insert_sep(content, e->asUString(lisp));
    else {
        if (idx >= content.size())
            res = content + e->asUString(lisp);
        else {
            res = content;
            res.insert(idx, e->asUString(lisp));
        }
    }
    return lisp->provideString(res);
}

Element* List::insert(LispE* lisp, Element* e, long idx) {
    if (idx < 0)
        throw new Error("Error: Wrong index in 'insert'");
 
    e = e->copying(false);
    List* l = (List*)duplicate_constant_container();
    l->liste.insert(idx, e);        
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

Element* List::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    List* list = lisp->provideList();
    long i, j;
    bool found;
    list->append(liste[0]->copying(false));
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < list->liste.size(); j++) {
            if (liste[i]->unify(lisp, list->liste[j], false)) {
                found = true;
                break;
            }
        }
        if (!found)
            list->append(liste[i]->copying(false));
    }
    return list;
}

Element* Numbers::insert(LispE* lisp, Element* e, long idx) {
    if (idx < 0)
        throw new Error("Error: Wrong index in 'insert'");
 
    Numbers* l = (Numbers*)duplicate_constant_container();
    l->liste.insert(l->liste.begin()+idx, e->asNumber());
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
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Integers::insert(LispE* lisp, Element* e, long idx) {
    if (idx < 0)
        throw new Error("Error: Wrong index in 'insert'");
 
    Integers* l = (Integers*)duplicate_constant_container();
    l->liste.insert(l->liste.begin()+idx, e->asInteger());
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
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

Element* Strings::insert(LispE* lisp, Element* e, long idx) {
    if (idx < 0)
        throw new Error("Error: Wrong index in 'insert'");
 
    Strings* l = (Strings*)duplicate_constant_container();
    l->liste.insert(l->liste.begin()+idx, e->asUString(lisp));
    return l;
}

Element* String::rotate(bool left) {
    if (content.size() <= 1)
        return this;
    
    String* s = new String(L"");
    if (left) {
        s->content = content.substr(1,content.size());
        s->content += content[0];
        return s;
    }
    s->content = content.back();
    s->content += content.substr(0, content.size()-1);
    return s;
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
    long i, j;
    bool found;
    nb->liste.push_back(liste[0]);
    for (i = 1; i < liste.size(); i++) {
        found = false;
        for (j = 0; j < nb->liste.size(); j++) {
            if (liste[i] == nb->liste[j]) {
                found = true;
                break;
            }
        }
        if (!found)
            nb->liste.push_back(liste[i]);
    }
    return nb;
}

//------------------------------------------------------------------------------------------

Element* Element::thekeys(LispE* lisp) {
    return emptylist_;
}

Element* String::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* List::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Numbers::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Integers::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
}

Element* Strings::thekeys(LispE* lisp) {
    Integers* keys = lisp->provideIntegers();
    for (long i = 0; i< size(); i++) {
        keys->liste.push_back(i);
    }
    return keys;
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

Element* Set_n::thekeys(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideNumber(a));
    }
    return keys;
}


Element* Dictionary::thekeys(LispE* lisp) {
    Strings* dkeys = lisp->provideStrings();
    u_ustring keyvalue;
    for (auto& a: dictionary) {
        keyvalue = a.first;
        dkeys->append(keyvalue);
    }
    return dkeys;
}

Element* Dictionary_n::thekeys(LispE* lisp) {
    Numbers* dkeys = lisp->provideNumbers();
    for (auto& a: dictionary) {
        dkeys->liste.push_back(a.first);
    }
    return dkeys;
}

Element* Element::thevalues(LispE* lisp) {
    return emptylist_;
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

Element* Set_n::thevalues(LispE* lisp) {
    Numbers* keys = lisp->provideNumbers();
    if (ensemble.empty())
        return keys;
    for (auto& a : ensemble) {
        keys->append(lisp->provideNumber(a));
    }
    return keys;
}

Element* Dictionary::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Dictionary_n::thevalues(LispE* lisp) {
    List* liste = lisp->provideList();
    for (auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

//------------------------------------------------------------------------------------------
Element* Element::Boolean(LispE* lisp) {
    return booleans_[Boolean()];
}

//------------------------------------------------------------------------------------------

Element* Set::next_iter(LispE* lisp, void* it) {
    std::set<u_ustring>::iterator* n = (std::set<u_ustring>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    u_ustring s = **n;
    Element* r = lisp->provideString(s);
    (*n)++;
    return r;
}

Element* Set_n::next_iter(LispE* lisp, void* it) {
    std::set<double>::iterator* n = (std::set<double>::iterator*)it;
    if (*n == ensemble.end())
        return emptyatom_;
    Element* r = lisp->provideNumber(**n);
    (*n)++;
    return r;
}

//------------------------------------------------------------------------------------------
long List::find_element(LispE* lisp, Element* valeur, long idx) {
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return i;
    }
    return -1;
}
//------------------------------------------------------------------------------------------
Element* Element::search_element(LispE* lisp, Element* valeur, long idx) {
    return null_;
}

Element* String::search_element(LispE* lisp, Element* valeur, long idx) {
    u_ustring val = valeur->asUString(lisp);
    idx =  content.find(val, idx);
    if (idx == -1)
        return null_;
    return lisp->provideInteger(idx);
}

Element* List::search_element(LispE* lisp, Element* valeur, long idx) {
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Numbers::search_element(LispE* lisp, Element* valeur, long idx) {
    double v = valeur->asNumber();
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}
Element* Integers::search_element(LispE* lisp, Element* valeur, long idx) {
    long v = valeur->asInteger();
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Strings::search_element(LispE* lisp, Element* valeur, long idx) {
    u_ustring v = valeur->asUString(lisp);
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Set::search_element(LispE* lisp, Element* valeur, long idx) {
    u_ustring k = valeur->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_n::search_element(LispE* lisp, Element* valeur, long idx) {
    double k = valeur->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return true_;
}

Element* Dictionary::search_element(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            u_ustring keyvalue = a.first;
           return lisp->provideString(keyvalue);
        }
    }
    return null_;
}


Element* Dictionary_n::search_element(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
       if (a.second->equal(lisp, valeur) == true_)
           return lisp->provideNumber(a.first);
    }
    return null_;
}

Element* Element::checkkey(LispE* lisp, Element* e) {
    return null_;
}

//------------------------------------------------------------------------------------------
Element* Set::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asUString(lisp)) == ensemble.end())
        return null_;
    return true_;
}

Element* Set_n::checkkey(LispE* lisp, Element* e) {
    if (ensemble.find(e->asNumber()) == ensemble.end())
        return null_;
    return true_;
}


Element* Dictionary::checkkey(LispE* lisp, Element* e) {
    try {
        return dictionary.at(e->asUString(lisp));
    }
    catch(...) {
        return null_;
    }
}

Element* Dictionary_n::checkkey(LispE* lisp, Element* e) {
    try {
        return dictionary.at(e->asNumber());
    }
    catch(...) {
        return null_;
    }
}


//------------------------------------------------------------------------------------------
Element* Element::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    return emptylist_;
}

Element* String::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    u_ustring val = valeur->asUString(lisp);
    idx =  content.find(val, idx);
    return s_findall(lisp,content, val, idx);
}

Element* List::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Integers* l = lisp->provideIntegers();
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_) {
            l->liste.push_back(i);
        }
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Numbers::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Integers* l = lisp->provideIntegers();
    double v = valeur->asNumber();
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Integers::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Integers* l = lisp->provideIntegers();
    long v = valeur->asInteger();
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Strings::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Integers* l = lisp->provideIntegers();
    u_ustring v = valeur->asUString(lisp);
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i] == v)
            l->liste.push_back(i);
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Set::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_n::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}


Element* Dictionary::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            keyvalue = a.first;
           l->append(keyvalue);
        }
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Dictionary_n::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    Numbers* l = lisp->provideNumbers();
    for (auto& a : dictionary) {
       if (a.second->equal(lisp, valeur) == true_)
           l->liste.push_back(a.first);
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

//------------------------------------------------------------------------------------------
Element* Element::search_reverse(LispE* lisp, Element* valeur, long idx) {
    return minusone_;
}

Element* String::search_reverse(LispE* lisp, Element* valeur, long idx) {
    u_ustring val = valeur->asUString(lisp);
    idx =  content.rfind(val, content.size() - idx);
    return lisp->provideInteger(idx);
}

Element* List::search_reverse(LispE* lisp, Element* valeur, long idx) {
    for (long i = liste.size() - 1; i >= idx; i--) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Numbers::search_reverse(LispE* lisp, Element* valeur, long idx) {
    double v = valeur->asNumber();
    for (long i = liste.size() - 1; i >= idx; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Integers::search_reverse(LispE* lisp, Element* valeur, long idx) {
    long v = valeur->asInteger();
    for (long i = liste.size() - 1; i >= idx; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Strings::search_reverse(LispE* lisp, Element* valeur, long idx) {
    u_ustring v = valeur->asUString(lisp);
    for (long i = liste.size() - 1; i >= idx; i--) {
        if (liste[i] == v)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Set::search_reverse(LispE* lisp, Element* valeur, long idx) {
    Strings* l = lisp->provideStrings();
    u_ustring keyvalue = valeur->asUString(lisp);
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Set_n::search_reverse(LispE* lisp, Element* valeur, long idx) {
    Numbers* l = lisp->provideNumbers();
    double keyvalue = valeur->asNumber();
    if (ensemble.find(keyvalue) == ensemble.end())
        return emptylist_;
    l->liste.push_back(keyvalue);
    return l;
}

Element* Dictionary::search_reverse(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            u_ustring keyvalue = a.first;
           return lisp->provideString(keyvalue);
        }
    }
    return emptystring_;
}

Element* Dictionary_n::search_reverse(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
       if (a.second->equal(lisp, valeur) == true_)
           return lisp->provideNumber(a.first);
    }
    return minusone_;
}


//------------------------------------------------------------------------------------------
Element* Element::reverse(LispE* lisp, bool duplique) {
    return emptylist_;
}

Element* Number::reverse(LispE* lisp, bool duplique) {
    return lisp->provideNumber(number*-1);
}

Element* Integer::reverse(LispE* lisp, bool duplique) {
    return lisp->provideInteger(integer*-1);
}

Element* String::reverse(LispE* lisp, bool duplique) {
    u_ustring resultat;
    for (long i = content.size()-1; i >= 0; i--)
        resultat += content[i];

    if (duplique)
        return lisp->provideString(resultat);

    content = resultat;
    return this;
}

Element* List::reverse(LispE* lisp, bool duplique) {
    if (liste.size() <= 1)
        return this;
    
    if (duplique) {
        List* l = lisp->provideList();
        for (long i = liste.size()-1; i >= 0; i--)
            l->append(liste[i]->copying(false));
        return l;
    }

    liste.reverse();
    return this;
}

Element* Numbers::reverse(LispE* lisp, bool duplique) {
    if (liste.size() <= 1)
        return this;
    
    if (duplique) {
        Numbers* l = lisp->provideNumbers();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }

    std::reverse(liste.begin(), liste.end());
    return this;
}

Element* Integers::reverse(LispE* lisp, bool duplique) {
    if (liste.size() <= 1)
        return this;
    
    if (duplique) {
        Integers* l = lisp->provideIntegers();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }

    std::reverse(liste.begin(), liste.end());
    return this;
}

Element* Strings::reverse(LispE* lisp, bool duplique) {
    if (liste.size() <= 1)
        return this;
    
    if (duplique) {
        Strings* l = lisp->provideStrings();
        for (long i = liste.size()-1; i >= 0; i--) {
            l->liste.push_back(liste[i]);
        }
        return l;
    }

    std::reverse(liste.begin(), liste.end());
    return this;
}


Element* Dictionary::reverse(LispE* lisp, bool duplique) {
    Dictionary* dico = new Dictionary;
    
    u_ustring k;
    Element* e;
    for (auto& a: dictionary) {
        k = a.second->asUString(lisp);
        e = dico->dictionary[k];
        if (e == NULL) {
            e = lisp->provideStrings();
            dico->dictionary[k] = e;
            e->incrementstatus(1, false);
        }
        k = a.first;
        ((Strings*)e)->append(k);
    }
    return dico;
}

Element* Dictionary_n::reverse(LispE* lisp, bool duplique) {
    Dictionary_n* dico = new Dictionary_n;
    
    double k;
    Element* e;
    for (auto& a: dictionary) {
        k = a.second->asNumber();
        e = dico->dictionary[k];
        if (e == NULL) {
            e = lisp->provideNumbers();
            dico->dictionary[k] = e;
            e->incrementstatus(1, false);
        }
        ((Numbers*)e)->liste.push_back(a.first);
    }
    return dico;
}

Element* List::rotate(LispE* lisp, long axis) {
    return reverse(lisp, true);
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
        e = new Numbers((Numbers*)liste[i]);
        revert_matrix->append(e);
    }
    return revert_matrix;
}

Element* Tenseur::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return new Numbers((Numbers*)value);
    
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

//------------------------------------------------------------------------------------------
Element* Element::protected_index(LispE* lisp,long i) {
    return null_;
}

Element* Element::protected_index(LispE* lisp, u_ustring&) {
    return null_;
}

Element* Element::protected_index(LispE* lisp, double k) {
    return null_;
}

Element* String::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* List::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return liste[i];
    return null_;
}

Element* Numbers::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    return null_;
}

Element* Integers::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    return null_;
}

Element* Strings::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    return null_;
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


//------------------------------------------------------------------------------------------
Element* Element::last_element(LispE* lisp) {
    return null_;
}

Element* String::last_element(LispE* lisp) {
    if (!content.size())
        return null_;
    wchar_t c = content.back();
    return lisp->provideString(c);
}

Element* List::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return liste.back();
}

Element* Numbers::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideNumber(liste.back());
}

Element* Integers::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideInteger(liste.back());
}

Element* Strings::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return lisp->provideString(liste.back());
}


//------------------------------------------------------------------------------------------
Element* Element::value_on_index(LispE* lisp, long i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    return null_;
}

Element* Numbers::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    return null_;
}

Element* Integers::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    return null_;
}

Element* Strings::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    return null_;
}

Element* String::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
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

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(wstring& k, LispE* lisp) {
    return null_;
}

Element* Element::value_on_index(u_ustring& k, LispE* lisp) {
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


Element* Dictionary::value_on_index(u_ustring& k, LispE* lisp) {
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary::value_on_index(wstring& u, LispE* lisp) {
    u_pstring k = _w_to_u(u);
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}


Element* Set::protected_index(LispE* lisp, u_ustring& k) {
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Dictionary::protected_index(LispE* lisp, u_ustring& k) {
    try {
        return dictionary.at(k);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(double k, LispE* lisp) {
    return null_;
}

Element* Dictionary_n::value_on_index(double k, LispE* lisp) {
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary_n::protected_index(LispE* lisp, double k) {
    try {
        return dictionary.at(k);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(LispE* lisp, Element* i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    
    return null_;
}

Element* Numbers::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    
    return null_;
}

Element* Integers::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    
    return null_;
}

Element* Strings::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    
    return null_;
}

Element* String::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* Set::value_on_index(LispE* lisp, Element* idx) {
    u_ustring k = idx->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_n::value_on_index(LispE* lisp, Element* idx) {
    double k = idx->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideNumber(k);
}


Element* Dictionary::value_on_index(LispE* lisp, Element* idx) {
    u_ustring k = idx->asUString(lisp);
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary_n::value_on_index(LispE* lisp, Element* idx) {
    try {
        return dictionary.at(idx->checkNumber(lisp))->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}
//------------------------------------------------------------------------------------------
Element* Element::protected_index(LispE* lisp,Element*) {
    throw new Error("Error: value cannot be access through index");
}

Element* String::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);

    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    throw new Error("Error: index out of bounds");
}

Element* List::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i];
    
    throw new Error("Error: index out of bounds");
}

Element* Numbers::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideNumber(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Integers::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideInteger(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Strings::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return lisp->provideString(liste[i]);
    
    throw new Error("Error: index out of bounds");
}

Element* Set::protected_index(LispE* lisp, Element* idx) {
    u_ustring k = idx->asUString(lisp);
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideString(k);
}

Element* Set_n::protected_index(LispE* lisp, Element* idx) {
    double k = idx->asNumber();
    if (ensemble.find(k) == ensemble.end())
        return null_;
    return lisp->provideNumber(k);
}


Element* Dictionary::protected_index(LispE* lisp, Element* idx) {
    u_ustring k = idx->asUString(lisp);
    try {
        return dictionary.at(k);
    }
    catch (const std::out_of_range& oor) {
        throw new Error("Error: index out of bounds");
    }
}

Element* Dictionary_n::protected_index(LispE* lisp, Element* idx) {
    try {
        return dictionary.at(idx->checkNumber(lisp));
    }
    catch (const std::out_of_range& oor) {
        throw new Error("Error: index out of bounds");
    }
}

//------------------------------------------------------------------------------------------
Element* Element::join_in_list(LispE* lisp, u_ustring& sep) {
    throw new Error("Error: 'join' can only be used for lists");
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

Element* Dictionary::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary_n::join_in_list(LispE* lisp, u_ustring& sep) {
    if (sep==U"")
        sep = U",";
    u_ustring str;
    u_ustring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += U":";
        str += a.second->asUString(lisp);
    }
    return lisp->provideString(str);
}


//------------------------------------------------------------------------------------------
Element* Element::charge(LispE* lisp, string chemin) {
    return emptyatom_;
}
//------------------------------------------------------------------------------------------
Element* Element::replace(LispE* lisp, long i, Element* e) {
    throw new Error("Error: cannot modify this element");
}

Element* String::replace(LispE* lisp, long i, Element* e) {
    if (i < 0 || i >= content.size())
        throw new Error("Error: cannot modify at this position");
    u_ustring c = content.substr(0, i);
    c += e->asUString(lisp);
    c += content.substr(i+1, content.size());
    return lisp->provideString(c);
}
//------------------------------------------------------------------------------------------

wstring Infinitelist::asString(LispE* lisp) {
    wstring tampon(L"(repeat ");
    tampon += value->asString(lisp);
    tampon += L")";
    return tampon;
}

wstring Cyclelist::asString(LispE* lisp) {
    wstring tampon(L"(cycle ");
    tampon += value->asString(lisp);
    tampon += L")";
    return tampon;
}

wstring InfiniterangeNumber::asString(LispE* lisp) {
    std::wstringstream str;
    str << L"(irange " << initial_value << " " << increment << L")";
    return str.str();
}

wstring InfiniterangeInteger::asString(LispE* lisp) {
    std::wstringstream str;
    str << L"(irange " << initial_value << " " << increment << L")";
    return str.str();
}

wstring Error::asString(LispE* lisp) {
    if (lisp != NULL && lisp->delegation->i_current_file != -1) {
        string s = lisp->delegation->allfiles_names[lisp->delegation->i_current_file];
        wstring w;
        s_utf8_to_unicode(w, USTR(s), s.size());
        std::wstringstream msg;
        msg << message << L", line: " << lisp->delegation->i_current_line << L" in: " << w;
        return msg.str();
    }
    else
        return message;
}
//------------------------------------------------------------------------------------------
u_ustring Infinitelist::asUString(LispE* lisp) {
    u_ustring tampon(U"(repeat ");
    tampon += value->asUString(lisp);
    tampon += U")";
    return tampon;
}

u_ustring Cyclelist::asUString(LispE* lisp) {
    u_ustring tampon(U"(cycle ");
    tampon += value->asUString(lisp);
    tampon += U")";
    return tampon;
}

//------------------------------------------------------------------------------------------
bool Element::check_arity(LispE* lisp, unsigned long arity) {
    if (type == t_atom)
        return eval(lisp)->check_arity(lisp, arity);
    if (type < l_final)
        return lisp->delegation->sameArity(type, arity);
    return false;
}

//------------------------------------------------------------------------------------------
Element* List::newInstance(Element* e) {
    long i;
    long sz = size();
    for (i = 0; i < sz; i++) {
        if (liste[i]->isList())
            break;
    }
    
    if (i == sz) {
        if (e->type == t_number)
            return new Numbers(sz, e->asNumber());
        if (e->type == t_integer)
            return new Integers(sz, e->asInteger());
        if (e->type == t_string)
            return new Strings(sz, e->asString(NULL));
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

//------------------------------------------------------------------------------------------
Element* Element::equal(LispE* lisp, Element* e) {
    return booleans_[(e==this)];
}

Element* Atome::equal(LispE* lisp, Element* e) {
    return booleans_[(e->label() == atome)];
}

Element* Maybe::equal(LispE* lisp, Element* e) {
    return booleans_[(e->label() == t_error)];
}

Element* List::equal(LispE* lisp, Element* e) {
    return booleans_[e->isList() && liste.equal(((List*)e)->liste)];
}

Element* Dictionary::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0) || e == this)];
}

Element* Dictionary_n::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0) || e== this)];
}

Element* String::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content == e->asUString(lisp))];
}

Element* Number::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && number == e->asNumber())];
}

Element* Integer::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && integer == e->asInteger())];
}

Element* Numbers::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_numbers && liste == ((Numbers*)e)->liste)];
}

Element* Integers::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_integers && liste == ((Integers*)e)->liste)];
}

Element* Strings::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_strings && liste == ((Strings*)e)->liste)];
}

Element* Set::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_set && ensemble == ((Set*)e)->ensemble)];
}

Element* Set_n::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_setn && ensemble == ((Set_n*)e)->ensemble)];
}

//------------------------------------------------------------------------------------------
Element* Element::less(LispE* lisp, Element* e) {
    return false_;
}

Element* Element::lessorequal(LispE* lisp, Element* e){
    return false_;
}

Element* Element::more(LispE* lisp, Element* e) {
    return false_;
}

Element* Element::moreorequal(LispE* lisp, Element* e) {
    return false_;
}

Element* String::less(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content < e->asUString(lisp))];
}

Element* String::lessorequal(LispE* lisp, Element* e){
    return booleans_[(e->type == t_string && content <= e->asUString(lisp))];
}

Element* String::more(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content > e->asUString(lisp))];
}

Element* String::moreorequal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content >= e->asUString(lisp))];
}

Element* Number::less(LispE* lisp, Element* e) {
    return booleans_[number < e->checkNumber(lisp)];
}

Element* Number::lessorequal(LispE* lisp, Element* e){
    return booleans_[number <= e->checkNumber(lisp)];
}

Element* Number::more(LispE* lisp, Element* e) {
    return booleans_[number > e->checkNumber(lisp)];
}

Element* Number::moreorequal(LispE* lisp, Element* e) {
    return booleans_[number >= e->checkNumber(lisp)];
}

Element* Integer::less(LispE* lisp, Element* e) {
    return booleans_[integer < e->checkInteger(lisp)];
}

Element* Integer::lessorequal(LispE* lisp, Element* e){
    return booleans_[integer <= e->checkInteger(lisp)];
}

Element* Integer::more(LispE* lisp, Element* e) {
    return booleans_[integer > e->checkInteger(lisp)];
}

Element* Integer::moreorequal(LispE* lisp, Element* e) {
    return booleans_[integer >= e->checkInteger(lisp)];
}

//------------------------------------------------------------------------------------------
Element* Element::plus(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '+' with these operands");
}

Element* Element::bit_not(LispE* lisp) {
    throw new Error("Error: cannot evaluate '~' for this operand");
}

Element* Element::bit_and(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '&' with these operands");
}

Element* Element::bit_and_not(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '&' with these operands");
}

Element* Element::bit_or(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '|' with these operands");
}

Element* Element::bit_xor(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '^' with these operands");
}

Element* Element::minus(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '-' with these operands");
}

Element* Element::multiply(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '*' with these operands");
}

Element* Element::divide(LispE* lisp, Element* e)  {
    throw new Error("Error: cannot evaluate '/' with these operands");
}

Element* Element::mod(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '%' with these operands");
}

Element* Element::power(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '^^' with these operands");
}

Element* Element::leftshift(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '<<' with these operands");
}

Element* Element::rightshift(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '>>' with these operands");
}

Element* String::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->plus(lisp, e);
    }

    if (status != s_constant) {
        content += e->asUString(lisp);
        return this;
    }
    u_ustring c = content + e->asUString(lisp);
    return lisp->provideString(c);
}

double Element::checkNumber(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}

long Element::checkInteger(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}

Element* Number::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->plus(lisp, e);
    }
    if (status != s_constant) {
        number += e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number+e->checkNumber(lisp));
}

Element* Number::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        number -= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number-e->checkNumber(lisp));
}

Element* Number::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->multiply(lisp, e);
    }
    if (status != s_constant) {
        number *= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number*e->checkNumber(lisp));
}

Element* Number::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    double v = e->checkNumber(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        number /= v;
        return this;
    }
    return lisp->provideNumber(number/v);
}

Element* Number::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }

    long v = e->checkInteger(lisp);
    if (!v)
        throw new Error("Error: division by zero");

    if (status != s_constant) {
        number = (long)number % v;
        return this;
    }
    return lisp->provideNumber((long)number%v);
}

Element* Number::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->power(lisp, e);
    }
    if (status != s_constant) {
        number = pow(number, e->checkNumber(lisp));
        return this;
    }
    return lisp->provideNumber(pow(number, e->checkNumber(lisp)));
}

Element* Number::bit_not(LispE* lisp)  {
    double64 d(number);
    d.bits = ~d.bits;
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}


Element* Number::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    double64 d(number);
    d.bits &= ~e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and(lisp, e);
    }
    double64 d(number);
    d.bits &= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}


Element* Number::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_or(lisp, e);
    }
    double64 d(number);
    d.bits |= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_xor(lisp, e);
    }
    double64 d(number);
    d.bits ^= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    double64 d(number);
    d.bits <<= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    double64 d(number);
    d.bits >>= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Integer::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->plus(lisp, e);
    }
    if (e->type == t_number) {
        double v = (double)integer + e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer += e->checkInteger(lisp);
        return this;
    }
        
    return lisp->provideInteger(integer+e->checkInteger(lisp));
}

Element* Integer::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (e->type == t_number) {
        double v = (double)integer - e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer -= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer-e->checkInteger(lisp));
}

Element* Integer::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->multiply(lisp, e);
    }
    if (e->type == t_number) {
        double v = (double)integer * e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer *= e->asInteger();
        return this;
    }
    return lisp->provideInteger(integer*e->asInteger());
}

Element* Integer::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    double v =  e->checkNumber(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    release();
    return lisp->provideNumber((double)integer/v);
}


Element* Integer::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }
    long v =  e->checkInteger(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    
    if (status != s_constant) {
        integer %= v;
        return this;
    }
    return lisp->provideInteger(integer%v);
}

Element* Integer::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Numbers* n = new Numbers(e->size(), integer);
        return n->power(lisp, e);
    }
    double v = pow((double)integer, e->checkNumber(lisp));
    release();
    return lisp->provideNumber(v);
}

Element* Integer::bit_not(LispE* lisp)  {
    if (status != s_constant) {
        integer = ~integer;
        return this;
    }
    return lisp->provideInteger(~integer);
}


Element* Integer::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and(lisp, e);
    }
    if (status != s_constant) {
        integer &= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer&e->checkInteger(lisp));
}

Element* Integer::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    if (status != s_constant) {
        integer &= ~e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer&~e->checkInteger(lisp));
}

Element* Integer::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_or(lisp, e);
    }
    if (status != s_constant) {
        integer |= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer|e->checkInteger(lisp));
}

Element* Integer::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_xor(lisp, e);
    }
    if (status != s_constant) {
        integer ^= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer^e->checkInteger(lisp));
}


Element* Integer::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    if (status != s_constant) {
        integer <<= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer<<e->checkInteger(lisp));
}


Element* Integer::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    if (status != s_constant) {
        integer >>= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer>>e->checkInteger(lisp));
}

Element* List::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, liste[i]->bit_not(l));
        }
        return this;
    }
    List* lst = (List*)newInstance();
    for (long i = 0; i < size(); i++) {
        lst->append(liste[i]->bit_not(l));
    }
    return lst;
}


Element* List::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_and(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_and(lisp, e));
    }
    return this;
}

Element* List::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_and_not(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_and_not(lisp, e));
    }
    return this;
}


Element* List::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_or(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_or(lisp, e));
    }
    return this;
}

Element* List::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_xor(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_xor(lisp, e));
    }
    return this;
}

Element* List::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->plus(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->plus(lisp, e));
    }
    return this;
}

Element* List::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->minus(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->minus(lisp, e));
    }
    return this;
}

Element* List::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->multiply(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->multiply(lisp, e));
    }
    return this;
}

Element* List::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->divide(lisp, e));
    }
    return this;
}

Element* List::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->mod(lisp, e));
    }
    return this;
}

Element* List::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->power(lisp, e));
    }
    return this;
}

Element* List::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->leftshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->leftshift(lisp, e));
    }
    return this;
}

Element* List::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->rightshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->rightshift(lisp, e));
    }
    return this;
}

Element* Strings::plus(LispE* lisp, Element* e) {
    //Two cases either e is a string or it is a list...
    if (e == NULL) {
        u_ustring d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste[i];
        }
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

Element* Set::plus(LispE* lisp, Element* e) {
    //Two cases either e is a string or it is a list...
    u_ustring d;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d += a;
        }
        return lisp->provideString(d);
    }
    Set* res = new Set;
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

Element* Numbers::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, index(i)->bit_not(l));
        }
        return this;
    }
    double64 d(0);
    Numbers* num = (Numbers*)newInstance();
    for (long i = 0; i < size(); i++) {
        d.v = liste[i];
        d.bits = ~d.bits;
        num->liste.push_back(d.v);
    }
    return num;
}


Element* Numbers::bit_and(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= r.bits;
        }
        return lisp->provideNumber(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_and(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_and(lisp, e));
    }
    return this;
}

Element* Numbers::bit_and_not(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= ~r.bits;
        }
        return lisp->provideNumber(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_and_not(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_and_not(lisp, e));
    }
    return this;
}

Element* Numbers::bit_or(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits |= r.bits;
        }
        return lisp->provideNumber(d.v);
    }

    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_or(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_or(lisp, e));
    }
    return this;
}

Element* Numbers::bit_xor(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits ^= r.bits;
        }
        return lisp->provideNumber(d.v);
    }

    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_xor(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_xor(lisp, e));
    }
    return this;
}

Element* Numbers::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] += e->asNumber();
    }
    return this;
}

Element* Numbers::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] -= e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] -= e->asNumber();
    }
    return this;
}

Element* Numbers::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d *= liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] *= e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] *= e->asNumber();
    }
    return this;
}

Element* Numbers::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d /= liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->divide(lisp, e));
    }
    return this;
}

Element* Numbers::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (liste[i] == 0)
                throw new Error("Error: division by zero");
            d %= (long)liste[i];
        }
        return lisp->provideNumber(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->mod(lisp, e));
    }
    return this;
}

Element* Numbers::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d,liste[i]);
        }
        return lisp->provideNumber(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->power(lisp, e));
    }
    return this;
}

Element* Numbers::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits <<= (long)liste[i];
        }
        return lisp->provideNumber(d.v);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->leftshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->leftshift(lisp, e));
    }
    return this;
}

Element* Numbers::rightshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits >>= (long)liste[i];
        }
        return lisp->provideNumber(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->rightshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->rightshift(lisp, e));
    }
    return this;
}

Element* Integers::bit_not(LispE* l) {
    if (!status) {
        for (long i = 0; i < size(); i++) {
            liste[i] = ~liste[i];
        }
        return this;
    }
    Integers* num = (Integers*)newInstance();
    for (long i = 0; i < size(); i++)
        num->liste.push_back(~liste[i]);
    return num;
}


Element* Integers::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d &= liste[i];
        }
        return lisp->provideInteger(d);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] &= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] &= e->asInteger();
    }
    return this;
}

Element* Integers::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d &= ~liste[i];
        }
        return lisp->provideInteger(d);
    }

    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] &= ~e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] &= ~e->asInteger();
    }
    return this;
}


Element* Integers::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d |= liste[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] |= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] |= e->asInteger();
    }
    return this;
}

Element* Integers::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d ^= liste[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] ^= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] ^= e->asInteger();
    }
    return this;
}

Element* Integers::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] += e->asInteger();
    }
    return this;
}

Element* Integers::minus(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste[i];
        }
        return lisp->provideInteger(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] -= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] -= e->asInteger();
    }
    return this;
}

Element* Integers::multiply(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d *= liste[i];
        }
        return lisp->provideInteger(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] *= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] *= e->asInteger();
    }
    return this;
}

Element* Integers::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d /= liste[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->divide(lisp, e));
    }
    return this;
}

Element* Integers::mod(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d %= liste[i];
        }
        return lisp->provideInteger(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->mod(lisp, e));
    }
    return this;
}

Element* Integers::power(LispE* lisp, Element* e) {
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d, (double)liste[i]);
        }
        return lisp->provideInteger(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->power(lisp, e));
    }
    return this;
}

Element* Integers::leftshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d <<= liste[i];
        }
        return lisp->provideInteger(d);
    }
   //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] <<= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] <<= e->asInteger();
    }
    return this;
}

Element* Integers::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d >>= liste[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] >>= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] >>= e->asInteger();
    }
    return this;
}

Element* Set_n::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d += a;
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
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
            d = a + e->index(i)->asNumber();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    double w = e->asNumber();
    for (auto& a: ensemble) {
        d = a + w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
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
        return lisp->provideNumber(d);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
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
            d = a - e->index(i)->asNumber();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    double w = e->asNumber();
    for (auto& a: ensemble) {
        d = a - w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 1;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d *= a;
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
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
            d = a * e->index(i)->asNumber();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    double w = e->asNumber();
    for (auto& a: ensemble) {
        d = a * w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::divide(LispE* lisp, Element* e) {
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

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
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


Element* Set_n::mod(LispE* lisp, Element* e) {
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

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
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

Element* Set_n::power(LispE* lisp, Element* e) {
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
                d = pow(d, a);
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = pow(a,*nxt);
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
            d = pow(a, w) ;
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
        d = pow(a, w);
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits &= ~r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits &= ~r.bits;
            res->add(d.v);
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
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits &= ~r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits &= ~r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits &= r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits &= r.bits;
            res->add(d.v);
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
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits &= r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits &= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits |= r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits |= r.bits;
            res->add(d.v);
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
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits |= r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits |= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits ^= r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits ^= r.bits;
            res->add(d.v);
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
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits ^= r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits ^= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    Set_n* res = new Set_n;

    for (auto& a: ensemble) {
        d.v = a;
        d.bits = ~d.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else
                d.bits <<= (long)a;
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            d.bits <<= (long)*nxt;
            res->add(d.v);
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
            d.v = a;
            d.bits <<= w;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits <<= w;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else
                d.bits >>= (long)a;
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = new Set_n;
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            d.bits >>= (long)*nxt;
            res->add(d.v);
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
            d.v = a;
            d.bits >>= w;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits >>= w;
        res->add(d.v);
    }
    release();
    return res;
}
//------------------------------------------------------------------------------------------
Element* Element::extraction(LispE* lisp, List* l) {
    return null_;
}

Element* List::extraction(LispE* lisp, List* l) {
    Element* e_from = l->liste[2];
    Element* e;
    
    long from = 0;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }

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
            if (e == minusone_)
                return emptylist_;
            //We skip the first characters
            from = e->asInteger() + 1;
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_number:
        case t_integer:
            from = e_from->asInteger();
            if (from < 0)
                from = size() + from;
            break;
        default:
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e_from->release();

    if (from < 0 || from >= size())
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
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }

    long upto;

    switch (ty) {
        case t_string: {
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_plus_string: {
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            //All characters are integrated
            upto = e->asInteger() + 1;
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger() - 1;
            break;
        }
        case t_number:
        case t_integer:
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
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }

    e_upto->release();
    if (upto <= from)
        return emptylist_;

    if (upto > size())
        upto = size();
    l = lisp->provideList();
    for (;from < upto; from++)
        l->append(liste[from]->copying(false));
    return l;
}

Element* Numbers::extraction(LispE* lisp, List* l) {
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
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideNumber(liste[depuis]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
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
    for (;depuis < upto; depuis++) {
        n->liste.push_back(liste[depuis]);
    }
    return n;
}

Element* Integers::extraction(LispE* lisp, List* l) {
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
    if (l->size() == 3) {
        //On returns only one element
        return lisp->provideInteger(liste[depuis]);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
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
    for (;depuis < upto; depuis++) {
        n->liste.push_back(liste[depuis]);
    }
    return n;
}

Element* Strings::extraction(LispE* lisp, List* l) {

    Element* e_from = l->liste[2];
    Element* e;
    
    long from = 0;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = l->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }

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
            if (e == minusone_)
                return emptylist_;
            //We skip the first characters
            from = e->asInteger() + 1;
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_from, 0);
            if (e == minusone_)
                return emptylist_;
            firstisString = e->asInteger();
            break;
        }
        case t_number:
        case t_integer:
            from = e_from->asInteger();
            if (from < 0)
                from = size() + from;
            break;
        default:
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e_from->release();

    if (from < 0 || from >= size())
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
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = l->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }

    long upto;

    switch (ty) {
        case t_string: {
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_plus_string: {
            e = search_element(lisp, e_upto, from + firstisString);
            if (e == null_)
                return emptylist_;
            //All characters are integrated
            upto = e->asInteger() + 1;
            break;
        }
        case t_minus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger();
            break;
        }
        case t_minus_plus_string: {
            e = search_reverse(lisp, e_upto, 0);
            if (e == minusone_)
                return emptylist_;
            upto = e->asInteger() - 1;
            break;
        }
        case t_number:
        case t_integer:
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
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }

    e_upto->release();
    if (upto <= from)
        return emptylist_;

    if (upto > size())
        upto = size();
    Strings* n = lisp->provideStrings();
    for (;from < upto; from++) {
        n->liste.push_back(liste[from]);
    }
    return n;
}

Element* String::extraction(LispE* lisp, List* liste) {
    Element* e_from = liste->liste[2];
    
    long from;
    long firstisString = -1;
    short nxt = 3;
    short ty;
    switch (e_from->label()) {
        case l_minus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-'");
            break;
        case l_plus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '+'");
            break;
        case l_minus_plus:
            e_from = liste->liste[3]->eval(lisp);
            nxt = 4;
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after first operator: '-+'");
            break;
        default:
            e_from = e_from->eval(lisp);
            ty = e_from->type;
    }

    switch (ty) {
        case t_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.find(ch);
            if (from == -1)
                return emptystring_;
            from += ch.size();
            firstisString = 0;
            break;
        }
        case t_plus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.find(ch);
            if (from == -1)
                return emptystring_;
            firstisString = ch.size();
            break;
        }
        case t_minus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.rfind(ch, content.size());
            if (from == -1)
                return emptystring_;
            //We skip the first characters
            from += ch.size();
            firstisString = 0;
            break;
        }
        case t_minus_plus_string: {
            u_ustring ch = e_from->asUString(lisp);
            from = content.rfind(ch, content.size());
            if (from == -1)
                return emptystring_;
            firstisString = ch.size();
            break;
        }
        case t_number:
        case t_integer:
            from = e_from->asInteger();
            if (from < 0)
                from = content.size() + from;
            break;
        default:
            e_from->release();
            throw new Error("Error: cannot use the first position in 'extract'");
    }
    
    e_from->release();

    if (from < 0 || from >= content.size())
        return emptystring_;

    if (nxt == liste->size()) {
        //Only one element is returned
        return lisp->provideString(content[from]);
    }

    Element* e_upto = liste->liste[nxt];
    switch (e_upto->label()) {
        case l_minus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_minus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-'");
            break;
        case l_plus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_upto->type;
            if (ty == t_string)
                ty = t_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '+'");
            break;
        case l_minus_plus:
            e_upto = liste->liste[nxt+1]->eval(lisp);
            ty = e_from->type;
            if (ty == t_string)
                ty = t_minus_plus_string;
            else
                throw new Error("Error: Wrong value after second operator: '-+'");
            break;
        default:
            e_upto = e_upto->eval(lisp);
            ty = e_upto->type;
    }

    long upto;

    switch (ty) {
        case t_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_plus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            //All characters are integrated
            upto += ch.size();
            break;
        }
        case t_minus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.rfind(ch, content.size());
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_minus_plus_string: {
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.rfind(ch, content.size());
            if (upto == -1)
                return emptystring_;
            //All characters are integrated
            upto += ch.size();
            break;
        }
        case t_number:
        case t_integer:
            upto = e_upto->asInteger();
            if (firstisString != -1 && upto > 0) {
                //in this case upto is a number of characters, not a position
                upto += from + firstisString;
            }
            else {
                if (upto <= 0) {
                    //We start from the end...
                    upto = content.size() + upto;
                }
            }
            break;
        default:
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'extract'");
    }

    e_upto->release();
    if (upto <= from)
        return emptystring_;

    if (upto > content.size())
        upto = content.size();
    u_ustring remplace = content.substr(from, upto - from);
    return lisp->provideString(remplace);
}
//------------------------------------------------------------------------------------------
Element* List::duplicate_constant_container(bool pair) {
    if (status == s_constant) {
        List* l;
        if (pair)
            l =  new Pair();
        else
            l = (List*)newInstance();
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copying(false));
        }
        return l;
    }
    return this;
}

Element* Numbers::duplicate_constant_container(bool pair) {
    if (status == s_constant) {
        Numbers* l = (Numbers*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Integers::duplicate_constant_container(bool pair) {
    if (status == s_constant) {
        Integers* l = (Integers*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}

Element* Strings::duplicate_constant_container(bool pair) {
    if (status == s_constant) {
        Strings* l = (Strings*)newInstance();
        l->liste = liste;
        return l;
    }
    return this;
}

//------------------------------------------------------------------------------------------
//For running car/cdr, everything that is not List is an error
Element* Element::cadr(LispE* lisp, Element*) {
    throw new Error("Error: You can only apply 'car/cdr' to a list");
}

Element* Element::car(LispE*) {
    throw new Error("Error: You can only apply 'car' to a list");
}

Element* Element::cdr(LispE*) {
    throw new Error("Error: 'cdr' can only be applied to a list");
}

Element* String::car(LispE* lisp) {
    if (content.size() == 0)
        throw new Error("Error: Empty string");
    return lisp->provideString(content[0]);
}

Element* String::cdr(LispE* lisp) {
    if (content.size() == 0)
        throw new Error("Error: Empty string");
    u_ustring w = content.substr(1, content.size()-1);
    return lisp->provideString(w);
}

Element* Cadr::cadr(LispE* lisp, Element* e) {
    long pos = 0;
    long sz = e->size();
    bool pair = (e->type == t_pair);
    
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            e = e->protected_index(lisp, pos);
            if (e == null_)
                throw new Error("Error: You can only apply 'car/cdr' to a list");
         
            pair = (e->type == t_pair);
            sz = e->size();
            pos = 0;
        }
        else {
            if (pos == sz)
                throw new Error("Error: You can only apply 'car/cdr' to a list");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        if (pair) {
            //The last one...
            if (pos == sz - 1)
                return e->index(pos);
            return new Pair((Pair*)e, pos);
        }
        else
            return new List((List*)e, pos);
    }

    return e;
}


Element* List::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return liste[0];
}

Element* List::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new List(this, 1);
}

Element* Numbers::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideNumber(liste[0]);
}

Element* Numbers::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    Numbers* n = lisp->provideNumbers();
    for (long i = 1; i < liste.size(); i++) {
        n->liste.push_back(liste[i]);
    }
    return n;
}

Element* Integers::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideInteger(liste[0]);
}

Element* Integers::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    Integers* n = lisp->provideIntegers();
    for (long i = 1; i < liste.size(); i++) {
        n->liste.push_back(liste[i]);
    }
    return n;
}

Element* Strings::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return lisp->provideString(liste[0]);
}

Element* Strings::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    Strings* n = lisp->provideStrings();
    for (long i = 1; i < liste.size(); i++) {
        n->liste.push_back(liste[i]);
    }
    return n;
}


Element* Pair::cdr(LispE* lisp) {
    long sz = liste.size();
    if (!sz)
        return null_;
    
    if (sz == 2)
        return liste.back();

    return new Pair(this, 1);
}

Element* InfiniterangeNumber::car(LispE* lisp) {
    return lisp->provideNumber(initial_value);
}

Element* InfiniterangeNumber::cdr(LispE* lisp) {
    return new InfiniterangeNumber(initial_value+increment, increment);
}

Element* InfiniterangeInteger::car(LispE* lisp) {
    return lisp->provideInteger(initial_value);
}

Element* InfiniterangeInteger::cdr(LispE* lisp) {
    return new InfiniterangeInteger(initial_value+increment, increment);
}
//------------------------------------------------------------------------------------------
