/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  math.cxx
//
//


/*
 Implantation des fonctions mathématiques
 */

#include "lispe.h"
#include "elements.h"
#include "tools.h"
#include "vecte.h"
#include <algorithm>

#ifdef WIN32
#define _USE_MATH_DEFINES
#endif

#include <math.h>

#ifdef UNIX
#define ABS(x) fabs((double)x)
#define TRUE true
#define FALSE false
#else
#define ABS(x) abs((double)x)
#endif

#define NMAX 100
#define TINY 1.5e-16
#define REAL double
#define ZERO (double)0.0
#define ONE (double)1.0
#define TWO (double)2.0

#ifdef INTELINTRINSICS
#ifdef WIN32
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

static const double M_GOLDEN = 1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475;
#define lmin(x,y) x<y?x:y
//------------------------------------------------------------------------------------------
Element* range(LispE* lisp, double init, double limit, double inc, bool int_eger) {
    double d = (limit - init) / inc;
    if (d<0)
        d *= -1;
    
    if (init > limit && inc > 0)
        inc *= -1;
    
    if (d <= 100000) {
        if (inc == 0)
            return emptylist_;
        
        //Integers ?
        if (int_eger) {
            Integers* range_list = lisp->provideIntegers();
            range_list->liste.reserve((long)d);
            if (inc > 0) {
                for (long i = init; i < limit; i += inc) {
                    range_list->liste.push_back(i);
                }
            }
            else {
                for (long i = init; i > limit; i += inc)
                    range_list->liste.push_back(i);
            }
            return range_list;
        }
        
        Numbers* range_list = lisp->provideNumbers();
        range_list->liste.reserve((long)d);
        if (inc > 0) {
            for (double i = init; i < limit; i += inc) {
                range_list->liste.push_back(i);
            }
        }
        else {
            for (double i = init; i > limit; i += inc)
                range_list->liste.push_back(i);
        }
        return range_list;
    }
    throw new Error("Error: Exceeding range");
}


Element* List::evall_range(LispE* lisp) {
    List_range_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_rangein(LispE* lisp) {
    List_rangein_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_irange(LispE* lisp) {
    List_irange_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_irangein(LispE* lisp) {
    List_irangein_eval m(this);
    return m.eval(lisp);
}



//------ Matrix operations ------------------------
union double32 {
public:
    
    uint32_t bits;
    float v;
    
    double32(float d) {
        v = d;
    }
};

union double64 {
public:
    
    uint64_t bits;
    double v;
    
    double64(double d) {
        v = d;
    }
};

//------------------------------------------------------------------------------------------

Matrice::Matrice(LispE* lisp, Matrice* m) {
    type = t_matrix;
    size_x = m->size_x;
    size_y = m->size_y;
    Numbers* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideNumbers();
        l->liste = ((Numbers*)m->liste[i])->liste;
        append(l);
    }
}

Matrice::Matrice(LispE* lisp, Matrice_float* m) {
    type = t_matrix;
    size_x = m->size_x;
    size_y = m->size_y;
    Numbers* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideNumbers();
        for (long j = 0; j < m->size_y; j++)
            l->liste.push_back(((Floats*)m->liste[i])->liste[j]);
        append(l);
    }
}

Matrice::Matrice(LispE* lisp, long x, long y, double n) {
    type = t_matrix;
    size_x = x;
    size_y = y;
    
    for (long i = 0; i < size_x; i++) {
        append(lisp->provideNumbers(size_y, n));
    }
}

void Matrice::build(LispE* lisp, Element* lst) {
    Numbers* l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = lisp->provideNumbers();
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            l->liste.push_back(lst->index(idx++)->asNumber());
        }
    }
}

void Matrice::concatenate(LispE* lisp, Element* e) {
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


Matrice_float::Matrice_float(LispE* lisp, Matrice* m) {
    type = t_matrix_float;
    size_x = m->size_x;
    size_y = m->size_y;
    Floats* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideFloats();
        for (long j = 0; j < m->size_y; j++)
            l->liste.push_back(((Numbers*)m->liste[i])->liste[j]);
        append(l);
    }
}

Matrice_float::Matrice_float(LispE* lisp, Matrice_float* m) {
    type = t_matrix_float;
    size_x = m->size_x;
    size_y = m->size_y;
    Floats* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideFloats();
        l->liste = ((Floats*)m->liste[i])->liste;
        append(l);
    }
}


Matrice_float::Matrice_float(LispE* lisp, long x, long y, float n) {
    type = t_matrix_float;
    size_x = x;
    size_y = y;
    
    for (long i = 0; i < size_x; i++) {
        append(lisp->provideFloats(size_y, n));
    }
}

void Matrice_float::concatenate(LispE* lisp, Element* e) {
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

void Matrice_float::build(LispE* lisp, Element* lst) {
    Floats* l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = lisp->provideFloats();
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            l->liste.push_back(lst->index(idx++)->asFloat());
        }
    }
}

void Tenseur::concatenate(LispE* lisp, Element* e) {
    if (e->isList()) {
        vecte<long> sz;
        e->getShape(sz);
        for (long i = 0; i < sz.size()-1; i++) {
            if (sz[i] != shape[i])
                throw new Error("Error: Incompatible dimensions");
        }
    }
    
    concatenate(lisp, 0, this, e);
}

void Tenseur::build(LispE* lisp, long isz, Element* res, double n) {
    if (isz == shape.size()-2) {
        Numbers* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideNumbers(shape[isz+1], n);
            res->append(lst);
        }
    }
    else {
        List* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideList();
            res->append(lst);
            build(lisp, isz+1, lst, n);
        }
    }
}

void Tenseur::build(LispE* lisp, long isz, Element* res, Element* lst, long& idx) {
    if (isz == shape.size()-2) {
        Numbers* l;
        long i,j;
        for (i = 0; i < shape[isz]; i++) {
            l = lisp->provideNumbers();
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
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1, l, lst, idx);
        }
    }
}

void Tenseur::build(LispE* lisp, long isz, Element* res) {
    if (isz == shape.size()-2) {
        Numbers* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideNumbers();
            res->append(l);
            l->liste = ((Numbers*)liste[i])->liste;
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1,l);
        }
    }
}

void Tenseur_float::concatenate(LispE* lisp, Element* e) {
    if (e->isList()) {
        vecte<long> sz;
        e->getShape(sz);
        for (long i = 0; i < sz.size()-1; i++) {
            if (sz[i] != shape[i])
                throw new Error("Error: Incompatible dimensions");
        }
    }
    
    concatenate(lisp, 0, this, e);
}


void Tenseur_float::build(LispE* lisp, long isz, Element* res, float n) {
    if (isz == shape.size()-2) {
        Floats* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideFloats(shape[isz+1], n);
            res->append(lst);
        }
    }
    else {
        List* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideList();
            res->append(lst);
            build(lisp, isz+1, lst, n);
        }
    }
}

void Tenseur_float::build(LispE* lisp, long isz, Element* res, Element* lst, long& idx) {
    if (isz == shape.size()-2) {
        Floats* l;
        long i,j;
        for (i = 0; i < shape[isz]; i++) {
            l = lisp->provideFloats();
            res->append(l);
            for (j = 0; j < shape[isz+1]; j++) {
                if (idx == lst->size())
                    idx = 0;
                l->liste.push_back(lst->index(idx++)->asFloat());
            }
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1, l, lst, idx);
        }
    }
}

void Tenseur_float::build(LispE* lisp, long isz, Element* res) {
    if (isz == shape.size()-2) {
        Floats* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideFloats();
            res->append(l);
            l->liste = ((Floats*)liste[i])->liste;
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1,l);
        }
    }
}

void List::build(LispE* lisp, vecte<long>& shape, long isz, Element* res, Element* lst, long& idx) {
    long i;
    if (isz == shape.size() - 2) {
        List* l;
        long j;
        for (i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            for (j = 0; j < shape[isz+1]; j++) {
                if (idx == lst->size())
                    idx = 0;
                l->append(lst->index(idx++)->copying(false));
            }
        }
    }
    else {
        List* l;
        for (i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, shape, isz+1, l, lst, idx);
        }
    }
}

void LList::build(LispE* lisp, vecte<long>& shape, long isz, LList* res, LList* lst, u_link** idx) {
    long i;
    if (isz == shape.size() - 2) {
        LList* l;
        long j;
        for (i = 0; i < shape[isz]; i++) {
            l = new LList(liste.mark);
            res->push_front(l);
            for (j = 0; j < shape[isz+1]; j++) {
                if (*idx == NULL)
                    *idx = lst->liste.begin();
                l->push_front((*idx)->value->copying(false));
                *idx = (*idx)->next();
            }
        }
    }
    else {
        LList* l;
        for (i = 0; i < shape[isz]; i++) {
            l = new LList(liste.mark);
            build(lisp, shape, isz+1, l, lst, idx);
            res->push_front(l);
        }
    }
}
//---------------------------------------------------------------------------------------------------
void List::combine(LispE* lisp, vecte<long>& isz1, vecte<long>& isz2, Element* l1, Element* l2, List* action) {
    if (!l1->isList() && !l2->isList()) {
        if (isz1.size() && isz2.size()) {
            action->in_quote(1, l1);
            action->in_quote(2, l2);
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
        }
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

void List::combine(LispE* lisp, Element* l1, Element* l2, List* action) {
    vecte<long> isz1;
    vecte<long> isz2;
    combine(lisp, isz1, isz2, l1, l2, action);
}
//------------------------------------------------------------------------------------------

//LU decomposition
long LUDCMP(long n, vecte<long>& indexes, long& d, Matrice* m) {
    d = 1;
    double AMAX, DUM, thesum;
    long i, i_max = 0, j, k;
    vector<double> values;
    
    for (i = 0; i < n; i++)  {
        AMAX = 0.0;
        for (j = 0; j<n; j++)  {
            thesum = m->val(i, j);
            if (ABS(thesum) > AMAX)
                AMAX = ABS(thesum);
        }
        
        if (AMAX < TINY)
            return 1;
        values.push_back(1.0 / AMAX);
    } // i loop
    
    for (j = 0; j < n; j++)  {
        
        for (i = 0; i < j; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < i; k++)
                thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
        } // i loop
        AMAX = 0.0;
        
        for (i = j; i < n; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < j; k++)
                thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
            DUM = values[i] * ABS(thesum);
            if (DUM >= AMAX) {
                i_max = i;
                AMAX = DUM;
            }
        } // i loop
        
        if (j != i_max)  {
            for (k = 0; k < n; k++)  {
                DUM = m->val(i_max, k);
                m->set(i_max,k,m->val(j, k));
                m->set(j,k,DUM);
            } // k loop
            d = -d;
            values[i_max] = values[j];
        }
        
        indexes.at(j, i_max);
        
        if (ABS(m->val(j, j)) < TINY)
            m->set(j,j,TINY);
        
        if (j != n - 1)  {
            DUM = 1.0 / m->val(j, j);
            for (i = j + 1; i < n; i++) {
                m->mult(i,j, DUM);
            }
        }
    } // j loop
    
    return 0;
    
} // subroutine LUDCMP

void LUBKSB(long n, vecte<long>& indexes, vecte<double>& b_values, Matrice* m)  {
    double thesum;
    long  i, ii, j, ll;
    
    ii = -1;
    
    for (i = 0; i < n; i++)  {
        ll = indexes[i];
        thesum = b_values[ll];
        b_values.at(ll, b_values[i]);
        if (ii != -1) {
            for (j = ii; j < i; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        else {
            if (thesum != 0.0)
                ii = i;
        }
        b_values.at(i, thesum);
    } // i loop
    
    for (i = n - 1; i >= 0; i--)  {
        thesum = b_values[i];
        if (i < n - 1)  {
            for (j = i + 1; j < n; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        b_values.at(i, thesum / m->val(i, i));
    } // i loop
    
} // LUBKSB

double Matrice::determinant(LispE* lisp) {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }
    
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'determinant' to square matrices");
    
    long i;
    i = 0;
    double det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;
        
        Matrice sub(size_x - 1, size_y - 1, 0.0);
        
        long pc = 0;
        long pr = 0;
        for (long r = 0; r < size_x; r++) {
            if (r == i)
                continue;
            pc = 0;
            for (long c = 0; c < size_y; c++) {
                if (c == j)
                    continue;
                sub.set(pr,pc, val(r,c));
                pc++;
            }
            pr++;
        }
        double sg = pow(-1, (i + j + 2));
        det += val(i,j) * sg*sub.determinant(lisp);
    }
    return det;
}

Element* Matrice::inversion(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'invert' to square matrices");
    
    //else Local decomposition
    Matrice m(this);
    
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice* Y = new Matrice(lisp, size_x, size_x, 0.0);
    
    long i;
    //We create an identity matrix, which will contain the final result...
    for (i = 0; i < size_x; i++) {
        Y->set(i,i, 1);
    }
    
    vecte<double> temp(size_x);
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice::solve(LispE* lisp, Matrice* y) {
    if (size_x != size_y || y->size_x != y->size_y || size_x != y->size_x)
        throw new Error("Error: we can only apply 'solve' to square matrices of equal sizes");
    
    //else Local decomposition
    Matrice m(this);
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice* Y = new Matrice(lisp, y);
    vecte<double> temp(size_x);
    long i;
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice::ludcmp(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'ludcmp' to square matrices");
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, this);
    if (rc == 1) {
        return emptylist_;
    }
    Integers* lst = lisp->provideIntegers();
    lst->liste = indexes;
    return lst;
}

Element* Matrice::lubksb(LispE* lisp, Integers* idxs, Matrice* Y) {
    if (size_x != size_y || idxs->size() != size_x) {
        throw new Error("Error: we can only apply 'lubksb' to square matrices with the same number of indexes");
    }
    
    long i;
    if (Y == NULL) {
        Y = new Matrice(lisp, size_x, size_x, 0.0);
        //We create an identity matrix, which will contain the final result...
        for (i = 0; i < size_x; i++) {
            Y->set(i,i, 1);
        }
    }
    else {
        if (Y->size_x != size_x)
            throw new Error("Error: we can only apply 'lubksb' to square matrices of the same shape");
    }
    
    vecte<long> indexes(size_x);
    for (i = 0; i < size_x; i++) {
        indexes.push_back(idxs->liste[i]);
    }
    
    vecte<double> temp(size_x);
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, this);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

long LUDCMP(long n, vecte<long>& indexes, long& d, Matrice_float* m) {
    d = 1;
    float AMAX, DUM, thesum;
    long i, i_max = 0, j, k;
    vector<float> values;
    
    for (i = 0; i < n; i++)  {
        AMAX = 0.0;
        for (j = 0; j<n; j++)  {
            thesum = m->val(i, j);
            if (ABS(thesum) > AMAX)
                AMAX = ABS(thesum);
        }
        
        if (AMAX < TINY)
            return 1;
        values.push_back(1.0 / AMAX);
    } // i loop
    
    for (j = 0; j < n; j++)  {
        
        for (i = 0; i < j; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < i; k++)
                thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
        } // i loop
        AMAX = 0.0;
        
        for (i = j; i < n; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < j; k++)
                thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
            DUM = values[i] * ABS(thesum);
            if (DUM >= AMAX) {
                i_max = i;
                AMAX = DUM;
            }
        } // i loop
        
        if (j != i_max)  {
            for (k = 0; k < n; k++)  {
                DUM = m->val(i_max, k);
                m->set(i_max,k,m->val(j, k));
                m->set(j,k,DUM);
            } // k loop
            d = -d;
            values[i_max] = values[j];
        }
        
        indexes.at(j, i_max);
        
        if (ABS(m->val(j, j)) < TINY)
            m->set(j,j,TINY);
        
        if (j != n - 1)  {
            DUM = 1.0 / m->val(j, j);
            for (i = j + 1; i < n; i++) {
                m->mult(i,j, DUM);
            }
        }
    } // j loop
    
    return 0;
    
} // subroutine LUDCMP

void LUBKSB(long n, vecte<long>& indexes, vecte<float>& b_values, Matrice_float* m)  {
    float thesum;
    long  i, ii, j, ll;
    
    ii = -1;
    
    for (i = 0; i < n; i++)  {
        ll = indexes[i];
        thesum = b_values[ll];
        b_values.at(ll, b_values[i]);
        if (ii != -1) {
            for (j = ii; j < i; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        else {
            if (thesum != 0.0)
                ii = i;
        }
        b_values.at(i, thesum);
    } // i loop
    
    for (i = n - 1; i >= 0; i--)  {
        thesum = b_values[i];
        if (i < n - 1)  {
            for (j = i + 1; j < n; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        b_values.at(i, thesum / m->val(i, i));
    } // i loop
    
} // LUBKSB

float Matrice_float::determinant(LispE* lisp) {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }
    
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'determinant' to square matrices");
    
    long i;
    i = 0;
    float det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;
        
        Matrice_float sub(size_x - 1, size_y - 1, 0.0);
        
        long pc = 0;
        long pr = 0;
        for (long r = 0; r < size_x; r++) {
            if (r == i)
                continue;
            pc = 0;
            for (long c = 0; c < size_y; c++) {
                if (c == j)
                    continue;
                sub.set(pr,pc, val(r,c));
                pc++;
            }
            pr++;
        }
        float sg = pow(-1, (i + j + 2));
        det += val(i,j) * sg*sub.determinant(lisp);
    }
    return det;
}

Element* Matrice_float::inversion(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'invert' to square matrices");
    
    //else Local decomposition
    Matrice_float m(this);
    
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice_float* Y = new Matrice_float(lisp, size_x, size_x, 0.0);
    
    long i;
    //We create an identity matrix, which will contain the final result...
    for (i = 0; i < size_x; i++) {
        Y->set(i,i, 1);
    }
    
    vecte<float> temp(size_x);
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice_float::solve(LispE* lisp, Matrice_float* y) {
    if (size_x != size_y || y->size_x != y->size_y || size_x != y->size_x)
        throw new Error("Error: we can only apply 'solve' to square matrices of equal sizes");
    
    //else Local decomposition
    Matrice_float m(this);
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice_float* Y = new Matrice_float(lisp, y);
    vecte<float> temp(size_x);
    long i;
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice_float::ludcmp(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'ludcmp' to square matrices");
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, this);
    if (rc == 1) {
        return emptylist_;
    }
    Integers* lst = lisp->provideIntegers();
    lst->liste = indexes;
    return lst;
}

Element* Matrice_float::lubksb(LispE* lisp, Integers* idxs, Matrice_float* Y) {
    if (size_x != size_y || idxs->size() != size_x) {
        throw new Error("Error: we can only apply 'lubksb' to square matrices with the same number of indexes");
    }
    
    long i;
    if (Y == NULL) {
        Y = new Matrice_float(lisp, size_x, size_x, 0.0);
        //We create an identity matrix, which will contain the final result...
        for (i = 0; i < size_x; i++) {
            Y->set(i,i, 1);
        }
    }
    else {
        if (Y->size_x != size_x)
            throw new Error("Error: we can only apply 'lubksb' to square matrices of the same shape");
    }
    
    vecte<long> indexes(size_x);
    for (i = 0; i < size_x; i++) {
        indexes.push_back(idxs->liste[i]);
    }
    
    vecte<float> temp(size_x);
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, this);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}


//------------------------------------------------------------------------------------------
float Element::checkFloat(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
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

int16_t Element::checkShort(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}
//------------------------------------------------------------------------------------------
Element* Float::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content += ((Float*)e)->content;
            return this;
        case t_number:
            content += ((Number*)e)->content;
            return this;
        case t_short:
            content += ((Short*)e)->content;
            return this;
        case t_integer:
            content += ((Integer*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content += content;
            release();
            return c;
        }
        default:
            return plus(lisp, e);
    }
}

Element* Float::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content -= ((Float*)e)->content;
            return this;
        case t_number:
            content -= ((Number*)e)->content;
            return this;
        case t_integer:
            content -= ((Integer*)e)->content;
            return this;
        case t_short:
            content -= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = (double)content - c->content;
            release();
            return c;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Float::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content *= ((Float*)e)->content;
            return this;
        case t_number:
            content *= ((Number*)e)->content;
            return this;
        case t_integer:
            content *= ((Integer*)e)->content;
            return this;
        case t_short:
            content *= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content *= content;
            release();
            return c;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Float::divide_direct(LispE* lisp, Element* e) {
    if (e->isEmpty())
        throw new Error("Error: division by zero");
    
    switch (e->type) {
        case t_float: {
            double v = ((Float*)e)->content;
            content /= v;
            return this;
        }
        case t_number: {
            double v = ((Number*)e)->content;
            content /= v;
            return this;
        }
        case t_integer: {
            double v = ((Integer*)e)->content;
            content /= v;
            return this;
        }
        case t_short: {
            float v = ((Short*)e)->content;
            content /= v;
            return this;
        }
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = (double)content / c->content;
            release();
            return c;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Float::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content += e->checkFloat(lisp);
        return this;
    }
    return lisp->provideFloat(content+e->checkFloat(lisp));
}

Element* Float::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        content -= e->checkFloat(lisp);
        return this;
    }
    return lisp->provideFloat(content-e->checkFloat(lisp));
}

Element* Float::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content *= e->checkFloat(lisp);
        return this;
    }
    return lisp->provideFloat(content*e->checkFloat(lisp));
}

Element* Float::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    float v = e->checkFloat(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        content /= v;
        return this;
    }
    return lisp->provideFloat(content/v);
}

Element* Float::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }
    
    long v = e->checkInteger(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    
    if (status != s_constant) {
        content = (long)content % v;
        return this;
    }
    return lisp->provideFloat((long)content%v);
}

Element* Float::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->power(lisp, e);
    }
    if (status != s_constant) {
        content = pow(content, e->checkFloat(lisp));
        return this;
    }
    return lisp->provideFloat(pow(content, e->checkFloat(lisp)));
}

Element* Float::bit_not(LispE* lisp)  {
    double32 d(content);
    d.bits = ~d.bits;
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    release();
    return lisp->provideFloat(d.v);
}


Element* Float::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    double32 d(content);
    d.bits &= ~e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideFloat(d.v);
}

Element* Float::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    double32 d(content);
    d.bits &= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideFloat(d.v);
}


Element* Float::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    double32 d(content);
    d.bits |= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideFloat(d.v);
}

Element* Float::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    double32 d(content);
    d.bits ^= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideFloat(d.v);
}

Element* Float::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    double32 d(content);
    d.bits <<= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideFloat(d.v);
}

Element* Float::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    double32 d(content);
    d.bits >>= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideFloat(d.v);
}

Element* Number::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content += ((Float*)e)->content;
            return this;
        case t_number:
            content += ((Number*)e)->content;
            return this;
        case t_integer:
            content += ((Integer*)e)->content;
            return this;
        case t_short:
            content += ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content += content;
            release();
            return c;
        }
        default:
            return plus(lisp, e);
    }
}

Element* Number::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content -= ((Float*)e)->content;
            return this;
        case t_number:
            content -= ((Number*)e)->content;
            return this;
        case t_integer:
            content -= ((Integer*)e)->content;
            return this;
        case t_short:
            content -= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = content - c->content;
            release();
            return c;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Number::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content *= ((Float*)e)->content;
            return this;
        case t_number:
            content *= ((Number*)e)->content;
            return this;
        case t_integer:
            content *= ((Integer*)e)->content;
            return this;
        case t_short:
            content *= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content *= content;
            release();
            return c;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Number::divide_direct(LispE* lisp, Element* e) {
    if (e->isEmpty())
        throw new Error("Error: division by zero");
    
    switch (e->type) {
        case t_float: {
            double v = ((Float*)e)->content;
            content /= v;
            return this;
        }
        case t_number: {
            double v = ((Number*)e)->content;
            content /= v;
            return this;
        }
        case t_integer: {
            double v = ((Integer*)e)->content;
            content /= v;
            return this;
        }
        case t_short: {
            double v = ((Short*)e)->content;
            content /= v;
            return this;
        }
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = content / c->content;
            release();
            return c;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Number::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content += e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(content+e->checkNumber(lisp));
}

Element* Number::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        content -= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(content-e->checkNumber(lisp));
}

Element* Number::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content *= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(content*e->checkNumber(lisp));
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
        content /= v;
        return this;
    }
    return lisp->provideNumber(content/v);
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
        content = (long)content % v;
        return this;
    }
    return lisp->provideNumber((long)content%v);
}

Element* Number::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->power(lisp, e);
    }
    if (status != s_constant) {
        content = pow(content, e->checkNumber(lisp));
        return this;
    }
    return lisp->provideNumber(pow(content, e->checkNumber(lisp)));
}

Element* Number::bit_not(LispE* lisp)  {
    double64 d(content);
    d.bits = ~d.bits;
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    release();
    return lisp->provideNumber(d.v);
}


Element* Number::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    double64 d(content);
    d.bits &= ~e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideNumber(d.v);
}

Element* Number::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    double64 d(content);
    d.bits &= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideNumber(d.v);
}


Element* Number::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    double64 d(content);
    d.bits |= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideNumber(d.v);
}

Element* Number::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    double64 d(content);
    d.bits ^= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
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
    double64 d(content);
    d.bits <<= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
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
    double64 d(content);
    d.bits >>= e->checkInteger(lisp);
    if (status != s_constant) {
        content = d.v;
        return this;
    }
    
    return lisp->provideNumber(d.v);
}

Element* Integer::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content += ((Float*)e)->content;
            return this;
        case t_number:
            content += ((Number*)e)->content;
            return this;
        case t_short:
            content += ((Short*)e)->content;
            return this;
        case t_integer:
            content += ((Integer*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content += content;
            release();
            return c;
        }
        default:
            return plus(lisp, e);
    }
}

Element* Integer::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content -= ((Float*)e)->content;
            return this;
        case t_number:
            content -= ((Number*)e)->content;
            return this;
        case t_integer:
            content -= ((Integer*)e)->content;
            return this;
        case t_short:
            content -= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = (double)content - c->content;
            release();
            return c;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Integer::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content *= ((Float*)e)->content;
            return this;
        case t_number:
            content *= ((Number*)e)->content;
            return this;
        case t_integer:
            content *= ((Integer*)e)->content;
            return this;
        case t_short:
            content *= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content *= content;
            release();
            return c;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Integer::divide_direct(LispE* lisp, Element* e) {
    if (e->isEmpty())
        throw new Error("Error: division by zero");
    
    switch (e->type) {
        case t_float: {
            float d = content;
            d /= ((Float*)e)->content;
            release();
            return new Float(d);
        }
        case t_number: {
            double d = content;
            d /= ((Number*)e)->content;
            release();
            return lisp->provideNumber(d);
        }
        case t_integer: {
            double d = content;
            d /= e->asNumber();
            release();
            return lisp->provideNumber(d);
        }
        case t_short: {
            double d = content;
            d /= e->asFloat();
            release();
            return lisp->provideNumber(d);
        }
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = (double)content / c->content;
            release();
            return c;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Integer::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content += e->checkInteger(lisp);
        return this;
    }
    
    return lisp->provideInteger(content+e->checkInteger(lisp));
}

Element* Integer::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        content -= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content-e->checkInteger(lisp));
}

Element* Integer::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content *= e->asInteger();
        return this;
    }
    return lisp->provideInteger(content*e->asInteger());
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
    return lisp->provideNumber((double)content/v);
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
        content %= v;
        return this;
    }
    return lisp->provideInteger(content%v);
}

Element* Integer::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Numbers* n = new Numbers(e->size(), content);
        return n->power(lisp, e);
    }
    double v = pow((double)content, e->checkNumber(lisp));
    release();
    return lisp->provideNumber(v);
}

Element* Integer::bit_not(LispE* lisp)  {
    if (status != s_constant) {
        content = ~content;
        return this;
    }
    release();
    return lisp->provideInteger(~content);
}


Element* Integer::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content &= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content&e->checkInteger(lisp));
}

Element* Integer::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    if (status != s_constant) {
        content &= ~e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content&~e->checkInteger(lisp));
}

Element* Integer::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content |= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content|e->checkInteger(lisp));
}

Element* Integer::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content ^= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content^e->checkInteger(lisp));
}


Element* Integer::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    if (status != s_constant) {
        content <<= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content<<e->checkInteger(lisp));
}


Element* Integer::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    if (status != s_constant) {
        content >>= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(content>>e->checkInteger(lisp));
}

Element* Short::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content += ((Float*)e)->content;
            return this;
        case t_number:
            content += ((Number*)e)->content;
            return this;
        case t_short:
            content += ((Short*)e)->content;
            return this;
        case t_integer:
            content += ((Integer*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content += content;
            release();
            return c;
        }
        default:
            return plus(lisp, e);
    }
}

Element* Short::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content -= ((Float*)e)->content;
            return this;
        case t_number:
            content -= ((Number*)e)->content;
            return this;
        case t_integer:
            content -= ((Integer*)e)->content;
            return this;
        case t_short:
            content -= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = (double)content - c->content;
            release();
            return c;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Short::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content *= ((Float*)e)->content;
            return this;
        case t_number:
            content *= ((Number*)e)->content;
            return this;
        case t_integer:
            content *= ((Integer*)e)->content;
            return this;
        case t_short:
            content *= ((Short*)e)->content;
            return this;
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content *= content;
            release();
            return c;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Short::divide_direct(LispE* lisp, Element* e) {
    if (e->isEmpty())
        throw new Error("Error: division by zero");
    
    switch (e->type) {
        case t_float: {
            float d = content;
            d /= ((Float*)e)->content;
            release();
            return new Float(d);
        }
        case t_number: {
            double d = content;
            d /= ((Number*)e)->content;
            release();
            return lisp->provideNumber(d);
        }
        case t_integer: {
            double d = content;
            d /= e->asNumber();
            release();
            return lisp->provideNumber(d);
        }
        case t_short: {
            double d = content;
            d /= e->asFloat();
            release();
            return lisp->provideNumber(d);
        }
        case t_complex: {
            Complex* c = lisp->provideComplex(((Complex*)e)->content);
            c->content = (double)content / c->content;
            release();
            return c;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Short::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content += e->checkShort(lisp);
        return this;
    }
    
    return new Short(content+e->checkShort(lisp));
}

Element* Short::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        content -= e->checkShort(lisp);
        return this;
    }
    return new Short(content-e->checkShort(lisp));
}

Element* Short::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content *= e->asInteger();
        return this;
    }
    return new Short(content*e->asInteger());
}


Element* Short::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    double v =  e->checkNumber(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    double vv = (double)content;
    release();
    return lisp->provideNumber(vv/v);
}


Element* Short::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }
    long v =  e->checkShort(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    
    if (status != s_constant) {
        content %= v;
        return this;
    }
    return new Short(content%v);
}

Element* Short::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Numbers* n = new Numbers(e->size(), content);
        return n->power(lisp, e);
    }
    double v = pow((double)content, e->checkNumber(lisp));
    release();
    return lisp->provideNumber(v);
}

Element* Short::bit_not(LispE* lisp)  {
    if (status != s_constant) {
        content = ~content;
        return this;
    }
    release();
    return new Short(~content);
}


Element* Short::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content &= e->checkShort(lisp);
        return this;
    }
    return new Short(content&e->checkShort(lisp));
}

Element* Short::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    if (status != s_constant) {
        content &= ~e->checkShort(lisp);
        return this;
    }
    return new Short(content&~e->checkShort(lisp));
}

Element* Short::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content |= e->checkShort(lisp);
        return this;
    }
    return new Short(content|e->checkShort(lisp));
}

Element* Short::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content ^= e->checkShort(lisp);
        return this;
    }
    return new Short(content^e->checkShort(lisp));
}


Element* Short::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    if (status != s_constant) {
        content <<= e->checkShort(lisp);
        return this;
    }
    return new Short(content<<e->checkShort(lisp));
}


Element* Short::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    if (status != s_constant) {
        content >>= e->checkShort(lisp);
        return this;
    }
    return new Short(content>>e->checkShort(lisp));
}

Element* Complex::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content += e->asNumber();
        return this;
    }
    Complex* c = lisp->provideComplex(content);
    c->content += e->asNumber();
    return c;
}

Element* Complex::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        content -= e->asNumber();
        return this;
    }
    Complex* c = lisp->provideComplex(content);
    c->content -= e->asNumber();
    return c;
}

Element* Complex::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(lisp, 1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        content *= e->asNumber();
        return this;
    }
    Complex* c = lisp->provideComplex(content);
    c->content *= e->asNumber();
    return c;
}

Element* Complex::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content += ((Float*)e)->content;
            return this;
        case t_number:
            content += ((Number*)e)->content;
            return this;
        case t_short:
            content += ((Short*)e)->content;
            return this;
        case t_integer:
            content += ((Integer*)e)->content;
            return this;
        case t_complex: {
            content += ((Complex*)e)->content;
            return this;
        }
        default:
            return plus(lisp, e);
    }
}

Element* Complex::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content -= ((Float*)e)->content;
            return this;
        case t_number:
            content -= ((Number*)e)->content;
            return this;
        case t_integer:
            content -= ((Integer*)e)->content;
            return this;
        case t_short:
            content -= ((Short*)e)->content;
            return this;
        case t_complex: {
            content -= ((Complex*)e)->content;
            return this;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Complex::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            content *= ((Float*)e)->content;
            return this;
        case t_number:
            content *= ((Number*)e)->content;
            return this;
        case t_integer:
            content *= ((Integer*)e)->content;
            return this;
        case t_short:
            content *= ((Short*)e)->content;
            return this;
        case t_complex: {
            content *= ((Complex*)e)->content;
            return this;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Complex::divide_direct(LispE* lisp, Element* e) {
    if (e->isEmpty())
        throw new Error("Error: division by zero");
    
    switch (e->type) {
        case t_number: {
            double v = ((Number*)e)->content;
            content /= v;
            return this;
        }
        case t_float:
        case t_integer:
        case t_short: {
            content /= e->asNumber();
            return this;
        }
        case t_complex: {
            content /= ((Complex*)e)->content;
            return this;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Complex::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    
    double v = e->asNumber();
    if (!v)
        throw new Error("Error: division by zero");
    
    if (status != s_constant) {
        content /= v;
        return this;
    }
    Complex* c = lisp->provideComplex(content);
    c->content /= v;
    return c;
}

Element* Complex::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        return n->power(lisp, e);
    }
    
    if (e->type == t_complex) {
        std::complex<double>& v = ((Complex*)e)->content;
        if (status != s_constant) {
            content = pow(content, v);
            return this;
        }
        
        Complex* c = lisp->provideComplex(0,1);
        c->content = pow(content, v);
        return c;
    }
    
    if (status != s_constant) {
        content = pow(content, e->asNumber());
        return this;
    }
    
    Complex* c = lisp->provideComplex(0,1);
    c->content = pow(content, e->asNumber());
    return c;
}

Element* LList::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
            it->value = it->value->bit_not(l);
        }
        return this;
    }
    LList* lst = new LList(liste.mark);
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        lst->append(it->value->bit_not(l));
    }
    release();
    return lst;
}


Element* LList::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->bit_and(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->bit_and(lisp, e);
    }
    return this;
}

Element* LList::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->bit_and_not(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->bit_and_not(lisp, e);
    }
    return this;
}


Element* LList::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->bit_or(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->bit_or(lisp, e);
    }
    return this;
}

Element* LList::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->bit_xor(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->bit_xor(lisp, e);
    }
    return this;
}

Element* LList::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->plus(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->plus(lisp, e);
    }
    return this;
}

Element* LList::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->minus(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->minus(lisp, e);
    }
    return this;
}

Element* LList::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->multiply(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->multiply(lisp, e);
    }
    return this;
}

Element* LList::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->divide(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->divide(lisp, e);
    }
    return this;
}

Element* LList::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->mod(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->mod(lisp, e);
    }
    return this;
}

Element* LList::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->power(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->power(lisp, e);
    }
    return this;
}

Element* LList::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->leftshift(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->leftshift(lisp, e);
    }
    return this;
}

Element* LList::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        long i = 0;
        for (u_link* it = liste.begin(); i < e->size() && it != NULL; it = it->next(), i++) {
            it->value = it->value->rightshift(lisp, e->index(i));
        }
    }
    for (u_link* it = liste.begin(); it != NULL; it = it->next()) {
        it->value =  it->value->rightshift(lisp, e);
    }
    return this;
}

Element* List::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, liste[i]->bit_not(l));
        }
        return this;
    }
    List* lst = l->provideList();
    for (long i = 0; i < size(); i++) {
        lst->append(liste[i]->bit_not(l));
    }
    release();
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

Element* Floats::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, index(i)->bit_not(l));
        }
        return this;
    }
    double32 d(0);
    Floats* num = l->provideFloats();
    for (long i = 0; i < size(); i++) {
        d.v = liste[i];
        d.bits = ~d.bits;
        num->liste.push_back(d.v);
    }
    release();
    return num;
}


Element* Floats::bit_and(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= r.bits;
        }
        return lisp->provideFloat(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::bit_and_not(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= ~r.bits;
        }
        return lisp->provideFloat(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::bit_or(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits |= r.bits;
        }
        return lisp->provideFloat(d.v);
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::bit_xor(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits ^= r.bits;
        }
        return lisp->provideFloat(d.v);
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 24) {
                long nb = (szl>>3)<<3;
                for (i = 0; i < nb; i += 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_add_ps(_mm256_loadu_ps(&liste[i]), _mm256_loadu_ps(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] += n->liste[nb];
                return this;
            }
#endif
            liste.plus(n->liste, szl);
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Integers*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Shorts*)e)->liste[i];
            }
            return this;
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Numbers*)e)->liste[i];
            }
            return this;
        case t_float:
        case t_short:
        case t_number:
        case t_integer: {
            float v = e->asFloat();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 24) {
                long i;
                __m256 vb = {v, v, v, v, v, v, v, v};
                long nb = (szl>>3)<<3;
                for (i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_add_ps(_mm256_loadu_ps(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] += v;
                return this;
            }
#endif
            liste.plus(v);
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Floats::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 24) {
                long nb = (szl>>3)<<3;
                for (i = 0; i < nb; i += 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_sub_ps(_mm256_loadu_ps(&liste[i]), _mm256_loadu_ps(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] -= n->liste[nb];
                return this;
            }
#endif
            liste.minus(n->liste, szl);
            return this;
        }
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Numbers*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            float v = e->asFloat();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 24) {
                long i;
                __m256 vb = {v, v, v, v, v, v, v, v};
                long nb = (szl>>3)<<3;
                for (i = 0; i < nb; i += 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_sub_ps(_mm256_loadu_ps(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] -= v;
                return this;
            }
#endif
            liste.minus(v);
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] - n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] - n->liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Floats::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 24) {
                long nb = (szl>>3)<<3;
                for (i = 0; i < nb; i += 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_mul_ps(_mm256_loadu_ps(&liste[i]), _mm256_loadu_ps(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] *= n->liste[nb];
                return this;
            }
#endif
            
            liste.multiply(n->liste, szl);
            return this;
        }
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Numbers*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            float v = e->asFloat();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 24) {
                __m256 vb = {v, v, v, v, v, v, v, v};
                long nb = (szl>>3)<<3;
                for (long i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_mul_ps(_mm256_loadu_ps(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] *= v;
                return this;
            }
#endif
            liste.multiply(v);
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Floats::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            if (n->liste.check(0))
                throw new Error("Error: division by zero");
            
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 24) {
                long nb = (szl>>3)<<3;
                for (i = 0; i < nb; i += 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_div_ps(_mm256_loadu_ps(&liste[i]), _mm256_loadu_ps(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] /= n->liste[nb];
                return this;
            }
#endif
            for (i = 0; i < szl; i++)
                liste[i] /= n->liste[i];
            return this;
        }
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Numbers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Numbers*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Shorts*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            float v = e->asFloat();
            if (!v)
                throw new Error("Error: division by zero");
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 24) {
                __m256 vb = {v, v, v, v, v, v, v, v};
                long nb = (szl>>3)<<3;
                for (long i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste[i], _mm256_div_ps(_mm256_loadu_ps(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] /= v;
                return this;
            }
#endif
            liste.divide(v);
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    if (n->liste.check(0)) {
                        result->release();
                        throw new Error("Error: division by zero");
                    }
                    n->liste[i] = liste[i] / n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                if (n->liste.check(0)) {
                    result->release();
                    throw new Error("Error: division by zero");
                }
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] / n->liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}


Element* Floats::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        return lisp->provideFloat(liste.sum());
    }
    
    if (e->isList()) {
        if (e->type == type) {
            liste.plus(((Floats*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asFloat();
        }
        return this;
    }
    liste.plus(e->asFloat());
    return this;
}

Element* Floats::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste[i];
        }
        return lisp->provideFloat(d);
    }
    
    if (e->isList()) {
        if (e->type == type) {
            liste.minus(((Floats*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] -= e->index(i)->asFloat();
        }
        return this;
    }
    liste.minus(e->asFloat());
    return this;
}

Element* Floats::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        return lisp->provideFloat(liste.product());
    }
    
    if (e->isList()) {
        if (e->type == type) {
            liste.multiply(((Floats*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] *= e->index(i)->asFloat();
        }
        return this;
    }
    liste.multiply(e->asFloat());
    return this;
}

Element* Floats::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d /= liste[i];
        }
        return lisp->provideFloat(d);
    }
    
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    float d = e->asFloat();
    if (d == 0)
        throw new Error("Error: division by zero");
    liste.divide(d);
    return this;
}

Element* Floats::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (liste[i] == 0)
                throw new Error("Error: division by zero");
            d %= (long)liste[i];
        }
        return lisp->provideFloat(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d,liste[i]);
        }
        return lisp->provideFloat(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits <<= (long)liste[i];
        }
        return lisp->provideFloat(d.v);
    }
    
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Floats::rightshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits >>= (long)liste[i];
        }
        return lisp->provideFloat(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Numbers::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, index(i)->bit_not(l));
        }
        return this;
    }
    double64 d(0);
    Numbers* num = l->provideNumbers();
    for (long i = 0; i < size(); i++) {
        d.v = liste[i];
        d.bits = ~d.bits;
        num->liste.push_back(d.v);
    }
    release();
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Numbers::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 20) {
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_add_pd(_mm256_loadu_pd(&liste[i]), _mm256_loadu_pd(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] += n->liste[nb];
                return this;
            }
#endif
            liste.plus(n->liste, szl);
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Floats*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Integers*)e)->liste[i];
            }
            return this;
        case t_float:
        case t_number:
        case t_short:
        case t_integer: {
            double v = e->asNumber();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 20) {
                __m256d vb = {v, v, v, v};
                long nb = (szl>>2)<<2;
                for (long i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_add_pd(_mm256_loadu_pd(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] += v;
                return this;
            }
#endif
            liste.plus(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Numbers::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 20) {
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_sub_pd(_mm256_loadu_pd(&liste[i]), _mm256_loadu_pd(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] -= n->liste[nb];
                return this;
            }
#endif
            liste.minus(n->liste, szl);
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Floats*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            double v = e->asNumber();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 20) {
                __m256d vb = {v, v, v, v};
                long nb = (szl>>2)<<2;
                for (long i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_sub_pd(_mm256_loadu_pd(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] -= v;
                return this;
            }
#endif
            liste.minus(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] - n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] - n->liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Numbers::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 20) {
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_mul_pd(_mm256_loadu_pd(&liste[i]), _mm256_loadu_pd(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] *= n->liste[nb];
                return this;
            }
#endif
            
            liste.multiply(n->liste, szl);
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Floats*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            double v = e->asNumber();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 20) {
                __m256d vb = {v, v, v, v};
                long nb = (szl>>2)<<2;
                for (long i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_mul_pd(_mm256_loadu_pd(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] *= v;
                return this;
            }
#endif
            liste.multiply(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Numbers::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            if (n->liste.check(0))
                throw new Error("Error: division by zero");
            
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 20) {
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_div_pd(_mm256_loadu_pd(&liste[i]), _mm256_loadu_pd(&n->liste[i])));
                }
                for (;nb < szl; nb++)
                    liste[nb] /= n->liste[nb];
                return this;
            }
#endif
            for (i = 0; i < szl; i++)
                liste[i] /= n->liste[i];
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Floats*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Floats*)e)->liste[i];
            }
            return this;
        case t_shorts:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Shorts*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Shorts*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            double v = e->asNumber();
            if (!v)
                throw new Error("Error: division by zero");
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 20) {
                __m256d vb = {v, v, v, v};
                long nb = (szl>>2)<<2;
                for (long i = 0; i < nb; i += 4) {
                    _mm256_storeu_pd(&liste[i], _mm256_div_pd(_mm256_loadu_pd(&liste[i]), vb));
                }
                for (;nb < szl; nb++)
                    liste[nb] /= v;
                return this;
            }
#endif
            liste.divide(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    if (n->liste.check(0)) {
                        result->release();
                        throw new Error("Error: division by zero");
                    }
                    n->liste[i] = liste[i] / n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                if (n->liste.check(0)) {
                    result->release();
                    throw new Error("Error: division by zero");
                }
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] / n->liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}


Element* Numbers::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        return lisp->provideNumber(liste.sum());
    }
    
    if (e->isList()) {
        if (e->type == type) {
            liste.plus(((Numbers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asNumber();
        }
        return this;
    }
    
    liste.plus(e->asNumber());
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
        if (e->type == type) {
            liste.minus(((Numbers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] -= e->index(i)->asNumber();
        }
        return this;
    }
    
    liste.minus(e->asNumber());
    return this;
}

Element* Numbers::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        return lisp->provideNumber(liste.product());
    }
    
    if (e->isList()) {
        if (e->type == type) {
            liste.multiply(((Numbers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] *= e->index(i)->asNumber();
        }
        return this;
    }
    
    liste.multiply(e->asNumber());
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    
    double d = e->asNumber();
    if (d == 0)
        throw new Error("Error: division by zero");
    liste.divide(d);
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
    Integers* num = l->provideIntegers();
    for (long i = 0; i < size(); i++)
        num->liste.push_back(~liste[i]);
    release();
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Integers::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Shorts*)e)->liste[i];
            }
            return this;
        }
        case t_integers: {
            Integers* n = (Integers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 20) {
                //if a number can be divided by 4, then the last two bits should be 0
                //we then compute how many elements are missing to make a vector with a size divisible by 4
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i += 4) {
                    _mm256_storeu_si256( (__m256i *)&liste[i],
                                        _mm256_add_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste[i]),
                                                         _mm256_loadu_si256((const __m256i *)&n->liste[i])
                                                         )
                                        );
                }
                for (;nb < szl; nb++)
                    liste[nb] += n->liste[nb];
                return this;
            }
#endif
            liste.plus(n->liste, szl);
            return this;
        }
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            long v = e->asInteger();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 20) {
                long i;
                __m256i vb = {v, v, v, v};
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i+= 4) {
                    _mm256_storeu_si256( (__m256i *)&liste[i],
                                        _mm256_add_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste[i]),
                                                         vb
                                                         )
                                        );
                }
                for (;nb < szl; nb++)
                    liste[nb] += v;
                return this;
            }
#endif
            liste.plus(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Floats*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Integers::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Shorts*)e)->liste[i];
            }
            return this;
        }
        case t_integers: {
            Integers* n = (Integers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            szl = lmin(szl, i);
            
#ifdef INTELINTRINSICS
            if (szl >= 20) {
                //if a number can be divided by 4, then the last two bits should be 0
                //we then compute how many elements are missing to make a vector with a size divisible by 4
                long nb = (szl>>2)<<2;
                for (i = 0; i < nb; i += 4) {
                    _mm256_storeu_si256( (__m256i *)&liste[i],
                                        _mm256_sub_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste[i]),
                                                         _mm256_loadu_si256((const __m256i *)&n->liste[i])
                                                         )
                                        );
                }
                for (;nb < szl; nb++)
                    liste[nb] -= n->liste[nb];
                return this;
            }
#endif
            liste.minus(n->liste, szl);
            return this;
        }
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            long v = e->asInteger();
#ifdef INTELINTRINSICS
            long szl = liste.size();
            if (szl >= 20) {
                __m256i vb = {v, v, v, v};
                long nb = (szl>>2)<<2;
                for (long i = 0; i < nb; i+= 4) {
                    _mm256_storeu_si256( (__m256i *)&liste[i],
                                        _mm256_sub_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste[i]),
                                                         vb
                                                         )
                                        );
                }
                for (;nb < szl; nb++)
                    liste[nb] -= v;
                return this;
            }
#endif
            liste.minus(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            float v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Floats*)result->index(m))->liste[i];
                    ((Floats*)result->index(m))->liste[i] = liste[i] - v;
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            double v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Numbers*)result->index(m))->liste[i];
                    ((Numbers*)result->index(m))->liste[i] = liste[i] - v;
                }
            }
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Integers::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Shorts*)e)->liste[i];
            }
            return this;
        }
        case t_integers:
            liste.multiply(((Integers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            liste.multiply(e->asInteger());
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Floats*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Integers::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Numbers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Floats*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Shorts*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Shorts*)e)->liste[i];
            }
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_short:
        case t_integer: {
            long v = e->asInteger();
            if (!v)
                throw new Error("Error: division by zero");
            liste.divide(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            float v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Floats*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Floats*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            double v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Numbers*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Numbers*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Integers::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL)
        return lisp->provideInteger(liste.sum());
    
    if (e->isList()) {
        if (e->type == type) {
            liste.plus(((Integers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asInteger();
        }
        return this;
    }
    
    liste.plus(e->asInteger());
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
        if (e->type == type) {
            liste.minus(((Integers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] -= e->index(i)->asInteger();
        }
        return this;
    }
    liste.minus(e->asInteger());
    return this;
}

Element* Integers::multiply(LispE* lisp, Element* e) {
    if (e == NULL) {
        return lisp->provideInteger(liste.product());
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->type == type) {
            liste.multiply(((Integers*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] *= e->index(i)->asInteger();
        }
        return this;
    }
    liste.multiply(e->asInteger());
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    
    long d = e->asInteger();
    if (d == 0)
        throw new Error("Error: division by zero");
    liste.divide(d);
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Shorts::bit_not(LispE* l) {
    if (!status) {
        for (long i = 0; i < size(); i++) {
            liste[i] = ~liste[i];
        }
        return this;
    }
    Shorts* num = new Shorts();
    for (long i = 0; i < size(); i++)
        num->liste.push_back(~liste[i]);
    release();
    return num;
}


Element* Shorts::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d &= liste[i];
        }
        return new Short(d);
    }
    
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] &= e->index(i)->asShort();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] &= e->asShort();
    }
    return this;
}

Element* Shorts::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d &= ~liste[i];
        }
        return new Short(d);
    }
    
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] &= ~e->index(i)->asShort();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] &= ~e->asShort();
    }
    return this;
}


Element* Shorts::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d |= liste[i];
        }
        return new Short(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] |= e->index(i)->asShort();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] |= e->asShort();
    }
    return this;
}

Element* Shorts::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d ^= liste[i];
        }
        return new Short(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] ^= e->index(i)->asShort();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] ^= e->asShort();
    }
    return this;
}

Element* Shorts::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            liste.plus(((Shorts*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        case t_integers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] += ((Integers*)e)->liste[i];
            }
            return this;
        }
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            liste.plus(e->asShort());
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Floats*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Shorts::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            liste.minus(((Shorts*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        case t_integers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] -= ((Integers*)e)->liste[i];
            }
            return this;
        }
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            liste.minus(e->asShort());
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            float v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Floats*)result->index(m))->liste[i];
                    ((Floats*)result->index(m))->liste[i] = liste[i] - v;
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            double v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Numbers*)result->index(m))->liste[i];
                    ((Numbers*)result->index(m))->liste[i] = liste[i] - v;
                }
            }
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Shorts::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            liste.multiply(((Shorts*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste[i] *= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            liste.multiply(e->asShort());
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Floats*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Shorts::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Numbers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Numbers*)e)->liste[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Floats*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Floats*)e)->liste[i];
            }
            return this;
        }
        case t_shorts: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Shorts*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Shorts*)e)->liste[i];
            }
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste[i] /= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_short:
        case t_float:
        case t_integer: {
            int16_t v = e->asShort();
            if (!v)
                throw new Error("Error: division by zero");
            liste.divide(v);
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            float v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Floats*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Floats*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            double v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Numbers*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Numbers*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Shorts::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        return new Short(liste.sum());
    }
    
    if (e->isList()) {
        if (e->type == type) {
            liste.plus(((Shorts*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] += e->index(i)->asShort();
        }
        return this;
    }
    
    liste.plus(e->asShort());
    return this;
}

Element* Shorts::minus(LispE* lisp, Element* e) {
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste[i];
        }
        return new Short(d);
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->type == type) {
            liste.minus(((Shorts*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] -= e->index(i)->asShort();
        }
        return this;
    }
    liste.minus(e->asShort());
    return this;
}

Element* Shorts::multiply(LispE* lisp, Element* e) {
    if (e == NULL) {
        return new Short(liste.product());
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->type == type) {
            liste.multiply(((Shorts*)e)->liste, lmin(liste.size(), e->size()));
            return this;
        }
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] *= e->index(i)->asShort();
        }
        return this;
    }
    liste.multiply(e->asShort());
    return this;
}

Element* Shorts::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d /= liste[i];
        }
        return new Short(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    int16_t d = e->asShort();
    if (d == 0)
        throw new Error("Error: division by zero");
    liste.divide(d);
    return this;
}

Element* Shorts::mod(LispE* lisp, Element* e) {
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d %= liste[i];
        }
        return new Short(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Shorts::power(LispE* lisp, Element* e) {
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d, (double)liste[i]);
        }
        return new Short(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
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

Element* Shorts::leftshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d <<= liste[i];
        }
        return new Short(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] <<= e->index(i)->asShort();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] <<= e->asShort();
    }
    return this;
}

Element* Shorts::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        int16_t d = liste[0];
        for (long i = 1; i < size(); i++) {
            d >>= liste[i];
        }
        return new Short(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch (Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste[i] >>= e->index(i)->asShort();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste[i] >>= e->asShort();
    }
    return this;
}

Element* Set_i::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        for (const auto& a: ensemble) {
            d += a;
        }
        return lisp->provideInteger(d);
    }
    
    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            d *= a;
        }
        return lisp->provideInteger(d);
    }
    
    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    
    for (const auto& a: ensemble) {
        d = pow((double)a, w);
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        bool first = true;
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d = a & r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_and(LispE* lisp, Element* e) {
    long d = 0;
    if (e == NULL) {
        bool first = true;
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d = a & r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_or(LispE* lisp, Element* e) {
    long d = 0;
    if (e == NULL) {
        for (const auto& a: ensemble) {
            d |= a;
        }
        return lisp->provideInteger(d);
    }
    
    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            d ^= a;
        }
        return lisp->provideInteger(d);
    }
    
    Set_i* res = lisp->provideSet_i();
    if (e->type == t_seti) {
        auto nxt = ((Set_i*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d = a ^ r;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::bit_not(LispE* lisp) {
    //Two cases either e is a number or it is a list...
    Set_i* res = lisp->provideSet_i();
    
    for (const auto& a: ensemble) {
        res->add(~a);
    }
    release();
    return res;
}

Element* Set_i::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        bool first = true;
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d = a << w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_i::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    long d = 0;
    if (e == NULL) {
        bool first = true;
        for (const auto& a: ensemble) {
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
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d = a >> w;
        res->add(d);
    }
    release();
    return res;
}


Element* Set_n::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
    if (e == NULL) {
        for (const auto& a: ensemble) {
            d += a;
        }
        return lisp->provideNumber(d);
    }
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d -= a;
        }
        return lisp->provideNumber(d);
    }
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            d *= a;
        }
        return lisp->provideNumber(d);
    }
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d = pow(d, a);
        }
        return lisp->provideNumber(d);
    }
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
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
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d.v = a;
        d.bits ^= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_not(LispE* lisp) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    Set_n* res = lisp->provideSet_n();
    
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else
                d.bits <<= (long)a;
        }
        return lisp->provideNumber(d.v);
    }
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
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
        for (const auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else
                d.bits >>= (long)a;
        }
        return lisp->provideNumber(d.v);
    }
    
    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (const auto& a : ensemble) {
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
        for (const auto& a : ensemble) {
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
    for (const auto& a: ensemble) {
        d.v = a;
        d.bits >>= w;
        res->add(d.v);
    }
    release();
    return res;
}

//---------------------------------------------------------------------------
//------ Arithmetic Operations
//---------------------------------------------------------------------------


Element* List::evall_bitand(LispE* lisp) {
    List_bitand m(this);
    return m.eval(lisp);
}

Element* List::evall_bitandnot(LispE* lisp) {
    List_bitandnot m(this);
    return m.eval(lisp);
}

Element* List::evall_bitor(LispE* lisp) {
    List_bitor m(this);
    return m.eval(lisp);
}

Element* List::evall_bitxor(LispE* lisp) {
    List_bitxor m(this);
    return m.eval(lisp);
}

Element* List::evall_divide(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->divide_direct(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '/' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '/' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->divide(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->divide_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->divide_direct(lisp, lst->index(i));
                        }
                    }
                    break;
                }
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(lisp, 1);
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    return first_element;
}

Element* List_dividen::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    first_element = first_element->copyatom(lisp, 1);
    
    int16_t listsize = liste.size();
    Element* second_element = null_;
    
    try {
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element = first_element->divide_direct(lisp, second_element);
            if (first_element != second_element)
                _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}



Element* List_divide2::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    if (!first_element->isList())
        throw new Error("Error: cannot apply '/' to one element");
    
    Element* lst = first_element;
    
    try {
        switch (lst->type) {
            case t_strings:
                throw new Error("Error: cannot apply '/' to a string");
            case t_floats:
            case t_shorts:
            case t_integers:
            case t_numbers:
                if (!lst->size()) {
                    first_element->release();
                    return zero_;
                }
                lst = lst->divide(lisp, NULL);
                first_element->release();
                return lst;
            case t_llist: {
                first_element = zero_;
                u_link* u = ((LList*)lst)->liste.begin();
                if (u != NULL) {
                    first_element = u->value->copyatom(lisp, 1);
                    u = u->next();
                    while (u != NULL) {
                        first_element = first_element->divide_direct(lisp, u->value);
                        u = u->next();
                    }
                }
                lst->release();
                break;
            }
            case t_list: {
                first_element = zero_;
                long listsize = lst->size();
                if (listsize) {
                    first_element = lst->index(0)->copyatom(lisp, 1);
                    for (long i = 1; i < listsize; i++) {
                        first_element = first_element->divide_direct(lisp, lst->index(i));
                    }
                }
                lst->release();
                break;
            }
        }
    }
    catch (Error* err) {
        if (lst != first_element)
            lst->release();
        first_element->release();
        throw err;
    }
    return first_element;
}

Element* List_divide3::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp)->copyatom(lisp, 1);
    Element* second_element = null_;
    
    try {
        second_element = liste[2]->eval(lisp);
        first_element = first_element->divide_direct(lisp, second_element);
        second_element->release();
    }
    catch (Error* err) {
        second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}


Element* List::evall_minus(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->minus_direct(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '-' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '-' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->minus(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->minus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->minus_direct(lisp, lst->index(i));
                        }
                    }
                    break;
                }
            }
            lst->release();
            return first_element;
        }
        
        first_element = first_element->copyatom(lisp, 1);
        for (i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element = first_element->minus_direct(lisp, second_element);
            if (first_element != second_element)
                _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_minusn::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    first_element = first_element->copyatom(lisp, 1);
    
    int16_t listsize = liste.size();
    Element* second_element = null_;
    
    try {
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element = first_element->minus_direct(lisp, second_element);
            if (first_element != second_element)
                _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_minus2::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    if (!first_element->isList())
        throw new Error("Error: cannot apply '-' to one element");
    
    Element* lst = first_element;
    
    try {
        switch (lst->type) {
            case t_strings:
                throw new Error("Error: cannot apply '-' to a string");
            case t_floats:
            case t_shorts:
            case t_integers:
            case t_numbers:
                if (!lst->size()) {
                    first_element->release();
                    return zero_;
                }
                lst = lst->minus(lisp, NULL);
                first_element->release();
                return lst;
            case t_llist: {
                first_element = zero_;
                u_link* u = ((LList*)lst)->liste.begin();
                if (u != NULL) {
                    first_element = u->value->copyatom(lisp, 1);
                    u = u->next();
                    while (u != NULL) {
                        first_element = first_element->minus_direct(lisp, u->value);
                        u = u->next();
                    }
                }
                lst->release();
                break;
            }
            case t_list: {
                first_element = zero_;
                long listsize = lst->size();
                if (listsize) {
                    first_element = lst->index(0)->copyatom(lisp, 1);
                    for (long i = 1; i < listsize; i++) {
                        first_element = first_element->minus_direct(lisp, lst->index(i));
                    }
                }
                lst->release();
                break;
            }
        }
    }
    catch (Error* err) {
        if (lst != first_element)
            lst->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_minus3::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp)->copyatom(lisp, 1);
    
    try {
        Element* second_element = liste[2]->eval(lisp);
        first_element = first_element->minus_direct(lisp, second_element);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List::evall_mod(LispE* lisp) {
    List_mod_eval m(this);
    return m.eval(lisp);
}

Element* List::evall_multiply(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->multiply_direct(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '*' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '*' to a string");
                case t_floats: {
                    float v = ((Floats*)lst)->liste.product();
                    first_element->release();
                    return v?lisp->provideFloat(v):zero_;
                }
                case t_shorts: {
                    int16_t v = ((Shorts*)lst)->liste.product();
                    first_element->release();
                    return v?new Short(v):zero_;
                }
                case t_integers: {
                    long v = ((Integers*)lst)->liste.product();
                    first_element->release();
                    return v?lisp->provideInteger(v):zero_;
                }
                case t_numbers: {
                    double v = ((Numbers*)lst)->liste.product();
                    first_element->release();
                    return v?lisp->provideNumber(v):zero_;
                }
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->multiply_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->multiply_direct(lisp, lst->index(i));
                        }
                    }
                    break;
                }
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(lisp, 1);
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_multiplyn::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    first_element = first_element->copyatom(lisp, 1);
    
    int16_t listsize = liste.size();
    Element* second_element = null_;
    
    try {
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element = first_element->multiply_direct(lisp, second_element);
            if (first_element != second_element)
                _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_multiply2::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    if (!first_element->isList())
        throw new Error("Error: cannot apply '*' to one element");
    
    Element* lst = first_element;
    
    try {
        switch (lst->type) {
            case t_strings:
                throw new Error("Error: cannot apply '*' to a string");
            case t_floats: {
                float v = ((Floats*)lst)->liste.product();
                first_element->release();
                return v?lisp->provideFloat(v):zero_;
            }
            case t_shorts: {
                int16_t v = ((Shorts*)lst)->liste.product();
                first_element->release();
                return v?new Short(v):zero_;
            }
            case t_integers: {
                long v = ((Integers*)lst)->liste.product();
                first_element->release();
                return v?lisp->provideInteger(v):zero_;
            }
            case t_numbers: {
                double v = ((Numbers*)lst)->liste.product();
                first_element->release();
                return v?lisp->provideNumber(v):zero_;
            }
            case t_llist: {
                first_element = zero_;
                u_link* u = ((LList*)lst)->liste.begin();
                if (u != NULL) {
                    first_element = u->value->copyatom(lisp, 1);
                    u = u->next();
                    while (u != NULL) {
                        first_element = first_element->multiply_direct(lisp, u->value);
                        u = u->next();
                    }
                }
                lst->release();
                break;
            }
            case t_list: {
                first_element = zero_;
                long listsize = lst->size();
                if (listsize) {
                    first_element = lst->index(0)->copyatom(lisp, 1);
                    for (long i = 1; i < listsize; i++) {
                        first_element = first_element->multiply_direct(lisp, lst->index(i));
                    }
                }
                lst->release();
                break;
            }
        }
    }
    catch (Error* err) {
        if (lst != first_element)
            lst->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_multiply3::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp)->copyatom(lisp, 1);
    
    try {
        Element* second_element = liste[2]->eval(lisp);
        first_element = first_element->multiply_direct(lisp, second_element);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List::evall_listand(LispE* lisp) {
    List_listand_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_listor(LispE* lisp) {
    List_listor_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_listxor(LispE* lisp) {
    List_listxor_eval m(this);
    return m.eval(lisp);
}

Element* List::evall_plusmultiply(LispE* lisp) {
    List_plusmultiply m(this);
    return m.eval(lisp);
}

Element* List::evall_plus(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->plus_direct(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '+' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings: {
                    u_ustring v = ((Strings*)lst)->liste.sum();
                    first_element->release();
                    return (v == U"")?emptystring_:lisp->provideString(v);
                }
                case t_floats: {
                    float v = ((Floats*)lst)->liste.sum();
                    first_element->release();
                    return v?lisp->provideFloat(v):zero_;
                }
                case t_shorts: {
                    int16_t v = ((Shorts*)lst)->liste.sum();
                    first_element->release();
                    return v?new Short(v):zero_;
                }
                case t_integers: {
                    long v = ((Integers*)lst)->liste.sum();
                    first_element->release();
                    return v?lisp->provideInteger(v):zero_;
                }
                case t_numbers: {
                    double v = ((Numbers*)lst)->liste.sum();
                    first_element->release();
                    return v?lisp->provideNumber(v):zero_;
                }
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->plus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->plus_direct(lisp, lst->index(i));
                        }
                    }
                    break;
                }
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(lisp, 1);
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_plusn::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    first_element = first_element->copyatom(lisp, 1);
    
    int16_t listsize = liste.size();
    Element* second_element = null_;
    
    try {
        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            first_element = first_element->plus_direct(lisp, second_element);
            if (first_element != second_element)
                _releasing(second_element);
        }
    }
    catch (Error* err) {
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_plus2::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    if (!first_element->isList())
        throw new Error("Error: cannot apply '+' to one element");
    
    Element* lst = first_element;
    
    try {
        switch (lst->type) {
            case t_strings: {
                u_ustring v = ((Strings*)lst)->liste.sum();
                first_element->release();
                return (v == U"")?emptystring_:lisp->provideString(v);
            }
            case t_floats: {
                float v = ((Floats*)lst)->liste.sum();
                first_element->release();
                return v?lisp->provideFloat(v):zero_;
            }
            case t_shorts: {
                int16_t v = ((Shorts*)lst)->liste.sum();
                first_element->release();
                return v?new Short(v):zero_;
            }
            case t_integers: {
                long v = ((Integers*)lst)->liste.sum();
                first_element->release();
                return v?lisp->provideInteger(v):zero_;
            }
            case t_numbers: {
                double v = ((Numbers*)lst)->liste.sum();
                first_element->release();
                return v?lisp->provideNumber(v):zero_;
            }
            case t_llist: {
                first_element = zero_;
                u_link* u = ((LList*)lst)->liste.begin();
                if (u != NULL) {
                    first_element = u->value->copyatom(lisp, 1);
                    u = u->next();
                    while (u != NULL) {
                        first_element = first_element->plus_direct(lisp, u->value);
                        u = u->next();
                    }
                }
                lst->release();
                break;
            }
            case t_list: {
                first_element = zero_;
                long listsize = lst->size();
                if (listsize) {
                    first_element = lst->index(0)->copyatom(lisp, 1);
                    for (long i = 1; i < listsize; i++) {
                        first_element = first_element->plus_direct(lisp, lst->index(i));
                    }
                }
                lst->release();
                break;
            }
        }
    }
    catch (Error* err) {
        if (lst != first_element)
            lst->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_plus3::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp)->copyatom(lisp, 1);
    
    try {
        Element* second_element = liste[2]->eval(lisp);
        first_element = first_element->plus_direct(lisp, second_element);
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }
    
    return first_element;
}


Element* List::evall_leftshift(LispE* lisp) {
    List_leftshift_eval m(this);
    return m.eval(lisp);
}

Element* List::evall_rightshift(LispE* lisp) {
    List_rightshift_eval m(this);
    return m.eval(lisp);
}

Element* List::evall_power(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->power(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '^^' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '^^' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->power(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->power(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->power(lisp, lst->index(i));
                        }
                    }
                    break;
                }
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(lisp, 1);
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->power(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return first_element;
}

Element* List_power2::eval(LispE* lisp) {
    Element* e = liste[1]->eval(lisp)->copyatom(lisp, 1);
    
    switch (e->type) {
        case t_float:
            ((Float*)e)->content *= ((Float*)e)->content;
            return e;
        case t_number:
            ((Number*)e)->content *= ((Number*)e)->content;
            return e;
        case t_integer:
            ((Integer*)e)->content *= ((Integer*)e)->content;
            return e;
        case t_short:
            ((Short*)e)->content *= ((Short*)e)->content;
            return e;
        case t_complex:
            ((Complex*)e)->content *= ((Complex*)e)->content;
            return e;
        default:
            return e->multiply_direct(lisp, e);
    }
}

//-------------------------------------------------------------------------------

Element* List::evall_bitandequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '&' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '&' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->bit_and(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_and(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_and(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_and(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List::evall_bitandnotequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '&~' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '&~' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->bit_and_not(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_and_not(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_and_not(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_and_not(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List::evall_bitorequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '|' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '|' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->bit_or(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_or(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_or(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_or(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}



Element* List::evall_bitxorequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '^' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '^' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->bit_xor(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_xor(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_xor(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_xor(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List::evall_divideequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '/' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '/' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->divide(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->divide_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->divide_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List_divideequal_list::eval(LispE* lisp) {
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    List* exec = lisp->provideList();
    exec->append(liste[1]->index(0));
    exec->append(liste[1]->index(1));
    listsize = liste[1]->size();
    try {
        for (i = 2; i < listsize; i++) {
            first_element = liste[1]->index(i)->eval(lisp);
            exec->append(first_element);
        }
        first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
    }
    catch (Error* err) {
        exec->release();
        throw err;
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '/' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '/' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->divide(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->divide_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->divide_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        exec->release();
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    exec->append(first_element->quoting());
    exec->evall_set_at(lisp);
    first_element->increment();
    exec->release();
    first_element->decrementkeep();
    return first_element;
}

Element* List_divideequal_var::eval(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* lst = this;
    
    try {
        first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '/' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '/' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->divide(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->divide_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    long lstsize = lst->size();
                    if (lstsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (long i = 1; i < lstsize; i++) {
                            first_element = first_element->divide_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (long i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return lisp->recording_back(first_element, label);
}

Element* List::evall_leftshiftequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '<<' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '<<' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->leftshift(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->leftshift(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->leftshift(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->leftshift(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List::evall_minusequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '-' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '-' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->minus(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->minus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->minus_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->minus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List_minusequal_list::eval(LispE* lisp) {
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    List* exec = lisp->provideList();
    exec->append(liste[1]->index(0));
    exec->append(liste[1]->index(1));
    listsize = liste[1]->size();
    try {
        for (i = 2; i < listsize; i++) {
            first_element = liste[1]->index(i)->eval(lisp);
            exec->append(first_element);
        }
        first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
    }
    catch (Error* err) {
        exec->release();
        throw err;
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '-' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '-' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->minus(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->minus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->minus_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->minus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        exec->release();
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    exec->append(first_element->quoting());
    exec->evall_set_at(lisp);
    first_element->increment();
    exec->release();
    first_element->decrementkeep();
    return first_element;
}

Element* List_minusequal_var::eval(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* lst = this;
    
    try {
        first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '-' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '-' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->minus(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->minus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    long lstsize = lst->size();
                    if (lstsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (long i = 1; i < lstsize; i++) {
                            first_element = first_element->minus_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (long i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->minus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return lisp->recording_back(first_element, label);
}

Element* List::evall_modequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '%' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '%' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->mod(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->mod(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->mod(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->mod(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}


Element* List::evall_multiplyequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '*' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '*' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->multiply(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->multiply_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->multiply_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List_multiplyequal_list::eval(LispE* lisp) {
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    List* exec = lisp->provideList();
    exec->append(liste[1]->index(0));
    exec->append(liste[1]->index(1));
    listsize = liste[1]->size();
    try {
        for (i = 2; i < listsize; i++) {
            first_element = liste[1]->index(i)->eval(lisp);
            exec->append(first_element);
        }
        first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
    }
    catch (Error* err) {
        exec->release();
        throw err;
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '*' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '*' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->multiply(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->multiply_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->multiply_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        exec->release();
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    exec->append(first_element->quoting());
    exec->evall_set_at(lisp);
    first_element->increment();
    exec->release();
    first_element->decrementkeep();
    return first_element;
}

Element* List_multiplyequal_var::eval(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* lst = this;
    
    try {
        first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '*' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '*' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->multiply(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->multiply_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    long lstsize = lst->size();
                    if (lstsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (long i = 1; i < lstsize; i++) {
                            first_element = first_element->multiply_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (long i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return lisp->recording_back(first_element, label);
}


Element* List::evall_plusequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '+' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    if (!lst->size()) {
                        first_element->release();
                        return emptystring_;
                    }
                    lst = lst->plus(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->plus(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->plus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->plus_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List_plusequal_list::eval(LispE* lisp) {
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    List* exec = lisp->provideList();
    exec->append(liste[1]->index(0));
    exec->append(liste[1]->index(1));
    listsize = liste[1]->size();
    try {
        for (i = 2; i < listsize; i++) {
            first_element = liste[1]->index(i)->eval(lisp);
            exec->append(first_element);
        }
        first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
    }
    catch (Error* err) {
        exec->release();
        throw err;
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '+' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    if (!lst->size()) {
                        first_element->release();
                        return emptystring_;
                    }
                    lst = lst->plus(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->plus(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->plus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->plus_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        exec->release();
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    exec->append(first_element->quoting());
    exec->evall_set_at(lisp);
    first_element->increment();
    exec->release();
    first_element->decrementkeep();
    return first_element;
}

Element* List_plusequal_var::eval(LispE* lisp) {
    Element* first_element = liste[1];
    Element* second_element = null_;
    Element* lst = this;
    
    try {
        first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '+' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    if (!lst->size()) {
                        first_element->release();
                        return emptystring_;
                    }
                    lst = lst->plus(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->plus(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->plus_direct(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    long lstsize = lst->size();
                    if (lstsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (long i = 1; i < lstsize; i++) {
                            first_element = first_element->plus_direct(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (long i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    return lisp->recording_back(first_element, label);
}

Element* List::evall_rightshiftequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec;
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    
    try {
        first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '>>' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '>>' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->rightshift(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->rightshift(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->rightshift(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->rightshift(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List::evall_powerequal(LispE* lisp) {
    List* exec = NULL;
    int16_t label = liste[1]->label();
    long i;
    int16_t listsize;
    Element* first_element = liste[1];
    
    if (label < l_final) {
        label = -1;
        if (liste[1]->isList() && liste[1]->index(0)->label() == l_at) {
            if (liste[1]->index(1)->label() < l_final)
                throw new Error("Error: Expecting a variable in embedded '@'");
            exec = lisp->provideList();
            exec->append(liste[1]->index(0));
            exec->append(liste[1]->index(1));
            listsize = liste[1]->size();
            try {
                for (i = 2; i < listsize; i++) {
                    first_element = liste[1]->index(i)->eval(lisp);
                    exec->append(first_element);
                }
                first_element = exec->evall_index_zero(lisp)->copyatom(lisp, s_constant);
            }
            catch (Error* err) {
                exec->release();
                throw err;
            }
        }
        else
            throw new Error("Error: Missing variable");
    }
    
    listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    
    try {
        if (label != -1)
            first_element = first_element->eval(lisp)->copyatom(lisp, s_constant);
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '^^' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                    throw new Error("Error: cannot apply '^^' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_;
                    }
                    lst = lst->power(lisp, NULL);
                    first_element->release();
                    first_element = lst;
                    lst = this;
                    break;
                case t_llist: {
                    first_element = zero_;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->power(lisp, u->value);
                            u = u->next();
                        }
                    }
                    lst->release();
                    break;
                }
                case t_list: {
                    first_element = zero_;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->power(lisp, lst->index(i));
                        }
                    }
                    lst->release();
                    break;
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                second_element = liste[i]->eval(lisp);
                first_element = first_element->power(lisp, second_element);
                if (first_element != second_element)
                    _releasing(second_element);
            }
        }
    }
    catch (Error* err) {
        if (exec != NULL) {
            exec->release();
        }
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }
    
    if (exec != NULL) {
        exec->append(first_element->quoting());
        exec->evall_set_at(lisp);
        first_element->increment();
        exec->release();
        first_element->decrementkeep();
        return first_element;
    }
    return lisp->recording_back(first_element, label);
}

Element* List::evall_powerequal2(LispE* lisp) {
    Element* first_element = liste[0];
    int16_t label;
    
    try {
        first_element = liste[1]->eval(lisp)->copyatom(lisp, s_constant);
        first_element = first_element->multiply_direct(lisp, first_element);
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording_back(first_element, label);
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }
    
    return first_element;
}

//------------------------------------------------------------------------------------------


Element* List::evall_sum(LispE* lisp) {
    List_sum_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_product(LispE* lisp) {
    List_product_eval m(this);
    return m.eval(lisp);
}

//------------------------------------------------------------------------------------------

Element* List::evall_invert(LispE* lisp) {
    List_invert_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_solve(LispE* lisp) {
    List_solve_eval m(this);
    return m.eval(lisp);
}



Element* List::evall_determinant(LispE* lisp) {
    List_determinant_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_ludcmp(LispE* lisp) {
    List_ludcmp_eval m(this);
    return m.eval(lisp);
}


Element* List::evall_lubksb(LispE* lisp) {
    List_lubksb_eval m(this);
    return m.eval(lisp);
}

//------------------------------------------------------------------------------------------

typedef enum {math_fabs,math_acos,math_acosh,math_asin,math_asinh,math_atan,math_atanh,math_cbrt,math_cos,math_cosh,math_erf,math_erfc,math_exp,math_exp2,math_expm1,math_floor,math_lgamma,math_log,math_log10,math_log1p,math_log2,math_logb,math_nearbyint,math_rint,math_round,math_sin,math_sinh,math_sqrt,math_tan,math_tanh,math_tgamma,math_trunc, math_radian, math_degree, math_gcd, math_hcf} math;


long gcd_math(long a, long b)
{
    // Everything divides 0
    if (a == 0)
        return b;
    if (b == 0)
        return a;
    // base case
    if (a == b)
        return a;
    // a is greater
    if (a > b)
        return gcd_math(a-b, b);
    return gcd_math(a, b-a);
}

long hcf_math(long x, long y) {
    return (!y)?x:hcf_math(y, x%y);
}

/*
 Tout d'abord on crée une nouvelle dérivation de Element
 On implante alors les méthodes "eval" et "commeChaine"
 Cette extension exporte la majorité des opérateurs mathématique
 */

class Math : public Element {
public:
    math m;
    int16_t v_val;
    Math(LispE* lisp, math s) : m(s), Element(l_lib) {
        //We chose val as variable name everywhere
        // we recover his code to speed up processing ...
        u_ustring val = U"val";
        v_val = lisp->encode(val);
    }
    
    Element* complex_math(Complex* c) {
        std::complex<double>& v = c->content;
        switch (m) {
            case math_fabs: {
                v.real(fabs(v.real()));
                v.imag(fabs(v.imag()));
                return c;
            }
            case math_acos: {
                v = acos(v);
                return c;
            }
            case math_acosh: {
                v = acosh(v);
                return c;
            }
            case math_asin: {
                v = asin(v);
                return c;
            }
            case math_asinh: {
                v = asinh(v);
                return c;
            }
            case math_atan: {
                v = atan(v);
                return c;
            }
            case math_atanh: {
                v = atanh(v);
                return c;
            }
            case math_cbrt: {
                v.real(cbrt(v.real()));
                v.imag(cbrt(v.imag()));
                return c;
            }
            case math_cos: {
                v = cos(v);
                return c;
            }
            case math_cosh: {
                v = cosh(v);
                return c;
            }
            case math_erf: {
                v.real(erf(v.real()));
                v.imag(erf(v.imag()));
                return c;
            }
            case math_erfc: {
                v.real(erfc(v.real()));
                v.imag(erfc(v.imag()));
                return c;
            }
            case math_exp: {
                v = exp(v);
                return c;
            }
            case math_exp2: {
                v.real(exp2(v.real()));
                v.imag(exp2(v.imag()));
                return c;
            }
            case math_expm1: {
                v.real(expm1(v.real()));
                v.imag(expm1(v.imag()));
                return c;
            }
            case math_floor: {
                v.real(floor(v.real()));
                v.imag(floor(v.imag()));
                return c;
            }
            case math_lgamma: {
                v.real(lgamma(v.real()));
                v.imag(lgamma(v.imag()));
                return c;
            }
            case math_log: {
                v = log(v);
                return c;
            }
            case math_log10: {
                v = log10(v);
                return c;
            }
            case math_log2: {
                v = log10(v);
                v /= log10(2);
                return c;
            }
            case math_nearbyint: {
                v.real(nearbyint(v.real()));
                v.imag(nearbyint(v.imag()));
                return c;
            }
            case math_rint: {
                v.real(rint(v.real()));
                v.imag(rint(v.imag()));
                return c;
            }
            case math_round: {
                v.real(round(v.real()));
                v.imag(round(v.imag()));
                return c;
            }
            case math_sin: {
                v = sin(v);
                return c;
            }
            case math_sinh: {
                v = sinh(v);
                return c;
            }
            case math_sqrt: {
                v = sqrt(v);
                return c;
            }
            case math_tan: {
                v = tan(v);
                return c;
            }
            case math_tanh: {
                v = tanh(v);
                return c;
            }
            case math_tgamma: {
                v.real(tgamma(v.real()));
                v.imag(tgamma(v.imag()));
                return c;
            }
            case math_trunc: {
                v.real(trunc(v.real()));
                v.imag(trunc(v.imag()));
                return c;
            }
            case math_radian: {
                v /= 180;
                v *= M_PI;
                return c;
            }
            case math_degree: {
                v *= 180;
                v /= M_PI;
                return c;
            }
            default:
                c->release();
                throw new Error("Error: Cannot apply this method on a complex value.");
        }
    }
    
    Element* eval(LispE* lisp) {
        //eval is either: command, setenv or getenv...
        Element* val_v = lisp->get_variable(v_val);
        if (val_v->type == t_complex) {
            Complex* c = new Complex(((Complex*)val_v)->content);
            return complex_math(c);
        }
        double v;
        switch (m) {
            case math_fabs: {
                v = val_v->asNumber();
                v = fabs(v);
                return lisp->provideNumber(v);
            }
            case math_acos: {
                v = val_v->asNumber();
                v = acos(v);
                return lisp->provideNumber(v);
            }
            case math_acosh: {
                v = val_v->asNumber();
                v = acosh(v);
                return lisp->provideNumber(v);
            }
            case math_asin: {
                v = val_v->asNumber();
                v = asin(v);
                return lisp->provideNumber(v);
            }
            case math_asinh: {
                v = val_v->asNumber();
                v = asinh(v);
                return lisp->provideNumber(v);
            }
            case math_atan: {
                v = val_v->asNumber();
                v = atan(v);
                return lisp->provideNumber(v);
            }
            case math_atanh: {
                v = val_v->asNumber();
                v = atanh(v);
                return lisp->provideNumber(v);
            }
            case math_cbrt: {
                v = val_v->asNumber();
                v = cbrt(v);
                return lisp->provideNumber(v);
            }
            case math_cos: {
                v = val_v->asNumber();
                v = cos(v);
                return lisp->provideNumber(v);
            }
            case math_cosh: {
                v = val_v->asNumber();
                v = cosh(v);
                return lisp->provideNumber(v);
            }
            case math_erf: {
                v = val_v->asNumber();
                v = erf(v);
                return lisp->provideNumber(v);
            }
            case math_erfc: {
                v = val_v->asNumber();
                v = erfc(v);
                return lisp->provideNumber(v);
            }
            case math_exp: {
                v = val_v->asNumber();
                v = exp(v);
                return lisp->provideNumber(v);
            }
            case math_exp2: {
                v = val_v->asNumber();
                v = exp2(v);
                return lisp->provideNumber(v);
            }
            case math_expm1: {
                v = val_v->asNumber();
                v = expm1(v);
                return lisp->provideNumber(v);
            }
            case math_floor: {
                v = val_v->asNumber();
                v = floor(v);
                return lisp->provideNumber(v);
            }
            case math_lgamma: {
                v = val_v->asNumber();
                v = lgamma(v);
                return lisp->provideNumber(v);
            }
            case math_log: {
                v = val_v->asNumber();
                v = log(v);
                return lisp->provideNumber(v);
            }
            case math_log10: {
                v = val_v->asNumber();
                v = log10(v);
                return lisp->provideNumber(v);
            }
            case math_log1p: {
                v = val_v->asNumber();
                v = log1p(v);
                return lisp->provideNumber(v);
            }
            case math_log2: {
                v = val_v->asNumber();
                v = log2(v);
                return lisp->provideNumber(v);
            }
            case math_logb: {
                v = val_v->asNumber();
                v = logb(v);
                return lisp->provideNumber(v);
            }
            case math_nearbyint: {
                v = val_v->asNumber();
                v = nearbyint(v);
                return lisp->provideNumber(v);
            }
            case math_rint: {
                v = val_v->asNumber();
                v = rint(v);
                return lisp->provideNumber(v);
            }
            case math_round: {
                v = val_v->asNumber();
                v = round(v);
                return lisp->provideNumber(v);
            }
            case math_sin: {
                v = val_v->asNumber();
                v = sin(v);
                return lisp->provideNumber(v);
            }
            case math_sinh: {
                v = val_v->asNumber();
                v = sinh(v);
                return lisp->provideNumber(v);
            }
            case math_sqrt: {
                v = val_v->asNumber();
                v = sqrt(v);
                return lisp->provideNumber(v);
            }
            case math_tan: {
                v = val_v->asNumber();
                v = tan(v);
                return lisp->provideNumber(v);
            }
            case math_tanh: {
                v = val_v->asNumber();
                v = tanh(v);
                return lisp->provideNumber(v);
            }
            case math_tgamma: {
                v = val_v->asNumber();
                v = tgamma(v);
                return lisp->provideNumber(v);
            }
            case math_trunc: {
                v = val_v->asNumber();
                v = trunc(v);
                return lisp->provideNumber(v);
            }
            case math_radian: {
                v = val_v->asNumber();
                v = M_PI*(v / 180);
                return lisp->provideNumber(v);
            }
            case math_degree: {
                v = val_v->asNumber();
                v = (v * 180) / M_PI;
                return lisp->provideNumber(v);
            }
            case math_gcd: {
                long v = val_v->asInteger();
                long vv = lisp->get_variable(U"vaal")->asInteger();
                return lisp->provideInteger(gcd_math(v,vv));
            }
            case math_hcf: {
                long v = val_v->asInteger();
                long vv = lisp->get_variable(U"vaal")->asInteger();
                return lisp->provideInteger(hcf_math(v,vv));
            }
        }
        return zero_;
    }
    
    //We use this instruction to return a description of the instruction
    //in effect, just do: (print getenv) to get this information
    wstring asString(LispE* lisp) {
        switch (m) {
            case math_fabs: {
                return L"compute the absolute value of a float type number";
            }
            case math_acos: {
                return L"compute the arc cosine";
            }
            case math_acosh: {
                return L"compute the hyperbolic arc cosine";
            }
            case math_asin: {
                return L"compute the arc sine";
            }
            case math_asinh: {
                return L"compute the hyperbolic sine arc";
            }
            case math_atan: {
                return L"compute the arc tangent";
            }
            case math_atanh: {
                return L"compute the hyperbolic arc tangent";
            }
            case math_cbrt: {
                return L"compute the cubic root";
            }
            case math_cos: {
                return L"compute the cosine";
            }
            case math_cosh: {
                return L"compute the hyperbolic cosine";
            }
            case math_erf: {
                return L"compute the error function";
            }
            case math_erfc: {
                return L"compute the complementary error function";
            }
            case math_exp: {
                return L"returns the high e to the required power";
            }
            case math_exp2: {
                return L"returns 2 high to the required power";
            }
            case math_expm1: {
                return L"Returns high e to the required power minus 1";
            }
            case math_floor: {
                return L"returns the nearest lower integer";
            }
            case math_gcd:
                return L"return Greater Common Divison";
            case math_hcf:
                return L"return Higher Common Factor";
            case math_lgamma: {
                return L"compute the natural logarithm of the absolute value of the gamma function";
            }
            case math_log: {
                return L"compute the natural logarithm (in base e)";
            }
            case math_log10: {
                return L"compute the decimal logarithm (base 10)";
            }
            case math_log1p: {
                return L"compute the natural logarithm (base e) of 1 plus the given number";
            }
            case math_log2: {
                return L"compute the binary logarithm (base 2)";
            }
            case math_logb: {
                return L"extracts the exponent of a number";
            }
            case math_nearbyint: {
                return L"returns the nearest integer using the current rounding method";
            }
            case math_rint: {
                return L"returns the nearest integer using current rounding method with exception if the result is different";
            }
            case math_round: {
                return L"returns the nearest integer, following the rounding rules";
            }
            case math_sin: {
                return L"compute the sine";
            }
            case math_sinh: {
                return L"compute the hyperbolic sine";
            }
            case math_sqrt: {
                return L"compute the square root";
            }
            case math_tan: {
                return L"compute the tangent";
            }
            case math_tanh: {
                return L"compute the hyperbolic tangent";
            }
            case math_tgamma: {
                return L"compute the gamma function";
            }
            case math_trunc: {
                return L"returns the nearest integer whose absolute value is smaller";
            }
            case math_radian: {
                return L"convert a value in 'degree' into 'radian'";
            }
            case math_degree: {
                return L"convert a value in 'radian' into 'degree'";
            }
        }
        return L"";
    }
};


//We are also going to implement the body of the call
void moduleMaths(LispE* lisp) {
    //We first create the body of the function
    lisp->extension("deflib fabs (val)", new Math(lisp, math_fabs));
    lisp->extension("deflib acos (val)", new Math(lisp, math_acos));
    lisp->extension("deflib acosh (val)", new Math(lisp, math_acosh));
    lisp->extension("deflib asin (val)", new Math(lisp, math_asin));
    lisp->extension("deflib asinh (val)", new Math(lisp, math_asinh));
    lisp->extension("deflib atan (val)", new Math(lisp, math_atan));
    lisp->extension("deflib atanh (val)", new Math(lisp, math_atanh));
    lisp->extension("deflib cbrt (val)", new Math(lisp, math_cbrt));
    lisp->extension("deflib ∛ (val)", new Math(lisp, math_cbrt));
    lisp->extension("deflib cos (val)", new Math(lisp, math_cos));
    lisp->extension("deflib cosh (val)", new Math(lisp, math_cosh));
    lisp->extension("deflib erf (val)", new Math(lisp, math_erf));
    lisp->extension("deflib erfc (val)", new Math(lisp, math_erfc));
    lisp->extension("deflib exp (val)", new Math(lisp, math_exp));
    lisp->extension("deflib exp2 (val)", new Math(lisp, math_exp2));
    lisp->extension("deflib expm1 (val)", new Math(lisp, math_expm1));
    lisp->extension("deflib floor (val)", new Math(lisp, math_floor));
    lisp->extension("deflib gcd (val vaal)", new Math(lisp, math_gcd));
    lisp->extension("deflib hcf (val vaal)", new Math(lisp, math_hcf));
    lisp->extension("deflib lgamma (val)", new Math(lisp, math_lgamma));
    lisp->extension("deflib log (val)", new Math(lisp, math_log));
    lisp->extension("deflib log10 (val)", new Math(lisp, math_log10));
    lisp->extension("deflib log1p (val)", new Math(lisp, math_log1p));
    lisp->extension("deflib log2 (val)", new Math(lisp, math_log2));
    lisp->extension("deflib logb (val)", new Math(lisp, math_logb));
    lisp->extension("deflib nearbyint (val)", new Math(lisp, math_nearbyint));
    lisp->extension("deflib rint (val)", new Math(lisp, math_rint));
    lisp->extension("deflib round (val)", new Math(lisp, math_round));
    lisp->extension("deflib sin (val)", new Math(lisp, math_sin));
    lisp->extension("deflib sinh (val)", new Math(lisp, math_sinh));
    lisp->extension("deflib sqrt (val)", new Math(lisp, math_sqrt));
    lisp->extension("deflib √ (val)", new Math(lisp, math_sqrt));
    lisp->extension("deflib tan (val)", new Math(lisp, math_tan));
    lisp->extension("deflib tanh (val)", new Math(lisp, math_tanh));
    lisp->extension("deflib tgamma (val)", new Math(lisp, math_tgamma));
    lisp->extension("deflib trunc (val)", new Math(lisp, math_trunc));
    lisp->extension("deflib radian (val)", new Math(lisp, math_radian));
    lisp->extension("deflib degree (val)", new Math(lisp, math_degree));
    
    u_ustring nom = U"_pi";
    Element* value = lisp->provideNumber(M_PI);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"π";
    lisp->recordingunique(value, lisp->encode(nom));
    
    nom = U"_tau";
    value = lisp->provideNumber(2 * M_PI);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"τ";
    lisp->recordingunique(value, lisp->encode(nom));
    
    nom = U"_e";
    value = lisp->provideNumber(M_E);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"ℯ";
    lisp->recordingunique(value, lisp->encode(nom));
    
    nom = U"_phi";
    value = lisp->provideNumber(M_GOLDEN);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"ϕ";
    lisp->recordingunique(value, lisp->encode(nom));
    
}

Element* Floats::matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21) {
    Floats* m2 = (Floats*)mat;
    
    Floats* result = lisp->provideFloats();
    result->liste.reserve(sh10 * sh21);

    long sz = size();
    long szf = m2->size();
    long i, j, k;
    vecte_a<float> transpose(szf);
    float v;

    for (i = 0; i < sh21; i++) {
        for (j = 0; j < sh; j++) {
            transpose.push_raw(m2->liste[i+j*sh21]);
        }
    }
    
    for (i = 0; i < sz; i += sh) {
        for (j = 0; j < szf; j += sh) {
            v = 0;
            for (k = 0; k < sh; k++) {
                v += liste[k + i] * transpose[k + j];
            }
            result->liste.push_raw(v);
        }
    }
    
    Integers* shapes = lisp->provideIntegers();
    shapes->liste.push_back(sh10);
    shapes->liste.push_back(sh21);
    
    List* l = lisp->provideList();
    l->append(shapes);
    l->append(result);
    return l;
}

Element* Numbers::matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21) {
    Numbers* m2 = (Numbers*)mat;
    
    Numbers* result = lisp->provideNumbers();
    result->liste.reserve(sh10 * sh21);

    long sz = size();
    long szf = m2->size();
    long i, j, k;
    vecte_a<double> transpose(szf);
    double v;

    transpose.reserve(szf);
    
    for (i = 0; i < sh21; i++) {
        for (j = 0; j < sh; j++) {
            transpose.push_raw(m2->liste[i+j*sh21]);
        }
    }
    
    for (i = 0; i < sz; i += sh) {
        for (j = 0; j < szf; j += sh) {
            v = 0;
            for (k = 0; k < sh; k++) {
                v += liste[k + i] * transpose[k + j];
            }
            result->liste.push_raw(v);
        }
    }

    Integers* shapes = lisp->provideIntegers();
    shapes->liste.push_back(sh10);
    shapes->liste.push_back(sh21);
    
    List* l = lisp->provideList();
    l->append(shapes);
    l->append(result);
    return l;
}

Element* Shorts::matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21) {
    Shorts* m2 = (Shorts*)mat;
    
    Shorts* result = new Shorts();
    result->liste.reserve(sh10 * sh21);

    long sz = size();
    long szf = m2->size();
    long i, j, k;
    vecte_a<short> transpose(szf);
    short v;

    transpose.reserve(szf);
    
    for (i = 0; i < sh21; i++) {
        for (j = 0; j < sh; j++) {
            transpose.push_raw(m2->liste[i+j*sh21]);
        }
    }
    
    for (i = 0; i < sz; i += sh) {
        for (j = 0; j < szf; j += sh) {
            v = 0;
            for (k = 0; k < sh; k++) {
                v += liste[k + i] * transpose[k + j];
            }
            result->liste.push_raw(v);
        }
    }

    Integers* shapes = lisp->provideIntegers();
    shapes->liste.push_back(sh10);
    shapes->liste.push_back(sh21);
    
    List* l = lisp->provideList();
    l->append(shapes);
    l->append(result);
    return l;
}

Element* Integers::matrix_product(LispE* lisp, Element* mat, long sh, long sh10, long sh21) {
    Integers* m2 = (Integers*)mat;
    
    Integers* result = lisp->provideIntegers();
    result->liste.reserve(sh10 * sh21);

    long sz = size();
    long szf = m2->size();
    long i, j, k;
    vecte_a<long> transpose(szf);
    long v;

    transpose.reserve(szf);
    
    for (i = 0; i < sh21; i++) {
        for (j = 0; j < sh; j++) {
            transpose.push_raw(m2->liste[i+j*sh21]);
        }
    }
    
    for (i = 0; i < sz; i += sh) {
        for (j = 0; j < szf; j += sh) {
            v = 0;
            for (k = 0; k < sh; k++) {
                v += liste[k + i] * transpose[k + j];
            }
            result->liste.push_raw(v);
        }
    }

    Integers* shapes = lisp->provideIntegers();
    shapes->liste.push_back(sh10);
    shapes->liste.push_back(sh21);
    
    List* l = lisp->provideList();
    l->append(shapes);
    l->append(result);
    return l;
}


