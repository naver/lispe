/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  tensors.cxx
//
//


#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>

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

#ifdef WIN32
#define _USE_MATH_DEFINES
#endif

#include <math.h>


#ifdef INTELINTRINSICS
#ifdef WIN32
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

//------------------------------------------------------------------------------------------
#define lmin(x,y) x<y?x:y
//------------------------------------------------------------------------------------------
Element* apply_op1_op2(LispE* lisp, Element* op1, Element* op2, Element* l1, Element* l2);
//------------------------------------------------------------------------------------------
// Matrice template: Function instantiations
//------------------------------------------------------------------------------------------

Element* Stringbytes::newTensor(LispE* lisp, List* l) {
    return new Matrice_stringbyte(l);
}

Element* Strings::newTensor(LispE* lisp, List* l) {
    return new Matrice_string(l);
}

Element* Floats::newTensor(LispE* lisp, List* l) {
    return new Matrice_float(l);
}

Element* Numbers::newTensor(LispE* lisp, List* l) {
    return new Matrice_number(l);
}

Element* Integers::newTensor(LispE* lisp, List* l) {
    return new Matrice_integer(l);
}

Element* Shorts::newTensor(LispE* lisp, List* l) {
    return new Matrice_short(l);
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
        tenseur = new Matrice_number(lisp, sz[0], sz[1], 0.0);
    else
        tenseur = new Tenseur_number(lisp, sz, zero_);
    
    Element* e;
    for (i = 0; i < sz[1]; i++) {
        e = liste[i];
        for (long j = 0; j < sz[0]; j++) {
            tenseur->index(j)->replacing(i, e->index(j));
        }
    }
    
    return tenseur;
}

//------------------------------------------------------------------------------------------

//LU decomposition
long LUDCMP(long n, vecte<long>& indexes, long& d, Matrice_number* m) {
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

void LUBKSB(long n, vecte<long>& indexes, vecte<double>& b_values, Matrice_number* m)  {
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

long LUDCMP(long n, vecte<long>& indexes, long& d, Matrice_integer* m) {
    d = 1;
    float AMAX, DUM, thesum;
    long i, i_max = 0, j, k;
    vector<long> values;
    
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

void LUBKSB(long n, vecte<long>& indexes, vecte<long>& b_values, Matrice_integer* m)  {
    long thesum;
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

//------------------------------------------------------------
template<> short Matrice_short::zeroValue() {
    return 0;
}

template<> double Matrice_number::zeroValue() {
    return 0;
}

template<> float Matrice_float::zeroValue() {
    return 0;
}

template<> u_ustring Matrice_string::zeroValue() {
    return U"";
}

template<> string Matrice_stringbyte::zeroValue() {
    return "";
}

template<> long Matrice_integer::zeroValue() {
    return 0;
}
//------------------------------------------------------------
template<> short Matrice_short::asValue(Element* e) {
    return e->asShort();
}

template<> double Matrice_number::asValue(Element* e) {
    return e->asNumber();
}

template<> float Matrice_float::asValue(Element* e) {
    return e->asFloat();
}

template<> u_ustring Matrice_string::asValue(Element* e) {
    return e->asUString(NULL);
}

template<> string Matrice_stringbyte::asValue(Element* e) {
    return e->toString(NULL);
}

template<> long Matrice_integer::asValue(Element* e) {
    return e->asInteger();
}
//------------------------------------------------------------
template<> Element* Matrice_short::provideValue(LispE* lisp, short v) {
    return new Short(v);
}

template<> Element* Matrice_number::provideValue(LispE* lisp, double v) {
    return lisp->provideNumber(v);
}

template<> Element* Matrice_float::provideValue(LispE* lisp, float v) {
    return lisp->provideFloat(v);
}

template<> Element* Matrice_string::provideValue(LispE* lisp, u_ustring v) {
    return lisp->provideString(v);
}

template<> Element* Matrice_stringbyte::provideValue(LispE* lisp, string v) {
    return new Stringbyte(v);
}

template<> Element* Matrice_integer::provideValue(LispE* lisp, long v) {
    return lisp->provideInteger(v);
}
//------------------------------------------------------------
template<> Shorts* Matrice_short::provide() {
    return new Shorts();
}

template<> Strings* Matrice_string::provide() {
    return new Strings();
}

template<> Stringbytes* Matrice_stringbyte::provide() {
    return new Stringbytes();
}

template<> Integers* Matrice_integer::provide() {
    return new Integers();
}

template<> Numbers* Matrice_number::provide() {
    return new Numbers();
}

template<> Floats* Matrice_float::provide() {
    return new Floats();
}
//------------------------------------------------------------
template<> Shorts* Matrice_short::provide(long nb, short val) {
    return new Shorts(nb, val);
}

template<> Integers* Matrice_integer::provide(long nb, long val) {
    return new Integers(nb, val);
}

template<> Numbers* Matrice_number::provide(long nb, double val) {
    return new Numbers(nb, val);
}

template<> Floats* Matrice_float::provide(long nb, float val) {
    return new Floats(nb, val);
}

template<> Strings* Matrice_string::provide(long nb, u_ustring val) {
    return new Strings(nb, val);
}

template<> Stringbytes* Matrice_stringbyte::provide(long nb, string val) {
    return new Stringbytes(nb, val);
}

//------------------------------------------------------------

template<> Numbers* Matrice_number::provide(LispE* lisp) {
    return lisp->provideNumbers();
}

template<> Floats* Matrice_float::provide(LispE* lisp) {
    return lisp->provideFloats();
}

template<> Strings* Matrice_string::provide(LispE* lisp) {
    return lisp->provideStrings();
}

template<> Stringbytes* Matrice_stringbyte::provide(LispE* lisp) {
    return new Stringbytes();
}

template<> Integers* Matrice_integer::provide(LispE* lisp) {
    return lisp->provideIntegers();
}

template<> Shorts* Matrice_short::provide(LispE* lisp) {
    return new Shorts();
}

//------------------------------------------------------------
template<> Numbers* Matrice_number::provide(LispE* lisp, long nb, double v) {
    return lisp->provideNumbers(nb, v);
}

template<> Floats* Matrice_float::provide(LispE* lisp, long nb, float v) {
    return lisp->provideFloats(nb, v);
}

template<> Strings* Matrice_string::provide(LispE* lisp, long nb, u_ustring v) {
    return lisp->provideStrings(nb, v);
}

template<> Stringbytes* Matrice_stringbyte::provide(LispE* lisp, long nb, string v) {
    return new Stringbytes(nb, v);
}

template<> Integers* Matrice_integer::provide(LispE* lisp, long nb, long v) {
    return lisp->provideIntegers(nb, v);
}

template<> Shorts* Matrice_short::provide(LispE* lisp, long nb, short v) {
    return new Shorts(nb, v);
}
//----------------------------------------------------------------------------

template<> Numbers* Matrice_number::provide(LispE* lisp, Numbers* n) {
    return lisp->provideNumbers(n);
}


template<> Floats* Matrice_float::provide(LispE* lisp, Floats* n) {
    return lisp->provideFloats(n);
}

template<> Integers* Matrice_integer::provide(LispE* lisp, Integers* n) {
    return lisp->provideIntegers(n);
}

template<> Shorts* Matrice_short::provide(LispE* lisp, Shorts* n) {
    return new Shorts(n);
}

template<> Strings* Matrice_string::provide(LispE* lisp, Strings* n) {
    return lisp->provideStrings(n);
}

template<> Stringbytes* Matrice_stringbyte::provide(LispE* lisp, Stringbytes* n) {
    return new Stringbytes(n);
}

//------------------------------------------------------------
template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(long x, long y, Element* n) {
    type = T;
    size_x = x;
    size_y = y;
    C l;
    A v = asValue(n);
    for (long i = 0; i < size_x; i++) {
        l = provide(size_y, v);
        append(l);
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(long x, long y, A n) {
    type = T;
    size_x = x;
    size_y = y;
    C l;
    
    for (long i = 0; i < size_x; i++) {
        l = provide(size_y, n);
        append(l);
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(C n, long x, long y) {
    type = T;
    size_x = x;
    size_y = y;
    long idx = 0;
    C l;
    for (x = 0; x < size_x; x++) {
        l  = provide();
        append(l);
        for (y = 0; y < size_y; y++) {
            if (idx == n->size())
                idx = 0;
            l->liste.push_back(n->liste[idx++]);
        }
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(LispE* lisp, Matrix* m) {
    type = T;
    size_x = m->size_x;
    size_y = m->size_y;
    C l;
    if (m->type == type) {
        for (long i = 0; i < size_x; i++) {
            l = provide(lisp);
            l->liste = ((C)m->liste[i])->liste;
            append(l);
        }
        return;
    }

    for (long i = 0; i < size_x; i++) {
        l = provide(lisp);
        for (long j = 0; j < m->size_y; j++)
            l->liste.push_back(asValue(m->liste[i]->index(j)));
        append(l);
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(LispE* lisp, long x, long y, A n) {
    type = T;
    size_x = x;
    size_y = y;
    
    for (long i = 0; i < size_x; i++) {
        append(provide(lisp, size_y, n));
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(LispE* lisp, Element* lst, long x, long y) {
    type = T;
    size_x = x;
    size_y = y;
    build(lisp, lst);
}

template <typename A, lisp_code T, lisp_code TT, typename C> Matrice<A,T,TT,C>::Matrice(LispE* lisp, long x, C lst, long y) {
    type = T;
    size_x = x;
    size_y = y;
    buildfromvalues(lisp, lst);
}

//------------------------------------------------------------
template <> double Matrice_number::determinant(LispE* lisp) {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        sent_error_0("Error: we can only apply 'determinant' to square matrices");

    long i;
    i = 0;
    double det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;
        
        Matrice_number sub(size_x - 1, size_y - 1, 0.0);
        
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


template <> double Matrice_float::determinant(LispE* lisp) {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        sent_error_0("Error: we can only apply 'determinant' to square matrices");

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
        double sg = pow(-1, (i + j + 2));
        det += val(i,j) * sg*sub.determinant(lisp);
    }
    return det;
}

template <> double Matrice_integer::determinant(LispE* lisp) {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        sent_error_0("Error: we can only apply 'determinant' to square matrices");

    long i;
    i = 0;
    float det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;
        
        Matrice_integer sub(size_x - 1, size_y - 1, 0.0);
        
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

template <> double Matrice_short::determinant(LispE* lisp) {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        sent_error_0("Error: we can only apply 'determinant' to square matrices");

    long i;
    i = 0;
    float det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;
        
        Matrice_short sub(size_x - 1, size_y - 1, 0.0);
        
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

template <> double Matrice_string::determinant(LispE* lisp) {
    return 0;
}

template <> double Matrice_stringbyte::determinant(LispE* lisp) {
    return 0;
}

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::inversion(LispE* lisp) {
    if (size_x != size_y)
        sent_error("Error: we can only apply 'invert' to square matrices");
    
    //else Local decomposition
    Matrice<A,T,TT,C> m(this);
    
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice<A,T,TT,C>* Y = new Matrice<A,T,TT,C>(lisp, size_x, size_x, 0.0);
    
    long i;
    //We create an identity matrix, which will contain the final result...
    for (i = 0; i < size_x; i++) {
        Y->set(i,i, 1);
    }
    
    vecte<A> temp(size_x);
    
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


template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::solve(LispE* lisp, Matrice<A,T,TT,C>* y) {
    if (size_x != size_y || y->size_x != y->size_y || size_x != y->size_x)
        sent_error("Error: we can only apply 'solve' to square matrices of equal sizes");
        
    //else Local decomposition
    Matrice<A,T,TT,C> m(this);
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice<A,T,TT,C>* Y = new Matrice<A,T,TT,C>(lisp, y);
    vecte<A> temp(size_x);
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

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::ludcmp(LispE* lisp) {
    if (size_x != size_y)
        sent_error("Error: we can only apply 'ludcmp' to square matrices");

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

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::lubksb(LispE* lisp, Integers* idxs, Matrice<A,T,TT,C>* Y) {
    if (size_x != size_y || idxs->size() != size_x)
        sent_error("Error: we can only apply 'lubksb' to square matrices with the same number of indexes");
    
    long i;
    if (Y == NULL) {
        Y = new Matrice<A,T,TT,C>(lisp, size_x, size_x, (A)0);
        //We create an identity matrix, which will contain the final result...
        for (i = 0; i < size_x; i++) {
            Y->set(i,i, 1);
        }
    }
    else {
        if (Y->size_x != size_x)
            sent_error("Error: we can only apply 'lubksb' to square matrices of the same shape");
    }
    
    vecte<long> indexes(size_x);
    for (i = 0; i < size_x; i++) {
        indexes.push_back(idxs->liste[i]);
    }
    
    vecte<A> temp(size_x);
    
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

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::check_member(LispE* lisp,Element* the_set) {
    Matrice<A,T,TT,C>* r = new Matrice<A,T,TT,C>;
    r->size_x = size_x;
    r->size_y = size_y;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::transposed(LispE* lisp) {
    Matrice<A,T,TT,C>* transposed_matrix = new Matrice<A,T,TT,C>(lisp, size_y, size_x, zeroValue());
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


template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::rank(LispE* lisp, vecte<long>& positions) {
    int16_t sz = positions.size();
    if (!sz || sz > 2)
        sent_error("Error: index mismatch");

    if (positions[0] == -1) {
        //We return all columns
        if (sz == 1 || positions[1] == -1) {
            Matrice<A,T,TT,C>* m = new Matrice<A,T,TT,C>();
            C result;
            
            for (long j = 0; j < size_y; j++) {
                result = provide(lisp);
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
                sent_error("Error: indexes out of bounds");
        }
    }
    else {
        if (sz == 2 && positions[1] != -1) {
            if (positions[1] >= size_y)
                sent_error("Error: indexes out of bounds");
            return provideValue(lisp, val(positions[0], positions[1]));
        }

        if (positions[0] >= size_x)
            sent_error("Error: indexes out of bounds");
        return provide(lisp, (C)liste[positions[0]]);
    }

    C result = provide(lisp);
    for (long i = 0; i < size_x; i++) {
        result->liste.push_back(val(i,positions[1]));
    }
    return result;
}

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::cdr(LispE* lisp) {
    return new Matrice<A,T,TT,C>((List*)this, size_x, size_y, 1);
}

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    Element* e = this;
    
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            e = e->protected_index(lisp, pos);
            if (e == null_)
                sent_error("Error: No more elements to traverse with 'cad..r'");
            
            sz = e->size();
            pos = 0;
        }
        else {
            if (pos == sz)
                sent_error("Error: No more elements to traverse with 'cad..r'");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        return new Matrice<A,T,TT,C>((List*)this, size_x, size_y, pos);
    }
    
    return e;
}


template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < size_x; i++) {
        lisp->replacestackvalue(liste[i], label);
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

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::rotating(LispE* lisp, bool left) {
    Matrice<A,T,TT,C>* revert_matrix = new Matrice<A,T,TT,C>;
    revert_matrix->size_x = size_x;
    revert_matrix->size_y = size_y;
    for (long i = 0; i < size_x; i++) {
        revert_matrix->append(liste[i]->rotating(lisp, left));
    }
    
    return revert_matrix;
}

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::rotate(LispE* lisp, long nb) {
    Matrice<A,T,TT,C>* revert_matrix = new Matrice<A,T,TT,C>;
    revert_matrix->size_x = size_x;
    revert_matrix->size_y = size_y;
    for (long i = 0; i < size_x; i++) {
        revert_matrix->append(liste[i]->rotate(lisp, nb));
    }
    
    return revert_matrix;
}

template <typename A, lisp_code T, lisp_code TT, typename C> void Matrice<A,T,TT,C>::build(LispE* lisp, Element* lst) {
    C l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = provide(lisp);
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            l->liste.push_back(asValue(lst->index(idx++)));
        }
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> void Matrice<A,T,TT,C>::buildfromvalues(LispE* lisp, C lst) {
    C l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = provide(lisp);
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            l->liste.push_back(lst->liste[idx++]);
        }
    }
}

template <typename A, lisp_code T, lisp_code TT, typename C> void Matrice<A,T,TT,C>::concatenate(LispE* lisp, Element* e) {
    if (e->isList()) {
        if (e->size() != size_x)
            sent_error_e("Error: Length error");
        
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

template <typename A, lisp_code T, lisp_code TT, typename C> Element* Matrice<A,T,TT,C>::negate(LispE* lisp) {
    Matrice<A,T,TT,C>* m = new Matrice<A,T,TT,C>();
    m->size_x = size_x;
    m->size_y = size_y;
    for (long i = 0; i < size_x; i++) {
        m->append(liste[i]->negate(lisp));
    }
    return m;
}


//----------------------------------------------------------------------------
// Tenseur Template code
//----------------------------------------------------------------------------
template<> short Tenseur_short::asValue(Element* e) {
    return e->asShort();
}

template<> double Tenseur_number::asValue(Element* e) {
    return e->asNumber();
}

template<> float Tenseur_float::asValue(Element* e) {
    return e->asFloat();
}

template<> long Tenseur_integer::asValue(Element* e) {
    return e->asInteger();
}

template<> u_ustring Tenseur_string::asValue(Element* e) {
    return e->asUString(NULL);
}

template<> string Tenseur_stringbyte::asValue(Element* e) {
    return e->toString(NULL);
}

//------------------------------------------------------------
template<> short Tenseur_short::zeroValue() {
    return 0;
}

template<> double Tenseur_number::zeroValue() {
    return 0;
}

template<> float Tenseur_float::zeroValue() {
    return 0;
}

template<> u_ustring Tenseur_string::zeroValue() {
    return U"";
}

template<> string Tenseur_stringbyte::zeroValue() {
    return "";
}

template<> long Tenseur_integer::zeroValue() {
    return 0;
}
//----------------------------------------------------------------------------

template<> Numbers* Tenseur_number::provide() {
    return new Numbers();
}

template<> Strings* Tenseur_string::provide() {
    return new Strings();
}

template<> Stringbytes* Tenseur_stringbyte::provide() {
    return new Stringbytes();
}

template<> Floats* Tenseur_float::provide() {
    return new Floats();
}

template<> Integers* Tenseur_integer::provide() {
    return new Integers();
}

template<> Shorts* Tenseur_short::provide() {
    return new Shorts();
}

//----------------------------------------------------------------------------

template<> Shorts* Tenseur_short::provide(long nb, short val) {
    return new Shorts(nb, val);
}

template<> Strings* Tenseur_string::provide(long nb, u_ustring v) {
    return new Strings(nb, v);
}

template<> Stringbytes* Tenseur_stringbyte::provide(long nb, string v) {
    return new Stringbytes(nb, v);
}

template<> Integers* Tenseur_integer::provide(long nb, long val) {
    return new Integers(nb, val);
}

template<> Numbers* Tenseur_number::provide(long nb, double val) {
    return new Numbers(nb, val);
}

template<> Floats* Tenseur_float::provide(long nb, float val) {
    return new Floats(nb, val);
}
//----------------------------------------------------------------------------

template<> Strings* Tenseur_string::provide(LispE* lisp, long nb, u_ustring val) {
    return lisp->provideStrings(nb, val);
}

template<> Stringbytes* Tenseur_stringbyte::provide(LispE* lisp, long nb, string val) {
    return new Stringbytes(nb, val);
}

template<> Integers* Tenseur_integer::provide(LispE* lisp, long nb, long val) {
    return lisp->provideIntegers(nb, val);
}

template<> Numbers* Tenseur_number::provide(LispE* lisp, long nb, double val) {
    return lisp->provideNumbers(nb, val);
}

template<> Floats* Tenseur_float::provide(LispE* lisp, long nb, float val) {
    return lisp->provideFloats(nb, val);
}

template<> Shorts* Tenseur_short::provide(LispE* lisp, long nb, short val) {
    return new Shorts(nb, val);
}

//----------------------------------------------------------------------------

template<> Strings* Tenseur_string::provide(LispE* lisp) {
    return lisp->provideStrings();
}

template<> Stringbytes* Tenseur_stringbyte::provide(LispE* lisp) {
    return new Stringbytes();
}

template<> Numbers* Tenseur_number::provide(LispE* lisp) {
    return lisp->provideNumbers();
}

template<> Floats* Tenseur_float::provide(LispE* lisp) {
    return lisp->provideFloats();
}

template<> Integers* Tenseur_integer::provide(LispE* lisp) {
    return lisp->provideIntegers();
}

template<> Shorts* Tenseur_short::provide(LispE* lisp) {
    return new Shorts();
}

//----------------------------------------------------------------------------

template<> Numbers* Tenseur_number::provide(LispE* lisp, Numbers* n) {
    return lisp->provideNumbers(n);
}


template<> Floats* Tenseur_float::provide(LispE* lisp, Floats* n) {
    return lisp->provideFloats(n);
}

template<> Integers* Tenseur_integer::provide(LispE* lisp, Integers* n) {
    return lisp->provideIntegers(n);
}

template<> Shorts* Tenseur_short::provide(LispE* lisp, Shorts* n) {
    return new Shorts(n);
}

template<> Strings* Tenseur_string::provide(LispE* lisp, Strings* n) {
    return lisp->provideStrings(n);
}

template<> Stringbytes* Tenseur_stringbyte::provide(LispE* lisp, Stringbytes* n) {
    return new Stringbytes(n);
}

//----------------------------------------------------------------------------

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(vecte<long>& sz, Element* n) {
    type = T;
    shape = sz;
    if (shape.size())
        build(0,this, asValue(n));
}

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(vecte<long>& sz, A n) {
    type = T;
    shape = sz;
    if (shape.size())
        build(0,this, n);
}

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(vecte<long>& sz, C n) {
    type = T;
    shape = sz;
    long idx = 0;
    if (shape.size())
        build(0, idx, this, n);
}

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(LispE* lisp, vecte<long>& sz, Element* n) {
    type = T;
    shape = sz;
    if (shape.size())
        build(lisp, 0,this, asValue(n));
}

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(LispE* lisp, Element* lst, vecte<long>& sz) {
    type = T;
    shape = sz;
    if (shape.size()) {
        long idx = 0;
        build(lisp, 0,this, lst, idx);
    }
}

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(Tenseur<A,T,C>* tensor) {
    type = T;
    shape = tensor->shape;
    tensor->build(0, this);
}

//We steal the ITEM structure of this list
template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(LispE* lisp, List* l) : List(l, 0) {
    type = T;
    Element* e = l;
    while (e->isList()) {
        shape.push_back(e->size());
        e = e->index(0);
    }
}

template <typename A, lisp_code T, typename C> Tenseur<A,T,C>::Tenseur(LispE* lisp, List* l, vecte<long>& sh, long pos) : List(l,pos) {
    type = T;
    shape = sh;
    shape.vecteur[0] -= pos;
}
//------------------------------------------------------------------------------------------
template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::cdr(LispE* lisp) {
    return new Tenseur<A,T,C>(lisp, (List*)this, shape, 1);
}

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    Element* e = this;
    
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            e = e->protected_index(lisp, pos);
            if (e == null_)
                sent_error("Error: No more elements to traverse with 'cad..r'");
            
            sz = e->size();
            pos = 0;
        }
        else {
            if (pos == sz)
                sent_error("Error: No more elements to traverse with 'cad..r'");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        return new Tenseur<A,T,C>(lisp, (List*)this, shape, pos);
    }
    
    return e;
}
//------------------------------------------------------------------------------------------
Element* List::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb)
        return lisp->provideList();
    return new List(l, 0);
}

template <> Element* Tenseur_short::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb) {
        switch (shape.size()) {
            case 2:
                return new Shorts();
            case 3:
                return new Matrice_short();
            default:
                return new Tenseur_short();
        }
    }
    switch (shape.size()) {
        case 2:
            return new Shorts();
        case 3:
            return new Matrice_short(l);
        default:
            return new Tenseur_short(lisp, l);
    }
}

template <> Element* Tenseur_integer::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb) {
        switch (shape.size()) {
            case 2:
                return new Integers();
            case 3:
                return new Matrice_integer();
            default:
                return new Tenseur_integer();
        }
    }
    switch (shape.size()) {
        case 2:
            return new Integers();
        case 3:
            return new Matrice_integer(l);
        default:
            return new Tenseur_integer(lisp, l);
    }
}

template <> Element* Tenseur_string::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb) {
        switch (shape.size()) {
            case 2:
                return new Strings();
            case 3:
                return new Matrice_string();
            default:
                return new Tenseur_string();
        }
    }
    switch (shape.size()) {
        case 2:
            return new Strings();
        case 3:
            return new Matrice_string(l);
        default:
            return new Tenseur_string(lisp, l);
    }
}

template <> Element* Tenseur_stringbyte::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb) {
        switch (shape.size()) {
            case 2:
                return new Stringbytes();
            case 3:
                return new Matrice_stringbyte();
            default:
                return new Tenseur_stringbyte();
        }
    }
    switch (shape.size()) {
        case 2:
            return new Stringbytes();
        case 3:
            return new Matrice_stringbyte(l);
        default:
            return new Tenseur_stringbyte(lisp, l);
    }
}

template <> Element* Tenseur_float::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb) {
        switch (shape.size()) {
            case 2:
                return new Floats();
            case 3:
                return new Matrice_float();
            default:
                return new Tenseur_float();
        }
    }
    switch (shape.size()) {
        case 2:
            return new Floats();
        case 3:
            return new Matrice_float(l);
        default:
            return new Tenseur_float(lisp, l);
    }
}

template <> Element* Tenseur_number::newTensor(bool nb, LispE* lisp, List* l) {
    if (nb) {
        switch (shape.size()) {
            case 2:
                return new Numbers();
            case 3:
                return new Matrice_number();
            default:
                return new Tenseur_number();
        }
    }
    switch (shape.size()) {
        case 2:
            return new Numbers();
        case 3:
            return new Matrice_number(l);
        default:
            return new Tenseur_number(lisp, l);
    }
}
//------------------------------------------------------------------------------------------

template<> void Tenseur_short::setvalue(Element* res, Element* lst) {
    if (lst->type == t_shorts) {
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


template<> void Tenseur_integer::setvalue(Element* res, Element* lst) {
    if (lst->type == t_integers) {
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

template<> void Tenseur_string::setvalue(Element* res, Element* lst) {
    if (lst->type == t_strings) {
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

template<> void Tenseur_stringbyte::setvalue(Element* res, Element* lst) {
    if (lst->type == t_stringbytes) {
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

template<> void Tenseur_float::setvalue(Element* res, Element* lst) {
    if (lst->type == t_floats) {
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

template<> void Tenseur_number::setvalue(Element* res, Element* lst) {
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

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::check_member(LispE* lisp, Element* the_set) {
    Tenseur<A,T,C>* r = new Tenseur<A,T,C>;
    r->shape = shape;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::transposed(LispE* lisp) {
    vecte<long> sz;
    sz = shape;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    
    Tenseur<A,T,C>* transposed_matrix = new Tenseur<A,T,C>(lisp, sz, zero_);
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

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::storeRank(LispE* lisp, Element* result, Element* current, vecte<long>& positions, long idx) {
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

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < shape[0]; i++) {
        lisp->replacestackvalue(liste[i], label);
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

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return provide(lisp, (C)value);
    
    Element* r;
    if (init) {
        r = new Tenseur<A,T,C>;
        ((Tenseur<A,T,C>*)r)->shape = shape;
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

template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::rotating(LispE* lisp, bool left) {
    Tenseur<A,T,C>* revert_matrix = new Tenseur<A,T,C>();
    revert_matrix->shape = shape;
    for (long i = 0; i < shape[0]; i++) {
        revert_matrix->append(liste[i]->rotating(lisp, left));
    }
    return revert_matrix;
}

template <typename A, lisp_code T, typename C> Element*  Tenseur<A,T,C>::rotate(LispE* lisp, long nb) {
    Tenseur<A,T,C>* revert_matrix = new Tenseur<A,T,C>();
    revert_matrix->shape = shape;
    for (long i = 0; i < shape[0]; i++) {
        revert_matrix->append(liste[i]->rotate(lisp, nb));
    }
    return revert_matrix;
}


template <typename A, lisp_code T, typename C> Element* Tenseur<A,T,C>::negate(LispE* lisp) {
    Tenseur<A,T,C>* m = new Tenseur<A,T,C>();
    m->shape = shape;
    for (long i = 0; i < shape[0]; i++) {
        m->append(liste[i]->negate(lisp));
    }
    return m;
}

template<> Element* Tenseur_short::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    int16_t sz = positions.size();
    if (!sz || sz > shape.size())
        sent_error("Error: index mismatch");

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            sent_error("Error: indexes out of bounds");
    }
    Element* result = lisp->provideList();
    Element* res = storeRank(lisp, result, this, positions, 0);
    if (res != result)
        result->release();
    
    if (res->type == t_shorts || res->type == t_short)
        return res;
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_shorts) {
        Matrice_short* m = new Matrice_short((List*)res);
        res->release();
        return m;
    }
    Tenseur_short* ts = new Tenseur_short(lisp, (List*)res);
    res->release();
    return ts;
}

template<> Element* Tenseur_integer::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    int16_t sz = positions.size();
    if (!sz || sz > shape.size())
        sent_error("Error: index mismatch");

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            sent_error("Error: indexes out of bounds");
    }
    Element* result = lisp->provideList();
    Element* res = storeRank(lisp, result, this, positions, 0);
    if (res != result)
        result->release();
    
    if (res->type == t_integers || res->type == t_integer)
        return res;
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_integers) {
        Matrice_integer* m = new Matrice_integer((List*)res);
        res->release();
        return m;
    }
    Tenseur_integer* ts = new Tenseur_integer(lisp, (List*)res);
    res->release();
    return ts;
}

template<> Element* Tenseur_string::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    int16_t sz = positions.size();
    if (!sz || sz > shape.size())
        sent_error("Error: index mismatch");

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            sent_error("Error: indexes out of bounds");
    }
    Element* result = lisp->provideList();
    Element* res = storeRank(lisp, result, this, positions, 0);
    if (res != result)
        result->release();
    
    if (res->type == t_strings || res->type == t_string)
        return res;
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_strings) {
        Matrice_string* m = new Matrice_string((List*)res);
        res->release();
        return m;
    }
    Tenseur_string* ts = new Tenseur_string(lisp, (List*)res);
    res->release();
    return ts;
}

template<> Element* Tenseur_stringbyte::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    int16_t sz = positions.size();
    if (!sz || sz > shape.size())
        sent_error("Error: index mismatch");

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            sent_error("Error: indexes out of bounds");
    }
    Element* result = lisp->provideList();
    Element* res = storeRank(lisp, result, this, positions, 0);
    if (res != result)
        result->release();
    
    if (res->type == t_stringbytes || res->type == t_stringbyte)
        return res;
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_stringbytes) {
        Matrice_stringbyte* m = new Matrice_stringbyte((List*)res);
        res->release();
        return m;
    }
    Tenseur_stringbyte* ts = new Tenseur_stringbyte(lisp, (List*)res);
    res->release();
    return ts;
}

template<> Element* Tenseur_float::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    int16_t sz = positions.size();
    if (!sz || sz > shape.size())
        sent_error("Error: index mismatch");

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            sent_error("Error: indexes out of bounds");
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

template<> Element* Tenseur_number::rank(LispE* lisp, vecte<long>& positions) {
    //We get rid of the final negative values (useless)
    int16_t sz = positions.size();
    if (!sz || sz > shape.size())
        sent_error("Error: index mismatch");

    //Check positions
    for (long i = 0; i < sz; i++) {
        if (positions[i] != -1 && (positions[i] < 0 || positions[i] >= shape[i]))
            sent_error("Error: indexes out of bounds");
    }
    Element* result = lisp->provideList();
    Element* res = storeRank(lisp, result, this, positions, 0);
    if (res != result)
        result->release();
    
    if (res->type == t_numbers || res->type == t_number)
        return res;
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->type == t_numbers) {
        Matrice_number* m = new Matrice_number((List*)res);
        res->release();
        return m;
    }
    Tenseur_number* ts = new Tenseur_number(lisp, (List*)res);
    res->release();
    return ts;
}

template <typename A, lisp_code T, typename C> void Tenseur<A,T,C>::concatenate(LispE* lisp, Element* e) {
    if (e->isList()) {
        vecte<long> sz;
        e->getShape(sz);
        for (long i = 0; i < sz.size()-1; i++) {
            if (sz[i] != shape[i])
                sent_error_e("Error: Incompatible dimensions");
        }
    }
    
    concatenate(lisp, 0, this, e);
}


template <typename A, lisp_code T, typename C> void Tenseur<A,T,C>::build(LispE* lisp, long isz, Element* res, A n) {
    if (isz == shape.size()-2) {
        C lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = provide(lisp, shape[isz+1], n);
            res->append(lst);
        }
    }
    else {
        Tenseur<A,T,C>* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = new Tenseur<A,T,C>(isz+1, shape);
            res->append(lst);
            build(lisp, isz+1, lst, n);
        }
    }
}

template <typename A, lisp_code T, typename C> void Tenseur<A,T,C>::build(LispE* lisp, long isz, Element* res, Element* lst, long& idx) {
    if (isz == shape.size()-2) {
        C l;
        long i,j;
        for (i = 0; i < shape[isz]; i++) {
            l = provide(lisp);
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
            build(lisp, isz+1, l, lst, idx);
        }
    }
}

template <typename A, lisp_code T, typename C> void Tenseur<A,T,C>::build(LispE* lisp, long isz, Element* res) {
    if (isz == shape.size()-2) {
        C l;
        for (long i = 0; i < shape[isz]; i++) {
            l = provide(lisp);
            res->append(l);
            l->liste = ((C)liste[i])->liste;
        }
    }
    else {
        Tenseur<A,T,C>* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = new Tenseur<A,T,C>(isz+1, shape);
            res->append(l);
            build(lisp, isz+1,l);
        }
    }
}



//----------------------------------------------------------------------------
// The following methods need the actual template definition for instantiation
// They deal with basic operations on containers including tensors and matrices
//----------------------------------------------------------------------------

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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] - n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
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
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] = liste[i] - n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            Shorts* n;
            
            for (long m = 0; m < result->size_x; m++) {
                n = (Shorts*)result->index(m);
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
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            Integers* n;
            
            for (long m = 0; m < result->size_x; m++) {
                n = (Integers*)result->index(m);
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Shorts*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Integers*)result->index(m))->liste[i] += liste[i];
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            short v;
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Shorts*)result->index(m))->liste[i];
                    ((Shorts*)result->index(m))->liste[i] = liste[i] - v;
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            long v;
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Integers*)result->index(m))->liste[i];
                    ((Integers*)result->index(m))->liste[i] = liste[i] - v;
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Shorts*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Integers*)result->index(m))->liste[i] *= liste[i];
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            short v;
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Shorts*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Shorts*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            long v;
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Integers*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Integers*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Shorts*)result->index(m))->liste[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Integers*)result->index(m))->liste[i] += liste[i];
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            long v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Shorts*)result->index(m))->liste[i];
                    ((Shorts*)result->index(m))->liste[i] = liste[i] - v;
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            long v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Integers*)result->index(m))->liste[i];
                    ((Integers*)result->index(m))->liste[i] = liste[i] - v;
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Shorts*)result->index(m))->liste[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Integers*)result->index(m))->liste[i] *= liste[i];
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
        case t_matrix_number: {
            Matrice_number* result = new Matrice_number(lisp, (Matrice_number*)e);
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
        case t_matrix_short: {
            Matrice_short* result = new Matrice_short(lisp, (Matrice_short*)e);
            short v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Shorts*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Shorts*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        case t_matrix_integer: {
            Matrice_integer* result = new Matrice_integer(lisp, (Matrice_integer*)e);
            long v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Integers*)result->index(m))->liste[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Integers*)result->index(m))->liste[i] = liste[i] / v;
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}


//----------------------------------------------------------------------------
// The following methods need the actual template definition for instantiation
//----------------------------------------------------------------------------

// (. '+ '* '((1 2) (5 4) (3 0)) '((6 2 3 4) (7 0 1 8)))
// (. '+ '* (iota 10) (iota 10.0))
//(setq m1 (rho 3 2 '(1 2 5 4 3 0)))
//(setq m2 (rho 2 4 '(6 2 3 4 7 0 1 8)))
Element* List_innerproduct_eval::eval(LispE* lisp) {
    Element* l1 = liste[3]->eval(lisp);
    
    Element* l2 = null_;
    Element* op1 = null_;
    Element* op2 = null_;
    Element* e = null_;
    bool sb = lisp->set_true_as_one();

    try {
        lisp->checkState(this);
        l2 = liste[4]->eval(lisp);
        
        long sx_1, sy_1;
        char t1 = l1->isPureList(sx_1, sy_1);

        long sx_2, sy_2;
        char t2 = l2->isPureList(sx_2, sy_2);

        if (!t1 || !t2 || t1 != t2)
            throw new Error("Error: arguments for '.' must be compatible lists or matrices");
        
        op1 = liste[1]->eval(lisp);
        if (op1->type == l_equal)
            op1 = lisp->provideAtom(l_equalonezero);
        op2 = liste[2]->eval(lisp);
        if (op2->type == l_equal)
            op2 = lisp->provideAtom(l_equalonezero);

        if (t1 == a_flat_list) {
            if (sx_1 != sx_2)
                throw new Error("Error: lists should have the same size for '.'");
                                
            l1->increment();
            l2->increment();
            e = apply_op1_op2(lisp, op1, op2, l1, l2);
            l1->decrement();
            l2->decrement();
            op1->release();
            op2->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return e;
        }

        if (t1 == a_valuelist) {
            if (sx_1 != sx_2)
                throw new Error("Error: lists should have the same size for '.'");
                                
            if (l1 == l2) {
                if (l2->type == t_numbers) {
                    l2 = lisp->provideNumbers();
                    ((Numbers*)l2)->liste = ((Numbers*)l1)->liste;
                }
                else {
                    l2 = lisp->provideIntegers();
                    ((Integers*)l2)->liste = ((Integers*)l1)->liste;
                }
            }
            
            l1->increment();
            l2->increment();
            e = apply_op1_op2(lisp, op1, op2, l1, l2);
            l1->decrement();
            l2->decrement();
            op1->release();
            op2->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return e;
        }

        if (sy_1 != sx_2)
            throw new Error("Error: incompatible matrices");

        Element* l2_transposed;
        long i, j = 0;
        
        l2_transposed = l2->transposed(lisp);
        List* res;
        if (l1->type == l2->type && l1->type != l_list) {
            switch (l1->type) {
                case t_matrix_string:
                    res = new Matrice_string(lisp, sx_1, sy_2, U"");
                    break;
                case t_matrix_stringbyte:
                    res = new Matrice_stringbyte(lisp, sx_1, sy_2, "");
                    break;
                case t_matrix_short:
                    res = new Matrice_short(lisp, sx_1, sy_2, 0.0);
                    break;
                case t_matrix_float:
                    res = new Matrice_float(lisp, sx_1, sy_2, 0.0);
                    break;
                case t_matrix_integer:
                    res = new Matrice_integer(lisp, sx_1, sy_2, 0);
                    break;
                default:
                    res = new Matrice_number(lisp, sx_1, sy_2, 0.0);
            }
        }
        else
            res = new Matrice_number(lisp, sx_1, sy_2, 0.0);

        Element* row;
        //We are dealing with matrices...
        for (i = 0; i < sx_1; i++) {
            row = l1->index(i);
            for (j = 0; j < sy_2; j++) {
                e = apply_op1_op2(lisp, op1, op2, row, l2_transposed->index(j));
                res->index(i)->replacing(j, e);
                e->release();
            }
        }
        l1->release();
        l2->release();
        op1->release();
        op2->release();
        l2_transposed->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return res;
    }
    catch (Error* err) {
        lisp->reset_to_true(sb);
        l1->release();
        l2->release();
        op1->release();
        op2->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return null_;
}


// (, (rho 2 3 '(4 5 6 9)) (rho 2 3 (iota 10)))
// (, (rho 3 3 3 (iota 90)) -1)
// (, (rho 3 3 3 (iota 90)) (* (iota 3) -1))
// (, (rho 3 3 3 (iota 90)) (* (rho 3 3 (iota 10)) -1))

Element* List_concatenate_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);

    Element* second_element = null_;
    Element* res = null_;
    bool sb = lisp->set_true_as_one();

    long listsize = size();

    try {
        lisp->checkState(this);
        if (listsize == 2) {
            lisp->reset_to_true(sb);
            
            if (first_element->isValueList()) {
                lisp->reset_to_true(sb);
                lisp->resetStack();
                return first_element;
            }
            switch (first_element->type) {
                case t_matrix_string:
                case t_tensor_string: {
                    Strings* l = lisp->provideStrings();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->reset_to_true(sb);
                    lisp->resetStack();
                    return l;
                }
                case t_matrix_short:
                case t_tensor_short: {
                    Shorts* l = new Shorts();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->reset_to_true(sb);
                    lisp->resetStack();
                    return l;
                }
                case t_matrix_integer:
                case t_tensor_integer: {
                    Integers* l = lisp->provideIntegers();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->reset_to_true(sb);
                    lisp->resetStack();
                    return l;
                }
                case t_matrix_number:
                case t_tensor_number: {
                    Numbers* l = lisp->provideNumbers();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->reset_to_true(sb);
                    lisp->resetStack();
                    return l;
                }
                case t_matrix_float:
                case t_tensor_float: {
                    Floats* l = lisp->provideFloats();
                    first_element->flatten(lisp, l);
                    first_element->release();
                    lisp->reset_to_true(sb);
                    lisp->resetStack();
                    return l;
                }
                default: {
                    res = lisp->provideList();
                    first_element->flatten(lisp,(List*)res);
                    first_element->release();
                    lisp->reset_to_true(sb);
                    lisp->resetStack();
                    return res;
                }
            }
        }

        if (!first_element->isList()) {
            switch (first_element->type) {
                case t_integer:
                    second_element = lisp->provideIntegers();
                    break;
                case t_float:
                    second_element = lisp->provideFloats();
                    break;
                case t_number:
                    second_element = lisp->provideNumbers();
                    break;
                case t_string:
                    second_element = lisp->provideStrings();
                    break;
                case t_stringbyte:
                    second_element = new Stringbytes();
                    break;
                default:
                    second_element = lisp->provideList();
            }
            second_element->append(first_element);
            first_element->release();
            first_element = second_element;
        }
        
        second_element = liste[2]->eval(lisp);
        
        vecte<long> sz1;
        vecte<long> sz2;
        switch (first_element->type) {
            case t_matrix_string: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_string(lisp, sz1[0], sz1[1], U"");
                ((Matrice_string*)res)->setvalue((Matrice_string*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_string*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_matrix_stringbyte: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_stringbyte(lisp, sz1[0], sz1[1], "");
                ((Matrice_stringbyte*)res)->setvalue((Matrice_stringbyte*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_stringbyte*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_matrix_short: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_short(lisp, sz1[0], sz1[1], 0.0);
                ((Matrice_short*)res)->setvalue((Matrice_short*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_short*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_matrix_integer: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_integer(lisp, sz1[0], sz1[1], 0.0);
                ((Matrice_integer*)res)->setvalue((Matrice_integer*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_integer*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_matrix_number: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_number(lisp, sz1[0], sz1[1], 0.0);
                ((Matrice_number*)res)->setvalue((Matrice_number*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_number*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_matrix_float: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Matrice_float(lisp, sz1[0], sz1[1], 0.0);
                ((Matrice_float*)res)->setvalue((Matrice_float*)first_element);
                res->concatenate(lisp,second_element);
                if (sz2.size() == 2)
                    sz1.vecteur[1] += sz2[1];
                else
                    sz1.vecteur[1] += 1;
                ((Matrice_float*)res)->size_y = sz1[1];
                first_element->release();
                break;
            }
            case t_tensor_string: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_string(lisp, sz1, zero_);
                ((Tenseur_string*)res)->setvalue((Tenseur_string*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_string*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_string*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            case t_tensor_stringbyte: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_stringbyte(lisp, sz1, zero_);
                ((Tenseur_stringbyte*)res)->setvalue((Tenseur_stringbyte*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_stringbyte*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_stringbyte*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            case t_tensor_number: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_number(lisp, sz1, zero_);
                ((Tenseur_number*)res)->setvalue((Tenseur_number*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_number*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_number*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            case t_tensor_float: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_float(lisp, sz1, zero_);
                ((Tenseur_float*)res)->setvalue((Tenseur_float*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_float*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_float*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            case t_tensor_short: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_short(lisp, sz1, zero_);
                ((Tenseur_short*)res)->setvalue((Tenseur_short*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_short*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_short*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            case t_tensor_integer: {
                first_element->getShape(sz1);
                second_element->getShape(sz2);
                if (sz1.size() < sz2.size())
                    throw new Error("Error: Dimension error");
                res = new Tenseur_integer(lisp, sz1, zero_);
                ((Tenseur_integer*)res)->setvalue((Tenseur_integer*)first_element);
                res->concatenate(lisp, second_element);
                long i = 0;
                while (i < sz2.size() && sz1[i] == sz2[i]) i++;
                if (i == sz2.size())
                    ((Tenseur_integer*)res)->shape.vecteur[i] += 1;
                else
                    ((Tenseur_integer*)res)->shape.vecteur[i] += sz2[i];
                first_element->release();
                break;
            }
            default:
                res = first_element->duplicate();
                res->concatenate(lisp, second_element);
                if (res != first_element)
                    first_element->release();
        }

        lisp->reset_to_true(sb);
        second_element->release();
    }
    catch (Error* err) {
        lisp->reset_to_true(sb);
        res->release();
        second_element->release();
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return res;
}

Element* List_tensor_number_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    vecte<long> shape;
    long s;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_number) {
                Tenseur_number* ts = new Tenseur_number(lisp, (List*)e);
                e->release();
                lisp->resetStack();
                return ts;
            }
            if (!e->isList())
                throw new Error("Error: The first element should be a list");

            if (e->type >= t_tensor_string && e->type <= t_tensor_integer)
                e->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isNumber())
                    throw new Error("Error: this list should contain numbers");
            }

            Numbers l;
            e->flatten(lisp,&l);
            Tenseur_number* ts = new Tenseur_number(lisp, &l, shape);
            e->release();
            lisp->resetStack();
            return ts;
        }

        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Tenseur_number(lisp, shape, zero_);
}

Element* List_tensor_short_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    vecte<long> shape;
    long s;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_short) {
                Tenseur_short* ts = new Tenseur_short(lisp, (List*)e);
                e->release();
                lisp->resetStack();
                return ts;
            }
            if (!e->isList())
                throw new Error("Error: The first element should be a list");

            if (e->type >= t_tensor_string && e->type <= t_tensor_integer)
                e->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isNumber())
                    throw new Error("Error: this list should contain integers");
            }

            Shorts l;
            e->flatten(lisp,&l);
            Tenseur_short* ts = new Tenseur_short(lisp, &l, shape);
            e->release();
            lisp->resetStack();
            return ts;
        }

        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Tenseur_short(lisp, shape, zero_);
}

Element* List_tensor_string_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    vecte<long> shape;
    long s;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_string) {
                Tenseur_string* ts = new Tenseur_string(lisp, (List*)e);
                e->release();
                lisp->resetStack();
                return ts;
            }
            if (!e->isList())
                throw new Error("Error: The first element should be a list");

            if (e->type >= t_tensor_string && e->type <= t_tensor_integer)
                e->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isString())
                    throw new Error("Error: this list should contain strings");
            }

            Strings l;
            e->flatten(lisp,&l);
            Tenseur_string* ts = new Tenseur_string(lisp, &l, shape);
            e->release();
            lisp->resetStack();
            return ts;
        }

        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Tenseur_string(lisp, shape, emptystring_);
}

Element* List_tensor_stringbyte_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    vecte<long> shape;
    long s;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_stringbyte) {
                Tenseur_stringbyte* ts = new Tenseur_stringbyte(lisp, (List*)e);
                e->release();
                lisp->resetStack();
                return ts;
            }
            if (!e->isList())
                throw new Error("Error: The first element should be a list");

            if (e->type >= t_tensor_string && e->type <= t_tensor_integer)
                e->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isString())
                    throw new Error("Error: this list should contain strings");
            }

            Stringbytes l;
            e->flatten(lisp,&l);
            Tenseur_stringbyte* ts = new Tenseur_stringbyte(lisp, &l, shape);
            e->release();
            lisp->resetStack();
            return ts;
        }

        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Tenseur_stringbyte(lisp, shape, emptystring_);
}


Element* List_tensor_integer_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    vecte<long> shape;
    long s;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_integer) {
                Tenseur_integer* ts = new Tenseur_integer(lisp, (List*)e);
                e->release();
                lisp->resetStack();
                return ts;
            }
            if (!e->isList())
                throw new Error("Error: The first element should be a list");

            if (e->type >= t_tensor_string && e->type <= t_tensor_integer)
                e->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isNumber())
                    throw new Error("Error: this list should contain integers");
            }

            Integers l;
            e->flatten(lisp,&l);
            Tenseur_integer* ts = new Tenseur_integer(lisp, &l, shape);
            e->release();
            lisp->resetStack();
            return ts;
        }

        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Tenseur_integer(lisp, shape, zero_);
}

Element* List_tensor_float_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    vecte<long> shape;
    long s;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_tensor_float) {
                Tenseur_float* ts = new Tenseur_float(lisp, (Tenseur_float*)e);
                e->release();
                lisp->resetStack();
                return ts;
            }
            if (!e->isList())
                throw new Error("Error: The first element should be a list");
            if (e->type >= t_tensor_string && e->type <= t_tensor_integer)
                e->getShape(shape);
            else {
                Element* c = e;
                while (c->isList()) {
                    shape.push_back(c->size());
                    c = c->index(0);
                }
                if (!c->isNumber())
                    throw new Error("Error: this list should contain numbers");
            }
            Floats l;
            e->flatten(lisp,&l);
            Tenseur_float* ts = new Tenseur_float(lisp, &l, shape);
            e->release();
            lisp->resetStack();
            return ts;
        }

        for (long i = 1; i < sz; i++) {
            evalAsInteger(i, lisp, s);
            shape.push_back(s);
        }
    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Tenseur_float(lisp, shape, zero_);
}

Element* List_to_tensor_eval::eval(LispE* lisp) {
    Element* values = liste[1]->eval(lisp);
    //First we check if we can guess a list of value lists or of tensors
    if (!values->isList()) {
        values->release();
        throw new Error("Error: Expecting a list");
    }

    if (values->isTensor() || values->isValueList())
        return values;
    
    long sz = values->size();
    if (!sz || sz == 1)
        return values;
    
    //We check if all the elements in list are of the same type
    Element* result = values->index(0);
    char char_tensor = result->isPureList();
    if (char_tensor == a_valuelist || char_tensor == a_tensor) {
        for (long i = 1; i < sz && (char_tensor == a_valuelist || char_tensor == a_tensor); i++) {
            char_tensor |= !result->is_same_tensor(values->index(i));
        }
        
        if (char_tensor == a_valuelist || char_tensor == a_tensor) {
            Element* tensor = result->newTensor(lisp, (List*)values);
            values->release();
            return tensor;
        }
        return values;
    }
    
    vecte<long> shape;
    values->getShape(shape);
    short element_type = v_null;
    
    if (values->structuredList(0, shape, element_type)) {
        Element* val;
        switch (element_type) {
            case t_short: {
                val = new Shorts();
                values->flatten(lisp, (Shorts*)val);
                break;
            }
            case t_integer: {
                val = lisp->provideIntegers();
                values->flatten(lisp, (Integers*)val);
                break;
            }
            case t_float: {
                val = lisp->provideFloats();
                values->flatten(lisp, (Floats*)val);
                break;
            }
            case t_number: {
                val = lisp->provideNumbers();
                values->flatten(lisp, (Numbers*)val);
                break;
            }
            case t_string: {
                val = lisp->provideStrings();
                values->flatten(lisp, (Strings*)val);
                break;
            }
            case t_stringbyte: {
                val = new Stringbytes();
                values->flatten(lisp, (Stringbytes*)val);
                break;
            }
            default:
                values->release();
                throw new Error("Error: cannot apply to_tensor to this object");
        }
        switch (shape.size()) {
            case 1:
                values->release();
                return val;
            case 2:
                switch (element_type) {
                    case t_short: {
                        result = new Matrice_short(lisp, shape[0], (Shorts*)val, shape[1]);
                        break;
                    }
                    case t_integer: {
                        result = new Matrice_integer(lisp, shape[0], (Integers*)val, shape[1]);
                        break;
                    }
                    case t_float: {
                        result = new Matrice_float(lisp, shape[0], (Floats*)val, shape[1]);
                        break;
                    }
                    case t_number: {
                        result = new Matrice_number(lisp, shape[0], (Numbers*)val, shape[1]);
                        break;
                    }
                    case t_string: {
                        result = new Matrice_string(lisp, shape[0], (Strings*)val, shape[1]);
                        break;
                    }
                    case t_stringbyte: {
                        result = new Matrice_stringbyte(lisp, shape[0], (Stringbytes*)val, shape[1]);
                        break;
                    }
                }
                break;
            default:
                switch (element_type) {
                    case t_short:
                        result = new Tenseur_short(shape, (Shorts*)val);
                        break;
                    case t_integer: {
                        result = new Tenseur_integer(shape, (Integers*)val);
                        break;
                    }
                    case t_float: {
                        result = new Tenseur_float(shape, (Floats*)val);
                        break;
                    }
                    case t_number: {
                        result = new Tenseur_number(shape, (Numbers*)val);
                        break;
                    }
                    case t_string: {
                        result = new Tenseur_string(shape, (Strings*)val);
                        break;
                    }
                    case t_stringbyte: {
                        result = new Tenseur_stringbyte(shape, (Stringbytes*)val);
                        break;
                    }
                }
        }
        val->release();
        values->release();
        return result;
    }
    return values;
}

Element* List_matrix_string_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    long sx, sy;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->isMatrix()) {
                Matrice_string* m = new Matrice_string(lisp, (Matrix*)e);
                e->release();
                lisp->resetStack();
                return m;
            }
            
            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");

            long size_x = e->size();
            long size_y = e->index(0)->size();
            Strings l;
            e->flatten(lisp, &l);
            Matrice_string* m = new Matrice_string(lisp, &l, size_x, size_y);
            e->release();
            lisp->resetStack();
            return m;
        }

        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);

    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Matrice_string(sx, sy, e);
}

Element* List_matrix_stringbyte_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    long sx, sy;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->isMatrix()) {
                Matrice_stringbyte* m = new Matrice_stringbyte(lisp, (Matrix*)e);
                e->release();
                lisp->resetStack();
                return m;
            }
            
            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");

            long size_x = e->size();
            long size_y = e->index(0)->size();
            Stringbytes l;
            e->flatten(lisp, &l);
            Matrice_string* m = new Matrice_string(lisp, &l, size_x, size_y);
            e->release();
            lisp->resetStack();
            return m;
        }

        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);

    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Matrice_stringbyte(sx, sy, e);
}

Element* List_matrix_number_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    long sx, sy;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->isMatrix()) {
                Matrice_number* m = new Matrice_number(lisp, (Matrix*)e);
                e->release();
                lisp->resetStack();
                return m;
            }
            
            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");

            long size_x = e->size();
            long size_y = e->index(0)->size();
            Numbers l;
            e->flatten(lisp, &l);
            Matrice_number* m = new Matrice_number(lisp, &l, size_x, size_y);
            e->release();
            lisp->resetStack();
            return m;
        }

        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);

    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Matrice_number(sx, sy, e);
}


Element* List_matrix_short_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    long sx, sy;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->isMatrix()) {
                Matrice_short* m = new Matrice_short(lisp, (Matrix*)e);
                e->release();
                lisp->resetStack();
                return m;
            }
            
            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");

            long size_x = e->size();
            long size_y = e->index(0)->size();
            Shorts l;
            e->flatten(lisp, &l);
            Matrice_short* m = new Matrice_short(lisp, &l, size_x, size_y);
            e->release();
            lisp->resetStack();
            return m;
        }

        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);

    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Matrice_short(sx, sy, e);
}

Element* List_matrix_integer_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    long sx, sy;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->isMatrix()) {
                Matrice_integer* m = new Matrice_integer(lisp, (Matrix*)e);
                e->release();
                lisp->resetStack();
                return m;
            }
            
            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");

            long size_x = e->size();
            long size_y = e->index(0)->size();
            Integers l;
            e->flatten(lisp, &l);
            Matrice_integer* m = new Matrice_integer(lisp, &l, size_x, size_y);
            e->release();
            lisp->resetStack();
            return m;
        }

        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);

    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Matrice_integer(sx, sy, e);
}

Element* List_matrix_float_eval::eval(LispE* lisp) {
    long sz = size();
    Element* e = zero_;
    long sx, sy;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            //then this is a list of lists
            e = liste[1]->eval(lisp);
            if (e->isMatrix()) {
                Matrice_float* m = new Matrice_float(lisp, (Matrix*)e);
                e->release();
                lisp->resetStack();
                return m;
            }
            
            if (!e->isList() || !e->index(0)->isList() || !e->index(0)->index(0)->isNumber())
                throw new Error("Error: Cannot initialize a matrix with this value");

            long size_x = e->size();
            long size_y = e->index(0)->size();
            Floats l;
            e->flatten(lisp, &l);
            Matrice_float* m = new Matrice_float(lisp, &l, size_x, size_y);
            e->release();
            lisp->resetStack();
            return m;
        }

        evalAsInteger(1, lisp, sx);
        evalAsInteger(2, lisp, sy);
        if (sz == 4)
            e = liste[3]->eval(lisp);

    }
    catch (Error* err) {
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return new Matrice_float(sx, sy, e);
}

Element* List_rho_eval::eval(LispE* lisp) {
    long listsize =  size();
    
    Element* e =  null_;
    Element* res = null_;
    bool sb = lisp->set_true_as_one();

    try {
        lisp->checkState(this);
        if (listsize == 2) {
            //In this case we return the shape of our list
            e = liste[1]->eval(lisp);
            res = lisp->provideIntegers();
            if (e->isList()) {
                vecte<long> shape;
                e->getShape(shape);
                ((Integers*)res)->liste = shape;
            }
            else
                ((Integers*)res)->liste.push_back(e->size());
            e->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }
        
        long ei = 0;
        long sz1;

        vecte<long> shape;
        e = liste[1]->eval(lisp);
        if (e->isInteger()) {
            shape.push_back(e->asInteger());
            e->release();
            for (long i = 2; i < listsize-1; i++) {
                evalAsInteger(i, lisp, sz1);
                shape.push_back(sz1);
            }
        }
        else {
            if (!e->isList() || listsize != 3)
                throw new Error("Error: Shape elements are either integers or a list of integers");
            for (long i = 0; i < e->size(); i++)
                shape.push_back(e->index(i)->asInteger());
            listsize += shape.size()-1;
            e->release();
        }

        e = liste.back()->eval(lisp);
        if (!e->isList())
            throw new Error("Error: last argument should be a list");

        if (listsize == 3) {
            sz1 = shape[0];
            switch (e->type) {
                case t_floats: {
                    listsize = e->size();
                    res = lisp->provideFloats();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        float v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asFloat();
                        for (long i = 0; i < sz1; i++) {
                            ((Floats*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Floats*)res)->liste.push_back(e->index(ei++)->asFloat());
                    }
                    break;
                }
                case t_numbers: {
                    listsize = e->size();
                    res = lisp->provideNumbers();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        double v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asNumber();
                        for (long i = 0; i < sz1; i++) {
                            ((Numbers*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Numbers*)res)->liste.push_back(e->index(ei++)->asNumber());
                    }
                    break;
                }
                case t_shorts: {
                    listsize = e->size();
                    res = new Shorts();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        int16_t v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asShort();
                        for (long i = 0; i < sz1; i++) {
                            ((Shorts*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Shorts*)res)->liste.push_back(e->index(ei++)->asShort());
                    }
                    break;
                }
                case t_integers: {
                    listsize = e->size();
                    res = lisp->provideIntegers();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        long v = 0;
                        if (listsize == 1)
                            v = e->index(0)->asInteger();
                        for (long i = 0; i < sz1; i++) {
                            ((Integers*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Integers*)res)->liste.push_back(e->index(ei++)->asInteger());
                    }
                    break;
                }
                case t_strings: {
                    listsize = e->size();
                    res = lisp->provideStrings();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        u_ustring v;
                        if (listsize == 1)
                            v = e->index(0)->asUString(lisp);
                        for (long i = 0; i < sz1; i++) {
                            ((Strings*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Strings*)res)->liste.push_back(e->index(ei++)->asUString(lisp));
                    }
                    break;
                }
                case t_stringbytes: {
                    listsize = e->size();
                    res = new Stringbytes();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        string v;
                        if (listsize == 1)
                            v = e->index(0)->toString(lisp);
                        for (long i = 0; i < sz1; i++) {
                            ((Stringbytes*)res)->liste.push_back(v);
                        }
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        ((Stringbytes*)res)->liste.push_back(e->index(ei++)->toString(lisp));
                    }
                    break;
                }
                case t_list: {
                    listsize = e->size();
                    res = lisp->provideList();
                    res->reserve(sz1);
                    if (listsize <= 1) {
                        Element* v = zero_;
                        if (listsize == 1)
                            v = e->index(0);
                        for (long i = 0; i < sz1; i++)
                            res->append(v);
                        break;
                    }
                    
                    for (long i = 0; i < sz1; i++) {
                        if (ei == listsize)
                            ei = 0;
                        res->append(e->index(ei++));
                    }
                    break;
                }
                case t_llist: {
                    res = new LList(&lisp->delegation->mark);
                    u_link* u = ((LList*)e)->liste.begin();
                    if (u == NULL) {
                        Element* v = zero_;
                        for (long i = 0; i < sz1; i++)
                            ((LList*)res)->push_front(v);
                        break;
                    }
                    else {
                        if (u->_next == NULL) {
                            Element* v = u->value->copying(false);
                            for (long i = 0; i < sz1; i++)
                                ((LList*)res)->push_front(v);
                        }
                        else {
                            for (long i = 0; i < sz1; i++) {
                                if (u == NULL)
                                    u = ((LList*)e)->liste.begin();
                                ((LList*)res)->push_front(u->value->copying(false));
                                u = u->next();
                            }
                        }
                    }
                }
            }
            e->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }
        
        if (listsize == 4) {
            sz1 = shape[0];
            long sz2 = shape[1];
            
            switch (e->type) {
                case t_strings:
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideStrings(1, U"");
                    }
                    res = new Matrice_string(lisp, e, sz1, sz2);
                    break;
                case t_stringbytes:
                    if (e->isEmpty()) {
                        e->release();
                        e = new Stringbytes(1, "");
                    }
                    res = new Matrice_stringbyte(lisp, e, sz1, sz2);
                    break;
                case t_shorts:
                    if (e->isEmpty()) {
                        e->release();
                        e = new Shorts(1, 0);
                    }
                    res = new Matrice_short(lisp, e, sz1, sz2);
                    break;
                case t_integers:
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideIntegers(1, 0);
                    }
                    res = new Matrice_integer(lisp, e, sz1, sz2);
                    break;
                case t_floats:
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideFloats(1, 0);
                    }
                    res = new Matrice_float(lisp, e, sz1, sz2);
                    break;
                case t_numbers:
                    if (e->isEmpty()) {
                        e->release();
                        e = lisp->provideIntegers(1, 0);
                    }
                    res = new Matrice_number(lisp, e, sz1, sz2);
                    break;
                case t_list: {
                    vecte<long> shape;
                    shape.push_back(sz1);
                    shape.push_back(sz2);
                    res = new List;
                    sz1 = 0;
                    ((List*)res)->build(lisp,shape, 0, res, e, sz1);
                    break;
                }
                case t_llist: {
                    vecte<long> shape;
                    shape.push_back(sz1);
                    shape.push_back(sz2);
                    res = new LList(&lisp->delegation->mark);
                    u_link* u = ((LList*)e)->liste.begin();
                    if (u == NULL) {
                        e->release();
                        e = new LList(&lisp->delegation->mark);
                        e->append(zero_);
                        u = ((LList*)e)->liste.begin();
                    }
                    ((LList*)res)->build(lisp,shape, 0, (LList*)res, (LList*)e, &u);
                    break;
                }
            }
            
            e->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }

        switch (e->type) {
            case t_shorts:
                if (e->isEmpty()) {
                    e->release();
                    e = new Shorts(1, 0);
                }
                res = new Tenseur_short(lisp, e, shape);
                break;
            case t_integers:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideIntegers(1, 0);
                }
                res = new Tenseur_integer(lisp, e, shape);
                break;
            case t_floats:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideFloats(1, 0);
                }
                res = new Tenseur_float(lisp, e, shape);
                break;
            case t_numbers:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideNumbers(1, 0);
                }
                res = new Tenseur_number(lisp, e, shape);
                break;
            case t_strings:
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideStrings(1, U"");
                }
                res = new Tenseur_string(lisp, e, shape);
                break;
            case t_stringbytes:
                if (e->isEmpty()) {
                    e->release();
                    e = new Stringbytes(1, "");
                }
                res = new Tenseur_stringbyte(lisp, e, shape);
                break;
            case t_list: {
                if (e->isEmpty()) {
                    e->release();
                    e = lisp->provideIntegers(1, 0);
                    res = new Tenseur_number(lisp, e, shape);
                }
                else {
                    res = new List;
                    sz1 = 0;
                    ((List*)res)->build(lisp,shape, 0,res, e, sz1);
                }
                break;
            }
            case t_llist: {
                res = new LList(&lisp->delegation->mark);
                u_link* u = ((LList*)e)->liste.begin();
                if (u == NULL) {
                    e->release();
                    e = new LList(&lisp->delegation->mark);
                    e->append(zero_);
                    u = ((LList*)e)->liste.begin();
                }
                ((LList*)res)->build(lisp,shape, 0, (LList*)res, (LList*)e, &u);
                break;
            }
        }

        e->release();
        lisp->reset_to_true(sb);
    }
    catch (Error* err) {
        e->release();
        res->release();
        lisp->reset_to_true(sb);
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return res;
}

Element* List_solve_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    Element* Y;
    Element* res;
    try {
        lisp->checkState(this);
        Y = liste[2]->eval(lisp);
        if (element->type != Y->type)
            throw new Error("Error: solve can only be applied to matrices of same type");
        
        switch (element->type) {
            case t_matrix_float:
                res = ((Matrice_float*)element)->solve(lisp, (Matrice_float*)Y);
                break;
            case t_matrix_number:
                res = ((Matrice_number*)element)->solve(lisp, (Matrice_number*)Y);
                break;
            case t_matrix_integer:
                res = ((Matrice_integer*)element)->solve(lisp, (Matrice_integer*)Y);
                break;
            default:
                throw new Error("Error: solve can only be applied to matrices");
        }
        
        Y->release();
        element->release();
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return res;
}


Element* List_ludcmp_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    Element* res;
    
    switch (element->type) {
        case t_matrix_float:
            res = ((Matrice_float*)element)->ludcmp(lisp);
            break;
        case t_matrix_number:
            res = ((Matrice_number*)element)->ludcmp(lisp);
            break;
        case t_matrix_integer:
            res = ((Matrice_integer*)element)->ludcmp(lisp);
            break;
        default:
            element->release();
            throw new Error("Error: solve can only be applied to matrices");
    }

    element->release();
    return res;
}


Element* List_lubksb_eval::eval(LispE* lisp) {
    Element* element = null_;
    Element* idxs = null_;
    Element* Y = NULL;

    try {
        lisp->checkState(this);
        idxs = liste[2]->eval(lisp);
        if (idxs->type != t_integers)
            throw new Error("Error: the second element should be an integers_ (a list of integers)");
        
        element = liste[1]->eval(lisp);
        if (liste.size() == 4) {
            Y = liste[3]->eval(lisp);
            if (element->type != Y->type)
                throw new Error("Error: solve can only be applied to matrices of same type");
        }

        switch (element->type) {
            case t_matrix_float:
                Y = ((Matrice_float*)element)->lubksb(lisp, (Integers*)idxs, (Matrice_float*)Y);
                break;
            case t_matrix_number:
                Y = ((Matrice_number*)element)->lubksb(lisp, (Integers*)idxs, (Matrice_number*)Y);
                break;
            case t_matrix_integer:
                Y = ((Matrice_integer*)element)->lubksb(lisp, (Integers*)idxs, (Matrice_integer*)Y);
                break;
            default:
                throw new Error("Error: solve can only be applied to matrices");
        }

        element->release();
        idxs->release();
    }
    catch (Error* err) {
        element->release();
        idxs->release();
        if (Y != NULL) {
            Y->release();
        }
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return Y;
}

Element* List_invert_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (element->type != t_matrix_number) {
        element->release();
        throw new Error("Error: 'invert' can only be applied to matrices");
    }

    Element* Y;
    Element* res;

    try {
        lisp->checkState(this);
        if (liste.size() == 3) {
            Y = liste[2]->eval(lisp);
            if (element->type != Y->type)
                throw new Error("Error: solve can only be applied to matrices of same type");
            
            switch (element->type) {
                case t_matrix_float:
                    res = ((Matrice_float*)element)->solve(lisp, (Matrice_float*)Y);
                    break;
                case t_matrix_number:
                    res = ((Matrice_number*)element)->solve(lisp, (Matrice_number*)Y);
                    break;
                case t_matrix_integer:
                    res = ((Matrice_integer*)element)->solve(lisp, (Matrice_integer*)Y);
                    break;
                default:
                    throw new Error("Error: solve can only be applied to matrices");
            }
            
            Y->release();
        }
        else {
            switch (element->type) {
                case t_matrix_float:
                    res = ((Matrice_float*)element)->inversion(lisp);
                    break;
                case t_matrix_number:
                    res = ((Matrice_number*)element)->inversion(lisp);
                    break;
                case t_matrix_integer:
                    res = ((Matrice_integer*)element)->inversion(lisp);
                    break;
                default:
                    throw new Error("Error: inversion can only be applied to matrices");
            }

        }
        element->release();
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return res;
}
