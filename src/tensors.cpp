//
//  tensors.cpp
//  LispE
//
//  Created by Claude Roux on 27/12/2021.
//  Copyright © 2021 Claude Roux. All rights reserved.
//

#include "lispe.h"
#include "tensors.h"

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


//LU decomposition
long LUDCMP(long n, vecte<long>& indexes, long& d, Matrix* m) {
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

void LUBKSB(long n, vecte<long>& indexes, vecte<double>& b_values, Matrix* m)  {
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

Element* Matrix::check_member(LispE* lisp,Element* the_set) {
    Matrix* r = new Matrix((lisp_code)type);
    r->size_x = size_x;
    r->size_y = size_y;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Matrix::transposed(LispE* lisp) {
    Matrix* transposed_matrix = new Matrix((lisp_code)type, lisp, size_y, size_x, 0.0);
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

Element* Matrix::rank(LispE* lisp, vecte<long>& positions) {
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
        
        switch (type) {
            case t_matrix_short:
                return new Shorts(((Shorts*)liste[positions[0]]));
            case t_matrix_integer:
                return lisp->provideIntegers(((Integers*)liste[positions[0]]));
            case t_matrix_float:
                return lisp->provideFloats(((Floats*)liste[positions[0]]));
            default:
                return lisp->provideNumbers(((Numbers*)liste[positions[0]]));
        }
    }
    
    if (sz == 1 || positions[1] >= size_y)
        throw new Error("Error: indexes out of bounds");
    
    Numbers* result = lisp->provideNumbers();
    for (long i = 0; i < size_x; i++) {
        result->liste.push_back(val(i,positions[1]));
    }
    return result;
}

Element* Matrix::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < size_x; i++) {
        lisp->replacingvalue(liste[i], label);
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

Element* Matrix::rotate(LispE* lisp, long axis) {
    Matrix* revert_matrix = new Matrix((lisp_code)type);
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
        e = newvalues(lisp, i);
        revert_matrix->append(e);
    }
    return revert_matrix;
}

Matrix::Matrix(LispE* lisp, Matrix* m) {
    type = m->type;
    size_x = m->size_x;
    size_y = m->size_y;
    Element* l;
    for (long i = 0; i < size_x; i++) {
        l = newvalues(lisp, i);
        append(l);
    }
}

Matrix::Matrix(lisp_code t, LispE* lisp, long x, long y, double n) {
    type = t;
    size_x = x;
    size_y = y;

    for (long i = 0; i < size_x; i++) {
        append(newvalues(lisp, n));
    }
}

void Matrix::build(LispE* lisp, Element* lst) {
    Element* l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = newvalues(lisp);
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            switch (type) {
                case t_matrix_short:
                    ((Shorts*)l)->liste.push_back(lst->index(idx++)->asShort());
                    break;
                case t_matrix_integer:
                    ((Integers*)l)->liste.push_back(lst->index(idx++)->asInteger());
                    break;
                case t_matrix_float:
                    ((Floats*)l)->liste.push_back(lst->index(idx++)->asFloat());
                    break;
                default:
                    ((Numbers*)l)->liste.push_back(lst->index(idx++)->asNumber());
            }
        }
    }
}

double Matrix::determinant() {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        throw new Error("Error: we can only apply 'determinant' to square Matrixs");
    
    long i;
    i = 0;
    double det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;

        Matrix sub(t_matrix_number, size_x - 1, size_y - 1, 0.0);

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
        det += val(i,j) * sg*sub.determinant();
    }
    return det;
}

Element* Matrix::inversion(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'invert' to square Matrixs");

    //else Local decomposition
    Matrix m(this);
    
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrix* Y = new Matrix(t_matrix_number, lisp, size_x, size_x, 0.0);
    
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

Element* Matrix::solve(LispE* lisp, Matrix* y) {
    if (size_x != size_y || y->size_x != y->size_y || size_x != y->size_x)
        throw new Error("Error: we can only apply 'solve' to square Matrixs of equal sizes");

    //else Local decomposition
    Matrix m(this);
        
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
        
    Matrix* Y = new Matrix(lisp, y);
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

Element* Matrix::ludcmp(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'ludcmp' to square Matrixs");

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

Element* Matrix::lubksb(LispE* lisp, Integers* idxs, Matrix* Y) {
    if (size_x != size_y || idxs->size() != size_x) {
        throw new Error("Error: we can only apply 'lubksb' to square Matrixs with the same number of indexes");
    }
    
    long i;
    if (Y == NULL) {
        Y = new Matrix(t_matrix_number, lisp, size_x, size_x, 0.0);
        //We create an identity matrix, which will contain the final result...
        for (i = 0; i < size_x; i++) {
            Y->set(i,i, 1);
        }
    }
    else {
        if (Y->size_x != size_x)
            throw new Error("Error: we can only apply 'lubksb' to square Matrixs of the same shape");
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



Element* Tensor::check_member(LispE* lisp, Element* the_set) {
    Tensor* r = new Tensor((lisp_code)type);
    r->shape = shape;
    Element* e;
    for (long i = 0; i < size(); i++) {
        e = liste[i]->check_member(lisp, the_set);
        r->append(e);
    }
    return r;
}

Element* Tensor::transposed(LispE* lisp) {
    vecte<long> sz;
    sz = shape;
    long i = sz[0];
    sz.vecteur[0] = sz[1];
    sz.vecteur[1] = i;
    
    Tensor* transposed_matrix = new Tensor((lisp_code)type, lisp, sz, zero_);
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

Element* Tensor::rank(LispE* lisp, vecte<long>& positions) {
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
    if (res->isNumber())
        return res;
    
    //We steal the ITEM structure of res
    //which is a very fast operation
    //Since its internal values are not copied but borrowed
    if (res->index(0)->isValueList()) {
        Matrix* m = new Matrix((lisp_code)type, (List*)res);
        res->release();
        return m;
    }
    Tensor* ts = new Tensor((lisp_code)type, lisp, (List*)res);
    res->release();
    return ts;
}

Element* Tensor::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    for (long i = 0; i < shape[0]; i++) {
        lisp->replacingvalue(liste[i], label);
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

Element* Tensor::reversion(LispE* lisp, Element* value, long pos, long axis, bool init) {
    if (pos == axis)
        return value->reverse(lisp,true);
    
    if (pos == shape.size() -1)
        return newvalues(lisp, value);
    
    Element* r;
    if (init) {
        r = new Tensor((lisp_code)type);
        ((Tensor*)r)->shape = shape;
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

Element* Tensor::rotate(LispE* lisp, long axis) {
    return reversion(lisp, this, 0, axis, true);
}

void Tensor::build(LispE* lisp, long isz, Element* res, double n) {
    if (isz == shape.size()-2) {
        Element* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = newvalues(lisp, n);
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

void Tensor::build(LispE* lisp, long isz, Element* res, Element* lst, long& idx) {
    if (isz == shape.size()-2) {
        Element* l;
        long i,j;
        for (i = 0; i < shape[isz]; i++) {
            l = newvalues(lisp);
            res->append(l);
            for (j = 0; j < shape[isz+1]; j++) {
                if (idx == lst->size())
                    idx = 0;
                switch (type) {
                    case t_tensor_short:
                        ((Shorts*)l)->liste.push_back(lst->index(idx++)->asShort());
                        break;
                    case t_tensor_integer:
                        ((Integers*)l)->liste.push_back(lst->index(idx++)->asInteger());
                        break;
                    case t_tensor_float:
                        ((Floats*)l)->liste.push_back(lst->index(idx++)->asFloat());
                        break;
                    default:
                        ((Numbers*)l)->liste.push_back(lst->index(idx++)->asNumber());
                }
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

void Tensor::build(LispE* lisp, long isz, Element* res) {
    if (isz == shape.size()-2) {
        Element* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = newvalues(lisp, i);
            res->append(l);
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
