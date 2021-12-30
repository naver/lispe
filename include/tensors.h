//
//  tensors.hpp
//  LispE
//
//  Created by Claude Roux on 27/12/2021.
//  Copyright © 2021 Claude Roux. All rights reserved.
//

#ifndef tensors_hpp
#define tensors_hpp

#include "listes.h"


class Matrix : public List {
public:
    long size_x, size_y;

    Matrix(lisp_code t) {
        type = t_matrix;
    }
    
    Matrix(lisp_code t, long x, long y, Element* n) {
        type = t;
        size_x = x;
        size_y = y;
        for (long i = 0; i < size_x; i++) {
            append(newvalues(n));
        }
    }

    Matrix(lisp_code t, LispE* lisp, Element* lst, long x, long y) {
        type = t;
        size_x = x;
        size_y = y;
        build(lisp, lst);
    }

    Matrix(lisp_code t, long x, long y, double n) {
        type = t;
        size_x = x;
        size_y = y;

        for (long i = 0; i < size_x; i++) {
            append(newvalues(n));
        }
    }

    Matrix(lisp_code t, LispE* lisp, long x, long y, double n);
    Matrix(LispE* lisp, Matrix* m);

    //We steal the ITEM structure of this list
    Matrix(lisp_code t, List* l) : List(l,0) {
        type = t;
        size_x = l->size();
        size_y = l->index(0)->size();
    }

    Matrix(Matrix* l) : List(l,0) {
        type = l->type;
        size_x = l->size();
        size_y = l->index(0)->size();
    }

    Element* newvalues(double n) {
        switch (type) {
            case t_matrix_short:
                return new Shorts(size_y, (short)n);
            case t_matrix_integer:
                return new Integers(size_y, (long)n);
            case t_matrix_float:
                return new Floats(size_y, (float)n);
            default:
                return new Numbers(size_y, n);
        }
    }
    
    Element* newvalues(Element* n) {
        switch (type) {
            case t_matrix_short:
                return new Shorts(size_y, n->asShort());
            case t_matrix_integer:
                return new Integers(size_y, n->asInteger());
            case t_matrix_float:
                return new Floats(size_y, n->asFloat());
            default:
                return new Numbers(size_y, n->asNumber());
        }
    }
    
    Element* newvalues(LispE* lisp) {
        switch (type) {
            case t_matrix_short:
                return new Shorts();
            case t_matrix_integer:
                return lisp->provideIntegers();
            case t_matrix_float:
                return lisp->provideFloats();
            default:
                return lisp->provideNumbers();
        }
    }
    
    Element* newvalues(LispE* lisp, double n) {
        switch (type) {
            case t_matrix_short:
                return new Shorts(size_y, (short)n);
            case t_matrix_integer:
                return lisp->provideIntegers(size_y, (long)n);
            case t_matrix_float:
                return lisp->provideFloats(size_y, (float)n);
            default:
                return lisp->provideNumbers(size_y, n);
        }
    }
    
    Element* newvalues(LispE* lisp, long i) {
        switch (type) {
            case t_matrix_short:
                return new Shorts((Shorts*)liste[i]);
            case t_matrix_integer:
                return lisp->provideIntegers((Integers*)liste[i]);
            case t_matrix_float:
                return lisp->provideFloats((Floats*)liste[i]);
            default:
                return lisp->provideNumbers((Numbers*)liste[i]);
        }
    }
    
    long shapesize() {
        return 2;
    }

    Element* check_member(LispE*, Element* the_set);
    
    Element* loop(LispE* lisp, short label,  List* code);
    
    inline double val(long i, long j) {
        switch (type) {
            case t_matrix_short:
                return ((Shorts*)liste[i])->liste[j];
            case t_matrix_integer:
                return ((Integers*)liste[i])->liste[j];
            case t_matrix_float:
                return ((Floats*)liste[i])->liste[j];
            default:
                return ((Numbers*)liste[i])->liste[j];
        }
    }

    inline Element* indexe(long i, long j) {
        return liste[i]->index(j);
    }
    
    inline void set(long i, long j, double v) {
        switch (type) {
            case t_matrix_short:
                ((Shorts*)liste[i])->liste.at(j,v);
                break;
            case t_matrix_integer:
                ((Integers*)liste[i])->liste.at(j,v);
                break;
            case t_matrix_float:
                ((Floats*)liste[i])->liste.at(j,v);
                break;
            default:
                ((Numbers*)liste[i])->liste.at(j,v);
        }
    }
    
    inline void mult(long i, long j, double v) {
        switch (type) {
            case t_matrix_short:
                ((Shorts*)liste[i])->liste[j] *= (short)v;
                break;
            case t_matrix_integer:
                ((Integers*)liste[i])->liste[j] *= (long)v;
                break;
            case t_matrix_float:
                ((Floats*)liste[i])->liste[j] *= (float)v;
                break;
            default:
                ((Numbers*)liste[i])->liste[j] *= v;
        }
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
        if (!is_protected() && liste.nocdr() && !duplicate)
            return this;
        
        return new Matrix(this);
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant(bool pair = false) {
        if (status == s_constant)
            return new Matrix(this);
        return this;
    }

    Element* fullcopy() {
        return new Matrix(this);
    }

    Element* inversion(LispE* lisp);
    Element* solve(LispE* lisp, Matrix* Y);
    double determinant();
    Element* ludcmp(LispE* lisp);
    Element* lubksb(LispE* lisp, Integers* indexes, Matrix* Y = NULL);

    void build(LispE* lisp, Element* lst);

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
    
    void setvalue(Matrix* lst) {
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
    
    Element* rank(LispE* lisp, vecte<long>& positions);
    
    Element* newInstance(Element* e) {
        return new Matrix((lisp_code)type, size_x, size_y, e);
    }
    
    Element* newInstance() {
        return new Matrix((lisp_code)type, size_x, size_y, 0.0);
    }

};


class Tensor : public List {
public:
    vecte<long> shape;

    Tensor(lisp_code t) {
        type = t;
    }
    
    Tensor(lisp_code t, vecte<long>& sz, Element* n) {
        type = t;
        shape = sz;
        if (shape.size())
            build(0,this, n->asNumber());
    }
    
    Tensor(lisp_code t, vecte<long>& sz, double n) {
        type = t;
        shape = sz;
        if (shape.size())
            build(0,this, n);
    }

    Tensor(lisp_code t, LispE* lisp, vecte<long>& sz, Element* n) {
        type = t;
        shape = sz;
        if (shape.size())
            build(lisp, 0,this, n->asNumber());
    }
    
    Tensor(lisp_code t, LispE* lisp, Element* lst, vecte<long>& sz) {
        type = t;
        shape = sz;
        if (shape.size()) {
            long idx = 0;
            build(lisp, 0,this, lst, idx);
        }
    }

    Tensor(Tensor* tensor) {
        type = tensor->type;
        shape = tensor->shape;
        tensor->build(0, this);
    }

    //We steal the ITEM structure of this list
    Tensor(lisp_code t, LispE* lisp, List* l) : List(l, 0) {
        type = t;
        Element* e = l;
        while (e->isList()) {
            shape.push_back(e->size());
            e = e->index(0);
        }
    }

    Tensor(LispE* lisp, Tensor* l) : List(l, 0) {
        type = l->type;
        Element* e = l;
        while (e->isList()) {
            shape.push_back(e->size());
            e = e->index(0);
        }
    }


    Element* newvalues() {
        switch (type) {
            case t_tensor_short:
                return new Shorts();
            case t_tensor_integer:
                return new Integers();
            case t_tensor_float:
                return new Floats();
            default:
                return new Numbers();
        }
    }

    Element* newvalues(double n) {
        switch (type) {
            case t_tensor_short:
                return new Shorts(shape.back(), (short)n);
            case t_tensor_integer:
                return new Integers(shape.back(), (long)n);
            case t_tensor_float:
                return new Floats(shape.back(), (float)n);
            default:
                return new Numbers(shape.back(), n);
        }
    }
    
    Element* newvalues(Element* n) {
        switch (type) {
            case t_tensor_short:
                return new Shorts(shape.back(), n->asShort());
            case t_tensor_integer:
                return new Integers(shape.back(), n->asInteger());
            case t_tensor_float:
                return new Floats(shape.back(), n->asFloat());
            default:
                return new Numbers(shape.back(), n->asNumber());
        }
    }
    
    Element* newvalues(LispE* lisp) {
        switch (type) {
            case t_tensor_short:
                return new Shorts();
            case t_tensor_integer:
                return lisp->provideIntegers();
            case t_tensor_float:
                return lisp->provideFloats();
            default:
                return lisp->provideNumbers();
        }
    }
    
    Element* newvalues(LispE* lisp, double n) {
        switch (type) {
            case t_tensor_short:
                return new Shorts(shape.back(), (short)n);
            case t_tensor_integer:
                return lisp->provideIntegers(shape.back(), (long)n);
            case t_tensor_float:
                return lisp->provideFloats(shape.back(), (float)n);
            default:
                return lisp->provideNumbers(shape.back(), n);
        }
    }
    
    Element* newvalues(LispE* lisp, Element* values) {
        switch (type) {
            case t_tensor_short:
                return new Shorts((Shorts*)values);
            case t_tensor_integer:
                if (lisp == NULL)
                    return new Integers((Integers*)values);
                return lisp->provideIntegers((Integers*)values);
            case t_tensor_float:
                if (lisp == NULL)
                    return new Floats((Floats*)values);
                return lisp->provideFloats((Floats*)values);
            default:
                if (lisp == NULL)
                    return new Numbers((Numbers*)values);
                return lisp->provideNumbers((Numbers*)values);
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
    
    Element* duplicate_constant(bool pair = false) {
        if (status == s_constant)
            return new Tensor(this);
        return this;
    }
    
    Element* fullcopy() {
        return new Tensor(this);
    }

    Element* storeRank(Element* current, vecte<long>& positions, long idx) {
        bool last = false;
        if (idx == shape.size() - 1) {
            last = true;
        }
        
        long p_idx = -1;
        if (idx < positions.size())
            p_idx = positions[idx];
        
        if (p_idx == -1) {
            if (last)
                return newvalues(NULL, current);
            
            Element* result;
            Element* e = storeRank(current->index(0), positions, idx+1);
            if (e->isNumber())
                result = newvalues();
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
    
    Element* rank(LispE* lisp, vecte<long>& positions);
    
    void build(LispE* lisp, long isz, Element* res, double n);
    void build(LispE* lisp, long isz, Element* res, Element* lst, long& idx);
    void build(LispE* lisp, long isz, Element* res);

    
    void build(long isz, Element* res, double n) {
        if (isz == shape.size()-2) {
            Element* lst;
            for (long i = 0; i < shape[isz]; i++) {
                lst = newvalues(n);
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
            Element* l;
            long i,j;
            for (i = 0; i < shape[isz]; i++) {
                l = newvalues();
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
                l = new List;
                res->append(l);
                build(isz+1, l, lst, idx);
            }
        }
    }

    void build(long isz, Element* res) {
        if (isz == shape.size()-2) {
            Element* l;
            for (long i = 0; i < shape[isz]; i++) {
                l = newvalues(NULL, liste[i]);
                res->append(l);
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

    void combine(LispE* lisp, vecte<long>& isz1, vecte<long>& isz2, Element* l1, Element* l2, List* action) {
        if (!l1->isList() && !l2->isList()) {
            if (isz1.size() && isz2.size()) {
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
    
    void combine(LispE* lisp, Element* l1, Element* l2, List* action) {
        vecte<long> isz1;
        vecte<long> isz2;
        combine(lisp, isz1, isz2, l1, l2, action);
    }
    
    char isPureList(long& x, long& y) {
        x = shape[0];
        y = shape[1];
        return 1;
    }

    void getShape(vecte<long>& sz) {
        sz = shape;
    }
    
    char isPureList() {
        return 4;
    }
    
    Element* copying(bool duplicate = true) {
        //If it is a CDR, we need to copy it...
        if (!is_protected() && liste.nocdr() && !duplicate)
            return this;
        
        return new Tensor(this);
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
            vecte<long> sz;
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
    
    void setvalue(Tensor* lst) {
        setvalue(this, lst);
    }

    Element* newInstance() {
        return new Tensor((lisp_code)type, shape, 0.0);
    }

    Element* newInstance(Element* e) {
        return new Tensor((lisp_code)type, shape, e);
    }
};


#endif /* tensors_hpp */
