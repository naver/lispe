/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  straight_eval.cxx
//
//

#include <stdio.h>
#include <sys/stat.h>
#include <stdlib.h>

#include "lispe.h"
#include"avl.h"
#include <math.h>
#include <algorithm>
#include <thread>
#include <chrono>

#ifdef __apple_build_version__
#define APPLE 1
#endif

Element* range(LispE* lisp, long init, long limit, long inc);
Element* range(LispE* lisp, double init, double limit, double inc);
Element* range(LispE* lisp, u_ustring& init, u_ustring& limit, long inc);

//------------------------------------------------------------------------------------------
Element* List_emptylist_eval::eval(LispE* lisp) {
    return emptylist_;
}


Element* List_and_eval::eval(LispE* lisp) {
    bool test = true;
    try {
        lisp->checkState(this);
        Element* element;
        for (long i = 1; i < size() && test; i++) {
            element = liste[i]->eval(lisp);
            test = element->Boolean();
            element->release();
        }
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return booleans_[test];
}

Element* List_bytes_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* cdr_result;

    try {
        lisp->checkState(this);
        cdr_result = container->bytes(lisp);
        container->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return cdr_result;
}

Element* List_cadr_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* cdr_result;

    try {
        lisp->checkState(this);
        cdr_result = liste[0]->cadr(lisp, container);
        if (!container->element_container(cdr_result))
            container->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return cdr_result;
}

Element* List_car_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* car_result;
    
    try {
        lisp->checkState(this);
        car_result = container->car(lisp);
        if (!container->element_container(car_result))
            container->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return car_result;
}

Element* List_check_eval::eval(LispE* lisp) {
    try {
        lisp->checkState(this);
        Element* element = liste[1]->eval(lisp);
        
        if (!element->Boolean()) {
            lisp->resetStack();
            element->release();
            return null_;
        }
        
        long listsize = liste.size();
        _releasing(element);
        liste.back()->setterminal(terminal);
        for (long i = 2; i < listsize && element->type != l_return; i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
        lisp->resetStack();
        return element;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}

Element* List_cond_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* first_element = liste[0];
    Element* second_element = null_;
    Element* third_element = null_;


    try {
        lisp->checkState(this);
        long szv;
        for (long i = 1; i < listsize; i++) {
            second_element = liste[i];
            szv = second_element->size();

            if (!second_element->isList() || szv <= 1)
                throw new Error(L"Error: in a 'cond' the first element must be a list");

            List* code = (List*)second_element;
            third_element = code->liste[0]->eval(lisp);
            if (third_element->Boolean()) {
                _releasing(third_element);
                first_element = null_;
                code->liste.back()->setterminal(terminal);
                for (long int j = 1; j < szv; j++) {
                    _releasing(first_element);
                    first_element = code->liste[j]->eval(lisp);
                }
                lisp->resetStack();
                return first_element;
            }
            _releasing(third_element);
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        third_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return null_;
}

Element* List_cons_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    if (first_element == emptylist_)
        first_element = null_;
    
    Element* second_element = null_;


    try {
        lisp->checkState(this);
        //merging an element into the next list
        second_element = liste[2]->eval(lisp);
        
        switch (second_element->type) {
            case t_floats:
            case t_shorts:
            case t_integers:
            case t_numbers:
            case t_stringbytes:
            case t_strings: {
                lisp->resetStack();
                second_element = second_element->copyatom(lisp, 1);
                second_element->insert(lisp, first_element, 0);
                first_element->release();
                return second_element;
            }
            case t_llist:
                lisp->resetStack();
                return second_element->insert(lisp, first_element, 0);
            case t_list: {
                second_element = second_element->duplicate_cdr(lisp);
                if (second_element->status) {
                    second_element->insert(lisp, first_element, 0);
                    Listpool* third_element = new Listpool(lisp, (List*)second_element, 0);
                    ((List*)second_element)->liste.home++;
                    lisp->resetStack();
                    return third_element;
                }
                lisp->resetStack();
                return second_element->insert(lisp, first_element, 0);
            default:
                if (second_element == null_ || second_element == emptylist_) {
                    List* third_element = lisp->provideList();
                    third_element->append(first_element->duplicate_constant(lisp));
                    lisp->resetStack();
                    return third_element;
                }

                LList* third_element = new LList(&lisp->delegation->mark);
                third_element->append(first_element->duplicate_constant(lisp));
                third_element->append_as_last(lisp, second_element->duplicate_constant(lisp));
                lisp->resetStack();
                return third_element;
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
}

Element* List_consb_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    if (first_element == emptylist_)
        first_element = null_;
    
    Element* second_element = null_;
    Element* result;


    try {
        lisp->checkState(this);
        //merging an element into the next list
        second_element = liste[2]->eval(lisp);
        if (first_element == null_ || first_element == emptylist_) {
            result = lisp->provideList();
            result->append(second_element->duplicate_constant(lisp));
            lisp->resetStack();
            return result;
        }

        if (!first_element->isList()) {
            switch (second_element->type) {
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                case t_stringbytes:
                case t_strings: {
                    lisp->resetStack();
                    second_element = second_element->copyatom(lisp, 1);
                    second_element->insert(lisp, first_element, 0);
                    first_element->release();
                    return second_element;
                }
                case t_llist:
                    lisp->resetStack();
                    return second_element->insert(lisp, first_element, 0);
                case t_list: {
                    second_element = second_element->duplicate_cdr(lisp);
                    if (second_element->status) {
                        second_element->insert(lisp, first_element, 0);
                        Element* third_element = new Listpool(lisp, (List*)second_element, 0);
                        ((List*)second_element)->liste.home++;
                        lisp->resetStack();
                        return third_element;
                    }
                    lisp->resetStack();
                    return second_element->insert(lisp, first_element, 0);
                }
                default:
                    result = new LList(&lisp->delegation->mark);
                    result->append(first_element->duplicate_constant(lisp));
                    ((LList*)result)->append_as_last(lisp, second_element->duplicate_constant(lisp));
                    lisp->resetStack();
                    return result;
            }
        }
        else {
            result = first_element->copyatom(lisp, 1);
            result->append(second_element->duplicate_constant(lisp));
        }

        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}

Element* List_slice_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    try {
        lisp->checkState(this);
        long index;
        long sz = container->size();
        evalAsInteger(2, lisp, index);
        if (container->isString()) {
            Strings* divided = lisp->provideStrings();
            u_ustring u = container->asUString(lisp);
            u_ustring sub;
            for (long i = 0; i < sz; i+=index) {
                sub = u.substr(i, index);
                divided->liste.push_back(sub);
            }
            container->release();
            lisp->resetStack();
            return divided;
        }
        List* divided = lisp->provideList();
        
        long j;
        for (long i = 0; i < sz; i += index) {
            List* sub = lisp->provideList();
            for (j = 0; j < index && (i+j) < sz; j++)
                sub->append(container->index(i + j)->copying());
            divided->append(sub);
        }
        container->release();
        lisp->resetStack();
        return divided;
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
}

Element* List_complex_eval::eval(LispE* lisp) {
    double d;
    evalAsNumber(1, lisp, d);
    double i;
    evalAsNumber(2, lisp, i);
    return lisp->provideComplex(d, i);
}

Element* List_real_eval::eval(LispE* lisp) {
    Element* e = liste[1]->eval(lisp);
    double d = e->asNumber();
    e->release();
    return lisp->provideNumber(d);
}

Element* List_imaginary_eval::eval(LispE* lisp) {
    Element* e = liste[1]->eval(lisp);
    if (e->type == t_complex) {
        double d = ((Complexe*)e)->content.imag();
        e->release();
        return lisp->provideNumber(d);
    }
    throw new Error("Error: expecting a complex value");
}


Element* List_integer_eval::eval(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = lisp->provideInteger(value->asInteger());
    value->release();
    return element;
}


Element* List_in_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* search_value;
    bool res = false;
    
    try {
        lisp->checkState(this);
        search_value = liste[2]->eval(lisp);
        res = container->check_element(lisp, search_value);
        container->release();
        search_value->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return booleans_[res];
}

Element* List_at_eval::eval(LispE* lisp) {
    long listsize = liste.size();

    Element* container = liste[1]->eval(lisp);
    Element* result = container;

    Element* value = null_;
    
    try {
        lisp->checkState(this);
        //The user might have provided a list of indexes
        //which we use to traverse a complex hierarchical structure...
        for (long i = 2; i < listsize; i++) {
            value = liste[i]->eval(lisp);
            result = result->protected_index(lisp, value);
            _releasing(value);
        }
        if (!container->element_container(result))
            container->release();
    }
    catch (Error* err) {
        value->release();
        container->release();
        if (container != result)
            result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}

Element* List_set_at_eval::eval(LispE* lisp) {
    long listsize = liste.size();

    Element* container = liste[1]->eval(lisp);
    Element* result = container;

    Element* value = null_;
    Element* ix;


    try {
        lisp->checkState(this);
        value = liste[listsize - 1]->eval(lisp)->copying(false);
        for (long i = 2; i < listsize - 2; i++) {
            ix = liste[i]->eval(lisp);
            result = result->protected_index(lisp, ix);
            ix->release();
        }
        ix = liste[listsize-2]->eval(lisp);
        result->replace(lisp, ix, value);
        value->release();
        ix->release();
        if (!container->element_container(result))
            container->release();
    }
    catch (Error* err) {
        value->release();
        container->release();
        if (container != result)
            result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}

Element* List_key_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty dictionary
        return lisp->provideDictionary();
    }

    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;


    try {
        lisp->checkState(this);
        if (listsize == 3 && first_element->isDictionary()) {
            //The second element is an a_key
            switch (first_element->type) {
                case t_tree:
                case t_dictionary: {
                    u_ustring a_key;
                    evalAsUString(2, lisp, a_key);
                    second_element = first_element->protected_index(lisp, a_key);
                    first_element->release();
                    lisp->resetStack();
                    return second_element;
                }
                case t_treei:
                case t_dictionaryi: {
                    long a_key;
                    evalAsInteger(2, lisp, a_key);
                    second_element = first_element->protected_index(lisp, a_key);
                    first_element->release();
                    lisp->resetStack();
                    return second_element;
                }
                case t_treen:
                case t_dictionaryn: {
                    double a_key;
                    evalAsNumber(2, lisp, a_key);
                    second_element = first_element->protected_index(lisp, a_key);
                    first_element->release();
                    lisp->resetStack();
                    return second_element;
                }
            }
        }

        long first = 2;
        if (first_element->isDictionary()) {
            if ((listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'key'");
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant(lisp);
        }
        else {
            if (!(listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'key'");
            first = 3;
            u_ustring a_key = first_element->asUString(lisp);
            first_element->release();
            first_element = lisp->provideDictionary();
            second_element = liste[2]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }

        //We store values
        switch (first_element->type) {
            case t_tree:
            case t_dictionary: {
                u_ustring a_key;
                for (long i = first; i < listsize; i+=2) {
                    evalAsUString(i, lisp, a_key);
                    second_element = liste[i+1]->eval(lisp);
                    first_element->recording(a_key, second_element->copying(false));
                }
                break;
            }
            case t_treei:
            case t_dictionaryi: {
                long a_key;
                for (long i = 2; i < listsize; i+=2) {
                    evalAsInteger(i, lisp, a_key);
                    second_element = liste[i+1]->eval(lisp);
                    first_element->recording(a_key, second_element->copying(false));
                }
                break;
            }
            case t_treen:
            case t_dictionaryn: {
                double a_key;
                for (long i = 2; i < listsize; i+=2) {
                    evalAsNumber(i, lisp, a_key);
                    second_element = liste[i+1]->eval(lisp);
                    first_element->recording(a_key, second_element->copying(false));
                }
                break;
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return first_element;
}

Element* List_keyi_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty dictionary
        return lisp->provideDictionary_i();
    }
    
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;


    try {
        lisp->checkState(this);
        long a_key;
        if (listsize == 3 && first_element->isDictionary()) {
            if (first_element->type != t_dictionaryi && first_element->type != t_treei)
                throw new Error("Error: wrong dictionary type for 'keyi'");

            evalAsInteger(2, lisp, a_key);
            second_element = first_element->protected_index(lisp, a_key);
            first_element->release();
            lisp->resetStack();
            return second_element;
        }

        long first;
        if (first_element->isDictionary()) {
            if (first_element->type != t_dictionaryi && first_element->type != t_treei)
                throw new Error("Error: wrong dictionary type for 'keyi'");

            if ((listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyi'");
            first = 2;
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant(lisp);
        }
        else {
            if (!(listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyi'");
            first = 3;
            a_key = first_element->asInteger();
            first_element->release();
            first_element = lisp->provideDictionary_i();
            second_element = liste[2]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }

        //We store the values
        for (long i = first; i < listsize; i+=2) {
            evalAsInteger(i, lisp, a_key);
            second_element = liste[i+1]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return first_element;
}

Element* List_keyn_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty dictionary
        return lisp->provideDictionary_n();
    }
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;


    try {
        lisp->checkState(this);
        double a_key;
        if (listsize == 3 && first_element->isDictionary()) {
            if (first_element->type != t_dictionaryn && first_element->type != t_treen)
                throw new Error("Error: wrong dictionary type for 'keyn'");

            evalAsNumber(2, lisp, a_key);
            second_element = first_element->protected_index(lisp, a_key);
            first_element->release();
            lisp->resetStack();
            return second_element;
        }

        long first;
        if (first_element->isDictionary()) {
            if (first_element->type != t_dictionaryn && first_element->type != t_treen)
                throw new Error("Error: wrong dictionary type for 'keyn'");

            if ((listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyn'");
            first = 2;
            // It is out of question to manipulate a dictionary declared in the code
            first_element = first_element->duplicate_constant(lisp);
        }
        else {
            if (!(listsize % 2 ))
                throw new Error("Error: wrong number of arguments for 'keyn'");
            first = 3;
            a_key = first_element->asNumber();
            first_element->release();
            first_element = lisp->provideDictionary_n();
            second_element = liste[2]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }

        //We store the values
        for (long i = first; i < listsize; i+=2) {
            evalAsNumber(i, lisp, a_key);
            second_element = liste[i+1]->eval(lisp);
            first_element->recording(a_key, second_element->copying(false));
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return first_element;
}

Element* List_dictionary_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty dictionary
        return lisp->provideDictionary();
    }

    if (listsize == 2) {
        Element* d = liste[1]->eval(lisp);
        if (!d->isDictionary()) {
            if (d->function_label(lisp) == t_class_instance) {
                Element* dico = d->asDictionary(lisp);
                d->release();
                return dico;
            }
            throw new Error("Error: wrong arguments for 'dictionary'");
        }
        if (d->type == t_dictionary)
            return d->copying(true);
        void* iter = d->begin_iter();
        Element* nxt  = d->next_iter(lisp, iter);
        Element* e;
        Dictionary* dico = lisp->provideDictionary();
        Element* u;
        while (nxt != emptyatom_) {
            e = d->value_on_index(lisp, nxt);
            u = e->copying(false);
            u->increment();
            dico->dictionary[nxt->asUString(lisp)] = u;
            e->release();
            nxt->release();
            nxt = d->next_iter(lisp, iter);
        }
        d->clean_iter(iter);
        d->release();
        return dico;
    }

    if (listsize == 3) {
        Dictionary* dico = lisp->provideDictionary();
        Element* l1 = liste[1]->eval(lisp);
        Element* l2 = liste[2]->eval(lisp);
        Element* v;
        if (l1->isList() && l2->isList()) {
            if (l1->size() != l2->size())
                throw new Error("Error: the two lists should have the same size 'dictionary'");
            long sz = l1->size();
            Element* idx;
            Element* val;
            for (long i = 0; i < sz; i++) {
                idx = l1->value_on_index(lisp, i);
                val = l2->value_on_index(lisp, i);
                v = val->copying(false);
                v->increment();
                dico->dictionary[idx->asUString(lisp)] = v;
                idx->release();
            }
        }
        else {
            v = l2->copying(false);
            dico->dictionary[l1->asUString(lisp)] = v;
            v->increment();
        }
        l1->release();
        l2->release();
        return dico;
    }

    if (!(listsize % 2 ))
        throw new Error("Error: wrong number of arguments for 'dictionary'");

    Dictionary* dico = lisp->provideDictionary();
    Element* element;

    
    try {
        lisp->checkState(this);
        u_ustring a_key;
        //We store values
        for (long i = 1; i < listsize; i+=2) {
            element = liste[i]->eval(lisp);
            a_key = element->asUString(lisp);
            element->release();
            element = liste[i+1]->eval(lisp);
            dico->recording(a_key, element->copying(false));
        }
    }
    catch (Error* err) {
        dico->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return dico;
}


Element* List_dictionaryi_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty dictionary
        return lisp->provideDictionary_i();
    }
    
    if (listsize == 2) {
        Element* d = liste[1]->eval(lisp);
        if (!d->isDictionary())
            throw new Error("Error: wrong arguments for 'dictionaryi'");
        if (d->type == t_dictionaryi)
            return d->copying(true);
        void* iter = d->begin_iter();
        Element* nxt  = d->next_iter(lisp, iter);
        Element* e;
        Dictionary_i* dico = lisp->provideDictionary_i();
        Element* u;
        while (nxt != emptyatom_) {
            e = d->value_on_index(lisp, nxt);
            u = e->copying(false);
            u->increment();
            dico->dictionary[nxt->asInteger()] = u;
            e->release();
            nxt->release();
            nxt = d->next_iter(lisp, iter);
        }
        d->clean_iter(iter);
        d->release();
        return dico;
    }

    if (listsize == 3) {
        Dictionary_i* dico = lisp->provideDictionary_i();
        Element* l1 = liste[1]->eval(lisp);
        Element* l2 = liste[2]->eval(lisp);
        Element* v;
        if (l1->isList() && l2->isList()) {
            if (l1->size() != l2->size())
                throw new Error("Error: the two lists should have the same size 'dictionaryi'");
            long sz = l1->size();
            Element* idx;
            Element* val;
            for (long i = 0; i < sz; i++) {
                idx = l1->value_on_index(lisp, i);
                val = l2->value_on_index(lisp, i);
                v = val->copying(false);
                v->increment();
                dico->dictionary[idx->asInteger()] = v;
                idx->release();
            }
        }
        else {
            v = l2->copying(false);
            dico->dictionary[l1->asInteger()] = v;
            v->increment();
        }

        l1->release();
        l2->release();
        return dico;
    }

    if (!(listsize % 2 ))
        throw new Error("Error: wrong number of arguments for 'dictionary'");

    Dictionary_i* dico = lisp->provideDictionary_i();
    Element* element;

    
    try {
        lisp->checkState(this);
        long a_key;
        //We store values
        for (long i = 1; i < listsize; i+=2) {
            element = liste[i]->eval(lisp);
            a_key = element->asInteger();
            element->release();
            element = liste[i+1]->eval(lisp);
            dico->recording(a_key, element->copying(false));
        }
    }
    catch (Error* err) {
        dico->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return dico;
}

Element* List_dictionaryn_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty dictionary
        return lisp->provideDictionary_n();
    }
    
    if (listsize == 2) {
        Element* d = liste[1]->eval(lisp);
        if (!d->isDictionary())
            throw new Error("Error: wrong arguments for 'dictionaryn'");
        if (d->type == t_dictionaryn)
            return d->copying(true);
        void* iter = d->begin_iter();
        Element* nxt  = d->next_iter(lisp, iter);
        Element* e;
        Dictionary_n* dico = lisp->provideDictionary_n();
        Element* u;
        while (nxt != emptyatom_) {
            e = d->value_on_index(lisp, nxt);
            u = e->copying(false);
            u->increment();
            dico->dictionary[nxt->asNumber()] = u;
            e->release();
            nxt->release();
            nxt = d->next_iter(lisp, iter);
        }
        d->clean_iter(iter);
        d->release();
        return dico;
    }

    if (listsize == 3) {
        Dictionary_n* dico = lisp->provideDictionary_n();
        Element* l1 = liste[1]->eval(lisp);
        Element* l2 = liste[2]->eval(lisp);
        Element* v;
        if (l1->isList() && l2->isList()) {
            if (l1->size() != l2->size())
                throw new Error("Error: the two lists should have the same size 'dictionaryn'");
            long sz = l1->size();
            Element* idx;
            Element* val;
            for (long i = 0; i < sz; i++) {
                idx = l1->value_on_index(lisp, i);
                val = l2->value_on_index(lisp, i);
                v = val->copying(false);
                v->increment();
                dico->dictionary[idx->asNumber()] = v;
                idx->release();
            }
        }
        else {
            v = l2->copying(false);
            dico->dictionary[l1->asFloat()] = v;
            v->increment();
        }

        l1->release();
        l2->release();
        return dico;
    }

    if (!(listsize % 2 ))
        throw new Error("Error: wrong number of arguments for 'dictionary'");

    Dictionary_n* dico = lisp->provideDictionary_n();
    Element* element;

    
    try {
        lisp->checkState(this);
        double a_key;
        //We store values
        for (long i = 1; i < listsize; i+=2) {
            element = liste[i]->eval(lisp);
            a_key = element->asNumber();
            element->release();
            element = liste[i+1]->eval(lisp);
            dico->recording(a_key, element->copying(false));
        }
    }
    catch (Error* err) {
        dico->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return dico;
}

Element* List_tree_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty tree
        return lisp->provideTree();
    }
    
    if (listsize == 2) {
        Element* d = liste[1]->eval(lisp);
        if (!d->isDictionary())
            throw new Error("Error: wrong arguments for 'tree'");
        if (d->type == t_tree)
            return d->copying(true);
        void* iter = d->begin_iter();
        Element* nxt  = d->next_iter(lisp, iter);
        Element* e;
        Tree* dico = lisp->provideTree();
        Element* u;
        while (nxt != emptyatom_) {
            e = d->value_on_index(lisp, nxt);
            u = e->copying(false);
            u->increment();
            dico->tree[nxt->asUString(lisp)] = u;
            e->release();
            nxt->release();
            nxt = d->next_iter(lisp, iter);
        }
        d->clean_iter(iter);
        d->release();
        return dico;
    }
    
    if (!(listsize % 2 ))
        throw new Error("Error: wrong number of arguments for 'tree'");

    Tree* dico = lisp->provideTree();
    Element* element;

    
    try {
        lisp->checkState(this);
        u_ustring a_key;
        //We store values
        for (long i = 1; i < listsize; i+=2) {
            element = liste[i]->eval(lisp);
            a_key = element->asUString(lisp);
            element->release();
            element = liste[i+1]->eval(lisp);
            dico->recording(a_key, element->copying(false));
        }
    }
    catch (Error* err) {
        dico->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return dico;
}


Element* List_treei_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty tree
        return lisp->provideTree_i();
    }
    
    if (listsize == 2) {
        Element* d = liste[1]->eval(lisp);
        if (!d->isDictionary())
            throw new Error("Error: wrong arguments for 'treei'");
        if (d->type == t_treei)
            return d->copying(true);
        void* iter = d->begin_iter();
        Element* nxt  = d->next_iter(lisp, iter);
        Element* e;
        Tree_i* dico = lisp->provideTree_i();
        Element* u;
        while (nxt != emptyatom_) {
            e = d->value_on_index(lisp, nxt);
            u = e->copying(false);
            u->increment();
            dico->tree[nxt->asInteger()] = u;
            e->release();
            nxt->release();
            nxt = d->next_iter(lisp, iter);
        }
        d->clean_iter(iter);
        d->release();
        return dico;
    }

    if (!(listsize % 2 ))
        throw new Error("Error: wrong number of arguments for 'tree'");

    Tree_i* dico = lisp->provideTree_i();
    Element* element;

    
    try {
        lisp->checkState(this);
        long a_key;
        //We store values
        for (long i = 1; i < listsize; i+=2) {
            element = liste[i]->eval(lisp);
            a_key = element->asInteger();
            element->release();
            element = liste[i+1]->eval(lisp);
            dico->recording(a_key, element->copying(false));
        }
    }
    catch (Error* err) {
        dico->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return dico;
}

Element* List_treen_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1) {
        //We create an empty tree
        return lisp->provideTree_n();
    }
    
    if (listsize == 2) {
        Element* d = liste[1]->eval(lisp);
        if (!d->isDictionary())
            throw new Error("Error: wrong arguments for 'treen'");
        if (d->type == t_treen)
            return d->copying(true);
        void* iter = d->begin_iter();
        Element* nxt  = d->next_iter(lisp, iter);
        Element* e;
        Tree_n* dico = lisp->provideTree_n();
        Element* u;
        while (nxt != emptyatom_) {
            e = d->value_on_index(lisp, nxt);
            u = e->copying(false);
            u->increment();
            dico->tree[nxt->asNumber()] = u;
            e->release();
            nxt->release();
            nxt = d->next_iter(lisp, iter);
        }
        d->clean_iter(iter);
        d->release();
        return dico;
    }

    if (!(listsize % 2 ))
        throw new Error("Error: wrong number of arguments for 'tree'");

    Tree_n* dico = lisp->provideTree_n();
    Element* element;

    
    try {
        lisp->checkState(this);
        double a_key;
        //We store values
        for (long i = 1; i < listsize; i+=2) {
            element = liste[i]->eval(lisp);
            a_key = element->asNumber();
            element->release();
            element = liste[i+1]->eval(lisp);
            dico->recording(a_key, element->copying(false));
        }
    }
    catch (Error* err) {
        dico->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return dico;
}


Element* List_list_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* a_list = lisp->provideList();
    Element* value;

    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            value = liste[i]->eval(lisp);
            a_list->append(value->copying(false));
        }
    }
    catch (Error* err) {
        a_list->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return a_list;
}

Element* List_loop_eval::eval(LispE* lisp) {
    int16_t label = liste[1]->label();
    if (label == v_null)
        return lisp->check_error(this, new Error(L"Error: Missing label for 'loop'"), idxinfo);

    Element* container = liste[2]->eval(lisp);
    Element* result;

    try {
        lisp->checkState(this);
        //We loop in a list
        result = container->loop(lisp, label, this);
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    container->release();
    return result;
}

Element* List_loopcount_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    long counter;
    int16_t label = 0;
    long first = 2;
    long increment = 1;
    Integer* element = NULL;
    evalAsInteger(1, lisp, counter);
    
    if (counter < 0) {
        increment = -1;
        counter *= -1;
    }

    if (liste[2]->isAtom()) {
        label = liste[2]->label();
        if (label == v_into) {
            label = liste[3]->label();
            if (label < l_final)
                throw new Error("Error: missing variable in loopcount");
            first = 4;
        }
        else
            first = 3;
        if (increment == -1)
            element = lisp->provideInteger(counter - 1);
        else
            element = lisp->provideInteger(0);
        lisp->storing_variable(element, label);
    }
    
    Element* result = null_;
    
    try {
        lisp->checkState(this);
        while (counter > 0) {
            for (long i = first; i < listsize && result->type != l_return; i++) {
                result->release();
                result = liste[i]->eval(lisp);
            }
            //If a 'return' or a 'break' has been placed in the code
            if (result->type == l_return) {
                lisp->resetStack();
                if (result->isBreak())
                    return null_;
                //this is a return, it goes back to the function call
                return result;
            }
            counter--;
            if (label)
                element->content += increment;
        }
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}

//(whilein iterator list condition instructions)
Element* List_whilein_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element = null_;

    int16_t label = liste[1]->label();
    lisp->storing_variable(element, label);
    Element* current_list = liste[2]->eval(lisp);
    void* iter = NULL;

    Element* result = null_;
    Element* condition;
    bool test;

    try {
        lisp->checkState(this);
        //(whilein i (iota 10) (< i 100) (print i))
        
        iter = current_list->begin_iter();
        element = current_list->next_iter_exchange(lisp, iter);
        lisp->storing_variable(element, label);
        
        condition = liste[3]->eval(lisp);
        test = condition->Boolean();
        condition->release();
        
        while (test && element != emptyatom_) {
            for (long i = 4; i < listsize && result->type != l_return; i++) {
                result->release();
                result = liste[i]->eval(lisp);
            }
            //If a 'return' or a 'break' has been placed in the code
            if (result->type == l_return) {
                current_list->clean_iter(iter);
                current_list->release();
                lisp->resetStack();
                if (result->isBreak())
                    return null_;
                //this is a return, it goes back to the function call
                return result;
            }
            
            element = current_list->next_iter_exchange(lisp, iter);
            lisp->storing_variable(element, label);
            condition = liste[3]->eval(lisp);
            test = condition->Boolean();
            condition->release();
        }
        current_list->clean_iter(iter);
        current_list->release();
     }
    catch (Error* err) {
        current_list->clean_iter(iter);
        current_list->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}

Element* List_ncheck_eval::eval(LispE* lisp) {
    try {
        lisp->checkState(this);
        Element* element = liste[1]->eval(lisp);
        
        if (!element->Boolean()) {
            element->release();
            lisp->resetStack();
            return liste[2]->eval_terminal(lisp, terminal);
        }
        
        
        _releasing(element);

        long listsize = liste.size();
        liste.back()->setterminal(terminal);
        for (long i = 3; i < listsize && element->type != l_return; i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
        lisp->resetStack();
        return element;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}

Element* List_not_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    Element* negated = element->negate(lisp);
    element->release();
    return negated;
}

Element* List_nullp_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool test = element->isNULL();
    element->release();
    return booleans_[test];
}

Element* List_numbers_eval::eval(LispE* lisp) {
    long listsz = liste.size();

    if (listsz == 1)
        return lisp->provideNumbers();

    Numbers* n = NULL;
    Element* values;


    try {
        lisp->checkState(this);
        if (listsz == 2) {
            values = liste[1]->eval(lisp);
            if (values->type == t_numbers && !values->status)
                n = (Numbers*)values;
            else {
                n = lisp->provideNumbers();
                values->flatten(lisp, n);
                values->release();
            }
        }
        else {
            n = lisp->provideNumbers();
            n->liste.reserve(listsz<<1);
            for (long e = 1; e < listsz; e++) {
                values = liste[e]->eval(lisp);
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->liste.push_back(values->index(i)->asNumber());
                    }
                }
                else
                    n->liste.push_back(values->asNumber());
                values->release();
            }
        }
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_integers_eval::eval(LispE* lisp) {
    long listsz = liste.size();

    if (listsz == 1)
        return lisp->provideIntegers();

    Integers* n = NULL;
    Element* values;


    try {
        lisp->checkState(this);
        if (listsz == 2) {
            values = liste[1]->eval(lisp);
            if (values->type == t_integers && !values->status)
                n = (Integers*)values;
            else {
                n = lisp->provideIntegers();
                values->flatten(lisp, n);
                values->release();
            }
        }
        else {
            n = lisp->provideIntegers();
            n->liste.reserve(listsz<<1);
            for (long e = 1; e < listsz; e++) {
                values = liste[e]->eval(lisp);
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->liste.push_back(values->index(i)->asInteger());
                    }
                }
                else
                    n->liste.push_back(values->asInteger());
                values->release();
            }
        }
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_strings_eval::eval(LispE* lisp) {
    long listsz = liste.size();

    if (listsz == 1)
        return lisp->provideStrings();

    Strings* n = NULL;
    Element* values;


    try {
        lisp->checkState(this);
        if (listsz == 2) {
            values = liste[1]->eval(lisp);
            if (values->type == t_strings && !values->status)
                n = (Strings*)values;
            else {
                n = lisp->provideStrings();
                values->flatten(lisp, n);
                values->release();
            }
        }
        else {
            n = lisp->provideStrings();
            n->liste.reserve(listsz<<1);
            for (long e = 1; e < listsz; e++) {
                values = liste[e]->eval(lisp);
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->liste.push_back(values->index(i)->asUString(lisp));
                    }
                }
                else
                    n->liste.push_back(values->asUString(lisp));
                values->release();
            }
        }
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_stringbytes_eval::eval(LispE* lisp) {
    long listsz = liste.size();

    if (listsz == 1)
        return new Stringbytes();

    Stringbytes* n = NULL;
    Element* values;


    try {
        lisp->checkState(this);
        if (listsz == 2) {
            values = liste[1]->eval(lisp);
            if (values->type == t_stringbytes && !values->status)
                n = (Stringbytes*)values;
            else {
                n = new Stringbytes();
                values->flatten(lisp, n);
                values->release();
            }
        }
        else {
            n = new Stringbytes();
            n->liste.reserve(listsz<<1);
            for (long e = 1; e < listsz; e++) {
                values = liste[e]->eval(lisp);
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->liste.push_back(values->index(i)->toString(lisp));
                    }
                }
                else
                    n->liste.push_back(values->toString(lisp));
                values->release();
            }
        }
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_or_eval::eval(LispE* lisp) {
    bool test = false;
    try {
        lisp->checkState(this);
        Element* element;
        for (long i = 1; i < size() && !test; i++) {
            element = liste[i]->eval(lisp);
            test = element->Boolean();
            element->release();
        }
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return booleans_[test];
}

Element* List_pop_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    if (container->isString()) {
        long keyvalue;
        u_ustring strvalue = container->asUString(lisp);
        if (liste.size() == 3)
            evalAsInteger(2, lisp, keyvalue);
        else {
            if (!strvalue.size())
                return emptystring_;
            strvalue.pop_back();
            return lisp->provideString(strvalue);
        }

        if (keyvalue < 0 || keyvalue >= strvalue.size())
            return emptystring_;
        strvalue.erase(strvalue.begin()+keyvalue);
        container->release();
        return lisp->provideString(strvalue);
    }

    if (liste.size() != 3) {
        if (container->type == t_llist) {
            if (container->removefirst())
                return container;
        }
        else {
            if (container->removelast())
                return container;
        }
        container->release();
        return null_;
    }
    
    try {
        lisp->checkState(this);
        Element* key = liste[2]->eval(lisp);
        if (container->remove(lisp, key)) {
            key->release();
            lisp->resetStack();
            return container;
        }
        key->release();
        container->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return null_;
}


Element* List_popfirst_eval::eval(LispE* lisp) {
    try {
        lisp->checkState(this);
        Element* first_element = liste[1]->eval(lisp);
        if (first_element->isString()) {
            u_ustring strvalue = first_element->asUString(lisp);
            if (!strvalue.size())
                return emptystring_;
            strvalue = strvalue.substr(1, strvalue.size());
            return lisp->provideString(strvalue);
        }
                
        lisp->resetStack();
        if (first_element->removefirst())
            return first_element;
        first_element->release();
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
}

Element* List_poplast_eval::eval(LispE* lisp) {
    try {
        lisp->checkState(this);
        Element* first_element = liste[1]->eval(lisp);
        if (first_element->isString()) {
            u_ustring strvalue = first_element->asUString(lisp);
            if (!strvalue.size())
                return emptystring_;
            strvalue.pop_back();
            return lisp->provideString(strvalue);
        }
        
        lisp->resetStack();
        if (first_element->removelast())
            return first_element;
        first_element->release();
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
}

Element* List_push_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    container = container->duplicate_constant(lisp);
    
    
    try {
        lisp->checkState(this);
        container->push_element(lisp, this);
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return container;
}

Element* List_pushtrue_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    container = container->duplicate_constant(lisp);
    
    try {
        lisp->checkState(this);
        container->push_element_true(lisp, this);
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return container;
}


Element* List_pushfirst_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    container = container->duplicate_constant(lisp);
    
    try {
        lisp->checkState(this);
        container->push_element_front(lisp, this);
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return container;
}

Element* List_pushlast_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    container = container->duplicate_constant(lisp);

    try {
        lisp->checkState(this);
        container->push_element_back(lisp, this);
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return container;
}

Element* List_switch_eval::eval(LispE* lisp) {
    List* code = NULL;
    u_ustring key;
    Element* e;
    
    try {
        lisp->checkState(this);
        e = liste[1]->eval(lisp);
        key = e->asUString(lisp);
        _releasing(e);
        
        if (!cases.count(key))
            code = default_value;
        else
            code = cases[key];
        
        if (code == NULL) {
            u_ustring msg = U"Error: Unknown 'switch' key: '";
            msg += key;
            msg += U"'";
            throw new Error(msg);
        }
                
        long sz = code->liste.size();
        for (long i = 1; i < sz; i++) {
            _releasing(e);
            e = code->liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return e;
}

Element* List_while_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* condition = liste[1]->eval(lisp);
    bool test = condition->Boolean();
    condition->release();
        
    Element* result = null_;
    
    try {
        lisp->checkState(this);
        while (test) {
            for (long i = 2; i < listsize && result->type != l_return; i++) {
                _releasing(result);
                result = liste[i]->eval(lisp);
            }
            
            //if a 'return' has been placed in the code
            if (result->type == l_return) {
                lisp->resetStack();
                lisp->stop_at_next_line(lisp->trace);
                if (result->isBreak())
                    return null_;
                //this is a return, it goes back to the function call
                return result;
            }
            
            condition = liste[1]->eval(lisp);
            test = condition->Boolean();
            condition->release();
        }
    }
    catch (Error* err) {
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}

Element* List_maplist_lambda_eval::eval(LispE* lisp) {
    Element* current_list = null_;
    Element* op = liste[1];
    Element* container = NULL;
    
    bool sb = lisp->set_true_as_one();
    void* iter = NULL;
    Element* save_variable = this;
        
    try {
        lisp->checkState(this);
        if (listesize == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        
        current_list = liste[2]->eval(lisp);
        long listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return emptylist_;
        }
                
        iter = current_list->begin_iter();
        Element* nxt = current_list->next_iter_exchange(lisp, iter);
        
        //if there is already a variable with this name on the stack
        //we record it to restore it later...
        save_variable = lisp->record_or_replace(nxt, label);
        
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        Element* e = op->eval_lambda_min(lisp);
        if (e->type == l_return)
            container = emptylist_;
        else {
            if (choice) {
                switch (e->type) {
                    case t_string:
                        container = lisp->provideStrings();
                        break;
                    case t_stringbyte:
                        container = new Stringbytes();
                        break;
                    case t_integer:
                        container = lisp->provideIntegers();
                        break;
                    case t_float:
                        container = lisp->provideFloats();
                        break;
                    case t_number:
                        container = lisp->provideNumbers();
                        break;
                    default:
                        container = lisp->provideList();
                }
            }
            else
                container = lisp->provideList();
            
            e = e->copying(false);
            check_tensor |= e->isPureList();
            container->append(e);
            e->release();
            nxt = current_list->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                lisp->replacestackvalue(nxt, label);
                e = op->eval_lambda_min(lisp);
                if (e->type == l_return)
                    break;
                
                check_tensor |= e->isPureList();
                check_tensor |= !e->is_same_tensor(container->last(lisp));
                e = e->copying(false);
                container->append(e);
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
        }
        current_list->clean_iter(iter);
        lisp->reset_in_stack(save_variable, label);
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            nxt = container->index(0)->newTensor(lisp, (List*)container);
            if (nxt != container) {
                container->release();
                return nxt;
            }
        }
        return container;
    }
    catch (Error* err) {
        if (save_variable != this)
            lisp->reset_in_stack(save_variable, label);
        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        if (container != NULL)
            container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    return emptylist_;
}


Element* List_takenb_eval::eval(LispE* lisp) {
    long nb;
    
    evalAsInteger(1, lisp, nb);
    
    Element* lst = liste[2]->eval(lisp);
    if (!lst->isList())
        throw new Error("Error: Expecting a list as second element");
    
    bool direction = true;
    if (liste.size() == 4)
        direction = false;
    
    if (nb < 0) {
        if (direction) {
            nb *= -1;
            direction = false;
        }
        else {
            lst->release();
            throw new Error("Error: wrong value for first argument");
        }
    }
    
    Element* l_result = lst->takenb(lisp, nb, direction);
    lst->release();
    return l_result;
}

Element* List_eq_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;
    bool test;

    try {
        lisp->checkState(this);
        second_element = liste[2]->eval(lisp);
        test = ( (first_element == second_element) || first_element->egal(second_element));
        second_element->release();
        
        for (long i = 3; i < listsize && test; i++) {
            second_element = liste[i]->eval(lisp);
            test = ( (first_element == second_element) || first_element->egal(second_element));
            second_element->release();
        }
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return booleans_[test];
}

Element* List_neq_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;
    bool test;

    try {
        lisp->checkState(this);
        second_element = liste[2]->eval(lisp);
        test = ( (first_element != second_element) && !first_element->egal(second_element));
        second_element->release();
        
        for (long i = 3; i < listsize && test; i++) {
            second_element = liste[i]->eval(lisp);
            test = ( (first_element != second_element) && !first_element->egal(second_element));
            second_element->release();
        }
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return booleans_[test];
}

Element* List_mask_eval::eval(LispE* lisp) {
    //We have two parameters, one is a list of binary elements, the other one a list to generate
    Element* binaries = liste[1]->eval(lisp);
    Element* current_list = NULL;
    
    if (!binaries->isList())
        throw new Error("Error: first element should be a list of 1 and 0");
    
    long sz = binaries->size();
    Element* res = null_;
    Element* zero = null_;
    try {
        lisp->checkState(this);
        current_list = liste[2]->eval(lisp);
        if (!current_list->isList())
            throw new Error("Error: first element should be a list of 1 and 0");
        
        long bi = -1;
        long basic_zero = -2;
        if (size() == 4) {
            bi = 0;
            zero = liste[3]->eval(lisp);
            basic_zero = zero->size();
            if (!basic_zero)
                throw new Error("Error: wrong third argument. Should be a non empty list");
        }

        long current_sz = current_list->size();
        res = current_list->newInstance();
        bool nb;
        long j = 0;
        long i;
        
        for (i = 0; i < sz; i++) {
            nb = binaries->index(i)->Boolean();
            if (!nb) {
                if (basic_zero == -1)
                    res->append(zero);
                else {
                    if (bi == basic_zero)
                        break;
                    res->append(zero->index(bi++));
                }
            }
            else {
                if (j == current_sz)
                    break;
                res->append(current_list->index(j++));
            }
        }
        if (i != sz)
            throw new Error("Error: size mismatch");
    }
    catch (Error* err) {
        res->release();
        zero->release();
        binaries->release();
        if (current_list != NULL)
            current_list->release();
        return lisp->check_error(this, err, idxinfo);
    }
    zero->release();
    current_list->release();
    binaries->release();
    lisp->resetStack();
    return res;
}

Element* List_stringf_eval::eval(LispE* lisp) {
    char* buffer = NULL;
    Element* e = liste[2]->eval(lisp);
    string format = e->toString(lisp);
    e->release();
    
    long sz = 0;
    
    e = liste[1]->eval(lisp);
    sz = format.size() + 100;
    buffer = new char[sz];
    switch (e->type) {
        case t_float:
            sprintf_s(buffer, sz, format.c_str(), e->asFloat());
            break;
        case t_number:
            sprintf_s(buffer, sz, format.c_str(), e->asNumber());
            break;
        case t_integer:
            sprintf_s(buffer, sz, format.c_str(), e->asInteger());
            break;
        default:
            e->release();
            throw new Error("Error: the first argument should be a number");
    }
    
    e->release();
    format = buffer;
    delete[] buffer;
    return lisp->provideString(format);
}

Element* List_iota_eval::eval(LispE* lisp) {
    long sz = size();
    Element* res = null_;
    Element* e = null_;
    Element* sub;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_integer) {
                res = lisp->provideIntegers();
                res->reserve(e->asInteger() + 1);
                for (long j = 1; j <= e->asInteger(); j++) {
                    ((Integers*)res)->liste.push_back(j);
                }
            }
            else {
                res = lisp->provideNumbers();
                res->reserve(e->asInteger() + 1);
                
                double v = e->asNumber();
                long u = v;
                
                for (double j = 1 + (v-u); j <= v; j++) {
                    ((Numbers*)res)->liste.push_back(j);
                }
            }
            e->release();
            lisp->resetStack();
            return res;
        }
        
        res = lisp->provideList();
        res->reserve(sz+1);
        for (long i = 1; i < sz; i++) {
            e = liste[i]->eval(lisp);
            if (e->type == t_integer) {
                sub = lisp->provideIntegers();
                sub->reserve(e->asInteger() + 1);
                res->append(sub);
                for (long j = 1; j <= e->asInteger(); j++) {
                    ((Integers*)sub)->liste.push_back(j);
                }
            }
            else {
                sub = lisp->provideNumbers();
                sub->reserve(e->asInteger() + 1);
                res->append(sub);
                double v = e->asNumber();
                long u = v;
                for (double j = 1 + (v-u); j <= v; j++) {
                    ((Numbers*)sub)->liste.push_back(j);
                }
            }
            e->release();
        }
    }
    catch (Error* err) {
        e->release();
        res->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return res;
}

Element* List_iota0_eval::eval(LispE* lisp) {
    long sz = size();
    Element* res = null_;
    Element* e = null_;
    Element* sub;
    try {
        lisp->checkState(this);
        if (sz == 2) {
            e = liste[1]->eval(lisp);
            if (e->type == t_integer) {
                res = lisp->provideIntegers();
                res->reserve(e->asInteger() + 1);
                for (long j = 0; j < e->asInteger(); j++) {
                    ((Integers*)res)->liste.push_back(j);
                }
            }
            else {
                res = lisp->provideNumbers();
                res->reserve(e->asInteger() + 1);
                double v = e->asNumber();
                long u = v;
                
                for (double j = (v-u); j < v; j++) {
                    ((Numbers*)res)->liste.push_back(j);
                }
            }
            e->release();
            lisp->resetStack();
            return res;
        }
        
        res = lisp->provideList();
        res->reserve(sz+1);
        for (long i = 1; i < sz; i++) {
            e = liste[i]->eval(lisp);
            if (e->type == t_integer) {
                sub = lisp->provideIntegers();
                sub->reserve(e->asInteger() + 1);
                res->append(sub);
                for (long j = 0; j < e->asInteger(); j++) {
                    ((Integers*)sub)->liste.push_back(j);
                }
            }
            else {
                sub = lisp->provideNumbers();
                sub->reserve(e->asInteger() + 1);
                res->append(sub);
                double v = e->asNumber();
                long u = v;
                
                for (double j = (v-u); j < v; j++) {
                    ((Numbers*)sub)->liste.push_back(j);
                }
            }
            e->release();
        }
    }
    catch (Error* err) {
        res->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return res;
}


Element* List_irank_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);

    long listsz = liste.size();
    
    Element* e = null_;
    Element* lst = null_;
    Rankloop* r = NULL;
    long i, v;
    
    try {
        lisp->checkState(this);
        r = new Rankloop(lisp, (List*)element);
        
        for (i = 2; i < listsz; i++) {
            e = liste[i]->eval(lisp);
            if (e->isNumber()) {
                v = e->asInteger();
                r->positions.push_back(v);
            }
            else
                r->positions.push_back(-1);
            _releasing(e);
        }
        vecte<long> shape;
        element->getShape(shape);
        
        if (shape.size() < r->positions.size())
            throw new Error("Error: cannot loop with 'irank' with these indexes");
        
        while (r->positions.size() > 1 && r->positions.back() < 0)
            r->positions.pop_back();

        if (shape.size() == r->positions.size()) {
            r->max_iterator = 1;
            r->last = true;
        }
        else
            r->max_iterator = shape[r->positions.size()];
    }
    catch (Error* err) {
        if (r != NULL)
            r->release();
        lst->release();
        element->release();
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return r;
}

//------------------------------------------------------------------
Element* List::scan(LispE* lisp, Element* current_list, long sz) {
    Element* res = current_list->newInstance();

    if (size() == 0)
        return res;

    try {
        //this is a filter, the first list
        long j = 0;
        long nb;

        for (long i = 0; i < size(); i++) {
            nb = liste[i]->asInteger();
            if (!nb)
                res->append(zero_value);
            else {
                if (j >= sz)
                    throw new Error("Error: List size mismatch");
                
                while (nb > 0) {
                    res->append(current_list->index(j));
                    nb--;
                }
                j++;
            }
        }
        if (j >= sz)
            throw new Error("Error: List size mismatch");
    }
    catch (Error* err) {
        res->release();
        throw err;
    }

    return res;
}

Element* List_lambda_eval::scan(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    List* res = lisp->provideList();

    Element* rarg1 = this;
    Element* rarg2 = this;
    
    int16_t arg1 = labels[0];
    int16_t arg2 = labels[1];

    try {
        Element* e = current_list->index(0)->copying(false);
        
        rarg1 = lisp->record_or_replace(e, arg1);
        rarg2 = lisp->record_or_replace(null_, arg2);
        res->append(e);
        
        for (long i = 1; i < sz; i++) {
            lisp->replacestackvalue(current_list->index(i), arg2);
            e = eval_lambda_min(lisp);
            if (e->type == l_return)
                break;
            e = e->copying(false);
            res->append(e);
            lisp->replacestackvalue(e, arg1);
        }
        
        lisp->reset_in_stack(rarg2, arg2);
        lisp->reset_in_stack(rarg1, arg1);
    }
    catch (Error* err) {
        if (rarg2 != this)
            lisp->reset_in_stack(rarg2, arg2);
        if (rarg1 != this)
            lisp->reset_in_stack(rarg1, arg1);
        res->release();
        throw err;
    }
    return res;
}


//---------------------------------------------------------------------
Element* List_member_eval::eval(LispE* lisp) {
    Element* element = liste[1];
    Element* the_set = liste[1];
    Element* result = liste[1];
    bool sb = lisp->set_true_as_one();
    
    try {
        lisp->checkState(this);
        element = element->eval(lisp);
        if (!element->isList())
            throw new Error("Error: expecting a list as first element");
        the_set = liste[2]->eval(lisp);
        if (!the_set->isList())
            throw new Error("Error: expecting a list as first element");
        result = element->check_member(lisp, the_set);
        the_set->release();
        element->release();
        lisp->reset_to_true(sb);
    }
    catch (Error* err) {
        lisp->reset_to_true(sb);
        element->release();
        the_set->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}

Element* Element::comparison(LispE* lisp, Element* l) {
    if (isequal(lisp, l))
        return one_value;
    else
        return zero_value;
}

Element* List::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        List* res = lisp->provideList();
        for (long i = 0; i < size(); i++) {
            res->append(liste[i]->comparison(lisp, l->index(i)));
        }
        return res;
    }
    
    Element* res = pureInstance();
    for (long i = 0; i < size(); i++) {
        res->append(liste[i]->comparison(lisp, l));
    }
    return res;
}

Element* Floats::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        Integers* res = lisp->provideIntegers();
        for (long i = 0; i < size(); i++) {
            if (liste[i] == l->index(i)->asFloat())
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        return res;
    }
    
    float v = l->asFloat();
    Integers* res = lisp->provideIntegers();
    for (long i = 0; i < size(); i++) {
        if (liste[i] == v)
            res->liste.push_back(1);
        else
            res->liste.push_back(0);
    }
    return res;
}

Element* Numbers::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        Numbers* res = lisp->provideNumbers();
        for (long i = 0; i < size(); i++) {
            if (liste[i] == l->index(i)->asNumber())
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        return res;
    }
    
    double v = l->asNumber();
    Numbers* res = lisp->provideNumbers();
    for (long i = 0; i < size(); i++) {
        if (liste[i] == v)
            res->liste.push_back(1);
        else
            res->liste.push_back(0);
    }
    return res;
}

Element* Integers::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        Integers* res = lisp->provideIntegers();
        for (long i = 0; i < size(); i++) {
            if (liste[i] == l->index(i)->asInteger())
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        return res;
    }
    
    long v = l->asInteger();
    Integers* res = lisp->provideIntegers();
    for (long i = 0; i < size(); i++) {
        if (liste[i] == v)
            res->liste.push_back(1);
        else
            res->liste.push_back(0);
    }
    return res;
}

Element* Shorts::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        Integers* res = lisp->provideIntegers();
        for (long i = 0; i < size(); i++) {
            if (liste[i] == l->index(i)->asShort())
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        return res;
    }
    
    short v = l->asShort();
    Integers* res = lisp->provideIntegers();
    for (long i = 0; i < size(); i++) {
        if (liste[i] == v)
            res->liste.push_back(1);
        else
            res->liste.push_back(0);
    }
    return res;
}

Element* Strings::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        Integers* res = lisp->provideIntegers();
        for (long i = 0; i < size(); i++) {
            if (liste[i] == l->index(i)->asUString(lisp))
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        return res;
    }
    
    u_ustring v = l->asUString(lisp);
    Integers* res = lisp->provideIntegers();
    for (long i = 0; i < size(); i++) {
        if (liste[i] == v)
            res->liste.push_back(1);
        else
            res->liste.push_back(0);
    }
    return res;
}

Element* Stringbytes::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_value;
        Integers* res = lisp->provideIntegers();
        for (long i = 0; i < size(); i++) {
            if (liste[i] == l->index(i)->toString(lisp))
                res->liste.push_back(1);
            else
                res->liste.push_back(0);
        }
        return res;
    }
    
    string v = l->toString(lisp);
    Integers* res = lisp->provideIntegers();
    for (long i = 0; i < size(); i++) {
        if (liste[i] == v)
            res->liste.push_back(1);
        else
            res->liste.push_back(0);
    }
    return res;
}

Element* List_flip_eval::eval(LispE* lisp) {
    Element* first_element = liste[0];
    Element* second_element = liste[1];
    bool reflip = false;
    
    try {
        lisp->checkState(this);
        if (second_element->isList() && second_element->size() >= 3) {
            second_element->swap(1,2);
            reflip = true;
            first_element =  second_element->eval(lisp);
            second_element->swap(1,2);
            lisp->resetStack();
            return first_element;
        }
        first_element = second_element->eval(lisp);
        switch (first_element->type) {
            case t_floats:
            case t_numbers:
            case t_shorts:
            case t_integers:
            case t_strings:
            case t_stringbytes:
            case t_list:
                if (first_element->size() < 2) {
                    lisp->resetStack();
                    return first_element;
                }
                second_element = first_element->copyatom(lisp, 1);
                second_element->swap(0, 1);
                lisp->resetStack();
                return second_element;
            case t_llist: {
                LList* elements = (LList*)first_element;
                if (!elements->atleast2()) {
                    lisp->resetStack();
                    return first_element;
                }
                LList* l = (LList*)elements->copyatom(lisp, 1);
                second_element = l->liste.first->value;
                l->liste.first->value = l->liste.first->_next->value;
                l->liste.first->_next->value = second_element;
                lisp->resetStack();
                return l;
            }
            default:
                throw new Error("Error: cannot flip this element");
        }
    }
    catch (Error* err) {
        if (reflip)
            second_element->swap(1,2);
        else {
            if (first_element != second_element)
                first_element->release();
            second_element->release();
        }
        return lisp->check_error(this, err, idxinfo);
    }

    return null_;
}

Element* List_swap_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    try {
        lisp->checkState(this);
        long beg;
        long end;
        long sz = first_element->size();
        evalAsInteger(2, lisp, beg);
        if (size() == 4)
            evalAsInteger(3, lisp, end);
        else
            end = beg + 1;
        if (beg < 0 || end >= sz)
            throw new Error("Error: cannot swap values in this list");
        
        switch (first_element->type) {
            case t_floats:
            case t_numbers:
            case t_shorts:
            case t_integers:
            case t_strings:
            case t_stringbytes:
            case t_list:
                if (sz < 2) {
                    lisp->resetStack();
                    return first_element;
                }
                first_element->swap(beg, end);
                lisp->resetStack();
                return first_element;
            default:
                throw new Error("Error: cannot swap in this element");
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return null_;
}


Element* List_equal_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;
    char test = true;

    try {
        lisp->checkState(this);
        for (long i = 2; i < size() && test; i++) {
            second_element = liste[i]->eval(lisp);
            test = first_element->isequal(lisp, second_element);
            first_element->release();
            first_element = second_element;
        }
        
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return booleans_[test];
}

Element* List_extract_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    Element* value;


    try {
        lisp->checkState(this);
        value = element->extraction(lisp, this);
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    element->release();
    lisp->resetStack();
    return value;
}

Element* List_set_range_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);

    int16_t label = liste[1]->label();
    Element* value;

    try {
        lisp->checkState(this);
        value = element->replace_in(lisp, this);
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    element->release();
    lisp->resetStack();
    if (label > l_final)
        return lisp->recording_variable(value, label);
    return value;
}


Element* List_flatten_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (element->isValueList())
        return element;
    
    if (element->type == t_matrix_number || element->type == t_tensor_number) {
        Numbers* l = lisp->provideNumbers();
        element->flatten(lisp, l);
        element->release();
        return l;
    }
    
    if (element->type == t_matrix_float || element->type == t_tensor_float) {
        Floats* l = lisp->provideFloats();
        element->flatten(lisp, l);
        element->release();
        return l;
    }
    
    List* l = lisp->provideList();
    element->flatten(lisp,l);
    element->release();
    return l;
}

Element* List_at_shape_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* result = liste[0];

    long instruction_size = liste.size();
    Integers* shapes = NULL;
    Element* shape = liste[0];

    try {
        lisp->checkState(this);
        //The second element is the shape...
        shape = liste[2]->eval(lisp);
        long shape_size = shape->size();

        if (!shape->isList() || !shape_size || shape_size < instruction_size - 3)
            throw new Error("Error: Expecting a list with enough values to match the indexes as second argument");
        
        long i, begin;

        if (shape->type == t_integers)
            shapes = (Integers*)shape;
        else {
            shapes = lisp->provideIntegers();
            for (i = 0; i < shape_size; i++)
                shapes->liste.push_back(shape->index(i)->asInteger());
        }

        //The user might have provided a list of indexes
        //which we use to traverse a complex hierarchical structure...
        long index = 0;
        long idx = 0;
        long cumul = 1;
        
        for (i = instruction_size - 1; i >= 3; i--) {
            evalAsInteger(i, lisp, index);
            if (index < 0) {
                i -= 3;
                break;
            }
            else {
                begin = shapes->liste[i - 3];
                if (index >= begin)
                    throw new Error("Error: index mismatch with shape");
            }
            idx += index*cumul;
            cumul *= begin;
        }

        instruction_size -= 3;

        if (index < 0) {
            bool choice = (index == -1);
            result = container->newInstance();
            //First we need to detect if -1 is in the middle of a structure...
            cumul = 1;
            long value_size;
            begin = 0;
            for (index = 0; index < i; index++) {
                evalAsInteger(index + 3, lisp, value_size);
                if (value_size < 0)
                    break;
                begin += value_size * cumul;
                cumul *= shapes->liste[index];
            }

            if (index) {
                //We have found a restriction
                cumul = 1;
                for (; index < shape_size; index++) {
                    cumul *= shapes->liste[index];
                }
                begin *= cumul;
                value_size = begin + cumul;
                cumul = 1;
            }
            else
                value_size = container->size();
            
            //First we need to find the new bloc size
            long block_size = 1;
            for (index = i + 1; index < shape_size; index++) {
                block_size *= shapes->liste[index];
                cumul *= (index >= instruction_size)?shapes->liste[index]:1;
            }
            if (choice) {
                if (cumul == 1) {
                    for (i = begin; i < value_size; i += block_size)
                        result->set_from(container, i + idx);
                }
                else {
                    idx *= cumul;
                    for (i = begin; i < value_size; i += block_size) {
                        result->set_from(container, i + idx, i + idx + cumul);
                    }
                }
            }
            else {
                shape_size = shapes->liste[i];
                if (cumul == 1) {
                    cumul = block_size;
                    block_size *= shape_size;
                    for (index = 0; index < shape_size; index++) {
                        instruction_size = idx + index*cumul;
                        for (i = begin; i < value_size; i += block_size)
                            result->set_from(container, i + instruction_size);
                    }
                }
                else {
                    idx *= cumul;
                    block_size *= shape_size;
                    for (index = 0; index < shape_size; index++) {
                        instruction_size = idx + (index*cumul);
                        for (i = begin; i < value_size; i += block_size) {
                            result->set_from(container, instruction_size + i, instruction_size + i + cumul);
                        }
                    }
                }
            }
        }
        else {
            if (shape_size == instruction_size) {
                if (idx >= container->size())
                    throw new Error("Error: Index out of bound");
                result = container->index(idx)->duplicate_constant(lisp);
            }
            else {
                result = container->newInstance();
                cumul = 1;
                for (i = instruction_size; i < shape_size; i++) {
                    index = shape->index(i)->asInteger();
                    cumul *= index;
                    idx *= index;
                }
                result->set_from(container, idx, idx + cumul);
            }
        }
        shapes->release();
        container->release();
        if (shapes != shape)
            shape->release();
    }
    catch (Error* err) {
        if (shapes != NULL)
            shapes->release();
        if (shape != shapes)
            shape->release();
        container->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}

Element* List_set_shape_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);

    int16_t instruction_size = liste.size();
    Element* value = null_;
    Element* shape = null_;
    
    try {
        lisp->checkState(this);
        //The second element is the shape...
        shape = liste[2]->eval(lisp);
        long shape_size = shape->size();
        if (!shape->isList() || !shape_size || shape_size < instruction_size - 4)
            throw new Error("Error: Expecting a list with enough values to match the indexes as second argument");

        value = liste[instruction_size - 1]->eval(lisp);

        //The user might have provided a list of indexes
        //which we use to traverse a complex hierarchical structure...
        long i, sh;
        long index = 0;
        long idx = 0;
        long cumul = 1;

        for (i = instruction_size - 2; i >= 3; i--) {
            evalAsInteger(i, lisp, index);
            if (index < 0)
                throw new Error("Error: cannot modify this value");
            sh = shape->index(i - 3)->asInteger();
            if (index >= sh)
                throw new Error("Error: index mismatch with shape");
            
            idx += index*cumul;
            cumul *= sh;
        }
            
        instruction_size -= 4;

        if (shape_size == instruction_size) {
            if (idx >= container->size())
                throw new Error("Error: Index out of bound");
            container->set_in(lisp, value, idx);
        }
        else {
            cumul = 1;
            for (i = instruction_size; i < shape_size; i++) {
                index = shape->index(i)->asInteger();
                cumul *= index;
                idx *= index;
            }

            if (cumul > value->size())
                throw new Error("Error: cannot store this value in this container");
            cumul += idx;
            for (i = idx; i < cumul; i++) {
                container->set_in(lisp, value->index(i - idx), i);
            }
        }
        value->release();
        shape->release();
    }
    catch (Error* err) {
        shape->release();
        value->release();
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return container;
}

//Infix Expressions: x op y op z op u
//Infix Expressions: x op y op z op u
Element* List::evall_infix(LispE* lisp) {
    long listsize = size();
    if (!listsize)
        return this;
    
    Element* e;
    Element* oper = NULL;
    List* root;

    long beg = 1;
    if (liste[0]->label() != l_infix) {
        if (liste[0]->isExecutable(lisp)) {
            //we need to check for lists in the arguments, we are still under the infix hood...
            root = lisp->provideList();
            root->append(liste[0]);
            bool lst = false;
            for (long i = 1; i < listsize; i++) {
                e = liste[i];
                if (e->isList()) {
                    oper = ((List*)e)->eval_infix(lisp);
                    if (oper != e) {
                        e = oper;
                        lst = true;
                    }
                }
                root->append(e);
            }
            return root;
        }
        beg = 0;
    }
    
    int16_t label;
    List* operations = lisp->provideList();
    bool addtolast = false;
    bool checkoperator = false;
    try {
        for (long i = beg; i < listsize; i++) {
            oper = liste[i];
            if (oper->isOperator()) { //10 + -> first operator
                if (!checkoperator)
                    throw new Error("Error: Infix expression is malformed");
                
                if (operations->size() == 1) {
                    e = operations->liste[0];
                    operations->liste[0] = oper;
                    operations->append(e);
                    addtolast = false;
                }
                else {
                    label = oper->label();
                    if (label == operations->liste[0]->label()) {
                        addtolast = false;
                    }
                    else {
                        if (label == l_plus || label == l_minus || label == l_and || label == l_xor || label == l_or) { // go up; this is how the
                            root = lisp->provideList();
                            root->append(oper);
                            root->append(operations);
                            operations = root;
                            addtolast = false;
                        }
                        else {
                            e = operations->last(lisp);
                            addtolast = true;
                            if (!e->isList() || e->index(0)->label() != label || e->size() == 2) {
                                root = lisp->provideList();
                                root->append(oper);
                                root->append(e);
                                operations->changelast(root);
                            }
                            else {
                                oper = e->change_to_n();
                                if (oper != e) {
                                    operations->liste[operations->size() - 1] = oper;
                                    oper->increment();
                                }
                            }
                        }
                    }
                }
                checkoperator = false;
                continue;
            }
            
            if (checkoperator)
                throw new Error("Error: Infix expression is malformed");
            
            checkoperator = true;
            e = oper->evall_infix(lisp);
            
            if (addtolast) {
                operations->last(lisp)->append(e);
            }
            else
                operations->append(e);
        }
        if (!checkoperator)
            throw new Error("Error: Infix expression is malformed");
        
        e = operations->eval(lisp);
        operations->release();
        return e;
    }
    catch(Error* err) {
        operations->release();
        throw err;
    }
}

//Infix Expressions: x op y op z op u
Element* List_infix_eval::eval(LispE* lisp) {
    try {
        lisp->checkState(this);
        Element* e = evall_infix(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}


Element* List_join_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* second_element;


    try {
        lisp->checkState(this);
        u_ustring sep;
        if (liste.size() == 3)
            evalAsUString(2,lisp,sep);
        second_element = container->join_in_list(lisp, sep);
        container->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return second_element;
}

Element* List_label_eval::eval(LispE* lisp) {
    int16_t label = liste[1]->label();
    if (label == v_null)
        return lisp->check_error(this, new Error(L"Error: Missing label for 'label'"), idxinfo);

    Element* value = liste[2]->eval(lisp);

    try {
        lisp->checkState(this);
        Element* e = lisp->recordingunique(value, label);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        value->release();
        return lisp->check_error(this, err, idxinfo);
    }
}

Element* List_zip_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    List* lists = lisp->provideList();
    Element* container = null_;
    Element* result = null_;
    long szl = -1;
    long i;
    long j = 0;
    int16_t thetype = 0;

    try {
        lisp->checkState(this);
        //We combine different lists together...
        for (i = 1; i < listsize; i++) {
            container = liste[i]->eval(lisp);
            if (!container->isList())
                throw new Error(L"Error: 'zip' only accepts lists as arguments");
            if (i == 1)
                szl = container->size();
            else {
                if (szl != container->size())
                    throw new Error(L"Error: Lists should all have the same size in 'zip'");
            }
            if (container->type == t_llist)
                lists->append(new Iter_llist((LList*)container, szl));
            else
                lists->append(container);
            if (!thetype)
                thetype = container->type;
            else {
                if (thetype != container->type) {
                    if (thetype == t_integers && container->type == t_numbers)
                        thetype = t_numbers;
                    else
                        thetype = t_list;
                }
            }
            container = null_;
        }
        
        result = lisp->provideList();
        Element* sub;
        Element* e;
        switch (thetype) {
            case t_strings:
                sub = lisp->provideStrings();
                break;
            case t_stringbytes:
                sub = new Stringbytes();
                break;
            case t_shorts:
                sub = new Shorts();
                break;
            case t_integers:
                sub = lisp->provideIntegers();
                break;
            case t_floats:
                sub = lisp->provideFloats();
                break;
            case t_numbers:
                sub = lisp->provideNumbers();
                break;
            default:
                sub = lisp->provideList();
        }
        for (j = 0; j < szl; j++) {
            if (j)
                sub = sub->newInstance();
            result->append(sub);
            for (i = 0; i <lists->size(); i++) {
                e = lists->liste[i]->value_on_index(lisp, j);
                sub->append(e);
                e->release();
            }
        }
        lists->release();
    }
    catch (Error* err) {
        container->release();
        result->release();
        lists->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}


Element* List_zipwith_lambda_eval::eval(LispE* lisp) {
    Element* container = liste[2]->eval(lisp);
    if (!container->isList()) {
        container->release();
        return lisp->check_error(this, new Error(L"Error: 'zipwith' only accepts lists as arguments"), idxinfo);
    }
    Element* value;
    
    List* lists = lisp->provideList();
    
    long i, lsz, j = 0, szl = container->size();
    
    if (container->type == t_llist)
        lists->append(new Iter_llist((LList*)container, szl));
    else
        lists->append(container);
    
    List* call = NULL;
    int16_t listsize = size();
    if (liste.back()->type == v_null)
        listsize--;
    
    try {
        lisp->checkState(this);
        //We combine different lists together with an operator
        //First element is the operation
        for (i = 3; i < listsize; i++) {
            container = liste[i]->eval(lisp);
            if (container->isList() && szl == container->size()) {
                if (container->type == t_llist)
                    lists->append(new Iter_llist((LList*)container, szl));
                else
                    lists->append(container);
                container = null_;
            }
            else
                throw new Error(L"Error: 'zipwith' only accepts lists of same size as arguments");
        }
        
        if (!szl) {
            lists->release();
            lisp->resetStack();
            return emptylist_;
        }
        
        lsz = lists->size();
        
        ITEM& item = *lists->liste.items;
        
        if (params.size() < lsz)
            throw new Error("Error: Wrong number of arguments");
        
        //if there is already a variable with this name on the stack
        //we record it to restore it later...
        call = lisp->provideList();
        
        for (i = 0; i < lsz; i++) {
            value = lisp->record_or_replace(item[i]->index(0), params[i]);
            call->liste.push_raw(value);
        }
        
        value = lambda_e->eval_lambda_min(lisp);
        if (value->type == l_return)
            container = emptylist_;
        else {
            if (choose) {
                switch (value->type) {
                    case t_string:
                        container = lisp->provideStrings();
                        break;
                    case t_stringbyte:
                        container = new Stringbytes();
                        break;
                    case t_integer:
                        container = lisp->provideIntegers();
                        break;
                    case t_float:
                        container = lisp->provideFloats();
                        break;
                    case t_number:
                        container = lisp->provideNumbers();
                        break;
                    default:
                        container = lisp->provideList();
                }
            }
            else
                container = lisp->provideList();
            container->append(value);
            value->release();
            
            for (j = 1; j < szl; j++) {
                for (i = 0; i < lsz; i++)
                    lisp->replacestackvalue(item[i]->index(j), params[i]);
                value = lambda_e->eval_lambda_min(lisp);
                if (value->type == l_return)
                    break;
                container->append(value);
                value->release();
            }
        }
        for (i = lsz - 1; i >= 0; i--)
            lisp->reset_in_stack(call->liste[i], params[i]);
        call->rawrelease();
        lists->release();
    }
    catch (Error* err) {
        container->increment();
        if (call != NULL) {
            for (i = call->size() - 1; i >= 0; i--)
                lisp->reset_in_stack(call->index(i), params[i]);
            call->rawrelease();
        }
        lists->release();
        container->decrement();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return container;
}


Element* List_search_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* third_element;


    try {
        lisp->checkState(this);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4)
            evalAsInteger(3,lisp, ix);
        third_element = first_element->search_element(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return third_element;
}


Element* List_count_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* third_element;


    try {
        lisp->checkState(this);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4) {
            evalAsInteger(3,lisp, ix);
        }
        third_element = first_element->count_all_elements(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return third_element;
}

Element* List_searchall_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* third_element;


    try {
        lisp->checkState(this);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4) {
            evalAsInteger(3,lisp, ix);
        }
        third_element = first_element->search_all_elements(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return third_element;
}

Element* List_replaceall_eval::eval(LispE* lisp) {
    Element* object = liste[1]->eval(lisp);
    Element* search = null_;
    Element* replace = null_;
    Element* e;
    
    try {
        lisp->checkState(this);
        search = liste[2]->eval(lisp);
        replace = liste[3]->eval(lisp);
        e = object->replace_all_elements(lisp, search, replace);
        object->release();
        search->release();
        replace->release();
    }
    catch (Error* err) {
        object->release();
        search->release();
        replace->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return e;
}


Element* List_select_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* second_element = null_;
    for (long i = 1; i < listsize && !second_element->Boolean(); i++) {
        second_element = liste[i]->eval_terminal(lisp, terminal);
    }
    return second_element;
}


Element* List_replicate_eval::eval(LispE* lisp) {
    /*
     (replicate nb list)
     */
    long nb;
    evalAsInteger(1, lisp, nb);
    List* l = lisp->provideList();
    Element* element = liste[2]->eval(lisp);
    
    while (nb) {
        l->append(element->copying());
        nb--;
    }
    element->release();
    return l;
}

Element* List_rotate_eval::eval(LispE* lisp) {
    Element* matrix = liste[1]->eval(lisp);
    Element* result = null_;

    bool left = false;
    
    try {
        lisp->checkState(this);
        if (liste.size() >= 3) {
            result = liste[2]->eval(lisp);
            if (result->isNumber()) {
                long nb = result->asInteger();
                result->release();
                if (liste.size() == 4 && matrix->isTensor()) {
                    result = liste[3]->eval(lisp);
                    left = result->Boolean();
                    result->release();
                    if (left)
                        result = ((List*)matrix)->List::rotate(lisp, nb);
                    else
                        result = matrix->rotate(lisp, nb);
                }
                else
                    result = matrix->rotate(lisp, nb);
                
                if (matrix != result)
                    matrix->release();
                lisp->resetStack();
                return result;
            }
            left = result->Boolean();
            result->release();
        }
        
        result = matrix->rotating(lisp, left);
        if (matrix != result)
            matrix->release();
    }
    catch (Error* err) {
        matrix->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}


Element* List_revertsearch_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* third_element;


    try {
        lisp->checkState(this);
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (liste.size() == 4) {
            evalAsInteger(3,lisp, ix);
        }
        third_element = first_element->search_reverse(lisp, second_element, ix);
        first_element->release();
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return third_element;
}

Element* List_extend_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    if (!container->isList()) {
        container->release();
        Error* err = new Error(L"Error: missing list in 'extend'");
        lisp->delegation->set_error_context(err, idxinfo);
        throw err;
    }
    
    container = container->duplicate_constant(lisp);
    
    Element* value = null_;
    Element* e;
    
    
    try {
        lisp->checkState(this);
        //We store a value in a list
        value = liste[2]->eval(lisp);
        if (value->isList()) {
            if (container->type == t_llist) {
                container->concatenate(lisp, value);
            }
            else {
                for (long i = 0; i < value->size(); i++) {
                    e = value->index(i)->copying(false);
                    container->append(e);
                    e->release();
                }
            }
        }
        else {
            value = value->copying(false);
            container->append(value);
        }
        value->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return container;
}

Element* List_type_eval::eval(LispE* lisp) {
    Element* element  = liste[1]->eval(lisp);
    Element* atom_type = lisp->provideAtom(element->type);
    element->release();
    
    return atom_type;
}

Element* List_reverse_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    Element* value;
    
    bool duplicate = false;
    
    if (liste.size() == 3) {
        value = liste[2]->eval(lisp);
        duplicate = value->Boolean();
        value->release();
    }
    value = container->reverse(lisp, duplicate);
    if (value != container)
        container->release();
    return value;
}

Element* List_xor_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    char check = (char)element->Boolean();
    
    long listsize = liste.size();
    char test = true;
    
    try {
        lisp->checkState(this);
        for (long i = 2; i < listsize && test; i++) {
            element->release();
            element = liste[i]->eval(lisp);
            test = check;
            check = (char)element->Boolean();
            test ^= check;
        }
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    element->release();
    return booleans_[test];
}

#ifdef LISPE_WASM_NO_EXCEPTION
Element* List_setqv_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    if (thrown_error)
        return element;
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            element->release();
            return new Error("Error: a list of variable should have the same size as its value.");
        }
        long sz = liste[1]->size();
        uint16_t label;
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            lisp->storing_variable(element->index(i), label);
        }
        element->release();
    }
    else
        lisp->storing_variable(element, liste[1]->label());
    return element;
}

Element* List_setq_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    if (thrown_error)
        return element;
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            element->release();
            return new Error("Error: a list of variable should have the same size as its value.");
        }
        long sz = liste[1]->size();
        uint16_t label;
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            lisp->storing_variable(element->index(i), label);
        }
        element->release();
    }
    else
        lisp->storing_variable(element, liste[1]->label());
    return True_;
}

Element* List_setqequal_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    if (thrown_error)
        return element;
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            element->release();
            return new Error("Error: a list of variable should have the same size as its value.");
        }
        long sz = liste[1]->size();
        uint16_t label;
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            lisp->recording_back(element->index(i), label);
        }
        element->release();
    }
    else
        lisp->recording_back(element, liste[1]->label());
    return True_;
}

#else

Element* List_record_in_stack_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->checkState(this);
    if (lisp->terminal_stack_variables)
        lisp->top_new_stack->replace_stack_value(element->duplicate_constant(lisp), liste[1]->label());
    else
        lisp->top_new_stack->storing_variable(element->duplicate_constant(lisp), liste[1]->label());
    lisp->resetStack();
    return into_stack_;
}

Element* List_setqv_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->checkState(this);
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            element->release();
            Error* err = new Error("Error: a list of variable should have the same size as its value.");
            lisp->delegation->set_error_context(err, idxinfo);
            throw err;

        }
        long sz = liste[1]->size();
        uint16_t label;
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            lisp->storing_variable(element->index(i), label);
        }
        element->release();
    }
    else
        lisp->storing_variable(element, liste[1]->label());
    lisp->resetStack();
    return element;
}

Element* List_setq_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->checkState(this);
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            lisp->resetStack();
            element->release();
            Error* err = new Error("Error: a list of variable should have the same size as its value.");
            lisp->delegation->set_error_context(err, idxinfo);
            throw err;
        }
        long sz = liste[1]->size();
        uint16_t label;
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            lisp->storing_variable(element->index(i), label);
        }
        element->release();
    }
    else
        lisp->storing_variable(element, liste[1]->label());
    lisp->resetStack();
    return True_;
}

Element* List_setqequal_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->checkState(this);
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            lisp->resetStack();
            element->release();
            Error* err = new Error("Error: a list of variable should have the same size as its value.");
            lisp->delegation->set_error_context(err, idxinfo);
            throw err;
        }
        long sz = liste[1]->size();
        uint16_t label;
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            lisp->recording_back(element->index(i), label);
        }
        element->release();
    }
    else
        lisp->recording_back(element, liste[1]->label());
    lisp->resetStack();
    return True_;
}

Element* List_setqi_eval::eval(LispE* lisp) {
    List_instance* instance = lisp->current_instance;
    if (instance == NULL) {
        Error* err = new Error("Error: setqi can only be used within an instance");
        lisp->delegation->set_error_context(err, idxinfo);
        throw err;
    }
    
    lisp->checkState(this);
    Element* element = liste[2]->eval(lisp)->duplicate_constant(lisp);
    int16_t label = liste[1]->label();
    long i = instance->names.search(label);
    if (i != -1) {
        //In this case, we force the value onto the existing variable...
        if (element != instance->liste[i]) {
            instance->liste[i]->decrement();
            instance->liste[i] = element;
            element->increment();
        }
    }
    else {
        instance->names.push_back(label);
        instance->append(element);
    }
    lisp->resetStack();
    return True_;
}

#endif

Element* List_set_const_eval::eval(LispE* lisp) {
    int16_t label = liste[1]->label();
    Element* element = liste[2]->eval(lisp);
    lisp->delegation->const_values[label] = true;
    lisp->checkState(this);
    lisp->storing_global(element, label);
    lisp->resetStack();
    if (element->status != s_constant)
        lisp->storeforgarbage(element);
    return True_;
}

Element* List_setg_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    if (liste[1]->type == t_list) {
        if (liste[1]->size() != element->size()) {
            element->release();
            Error* err = new Error("Error: a list of variable should have the same size as its value.");
            lisp->delegation->set_error_context(err, idxinfo);
            throw err;
        }
        long sz = liste[1]->size();
        uint16_t label;
        lisp->checkState(this);
        for (long i = 0; i < sz; i++) {
            label = liste[1]->index(i)->label();
            if (!lisp->delegation->replaceFunction(element->index(i), label, lisp->current_space))
                lisp->storing_global(element->index(i), label);
        }
        element->release();
        lisp->resetStack();
        return True_;
    }
    
    int16_t label = liste[1]->label();
    lisp->checkState(this);
    if (!lisp->delegation->replaceFunction(element, label, lisp->current_space))
        lisp->storing_global(element, label);
    lisp->resetStack();
    return True_;
}

Element* List_seth_eval::eval(LispE* lisp) {
    if (lisp->check_thread_stack) {
        Element* element = liste[2]->eval(lisp);
        if (liste[1]->type == t_list) {
            if (liste[1]->size() != element->size()) {
                element->release();
                return lisp->check_error(this, new Error("Error: a list of variable should have the same size as its value."), idxinfo);
            }
            long sz = liste[1]->size();
            uint16_t label;
            lisp->checkState(this);
            for (long i = 0; i < sz; i++) {
                label = liste[1]->index(i)->label();
                lisp->delegation->thread_stack.storing_variable(element->index(i)->duplicate_constant(lisp), liste[1]->label());
            }
            element->release();
            lisp->resetStack();
            return True_;
        }

        lisp->checkState(this);
        lisp->delegation->thread_stack.storing_variable(element->duplicate_constant(lisp), liste[1]->label());
        lisp->resetStack();
        return True_;
    }
    throw new Error("Error: this instruction can only be used in a 'threadspace' block");
}


Element* List_print_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element = null_;
    
    string val;
    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            element = liste[i]->eval(lisp);
#ifdef LISPE_WASM_NO_EXCEPTION
            if (thrown_error != NULL)
                return thrown_error;
#endif
            val += element->toString(lisp);
            element->release();
        }
        
        lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
        if (lisp->isThread)
            std::cout.flush();
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return emptyatom_;
}


Element* List_printerr_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element;
    
    
    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            element = liste[i]->eval(lisp);
#ifdef LISPE_WASM_NO_EXCEPTION
            if (thrown_error != NULL)
                return thrown_error;
#endif
            std::cerr << element->toString(lisp);
            element->release();
        }
        
        if (lisp->isThread)
            std::cerr.flush();
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return emptyatom_;
}


Element* List_printerrln_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element;
    
    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            element = liste[i]->eval(lisp);
#ifdef LISPE_WASM_NO_EXCEPTION
            if (thrown_error != NULL)
                return thrown_error;
#endif
            if (i != 1)
                std::cerr << " ";
            std::cerr << element->toString(lisp);
            element->release();
        }
        
        std::cerr << std::endl;
        if (lisp->isThread)
            std::cerr.flush();
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return emptyatom_;
}


Element* List_println_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element;
    string val;
    
    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            element = liste[i]->eval(lisp);
#ifdef LISPE_WASM_NO_EXCEPTION
            if (thrown_error != NULL)
                return thrown_error;
#endif
            if (i != 1)
                val += " ";
            val += element->toString(lisp);
            element->release();
        }
        
#ifdef WIN32
        val += "\r\n";
#else
        val += "\n";
#endif
        
        lisp->delegation->display_string_function(val, lisp->delegation->reading_string_function_object);
        if (lisp->isThread)
            std::cout.flush();
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return emptyatom_;
}



Element* List_set_eval::eval(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet();

    Set* n = lisp->provideSet();
    Element* values;


    try {
        lisp->checkState(this);
        for (long e = 1; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            n->add(lisp, values);
            values->release();
        }
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_sets_eval::eval(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_s();

    Element* values = liste[1]->eval(lisp);

    Set_s* n = lisp->provideSet_s();

    try {
        lisp->checkState(this);
        if (values->isSet()) {
            if (values->type == t_sets)
                n->ensemble = ((Set_s*)values)->ensemble;
            else {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asUString(lisp));
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
        }
        else {
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->add(values->index(i)->asUString(lisp));
                }
            }
            else
                n->add(values->asUString(lisp));
        }
        values->release();

        for (long e = 2; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isSet()) {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asUString(lisp));
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asUString(lisp));
                    }
                }
                else
                    n->add(values->asUString(lisp));
            }
            values->release();
        }
        
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_seti_eval::eval(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_i();

    Element* values = liste[1]->eval(lisp);

    Set_i* n = lisp->provideSet_i();

    try {
        lisp->checkState(this);
        if (values->isSet()) {
            if (values->type == t_seti)
                n->ensemble = ((Set_i*)values)->ensemble;
            else {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asInteger());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
        }
        else {
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->add(values->index(i)->asInteger());
                }
            }
            else
                n->add(values->asNumber());
        }
        
        values->release();

        for (long e = 2; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isSet()) {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asInteger());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asInteger());
                    }
                }
                else
                    n->add(values->asInteger());
            }
            values->release();
        }
        
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}

Element* List_setn_eval::eval(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideSet_n();

    Element* values = liste[1]->eval(lisp);

    Set_n* n = lisp->provideSet_n();

    try {
        lisp->checkState(this);
        if (values->isSet()) {
            if (values->type == t_setn)
                n->ensemble = ((Set_n*)values)->ensemble;
            else {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asNumber());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
        }
        else {
            if (values->isList()) {
                for (long i = 0; i < values->size(); i++) {
                    n->add(values->index(i)->asNumber());
                }
            }
            else
                n->add(values->asNumber());
        }
        values->release();

        for (long e = 2; e < listsz; e++) {
            values = liste[e]->eval(lisp);
            if (values->isSet()) {
                void* iter = values->begin_iter();
                Element* nxt = values->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    n->ensemble.insert(nxt->asNumber());
                    nxt = values->next_iter_exchange(lisp, iter);
                }
                values->clean_iter(iter);
            }
            else {
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->add(values->index(i)->asNumber());
                    }
                }
                else
                    n->add(values->asNumber());
            }
            values->release();
        }
        
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return n;
}


Element* List_minmax_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);

    long listsize = liste.size();

    Element* max_element = first_element;
    Element* min_element = first_element;
    Element* res;

    try {
        lisp->checkState(this);
        if (listsize == 2) {
            //In this case, the argument must be a list
            res = first_element->minmax(lisp);
            first_element->release();
            lisp->resetStack();
            return res;
        }

        for (long i = 2; i < listsize; i++) {
            first_element = liste[i]->eval(lisp);
            if (first_element->less(lisp, min_element)->Boolean()) {
                if (max_element != min_element)
                    min_element->release();
                min_element = first_element;
            }
            else {
                if (first_element->more(lisp, max_element)->Boolean()) {
                    if (max_element != min_element)
                        max_element->release();
                    max_element = first_element;
                }
                else {
                    first_element->release();
                }
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        if (first_element != max_element)
            max_element->release();
        if (first_element != min_element)
            min_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    res = lisp->provideList();
    res->append(min_element);
    res->append(max_element);
    min_element->release();
    if (min_element != max_element)
        max_element->release();
    lisp->resetStack();
    return res;
}

Element* List_max_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    long listsize = liste.size();
    Element* second_element;

    try {
        lisp->checkState(this);
        if (listsize == 2) {
            //In this case, the argument must be a list
            second_element = first_element->maximum(lisp);
            first_element->release();
            lisp->resetStack();
            return second_element;
        }

        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (first_element->less(lisp, second_element)->Boolean()) {
                first_element->release();
                first_element = second_element;
            }
            else {
                second_element->release();
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return first_element;
}

#ifdef LISPE_WASM_NO_EXCEPTION
Element* List_maybe_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* first_element;
    lisp->checkState(this);
    
    if (listsize == 2) {
        first_element = liste[1]->eval(lisp);
        bool val = (first_element->label() == t_maybe);
        first_element->release();
        lisp->resetStack();
        return booleans_[val];
    }
    
    first_element = null_;
    for (long i = 1; i < listsize - 1 && thrown_error == NULL; i++) {
        first_element->release();
        first_element = liste[i]->eval(lisp);
    }

    if (thrown_error) {
        lisp->delegation->reset_context();
        first_element = liste.back()->eval(lisp);
        lisp->resetStack();
        return first_element;
    }
    lisp->resetStack();
    return first_element;
}
#else
Element* List_maybe_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* first_element;

    

    //Otherwise, we test each value for error and then we send the last one back
    try {
        lisp->checkState(this);
        if (listsize == 2) {
            first_element = liste[1]->eval(lisp);
            bool val = (first_element->label() == t_maybe);
            first_element->release();
            lisp->resetStack();
            return booleans_[val];
        }
        
        first_element = null_;
        for (long i = 1; i < listsize - 1; i++) {
            first_element->release();
            first_element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        first_element = liste.back();
        if (first_element->type == t_call_lambda) {
            List_call_lambda exec;
            exec.append(first_element);
            u_ustring u = err->asUString(lisp);
            exec.append(lisp->provideString(u));
            err->release();
            lisp->delegation->reset_context();
            first_element = exec.eval(lisp);
            lisp->resetStack();
            exec.liste.decrement();
            return first_element;
        }
        
        err->release();
        lisp->delegation->reset_context();
        first_element = first_element->eval(lisp);
        lisp->resetStack();
        return first_element;
    }
    lisp->resetStack();
    return first_element;
}
#endif

Element* List_min_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    long listsize = liste.size();
    Element* second_element;

    try {
        lisp->checkState(this);
        if (listsize == 2) {
            //In this case, the argument must be a list
            second_element = first_element->minimum(lisp);
            first_element->release();
            lisp->resetStack();
            return second_element;
        }

        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            if (first_element->more(lisp, second_element)->Boolean()) {
                first_element->release();
                first_element=second_element;
            }
            else {
                second_element->release();
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return first_element;
}


Element* List_nconc_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* result = null_;
    Element* element = null_;
    Element* e;

    long i, l;

    try {
        lisp->checkState(this);
        element = liste[1]->eval(lisp);
        if (element->isList())
            result = element->duplicate_constant(lisp);
        else
            throw new Error("Error: first element is not a list");

        element = emptylist_;
        for (i = 2; i < listsize; i++) {
            element = liste[i]->eval(lisp);
            if (element->isList()) {
                for (l = 0; l < element->size(); l++) {
                    e = element->value_on_index(lisp, l);
                    result->append(e);
                    e->release();
                }
            }
            else
                result->append(element);
            
            _releasing(element);
        }
    }
    catch (Error* err) {
        result->release();
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}

Element* List_nconcn_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1)
        return emptylist_;

    Element* result = null_;
    Element* element = null_;
    Element* e;

    long i, l;

    try {
        lisp->checkState(this);
        element = liste[1]->eval(lisp);
        if (element->isList())
            result = element->copyatom(lisp, 1);
        else
            throw new Error("Error: first element is not a list");

        element = emptylist_;
        for (i = 2; i < listsize; i++) {
            element = liste[i]->eval(lisp);
            if (element->isList()) {
                for (l = 0; l < element->size(); l++) {
                    e = element->value_on_index(lisp, l);
                    result->append(e);
                    e->release();
                }
            }
            else
                result->append(element);
            _releasing(element);
        }
    }
    catch (Error* err) {
        result->release();
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return result;
}
//--------------------
Element* List_andvalue_eval::eval(LispE* lisp) {
    Element* second_element = null_;
    bool test = true;

    for (long i = 1; i < size() && test; i++) {
        second_element->release();
        second_element = liste[i]->eval(lisp);
        test = second_element->Boolean();
    }
    return second_element;
}


Element* List_apply_eval::eval(LispE* lisp) {
    Element* function = liste[1]->eval(lisp);
    Element* arguments = NULL;
    List* call = NULL;
    Element* result = NULL;
    int16_t lab = function->label();
    bool use_list = !lisp->delegation->check_straight(lab);

    try {
        lisp->checkState(this);
        //(apply func l)
        arguments = liste[2]->eval(lisp);
        if (arguments->type == t_list) {
            if (arguments->status) {
                if (use_list)
                    call = lisp->provideList();
                else
                    call = lisp->delegation->straight_eval[lab]->cloning();
                call->append(function);
                call->extend((List*)arguments);
                arguments = call;
                call = NULL;
            }
            else {
                if (!arguments->insertion(function, 0))
                    throw new Error("Error: cannot insert this element in this list");
            }
            
            lisp->check_arity(lab, arguments->size());
            result = arguments->eval(lisp);
            arguments->force_release();
            lisp->resetStack();
            return result;
        }

        if (!arguments->isList())
            throw new Error("Error: arguments to 'apply' should be given as a list");

        if (use_list)
            call = lisp->provideList();
        else
            call = lisp->delegation->straight_eval[lab]->cloning();
        call->append(function);
        call = (List*)arguments->asList(lisp, call);
        if (call == arguments) {
            call = NULL;
            arguments = NULL;
            throw new Error("Error: wrong use of 'apply'");
        }

        lisp->check_arity(lab, call->size());
        result = call->eval(lisp);

        call->force_release();
        arguments->release();
    }
    catch (Error* err) {
        if (call != NULL) {
            call->force_release();
            arguments->release();
        }
        else {
            if (arguments == NULL)
                function->release();
            else
                arguments->force_release();
        }
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}

Element* List_over_eval::eval(LispE* lisp) {
    Element* function = liste[1]->eval(lisp);
    Element* arguments = NULL;
    List* call = NULL;
    Element* result = NULL;
    int16_t lab = function->label();
    bool use_list = !lisp->delegation->check_straight(lab);

    try {
        lisp->checkState(this);
        if (use_list)
            call = lisp->provideList();
        else
            call = lisp->delegation->straight_eval[lab]->cloning();

        arguments = liste[2]->eval(lisp);
        if (!arguments->isList())
            throw new Error("Error: argument to 'over' should be a list");
        
        call->append(function);
        call->append(lisp->quoted(arguments));
        result = call->eval(lisp);
        //We then replace the values in arguments with the values in result
        if (result->label() != arguments->label())
            throw new Error("Error: The result should be of the same type as the argument");
        arguments->copyfrom(result);
        function->release();
        call->force_release();
        result->release();
    }
    catch (Error* err) {
        if (call != NULL) {
            call->force_release();
            arguments->release();
        }
        else {
            if (arguments == NULL)
                function->release();
            else
                arguments->release();
        }
        if (result != NULL)
            result->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return arguments;
}

Element* List_shift_eval::eval(LispE* lisp) {
    //(shift func l (nb))
    Element* function = liste[1]->eval(lisp);
    long nb = 1;
    Element* arguments = NULL;
    List* call = NULL;
    Element* result = NULL;
    int16_t lab = function->label();
    bool use_list = !lisp->delegation->check_straight(lab);
    char right = 0;

    try {
        lisp->checkState(this);
        arguments = liste[2]->eval(lisp);
        long sz = arguments->size();

        if (liste.size() == 4) {
            nb = liste[3]->eval(lisp)->asInteger();
            if (nb < 0) {
                right = true;
                nb *= -1;
            }
        }

        if (sz <= nb) {
            function->release();
            arguments->release();
            lisp->resetStack();
            return null_;
        }
        long i;
        bool change_to_llist = false;

        if (arguments->type == t_list || arguments->type == t_llist) {
            if (use_list)
                call = lisp->provideList();
            else
                call = lisp->delegation->straight_eval[lab]->cloning();
            call->append(function);
            List* l1 = lisp->provideList();
            List* l2;
            if (arguments->type == t_llist) {
                change_to_llist = true;
                LList* l = (LList*)arguments;
                l2 = lisp->provideList();
                i = 0;
                for (u_link* a = l->liste.begin(); a != NULL; a = a->next()) {
                    if (i >= nb)
                        l2->append(a->value);
                    if (i < sz - nb)
                        l1->append(a->value);
                    i++;
                }
            }
            else {
                l2 = new List((List*)arguments, nb);
                for (i = 0; i < sz - nb; i++)
                    l1->append(((List*)arguments)->liste[i]);
            }
            call->append(lisp->quoted());
            call->append(lisp->quoted());
            call->in_quote(1+right, l1);
            call->in_quote(2-right, l2);
            
            lisp->check_arity(lab, P_TWO);
            result = call->eval(lisp);
            call->force_release();
            arguments->release();
            lisp->resetStack();
            if (change_to_llist) {
                LList* nl = new LList(&lisp->delegation->mark);
                for (i = result->size() - 1; i >= 0; i--) {
                    nl->push_front(result->index(i));
                }
                result->release();
                result = nl;
            }
            return result;
        }

        if (!arguments->isList())
            throw new Error("Error: arguments to 'shift' should be given as a list");

        if (use_list)
            call = lisp->provideList();
        else
            call = lisp->delegation->straight_eval[lab]->cloning();
        call->append(function);
        Element* l1;
        Element* l2;
        switch (arguments->type) {
            case t_shorts:
                l1 = new Shorts();
                for (i = 0; i < sz - nb; i++)
                    ((Shorts*)l1)->liste.push_back(((Shorts*)arguments)->liste[i]);
                l2 = new Shorts((Shorts*)arguments, nb);
                break;
            case t_integers:
                l1 = lisp->provideIntegers();
                for (i = 0; i < sz - nb; i++)
                    ((Integers*)l1)->liste.push_back(((Integers*)arguments)->liste[i]);
                l2 = lisp->provideIntegers((Integers*)arguments, nb);
                break;
            case t_floats:
                l1 = lisp->provideFloats();
                for (i = 0; i < sz - nb; i++)
                    ((Floats*)l1)->liste.push_back(((Floats*)arguments)->liste[i]);
                l2 = lisp->provideFloats((Floats*)arguments, nb);
                break;
            case t_numbers:
                l1 = lisp->provideNumbers();
                for (i = 0; i < sz - nb; i++)
                    ((Numbers*)l1)->liste.push_back(((Numbers*)arguments)->liste[i]);
                l2 = lisp->provideNumbers((Numbers*)arguments, nb);
                break;
            case t_strings:
                l1 = lisp->provideStrings();
                for (i = 0; i < sz - nb; i++)
                    ((Strings*)l1)->liste.push_back(((Strings*)arguments)->liste[i]);
                l2 = new Strings((Strings*)arguments, nb);
                break;
            case t_stringbytes:
                l1 = new Stringbytes();
                for (i = 0; i < sz - nb; i++)
                    ((Stringbytes*)l1)->liste.push_back(((Stringbytes*)arguments)->liste[i]);
                l2 = new Stringbytes((Stringbytes*)arguments, nb);
                break;
            default:
                throw new Error("Error: arguments to 'shift' should be given as a list");
        }
        
        call->append(null_);
        call->append(null_);
        call->set_in(lisp, l1, 1+right);
        call->set_in(lisp, l2, 2-right);
        
        lisp->check_arity(lab, P_TWO);
        result = call->eval(lisp);

        call->force_release();
        arguments->release();
    }
    catch (Error* err) {
        if (call != NULL) {
            call->force_release();
            arguments->release();
        }
        else {
            if (arguments == NULL)
                function->release();
            else
                arguments->release();
        }
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}


Element* List_atomise_eval::eval(LispE* lisp) {
    Element* values = liste[1]->eval(lisp);
    Element* theatoms = lisp->atomise(values->asUString(lisp));
    values->release();
    return theatoms;
}


Element* List_converttoatom_eval::eval(LispE* lisp) {
    u_ustring tampon;
    evalAsUString(1,lisp,tampon);
    long sz = tampon.size();
    if (tampon[0] == 'c' && tampon.back() == 'r' && sz > 3) {
        // we check if we don't have a variation on car/cdr/cadr/caar etc...
        sz-=2;
        while (sz != 1) {
            if (tampon[sz] != 'a' && tampon[sz] != 'd')
                return lisp->provideAtomProtected(tampon);
            sz--;
        }
        return  lisp->provideCADR(tampon);
    }
    return lisp->provideAtomProtected(tampon);
}


Element* List_bodies_eval::eval(LispE* lisp) {
    Element* function = liste[1]->eval(lisp);
    int16_t ty = function->protected_index(lisp, (long)0)->type;
    if (ty == l_defpat || ty == l_defpred || ty == l_defprol) {
        List* functions =  lisp->provideList();
        int16_t label = function->protected_index(lisp, (long)1)->label();
        lisp->checkState(this);
        const auto& am = lisp->delegation->method_pool[lisp->current_space]->find(label);
        if (am != lisp->delegation->method_pool[lisp->current_space]->end()) {
            for (const auto& a: am->second) {
                for (const auto& b : a.second) {
                    functions->append(b);
                }
            }
        }
        lisp->resetStack();
        return functions;
    }
    return function;
}

#ifdef LISPE_WASM_NO_EXCEPTION
Element* List_catch_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element = null_;

    lisp->checkState(this);
    for (int16_t i = 1; i < listsize && thrown_error == NULL; i++) {
        element->release();
        element = liste[i]->eval(lisp);
    }
    if (thrown_error) {
        //This error is converted into a non-blocking error message .
        element = new Maybe(lisp, thrown_error);
        lisp->delegation->reset_context();
    }
    lisp->resetStack();
    return element;
}
#else
Element* List_catch_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* element = null_;

    try {
        lisp->checkState(this);
        for (int16_t i = 1; i < listsize; i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        //This error is converted into a non-blocking error message .
        element = new Maybe(lisp, err);
        lisp->delegation->reset_context();
        err->release();
    }
    lisp->resetStack();
    return element;
}
#endif

Element* List_cyclicp_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    bool b = element->is_cyclic();
    element->release();
    return booleans_[b];
}


Element* List_converttoshort_eval::eval(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = new Short(value->asShort());
    value->release();
    return element;
}


Element* List_converttofloat_eval::eval(LispE* lisp) {
    Element* value = liste[1]->eval(lisp);
    Element* element = lisp->provideFloat(value->asFloat());
    value->release();
    return element;
}


Element* List_elapse_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* second_element = null_;
    double diff = 0;

    try {
        lisp->checkState(this);
        std::chrono::high_resolution_clock::time_point chrono_beg = std::chrono::high_resolution_clock::now();
        second_element = null_;
        for (long i = 1; i < listsize && second_element->type != l_return; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
        }
        std::chrono::high_resolution_clock::time_point chrono_end = std::chrono::high_resolution_clock::now();

        if (second_element->type == l_return) {
            Element* body = second_element->eval(lisp);
            body->release();
        }
        second_element->release();
        diff = std::chrono::duration_cast<std::chrono::milliseconds>( chrono_end - chrono_beg).count();
    }
    catch (Error* err) {
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return lisp->provideNumber(diff);
}

Element* List_getfast_eval::eval(LispE* lisp) {
    //The number of values is 16, which means the remainder of X/16 is the same as X & 15
    return lisp->fast_variables[(liste[1]->label() - l_var0)&15];
}

Element* List_setfast_eval::eval(LispE* lisp) {
    int16_t label = (liste[1]->label() - l_var0)&15;
    Element* val = liste[2]->eval(lisp);
    val->increment();
    lisp->fast_variables[label]->decrement();
    lisp->fast_variables[label] = val;
    return val;
}

//(tan . sqrt . sum . integers 10 20 30 40 50)
//(tan . sqrt . sum . map '(2 /) . floats 10 20 1)
Element* List_run_linear::eval(LispE* lisp) {
    Element* e = null_;
    //cout << components.toString(lisp) << endl;
    for (long i = 0; i < components.size(); i++) {
        e = components.liste[i]->eval(lisp);
    }
    
    return e;
}

Element* List_eval_eval::eval(LispE* lisp) {
    Element* code = liste[1]->eval(lisp);
    Element* result;
    try {
        lisp->checkState(this);
        //This is a specific case, when the element is a string
        //we need then to call the a meta-eval, the one that
        //comes with Lisp itself
        if (code->isString()) {
            u_ustring u_code = code->asUString(lisp);
            result = lisp->eval(u_code);
            if (result->isError()) {
                u_ustring msg = result->asUString(lisp);
                delete result;
                throw new Error(msg);
            }
            code->release();
            lisp->resetStack();
            return result;
        }

        if (!code->size()) {
            lisp->resetStack();
            return code;
        }

        //We just need to evaluate this element...
        lisp->check_arity_on_fly = true;
        result = code->eval(lisp);
        lisp->check_arity_on_fly = false;
        if (result != code) {
            if (!code->element_container(result))
                code->release();
        }
    }
    catch (Error* err) {
        code->release();
        lisp->check_arity_on_fly = false;
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}


Element* List_factorial_eval::eval(LispE* lisp) {
    static uint64_t factorials[] = {1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880L, 3628800L, 39916800L, 479001600L, 6227020800L, 87178291200L, 1307674368000L, 20922789888000L, 355687428096000L, 6402373705728000L, 121645100408832000L, 2432902008176640000L};

    Element* e = liste[1]->eval(lisp);

    Element* r = null_;
    long value = 0;
    unsigned long res = 1;
    try {
        lisp->checkState(this);
        if (e->isList()) {
            long listsize = e->size();
            r = lisp->provideNumbers();
            for (long j = 0; j < listsize; j++) {
                res = 1;
                value = e->index(j)->asInteger();
                if (value < 0)
                    throw new Error("Error: factorial of a negative number does not exists");
                if (value <= 20)
                    ((Numbers*)r)->liste.push_back(factorials[value]);
                else {
                    res = (long)factorials[20];
                    for (long i = 21; i <= value; i++) {
                        res *= i;
                    }
                    ((Numbers*)r)->liste.push_back(factorials[value]);
                }
            }
            e->release();
            lisp->resetStack();
            return r;
        }

        value = e->asInteger();
        if (value < 0)
            throw new Error("Error: factorial of a negative number does not exists");
        e->release();
        if (value <= 20) {
            lisp->resetStack();
            return lisp->provideInteger((long)factorials[value]);
        }
        res = (long)factorials[20];
        for (long i = 21; i <= value; i++)
        res *= i;
    }
    catch (Error* err) {
        e->release();
        r->release();
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return lisp->provideInteger(res);
}


Element* List_bappend_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string chemin = element->toString(lisp);
    element->release();
    std::ofstream o(chemin.c_str(), std::ios::binary|std::ios::app);
    if (o.fail()) {
        string erreur = "Error: Cannot write in file: ";
        erreur += chemin;
        throw new Error(erreur);
    }

    element = liste[2]->eval(lisp);
    //In this case, element must be a list of numerical values;
    if (!element->isList()) {
        element->release();
        throw new Error("Error: Expecting a list of numerical values");
    }
    uchar c;
    for (long i = 0; i < element->size(); i++) {
        c = element->index(i)->asShort();
        o << c;
    }
    
    element->release();
    return True_;
}


Element* List_bread_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string filename = element->toString(lisp);
    element->release();
    element = lisp->provideString();
    try {
        lisp->checkState(this);
        element = element->chargebin(lisp, filename);
        lisp->resetStack();
        return element;
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }
}


Element* List_bwrite_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string filename = element->toString(lisp);
    element->release();

    std::ofstream o(filename.c_str(), std::ios::binary);
    if (o.fail()) {
        string erreur = "Error: Cannot write in file: ";
        erreur += filename;
        throw new Error(erreur);
    }

    element = liste[2]->eval(lisp);
    //In this case, element must be a list of numerical values;
    if (!element->isList()) {
        element->release();
        throw new Error("Error: Expecting a list of numerical values");
    }
    uchar c;
    for (long i = 0; i < element->size(); i++) {
        c = element->index(i)->asShort();
        o << c;
    }
    element->release();
    return True_;
}

Element* List_fappend_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string chemin = element->toString(lisp);
    element->release();
    std::ofstream o(chemin.c_str(), std::ios::binary|std::ios::app);
    if (o.fail()) {
        string erreur = "Error: Cannot write in file: ";
        erreur += chemin;
        throw new Error(erreur);
    }

    element = liste[2]->eval(lisp);

    //We put ourselves in append mode
    chemin = element->toString(lisp);
    o << chemin;
    element->release();
    return True_;
}


Element* List_fread_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string filename = element->toString(lisp);
    element->release();
    element = new Stringbyte();
    try {
        lisp->checkState(this);
        element = element->charge(lisp, filename);
        lisp->resetStack();
        return element;
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }
}


class File_element : public Element {
public:
    FILE* f;
    string op;

    File_element() : Element(t_fileelement) {
        f = NULL;
        op = "";
    }
    
    File_element(LispE* lisp, Element* n, string o) : Element(t_fileelement) {
        op = o;
        string filename = n->toString(lisp);
        n->release();
#ifdef WIN32
        fopen_s(&f, STR(filename), STR(op));
#else
        f = fopen(STR(filename), STR(op));
#endif
    }
    
    ~File_element() {
        if (f != NULL)
            fclose(f);
    }
    
    bool isFile() {
        return (f != NULL);
    }

    void close() {
        if (f != NULL)
            fclose(f);
        f = NULL;
    }
};

Element* List_fopen_eval::eval(LispE* lisp) {
    Element* element;
    string op = "r";
    if (size() == 3) {
        element = liste[2]->eval(lisp);
        op = element->toString(lisp);
        element->release();
    }
    element = liste[1]->eval(lisp);
    return new File_element(lisp, element, op);
}

Element* List_fclose_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (!element->isFile())
        throw new Error("Error: Not a file element");
    
    ((File_element*)element)->close();
    element->release();
    return True_;
}

Element* List_fseek_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (!element->isFile())
        throw new Error("Error: Not a file element");
    
    FILE* f = ((File_element*)element)->f;
    long nb;
    evalAsInteger(2, lisp, nb);
    fseek(f, nb, 0);
    element->release();
    return True_;
}

Element* List_ftell_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (!element->isFile())
        throw new Error("Error: Not a file element");
    
    FILE* f = ((File_element*)element)->f;
    long nb = ftell(f);
    element->release();
    return lisp->provideInteger(nb);
}

Element* List_fgetchars_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (!element->isFile())
        throw new Error("Error: Not a file element");
    
    FILE* f = ((File_element*)element)->f;
    long nb;
    evalAsInteger(2, lisp, nb);
    char* buffer = new char[nb + 1];
    fread(buffer, nb, 1, f);
    string b(buffer);
    delete[] buffer;
    element->release();
    return new Stringbyte(b);
}

Element* List_fputchars_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    string s = element->toString(lisp);
    element->release();
    
    element = liste[1]->eval(lisp);
    if (!element->isFile())
        throw new Error("Error: Not a file element");
    
    File_element* fe = (File_element*)element;
    if (fe->op[0] != 'r') {
        FILE* f = fe->f;
        
        for (long i = 0; i < s.size(); i++)
            fputc(s[i], f);
        
        fe->release();
        return True_;
    }
    fe->release();
    return False_;
}

Element* List_fsize_eval::eval(LispE* lisp) {
    struct stat scible;
    int stcible = -1;
    long size = -1;
    
    string name;

    FILE* thefile;
    
    Element* element = liste[1]->eval(lisp);
    if (element->type != t_fileelement)
        element = new File_element(lisp, element, "r");

    thefile = ((File_element*)element)->f;
    
    if (thefile != NULL) {
#ifdef LISPE_WASM
        stcible = fstat(fileno(thefile), &scible);
#else
#if (_MSC_VER >= 1900)
        stcible = fstat(_fileno(thefile), &scible);
#else
#if  defined(WIN32) | defined(APPLE)
        stcible = fstat(thefile->_file, &scible);
#else
        stcible = fstat(thefile->_fileno, &scible);
#endif
#endif
#endif
        if (stcible >= 0)
            size = scible.st_size;
    }
    element->release();
    return lisp->provideInteger(size);
}


Element* List_fwrite_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    string filename = element->toString(lisp);
    element->release();

    std::ofstream o(filename.c_str(), std::ios::binary);
    if (o.fail()) {
        string erreur = "Error: Cannot write in file: ";
        erreur += filename;
        throw new Error(erreur);
    }

    element = liste[2]->eval(lisp);

    filename = element->toString(lisp);
    o << filename;
    element->release();
    return True_;
}

Element* List_shorts_eval::eval(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return new Shorts();
    Shorts* n = NULL;
    Element* values;
    try {
        lisp->checkState(this);
        if (listsz == 2) {
            values = liste[1]->eval(lisp);
            if (values->type == t_shorts && !values->status)
                n = (Shorts*)values;
            else {
                n = new Shorts();
                values->flatten(lisp, n);
                values->release();
            }
        }
        else {
            n = new Shorts();
            n->liste.reserve(listsz<<1);
            for (long e = 1; e < listsz; e++) {
                values = liste[e]->eval(lisp);
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->liste.push_back(values->index(i)->asShort());
                    }
                }
                else
                    n->liste.push_back(values->asShort());
                values->release();
            }
        }
    }
    catch (Error* err) {
        delete n;
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return n;
}

Element* List_enumerate_eval::eval(LispE* lisp) {
    Element* e = liste[1]->eval(lisp);
    Element* lstr = e->to_strings(lisp);
    long i = 0;
    if (liste.size() == 3)
        evalAsInteger(2, lisp, i);
    if (lstr != e) {
        e->release();
        e = lstr;
    }
    return new Enumlist(e, i, null_);
}

Element* List_irange_eval::eval(LispE* lisp) {
    long sz  = liste.size();

    double init, inc;
    evalAsNumber(1, lisp, init);
    if (sz == 4) {
        double bound;
        evalAsNumber(2, lisp, bound);
        evalAsNumber(3, lisp, inc);
        if (init == (long)init && inc == (long)inc)
            return lisp->providerange_Integer(init, inc, bound);
        return lisp->providerange_Number(init, inc, bound);
    }

    evalAsNumber(2, lisp, inc);
    if (init == (long)init && inc == (long)inc)
        return lisp->providerange_Integer(init, inc);
    return lisp->providerange_Number(init, inc);
}


Element* List_irangein_eval::eval(LispE* lisp) {
    long sz  = liste.size();

    double init, inc;
    evalAsNumber(1, lisp, init);
    if (sz == 4) {
        double bound;
        evalAsNumber(2, lisp, bound);
        evalAsNumber(3, lisp, inc);
        if (init == (long)init && inc == (long)inc)
            return lisp->providerange_Integer(init, inc, bound + inc);
        return lisp->providerange_Number(init, inc, bound + inc);
    }

    evalAsNumber(2, lisp, inc);
    if (init == (long)init && inc == (long)inc)
        return lisp->providerange_Integer(init, inc);
    return lisp->providerange_Number(init, inc);
}


Element* List_llist_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    if (listsize == 1)
        return new LList(&lisp->delegation->mark);
    LList* first_element = new LList(&lisp->delegation->mark);
    Element* second_element;
    try {
        lisp->checkState(this);
        for (long i = listsize - 1; i > 0; i--) {
            second_element = liste[i]->eval(lisp);
            first_element->push_front(second_element->copying(false));
        }
    }
    catch (Error* err) {
        first_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return first_element;
}


Element* List_to_list_eval::eval(LispE* lisp) {
    Element* values = liste[1]->eval(lisp);
    if (size() == 2) {
        if (values->isTensor()) {
            Element* result = new List((List*)values, 0);
            values->release();
            return result;
        }
        if (values->type == t_list) {
            return values;
        }
        Element* results = values->asList(lisp, lisp->provideList());
        values->release();
        return results;
    }
    long counter = 0;
    evalAsInteger(2, lisp, counter);
    List* l = lisp->provideList();
    while (counter > 0) {
        l->append(values->copying());
        counter--;
    }
    values->release();
    return l;
}


Element* List_to_llist_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);

    if (container->type == l_llist)
        return container;
    if (container->type == t_heap) {
        Element* a_llist = (LList*)((Heap*)container)->asLList(lisp);
        container->release();
        return a_llist;
    }
    LList* a_llist = new LList(&lisp->delegation->mark);
    try {
        lisp->checkState(this);
        if (container->isSet()) {
            void* iter = container->begin_iter();
            Element* next_value = container->next_iter(lisp, iter);
            while (next_value != emptyatom_) {
                if (container->check_element(lisp, next_value))
                    a_llist->push_front(next_value);
                next_value = container->next_iter(lisp, iter);
            }
            container->clean_iter(iter);
        }
        else {
            if (container->isList()) {
                for (long i = container->size() - 1; i >= 0; i--) {
                    a_llist->push_front(container->index(i)->copying(false));
                }
            }
            else {
                a_llist->push_front(container->copying(false));
            }
        }
        container->release();
    }
    catch (Error* err) {
        a_llist->release();
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return a_llist;
}

Element* List_lock_eval::eval(LispE* lisp) {
    u_ustring key;
    evalAsUString(1, lisp, key);
    ThreadLock* _lock = lisp->delegation->getlock(key);
    long listsize = liste.size();
    bool test = lisp->threaded();
    _lock->locking(test);
    Element* value = null_;
    try {
        lisp->checkState(this);
        for (long i = 2; i < listsize && value->type != l_return; i++) {
            value->release();
            value = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        _lock->unlocking(test);
        return lisp->check_error(this, err, idxinfo);
    }
    _lock->unlocking(test);
    lisp->resetStack();
    return value;
}


Element* List_mloop_eval::eval(LispE* lisp) {
    List* values = lisp->provideList();
    List* indexes = lisp->provideList();
    Element* e = null_;
    Element* ix;
    long sz = size();
    long var;
    long indexe = 0;
    long nbvars = liste[1]->size();

    int16_t label;

    try {
        lisp->checkState(this);
        for (var = 0; var < nbvars; var++) {
            label = liste[1]->index(var)->label();
            lisp->recording(null_, label);
            e = liste[var+2]->eval(lisp);
            values->append(e);
            indexes->append(e->thekeys(lisp));
        }

        long nb = values->liste[0]->size();
        e = null_;
        while (indexe < nb) {
            _releasing(e);
            for (var = 0; var < nbvars; var++) {
                ix = indexes->liste[var]->index(indexe);
                e = values->liste[var]->protected_index(lisp, ix);
                label = liste[1]->index(var)->label();
                lisp->replacestackvalue(e, label);
            }
            //We then execute our instructions
            e = null_;
            for (var = nbvars + 2; var < sz && e->type != l_return; var++) {
                e->release();
                e = liste[var]->eval(lisp);
            }
            if (e->type == l_return) {
                indexes->release();
                if (e->isBreak()) {
                    values->release();
                    lisp->resetStack();
                    return null_;
                }
                values->release(e->eval(lisp));
                lisp->resetStack();
                return e;
            }
            indexe++;
        }
    }
    catch (Error* err) {
        values->release();
        indexes->release();
        return lisp->check_error(this, err, idxinfo);
    }

    values->release(e);
    indexes->release();
    lisp->resetStack();
    return e;
}


Element* List_lloop_eval::eval(LispE* lisp) {
    List* values = lisp->provideList();
    Element* e = null_;
    unsigned long sz;
    long var;
    long indexe = 0;
    long nbvars = liste[1]->size();
    unsigned long nb = -1;

    int16_t label;

    try {
        lisp->checkState(this);
        for (var = 0; var < nbvars; var++) {
            label = liste[1]->index(var)->label();
            lisp->recording(null_, label);
            e = liste[var + 2]->eval(lisp);
            sz = e->size();
            nb = nb<sz?nb:sz;
            values->append(e);
        }

        sz = size();

        while (indexe < nb) {
            _releasing(e);
            for (var = 0; var < nbvars; var++) {
                e = values->liste[var]->index(indexe);
                label = liste[1]->index(var)->label();
                lisp->replacestackvalue(e, label);
            }
            e = null_;
            //We then execute our instructions
            for (var = nbvars + 2; var < sz && e->type != l_return; var++) {
                e->release();
                e = liste[var]->eval(lisp);
            }
            if (e->type == l_return) {
                if (e->isBreak()) {
                    values->release();
                    lisp->resetStack();
                    return null_;
                }
                values->release(e->eval(lisp));
                lisp->resetStack();
                return e;
            }
            indexe++;
        }
    }
    catch (Error* err) {
        values->release();
        return lisp->check_error(this, err, idxinfo);
    }

    values->release(e);
    lisp->resetStack();
    return e;
}


Element* List_mark_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);

    bool test;

    if (liste.size() == 2) {
        test = container->usermark();
        container->release();
        return booleans_[test];
    }

    try{
        test = liste[2]->eval(lisp)->Boolean();
        container->setusermark(test);
        container->release();
    }
    catch (Error* err) {
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return True_;
}


Element* List_resetmark_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);
    container->resetusermark();
    container->release();
    return True_;
}

Element* List_floats_eval::eval(LispE* lisp) {
    long listsz = size();
    if (listsz == 1)
        return lisp->provideFloats();
    Floats* n = NULL;
    Element* values;
    try {
        lisp->checkState(this);
        if (listsz == 2) {
            values = liste[1]->eval(lisp);
            if (values->type == t_floats && !values->status)
                n = (Floats*)values;
            else {
                n = lisp->provideFloats();
                values->flatten(lisp, n);
                values->release();
            }
        }
        else {
            n = lisp->provideFloats();
            n->liste.reserve(listsz<<1);
            for (long e = 1; e < listsz; e++) {
                values = liste[e]->eval(lisp);
                if (values->isList()) {
                    for (long i = 0; i < values->size(); i++) {
                        n->liste.push_back(values->index(i)->asFloat());
                    }
                }
                else
                    n->liste.push_back(values->asFloat());
                values->release();
            }
        }
    }
    catch (Error* err) {
        n->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return n;
}


Element* List_pipe_eval::eval(LispE* lisp) {
    //pipe returns a line read on std::cin
    //It returns nil, if the stream is closed...
    string code;
    getline(std::cin, code);
    if (std::cin.eof())
        return null_;
    return lisp->provideString(code);
}


Element* List_prettify_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    long mx = 50;
    if (liste.size() == 3)
        evalAsInteger(2, lisp, mx);
    string s = first_element->prettify(lisp, mx);
    first_element->release();
    return lisp->provideString(s);
}


Element* List_range_eval::eval(LispE* lisp) {
    Element* e1 = liste[1]->eval(lisp);
    Element* e2 = liste[2]->eval(lisp);
    Element* e3 = liste[3]->eval(lisp);
    Element* e;
    
    if (e1->isString() && e2->isString()) {
        if (!e3->isInteger())
            throw new Error("Error: expecting an increment as integer");
        u_ustring s1 = e1->asUString(lisp);
        u_ustring s2 = e2->asUString(lisp);
        if (!s1.size() || !s2.size())
            throw new Error("Error: cannot compute range with empty strings");
        
        e = range(lisp, s1, s2, e3->asInteger());
    }
    else {
        if (e1->isInteger() && e2->isInteger() && e3->isInteger())
            e = range(lisp, e1->asInteger(), e2->asInteger(), e3->asInteger());
        else
            e = range(lisp, e1->asNumber(), e2->asNumber(), e3->asNumber());
    }
    
    e1->release();
    e2->release();
    e3->release();
    return e;
}


Element* List_rangein_eval::eval(LispE* lisp) {
    Element* e1 = liste[1]->eval(lisp);
    Element* e2 = liste[2]->eval(lisp);
    Element* e3 = liste[3]->eval(lisp);
    
    Element* e;
    
    if (e1->isString() && e2->isString()) {
        if (!e3->isInteger())
            throw new Error("Error: expecting an increment as integer");
        long inc = e3->asInteger();
        u_ustring s1 = e1->asUString(lisp);
        u_ustring s2 = e2->asUString(lisp);
        if (!s1.size() || !s2.size())
            throw new Error("Error: cannot compute range with empty strings");
        //By default the last value is always part of the final range
        e = range(lisp, s1, s2, inc);
    }
    else {
        if (e1->isInteger() && e2->isInteger() && e3->isInteger()) {
            long inc = e3->asInteger();
            //The last value is part of the final range list
            //hence the "+ inc" with e2
            e = range(lisp, e1->asInteger(), e2->asInteger() + inc, inc);
        }
        else {
            double inc = e3->asNumber();
            //The last value is part of the final range list
            //hence the "+ inc" with e2
            e = range(lisp, e1->asNumber(), e2->asNumber() + inc, inc);
        }
    }
    e1->release();
    e2->release();
    e3->release();
    return e;
}


Element* List_sign_eval::eval(LispE* lisp) {
    return liste[1]->eval(lisp)->invert_sign(lisp);
}

Element* List_size_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    long sz = element->size();
    element->release();
    return lisp->provideInteger(sz);
}

Element* List_tally_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    long sz = element->tally();
    element->release();
    return lisp->provideInteger(sz);
}


Element* List_signp_eval::eval(LispE* lisp) {
    double v = 0;
    evalAsNumber(1, lisp, v);
    if (v == 0)
        return zero_value;
    if (v < 0)
        return minusone_;
    return one_value;
}


Element* List_sleep_eval::eval(LispE* lisp) {
    long tm;
    evalAsInteger(1, lisp, tm);
    std::this_thread::sleep_for(std::chrono::milliseconds(tm));
    return True_;
}

Element* List_withclass_eval::eval(LispE* lisp) {
    Element* e = null_;
    try {
        lisp->checkState(this);
        for (long i = 2; i < size(); i++) {
            e->release();
            e = liste[i]->eval(lisp);
        }
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return e;
}

Element* List_space_eval::eval(LispE* lisp) {
    short label = liste[1]->label();
    if (label == l_thread)
        return evalthreadspace(lisp, liste.size(), 2);

    if (!lisp->delegation->namespaces.check(label)) {
        Element* e = liste[1]->eval(lisp);
        label = e->label();
        e->release();
        if (!lisp->delegation->namespaces.check(label)) {
            u_ustring w = U"Error: Unknown space: ";
            w += lisp->asUString(label);
            throw new Error(w);
        }
    }

    uint16_t current = lisp->current_space;
    lisp->current_space = lisp->delegation->namespaces[label];
    Element* r = null_;
    for (long i = 2; i < liste.size(); i++) {
        r->release();
        r = liste[i]->eval(lisp);
    }
    lisp->current_space = current;
    return r;
}


Element* List_sum_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);

    switch (first_element->type) {
        case t_floats: {
            float v = ((Floats*)first_element)->liste.sum();
            first_element->release();
            return v?lisp->provideFloat(v):zero_value;
        }
        case t_numbers: {
            double v = ((Numbers*)first_element)->liste.sum();
            first_element->release();
            return v?lisp->provideNumber(v):zero_value;
        }
        case t_shorts: {
            int16_t v = ((Shorts*)first_element)->liste.sum();
            first_element->release();
            return v?new Short(v):zero_value;
        }
        case t_integers: {
            long v = ((Integers*)first_element)->liste.sum();
            first_element->release();
            return v?lisp->provideInteger(v):zero_value;
        }
        case t_strings: {
            u_ustring v = ((Strings*)first_element)->liste.sum();
            first_element->release();
            return (v == U"")?emptystring_:lisp->provideString(v);
        }
        case t_stringbytes: {
            string v = ((Stringbytes*)first_element)->liste.sum();
            first_element->release();
            return (v == "")?new Stringbyte():new Stringbyte(v);
        }
        case t_list: {
            double v = 0;
            List* lst = (List*)first_element;
            long listsize = lst->size();
            for (long i = 0; i < listsize; i++) {
                v += lst->liste[i]->checkNumber(lisp);
            }
            first_element->release();
            return lisp->provideNumber(v);
        }
        case t_llist: {
            double v = 0;
            LList* lst = (LList*)first_element;
            for (u_link* a = lst->liste.begin(); a != NULL; a = a->next())
            v += a->value->checkNumber(lisp);
            first_element->release();
            return lisp->provideNumber(v);
        }
        case t_seti: {
            long v = 0;
            Set_i* lst = (Set_i*)first_element;
            for (const auto& a: lst->ensemble)
                v += a;
            first_element->release();
            return lisp->provideInteger(v);
        }
        case t_setn: {
            double v = 0;
            Set_n* lst = (Set_n*)first_element;
            for (const auto& a: lst->ensemble)
                v += a;
            first_element->release();
            return lisp->provideNumber(v);
        }
        default:
            first_element->release();
        throw new Error("Error: expecting a container as argument");
    }
}


Element* List_product_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    switch (first_element->type) {
        case t_floats: {
            float v = ((Floats*)first_element)->liste.product();
            first_element->release();
            return v?lisp->provideFloat(v):zero_value;
        }
        case t_numbers: {
            double v = ((Numbers*)first_element)->liste.product();
            first_element->release();
            return v?lisp->provideNumber(v):zero_value;
        }
        case t_shorts: {
            int16_t v = ((Shorts*)first_element)->liste.product();
            first_element->release();
            return v?new Short(v):zero_value;
        }
        case t_integers: {
            long v = ((Integers*)first_element)->liste.product();
            first_element->release();
            return v?lisp->provideInteger(v):zero_value;
        }
        case t_list: {
            double v = 1;
            List* lst = (List*)first_element;
            long listsize = lst->size();
            for (long i = 0; i < listsize && v; i++)
            v *= lst->liste[i]->checkNumber(lisp);
            first_element->release();
            return lisp->provideNumber(v);
        }
        case t_llist: {
            double v = 1;
            LList* lst = (LList*)first_element;
            for (u_link* a = lst->liste.begin(); a != NULL && v; a = a->next())
            v *= a->value->checkNumber(lisp);
            first_element->release();
            return lisp->provideNumber(v);
        }
        case t_seti: {
            long v = 1;
            Set_i* lst = (Set_i*)first_element;
            for (const auto& a: lst->ensemble)
                v *= a;
            first_element->release();
            return lisp->provideInteger(v);
        }
        case t_setn: {
            double v = 1;
            Set_n* lst = (Set_n*)first_element;
            for (const auto& a: lst->ensemble)
                v *= a;
            first_element->release();
            return lisp->provideNumber(v);
        }
        default:
            first_element->release();
        throw new Error("Error: expecting a container as argument");
    }
}
//(transpose (rho 2 4 '(1 3 9 10 12 34)))


Element* List_transpose_eval::eval(LispE* lisp) {
    Element* matrix = liste[1]->eval(lisp);
    Element* transposed_matrix = matrix->transposed(lisp);
    matrix->release();
    return transposed_matrix;
}


Element* List_heap_eval::eval(LispE* lisp) {
    long listsize = liste.size();
    Element* oper = null_;
    List* compare = NULL;
    Heap* tas = NULL;
    Element* second_element;

    try {
        lisp->checkState(this);
        if (listsize != 1)
            oper = liste[1]->eval(lisp);

        if (oper == null_ || oper->size() == 0)
            oper = lisp->provideAtom(l_compare);

        if (oper->isLambda())
            tas =  new Heaplambda((List*)oper);
        else
            tas =  new Heap(lisp->provideCall(oper, 2));

        for (long i = 2; i < listsize; i++) {
            second_element = liste[i]->eval(lisp);
            tas->insert(lisp, second_element->copying(false));
        }
    }
    catch (Error* err) {
        if (compare != NULL)
            compare->release();
        if (tas != NULL)
            tas->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return tas;
}

Element* List_waiton_eval::eval(LispE* lisp) {
    u_ustring key;
    evalAsUString(1, lisp, key);
    lisp->delegation->waiton(key);
    return True_;
}

Element* List_determinant_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (element->type != t_matrix_number) {
        element->release();
        throw new Error("Error: We can only compute the determinant of a matrix");
    }
    double det = 0;

    try {
        lisp->checkState(this);
        det = element->determinant(lisp);
    }
    catch (Error* err) {
        element->release();
        return lisp->check_error(this, err, idxinfo);
    }

    element->release();
    lisp->resetStack();
    return lisp->provideNumber(det);
}

Element* List_listand_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* result = first_element;
    long listsize = liste.size();

    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            result = first_element->list_and(lisp, second_element);
            if (first_element != result) {
                first_element->release();
                first_element = result;
            }
        }
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}


Element* List_listor_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* result = first_element;
    long listsize = liste.size();
    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            result = first_element->list_or(lisp, second_element);
            if (first_element != result) {
                first_element->release();
                first_element = result;
            }
        }
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}

Element* List_listxor_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element = null_;
    Element* result = first_element;
    long listsize = liste.size();
    try {
        lisp->checkState(this);
        for (long i = 1; i < listsize; i++) {
            _releasing(second_element);
            second_element = liste[i]->eval(lisp);
            result = first_element->list_xor(lisp, second_element);
            if (first_element != result) {
                first_element->release();
                first_element = result;
            }
        }
        second_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return result;
}


Element* List_mod_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;

    try {
        lisp->checkState(this);
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->mod(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            lisp->resetStack();
            return first_element;
        }
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '%' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '%' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                    first_element->release();
                    lisp->resetStack();
                    return zero_value;
                }
                lst = lst->mod(lisp, NULL);
                first_element->release();
                lisp->resetStack();
                return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->mod(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->mod(lisp, lst->index(i));
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
                first_element = first_element->mod(lisp, second_element);
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
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return first_element;
}


Element* List_leftshift_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;

    try {
        lisp->checkState(this);
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->leftshift(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            lisp->resetStack();
            return first_element;
        }
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '<<' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '<<' to a string");
            case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                    first_element->release();
                    lisp->resetStack();
                    return zero_value;
                }
                lst = lst->leftshift(lisp, NULL);
                first_element->release();
                lisp->resetStack();
                return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->leftshift(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->leftshift(lisp, lst->index(i));
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
                first_element = first_element->leftshift(lisp, second_element);
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
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return first_element;
}


Element* List_rightshift_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;

    try {
        lisp->checkState(this);
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->rightshift(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            lisp->resetStack();
            return first_element;
        }
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '>>' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '>>' to a string");
            case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                    first_element->release();
                    lisp->resetStack();
                    return zero_value;
                }
                lst = lst->rightshift(lisp, NULL);
                first_element->release();
                lisp->resetStack();
                return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->rightshift(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->rightshift(lisp, lst->index(i));
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
                first_element = first_element->rightshift(lisp, second_element);
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
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return first_element;
}

Element* List_bitand::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->bit_and(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '&' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '&' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_value;
                    }
                    lst = lst->bit_and(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_and(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_and(lisp, lst->index(i));
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
                first_element = first_element->bit_and(lisp, second_element);
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

Element* List_bitor::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->bit_or(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '|' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '|' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_value;
                    }
                    lst = lst->bit_or(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_or(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_or(lisp, lst->index(i));
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
                first_element = first_element->bit_or(lisp, second_element);
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

Element* List_bitxor::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->bit_xor(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '^' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '^' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_value;
                    }
                    lst = lst->bit_xor(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_xor(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_xor(lisp, lst->index(i));
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
                first_element = first_element->bit_xor(lisp, second_element);
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

Element* List_bitandnot::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    
    int16_t listsize = liste.size();
    Element* lst = this;
    Element* second_element = null_;
    long i;
    
    try {
        if (listsize == 3) {
            first_element = first_element->copyatom(lisp, 1);
            second_element = liste[2]->eval(lisp);
            first_element = first_element->bit_and_not(lisp, second_element);
            if (first_element != second_element)
                second_element->release();
            return first_element;
        }
        
        if (listsize == 2) {
            if (!first_element->isList())
                throw new Error("Error: cannot apply '&~' to one element");
            lst = first_element;
            switch (lst->type) {
                case t_strings:
                case t_stringbytes:
                    throw new Error("Error: cannot apply '&~' to a string");
                case t_floats:
                case t_shorts:
                case t_integers:
                case t_numbers:
                    if (!lst->size()) {
                        first_element->release();
                        return zero_value;
                    }
                    lst = lst->bit_and_not(lisp, NULL);
                    first_element->release();
                    return lst;
                case t_llist: {
                    first_element = zero_value;
                    u_link* u = ((LList*)lst)->liste.begin();
                    if (u != NULL) {
                        first_element = u->value->copyatom(lisp, 1);
                        u = u->next();
                        while (u != NULL) {
                            first_element = first_element->bit_and_not(lisp, u->value);
                            u = u->next();
                        }
                    }
                    break;
                }
                case t_list: {
                    first_element = zero_value;
                    listsize = lst->size();
                    if (listsize) {
                        first_element = lst->index(0)->copyatom(lisp, 1);
                        for (i = 1; i < listsize; i++) {
                            first_element = first_element->bit_and_not(lisp, lst->index(i));
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
                first_element = first_element->bit_and_not(lisp, second_element);
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

Element* List_bitnot::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    first_element = first_element->copyatom(lisp, 1);
    Element* e;
        
    try {
        e = first_element->bit_not(lisp);
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    if (e != first_element)
        first_element->release();
    
    return e;
}

//Fast matrix multiplication with shapes

Element* List_plusmultiply::eval(LispE* lisp) {
    Element* m1 = null_;
    Element* shape1 = null_;
    Element* m2 = null_;
    Element* shape2 = null_;
    Element* res;
    
    try {
        m1 = liste[1]->eval(lisp);
        shape1 = liste[2]->eval(lisp);
        m2 = liste[3]->eval(lisp);
        shape2 = liste[4]->eval(lisp);
        
        if (!shape1->isList() || shape1->size() != 2)
            throw new Error("Error: The first shape should be a list of two elements");
        
        if (!shape2->isList() || shape2->size() != 2)
            throw new Error("Error: The second shape should be a list of two elements");

        long sh10 = shape1->index(0)->asInteger();
        long sh11 = shape1->index(1)->asInteger();
        long sh20 = shape2->index(0)->asInteger();
        long sh21 = shape2->index(1)->asInteger();

        if (m1->type != m2->type)
            throw new Error("Error: The two matrices should have the same type");
                
        if (sh20 != sh11)
            throw new Error("Error: Incompatible shapes");
        
        if (m1->size() != sh10*sh11)
            throw new Error("Error: Mismatch between shape and size of first matrix");
        if (m2->size() != sh20*sh21)
            throw new Error("Error: Mismatch between shape and size of second matrix");
        
        res = m1->matrix_product(lisp, m2, sh11, sh10, sh21);
    }
    catch(Error* err) {
        m1->release();
        m2->release();
        shape1->release();
        shape2->release();
        throw err;
    }
        
    m1->release();
    m2->release();
    shape1->release();
    shape2->release();
    return res;
}

Element* List_if_eval::eval(LispE* lisp) {
    Element* res;
        
    try {
        lisp->checkState(this);
        res = liste[1]->eval(lisp);
        char test = 3 - res->Boolean();
        res->release();
        
        res = liste[test]->eval_terminal(lisp, terminal);
    }
    catch(Error* err) {
        lisp->resetStack();
        throw err;
    }
    lisp->resetStack();
    return res;
}

Element* List_ife_eval::eval(LispE* lisp) {
    Element* res;
    
    try {
        lisp->checkState(this);
        
        res = liste[1]->eval(lisp);
        char test = res->Boolean();
        res->release();

        if (test)
            return liste[2]->eval_terminal(lisp, terminal);
        
        long listsize = liste.size();
        liste.back()->setterminal(terminal);
        
        for (long i = 3; i < listsize && res->type != l_return; i++) {
            res->release();
            res = liste[i]->eval(lisp);
        }
    }
    catch(Error* err) {
        lisp->resetStack();
        throw err;
    }
    lisp->resetStack();
    return res;
}

Element* List_unique_eval::eval(LispE* lisp) {
    Element* res;
    try {
        lisp->checkState(this);
        res = evall_unique(lisp);
    }
    catch(Error* err) {
        lisp->resetStack();
        throw err;
    }
    lisp->resetStack();
    return res;
}
