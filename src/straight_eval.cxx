/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  straight_eval.cxx
//
//

#include "lispe.h"
#include"avl.h"
#include <math.h>
#include <algorithm>
#include <thread>
#include <chrono>

Element* range(LispE* lisp, long init, long limit, long inc);
Element* range(LispE* lisp, double init, double limit, double inc);
Element* range(LispE* lisp, u_ustring& init, u_ustring& limit, long inc);

//------------------------------------------------------------------------------------------
//We return one value less since, the arity comprises the function definition as first element of a list
//For functions, it is one too many.
long arity_value(unsigned long arity) {
    long nb = 0;
    while (arity != 2) {
        nb++;
        arity >>= 1;
    }
    return nb;
}

Element* eval_body_as_argument_min(LispE* lisp, Element* function, unsigned long arity, bool lmbd = false) {
    if (function->isInstruction()) {
        lisp->arity_check(function->label(), arity);
        Element* value = lisp->cloning(function->label());
        value->append(function);
        value->type = t_eval;
        return value;
    }
    
    if (function->isAtom())
        function = function->eval(lisp);
    
    if (function->isList()) {
        switch(function->function_label(lisp)) {
            case l_defpat:
                function = new List_pattern_eval((List*)function);
                break;
            case l_deflib:
                function = new List_library_eval((List*)function, arity_value(arity));
                break;
            case l_defun:
                function = new List_function_eval(lisp, (List*)function, arity_value(arity));
                break;
            case l_lambda:
                if (lmbd)
                    function = new List_call_lambda((List_lambda_eval*)function);
                else
                    function = new Atomefonction(function, t_lambda);
                break;
            default:
                throw new Error("Error: wrong function call");
        }
        
    }

    return function;
}

//We evaluation function beforehand
Element* eval_body_as_argument(LispE* lisp, Element* function, const unsigned long arity) {
    return eval_body_as_argument_min(lisp, function->eval(lisp), arity);
}

Element* eval_body_as_argument(LispE* lisp, Element* function) {
    return eval_body_as_argument_min(lisp, function->eval(lisp), P_TWO);
}


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
        if (container->element_container()) {
            cdr_result->increment();
            container->release();
            cdr_result->decrementkeep();
        }
        else
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
        if (container->element_container()) {
            car_result->increment();
            container->release();
            car_result->decrementkeep();
        }
        else
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
                second_element = second_element->duplicate_constant(lisp);
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
                    second_element = second_element->duplicate_constant(lisp);
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
        double d = ((Complex*)e)->content.imag();
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
        if (container->element_container()) {
            result->increment();
            container->release();
            result->decrementkeep();
        }
        else
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
        if (container->element_container()) {
            result->increment();
            container->release();
            result->decrementkeep();
        }
        else
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
        if (!d->isDictionary())
            throw new Error("Error: wrong arguments for 'dictionary'");
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
        throw new Error(L"Error: Missing label for 'loop'");

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
    Integer* element = NULL;
    evalAsInteger(1, lisp, counter);

    if (liste[2]->isAtom()) {
        label = liste[2]->label();
        element = lisp->provideInteger(counter);
        lisp->storing_variable(element, label);
        first = 3;
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
                element->content = counter;
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
            liste[2]->setterminal(terminal);
            lisp->resetStack();
            return liste[2]->eval(lisp);
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

    Numbers* n = lisp->provideNumbers();
    Element* values;


    try {
        lisp->checkState(this);
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

    Integers* n = lisp->provideIntegers();
    Element* values;


    try {
        lisp->checkState(this);
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

    Strings* n = lisp->provideStrings();
    Element* values;


    try {
        lisp->checkState(this);
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

    Stringbytes* n = new Stringbytes();
    Element* values;


    try {
        lisp->checkState(this);
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

Element* List_call_lambda::eval(LispE* lisp) {
    //The first element is the lambda itself,
    //The rest the arguments...
    // It is either a lambda or a function
    //otherwise it's an error
    Element* element;
        
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    
    //((lambda (x y) (if (eq x 0) (* 2 y) (self (- x 1) (+ y 2)))) 20 1)
    //( (lambda (x) (if (eq x 1) 1 (+ x (self (- x 1))))) 20)
    
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    
    if (terminal && lisp->called() == body) {
        sameSizeTerminalArguments(lisp, body->parameters);
        lisp->check_end_trace(lisp->check_trace_in_function());
        return terminal_;
    }
    
    // For each of the parameters we record in the stack
    long i;
    vecte<Element*> recorded(nbarguments);
    
    try {
        lisp->setStack();
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        for (i = 0; i < nbarguments; i++) {
            //The evaluation must be done on the previous stage of the stack
            element = liste[i+1]->eval(lisp);
            recorded.push_raw(lisp->record_or_replace(element, body->labels[i]));
        }
    }
    catch (Error* err) {
        lisp->resetStack();
        //We must call reset_in_stack from end to begin
        //the status should popped out from Stack::status
        for (i = nbarguments - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], body->labels[i]);
        throw err;
    }

    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.
    char tr = lisp->check_trace_in_function();
    long nbinstructions = body->size();
    Element* stackfunction = lisp->exchangestackfunction(body);
    try {
        do {
            if (nbinstructions == 3)
                element = body->liste[2]->eval(lisp);
            else {
                element = null_;
                for (i = 2; i < nbinstructions && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                Element* e = element->eval(lisp);
                element->release();
                //We must call reset_in_stack from end to begin
                //the status should popped out from Stack::status
                for (i = nbarguments - 1; i >= 0; i--)
                    lisp->reset_in_stack(recorded[i], body->labels[i], e);
                lisp->resetStack();
                lisp->check_end_trace(tr);
                return e;
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->check_end_trace(tr);
        lisp->setstackfunction(stackfunction);
        for (i = nbarguments - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], body->labels[i]);
        throw err;
    }
    
    for (i = nbarguments - 1; i >= 0; i--)
        lisp->reset_in_stack(recorded[i], body->labels[i], element);
    lisp->setstackfunction(stackfunction);
    lisp->resetStack();
    lisp->check_end_trace(tr);
    return element;
}

//Execution of a function as well as the shift of parameters with arguments
//For this specific lambda, we do not create a stack
Element* List_lambda_eval::eval_lambda_min(LispE* lisp) {
    Element* stackfunction = lisp->exchangestackfunction(this);
    Element* element;
    
    try {
        if (nbinstructions == 3)
            element = liste[2]->eval(lisp);
        else {
            element = null_;
            for (int16_t i = 2; i < nbinstructions && element->type != l_return; i++) {
                element->release();
                element = liste[i]->eval(lisp);
            }
        }
        if (element->type == l_return) {
            Element* e = element->eval(lisp);
            element->release();
            lisp->setstackfunction(stackfunction);
            //This version protects 'e' from being destroyed in the stack.
            return e;
        }
    }
    catch (Error* err) {
        lisp->setstackfunction(stackfunction);
        throw err;
    }
    
    lisp->setstackfunction(stackfunction);
    //This version protects 'e' from being destroyed in the stack.
    return element;
}

Element* List_function_eval::eval(LispE* lisp) {
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    if (terminal && lisp->called() == body) {
        if (same)
            sameSizeTerminalArguments(lisp, (List*)parameters);
        else
            differentSizeTerminalArguments(lisp, (List*)parameters, nbarguments, defaultarguments);
        lisp->check_end_trace(lisp->check_trace_in_function());
        return terminal_;
    }
    

    if (same)
        sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
        
    char tr = lisp->check_trace_in_function();
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    long i;
    long nbinstructions = body->size();
    Element* element;
    try {
        lisp->checkState(this);
        do {
            if (nbinstructions == 4)
                element = body->liste[3]->eval(lisp);
            else {
                element = null_;
                for (i = 3; i < nbinstructions && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                Element* current_value = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                lisp->resetStack();
                lisp->check_end_trace(tr);
                return lisp->pop(current_value);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->pop();
        lisp->check_end_trace(tr);
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    lisp->check_end_trace(tr);
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}

Element* List_library_eval::eval(LispE* lisp) {
    if (same)
        sameSizeNoTerminalArguments(lisp, body, parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, parameters, nbarguments, defaultarguments);
        
    Element* element;
    try {
        lisp->checkState(this);
        element = body->liste[3]->eval(lisp);
    }
    catch (Error* err) {
        lisp->pop();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}

Element* List_pattern_eval::eval(LispE* lisp) {
    List* arguments = lisp->provideList();
    Element* element;
    Element* body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    int16_t ilabel = -1;
    int16_t sublabel = -1;
    char match;
    char depth = lisp->depths[function_label] - 1;

    try {
        lisp->checkState(this);
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractdynamiclabel(element, depth);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    arguments->clear();
                    if (match == check_mismatch)
                        throw new Error(L"Error: Size mismatch between argument list and data structure definition");
                    else {
                        std::wstringstream message;
                        message << L"Error: Mismatch on argument: " << match;
                        message << " (" << lisp->asString(lisp->getDataStructure(ilabel)->index(match)->label()) << " required)";
                        throw new Error(message.str());
                    }
                }
            }
            //We keep the track of the first element as it used as an index to gather pattern methods
            if (i == 1)
                sublabel = ilabel;
            arguments->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        arguments->release();
        return lisp->check_error(this, err, idxinfo);
    }

    char tr = lisp->check_trace_in_function();

    body = NULL;
    auto& functions = lisp->delegation->method_pool[lisp->current_space]->at(function_label);
    auto subfunction = functions.find(sublabel);
    if (subfunction == functions.end()) {
        sublabel = v_null;
        //We check, if we have a rollback function
        subfunction = functions.find(sublabel);
        if (subfunction == functions.end()) {
            arguments->release();
            wstring message = L"Error: Could not find a match for function: '";
            message += lisp->asString(function_label);
            message += L"'";
            lisp->resetStack();
            throw new Error(message);
        }
    }

    body = subfunction->second[0];

    ilabel = 1;
    match = 0;
    long sz = subfunction->second.size();
    Stackelement* sta = lisp->topstack();
    lisp->push(body);
    
    while (body != NULL) {
        element = body->index(2);
        if (element->size() == nbarguments) {
            match = true;
            for (i = 0; i < nbarguments && match; i++) {
                match = element->index(i)->unify(lisp, arguments->liste[i], true);
            }
            
            if (match) {
                if (terminal && sta->called() == body) {
                    lisp->remove_sub_stack(sta);
                    arguments->release();
                    lisp->resetStack();
                    lisp->check_end_trace(tr);
                    return terminal_;
                }
                lisp->setstackfunction(body);
                break;
            }
            
            lisp->clear_top_stack();
        }
        body = NULL;
        if (ilabel < sz)
            body = subfunction->second[ilabel++];
        else {
            if (sublabel != v_null) {
                sublabel = v_null;
                ilabel = 1;
                //We check, if we have a rollback function
                subfunction = functions.find(sublabel);
                if (subfunction != functions.end()) {
                    body = subfunction->second[0];
                    sz = subfunction->second.size();
                }
            }
        }
    }

    if (!match) {
        lisp->pop();
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        lisp->resetStack();
        throw new Error(message);
    }
        
    try {
        nbarguments = body->size();
        do {
            if (nbarguments == 4)
                element = body->index(3)->eval(lisp);
            else {
                element = null_;
                for (i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->index(i)->eval(lisp);
                }
            }
            if (element->type == l_return) {
                body = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                arguments->release(body);
                lisp->resetStack();
                lisp->check_end_trace(tr);
                return lisp->pop(body);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        arguments->release();
        lisp->pop();
        lisp->check_end_trace(tr);
        return lisp->check_error(this, err, idxinfo);
    }

    arguments->release(element);
    lisp->resetStack();
    lisp->check_end_trace(tr);
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
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
                check_tensor |= !e->is_same_tensor(container->last());
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

Element* List_maplist_eval::eval(LispE* lisp) {
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = liste[1];
    Element* container = NULL;
    
    bool sb = lisp->set_true_as_one();
    void* iter = NULL;
    List* call = NULL;
    
    try {
        lisp->checkState(this);
        if (listesize == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
    
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        current_list = liste[2]->eval(lisp);
        long listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return emptylist_;
        }
         
        Element* e = op;
        if (op->is_quote())
            e = op->eval(lisp);
                    
        /*
         The first element is "quoted" to avoid it to be evaluated later
         otherwise we might have an error later.
         Basically, if we have: (map 'string '(ab cd ef))
         
         We are going to create as many calls to "string"" as there are elements in the list:
         
         (string ab)
         (string cd) etc..
         
         We don't want ab, cd to be evaluated or it will yield an error
         Instead we produce:
         
         (string 'ab)
         (string 'cd) etc...
         
         Each argument is "quoted".
         We then use "in_quote" to replace the current element in the quote with a new one.
         */

        int16_t ps = 1;
        if (e->isList()) {
            if (e->size())
                call = lisp->provideCallforTWO(e, ps);
            else
                throw new Error("Error: empty list not accepted here");
        }
        else {
            op = eval_body_as_argument(lisp, op);
            if (op->is_straight_eval())
                call = (List*)op;
            else
                call = new List_eval(lisp, op);
            call->append(lisp->quoted());
        }
        
        iter = current_list->begin_iter();
        Element* nxt = current_list->next_iter_exchange(lisp, iter);
        //Replacing the element in position 1, which is quoted with nxt
        call->in_quote(ps, nxt);
        //"met" is a List function, hence the weird call: call->*met, which consists of executing
        //this method within the current List object: call
        e = call->eval(lisp);
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
        
        check_tensor |= e->isPureList();
        e = e->copying(false);
        container->append(e);
        e->release();
        nxt = current_list->next_iter_exchange(lisp, iter);
        while (nxt != emptyatom_) {
            call->in_quote(ps, nxt);
            e = call->eval(lisp)->copying(false);
            check_tensor |= e->isPureList();
            check_tensor |= !e->is_same_tensor(container->last());
            container->append(e);
            e->release();
            nxt = current_list->next_iter_exchange(lisp, iter);
        }
        call->force_release();
        current_list->clean_iter(iter);
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
        if (call != NULL)
            call->force_release();
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

Element* List_filterlist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter =  NULL;
    bool choice = (liste[0]->label() == l_filterlist);
    
    try {
        lisp->checkState(this);
        if (listsz == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        current_list = liste[2]->eval(lisp);
        
        if (choice) {
            switch (current_list->type) {
                case t_floats:
                    result = lisp->provideFloats();
                    break;
                case t_numbers:
                    result = lisp->provideNumbers();
                    break;
                case t_shorts:
                    result = new Shorts();
                    break;
                case t_integers:
                    result = lisp->provideIntegers();
                    break;
                case t_strings:
                    result = lisp->provideStrings();
                    break;
                case t_stringbytes:
                    result = new Stringbytes();
                    break;
                case t_string:
                    result = lisp->provideString();
                    break;
                case t_stringbyte:
                    result = new Stringbyte();
                    break;
                case t_llist:
                    result = new LList(&lisp->delegation->mark);
                    break;
                default:
                    result = lisp->provideList();
            }
        }
        else
            result = lisp->provideList();

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
        Element* e;
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            
            e = op->eval_lambda_min(lisp);
            if (e->type != l_return) {
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    result->append(e);
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    lisp->replacestackvalue(nxt, label);
                    e = op->eval_lambda_min(lisp);
                    if (e->type == l_return)
                        break;
                    if (e->Boolean()) {
                        e->release();
                        e = nxt->copying(false);
                        check_tensor |= e->isPureList();
                        check_tensor |= !e->is_same_tensor(result->last());
                        result->append(e);
                    }
                    e->release();
                    nxt = current_list->next_iter_exchange(lisp, iter);
                }
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            //(filterlist '(< _ 10) (iota 10))
            //(filterlist '(< 10 _) (iota 10)) <=> (filterlist '(< 10) (iota 10))
            //where _ is the slot filling for our variable

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }
                        
            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                e = call->eval(lisp);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    if (check_tensor == a_tensor && result->size())
                        check_tensor |= !e->is_same_tensor(result->last());
                    result->append(e);
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            current_list = result->index(0)->newTensor(lisp, (List*)result);
            if (current_list != result) {
                result->release();
                return current_list;
            }
        }
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        lisp->reset_to_true(sb);
        if (iter != NULL)
            current_list->clean_iter(iter);
        current_list->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return emptylist_;
}

Element* List_takelist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter = NULL;
    bool choice = (liste[0]->label() == l_takelist);
    
    try {
        lisp->checkState(this);
        if (listsz == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        current_list = liste[2]->eval(lisp);
        
        if (choice) {
            switch (current_list->type) {
                case t_floats:
                    result = lisp->provideFloats();
                    break;
                case t_numbers:
                    result = lisp->provideNumbers();
                    break;
                case t_shorts:
                    result = new Shorts();
                    break;
                case t_integers:
                    result = lisp->provideIntegers();
                    break;
                case t_strings:
                    result = lisp->provideStrings();
                    break;
                case t_string:
                    result = lisp->provideString();
                    break;
                case t_stringbytes:
                    result = new Stringbytes();
                    break;
                case t_stringbyte:
                    result = new Stringbyte();
                    break;
                case t_llist:
                    result = new LList(&lisp->delegation->mark);
                    break;
                default:
                    result = lisp->provideList();
            }
        }
        else
            result = lisp->provideList();

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
        Element* e;
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            
            e = op->eval_lambda_min(lisp);
            if (e->type != l_return) {
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    result->append(e);
                }
                else
                    listsz = 0; //we force to stop now...
                
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    lisp->replacestackvalue(nxt, label);
                    e = op->eval_lambda_min(lisp);
                    if (e->type == l_return)
                        break;
                    if (e->Boolean()) {
                        e->release();
                        e = nxt->copying(false);
                        check_tensor |= e->isPureList();
                        check_tensor |= !e->is_same_tensor(result->last());
                        result->append(e);
                    }
                    else {
                        e->release();
                        break;
                    }
                    e->release();
                    nxt = current_list->next_iter_exchange(lisp, iter);
                }
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            //(takelist '(< _ 10) (iota 10))
            //(takelist '(< 10 _) (iota 10)) <=> (takelist '(< 10) (iota 10))
            //where _ is the slot filling for our variable

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                e = call->eval(lisp);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    if (check_tensor == a_tensor && result->size())
                        check_tensor |= !e->is_same_tensor(result->last());
                    result->append(e);
                    e->release();
                }
                else {
                    e->release();
                    break;
                }
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            current_list = result->index(0)->newTensor(lisp, (List*)result);
            if (current_list != result) {
                result->release();
                return current_list;
            }
        }
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return emptylist_;
}

Element* List_droplist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter = NULL;
    bool choice = (liste[0]->label() == l_droplist);
    
    try {
        lisp->checkState(this);
        if (listsz == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        current_list = liste[2]->eval(lisp);
        
        if (choice) {
            switch (current_list->type) {
                case t_floats:
                    result = lisp->provideFloats();
                    break;
                case t_numbers:
                    result = lisp->provideNumbers();
                    break;
                case t_shorts:
                    result = new Shorts();
                    break;
                case t_integers:
                    result = lisp->provideIntegers();
                    break;
                case t_strings:
                    result = lisp->provideStrings();
                    break;
                case t_string:
                    result = lisp->provideString();
                    break;
                case t_stringbytes:
                    result = new Stringbytes();
                    break;
                case t_stringbyte:
                    result = new Stringbyte();
                    break;
                case t_llist:
                    result = new LList(&lisp->delegation->mark);
                    break;
                default:
                    result = lisp->provideList();
            }
        }
        else
            result = lisp->provideList();
        

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
        
        Element* e;
        bool add = false;

        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            
            e = op->eval_lambda_min(lisp);
            if (e->type != l_return) {
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    result->append(e);
                    add = true;
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
                
                while (nxt != emptyatom_) {
                    lisp->replacestackvalue(nxt, label);
                    e = op->eval_lambda_min(lisp);
                    if (e->type == l_return)
                        break;
                    if (add || e->Boolean()) {
                        e->release();
                        e = nxt->copying(false);
                        check_tensor |= e->isPureList();
                        check_tensor |= !e->is_same_tensor(result->last());
                        result->append(e);
                        add = true;
                    }
                    e->release();
                    nxt = current_list->next_iter_exchange(lisp, iter);
                }
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            //(droplist '(< _ 10) (iota 10))
            //(droplist '(< 10 _) (iota 10)) <=> (droplist '(< 10) (iota 10))
            //where _ is the slot filling for our variable

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                e = call->eval(lisp);
                if (add || e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    if (check_tensor == a_tensor && result->size())
                        check_tensor |= !e->is_same_tensor(result->last());
                    result->append(e);
                    add = true;
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            current_list = result->index(0)->newTensor(lisp, (List*)result);
            if (current_list != result) {
                result->release();
                return current_list;
            }
        }
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        result->release();
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
                for (long j = 1; j <= e->asInteger(); j++) {
                    ((Integers*)res)->liste.push_back(j);
                }
            }
            else {
                res = lisp->provideNumbers();
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
        for (long i = 1; i < sz; i++) {
            e = liste[i]->eval(lisp);
            if (e->type == t_integer) {
                sub = lisp->provideIntegers();
                res->append(sub);
                for (long j = 1; j <= e->asInteger(); j++) {
                    ((Integers*)sub)->liste.push_back(j);
                }
            }
            else {
                sub = lisp->provideNumbers();
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
                for (long j = 0; j < e->asInteger(); j++) {
                    ((Integers*)res)->liste.push_back(j);
                }
            }
            else {
                res = lisp->provideNumbers();
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
        for (long i = 1; i < sz; i++) {
            e = liste[i]->eval(lisp);
            if (e->type == t_integer) {
                sub = lisp->provideIntegers();
                res->append(sub);
                for (long j = 0; j < e->asInteger(); j++) {
                    ((Integers*)sub)->liste.push_back(j);
                }
            }
            else {
                sub = lisp->provideNumbers();
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



// (° '* '(2 3 4)  '(1 2 3 4))
// (° '* (rho 2 3 '(4 5 6 9)) (rho 3 3 (iota 10)))
Element* List_outerproduct_eval::eval(LispE* lisp) {
    //Operation is: (° operation l1 l2)
    Element* l1 = liste[2]->eval(lisp);
    
    Element* l2 = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    bool sb = lisp->set_true_as_one();

    try {
        lisp->checkState(this);
        l2 = liste[3]->eval(lisp);
        
        if (!l1->isList() || !l2->isList())
            throw new Error("Error: arguments for '°' are lists");
        
        if (l1 == l2) {
            switch (l2->type) {
                case t_floats:
                    l2 = lisp->provideFloats();
                    ((Floats*)l2)->liste = ((Floats*)l1)->liste;
                    break;
                case t_numbers:
                    l2 = lisp->provideNumbers();
                    ((Numbers*)l2)->liste = ((Numbers*)l1)->liste;
                    break;
                case t_shorts:
                    l2 = new Shorts();
                    ((Shorts*)l2)->liste = ((Shorts*)l1)->liste;
                    break;
                case t_integers:
                    l2 = lisp->provideIntegers();
                    ((Integers*)l2)->liste = ((Integers*)l1)->liste;
                    break;
                case t_strings:
                    l2 = lisp->provideStrings();
                    ((Strings*)l2)->liste = ((Strings*)l1)->liste;
                    break;
                case t_stringbytes:
                    l2 = new Stringbytes();
                    ((Stringbytes*)l2)->liste = ((Stringbytes*)l1)->liste;
                    break;
            }
        }

        op = eval_body_as_argument(lisp, liste[1], P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        call = lisp->provideCall(op, 2);
        
        vecte<long> size;
        l1->getShape(size);
        l2->getShape(size);
        char char_tensor = 0;
        res = lisp->provideList();
        ((List*)res)->combine(lisp, res, l1, l2, call, char_tensor);
        if (char_tensor == a_tensor || char_tensor == a_valuelist) {
            Element* nxt = res->index(0)->newTensor(lisp, (List*)res);
            if (nxt != res) {
                res->release();
                res = nxt;
            }
        }
        
        call->force_release();
        l1->release();
        l2->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return res;
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        lisp->reset_to_true(sb);
        l1->release();
        l2->release();
        res->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return null_;
}

Element* List_rank_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    
    long listsz = liste.size();

    Element* e = null_;
    Element* lst = null_;
    vecte<long> positions;
    long i, v;
    
    try {
        lisp->checkState(this);
        for (i = 2; i < listsz; i++) {
            e = liste[i]->eval(lisp);
            if (e->isNumber()) {
                v = e->asInteger();
                positions.push_back(v);
            }
            else
                positions.push_back(-1);
            _releasing(e);
        }
        
        while (positions.size() > 1 && positions.back() < 0)
            positions.pop_back();

        lst = element->rank(lisp, positions);
        element->release();
        if (lst == NULL) {
            lisp->resetStack();
            return emptylist_;
        }
    }
    catch (Error* err) {
        lst->release();
        element->release();
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return lst;
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
                res->append(zero_);
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

Element* List_scan_eval::eval(LispE* lisp) {
    //Operation is: (\\ operation l1 l2)
    
    Element* current_list = liste[2]->eval(lisp);
    

    if (!current_list->isList()) {
        current_list->release();
        throw new Error("Error: argument for 'scan' should be a list");
    }

    Element* op = null_;
    Element* res = null_;
    List* call = NULL;

    long sz = current_list->size();
    if (!sz)
        return emptylist_;

    bool sb = lisp->set_true_as_one();
    
    try {
        //If we have a slice increment
        if (size() == 4) {
            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_scan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = 0; i < sz; i += increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
    
        //if l1 is a matrix, we recursively call the function on each sublist
        if (current_list->isTensor()) {
            call = new List_scan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            Element* e;
            Element* lines;
            
            res = current_list->newTensor(true, lisp);
            for (long i = 0; i < shape[0]; i++) {
                lines = current_list->newTensor(false, lisp, (List*)current_list->index(i));
                call->in_quote(2, lines);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            res->setShape();
            
            current_list->release();
            call->force_release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        if (current_list->checkListofTensor()) {
            call = new List_backscan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            call->in_quote(2, current_list);
            //current_list will be cleaned by call if necessary
            current_list = null_;
            res = call->eval(lisp);
            call->force_release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }

        lisp->checkState(this);
        op = liste[1]->eval(lisp);

        if (op->isLambda() || op->isList()) {
            res = op->scan(lisp, current_list, sz);
            current_list->release();
            op->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        res = lisp->provideList();
        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        bool monadic = op->check_arity(lisp, P_TWO);
        
        Element* e;
        
        if (lisp->delegation->comparators.check(op->label0())) {
            call = lisp->provideCall(op, 2);
            for (long i = 0; i < sz - 1; i++) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i + 1));
                e = call->eval(lisp);
                res->append(e);
            }
        }
        else {
            call = lisp->provideCall(op, 1);
            
            e = current_list->value_on_index(lisp, (long)0);
            res->append(e);
            
            call->in_quote(1, e);
            
            if (!monadic) {
                call->append(lisp->quoted());
                for (long i = 1; i < sz; i++) {
                    call->in_quote(2, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                    call->in_quote(1, e);
                }
            }
            else {
                for (long i = 1; i < sz; i++) {
                    call->in_quote(1, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                }
            }
        }
        call->force_release();
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        lisp->reset_to_true(sb);
        res->release();
        current_list->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    current_list->release();
    lisp->reset_to_true(sb);
    lisp->resetStack();
    return res;
}
//------------------------------------------------------------------
Element* List::backscan(LispE* lisp, Element* current_list, long sz) {
    Element* res = current_list->newInstance();

    if (size() == 0)
        return res;
    
    //this is a filter, the first list
    long j = sz - 1;
    long nb;
    try {
        for (long i = size() - 1; i >= 0 ; i--) {
            nb = liste[i]->asInteger();
            if (!nb)
                res->append(zero_);
            else {
                if (j <= -1)
                    throw new Error("Error: List size mismatch");

                while (nb > 0) {
                    res->append(current_list->index(j));
                    nb--;
                }
                j--;
            }
        }
        if (j <= -1)
            throw new Error("Error: List size mismatch");
    }
    catch (Error* err) {
        res->release();
        throw err;
    }
    return res;
}

Element* List_lambda_eval::backscan(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    List* res = lisp->provideList();

    Element* rarg1 = this;
    Element* rarg2 = this;
    
    int16_t arg1 = labels[0];
    int16_t arg2 = labels[1];

    try {
        sz--;
        Element* e = current_list->index(sz)->copying(false);

        //if there is already a variable with this name on the stack
        //we record it to restore it later...
        rarg1 = lisp->record_or_replace(e, arg1);
        rarg2 = lisp->record_or_replace(null_, arg2);

        res->append(e);
        
        for (long i = sz-1; i >= 0; i--) {
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

Element* List_backscan_eval::eval(LispE* lisp) {
    //Operation is: (-\\ operation l1 l2)
    Element* current_list = liste[2]->eval(lisp);
    if (!current_list->isList()) {
        current_list->release();
        throw new Error("Error: argument for 'backscan' should be a list");
    }

    Element* op = null_;

    Element* res = null_;
    List* call = NULL;

    long sz = current_list->size();
    if (!sz)
        return null_;

    bool sb = lisp->set_true_as_one();
    try {
        //If we have a slice increment
        if (size() == 4) {
            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_backscan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = sz - increment; i >= 0; i -= increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        //if l1 is a matrix, we recursively call the function on each sublist out of the transposed matrix
        if (current_list->isTensor() || current_list->checkListofTensor()) {
            call = new List_backscan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            //We extract our columns
            Element* e;
            Element* columns = NULL;
            
            res = current_list->newTensor(true, lisp);
            for (long i = 0; i < shape[1]; i++) {
                columns = current_list->newTensor(true, lisp);
                for (long j = 0; j < shape[0]; j++) {
                    columns->append(current_list->index(j)->index(i));
                }
                columns->setShape();
                call->in_quote(2, columns);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            if (current_list->type == t_list) {
                columns = current_list->index(0)->newTensor(lisp, (List*)res);
                res->release();
                res = columns;
            }
            else
                res->setShape();
            
            call->force_release();
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }
        
        lisp->checkState(this);
        
        op = liste[1]->eval(lisp);
        
        if (op->isLambda() || op->isList()) {
            res = op->backscan(lisp, current_list, sz);
            op->release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }

        res = lisp->provideList();
        
        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        Element* e;
        
        if (lisp->delegation->comparators.check(op->label0())) {
            call = lisp->provideCall(op, 2);
            for (long i = sz - 1; i > 0; i--) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i - 1));
                e = call->eval(lisp);
                res->append(e);
            }
        }
        else {
            bool monadic = op->check_arity(lisp, P_TWO);
            
            call = lisp->provideCall(op, 1);
            
            sz--;
            Element* e = current_list->value_on_index(lisp, sz);
            res->append(e);
            call->in_quote(1, e);
            
            if (!monadic) {
                call->append(lisp->quoted());
                for (long i = sz-1; i >= 0; i--) {
                    call->in_quote(2, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                    call->in_quote(1, e);
                }
            }
            else {
                for (long i = sz-1; i >= 0; i--) {
                    call->in_quote(1, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                }
            }
        }
        call->force_release();
        current_list->release();
        lisp->reset_to_true(sb);
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        res->release();
        lisp->reset_to_true(sb);
        current_list->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return res;
}

//------------------------------------------------------------------------------------
Element* List::reduce(LispE* lisp, Element* current_list, long sz) {
    if (sz == 1) {
        Element* op = current_list->value_on_index(lisp, (long)0);
        lisp->resetStack();
        return op;
    }
    
    Element* res = current_list->newInstance();

    long lsz = size();
    
    if (lsz == 0)
        return res;
    
    try {
        long j = 0;
        long nb;
        
        for (long i = 0; i < lsz; i++) {
            nb = liste[i]->asInteger();
            if (!nb) {
                j++;
                continue;
            }
            
            if (j >= sz)
                throw new Error("Error: List size mismatch");
            
            while (nb > 0) {
                res->append(current_list->index(j));
                nb--;
            }
            j++;
        }
    }
    catch (Error* err) {
        res->release();
        throw err;
    }

    return res;
}


Element* List_lambda_eval::reduce(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    int16_t arg1 = 0;
    int16_t arg2 = 0;
    Element* rarg1 = this;
    Element* rarg2 = this;
    try {
        arg1 = labels[0];
        arg2 = labels[1];
        
        Element* element = current_list->index(0)->copying(false);
        rarg1 = lisp->record_or_replace(element, arg1);
        rarg2 = lisp->record_or_replace(current_list->index(1), arg2);
        
        element = eval_lambda_min(lisp);
        if (element->type != l_return) {
            lisp->replacestackvalue(element, arg1);
            
            for (long i = 2; i < sz; i++) {
                lisp->replacestackvalue(current_list->index(i), arg2);
                element = eval_lambda_min(lisp);
                if (element->type == l_return)
                    break;
                lisp->replacestackvalue(element, arg1);
            }
        }
        element->increment();
        lisp->reset_in_stack(rarg2, arg2);
        lisp->reset_in_stack(rarg1, arg1);
        element->decrementkeep();
        return element;
    }
    catch (Error* err) {
        if (rarg2 != this)
            lisp->reset_in_stack(rarg2, arg2);
        if (rarg1 != this)
            lisp->reset_in_stack(rarg1, arg1);
        throw err;
    }
    
    return null_;
}

Element* List_reduce_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
        
    Element* current_list = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    bool sb = false;
    long sz = 0;
    
    try {
        lisp->checkState(this);
        if (listsz == 2) {
            //This is a copy
            current_list = liste[1]->eval(lisp);
            op = current_list->fullcopy();
            if (op != current_list)
                current_list->release();
            lisp->resetStack();
            return op;
        }
        
        current_list = liste[2]->eval(lisp);
        
        if (!current_list->isList())
            throw new Error("Error: argument for 'reduce' should be a list");
        
        sz = current_list->size();
        if (!sz) {
            lisp->resetStack();
            return null_;
        }
        
        sb = lisp->set_true_as_one();
        
        //If we have a slice increment
        if (listsz == 4) {
            // (setq r (integers 1 3 2 7 8 9 11 2 4 5))

            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_reduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = 0; i < sz; i += increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        // (// '+ (rho 5 4 3 2 (iota 120)))
        //if l1 is a matrix, we recursively call the function on each sublist
        if (current_list->isTensor()) {
            call = new List_reduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            //We extract our columns
            Element* e;
            Element* lines;
            
            res = current_list->newTensor(true, lisp);
            
            for (long i = 0; i < shape[0]; i++) {
                lines = current_list->newTensor(false, lisp, (List*)current_list->index(i));
                call->in_quote(2, lines);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            res->setShape();
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        if (current_list->checkListofTensor()) {
            call = new List_backreduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            call->in_quote(2, current_list);
            //It will be cleaned with call
            current_list = null_;
            res = call->eval(lisp);
            call->force_release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }

        op = liste[1]->eval(lisp);

        if (op->isLambda() || op->isList()) {
            res = op->reduce(lisp, current_list, sz);
            current_list->release();
            op->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }

        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        
        call = lisp->provideCall(op, 2);
        call->in_quote(1, current_list->value_on_index(lisp, (long)0));
        call->in_quote(2, current_list->index(1));
        
        Element* e = call->eval(lisp);
        
        if (lisp->delegation->comparators.check(op->label0())) {
            for (long i = 1; i < current_list->size() - 1 && e == one_; i++) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i + 1));
                e = call->eval(lisp);
            }
            
            call->force_release();
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return e;
        }
        
        call->in_quote(1, e);
        for (long i = 2; i < current_list->size(); i++) {
            call->in_quote(2, current_list->index(i));
            e = call->eval(lisp);
            call->in_quote(1, e);
        }
        e->increment();
        call->force_release();
        e->decrementkeep();
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return e->release_but_last();
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        res->release();
        current_list->release();
        lisp->reset_to_true(sb);
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
}
//------------------------------------------------------------------------------------
Element* List::backreduce(LispE* lisp, Element* current_list, long sz) {
    if (sz == 1) {
        Element* op = current_list->value_on_index(lisp, (long)0);
        lisp->resetStack();
        return op;
    }

    Element* res = current_list->newInstance();

    if (size() == 0)
        return res;

    try {
        //this is a filter, the first list
        long j = sz - 1;
        long nb;
        
        for (long i = size() - 1; i >= 0; i--) {
            nb = liste[i]->asInteger();
            if (!nb) {
                j--;
                continue;
            }
            if (j <= -1)
                throw new Error("Error: List size mismatch");
            while (nb > 0) {
                res->append(current_list->index(j));
                nb--;
            }
            j--;
        }
    }
    catch (Error* err) {
        res->release();
        throw err;
    }

    return res;
}

Element* List_lambda_eval::backreduce(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    int16_t arg1 = 0;
    int16_t arg2 = 0;
    Element* rarg1 = this;
    Element* rarg2 = this;

    current_list = current_list->reverse(lisp);

    try {
        
        arg1 = labels[0];
        arg2 = labels[1];
        
        Element* element = current_list->index(0)->copying(false);
        
        rarg1 = lisp->record_or_replace(element, arg1);
        rarg2 = lisp->record_or_replace(current_list->index(1), arg2);
        
        element = eval_lambda_min(lisp);
        if (element->type != l_return) {
            lisp->replacestackvalue(element, arg1);
            
            for (long i = 2; i < sz; i++) {
                lisp->replacestackvalue(current_list->index(i), arg2);
                element = eval_lambda_min(lisp);
                if (element->type == l_return)
                    break;
                lisp->replacestackvalue(element, arg1);
            }
        }
        
        element->increment();
        
        lisp->reset_in_stack(rarg2, arg2);
        lisp->reset_in_stack(rarg1, arg1);
        current_list->release();
        
        element->decrementkeep();
        return element;
    }
    catch (Error* err) {
        if (rarg2 != this)
            lisp->reset_in_stack(rarg2, arg2);
        if (rarg1 != this)
            lisp->reset_in_stack(rarg1, arg1);
        current_list->release();
        throw err;
    }
    return null_;
}

Element* List_backreduce_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (-// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    long sz = 0;
    bool sb = false;
    
    try {
        lisp->checkState(this);
        if (listsz == 2) {
            //This is a copy
            current_list = liste[1]->eval(lisp);
            op = current_list->reverse(lisp);
            if (op != current_list)
                current_list->release();
            lisp->resetStack();
            return op;
        }
        
        current_list = liste[2]->eval(lisp);
        
        if (!current_list->isList())
            throw new Error("Error: argument for 'backreduce' should be a list");
        
        sz = current_list->size();
        if (!sz) {
            lisp->resetStack();
            return null_;
        }
        
        sb = lisp->set_true_as_one();
        
        //If we have a slice increment
        if (listsz == 4) {
            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_backreduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = sz - increment; i >= 0; i -= increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        if (current_list->isTensor() || current_list->checkListofTensor()) {
            call = new List_backreduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            //We extract our columns
            Element* e;
            Element* columns = NULL;
            
            // (-// '+ (rho 5 4 3 2 (iota 120)))
            // (-// '+ (rho 4 3 2 (iota 24)))
            res = current_list->newTensor(true, lisp);
            for (long i = 0; i < shape[1]; i++) {
                columns = current_list->newTensor(true, lisp);
                for (long j = 0; j < shape[0]; j++) {
                    columns->append(current_list->index(j)->index(i));
                }
                columns->setShape();
                call->in_quote(2, columns);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            if (current_list->type == t_list) {
                columns = current_list->index(0)->newTensor(lisp, (List*)res);
                res->release();
                res = columns;
            }
            else
                res->setShape();
            
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        op = liste[1]->eval(lisp);
        
        if (op->isLambda() || op->isList()) {
            res = op->backreduce(lisp, current_list, sz);
            op->release();
            current_list->release();
            lisp->resetStack();
            return res;
        }
        
        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        Element* e = current_list->reverse(lisp);
        current_list->release();
        current_list = e;
        
        call = lisp->provideCall(op, 2);
        
        call->in_quote(1, current_list->value_on_index(lisp, (long)0));
        call->in_quote(2, current_list->index(1));
        
        e = null_;
        e = call->eval(lisp);
        
        if (lisp->delegation->comparators.check(op->label0())) {
            for (long i = 1; i < current_list->size() - 1 && e == one_; i++) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i + 1));
                e = call->eval(lisp);
            }
            
            call->force_release();
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return e;
        }

        
        call->in_quote(1, e);
        for (long i = 2; i < current_list->size(); i++) {
            call->in_quote(2, current_list->index(i));
            e = call->eval(lisp);
            call->in_quote(1, e);
        }
        e->increment();
        call->force_release();
        e->decrementkeep();
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return e->release_but_last();
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        current_list->release();
        lisp->reset_to_true(sb);
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
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
        return one_;
    else
        return zero_;
}

Element* List::comparison(LispE* lisp, Element* l) {
    if (l->isList()) {
        if (l->size() != size())
            return zero_;
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
            return zero_;
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
            return zero_;
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
            return zero_;
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
            return zero_;
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
            return zero_;
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
            return zero_;
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

Element* List_equal_eval::eval(LispE* lisp) {
    Element* first_element = liste[1]->eval(lisp);
    Element* second_element;
    char test = true;

    try {
        lisp->checkState(this);
        for (long i = 2; i < size(); i++) {
            second_element = liste[i]->eval(lisp);
            if (!first_element->isequal(lisp, second_element)) {
                test = false;
                break;
            }
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
Element* List::evall_infix(LispE* lisp) {
    List_infix_eval m(this);
    return m.eval(lisp);
}

//Infix Expressions: x op y op z op u
Element* List_infix_eval::eval(LispE* lisp) {
    Element* expression = liste[1]->eval(lisp);
    long listsize = expression->size();

    if (expression->type != t_list || !listsize)
        return expression;
    
    expression = expression->duplicate_constant(lisp);
    //We need first to detect the atom LIST structure
    long ival;
    Element* e = null_;
    Element* eprec;
    List* inter = lisp->provideList();
    //We use sub to execute infix on sub-list within the current list
    List* sub = lisp->provideList();
    sub->append(lisp->provideAtom(l_infix));
    List* root;
    
    if (listsize == 2 && expression->index(0)->isNonOperatorAtom() && expression->index(1)->type == t_list) {
        //this is a function call: test(i j k) --> (test i j k l)
        e = expression->index(1);
        sub->append(e->quoting());
        e = sub->evall_infix(lisp);
        inter->append(expression->index(0));
        if (e->index(0)->label() == l_concatenate) {
            for (ival = 1; ival < e->size(); ival++)
                inter->append(e->index(ival));
            e->release();
        }
        else {
            if (e->size() == 1) {
                inter->append(e->index(0));
                e->release();
            }
            else
                inter->append(e);
        }
        sub->release();
        expression->release();
        return inter;
    }
    
    bool modified = false;
    for (ival = 0; ival < listsize; ival++) {
        eprec = e;
        e = expression->index(ival);
        if (e->type == t_list) {
            sub->append(e->quoting());
            e = sub->evall_infix(lisp);
            if (eprec->isPureAtom() && ival) {
                root = lisp->provideList();
                root->append(eprec);
                root->append(e);
                e = root;
                inter->pop();
            }
            inter->append(e);
            sub->pop();
            modified = true;
        }
        else
            inter->append(e);
    }
    
    if (modified) {
        expression->release();
        expression = inter;
        listsize = expression->size();
    }
    else
        inter->release();
    
    if (listsize <= 1 || !(listsize & 1)) {
        sub->release();
        return expression;
    }
    
    List* operations = lisp->provideList();
    root = operations;

    //First we gather all our ops, there should one be every two elements
    Element* op = expression->index(1);
    operations->append(op);
    
    long iop = 3;
    ival = 0;

    try {
        lisp->checkState(this);
        while (ival < listsize) {
            
            //We check the sequence of operators of the same kind
            for (; iop < listsize - 1; iop++) {
                if (expression->index(iop)->isAtom() && op != expression->index(iop)) {
                    op = expression->index(iop);
                    break;
                }
            }
            
            //we then push all the values up to the operator that is different
            for (;ival < iop; ival += 2)
                operations->append(expression->index(ival));
            
            if (iop < listsize -1) {
                //On this case, the current operator becomes the new head
                char comp = lisp->delegation->checkComparator(op->type, root->index(0)->type);
                if (comp == -1) {
                    operations = root;
                    iop += 2;
                    continue;
                }
                
                if (comp) {
                    inter = lisp->provideList();
                    inter->append(op);
                    inter->append(root);
                    root = inter;
                    operations = root;
                }
                else {
                    //We create one level down.
                    //We feed this new List with the last element of the current list
                    inter = lisp->provideList();
                    inter->append(op);
                    Element* last = operations->liste.back();
                    inter->append(last);
                    operations->pop();
                    operations->append(inter);
                    operations = inter;
                }
            }
            iop += 2;
        }
    }
    catch (Error* err) {
        expression->release();
        root->release();
        sub->rawrelease();
        return lisp->check_error(this, err, idxinfo);
    }
    expression->release();
    sub->rawrelease();
    lisp->resetStack();
    return root;
}


Element* List_insert_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);

    int16_t lstsize = liste.size();
    Element* second_element = null_;
    Element* third_element = NULL;
    List* comparison = NULL;

    try {
        lisp->checkState(this);
        //We insert a value in a list
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (lstsize == 3) {
            ix = container->default_insertion();
        }
        else {
            third_element = liste[3]->eval(lisp);
            if (third_element->isNumber())
                ix = third_element->asInteger();
            else {
                third_element = eval_body_as_argument_min(lisp, third_element, P_THREE);
                
                comparison = lisp->provideCall(third_element, 2);
                comparison->in_quote(1, second_element);
                
                Element* result = container->insert_with_compare(lisp, second_element, *comparison);
                container->release();
                comparison->force_release();
                lisp->resetStack();
                return result;
            }
        }
        
        third_element = container->insert(lisp, second_element, ix);
        second_element->release();
        if (third_element != container) {
            container->release();
            lisp->resetStack();
            return third_element;
        }
    }
    catch (Error* err) {
        container->release();
        if (comparison != NULL)
            comparison->force_release();
        else {
            second_element->release();
            if (third_element != NULL)
                third_element->release();
        }
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return container;
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
        throw new Error(L"Error: Missing label for 'label'");

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
        throw new Error(L"Error: 'zipwith' only accepts lists as arguments");
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
        
        ITEM& item = *lists->liste.item;
        
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

Element* List_zipwith_eval::eval(LispE* lisp) {
    Element* container = liste[2]->eval(lisp);
    if (!container->isList()) {
        container->release();
        throw new Error(L"Error: 'zipwith' only accepts lists as arguments");
    }
    
    int16_t listsize = size();
    if (liste.back()->type == v_null)
        listsize--;

    List* call = NULL;
    
    List* lists = lisp->provideList();
    Element* function = liste[1];
    
    long i, lsz, j = 0, szl = container->size();
    
    if (container->type == t_llist)
        lists->append(new Iter_llist((LList*)container, szl));
    else
        lists->append(container);
    
    
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
        
        ITEM& item = *lists->liste.item;
        
        function = eval_body_as_argument(lisp, function, _arity(lsz+1));
        if (function->is_straight_eval())
            call = (List*)function;
        else {
            call = lisp->provideList();
            call->append(function);
        }
        
        methodEval met = lisp->delegation->evals[function->type];
        for (i = 0; i < lsz; i++)
            call->append(lisp->quoted(item[i]->index(0)));
        
        Element* value = (call->*met)(lisp);
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
                call->in_quote(i + 1, item[i]->index(j));
            value = (call->*met)(lisp);
            container->append(value);
            value->release();
        }
        
        call->force_release();
        lists->release();
    }
    catch (Error* err) {
        container->increment();
        if (call != NULL)
            call->force_release();
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
    for (long i = 1; i < listsize && second_element == null_; i++) {
        liste[i]->setterminal(terminal);
        second_element = liste[i]->eval(lisp);
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
        throw new Error(L"Error: missing list in 'extend'");
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

Element* List_sort_eval::eval(LispE* lisp) {
    Element* comparator = liste[1]->eval(lisp);
    Element* container = null_;

    comparator = eval_body_as_argument_min(lisp, comparator, P_THREE, true);

    try {
        lisp->checkState(this);
        //First element is the comparison function OR an operator
        container = liste[2]->eval(lisp);
        if (!container->isList())
            throw new Error(L"Error: the second argument should be a list for 'sort'");
    }
    catch (Error* err) {
        comparator->force_release();
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    
    List* complist;
    switch (container->type) {
        case t_floats:
        case t_numbers:
        case t_shorts:
        case t_integers:
        case t_strings:
        case t_stringbytes: { // (sort (\(x y) (< y x)) (iota 10))
            try {
                complist = (List*)comparator;
                complist->append(null_);
                complist->append(null_);
                
                container->sorting(lisp, complist);
                
                complist->force_release();
                lisp->resetStack();
                return container;
            }
            catch (Error* err) {
                complist->liste[1] = null_;
                complist->liste[2] = null_;
                complist->force_release();
                container->release();
                return lisp->check_error(this, err, idxinfo);
            }
        }
        case t_llist: {
            List* l = (List*)container->asList(lisp, lisp->provideList());
            if (l->size() <= 1) {
                l->release();
                comparator->force_release();
                lisp->resetStack();
                return container;
            }
            
            complist = lisp->provideCall(comparator, 2);

            complist->in_quote(1, l->index(0));
            complist->in_quote(2, l->index(0));
            try {
                if (complist->eval(lisp)->Boolean()) {
                    throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
                }
            }
            catch (Error* err) {
                container->release();
                complist->force_release();
                return lisp->check_error(this, err, idxinfo);
            }

            l->liste.sorting(lisp, complist);
            u_link* it = ((LList*)container)->liste.begin();
            for (long i = 0; i < l->size(); i++) {
                it->value = l->index(i);
                it = it->next();
            }
            
            l->release();
            complist->force_release();
            lisp->resetStack();
            return container;
        }
        default: {
            List* l = (List*)container;
            if (l->size() <= 1) {
                comparator->force_release();
                lisp->resetStack();
                return container;
            }
            
            complist = lisp->provideCall(comparator, 2);

            complist->in_quote(1, l->index(0));
            complist->in_quote(2, l->index(0));
            try {
                if (complist->eval(lisp)->Boolean()) {
                    throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
                }
            }
            catch (Error* err) {
                container->release();
                complist->force_release();
                return lisp->check_error(this, err, idxinfo);
            }
            l->liste.sorting(lisp, complist);
            complist->force_release();
            lisp->resetStack();
            return container;
        }
    }
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

#ifdef LISPE_WASM
Element* List_setq_eval::eval(LispE* lisp) {
    Element* e = liste[2]->eval(lisp);
    if (thrown_error)
        return e;
    
    lisp->storing_variable(e, liste[1]->label());
    return True_;
}

Element* List_let_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    if (thrown_error)
        return element;
    lisp->storing_variable(element, liste[1]->label());
    
    long sz = size();
    if (sz > 3) {
        Element* e = null_;
        for (long i = 3; i < sz && !thrown_error; i++) {
            e->release();
            e = liste[i]->eval(lisp);
        }
        if (thrown_error)
            lisp->removefromstack(liste[1]->label());
        else
            lisp->removefromstack(liste[1]->label(), e);
        return e;
    }
    return True_;
}
#else
Element* List_setq_eval::eval(LispE* lisp) {
    Element* element = liste[2]->eval(lisp);
    lisp->checkState(this);
    lisp->storing_variable(element, liste[1]->label());
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
    int16_t label = liste[1]->label();
    Element* element = liste[2]->eval(lisp);
    lisp->checkState(this);
    if (!lisp->delegation->replaceFunction(element, label, lisp->current_space))
        lisp->storing_global(element, label);
    lisp->resetStack();
    return True_;
}

Element* List_seth_eval::eval(LispE* lisp) {
    if (lisp->check_thread_stack) {
        Element* element = liste[2]->eval(lisp);
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
#ifdef LISPE_WASM
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
#ifdef LISPE_WASM
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
#ifdef LISPE_WASM
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
#ifdef LISPE_WASM
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

#ifdef LISPE_WASM
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
        err->release();
        lisp->delegation->reset_context();
        first_element = liste.back()->eval(lisp);
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
    if (function->protected_index(lisp, (long)0)->type == l_defpat) {
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

#ifdef LISPE_WASM
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
            if (code->element_container()) {
                result->increment();
                code->release();
                result->decrementkeep();
            }
            else
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
    Shorts* n = new Shorts();
    Element* values;
    try {
        lisp->checkState(this);
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
    LList* a_llist = a_llist = new LList(&lisp->delegation->mark);
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
                e = values->liste[var]->value_on_index(lisp, ix);
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
    Floats* n = lisp->provideFloats();
    Element* values;
    try {
        lisp->checkState(this);
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
        return zero_;
    if (v < 0)
        return minusone_;
    return one_;
}


Element* List_sleep_eval::eval(LispE* lisp) {
    long tm;
    evalAsInteger(1, lisp, tm);
    std::this_thread::sleep_for(std::chrono::milliseconds(tm));
    return True_;
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
            return v?lisp->provideFloat(v):zero_;
        }
        case t_numbers: {
            double v = ((Numbers*)first_element)->liste.sum();
            first_element->release();
            return v?lisp->provideNumber(v):zero_;
        }
        case t_shorts: {
            int16_t v = ((Shorts*)first_element)->liste.sum();
            first_element->release();
            return v?new Short(v):zero_;
        }
        case t_integers: {
            long v = ((Integers*)first_element)->liste.sum();
            first_element->release();
            return v?lisp->provideInteger(v):zero_;
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
            return v?lisp->provideFloat(v):zero_;
        }
        case t_numbers: {
            double v = ((Numbers*)first_element)->liste.product();
            first_element->release();
            return v?lisp->provideNumber(v):zero_;
        }
        case t_shorts: {
            int16_t v = ((Shorts*)first_element)->liste.product();
            first_element->release();
            return v?new Short(v):zero_;
        }
        case t_integers: {
            long v = ((Integers*)first_element)->liste.product();
            first_element->release();
            return v?lisp->provideInteger(v):zero_;
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
                    return zero_;
                }
                lst = lst->mod(lisp, NULL);
                first_element->release();
                lisp->resetStack();
                return lst;
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
                    return zero_;
                }
                lst = lst->leftshift(lisp, NULL);
                first_element->release();
                lisp->resetStack();
                return lst;
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
                    return zero_;
                }
                lst = lst->rightshift(lisp, NULL);
                first_element->release();
                lisp->resetStack();
                return lst;
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
                        return zero_;
                    }
                    lst = lst->bit_and(lisp, NULL);
                    first_element->release();
                    return lst;
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
                        return zero_;
                    }
                    lst = lst->bit_or(lisp, NULL);
                    first_element->release();
                    return lst;
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
                        return zero_;
                    }
                    lst = lst->bit_xor(lisp, NULL);
                    first_element->release();
                    return lst;
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
                        return zero_;
                    }
                    lst = lst->bit_and_not(lisp, NULL);
                    first_element->release();
                    return lst;
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
        res = evall_if(lisp);
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
        res = evall_ife(lisp);
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
