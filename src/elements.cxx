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
binSetIter::binSetIter(binSet& b) {
    indexes = b.indexes;
    tsize = b.tsize;
    base = b.base;
    idx = 0;
    nb = 0;
    filter = indexes[0];
}

void binSetIter::set(binSet& b) {
    indexes = b.indexes;
    tsize = b.tsize;
    base = b.base;
    idx = 0;
    nb = 0;
    filter = indexes[0];
}

//------------------------------------------------------------------------------------------
Returnpool::Returnpool(LispE* l) : lisp(l), Element(l_return) {
    value = null_;
}
//------------------------------------------------------------------------------------------
Rankloop::Rankloop(LispE* lp, List* l) : List(l,0) {
    last = false;
    type = l_irank;
    lisp = lp;
    index_value = null_;
    lst = l;
    max_iterator = 0;
}
//------------------------------------------------------------------------------------------
int16_t Element::function_label() {
    throw new Error("Error: Not a function or a data structure");
}
//------------------------------------------------------------------------------------------
Element* Float::duplicate_constant(LispE* lisp) {
    return !status?this:lisp->provideFloat(content);
}

Element* Number::duplicate_constant(LispE* lisp) {
    return !status?this:lisp->provideNumber(content);
}

Element* Short::duplicate_constant(LispE* lisp) {
    return !status?this:new Short(content);
}

Element* Complex::duplicate_constant(LispE* lisp) {
    return !status?this:new Complex(content);
}

Element* Integer::duplicate_constant(LispE* lisp) {
    return !status?this:lisp->provideInteger(content);
}
Element* String::duplicate_constant(LispE* lisp) {
    return !status?this:lisp->provideString(content);
}
//------------------------------------------------------------------------------------------

Element* Float::copyatom(LispE* lisp, uint16_t s) {
    return (status < s)?this:lisp->provideFloat(content);
}

Element* Number::copyatom(LispE* lisp, uint16_t s) {
    return (status < s)?this:lisp->provideNumber(content);
}


Element* Integer::copyatom(LispE* lisp, uint16_t s) {
    return (status < s)?this:lisp->provideInteger(content);
}

Element* String::copyatom(LispE* lisp, uint16_t s) {
    return (status < s)?this:lisp->provideString(content);
}

Element* Dictionary::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary* d = lisp->provideDictionary();
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Dictionary_n::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary_n* d = lisp->provideDictionary_n();
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Dictionary_i::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Dictionary_i* d = lisp->provideDictionary_i();
    Element* e;
    for (const auto& a: dictionary) {
        e = a.second->copying(false);
        d->dictionary[a.first] = e;
        e->increment();
    }
    return d;
}

Element* Set_s::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Set_s* st = lisp->provideSet_s();
    st->ensemble = ensemble;
    return st;
}

Element* Set_i::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Set_i* st = lisp->provideSet_i();
    st->ensemble = ensemble;
    return st;
}

Element* Set_n::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Set_n* st = lisp->provideSet_n();
    st->ensemble = ensemble;
    return st;
}

Element* Set::copyatom(LispE* lisp, uint16_t s) {
    if (status < s)
        return this;
    
    Set* st = lisp->provideSet();
    st->dictionary = dictionary;
    for (const auto& a : st->dictionary)
        a.second->increment();
    return st;
}

Element* List::copyatom(LispE* lisp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    List* l = lisp->provideList();
    for (long i = 0; i < liste.size(); i++) {
        l->append(liste[i]->copyatom(lisp, s));
    }
    //The release here needs a bit of explanation
    //the current list could be a cdr on a new list
    //which means that shared would return a value > 1
    release();
    return l;
}

Element* Floats::copyatom(LispE* lisp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Floats* f = lisp->provideFloats(this);
    release();
    return f;
}

Element* Numbers::copyatom(LispE* lisp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Numbers* n = lisp->provideNumbers(this);
    release();
    return n;
}

Element* Integers::copyatom(LispE* lisp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Integers* i = lisp->provideIntegers(this);
    release();
    return i;
}

Element* Strings::copyatom(LispE* lisp, uint16_t s) {
    if (liste.shared(status) < s)
        return this;
    
    Strings* sl = lisp->provideStrings(this);
    release();
    return sl;
}

//------------------------------------------------------------------------------------------
void Quotedpool::release() {
    if (!status) {
        value->decrement();
        lisp->quoted_pool.push_back(this);
    }
}

void Quotedpool::decrement() {
    status -= not_protected();
    if (!status) {
        value->decrement();
        lisp->quoted_pool.push_back(this);
    }
}

void Quotedpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        value->decrement();
        lisp->quoted_pool.push_back(this);
    }
}

void Floatpool::decrement() {
    status -= not_protected();
    if (!status)
        lisp->float_pool.push_back(this);
}

void Floatpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status)
        lisp->float_pool.push_back(this);
}

void Floatpool::release() {
    if (!status)
        lisp->float_pool.push_back(this);
}

void Numberpool::decrement() {
    status -= not_protected();
    if (!status)
        lisp->number_pool.push_back(this);
}

void Numberpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status)
        lisp->number_pool.push_back(this);
}

void Numberpool::release() {
    if (!status)
        lisp->number_pool.push_back(this);
}

void Returnpool::decrement() {
    status -= not_protected();
    if (!status)
        lisp->return_pool.push_back(this);
}

void Returnpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status)
        lisp->return_pool.push_back(this);
}

void Returnpool::release() {
    if (!status)
        lisp->return_pool.push_back(this);
}

void Integerpool::decrement() {
    status -= not_protected();
    if (!status) {
        lisp->integer_pool.push_back(this);
    }
}

void Integerpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        lisp->integer_pool.push_back(this);
    }
}

void Integerpool::release() {
    if (!status) {
        lisp->integer_pool.push_back(this);
    }
}

void Stringpool::decrement() {
    status -= not_protected();
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

void Stringpool::decrementstatus(uint16_t nb) {
    status -= nb * not_protected();
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

void Stringpool::release() {
    if (!status) {
        content = U"";
        lisp->string_pool.push_back(this);
    }
}

Element* Floatpool::fullcopy() {
    if (lisp->preparingthread)
        return new Number(content);
    return lisp->provideFloat(content);
}

Element* Floatpool::copyatom(LispE* lsp, uint16_t s) {
    return (status < s)?this:lsp->provideFloat(content);
}

Element* Floatpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Float(content);
    
    if (!status)
        return this;
    return lisp->provideFloat(content);
}

Element* Numberpool::fullcopy() {
    if (lisp->preparingthread)
        return new Number(content);
    return lisp->provideNumber(content);
}

Element* Numberpool::copyatom(LispE* lsp, uint16_t s) {
    return (status < s)?this:lsp->provideNumber(content);
}

Element* Numberpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Number(content);
    
    if (!status)
        return this;
    return lisp->provideNumber(content);
}

Element* Integerpool::fullcopy() {
    if (lisp->preparingthread)
        return new Integer(content);
    return lisp->provideInteger(content);
}

Element* Integerpool::copyatom(LispE* lsp, uint16_t s) {
    return (status < s)?this:lsp->provideInteger(content);
}

Element* Integerpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new Integer(content);
    
    if (!status)
        return this;
    
    return lisp->provideInteger(content);
}

Element* Constfloat::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new Number(content);
    return lisp->provideFloat(content);
}

Element* Constfloat::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new Number(content);
    return lisp->provideFloat(content);
}

Element* Constfloat::copyatom(LispE* lsp, uint16_t s) {
    return lsp->provideFloat(content);
}

Element* Constfloat::duplicate_constant(LispE* lisp) {
    return lisp->provideFloat(content);
}

Element* Constnumber::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new Number(content);
    return lisp->provideNumber(content);
}

Element* Constnumber::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new Number(content);
    return lisp->provideNumber(content);
}

Element* Constnumber::copyatom(LispE* lsp, uint16_t s) {
    return lsp->provideNumber(content);
}

Element* Constnumber::duplicate_constant(LispE* lisp) {
    return lisp->provideNumber(content);
}

Element* Constinteger::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new Integer(content);
    return lisp->provideInteger(content);
}

Element* Constinteger::copyatom(LispE* lsp, uint16_t s) {
    return lsp->provideInteger(content);
}

Element* Constinteger::duplicate_constant(LispE* lisp) {
    return lisp->provideInteger(content);
}

Element* Constinteger::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new Integer(content);
    return lisp->provideInteger(content);
}

Element* Constshort::fullcopy() {
    return new Short(content);
}

Element* Constshort::copyatom(LispE* lsp, uint16_t s) {
    return new Short(content);
}

Element* Constshort::duplicate_constant(LispE* lisp) {
    return new Short(content);
}

Element* Constshort::copying(bool duplicate) {
    return new Short(content);
}

Element* Conststring::copying(bool duplicate) {
    if (!provide || lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Conststring::fullcopy() {
    if (!provide || lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Conststring::copyatom(LispE* lsp, uint16_t s) {
    return lsp->provideString(content);
}

Element* Conststring::duplicate_constant(LispE* lisp) {
    return lisp->provideString(content);
}

Element* Stringpool::fullcopy() {
    if (lisp->preparingthread)
        return new String(content);
    return lisp->provideString(content);
}

Element* Stringpool::copyatom(LispE* lsp, uint16_t s) {
    return (status < s)?this:lsp->provideString(content);
}

Element* Stringpool::copying(bool duplicate) {
    //If we are in a thread preparation, then we
    //copy it as non pool objects
    //to avoid pool objects to access a lisp thread environment
    //through the wrong lisp pointer
    if (lisp->preparingthread)
        return new String(content);
    
    if (!status)
        return this;
    
    return lisp->provideString(content);
}

//------------------------------------------------------------------------------------------
Infinitelist::Infinitelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

Cyclelist::Cyclelist(LispE* lisp) : Element(t_list) {
    value = null_;
    idx = 0;
}

Dictionary_as_buffer::Dictionary_as_buffer(LispE* lisp) : Element(t_dictionary) {
    dico = lisp->provideDictionary();
    choice = true;
}


//------------------------------------------------------------------------------------------
Element* Element::minimum(LispE* lisp) {
    throw new Error("Error: cannot find the minimum for this object");
}

//------------------------------------------------------------------------------------------
Element* Element::maximum(LispE* lisp) {
    throw new Error("Error: cannot find the maximum for this object");
}

//------------------------------------------------------------------------------------------

Element* Element::minmax(LispE* lisp) {
    throw new Error("Error: cannot find the minmax for this object");
}

//------------------------------------------------------------------------------------------
void Element::flatten(LispE* lisp, List* l) {
    l->append(this);
}

void Element::flatten(LispE* lisp, Numbers* l) {
    l->append(this);
}

//------------------------------------------------------------------------------------------

void Element::prettyfying(LispE* lisp, string& code, long mx) {
    List* l = NULL;
    if (type_element() == t_list) {
        l = (List*)this;
        if (l->usermark()) {
            code += "...";
            return;
        }
        l->setusermark(true);
    }
    
    if (isList()) {
        if (size() == 0) {
            code += " ()";
            if (l)
                l->setusermark(false);
            return;
        }
        
        int16_t type = index(0)->type;
        if (type == l_lambda) {
            code += " ";
            code += toString(lisp);
            if (l)
                l->setusermark(false);
            return;
        }
        
        Element* params;
        
        if (type == l_defun || type == l_defpat || type == l_deflib) {
            code += "(";
            code += lisp->toString(type);
            code += " ";
            code += index(1)->toString(lisp);
            code += " ";
            params = index(2);
            if (type == l_defpat) {
                code += "(";
                string local;
                for (long i = 0; i < params->size(); i++) {
                    if (i)
                        code += " ";
                    local = index(i)->toString(lisp);
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
                index(i)->prettyfying(lisp, code, mx);
            }
            code += ")\n";
            if (l)
                l->setusermark(false);
            return;
        }
        
        if (type == l_loop || type == l_mloop || type == l_lloop) {
            code += "(";
            //The command name
            code += lisp->toString(type);
            code += " ";
            //Iterative variables
            code += index(1)->toString(lisp);
            code += " ";
            params = index(2);
            if (params->isList()) {
                code += "\n";
                params->prettyfying(lisp, code, mx);
            }
            else {
                code += params->toString(lisp);
                code += "\n";
            }
            for (long i = 3; i < size(); i++) {
                index(i)->prettyfying(lisp, code, mx);
            }
            code += ")\n";
            if (l)
                l->setusermark(false);
            return;
        }
        
        long i = 0;
        
        if (type == l_if || type == l_check || type == l_ncheck || type == l_ife) {
            code += "(";
            code += index(i++)->toString(lisp);
            code += " ";
            string condition = index(i++)->toString(lisp);
            if (condition.size() >= mx) {
                index(i-1)->prettyfying(lisp, code, mx);
            }
            else {
                code += condition;
                code += "\n";
            }
            for (; i < size(); i++) {
                index(i)->prettyfying(lisp, code, mx);
            }
            code += ")\n";
            if (l)
                l->setusermark(false);
            return;
        }
        
        string local = toString(lisp);
        if (local.size() < mx) {
            code += local;
            code += "\n";
            if (l)
                l->setusermark(false);
            return;
        }
        
        code += "(";
        
        if (type == l_while || type == l_setq || type == l_setg || type == l_loopcount || type == l_key || type == l_keyn) {
            code += index(i++)->toString(lisp);
            code += " ";
            string condition = index(i++)->toString(lisp);
            if (condition.size() >= mx) {
                index(i-1)->prettyfying(lisp, code, mx);
            }
            else {
                code += condition;
                code += "\n";
            }
        }
        else {
            if (type > t_error && type < l_final) {
                code += index(i++)->toString(lisp);
                i = 1;
            }
            code += "\n";
        }
        
        for (; i < size(); i++) {
            index(i)->prettyfying(lisp, code, mx);
            if (code.back() != '\n')
                code += "\n";
        }
        code += ")\n";
        if (l)
            l->setusermark(false);
        return;
    }
    if (isString())
        code += jsonstring(toString(lisp));
    else {
        if (isDictionary()) {
            string local = toString(lisp);
            if (local.size() < 50) {
                code += local;
                if (l)
                    l->setusermark(false);
                return;
            }
            code += "{\n";
            if (type == t_dictionary) {
                map<u_ustring, Element*>& dico = ((Dictionary*)this)->dictionary;
                u_ustring key;
                for (const auto& a: dico) {
                    local = "";
                    key = a.first;
                    s_unicode_to_utf8(local, key);
                    code += local;
                    code += ":";
                    a.second->prettyfying(lisp, code, mx);
                    if (code.back() != '\n')
                        code += "\n";
                }
                code += "}\n";
                if (l)
                    l->setusermark(false);
                return;
            }
            unordered_map<double, Element*>& dico = ((Dictionary_n*)this)->dictionary;
            for (const auto& a: dico) {
                local = convertToString(a.first);
                code += local;
                code += ":";
                a.second->prettyfying(lisp, code, mx);
                if (code.back() != '\n')
                    code += "\n";
            }
            code += "}\n";
            if (l)
                l->setusermark(false);
            return;
        }
        else
            code += toString(lisp);
        
        if (l)
            l->setusermark(false);
    }
}

//(defpat action ( [Take 'x] [Take y] )(if (check_object position x) (block (push belongings x) (println "Ok we have picked up" x)) (println "Cannot pick up the" x)))
//(prettify '((12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10)))

string Element::prettify(LispE* lisp, long mx) {
    if (isString())
        return toString(lisp);
    
    string code;
    prettyfying(lisp, code, mx);
    string body;
    IndentCode(code, body, GetBlankSize(), true, false);
    return body;
}

//------------------------------------------------------------------------------------------
Element* Element::invert_sign(LispE* lisp) {
    throw new Error("Error: Cannot invert this object");
}

Element* Float::invert_sign(LispE* lisp) {
    if (!status) {
        content *= -1;
        return this;
    }
    return lisp->provideFloat(content * -1);
}

Element* Number::invert_sign(LispE* lisp) {
    if (!status) {
        content *= -1;
        return this;
    }
    return lisp->provideNumber(content * -1);
}

Element* Short::invert_sign(LispE* lisp) {
    if (!status) {
        content *= -1;
        return this;
    }
    return new Short(content * -1);
}

Element* Complex::invert_sign(LispE* lisp) {
    if (!status) {
        content *= -1;
        return this;
    }
    std::complex<double> c(content);
    c *= -1;
    return new Complex(c);
}

Element* Integer::invert_sign(LispE* lisp) {
    if (!status) {
        content *= -1;
        return this;
    }
    return lisp->provideInteger(content * -1);
}

//------------------------------------------------------------------------------------------
//This method returns the needed instructions to build the dictionary
Element* Dictionary_as_list::dictionary(LispE* lisp) {
    Element* keycmd;
    List* last_element = lisp->provideList();
    
    if (!choice || keyvalues.size() != valuevalues.size()) {
        if (valuevalues.size() == 0) {
            //We create a set...
            if (type == t_float || type == t_number)
                keycmd = lisp->delegation->_SET_NUMBERS;
            else {
                if (type == t_short || type == t_integer)
                    keycmd = lisp->delegation->_SET_INTEGERS;
                else
                    keycmd = lisp->delegation->_SET_STRINGS;
            }
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
        if (type == t_float || type == t_number)
            keycmd = lisp->delegation->_DICO_NUMBER;
        else {
            if (type == t_short || type == t_integer)
                keycmd = lisp->delegation->_DICO_INTEGER;
            else
                keycmd = lisp->delegation->_DICO_STRING;
        }
        
        //We generate: (key k v k' v' k" v"...)
        last_element->append(keycmd);
        for (long i = 0; i < keyvalues.size(); i++) {
            last_element->append(keyvalues[i]);
            last_element->append(valuevalues[i]);
        }
    }
    
    try {
        //See below for some explanations
        keycmd = last_element->eval(lisp);
        last_element->release();
        return keycmd;
    }
    catch(Error* err) {
        err->release();
        return last_element;
    }
}

//A little bit of explanation here
//When a dictionary is created with actual values in its description.
//It has no variables for instance, then we create the actual dictionary by calling eval on (key ...)
//However, the values in it can be regular values that must be stored in the garbage.
//However, there is a catch, when using eval on a string, we will store these values here in the garbage.
//We do not want these values to be protected... However, they will be removed from the garbage in eval and
//never be deleted. We set their status with a specific value: s_inside to avoid this problem.

//------------------------------------------------------------------------------------------

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
Element* Element::rank(LispE* lisp, vecte<long>& positions) {
    throw new Error("Error: cannot apply 'rank' to this element");
}

//------------------------------------------------------------------------------------------

Element* Element::loop(LispE* lisp, int16_t label,  List* code) {
    return null_;
}

Element* String::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    long i = 0;
    u_ustring localvalue;
    long szc = content.size();
    while (i < szc) {
        lisp->handlingutf8->getchar(content, localvalue, i, szc);
        element = lisp->provideString(localvalue);
        lisp->replacingvalue(element, label);
        _releasing(e);
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


void* Rankloop::begin_iter() {
    i_nxt = 0;
    return null_;
}

Element* Rankloop::next_iter(LispE* lisp, void* iter) {
    if (i_nxt == max_iterator)
        return emptyatom_;
    
    Element* rank;
    if (last)
        rank = lst->rank(lisp, positions);
    else {
        positions.push_back(i_nxt);
        rank = lst->rank(lisp, positions);
        positions.pop_back();
    }
    i_nxt++;
    return rank;
}

Element* Rankloop::next_iter_exchange(LispE* lisp, void* iter)  {
    if (i_nxt == max_iterator)
        return emptyatom_;
    
    Element* rank;
    if (last)
        rank = lst->rank(lisp, positions);
    else {
        positions.push_back(i_nxt);
        rank = lst->rank(lisp, positions);
        positions.pop_back();
    }
    i_nxt++;
    return rank;
}

Element* Rankloop::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* ranks = null_;
    
    for (long i = 0; i < max_iterator; i++) {
        if (last)
            ranks = lst->rank(lisp, positions);
        else {
            positions.push_back(i);
            ranks = lst->rank(lisp, positions);
            positions.pop_back();
        }
        lisp->replacingvalue(ranks, label);
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

Element* InfiniterangeNumber::loop(LispE* lisp, int16_t label, List* code) {
    if (!increment)
        throw new Error("Error: increment cannot be 0");
    long i_loop;
    Element* e = null_;
    long sz = code->liste.size();
    double value = initial_value;
    
    Number* element;
    lisp->recording(null_, label);
    
    char check = 0;
    if (!infinite_loop) {
        if (increment < 0)
            check = -1;
        else
            check = 1;
    }
    
    while (!lisp->hasStopped() && compare(check, value)) {
        element = lisp->provideNumber(value);
        lisp->replacingvalue(element, label);
        _releasing(e);
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

Element* InfiniterangeInteger::loop(LispE* lisp, int16_t label, List* code) {
    if (!increment)
        throw new Error("Error: increment cannot be 0");
    
    long i_loop;
    Element* e = null_;
    long sz = code->liste.size();
    long value = initial_value;
    Integer* element;
    lisp->recording(null_, label);
    char check = 0;
    if (!infinite_loop) {
        if (increment < 0)
            check = -1;
        else
            check = 1;
    }
    
    while (!lisp->hasStopped() && compare(check, value)) {
        element = lisp->provideInteger(value);
        lisp->replacingvalue(element, label);
        _releasing(e);
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

Element* Infinitelist::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    lisp->recording(element, label);
    
    while (!lisp->hasStopped()) {
        _releasing(e);
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

Element* Cyclelist::loop(LispE* lisp, int16_t label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    if (element->type != t_list) {
        element->release();
        throw new Error("Error: we can only cycle on list");
    }
    List* values = (List*)element;
    long sze = values->liste.size();
    long i = 0;
    //We then execute our instructions
    while (!lisp->hasStopped()) {
        element = values->liste[i]->copying(false);
        lisp->replacingvalue(element, label);
        _releasing(e);
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

/*
 In this case variables are in a list...
 (loop (x y z) lx ly lz ...)
 
 (loop [x y z] '(1 3 4) '(5 6 7) '(9 10 13) (println (+ x y z)))
 */

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

Element* s_count(LispE* lisp, u_ustring& s, u_ustring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return zero_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return zero_;
    long nb = 0;
    while (pos != -1) {
        nb++;
        pos=s.find(sub,pos+sz);
    }
    return lisp->provideInteger(nb);
}

//------------------------------------------------------------------------------------------
Element* Element::insert(LispE* lisp, Element* e, long ix) {
    return null_;
}

Element* Element::insert_with_compare(LispE*, Element* e, List& comparison) {
    throw new Error("Error: insertion impossible");
}

void String::push_element(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        content += value->asUString(lisp);
        value->release();
    }
}

void String::push_element_true(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        if (value->label() > 1) {
            content += value->asUString(lisp);
            value->release();
        }
    }
}

void String::push_element_front(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        content.insert(0, value->asUString(lisp));
        value->release();
    }
}

void String::push_element_back(LispE* lisp, List* l) {
    Element* value;
    for (long i = 2; i < l->size(); i++) {
        value = l->liste[i]->eval(lisp);
        content += value->asUString(lisp);
        value->release();
    }
}

Element* String::insert(LispE* lisp, Element* e, long ix) {
    u_ustring res;
    if (ix < 0)
        res = lisp->handlingutf8->u_insert_sep(content, e->asUString(lisp));
    else {
        if (ix >= content.size())
            res = content + e->asUString(lisp);
        else {
            res = content;
            res.insert(ix, e->asUString(lisp));
        }
    }
    return lisp->provideString(res);
}



Element* String::insert_with_compare(LispE* lisp, Element* e, List& comparison) {
    long end = size();
    if (!end) {
        content += e->asUString(lisp);
        return this;
    }
    
    String* ct = lisp->provideString();
    
    comparison.in_quote(2, ct);
    
    Element* test = NULL;
    
    if (end < 3) {
        ct->content = content[0];
        test = comparison.eval(lisp);
        if (test->Boolean())
            insertion(e, 0);
        else {
            if (end == 2) {
                ct->content = content[1];
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
        ct->content = content[i];
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
            ct->content = content[begin];
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
            ct->content = content[end];
            test = comparison.eval(lisp);
            if (test->Boolean())
                insertion(e, end);
            else
                insertion(e, end + 1);
        }
    }
    return this;
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

Element* Element::thevalues(LispE* lisp) {
    return emptylist_;
}

//------------------------------------------------------------------------------------------
Element* Element::Boolean(LispE* lisp) {
    return booleans_[Boolean()];
}

//------------------------------------------------------------------------------------------
Element* Element::next_iter(LispE* lisp, void* it) {
    return emptyatom_;
}

Element* Element::next_iter_exchange(LispE* lisp, void* it) {
    return emptyatom_;
}

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
Element* Element::search_element(LispE* lisp, Element* valeur, long ix) {
    return null_;
}

Element* String::search_element(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.find(val, ix);
    return (ix == -1)?null_:lisp->provideInteger(ix);
}

//------------------------------------------------------------------------------------------
bool Element::check_element(LispE* lisp, Element* valeur) {
    return false;
}

bool String::check_element(LispE* lisp, Element* valeur) {
    u_ustring val = valeur->asUString(lisp);
    return (content.find(val, 0) != -1);
}

//------------------------------------------------------------------------------------------

Element* Element::checkkey(LispE* lisp, Element* e) {
    return null_;
}

//------------------------------------------------------------------------------------------
Element* Element::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    return zero_;
}

Element* String::replace_all_elements(LispE* lisp, Element* valeur, Element* remp) {
    u_ustring cherche = valeur->asUString(lisp);
    u_ustring remplacement = remp->asUString(lisp);
    long nb = nb_ureplacestring(content,cherche, remplacement);
    return lisp->provideInteger(nb);
}

//------------------------------------------------------------------------------------------
Element* Element::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    return emptylist_;
}

Element* String::search_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.find(val, ix);
    return s_findall(lisp,content, val, ix);
}

//------------------------------------------------------------------------------------------
Element* Element::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    return zero_;
}

Element* String::count_all_elements(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.find(val, ix);
    return s_count(lisp,content, val, ix);
}

//------------------------------------------------------------------------------------------
Element* Element::list_and(LispE* lisp, Element* value) {
    return this;
}

Element* String::list_and(LispE* lisp, Element* value) {
    String* result = lisp->provideString();
    u_ustring val = value->asUString(lisp);
    for (long i = 0; i < content.size(); i++) {
        if (val.find(content[i]) != -1 && result->content.find(content[i]) == -1)
            result->content += content[i];
    }
    return result;
}

//------------------------------------------------------------------------------------------
Element* Element::list_or(LispE* lisp, Element* value) {
    return this;
}

Element* String::list_or(LispE* lisp, Element* value) {
    String* result = lisp->provideString();
    u_ustring val = value->asUString(lisp);
    long i;
    for (i = 0; i < content.size(); i++) {
        if (result->content.find(content[i]) == -1)
            result->content += content[i];
    }
    
    for (i = 0; i < val.size(); i++) {
        if (result->content.find(val[i]) == -1)
            result->content += val[i];
    }
    
    return result;
}

//------------------------------------------------------------------------------------------
Element* Element::list_xor(LispE* lisp, Element* value) {
    return this;
}

Element* String::list_xor(LispE* lisp, Element* value) {
    String* intersection = (String*)list_and(lisp, value);
    
    String* result = lisp->provideString();
    u_ustring val = value->asUString(lisp);
    long i;
    for (i = 0; i < content.size(); i++) {
        if (intersection->content.find(content[i]) == -1 && result->content.find(content[i]) == -1)
            result->content += content[i];
    }
    
    for (i = 0; i < val.size(); i++) {
        if (intersection->content.find(val[i]) == -1 && result->content.find(val[i]) == -1)
            result->content += val[i];
    }
    intersection->release();
    return result;
}

//------------------------------------------------------------------------------------------
Element* Element::search_reverse(LispE* lisp, Element* valeur, long ix) {
    return null_;
}

Element* String::search_reverse(LispE* lisp, Element* valeur, long ix) {
    u_ustring val = valeur->asUString(lisp);
    ix =  content.rfind(val, content.size() - ix);
    return (ix == -1)?null_:lisp->provideInteger(ix);
}

//------------------------------------------------------------------------------------------
Element* Element::reverse(LispE* lisp, bool duplicate) {
    return emptylist_;
}

Element* Float::reverse(LispE* lisp, bool duplicate) {
    return lisp->provideFloat(content*-1);
}

Element* Number::reverse(LispE* lisp, bool duplicate) {
    return lisp->provideNumber(content*-1);
}

Element* Short::reverse(LispE* lisp, bool duplicate) {
    return new Short(content*-1);
}

Element* Complex::reverse(LispE* lisp, bool duplicate) {
    std::complex<double> c(content);
    c *= -1;
    return new Complex(c);
}

Element* Integer::reverse(LispE* lisp, bool duplicate) {
    return lisp->provideInteger(content*-1);
}

Element* String::reverse(LispE* lisp, bool duplicate) {
    u_ustring resultat;
    for (long i = content.size()-1; i >= 0; i--)
        resultat += content[i];
    
    if (duplicate)
        return lisp->provideString(resultat);
    
    content = resultat;
    return this;
}

// duplicate is not taking into account for LList
// there are two many cases where it creates dangling structures...

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

//------------------------------------------------------------------------------------------
Element* Element::value_on_index(LispE* lisp, long i) {
    return null_;
}

Element* String::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* Element::value_from_index(LispE* lisp, long i) {
    return null_;
}

Element* String::value_from_index(LispE* lisp, long i) {
    return lisp->provideString(content[i]);
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(wstring& k, LispE* lisp) {
    return null_;
}

Element* Element::value_on_index(u_ustring& k, LispE* lisp) {
    return null_;
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(double k, LispE* lisp) {
    return null_;
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(LispE* lisp, Element* i) {
    return null_;
}

Element* String::value_on_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

//------------------------------------------------------------------------------------------
Element* Element::protected_index(LispE* lisp,Element*) {
    throw new Error("Error: value cannot be access through index");
}

Element* String::protected_index(LispE* lisp, Element* ix) {
    long i = ix->checkInteger(lisp);
    
    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    throw new Error("Error: index out of bounds");
}

//------------------------------------------------------------------------------------------
Element* Element::join_in_list(LispE* lisp, u_ustring& sep) {
    throw new Error("Error: 'join' can only be used for lists");
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
    if (i < 0) {
        i += content.size();
        if (i < 0)
            throw new Error("Error: index out of bounds");
    }
    
    if (i >= content.size())
        throw new Error("Error: index out of bounds");
    
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
        s_utf8_to_unicode(w, s, s.size());
        std::wstringstream msg;
        msg << message << L", line: " << lisp->delegation->i_current_line << L" in: " << w;
        return msg.str();
    }
    else
        return message;
}
//------------------------------------------------------------------------------------------
u_ustring Atomefonction::asUString(LispE* lisp) {
    return lisp->asUString(function_label);
}

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

Element* String::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content == ((String*)e)->content)];
}

Element* Number::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && content == e->asNumber())];
}

Element* Float::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && content == e->asFloat())];
}

Element* Short::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && content == e->asShort())];
}

Element* Complex::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_complex && content == ((Complex*)e)->content)];
}

Element* Integer::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && content == e->asInteger())];
}

bool Element::egal(Element* e) {
    return (e==this);
}

bool Atome::egal(Element* e) {
    return (e->label() == atome);
}

bool Maybe::egal(Element* e) {
    return (e->label() == t_error);
}

bool String::egal(Element* e) {
    return (e->type == t_string && content == ((String*)e)->content);
}

bool Number::egal(Element* e) {
    return (e->isNumber() && content == e->asNumber());
}

bool Float::egal(Element* e) {
    return (e->isNumber() && content == e->asFloat());
}

bool Short::egal(Element* e) {
    return (e->isNumber() && content == e->asShort());
}

bool Complex::egal(Element* e) {
   return (e->type == t_complex && content == ((Complex*)e)->content);
}

                      
bool Integer::egal(Element* e) {
    return (e->isNumber() && content == e->asInteger());
}

//------------------------------------------------------------------------------------------
Element* Element::less(LispE* lisp, Element* e) {
    return False_;
}

Element* Element::compare(LispE* lisp, Element* e) {
    return null_;
}

Element* Element::lessorequal(LispE* lisp, Element* e){
    return False_;
}

Element* Element::more(LispE* lisp, Element* e) {
    return False_;
}

Element* Element::moreorequal(LispE* lisp, Element* e) {
    return False_;
}

Element* String::less(LispE* lisp, Element* e) {
    return booleans_[content < e->asUString(lisp)];
}

Element* String::compare(LispE* lisp, Element* e) {
    u_ustring v = e->asUString(lisp);
    int16_t test = (content == v) + (content < v) * 2;
    return lisp->delegation->_COMPARE_BOOLEANS[test];
}

Element* String::lessorequal(LispE* lisp, Element* e){
    return booleans_[content <= e->asUString(lisp)];
}

Element* String::more(LispE* lisp, Element* e) {
    return booleans_[content > e->asUString(lisp)];
}

Element* String::moreorequal(LispE* lisp, Element* e) {
    return booleans_[content >= e->asUString(lisp)];
}

Element* Number::less(LispE* lisp, Element* e) {
    return booleans_[content < e->asNumber()];
}

Element* Number::compare(LispE* lisp, Element* e) {
    double v = e->asNumber();
    int16_t test = (content == v) + (content < v) * 2;
    return lisp->delegation->_COMPARE_BOOLEANS[test];
}

Element* Float::less(LispE* lisp, Element* e) {
    return booleans_[content < e->asFloat()];
}

Element* Float::compare(LispE* lisp, Element* e) {
    float v = e->asFloat();
    int16_t test = (content == v) + (content < v) * 2;
    return lisp->delegation->_COMPARE_BOOLEANS[test];
}

Element* Float::lessorequal(LispE* lisp, Element* e){
    return booleans_[content <= e->asFloat()];
}

Element* Number::lessorequal(LispE* lisp, Element* e){
    return booleans_[content <= e->asNumber()];
}

Element* Float::more(LispE* lisp, Element* e) {
    return booleans_[content > e->asFloat()];
}

Element* Number::more(LispE* lisp, Element* e) {
    return booleans_[content > e->asNumber()];
}

Element* Float::moreorequal(LispE* lisp, Element* e) {
    return booleans_[content >= e->asFloat()];
}

Element* Number::moreorequal(LispE* lisp, Element* e) {
    return booleans_[content >= e->asNumber()];
}

Element* Integer::less(LispE* lisp, Element* e) {
    return booleans_[content < e->asInteger()];
}

Element* Integer::compare(LispE* lisp, Element* e) {
    long v = e->asInteger();
    int16_t test = (content == v) + (content < v) * 2;
    return lisp->delegation->_COMPARE_BOOLEANS[test];
}

Element* Short::less(LispE* lisp, Element* e) {
    return booleans_[content < e->asShort()];
}

char compare_complex(std::complex<double>& c, std::complex<double>& v, char cmp) {
    switch (cmp) {
        case 0:
            return (c.real() < v.real() && c.imag() < v.imag());
        case 1:
            return (c.real() <= v.real() && c.imag() <= v.imag());
        case 2:
            return (c.real() > v.real() && c.imag() > v.imag());
        case 3:
            return (c.real() >= v.real() && c.imag() >= v.imag());
        default:
            return ((c == v) + 2*(c.real() < v.real() && c.imag() < v.imag()));
    }
}

Element* Complex::less(LispE* lisp, Element* e) {
    return booleans_[e->type == t_complex && compare_complex(content, ((Complex*)e)->content, 0) == -1];
}

Element* Short::compare(LispE* lisp, Element* e) {
    int16_t v = e->asShort();
    int16_t test = (content == v) + (content < v) * 2;
    return lisp->delegation->_COMPARE_BOOLEANS[test];
}

Element* Complex::compare(LispE* lisp, Element* e) {
    if (e->type == t_complex) {
        int16_t test = compare_complex(content, ((Complex*)e)->content, 4);
        return lisp->delegation->_COMPARE_BOOLEANS[test];
    }
    else
        throw new Error("Error: cannot compare these values");
}

Element* Short::lessorequal(LispE* lisp, Element* e) {
    return booleans_[content <= e->asShort()];
}

Element* Complex::lessorequal(LispE* lisp, Element* e) {
    return booleans_[e->type == t_complex && compare_complex(content, ((Complex*)e)->content, 1) == -1];
}

Element* Integer::lessorequal(LispE* lisp, Element* e){
    return booleans_[content <= e->asInteger()];
}

Element* Integer::more(LispE* lisp, Element* e) {
    return booleans_[content > e->asInteger()];
}

Element* Integer::moreorequal(LispE* lisp, Element* e) {
    return booleans_[content >= e->asInteger()];
}

Element* Short::more(LispE* lisp, Element* e) {
    return booleans_[content > e->asShort()];
}

Element* Complex::more(LispE* lisp, Element* e) {
    return booleans_[e->type == t_complex && compare_complex(content, ((Complex*)e)->content, 2) == -1];
}

Element* Short::moreorequal(LispE* lisp, Element* e) {
    return booleans_[content >= e->asShort()];
}

Element* Complex::moreorequal(LispE* lisp, Element* e) {
    return booleans_[e->type == t_complex && compare_complex(content, ((Complex*)e)->content, 3) == -1];
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

//------------------------------------------------------------------------------------------
Element* Element::extraction(LispE* lisp, List* l) {
    return null_;
}

Element* String::extraction(LispE* lisp, List* liste) {
    Element* e_from = liste->liste[2];
    
    long from;
    long firstisString = -1;
    int16_t nxt = 3;
    int16_t ty;
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
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
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
            if (firstisString == -1) firstisString = 0;
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
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
Element* Element::replace_in(LispE* lisp, List* l) {
    return null_;
}

Element* String::replace_in(LispE* lisp, List* liste) {
    Element* e_from = liste->liste.back()->eval(lisp);
    u_ustring last = e_from->asUString(lisp);
    e_from->release();
    
    e_from = liste->liste[2];
    
    long from;
    long firstisString = -1;
    int16_t nxt = 3;
    int16_t ty;
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
        case t_float:
        case t_short:
        case t_integer:
        case t_number:
            from = e_from->asInteger();
            if (from < 0)
                from = content.size() + from;
            break;
        default:
            e_from->release();
            throw new Error("Error: cannot use the first position in 'setrange'");
    }
    
    e_from->release();
    
    if (from < 0 || from >= content.size())
        return this;
    
    if (nxt == liste->size() - 1) {
        //Only one element is returned
        u_ustring result = content.substr(0, from);
        result += last;
        result += content.substr(from+1, content.size());
        return lisp->provideString(result);
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
            if (firstisString == -1) firstisString = 0;
            u_ustring ch = e_upto->asUString(lisp);
            upto = content.find(ch, from + firstisString);
            if (upto == -1)
                return emptystring_;
            break;
        }
        case t_plus_string: {
            if (firstisString == -1) firstisString = 0;
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
                    upto = content.size() + upto;
                }
            }
            break;
        default:
            e_upto->release();
            throw new Error("Error: cannot use the second position in 'setrange'");
    }
    
    e_upto->release();
    if (upto <= from)
        return emptystring_;
    
    if (upto > content.size())
        upto = content.size();
    u_ustring result = content.substr(0, from);
    result += last;
    result += content.substr(upto, content.size());
    return lisp->provideString(result);
}
//------------------------------------------------------------------------------------------
void Listincode::protecting(bool protection, LispE* lisp) {
    if (protection) {
        if (status == s_constant)
            status = s_protect;
    }
    else {
        if (status == s_protect) {
            status = s_constant;
            lisp->garbaging(this);
        }
    }
    
    for (long i = 0; i < liste.size(); i++)
        liste[i]->protecting(protection, lisp);
}
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
Element* Element::asList(LispE* lisp, List* l) {
    l->append(copying(false));
    return l;
}

Element* String::asList(LispE* lisp, List* courant) {
    Tokenizer parse;
    
    parse.asList = true;
    lisp_code retour = lisp->segmenting(content, parse);
    if (!parse.tokens.size())
        return emptylist_;
    
    long index = 0;
    switch (retour) {
        case e_error_brace:
            throw new Error("Error: braces do not balance");
        case e_error_bracket:
            throw new Error("Error: brackets do not balance");
        case e_error_parenthesis:
            throw new Error("Error: parentheses do not balance");
        case e_error_string:
            throw new Error("Error: missing end of string");
        default:
            index = 0;
    }
    
    return lisp->syntaxTree(courant, parse, index, false);
}

//------------------------------------------------------------------------------------------
//For running car/cdr, everything that is not List is an error
Element* Element::cadr(LispE* lisp, Element*) {
    throw new Error("Error: No more elements to traverse with 'cad..r'");
}

Element* Element::car(LispE* lisp) {
    u_ustring err = U"Error: You cannot apply 'car' to: '";
    err += asUString(lisp);
    err += U"'";
    throw new Error(err);
}

Element* Element::cdr(LispE* lisp) {
    u_ustring err = U"Error: You cannot apply 'cdr' to: '";
    err += asUString(lisp);
    err += U"'";
    throw new Error(err);
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

Element* Element::cadr(LispE* lisp, u_ustring& actions) {
    u_ustring err = U"Error: You cannot apply 'car' or 'cdr' to: '";
    err += asUString(lisp);
    err += U"'";
    throw new Error(err);
}

//cadr and cdr do not take into account cycles
//They are bounded by the number of times cdr is called...
//Beware that in the case of recursive calls, you cannot
//expect cadr to return an empty list if there is a cycle within
//This is the main difference with loop, which will detect a cycle and stop

Element* String::cadr(LispE* lisp, u_ustring& action) {
    long pos = 0;
    long sz = size();
    long i;
    u_ustring u;
    
    for (i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            if (i)
                throw new Error("Error: cannot apply 'car/cdr' to one character");
            u = content[pos];
            return lisp->provideString(u);
        }
        else {
            if (pos == sz)
                throw new Error("Error: No more elements to traverse with 'cad..r'");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        u = content.substr(pos, sz);
        return lisp->provideString(u);
    }
    
    return null_;
}

//Note that cycles are not taken into account here
//If the next element points inside the list again
//then cdr will return the list at this point...
//There is potentially a infinite potential loop
//in the list...

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


Element* String::next_iter(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == content.size())
        return emptyatom_;
    u_uchar u = content[n[0]];
    n[0]++;
    return lisp->provideString(u);
}

Element* String::next_iter_exchange(LispE* lisp, void* it) {
    long* n = (long*)it;
    if (n[0] == content.size())
        return emptyatom_;
    u_uchar u = content[n[0]];
    n[0]++;
    return lisp->provideString(u);
}
