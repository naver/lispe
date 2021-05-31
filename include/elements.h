/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  elements.h
//
//

#ifndef elements_h
#define elements_h

#include "tools.h"

//The status to be able to manage the life cycle of an object
const unsigned char s_destructible =  0;
const unsigned char s_protect = 254;
const unsigned char s_constant = 255;

class LispE;
class Listincode;
class Numbers;

typedef enum {
    //Default values
    v_null, v_emptylist, v_emptyatom, v_true,
    
    //Default types
    t_emptystring, t_operator, t_atom, t_number, t_integer,
    t_string, t_plus_string, t_minus_string, t_minus_plus_string,
    t_numbers, t_integers, t_strings, t_list, t_matrix, t_tensor,
    t_dictionary, t_dictionaryn, t_data, t_maybe, t_pair, t_error,
    
    l_set_max_stack_size,
    
    //Default Lisp instructions
    l_converttonumber, l_converttostring, l_converttointeger, l_converttoatom, 
    
    l_quote,
    
    //threads
    l_lock, l_waiton, l_trigger, l_threadstore, l_threadretrieve, l_threadclear,
    
    //Recording in the stack or in memory
    l_sleep, l_wait,
    l_lambda, l_defun, l_infix, l_dethread, l_deflib, l_deflibpat, l_defpat, l_defmacro, l_lib,
    l_label, l_setq, l_setg, l_block,
    l_if, l_ife,  l_ncheck, l_check, l_cond, 
    l_catch, l_throw, l_maybe, l_terminal,
    
    //Check values
    l_atomp, l_numberp, l_consp, l_zerop, l_nullp, l_stringp, l_trace, l_flip, l_select,
    
    //Numerical operations
    l_sign, l_minus_plus, l_plus, l_minus, l_multiply, l_power,
    l_leftshift, l_rightshift, l_bitand, l_bitor, l_bitxor, l_bitandnot,
    //+11 = l_opequal
    l_plusequal, l_minusequal, l_multiplyequal,  l_powerequal,
    l_leftshiftequal, l_rightshiftequal,
    l_bitnot, l_bitandequal, l_bitandnotequal, l_bitorequal, l_bitxorequal,
    l_divide, l_mod, l_divideequal,l_modequal, l_concatenate,
    l_sum, l_product,
    l_innerproduct, l_matrix, l_tensor, l_outerproduct, l_factorial, l_iota, l_iota0,
    l_reduce, l_scan, l_backreduce, l_backscan, l_equalonezero, l_rho, l_rank,
    l_member, l_transpose, l_invert, l_determinant, l_solve, l_ludcmp, l_lubksb,
    
    //Comparisons
    l_equal , l_different, l_lower, l_greater, l_lowerorequal,l_greaterorequal, l_max, l_min,
    l_size, l_use, l_index, l_at_index, l_set_at, l_extract,
    l_in, l_search, l_revertsearch, l_searchall, l_car, l_cdr, l_cadr, l_last,
    l_fread, l_fwrite, l_fappend,
    l_and, l_or, l_xor, l_not, l_eq, l_neq,
    
    //mutable operations
    l_key, l_keyn, l_keys, l_values, l_pop,
    l_list, l_cons, l_flatten, l_nconc, l_push, l_insert,
    l_unique, l_duplicate, l_rotate,
    l_numbers, l_integers, l_strings,
    
    //Display values
    l_print, l_println, l_printerr, l_printerrln, l_prettify, l_bodies,
    
    l_self, l_while, l_eval, l_mark, l_resetmark, l_loop, l_loopcount, l_range, l_irange, l_atoms, l_atomise, l_join, l_sort,
    l_load, l_input, l_getchar, l_pipe, l_type,  l_return, l_break, l_reverse,
    l_apply, l_maplist, l_filterlist, l_mapping, l_checking, l_folding,
    l_composenot, l_data, l_compose, l_map, l_filter, l_take, l_repeat, l_cycle, l_replicate, l_drop, l_takewhile, l_dropwhile,
    l_foldl, l_scanl, l_foldr, l_scanr, l_foldl1, l_scanl1, l_foldr1, l_scanr1,
    l_zip, l_zipwith,
    l_link,
    c_opening, c_closing, c_opening_bracket, c_closing_bracket, c_opening_data_brace, c_opening_brace, c_closing_brace, c_colon,
    e_error_brace, e_error_bracket, e_error_parenthesis, e_error_string, e_no_error,
    t_comment, l_final
} lisp_code;


const unsigned long P_NONE = 1 << 0;
const unsigned long P_ONE = 1 << 1;
const unsigned long P_TWO = 1 << 2;
const unsigned long P_THREE = 1 << 3;
const unsigned long P_FOUR = 1 << 4;
const unsigned long P_FIVE = 1 << 5;
const unsigned long P_SIX = 1 << 6;
const unsigned long P_SEVEN = 1 << 7;
const unsigned long P_EIGHT = 1 << 8;
const unsigned long P_NINE = 1 << 9;
const unsigned long P_TEN = 1 << 10;
const unsigned long P_ELEVEN = 1 << 11;
const unsigned long P_TWELVE = 1 << 12;
const unsigned long P_THIRTEEN = 1 << 13;
const unsigned long P_FOURTEEN = 1 << 14;
const unsigned long P_FIFTEEN = 1 << 15;
const unsigned long P_FULL = -1;
const unsigned long P_ATLEASTONE = P_FULL^P_NONE;
const unsigned long P_ATLEASTTWO = P_ATLEASTONE^P_ONE;
const unsigned long P_ATLEASTTHREE = P_ATLEASTTWO^P_TWO;
const unsigned long P_ATLEASTFOUR = P_ATLEASTTHREE^P_THREE;
const unsigned long P_ATLEASTFIVE = P_ATLEASTFOUR^P_FOUR;
const unsigned long P_ATLEASTSIX = P_ATLEASTFIVE^P_FIVE;
const unsigned long P_ATLEASTSEVEN = P_ATLEASTSIX^P_SIX;
const unsigned long P_ATLEASTEIGHT = P_ATLEASTSEVEN^P_SEVEN;
const unsigned long P_ATLEASTNINE = P_ATLEASTEIGHT^P_EIGHT;
const unsigned long P_ATLEASTTEN = P_ATLEASTNINE^P_NINE;
const unsigned long P_ATLEASTELEVEN = P_ATLEASTTEN^P_TEN;
const unsigned long P_ATLEASTTWELVE = P_ATLEASTELEVEN^P_ELEVEN;
const unsigned long P_ATLEASTTHIRTEEN = P_ATLEASTTWELVE^P_TWELVE;
const unsigned long P_ATLEASTFOURTEEN = P_ATLEASTTHIRTEEN^P_THIRTEEN;
const unsigned long P_ATLEASTFIFTEEN = P_ATLEASTFOURTEEN^P_FOURTEEN;

//false_ is actually a bit misleading as it is an alias to null_
#define false_ lisp->delegation->_NULL
#define true_ lisp->delegation->_TRUE
#define null_ lisp->delegation->_NULL

#define emptyatom_ lisp->delegation->_EMPTYATOM
#define emptystring_ lisp->delegation->_EMPTYSTRING
#define emptylist_ lisp->delegation->_EMPTYLIST
#define emptydictionary_ lisp->delegation->_EMPTYDICTIONARY

#define minusone_ lisp->delegation->_MINUSONE
#define zero_ lisp->delegation->_ZERO
#define one_ lisp->delegation->_ONE
#define two_ lisp->delegation->_TWO

#define booleans_ lisp->_BOOLEANS
#define numbools_ lisp->delegation->_NUMERICAL_BOOLEANS


#define separator_ lisp->delegation->_LISTSEPARATOR

#define error_ lisp->delegation->_ERROR
#define break_ lisp->delegation->_BREAK

#define check_mismatch -2
#define check_ok -1

class List;

class Element {
public:
    
    short type;
#ifdef DEBUG
    lisp_code lc;
#endif
    uchar status;
    
    Element(short ty) {
        type = ty;
#ifdef DEBUG
        lc = (lisp_code)type;
#endif
        status = s_destructible;
    }
    
    Element(short ty, uchar s) {
        type = ty;
#ifdef DEBUG
        lc = (lisp_code)type;
#endif 
        status = s;
    }
    
    virtual ~Element() {}
    
    virtual void clean() {}
    
    void generate_body_from_macro(LispE* lisp, Listincode* code, unordered_map<short,Element*>& dico_variables);
    void replaceVariableNames(LispE* lisp, unordered_map<short, Element*>& names);
    bool replaceVariableNames(LispE* lisp);
    
    virtual void incrementstatus(uchar nb, bool top) {
        if (status < s_protect)
            status += nb;
    }
    
    virtual void decrementstatus(uchar nb, bool top) {
        if (status > s_destructible && status < s_protect)
            status -= nb;
        if (!status)
            delete this;
    }
    
    //The status is decremented without destroying the element.
    virtual void decrementSansDelete(uchar nb) {
        if (status > s_destructible && status < s_protect)
            status-=nb;
    }
    
    virtual char isPureList() {
        return 0;
    }

    virtual char isPureList(long& x, long& y) {
        return 0;
    }
    
    virtual void getShape(vector<long>& sz) {}
    
    virtual Element* transposed(LispE* lisp) {
        return this;
    }

    virtual Element* rotate(LispE* lisp, long axis) {
        return reverse(lisp, true);
    }

    virtual void concatenate(LispE* lisp, Element* e) {}

    virtual Element* last_element(LispE* lisp);
    virtual Element* last() {
        return this;
    }
    
    virtual Element* unique(LispE* lisp) {
        return this;
    }

    virtual Element* rotate(bool left) {
        return this;
    }

    virtual bool tobegarbaged() {
        return true;
    }
    
    virtual void append(LispE* lisp, wstring& k) {}
    virtual void append(LispE* lisp, double v) {}
    virtual void append(LispE* lisp, long v) {}
    
    /*
     Duplication is forced
     unless it is for status == s_destructible
     copy is used in operations such as
     plus, minus etc. as a receptacle for calculations
     and return this object as the result of the calculation.
     If the status is s_destructible, this
     means that we can use this object as a receptacle for the calculation
     Thus in the following code:
     (setq r 100)
     (+ r 10 20)
     We want to prevent 'r' from changing its value, knowing that r->status == s_label
     
     We also use copy to stack:
     
     (setq r '(1 2 3))
     (setq l r) # call for copy, r will be duplicated
     (setq r 10) # the value of l is not lost
     
     We also use copy to manage list mutability cases.
     and dictionaries.
     unless it is for status == s_destructible
     Thus in the following code:
     
     (setq l '(1 2 3))
     (push l 10)
     
     We want to prevent the saved list: '(1 2 3) from being modified.
     Without a copy, we risk a side effect, if this code is
     in a function. So we will call 'copy' in the setq...
     
     */

    Element* duplicate() {
        if (!status)
            return this;
        return copying(true);
    }
    
    virtual Element* fullcopy() {
        return copying(true);
    }

    virtual Element* copyatom(uchar s) {
        return this;
    }

    virtual Element* newInstance() {
        return this;
    }

    virtual Element* newInstance(Element* v) {
        return this;
    }

    virtual bool checkShape(long depth, vector<long>& sz) {
        return false;
    }

    virtual Element* copying(bool duplicate = true) {
        return this;
    }
    
    virtual Element* rank(LispE* lisp, vector<long>&) {
        return NULL;
    }
    
    //We only duplicate constant containers...
    virtual Element* duplicate_constant_container(bool pair = false) {
        return this;
    }
    
    virtual void flatten(LispE*, List* l);
    virtual void flatten(LispE*, Numbers* l);
    
    virtual Element* search_element(LispE*, Element* element_value, long idx);
    virtual Element* search_all_elements(LispE*, Element* element_value, long idx);
    virtual Element* search_reverse(LispE*, Element* element_value, long idx);
    
    virtual char check_match(LispE* lisp, Element* value) {
        if (value == this)
            return check_ok;
        return false;
    }
    
    virtual bool check_arity(LispE* lisp, unsigned long);
    virtual void setmark(bool v) {}
    virtual bool mark() {return false;}
    virtual void setusermark(bool v) {}
    virtual bool usermark() {return false;}
    virtual void resetusermark() {}
    
    virtual bool unify(LispE* lisp, Element* value, bool record);
    virtual Element* check_member(LispE*, Element* s) {
        return this;
    }
    
    virtual void setterminal(bool v = true) {}
    /*
     This copy version has two purposes:
     
     a) In the case of a nulemic value or a string, we want to be able to save it as a constant.
     in the appropriate pool. This happens especially when a value is stored in a container after an arithmetic operation.
     
     b) in the case of containers, copy only duplicates the object if it is a constant.
     This is a case that appears mainly for instructions such as push, key and keys that can locally modify an object (side effect).
     but must not modify a value stored in the garbage collector from the compilation.
     */
    
    virtual Element* quoted(LispE*) {
        return this;
    }
    
    virtual Element* quoting(Element*);
    
    void prettyfying(LispE* lisp, string& code);
    string prettify(LispE* lisp);
    
    virtual string toString(LispE* lisp) {
        string s;
        wstring w = asString(lisp);
        s_unicode_to_utf8(s, w);
        return s;
    }
    
    virtual Element* loop(LispE* lisp, short label,  List* code);
    virtual wstring stringInList(LispE* lisp) {
        return asString(lisp);
    }
    
    virtual wstring jsonString(LispE* lisp) {
        return stringInList(lisp);
    }
    
    virtual wstring asString(LispE* lisp) {
        return L"";
    }
    
    virtual double asNumber() {
        return 0;
    }
    
    virtual long asInteger() {
        return 0;
    }
    
    virtual int asInt() {
        return (int)asInteger();
    }
    
    virtual bool Boolean() {
        return true;
    }
    
    Element* Boolean(LispE* lisp);
    
    virtual void append(Element* e) {}
    virtual Element* insert(LispE*, Element* e, long idx);
    virtual void insertion(Element* e, long idx) {}
    virtual void front(Element* e) {}
    virtual void beforelast(Element* e) {}
    
    virtual void appendraw(Element* e) {}
    virtual Element* replace(LispE* lisp, long i, Element* e);
    virtual Element* replace(LispE* lisp, Element* i, Element* e) {
        return replace(lisp, i->asInteger(), e);
    }
    
    virtual void replacing(long i, Element* e) {}
    virtual void change(long i, Element* e) {}
    virtual void changelast(Element* e) {}
    
    virtual Element* composing(LispE*, bool compose) {
        return this;
    }
    
    virtual Element* eval(LispE*) {
        return this;
    }
    
    virtual bool isContainer() {
        return false;
    }
    
    virtual bool isOperator() {
        return false;
    }
    
    virtual bool isExecutable(LispE*) {
        return false;
    }
    
    virtual bool isInstruction() {
        return false;
    }
    
    virtual bool isFunction() {
        return false;
    }

    virtual bool isPureAtom() {
        return false;
    }

    virtual bool isBreak() {
        return false;
    }
    
    virtual bool isAtom() {
        return false;
    }
    
    virtual bool isError() {
        return false;
    }
    
    virtual bool isString() {
        return false;
    }
    
    virtual bool isList() {
        return false;
    }

    virtual bool isValueList() {
        return false;
    }

    virtual bool isNotEmptyList() {
        return false;
    }
    
    virtual bool isNumber() {
        return false;
    }
    
    virtual double checkNumber(LispE* lisp);
    virtual long checkInteger(LispE* lisp);
    
    virtual bool isDictionary() {
        return false;
    }
    
    virtual Element* extraction(LispE* lisp, List*);
    
    virtual Element* charge(LispE*,string chemin);
    
    virtual Element* join_in_list(LispE* lisp, wstring& sep);
    
    virtual Element* dictionary(LispE* lisp) {
        return this;
    }
    
    virtual void reversechoice() {}
    
    virtual bool verify() {
        return true;
    }
    
    //Atoms cannot be present in the garbage
    virtual void protecting(bool protection) {}
    
    virtual void release() {
        if (status == s_destructible)
            delete this;
    }
    
    virtual bool equalvalue(double v) {
        return false;
    }
    
    virtual bool equalvalue(wstring& v) {
        return false;
    }
    
    virtual Element* equal(LispE* lisp, Element* e);
    virtual Element* less(LispE* lisp, Element* e);
    virtual Element* lessorequal(LispE* lisp, Element* e);
    virtual Element* more(LispE* lisp, Element* e);
    virtual Element* moreorequal(LispE* lisp, Element* e);
    
    virtual Element* cadr(LispE*,Element*);
    virtual Element* car(LispE*);
    virtual Element* cdr(LispE*);
    
    virtual long argumentsize(LispE*, long sz) {
        return -1;
    }
    
    virtual long size() {
        return 0;
    }
    
    virtual long shapesize() {
        return 0;
    }
    
    virtual short label() {
        return type;
    }
    
    virtual short type_element() {
        return type;
    }
    
    virtual Element* thekeys(LispE* lisp);
    virtual Element* thevalues(LispE* lisp);
    
    virtual Element* value_on_index(LispE*, Element* idx);
    virtual Element* value_on_index(LispE*, long i);
    virtual Element* value_on_index(wstring& k, LispE* l);
    virtual Element* value_on_index(double k, LispE* l);
    
    virtual Element* index(long i) {
        return this;
    }

    virtual Element* protected_index(LispE*, Element* k);
    virtual Element* protected_index(LispE*, double k);
    virtual Element* protected_index(LispE*, long i);
    virtual Element* protected_index(LispE*, wstring&);


    virtual Element* checkkey(LispE*, Element* e);
    
    virtual void recording(string&, Element*) {}
    virtual void recording(wstring&, Element*) {}
    virtual void recording(double, Element*) {}
    
    virtual bool remove(LispE*, Element*) {
        return false;
    }
    
    virtual bool remove(double) {
        return false;
    }
    
    virtual bool remove(long) {
        return false;
    }

    virtual bool removelast() {
        return false;
    }

    virtual bool remove(wstring&) {
        return false;
    }
    
    virtual Element* reverse(LispE*, bool duplique = true);
    
    virtual Element* bit_not(LispE* l);
    virtual Element* bit_and(LispE* l, Element* e);
    virtual Element* bit_and_not(LispE* l, Element* e);
    virtual Element* bit_or(LispE* l, Element* e);
    virtual Element* bit_xor(LispE* l, Element* e);
    virtual Element* plus(LispE* l, Element* e);
    virtual Element* minus(LispE* l, Element* e);
    virtual Element* multiply(LispE* l, Element* e);
    virtual Element* divide(LispE* l, Element* e);
    virtual Element* mod(LispE* l, Element* e);
    virtual Element* power(LispE* l, Element* e);
    virtual Element* leftshift(LispE* l, Element* e);
    virtual Element* rightshift(LispE* l, Element* e);
    
};

class Error : public Element {
public:
    
    wstring message;
    
    Error(string m) : Element(t_error) {
        s_utf8_to_unicode(message, USTR(m), m.size());
    }
    
    Error(wstring m) : message(m), Element(t_error) {}
    
    Error(wstring m, uchar s) : message(m), Element(t_error,s) {}
        
    wstring asString(LispE* lisp);
    
    bool Boolean() {
        return false;
    }
    
    bool isError() {
        return true;
    }
};

class Maybe : public Element {
public:
    
    wstring message;
    
    Maybe(LispE* lisp, Element* e) : message(e->asString(lisp)), Element(t_maybe) {}
    
    bool unify(LispE* lisp, Element* value, bool record) {
        return (value->label() == t_maybe);
    }
    
    wstring asString(LispE* lisp) {
        return message;
    }
    
    Element* equal(LispE* lisp, Element* e);
};

class Atome : public Element {
public:
    wstring name;
    short atome;
    
    Atome(short a, wstring w) : name(w), atome(a), Element(t_atom) {}
    Atome(short a, uchar s, wstring w) : name(w), atome(a), Element(t_atom, s) {}
    
    wstring asString(LispE* lisp) {
        return name;
    }
    
    wstring jsonString(LispE* lisp) {
        return wjsonstring(name);
    }
    
    Element* eval(LispE* lisp);
    
    virtual short label() {
        return atome;
    }
    
    bool isPureAtom() {
        return true;
    }
    
    bool isAtom() {
        return true;
    }
    
    Element* equal(LispE* lisp, Element* e);
    
    bool Boolean() {
        return atome;
    }
    
    virtual char check_match(LispE* lisp, Element* value) {
        if (atome == value->label())
            return check_ok;
        return false;
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isExecutable(LispE* lisp);
    
};

//all atoms between t_atom to t_error are Atomtype
class Atomtype : public Atome {
public:

    Atomtype(short a, wstring w) : Atome(a, w) {}
    Atomtype(short a, uchar s, wstring w) : Atome(a, s, w) {}

    char check_match(LispE* lisp, Element* value) {
        if (atome == value->type_element())
            return check_ok;
        return false;
    }

};

class Atomnotlabel : public Atome {
public:
    
    Atomnotlabel(short a, wstring w) : Atome(a, w) {}
    Atomnotlabel(short a, uchar s, wstring w) : Atome(a, s, w) {}
    
    short label() {
        return v_null;
    }
    
};

class Operator : public Element {
public:
    wstring name;
    
    Operator(short c, wstring w) : name(w), Element(c) {}
    wstring asString(LispE* lisp) {
        return name;
    }

    wstring jsonString(LispE* lisp) {
        return wjsonstring(name);
    }

    bool isAtom() {
        return true;
    }
    
    short label() {
        return type;
    }
    
    //Notice that we return t_atom
    //This function is used in Atomtype::check_match
    //so that 'key' for instance is considered as an atom
    short type_element() {
        return t_atom;
    }

    bool isInstruction() {
        return true;
    }

    bool isOperator() {
        return true;
    }
    
    bool isExecutable(LispE*) {
        return true;
    }

};


class Instruction : public Element {
public:
    wstring name;
    
    Instruction(short c, wstring w) : name(w), Element(c) {}
    
    wstring asString(LispE* lisp) {
        return name;
    }

    wstring jsonString(LispE* lisp) {
        return wjsonstring(name);
    }

    bool isInstruction() {
        return true;
    }
    
    bool isAtom() {
        return true;
    }
    
    short type_element() {
        return t_atom;
    }

    short label() {
        return type;
    }
    
    bool isExecutable(LispE*) {
        return true;
    }
    
    Element* eval(LispE*);
};

class Return : public Element {
public:
    Element* value;
    
    Return(Element* e) : Element(l_return) {
        value = e ;
    }
    
    wstring asString(LispE* lisp) {
        return value->asString(lisp);
    }
    
    double asNumber() {
        return value->asNumber();
    }
    
    long asInteger() {
        return value->asInteger();
    }
    
    int asInt() {
        return value->asInt();
    }
    
    Element* eval(LispE* lisp) {
        return value;
    }
    
    bool isInstruction() {
        return true;
    }
    
    bool isExecutable(LispE* lisp) {
        return true;
    }

};

class Cadr : public Element {
public:
    
    wstring action;
    
    Cadr(string c) : Element(l_cadr) {
        c = c.substr(1, c.size()-2);
        for (long i = 0; i < c.size(); i++)
            action += (wchar_t)c[i];
    }
    
    Cadr(wstring c) : Element(l_cadr) {
        action = c.substr(1, c.size()-2);
    }
    
    wstring asString(LispE* lisp) {
        return(L"c" + action + L"r");
    }
    
    Element* cadr(LispE*, Element*);
    
    bool isAtom() {
        return true;
    }
    
    short type_element() {
        return t_atom;
    }

    bool isExecutable(LispE* lisp) {
        return true;
    }

    bool isInstruction() {
        return true;
    }
};

class Number : public Element {
public:
  
    double number;
    Number(double d) : Element(t_number) {
        number = d;
    }
    
    Number(double d, uchar s) : number(d), Element(t_number, s) {}
    
    bool equalvalue(double v) {
        return (v == number);
    }

    Element* quoting(Element*) {
        return this;
    }

    Element* equal(LispE* lisp, Element* e);
    Element* less(LispE* lisp, Element* e);
    Element* lessorequal(LispE* lisp, Element* e);
    Element* more(LispE* lisp, Element* e);
    Element* moreorequal(LispE* lisp, Element* e);
    
    char check_match(LispE* lisp, Element* value) {
        if (number == value->asNumber())
            return check_ok;
        return false;
    }
    
    bool unify(LispE* lisp, Element* value, bool record) {
        return (value == this || value->asNumber() == number);
    }
    
    bool isNumber() {
        return true;
    }
    
    double checkNumber(LispE* lisp) {
        return number;
    }
    
    long checkInteger(LispE* lisp) {
        return number;
    }
    
    wstring asString(LispE* lisp) {
        return convertToWString(number);
    }

    string toString(LispE* lisp) {
        return convertToString(number);
    }

    double asNumber() {
        return number;
    }
    
    long asInteger() {
        return (long)number;
    }
    
    int asInt() {
        return (int)number;
    }

    bool Boolean() {
        return (number);
    }
    
    // Numbers cannot be present in the garbage
    void protecting(bool protection) {}
    
    Element* fullcopy() {
        return new Number(number);
    }
    
    Element* copyatom(uchar s) {
        if (status < s)
            return this;
        return new Number(number);
    }

    Element* copying(bool duplicate = true) {
        if (status < s_protect || !duplicate)
            return this;
        
        return new Number(number);
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

class Constnumber : public Number {
public:
    
    Constnumber(double d) : Number(d, s_constant) {}
    
    void decrementstatus(uchar nb, bool top) {}
    void release() {}

};

class Integer : public Element {
public:
 
    long integer;
    Integer(long d) : Element(t_integer) {
        integer = d;
    }
    
    Integer(long d, uchar s) : integer(d), Element(t_integer, s) {}

    Element* quoting(Element*) {
        return this;
    }

    Element* equal(LispE* lisp, Element* e);
    Element* less(LispE* lisp, Element* e);
    Element* lessorequal(LispE* lisp, Element* e);
    Element* more(LispE* lisp, Element* e);
    Element* moreorequal(LispE* lisp, Element* e);
    
    char check_match(LispE* lisp, Element* value) {
        if (integer == value->asInteger())
            return check_ok;
        return false;
    }
    
    bool unify(LispE* lisp, Element* value, bool record) {
        return (value == this || value->asInteger() == integer);
    }
    
    // Numbers cannot be present in the garbage
    void protecting(bool protection) {}
    
    wstring asString(LispE* lisp) {
        return convertToWString(integer);
    }

    string toString(LispE* lisp) {
        return convertToString(integer);
    }

    bool isNumber() {
        return true;
    }
    
    double checkNumber(LispE* lisp) {
        return integer;
    }
    
    long checkInteger(LispE* lisp) {
        return integer;
    }

    double asNumber() {
        return integer;
    }
    
    long asInteger() {
        return integer;
    }
    
    int asInt() {
        return (int)integer;
    }

    bool Boolean() {
        return (integer);
    }
    
    Element* fullcopy() {
        return new Integer(integer);
    }

    Element* copyatom(uchar s) {
        if (status < s)
            return this;
        return new Integer(integer);
    }

    // There is a difference between the two copies
    //The first one makes a final copy
    Element* copying(bool duplicate = true) {
        if (status < s_protect || !duplicate)
            return this;
        
        return new Integer(integer);
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

class Constinteger : public Integer {
public:
    
    Constinteger(long d) : Integer(d, s_constant) {}
    
    void decrementstatus(uchar nb, bool top) {}
    void release() {}

};

class String : public Element {
public:
  
    wstring content;
    
    String(wchar_t c) : Element(t_string) {
        content = c;
    }
    String(string c) : Element(t_string) {
        s_utf8_to_unicode(content, USTR(c), c.size());
    }
    String(wstring c) : content(c), Element(t_string) {}
    String(wstring c, uchar s) :content(c), Element(t_string, s) {}
    
    bool isString() {
        return true;
    }
    
    Element* quoting(Element*) {
        return this;
    }
    
    Element* rotate(bool left);
    
    char check_match(LispE* lisp, Element* value) {
        if (content == value->asString(lisp))
            return check_ok;
        return false;
    }
    
    bool unify(LispE* lisp, Element* value, bool record) {
        return (value == this || value->asString(lisp) == content);
    }
    
    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    
    Element* insert(LispE* lisp, Element* e, long idx);
    bool equalvalue(wstring& v) {
        return (v == content);
    }

    Element* replace(LispE* lisp, long i, Element* e);
    
    Element* equal(LispE* lisp, Element* e);
    Element* less(LispE* lisp, Element* e);
    Element* lessorequal(LispE* lisp, Element* e);
    Element* more(LispE* lisp, Element* e);
    Element* moreorequal(LispE* lisp, Element* e);
    
    Element* protected_index(LispE*,long i);
    
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    Element* reverse(LispE*, bool duplique = true);

    Element* last_element(LispE* lisp);
    
    //The strings cannot be present in the garbage
    void protecting(bool protection) {}
    
    long size() {
        return content.size();
    }
    
    Element* extraction(LispE* lisp, List*);
    
    Element* car(LispE*);
    Element* cdr(LispE*);
    
    
    virtual wstring stringInList(LispE* lisp) {
        return wjsonstring(content);
    }
    
    virtual Element* copyatom(uchar s) {
        if (status < s)
            return this;
        return new String(content);
    }
    
    virtual Element* fullcopy() {
        return new String(content);
    }

    // There is a difference between the two copies
    //The first one makes a final copy
    virtual Element* copying(bool duplicate = true) {
        if (status < s_protect || !duplicate)
            return this;
        
        return new String(content);
    }
    
    virtual wstring asString(LispE* lisp) {
        return content;
    }
    
    Element* charge(LispE* lisp, string chemin) {
        std::ifstream f(chemin.c_str(),std::ios::in|std::ios::binary);
        if (f.fail()) {
            string erreur = "Unknown file: ";
            erreur += chemin;
            throw new Error(erreur);
        }
        
        string ch = "";
        string ln;
        while (!f.eof()) {
            getline(f, ln);
            ch += ln + "\n";
        }
        
        content = L"";
        s_utf8_to_unicode(content, USTR(ch), ch.size());
        return this;
    }
    
    bool Boolean() {
        return (content != L"");
    }
    
    double asNumber() {
        string code;
        s_unicode_to_utf8(code, content);
        return convertingfloathexa(code.c_str());
    }
    
    long asInteger() {
        string code;
        s_unicode_to_utf8(code, content);
        return (long)convertingfloathexa(code.c_str());
    }
    
    Element* plus(LispE* l, Element* e);
    Element* thekeys(LispE* lisp);
};

class Conststring : public String {
public:
    
    Conststring(wstring w) : String(w, s_constant) {}
    
    void decrementstatus(uchar nb, bool top) {}
    void release() {}

};


class InfiniterangeNumber : public Element {
public:
    double initial_value;
    double increment;
    double bound;
    bool infinite_loop;

    InfiniterangeNumber(double v, double i) : Element(l_list) {
        initial_value = v;
        increment = i;
        infinite_loop = true;
        bound = 0;
    }
    
    InfiniterangeNumber(double v, double i, double b) : Element(l_list) {
        initial_value = v;
        increment = i;
        infinite_loop = false;
        bound = b;
    }
    
    bool isContainer() {
        return true;
    }
    
    bool isList() {
        return true;
    }
    
    wstring asString(LispE* lisp);
    
    Element* loop(LispE* lisp, short label,  List* code);
    Element* car(LispE*);
    Element* cdr(LispE*);
};

class InfiniterangeInteger : public Element {
public:
    long initial_value;
    long increment;
    long bound;
    bool infinite_loop;
    
    InfiniterangeInteger(long v, long i) : Element(l_list) {
        initial_value = v;
        increment = i;
        infinite_loop = true;
        bound = 0;
    }
    
    InfiniterangeInteger(long v, long i, long b) : Element(l_list) {
        initial_value = v;
        increment = i;
        infinite_loop = false;
        bound = b;
    }
    
    bool isContainer() {
        return true;
    }
    
    bool isList() {
        return true;
    }
    
    wstring asString(LispE* lisp);

    
    Element* loop(LispE* lisp, short label,  List* code);
    Element* car(LispE*);
    Element* cdr(LispE*);

};


class Infinitelist : public Element {
public:
    Element* value;
    
    Infinitelist(LispE* lisp);
    
    bool isContainer() {
        return true;
    }
    
    bool isList() {
        return true;
    }
    
    wstring asString(LispE* lisp);

    void append(Element* e) {
        e->incrementstatus(status + 1, false);
        value->decrementstatus(status + 1, false);
        value = e;
    }
    
    Element* loop(LispE* lisp, short label,  List* code);
};

class Cyclelist : public Element {
public:
    Element* value;
    
    Cyclelist(LispE* lisp);
    
    bool isContainer() {
        return true;
    }
    
    bool isList() {
        return true;
    }
    
    void append(Element* e) {
        e->incrementstatus(status + 1, false);
        value->decrementstatus(status + 1, false);
        value = e;
    }
    
    wstring asString(LispE* lisp);

    Element* loop(LispE* lisp, short label,  List* code);
};


class Dictionary : public Element {
public:
    
    map<wstring, Element*> dictionary;
    Element* object;
    bool marking;
    bool usermarking;
    
    Dictionary() : Element(t_dictionary) {
        object = NULL;
        marking = false;
        usermarking =  false;
    }
    
    Dictionary(uchar s) : Element(t_dictionary, s) {
        object = NULL;
        marking = false;
        usermarking =  false;
    }
    
    bool isDictionary() {
        return true;
    }
    
    bool isContainer() {
        return true;
    }
    
    void setmark(bool v) {
        marking = v;
    }
    
    bool mark() {
        return marking;
    }

    void setusermark(bool v) {
        usermarking = v;
    }
    
    bool usermark() {
        return  usermarking;
    }

    void resetusermark() {
        if (marking)
            return;
        marking = true;
        usermarking = false;
        for (auto& a: dictionary) {
            a.second->resetusermark();
        }
        marking = false;
    }

    
    Element* loop(LispE* lisp, short label,  List* code);
    
    void flatten(LispE*, List* l);
    
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);    
    Element* checkkey(LispE* lisp, Element* e);

    Element* fullcopy() {
        if (marking)
            return object;
        
        marking = true;
        Dictionary* d = new Dictionary;
        object = d;
        Element* e;
        for (auto& a: dictionary) {
            e = a.second->fullcopy();
            d->dictionary[a.first] = e;
            e->incrementstatus(1, false);
        }
        marking = false;
        return d;
    }

    Element* copying(bool duplicate = true) {
        if (status < s_protect && !duplicate)
            return this;
        
        Dictionary* d = new Dictionary;
        Element* e;
        for (auto& a: dictionary) {
            e = a.second->copying(false);
            d->dictionary[a.first] = e;
            e->incrementstatus(1, false);
        }
        return d;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant_container(bool pair = false) {
        if (status == s_constant) {
            Dictionary* d = new Dictionary;
            Element* e;
            for (auto& a: dictionary) {
                e = a.second->copying(false);
                d->dictionary[a.first] = e;
                e->incrementstatus(1, false);
            }
        }
        return this;
    }
    
    Element* join_in_list(LispE* lisp, wstring& sep);
    
    void release() {
        if (!status) {
            if (marking)
                return;
            marking = true;
            for (auto& a: dictionary)
                a.second->decrementstatus(1, false);
            marking = false;
            delete this;
        }
    }
    
    void incrementstatus(uchar nb, bool top) {
        if (status < s_protect) {
            if (marking)
                return;
            marking = true;
            status+=nb;
            if (top) {
                for (auto& a : dictionary) {
                    a.second->incrementstatus(nb, false);
                }
            }
            marking = false;
        }
    }
    
    void decrementstatus(uchar nb, bool top) {
        if (marking)
            return;
        marking = true;

        if (status > s_destructible && status < s_protect) {
            status-=nb;
            if (top) {
                for (auto& a : dictionary)
                    a.second->decrementstatus(nb, false);
            }
        }
        
        if (!status) {
            for (auto& a : dictionary)
                a.second->decrementstatus(1, false);
            delete this;
        }
        else
            marking = false;
    }
    
    //The status is decremented without destroying the element.
    void decrementSansDelete(uchar nb) {
        if (status > s_destructible && status < s_protect) {
            if (marking)
                return;
            marking = true;

            status-=nb;
            for (auto& a : dictionary)
                a.second->decrementstatus(nb, false);
            marking = false;
        }
    }
    
    
    bool unify(LispE* lisp, Element* e, bool record) {
        if (marking)
            return (e == object);
        
        if (e == this)
            return true;
        
        if (e->type != t_dictionary || e->size() != dictionary.size())
            return false;

        marking = true;
        object = e;

        Dictionary* d = (Dictionary*)e;
        for (auto& a: dictionary) {
            try {
                if (!d->dictionary.at(a.first)->unify(lisp, a.second, record)) {
                    marking = false;
                    return false;
                }
            }
            catch(const std::out_of_range& oor) {
                marking = false;
                return false;
            }
        }
        marking = false;
        return true;
    }
     
    Element* equal(LispE* lisp, Element* e);
    
    long size() {
        return dictionary.size();
    }
    
    Element* reverse(LispE*, bool duplique = true);
    
    void protecting(bool protection) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
        
        for (auto& a: dictionary)
            a.second->protecting(protection);
    }
    
    wstring jsonString(LispE* lisp) {
        if (!dictionary.size())
            return L"{}";
                
        if (marking)
            return L"#inf";
        
        marking = true;
        wstring tampon(L"{");
        
        bool premier = true;
        for (auto& a: dictionary) {
            if (!premier) {
                tampon += L",";
            }
            else
                premier = false;
            tampon += wjsonstring(a.first);
            tampon += L":";
            tampon += a.second->jsonString(lisp);
        }
        tampon += L"}";
        marking = false;
        return tampon;
    }
    
    wstring asString(LispE* lisp) {
        long taille = dictionary.size();
        if (!taille)
            return L"{}";

        if (marking)
            return L"...";
        
        marking = true;

        wstring tampon(L"{");
        
        bool premier = true;
        for (auto& a: dictionary) {
            if (!premier) {
                tampon += L" ";
            }
            else
                premier = false;
            tampon += wjsonstring(a.first);
            tampon += L":";
            tampon += a.second->stringInList(lisp);
        }
        tampon += L"}";
        marking = false;
        return tampon;
    }
    
    bool Boolean() {
        return (dictionary.size());
    }
    
    Element* protected_index(LispE*, wstring&);

    Element* value_on_index(wstring& k, LispE* l);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void recording(string& c, Element* e) {
        wstring k;
        s_utf8_to_unicode(k, USTR(c), c.size());
        try {
            Element* a = dictionary.at(k);
            a->decrementstatus(status+1,false);
            dictionary[k] = e;
        }
        catch (const std::out_of_range& oor) {
            dictionary[k] = e;
        }
        e->incrementstatus(status+1,false);
    }
    
    void recording(wstring& k, Element* e) {
        try {
            Element* a = dictionary.at(k);
            a->decrementstatus(status+1, false);
            dictionary[k] = e;
        }
        catch (const std::out_of_range& oor) {
            dictionary[k] = e;
        }
        e->incrementstatus(status+1,false);
    }
    
    Element* replace(LispE* lisp, Element* i, Element* e) {
        wstring k = i->asString(lisp);
        recording(k, e);
        return this;
    }

    Element* thekeys(LispE* lisp);
    
    Element* thevalues(LispE* lisp);

    bool remove(LispE* lisp, Element* e) {
        wstring d =  e->asString(lisp);
        return remove(d);
    }

    bool remove(wstring& k) {
        try {
            Element* e = dictionary.at(k);
            dictionary.erase(k);
            e->decrementstatus(status+1,false);
            return true;
        }
        catch (const std::out_of_range& oor) {
            return false;
        }
    }
};

//This version of the dictionary is indexed on a number
class Dictionary_n : public Element {
public:

    unordered_map<double, Element*> dictionary;
    Element* object;
    bool marking;
    bool usermarking;
    

    Dictionary_n() : Element(t_dictionaryn) {
        object = NULL;
        marking = false;
        usermarking = false;
    }
    
    Dictionary_n(uchar s) : Element(t_dictionaryn, s) {
        object = NULL;
        marking = false;
        usermarking = false;
    }
    
    bool isDictionary() {
        return true;
    }
    
    bool isContainer() {
        return true;
    }
    
    void setmark(bool v) {
        marking = v;
    }
    
    bool mark() {
        return marking;
    }

    void setusermark(bool v) {
        usermarking = v;
    }
    
    bool usermark() {
        return  usermarking;
    }

    void resetusermark() {
        if (marking)
            return;
        marking = true;
        usermarking = false;
        for (auto& a: dictionary) {
            a.second->resetusermark();
        }
        marking = false;
    }

    void flatten(LispE*, List* l);
    
    Element* loop(LispE* lisp, short label,  List* code);
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* search_all_elements(LispE*, Element* element_value, long idx);
    Element* search_reverse(LispE*, Element* element_value, long idx);
    Element* checkkey(LispE* lisp, Element* e);
    Element* reverse(LispE*, bool duplique = true);

    Element* fullcopy() {
        if (marking)
            return object;
        
        marking = true;
        Dictionary_n* d = new Dictionary_n;
        object = d;
        Element* e;
        for (auto& a: dictionary) {
            e = a.second->fullcopy();
            d->dictionary[a.first] = e;
            e->incrementstatus(1,false);
        }
        marking = false;
        return d;
    }
    

    Element* copying(bool duplicate = true) {
        if (status < s_protect && !duplicate)
            return this;
        
        Dictionary_n* d = new Dictionary_n;
        Element* e;
        for (auto& a: dictionary) {
            e = a.second->copying(false);
            d->dictionary[a.first] = e;
            e->incrementstatus(1,false);
        }
        return d;
    }
    
    //In the case of a container for push, key and keyn
    // We must force the copy when it is a constant
    Element* duplicate_constant_container(bool pair = false) {
        if (status == s_constant) {
            Dictionary_n* d = new Dictionary_n;
            Element* e;
            for (auto& a: dictionary) {
                e = a.second->copying(false);
                d->dictionary[a.first] = e;
                e->incrementstatus(1,false);
            }
        }
        return this;
    }
    
    Element* join_in_list(LispE* lisp, wstring& sep);
    
    void release() {
        if (!status) {
            if (marking)
                return;
            marking = true;
            for (auto& a: dictionary)
                a.second->decrementstatus(1, false);
            marking = false;
            delete this;
        }
    }
    
    void incrementstatus(uchar nb, bool top) {
        if (status < s_protect) {
            if (marking)
                return;
            marking = true;
            status+=nb;
            if (top) {
                for (auto& a : dictionary) {
                    a.second->incrementstatus(nb, false);
                }
            }
            marking = false;
        }
    }
    
    void decrementstatus(uchar nb, bool top) {
        if (marking)
            return;
        marking = true;

        if (status > s_destructible && status < s_protect) {
            status-=nb;
            if (top) {
                for (auto& a : dictionary)
                    a.second->decrementstatus(nb, false);
            }
        }
        
        if (!status) {
            for (auto& a : dictionary)
                a.second->decrementstatus(1, false);
            delete this;
        }
        else
            marking = false;
    }
    
    //The status is decremented without destroying the element.
    void decrementSansDelete(uchar nb) {
        if (status > s_destructible && status < s_protect) {
            if (marking)
                return;
            marking = true;

            status-=nb;
            for (auto& a : dictionary)
                a.second->decrementstatus(nb, false);
            marking = false;
        }
    }
    
    bool unify(LispE* lisp, Element* e, bool record) {
        if (marking)
            return (object == e);
        
        if (e == this)
            return true;
        
        if (e->type != t_dictionaryn || e->size() != dictionary.size())
            return false;
        
        marking =  true;
        object = e;
        
        Dictionary_n* d = (Dictionary_n*)e;
        for (auto& a: dictionary) {
            try {
                if (!d->dictionary.at(a.first)->unify(lisp, a.second, record)) {
                    marking = false;
                    return false;
                }
            }
            catch(const std::out_of_range& oor) {
                marking = false;
                return false;
            }
        }
        
        marking = false;
        return true;
    }
    
    Element* equal(LispE* lisp, Element* e);
    
    long size() {
        return dictionary.size();
    }
    
    void protecting(bool protection) {
        if (protection) {
            if (status == s_constant)
                status = s_protect;
        }
        else {
            if (status == s_protect)
                status = s_destructible;
        }
        
        for (auto& a: dictionary)
            a.second->protecting(protection);
    }
    
    
    wstring jsonString(LispE* lisp) {
        if (!dictionary.size())
            return L"{}";
                
        if (marking)
            return L"#inf";
        
        marking = true;

        wstring tampon;
        tampon += L"{";
        
        bool premier = true;
        for (auto& a: dictionary) {
            if (!premier) {
                tampon += L",";
            }
            else
                premier = false;
            
            tampon += convertToWString(a.first);
            tampon += L":";
            tampon += a.second->jsonString(lisp);
        }
        tampon += L"}";
        marking = false;
        return tampon;
    }
    
    
    wstring asString(LispE* lisp) {
        long taille = dictionary.size();
        if (!taille)
            return L"{}";

        if (marking)
            return L"...";
        
        marking = true;

        wstring tampon;
        tampon += L"{";
        
        bool premier = true;
        for (auto& a: dictionary) {
            if (!premier) {
                tampon += L" ";
            }
            else
                premier = false;
            tampon += convertToWString(a.first);
            tampon += L":";
            tampon += a.second->stringInList(lisp);
        }
        tampon += L"}";
        marking = false;
        return tampon;
    }
    
    bool Boolean() {
        return (dictionary.size());
    }
    
    Element* protected_index(LispE*, double k);

    Element* value_on_index(double k, LispE* l);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    void recording(double  k, Element* e) {
        try {
            Element* a = dictionary.at(k);
            a->decrementstatus(status+1, false);
            dictionary[k] = e;
        }
        catch (const std::out_of_range& oor) {
            dictionary[k] = e;
        }
        e->incrementstatus(status+1,false);
    }
    
    Element* replace(LispE* lisp, Element* i, Element* e) {
        recording(i->asNumber(), e);
        return e;
    }
    
    Element* thekeys(LispE* lisp);
    
    Element* thevalues(LispE* lisp);
    
    bool remove(LispE*, Element* e) {
        double d =  e->asNumber();
        return remove(d);
    }

    bool remove(double d) {
        try {
            Element* e = dictionary.at(d);
            dictionary.erase(d);
            e->decrementstatus(status+1,false);
            return true;
        }
        catch (const std::out_of_range& oor) {
            return false;
        }
    }
    
    //bool traverse(LispE*, Dictionary_as_list*);
};

// A temporary structure to read a dictionary
class Dictionary_as_list : public Element {
public:
    vector<Element*> keyvalues;
    vector<Element*> valuevalues;
    uint64_t keyvalue;
    short mxkeyvalue;
    bool purekeys;
    bool choice;
    bool select;
    
    Dictionary_as_list() : Element(t_dictionary) {
        choice = true;
        purekeys = true;
        select = true;
        
        //To the bits are set to zeo
        keyvalue = 0;
        mxkeyvalue = 0;
    }
    
    short label() {
        return t_dictionary;
    }

    wstring asString(LispE* lisp) {
        wstring s = L"{";
        for (long i = 0; i < keyvalues.size(); i++) {
            if (i)
                s += L" ";
            if (keyvalues[i]->label() == v_null)
                s += L"_";
            else
                s += keyvalues[i]->asString(lisp);
            s += L":";
            s += valuevalues[i]->stringInList(lisp);
        }
        s += L"}";
        return s;
    }
    
    void append(LispE* lisp, wstring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);

    void append(Element* e) {
        if (choice) {
            keyvalues.push_back(e);
            if (type == t_dictionary) {
                //Initial, type prend le type de 'e'
                if (e->isNumber())
                    type = t_number;
                else
                    type = t_string;
            }
            else {
                //If the elements are mangled, then it is a dictionary indexed on strings
                if (type == t_number && !e->isNumber()) {
                    type = t_string;
                }
            }
        }
        else {
            valuevalues.push_back(e);
            choice = true;
        }
        select = 1 - select;
    }
    
    void reversechoice() {
        choice = 1 - choice;
    }
    
    void push(Element* e) {
        if (select) {
            if (!e->isString() && !e->isNumber())
                purekeys = false;
            else {
                //We record its position in bit vector
                mxkeyvalue = keyvalues.size();
                keyvalue |= ((uint64_t)1 << mxkeyvalue++);
            }
            keyvalues.push_back(e);
        }
        else
            valuevalues.push_back(e);
        select = 1 - select;
    }
    
    bool verify() {
        if (keyvalue) {
            //Then it should be a sequence of 1 from bit 0
            return ((((uint64_t)1 << mxkeyvalue) - 1) == keyvalue);
        }
        return select;
    }
    
    Element* dictionary(LispE* lisp);
    
    bool isDictionary() {
        return true;
    }
    
    bool unify(LispE* lisp, Element* e, bool record);
    bool traverse(LispE*, Dictionary*, vector<wstring>& keys, vector<long>& consummed, long di, long i, bool record);
    bool traverse(LispE*, Dictionary_n*, vector<double>& keys, vector<long>& consummed, long di, long i, bool record);

    void release() {
        if (!status) {
            for (long i = 0; i < keyvalues.size(); i++) {
                keyvalues[i]->release();
            }
            for (long i = 0; i < valuevalues.size(); i++) {
                valuevalues[i]->release();
            }
        }
    }
};

class Dictionary_as_buffer : public Element {
public:
    Dictionary* dico;
    wstring key;
    bool choice;
    
    Dictionary_as_buffer() : Element(t_dictionary) {
        dico = new Dictionary;
        choice = true;
    }
    
    bool tobegarbaged() {
        return !choice;
    }
    
    void reversechoice() {
        choice = 1 - choice;
    }

    bool verify() {
        return choice;
    }
    
    void append(LispE* lisp, wstring& k);
    void append(LispE* lisp, double v);
    void append(LispE* lisp, long v);

    void append(Element* e) {
        if (choice) {
            if (!e->isNumber() && !e->isString())
                throw new Error("Error: a key should be a string or a number");
            else
                if (key != L"") {
                    wstring msg = L"Error: missing value for key:'";
                    msg += key;
                    msg += L"'";
                    throw new Error(msg);
                }
            key = e->asString(NULL);
            e->release();
        }
        else {
            dico->dictionary[key] = e;
            e->incrementstatus(1, false);
            key = L"";
            reversechoice();
        }
    }

    bool isDictionary() {
        return true;
    }
    
    Element* dictionary(LispE*) {
        return dico;
    }
    
    void release() {
        dico->release();
    }
    
};
#endif /* elements_h */
