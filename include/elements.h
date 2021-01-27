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

typedef enum {
    //Default values
    v_null, v_emptylist, v_emptyatom, v_true,
    
    //Default types
    t_emptystring, t_operator, t_atom, t_number, t_integer, t_string, t_list,
    t_dictionary, t_dictionaryn, t_data, t_maybe, t_pair,  t_error,
    
    l_set_max_stack_size,
    
    //Default Lisp instructions
    l_converttonumber, l_converttostring, l_converttointeger, l_converttoatom, 
    
    l_quote,
    
    //threads
    l_lock, l_waiton, l_trigger, l_threadstore, l_threadretrieve, l_threadclear,
    
    //Recording in the stack or in memory
    l_sleep, l_wait,
    l_lambda, l_defun, l_dethread, l_deflib, l_defpat, l_defmacro, l_lib,
    l_label, l_setq, l_setg, l_block,
    l_if, l_ife,  l_ncheck, l_check, l_cond, 
    l_catch, l_throw, l_maybe, l_terminal,
    
    //Check values
    l_atomp, l_numberp, l_consp, l_zerop, l_nullp, l_stringp, l_trace, l_flip, l_select,
    
    //Numerical operations
    l_plus, l_minus, l_multiply, l_power,
    l_leftshift, l_rightshift, l_bitand, l_bitor, l_bitxor,
    //+11 = l_opequal
    l_plusequal, l_minusequal, l_multiplyequal,  l_powerequal,
    l_leftshiftequal, l_rightshiftequal, l_bitandequal, l_bitorequal, l_bitxorequal,
    l_divide, l_mod, l_divideequal,l_modequal,
    l_sum, l_product,
    
    //Comparisons
    l_equal , l_different, l_lower, l_greater, l_lowerorequal,l_greaterorequal, l_max, l_min,
    l_size, l_use, l_at_index, l_extract, l_in, l_search, l_revertsearch, l_searchall, l_car, l_cdr, l_cadr, l_last,
    l_fread, l_fwrite, l_fappend,
    l_and, l_or, l_xor, l_not, l_eq, l_neq,
    
    //mutable operations
    l_key, l_keyn, l_keys, l_values, l_pop, l_list, l_cons, l_nconc, l_push, l_insert, l_unique,
    
    //Display values
    l_print, l_println, l_printerr, l_printerrln, l_prettify, l_bodies,
    
    l_self, l_while, l_eval, l_mark, l_resetmark, l_loop, l_loopcount, l_range, l_irange, l_atoms, l_atomise, l_join, l_sort,
    l_load, l_input, l_getchar, l_pipe, l_type,  l_return, l_break, l_reverse,
    l_apply, l_mapping, l_checking, l_folding,
    l_composenot, l_data, l_compose, l_map, l_filter, l_take, l_repeat, l_cycle, l_replicate, l_drop, l_takewhile, l_dropwhile,
    l_foldl, l_scanl, l_foldr, l_scanr, l_foldl1, l_scanl1, l_foldr1, l_scanr1,
    l_zip, l_zipwith,
    l_link,
    c_opening, c_closing, c_opening_bracket, c_closing_bracket, c_opening_brace, c_closing_brace, c_colon,
    e_error_brace, e_error_bracket, e_error_parenthesis, e_error_string, e_no_error,
    l_final
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

#define booleans_ lisp->delegation->_BOOLEANS

#define separator_ lisp->delegation->_LISTSEPARATOR

#define error_ lisp->delegation->_ERROR
#define break_ lisp->delegation->_BREAK

#define check_mismatch -2
#define check_ok -1

class List;

class Element {
public:
    /* Methods in Elements
     //This method transforms a Boolean value (true or false) into a LispE value (_TRUE or _NULL)
     Element* Boolean(LispE* lisp);
     
     //This is a method, which is used to display a function as indented
     string prettify(LispE* lisp);
     
     //Used to detect if the size of the parameters in a function matches the arguments
     virtual bool argumentsize(long sz);
     
     //return true or false
     virtual bool Boolean();
     
     //Check the nature of an element
     virtual bool isAtom();
     virtual bool isContainer();
     virtual bool isDictionary();
     virtual bool isError();
     virtual bool isOperator();
     virtual bool isInstruction();
     virtual bool isList();
     virtual bool isNumber();
     virtual bool isString();
     
     //Method to remove values from a dictionary or a list
     virtual bool remove(double);
     virtual bool remove(long);
     virtual bool remove(wstring&);
     
     //Check if two structures can unify or be equal. In the case of pattern functions, record is true as a value
     //might be recorded with a label in the stack
     virtual bool unify(LispE* lisp, Element* value, bool record);
     
     //used only when parsing dictionary in code, check if values and keys are in the same number
     //see Dictionary_as_list below
     virtual bool verify();
     
     //check if a data structure element matches its description
     virtual char check_match(LispE* lisp, Element* value);
     
     //Return the element as a Number
     virtual double asNumber();
     
     //bitwise operations
     virtual Element* bit_and(LispE* l, Element* e);
     virtual Element* bit_or(LispE* l, Element* e);
     virtual Element* bit_xor(LispE* l, Element* e);
     
     //cadr applies multiple extraction combining car and cdr
     virtual Element* cadr(LispE*,Element*);
     //returns the first element of a list or a string
     virtual Element* car(LispE*);
     //returns the remainder of a list
     virtual Element* cdr(LispE*);
     
     //loads a file in a String variable
     virtual Element* charge(LispE*,string chemin);
     
     //composing is called at compile time: it composes functions such as map, filter, take etc..
     //into one single loop to enable lazy evaluation
     virtual Element* composing(LispE*);
     
     //duplicating an element. If duplicate is false, returns the element as such
     //unless it is a container. In this case, the container is systematicaly duplicated
     virtual Element* copying(bool duplicate = true);
     
     
     //Returns the dictionary corresponding to the keys/values gathered in a Dictionary_as_list
     virtual Element* dictionary(LispE* lisp);
     
     // Arithmetic division
     virtual Element* divide(LispE* l, Element* e);
     
     //Constant containers must be duplicated before being stored in the stack, in another container or in
     //a function call... This is important in order to make these containers immutable.
     virtual Element* duplicate_constant_container();
     
     //Is called by eq. Does not check recursivly container equality...
     virtual Element* equal(LispE* lisp, Element* e);
     
     //The eval method. By default this method is the identity.
     //For other object, it might be different. For instance, for a List, it considers this list to
     //be an instruction and it will evaluate it
     virtual Element* eval(LispE*);
     
     //Method to extract sub-strings or sub_list with indexes.
     //In the case of strings, these indexes can also be strings.
     virtual Element* extraction(LispE* lisp, List*);
     
     //Returns the element at position 'i' in a list, as such, without duplicating it
     virtual Element* index(long i);
     
     //Inserts the element 'e' in a list at position 'idx'
     virtual Element* insert(LispE*, Element* e, long idx);
     
     //Reverse a list or a string
     virtual Element* reverse(LispE*, bool duplique = true);
     
     //Joins the elements in a list as one string
     virtual Element* join_in_list(LispE* lisp, wstring& sep);
     
     //Returns a copy of the last element of a string or a list
     virtual Element* last_element(LispE* lisp);
     
     //Returns the last element in a List
     virtual Element* last();
     
     //Bitwise left shift
     virtual Element* leftshift(LispE* l, Element* e);
     
     //Return true if the current value is lower than 'e'
     virtual Element* less(LispE* lisp, Element* e);
     
     //Return true if the current value is lower or equal than 'e'
     virtual Element* lessorequal(LispE* lisp, Element* e);
     
     //This is a loop in a container or in string
     virtual Element* loop(LispE* lisp, short label,  List* code);
     
     //Arithmetic minus between the current element and 'e'
     virtual Element* minus(LispE* l, Element* e);
     
     //Arithmetic remainder between the current element and 'e'
     virtual Element* mod(LispE* l, Element* e);
     
     //Return true if the current value is greater than e
     virtual Element* more(LispE* lisp, Element* e);
     //Return true if the current value is greater or equal than e
     virtual Element* moreorequal(LispE* lisp, Element* e);
     
     //Arithmetic operation between the current element and 'e'
     virtual Element* multiply(LispE* l, Element* e);
     virtual Element* plus(LispE* l, Element* e);
     virtual Element* power(LispE* l, Element* e);
     virtual Element* rightshift(LispE* l, Element* e);
     
     //search all the hits of 'element_value' in a string or a list, starting at position 'idx'
     virtual Element* search_all_elements(LispE*, Element* element_value, long idx);
     
     //search the first hit of 'element_value' in a string or a list, starting at position 'idx'
     virtual Element* search_element(LispE*, Element* element_value, long idx);
     
     //search the first hit of 'element_value' in a string or a list, from position 'idx' and back
     virtual Element* search_reverse(LispE*, Element* element_value, long idx);
     
     //Returns the keys of a dictionary as a list
     virtual Element* thekeys(LispE* lisp);
     
     //Returns the values of a dictionary as a list
     virtual Element* thevalues(LispE* lisp);
     
     //These methods return a copy of the element at key k in a dictionary
     virtual Element* value_on_index(double k, LispE* l);
     virtual Element* value_on_index(wstring& k, LispE* l);

     //These methods returns a copy of an element at position idx in a list or a string
     virtual Element* value_on_index(LispE*, Element* idx);
     virtual Element* value_on_index(LispE*, long idx);
     
     //Returns the current value as an integer
     virtual long asInteger();
     
     //Returns the size of the element
     //For a container, it is its number of elements
     //For a string, it is its length
     virtual long size();
     
     //Returns the label of an instruction or an atom
     virtual short label();
     
     //Convert the content of element into a std::string
     virtual string toString(LispE* lisp);
     
     //Add an element to a list
     virtual void append(Element* e);
     
     //Add an element to a list without modifying its status
     virtual void appendraw(Element* e);
     
     //decrements the status of an element, unless it is a constant
     //and destroy it, if its status is back to 0
     virtual void decrementstatus(uchar nb, bool top);
     
     //decrements the status of an element, unless it is a constant
     //but does not destroy it when its status is 0
     virtual void decrementstatusraw(uchar nb);
     
     //increments the status of an element
     virtual void incrementstatus(uchar nb, bool top);
     
     //protect an element from being deleted, when cleaning the garbage
     //after an eval of a string.
     virtual void protecting(bool protection);
     
     //recording an element in a dictionary
     virtual void recording(double, Element*) {}
     virtual void recording(string&, Element*) {}
     virtual void recording(wstring&, Element*) {}
     
     //Delete the current element if its status is 0
     //Basically if an element is returned by an evaluation and
     //its value is no longer necessary, if its status is 0, then
     //we can discard it.
     virtual void release();
     
     //replace the element in list at position i with 'e'
     virtual Element* replace(LispE* lisp, long i, Element* e) {}
     
     //Indicates if a sub-list is a terminal element in a List
     virtual void setterminal(bool v);
     
     //returns the object as a string
     virtual wstring asString(LispE* lisp);
     
     //returns the object as a jsonString. In particular,
     //this method adds \ in front of " characters
     virtual wstring jsonString(LispE* lisp);
     
     //When a string is displayed in a container,
     //we add double quotes around.
     virtual wstring stringInList(LispE* lisp);
     */
    
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
    virtual void decrementstatusraw(uchar nb) {
        if (status > s_destructible && status < s_protect)
            status-=nb;
    }
    
    virtual Element* last_element(LispE* lisp);
    virtual Element* last() {
        return this;
    }
    
    virtual Element* unique(LispE* lisp) {
        return this;
    }
    
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

    virtual Element* fullcopy() {
        return copying(true);
    }

    virtual Element* copyatom(uchar s) {
        return this;
    }
    

    virtual Element* copying(bool duplicate = true) {
        return this;
    }
    
    //We only duplicate constant containers...
    virtual Element* duplicate_constant_container(bool pair = false) {
        return this;
    }
    
    virtual Element* search_element(LispE*, Element* element_value, long idx);
    virtual Element* search_all_elements(LispE*, Element* element_value, long idx);
    virtual Element* search_reverse(LispE*, Element* element_value, long idx);
    
    virtual char check_match(LispE* lisp, Element* value) {
        if (value == this)
            return check_ok;
        return false;
    }
    
    virtual void setmark(bool v) {}
    virtual bool mark() {return false;}
    virtual void setusermark(bool v) {}
    virtual bool usermark() {return false;}
    virtual void resetusermark() {}
    
    virtual bool unify(LispE* lisp, Element* value, bool record);
    
    virtual void setterminal(bool v = true) {}
    /*
     This copy version has two purposes:
     
     a) In the case of a nulemic value or a string, we want to be able to save it as a constant.
     in the appropriate pool. This happens especially when a value is stored in a container after an arithmetic operation.
     
     b) in the case of containers, copy only duplicates the object if it is a constant.
     This is a case that appears mainly for instructions such as push, key and keys that can locally modify an object (side effect).
     but must not modify a value stored in the garbage collector from the compilation.
     */
    
    
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
    
    virtual Element* rawdictionary(LispE* lisp) {
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
    
    virtual short label() {
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
    
    virtual Element* bit_and(LispE* l, Element* e);
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

class Atom : public Element {
public:
    
    short atome;
    
    Atom(short a) : atome(a), Element(t_atom) {}
    Atom(short a, uchar s) : atome(a), Element(t_atom, s) {}
    wstring asString(LispE* lisp);
    wstring jsonString(LispE* lisp);
    
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
    
    char check_match(LispE* lisp, Element* value) {
        if (atome == value->label())
            return check_ok;
        return false;
    }
    
    bool unify(LispE* lisp, Element* value, bool record);
    bool isExecutable(LispE* lisp);
    
};

class Atomnotlabel : public Atom {
public:
    
    Atomnotlabel(short a) : Atom(a) {}
    Atomnotlabel(short a, uchar s) : Atom(a, s) {}
    
    short label() {
        return v_null;
    }
    
};

class Operator : public Element {
public:
    
    Operator(short c) : Element(c) {}
    wstring asString(LispE* lisp);
    bool isAtom() {
        return true;
    }
    short label() {
        return type;
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
    
    Instruction(short c) : Element(c) {}
    
    wstring asString(LispE* lisp);
    
    bool isInstruction() {
        return true;
    }
    
    bool isAtom() {
        return true;
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
    
    bool isExecutable(LispE* lisp) {
        return true;
    }

    bool isInstruction() {
        return true;
    }
};

class Number : public Element {
public:
    /* Methods in Number
     bool Boolean();
     bool isNumber();
     bool unify(LispE* lisp, Element* value, bool record);
     char check_match(LispE* lisp, Element* value);
     double asNumber();
     Element* bit_and(LispE* l, Element* e);
     Element* bit_or(LispE* l, Element* e);
     Element* bit_xor(LispE* l, Element* e);
     Element* copying(bool duplicate = true);
     Element* divide(LispE* l, Element* e);
     Element* equal(LispE* lisp, Element* e);
     Element* leftshift(LispE* l, Element* e);
     Element* less(LispE* lisp, Element* e);
     Element* lessorequal(LispE* lisp, Element* e);
     Element* minus(LispE* l, Element* e);
     Element* mod(LispE* l, Element* e);
     Element* more(LispE* lisp, Element* e);
     Element* moreorequal(LispE* lisp, Element* e);
     Element* multiply(LispE* l, Element* e);
     Element* plus(LispE* l, Element* e);
     Element* power(LispE* l, Element* e);
     Element* rightshift(LispE* l, Element* e);
     long asInteger();
     void protecting(bool protection);
     wstring asString(LispE* lisp);
     */
    
    double number;
    Number(double d) : Element(t_number) {
        number = d;
    }
    
    Number(double d, uchar s) : number(d), Element(t_number, s) {}
    
    bool equalvalue(double v) {
        return (v == number);
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
        return std::to_wstring(number);
    }

    string toString(LispE* lisp) {
        return std::to_string(number);
    }

    double asNumber() {
        return number;
    }
    
    long asInteger() {
        return number;
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
    
    Element* bit_and(LispE* l, Element* e);
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

class Integer : public Element {
public:
    /* Methods in Integer
     bool Boolean();
     bool isNumber();
     bool unify(LispE* lisp, Element* value, bool record);
     char check_match(LispE* lisp, Element* value);
     double asNumber();
     Element* bit_and(LispE* l, Element* e);
     Element* bit_or(LispE* l, Element* e);
     Element* bit_xor(LispE* l, Element* e);
     Element* copying(bool duplicate = true);
     Element* divide(LispE* l, Element* e);
     Element* equal(LispE* lisp, Element* e);
     Element* leftshift(LispE* l, Element* e);
     Element* less(LispE* lisp, Element* e);
     Element* lessorequal(LispE* lisp, Element* e);
     Element* minus(LispE* l, Element* e);
     Element* mod(LispE* l, Element* e);
     Element* more(LispE* lisp, Element* e);
     Element* moreorequal(LispE* lisp, Element* e);
     Element* multiply(LispE* l, Element* e);
     Element* plus(LispE* l, Element* e);
     Element* power(LispE* l, Element* e);
     Element* rightshift(LispE* l, Element* e);
     long asInteger();
     void protecting(bool protection);
     wstring asString(LispE* lisp);
     */
    
    long integer;
    Integer(long d) : Element(t_integer) {
        integer = d;
    }
    
    Integer(long d, uchar s) : integer(d), Element(t_integer, s) {}
    
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
        return std::to_wstring(integer);
    }

    string toString(LispE* lisp) {
        return std::to_string(integer);
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
    
    
    Element* bit_and(LispE* l, Element* e);
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

class String : public Element {
public:
    /* Methods in String
     bool Boolean();
     bool isString();
     bool unify(LispE* lisp, Element* value, bool record);
     char check_match(LispE* lisp, Element* value);
     double asNumber();
     Element* car(LispE*);
     Element* cdr(LispE*);
     Element* charge(LispE* lisp, string chemin);
     Element* copying(bool duplicate = true);
     Element* equal(LispE* lisp, Element* e);
     Element* extraction(LispE* lisp, List*);
     Element* insert(LispE* lisp, Element* e, long idx);
     Element* reverse(LispE*, bool duplique = true);
     Element* last_element(LispE* lisp);
     Element* less(LispE* lisp, Element* e);
     Element* lessorequal(LispE* lisp, Element* e);
     Element* loop(LispE* lisp, short label,  List* code);
     Element* more(LispE* lisp, Element* e);
     Element* moreorequal(LispE* lisp, Element* e);
     Element* plus(LispE* l, Element* e);
     Element* search_all_elements(LispE*, Element* element_value, long idx);
     Element* search_element(LispE*, Element* element_value, long idx);
     Element* search_reverse(LispE*, Element* element_value, long idx);
     Element* value_on_index(LispE*, Element* idx);
     Element* value_on_index(LispE*, long i);
     long asInteger();
     long size();
     void protecting(bool protection) {}
     wstring asString(LispE* lisp);
     wstring stringInList(LispE* lisp);
     
     */
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
    
    
    wstring stringInList(LispE* lisp) {
        return wjsonstring(content);
    }
    
    Element* copyatom(uchar s) {
        if (status < s)
            return this;
        return new String(content);
    }
    
    Element* fullcopy() {
        return new String(content);
    }

    // There is a difference between the two copies
    //The first one makes a final copy
    Element* copying(bool duplicate = true) {
        if (status < s_protect || !duplicate)
            return this;
        
        return new String(content);
    }
    
    wstring asString(LispE* lisp) {
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
};


class Infiniterange : public Element {
public:
    double initial_value;
    double increment;
    
    Infiniterange(double v, double i) : Element(l_list) {
        initial_value = v;
        increment = i;
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
    
    InfiniterangeInteger(long v, long i) : Element(l_list) {
        initial_value = v;
        increment = i;
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
    
    /* Method in Dictionary
     bool Boolean();
     bool isContainer();
     bool isDictionary();
     bool remove(wstring& k);
     bool unify(LispE* lisp, Element* e, bool record);
     Element* copying(bool duplicate = true);
     Element* duplicate_constant_container();
     Element* equal(LispE* lisp, Element* e);
     Element* join_in_list(LispE* lisp, wstring& sep);
     Element* loop(LispE* lisp, short label,  List* code);
     Element* search_all_elements(LispE*, Element* element_value, long idx);
     Element* search_element(LispE*, Element* element_value, long idx);
     Element* search_reverse(LispE*, Element* element_value, long idx);
     Element* thekeys(LispE* lisp);
     Element* thevalues(LispE* lisp);
     Element* value_on_index(LispE*, Element* idx);
     Element* value_on_index(wstring& k, LispE* l);
     long size();
     void decrementstatus(uchar nb, bool top);
     void decrementstatusraw(uchar nb);
     void incrementstatus(uchar nb, bool top);
     void protecting(bool protection);
     void recording(string& c, Element* e);
     void recording(wstring& k, Element* e);
     void release();
     wstring asString(LispE* lisp);
     wstring jsonString(LispE* lisp);
     
     */
    
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
    void decrementstatusraw(uchar nb) {
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
    /* Methods in Dictionary_n
     bool Boolean();
     bool isContainer();
     bool isDictionary();
     bool remove(wstring& k);
     bool unify(LispE* lisp, Element* e, bool record);
     Element* copying(bool duplicate = true);
     Element* duplicate_constant_container();
     Element* equal(LispE* lisp, Element* e);
     Element* join_in_list(LispE* lisp, wstring& sep);
     Element* loop(LispE* lisp, short label,  List* code);
     Element* search_all_elements(LispE*, Element* element_value, long idx);
     Element* search_element(LispE*, Element* element_value, long idx);
     Element* search_reverse(LispE*, Element* element_value, long idx);
     Element* thekeys(LispE* lisp);
     Element* thevalues(LispE* lisp);
     Element* value_on_index(LispE*, Element* idx);
     Element* value_on_index(wstring& k, LispE* l);
     long size();
     void decrementstatus(uchar nb, bool top);
     void decrementstatusraw(uchar nb);
     void incrementstatus(uchar nb, bool top);
     void protecting(bool protection);
     void recording(string& c, Element* e);
     void recording(wstring& k, Element* e);
     void release();
     wstring asString(LispE* lisp);
     wstring jsonString(LispE* lisp);
     */
    
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
    void decrementstatusraw(uchar nb) {
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
            
            tampon += std::to_wstring(a.first);
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
            tampon += std::to_wstring(a.first);
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
    
    void append(Element* e) {
        if (choice) {
            keyvalues.push_back(e);
            if (type == t_dictionary) {
                //Initial, type prend le type de 'e'
                type = e->type;
            }
            else {
                //If the elements are mangled, then it is a dictionary indexed on strings
                if (e->type != type)
                    type = t_string;
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
    Element* rawdictionary(LispE* lisp);
    
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

#endif /* elements_h */
