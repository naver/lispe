#ifndef minilisp_h
#define minilisp_h
#include "tokens.h"
#include "tokenize.h"

//-------------------------------------------------------------------------------------
typedef enum {
    next_action = 0, first_action = 1, one_action = 2, parenthetic_action = 3, map_key = 4, map_value = 5, error_action = 6
} compile_action;


typedef enum
{
    v_nil,
    v_atom,
    v_error,
    v_string,
    v_list,
    v_map,
    v_float,
    v_integer,
    v_unix,
    v_boolean,
    v_break,

    l_plus,
    l_minus,
    l_multiply,
    l_divide,
    l_mod,
    l_size,
    l_car,
    l_cdr,
    l_cons,
    l_split,
    l_at,
    l_list,
    l_command,
    l_setq,
    l_quote,
    l_print,
    l_if,
    l_cond,
    l_block,
    l_map,
    l_mapcar,
    l_key,
    l_defun,
    l_equal,
    l_different,
    l_not,
    l_eq,
    l_neq,
    l_inf,
    l_sup,
    l_infeq,
    l_supeq,
    l_loop,
    l_while,
    l_lambda,
    l_eval,
    l_float,
    l_integer,
    l_string,
    l_consp,
    l_zerop,
    l_nullp,
    l_stringp,
    l_numberp,
    l_stats,
    l_push,
    l_pop,
    l_type,
    l_sub,
    l_apply,
    l_replace,
    l_final
} lisp_instruction_code;

//-------------------------------------------------------------------------------------
// Some specific values
// Note that nil cannot be stored in the garbage;
class lisp_element;
class lisp_list;
class lisp_string;
class lisp_error;
class lisp_atom;
class lisp_boolean;
class lisp_mini;

lisp_mini *create_mini_lisp_instance();
string execute_unix_command(string wcmd);

extern lisp_element *lisp_nil;
extern lisp_element *lisp_true;

extern lisp_string *lisp_emptystring;
extern lisp_atom* lisp_break;

extern lisp_error *lisperror;
extern lisp_error *lispargnbserror;
extern lisp_error *lisptokenizeerror;
extern lisp_error *lisperrorrange;
extern lisp_error *lisperrordivided0;
extern lisp_error *lispunknownatom;
extern lisp_error *lispunknownmethod;
extern lisp_error *lispstackerror;
extern lisp_error *lisplambdaerror;
extern lisp_error *lisp_end;

extern std::map<std::string, uint16_t> code_dictionary;
extern std::map<uint16_t, std::string> string_dictionary;
extern std::map<uint16_t, lisp_element*> instructions_dictionary;

uint16_t get_code(string &w);

const uint16_t s_constant = 65535; //all bits set to 1
const uint16_t s_protect = 32768; //bit 15 set to 1
const uint16_t s_protected = 16384; //bit 14 set to 1
const uint16_t s_check = 49152; //bit 15 and 14 set to 1

class lisp_element
{
public:
    uint16_t code;
    uint16_t status;
#ifdef DEBUGGER
    long idx;
#endif

    lisp_element(uint16_t cst, uint16_t c) : status(cst), code(c) {
#ifdef DEBUGGER 
        idx = 0;
#endif        
    }
    lisp_element(vector<lisp_element*>& storage, uint16_t c);
    lisp_element(uint16_t c);
    ~lisp_element();

    inline uint16_t s_status() 
    {
        return ((status & s_check) == 0);
    }

    void mark() {
        status += s_status();
    }

    void demark()
    {
        status -= (status && s_status());
    }

    virtual void unmark()
    {
        status -= (status && s_status());
        if (!status)
        {
            delete this;
        }
    }

    virtual void clear() {}
    virtual lisp_element* loop(lisp_mini* lisp, lisp_list* code, lisp_element* variable) {
        return lisp_nil;
    }

    virtual lisp_element* mapcar(lisp_mini* lisp, lisp_element* oper) {
        return lisp_nil;
    }

    virtual void remove()
    {
        if (status == s_constant)
            return;

        status &= ~s_protected;
        unmark();
    }

    virtual void protect()
    {
        status |= s_protect;
    }

    virtual void unprotect()
    {
        if (status != s_constant)
            status &= ~s_protect;
    }


    virtual lisp_element *release()
    {
        if (!status)
        {
            delete this;
            return lisp_nil;
        }
        return this;
    }

    virtual long size()
    {
        return 0;
    }

    virtual lisp_element* replace(lisp_element* a, lisp_element* v);
    virtual lisp_element* sub(double b, double e);
    virtual lisp_element *append(lisp_element *e);
    virtual lisp_element *append(string&, lisp_element *e);
    compile_action store(string& , lisp_element* e, compile_action a);
    virtual void pop(lisp_element*);
    virtual lisp_element *push_first(lisp_element *e);
    virtual lisp_element *car();
    virtual lisp_element *cdr();
    virtual lisp_element *at_position(lisp_element* i) {
        return lisp_nil;
    }
    virtual lisp_element *at(long i) {
        return lisp_nil;
    }
    virtual lisp_element *command();
    virtual lisp_element *split(lisp_element*) {
        return lisp_nil;
    }
    virtual lisp_element *plus(lisp_element *v);
    virtual lisp_element *minus(lisp_element *v);
    virtual lisp_element *multiply(lisp_element *v);
    virtual lisp_element *divide(lisp_element *v);
    virtual lisp_element *mod(lisp_element *v);
    virtual bool inf(lisp_element *v) {
        return lisp_nil;
    }
    virtual bool sup(lisp_element *v) {
        return lisp_nil;
    }
    virtual bool infeq(lisp_element *v) {
        return lisp_nil;
    }
    virtual bool supeq(lisp_element *v) {
        return lisp_nil;
    }
    virtual lisp_element* cons_apply(lisp_element* op);

    virtual bool eq(lisp_element *v)
    {
        return v->code == code;
    }

    virtual bool equal(lisp_element *v)
    {
        return eq(v);
    }

    virtual bool neq(lisp_element *v)
    {
        return v->code != code;
    }

    virtual double doublevalue()
    {
        return 0;
    }

    virtual long longvalue()
    {
        return 0;
    }

    virtual void stringvalue(string &v)
    {
        v += string_dictionary[code];
    }

    virtual void asstring(std::stringstream &os)
    {
        os << string_dictionary[code];
    }

    virtual bool is_instruction()
    {
        return false;
    }

    virtual bool is_number()
    {
        return false;
    }

    virtual bool is_list()
    {
        return false;
    }

    virtual bool is_map()
    {
        return false;
    }

    virtual lisp_element *eval(lisp_mini *)
    {
        return this;
    }

    virtual lisp_element *clone(bool cst)
    {
        return this;
    }

    virtual bool is_atom()
    {
        return false;
    }

    virtual bool boolean()
    {
        return false;
    }
};
//-------------------------------------------------------------------------------------
class lisp_error : public lisp_element
{
public:
    string message;

    lisp_error(string m) : message(m), lisp_element(s_constant, v_error) {}
    lisp_error(lisp_element* code, string m) : lisp_element(v_error) {
        message = m;
        message += " in '";
        code->stringvalue(message);
        message += "'";
    }

    lisp_element *eval(lisp_mini *)
    {
        throw this;
    }

    void stringvalue(string &v)
    {
        v += message;
    }

    void asstring(std::stringstream &os)
    {
        os << message;
    }
};
//-------------------------------------------------------------------------------------
class lisp_boolean : public lisp_element
{
public:
    bool value;

    lisp_boolean(bool v) : value(v), lisp_element(s_constant, v_boolean) {}

    bool boolean()
    {
        return value;
    }

    void stringvalue(string &v)
    {
        if (value)
            v += "true";
        else
            v += "false";
    }

    void asstring(std::stringstream &os)
    {
        if (value)
            os << "true";
        else
            os << "false";
    }

    bool eq(lisp_element *v)
    {
        return v->boolean() == value;
    }

    bool neq(lisp_element *v)
    {
        return v->boolean() != value;
    }
};
//-------------------------------------------------------------------------------------
class lisp_atom : public lisp_element
{
public:
    lisp_atom(uint16_t c) : lisp_element(s_constant, c) {}

    lisp_element *eval(lisp_mini *);

    bool is_atom()
    {
        return true;
    }

    bool eq(lisp_element *v) {
        return (v == this);
    }

    bool neq(lisp_element *v) {
        return (v != this);
    }

};

class lisp_instruction : public lisp_atom
{
public:
    lisp_instruction(uint16_t c) : lisp_atom(c) {}

    bool is_instruction()
    {
        return true;
    }
};

//-------------------------------------------------------------------------------------
class lisp_integer : public lisp_element
{
public:
    long value;

    lisp_integer(uint16_t c, long v) : value(v), lisp_element(c, v_integer) {}
    lisp_integer(vector<lisp_element*>& storage, long v) : value(v), lisp_element(storage, v_integer) {}    
    lisp_integer(long v) : value(v), lisp_element(v_integer) {}

    void clear() {
        value = 0;
    }

    lisp_element *plus(lisp_element *v)
    {
        value += v->longvalue();
        return this;
    }

    lisp_element *clone(bool cst)
    {
        if (!status || (cst && s_status()))
            return this;
        return new lisp_integer(value);
    }

    lisp_element *minus(lisp_element *v)
    {
        value -= v->longvalue();
        return this;
    }

    lisp_element *multiply(lisp_element *v)
    {
        value *= v->longvalue();
        return this;
    }

    lisp_element *divide(lisp_element *v)
    {
        long d = v->longvalue();
        if (d == 0)
            return lisperrordivided0->eval(NULL);
        value /= d;
        return this;
    }

    lisp_element *mod(lisp_element *v)
    {
        long d = v->longvalue();
        if (d == 0)
            return lisperrordivided0->eval(NULL);
        d = value % d;
        value = d;
        return this;
    }

    bool is_number()
    {
        return true;
    }

    bool boolean()
    {
        return value != 0;
    }

    double doublevalue()
    {
        return value;
    }

    long longvalue()
    {
        return value;
    }

    void stringvalue(string &v)
    {
        std::stringstream os;
        os << value;
        v += os.str();
    }

    void asstring(std::stringstream &os)
    {
        os << value;
    }

    bool eq(lisp_element *v)
    {
        return v->longvalue() == value;
    }

    bool neq(lisp_element *v)
    {
        return v->longvalue() == value;
    }

    bool inf(lisp_element *v)
    {
        return value < v->longvalue();
    }

    bool sup(lisp_element *v)
    {
        return value > v->longvalue();
    }

    bool infeq(lisp_element *v)
    {
        return value <= v->longvalue();
    }

    bool supeq(lisp_element *v)
    {
        return value >= v->longvalue();
    }
};
//-------------------------------------------------------------------------------------
class lisp_float : public lisp_element
{
public:
    double value;

    lisp_float(uint16_t c, double v) : value(v), lisp_element(c, v_float) {}
    lisp_float(vector<lisp_element*>& storage, double v) : value(v), lisp_element(storage, v_float) {}    
    lisp_float(double v) : value(v), lisp_element(v_float) {}

    void clear() {
        value = 0;
    }

    lisp_element *plus(lisp_element *v)
    {
        value += v->doublevalue();
        return this;
    }

    lisp_element *clone(bool cst)
    {
        if (!status || (cst && s_status()))
            return this;
        return new lisp_float(value);
    }

    lisp_element *minus(lisp_element *v)
    {
        value -= v->doublevalue();
        return this;
    }

    lisp_element *multiply(lisp_element *v)
    {
        value *= v->doublevalue();
        return this;
    }

    lisp_element *divide(lisp_element *v)
    {
        double d = v->doublevalue();
        if (d == 0)
            return lisperrordivided0->eval(NULL);
        value /= d;
        return this;
    }

    lisp_element *mod(lisp_element *v)
    {
        long d = (long)v->doublevalue();
        if (d == 0)
            return lisperrordivided0->eval(NULL);
        d = (long)value % d;
        value = d;
        return this;
    }

    bool is_number()
    {
        return true;
    }

    bool boolean()
    {
        return value != 0;
    }

    double doublevalue()
    {
        return value;
    }

    long longvalue()
    {
        return value;
    }

    void stringvalue(string &v)
    {
        std::stringstream os;
        os << value;
        v += os.str();
    }

    void asstring(std::stringstream &os)
    {
        os << value;
    }

    bool eq(lisp_element *v)
    {
        return v->doublevalue() == value;
    }

    bool neq(lisp_element *v)
    {
        return v->doublevalue() == value;
    }

    bool inf(lisp_element *v)
    {
        return value < v->doublevalue();
    }

    bool sup(lisp_element *v)
    {
        return value > v->doublevalue();
    }

    bool infeq(lisp_element *v)
    {
        return value <= v->doublevalue();
    }

    bool supeq(lisp_element *v)
    {
        return value >= v->doublevalue();
    }
};
//-------------------------------------------------------------------------------------
class lisp_list : public lisp_element
{
public:
    vector<lisp_element *> values;

    lisp_list() : lisp_element(v_list) {}
    lisp_list(vector<lisp_element*>& storage) : lisp_element(storage, v_list) {}    
    lisp_list(uint16_t c) : lisp_element(c, v_list) {}

    bool is_list()
    {
        return true;
    }

    lisp_element* loop(lisp_mini* lisp, lisp_list* code, lisp_element* variable);
    lisp_element* mapcar(lisp_mini* lisp, lisp_element* oper);

    lisp_element* cons_apply(lisp_element* op) {
        lisp_list* l = new lisp_list();
        l->append(op);
        for (long i = 0; i < size(); i++)
            l->append(values[i]);
        return l;
    }

    virtual lisp_element *append(lisp_element *e)
    {
        e->mark();
        values.push_back(e);
        return this;
    }

    lisp_element* sub(double b, double e);
    virtual void unmark();
    virtual void remove();
    virtual void protect();
    virtual void unprotect();

    virtual lisp_element *release();
    virtual void pop(lisp_element* e)
    {
        long sz = values.size();
        long  i = sz - 1;
        if (e != lisp_nil)
            i = e->longvalue();
        if (i < 0 || i >= sz)
            lisperrorrange->eval(NULL);

        if (i == sz - 1)
        {
            values.back()->unmark();
            values.pop_back();
            return;
        }
        values[i]->unmark();
        values.erase(values.begin()+i);
    }

    virtual lisp_element *push_first(lisp_element *e)
    {
        e->mark();
        values.insert(values.begin(), e);
        return this;
    }

    virtual long size()
    {
        return values.size();
    }

    virtual bool boolean()
    {
        return values.size();
    }

    void clear() {
        for (long i = 0; i < values.size(); i++) {
            values[i]->unmark();
        }
        values.clear();
    }

    virtual lisp_element *execute_lambda(lisp_mini *, lisp_element *lmbd);
    virtual lisp_element *execute_function(lisp_mini *, lisp_element *function);

    virtual lisp_element *eval(lisp_mini *);

    virtual lisp_element *car();
    virtual lisp_element *cdr();

    virtual lisp_element *at_position(lisp_element* e)
    {
        long i = e->doublevalue();
        if (i >= 0 && i < values.size())
            return values[i];
        return lisp_nil;
    }

    virtual lisp_element *at(long i)
    {
        if (i >= 0 && i < values.size())
            return values[i];
        return lisp_nil;
    }

    virtual void stringvalue(string &v)
    {
        v += "(";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                v += " ";
            first = false;
            a->stringvalue(v);
        }
        v += ")";
    }

    virtual void asstring(std::stringstream &os)
    {
        os << "(";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                os << " ";
            first = false;
            a->asstring(os);
        }
        os << ")";
    }

    virtual lisp_element *clone(bool cst)
    {
        if (!status || (cst && s_status()))
            return this;
        
        lisp_list *l = new lisp_list();
        for (long i = 0; i < values.size(); i++)
        {
            l->append(values[i]->clone(cst));
        }
        return l;
    }

    bool equal(lisp_element *v) {
        if (v->code != v_list || v->size() != size())
            return false;
        lisp_list* val = (lisp_list*)v;
        for (long i = 0; i < values.size(); i++) {
            if (!values[i]->equal(val->values[i]))
                return false;
        }
        return true;
    }

    bool eq(lisp_element *v)
    {
        return v == this;
    }

    bool neq(lisp_element *v)
    {
        return v != this;
    }
};
//-------------------------------------------------------------------------------------
class lisp_list_nil : public lisp_list {
public:
    lisp_list_nil() : lisp_list(s_constant) {
        code = v_nil;
    }

    lisp_element *append(lisp_element *e) {
        lisp_list* l = new lisp_list();
        l->append(e);
        return l;
    }

    void unmark() {}
    void remove() {}
    void protect() {}
    void unprotect() {}
    lisp_element *release() {}

    void pop(lisp_element*) {
        lisperrorrange->eval(NULL);
    }

    lisp_element *push_first(lisp_element *e) {
        lisp_list* l = new lisp_list();
        l->append(e);
        return l;
    }

    long size()
    {
        return 0;
    }

    bool boolean()
    {
        return false;
    }

    lisp_element *execute_lambda(lisp_mini *, lisp_element *lmbd) {
        lisperror->eval(NULL);
    }

    lisp_element *execute_function(lisp_mini *, lisp_element *function) {
        lisperror->eval(NULL);
    }

    lisp_element *eval(lisp_mini *) {
        return this;
    }

    lisp_element *car() {
        return this;
    }

    lisp_element *cdr() {
        return this;
    }

    lisp_element *at_position(lisp_element* i)
    {
        return lisp_nil;
    }

    void stringvalue(string &v)
    {
        v += "()";
    }

    void asstring(std::stringstream &os)
    {
        os << "()";
    }

    lisp_element *clone(bool cst) {
        return this;
    }

};
//-------------------------------------------------------------------------------------
class lisp_map : public lisp_element
{
public:
    std::map<string, lisp_element *> values;

    lisp_map() : lisp_element(v_map) {}
    lisp_map(vector<lisp_element*>& storage) : lisp_element(storage, v_map) {}    
    lisp_map(uint16_t c) : lisp_element(c, v_map) {}

    bool is_map()
    {
        return true;
    }

    lisp_element *append(string& key, lisp_element *v)
    {
        v->mark();
        values[key] = v;
        return this;
    }

    lisp_element* loop(lisp_mini* lisp, lisp_list* code, lisp_element* variable);
    lisp_element* mapcar(lisp_mini* lisp, lisp_element* oper);

    void unmark();
    void remove();
    void protect();
    void unprotect();
    lisp_element *release();

    void pop(lisp_element* e)
    {
        string key;
        e->stringvalue(key);

        if (values.find(key) != values.end())
        {
            values[key]->unmark();
            values.erase(key);
        }
        else
            lisperrorrange->eval(NULL);
    }

    void clear() {
        for (const auto& a:values) {
            a.second->unmark();
        }
        values.clear();
    }

    long size()
    {
        return values.size();
    }

    bool boolean()
    {
        return values.size();
    }

    virtual lisp_element *at_position(lisp_element* k)
    {
        string key;
        k->stringvalue(key);
        if (values.find(key) != values.end())
            return values[key];
        return lisp_nil;
    }

    void stringvalue(string &v)
    {
        v += "{";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                v += " ";
            first = false;
            v += a.first;
            v += ":";
            a.second->stringvalue(v);
        }
        v += "}";
    }

    virtual void asstring(std::stringstream &os)
    {
        os << "{";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                os << " ";
            first = false;
            os << a.first << ":";
            a.second->asstring(os);
        }
        os << "}";
    }

    lisp_element *clone(bool cst)
    {
        if (!status || (cst && s_status()))
            return this;
        
        lisp_map *m = new lisp_map();
        lisp_element* e;
        for (const auto& a : values)
        {
            e = a.second->clone(cst);
            e->mark();
            m->values[a.first] = e;
        }
        return m;
    }

    bool equal(lisp_element *v) {
        if (v->code != v_map || v->size() != size())
            return false;
        lisp_map* val = (lisp_map*)v;
        for (const auto& a : values) {
            if (val->values.find(a.first) == val->values.end())            
                return false;

            if (!a.second->equal(val->values[a.first]))
                return false;
        }
        return true;
    }

    bool eq(lisp_element *v)
    {
        return v == this;
    }

    bool neq(lisp_element *v)
    {
        return v != this;
    }
};

//-------------------------------------------------------------------------------------
class lisp_string : public lisp_element
{
public:
    string value;

    lisp_string(uint16_t constant, string v) : value(v), lisp_element(constant, v_string) {}
    lisp_string(vector<lisp_element*>& storage, string v) : value(v), lisp_element(storage, v_string) {}    
    lisp_string(string v) : value(v), lisp_element(v_string) {}
    lisp_string(char v) : lisp_element(v_string)
    {
        value = v;
    }


    void clear() {
        value = "";
    }

    lisp_element* clone(bool cst) {
        if (!status || (cst && s_status()))
            return this;

        return new lisp_string(value);
    }

    lisp_element* loop(lisp_mini* lisp, lisp_list* code, lisp_element* variable);
    lisp_element* mapcar(lisp_mini* lisp, lisp_element* oper);

    lisp_element *at_position(lisp_element* i);
    lisp_element *at(long i);
    
    lisp_element* sub(double b, double e);
    lisp_element* car();
    lisp_element* cdr();

    void pop(lisp_element* e);
    lisp_element* replace(lisp_element* a, lisp_element* v);

    long size()
    {
        return size_c(value);
    }

    bool boolean()
    {
        return value.size();
    }

    lisp_element *plus(lisp_element *v)
    {
        string s;
        v->stringvalue(s);
        value += s;
        return this;
    }

    lisp_element *split(lisp_element* splitter);

    void stringvalue(string &v)
    {
        v += value;
    }

    void asstring(std::stringstream &os)
    {
        os << value;
    }

    bool eq(lisp_element *v)
    {
        string w;
        v->stringvalue(w);
        return w == value;
    }

    bool neq(lisp_element *v)
    {
        string w;
        v->stringvalue(w);
        return w != value;
    }

    bool inf(lisp_element *v)
    {
        string w;
        v->stringvalue(w);
        return w < value;
    }

    bool sup(lisp_element *v)
    {
        string w;
        v->stringvalue(w);
        return w > value;
    }

    bool infeq(lisp_element *v)
    {
        string w;
        v->stringvalue(w);
        return w <= value;
    }

    bool supeq(lisp_element *v)
    {
        string w;
        v->stringvalue(w);
        return w >= value;
    }
};
//-------------------------------------------------------------------------------------
class lisp_unix : public lisp_element
{
public:
    string value;

    lisp_unix(vector<lisp_element*>& storage, string v) : value(v), lisp_element(storage, v_unix) {}
    lisp_unix(uint16_t c, string v) : value(v), lisp_element(c, v_unix) {}
    lisp_unix(string v) : value(v), lisp_element(v_unix) {}

    lisp_element *command()
    {
        string result = execute_unix_command(value);
        return new lisp_string(result);
    }

    void stringvalue(string &v)
    {
        v += value;
    }

    void asstring(std::stringstream &os)
    {
        os << value;
    }
};
//-------------------------------------------------------------------------------------
class lisp_mini
{
public:
    vector<std::map<uint16_t, lisp_element *>> variables;
    std::map<uint16_t, lisp_atom*> atoms;

    Segmentingtype infos;
    double count_data;
    bool stop_execution;

    void garbage_clean();

    void stack_variables_on(std::map<uint16_t, lisp_element *> &local_vars)
    {
        variables.push_back(local_vars);
    }

    lisp_atom* get_atom(uint16_t a) {
        lisp_atom* la = atoms[a];
        if (la == NULL) {
            la = new lisp_atom(a);
            atoms[a] = la;
        }
        return la;
    }

    void stack_off(lisp_element *e)
    {
        if (variables.size() == 0)
            lispstackerror->eval(this);
        e->protect();
        for (auto &v : variables.back())
        {
            v.second->unmark();
        }
        e->unprotect();
        variables.pop_back();
    }

    void protect_variables()
    {
        for (auto &v : variables.back())
        {
            v.second->protect();
        }
    }

    void unprotect_variables()
    {
        for (auto &v : variables.back())
        {
            v.second->unprotect();
        }
    }

    void clean(vector<lisp_element*>& storage) {
        protect_variables();
        for (auto& a : storage) {
            if (((a->status) & s_protect) != s_protect)
                delete a;
        }
        unprotect_variables();
    }

    lisp_mini();

    void check_stop()
    {
        if (stop_execution)
            lisp_end->eval(this);
    }

    void stop()
    {
        stop_execution = true;
    }
    //-------------------------------------------------------------------------------------
    bool compile(lisp_element *program, vector<lisp_element*>& storage, long &pos, compile_action first);
    lisp_element *run(string code);
    void insert(uint16_t c, lisp_element *l)
    {
        lisp_element *previous = variables.back()[c];
        if (previous != NULL)
        {
            if (previous == l)
                return;
            previous->unmark();
        }
        l = l->clone(true);
        l->mark();
        variables.back()[c] = l;
    }

    ~lisp_mini()
    {
        garbage_clean();
    }
};

#endif