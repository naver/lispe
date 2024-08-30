#ifndef minilisp_h
#define minilisp_h
#include "tokens.h"
#include "tokenize.h"

//-------------------------------------------------------------------------------------
typedef enum
{
    next_action = 0,
    first_action = 1,
    one_action = 2,
    parenthetic_action = 3,
    map_key = 4,
    map_value = 5,
    error_action = 6
} compile_action;

typedef enum
{
    e_no_error,
    e_error_segmenting,
    e_error_bracket,
    e_error_parenthesis,
    e_error_brace,
    e_execution_error
} error_tokenize;

error_tokenize code_segmenting(string &code, Segmentingtype &infos, UTF8_Handler *special_characters);

typedef enum
{
    v_nil,
    v_atom,
    v_boolean,
    v_break,
    v_error,
    v_float,
    v_integer,
    v_list,
    v_map,
    v_string,
    v_unix,
    v_tail,

    l_append,
    l_apply,
    l_at,
    l_atom,
    l_atomp,
    l_base,
    l_block,
    l_car,
    l_cdr,
    l_chr,
    l_cadar,
    l_command,
    l_cond,
    l_cons,
    l_consp,
    l_defun,
    l_different,
    l_divide,
    l_eq,
    l_equal,
    l_eval,
    l_filtercar,
    l_find,
    l_float,
    l_if,
    l_inf,
    l_infeq,
    l_insert,
    l_integer,
    l_join,
    l_label,
    l_lambda,
    l_list,
    l_load,
    l_loop,
    l_lower,
    l_map,
    l_mapcar,
    l_minus,
    l_mod,
    l_multiply,
    l_nconc,
    l_neq,
    l_not,
    l_nullp,
    l_numberp,
    l_ord,
    l_plus,
    l_pop,
    l_print,
    l_println,
    l_push,
    l_put,
    l_quote,
    l_range,
    l_read,
    l_replace,
    l_setq,
    l_size,
    l_sort,
    l_split,
    l_stats,
    l_string,
    l_stringp,
    l_sub,
    l_sup,
    l_supeq,
    l_trim,
    l_type,
    l_upper,
    l_while,
    l_write,
    l_zerop,
    l_zip,

    math_acos,
    math_acosh,
    math_asin,
    math_asinh,
    math_atan,
    math_atanh,
    math_cbrt,
    math_cos,
    math_cosh,
    math_degree,
    math_erf,
    math_erfc,
    math_exp,
    math_exp2,
    math_expm1,
    math_fabs,
    math_floor,
    math_gcd,
    math_hcf,
    math_lgamma,
    math_log,
    math_log10,
    math_log1p,
    math_log2,
    math_logb,
    math_nearbyint,
    math_radian,
    math_rint,
    math_round,
    math_sin,
    math_sinh,
    math_sqrt,
    math_tan,
    math_tanh,
    math_tgamma,
    math_trunc,

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
class lisp_cadar;
class lisp_mini;

extern lisp_element *lisp_nil;
extern lisp_element *lisp_true;

extern lisp_string *lisp_emptystring;
extern lisp_atom *lisp_break;

extern lisp_atom *lisp_tail;

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

const uint16_t s_constant = 65535;  // all bits set to 1
const uint16_t s_protect = 32768;   // bit 15 set to 1
const uint16_t s_protected = 16384; // bit 14 set to 1
const uint16_t s_check = 49152;     // bit 15 and 14 set to 1

class lisp_element
{
public:
    uint16_t code;
    uint16_t status;
#ifdef DEBUGGER
    long idx;
#endif

    lisp_element(uint16_t cst, uint16_t c) : status(cst), code(c)
    {
#ifdef DEBUGGER
        idx = 0;
#endif
    }
    lisp_element(vector<lisp_element *> &storage, uint16_t c);
    lisp_element(uint16_t c);
    ~lisp_element();

    inline uint16_t status_not_constant()
    {
        return ((status & s_check) == 0);
    }

    void mark()
    {
        status += status_not_constant();
    }

    void demark()
    {
        status -= (status && status_not_constant());
    }

    virtual void unmark()
    {
        status -= (status && status_not_constant());
        if (!status)
        {
            delete this;
        }
    }

    virtual lisp_element *find(lisp_element *v)
    {
        return lisp_nil;
    }

    virtual void concatenate(lisp_element *c);
    lisp_element *range(lisp_mini *lisp)
    {
        return lisp_nil;
    }
    lisp_element *methodBase(lisp_mini *lisp, lisp_element *v_base, bool toconvert);

    virtual lisp_element *sort(bool direction)
    {
        return this;
    }

    virtual void clear() {}
    virtual lisp_element *loop(lisp_mini *lisp, lisp_list *code, lisp_element *variable)
    {
        return lisp_nil;
    }

    virtual lisp_element *join(lisp_element *sep);
    virtual lisp_element *mapcar(lisp_mini *lisp, lisp_element *oper)
    {
        return lisp_nil;
    }

    virtual lisp_element *filtercar(lisp_mini *lisp, lisp_element *oper)
    {
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

    virtual lisp_element *replace(lisp_element *a, lisp_element *v);
    virtual lisp_element *sub(long b, long e);
    virtual lisp_element *append(lisp_element *e);
    virtual lisp_element *append(u_ustring &key, lisp_element *e);
    virtual lisp_element *insert(lisp_element *key, lisp_element *e);
    virtual lisp_element *put(lisp_element *key, lisp_element *e);
    compile_action store(string &, lisp_element *e, compile_action a);
    virtual void pop_raw() {}
    virtual void pop(lisp_element *);
    virtual lisp_element *push_first(lisp_element *e);
    virtual lisp_element *car();
    virtual lisp_element *cadar(lisp_cadar*);
    virtual lisp_element *cdr();
    virtual lisp_element *at_position(lisp_element *i)
    {
        return lisp_nil;
    }
    virtual lisp_element *at(long i)
    {
        return lisp_nil;
    }
    virtual lisp_element *command(lisp_mini*);
    virtual lisp_element *split(lisp_element *)
    {
        return lisp_nil;
    }
    virtual lisp_element *plus(lisp_element *v);
    virtual lisp_element *minus(lisp_element *v);
    virtual lisp_element *multiply(lisp_element *v);
    virtual lisp_element *divide(lisp_element *v);
    virtual lisp_element *mod(lisp_element *v);
    virtual bool inf(lisp_element *v)
    {
        return lisp_nil;
    }
    virtual bool sup(lisp_element *v)
    {
        return lisp_nil;
    }
    virtual bool infeq(lisp_element *v)
    {
        return lisp_nil;
    }
    virtual bool supeq(lisp_element *v)
    {
        return lisp_nil;
    }
    virtual lisp_element *cons_apply(lisp_element *op);

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

    virtual void stringvalue(u_ustring &v, bool into = false);
    virtual void string_to_os(std::stringstream &os, bool into = false);

    virtual bool is_instruction()
    {
        return false;
    }

    virtual bool is_string()
    {
        return false;
    }

    virtual bool is_number()
    {
        return false;
    }

    virtual bool is_integer()
    {
        return false;
    }

    virtual bool is_float()
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

/*
    1. When duplicate_only_constant is true, only constant values are duplicated
    2. When duplicate_only_constant is false, all values with status other than 0 are duplicated.

    Basically, you use duplicate_only_constant when you want a regular value to be modified.
    When duplicate_only_constant is false, you duplicate all values unless they are new. 
    Numerical operations on values are typically done with duplicate_only_constant=false. 
    It is then possible to locally modify a numerical value and reduce the creation of numerical or string values.
*/
    virtual lisp_element *clone(bool duplicate_only_constant)
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
    u_ustring message;

    lisp_error(u_ustring m) : message(m), lisp_element(s_constant, v_error) {}
    lisp_error(string m) : lisp_element(s_constant, v_error)
    {
        s_utf8_to_unicode(message, m, m.size());
    }

    lisp_error(lisp_element *code, u_ustring m) : lisp_element(v_error)
    {
        message = m;
        message += U" in '";
        code->stringvalue(message);
        message += U"'";
    }

    lisp_error(lisp_element *code, string m) : lisp_element(v_error)
    {
        s_utf8_to_unicode(message, m, m.size());
        message += U" in '";
        code->stringvalue(message);
        message += U"'";
    }

    lisp_element *eval(lisp_mini *)
    {
        throw this;
    }

    void stringvalue(u_ustring &v, bool into = false)
    {
        v += message;
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        string s;
        s_unicode_to_utf8(s, message);
        os << s;
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

    void stringvalue(u_ustring &v, bool into = false)
    {
        if (value)
            v += U"true";
        else
            v += U"nil";
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        if (value)
            os << "true";
        else
            os << "nil";
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

    bool eq(lisp_element *v)
    {
        return (v == this);
    }

    bool neq(lisp_element *v)
    {
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

class lisp_cadar : public  lisp_element {
public:
    string action;

    lisp_cadar(vector<lisp_element *> &storage, string a) : action(a), lisp_element(storage, l_cadar) {}
    
    void stringvalue(u_ustring &v, bool into = false)
    {
        for (long i = 0; i < action.size(); i++)
            v += (u_uchar)action[i];
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        os << action;
    }

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
    lisp_integer(vector<lisp_element *> &storage, long v) : value(v), lisp_element(storage, v_integer) {}
    lisp_integer(long v) : value(v), lisp_element(v_integer) {}

    void clear()
    {
        value = 0;
    }

    lisp_element *plus(lisp_element *v)
    {
        value += v->longvalue();
        return this;
    }

    lisp_element *clone(bool duplicate_only_constant)
    {
        if (!status || (duplicate_only_constant && status_not_constant()))
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

    bool is_integer()
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

    void stringvalue(u_ustring &v, bool into = false)
    {
        v += convertToUString(value);
    }

    void string_to_os(std::stringstream &os, bool into = false)
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
    lisp_float(vector<lisp_element *> &storage, double v) : value(v), lisp_element(storage, v_float) {}
    lisp_float(double v) : value(v), lisp_element(v_float) {}

    void clear()
    {
        value = 0;
    }

    lisp_element *plus(lisp_element *v)
    {
        value += v->doublevalue();
        return this;
    }

    lisp_element *clone(bool duplicate_only_constant)
    {
        if (!status || (duplicate_only_constant && status_not_constant()))
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
        long d = (long)v->longvalue();
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

    bool is_float()
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

    void stringvalue(u_ustring &v, bool into = false)
    {
        v += convertToUString(value);
    }

    void string_to_os(std::stringstream &os, bool into = false)
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
class lisp_long_range : public lisp_element
{
public:
    long init, limit, inc;

    lisp_long_range(long i, long l, long ic) : lisp_element(l_range)
    {
        init = i;
        limit = l;
        inc = ic;

        if (init > limit && inc > 0)
            inc *= -1;
    }

    lisp_element *loop(lisp_mini *lisp, lisp_list *code, lisp_element *variable);
    lisp_element *mapcar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *filtercar(lisp_mini *lisp, lisp_element *oper);

    void stringvalue(u_ustring &v, bool into = false)
    {
        std::wstringstream os;
        os << L"(range " << init << L" " << limit << L" " << inc << L")";
        wstring w = os.str();
        for (long i = 0; i < w.size(); i++)
            v += (u_uchar)w[i];
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        os << "(range " << init << " " << limit << " " << inc << ")";
    }
};

class lisp_double_range : public lisp_element
{
public:
    double init, limit, inc;

    lisp_double_range(double i, double l, double ic) : lisp_element(l_range)
    {
        init = i;
        limit = l;
        inc = ic;
        if (init > limit && inc > 0)
            inc *= -1;
    }

    lisp_element *loop(lisp_mini *lisp, lisp_list *code, lisp_element *variable);
    lisp_element *mapcar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *filtercar(lisp_mini *lisp, lisp_element *oper);

    void stringvalue(u_ustring &v, bool into = false)
    {
        std::wstringstream os;
        os << L"(range " << init << L" " << limit << L" " << inc << L")";
        wstring w = os.str();
        for (long i = 0; i < w.size(); i++)
            v += (u_uchar)w[i];
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        os << "(range " << init << " " << limit << " " << inc << ")";
    }
};
//-------------------------------------------------------------------------------------
class lisp_string : public lisp_element
{
public:
    u_ustring value;

    lisp_string(uint16_t constant, u_ustring v) : value(v), lisp_element(constant, v_string) {}
    lisp_string(vector<lisp_element *> &storage, string v) : lisp_element(storage, v_string)
    {
        s_utf8_to_unicode(value, v, v.size());
    }
    lisp_string(u_ustring v) : value(v), lisp_element(v_string) {}
    lisp_string(string v) : lisp_element(v_string)
    {
        s_utf8_to_unicode(value, v, v.size());
    }
    lisp_string(char v) : lisp_element(v_string)
    {
        value = v;
    }

    void concatenate(lisp_element *c) {
        u_ustring str;
        c->stringvalue(str);
        value += str;
    }
    
    lisp_string(u_uchar v) : lisp_element(v_string)
    {
        value = v;
    }

    void clear()
    {
        value = U"";
    }

    bool is_string()
    {
        return true;
    }

    lisp_element *clone(bool duplicate_only_constant)
    {
        if (!status || (duplicate_only_constant && status_not_constant()))
            return this;

        return new lisp_string(value);
    }

    lisp_element *find(lisp_element *v);

    lisp_element *loop(lisp_mini *lisp, lisp_list *code, lisp_element *variable);
    lisp_element *mapcar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *filtercar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *insert(lisp_element *k, lisp_element *v);
    lisp_element *put(lisp_element *k, lisp_element *v);
    lisp_element *append(lisp_element *v) {
        u_ustring key;
        v->stringvalue(key);
        value += key;
        return this;
    }

    lisp_element *at_position(lisp_element *i);
    lisp_element *at(long i);

    lisp_element *sub(long b, long e);
    lisp_element *car();
    lisp_element *cadar(lisp_cadar*);
    lisp_element *cdr();

    void pop(lisp_element *e);
    lisp_element *replace(lisp_element *a, lisp_element *v);

    long size()
    {
        return value.size();
    }

    bool boolean()
    {
        return value.size();
    }

    lisp_element *plus(lisp_element *v)
    {
        u_ustring s;
        v->stringvalue(s);
        value += s;
        return this;
    }

    lisp_element *split(lisp_element *splitter);

    void stringvalue(u_ustring &v, bool into = false)
    {
        if (into)
        {
            v += U"\"";
            v += value;
            v += U"\"";
        }
        else
            v += value;
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        string s;
        s_unicode_to_utf8(s, value);
        if (into)
            os << "\"" << s << "\"";
        else
            os << s;
    }

    bool eq(lisp_element *v)
    {
        u_ustring w;
        v->stringvalue(w);
        return w == value;
    }

    bool neq(lisp_element *v)
    {
        u_ustring w;
        v->stringvalue(w);
        return w != value;
    }

    bool inf(lisp_element *v)
    {
        u_ustring w;
        v->stringvalue(w);
        return w < value;
    }

    bool sup(lisp_element *v)
    {
        u_ustring w;
        v->stringvalue(w);
        return w > value;
    }

    bool infeq(lisp_element *v)
    {
        u_ustring w;
        v->stringvalue(w);
        return w <= value;
    }

    bool supeq(lisp_element *v)
    {
        u_ustring w;
        v->stringvalue(w);
        return w >= value;
    }
};
//-------------------------------------------------------------------------------------
class lisp_list : public lisp_element
{
public:
    vector<lisp_element *> values;

    lisp_list() : lisp_element(v_list) {}
    lisp_list(vector<lisp_element *> &storage) : lisp_element(storage, v_list) {}
    lisp_list(uint16_t c) : lisp_element(c, v_list) {}

    bool is_list()
    {
        return true;
    }

    void concatenate(lisp_element *c)
    {
        if (c->is_list())
        {
            for (long i = 0; i < c->size(); i++)
            {
                append(c->at(i));
            }
        }
        else
            append(c);
    }

    lisp_element *range(lisp_mini *lisp);
    lisp_element *loop(lisp_mini *lisp, lisp_list *code, lisp_element *variable);
    lisp_element *mapcar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *filtercar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *join(lisp_element *sep);

    lisp_element *cons_apply(lisp_element *op)
    {
        lisp_list *l = new lisp_list();
        l->append(op);
        for (long i = 0; i < size(); i++)
            l->append(values[i]);
        return l;
    }

    lisp_element *find(lisp_element *v)
    {
        for (long i = 0; i < values.size(); i++)
        {
            if (v->equal(values[i]))
                return new lisp_integer(i);
        }
        return lisp_nil;
    }

    virtual lisp_element *put(lisp_element *k, lisp_element *v) {
        long key = k->longvalue();
        if (key < 0)
            throw new lisp_error(this, "out of range");

        v->mark();

        if (key >= values.size())
            values.push_back(v);
        else {
            lisp_element* e = values[key];
            e->unmark();
            values[key] = v;
        }            
        return this;
    } 

    virtual lisp_element *insert(lisp_element *k, lisp_element *v)
    {
        long key = k->longvalue();
        if (key < 0)
            throw new lisp_error(this, "out of range");

        v->mark();

        if (key >= values.size())
            values.push_back(v);
        else
            values.insert(values.begin() + key, v);
        return this;
    }

    virtual lisp_element *append(lisp_element *e)
    {
        e->mark();
        values.push_back(e);
        return this;
    }

    lisp_element *sub(long b, long e);
    lisp_element *sort(bool direction);
    virtual void unmark();
    virtual void remove();
    virtual void protect();
    virtual void unprotect();

    virtual lisp_element *release();
    void pop_raw()
    {
        values.pop_back();
    }

    virtual void pop(lisp_element *e)
    {
        long sz = values.size();
        long i = sz - 1;
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
        values.erase(values.begin() + i);
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

    void clear()
    {
        for (long i = 0; i < values.size(); i++)
        {
            values[i]->unmark();
        }
        values.clear();
    }

    virtual lisp_element *execute_lambda(lisp_mini *, lisp_element *lmbd);
    virtual lisp_element *execute_function(lisp_mini *, lisp_element *function);

    virtual lisp_element *eval(lisp_mini *);

    virtual lisp_element *car();
    virtual lisp_element *cadar(lisp_cadar*);
    virtual lisp_element *cdr();

    virtual lisp_element *at_position(lisp_element *e)
    {
        long i = e->longvalue();
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

    virtual void stringvalue(u_ustring &v, bool into = false)
    {
        v += U"(";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                v += U" ";
            first = false;
            a->stringvalue(v, true);
        }
        v += U")";
    }

    virtual void string_to_os(std::stringstream &os, bool into = false)
    {
        os << "(";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                os << " ";
            first = false;
            a->string_to_os(os, true);
        }
        os << ")";
    }

    virtual lisp_element *clone(bool duplicate_only_constant)
    {
        if (!status || (duplicate_only_constant && status_not_constant()))
            return this;

        lisp_list *l = new lisp_list();
        for (long i = 0; i < values.size(); i++)
        {
            l->append(values[i]->clone(duplicate_only_constant));
        }
        return l;
    }

    bool equal(lisp_element *v)
    {
        if (v->code != v_list || v->size() != size())
            return false;
        lisp_list *val = (lisp_list *)v;
        for (long i = 0; i < values.size(); i++)
        {
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
class lisp_list_nil : public lisp_list
{
public:
    lisp_list_nil() : lisp_list(s_constant)
    {
        code = v_nil;
    }

    lisp_element *put(lisp_element *k, lisp_element *v)
    {
        lisp_list *l = new lisp_list();
        l->append(v);
        return l;
    }

    lisp_element *insert(lisp_element *k, lisp_element *v)
    {
        lisp_list *l = new lisp_list();
        l->append(v);
        return l;
    }

    lisp_element *append(lisp_element *e)
    {
        lisp_list *l = new lisp_list();
        l->append(e);
        return l;
    }

    void unmark() {}
    void remove() {}
    void protect() {}
    void unprotect() {}
    lisp_element *release() {}

    void pop(lisp_element *)
    {
        lisperrorrange->eval(NULL);
    }

    lisp_element *push_first(lisp_element *e)
    {
        lisp_list *l = new lisp_list();
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

    lisp_element *execute_lambda(lisp_mini *, lisp_element *lmbd)
    {
        lisperror->eval(NULL);
    }

    lisp_element *execute_function(lisp_mini *, lisp_element *function)
    {
        lisperror->eval(NULL);
    }

    lisp_element *eval(lisp_mini *)
    {
        return this;
    }

    lisp_element *car()
    {
        return this;
    }

    lisp_element *cadar(lisp_cadar*)
    {
        return this;
    }

    lisp_element *cdr()
    {
        return this;
    }

    lisp_element *at_position(lisp_element *i)
    {
        return lisp_nil;
    }

    void stringvalue(u_ustring &v, bool into = false)
    {
        v += U"()";
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        os << "()";
    }

    lisp_element *clone(bool duplicate_only_constant)
    {
        return this;
    }
};
//-------------------------------------------------------------------------------------
class lisp_map : public lisp_element
{
public:
    std::map<u_ustring, lisp_element *> values;

    lisp_map() : lisp_element(v_map) {}
    lisp_map(vector<lisp_element *> &storage) : lisp_element(storage, v_map) {}
    lisp_map(uint16_t c) : lisp_element(c, v_map) {}

    bool is_map()
    {
        return true;
    }

    lisp_element *put(lisp_element *k, lisp_element *v)
    {
        u_ustring key;
        k->stringvalue(key);
        v->mark();
        k = values[key];
        if (k != NULL)
            k->unmark();
        values[key] = v;
        return this;
    }

    lisp_element *insert(lisp_element *k, lisp_element *v)
    {
        u_ustring key;
        k->stringvalue(key);
        v->mark();
        k = values[key];
        if (k != NULL)
            k->unmark();
        values[key] = v;
        return this;
    }

    lisp_element *append(u_ustring &key, lisp_element *v)
    {
        v->mark();
        lisp_element *k = values[key];
        if (k != NULL)
            k->unmark();
        values[key] = v;
        return this;
    }

    void concatenate(lisp_element *c)
    {
        if (c->is_map())
        {
            lisp_map *m = (lisp_map *)c;
            for (const auto &a : m->values)
            {
                c = values[a.first];
                if (c != NULL)
                    c->unmark();
                a.second->mark();
                values[a.first] = a.second;
            }
        }
        else
            throw new lisp_error(c, "A map is expected here");
    }

    lisp_element *loop(lisp_mini *lisp, lisp_list *code, lisp_element *variable);
    lisp_element *mapcar(lisp_mini *lisp, lisp_element *oper);
    lisp_element *filtercar(lisp_mini *lisp, lisp_element *oper);

    void unmark();
    void remove();
    void protect();
    void unprotect();
    lisp_element *release();

    lisp_element *find(lisp_element *v)
    {
        for (const auto &a : values)
        {
            if (v->equal(a.second))
                return new lisp_string(a.first);
        }
        return lisp_nil;
    }

    void pop(lisp_element *e)
    {
        u_ustring key;
        e->stringvalue(key);

        if (values.find(key) != values.end())
        {
            values[key]->unmark();
            values.erase(key);
        }
        else
            lisperrorrange->eval(NULL);
    }

    void clear()
    {
        for (const auto &a : values)
        {
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

    lisp_element *at_position(lisp_element *k)
    {
        u_ustring key;
        k->stringvalue(key);
        if (values.find(key) != values.end())
            return values[key];
        return lisp_nil;
    }

    void stringvalue(u_ustring &v, bool into = false)
    {
        v += U"{";
        bool first = true;
        for (const auto &a : values)
        {
            if (!first)
                v += U" ";
            first = false;
            v += a.first;
            v += U":";
            a.second->stringvalue(v, true);
        }
        v += U"}";
    }

    virtual void string_to_os(std::stringstream &os, bool into = false)
    {
        os << "{";
        bool first = true;
        string s;
        u_ustring k;
        for (const auto &a : values)
        {
            if (!first)
                os << " ";
            first = false;
            s = "";
            k = a.first;
            s_unicode_to_utf8(s, k);
            os << s << ":";
            a.second->string_to_os(os, true);
        }
        os << "}";
    }

    lisp_element *clone(bool duplicate_only_constant)
    {
        if (!status || (duplicate_only_constant && status_not_constant()))
            return this;

        lisp_map *m = new lisp_map();
        lisp_element *e;
        for (const auto &a : values)
        {
            e = a.second->clone(duplicate_only_constant);
            e->mark();
            m->values[a.first] = e;
        }
        return m;
    }

    bool equal(lisp_element *v)
    {
        if (v->code != v_map || v->size() != size())
            return false;
        lisp_map *val = (lisp_map *)v;
        for (const auto &a : values)
        {
            if (val->values.find(a.first) == val->values.end() ||
                !a.second->equal(val->values[a.first]))
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
class lisp_unix : public lisp_element
{
public:
    u_ustring value;

    lisp_unix(vector<lisp_element *> &storage, string v) : lisp_element(storage, v_unix)
    {
        s_utf8_to_unicode(value, v, v.size());
    }
    lisp_unix(uint16_t c, string v) : lisp_element(c, v_unix)
    {
        s_utf8_to_unicode(value, v, v.size());
    }
    lisp_unix(string v) : lisp_element(v_unix)
    {
        s_utf8_to_unicode(value, v, v.size());
    }

    lisp_element *command(lisp_mini* lisp);

    void stringvalue(u_ustring &v, bool into = false)
    {
        if (into)
        {
            v += U"\"";
            v += value;
            v += U"\"";
        }
        else
            v += value;
    }

    void string_to_os(std::stringstream &os, bool into = false)
    {
        string s;
        s_unicode_to_utf8(s, value);
        if (into)
            os << "\"" << s << "\"";
        else
            os << s;
    }
};
//-------------------------------------------------------------------------------------
class lisp_mini
{
public:
    vector<std::map<uint16_t, lisp_element *>> variables;
    std::map<uint16_t, lisp_atom *> atoms;
    vector<uint16_t> stack;

    Segmentingtype infos;
    string current_directory;
    string current_file_name;

    bool stop_execution;

    void set_file_name(string &);
    void garbage_clean();

    void stack_variables_on(std::map<uint16_t, lisp_element *> &local_vars)
    {
        variables.push_back(local_vars);
    }

    void stack_variables_replace(std::map<uint16_t, lisp_element *> &local_vars)
    {
        //We merge and keep our variables locally
        lisp_element* e;
        for (const auto& a : local_vars) {
            e = variables.back()[a.first];
            variables.back()[a.first] = a.second;
            e->unmark();
        }
    }

    void stack_variables_in(std::map<uint16_t, lisp_element *> &local_vars)
    {
        //We merge and keep our variables locally
        lisp_element* e;
        for (const auto& a : local_vars) {
            e = variables.back()[a.first];
            variables.back()[a.first] = a.second;
            a.second->mark();
            local_vars[a.first] = e;
        }
    }

    void stack_variables_out(std::map<uint16_t, lisp_element *> &local_vars)
    {
        //We merge and keep our variables locally
        for (const auto& a : local_vars) {
            variables.back()[a.first]->unmark();
            if (a.second == NULL) {                
                variables.back().erase(a.first);
            }
            else
                variables.back()[a.first] = a.second;
        }
    }

    lisp_atom *get_atom(uint16_t a)
    {
        lisp_atom *la = atoms[a];
        if (la == NULL)
        {
            la = new lisp_atom(a);
            atoms[a] = la;
        }
        return la;
    }

    lisp_atom *get_atom(u_ustring &u);

    void stack_off(lisp_element *e)
    {
        if (variables.size() == 0)
            lispstackerror->eval(this);
        e->mark();
        for (auto &v : variables.back())
        {
            v.second->unmark();
        }
        e->demark();
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

    void clean(vector<lisp_element *> &storage)
    {
        protect_variables();
        for (auto &a : storage)
        {
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
    bool compile(lisp_element *program, vector<lisp_element *> &storage, long &pos, compile_action first);
    lisp_element *run(string code);

    bool check_atom(uint16_t c)
    {
        return (variables.back().find(c) != variables.back().end());
    }

    bool check_function(uint16_t c)
    {
        return (variables[0].find(c) != variables[0].end());
    }

    void insert_bottom(uint16_t c, lisp_element *l) {
        lisp_element *previous = variables[0][c];
        if (previous != NULL)
        {
            if (previous == l)
                return;
            previous->unmark();
        }
        l = l->clone(true);
        l->mark();
        variables[0][c] = l;
    }

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

    void store_atom(uint16_t c, lisp_element *l)
    {
        variables.back()[c] = l;
    }

    void initmathvalues();
    string read_file(lisp_element *e);
    void write_file(lisp_element *e, lisp_element *txt);
    void append_file(lisp_element *e, lisp_element *txt);
    lisp_element *load_program(lisp_element *program, lisp_element *e, vector<lisp_element *> &storage);
    string execute_code(string &code);
    string execute_file(string filename, vector<string> &args);
    string execute_unix_command(string cmd);

    ~lisp_mini()
    {
        garbage_clean();
    }
};

#endif