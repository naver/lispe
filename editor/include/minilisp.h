#ifndef minilisp_h
#define minilisp_h
#include "tokens.h"
#include "tokenize.h"

//-------------------------------------------------------------------------------------
typedef enum {
    next_action = 0, first_action = 1, one_action = 2
} compile_action;


typedef enum
{
    v_nil,
    v_atom,
    v_error,
    v_string,
    v_list,
    v_number,
    v_unix,
    v_boolean,
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
    l_clean,
    l_if,
    l_cond,
    l_block,
    l_defun,
    l_eq,
    l_neq,
    l_inf,
    l_sup,
    l_infeq,
    l_supeq,
    l_loop,
    l_lambda,
    l_eval,
    l_number,
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
    l_final
} lisp_instruction_code;

//-------------------------------------------------------------------------------------
// Some specific values
// Note that nil cannot be stored in the garbage;
class lisp_element;
class lisp_error;
class lisp_mini;

lisp_mini *create_mini_lisp_instance();
string execute_unix_command(string wcmd);

extern lisp_element *lisp_nil;
extern lisp_element *lisp_emptystring;
extern lisp_element *lisp_true;

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
uint16_t get_code(string &w);

const uint16_t s_constant = 65535;
const uint16_t s_protect = 32768;

class lisp_element
{
public:
    uint16_t code;
    uint16_t status;

    lisp_element(bool cst, uint16_t c) : status(s_constant), code(c) {}
    lisp_element(uint16_t c);
    ~lisp_element();

    void mark();

    virtual void unmark();
    virtual void clean(std::set<lisp_element *> &garbage);
    virtual void protect();
    virtual void unprotect();
    virtual lisp_element *release();

    void demark()
    {
        status -= (status && status != s_constant);
    }

    virtual long size()
    {
        return 0;
    }

    virtual lisp_element *append(lisp_element *e);

    virtual void pop();

    virtual lisp_element *push_first(lisp_element *e);

    virtual lisp_element *car();

    virtual lisp_element *cdr();

    virtual lisp_element *at(long i);

    virtual lisp_element *command();

    virtual lisp_element *split(string &splitter);

    virtual lisp_element *plus(lisp_element *v);

    virtual lisp_element *minus(lisp_element *v);

    virtual lisp_element *multiply(lisp_element *v);

    virtual lisp_element *divide(lisp_element *v);

    virtual lisp_element *mod(lisp_element *v);

    virtual bool inf(lisp_element *v);

    virtual bool sup(lisp_element *v);

    virtual bool infeq(lisp_element *v);

    virtual bool supeq(lisp_element *v);

    virtual bool eq(lisp_element *v)
    {
        return v->code == code;
    }

    virtual bool neq(lisp_element *v)
    {
        return v->code != code;
    }

    virtual double numerical_value()
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

    virtual lisp_element *eval(lisp_mini *)
    {
        return this;
    }

    virtual lisp_element *clone()
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

class lisp_error : public lisp_element
{
public:
    string message;

    lisp_error(string m) : message(m), lisp_element(true, v_error) {}

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

class lisp_boolean : public lisp_element
{
public:
    bool value;

    lisp_boolean(bool v) : value(v), lisp_element(true, v_boolean) {}

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

class lisp_atom : public lisp_element
{
public:
    lisp_atom(uint16_t c) : lisp_element(c) {}

    lisp_element *eval(lisp_mini *);

    bool is_atom()
    {
        return true;
    }
};

class lisp_instruction : public lisp_element
{
public:
    lisp_instruction(uint16_t c) : lisp_element(c) {}

    bool is_instruction()
    {
        return true;
    }
};

//-------------------------------------------------------------------------------------
class lisp_number : public lisp_element
{
public:
    double value;

    lisp_number(double v) : value(v), lisp_element(v_number) {}

    lisp_element *plus(lisp_element *v)
    {
        value += v->numerical_value();
        return this;
    }

    lisp_element *clone()
    {
        if (status)
            return new lisp_number(value);
        else
            return this;
    }

    lisp_element *minus(lisp_element *v)
    {
        value -= v->numerical_value();
        return this;
    }

    lisp_element *multiply(lisp_element *v)
    {
        value *= v->numerical_value();
        return this;
    }

    lisp_element *divide(lisp_element *v)
    {
        double d = v->numerical_value();
        if (d == 0)
            return lisperrordivided0->eval(NULL);
        value /= d;
        return this;
    }

    lisp_element *mod(lisp_element *v)
    {
        long d = (long)v->numerical_value();
        if (d == 0)
            return lisperrordivided0->eval(NULL);
        d = (long)value % d;
        value = d;
        return this;
    }

    bool boolean()
    {
        return value != 0;
    }

    double numerical_value()
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
        return v->numerical_value() == code;
    }

    bool neq(lisp_element *v)
    {
        return v->numerical_value() == code;
    }

    bool inf(lisp_element *v)
    {
        return code < v->numerical_value();
    }

    bool sup(lisp_element *v)
    {
        return code > v->numerical_value();
    }

    bool infeq(lisp_element *v)
    {
        return code <= v->numerical_value();
    }

    bool supeq(lisp_element *v)
    {
        return code >= v->numerical_value();
    }
};

class lisp_list : public lisp_element
{
public:
    vector<lisp_element *> list_of_elements;
    lisp_list() : lisp_element(v_list) {}
    lisp_list(bool c) : lisp_element(c, v_nil) {}

    lisp_element *append(lisp_element *e)
    {
        e->mark();
        list_of_elements.push_back(e);
        return this;
    }

    void unmark();
    void clean(std::set<lisp_element *> &garbage);
    void protect();
    void unprotect();
    lisp_element *release();

    void pop()
    {
        if (list_of_elements.size())
        {
            list_of_elements.back()->unmark();
            list_of_elements.pop_back();
        }
        else
            lisperror->eval(NULL);
    }

    virtual lisp_element *push_first(lisp_element *e)
    {
        e->mark();
        list_of_elements.insert(list_of_elements.begin(), e);
        return this;
    }

    long size()
    {
        return list_of_elements.size();
    }

    bool boolean()
    {
        return list_of_elements.size();
    }

    lisp_element *execute_lambda(lisp_mini *, lisp_element *lmbd);
    lisp_element *execute_function(lisp_mini *, lisp_element *function);

    lisp_element *eval(lisp_mini *);

    lisp_element *car()
    {
        if (list_of_elements.size())
            return list_of_elements[0];
        return lisp_nil;
    }

    lisp_element *cdr()
    {
        if (!list_of_elements.size())
            return lisp_nil;

        lisp_list *l = new lisp_list();
        for (long i = 1; i < list_of_elements.size(); i++)
            l->append(list_of_elements[i]);
        return l;
    }

    lisp_element *at(long i)
    {
        if (i >= 0 && i < list_of_elements.size())
            return list_of_elements[i];
        return lisperrorrange->eval(NULL);
    }

    void stringvalue(string &v)
    {
        v += "(";
        bool first = true;
        for (const auto &a : list_of_elements)
        {
            if (!first)
                v += " ";
            first = false;
            a->stringvalue(v);
        }
        v += ")";
    }

    void asstring(std::stringstream &os)
    {
        os << "(";
        bool first = true;
        for (const auto &a : list_of_elements)
        {
            if (!first)
                os << " ";
            first = false;
            a->asstring(os);
        }
        os << ")";
    }

    lisp_element *clone()
    {
        if (!status || lisp_nil == this)
            return this;

        lisp_list *l = new lisp_list();
        for (long i = 0; i < list_of_elements.size(); i++)
        {
            l->append(list_of_elements[i]->clone());
        }
        return l;
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

class lisp_string : public lisp_element
{
public:
    string value;

    lisp_string(bool constant) : lisp_element(constant, v_string) {}
    lisp_string(string v) : value(v), lisp_element(v_string) {}
    lisp_string(char v) : lisp_element(v_string)
    {
        value = v;
    }

    lisp_element *at(long i)
    {
        if (i >= 0 && i < value.size())
            return new lisp_string(value[i]);
        return lisperrorrange->eval(NULL);
    }

    long size()
    {
        return value.size();
    }

    lisp_element *clone()
    {
        if (status)
            return new lisp_string(value);
        return this;
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

    lisp_element *split(string &splitter)
    {
        vector<string> vs;
        s_split(value, splitter, vs, false);
        lisp_list *l = new lisp_list();
        for (long i = 0; i < vs.size(); i++)
        {
            l->append(new lisp_string(vs[i]));
        }
        return l;
    }

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

class lisp_unix : public lisp_element
{
public:
    string value;

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

class lisp_mini
{
public:
    vector<std::map<uint16_t, lisp_element *>> variables;
    Segmentingtype infos;
    double count_data;
    bool stop_execution;

    void garbage_clean();

    void stack_variables_on(std::map<uint16_t, lisp_element *> &local_vars)
    {
        variables.push_back(local_vars);
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

    void clean(lisp_element* program) {
        protect_variables();
        std::set<lisp_element*> garbage;
        program->clean(garbage);
        unprotect_variables();
        for (auto& a : garbage)
            delete a;
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
    bool compile(lisp_element *program, long &pos, compile_action first);
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
        l->mark();
        variables.back()[c] = l;
    }

    ~lisp_mini()
    {
        garbage_clean();
    }
};

#endif