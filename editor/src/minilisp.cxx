#include "minilisp.h"

// We want lists, numbers and dictionary
extern UTF8_Handler special_characters;
//------------------------------------------------------------------------
static void initialisation_static_values()
{
    if (lisp_nil == NULL)
    {
        lisp_nil = new lisp_list_nil();
        lisp_true = new lisp_boolean(true);
        lisp_emptystring = new lisp_string(s_constant, "");
        lisp_break = new lisp_atom(v_break);

        // Error messages
        lisperror = new lisp_error("Error: wrong method for this element");
        lispargnbserror = new lisp_error("Error: wrong number of arguments");
        lisptokenizeerror = new lisp_error("Error: tokenization error");
        lisperrorrange = new lisp_error("Error: out of range");
        lisperrordivided0 = new lisp_error("Error: divided by 0");
        lispunknownatom = new lisp_error("Error: unknown atom");
        lispunknownmethod = new lisp_error("Error: unknown method");
        lispstackerror = new lisp_error("Stack error");
        lisplambdaerror = new lisp_error("Lambda error");
        lisp_end = new lisp_error("reset al");

        code_dictionary["nil"] = v_nil;
        code_dictionary["true"] = v_boolean;
        code_dictionary["break"] = v_break;

        code_dictionary["atom_"] = v_atom;
        code_dictionary["error_"] = v_error;
        code_dictionary["string_"] = v_string;
        code_dictionary["list_"] = v_list;
        code_dictionary["float_"] = v_float;
        code_dictionary["integer_"] = v_integer;
        code_dictionary["unix_"] = v_unix;

        code_dictionary["lambda"] = l_lambda;
        code_dictionary["defun"] = l_defun;
        code_dictionary["eval"] = l_eval;
        code_dictionary["+"] = l_plus;
        code_dictionary["-"] = l_minus;
        code_dictionary["*"] = l_multiply;
        code_dictionary["/"] = l_divide;
        code_dictionary["%"] = l_mod;
        code_dictionary["car"] = l_car;
        code_dictionary["cdr"] = l_cdr;
        code_dictionary["cons"] = l_cons;
        code_dictionary["split"] = l_split;
        code_dictionary["replace"] = l_replace;
        code_dictionary["print"] = l_print;
        code_dictionary["at"] = l_at;
        code_dictionary["list"] = l_list;
        code_dictionary["map"] = l_map;
        code_dictionary["mapcar"] = l_mapcar;
        code_dictionary["filtercar"] = l_filtercar;
        code_dictionary["key"] = l_key;
        code_dictionary["loop"] = l_loop;
        code_dictionary["while"] = l_while;
        code_dictionary["cond"] = l_cond;
        code_dictionary["if"] = l_if;
        code_dictionary["not"] = l_not;
        code_dictionary["="] = l_equal;
        code_dictionary["!="] = l_different;
        code_dictionary["eq"] = l_eq;
        code_dictionary["neq"] = l_neq;
        code_dictionary["<"] = l_inf;
        code_dictionary[">"] = l_sup;
        code_dictionary["<="] = l_infeq;
        code_dictionary[">="] = l_supeq;
        code_dictionary["setq"] = l_setq;
        code_dictionary["'"] = l_quote;
        code_dictionary["command"] = l_command;
        code_dictionary["size"] = l_size;
        code_dictionary["block"] = l_block;
        code_dictionary["base"] = l_base;
        code_dictionary["chr"] = l_chr;
        code_dictionary["ord"] = l_ord;
        code_dictionary["type"] = l_type;
        code_dictionary["cons?"] = l_consp;
        code_dictionary["zero?"] = l_zerop;
        code_dictionary["null?"] = l_nullp;
        code_dictionary["string?"] = l_stringp;
        code_dictionary["number?"] = l_numberp;
        code_dictionary["float"] = l_float;
        code_dictionary["integer"] = l_integer;
        code_dictionary["string"] = l_string;
        code_dictionary["stats"] = l_stats;
        code_dictionary["push"] = l_push;
        code_dictionary["pop"] = l_pop;
        code_dictionary["sub"] = l_sub;
        code_dictionary["apply"] = l_apply;
        code_dictionary["range"] = l_range;
        code_dictionary["join"] = l_join;

        code_dictionary["€"] = l_final;

        for (const auto &a : code_dictionary)
        {
            string_dictionary[a.second] = a.first;
            if (a.second == v_nil || a.second == v_boolean)
                continue;
            instructions_dictionary[a.second] = new lisp_instruction(a.second);
        }

        instructions_dictionary[v_nil] = lisp_nil;
        instructions_dictionary[v_boolean] = lisp_true;

        code_dictionary["quote"] = l_quote;
        code_dictionary["true"] = v_boolean;
    }
}
//-------------------------------------------------------------------------------------
lisp_element *lisp_element::methodBase(lisp_mini* lisp, lisp_element* v_base, bool toconvert)
{
    static vector<string> caracs;
    static std::unordered_map<unsigned char, long> mcaracs;
    string w;
    long n, b;

    b = v_base->longvalue();
    if (b <= 1)
        throw new lisp_error(this, "Error: cannot convert to this base");

    if (caracs.size() == 0)
    {
        w = "0";
        for (n = 0; n < 10; n++)
        {
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w[0]++;
        }
        w = "A";
        for (n = 10; n < 36; n++)
        {
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w[0]++;
        }
        w = "a";
        for (n = 36; n < 62; n++)
        {
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w[0]++;
        }
        w = "#";
        mcaracs[w[0]] = caracs.size();
        caracs.push_back(w);
        w = "@";
        mcaracs[w[0]] = caracs.size();
        caracs.push_back(w);
        w = "";
        if (!b)
            return lisp_true;
    }

    if (b > caracs.size())
    {
        return new lisp_error(this, "Error: Base too large");
    }

    unsigned long v = 0;
    if (!toconvert)
    {
        // we convert a base 10 number into the local base
        v = longvalue();
        long rest;
        string res = "";
        while (v)
        {
            rest = v % b;
            v /= b;
            res = caracs[rest] + res;
        }
        return new lisp_string(res);
    }

    w = "";
    stringvalue(w);
    unsigned char wc;
    for (n = 0; n < w.size(); n++)
    {
        wc = w[n];
        if (!mcaracs.count(wc) || mcaracs[wc] >= b)
            throw new lisp_error(this, "Error: Cannot analyze this string in this base.");

        v *= b;
        v += mcaracs[wc];
    }
    return new lisp_integer(v);
}

//-------------------------------------------------------------------------------------
#ifdef DEBUGGER
static std::set<lisp_element *> garbages;

lisp_element::lisp_element(uint16_t c) : code(c)
{
    idx = garbages.size();
    garbages.insert(this);
    status = 0;
}

lisp_element::lisp_element(std::vector<lisp_element *> &storage, uint16_t c) : status(s_protected), code(c)
{
    idx = garbages.size();
    garbages.insert(this);
    storage.push_back(this);
}

lisp_element::~lisp_element()
{
    garbages.erase(this);
}
#else
lisp_element::lisp_element(uint16_t c) : code(c)
{
    status = 0;
}

lisp_element::lisp_element(std::vector<lisp_element *> &storage, uint16_t c) : status(s_protected), code(c)
{
    storage.push_back(this);
}

lisp_element::~lisp_element() {}
#endif

//-------------------------------------------------------------------------------------

lisp_element *lisp_element::replace(lisp_element *a, lisp_element *v)
{
    return lisp_emptystring;
}

lisp_element *lisp_element::sub(double b, double e)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::cons_apply(lisp_element *op)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::join(lisp_element *sep)
{
    return lisp_emptystring;
}

lisp_element *lisp_element::append(lisp_element *e)
{
    return lisperror->eval(NULL);
}

void lisp_element::pop(lisp_element *)
{
    lisperror->eval(NULL);
}

lisp_element *lisp_element::append(string &k, lisp_element *e)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::push_first(lisp_element *e)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::car()
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::cdr()
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::command()
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::plus(lisp_element *v)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::minus(lisp_element *v)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::multiply(lisp_element *v)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::divide(lisp_element *v)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::mod(lisp_element *v)
{
    return lisperror->eval(NULL);
}
//-------------------------------------------------------------------------------------
compile_action lisp_element::store(string &current_key, lisp_element *e, compile_action action)
{
    if (action == map_value)
    {
        append(current_key, e);
        return map_key;
    }

    if (action == map_key)
        return error_action;

    append(e);
    return action;
}
//-------------------------------------------------------------------------------------

lisp_element *lisp_string::replace(lisp_element *a, lisp_element *v)
{
    string to_be_replaced;
    string with;

    a->stringvalue(to_be_replaced);
    v->stringvalue(with);

    with = s_replacingstring(value, to_be_replaced, with);
    return new lisp_string(with);
}

lisp_element *lisp_string::split(lisp_element *search)
{
    string search_string;
    lisp_list *result = new lisp_list();

    if (search == lisp_nil)
    {
        // split on space/cr characters
        u_uchar c;
        long sz = value.size();
        for (long i = 0; i < sz; i++)
        {
            c = value[i];
            if (c <= 32)
            {
                if (search_string != "")
                {
                    result->append(new lisp_string(search_string));
                    search_string = "";
                }
            }
            else
                search_string += c;
        }
        if (search_string != "")
            result->append(new lisp_string(search_string));
        return result;
    }

    search->stringvalue(search_string);

    string localvalue;
    long pos = 0;

    if (search_string == "")
    {
        long sz = value.size();
        // we split the string into an array of characters
        while (pos < sz)
        {
            special_characters.getchar(value, localvalue, pos);
            result->append(new lisp_string(localvalue));
        }
        return result;
    }

    size_t found = 0;
    while (pos != string::npos)
    {
        found = value.find(search_string, pos);
        if (found != string::npos)
        {
            localvalue = value.substr(pos, found - pos);
            if (localvalue != "")
            {
                result->append(new lisp_string(localvalue));
            }
            pos = found + search_string.size();
        }
        else
            break;
    }

    localvalue = value.substr(pos, value.size() - pos);
    if (localvalue != "")
        result->append(new lisp_string(localvalue));

    return result;
}

void lisp_string::pop(lisp_element *e)
{
    long sz = size();
    long i = sz - 1;
    if (e != lisp_nil)
    {
        i = e->longvalue();
        if (i < 0 || i >= sz)
            lisperrorrange->eval(NULL);
    }

    if (i == sz - 1)
        value = s_left(value, i);
    else
        value = s_left(value, i) + s_right(value, sz - i - 1);
}

lisp_element *lisp_string::at_position(lisp_element *e)
{
    long i = e->doublevalue();
    string v;
    if (!special_characters.getAtchar(value, v, i))
        return lisp_nil;
    return new lisp_string(v);
}

lisp_element *lisp_string::at(long i)
{
    string v;
    if (!special_characters.getAtchar(value, v, i))
        return lisp_nil;
    return new lisp_string(v);
}

lisp_element *lisp_string::sub(double b, double e)
{
    long lg = size_c(value);
    if (e <= 0)
        e = lg + e;
    if (b < 0)
        b = lg + b;
    if (e <= b)
        return lisp_emptystring;
    return new lisp_string(s_middle(value, b, e - b));
}

lisp_element *lisp_string::car()
{
    string v;
    long i = 0;
    if (!special_characters.getAtchar(value, v, i))
        return lisp_emptystring;
    return new lisp_string(v);
}

lisp_element *lisp_string::cdr()
{
    if (!value.size())
        return lisp_emptystring;

    long lg = size_c(value);
    return new lisp_string(s_right(value, lg - 1));
}

//--------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
lisp_element* long_range(lisp_mini* lisp, long init, long limit, long inc) {
    long d = (limit - init) / inc;
    if (d<0)
        d *= -1;
    
    if (init > limit && inc > 0)
        inc *= -1;
    
    if (d <= 100000) {
        if (inc == 0)
            return lisp_nil;
        
        //Integers ?
        lisp_list* range_list = new lisp_list();
        range_list->values.reserve((long)d);
        if (inc > 0) {
            for (long i = init; i < limit; i += inc) {
                range_list->values.push_back(new lisp_integer(i));
            }
        }
        else {
            for (long i = init; i > limit; i += inc)
                range_list->values.push_back(new lisp_integer(i));
        }
        return range_list;
    }
    throw new lisp_error(lisp_nil, "Error: Exceeding range");
}

lisp_element* double_range(lisp_mini* lisp, double init, double limit, double inc) {
    double d = (limit - init) / inc;
    if (d<0)
        d *= -1;
    
    if (init > limit && inc > 0)
        inc *= -1;
    
    if (d <= 100000) {
        if (inc == 0)
            return lisp_nil;
        
        lisp_list* range_list = new lisp_list();
        range_list->values.reserve((long)d);
        if (inc > 0) {
            for (double i = init; i < limit; i += inc) {
                range_list->values.push_back(new lisp_float(i));
            }
        }
        else {
            for (double i = init; i > limit; i += inc)
                range_list->values.push_back(new lisp_float(i));
        }
        return range_list;
    }
    throw new lisp_error(lisp_nil, "Error: Exceeding range");
}

lisp_element* lisp_list::range(lisp_mini* lisp) {
    
    lisp_element* e1 = values[1]->eval(lisp);
    lisp_element* e2 = values[2]->eval(lisp);
    lisp_element* e3 = values[3]->eval(lisp);
    lisp_element* e;
    
    try {
        if (e1->is_integer() && e2->is_integer() && e3->is_integer())
            e = long_range(lisp, e1->longvalue(), e2->longvalue(), e3->longvalue());
        else
            e = double_range(lisp, e1->doublevalue(), e2->doublevalue(), e3->doublevalue());
    }
    catch(lisp_error* err) {
        e1->release();
        e2->release();
        e3->release();
        throw err;
    }
    e1->release();
    e2->release();
    e3->release();
    return e;
}

//--------------------------------------------------------------------------------
lisp_element *lisp_list::release()
{
    if (!status)
    {
        for (long i = 0; i < values.size(); i++)
            values[i]->unmark();
        delete this;
        return lisp_nil;
    }
    return this;
}

void lisp_list::protect()
{
    if ((status & s_protect) == s_protect)
        return;

    status |= s_protect;
    for (long i = 0; i < values.size(); i++)
        values[i]->protect();
}

void lisp_list::unprotect()
{
    if (status == s_constant)
        return;

    if ((status & s_protect) != s_protect)
        return;

    status &= ~s_protect;
    for (long i = 0; i < values.size(); i++)
        values[i]->unprotect();
}

void lisp_list::unmark()
{
    status -= (status && s_status());
    if (!status)
    {
        for (long i = 0; i < values.size(); i++)
            values[i]->unmark();
        delete this;
    }
}

void lisp_list::remove()
{
    if (status == s_constant)
        return;

    status &= ~s_protected;
    status -= (status && s_status());
    if (!status)
    {
        for (long i = 0; i < values.size(); i++)
            values[i]->remove();
        delete this;
    }
}
//--------------------------------------------------------------------------------
lisp_element *lisp_map::release()
{
    if (!status)
    {
        for (const auto &a : values)
            a.second->unmark();
        delete this;
        return lisp_nil;
    }
    return this;
}

void lisp_map::protect()
{
    if ((status & s_protect) == s_protect)
        return;

    status |= s_protect;
    for (const auto &a : values)
        a.second->protect();
}

void lisp_map::unprotect()
{
    if (status == s_constant)
        return;

    if ((status & s_protect) != s_protect)
        return;

    status &= ~s_protect;
    for (const auto &a : values)
        a.second->unprotect();
}

void lisp_map::unmark()
{
    status -= (status && s_status());
    if (!status)
    {
        for (const auto &a : values)
            a.second->unmark();
        delete this;
    }
}

void lisp_map::remove()
{
    if (status == s_constant)
        return;

    status &= ~s_protected;
    status -= (status && s_status());
    if (!status)
    {
        for (const auto &a : values)
            a.second->remove();
        delete this;
    }
}
//--------------------------------------------------------------------------------
lisp_element *lisp_atom::eval(lisp_mini *lisp)
{
    if (lisp->variables.back().find(code) != lisp->variables.back().end())
        return lisp->variables.back()[code];

    if (lisp->variables.size() > 1)
    {
        // it could be a global variable
        if (lisp->variables[0].find(code) != lisp->variables[0].end())
            return lisp->variables[0][code];
    }

    throw new lisp_error(lisp->atoms[code], lispunknownatom->message);
}
//------------------------------------------------------------------------
lisp_element *lisp_list::join(lisp_element *sep)
{
    string ssep;
    sep->stringvalue(ssep);

    string value;
    for (long i = 0; i < values.size(); i++)
    {
        if (i)
            value += ssep;
        values[i]->stringvalue(value);
    }
    return new lisp_string(value);
}

lisp_element *lisp_list::loop(lisp_mini *lisp, lisp_list *code, lisp_element *var)
{
    if (!var->is_atom())
        throw new lisp_error(var, lispunknownatom->message);
    uint16_t variable = var->code;

    long sz = code->size();
    lisp_element *r = lisp_nil;
    for (long i = 0; i < values.size(); i++)
    {
        lisp->insert(variable, values[i]);

        for (long j = 3; j < sz && r != lisp_break; j++)
        {
            r = r->release();
            r = code->values[j]->eval(lisp);
        }
        if (r == lisp_break)
            break;
    }
    return r;
}

lisp_element *lisp_string::loop(lisp_mini *lisp, lisp_list *code, lisp_element *var)
{
    if (!var->is_atom())
        throw new lisp_error(var, lispunknownatom->message);
    uint16_t variable = var->code;

    lisp_element *r = lisp_nil;
    lisp_string *v = new lisp_string("");
    lisp->insert(variable, v);

    long pos = 0;
    long sz = value.size();
    string localvalue;
    long szc = code->size();
    // we split the string into an array of characters
    while (pos < sz)
    {
        special_characters.getchar(value, localvalue, pos);
        v->value = localvalue;
        for (long j = 3; j < szc && r != lisp_break; j++)
        {
            r = r->release();
            r = code->values[j]->eval(lisp);
        }
        if (r == lisp_break)
            break;
    }
    return r;
}

lisp_element *lisp_map::loop(lisp_mini *lisp, lisp_list *code, lisp_element *var)
{
    if (!var->is_list() || var->size() != 2)
        throw new lisp_error(var, "Expecting a list of two atoms");

    if (!var->at(0)->is_atom() || !var->at(1)->is_atom())
        throw new lisp_error(var, "Elements should be atoms");

    uint16_t key = var->at(0)->code;
    uint16_t val = var->at(1)->code;

    lisp_element *r = lisp_nil;

    long sz = code->size();
    lisp_string *str = new lisp_string("");
    lisp->insert(key, str);
    lisp->insert(val, lisp_nil);
    for (const auto &a : values)
    {
        str->value = a.first;
        lisp->insert(val, a.second);
        for (long j = 3; j < sz && r != lisp_break; j++)
        {
            r = r->release();
            r = code->values[j]->eval(lisp);
        }
        if (r == lisp_break)
            break;
    }
    return r;
}

lisp_element *lisp_list::mapcar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();

    lisp_list l(s_constant);

    l.append(op);
    l.append(lisp_nil);
    try
    {
        for (long i = 0; i < values.size(); i++)
        {
            values[i]->mark();
            l.values[1] = values[i];
            result->append(l.eval(lisp));
            values[i]->unmark();
        }
    }
    catch (lisp_error *err)
    {
        l.values[1] = lisp_nil;
        l.clear();
        result->release();
        throw err;
    }
    l.values[1] = lisp_nil;
    l.clear();
    return result;
}

lisp_element *lisp_string::mapcar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list l(s_constant);

    lisp_string *v = new lisp_string("");

    l.append(op);
    l.append(v);

    lisp_list *result = new lisp_list();
    long pos = 0;
    long sz = value.size();
    string localvalue;
    // we split the string into an array of characters
    try
    {
        while (pos < sz)
        {
            special_characters.getchar(value, localvalue, pos);
            v->value = localvalue;
            result->append(l.eval(lisp));
        }
    }
    catch (lisp_error *err)
    {
        l.clear();
        result->release();
        throw err;
    }
    l.clear();
    return result;
}

lisp_element *lisp_map::mapcar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);
    lisp_string* v = new lisp_string("");
    l.append(op);
    l.append(v);
    l.append(lisp_nil);

    try
    {
        for (const auto &a : values)
        {
            v->value = a.first;
            a.second->mark();
            l.values[2] = a.second;
            result->append(l.eval(lisp));
            a.second->unmark();
        }
    }
    catch (lisp_error *err)
    {
        l.values[2] = lisp_nil;
        l.clear();
        result->release();
        throw err;
    }

    l.values[2] = lisp_nil;
    l.clear();
    return result;
}

lisp_element *lisp_list::filtercar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);
    lisp_element *r;

    l.append(op);
    l.append(lisp_nil);
    try
    {
        for (long i = 0; i < values.size(); i++)
        {
            values[i]->mark();
            l.values[1] = values[i];
            r = l.eval(lisp);
            if (r->boolean())
                result->append(values[i]);
            r->release();
            values[i]->unmark();
        }
    }
    catch (lisp_error *err)
    {
        l.values[1] = lisp_nil;
        l.clear();
        result->release();
        throw err;
    }
    l.values[1] = lisp_nil;
    l.clear();
    return result;
}

lisp_element *lisp_string::filtercar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list l(s_constant);

    lisp_string *v = new lisp_string("");

    l.append(op);
    l.append(v);

    lisp_element *r;
    lisp_list *result = new lisp_list();
    long pos = 0;
    long sz = value.size();
    string localvalue;
    // we split the string into an array of characters
    try
    {
        while (pos < sz)
        {
            special_characters.getchar(value, localvalue, pos);
            v->value = localvalue;
            r = l.eval(lisp);
            if (r->boolean())
                result->append(v->clone(false));
            r->release();
        }
    }
    catch (lisp_error *err)
    {
        l.clear();
        result->release();
        throw err;
    }
    l.clear();
    return result;
}

lisp_element *lisp_map::filtercar(lisp_mini *lisp, lisp_element *op)
{
    lisp_element *r;
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);
    lisp_string* v = new lisp_string("");
    l.append(op);
    l.append(v);
    l.append(lisp_nil);

    try
    {
        for (const auto &a : values)
        {
            v->value = a.first;
            a.second->mark();
            l.values[2] = a.second;
            r = l.eval(lisp);
            if (r->boolean())
                result->append(a.second);
            a.second->unmark();
        }
    }
    catch (lisp_error *err)
    {
        l.values[2] = lisp_nil;
        l.clear();
        result->release();
        throw err;
    }

    l.values[2] = lisp_nil;
    l.clear();
    return result;
}
//------------------------------------------------------------------------
// These are elements, which are never deleted and common to all lisps
//------------------------------------------------------------------------
lisp_element *lisp_nil = NULL;
lisp_element *lisp_true = NULL;
lisp_string *lisp_emptystring = NULL;
lisp_atom *lisp_break = NULL;

lisp_error *lisperror = NULL;
lisp_error *lispargnbserror = NULL;
lisp_error *lisptokenizeerror = NULL;
lisp_error *lisperrorrange = NULL;
lisp_error *lisperrordivided0 = NULL;
lisp_error *lispunknownatom = NULL;
lisp_error *lispunknownmethod = NULL;
lisp_error *lisp_end = NULL;
lisp_error *lispstackerror = NULL;
lisp_error *lisplambdaerror = NULL;

std::map<std::string, uint16_t> code_dictionary;
std::map<uint16_t, lisp_element *> instructions_dictionary;
std::map<uint16_t, std::string> string_dictionary;
//------------------------------------------------------------------------
uint16_t get_code(string &w)
{
    uint16_t c;
    if (code_dictionary.find(w) == code_dictionary.end())
    {
        c = code_dictionary.size();
        code_dictionary[w] = c;
        string_dictionary[c] = w;
    }
    else
        c = code_dictionary[w];
    return c;
}
//------------------------------------------------------------------------
lisp_mini::lisp_mini()
{
    initialisation_static_values();
    stop_execution = false;
    count_data = 0;
    std::map<uint16_t, lisp_element *> v;
    variables.push_back(v);
}

void lisp_mini::garbage_clean()
{
#ifdef DEBUGGER
    cerr << "GB:" << garbages.size() << endl;
#endif

    for (auto &v : variables)
    {
        for (auto &a : v)
        {
            a.second->remove();
        }
    }

    variables.clear();
    for (auto &a : atoms)
    {
        delete a.second;
    }
#ifdef DEBUGGER
    cerr << "GB:" << garbages.size() << endl;
#endif
}

//------------------------------------------------------------------------
bool lisp_mini::compile(lisp_element *program, vector<lisp_element *> &storage, long &pos, compile_action action)
{
    lisp_element *e;
    string current_key;
    long sz = infos.size();
    if (action == one_action)
        sz = pos + 1;

    for (; pos < sz; pos++)
    {
        if (action == error_action)
        {
            cerr << infos.types[pos] << endl;
            return false;
        }

        switch (infos.types[pos])
        {
        case jt_string:
        {
            if (action == map_key)
                current_key = infos.strings[pos];
            else
            {
                e = new lisp_string(storage, infos.strings[pos]);
                action = program->store(current_key, e, action);
            }
            break;
        }
        case jt_emptystring:
            action = program->store(current_key, lisp_emptystring, action);
            break;
        case jt_colon:
            action = map_value;
            break;
        case jt_keyword:
        {
            uint16_t c = get_code(infos.strings[pos]);
            if (c < l_final)
                e = instructions_dictionary[c];
            else
                e = get_atom(c);
            action = program->store(current_key, e, action);
            break;
        }
        case jt_quote:
        {
            e = new lisp_list(storage);
            e->append(instructions_dictionary[l_quote]);
            action = program->store(current_key, e, action);
            pos++;
            compile(e, storage, pos, one_action);
            pos--;
            break;
        }
        case jt_quote_list:
        {
            e = new lisp_list(storage);
            e->append(instructions_dictionary[l_quote]);
            action = program->store(current_key, e, action);
            pos++;
            compile(e, storage, pos, parenthetic_action);
            break;
        }
        case jt_number:
            if (action == map_key)
                current_key = infos.strings[pos];
            else
            {
                if (infos.strings[pos].find(".") != -1)
                    e = new lisp_float(storage, infos.numbers[pos]);
                else
                    e = new lisp_integer(storage, convertinginteger(infos.strings[pos]));
                action = program->store(current_key, e, action);
            }
            break;
        case jt_o_parenthesis:
        {
            if (action != first_action || program->size())
            {
                e = new lisp_list(storage);
                action = program->store(current_key, e, action);
            }
            else
                e = program;
            pos++;
            if (!compile(e, storage, pos, next_action))
                return false;
            if (action == parenthetic_action)
                return true;
            break;
        }
        case jt_c_parenthesis:
            return true;
        case jt_bracket:
        {
            lisp_list *l = new lisp_list(storage);
            l->append(instructions_dictionary[l_command]);
            l->append(new lisp_unix(storage, infos.strings[pos]));
            action = program->store(current_key, l, action);
            break;
        }
        case jt_o_brace:
        {
            lisp_map *m = new lisp_map(storage);
            action = program->store(current_key, m, action);
            pos++;
            if (!compile(m, storage, pos, map_key))
                return false;
            break;
        }
        case jt_c_brace:
            return true;
        default:
            cerr << infos.types[pos] << endl;
        }
    }
}
//-------------------------------------------------------------------------------------

lisp_element *lisp_list::car()
{
    if (values.size())
        return values[0];
    return lisp_nil;
}

lisp_element *lisp_list::cdr()
{
    if (!values.size())
        return lisp_nil;

    lisp_list *l = new lisp_list();
    for (long i = 1; i < values.size(); i++)
        l->append(values[i]);
    return l;
}

lisp_element *lisp_list::sub(double b, double e)
{
    if (e <= 0)
        e = values.size() + e;
    if (b < 0)
        b = values.size() + b;

    if (e <= b)
        return lisp_nil;

    lisp_list *l = new lisp_list();
    for (; b < e; b++)
        l->append(values[b]);
    return l;
}

lisp_element *lisp_list::execute_lambda(lisp_mini *lisp, lisp_element *lmbd)
{
    if (lmbd->size() && lmbd->at(0)->code == l_lambda)
    {
        // then we have ((lambda (p1 .. p2) code) a1..a2)
        // e is (lambda (p1 .. p2 ) code)
        if (lmbd->size() < 3 || lmbd->at(1)->size() != values.size() - 1)
            throw new lisp_error(this, lispargnbserror->message);
        // first we push a new stack element
        lisp_element *r;
        lisp_element *a;
        lisp_element *p;
        long i;
        std::map<uint16_t, lisp_element *> local_vars;
        r = lmbd->at(1);
        // We prepare a place where to store our values
        try
        {
            for (i = 0; i < r->size(); i++)
            {
                p = r->at(i);
                if (!p->is_atom())
                    throw new lisp_error(this, lispunknownatom->message);
                a = values[i + 1]->eval(lisp);
                // we keep our variables in a local map
                // in order to access the local variables
                local_vars[p->code] = a;
                a->mark();
            }
        }
        catch (lisp_error *err)
        {
            for (auto &e : local_vars)
                e.second->unmark();
            throw err;
        }
        // Then we push our local variables on the stack
        lisp->stack_variables_on(local_vars);
        // We then execute our lambda
        r = lisp_nil;
        for (i = 2; i < lmbd->size(); i++)
        {
            r = r->release();
            r = lmbd->at(i)->eval(lisp);
        }
        // then we clean our stack and we keep the last value
        // that was returned...
        lisp->stack_off(r);
        return r;
    }
    throw new lisp_error(this, lispunknownmethod->message);
}

lisp_element *lisp_list::execute_function(lisp_mini *lisp, lisp_element *function)
{
    lisp_element *e = values[0];
    if (!function->is_list() || function->size() < 4 || function->at(0)->code != l_defun)
        throw new lisp_error(this, lispunknownmethod->message);

    // We are executing a function
    // First we need to initialize the arguments
    lisp_element *args = function->at(2);
    if (args->size() != values.size() - 1)
        throw new lisp_error(this, lispargnbserror->message);

    lisp_element *a;
    lisp_element *p;
    long i;
    std::map<uint16_t, lisp_element *> local_vars;

    // We prepare our garbage environment
    try
    {
        for (i = 0; i < args->size(); i++)
        {
            p = args->at(i);
            if (!p->is_atom())
                throw new lisp_error(this, lispunknownatom->message);
            a = values[i + 1]->eval(lisp);
            // we keep our variables in a local map
            // in order to access the local variables
            local_vars[p->code] = a;
            a->mark();
        }
    }
    catch (lisp_error *err)
    {
        // In case of error, we clean our garbage
        for (auto &e : local_vars)
            e.second->unmark();
        throw err;
    }

    // Then we push our local variables on the stack
    lisp->stack_variables_on(local_vars);
    a = lisp_nil;
    for (i = 3; i < function->size(); i++)
    {
        a->release();
        a = function->at(i)->eval(lisp);
    }
    lisp->stack_off(a);
    return a;
}

lisp_element *lisp_mini::run(string code)
{
    infos.clear();
    error_tokenize e = code_segmenting(code, infos, &special_characters);
    if (e != e_no_error)
        return lisptokenizeerror->eval(this);

    vector<lisp_element *> storage;
    long pos = 0;
    lisp_list *program = new lisp_list(storage);
    compile(program, storage, pos, first_action);
    stop_execution = false;
    lisp_element *res;
    try
    {
        res = program->eval(this);
        res->protect();
        clean(storage);
        res->unprotect();
        return res;
    }
    catch (lisp_error *err)
    {
        for (auto &a : storage)
            delete a;
        throw err;
    }
}

lisp_mini *create_mini_lisp_instance()
{
    return new lisp_mini();
}

string execute_some_lisp(lisp_mini *lisp, string &code)
{
    if (lisp == NULL)
        return "Error: lisp not initialized";

    lisp->infos.clear();
    error_tokenize e = code_segmenting(code, lisp->infos, &special_characters);
    std::stringstream os;

    if (e != e_no_error)
    {
        lisptokenizeerror->asstring(os);
        os << ":" << e;
        return os.str();
    }

    vector<lisp_element *> storage;
    lisp_list *program = new lisp_list(storage);
    long pos = 0;
    lisp->compile(program, storage, pos, first_action);
    lisp_element *res = lisp_nil;
    try
    {
        lisp->stop_execution = false;
        res = program->eval(lisp);
        res->asstring(os);
        res->release();
    }
    catch (lisp_error *l)
    {
        l->asstring(os);
        l->release();
    }

    lisp->clean(storage);

#ifdef DEBUGGER
    cerr << "GB:" << garbages.size() << endl;
#endif
    return os.str();
}
