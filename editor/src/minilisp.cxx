#include "minilisp.h"

// We want lists, numbers and dictionary
//------------------------------------------------------------------------
lisp_element *lisp_element::methodBase(lisp_mini *lisp, lisp_element *v_base, bool toconvert)
{
    static vector<u_ustring> caracs;
    static std::unordered_map<u_uchar, long> mcaracs;
    u_ustring w;
    long n, b;

    b = v_base->longvalue();
    if (b <= 1)
        throw new lisp_error(this, "Error: cannot convert to this base");

    if (caracs.size() == 0)
    {
        w = '0';
        for (n = 0; n < 10; n++)
        {
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w[0]++;
        }
        w = 'A';
        for (n = 10; n < 36; n++)
        {
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w[0]++;
        }
        w = 'a';
        for (n = 36; n < 62; n++)
        {
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w[0]++;
        }
        w = '#';
        mcaracs[w[0]] = caracs.size();
        caracs.push_back(w);
        w = '@';
        mcaracs[w[0]] = caracs.size();
        caracs.push_back(w);
        w = U"";
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
        u_ustring res = U"";
        while (v)
        {
            rest = v % b;
            v /= b;
            res = caracs[rest] + res;
        }
        return new lisp_string(res);
    }

    w = U"";
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

void displaygarbagesize()
{
    cerr << "GB:" << garbages.size() << endl;
}
#endif

lisp_element::lisp_element(uint16_t c) : code(c)
{
#ifdef DEBUGGER
    idx = garbages.size();
    garbages.insert(this);
#endif
    status = 0;
}

lisp_element::lisp_element(std::vector<lisp_element *> &storage, uint16_t c) : status(s_protected), code(c)
{
#ifdef DEBUGGER
    idx = garbages.size();
    garbages.insert(this);
#endif
    storage.push_back(this);
}

lisp_element::~lisp_element()
{
#ifdef DEBUGGER
    garbages.erase(this);
#endif
}

//-------------------------------------------------------------------------------------

lisp_element *lisp_element::replace(lisp_element *a, lisp_element *v)
{
    return lisp_emptystring;
}

lisp_element *lisp_element::join(lisp_element *sep)
{
    return lisp_emptystring;
}

lisp_element *lisp_element::sub(long b, long e)
{
    throw new lisp_error(this, lisperror->message);
}

void lisp_element::concatenate(lisp_element *r)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::cons_apply(lisp_element *op)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::append(lisp_element *e)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::put(lisp_element *k, lisp_element *e)
{
    throw new lisp_error(this, lisperror->message);
}

void lisp_element::pop(lisp_element *e)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::insert(lisp_element *k, lisp_element *e)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::append(u_ustring &k, lisp_element *e)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::push_first(lisp_element *e)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::car()
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::cadar(lisp_cadar*)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::cdr()
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::command(lisp_mini *lisp)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::plus(lisp_element *v)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::minus(lisp_element *v)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::multiply(lisp_element *v)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::divide(lisp_element *v)
{
    throw new lisp_error(this, lisperror->message);
}

lisp_element *lisp_element::mod(lisp_element *v)
{
    throw new lisp_error(this, lisperror->message);
}
//-------------------------------------------------------------------------------------
compile_action lisp_element::store(string &key, lisp_element *e, compile_action action)
{
    u_ustring current_key;
    s_utf8_to_unicode(current_key, key, key.size());

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
lisp_element *lisp_unix::command(lisp_mini *lisp)
{
    string result;
    s_unicode_to_utf8(result, value);
    result = lisp->execute_unix_command(result);
    return new lisp_string(result);
}

lisp_element *lisp_string::find(lisp_element *v)
{
    u_ustring val;
    v->stringvalue(val);
    return new lisp_integer(value.find(val));
}

lisp_element *lisp_string::replace(lisp_element *a, lisp_element *v)
{
    u_ustring to_be_replaced;
    u_ustring with;

    a->stringvalue(to_be_replaced);
    v->stringvalue(with);

    with = s_ureplacestring(value, to_be_replaced, with);
    return new lisp_string(with);
}

lisp_element *lisp_string::split(lisp_element *search)
{
    u_ustring search_string;
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
                if (search_string != U"")
                {
                    result->append(new lisp_string(search_string));
                    search_string = U"";
                }
            }
            else
                search_string += c;
        }
        if (search_string != U"")
            result->append(new lisp_string(search_string));
        return result;
    }

    search->stringvalue(search_string);

    u_ustring localvalue;
    long pos = 0;

    if (search_string == U"")
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
            if (localvalue != U"")
            {
                result->append(new lisp_string(localvalue));
            }
            pos = found + search_string.size();
        }
        else
            break;
    }

    localvalue = value.substr(pos, value.size() - pos);
    if (localvalue != U"")
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
        value = s_uleft(value, i);
    else
        value = s_uleft(value, i) + s_uright(value, sz - i - 1);
}

lisp_element *lisp_string::at_position(lisp_element *e)
{
    long i = e->longvalue();
    if (i >= value.size())
        return lisp_emptystring;
    return new lisp_string(value[i]);
}

lisp_element *lisp_string::at(long i)
{
    if (i >= value.size())
        return lisp_emptystring;
    return new lisp_string(value[i]);
}

lisp_element *lisp_string::put(lisp_element *k, lisp_element *v)
{
    long key = k->longvalue();
    if (key < 0)
        throw new lisp_error(this, "out of range");

    u_ustring val;
    v->stringvalue(val);
    long sz = value.size();

    if (key >= sz)
        value += val;
    else
    {
        if (val.size() == 1)
            value[key] = val[0];
        else
        {
            value = s_uleft(value, key) + val + s_uright(value, sz - key - 1);
        }
    }
    return this;
}

lisp_element *lisp_string::insert(lisp_element *k, lisp_element *v)
{
    long key = k->longvalue();
    if (key < 0)
        throw new lisp_error(this, "out of range");

    u_ustring val;
    v->stringvalue(val);
    long sz = value.size();

    if (key >= sz)
        value += val;
    else
    {
        if (val.size() == 1)
            value[key] = val[0];
        else
        {
            value = s_uleft(value, key) + val + s_uright(value, sz - key);
        }
    }
    return this;
}

lisp_element *lisp_string::sub(long b, long e)
{
    long lg = value.size();
    if (e <= 0)
        e = lg + e;
    if (b < 0)
        b = lg + b;
    if (e <= b)
        return lisp_emptystring;
    return new lisp_string(s_umiddle(value, b, e - b));
}

lisp_element *lisp_string::car()
{
    if (!value.size())
        return lisp_emptystring;
    return new lisp_string(value[0]);
}

lisp_element *lisp_string::cadar(lisp_cadar* c) {
    u_ustring v = value;
    
    for (long i = c->action.size()-1; i>= 0 && v.size(); i--) {
        if (c->action[i] == 'a')
            v = v[0];
        else            
            v = s_uright(v, v.size() - 1);
    }
    if (v== U"")
        return lisp_emptystring;
    return new lisp_string(v);
}

lisp_element *lisp_string::cdr()
{
    if (!value.size())
        return lisp_emptystring;

    return new lisp_string(s_uright(value, value.size() - 1));
}

//--------------------------------------------------------------------------------
lisp_element *long_range(lisp_mini *lisp, long init, long limit, long inc)
{
    long d = (limit - init) / inc;
    if (d < 0)
        d *= -1;

    if (init > limit && inc > 0)
        inc *= -1;

    if (d <= 100000)
    {
        if (inc == 0)
            return lisp_nil;

        // Integers ?
        lisp_list *range_list = new lisp_list();
        range_list->values.reserve((long)d);
        if (inc > 0)
        {
            for (long i = init; i < limit; i += inc)
            {
                range_list->append(new lisp_integer(i));
            }
        }
        else
        {
            for (long i = init; i > limit; i += inc)
                range_list->append(new lisp_integer(i));
        }
        return range_list;
    }
    throw new lisp_error(lisp_nil, "Error: Exceeding range");
}

lisp_element *double_range(lisp_mini *lisp, double init, double limit, double inc)
{
    double d = (limit - init) / inc;
    if (d < 0)
        d *= -1;

    if (init > limit && inc > 0)
        inc *= -1;

    if (d <= 100000)
    {
        if (inc == 0)
            return lisp_nil;

        lisp_list *range_list = new lisp_list();
        range_list->values.reserve((long)d);
        if (inc > 0)
        {
            for (double i = init; i < limit; i += inc)
            {
                range_list->append(new lisp_float(i));
            }
        }
        else
        {
            for (double i = init; i > limit; i += inc)
                range_list->append(new lisp_float(i));
        }
        return range_list;
    }
    throw new lisp_error(lisp_nil, "Error: Exceeding range");
}

lisp_element *lisp_list::range(lisp_mini *lisp)
{

    lisp_element *e1 = values[1]->eval(lisp);
    lisp_element *e2 = values[2]->eval(lisp);
    lisp_element *e3 = values[3]->eval(lisp);

    lisp_element *e;

    if (size() == 5)
    {
        try
        {
            if (e1->is_integer() && e2->is_integer() && e3->is_integer())
                e = long_range(lisp, e1->longvalue(), e2->longvalue(), e3->longvalue());
            else
                e = double_range(lisp, e1->doublevalue(), e2->doublevalue(), e3->doublevalue());
        }
        catch (lisp_error *err)
        {
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

    if (size() == 4)
    {
        if (e1->is_integer() && e2->is_integer() && e3->is_integer())
            e = new lisp_long_range(e1->longvalue(), e2->longvalue(), e3->longvalue());
        else
            e = new lisp_double_range(e1->doublevalue(), e2->doublevalue(), e3->doublevalue());
        e1->release();
        e2->release();
        e3->release();
        return e;
    }

    throw new lisp_error(this, lispargnbserror->message);
}
//--------------------------------------------------------------------------------
bool compareElementsSup(lisp_element *e1, lisp_element *e2)
{
    return e1->sup(e2);
}

bool compareElementsInf(lisp_element *e1, lisp_element *e2)
{
    return e1->inf(e2);
}

lisp_element *lisp_list::sort(bool direction)
{
    if (direction)
        std::sort(values.begin(), values.end(), compareElementsSup);
    else
        std::sort(values.begin(), values.end(), compareElementsInf);
    return this;
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
    status -= (status && status_not_constant());
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
    status -= (status && status_not_constant());
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
    status -= (status && status_not_constant());
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
    status -= (status && status_not_constant());
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
    if (lisp->check_atom(code))
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
    u_ustring ssep;
    sep->stringvalue(ssep);

    u_ustring value;
    for (long i = 0; i < values.size(); i++)
    {
        if (i)
            value += ssep;
        values[i]->stringvalue(value);
    }
    return new lisp_string(value);
}
//------------------------------------------------------------------------
lisp_element *lisp_long_range::loop(lisp_mini *lisp, lisp_list *code, lisp_element *var)
{
    if (!var->is_atom())
        throw new lisp_error(var, lispunknownatom->message);
    uint16_t variable = var->code;
    lisp_integer *li = new lisp_integer(0);
    lisp->insert(variable, li);
    long sz = code->size();
    lisp_element *r = lisp_nil;
    if (inc > 0)
    {
        for (long i = init; i < limit; i += inc)
        {
            li->value = i;
            for (long j = 3; j < sz && r != lisp_break; j++)
            {
                r = r->release();
                r = code->values[j]->eval(lisp);
            }
            if (r == lisp_break)
            {
                r = lisp_nil;
                break;
            }
        }
    }
    else
    {
        for (long i = init; i > limit; i += inc)
        {
            li->value = i;
            for (long j = 3; j < sz && r != lisp_break; j++)
            {
                r = r->release();
                r = code->values[j]->eval(lisp);
            }
            if (r == lisp_break)
            {
                r = lisp_nil;
                break;
            }
        }
    }
    return r;
}

lisp_element *lisp_double_range::loop(lisp_mini *lisp, lisp_list *code, lisp_element *var)
{
    if (!var->is_atom())
        throw new lisp_error(var, lispunknownatom->message);
    uint16_t variable = var->code;
    lisp_float *li = new lisp_float(0);
    lisp->insert(variable, li);
    long sz = code->size();
    lisp_element *r = lisp_nil;
    if (inc > 0)
    {
        for (double i = init; i < limit; i += inc)
        {
            li->value = i;
            for (long j = 3; j < sz && r != lisp_break; j++)
            {
                r = r->release();
                r = code->values[j]->eval(lisp);
            }
            if (r == lisp_break)
            {
                r = lisp_nil;
                break;
            }
        }
    }
    else
    {
        for (double i = init; i > limit; i += inc)
        {
            li->value = i;
            for (long j = 3; j < sz && r != lisp_break; j++)
            {
                r = r->release();
                r = code->values[j]->eval(lisp);
            }
            if (r == lisp_break)
            {
                r = lisp_nil;
                break;
            }
        }
    }
    return r;
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
        {
            r = lisp_nil;
            break;
        }
    }
    return r;
}

lisp_element *lisp_string::loop(lisp_mini *lisp, lisp_list *code, lisp_element *var)
{
    if (!var->is_atom())
        throw new lisp_error(var, lispunknownatom->message);
    uint16_t variable = var->code;

    lisp_element *r = lisp_nil;
    lisp_string *v = new lisp_string(U"");
    lisp->insert(variable, v);

    long pos = 0;
    long sz = value.size();
    u_ustring localvalue;
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
        {
            r = lisp_nil;
            break;
        }
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
        {
            r = lisp_nil;
            break;
        }
    }
    return r;
}

lisp_element *lisp_long_range::mapcar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);

    lisp_integer *li = new lisp_integer(0);
    l.append(op);
    l.append(li);

    try
    {
        if (inc > 0)
        {
            for (long i = init; i < limit; i += inc)
            {
                li->value = i;
                result->append(l.eval(lisp));
            }
        }
        else
        {
            for (long i = init; i > limit; i += inc)
            {
                li->value = i;
                result->append(l.eval(lisp));
            }
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

lisp_element *lisp_double_range::mapcar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);

    lisp_float *li = new lisp_float(0);
    l.append(op);
    l.append(li);

    try
    {
        if (inc > 0)
        {
            for (double i = init; i < limit; i += inc)
            {
                li->value = i;
                result->append(l.eval(lisp));
            }
        }
        else
        {
            for (double i = init; i > limit; i += inc)
            {
                li->value = i;
                result->append(l.eval(lisp));
            }
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

    lisp_string *v = new lisp_string(U"");

    l.append(op);
    l.append(v);

    lisp_list *result = new lisp_list();
    long pos = 0;
    long sz = value.size();
    u_ustring localvalue;
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
    lisp_string *v = new lisp_string("");
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

lisp_element *lisp_long_range::filtercar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);

    lisp_integer *li = new lisp_integer(0);
    l.append(op);
    l.append(li);

    lisp_element *r;
    try
    {
        if (inc > 0)
        {
            for (long i = init; i < limit; i += inc)
            {
                li->value = i;
                r = l.eval(lisp);
                if (r->boolean())
                    result->append(new lisp_integer(i));
                r->release();
            }
        }
        else
        {
            for (long i = init; i > limit; i += inc)
            {
                li->value = i;
                r = l.eval(lisp);
                if (r->boolean())
                    result->append(new lisp_integer(i));
                r->release();
            }
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

lisp_element *lisp_double_range::filtercar(lisp_mini *lisp, lisp_element *op)
{
    lisp_list *result = new lisp_list();
    lisp_list l(s_constant);

    lisp_float *li = new lisp_float(0);
    l.append(op);
    l.append(li);

    lisp_element *r;

    try
    {
        if (inc > 0)
        {
            for (double i = init; i < limit; i += inc)
            {
                li->value = i;
                r = l.eval(lisp);
                if (r->boolean())
                    result->append(new lisp_float(i));
                r->release();
            }
        }
        else
        {
            for (double i = init; i > limit; i += inc)
            {
                li->value = i;
                r = l.eval(lisp);
                if (r->boolean())
                    result->append(new lisp_float(i));
                r->release();
            }
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

    lisp_string *v = new lisp_string(U"");

    l.append(op);
    l.append(v);

    lisp_element *r;
    lisp_list *result = new lisp_list();
    long pos = 0;
    long sz = value.size();
    u_ustring localvalue;
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
    lisp_string *v = new lisp_string("");
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

//-------------------------------------------------------------------------------------

lisp_element *lisp_list::car()
{
    if (values.size())
        return values[0];
    return lisp_nil;
}

lisp_element *lisp_list::cadar(lisp_cadar* c) {
    lisp_element* v = this;
    lisp_element* previous = this;
    
    for (long i = c->action.size()-1; i>= 0 && v->size(); i--) {
        if (c->action[i] == 'a')
            v = previous->car();
        else            
            v = previous->cdr();
        if (previous != this)
            previous->release();
        previous = v;
    }
    return v;
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

lisp_element *lisp_list::sub(long b, long e)
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

