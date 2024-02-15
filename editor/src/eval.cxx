#include "minilisp.h"

//------------------------------------------------------------------------

u_ustring get_label(uint16_t);
long gcd_math(long a, long b)
{
    // Everything divides 0
    if (a == 0)
        return b;
    if (b == 0)
        return a;
    // base case
    if (a == b)
        return a;
    // a is greater
    if (a > b)
        return gcd_math(a - b, b);
    return gcd_math(a, b - a);
}

long hcf_math(long x, long y)
{
    return (!y) ? x : hcf_math(y, x % y);
}
//-----------------------------------------------------------------
class stack_action
{
public:
    bool pop;
    lisp_mini *lisp;
    stack_action(lisp_mini *l, uint16_t c)
    {
        pop = true;
        l->stack.push_back(c);
        lisp = l;
    }
    ~stack_action()
    {
        if (pop)
            lisp->stack.pop_back();
    }

    void disable()
    {
        pop = false;
        lisp->stack.pop_back();
    }
};
//-----------------------------------------------------------------
lisp_element *lisp_list::eval(lisp_mini *lisp)
{
    lisp->check_stop();
    long sz = values.size();
    if (!sz)
        return this;

    lisp_element *e = values[0];
    lisp_element *r = lisp_nil;
    stack_action st(lisp, e->code);

    try
    {
        switch (e->code)
        {
        case l_append: //(append pathname txt)
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp->append_file(e, r);
            e->release();
            r->release();
            return lisp_true;
        }
        case l_apply: //(apply 'operator list)
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (!e->is_atom())
                throw new lisp_error(this, "Error: first element shoud be an atom");
            r = values[2]->eval(lisp);
            if (!r->is_list())
                throw new lisp_error(this, "Error: second element shoud be a list");
            e = r->cons_apply(e);
            r = r->release();
            r = e->eval(lisp);
            e->release();
            return r;
        }
        case l_at:
        { //(at e pos)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *v = e->at_position(r);
            r->release();
            v->protect();
            e->release();
            v->unprotect();
            return v;
        }
        case l_atom:
        { //(atom str)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_atom())
                return e;
            u_ustring a;
            e->stringvalue(a);
            r = lisp->get_atom(a);
            e->release();
            return r;
        }
        case l_atomp:
        { //(atom? e)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_atom())
                return lisp_true;
            return lisp_nil;
        }
        case l_base:
        { //(base value base conversion), if conversion is true base->base10 else base10->base
            bool conversion = false;
            if (sz == 4)
            {
                r = values[3]->eval(lisp);
                conversion = r->boolean();
                r->release();
            }
            else
            {
                if (sz != 3)
                    throw new lisp_error(this, lispargnbserror->message);
            }

            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->methodBase(lisp, r, conversion);
            e->release();
            r->release();
            return res;
        }
        case l_block:
        {
            for (long i = 1; i < sz - 1; i++)
            {
                r = r->release();
                r = values[i]->eval(lisp);
            }

            // For the last instruction of the block
            // We clean the stack in order to detect tail calls.
            st.disable();
            r->release();
            return values[sz - 1]->eval(lisp);
        }
        case l_car: //(car l)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->car();
            r->mark();
            e->release();
            r->demark();
            return r;
        case l_cadar:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->cadar((lisp_cadar*)values[0]);
            r->mark();
            r->release();
            r->demark();
            return r;
        case l_cdr: //(cdr l)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->cdr();
            r->mark();
            e->release();
            r->demark();
            return r;
        case l_chr: //(chr code)
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            long c = e->longvalue();
            e->release();
            return new lisp_string(cs_unicode_to_utf8(c));
        }
        case l_command: //[unix command] example: (print [ls -l])
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->command(lisp);
            e->release();
            return r;
        case l_cond: //(cond ((test) code) ((test) code) ...)
        {
            for (long i = 1; i < sz; i++)
            {
                e = values[i];
                if (e->is_list() && e->size() == 2)
                {
                    r = e->at(0)->eval(lisp);
                    bool b = r->boolean();
                    r = r->release();
                    if (b)
                    {
                        // In a cond, a potential tail call is possible
                        st.disable();
                        return e->at(1)->eval(lisp);
                    }
                }
            }
            return lisp_nil;
        }
        case l_cons: //(cons e l)
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            if (!r->is_list())
            {
                lisp_list *l = new lisp_list();
                l->append(e);
                l->append(r);
                return l;
            }
            r->push_first(e);
            return r;
        }
        case l_consp: //(cons? l)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->is_list() ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_defun: //(defun name(p1 p2..) code)
            // We record a function name
            if (sz < 4)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1];
            if (!e->is_atom())
                return lispunknownatom->eval(lisp);
            lisp->store_atom(e->code, this);
            return e;
        case l_different:
        { //(!= e v)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->equal(r) ? lisp_nil : lisp_true;
            e->release();
            r->release();
            return res;
        }
        case l_divide:
        { //(/ e v)
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp)->clone(false);
            for (long i = 2; i < sz; i++)
            {
                r = values[i]->eval(lisp);
                e->divide(r);
                r = r->release();
            }
            return e;
        }
        case l_eq:
        { //(eq e val)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->eq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_equal:
        { //(equal e val)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->equal(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_eval:
        { //(eval expression)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->code == v_string)
            {
                // We need to compile it...
                string s;
                s_unicode_to_utf8(s, ((lisp_string *)e)->value);
                r = lisp->run(s);
                e->release();
                return r;
            }
            // this is a list
            r = e->eval(lisp);
            e->release();
            return r;
        }
        case l_filtercar:
        { //(filter 'operator list)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            e = r->filtercar(lisp, e);
            r->release();
            return e;
        }
        case l_find:
        { //(find ct val)
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->find(r);
            e->release();
            r->release();
            return res;
        }
        case l_float:
        { //(float v)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_float())
                return e;
            if (e->is_integer())
            {
                r = new lisp_float(e->doublevalue());
                e->release();
                return r;
            }
            u_ustring v;
            e->stringvalue(v);
            double d = convertingfloathexa(STR(v));
            e->release();
            return new lisp_float(d);
        }
        case l_if:
        { //(if pred then else)
            if (sz != 3 && sz != 4)
                throw new lisp_error(this, lispargnbserror->message);

            e = values[1]->eval(lisp);
            if (e->boolean())
            {
                e = e->release();
                if (sz < 3)
                    throw new lisp_error(this, lispargnbserror->message);
                // This is a potential tail call, we clean the stack from its "if" value
                st.disable();
                return values[2]->eval(lisp);
            }
            else
            {
                e = e->release();
                if (sz != 4)
                    throw new lisp_error(this, lispargnbserror->message);
                // This is a potential tail call, we clean the stack from its "if" value
                st.disable();
                return values[3]->eval(lisp);
            }
        }
        case l_inf:
        { //(< e1 e2)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->inf(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_infeq:
        { //(<= e1 e2)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->infeq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_insert:
        { //(insert cts key value)
            if (sz != 4)
                throw new lisp_error(this, lispargnbserror->message);

            e = values[1]->eval(lisp);
            e = e->clone(true);
            r = values[2]->eval(lisp);
            lisp_element *v = values[3]->eval(lisp);
            e->insert(r, v);
            r->release();
            v->release();
            return e;
        }
        case l_integer:
        { //(integer x)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_integer())
                return e;
            if (e->is_float())
            {
                r = new lisp_integer(e->longvalue());
                e->release();
                return r;
            }
            u_ustring v;
            e->stringvalue(v);
            long d = convertinginteger(v);
            e->release();
            return new lisp_integer(d);
        }
        case l_join:
        { //(join lst sep)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->join(r);
            e->release();
            r->release();
            return res;
        }
        case l_label: //(label e code)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1];
            if (!e->is_atom())
                throw new lisp_error(this, lispunknownatom->message);
            r = values[2]->eval(lisp);
            lisp->insert_bottom(e->code, r);
            return r;
        case l_lambda:
            return this;
        case l_list: //(list a1 a2 ...)
        {
            r = new lisp_list();
            for (long i = 1; i < sz; i++)
            {
                e = values[i]->eval(lisp)->clone(true);
                r->append(e);
            }
            return r;
        }
        case l_loop: // (loop a lst code)
        {
            if (sz < 4)
                throw new lisp_error(this, lispargnbserror->message);

            e = values[2]->eval(lisp);
            r = e->loop(lisp, this, values[1]);
            e->release();
            return r;
        }
        case l_lower:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            u_ustring v;
            e->stringvalue(v);
            v = special_characters.u_to_lower(v);
            e->release();
            return new lisp_string(v);
        }
        case l_map:
        { //(map (key value) (key value) ..)
            e = new lisp_map();
            lisp_element *v;
            for (long i = 1; i < sz; i++)
            {
                if (values[i]->code != v_list || values[i]->size() != 2)
                    throw new lisp_error(this, lispargnbserror->message);
                r = values[i]->at(0)->eval(lisp);
                v = values[i]->at(1)->eval(lisp)->clone(true);
                e->put(r, v);
                r = r->release();
                v->release();
            }
            return e;
        }
        case l_mapcar:
        { //(mapcar 'operator list)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            e = r->mapcar(lisp, e);
            r->release();
            return e;
        }
        case l_minus:
        { //(- a1 a2 ...)
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp)->clone(false);
            for (long i = 2; i < sz; i++)
            {
                r = values[i]->eval(lisp);
                e->minus(r);
                r = r->release();
            }
            return e;
        }
        case l_mod:
        { //(% a1 a2 ..)
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp)->clone(false);
            for (long i = 2; i < sz; i++)
            {
                r = values[i]->eval(lisp);
                e->mod(r);
                r = r->release();
            }
            return e;
        }
        case l_multiply:
        { //(* a1 a2 ..)
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp)->clone(false);
            for (long i = 2; i < sz; i++)
            {
                r = values[i]->eval(lisp);
                e->multiply(r);
                r = r->release();
            }
            return e;
        }
        case l_nconc:
        { //(nconc l ll)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp)->clone(true);
            r = values[2]->eval(lisp);
            e->concatenate(r);
            r->release();
            return e;
        }
        case l_neq:
        { //(neq e val)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->neq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_not: //(not v)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->boolean() ? lisp_nil : lisp_true;
            e->release();
            return r;
        case l_nullp: //(null? value)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e == lisp_nil ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_numberp: //(number? value)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->is_number() ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_ord: //(ord str)
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            long pos = 0;
            u_ustring value;
            e = values[1]->eval(lisp);
            e->stringvalue(value);
            e = e->release();
            long sz = value.size();
            lisp_list *result = new lisp_list();
            UWCHAR v;
            // we split the string into an array of characters
            while (pos < sz)
            {
                v = getonechar(USTR(value), pos);
                result->append(new lisp_integer(v));
                pos++;
            }
            return result;
        }
        case l_plus:
        { //(+ a1 a2 ..)
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp)->clone(false);
            for (long i = 2; i < sz; i++)
            {
                r = values[i]->eval(lisp);
                e->plus(r);
                r = r->release();
            }
            return e;
        }
        case l_pop:
        { //(pop lst k)
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (sz == 2)
                e->pop(lisp_nil);
            else
            {
                r = values[2]->eval(lisp);
                e->pop(r);
                r->release();
            }

            return e;
        }
        case l_print:
        {
            stringstream os;
            for (long i = 1; i < sz; i++)
            {
                e = values[i]->eval(lisp);
                e->string_to_os(os);
                e = e->release();
            }
            cerr << os.str();
            return lisp_emptystring;
        }
        case l_println:
        {
            stringstream os;
            for (long i = 1; i < sz; i++)
            {
                if (i != 1)
                    os << " ";
                e = values[i]->eval(lisp);
                e->string_to_os(os);
                e = e->release();
            }
            cerr << os.str() << endl;
            return lisp_emptystring;
        }
        case l_push:
        { //(push lst v)
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (!e->is_list())
                throw new lisp_error(this, lisperror->message);
            r = values[2]->eval(lisp);
            e = e->append(r);
            return e;
        }
        case l_put:
        { //(put cts key value)
            if (sz != 4)
                throw new lisp_error(this, lispargnbserror->message);

            e = values[1]->eval(lisp);
            e = e->clone(true);
            r = values[2]->eval(lisp);
            lisp_element *v = values[3]->eval(lisp);
            e->put(r, v);
            r->release();
            v->release();
            return e;
        }
        case l_quote:
            return values[1];
        case l_range: //(range init limit increment)
            return range(lisp);
        case l_read: //(read pathname)
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            string code = lisp->read_file(e);
            e->release();
            return new lisp_string(code);
        }
        case l_replace:
        { //(replace s a v) replace a in s with v
            if (sz != 4)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *w = values[3]->eval(lisp);
            lisp_element *res = e->replace(r, w);
            e->release();
            r->release();
            w->release();
            return res;
        }
        case l_setq: //(setq n value)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1];
            if (!e->is_atom())
                throw new lisp_error(this, lispunknownatom->message);
            r = values[2]->eval(lisp);
            lisp->insert(e->code, r);
            return r;
        case l_size: //(size e)
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = new lisp_float(e->size());
            e->release();
            return r;
        case l_sort:
        { //(sort lst true/nil)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);

            r = values[2]->eval(lisp);
            bool drt = r->boolean();
            e = values[1]->eval(lisp);
            r->release();
            return e->sort(drt);
        }
        case l_split:
        { //(split e splitter)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *s = e->split(r);
            r->release();
            e->release();
            return s;
        }
        case l_string: //(string e)
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->code == v_string)
                return e;
            u_ustring v;
            e->stringvalue(v);
            e->release();
            return new lisp_string(v);
        }
        case l_stringp: //(string? v)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->code == v_string ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_sub: //(sub s beg (end))
        {
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            long beg = r->longvalue();
            r = r->release();
            long end = 0;
            if (sz == 4)
            {
                r = values[3]->eval(lisp);
                end = r->longvalue();
                r = r->release();
            }
            r = e->sub(beg, end);
            e->release();
            return r;
        }
        case l_sup:
        { //(> e val)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->sup(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_supeq:
        { //(>= e val)
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->supeq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_trim:
        { //(trim str)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            u_ustring v;
            e->stringvalue(v);
            u_trim(v);
            e->release();
            return new lisp_string(v);
        }
        case l_type: //(type e)
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            long d = e->code;
            e->release();
            return new lisp_string(get_label(d));
        }
        case l_upper:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            u_ustring v;
            e->stringvalue(v);
            v = special_characters.u_to_upper(v);
            e->release();
            return new lisp_string(v);
        }
        case l_while: //(while condition code)
        {
            lisp_element *cond = values[1]->eval(lisp);
            while (cond->boolean())
            {
                cond->release();
                for (long i = 2; i < sz && r != lisp_break; i++)
                {
                    r = r->release();
                    r = values[i]->eval(lisp);
                }
                if (r == lisp_break)
                {
                    r = lisp_nil;
                    break;
                }
                cond = values[1]->eval(lisp);
            }
            cond->release();
            return r;
        }
        case l_write: //(write pathname txt)
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp->write_file(e, r);
            e->release();
            r->release();
            return lisp_true;
        }
        case l_zerop: //(zero? v)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->doublevalue() == 0 ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_zip:
        { //(zip '(a b c) '(1 2 3) '("a" "b" "c") '(e f g))
            if (sz == 1)
                throw new lisp_error(this, lispargnbserror->message);
            r = new lisp_list();
            long sz = 0;
            for (long i = 1; i < values.size(); i++)
            {
                e = values[i]->eval(lisp);
                if (!e->is_list())
                    throw new lisp_error(this, "Expecting a list object");
                if (i == 1)
                    sz = e->size();
                else if (sz != e->size())
                    throw new lisp_error(this, "lists should all have the same size");
                r->append(e);
            }
            // r-> ((a b c) (1 2 3) ("a" "b" "c") (e f g))
            stringstream os;
            r->string_to_os(os);
            cerr << os.str() << endl;
            e = new lisp_list();
            lisp_element *ll;
            for (long l = 0; l < sz; l++)
            {
                ll = new lisp_list();
                e->append(ll);
                for (long i = 0; i < r->size(); i++)
                {
                    ll->append(r->at(i)->at(l));
                }
            }
            r->release();
            return e;
        }
        case math_acos:
        case math_acosh:
        case math_asin:
        case math_asinh:
        case math_atan:
        case math_atanh:
        case math_cbrt:
        case math_cos:
        case math_cosh:
        case math_degree:
        case math_erf:
        case math_erfc:
        case math_exp:
        case math_exp2:
        case math_expm1:
        case math_fabs:
        case math_floor:
        case math_lgamma:
        case math_log:
        case math_log10:
        case math_log1p:
        case math_log2:
        case math_logb:
        case math_nearbyint:
        case math_radian:
        case math_rint:
        case math_round:
        case math_sin:
        case math_sinh:
        case math_sqrt:
        case math_tan:
        case math_tanh:
        case math_tgamma:
        case math_trunc:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            r = values[1]->eval(lisp);
            double v = r->doublevalue();
            r->release();
            switch (e->code)
            {
            case math_acos:
                return new lisp_float(acos(v));
            case math_acosh:
                return new lisp_float(acosh(v));
            case math_asin:
                return new lisp_float(asin(v));
            case math_asinh:
                return new lisp_float(asinh(v));
            case math_atan:
                return new lisp_float(atan(v));
            case math_atanh:
                return new lisp_float(atanh(v));
            case math_cbrt:
                return new lisp_float(cbrt(v));
            case math_cos:
                return new lisp_float(cos(v));
            case math_cosh:
                return new lisp_float(cosh(v));
            case math_degree:
                v = (v * 180) / M_PI;
                return new lisp_float(v);
            case math_erf:
                return new lisp_float(erf(v));
            case math_erfc:
                return new lisp_float(erfc(v));
            case math_exp:
                return new lisp_float(exp(v));
            case math_exp2:
                return new lisp_float(exp2(v));
            case math_expm1:
                return new lisp_float(expm1(v));
            case math_fabs:
                return new lisp_float(fabs(v));
            case math_floor:
                return new lisp_float(floor(v));
            case math_lgamma:
                return new lisp_float(lgamma(v));
            case math_log:
                return new lisp_float(log(v));
            case math_log10:
                return new lisp_float(log10(v));
            case math_log1p:
                return new lisp_float(log1p(v));
            case math_log2:
                return new lisp_float(log2(v));
            case math_logb:
                return new lisp_float(logb(v));
            case math_nearbyint:
                return new lisp_float(nearbyint(v));
            case math_radian:
                v = M_PI * (v / 180);
                return new lisp_float(v);
            case math_rint:
                return new lisp_float(rint(v));
            case math_round:
                return new lisp_float(round(v));
            case math_sin:
                return new lisp_float(sin(v));
            case math_sinh:
                return new lisp_float(sinh(v));
            case math_sqrt:
                return new lisp_float(sqrt(v));
            case math_tan:
                return new lisp_float(tan(v));
            case math_tanh:
                return new lisp_float(tanh(v));
            case math_tgamma:
                return new lisp_float(tgamma(v));
            case math_trunc:
                return new lisp_float(trunc(v));
            }
        }
        case math_gcd:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            long v = e->longvalue();
            long vv = r->longvalue();
            e->release();
            r->release();
            return new lisp_integer(gcd_math(v, vv));
        }
        case math_hcf:
        {
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            long v = e->longvalue();
            long vv = r->longvalue();
            e->release();
            r->release();
            return new lisp_integer(hcf_math(v, vv));
        }
        case v_list: //((lambda (a1 a2) code) v1 v2)
        {
            // in this case, it might be a lambda function
            // It is a list of lists
            return execute_lambda(lisp, e);
        }
        default:
            // it could be a function name
            if (lisp->check_function(e->code))
            {
                // If we don't clean the stack now from this call
                // Lisp will confuse this stack top with a tail call
                // So we clean the stack preventely
                st.disable();
                return execute_function(lisp, lisp->variables[0][e->code]);
            }
            throw new lisp_error(this, lispunknownmethod->message);
        }
    }
    catch (lisp_error *err)
    {
        e->release();
        if (r != e)
            r->release();
        throw err;
    }
}

//-----------------------------------------------------------------

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
            lisp->stack_variables_out(local_vars);
            throw err;
        }
        /*
        Then we merge our local variables with the top of the stack
        This is different from what we do with a function call...
        A lambda can benefit from the environment variable, in which it is executed...

        ((lambda (x)
            (lambda (u) (+ x u))) 100)

        So we need to have access to x even if we are in a sub-lambda
        */
        lisp->stack_variables_in(local_vars);
        // We then execute our lambda
        r = lisp_nil;
        for (i = 2; i < lmbd->size(); i++)
        {
            r = r->release();
            r = lmbd->at(i)->eval(lisp);
        }
        // then we clean our stack and we keep the last value
        // that was returned...
        r->mark();
        lisp->stack_variables_out(local_vars);
        r->demark();
        return r;
    }
    throw new lisp_error(this, lispunknownmethod->message);
}

//-----------------------------------------------------------------

lisp_element *lisp_list::execute_function(lisp_mini *lisp, lisp_element *function)
{
    lisp_element *a = function->at(0);

    // The first case corresponds to: (label tst (lambda(x) (+ x 10))) for instance
    if (a->code == l_lambda)
        return execute_lambda(lisp, function);

    long sz = function->size();
    if (a->code != l_defun || sz < 4)
        throw new lisp_error(this, lispunknownmethod->message);

    lisp_list *lfunction = (lisp_list *)function;
    // We are executing a function
    // First we need to initialize the arguments
    lisp_element *args = lfunction->values[2];
    if (args->size() != values.size() - 1)
        throw new lisp_error(this, lispargnbserror->message);

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

    uint16_t name = lfunction->values[1]->code;

    // This is a tail call. The top of the stack is the same as the current name
    if (lisp->stack.back() == name)
    {
        // We replace the values in the stack with the new values
        // Basically, we are going to loop on the same stack over and over again
        // The previous values are cleaned.
        lisp->stack_variables_replace(local_vars);
        // lisp_tail is a very important flag that indicates that
        // a new loop should be made on the function instructions
        // see below
        return lisp_tail;
    }

    // Then we push our local variables on the stack
    lisp->stack_variables_on(local_vars);
    a = lisp_tail;
    while (a == lisp_tail)
    {
        for (i = 3; i < sz - 1; i++)
        {
            a->release();
            a = lfunction->values[i]->eval(lisp);
        }
        a->release();
        // For the last instruction of our list of instructions
        // we push the name on top of the stack to prepare
        // for a potential tail call.
        lisp->stack.push_back(name);
        a = lfunction->values[sz - 1]->eval(lisp);
        lisp->stack.pop_back();
    }
    lisp->stack_off(a);
    return a;
}
