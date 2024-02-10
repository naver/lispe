#include "minilisp.h"

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
        return gcd_math(a-b, b);
    return gcd_math(a, b-a);
}

long hcf_math(long x, long y) {
    return (!y)?x:hcf_math(y, x%y);
}
//-----------------------------------------------------------------
lisp_element *lisp_list::eval(lisp_mini *lisp)
{
    lisp->check_stop();
    long sz = values.size();
    if (!sz)
        return this;

    lisp_element *e = values[0];
    lisp_element *r = lisp_nil;

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
            for (long i = 1; i < sz; i++)
            {
                r = r->release();
                r = values[i]->eval(lisp);
            }
            return r;
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
            r = e->command();
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
                        return e->at(1)->eval(lisp);
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
            if (e->is_number())
                return e;
            u_ustring v;
            long lg_value = 0;
            e->stringvalue(v);
            double d = convertingfloathexa(STR(v), lg_value);
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
                return values[2]->eval(lisp);
            }
            else
            {
                e = e->release();
                if (sz != 4)
                    throw new lisp_error(this, lispargnbserror->message);
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
            e = e->clone(false);
            r = values[2]->eval(lisp);
            lisp_element *v = values[3]->eval(lisp);
            e->append(r, v);
            r->release();
            v->release();
            return e;
        }
        case l_integer:
        { //(integer x)
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_number())
                return e;
            u_ustring v;
            long lg_value = 0;
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
        case l_lambda:
            return this;
        case l_list: //(list a1 a2 ...)
        {
            r = new lisp_list();
            for (long i = 1; i < sz; i++)
            {
                e = values[i]->eval(lisp)->clone(false);
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
        case l_map:
        { //(map (key value) (key value) ..)
            e = new lisp_map();
            lisp_element *v;
            for (long i = 1; i < sz; i++)
            {
                if (values[i]->code != v_list || values[i]->size() != 2)
                    throw new lisp_error(this, lispargnbserror->message);
                r = values[i]->at(0)->eval(lisp);
                v = values[i]->at(1)->eval(lisp)->clone(false);
                e->append(r, v);
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
            e = values[1]->eval(lisp)->clone(false);
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
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = acos(v);
            return new lisp_float(v);
        }
        case math_acosh:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = acosh(v);
            return new lisp_float(v);
        }
        case math_asin:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = asin(v);
            return new lisp_float(v);
        }
        case math_asinh:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = asinh(v);
            return new lisp_float(v);
        }
        case math_atan:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = atan(v);
            return new lisp_float(v);
        }
        case math_atanh:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = atanh(v);
            return new lisp_float(v);
        }
        case math_cbrt:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = cbrt(v);
            return new lisp_float(v);
        }
        case math_cos:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = cos(v);
            return new lisp_float(v);
        }
        case math_cosh:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = cosh(v);
            return new lisp_float(v);
        }
        case math_degree:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = (v * 180) / M_PI;
            return new lisp_float(v);
        }
        case math_erf:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = erf(v);
            return new lisp_float(v);
        }
        case math_erfc:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = erfc(v);
            return new lisp_float(v);
        }
        case math_exp:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = exp(v);
            return new lisp_float(v);
        }
        case math_exp2:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = exp2(v);
            return new lisp_float(v);
        }
        case math_expm1:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = expm1(v);
            return new lisp_float(v);
        }
        case math_fabs:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = fabs(v);
            return new lisp_float(v);
        }
        case math_floor:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = floor(v);
            return new lisp_float(v);
        }
        case math_gcd:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            long v = e->longvalue();
            long vv = r->longvalue();
            return new lisp_integer(gcd_math(v, vv));
        }
        case math_hcf:
        {
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            long v = e->longvalue();
            long vv = r->longvalue();
            return new lisp_integer(hcf_math(v, vv));
        }
        case math_lgamma:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = lgamma(v);
            return new lisp_float(v);
        }
        case math_log:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = log(v);
            return new lisp_float(v);
        }
        case math_log10:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = log10(v);
            return new lisp_float(v);
        }
        case math_log1p:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = log1p(v);
            return new lisp_float(v);
        }
        case math_log2:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = log2(v);
            return new lisp_float(v);
        }
        case math_logb:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = logb(v);
            return new lisp_float(v);
        }
        case math_nearbyint:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = nearbyint(v);
            return new lisp_float(v);
        }
        case math_radian:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = M_PI * (v / 180);
            return new lisp_float(v);
        }
        case math_rint:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = rint(v);
            return new lisp_float(v);
        }
        case math_round:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = round(v);
            return new lisp_float(v);
        }
        case math_sin:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = sin(v);
            return new lisp_float(v);
        }
        case math_sinh:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = sinh(v);
            return new lisp_float(v);
        }
        case math_sqrt:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = sqrt(v);
            return new lisp_float(v);
        }
        case math_tan:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = tan(v);
            return new lisp_float(v);
        }
        case math_tanh:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = tanh(v);
            return new lisp_float(v);
        }
        case math_tgamma:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = tgamma(v);
            return new lisp_float(v);
        }
        case math_trunc:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double v = e->doublevalue();
            v = trunc(v);
            return new lisp_float(v);
        }
        case v_list: //((lambda (a1 a2) code) v1 v2)
        {
            // in this case, it might be a lambda function
            // It is a list of lists
            return execute_lambda(lisp, e);
        }
        default:
            // it could be a function name
            if (lisp->check_atom(e->code))
            {
                return execute_function(lisp, lisp->variables.back()[e->code]);
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
