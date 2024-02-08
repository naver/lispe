#include "minilisp.h"

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
        case l_push:
        {
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (!e->is_list())
                throw new lisp_error(this, lisperror->message);
            r = values[2]->eval(lisp);
            e = e->append(r);
            return e;
        }
        case l_pop:
        {
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
        case l_sub:
        {
            if (sz < 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            double beg = r->doublevalue();
            r = r->release();
            double end = 0;
            if (sz == 4)
            {
                r = values[3]->eval(lisp);
                end = r->doublevalue();
                r = r->release();
            }
            r = e->sub(beg, end);
            e->release();
            return r;
        }
        case l_plus:
        {
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
        case l_minus:
        {
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
        case l_multiply:
        {
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
        case l_divide:
        {
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
        case l_mod:
        {
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
        case l_range:
            return range(lisp);
        case l_chr:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            long c = e->longvalue();
            e->release();
            return new lisp_string(cs_unicode_to_utf8(c));
        }
        case l_ord:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            long pos = 0;
            string value;
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
        case l_size:
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = new lisp_float(e->size());
            e->release();
            return r;
        case l_car:
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->car();
            r->mark();
            e->release();
            r->demark();
            return r;
        case l_cdr:
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->cdr();
            r->mark();
            e->release();
            r->demark();
            return r;
        case l_cons:
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
        case l_apply:
        { //(apply 'operator list)
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
        case l_type:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            double d = e->code;
            e->release();
            return new lisp_float(d);
        }
        case l_consp:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->is_list() ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_zerop:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->doublevalue() == 0 ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_nullp:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e == lisp_nil ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_stringp:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->code == v_string ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_numberp:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = (e->is_number() ? lisp_true : lisp_nil);
            e->release();
            return r;
        case l_float:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_number())
                return e;
            string v;
            long lg_value = 0;
            e->stringvalue(v);
            double d = convertingfloathexa(STR(v), lg_value);
            e->release();
            return new lisp_float(d);
        }
        case l_integer:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->is_number())
                return e;
            string v;
            long lg_value = 0;
            e->stringvalue(v);
            long d = convertinginteger(v);
            e->release();
            return new lisp_integer(d);
        }
        case l_string:
        {
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->code == v_string)
                return e;
            string v;
            e->stringvalue(v);
            e->release();
            return new lisp_string(v);
        }
        case l_print:
        {
            stringstream os;
            for (long i = 1; i < sz; i++)
            {
                e = values[i]->eval(lisp);
                e->asstring(os);
                e = e->release();
            }
            cerr << os.str() << endl;
            return lisp_emptystring;
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
        case l_split:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *s = e->split(r);
            r->release();
            e->release();
            return s;
        }
        case l_at:
        {
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
        case l_list:
        {
            r = new lisp_list();
            for (long i = 1; i < sz; i++)
            {
                e = values[i]->eval(lisp)->clone(false);
                r->append(e);
            }
            return r;
        }
        case l_map:
        { //(map (key value) (key value) ..)
            e = new lisp_map();
            string k;
            for (long i = 1; i < sz; i++)
            {
                if (values[i]->code != v_list || values[i]->size() != 2)
                    throw new lisp_error(this, lispargnbserror->message);
                r = values[i]->at(0)->eval(lisp);
                k = "";
                r->stringvalue(k);
                r = r->release();
                r = values[i]->at(1)->eval(lisp)->clone(false);
                e->append(k, r);
                r = r->release();
            }
            return e;
        }
        case l_key:
        { //(key map key value)
            if (sz != 4)
                throw new lisp_error(this, lispargnbserror->message);

            e = values[1]->eval(lisp);
            if (!e->is_map())
                throw new lisp_error(this, "Expecting a map");
            r = values[2]->eval(lisp);
            string k;
            r->stringvalue(k);
            r = r->release();
            lisp_element *v = values[3]->eval(lisp);
            e->append(k, v);
            v->release();
            return e;
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
                    break;
                cond = values[1]->eval(lisp);
            }
            cond->release();
            return r;
        }
        case l_if:
        {
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
        case l_not:
            if (sz != 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->boolean() ? lisp_nil : lisp_true;
            e->release();
            return r;
        case l_equal:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->equal(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_different:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->equal(r) ? lisp_nil : lisp_true;
            e->release();
            r->release();
            return res;
        }
        case l_eq:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->eq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_neq:
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->neq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_inf:
        {
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
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->infeq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_sup:
        {
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
        {
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = values[2]->eval(lisp);
            lisp_element *res = e->supeq(r) ? lisp_true : lisp_nil;
            e->release();
            r->release();
            return res;
        }
        case l_cond:
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
        case l_command:
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            r = e->command();
            e->release();
            return r;
        case l_setq:
            if (sz != 3)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1];
            if (!e->is_atom())
                throw new lisp_error(this, lispunknownatom->message);
            r = values[2]->eval(lisp);
            lisp->insert(e->code, r);
            return r;
        case l_quote:
            return values[1];
        case l_eval:
        {
            if (sz < 2)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1]->eval(lisp);
            if (e->code == v_string)
            {
                // We need to compile it...
                r = lisp->run(((lisp_string *)e)->value);
                e->release();
                return r;
            }
            // this is a list
            r = e->eval(lisp);
            e->release();
            return r;
        }
        case l_defun:
            // We record a function name
            if (sz < 4)
                throw new lisp_error(this, lispargnbserror->message);
            e = values[1];
            if (!e->is_atom())
                return lispunknownatom->eval(lisp);
            lisp->keep(e->code, this);
            return e;
        case v_list:
        {
            // in this case, it might be a lambda function
            // It is a list of lists
            return execute_lambda(lisp, e);
        }
        default:
            // it could be a function name
            if (lisp->variables.back().find(e->code) != lisp->variables.back().end())
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
