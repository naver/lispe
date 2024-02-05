#include "minilisp.h"

// We want lists, numbers and dictionary
extern UTF8_Handler special_characters;
//------------------------------------------------------------------------
static void initialisation_static_values()
{
    if (lisp_nil == NULL)
    {
        lisp_nil = new lisp_list(true);
        lisp_true = new lisp_boolean(true);
        lisp_emptystring = new lisp_string(true);

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
        code_dictionary["atom_"] = v_atom;
        code_dictionary["error_"] = v_error;
        code_dictionary["string_"] = v_string;
        code_dictionary["list_"] = v_list;
        code_dictionary["number_"] = v_number;
        code_dictionary["unix_"] = v_unix;
        code_dictionary["bool_"] = v_boolean;

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
        code_dictionary["print"] = l_print;
        code_dictionary["at"] = l_at;
        code_dictionary["list"] = l_list;
        code_dictionary["loop"] = l_loop;
        code_dictionary["cond"] = l_cond;
        code_dictionary["if"] = l_if;
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
        code_dictionary["clean"] = l_clean;
        code_dictionary["type"] = l_type;
        code_dictionary["cons?"] = l_consp;
        code_dictionary["zero?"] = l_zerop;
        code_dictionary["null?"] = l_nullp;
        code_dictionary["string?"] = l_stringp;
        code_dictionary["number?"] = l_numberp;
        code_dictionary["number"] = l_number;
        code_dictionary["string"] = l_string;
        code_dictionary["stats"] = l_stats;
        code_dictionary["push"] = l_push;
        code_dictionary["pop"] = l_pop;

        code_dictionary["€"] = l_final;

        for (const auto &a : code_dictionary)
            string_dictionary[a.second] = a.first;

        code_dictionary["quote"] = l_quote;
        code_dictionary["true"] = v_boolean;
    }
}
//------------------------------------------------------------------------

typedef enum
{
    e_no_error,
    e_error_segmenting,
    e_error_bracket,
    e_error_parenthesis,
    e_error_brace,
    e_execution_error
} error_tokenize;

error_tokenize code_segmenting(string &code, Segmentingtype &infos)
{
    static bool compile_token = false;
    static tokenizer_automaton tok(&special_characters);
    if (compile_token)
    {
        tok.initialize();
        compile_token = true;
    }

    tokenizer_result<string> r_parse;
    string buffer;

    long line_number = 1;

    long nb_parentheses = 0;
    long nb_braces = 0;
    long nb_brackets = 0;
    long culprit;

    int16_t lc = 0;
    int in_quote = 0;
    double d;
    long lg_value = 0;
    long left = 0;
    long right = 0;

    // We then tokenize our code
    // r_parse.stack contains the substrings
    // r_parse.stacktype contains their type
    // r_parse.stackln contains their line number
    long sz;
    try
    {
        sz = tok.tokenize<string>(code, r_parse);
    }
    catch (Token_error *err)
    {
        string m;
        s_unicode_to_utf8(m, err->msg);
        delete err;
        return e_error_segmenting;
    }

    culprit = -1;
    long i;
    long ipos = 0;

    for (i = 0; i < sz; i++)
    {
        line_number = r_parse.stackln[i] + 1;
        left = r_parse.positions[ipos++];
        right = r_parse.positions[ipos++];

        buffer = r_parse.stack[i];
        // Note that the code in lc are also characters
        // that mimic the type of the element that was identified
        // They do not always correspond to a specific character in the buffer
        lc = r_parse.stacktype[i];
        switch (lc)
        {
        case '\n': // Carriage return
            continue;
        case '"': // a string
            lc = jt_string;
            buffer = buffer.substr(1, buffer.size() - 2);
            lg_value = buffer.find("\\");
            if (lg_value != -1)
            {
                string intermediate = buffer.substr(0, lg_value);
                for (long j = lg_value; j < buffer.size(); j++)
                {
                    if (buffer[j] == '\\')
                    {
                        switch (buffer[j + 1])
                        {
                        case 'n':
                            intermediate += "\n";
                            j++;
                            break;
                        case 'r':
                            intermediate += "\r";
                            j++;
                            break;
                        case 't':
                            intermediate += "\t";
                            j++;
                            break;
                        default:
                            intermediate += buffer[j + 1];
                            j++;
                        }
                        continue;
                    }
                    intermediate += buffer[j];
                }
                buffer = intermediate;
            }
            if (buffer == "")
                infos.append(buffer, jt_emptystring, left, right);
            else
                infos.append(buffer, (jag_code)lc, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        case '`': // a long string
            lc = jt_string;
            buffer = buffer.substr(1, buffer.size() - 2);
            if (buffer == "")
                infos.append(buffer, jt_emptystring, left, right);
            else
                infos.append(buffer, (jag_code)lc, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        case '\'': // a quote
            if (in_quote)
                infos.append(buffer, jt_keyword, left, right);
            else
                infos.append(buffer, jt_quote, left, right);
            if (!in_quote)
                in_quote = 1;
            break;
        case ';':
        { // a quoted #, we have a specific rule for this character to handle potential comments
            string q = "'";
            infos.append(q, jt_quote, left, left + 1);
            buffer = buffer.substr(1, buffer.size());
            infos.append(buffer, jt_keyword, left + 1, right);
            break;
        }
        case '9': // a float: contains a '.'
            d = convertingfloathexa((char *)buffer.c_str(), lg_value);
            infos.append(d, jt_number, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        case '?': // operators and comparators
            infos.append(buffer, jt_keyword, left, right);
            if (in_quote)
                in_quote--;
            break;
        case 'A': // a simple token
            infos.append(buffer, jt_keyword, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        case '(': // (
            nb_parentheses++;
            if (in_quote == 1 && infos.types.back() == jt_quote) {
                infos.types.pop_back();
                infos.types.push_back(jt_quote_list);
            }
            infos.append(buffer, jt_opening_p, left, right);
            if (in_quote)
                in_quote++;
            break;
        case ')': // )
            nb_parentheses--;
            if (nb_parentheses <= 0)
            {
                if (culprit == -1)
                    culprit = line_number;
            }
            infos.append(buffer, jt_closing_p, left, right);
            if (in_quote < 3)
                in_quote = 0;
            else
                in_quote--;
            break;
        case '[': // [
            buffer = buffer.substr(1, buffer.size() - 2);
            infos.append(buffer, jt_opening_bk, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        default:
            infos.append(buffer, jt_keyword, left, right);
            if (in_quote == 1)
                in_quote = 0;
        }
    }

    if (nb_brackets)
    {
        return e_error_bracket;
    }

    if (nb_parentheses)
    {
        return e_error_parenthesis;
    }

    if (nb_braces)
    {
        return e_error_brace;
    }

    return e_no_error;
}

//-------------------------------------------------------------------------------------
#ifdef DEBUGGER
static std::set<lisp_element *> garbages;

lisp_element::lisp_element(uint16_t c) : code(c)
{
    garbages.insert(this);
    status = 0;
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

lisp_element::~lisp_element() {}
#endif

lisp_element *lisp_element::append(lisp_element *e)
{
    return lisperror->eval(NULL);
}

void lisp_element::pop()
{
    lisperror->eval(NULL);
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

lisp_element *lisp_element::at(long i)
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::command()
{
    return lisperror->eval(NULL);
}

lisp_element *lisp_element::split(string &splitter)
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

bool lisp_element::inf(lisp_element *v)
{
    return lisperror->eval(NULL);
}

bool lisp_element::sup(lisp_element *v)
{
    return lisperror->eval(NULL);
}

bool lisp_element::infeq(lisp_element *v)
{
    return lisperror->eval(NULL);
}

bool lisp_element::supeq(lisp_element *v)
{
    return lisperror->eval(NULL);
}

void lisp_element::mark()
{
    status += (status != s_constant);
}

void lisp_element::unmark()
{
    status -= (status && status != s_constant);
    if (!status)
    {
        delete this;
    }
}

void lisp_element::protect()
{
    status |= s_protect;
}

void lisp_list::protect()
{
    if ((status & s_protect) == s_protect)
        return;

    status |= s_protect;
    for (long i = 0; i < list_of_elements.size(); i++)
        list_of_elements[i]->protect();
}

void lisp_element::unprotect()
{
    if (status != s_constant)
        status &= ~s_protect;
}

void lisp_list::unprotect()
{
    if (status == s_constant)
        return;

    if ((status & s_protect) != s_protect)
        return;

    status &= ~s_protect;
    for (long i = 0; i < list_of_elements.size(); i++)
        list_of_elements[i]->unprotect();
}

void lisp_list::unmark()
{
    status -= (status && status != s_constant);
    if (!status)
    {
        for (long i = 0; i < list_of_elements.size(); i++)
            list_of_elements[i]->unmark();
        delete this;
    }
}

void lisp_element::clean(std::set<lisp_element *> &garbage)
{
    if ((status & s_protect) != s_protect)
        garbage.insert(this);
}

void lisp_list::clean(std::set<lisp_element *> &garbage)
{
    if (garbage.find(this) != garbage.end())
        return;

    if ((status & s_protect) != s_protect)
    {
        for (long i = 0; i < list_of_elements.size(); i++)
            list_of_elements[i]->clean(garbage);
        garbage.insert(this);
    }
}

lisp_element *lisp_element::release()
{
    if (!status)
    {
        delete this;
        return lisp_nil;
    }
    return this;
}

lisp_element *lisp_list::release()
{
    if (!status)
    {
        for (long i = 0; i < list_of_elements.size(); i++)
            list_of_elements[i]->unmark();
        delete this;
        return lisp_nil;
    }
    return this;
}

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

    return lispunknownatom->eval(lisp);
}

class lisp_element;
class lisp_error;

//------------------------------------------------------------------------
// These are elements, which are never deleted and common to all lisps
//------------------------------------------------------------------------
lisp_element *lisp_nil = NULL;
lisp_element *lisp_emptystring = NULL;
lisp_element *lisp_true = NULL;

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
    for (auto &v : variables)
    {
        for (auto &a : v)
        {
            a.second->unmark();
        }
    }

    variables.clear();
}

//------------------------------------------------------------------------
bool lisp_mini::compile(lisp_element *program, long &pos, compile_action action)
{
    lisp_element *e;
    long sz = infos.size();
    if (action == one_action)
        sz = pos + 1;

    for (; pos < sz; pos++)
    {
        switch (infos.types[pos])
        {
        case jt_string:
        {
            e = new lisp_string(infos.strings[pos]);
            program->append(e);
            break;
        }
        case jt_emptystring:
            program->append(lisp_emptystring);
            break;
        case jt_keyword:
        {
            uint16_t c = get_code(infos.strings[pos]);
            if (c < l_final)
            {
                if (c == v_nil)
                    e = lisp_nil;
                else
                {
                    if (c == v_boolean)
                        e = lisp_true;
                    else
                        e = new lisp_instruction(c);
                }
            }
            else
                e = new lisp_atom(c);

            program->append(e);
            break;
        }
        case jt_quote: {
            e = new lisp_list();
            e->append(new lisp_instruction(l_quote));
            program->append(e);
            pos++;
            compile(e, pos, one_action);
            pos--;
            break;
        }
        case jt_quote_list: {
            e = new lisp_list();
            e->append(new lisp_instruction(l_quote));
            program->append(e);
            pos++;
            compile(e, pos, next_action);
            break;
        }
        case jt_number:
            e = new lisp_number(infos.numbers[pos]);
            program->append(e);
            break;
        case jt_opening_p:
        {
            e = new lisp_list();
            program->append(e);
            pos++;
            if (!compile(e, pos, next_action))
                return false;
            break;
        }
        case jt_closing_p:
            return true;
        case jt_opening_bk:
        {
            lisp_list *l = new lisp_list();
            l->append(new lisp_instruction(l_command));
            l->append(new lisp_unix(infos.strings[pos]));
            program->append(l);
            break;
        }
        case jt_closing_bk:
            return true;
        default:
            cerr << infos.types[pos] << endl;
        }
    }
}
//-------------------------------------------------------------------------------------
lisp_element *lisp_list::eval(lisp_mini *lisp)
{
    lisp->check_stop();
    long sz = list_of_elements.size();
    if (!sz)
        return this;

    lisp_element *e = list_of_elements[0];
    lisp_element *r = lisp_nil;

    try
    {
        switch (e->code)
        {
        case l_push:
        {
            if (sz < 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            if (e->code != v_list)
                return lisperror->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            e->append(r);
            return e;
        }
        case l_pop:
        {
            if (sz < 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            if (e->code != v_list)
                return lisperror->eval(lisp);
            e->pop();
            return e;
        }
        case l_plus:
        {
            if (sz < 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp)->clone();
            for (long i = 2; i < sz; i++)
            {
                r = list_of_elements[i]->eval(lisp);
                e->plus(r);
                r = r->release();
            }
            return e;
        }
        case l_minus:
        {
            if (sz < 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp)->clone();
            for (long i = 2; i < sz; i++)
            {
                r = list_of_elements[i]->eval(lisp);
                e->minus(r);
                r = r->release();
            }
            return e;
        }
        case l_multiply:
        {
            if (sz < 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp)->clone();
            for (long i = 2; i < sz; i++)
            {
                r = list_of_elements[i]->eval(lisp);
                e->multiply(r);
                r = r->release();
            }
            return e;
        }
        case l_divide:
        {
            if (sz < 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp)->clone();
            for (long i = 2; i < sz; i++)
            {
                r = list_of_elements[i]->eval(lisp);
                e->divide(r);
                r = r->release();
            }
            return e;
        }
        case l_mod:
        {
            if (sz < 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp)->clone();
            for (long i = 2; i < sz; i++)
            {
                r = list_of_elements[i]->eval(lisp);
                e->mod(r);
                r = r->release();
            }
            return e;
        }
        case l_size:
            if (sz < 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = new lisp_number(e->size());
            e = e->release();
            return r;
        case l_car:
            if (sz < 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = e->car();
            r->mark();
            e = e->release();
            r->demark();
            return r;
        case l_cdr:
            if (sz < 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = e->cdr();
            r->mark();
            e = e->release();
            r->demark();
            return r;
        case l_cons:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            if (r->code != v_list)
            {
                lisp_list *l = new lisp_list();
                l->append(e);
                l->append(r);
                return l;
            }
            r->push_first(e);
            return r;
        }
        case l_type:
        {
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            double d = e->code;
            e = e->release();
            return new lisp_number(d);
        }
        case l_consp:
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = (e->code == v_list ? lisp_true : lisp_nil);
            e = e->release();
            return r;
        case l_zerop:
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = (e->numerical_value() == 0 ? lisp_true : lisp_nil);
            e = e->release();
            return r;
        case l_nullp:
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = (e == lisp_nil ? lisp_true : lisp_nil);
            e = e->release();
            return r;
        case l_stringp:
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = (e->code == v_string ? lisp_true : lisp_nil);
            e = e->release();
            return r;
        case l_numberp:
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = (e->code == v_number ? lisp_true : lisp_nil);
            e = e->release();
            return r;
        case l_number:
        {
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            if (e->code == v_number)
                return e;
            string v;
            long lg_value = 0;
            e->stringvalue(v);
            double d = convertingfloathexa(STR(v), lg_value);
            e = e->release();
            return new lisp_number(d);
        }
        case l_string:
        {
            if (sz != 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            if (e->code == v_string)
                return e;
            string v;
            e->stringvalue(v);
            e = e->release();
            return new lisp_string(v);
        }
        case l_print:
        {
            stringstream os;
            for (long i = 1; i < sz; i++)
            {
                e = list_of_elements[i]->eval(lisp);
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
                r = list_of_elements[i]->eval(lisp);
            }
            return r;
        }
        case l_split:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            string v;
            r->stringvalue(v);
            r = r->release();
            r = e->split(v);
            e = e->release();
            return r;
        }
        case l_at:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            double d = r->numerical_value();
            r = r->release();
            r = e->at(d);
            r->mark();
            e = e->release();
            return r;
        }
        case l_list:
        {
            r = new lisp_list(this);
            for (long i = 1; i < sz; i++)
            {
                e = list_of_elements[i]->eval(lisp);
                r->append(e);
            }
            return r;
        }
        case l_loop:
        {
            lisp_element *cond = list_of_elements[1]->eval(lisp);
            while (cond->boolean())
            {
                cond->release();
                for (long i = 2; i < sz; i++)
                {
                    r = r->release();
                    r = list_of_elements[i]->eval(lisp);
                }
                cond = list_of_elements[1]->eval(lisp);
            }
            cond->release();
            return r;
        }
        case l_if:
        {
            if (sz != 3 && sz != 4)
                return lispargnbserror->eval(lisp);

            e = list_of_elements[1]->eval(lisp);
            if (e->boolean())
            {
                e = e->release();
                if (sz < 3)
                    return lispargnbserror->eval(lisp);
                return list_of_elements[2]->eval(lisp);
            }
            else
            {
                e = e->release();
                if (sz != 4)
                    return lispargnbserror->eval(lisp);
                return list_of_elements[3]->eval(lisp);
            }
        }
        case l_eq:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp_element *res = e->eq(r) ? lisp_true : lisp_nil;
            e = e->release();
            r = r->release();
            return res;
        }
        case l_neq:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp_element *res = e->neq(r) ? lisp_true : lisp_nil;
            e = e->release();
            r = r->release();
            return res;
        }
        case l_inf:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp_element *res = e->inf(r) ? lisp_true : lisp_nil;
            e = e->release();
            r = r->release();
            return res;
        }
        case l_infeq:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp_element *res = e->infeq(r) ? lisp_true : lisp_nil;
            e = e->release();
            r = r->release();
            return res;
        }
        case l_sup:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp_element *res = e->sup(r) ? lisp_true : lisp_nil;
            e = e->release();
            r = r->release();
            return res;
        }
        case l_supeq:
        {
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp_element *res = e->supeq(r) ? lisp_true : lisp_nil;
            e = e->release();
            r = r->release();
            return res;
        }
        case l_cond:
        {
            for (long i = 1; i < sz; i++)
            {
                e = list_of_elements[i];
                if (e->code == v_list && e->size() == 2)
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
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            r = e->command();
            e = e->release();
            return r;
        case l_setq:
            if (sz != 3)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1];
            if (!e->is_atom())
                return lispunknownatom->eval(lisp);
            r = list_of_elements[2]->eval(lisp);
            lisp->insert(e->code, r);
            r = r->release();
            return r;
        case l_quote:
            return list_of_elements[1];
        case l_eval:
        {
            if (sz < 2)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1]->eval(lisp);
            if (e->code == v_string)
            {
                // We need to compile it...
                r = lisp->run(((lisp_string *)e)->value);
                e = e->release();
                return r;
            }
            // this is a list
            r = e->eval(lisp);
            e = e->release();
            return r;
        }
        case l_defun:
            // We record a function name
            if (sz < 4)
                return lispargnbserror->eval(lisp);
            e = list_of_elements[1];
            if (!e->is_atom())
                return lispunknownatom->eval(lisp);
            lisp->insert(e->code, this);
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
            return lispunknownmethod->eval(lisp);
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

lisp_element *lisp_list::execute_lambda(lisp_mini *lisp, lisp_element *lmbd)
{
    if (lmbd->size() && lmbd->at(0)->code == l_lambda)
    {
        // then we have ((lambda (p1 .. p2) code) a1..a2)
        // e is (lambda (p1 .. p2 ) code)
        if (lmbd->size() < 3 || lmbd->at(1)->size() != list_of_elements.size() - 1)
            return lispargnbserror->eval(lisp);
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
                    return lispunknownatom->eval(lisp);
                a = list_of_elements[i + 1]->eval(lisp);
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
    return lispunknownmethod->eval(lisp);
}

lisp_element *lisp_list::execute_function(lisp_mini *lisp, lisp_element *function)
{
    lisp_element *e = list_of_elements[0];
    if (function->code != v_list || function->size() < 4 || function->at(0)->code != l_defun)
        return lispunknownmethod->eval(lisp);

    // We are executing a function
    // First we need to initialize the arguments
    lisp_element *args = function->at(2);
    if (args->size() != list_of_elements.size() - 1)
        return lispargnbserror->eval(lisp);

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
                return lispunknownatom->eval(lisp);
            a = list_of_elements[i + 1]->eval(lisp);
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
    error_tokenize e = code_segmenting(code, infos);
    if (e != e_no_error)
        return lisptokenizeerror->eval(this);

    long pos = 0;
    lisp_list *p_code = new lisp_list();
    compile(p_code, pos, first_action);
    p_code->mark();
    lisp_element* program = p_code->at(0);
    stop_execution = false;
    lisp_element *res; 
    try {
        res = program->eval(this);
        res->protect();
        clean(p_code);
        res->unprotect();
        return res;
    }
    catch(lisp_error* err) {
        clean(p_code);
        return err;
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
    error_tokenize e = code_segmenting(code, lisp->infos);
    std::stringstream os;

    if (e != e_no_error)
    {
        lisptokenizeerror->asstring(os);
        os << ":" << e;
        return os.str();
    }

    lisp_list *p_code = new lisp_list();
    long pos = 0;
    lisp->compile(p_code, pos, first_action);
    p_code->mark();
    lisp_element* program = p_code->at(0);
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
    }

    lisp->clean(p_code);

    return os.str();
}
