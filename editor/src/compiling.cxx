#include "minilisp.h"
#include <unistd.h>

#ifdef DEBUGGER
void displaygarbagesize();
#endif

//------------------------------------------------------------------------
// These are elements, which are never deleted and common to all lisps
//------------------------------------------------------------------------
lisp_element *lisp_nil = NULL;
lisp_element *lisp_true = NULL;
lisp_string *lisp_emptystring = NULL;

lisp_atom *lisp_break = NULL;
lisp_atom *lisp_tail = NULL;

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

lisp_float* lisp_pi = NULL;
lisp_float* lisp_tau = NULL;
lisp_float* lisp_e = NULL;
lisp_float* lisp_phi = NULL;


static const double M_GOLDEN = 1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475;

//------------------------------------------------------------------------
static std::map<u_ustring, uint16_t> code_dictionary;
static std::map<uint16_t, lisp_element *> instructions_dictionary;
static std::map<uint16_t, u_ustring> string_dictionary;
//-------------------------------------------------------------------------------------
static void initialisation_static_values()
{
    if (lisp_nil == NULL)
    {
        lisp_nil = new lisp_list_nil();
        lisp_true = new lisp_boolean(true);
        lisp_emptystring = new lisp_string(s_constant, U"");
        lisp_break = new lisp_atom(v_break);
        lisp_tail = new lisp_atom(v_tail);

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

        lisp_pi = new lisp_float(s_constant, M_PI);
        lisp_tau = new lisp_float(s_constant, M_PI * 2);
        lisp_e = new lisp_float(s_constant, M_E);
        lisp_phi = new lisp_float(s_constant, M_GOLDEN);


        code_dictionary[U"nil"] = v_nil;
        code_dictionary[U"true"] = v_boolean;
        code_dictionary[U"break"] = v_break;
        code_dictionary[U"tail#"] = v_tail;

        code_dictionary[U"atom_"] = v_atom;
        code_dictionary[U"error_"] = v_error;
        code_dictionary[U"string_"] = v_string;
        code_dictionary[U"list_"] = v_list;
        code_dictionary[U"float_"] = v_float;
        code_dictionary[U"integer_"] = v_integer;
        code_dictionary[U"unix_"] = v_unix;

        code_dictionary[U"!="] = l_different;
        code_dictionary[U"%"] = l_mod;
        code_dictionary[U"'"] = l_quote;
        code_dictionary[U"*"] = l_multiply;
        code_dictionary[U"+"] = l_plus;
        code_dictionary[U"-"] = l_minus;
        code_dictionary[U"/"] = l_divide;
        code_dictionary[U"<"] = l_inf;
        code_dictionary[U"<="] = l_infeq;
        code_dictionary[U"="] = l_equal;
        code_dictionary[U">"] = l_sup;
        code_dictionary[U">="] = l_supeq;
        code_dictionary[U"append"] = l_append;
        code_dictionary[U"apply"] = l_apply;
        code_dictionary[U"at"] = l_at;
        code_dictionary[U"atom"] = l_atom;
        code_dictionary[U"atom?"] = l_atomp;
        code_dictionary[U"base"] = l_base;
        code_dictionary[U"block"] = l_block;
        code_dictionary[U"car"] = l_car;
        code_dictionary[U"cadar"] = l_cadar;
        code_dictionary[U"cdr"] = l_cdr;
        code_dictionary[U"chr"] = l_chr;
        code_dictionary[U"command"] = l_command;
        code_dictionary[U"cond"] = l_cond;
        code_dictionary[U"cons"] = l_cons;
        code_dictionary[U"cons?"] = l_consp;
        code_dictionary[U"defun"] = l_defun;
        code_dictionary[U"eq"] = l_eq;
        code_dictionary[U"eval"] = l_eval;
        code_dictionary[U"filtercar"] = l_filtercar;
        code_dictionary[U"find"] = l_find;
        code_dictionary[U"float"] = l_float;
        code_dictionary[U"if"] = l_if;
        code_dictionary[U"insert"] = l_insert;
        code_dictionary[U"integer"] = l_integer;
        code_dictionary[U"join"] = l_join;
        code_dictionary[U"label"] = l_label;
        code_dictionary[U"λ"] = l_lambda;
        code_dictionary[U"list"] = l_list;
        code_dictionary[U"load"] = l_load;
        code_dictionary[U"loop"] = l_loop;
        code_dictionary[U"lower"] = l_lower;
        code_dictionary[U"map"] = l_map;
        code_dictionary[U"mapcar"] = l_mapcar;
        code_dictionary[U"nconc"] = l_nconc;
        code_dictionary[U"neq"] = l_neq;
        code_dictionary[U"not"] = l_not;
        code_dictionary[U"null?"] = l_nullp;
        code_dictionary[U"number?"] = l_numberp;
        code_dictionary[U"ord"] = l_ord;
        code_dictionary[U"pop"] = l_pop;
        code_dictionary[U"print"] = l_print;
        code_dictionary[U"println"] = l_println;
        code_dictionary[U"push"] = l_push;
        code_dictionary[U"put"] = l_put;
        code_dictionary[U"range"] = l_range;
        code_dictionary[U"read"] = l_read;
        code_dictionary[U"replace"] = l_replace;
        code_dictionary[U"setq"] = l_setq;
        code_dictionary[U"size"] = l_size;
        code_dictionary[U"sort"] = l_sort;
        code_dictionary[U"split"] = l_split;
        code_dictionary[U"stats"] = l_stats;
        code_dictionary[U"string"] = l_string;
        code_dictionary[U"string?"] = l_stringp;
        code_dictionary[U"sub"] = l_sub;
        code_dictionary[U"trim"] = l_trim;
        code_dictionary[U"type"] = l_type;
        code_dictionary[U"upper"] = l_upper;
        code_dictionary[U"while"] = l_while;
        code_dictionary[U"write"] = l_write;
        code_dictionary[U"zero?"] = l_zerop;
        code_dictionary[U"zip"] = l_zip;

        code_dictionary[U"acos"] = math_acos;
        code_dictionary[U"acosh"] = math_acosh;
        code_dictionary[U"asin"] = math_asin;
        code_dictionary[U"asinh"] = math_asinh;
        code_dictionary[U"atan"] = math_atan;
        code_dictionary[U"atanh"] = math_atanh;
        code_dictionary[U"cbrt"] = math_cbrt;
        code_dictionary[U"cos"] = math_cos;
        code_dictionary[U"cosh"] = math_cosh;
        code_dictionary[U"degree"] = math_degree;
        code_dictionary[U"erf"] = math_erf;
        code_dictionary[U"erfc"] = math_erfc;
        code_dictionary[U"exp"] = math_exp;
        code_dictionary[U"exp2"] = math_exp2;
        code_dictionary[U"expm1"] = math_expm1;
        code_dictionary[U"fabs"] = math_fabs;
        code_dictionary[U"floor"] = math_floor;
        code_dictionary[U"gcd"] = math_gcd;
        code_dictionary[U"hcf"] = math_hcf;
        code_dictionary[U"lgamma"] = math_lgamma;
        code_dictionary[U"log"] = math_log;
        code_dictionary[U"log10"] = math_log10;
        code_dictionary[U"log1p"] = math_log1p;
        code_dictionary[U"log2"] = math_log2;
        code_dictionary[U"logb"] = math_logb;
        code_dictionary[U"nearbyint"] = math_nearbyint;
        code_dictionary[U"radian"] = math_radian;
        code_dictionary[U"rint"] = math_rint;
        code_dictionary[U"round"] = math_round;
        code_dictionary[U"sin"] = math_sin;
        code_dictionary[U"sinh"] = math_sinh;
        code_dictionary[U"sqrt"] = math_sqrt;
        code_dictionary[U"tan"] = math_tan;
        code_dictionary[U"tanh"] = math_tanh;
        code_dictionary[U"tgamma"] = math_tgamma;
        code_dictionary[U"trunc"] = math_trunc;

        code_dictionary[U"€"] = l_final;

        for (const auto &a : code_dictionary)
        {
            string_dictionary[a.second] = a.first;
            if (a.second == v_nil || a.second == v_boolean)
                continue;
            instructions_dictionary[a.second] = new lisp_instruction(a.second);
        }

        instructions_dictionary[v_nil] = lisp_nil;
        instructions_dictionary[v_boolean] = lisp_true;

        code_dictionary[U"quote"] = l_quote;
        code_dictionary[U"false"] = v_nil;
        code_dictionary[U"@"] = l_at;
        code_dictionary[U"nth"] = l_at;
        code_dictionary[U"lambda"] = l_lambda;
        code_dictionary[U"\\"] = l_lambda;
        code_dictionary[U"√"] = math_sqrt;
        code_dictionary[U"∛"] = math_cbrt;
    }
}
//------------------------------------------------------------------------
uint16_t get_code(string &s)
{
    uint16_t c;
    u_ustring w;
    s_utf8_to_unicode(w, s, s.size());
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

uint16_t get_code(u_ustring &w)
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

u_ustring get_label(uint16_t c)
{

    if (string_dictionary.find(c) != string_dictionary.end())
    {
        if (c < l_final)
            return string_dictionary[c];
        return U"atom_";
    }
    return U"unknown!!!";
}

lisp_atom *lisp_mini::get_atom(u_ustring &u)
{
    uint16_t a = get_code(u);
    lisp_atom *la = atoms[a];
    if (la == NULL)
    {
        la = new lisp_atom(a);
        atoms[a] = la;
    }
    return la;
}
//-------------------------------------------------------------------------------------
void lisp_element::stringvalue(u_ustring &v, bool into)
{
    v += string_dictionary[code];
}

void lisp_element::string_to_os(std::stringstream &os, bool into)
{
    string s;
    s_unicode_to_utf8(s, string_dictionary[code]);
    os << s;
}

void lisp_mini::initmathvalues() {
        u_ustring name = U"_pi";
        lisp_atom* a = get_atom(name);
        store_atom(a->code, lisp_pi);
        name = U"π";
        a = get_atom(name);
        store_atom(a->code, lisp_pi);

        name = U"_tau";
        a = get_atom(name);
        store_atom(a->code, lisp_tau);
        name = U"τ";
        a = get_atom(name);
        store_atom(a->code, lisp_tau);

        name = U"_e";
        a = get_atom(name);
        store_atom(a->code, lisp_e);
        name = U"ℯ";
        a = get_atom(name);
        store_atom(a->code, lisp_e);

        name = U"_phi";
        a = get_atom(name);
        store_atom(a->code, lisp_phi);
        name = U"ϕ";
        a = get_atom(name);
        store_atom(a->code, lisp_phi);
}
//-------------------------------------------------------------------------------------
// We want lists, numbers and dictionary
error_tokenize code_segmenting(string &code, Segmentingtype &infos, UTF8_Handler *special_characters)
{
    static tokenizer_automaton tok(special_characters);

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
            infos.append(buffer, jt_number, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        case '?': // operators and comparators
            infos.append(buffer, jt_keyword, left, right);
            if (in_quote)
                in_quote--;
            break;
        case 'A': {// a simple token
            jag_code iscadar = jt_keyword;
            if (buffer[0] == 'c' && buffer.back() == 'r' && buffer.size() > 3) {
                iscadar = jt_cadar;
                //It could be a variation on cadar
                for (long u = 1; u < buffer.size() - 1; u++) {
                    if (buffer[u] != 'a' && buffer[u] != 'd') {
                        iscadar = jt_keyword;
                        break;
                    }
                }
                if (iscadar == jt_cadar)
                    buffer = buffer.substr(1, buffer.size()-2);
            }
            infos.append(buffer, iscadar, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        }
        case '(': // (
            nb_parentheses++;
            if (in_quote == 1 && infos.types.back() == jt_quote)
            {
                infos.types.pop_back();
                infos.types.push_back(jt_quote_list);
            }
            infos.append(buffer, jt_o_parenthesis, left, right);
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
            infos.append(buffer, jt_c_parenthesis, left, right);
            if (in_quote < 3)
                in_quote = 0;
            else
                in_quote--;
            break;
        case '[': // [
            buffer = buffer.substr(1, buffer.size() - 2);
            infos.append(buffer, jt_bracket, left, right);
            if (in_quote == 1)
                in_quote = 0;
            break;
        case '{': // (
            nb_braces++;
            if (in_quote == 1 && infos.types.back() == jt_quote)
            {
                infos.types.pop_back();
                infos.types.push_back(jt_quote_list);
            }
            infos.append(buffer, jt_o_brace, left, right);
            if (in_quote)
                in_quote++;
            break;
        case '}': // )
            nb_braces--;
            if (nb_braces <= 0)
            {
                if (culprit == -1)
                    culprit = line_number;
            }
            infos.append(buffer, jt_c_brace, left, right);
            if (in_quote < 3)
                in_quote = 0;
            else
                in_quote--;
            break;
        case ':':
            infos.append(buffer, jt_colon, left, right);
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
        case jt_cadar:
            e = new lisp_cadar(storage, infos.strings[pos]);
            action = program->store(current_key, e, action);
            break;
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
                    e = new lisp_float(storage, convertingfloathexa(STR(infos.strings[pos])));
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

            // We store the function definition at compile time
            if (e->size() > 3 && e->at(0)->code == l_defun)
            {
                store_atom(e->at(1)->code, e);
                if (e != program)
                    program->pop_raw();
            }

            if (e->size() == 2 && e->at(0)->code == l_load)
            {
                // We load some code locally from a file
                if (e != program)
                    program->pop_raw();
                load_program(program, e->at(1), storage);
            }
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
    return true;
}

void lisp_mini::set_file_name(string &spath)
{
    char localpath[4100];

#ifdef WIN32
    _fullpath(localpath, STR(spath), 4096);
#else
    realpath(STR(spath), localpath);
#endif

    current_file_name = spath;

#ifdef WIN32
    string end_path = "\\";
#else
    string end_path = "/";
#endif
    long pos = spath.rfind(end_path);
    if (pos == string::npos)
    {
        spath = localpath;
        if (spath.back() != end_path[0])
            spath += end_path;
    }
    else
        spath = spath.substr(0, pos + 1);
    current_directory = spath;
}

void lisp_mini::write_file(lisp_element *e, lisp_element *txt)
{
    u_ustring upath;
    e->stringvalue(upath);

    string path;
    s_unicode_to_utf8(path, upath);

    std::ofstream f(path, std::ios::out | std::ios::binary);
    if (f.fail())
        throw new lisp_error(e, "Error: writing");
    upath = U"";
    txt->stringvalue(upath);

    path = "";
    s_unicode_to_utf8(path, upath);
    f << path;
    f.close();
}

void lisp_mini::append_file(lisp_element *e, lisp_element *txt)
{
    u_ustring upath;
    e->stringvalue(upath);

    string path;
    s_unicode_to_utf8(path, upath);

    std::ofstream f(path, std::ios::app | std::ios::binary);
    if (f.fail())
        throw new lisp_error(e, "Error: writing");
    upath = U"";
    txt->stringvalue(upath);

    path = "";
    s_unicode_to_utf8(path, upath);
    f << path;
    f.close();
}

string lisp_mini::read_file(lisp_element *e)
{
    u_ustring upath;
    e->stringvalue(upath);

    string path;
    s_unicode_to_utf8(path, upath);

    std::ifstream f(path, std::ios::in | std::ios::binary);
    if (f.fail())
        throw new lisp_error(e, "Error: loading");

    string line;
    string codes;
    while (!f.eof())
    {
        getline(f, line);
        codes += line + "\n";
    }
    s_trim(codes);
    return codes;
}

lisp_element *lisp_mini::load_program(lisp_element *program, lisp_element *path, vector<lisp_element *> &storage)
{
    lisp_element *e = path->eval(this);
    string codes = read_file(path);
    e->release();
    Segmentingtype infos;
    error_tokenize tk = code_segmenting(codes, infos, &special_characters);
    if (tk != e_no_error)
    {
        throw new lisp_error(path, lisptokenizeerror->message);
    }

    long pos = 0;
    compile(program, storage, pos, next_action);
    return program;
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

string lisp_mini::execute_code(string &code)
{
    infos.clear();
    error_tokenize e = code_segmenting(code, infos, &special_characters);
    std::stringstream os;

    if (e != e_no_error)
    {
        lisptokenizeerror->string_to_os(os);
        os << ":" << e;
        return os.str();
    }

    vector<lisp_element *> storage;
    lisp_list *program = new lisp_list(storage);
    long pos = 0;
    compile(program, storage, pos, first_action);

    lisp_element *res = lisp_nil;
    try
    {
        stop_execution = false;
        stack.clear();
        res = program->eval(this);
        res->string_to_os(os);
        res->release();
    }
    catch (lisp_error *l)
    {
        l->string_to_os(os);
        l->release();
    }

    clean(storage);

#ifdef DEBUGGER
    displaygarbagesize();
#endif
    return os.str();
}

string lisp_mini::execute_file(string path, vector<string> &args)
{

    std::stringstream os;
    std::ifstream f(path, std::ios::in | std::ios::binary);
    if (f.fail())
    {
        os << "error loading file: " << path;
        return os.str();
    }

    string line;
    string code = "(block ";
    while (!f.eof())
    {
        getline(f, line);
        code += line + "\n";
    }

    code += ")";

    infos.clear();
    error_tokenize e = code_segmenting(code, infos, &special_characters);

    if (e != e_no_error)
    {
        lisptokenizeerror->string_to_os(os);
        os << ":" << e;
        return os.str();
    }

    // We create the _current variable that points to the file directory
    set_file_name(path);
    vector<lisp_element *> storage;

    u_ustring name = U"_current";
    lisp_atom *a = get_atom(name);
    store_atom(a->code, new lisp_string(storage, current_directory));

    lisp_list *program = new lisp_list(storage);
    for (long i = 0; i < args.size(); i++)
    {
        program->append(new lisp_string(storage, args[i]));
    }

    name = U"_args";
    a = get_atom(name);
    store_atom(a->code, program);

    program = new lisp_list(storage);
    long pos = 0;
    compile(program, storage, pos, first_action);

    lisp_element *res = lisp_nil;
    try
    {
        stop_execution = false;
        stack.clear();
        res = program->eval(this);
        res->string_to_os(os);
        res->release();
    }
    catch (lisp_error *l)
    {
        l->string_to_os(os);
        l->release();
    }

    clean(storage);

#ifdef DEBUGGER
    displaygarbagesize();
#endif
    return os.str();
}

//------------------------------------------------------------------------
lisp_mini::lisp_mini()
{
    initialisation_static_values();
    stop_execution = false;
    std::map<uint16_t, lisp_element *> v;
    variables.push_back(v);
    initmathvalues();
}

void lisp_mini::garbage_clean()
{
#ifdef DEBUGGER
    displaygarbagesize();
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
    displaygarbagesize();
#endif
}
//-------------------------------------------------------------------------------------
string lisp_mini::execute_unix_command(string cmd)
{
    FILE *fp;
    int status;

    char res[PATH_MAX];

    bool get_current = false;
    if (cmd[0] == 'c' && cmd[1] == 'd' && (!cmd[2] || cmd[2] == ' '))
    {
        // In this case we do a fwrite to force the completion
        get_current = true;
        cmd += ";pwd";
    }

    if (current_directory != "")
    {
        chdir(STR(current_directory));
    }

#ifdef WIN32
    fp = _popen(STR(cmd), "r");
#else
    fp = popen(STR(cmd), "r");
#endif
    if (fp == NULL)
        return "Error: the pipe did not open properly\n";

    string result;
    while (fgets(res, PATH_MAX, fp) != NULL)
    {
        cmd = res;
        result += cmd;
    }

#ifdef WIN32
    status = _pclose(fp);
#else
    status = pclose(fp);
#endif
    if (status == -1)
    {
        result = "Error: when closing the pipe\n";
    }
    else
    {
        if (get_current)
        {
            current_directory = result;
            s_trim(current_directory);
            result = "true\n";
        }
    }
    return result;
}
