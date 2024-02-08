#include "minilisp.h"

// We want lists, numbers and dictionary
error_tokenize code_segmenting(string &code, Segmentingtype &infos, UTF8_Handler* special_characters)
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
            d = convertingfloathexa((char *)buffer.c_str(), lg_value);
            infos.append(d, buffer, jt_number, left, right);
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
            
            //We store the function definition at compile time
            if (e->size() > 3 && e->at(0)->code == l_defun) {
                store_function(e->at(1)->code, e);
                if (e != program)
                    program->pop_raw();
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
}
