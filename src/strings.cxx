/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  strings.cxx
//
//


/*
 Implementation of methods to manage strings
*/

#include "lispe.h"
#include "elements.h"
#include "tools.h"
#include "rgx.h"
#include "tokens.h"

//------------------------------------------------------------
//String method implementation
//------------------------------------------------------------

typedef enum {str_lowercase, str_uppercase, str_is_vowel, str_is_consonant, str_deaccentuate, str_is_emoji, str_is_lowercase, str_is_uppercase, str_is_alpha, str_remplace, str_left, str_right, str_middle, str_trim, str_trim0, str_trimleft, str_trimright, str_base, str_is_digit,
    str_segment_lispe, str_segment_empty, str_split, str_split_empty, str_ord, str_chr, str_is_punctuation,
    str_format, str_padding, str_fill, str_getstruct, str_startwith, str_endwith, str_sprintf,
    str_edit_distance, str_read_json, str_parse_json, str_string_json, str_ngrams,
    str_tokenizer_rules, str_tokenizer_display_rules, str_tokenize_rules, str_tokenizer_main,
    str_get_rules, str_set_rules, str_get_operators, str_set_operators, str_segmenter, str_indent
    
} string_method;

/*
 First of all we create a new Element derivation
 We then implement the "eval" and "asString" methods.
 These are functions around the processing of character strings
 */

class Rulemethod : public Element {
public:
    LispE* lisp;
    
    Rulemethod(LispE* l, int16_t l_rule_tokenize) : Element(l_rule_tokenize) {
        lisp = l;
    }
    
    virtual long size() {
        return 0;
    }
    
    virtual void rulestring(std::wstringstream& str, tokenizer_node* a) {}
    
    virtual u_ustring rule(long pos) {
        return U"";
    }

    virtual tokenizer_node* compiled_rule(long pos) {
        return NULL;
    }
    
    virtual Element* tokenize(u_ustring& u, bool types) {
        return null_;
    }

    virtual Element* tokenize(string& u, bool types) {
        return null_;
    }

    virtual Strings* getrules() {
        return NULL;
    }
    
    virtual void setrules(vecte_n<u_ustring>& v) {}
    
    virtual Element* getoperators() {
        return NULL;
    }
    
    virtual Element* setoperators(Set_s* lst) {
        return null_;
    }
};

class Tokenizermethod : public Rulemethod {
public:
    tokenizer_automaton* tok;
    bool main;
    
    Tokenizermethod(LispE* lisp, int16_t l_rule_tokenize) : Rulemethod(lisp, l_rule_tokenize) {
        tok = new tokenizer_automaton(lisp->handlingutf8);
        tok->setrules();
        tok->compile();
        main = false;
    }

    Tokenizermethod(int16_t l_rule_tokenize, LispE* lisp) : Rulemethod(lisp, l_rule_tokenize) {
        tok = &lisp->delegation->main_tokenizer;
        main = true;
    }

    ~Tokenizermethod() {
        if (!main)
            delete tok;
    }
    
    wstring asString(LispE* lisp) {
        return L"rules";
    }
    
    void rulestring(std::wstringstream& str, tokenizer_node* a) {
        std::set<int16_t> shared;
        tok->asString(shared, str, a, 0, true);
    }
    
    long size() {
        return tok->compiled_rules.size();
    }
    
    u_ustring rule(long pos) {
        return tok->rules[pos];
    }
    
    virtual tokenizer_node* compiled_rule(long pos) {
        return tok->compiled_rules[pos];
    }

    Element* tokenize(u_ustring& u, bool types) {
        Strings* res = lisp->provideStrings();
        tokenizer_result<u_ustring> tokres(&res->liste, types);
        tok->tokenize(u, tokres);
        if (!types)
            return res;
        List* l = lisp->provideList();
        l->append(res);
        Shorts* ty = new Shorts();
        ty->liste = tokres.stacktype;
        l->append(ty);
        return l;
    }

    Element* tokenize(string& u, bool types) {
        Stringbytes* res = new Stringbytes();
        tokenizer_result<string> tokres(&res->liste, types);
        tok->tokenize(u, tokres);
        if (!types)
            return res;
        List* l = lisp->provideList();
        l->append(res);
        Shorts* ty = new Shorts();
        ty->liste = tokres.stacktype;
        l->append(ty);
        return l;
    }


    Strings* getrules() {
        Strings* vstr = lisp->provideStrings();
        tok->get_rules(vstr->liste);
        return vstr;
    }
    
    void setrules(vecte_n<u_ustring>& v) {
        if (main && lisp->id_thread)
            throw new Error("You cannot modify the main tokenizer rules in a thread");
        
        tok->set_rules(v);
    }
    
    Element* getoperators() {
        Set_s* s_str = lisp->provideSet_s();
        u_ustring u;
        for (const auto& a : tok->operators) {
            u = a;
            s_str->add(u);
        }
        return s_str;

    }
    
    Element* setoperators(Set_s* lst) {
        if (main && lisp->id_thread)
            throw new Error("You cannot modify the main tokenizer oeprators in a thread");
        
        tok->operators.clear();
        for (const auto& u: lst->ensemble) {
            if (u.size())
                tok->operators.insert(u[0]);
        }
        return True_;
    }
};

class Segmentmethod : public Rulemethod {
public:
    segmenter_automaton tok;
    
    Segmentmethod(LispE* l, int16_t l_rule_tokenize, bool keepblanks, char decimalpoint) : Rulemethod(l, l_rule_tokenize), tok(l->handlingutf8) {
        tok.setrules();
        tok.compile();
        tok.keepblanks(keepblanks);
        tok.setdecimalmode(decimalpoint);
    }
    
    Element* tokenize(u_ustring& u, bool types) {
        Strings* res = lisp->provideStrings();
        tokenizer_result<u_ustring> tokres(&res->liste, false);
        tok.tokenize<u_ustring>(u, tokres);
        if (!types)
            return res;
        List* l = lisp->provideList();
        l->append(res);
        Shorts* ty = new Shorts();
        ty->liste = tokres.stacktype;
        l->append(ty);
        return l;
    }

    Element* tokenize(string& u, bool types) {
        Stringbytes* res = new Stringbytes();
        tokenizer_result<string> tokres(&res->liste, false);
        tok.tokenize<string>(u, tokres);
        if (!types)
            return res;
        List* l = lisp->provideList();
        l->append(res);
        Shorts* ty = new Shorts();
        ty->liste = tokres.stacktype;
        l->append(ty);
        return l;
    }

    wstring asString(LispE* lisp) {
        return L"segmenter";
    }
    
    long size() {
        return tok.compiled_rules.size();
    }

    u_ustring rule(long pos) {
        return tok.rules[pos];
    }
    
    virtual tokenizer_node* compiled_rule(long pos) {
        return tok.compiled_rules[pos];
    }

    void rulestring(std::wstringstream& str, tokenizer_node* a) {
        std::set<int16_t> shared;
        tok.asString(shared, str, a, 0, true);
    }

    Strings* getrules() {
        Strings* vstr = lisp->provideStrings();
        tok.get_rules(vstr->liste);
        return vstr;
    }

    void setrules(vecte_n<u_ustring>& v) {
        tok.set_rules(v);
    }

    Element* getoperators() {
        Set_s* s_str = lisp->provideSet_s();
        u_ustring u;
        for (const auto& a : tok.operators) {
            u = a;
            s_str->add(u);
        }
        return s_str;

    }

    Element* setoperators(Set_s* lst) {
        tok.operators.clear();
        for (const auto& u: lst->ensemble) {
            if (u.size())
                tok.operators.insert(u[0]);
        }
        return True_;
    }

};

void IndentatingCode(string& str, string& codeindente);
void IndenteCode(string& codestr, string& indentedcode, bool lispmode) {
    indentedcode = "";
    
    s_trimright(codestr);
    codestr+="\n";
    cr_normalise(codestr);
    if (lispmode)
        IndentCode(codestr, indentedcode, GetBlankSize(), true, false);
    else
        IndentatingCode(codestr, indentedcode);
    indentedcode += "\n";
}


class Stringmethod : public Element {
public:
    string_method met;
    int16_t v_str;
    int16_t v_fnd;
    int16_t v_rep;
    int16_t v_nb;
    int16_t v_pos;
    int16_t v_tokenize;
    int16_t v_segment;
    
    Stringmethod(LispE* lisp, string_method s) : met(s), Element(l_lib) {
        //We know the names of variables in advance, so we might as well take advantage of it to retrieve their codes.
        u_ustring nom = U"str";
        v_str = lisp->encode(nom);
        nom = U"fnd";
        v_fnd = lisp->encode(nom);
        nom = U"rep";
        v_rep = lisp->encode(nom);
        nom = U"nb";
        v_nb = lisp->encode(nom);
        nom = U"pos";
        v_pos = lisp->encode(nom);
        nom = U"tokenize_rule";
        v_tokenize = lisp->encode(nom);
        nom = U"segment_rule";
        v_segment = lisp->encode(nom);
    }
    
    Element* parse_json(LispE* lisp, u_ustring& w) {
        LispEJsonCompiler json;
        if (!json.compile(lisp, w)) {
            std::wstringstream wstr;
            wstr << "Error: JSON' structure contains errors line: ";
            wstr << json.line;
            throw new Error(wstr.str());
        }
        return json.compiled_result;
    }
    
    Element* read_json(LispE* lisp, string name) {
        String ch("");
        ch.charge(lisp, name);
        return parse_json(lisp, ch.content);
    }
    
    Element* methodFormat(LispE* lisp, Element* val) {
        string sformat =  val->toString(lisp);
        string v("n%");
        Element* e;
        for (long i = 57; i >= 49; i--) {
            v[1] = (u_uchar)i;
            e = lisp->get_variable(v);
            if (e == null_)
                continue;
            v[0] = '%';
            sformat = s_replacingstring(sformat, v, e->toString(lisp));
            v[0] = 'n';
        }
        return new Stringbyte(sformat);
    }

    Element* methodSprintf(LispE* lisp) {
        string sformat =  lisp->get_variable(v_str)->toString(lisp);
        Element* e = lisp->get_variable(v_nb);
        char value[100];
        
        try {
            switch (e->type) {
                case t_integer: {
                    long nb = e->asInteger();
                    sprintf_s(value, 100, STR(sformat), nb);
                    break;
                }
                case t_float: {
                    float nb = e->asFloat();
                    sprintf_s(value, 100, STR(sformat), nb);
                    break;
                }
                case t_short: {
                    short nb = e->asShort();
                    sprintf_s(value, 100, STR(sformat), nb);
                    break;
                }
                case t_number: {
                    double nb = e->asNumber();
                    sprintf_s(value, 100, STR(sformat), nb);
                    break;
                }
                default:
                    throw new Error("Error: 'spritnf' can only be used with numerical values");
            }
        }
        catch(...) {
            throw new Error("Error: wrong format");
        }
        sformat = value;
        return lisp->provideString(sformat);
    }
    
    Element* methodFormat(LispE* lisp) {
        Element* val = lisp->get_variable(v_str);
        if (val->type == t_stringbyte)
            return methodFormat(lisp, val);
        
        u_ustring sformat =  val->asUString(lisp);

        //In this case, we might have more than two parameters...
        u_ustring v(U"n%");
        Element* e;
        for (long i = 57; i >= 49; i--) {
            v[1] = (u_uchar)i;
            e = lisp->get_variable(v);
            if (e == null_)
                continue;
            v[0] = U'%';
            sformat = s_ureplacestring(sformat, v, e->asUString(lisp));
            v[0] = U'n';
        }
        return lisp->provideString(sformat);
    }

    Element* methodPadding(LispE* lisp, Element* val, long nb) {
        string value =  val->toString(lisp);
        string sval = lisp->get_variable(U"c")->toString(lisp);
        long sz = nb - value.size();
        if (sval.size() == 1) {
            while (sz) {
                value += sval;
                sz--;
            }
        }
        else {
            long i = 0;
            nb = sval.size();
            while (sz) {
                value += sval[i++];
                sz--;
                if (i == nb)
                    i = 0;
            }
        }
        
        return new Stringbyte(value);
    }

    Element* methodPadding(LispE* lisp) {
        Element* val = lisp->get_variable(v_str);
        long nb = lisp->get_variable(v_nb)->asInteger();

        if (val->type == t_stringbyte)
            return methodPadding(lisp, val, nb);
        
        u_ustring value =  val->asUString(lisp);
        u_ustring sval = lisp->get_variable(U"c")->asUString(lisp);
        long sz = nb - value.size();
        if (sval.size() == 1) {
            while (sz) {
                value += sval;
                sz--;
            }
        }
        else {
            long i = 0;
            nb = sval.size();
            while (sz) {
                value += sval[i++];
                sz--;
                if (i == nb)
                    i = 0;
            }
        }
        
        return lisp->provideString(value);
    }

    Element* getstruct8(LispE* lisp, Element* var) {
        string value =  var->toString(lisp);
        string s_open = lisp->get_variable(U"open")->toString(lisp);
        string s_close = lisp->get_variable(U"close")->toString(lisp);
        long i = lisp->get_variable(U"pos")->asInteger();
        
        uchar o = s_open[0];
        uchar c = s_close[0];
        
        
        long sz = value.size();
        while (i < sz && value[i] != o) i++;
        
        if (i == sz)
            return null_;
        
        long initial = i;
        
        //We extract a full-fledge structure...
        //Which could contain embeddings...
        long count = 1;
        
        if (value[++i] == c) {
            string str = s_open + s_close;
            List* l = lisp->provideList();
            l->append(lisp->provideString(str));
            l->append(lisp->provideInteger(initial));
            l->append(lisp->provideInteger(i+1));
            return l;
        }

        while (count && i < sz) {
            if (value[i] == o)
                count++;
            if (value[i] == '"') {
                i++;
                while (i < sz && value[i] != '"') {
                    if (value[i] == '\\')
                        i++;
                    i++;
                }
            }
            if (value[++i] == c)
                count--;
        }
        
        if (i == sz)
            return null_;

        i++;
        string str = value.substr(initial, i - initial);
        List* l = lisp->provideList();
        l->append(lisp->provideString(str));
        l->append(lisp->provideInteger(initial));
        l->append(lisp->provideInteger(i));
        return l;
    }
    
    Element* getstruct(LispE* lisp) {
        Element* var = lisp->get_variable(v_str);
        
        if (var->type == t_stringbyte)
            return getstruct8(lisp,var);
        
        u_ustring value =  var->asUString(lisp);
        u_ustring s_open = lisp->get_variable(U"open")->asUString(lisp);
        u_ustring s_close = lisp->get_variable(U"close")->asUString(lisp);
        long i = lisp->get_variable(U"pos")->asInteger();
        
        u_uchar o = s_open[0];
        u_uchar c = s_close[0];
        
        
        long sz = value.size();
        while (i < sz && value[i] != o) i++;
        
        if (i == sz)
            return null_;
        
        long initial = i;
        
        //We extract a full-fledge structure...
        //Which could contain embeddings...
        long count = 1;
        
        if (value[++i] == c) {
            u_ustring str = s_open + s_close;
            List* l = lisp->provideList();
            l->append(lisp->provideString(str));
            l->append(lisp->provideInteger(initial));
            l->append(lisp->provideInteger(i+1));
            return l;
        }

        while (count && i < sz) {
            if (value[i] == o)
                count++;
            if (value[i] == '"') {
                i++;
                while (i < sz && value[i] != '"') {
                    if (value[i] == '\\')
                        i++;
                    i++;
                }
            }
            if (value[++i] == c)
                count--;
        }
        
        if (i == sz)
            return null_;

        i++;
        u_ustring str = value.substr(initial, i - initial);
        List* l = lisp->provideList();
        l->append(lisp->provideString(str));
        l->append(lisp->provideInteger(initial));
        l->append(lisp->provideInteger(i));
        return l;
    }
    
    Element* methodFill(LispE* lisp) {
        Element* v = lisp->get_variable(v_str);
        long nb = lisp->get_variable(v_nb)->asInteger();
        if (v->type == t_stringbyte) {
            string sval;
            if (nb <= 0)
                return new Stringbyte(sval);
            
            sval = v->toString(lisp);
            string val;
            while (nb > 0) {
                val += sval;
                nb--;
            }
            
            return new Stringbyte(val);
        }
        
        u_ustring sval;
        if (nb <= 0)
            return lisp->provideString(sval);
        
        sval = v->asUString(lisp);
        u_ustring val;
        while (nb > 0) {
            val += sval;
            nb--;
        }
        
        return lisp->provideString(val);
    }
    
    Element* methodBase(LispE* lisp) {
        static vector<u_ustring> caracs;
        static std::unordered_map<u_uchar, long> mcaracs;
        u_ustring w;
        long n, b = -1;

        bool toconvert = lisp->get_variable(U"convert")->Boolean();
        b = lisp->get_variable(U"b")->asInteger();
        if (b <= 1)
            throw new Error("Error: cannot convert to this base");
        
        if (caracs.size() == 0) {
            w = U"0";
            for (n = 0; n < 10; n++) {
                mcaracs[w[0]] = caracs.size();
                caracs.push_back(w);
                w[0]++;
            }
            w = U"A";
            for (n = 10; n < 36; n++) {
                mcaracs[w[0]] = caracs.size();
                caracs.push_back(w);
                w[0]++;
            }
            w = U"a";
            for (n = 36; n < 62; n++) {
                mcaracs[w[0]] = caracs.size();
                caracs.push_back(w);
                w[0]++;
            }
            w = U"#";
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w = U"@";
            mcaracs[w[0]] = caracs.size();
            caracs.push_back(w);
            w = U"";
            if (!b)
                return True_;
        }
        
        if (b > caracs.size()) {
            return new Error("Error: Base too large");
        }

        unsigned long v = 0;
        if (!toconvert) {
            //we convert a base 10 number into the local base
            v = lisp->get_variable(v_str)->asInteger();
            long rest;
            u_ustring res = U"";
            while (v) {
                rest = v%b;
                v /=b;
                res = caracs[rest]+res;
            }
            return lisp->provideString(res);
        }

        w = lisp->get_variable(v_str)->asUString(lisp);
        wchar_t wc;
        for (n = 0; n < w.size(); n++) {
            wc = w[n];
            if (!mcaracs.count(wc) || mcaracs[wc] >= b)
                throw new Error(U"Error: Cannot analyze this string in this base.");

            v *= b;
            v += mcaracs[wc];
        }
        return lisp->provideInteger(v);
    }
    
    #define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))
    Element* methodEditDistance8(LispE* lisp, Element* v) {
        unsigned long s1len, s2len, x, y, lastdiag, olddiag;

        string s1 = v->toString(lisp);
        string s2 = lisp->get_variable("strbis")->toString(lisp);
        
        s1len = s1.size();
        s2len = s2.size();
        size_t* column = new size_t[s1len + 1];
        for (y = 1; y <= s1len; y++)
            column[y] = y;
        for (x = 1; x <= s2len; x++) {
            column[0] = x;
            for (y = 1, lastdiag = x - 1; y <= s1len; y++) {
                olddiag = column[y];
                column[y] = MIN3(column[y] + 1, column[y - 1] + 1, lastdiag + (s1[y - 1] == s2[x - 1] ? 0 : 1));
                lastdiag = olddiag;
            }
        }
        s2len = column[s1len];
        delete[] column;
        return lisp->provideInteger(s2len);
    }
    
    Element* methodEditDistance(LispE* lisp) {
        Element* v = lisp->get_variable(v_str);

        if (v->type == t_stringbyte)
            return methodEditDistance8(lisp, v);
        
        unsigned long s1len, s2len, x, y, lastdiag, olddiag;

        u_ustring s1 = lisp->get_variable(v_str)->asUString(lisp);
        u_ustring s2 = lisp->get_variable("strbis")->asUString(lisp);
        
        s1len = s1.size();
        s2len = s2.size();
        size_t* column = new size_t[s1len + 1];
        for (y = 1; y <= s1len; y++)
            column[y] = y;
        for (x = 1; x <= s2len; x++) {
            column[0] = x;
            for (y = 1, lastdiag = x - 1; y <= s1len; y++) {
                olddiag = column[y];
                column[y] = MIN3(column[y] + 1, column[y - 1] + 1, lastdiag + (s1[y - 1] == s2[x - 1] ? 0 : 1));
                lastdiag = olddiag;
            }
        }
        s2len = column[s1len];
        delete[] column;
        return lisp->provideInteger(s2len);
    }

    Element* method_replace(LispE* lisp, Element* end) {
        u_ustring strvalue = end->asUString(lisp);
        
        end = lisp->get_variable(U"index");

        u_ustring cherche;
        if (end == null_) {
            cherche = lisp->get_variable(v_fnd)->asUString(lisp);
            u_ustring remplacement = lisp->get_variable(v_rep)->asUString(lisp);
            strvalue = s_ureplacestring(strvalue,cherche, remplacement);
            return lisp->provideString(strvalue);
        }
        //In this case, we have indexes...
        u_ustring remplacement = lisp->get_variable(v_fnd)->asUString(lisp);
        long i_beg = lisp->get_variable(v_rep)->asInteger();
        long i_end = end->asInteger();
        long sz = strvalue.size();
        if (i_beg < 0)
            i_beg = sz + i_beg;
        if (i_beg >= sz)
            throw new Error("Error: out of range");
        if (i_beg < 0)
            i_beg = 0;
        if (i_end < 0)
            i_end = sz + i_end;
        if (i_end < i_beg)
            throw new Error("Error: out of range");
        if (i_end > sz)
            i_end = sz;
        cherche = strvalue.substr(0, i_beg);
        cherche += remplacement;
        cherche += strvalue.substr(i_end, sz);
        return lisp->provideString(cherche);
    }

    Element* method_replace8(LispE* lisp, Element* end) {
        string strvalue = end->toString(lisp);
        
        end = lisp->get_variable(U"index");

        string cherche;
        if (end == null_) {
            cherche = lisp->get_variable(v_fnd)->toString(lisp);
            string remplacement = lisp->get_variable(v_rep)->toString(lisp);
            strvalue = s_replacingstring(strvalue,cherche, remplacement);
            return lisp->provideString(strvalue);
        }
        //In this case, we have indexes...
        string remplacement = lisp->get_variable(v_fnd)->toString(lisp);
        long i_beg = lisp->get_variable(v_rep)->asInteger();
        long i_end = end->asInteger();
        long sz = strvalue.size();
        if (i_beg < 0)
            i_beg = sz + i_beg;
        if (i_beg >= sz)
            throw new Error("Error: out of range");
        if (i_beg < 0)
            i_beg = 0;
        if (i_end < 0)
            i_end = sz + i_end;
        if (i_end < i_beg)
            throw new Error("Error: out of range");
        if (i_end > sz)
            i_end = sz;
        cherche = strvalue.substr(0, i_beg);
        cherche += remplacement;
        cherche += strvalue.substr(i_end, sz);
        return new Stringbyte(cherche);
    }

    Element* method_split_byte(LispE* lisp, Element* vstr) {
        string strvalue =  vstr->toString(lisp);
        Element* u_find = lisp->get_variable(v_fnd);
        Stringbytes* result = new Stringbytes();
        string search_string;
        
        if (u_find == null_) {
            u_uchar c;
            long sz = strvalue.size();
            for (long i = 0; i < sz; i++) {
                c = strvalue[i];
                if (c <= 32) {
                    if (search_string != "") {
                        result->liste.push_back(search_string);
                        search_string = "";
                    }
                }
                else
                    search_string += c;
            }
            if (search_string != "")
                result->liste.push_back(search_string);
            return result;
        }
        
        string localvalue;
        long pos = 0;
        
        search_string = u_find->toString(lisp);
        if (search_string == "") {
            long sz = strvalue.size();
            //we split the string into an array of characters
            while (pos < sz) {
                lisp->handlingutf8->getchar(strvalue, localvalue, pos);
                result->append(localvalue);
            }
            return result;
        }

        size_t found = 0;
        while (pos != string::npos) {
            found = strvalue.find(search_string, pos);
            if (found != string::npos) {
                localvalue = strvalue.substr(pos, found - pos);
                if (localvalue != "") {
                    result->append(localvalue);
                }
                pos = found + search_string.size();
            }
            else
                break;
        }
        
        localvalue = strvalue.substr(pos, strvalue.size() - pos);
        if (localvalue != "") {
            result->append(localvalue);
        }
        
        return result;
    }

    Element* method_startwith(LispE* lisp) {
        Element* vstr = lisp->get_variable(v_str);
        Element* vfnd = lisp->get_variable(v_fnd);
        long v_size;
        long f_size;

        if (vstr->type == t_stringbyte) {
            string str = vstr->toString(lisp);
            string fnd = vfnd->toString(lisp);
            v_size = str.size();
            f_size = fnd.size();
            return (f_size && f_size <= v_size)?booleans_[str.substr(0, f_size) == fnd]:null_;
        }
        
        u_ustring str = vstr->asUString(lisp);
        u_ustring fnd = vfnd->asUString(lisp);

        v_size = str.size();
        f_size = fnd.size();
        return (f_size && f_size <= v_size)?booleans_[str.substr(0, f_size) == fnd]:null_;
    }

    Element* method_endwith(LispE* lisp) {
        Element* vstr = lisp->get_variable(v_str);
        Element* vfnd = lisp->get_variable(v_fnd);
        long v_size;
        long f_size;

        if (vstr->type == t_stringbyte) {
            string str = vstr->toString(lisp);
            string fnd = vfnd->toString(lisp);
            
            v_size = str.size();
            f_size = fnd.size();
            return (f_size && f_size <= v_size)?booleans_[str.substr(v_size - f_size, f_size) == fnd]:null_;
        }
        
        u_ustring str = vstr->asUString(lisp);
        u_ustring fnd = vfnd->asUString(lisp);
        v_size = str.size();
        f_size = fnd.size();

        return (f_size && f_size <= v_size)?booleans_[str.substr(v_size - f_size, f_size) == fnd]:null_;
    }

    Element* method_split(LispE* lisp) {
        Element* vstr = lisp->get_variable(v_str);
        if (vstr->type == t_stringbyte)
            return method_split_byte(lisp, vstr);
        
        u_ustring strvalue =  vstr->asUString(lisp);
        Element* u_find = lisp->get_variable(v_fnd);
        Strings* result = lisp->provideStrings();
        u_ustring search_string;
        
        if (u_find == null_) {
            u_uchar c;
            long sz = strvalue.size();
            for (long i = 0; i < sz; i++) {
                c = strvalue[i];
                if (c <= 32) {
                    if (search_string != U"") {
                        result->liste.push_back(search_string);
                        search_string = U"";
                    }
                }
                else
                    search_string += c;
            }
            if (search_string != U"")
                result->liste.push_back(search_string);
            return result;
        }
        
        u_ustring localvalue;
        long pos = 0;
        
        search_string = u_find->asUString(lisp);
        if (search_string == U"") {
            long sz = strvalue.size();
            //we split the string into an array of characters
            while (pos < sz) {
                lisp->handlingutf8->getchar(strvalue, localvalue, pos);
                result->append(localvalue);
            }
            return result;
        }

        size_t found = 0;
        while (pos != string::npos) {
            found = strvalue.find(search_string, pos);
            if (found != string::npos) {
                localvalue = strvalue.substr(pos, found - pos);
                if (localvalue != U"") {
                    result->append(localvalue);
                }
                pos = found + search_string.size();
            }
            else
                break;
        }
        
        localvalue = strvalue.substr(pos, strvalue.size() - pos);
        if (localvalue != U"") {
            result->append(localvalue);
        }
        
        return result;
    }
    
    Element* method_splite_byte(LispE* lisp, Element* vstr) {
        string strvalue =  vstr->toString(lisp);
        Element* u_find = lisp->get_variable(v_fnd);
        Stringbytes* result = new Stringbytes();
        string search_string;
        
        //Cutting at space characters
        if (u_find == null_) {
            u_uchar c;
            long sz = strvalue.size();
            for (long i = 0; i < sz; i++) {
                c = strvalue[i];
                if (c <= 32) {
                    result->liste.push_back(search_string);
                    search_string = "";
                }
                else
                    search_string += c;
            }
            if (search_string != "")
                result->liste.push_back(search_string);
            return result;
        }
        
        string localvalue;
        long pos = 0;

        search_string = u_find->toString(lisp);
        if (search_string == "") {
            long sz = strvalue.size();
            //we split the string into an array of characters
            while (pos < sz) {
                lisp->handlingutf8->getchar(strvalue, localvalue, pos);
                result->append(localvalue);
            }
            return result;
        }

        size_t found = 0;
        while (pos != string::npos) {
            found = strvalue.find(search_string, pos);
            if (found != string::npos) {
                localvalue = strvalue.substr(pos, found - pos);
                result->append(localvalue);
                pos = found + search_string.size();
            }
            else
                break;
        }

        if (strvalue.size() < pos) {
            localvalue = strvalue.substr(pos, strvalue.size() - pos);
            result->append(localvalue);
        }
    
        return result;

    }
    
    Element* method_splite(LispE* lisp) {
        Element* vstr = lisp->get_variable(v_str);
        if (vstr->type == t_stringbyte)
            return method_splite_byte(lisp, vstr);
        
        u_ustring strvalue =  vstr->asUString(lisp);
        Element* u_find = lisp->get_variable(v_fnd);
        Strings* result = lisp->provideStrings();
        u_ustring search_string;
        
        if (u_find == null_) {
            u_uchar c;
            long sz = strvalue.size();
            for (long i = 0; i < sz; i++) {
                c = strvalue[i];
                if (c <= 32) {
                    result->liste.push_back(search_string);
                    search_string = U"";
                }
                else
                    search_string += c;
            }
            if (search_string != U"")
                result->liste.push_back(search_string);
            return result;
        }
        
        u_ustring localvalue;
        long pos = 0;

        search_string = u_find->asUString(lisp);
        if (search_string == U"") {
            long sz = strvalue.size();
            //we split the string into an array of characters
            while (pos < sz) {
                lisp->handlingutf8->getchar(strvalue, localvalue, pos);
                result->append(localvalue);
            }
            return result;
        }

        size_t found = 0;
        while (pos != string::npos) {
            found = strvalue.find(search_string, pos);
            if (found != string::npos) {
                localvalue = strvalue.substr(pos, found - pos);
                result->append(localvalue);
                pos = found + search_string.size();
            }
            else
                break;
        }

        if (strvalue.size() < pos) {
            localvalue = strvalue.substr(pos, strvalue.size() - pos);
            result->append(localvalue);
        }
    
        return result;

    }

    Element* method_ngrams(LispE* lisp, long nb, Element* vstr) {
        string s =  vstr->toString(lisp);
        long j;
        long mx = s.size() - nb + 1;
        string u;
        Stringbytes* ke = new Stringbytes();
        for (long i = 0; i < mx; i++) {
            u = "";
            for (j = i; j < i + nb; j++) {
                u += lisp->handlingutf8->getachar(s,j);
            }
            ke->liste.push_back(u);
        }
        return ke;
    }

    Element* method_ngrams(LispE* lisp, long nb) {
        Element* vstr = lisp->get_variable(v_str);
        if (vstr->type == t_stringbyte)
            return method_ngrams(lisp, nb, vstr);
        
        u_ustring s =  vstr->asUString(lisp);
        long j;
        long mx = s.size() - nb + 1;
        u_ustring u;
        Strings* ke = lisp->provideStrings();
        for (long i = 0; i < mx; i++) {
            u = U"";
            for (j = i; j < i + nb; j++) {
                u += lisp->handlingutf8->getachar(s,j);
            }
            ke->liste.push_back(u);
        }
        return ke;
    }
    
    Element* eval(LispE* lisp) {
        switch (met) {
            case str_remplace: {
                Element* end =  lisp->get_variable(v_str);
                if (end->type == t_stringbyte)
                    return method_replace8(lisp, end);
                if (end->type != t_string)
                    throw new Error("Error: cannot apply 'replace' to this type of object");
                return method_replace(lisp, end);
            }
            case str_lowercase: {
                Element* v = lisp->get_variable(v_str);
                u_ustring s =  v->asUString(lisp);
                s = lisp->handlingutf8->u_to_lower(s);
                return lisp->provideString(s);
            }
            case str_uppercase: {
                Element* v = lisp->get_variable(v_str);
                u_ustring s =  v->asUString(lisp);
                s = lisp->handlingutf8->u_to_upper(s);
                if (v->type == t_stringbyte)
                    return new Stringbyte(s);
                return lisp->provideString(s);
            }
            case str_is_emoji: {
                Element* v = lisp->get_variable(v_str);
                u_ustring s =  v->asUString(lisp);
                if (v->type == t_stringbyte)
                    return new Stringbyte(s);
                return booleans_[lisp->handlingutf8->u_is_emoji(s)];
            }
            case str_is_lowercase: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->u_is_lower(s)];
            }
            case str_is_uppercase: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->u_is_upper(s)];
            }
            case str_is_alpha: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->u_is_alpha(s)];
            }
            case str_is_digit: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->s_is_digit(s)];
            }
            case str_left: {
                Element* v = lisp->get_variable(v_str);
                long n = lisp->get_variable(v_nb)->asInteger();
                if (v->type == t_stringbyte) {
                    string s = v->toString(lisp);
                    s = s_left(s,n);
                    return new Stringbyte(s);
                }
                u_ustring s =  v->asUString(lisp);
                s = s_uleft(s,n);
                return lisp->provideString(s);
            }
            case str_right: {
                Element* v = lisp->get_variable(v_str);
                long n = lisp->get_variable(v_nb)->asInteger();
                if (v->type == t_stringbyte) {
                    string s = v->toString(lisp);
                    s = s_right(s,n);
                    return new Stringbyte(s);
                }
                u_ustring s =  v->asUString(lisp);
                s = s_uright(s, n);
                return lisp->provideString(s);
            }
            case str_middle: {
                Element* v = lisp->get_variable(v_str);
                long p = lisp->get_variable(v_pos)->asInteger();
                long n = lisp->get_variable(v_nb)->asInteger();
                if (v->type == t_stringbyte) {
                    string strvalue =  v->toString(lisp);
                    strvalue = s_middle(strvalue,p,n);
                    return new Stringbyte(strvalue);
                }
                u_ustring strvalue =  v->asUString(lisp);
                strvalue = s_umiddle(strvalue,p,n);
                return lisp->provideString(strvalue);
            }
            case str_ngrams: {
                long nb = lisp->get_variable(v_nb)->asNumber();
                if (nb <= 0)
                    throw new Error("Error: nb should be a positive value");
                return method_ngrams(lisp, nb);
            }
            case str_trim0: {
                Element* v = lisp->get_variable(v_str);
                if (v->type == t_stringbyte) {
                    string strvalue =  v->toString(lisp);
                    strvalue = s_trim0(strvalue);
                    return new Stringbyte(strvalue);
                }
                
                u_ustring strvalue =  v->asUString(lisp);
                strvalue = u_trim0(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trim:  {
                Element* v = lisp->get_variable(v_str);
                if (v->type == t_stringbyte) {
                    string strvalue =  v->toString(lisp);
                    strvalue = s_trim(strvalue);
                    return new Stringbyte(strvalue);
                }
                
                u_ustring strvalue =  v->asUString(lisp);
                strvalue = u_trim(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trimleft:  {
                Element* v = lisp->get_variable(v_str);
                if (v->type == t_stringbyte) {
                    string strvalue =  v->toString(lisp);
                    strvalue = s_trimleft(strvalue);
                    return new Stringbyte(strvalue);
                }
                
                u_ustring strvalue =  v->asUString(lisp);
                strvalue = u_trimleft(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trimright:  {
                Element* v = lisp->get_variable(v_str);
                if (v->type == t_stringbyte) {
                    string strvalue =  v->toString(lisp);
                    strvalue = s_trimright(strvalue);
                    return new Stringbyte(strvalue);
                }
                
                u_ustring strvalue =  v->asUString(lisp);
                strvalue = u_trimright(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_split_empty:
                return method_splite(lisp);
            case str_getstruct:
                return getstruct(lisp);
            case str_split:
                return method_split(lisp);
            case str_startwith:
                return method_startwith(lisp);
            case str_endwith:
                return method_endwith(lisp);
            case str_ord: {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                if (strvalue.size() == 0)
                    return emptylist_;
                
                Integers* liste =  lisp->provideIntegers();
                for (long i = 0; i < strvalue.size(); i++) {
                    liste->liste.push_back(strvalue[i]);
                }
                return liste;
            }
            case str_chr: {
                u_uchar c = (u_uchar)lisp->get_variable(v_nb)->asInteger();
                return lisp->provideString(c);
            }
            case str_is_punctuation: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->u_is_punctuation(s)];
            }
            case str_read_json: {
                string filename = lisp->get_variable(U"filename")->toString(lisp);
                return read_json(lisp, filename);
            }
            case str_parse_json: {
                u_ustring str = lisp->get_variable(U"str")->asUString(lisp);
                return parse_json(lisp, str);
            }
            case str_string_json: {
                Element* e = lisp->get_variable(U"element");
                wstring w = e->jsonString(lisp);
                return lisp->provideString(w);
            }
            case str_is_vowel: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->s_is_vowel(s)];
            }
            case str_is_consonant: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                return booleans_[lisp->handlingutf8->s_is_consonant(s)];
            }
            case str_deaccentuate: {
                Element* v = lisp->get_variable(v_str);
                u_ustring s =  v->asUString(lisp);
                s = lisp->handlingutf8->s_deaccentuate(s);
                if (v->type == t_stringbyte)
                    return new Stringbyte(s);
                return lisp->provideString(s);
            }
            case str_segment_lispe: {
                Element* vstr = lisp->get_variable(v_str);
                short point = lisp->get_variable("point")->asShort();
                if (vstr->type == t_stringbyte) {
                    string strvalue =  vstr->toString(lisp);
                    return lisp->tokenize(strvalue, false, point);
                }
                u_ustring strvalue =  vstr->asUString(lisp);
                return lisp->tokenize(strvalue, false, point);
            }
            case str_segment_empty: {
                Element* vstr = lisp->get_variable(v_str);
                short point = lisp->get_variable("point")->asShort();
                if (vstr->type == t_stringbyte) {
                    string strvalue =  vstr->toString(lisp);
                    return lisp->tokenize(strvalue, true, point);
                }
                u_ustring strvalue =  vstr->asUString(lisp);
                return lisp->tokenize(strvalue, true, point);
            }
            case str_tokenizer_main: {
                return new Tokenizermethod(v_tokenize, lisp);
            }
            case str_tokenizer_rules: {
                return new Tokenizermethod(lisp, v_tokenize);
            }
            case str_segmenter: {
                bool keep = lisp->get_variable("keepblanks")->Boolean();
                short point = lisp->get_variable("point")->asShort();
                return new Segmentmethod(lisp, v_tokenize, keep, point);
            }
            case str_tokenizer_display_rules: {
                Element* rtok = lisp->get_variable(U"rules");
                if (rtok->type != v_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Rulemethod* tok = (Rulemethod*)rtok;
                std::wstringstream str;
                tokenizer_node* a;
                u_ustring r;
                for (long i = 0; i < tok->size(); i++) {
                    a = tok->compiled_rule(i);
                    if (a != NULL) {
                        r = tok->rule(i);
                        str << _u_to_w(r) << endl;
                        tok->rulestring(str, a);
                    }
                }
                wstring w = str.str();
                return lisp->provideString(w);
            }
            case str_tokenize_rules: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != v_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Element* types = lisp->get_variable(U"types");
                Element* vstr = lisp->get_variable(v_str);
                if (vstr->type == t_stringbyte) {
                    string s =  vstr->toString(lisp);
                    return ((Rulemethod*)tok)->tokenize(s, types->Boolean());
                }
                u_ustring s =  vstr->asUString(lisp);
                return ((Rulemethod*)tok)->tokenize(s, types->Boolean());
            }
            case str_get_rules: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != v_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                return ((Rulemethod*)tok)->getrules();
            }
            case str_set_rules: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != v_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Element* lst = lisp->get_variable(U"lst");
                if (!lst->isList())
                    throw new Error("Error: This function expects a list");
                vecte_n<u_ustring> wlst;
                for (long i = 0; i < lst->size(); i++) {
                    wlst.push_back(lst->index(i)->asUString(lisp));
                }
                ((Rulemethod*)tok)->setrules(wlst);
                return True_;
            }
            case str_get_operators: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != v_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                return ((Rulemethod*)tok)->getoperators();
            }
            case str_set_operators: {
                Element* rtok = lisp->get_variable(U"rules");
                if (rtok->type != v_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Element* lst = lisp->get_variable(U"a_set");
                if (lst->type != t_sets)
                    throw new Error("Error: This function expects a set of strings (sets)");
                return ((Rulemethod*)rtok)->setoperators((Set_s*)lst);
            }
            case str_format: {
                return methodFormat(lisp);
            }
            case str_sprintf: {
                return methodSprintf(lisp);
            }
            case str_fill: {
                return methodFill(lisp);
            }
            case str_padding: {
                return methodPadding(lisp);
            }
            case str_edit_distance: {
                return methodEditDistance(lisp);
            }
            case str_base: {
                return methodBase(lisp);
            }
            case str_indent: {
                bool lm = lisp->get_variable(U"lispmode")->Boolean();
                string s =  lisp->get_variable(v_str)->toString(lisp);
                string ide;
                IndenteCode(s, ide, lm);
                return lisp->provideString(ide);
            }
        }
		return null_;
    }
    
    //We use this instruction to return a description of the instruction
    //in effect, just do: (print getenv) to get this information
    wstring asString(LispE* lisp) {
        switch (met) {
            case str_is_emoji:
                return L"Return true is the character is an emoji";
            case str_remplace: {
                return L"Replaces all sub-strings";
            }
            case str_lowercase: {
                return L"Put in lower case";
            }
            case str_uppercase: {
                return L"Put in uppercase";
            }
            case str_is_lowercase: {
                return L"Checks if the string is only lowercase";
            }
            case str_is_uppercase:{
                return L"Checks if the string contains only uppercases";
            }
            case str_is_alpha: {
                return L"Checks if the string contains only alphabetic characters";
            }
            case str_is_digit: {
                return L"Checks if the string contains only digits";
            }
            case str_left: {
                return L"Returns the 'n' characters to left";
            }
            case str_right: {
                return L"Returns the last 'n' characters to right";
            }
            case str_middle: {
                return L"Returns the 'n' characters from position 'p'";
            }
            case str_trim0:  {
                return L"Trim '0' at the end of string";
            }
            case str_trim:  {
                return L"Trim all characters 'space'";
            }
            case str_trimleft:  {
            }
                return L"Trim all 'space' characters to left";
            case str_trimright:  {
                return L"Trim all 'space' characters to right";
            }
            case str_split: {
                return L"Splits the string into sub-strings according to a given string";
            }
            case str_split_empty: {
                return L"Splits the string into sub-strings according to a given string. Keeps empty values";
            }
            case str_ord: {
                return L"Returns the Unicode codes of each 'str' character";
            }
            case str_chr: {
                return L"Returns the Unicode character corresponding to the code 'nb'";
            }
            case str_is_punctuation: {
                return L"Checks if the string contains only punctuation marks";
            }
            case str_ngrams:
                return L"Builds a list of ngrams of size nb";
            case str_read_json:
                return L"Reads a JSON file";
            case str_parse_json:
                return L"Parse a JSON string";
            case str_string_json:
                return L"Returns a JSON string";
            case str_is_vowel:
                return L"Check if a string only contains vowels";
            case str_is_consonant:
                return L"Check if a string only contains consonants";
            case str_deaccentuate:
                return L"Remove the accents from letters in a string";
            case str_segment_lispe:
                return L"Tokenize a string into a list of tokens with LispE tokenizer";
            case str_segment_empty:
                return L"Tokenize a string into a list of tokens with LispE tokenize. Keep also the blanks";
            case str_tokenizer_display_rules:
                return L"Display the underlying automata, into which rules were compiled";
            case str_tokenize_rules:
                return L"Tokenize a string into a list of tokens with internal rules";
            case str_get_rules:
                return L"Return the internal tokenization rules";
            case str_set_rules:
                return L"Set the internal tokenization rules";
            case str_tokenizer_rules:
                return L"Return a rule object";
            case str_tokenizer_main:
                return L"Return LispE internal tokenizer";
            case str_format:
                return L"Takes as input a format and a list of variables. Variables in the format of the form: %n, where 1<=n<=9 are replaced with their corresponding arguments";
            case str_sprintf:
                return L"Uses 'sprintf' format to display values";
            case str_padding:
                return L"(padding str c nb): padds the string str with c up to 'nb' characters";
            case str_fill:
                return L"(fill c nb): creates a string made of nb 'c' characters";
            case str_edit_distance:
                return L"(editdistance str strbis): compute the Levenshtein distance between str and strbis";
            case str_base:
                return L"(convert_in_base str b (convert_from): convert str into b or from base b according to convert_from";
            case str_getstruct:
                return L"(getstruct str open close (pos)): extracts a sub-string from an 'open' chararacter to a 'close' character starting at 'pos'";
            case str_get_operators:
                return L"Returns the list of operators, which are used with metacharacter: %o";
            case str_set_operators:
                return L"Sets the list of operators, which are used with metacharacter: %o";
            case str_segmenter:
                return L"Returns the segmenter object: (segmenter keepblanks point). point == 0 is regular decimal point. point == 1 uses comma as decimal point. point == 2 enables both point and comma.";
            case str_indent:
                return L"Indent a string containing code";
            case str_startwith:
                return L"Check if a string starts with a given string";
            case str_endwith:
                return L"Check if a string ends with a given string";
        }
		return L"";
    }
};

//We are also going to implement the body of the call
void moduleChaines(LispE* lisp) {
    //We first create the body of the function

    lisp->extension("deflib trim0 (str)", new Stringmethod(lisp, str_trim0));
    lisp->extension("deflib trim (str)", new Stringmethod(lisp, str_trim));
    lisp->extension("deflib trimleft (str)", new Stringmethod(lisp, str_trimleft));
    lisp->extension("deflib trimright (str)", new Stringmethod(lisp, str_trimright));
    lisp->extension("deflib lower (str)", new Stringmethod(lisp, str_lowercase));
    lisp->extension("deflib format (str n1 (n2) (n3) (n4) (n5) (n6) (n7) (n8) (n9))", new Stringmethod(lisp, str_format));
    lisp->extension("deflib sprintf (nb str)", new Stringmethod(lisp, str_sprintf));
    lisp->extension("deflib upper (str)", new Stringmethod(lisp, str_uppercase));
    lisp->extension("deflib replace (str fnd rep (index))", new Stringmethod(lisp, str_remplace));
    lisp->extension("deflib convert_in_base (str b (convert))", new Stringmethod(lisp, str_base));
    lisp->extension("deflib left (str nb)", new Stringmethod(lisp, str_left));
    lisp->extension("deflib ngrams (str nb)", new Stringmethod(lisp, str_ngrams));
    lisp->extension("deflib right (str nb)", new Stringmethod(lisp, str_right));
    lisp->extension("deflib middle (str pos nb)", new Stringmethod(lisp, str_middle));
    lisp->extension("deflib getstruct (str open close (pos 0))", new Stringmethod(lisp, str_getstruct));
    lisp->extension("deflib split (str (fnd))", new Stringmethod(lisp, str_split));
    lisp->extension("deflib splite (str (fnd))", new Stringmethod(lisp, str_split_empty));
    lisp->extension("deflib ord (str)", new Stringmethod(lisp, str_ord));
    lisp->extension("deflib chr (nb)", new Stringmethod(lisp, str_chr));
    lisp->extension("deflib padding (str c nb)", new Stringmethod(lisp, str_padding));
    lisp->extension("deflib fill (str nb)", new Stringmethod(lisp, str_fill));
    lisp->extension("deflib editdistance (str strbis)", new Stringmethod(lisp, str_edit_distance));
    lisp->extension("deflib deaccentuate (str)", new Stringmethod(lisp, str_deaccentuate));
    lisp->extension("deflib startwith (str fnd)", new Stringmethod(lisp, str_startwith));
    lisp->extension("deflib endwith (str fnd)", new Stringmethod(lisp, str_endwith));

    lisp->extension("deflib lowerp (str)", new Stringmethod(lisp, str_is_lowercase));
    lisp->extension("deflib upperp (str)", new Stringmethod(lisp, str_is_uppercase));
    lisp->extension("deflib alphap (str)", new Stringmethod(lisp, str_is_alpha));
    lisp->extension("deflib digitp (str)", new Stringmethod(lisp, str_is_digit));
    lisp->extension("deflib emojip (str)", new Stringmethod(lisp, str_is_emoji));
    lisp->extension("deflib punctuationp (str)", new Stringmethod(lisp, str_is_punctuation));
    lisp->extension("deflib vowelp (str)", new Stringmethod(lisp, str_is_vowel));
    lisp->extension("deflib consonantp (str)", new Stringmethod(lisp, str_is_consonant));

    lisp->extension("deflib lower? (str)", new Stringmethod(lisp, str_is_lowercase));
    lisp->extension("deflib upper? (str)", new Stringmethod(lisp, str_is_uppercase));
    lisp->extension("deflib alpha? (str)", new Stringmethod(lisp, str_is_alpha));
    lisp->extension("deflib digit? (str)", new Stringmethod(lisp, str_is_digit));
    lisp->extension("deflib emoji? (str)", new Stringmethod(lisp, str_is_emoji));
    lisp->extension("deflib punctuation? (str)", new Stringmethod(lisp, str_is_punctuation));
    lisp->extension("deflib vowel? (str)", new Stringmethod(lisp, str_is_vowel));
    lisp->extension("deflib consonant? (str)", new Stringmethod(lisp, str_is_consonant));

    lisp->extension("deflib indent (str lispmode)", new Stringmethod(lisp, str_indent));

    //Tokenization methods
    lisp->extension("deflib segment (str (point 0))", new Stringmethod(lisp, str_segment_lispe));
    lisp->extension("deflib segment_e (str (point 0))", new Stringmethod(lisp, str_segment_empty));
    lisp->extension("deflib tokenizer_main ()", new Stringmethod(lisp, str_tokenizer_main));
    lisp->extension("deflib segmenter (keepblanks point)", new Stringmethod(lisp, str_segmenter));
    lisp->extension("deflib tokenizer_rules ()", new Stringmethod(lisp, str_tokenizer_rules));
    lisp->extension("deflib tokenize_rules (rules str (types))", new Stringmethod(lisp, str_tokenize_rules));
    lisp->extension("deflib tokenizer ()", new Stringmethod(lisp, str_tokenizer_rules));
    lisp->extension("deflib tokenizer_display (rules)", new Stringmethod(lisp, str_tokenizer_display_rules));
    lisp->extension("deflib tokenize (rules str (types))", new Stringmethod(lisp, str_tokenize_rules));
    lisp->extension("deflib get_tokenizer_rules (rules)", new Stringmethod(lisp, str_get_rules));
    lisp->extension("deflib set_tokenizer_rules (rules lst)", new Stringmethod(lisp, str_set_rules));
    lisp->extension("deflib get_tokenizer_operators (rules)", new Stringmethod(lisp, str_get_operators));
    lisp->extension("deflib set_tokenizer_operators (rules a_set)", new Stringmethod(lisp, str_set_operators));
    
    lisp->extension("deflib json_read (filename)", new Stringmethod(lisp, str_read_json));
    lisp->extension("deflib json_parse (str)", new Stringmethod(lisp, str_parse_json));
    lisp->extension("deflib json (element)", new Stringmethod(lisp, str_string_json));
}
