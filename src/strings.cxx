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
//This extension is done in two steps

#ifdef WIN32
#define wset(x, y) x[0] = y[0]; x[1] = y[1];
#define waddtoken(tok, c, itok) tok[itok++] = c[0]; if (c[1]) tok[itok++] =  c[1]; tok[itok] = 0
#else
#define wset(x, y) x[0] = y[0]
#define waddtoken(tok, c, itok) tok[itok++] = c[0]; tok[itok] = 0
#endif

char x_tokens::loop(u_ustring& toparse, short i, u_uchar* token, u_uchar* chr, long& itoken, short& r, long& l, long& posc) {
    long sz;
    short type;
    
    vector<short>& element = ruleelements[i];
    short* closed = closing[i];
    vector<u_ustring>& rule = tokenizer[i];

    sz = rule.size();
    
    for (;r<sz;r++) {
        type=element[r];
        if (r && (type & xr_optionality)) {
            if (verif(type,xr_endoptional))
                return true;
            
            if (verif(type,xr_optional)) {
                long ps = posc;
                long itok =  itoken;
                short rr = r + 1;
                if (loop(toparse, i, token, chr, itok, rr, l, ps)) {
                    if (verif(element[rr],xr_plus))//if we can loop, we try...
                        r--;
                    else
                        r = rr;
                    itoken = itok;
                    posc = ps;
                    continue;
                }
                
                token[itoken] = 0;
         
                //we need to find the closing parenthesis
                r = closed[r];
                continue;
            }
        }
        
        u_ustring& label = rule[r];
        
        switch(check(label,type, chr)) {
            case 0:
                if (!r && verif(type,xr_char))
                    return 2;
                return false;
            case 2:
                waddtoken(token,chr,itoken);
                getnext(toparse,chr,posc,l);
        }

        if (!verif(type,xr_skip)) //do not store this character
            waddtoken(token,chr,itoken);
        
        getnext(toparse,chr,posc,l);

        if (verif(type,xr_singlebody)) //this is a single body rule, we can stop here...
            return true;

        if (verif(type,xr_plus)) {
            short nxt = 0;
            short ni = 0;
            //We then try to find the first actual character to stop at when traversing the RGX
            if (verif(type,xr_neednext)) {
                ni=r+1;
                while (ni<sz) {
                    if (!(element[ni] & xr_metachar))
                        break;
                    nxt++;
                    ni++;
                }
            }

            short esc_char = check(label,type,chr);
            
            while (esc_char) {
                if (esc_char==2) {
                    waddtoken(token,chr,itoken);
                    getnext(toparse,chr,posc,l); //the next character should be copied without further analysis
                }
                else {
                    if (nxt) {
                        if (check(rule[r + 1], element[r + 1], chr)) {
                            if (nxt==1)
                                break;
                            
                            long cp = posc;
                            u_uchar cc[] = {0,0,0};
                            getnext(toparse, cc, cp);
                            bool found = true;
                            for (short k = r+2; k < ni; k++) {
                                if (!check(rule[k],element[k],cc)) {
                                    found = false;
                                    break;
                                }
                                getnext(toparse, cc, cp);
                            }
                            
                            if (found)
                                break;
                        }
                    }
                }
                
                waddtoken(token,chr,itoken);
                getnext(toparse,chr,posc,l);
                esc_char = check(label,type,chr);
            }
        }
    }
    return true;
}

void x_tokens::apply(u_ustring& toparse, vecte_n<u_ustring>* vstack) {
    u_uchar chr[] = {0,0,0};
    u_uchar currentchr[] = {0,0,0};

    long wsz=toparse.size();

    u_uchar* token =  new u_uchar[wsz+1];

    long itoken = 0;
    long line=0,i, l;
    short r;
    long pos=0, posc;
    long sztokenizer;
    
    short ty;
    
    bool getit=false;
    char found=true;

    if (!loaded) {
        setrules();
        parserules();
        loaded=true;
    }
    
    stacktype.clear();
    
    if (vstack==NULL)
        vstack=&stack;
    
    sztokenizer = tokenizer.size();

    getnext(toparse,currentchr, pos,line);
    while (pos < wsz || currentchr[0]) {
        getit=false;
        posc=pos;
        if (currentchr[0]>=256)
            i=firstrule;
        else {
            i=table[(uchar)currentchr[0]];
            if (i==255) //this is not a character, which a rule is indexed for, we jump to the first non character rules...
                i=firstrule;
            else {
                if (verif(ruleelements[i][0],xr_singlebody)) {
                    //if the rule only checks one character, and it is a direct check, we can stop there
                    ty = action[i];
                    if (ty != -1) {
                        vstack->push_back(currentchr);
                        stacktype.push_back(ty);
                        if (!juststack) {
                            stackln.push_back(line);
                        }
                    }
                    getnext(toparse,currentchr, pos,line);
                    continue;
                }
            }
        }
        bool breaking = false;
        for (;i<sztokenizer;i++) {
            if (action[i]==xr_skiprule)
                continue;
                        
            token[0] = 0;
            l = line;
            wset(chr, currentchr);
            r = 0;
            itoken = 0;
            posc = pos;
            found = loop(toparse, i, token, chr, itoken, r, l, posc);
            if (found != true) {
                if (found == 2) {
                    if (breaking) //already done...
                        break;
                    i = firstrule - 1;
                    breaking = true;
                }
                
                continue;
            }

            ty=action[i];
            if (ty != -1) {
                vstack->push_back(token);
                stacktype.push_back(ty);
                if (!juststack) {
                    stackln.push_back(line);
                }
            }
            getit=true;
            wset(currentchr,chr);
            line=l;
            pos=posc;
            break;
        }
        
        if (!getit) { //Character not taken into account by a rule, we suppose it is a simple UTF8 character...
            vstack->push_back(currentchr);
            stacktype.push_back(0);
            stackln.push_back(line);
            getnext(toparse,currentchr, pos,l);
        }
    }
    delete[] token;
}

typedef enum {str_lowercase, str_uppercase, str_is_vowel, str_is_consonant, str_deaccentuate, str_is_emoji, str_emoji_description, str_is_lowercase, str_is_uppercase, str_is_alpha, str_remplace, str_left, str_right, str_middle, str_trim, str_trim0, str_trimleft, str_trimright, str_base,
    str_tokenize_lispe, str_tokenize_empty, str_split, str_split_empty, str_ord, str_chr, str_is_punctuation,
    str_format, str_padding, str_fill,
    str_edit_distance, str_read_json, str_parse_json, str_string_json, str_ngrams,
    str_rules,str_tokenize_rules, str_getrules, str_setrules} string_method;

/*
 First of all we create a new Element derivation
 We then implement the "eval" and "asString" methods.
 These are functions around the processing of character strings
 */

class Rulemethod : public Element {
public:
  x_tokens tok;
    
    Rulemethod(LispE* lisp, short l_rule_tokenize) : Element(l_rule_tokenize) {
        tok.access = lisp->handlingutf8;
    }
    
    wstring asString(LispE* lisp) {
        return L"rules";
    }
};

class Stringmethod : public Element {
public:
    string_method met;
    short v_str;
    short v_fnd;
    short v_rep;
    short v_nb;
    short v_pos;
    short l_tokenize;
    
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
        l_tokenize = lisp->encode(nom);
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
    
    Element* methodFormat(LispE* lisp) {
        u_ustring sformat =  lisp->get_variable(v_str)->asUString(lisp);

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

    Element* methodPadding(LispE* lisp) {
        u_ustring value =  lisp->get_variable(v_str)->asUString(lisp);
        u_ustring sval = lisp->get_variable(U"c")->asUString(lisp);
        long nb = lisp->get_variable(v_nb)->asInteger();
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

    Element* methodFill(LispE* lisp) {
        long nb = lisp->get_variable(v_nb)->asInteger();
        u_ustring sval;
        if (nb <= 0)
            return lisp->provideString(sval);
        
        sval = lisp->get_variable(v_str)->asUString(lisp);
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
                return true_;
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
            if (mcaracs.find(wc) == mcaracs.end() || mcaracs[wc] >= b)
                throw new Error(U"Error: Cannot analyze this string in this base.");

            v *= b;
            v += mcaracs[wc];
        }
        return lisp->provideInteger(v);
    }
    
    #define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

    Element* methodEditDistance(LispE* lisp) {
        u_ustring s1 = lisp->get_variable(v_str)->asUString(lisp);
        u_ustring s2 = lisp->get_variable("strbis")->asUString(lisp);
        
        unsigned long s1len, s2len, x, y, lastdiag, olddiag;
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

    
    Element* eval(LispE* lisp) {
        //eval is either: command, setenv or getenv...
        switch (met) {
            case str_remplace: {
                u_ustring cherche;
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                Element* end = lisp->get_variable(U"index");
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
            case str_lowercase: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                s = lisp->handlingutf8->u_to_lower(s);
                return lisp->provideString(s);
            }
            case str_uppercase: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                s = lisp->handlingutf8->u_to_upper(s);
                return lisp->provideString(s);
            }
            case str_is_emoji: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
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
            case str_left: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                long n = lisp->get_variable(v_nb)->asInteger();
                s = s_uleft(s,n);
                return lisp->provideString(s);
            }
            case str_right: {
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                long n = lisp->get_variable(v_nb)->asInteger();
                s = s_uright(s, n);
                return lisp->provideString(s);
            }
            case str_middle: {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                long p = lisp->get_variable(v_pos)->asInteger();
                long n = lisp->get_variable(v_nb)->asInteger();
                strvalue = s_umiddle(strvalue,p,n);
                return lisp->provideString(strvalue);
            }
            case str_ngrams: {
                long nb = lisp->get_variable(v_nb)->asNumber();
                if (nb <= 0)
                    throw new Error("Error: nb should be a positive value");
                
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
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
            case str_trim0: {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                strvalue = u_trim0(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trim:  {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                strvalue = u_trim(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trimleft:  {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                strvalue = u_trimleft(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trimright:  {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                strvalue = u_trimright(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_emoji_description: {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
                string res = lisp->handlingutf8->emoji_description(strvalue);
                return lisp->provideString(res);
            }
            case str_tokenize_lispe: {
                wstring strvalue =  lisp->get_variable(v_str)->asString(lisp);
                return lisp->tokenize(strvalue, false);
            }
            case str_tokenize_empty: {
                wstring strvalue =  lisp->get_variable(v_str)->asString(lisp);
                return lisp->tokenize(strvalue, true);
            }
            case str_split_empty: {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
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
                    while (pos < strvalue.size()) {
                        lisp->handlingutf8->getchar(strvalue, localvalue, pos, sz);
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
            case str_split: {
                u_ustring strvalue =  lisp->get_variable(v_str)->asUString(lisp);
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
                    while (pos < strvalue.size()) {
                        lisp->handlingutf8->getchar(strvalue, localvalue, pos, sz);
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
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                s = lisp->handlingutf8->s_deaccentuate(s);
                return lisp->provideString(s);
            }
            case str_rules: {
                return new Rulemethod(lisp, l_tokenize);
            }
            case str_tokenize_rules: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != l_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Element* types = lisp->get_variable(U"types");
                u_ustring s =  lisp->get_variable(v_str)->asUString(lisp);
                Strings* vstr = lisp->provideStrings();
                ((Rulemethod*)tok)->tok.tokenize(s, &vstr->liste);
                if (types != null_) {
                    List* l = lisp->provideList();
                    l->append(vstr);
                    Integers* t_ypes = lisp->provideIntegers();
                    t_ypes->liste = ((Rulemethod*)tok)->tok.stacktype;
                    l->append(t_ypes);
                    return l;
                }
                return vstr;
            }
            case str_getrules: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != l_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Strings* vstr = lisp->provideStrings();
                ((Rulemethod*)tok)->tok.getrules(vstr->liste);
                return vstr;
            }
            case str_setrules: {
                Element* tok = lisp->get_variable(U"rules");
                if (tok->type != l_tokenize)
                    throw new Error("Error: the first element should be a string_rule object");
                Element* lst = lisp->get_variable(U"lst");
                if (!lst->isList())
                    throw new Error("Error: This function expects a list");
                vecte_n<u_ustring> wlst;
                for (long i = 0; i < lst->size(); i++) {
                    wlst.push_back(lst->index(i)->asUString(lisp));
                }
                ((Rulemethod*)tok)->tok.setrules(wlst);
                return true_;
            }
            case str_format: {
                return methodFormat(lisp);
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
        }
		return null_;
    }
    
    //We use this instruction to return a description of the instruction
    //in effect, just do: (print getenv) to get this information
    wstring asString(LispE* lisp) {
        switch (met) {
            case str_is_emoji:
                return L"Return true is the character is an emoji";
            case str_emoji_description:
                return L"Return the textual description of an emoji";
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
            case str_tokenize_lispe:
                return L"Tokenize a string into a list of tokens with LispE tokenizer";
            case str_tokenize_empty:
                return L"Tokenize a string into a list of tokens with LispE tokenize. Keep also the blanks";
            case str_tokenize_rules:
                return L"Tokenize a string into a list of tokens with internal rules";
            case str_getrules:
                return L"Return the internal tokenization rules";
            case str_setrules:
                return L"Set the internal tokenization rules";
            case str_rules:
                return L"Return a rule object";
            case str_format:
                return L"Takes as input a format and a list of variables. Variables in the format of the form: %n, where 1<=n<=9 are replaced with their corresponding arguments";
            case str_padding:
                return L"(padding str c nb): padds the string str with c up to 'nb' characters";
            case str_fill:
                return L"(fill c nb): creates a string made of nb 'c' characters";
            case str_edit_distance:
                return L"(editdistance str strbis): compute the Levenshtein distance between str and strbis";
            case str_base:
                return L"(convert_in_base str b (convert_from): convert str into b or from base b according to convert_from";
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
    lisp->extension("deflib upper (str)", new Stringmethod(lisp, str_uppercase));
    lisp->extension("deflib lowerp (str)", new Stringmethod(lisp, str_is_lowercase));
    lisp->extension("deflib upperp (str)", new Stringmethod(lisp, str_is_uppercase));
    lisp->extension("deflib alphap (str)", new Stringmethod(lisp, str_is_alpha));
    lisp->extension("deflib emojip (str)", new Stringmethod(lisp, str_is_emoji));
    lisp->extension("deflib punctuationp (str)", new Stringmethod(lisp, str_is_punctuation));
    lisp->extension("deflib emoji (str)", new Stringmethod(lisp, str_emoji_description));
    lisp->extension("deflib replace (str fnd rep (index))", new Stringmethod(lisp, str_remplace));
    lisp->extension("deflib convert_in_base (str b (convert))", new Stringmethod(lisp, str_base));
    lisp->extension("deflib left (str nb)", new Stringmethod(lisp, str_left));
    lisp->extension("deflib ngrams (str nb)", new Stringmethod(lisp, str_ngrams));
    lisp->extension("deflib right (str nb)", new Stringmethod(lisp, str_right));
    lisp->extension("deflib middle (str pos nb)", new Stringmethod(lisp, str_middle));
    lisp->extension("deflib tokenize (str)", new Stringmethod(lisp, str_tokenize_lispe));
    lisp->extension("deflib tokenizee (str)", new Stringmethod(lisp, str_tokenize_empty));
    lisp->extension("deflib split (str (fnd))", new Stringmethod(lisp, str_split));
    lisp->extension("deflib splite (str (fnd))", new Stringmethod(lisp, str_split_empty));
    lisp->extension("deflib ord (str)", new Stringmethod(lisp, str_ord));
    lisp->extension("deflib chr (nb)", new Stringmethod(lisp, str_chr));
    lisp->extension("deflib padding (str c nb)", new Stringmethod(lisp, str_padding));
    lisp->extension("deflib fill (str nb)", new Stringmethod(lisp, str_fill));
    lisp->extension("deflib editdistance (str strbis)", new Stringmethod(lisp, str_edit_distance));
    lisp->extension("deflib vowelp (str)", new Stringmethod(lisp, str_is_vowel));
    lisp->extension("deflib consonantp (str)", new Stringmethod(lisp, str_is_consonant));
    lisp->extension("deflib deaccentuate (str)", new Stringmethod(lisp, str_deaccentuate));
    lisp->extension("deflib tokenizer_rules ()", new Stringmethod(lisp, str_rules));
    lisp->extension("deflib tokenize_rules (rules str (types))", new Stringmethod(lisp, str_tokenize_rules));
    lisp->extension("deflib get_tokenizer_rules (rules)", new Stringmethod(lisp, str_getrules));
    lisp->extension("deflib set_tokenizer_rules (rules lst)", new Stringmethod(lisp, str_setrules));

    lisp->extension("deflib json_read (filename)", new Stringmethod(lisp, str_read_json));
    lisp->extension("deflib json_parse (str)", new Stringmethod(lisp, str_parse_json));
    lisp->extension("deflib json (element)", new Stringmethod(lisp, str_string_json));
}
