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

//This extension is done in two steps

typedef enum {str_lowercase, str_uppercase, str_is_vowel, str_is_consonant, str_deaccentuate, str_is_emoji, str_emoji_description, str_is_lowercase, str_is_uppercase, str_is_alpha, str_remplace, str_left, str_right, str_middle, str_trim, str_trimleft, str_trimright, str_tokenize, str_tokenize_empty, str_split, str_split_empty, str_ord, str_chr, str_is_punctuation, str_read_json, str_parse_json, str_string_json, str_trim0} string_method;

/*
 First of all we create a new Element derivation
 We then implement the "eval" and "asString" methods.
 These are functions around the processing of character strings
 */

class Stringmethod : public Element {
public:
    string_method met;
    short v_str;
    short v_fnd;
    short v_rep;
    short v_nb;
    short v_pos;
    
    Stringmethod(LispE* lisp, string_method s) : met(s), Element(l_lib) {
        //We know the names of variables in advance, so we might as well take advantage of it to retrieve their codes.
        wstring nom = L"str";
        v_str = lisp->encode(nom);
        nom = L"fnd";
        v_fnd = lisp->encode(nom);
        nom = L"rep";
        v_rep = lisp->encode(nom);
        nom = L"nb";
        v_nb = lisp->encode(nom);
        nom = L"pos";
        v_pos = lisp->encode(nom);
    }
    
    Element* parse_json(LispE* lisp, wstring& w) {
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
    
    
    Element* eval(LispE* lisp) {
        //eval is either: command, setenv or getenv...
        switch (met) {
            case str_remplace: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                wstring cherche = lisp->get(v_fnd)->asString(lisp);
                wstring remplacement = lisp->get(v_rep)->asString(lisp);
                strvalue = s_wreplacestring(strvalue,cherche, remplacement);
                return lisp->provideString(strvalue);
            }
            case str_lowercase: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                s = lisp->handlingutf8->s_to_lower(s);
                return lisp->provideString(s);
            }
            case str_uppercase: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                s = lisp->handlingutf8->s_to_upper(s);
                return lisp->provideString(s);
            }
            case str_is_emoji: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_emoji(s)];
            }
            case str_is_lowercase: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_lower(s)];
            }
            case str_is_uppercase: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_upper(s)];
            }
            case str_is_alpha: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_alpha(s)];
            }
            case str_left: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                long n = lisp->get(v_nb)->asInteger();
                s = s_wleft(s,n);
                return lisp->provideString(s);
            }
            case str_right: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                long n = lisp->get(v_nb)->asInteger();
                s = s_wright(s, n);
                return lisp->provideString(s);
            }
            case str_middle: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                long p = lisp->get(v_pos)->asInteger();
                long n = lisp->get(v_nb)->asInteger();
                strvalue = s_wmiddle(strvalue,p,n);
                return lisp->provideString(strvalue);
            }
            case str_trim0: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                strvalue = s_trim0(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trim:  {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                strvalue = s_trim(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trimleft:  {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                strvalue = s_trimleft(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_trimright:  {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                strvalue = s_trimright(strvalue);
                return lisp->provideString(strvalue);
            }
            case str_emoji_description: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                string res = lisp->handlingutf8->emoji_description(strvalue);
                return lisp->provideString(res);
            }
            case str_tokenize: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                return lisp->tokenize(strvalue, false);
            }
            case str_tokenize_empty: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                return lisp->tokenize(strvalue, true);
            }
            case str_split_empty: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                wstring search_string =  lisp->get(v_fnd)->asString(lisp);
                Element* ch;
                List* result = new List;
                wstring localvalue;
                long pos = 0;
                if (search_string == L"") {
                    long sz = strvalue.size();
                    //we split the string into an array of characters
                    while (pos < strvalue.size()) {
                        lisp->handlingutf8->getchar(strvalue, localvalue, pos, sz);
                        ch = lisp->provideString(localvalue);
                        result->append(ch);
                    }
                    return result;
                }

                size_t found = 0;
                while (pos != string::npos) {
                    found = strvalue.find(search_string, pos);
                    if (found != string::npos) {
                        localvalue = strvalue.substr(pos, found - pos);
                        ch = lisp->provideString(localvalue);
                        result->append(ch);
                        pos = found + search_string.size();
                    }
                    else
                        break;
                }

                if (strvalue.size() < pos) {
                    localvalue = strvalue.substr(pos, strvalue.size() - pos);
                    ch = lisp->provideString(localvalue);
                    result->append(ch);
                }
            
                return result;
            }
            case str_split: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                wstring search_string =  lisp->get(v_fnd)->asString(lisp);
                
                Element* ch;
                List* result = new List;
                wstring localvalue;
                long pos = 0;
                if (search_string == L"") {
                    long sz = strvalue.size();
                    //we split the string into an array of characters
                    while (pos < strvalue.size()) {
                        lisp->handlingutf8->getchar(strvalue, localvalue, pos, sz);
                        ch = lisp->provideString(localvalue);
                        result->append(ch);
                    }
                    return result;
                }

                size_t found = 0;
                while (pos != string::npos) {
                    found = strvalue.find(search_string, pos);
                    if (found != string::npos) {
                        localvalue = strvalue.substr(pos, found - pos);
                        if (localvalue != L"") {
                            ch = lisp->provideString(localvalue);
                            result->append(ch);
                        }
                        pos = found + search_string.size();
                    }
                    else
                        break;
                }
                
                localvalue = strvalue.substr(pos, strvalue.size() - pos);
                if (localvalue != L"") {
                    ch = lisp->provideString(localvalue);
                    result->append(ch);
                }
                
                return result;
            }
            case str_ord: {
                wstring strvalue =  lisp->get(v_str)->asString(lisp);
                if (strvalue.size() == 0)
                    return emptylist_;
                
                wchar_t d;
                Element* e;
                List* liste =  new List;
                for (long i = 0; i < strvalue.size(); i++) {
                    d = strvalue[i];
                    e = lisp->provideInteger((long)d);
                    liste->append(e);
                }
                return liste;
            }
            case str_chr: {
                wchar_t c = (wchar_t)lisp->get(v_nb)->asInteger();
                return lisp->provideString(c);
            }
            case str_is_punctuation: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_punctuation(s)];
            }
            case str_read_json: {
                string filename = lisp->get(L"filename")->toString(lisp);
                return read_json(lisp, filename);
            }
            case str_parse_json: {
                wstring str = lisp->get(L"str")->asString(lisp);
                return parse_json(lisp, str);
            }
            case str_string_json: {
                Element* e = lisp->get(L"element");
                wstring w = e->jsonString(lisp);
                return lisp->provideString(w);
            }
            case str_is_vowel: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_vowel(s)];
            }
            case str_is_consonant: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                return booleans_[lisp->handlingutf8->s_is_consonant(s)];
            }
            case str_deaccentuate: {
                wstring s =  lisp->get(v_str)->asString(lisp);
                s = lisp->handlingutf8->s_deaccentuate(s);
                return lisp->provideString(s);
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
            case str_tokenize:
                return L"Tokenize a string into a list of tokens";
            case str_tokenize_empty:
                return L"Tokenize a string into a list of tokens. Keep also the blanks";
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
    lisp->extension("deflib upper (str)", new Stringmethod(lisp, str_uppercase));
    lisp->extension("deflib lowerp (str)", new Stringmethod(lisp, str_is_lowercase));
    lisp->extension("deflib upperp (str)", new Stringmethod(lisp, str_is_uppercase));
    lisp->extension("deflib alphap (str)", new Stringmethod(lisp, str_is_alpha));
    lisp->extension("deflib emojip (str)", new Stringmethod(lisp, str_is_emoji));
    lisp->extension("deflib punctuationp (str)", new Stringmethod(lisp, str_is_punctuation));
    lisp->extension("deflib emoji (str)", new Stringmethod(lisp, str_emoji_description));
    lisp->extension("deflib replace (str fnd rep)", new Stringmethod(lisp, str_remplace));
    lisp->extension("deflib left (str nb)", new Stringmethod(lisp, str_left));
    lisp->extension("deflib right (str nb)", new Stringmethod(lisp, str_right));
    lisp->extension("deflib middle (str pos nb)", new Stringmethod(lisp, str_middle));
    lisp->extension("deflib tokenize (str)", new Stringmethod(lisp, str_tokenize));
    lisp->extension("deflib tokenizee (str)", new Stringmethod(lisp, str_tokenize_empty));
    lisp->extension("deflib split (str fnd)", new Stringmethod(lisp, str_split));
    lisp->extension("deflib splite (str fnd)", new Stringmethod(lisp, str_split_empty));
    lisp->extension("deflib ord (str)", new Stringmethod(lisp, str_ord));
    lisp->extension("deflib chr (nb)", new Stringmethod(lisp, str_chr));
    lisp->extension("deflib vowelp (str)", new Stringmethod(lisp, str_is_vowel));
    lisp->extension("deflib consonantp (str)", new Stringmethod(lisp, str_is_consonant));
    lisp->extension("deflib deaccentuate (str)", new Stringmethod(lisp, str_deaccentuate));

    lisp->extension("deflib json_read (filename)", new Stringmethod(lisp, str_read_json));
    lisp->extension("deflib json_parse (str)", new Stringmethod(lisp, str_parse_json));
    lisp->extension("deflib json (element)", new Stringmethod(lisp, str_string_json));
}
