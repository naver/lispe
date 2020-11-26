/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  tools.h
//  Lispe
//
//

#ifndef tools_hpp
#define tools_hpp

#include <stdio.h>
#include <string.h>
#include <ostream>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>

#ifdef WIN32
#define Exporting __declspec(dllexport)
#else
#define Exporting
#ifndef PATH_MAX
#include <linux/limits.h>
#endif
#endif

#ifdef WIN32
#define UWCHAR uint32_t
#else
#define UWCHAR wchar_t
#endif

//------------------------------------------------------------

class LispE;
class Element;

//------------------------------------------------------------------------------------

#define STR(x) (char*)x.c_str()
#define USTR(x) (uchar*)x.c_str()
#define uchar unsigned char
#define sprintf_s snprintf
#define swprintf_s swprintf

using std::string;
using std::wstring;
using std::vector;
using std::map;
using std::unordered_map;
using std::stringstream;
using std::cout;
using std::cerr;
using std::endl;

//------------------------------------------------------------------------------------
string jsonstring(string value);
wstring wjsonstring(wstring value);
string cs_unicode_to_utf8(UWCHAR code);

string NormalizePathname(string n);
bool c_is_space(wchar_t code);
long size_c(string& s);
long GetBlankSize();
long VirtualIndentation(string& codestr);
void IndentCode(string& codestr, string& codeindente, long blancs);
long convertinginteger(wstring& number);
long convertinginteger(string& number);
void s_split(wstring& s, wstring& splitter, vector<wstring>& strs, bool keepblanks = false);
void s_split(string& s, string& splitter, vector<string>& vs, bool keepblanks = false);
long getindex(unsigned char* contenu, long lg, long i);

void noconvertingfloathexa(const char* s, short& l);
double convertingfloathexa(const char* s, long& l);
double convertingfloathexa(const char* s);

string s_replacingstring(string& s, string reg, string rep);
wstring s_wreplacestring(wstring& s, wstring reg, wstring rep);

string s_left(string& s, long nb);
string s_right(string& s, long nb);
string s_middle(string& s, long g, long nb);

wstring s_wleft(wstring& s, long nb);
wstring s_wright(wstring& s, long nb);
wstring s_wmiddle(wstring& s, long g, long nb);

string& s_trim(string& strvalue);
string& s_trimleft(string& strvalue);
string& s_trimright(string& strvalue);

wstring& s_trim(wstring& strvalue);
wstring& s_trimleft(wstring& strvalue);
wstring& s_trimright(wstring& strvalue);

void c_chars_get_next(unsigned char* m, char* str, size_t& i);
bool c_char_index_insert(string& s, string c, size_t i);
void s_utf8_to_unicode(wstring& s, unsigned char* str, long sz);
unsigned char c_utf8_to_unicode(unsigned char* utf, UWCHAR& code);
void s_unicode_to_utf8(string& s, wstring& str);
bool s_is_utf8(unsigned char* contenu, long longueur);
Exporting bool c_utf16_to_unicode(uint32_t& r, uint32_t code, bool second);
Exporting bool c_unicode_to_utf16(uint32_t& res, uint32_t code);

#define c_is_digit(c) (c >= '0' && c <= '9')
//--------------------------------------------------------------------
bool c_is_hexa(wchar_t code);
//------------------------------------------------------------------------------------
class Chaine_UTF8 {
public:
    unordered_map<short, wchar_t> utf8codemin;
    unordered_map<short, wchar_t> utf8codemaj;
    unordered_map<short,bool> punctuations;
    unordered_map<wchar_t, wchar_t> wvowels;
    unordered_map<wchar_t, wchar_t> wconsonants;
    
    unordered_map<UWCHAR, string> emojis;
    unordered_map<UWCHAR, bool> emojiscomplement;

    bool c_is_punctuation(wchar_t str);
    bool s_is_punctuation(wstring& str);
    bool s_is_upper(wstring& s);
    bool s_is_alpha(wstring& s);
    char c_is_alpha(wchar_t);
    char c_is_alpha(unsigned char* m, long& i);
    char est_une_lettre(unsigned char* m, long& i);
    bool s_is_lower(wstring& s);
    bool s_is_digit(wstring& str);
    bool s_is_space(wstring& str);
    bool s_is_space(string& str);
    wstring s_to_lower(wstring& s);
    wstring s_to_upper(wstring& s);
    wchar_t c_to_lower(wchar_t c);
    wchar_t c_to_upper(wchar_t c);
    
    void getchar(wstring& s, wstring& res, size_t& i, long sz);
    UWCHAR getachar(wstring& s, long& i);
    
    void l_emojis(map<UWCHAR, string>& dico);
    string emoji_description(UWCHAR c);
    bool c_is_emojicomp(UWCHAR c);
    bool c_is_emoji(UWCHAR c);
    
    string emoji_description(wstring& s);
    string emoji_description(string& s);
    bool s_is_emoji(wstring& s);
    bool s_is_emoji(string& s);
    bool c_is_emojicomp(unsigned char* m, long& i);
    bool c_is_emoji(unsigned char* m, long& i);
    
    
    bool c_is_upper(wchar_t c) {
        char ty = c_is_alpha(c);
        if (ty == 2)
            return true;
        return false;
    }
    
    bool c_is_lower(wchar_t c) {
        char ty = c_is_alpha(c);
        if (ty == 1)
            return true;
        return false;
    }
    
    
    bool c_is_consonant(wchar_t c) {
        try {
            wconsonants.at(c);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }
    
    bool c_is_vowel(wchar_t c) {
        try {
            wvowels.at(c);
            return true;
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }
    
    
    bool s_is_consonant(wstring& s) {
        if (s == L"")
            return false;
        for (long i = 0; i < s.size(); i++) {
            if (!c_is_consonant(s[i]))
                return false;
        }
        return true;
    }
    
    bool s_is_vowel(wstring& s) {
        if (s == L"")
            return false;
        for (long i = 0; i < s.size(); i++) {
            if (!c_is_vowel(s[i]))
                return false;
        }
        return true;
    }
    
    bool compare_vowel(wchar_t c, wchar_t cc) {
        try {
            c = wvowels.at(c);
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
        
        try {
            cc = wvowels.at(cc);
        }
        catch(const std::out_of_range& oor) {
            return false;
        }

        return (c == cc);
    }

    wstring s_deaccentuate(wstring& s) {
        if (s == L"")
            return L"";
        
        long lg = s.size();
        wchar_t code;
        wstring v;
        
        for (long i = 0; i < lg; i++) {
            code = s[i];
            try {
                v += wvowels.at(code);
                continue;
            }
            catch(const std::out_of_range& oor) {
                try {
                    v += wconsonants.at(code);
                    continue;
                }
                catch(const std::out_of_range& oor) {
                    v += (wchar_t)code;
                }
            }
        }
        return v;
    }

    Chaine_UTF8();
};

//------------------------------------------------------------

void split_container(wchar_t* src, long lensrc, vector<long>&);

class LispEJsonCompiler {
public:
    Element* compiled_result;
    vector<long> pos;
    wstring token;
    
    wchar_t* src;
    
    double v;
    long line;
    long i;
    long r;
    long sz;
    long to;
    long l;
    uchar c;
    
    bool compile(LispE* lisp, wstring& s);
    char buildexpression(LispE*, Element* kf);
};

#endif /* elements_hpp */
