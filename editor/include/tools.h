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

#include "mapbin.h"
#include "fullstring.h"

#ifdef WIN32
#define Exporting __declspec(dllexport)
#else
#define Exporting
#ifndef PATH_MAX
#include <linux/limits.h>
#endif
#endif

#define UWCHAR u_uchar

//------------------------------------------------------------
class Emojis;
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

#define hmap std::unordered_map
//------------------------------------------------------------------------------------
//The actual size of the displayed string, the problem here is that multibyte characters are sometimes displayed with an extra-space...
//Especially for CJK characters.... (Chinese, Japanese, Korean)... We need to integrate this extra-space into our calculus...

//Lines extracted from the function "mk_wcwidth": https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
bool ckjchar(wchar_t ucs);

UWCHAR getonechar(unsigned char* s, long& i);

void s_unicode_to_utf16(wstring& w, u_ustring& u);
void s_unicode_to_utf16(std::u16string& w, u_ustring& u);
void s_utf16_to_unicode(u_ustring& u, wstring& w);
Exporting void s_utf16_to_utf8(string& s, u_ustring& str);
Exporting void s_utf16_to_utf8(string& s, int32_t* str, long sz);
void s_utf16_to_unicode(u_ustring& u, int32_t* str, long sz);

Exporting string jsonstring(string value);
Exporting wstring wjsonstring(wstring value);
Exporting wstring wjsonstring(u_ustring value);
Exporting u_ustring ujsonstring(u_ustring value);
Exporting string cs_unicode_to_utf8(UWCHAR code);

UWCHAR getonechar(string& s, long& i);

string NormalizePathname(string n);
long size_c(string& s);
Exporting long size_raw_c(string& contenu, long sz);
Exporting void SetBlankSize(long sz);
Exporting long GetBlankSize();
long VirtualIndentation(string& codestr, bool lisp, bool python);
void IndentCode(string& codestr, string& codeindente, long blancs, bool lisp, bool python);
void cr_normalise(string& code);

long convertinginteger(u_ustring& number);
long convertinginteger(wstring& number);
long convertinginteger(string& number);
void noconvertingfloathexa(wchar_t* s, long& l);

void s_split(wstring& s, wstring& splitter, vector<wstring>& strs, bool keepblanks = false);
void s_split(string& s, string& splitter, vector<string>& vs, bool keepblanks = false);
long getindex(unsigned char* contenu, long lg, long i);

void noconvertingfloathexa(const char* s, int16_t& l);
double convertingfloathexa(const char* s, long& l);
double convertingfloathexa(const char* s);
double convertingfloathexa(wchar_t* s, long& l);
double convertingfloathexa(const wchar_t* s);
double convertingfloathexa(u_uchar* s, long& l);
double convertingfloathexa(const u_uchar* s);

double convertingfloathexa(u_ustring& u);
double convertingfloathexa(string& u);

#ifdef WIN32
Exporting wstring u_to_w(u_ustring u);
Exporting u_ustring w_to_u(wstring w);
Exporting wstring _u_to_w(u_ustring& u);
Exporting u_ustring _w_to_u(wstring& w);
#else
inline wstring u_to_w(u_ustring u) {
    return (wchar_t*)u.c_str();
}

inline u_ustring w_to_u(wstring w) {
    return (u_uchar*)w.c_str();
}

#define _u_to_w(u) (std::wstring&)u
#define _w_to_u(w) (u_ustring&)w
#endif

wstring convertToWString(long d);
string convertToString(long d);
u_ustring convertToUString(long d);
wstring convertToWString(double d);
string convertToString(double d);
u_ustring convertToUString(double d);
wstring convertToWString(float d);
string convertToString(float d);
u_ustring convertToUString(float d);

string s_replacingstring(string& s, string reg, string rep);
wstring s_wreplacestring(wstring& s, wstring reg, wstring rep);
u_ustring s_ureplacestring(u_ustring& s, u_ustring reg, u_ustring rep);
u_ustring s_ureplacechar(u_ustring& s, u_uchar reg, u_ustring rep);

long nb_ureplacestring(u_ustring& s, u_ustring reg, u_ustring rep);
long nb_replacestring(string& s, string reg, string rep);


string s_left(string& s, long nb);
string s_right(string& s, long nb);
string s_middle(string& s, long g, long nb);

wstring s_wleft(wstring& s, long nb);
wstring s_wright(wstring& s, long nb);
wstring s_wmiddle(wstring& s, long g, long nb);

u_ustring s_uleft(u_ustring& s, long nb);
u_ustring s_uright(u_ustring& s, long nb);
u_ustring s_umiddle(u_ustring& s, long g, long nb);


string& s_trim(string& strvalue);
string& s_trimleft(string& strvalue);
string& s_trimright(string& strvalue);

string& s_trim0(string& strvalue);
wstring& s_trim0(wstring& strvalue);
wstring& s_trim(wstring& strvalue);
wstring& s_trimleft(wstring& strvalue);
wstring& s_trimright(wstring& strvalue);

u_ustring& u_trim0(u_ustring& strvalue);
u_ustring& u_trim(u_ustring& strvalue);
u_ustring& u_trimleft(u_ustring& strvalue);
u_ustring& u_trimright(u_ustring& strvalue);

Exporting void c_unicode_to_utf16(wstring& w, u_uchar c);
Exporting bool c_char_index_insert(string& s, string c, long i);
Exporting void s_utf8_to_utf16(wstring& w, string& str , long sz);
Exporting void s_utf8_to_unicode(wstring& s, string& str, long sz);
Exporting void s_utf8_to_unicode(u_ustring& s, string& str, long sz);
Exporting void s_utf8_to_unicode_clean(wstring& s, string& str, long sz);
Exporting unsigned char c_utf8_to_unicode(string& utf, long i, UWCHAR& code);
Exporting unsigned char c_utf8_to_unicode(string* utf, long i, UWCHAR& code);

Exporting void s_utf8_to_unicode_u(wstring& w, unsigned char* str , long sz);
Exporting unsigned char c_utf8_to_unicode(unsigned char* utf, UWCHAR& code);

Exporting void s_utf16_to_utf8(string& s, wchar_t* str, long sz);
Exporting void s_utf16_to_utf8(string& s, wstring& str);
Exporting void s_utf16_to_utf8_clean(string& s, wstring& str);

Exporting void s_unicode_to_utf8(string& s, wchar_t* str, long sz);
Exporting void s_unicode_to_utf8(string& s, wstring& str);
Exporting void s_unicode_to_utf8_clean(string& s, wstring& str);

Exporting void s_unicode_to_utf8(string& s, u_ustring& str);
Exporting bool s_is_utf8(string& contenu, long longueur);
Exporting bool c_utf16_to_unicode(u_uchar& r, u_uchar code, bool second);
Exporting bool c_unicode_to_utf16(u_uchar& res, u_uchar code);
Exporting bool c_utf16(u_uchar code);
Exporting char* unicode_2_utf8(long code, char* utf);

Exporting void get_one_char(string& utf, string& res, long& i);

Exporting char c_test_utf8(string& utf, long i);
Exporting char c_test_utf8(unsigned char* utf);

#define c_is_digit(c) (c >= '0' && c <= '9')

#ifdef WIN32
inline UWCHAR getonewchar(wstring& s, long& i) {
    UWCHAR c;
    if (c_utf16_to_unicode(c, s[i], false))
        c_utf16_to_unicode(c, s[++i], true);
    return c;
}
Exporting void concat_to_wstring(wstring& res, UWCHAR code);
#else
#define getonewchar(w, i) w[i]
#define concat_to_wstring(res, code) res += code
#endif

inline bool c_is_nbs_space(u_uchar code) {
    return (code == 160 || code == 0x202F || code == 0x3000);
}

inline bool c_is_space(u_uchar code) {
    return (code  == 9 || code == 32 || code == 160 || code == 0x202F || code == 0x3000);
}

inline bool c_is_space_or_cr(u_uchar code) {
    return (code  == 9 || code == 10 || code == 13 || code == 32 || code == 160 || code == 0x202F || code == 0x3000);
}

//--------------------------------------------------------------------
Exporting bool c_is_hexa(wchar_t code);
//------------------------------------------------------------------------------------
class UTF8_Handler {
public:
    binHash<u_uchar> utf8codemin;
    binHash<u_uchar> utf8codemaj;
    binSet punctuations;
    vector<u_uchar> vpunctuations;
    binHash<u_uchar> wvowels;
    binHash<u_uchar> wconsonants;
    
    Emojis* emojis_characters;
    
    bool scan_emoji(unsigned char* u, long& i);
    bool get_emoji(unsigned char* u, string& res, long& i);
    bool store_emoji(unsigned char* u, string& res, long& i);

    bool scan_emoji(string& u, long& i);
    bool get_emoji(string& u, string& res, long& i);
    bool store_emoji(string& u, string& res, long& i);

    bool scan_emoji(wstring& u, long& i);
    bool get_emoji(wstring& u, wstring& res, long& i);
    bool store_emoji(wstring& u, wstring& res, long& i);

    bool scan_emoji16(wstring& u, long& i);
    bool get_emoji16(wstring& u, wstring& res, long& i);
    bool store_emoji16(wstring& u, wstring& res, long& i);

    bool scan_emoji(u_ustring& u, long& i);
    bool get_emoji(u_ustring& u, u_ustring& res, long& i);
    bool store_emoji(u_ustring& u, u_ustring& res, long& i);

    bool c_is_punctuation(u_uchar str);
    bool c_is_punctuation(wchar_t str);
    bool u_is_punctuation(u_ustring& str);
    bool s_is_punctuation(wstring& str);
    bool u_is_upper(u_ustring& s);
    bool u_is_lower(u_ustring& s);
    bool u_is_alpha(u_ustring& s);
    
    bool s_is_lower(wstring& s);
    bool s_is_upper(wstring& s);
    bool s_is_alpha(wstring& s);
    
    long c_bytetocharposition(string& contenu, long charpos);
    long c_bytetoutf16position(string& contenu, long charpos);
    
    char c_is_alpha(u_uchar);
    char c_is_alpha(wchar_t);
    char c_is_alpha(unsigned char* m, long& i);
    char is_a_valid_letter(unsigned char* m, long& i);
    char is_a_valid_letter(UWCHAR c);
    char is_a_valid_letter(wstring& m, long& i);
    bool s_is_digit(u_ustring& str);
    bool s_is_space(u_ustring& str);
    bool s_is_space(wstring& str);
    bool s_is_space(string& str);
	
    string u_to_lower(string& s);
    string u_to_upper(string& s);
    wstring s_to_lower(wstring& s);
    wstring s_to_upper(wstring& s);

    u_ustring u_to_lower(u_ustring& s);
    u_ustring u_to_upper(u_ustring& s);
    u_uchar uc_to_lower(u_uchar c);
    u_uchar uc_to_upper(u_uchar c);
    wchar_t c_to_lower(wchar_t c);
    wchar_t c_to_upper(wchar_t c);
    
    u_ustring u_insert_sep(u_ustring& s, u_ustring sep);
    string insert_sep(string& s, string sep);

    void getchar(string& s, string& res, long& i);
    void getchar(u_ustring& s, u_ustring& res, long& i);
    void getandaddchar(u_ustring& s, u_ustring& res, long& i);
    void getandaddchar(string& s, string& res, long& i);
    bool getAtchar(string& s, string& res, long i);
    long charTobyte(string& s, long i);
    long byteTochar(string& s, long i);
    UWCHAR getachar(u_ustring& s, long& i);
    string getachar(string& s, long& i);

    bool c_is_emojicomp(UWCHAR c);
    bool c_is_emoji(UWCHAR c);
    long getonchar(wstring& w, long position);
    long getonchar(u_ustring& w, long position);

    bool u_is_emoji(u_ustring& s);
    bool s_is_emoji(wstring& s);
    bool s_is_emoji(string& s);
    bool c_is_emojicomp(string& m, long& i);
    bool c_is_emoji(string& m, long& i);

    bool c_is_emojicomp(unsigned char* m, long& i);
    bool c_is_emoji(unsigned char* m, long& i);

    uint32_t min_emojicomp;
    
    long size_w(u_ustring& w);
    long size_utf16(wstring& s);
    
    bool c_is_upper(u_uchar c) {
        char ty = c_is_alpha(c);
        if (ty == 2)
            return true;
        return false;
    }
    
    bool c_is_lower(u_uchar c) {
        char ty = c_is_alpha(c);
        if (ty == 1)
            return true;
        return false;
    }
    
    
    bool c_is_consonant(u_uchar c) {
        return wconsonants.check(c);
    }
    
    bool c_is_vowel(u_uchar c) {
        return wvowels.check(c);
    }
    
    
    bool s_is_consonant(u_ustring& s) {
        if (s == U"")
            return false;
        for (long i = 0; i < s.size(); i++) {
            if (!c_is_consonant(s[i]))
                return false;
        }
        return true;
    }
    
    bool s_is_vowel(u_ustring& s) {
        if (s == U"")
            return false;
        for (long i = 0; i < s.size(); i++) {
            if (!c_is_vowel(s[i]))
                return false;
        }
        return true;
    }
    
    bool compare_vowel(u_uchar c, u_uchar cc) {
        return (wvowels.check(c) && wvowels.check(cc) && wvowels.at(c) == wvowels.at(cc));
    }

    u_ustring s_deaccentuate(u_ustring& s) {
        if (s == U"")
            return U"";
        
        long lg = s.size();
        u_uchar code;
        u_ustring v;
        
        for (long i = 0; i < lg; i++) {
            code = s[i];
            if (wvowels.check(code))
                v += wvowels.at(code);
            else {
                if (wconsonants.check(code))
                    v += wconsonants.at(code);
                else
                    v += (wchar_t)code;
            }
        }
        return v;
    }

    UTF8_Handler();
    ~UTF8_Handler();
};

//------------------------------------------------------------
extern UTF8_Handler special_characters;
//------------------------------------------------------------

void split_container(wchar_t* src, long lensrc, vector<long>&);
void split_container(u_uchar* src, long lensrc, vector<long>&);


#endif
