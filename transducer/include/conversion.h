/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//conversion.h

#ifndef conversion_h
#define conversion_h

//On certain platforms, memset is unknown
#ifndef Tamgu_REGEX
#include <string.h>
#endif

#include <ostream>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <list>
#include <vector>
#include <map>
#include <unordered_map>
#include "mathtypes.h"

#include "binmap.h"

using std::stringstream;
using std::string;
using std::map;
using std::vector;
using std::ifstream;
using std::ofstream;
using std::list;
using std::ios;
using std::ostream;
using std::istream;
using std::fstream;
using std::cout;
using std::cerr;
using std::cin;
using std::endl;
using std::ostringstream;

#define hmap std::unordered_map
#define openMode std::ios::in|std::ios::binary

union double64 {
public:
    
    uint64_t bits;
    double v;
    
    double64(double d) {
        v = d;
    }
};

union float32 {
public:
    
    uint32_t bits;
    float v;
    
    float32(float f) {
        v = f;
    }
};


//--------------------------------------------------------------------

const short token_comma = 1;
const short token_separator = 2;
const short token_period = 4;
const short token_keepwithdigit = 8;
const short token_keeprc = 16;

using std::wstring;

union bulongchar {
	uint64_t val;
	unsigned char ustr[8];
	char str[8];
	bulongchar() {
		memset(str, '\0', 8);
	}
	void clear() {
		memset(str, '\0', 8);
	}
};


union w_u_char {
    uint32_t c;
    unsigned char cc[4];
    w_u_char() {
        c = 0;
    }
    
    //little endian
    void invert() {
        uchar c = cc[0];
        cc[0] = cc[1];
        cc[1] = c;
        c = cc[2];
        cc[2] = cc[3];
        cc[3] = c;
    }
};


//--------------------------------------------------------------------------------------------------------

//Types for fast search on wstring
#ifdef WIN32
#define doublechar int32_t
#define shiftint 16
#else
#define doublechar int64_t
#define shiftint 32
#endif

//--------------------- Main Initialization, to be launched before using any of the following methods...
class LispE;
Exporting void inittableutf8(Chaine_UTF8* h);

//--------------------- Trimming strings
Exporting string& Trim(string& chaine);
Exporting string& Trimleft(string& chaine);
Exporting string& Trimright(string& chaine);

Exporting wstring& Trim(wstring& chaine);
Exporting wstring& Trimleft(wstring& chaine);
Exporting wstring& Trimright(wstring& chaine);

//--------------------- Digits conversion
extern "C" {
double conversionfloathexa(const char* s);
}

double conversiontofloathexa(const char* s, int sign, short& l);
double conversiontofloathexa(const wchar_t* s, int sign, short& l);
double conversionfloathexa(const char* s, short& l);
double conversionfloathexa(const wchar_t* s, short& l);

Exporting BLONG conversionintegerhexa(char* number);
Exporting BLONG conversionintegerhexa(wstring& number);
Exporting double convertfloat(char* s);
Exporting double convertfloat(wstring value);
Exporting long convertinteger(wstring value);
Exporting double convertfloat(string value);
Exporting long convertinteger(string value);

Exporting double convertdouble(wstring value);
Exporting long convertlong(wstring value);
Exporting double convertdouble(string value);
Exporting long convertlong(string value);

//===================================================================
void DoubleMetaphone(const string &str, vector<string>& codes);
//===================================================================
Exporting void convertnumber(long v, string& s);
Exporting void convertnumber(double v, string& s);
Exporting void convertnumber(long v, wstring& s);
Exporting void convertnumber(double v, wstring& s);


Exporting string convertfromnumber(long l);
Exporting wstring wconvertfromnumber(long l);

//--------------------- Conversions
Exporting bool valid_latin_table(short table);

Exporting unsigned char c_unicode_to_utf8(TRANSCHAR code, unsigned char* utf);
Exporting string c_unicode_to_utf8(TRANSCHAR code);

Exporting char c_detect_utf8(unsigned char* utf);
Exporting char c_test_utf8(unsigned char* utf);

Exporting string c_unicode_to_utf8(TRANSCHAR code);


Exporting void s_doubleutf8_to_unicode(wstring& s, wchar_t* str, long sz);
Exporting uchar c_latin_to_utf8(unsigned char code, unsigned char* utf);

Exporting TRANSCHAR c_latin_table_to_unicode(int table, TRANSCHAR c);
Exporting TRANSCHAR c_unicode_to_latin(TRANSCHAR u);

//--------------------- Conversion with the recipient string cleared
Exporting void sc_unicode_to_utf8(string& s, wstring& str);
Exporting void s_unicode_to_utf8(string& s, wchar_t* str, long sz);
Exporting void sc_utf8_to_unicode(wstring& s, unsigned char* str, long sz);
//--------------------- Character conversion
Exporting bool c_unicode_to_utf16(uint32_t& r, uint32_t code);
Exporting bool c_utf16_to_unicode(uint32_t& r, uint32_t code, bool second);
//--------------------- String conversion
Exporting unsigned char conversion_utf8_to_latin(short);
Exporting string conversion_utf8_to_latin(string contenu);
Exporting string conversion_latin_to_utf8(string contenu);
Exporting string conversion_latin_to_utf8(unsigned char* contenu, long sz);
Exporting string conversion_unicode_to_latin(wstring& w);

Exporting string conversion_latin_table_to_utf8(short, unsigned char* contenu, long sz);
Exporting wstring conversion_latin_table_to_unicode(short table, unsigned char* contenu, long sz);

Exporting long conversion_utf8_to_fat(unsigned char* contenu, vector<string>& l);

Exporting string s_utf8_to_latin(unsigned char* str, long sz);
Exporting void s_latin_to_utf8(string& res, unsigned char* contenu, long sz);
Exporting void s_latin_to_unicode(wstring& res, unsigned char* contenu, long sz);

//--------------------- UNICODE Manipulation
Exporting bool c_is_punctuation(TRANSCHAR c);
Exporting bool c_is_lower(TRANSCHAR code);
Exporting bool c_is_upper(TRANSCHAR code);

Exporting bool c_is_separator(TRANSCHAR code);

Exporting char c_is_alpha(TRANSCHAR c);

Exporting TRANSCHAR c_to_lower(TRANSCHAR c);
Exporting TRANSCHAR c_to_upper(TRANSCHAR c);

//--------------------- Character manipulation
Exporting void c_to_lower(string&, unsigned char* s, long lg);
Exporting void c_to_upper(string&, unsigned char* s, long lg);

Exporting char c_is_alpha(unsigned char* m, long& i);

Exporting string c_latin_utf8(long c);

//--------------------- String manipulation
Exporting bool s_is_upper(string& s);
Exporting bool s_is_alpha(string& s);
Exporting bool s_is_lower(string& s);
Exporting bool s_is_digit(string& str);
Exporting bool s_is_punctuation(string& str);
Exporting bool s_is_space(string& str);


Exporting bool s_is_punctuation(wstring& str);
Exporting bool s_is_alpha(wstring& s);
Exporting bool s_is_upper(wstring& s);
Exporting bool s_is_lower(wstring& s);
Exporting bool s_is_vowel(wstring& s);
Exporting bool s_is_consonant(wstring& s);
Exporting bool s_is_digit(wstring& str);

Exporting bool s_is_consonant(unsigned char* s);
Exporting bool s_is_vowel(unsigned char* s);

Exporting string s_to_lower(string& s);
Exporting string s_to_upper(string& s);
Exporting string s_deaccentuate(unsigned char* s);

Exporting wstring s_to_upper(wstring& s);
Exporting wstring s_to_lower(wstring& s);
Exporting wstring s_deaccentuate(wstring& s);
Exporting bool compare_vowel(TRANSCHAR c, TRANSCHAR cc);

Exporting bool c_is_consonant(TRANSCHAR c);
Exporting bool c_is_vowel(TRANSCHAR c);

Exporting char s_is_number(unsigned char* str, char decimal, long& l, double& f);

//--------------------- Extracting substring

Exporting string s_replacestring(string& s, string& r, string& rep);
Exporting string s_replacestrings(string& s, string r, string rep);

Exporting wstring s_replacestring(wstring& str, wstring reg, wstring rep);


Exporting long size_utf16(unsigned char* str, long sz, long& charsize);
Exporting long size_c(unsigned char* contenu, long sz);

#ifdef WSTRING_IS_UTF16
Exporting size_t size_w(wchar_t* w);
Exporting size_t size_w(wstring& w, long& first);
size_t size_w(wstring& w);
long convertchartoposutf16(wstring& w, long first, long i);
long convertpostocharutf16(wstring& w, long first, long i);
inline bool checklargeutf16(wchar_t c) {
	if ((c & 0xFF00) == 0xD800)
		return true;
	return false;
}
#endif

//--------------------- Character extraction from potential UTF8 string-----------------
Exporting TRANSCHAR c_char_get_wide(unsigned char* m, long& i);

Exporting unsigned char c_utf8_latin(string s);

Exporting long convertpostochar(wstring& w, long first, long spos);
Exporting long convertchartopos(wstring& w, long first, long cpos);

Exporting long c_code_get(unsigned char* m, long& i, TRANSCHAR& code);
Exporting long c_chartobyteposition(unsigned char* contenu, long sz, long charpos);
Exporting long c_chartobyteposition(unsigned char* contenu, long charpos);
Exporting long c_bytetocharposition(unsigned char* contenu, long charpos);
Exporting long c_char_next(unsigned char* m, long& i);

Exporting string c_char_index(string& s, size_t i);
Exporting string c_char_index_remove(string& s, size_t i);

Exporting bool c_char_index_assign(string& s, string c, long i);
Exporting bool c_char_index_insert(string& s, string c, size_t i);

Exporting TRANSCHAR c_char_index_code(string& s, size_t i);

Exporting void c_char_index_code_all(string& s, vector<long>& vect);
Exporting void c_char_index_code_all_long(string& s, vector<TRANSCHAR>& vect);

Exporting string c_char_get(unsigned char* m, long& i);
Exporting string c_char_get_next(unsigned char* m, size_t& i);
void c_chars_get_next(unsigned char* m, char* str, size_t& i);

//--------------------- agnostring implementation

class agnostring : public string {
public:
	size_t bytepos;
	size_t charpos;

	agnostring() : string() {}
	agnostring(agnostring& s) : string(s.c_str()) { bytepos = 0; charpos = 0; }

	agnostring(const char* s) : string(s) { bytepos = 0; charpos = 0; }
	agnostring(const unsigned char* s) : string((const char*)s) { bytepos = 0; charpos = 0; }

	agnostring(char* s) : string(s) { bytepos = 0; charpos = 0; }
	agnostring(unsigned char* s) : string((char*)s) { bytepos = 0; charpos = 0; }

	agnostring(string s) : string(s) { bytepos = 0; charpos = 0; }
	agnostring(wstring s) { s_unicode_to_utf8(*this, s);  bytepos = 0; charpos = 0; }

	agnostring(TRANSCHAR b) : string(c_unicode_to_utf8(b)) { bytepos = 0; charpos = 0; }

	bool isutf8() {
		return s_is_utf8((unsigned char*)c_str(), size());
	}

	void begin() {
		bytepos = 0;
		charpos = 0;
	}

	bool end() {
		if (bytepos >= size())
			return true;
		return false;
	}

	size_t getbytepos() {
		return bytepos;
	}

	void setbytepos(size_t i) {
		bytepos = i;
		charpos = c_bytetocharposition((unsigned char*)c_str(), (long)bytepos);
	}

	size_t getcharpos() {
		return charpos;
	}

	void setcharpos(long i) {
		charpos = i;
		bytepos = c_chartobyteposition((unsigned char*)c_str(), (long)charpos);
	}

    void getpos(size_t& b, size_t& c) {
        b = bytepos;
        c = charpos;
    }

    void setpos(size_t b, size_t c) {
        bytepos = b;
        charpos = c;
    }

    string next() {
        char e[] = {0,0,0,0,0};

        charpos++;
        c_chars_get_next((unsigned char*)c_str(), e, bytepos);
        return e;
    }

    string next(long& line) {
        char e[] = {0,0,0,0,0};

        charpos++;
        c_chars_get_next((unsigned char*)c_str(), e, bytepos);
        if (e[0]=='\n')
            line++;
        
        return e;
    }

    void nextc(char* e) {
        charpos++;
        c_chars_get_next((unsigned char*)c_str(), e, bytepos);
    }

    void nextc(char* e, long& line) {
        charpos++;
        c_chars_get_next((unsigned char*)c_str(), e, bytepos);
        if (e[0]=='\n')
            line++;
    }

    void nextc(long& line) {
        charpos++;
        bytepos +=  1 + c_test_utf8(((unsigned char*)c_str()) + bytepos);
        if (at(bytepos) == '\n')
            line++;
    }

	TRANSCHAR nextcode() {
		charpos++;
		TRANSCHAR v;
		bytepos += 1 + c_utf8_to_unicode((unsigned char*)c_str() + bytepos, v);
		return v;
	}

	void replacecurrent(TRANSCHAR w) {
		uchar c[4];
		c_unicode_to_utf8(w, c);
		string& s = *this;

		for (long u = 0; c[u]; u++)
			s[bytepos+u] = c[u];
	}


    void switchcurrent() {
        size_t i = bytepos;
        string lc = c_char_get_next((unsigned char*)c_str(), i);
        string nx = c_char_get_next((unsigned char*)c_str(), i);
        nx+=lc;
        string& s = *this;
        for (long u = 0; u < nx.size(); u++)
            s[bytepos+u] = nx[u];
    }
    
	void following() {
		charpos++;
		bytepos += 1 + c_test_utf8((unsigned char*)c_str() + bytepos);
	}

	string current() {
		size_t i = bytepos;
		return c_char_get_next((unsigned char*)c_str(), i);
	}

	wstring wcurrent() {
		size_t i = bytepos;
		string s = c_char_get_next((unsigned char*)c_str(), i);
		wstring ws;
		s_utf8_to_unicode(ws, USTR(s), s.size());
		return ws;
	}

	string next(size_t& i) {
		return c_char_get_next((unsigned char*)c_str(), i);
	}

	wchar_t nextcode(long& i) {
		TRANSCHAR v;
		i += 1 + c_utf8_to_unicode((unsigned char*)c_str() + i, v);
		return v;
	}

	void following(long& i) {
		i += 1 + c_test_utf8((unsigned char*)c_str() + i);
	}

	char getbyte(long i) {
		string& s = *this;
		return s[i];
	}

	string operator [](long i) {
		return c_char_index(*this, i);
	}

	long chartobyteposition(long pos) {
		return c_chartobyteposition((unsigned char*)c_str(), pos);
	}

	long bytetocharposition(long pos) {
		return c_bytetocharposition((unsigned char*)c_str(), pos);
	}

	void unicodes(vector<long>& vect) {
		c_char_index_code_all(*this, vect);
	}

	size_t sizec() {
		return size_c((unsigned char*)c_str(), size());
	}

	wchar_t code(size_t i) {
		return c_char_index_code(*this, i);
	}
    
    string trim() {
        return Trim(*this);
    }

    wstring utf8tounicode() {
        wstring ws;
        s_utf8_to_unicode(ws, (unsigned char*)c_str(), size());
        return ws;
    }

    wstring latintounicode(short table) {
        return conversion_latin_table_to_unicode(table, (unsigned char*)c_str(), size());
    }

    string latintoutf8(short table) {
        return conversion_latin_table_to_utf8(table, (unsigned char*)c_str(), size());
    }


};

//------------------------------------------------------------------------------------------
//This is a specific class for OS storing UTF16 characters in wstring (mainly Windows)
//This class transforms a UTF16 wstring into a buffer of uint32_t unicode codes.
//It simplifies the computing of size and detection of characters, since large unicode codes are stored
//as one element instead of two... The problem is especially acute with emojis.
#ifdef WSTRING_IS_UTF16
class unicodestring {
	TRANSCHAR* buffer;
	long bsz;
	long sz;

public:

	unicodestring() {
		buffer = NULL;
		bsz = 0;
		sz = 0;
	}

	unicodestring(wstring& w) {
		bsz = 1 + (w.size() << 1);
		buffer = new TRANSCHAR[bsz];
		TRANSCHAR c;
		sz = 0;
		for (long i = 0; i < w.size(); i++) {
			if (c_utf16_to_unicode(c, w[i], false))
				c_utf16_to_unicode(c, w[++i], true);
			buffer[sz++] = c;
		}
		buffer[sz] = 0;
	}

	unicodestring(unicodestring& w) {
		sz = w.sz;
		bsz = w.bsz;
		buffer = new TRANSCHAR[bsz];
		for (long i = 0; i <= sz; i++) {
			buffer[i] = w.buffer[i];
		}
	}

	~unicodestring() {
		if (buffer != NULL)
			delete[] buffer;
	}

	wstring value() {
		wstring w;
		TRANSCHAR c;
		for (long i = 0; i < sz; i++) {
			if (c_unicode_to_utf16(c, buffer[i])) {
				w += (wchar_t)(c >> 16);
				w += (wchar_t)(c & 0xFFFF);
			}
			else
				w += (wchar_t)buffer[i];
		}
		return w;
	}

	void replace(TRANSCHAR rep, long pos, long ssz) {
		buffer[pos++] = rep;
		while (pos + ssz <= sz) {
			buffer[pos] = buffer[pos + ssz - 1];
			pos++;
		}
		sz -= ssz - 1;
		buffer[sz] = 0;
	}

	long size() {
		return sz;
	}

	unicodestring substr(long pos, long offset) {
		unicodestring u;
		u.bsz = offset << 1;
		u.buffer = new TRANSCHAR[bsz];
		for (; pos < sz && offset; pos++) {
			u.buffer[u.sz++] = buffer[pos];
			--offset;
		}
		u.buffer[u.sz];
		return u;
	}

	long find(wstring s, long init) {
		long ps = -1;
		long ssz = s.size();
		if (ssz > sz)
			return -1;
		for (long i = init; i <= sz - ssz; i++) {
			if (buffer[i] == s[0]) {
				if (ssz == 1)
					return i;
				ps = 1;
				while (ps < ssz && buffer[i + ps] == s[ps]) ps++;
				if (ps == ssz)
					return i;
			}
		}
		return -1;
	}

	inline void operator =(int c) {
		if (sz >= bsz) {
			if (buffer != NULL)
				delete[] buffer;

			bsz = 2;
			buffer = new TRANSCHAR[bsz];
		}
		buffer[0] = c;
		sz = 1;
	}

	inline void operator =(wstring& w) {
		if (w.size() >= bsz) {
			if (buffer != NULL)
				delete[] buffer;

			bsz = 1 + (w.size() << 1);
			buffer = new TRANSCHAR[bsz];
		}
		sz = 0;
		TRANSCHAR c;
		for (long i = 0; i < w.size(); i++) {
			if (c_utf16_to_unicode(c, w[i], false))
				c_utf16_to_unicode(c, w[++i], true);
			buffer[sz++] = c;
		}
		buffer[sz] = 0;
	}

	inline TRANSCHAR& operator[](long i) {
		return buffer[i];
	}

	inline void operator +=(wstring& w) {
		if (sz + w.size() >= bsz) {
			bsz = (sz + w.size()) * 2;
			if (buffer == NULL)
				buffer = new TRANSCHAR[bsz];
			else {
				TRANSCHAR* bb = new TRANSCHAR[bsz];
				for (long i = 0; i <= sz; i++)
					bb[i] = buffer[i];
				delete[] buffer;
				buffer = bb;
			}
		}
		TRANSCHAR c;
		for (long i = 0; i < w.size(); i++) {
			if (c_utf16_to_unicode(c, w[i], false))
				c_utf16_to_unicode(c, w[++i], true);
			buffer[sz++] = c;
		}
		buffer[sz] = 0;
	}
};

#define TAMGUVALUE(w) w.value()
Exporting void concat_char_check_utf16(wstring& res, TRANSCHAR code);
Exporting void store_char_check_utf16(wstring& res, TRANSCHAR code);
inline TRANSCHAR getachar(wstring& s, long& i) {
    TRANSCHAR c;
    if (c_utf16_to_unicode(c, s[i], false))
        c_utf16_to_unicode(c, s[++i], true);
    return c;
}

inline long getChar(wstring& s, long i, TRANSCHAR& c) {
	if (c_utf16_to_unicode(c, s[i], false)) {
		c_utf16_to_unicode(c, s[i + 1], true);
		return 2;
	}
	return 1;
}

#else
#define unicodestring wstring
#define TAMGUVALUE(w) w
#define concat_char_check_utf16(w,c) w += c
#define store_char_check_utf16(w,c) w = c
#define getachar(s,i) s[i]
#endif

//return both a wstring, which may contain an emoji
wstring getfullchar(wstring& s, long& i);

//--------------------------------------------------------------------------------------------------------
class Fast_String {
    uchar buff[33];
    long lenneo;
    long ineo;

    public:
    
    uchar* neo;
    
    Fast_String(long l) : lenneo(l), ineo(0), neo(new uchar[l]) {}
    
    ~Fast_String() {
        delete[] neo;
    }

    inline void substr(long from, long to) {
        ineo = to-from;
        memcpy(neo, neo+from, ineo);
        neo[ineo] = 0;
    }
    
    inline void substr(uchar* val, long from, long to) {
        ineo = to-from;
        if (ineo >= lenneo) {
            lenneo += ineo;
            lenneo <<= 1;
            delete[] neo;
            neo = new uchar[lenneo];
        }
        memcpy(neo, val+from, ineo);
        neo[ineo] = 0;
    }
    
    inline void set(string& ctn) {
        ineo = ctn.size();
        if (ineo >= lenneo) {
            lenneo += ineo;
            lenneo <<= 1;
            delete[] neo;
            neo = new uchar[lenneo];
        }
        memcpy(neo,STR(ctn), ineo);
        neo[ineo] = 0;
    }

    inline void set(uchar* ctn, long size_ctn) {
        ineo = size_ctn;
        if (ineo >= lenneo) {
            lenneo += ineo;
            lenneo <<= 1;
            delete[] neo;
            neo = new uchar[lenneo];
        }
        memcpy(neo,ctn, ineo);
        neo[ineo] = 0;
    }
    
    inline void reset(long i) {
        ineo = i;
        neo[ineo] = 0;
    }
    
    inline void add(uchar* ctn, long size_ctn) {
        if ((ineo + size_ctn) >= lenneo) {
            lenneo += size_ctn;
            lenneo <<= 1;
            uchar* s = new uchar[lenneo];
            memcpy(s, neo, ineo);
            delete[] neo;
            neo = s;
        }
        memcpy(neo+ineo,ctn, size_ctn);
        ineo += size_ctn;
        neo[ineo] = 0;
    }
    
    inline void add(uchar c) {
        if ((ineo + 1) >= lenneo) {
            lenneo <<= 1;
            uchar* s = new uchar[lenneo];
            memcpy(s, neo, ineo);
            delete[] neo;
            neo = s;
        }
        neo[ineo++] = c;
    }

    inline long size() {
        return ineo;
    }
    
    inline char* str() {
        return (char*)neo;
    }
    
    inline uchar operator[](long i) {
        return neo[i];
    }
    
    inline uchar get(long i) {
        return neo[i];
    }
    
    inline void downsize(long sz) {
        if (sz < lenneo) {
            delete[] neo;
            lenneo = sz;
            neo = new uchar[lenneo];
        }
        memset(neo, '\0', lenneo);
        ineo = 0;
    }

    inline void clear() {
        ineo = 0;
        neo[0]=0;
    }
    
    inline void signature() {
        if (neo[0] == 239 && neo[1] == 187 && neo[2] == 191) {
            ineo -= 3;
            memcpy(neo, neo + 3, ineo);
        }
    }
    
    inline wstring& latintounicode(wstring& ws) {
        sc_utf8_to_unicode(ws, neo, ineo);
        return ws;
    }
};

#endif







