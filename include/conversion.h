/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  conversion methods
//
//


#ifndef conversion_h
#define conversion_h

//Function Prototypes
//character based functions
bool c_utf16_to_unicode(char32_t& r, char32_t code, bool second);
void c_unicode_to_utf16(wstring& w, char32_t c);
bool c_unicode_to_utf16(char32_t& res, char32_t code);
unsigned char c_utf8_to_unicode(unsigned char* utf, char32_t& code);
unsigned char c_unicode_to_utf8(char32_t code, unsigned char* utf);
char* unicode_2_utf8(long code, char* utf);

//string based functions
void s_utf16_to_unicode(u32string& u, wstring& w);
void s_unicode_to_utf16(wstring& w, u32string& u);
void s_utf16_to_utf8(string& s, wstring& str);
void s_utf8_to_utf16(wstring& w, string& str);
void s_utf8_to_unicode(u32string& w, string& str);
void s_unicode_to_utf8(string& s, u32string& str);

void get_one_char(string& utf, string& res, long& i);
void get_one_char16(wstring& utf, wstring& res, long& i);

/// Implementation
char* unicode_2_utf8(long code, char* utf) {
    if (code < 0x0080) {
        utf[0] = (unsigned char)code;
        utf[1] = 0;
        return utf;
    }
    if (code < 0x0800) {
        utf[0] = 0xc0 | (code >> 6);
        utf[1] = 0x80 | (code & 0x3f);
        utf[2] = 0;
        return utf;
    }
    if (code < 0x10000) {
        utf[0] = 0xe0 | (code >> 12);
        utf[1] = 0x80 | ((code >> 6) & 0x3f);
        utf[2] = 0x80 | (code & 0x3f);
        utf[3] = 0;
        return utf;
    }

    utf[0] = 0xF0 | (code >> 18);
    utf[1] = 0x80 | ((code >> 12) & 0x3f);
    utf[2] = 0x80 | ((code >> 6) & 0x3f);
    utf[3] = 0x80 | (code & 0x3f);
    utf[4] = 0;
    return utf;
}

unsigned char c_unicode_to_utf8(char32_t code, unsigned char* utf) {
    if (code < 0x0080) {
        utf[0] = (unsigned char)code;
        utf[1] = 0;
        return 1;
    }
    if (code < 0x0800) {
        utf[0] = 0xc0 | (code >> 6);
        utf[1] = 0x80 | (code& 0x3f);
        utf[2] = 0;
        return 2;
    }
    if (code < 0x10000) {
        utf[0] = 0xe0 | (code >> 12);
        utf[1] = 0x80 | ((code >> 6) & 0x3f);
        utf[2] = 0x80 | (code& 0x3f);
        utf[3] = 0;
        return 3;
    }

    utf[0] = 0xF0 | (code >> 18);
    utf[1] = 0x80 | ((code >> 12) & 0x3f);
    utf[2] = 0x80 | ((code >> 6) & 0x3f);
    utf[3] = 0x80 | (code& 0x3f);
    utf[4] = 0;
    return 4;
}

bool c_utf16_to_unicode(char32_t& r, char32_t code, bool second) {
    // If you need an additional UTF-16 code
    if (second) {
        //Or it with r
        r |= code & 0x3FF;
        return false;
    }
    
    //If the first byte is 0xD8000000 then it is a 4 byte encoding
    if ((code & 0xFF00) == 0xD800) {
        //First of all we extract the first part of the code
        //I know it's horrible... And again we don't take into account the Endian here
        //c it's just for Windows and Mac OS GUIs
        r = ((((code & 0x03C0) >> 6) + 1) << 16) | ((code & 0x3F) << 10);
        //Return true to indicate that a second UTF-16 code is needed
        return true;
    }
    
    //if not r IS the code
    r = code;
    return false;
}

void c_unicode_to_utf16(wstring& w, char32_t c) {
    if (!(c & 0xFFFF0000))
        w = (wchar_t)c;
    else {
        char32_t c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
        w = c16;
        w += 0xDC00 | (c & 0x3FF);
    }
}

bool c_unicode_to_utf16(char32_t& res, char32_t code) {
    //A unicode character is encoded over 4 bytes: 3 -> 0
    //if we have bits on byte 2, then we need to provide 4 bytes...
    if ((code & 0x1F0000) == 0) {
        res = code;
        return false;
    }
    
    //00000000 000uuuuu xxxxxxyy yyyyyyyy
    //110110ww    wwxxxxxx    110111yy    yyyyyyyy
    //wwww is uuuu-1
    //We need to provide 4 bytes...
    //The first byte should by 1101 1000 which is 0xD800
    uint32_t r = 0xD800 | ((code & 0xFC00) >> 10) | ((((code & 0x1F0000) >> 16) - 1) << 6);
    //The xxxxx are the six bytes on the right of byte 1
    //the yyyyy
    res = (r << 16) | 0xDC00 | (code & 0x3FF);
    return true;
}

unsigned char c_utf8_to_unicode(unsigned char* utf, char32_t& code) {
    code = utf[0];

    if (!(utf[0] & 0x0080))
        return 0;

    //....
    if ((utf[0] & 0xF0)== 0xF0) {
        if ((utf[1] & 0x80) == 0x80 && (utf[2] & 0x80)== 0x80 && (utf[3] & 0x80)== 0x80) {
            code = (utf[0] & 0x7) << 18;
            code |= (utf[1] & 0x3F) << 12;
            code |= (utf[2] & 0x3F) << 6;
            code |= (utf[3] & 0x3F);
            return 3;
        }
        return 0;
    }
    
    if ((utf[0] & 0xE0)== 0xE0) {
        if ((utf[1] & 0x80)== 0x80 && (utf[2] & 0x80)== 0x80) {
            code = (utf[0] & 0xF) << 12;
            code |= (utf[1] & 0x3F) << 6;
            code |= (utf[2] & 0x3F);
            return 2;
        }
        return 0;
    }
    
    if ((utf[0] & 0xC0)== 0xC0 && (utf[1] & 0x80)== 0x80) {
        code = (utf[0] & 0x1F) << 6;
        code |= (utf[1] & 0x3F);
        return 1;
    }
    
    return 0;
}

//Implementation of string based methods
void s_utf16_to_unicode(u32string& u, wstring& w) {
    char32_t c;
    for (long i = 0; i < w.size(); i++) {
        if (c_utf16_to_unicode(c, w[i], false))
            c_utf16_to_unicode(c, w[++i], true);
        u += c;
    }
}

void s_unicode_to_utf16(wstring& w, u32string& u) {
    char32_t c;
    char32_t c16;
    
    for (long i = 0; i < u.size(); i++) {
        c = u[i];
        if (!(c & 0xFFFF0000)) {
            w += (wchar_t)c;
            continue;
        }
        
        c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
        w += c16;
        w += 0xDC00 | (c & 0x3FF);
    }
}

void s_utf16_to_utf8(string& s, wstring& str) {
    long sz = str.size();
    if (!sz)
        return;

    long i = 0;
    char inter[5];
    long szo = 1 + (sz << 1);
    string neo;
    long nb;
    char32_t c;

    while (i < sz) {
        if (str[i] < 0x0080) {
            neo += (char)str[i];
            i++;
            continue;
        }

        if (c_utf16_to_unicode(c, str[i], false))
            c_utf16_to_unicode(c, str[++i], true);

        nb = c_unicode_to_utf8(c, (unsigned char*)inter);
        neo += inter;
        i++;
    }

    s += neo;
}

void s_utf8_to_utf16(wstring& w, unsigned char* str , long sz) {
    if (!sz)
        return;
    
    
    char32_t c;
    unsigned char nb;
    
    char32_t c16;
    while (sz--) {
        if (*str & 0x80) {
            nb = c_utf8_to_unicode(str, c);
            str += nb + 1;
            sz = (sz >= nb)?sz-nb:0;
            if (!(c & 0xFFFF0000)) {
                w += (wchar_t)c;
                continue;
            }
            
            c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
            w += c16;
            w += 0xDC00 | (c & 0x3FF);
            continue;
        }
        w += (wchar_t)*str;
        ++str;
    }
}

void s_utf8_to_utf16(wstring& w, string& str) {
    s_utf8_to_utf16(w, (unsigned char*)str.c_str(), str.size());
}

void s_utf8_to_unicode(u32string& w, unsigned char* str , long sz) {
    if (!sz)
        return;

    u32string neo;
    neo[0] = 0;

    char32_t c;
    unsigned char nb;


    while (sz--) {
        if (*str & 0x80) {
            nb = c_utf8_to_unicode(str, c);
            str += nb+1;
            sz = (sz >= nb)?sz-nb:0;
            neo += c;
            continue;
        }
        neo += (wchar_t)*str;
        ++str;
    }

    w += neo;
}

void s_utf8_to_unicode(u32string& w, string& str) {
    s_utf8_to_unicode(w, (unsigned char*)str.c_str(), str.size());
}

void s_unicode_to_utf8(string& s, u32string& str) {
    long i = 0;
    char inter[5];
    long sz = str.size();
    if (!sz)
        return;

    long szo = 1 + (sz << 1);
    string neo;
    long nb;

    while (i < sz) {
        if (str[i] < 0x0080) {
            neo += (char)str[i];
            i++;
            continue;
        }

        nb = c_unicode_to_utf8(str[i], (unsigned char*)inter);
        neo += inter;
        i++;
    }

    s += neo;
}

void get_one_char(string& utf, string& res, long& i) {
    res = utf[i];
    if (!(utf[i] & 0x0080))
        return;

    res += utf[i + 1];

    //3 more bytes
    if ((utf[i] & 0xF0)== 0xF0) {
        if ((utf[i + 1] & 0x80) == 0x80 && (utf[i + 2] & 0x80)== 0x80 && (utf[i + 3] & 0x80)== 0x80) {
            res += utf[i + 2];
            res += utf[i + 3];
            i += 3;
        }
        return;
    }

    //2 more bytes
    if ((utf[i] & 0xE0)== 0xE0) {
        if ((utf[i + 1] & 0x80)== 0x80 && (utf[i + 2] & 0x80)== 0x80) {
            res += utf[i + 2];
            i += 2;
        }
        return;
    }

    //1 more bytes
    if ((utf[i] & 0xC0)== 0xC0 && (utf[i + 1] & 0x80)== 0x80) {
        i++;
    }
}

void get_one_char16(wstring& utf, wstring& res, long& i) {
    res = utf[i];
    if ((utf[i] & 0xFF00) == 0xD800) {
        res += utf[++i];
    }
}

#endif

