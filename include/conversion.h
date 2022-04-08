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

void c_unicode_to_utf16(wstring& w, char32_t c) {
    if (!(c & 0xFFFF0000))
        w = (wchar_t)c;
    else {
        char32_t c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
        w = c16;
        w += 0xDC00 | (c & 0x3FF);
    }
}

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

void s_utf8_to_unicode(u32string& w, unsigned char* str , long sz) {
    if (!sz)
        return;

    long ineo = 0;
    char32_t* neo = new char32_t[sz+1];
    neo[0] = 0;

    char32_t c;
    unsigned char nb;


    while (sz--) {
        if (*str & 0x80) {
            nb = c_utf8_to_unicode(str, c);
            str += nb+1;
            sz = (sz >= nb)?sz-nb:0;
            neo[ineo++] = c;
            continue;
        }
        neo[ineo++] = (wchar_t)*str;
        ++str;
    }

    neo[ineo] = 0;
    w += neo;
    delete[] neo;
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

void s_unicode_to_utf8(string& s, wstring& str) {
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


#endif

