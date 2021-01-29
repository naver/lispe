/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//conversion.cxx

#include "tools.h"
#include "conversion.h"
#include "x_tokenize.h"

#ifdef INTELINTRINSICS
#ifdef WIN32
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

#ifdef WSTRING_IS_UTF16
void concat_char_convert_utf16(wstring& res, TRANSCHAR code);
#else
#define concat_char_convert_utf16(res,code) res += code;
#endif

#define isadigit(c) c >= '0' && c <= '9'

static Chaine_UTF8* handlingutf8;

static const char _tocheck[] = {'"', '\'', '@', ':', ',','-', '+','0','1','2','3','4','5', '6','7','8', '9','[',']','{', '}', 0};
static const char _checkingmore[] = {'\n', '/', '(', ')', '<', '>','=',';', 0};

static char digitaction[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    '+',0,'-','.',0,
    '0','0','0','0','0','0','0','0','0','0',0,0,0,0,0,0,0,
    'X','X','X','X','X','X',
    0,0,0,0,0,0,0,0,0,'p',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    'x','x','x','x','x','x',0,0,0,0,0,0,0,0,0,'p',
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

static inline char* concatstrings(char* str, char* ctn, long& i, long& size_str, long size_ctn) {
    if ((i + size_ctn) >= size_str) {
        size_str += size_ctn;
        size_str <<= 1;
        char* s = new char[size_str];
        memcpy(s, str, i);
        delete[] str;
        str = s;
    }
    memcpy(str+i,ctn, size_ctn);
    i += size_ctn;
    str[i] = 0;
    return str;
}

static inline wchar_t* concatstrings(wchar_t* str, wchar_t* ctn, long& i, long& size_str, long size_ctn) {
    long j;
    if ((i + size_ctn) >= size_str) {
        size_str += size_ctn;
        size_str <<= 1;
        wchar_t* s = new wchar_t[size_str];
        for (j = 0; j < i; j++)
            s[j] = str[j];
        delete[] str;
        str = s;
    }

    for (j = 0; j < size_ctn; j++)
        str[i++] = ctn[j];
    
    str[i] = 0;
    return str;
}

//------------------------------------------------------------------------

//------------------------------------------------------------------------

static TRANSCHAR latincodetable[] = {
    133, 0x2026, 160, 0x00A0, 161, 0x00A1, 166, 0x00A6, 170, 0x00AA, 172, 0x00AC, 173, 0x00AD, 175, 0x00AF, 176, 0x00B0, 177, 0x00B1,
    181, 0x00B5, 182, 0x00B6, 183, 0x00B7, 184, 0x00B8, 186, 0x00BA, 191, 0x00BF, 215, 0x00D7, 247, 0x00F7, 223, 0x00DF, 150, 0x2013, 151, 0x2014, 171, 0x00AB,
    187, 0x00BB, 139, 0x3008, 155, 0x3009, 134, 0x2020, 135, 0x2021, 162, 0x00A2, 163, 0x00A3, 164, 0x00A4, 128, 0x20AC, 165, 0x00A5, 188, 0x00BC, 189, 0x00BD,
    190, 0x00BE, 185, 0x00B9, 178, 0x00B2, 179, 0x00B3, 39, 0x2018, 39, 0x2019, 34, 0x201C, 34, 0x201D, 0, 0 };

static TRANSCHAR latin2codetable[] = {
    161, 0x0104, 162, 0x02D8, 163, 0x0141, 164, 0x00A4, 165, 0x013D, 166, 0x015A, 167, 0x00A7, 168, 0x00A8, 169, 0x0160, 170, 0x015E,
    171, 0x0164, 172, 0x0179, 174, 0x017D, 175, 0x017B, 176, 0x00B0, 177, 0x0105, 178, 0x02DB, 179, 0x0142, 180, 0x00B4,
    181, 0x013E, 182, 0x015B, 183, 0x02C7, 184, 0x00B8, 185, 0x0161, 186, 0x015F, 187, 0x0165, 188, 0x017A, 189, 0x02DD, 190, 0x017E,
    191, 0x017C, 192, 0x0154, 193, 0x00C1, 194, 0x00C2, 195, 0x0102, 196, 0x00C4, 197, 0x0139, 198, 0x0106, 199, 0x00C7, 200, 0x010C,
    201, 0x00C9, 202, 0x0118, 203, 0x00CB, 204, 0x011A, 205, 0x00CD, 206, 0x00CE, 207, 0x010E, 208, 0x0110, 209, 0x0143, 210, 0x0147,
    211, 0x00D3, 212, 0x00D4, 213, 0x0150, 214, 0x00D6, 215, 0x00D7, 216, 0x0158, 217, 0x016E, 218, 0x00DA, 219, 0x0170, 220, 0x00DC,
    221, 0x00DD, 222, 0x0162, 223, 0x00DF, 224, 0x0155, 225, 0x00E1, 226, 0x00E2, 227, 0x0103, 228, 0x00E4, 229, 0x013A, 230, 0x0107,
    231, 0x00E7, 232, 0x010D, 233, 0x00E9, 234, 0x0119, 235, 0x00EB, 236, 0x011B, 237, 0x00ED, 238, 0x00EE, 239, 0x010F, 240, 0x0111,
    241, 0x0144, 242, 0x0148, 243, 0x00F3, 244, 0x00F4, 245, 0x0151, 246, 0x00F6, 247, 0x00F7, 248, 0x0159, 249, 0x016F, 250, 0x00FA,
    251, 0x0171, 252, 0x00FC, 253, 0x00FD, 254, 0x0163, 255, 0x02D9,
    0, 0 };


static TRANSCHAR latin3codetable[] = {
    161, 0x0126, 162, 0x02D8, 163, 0x00A3, 164, 0x00A4, 166, 0x0124, 167, 0x00A7, 168, 0x00A8, 169, 0x0130, 170, 0x015E, 171, 0x011E,
    172, 0x0134, 175, 0x017B, 176, 0x00B0, 177, 0x0127, 178, 0x00B2, 179, 0x00B3, 180, 0x00B4, 181, 0x00B5, 182, 0x0125,
    183, 0x00B7, 184, 0x00B8, 185, 0x0131, 186, 0x015F, 187, 0x011F, 188, 0x0135, 189, 0x00BD, 191, 0x017C, 192, 0x00C0, 193, 0x00C1,
    194, 0x00C2, 196, 0x00C4, 197, 0x010A, 198, 0x0108, 199, 0x00C7, 200, 0x00C8, 201, 0x00C9, 202, 0x00CA, 203, 0x00CB, 204, 0x00CC,
    205, 0x00CD, 206, 0x00CE, 207, 0x00CF, 209, 0x00D1, 210, 0x00D2, 211, 0x00D3, 212, 0x00D4, 213, 0x0120, 214, 0x00D6, 215, 0x00D7,
    216, 0x011C, 217, 0x00D9, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC, 221, 0x016C, 222, 0x015C, 223, 0x00DF, 224, 0x00E0, 225, 0x00E1,
    226, 0x00E2, 228, 0x00E4, 229, 0x010B, 230, 0x0109, 231, 0x00E7, 232, 0x00E8, 233, 0x00E9, 234, 0x00EA, 235, 0x00EB, 236, 0x00EC,
    237, 0x00ED, 238, 0x00EE, 239, 0x00EF, 241, 0x00F1, 242, 0x00F2, 243, 0x00F3, 244, 0x00F4, 245, 0x0121, 246, 0x00F6, 247, 0x00F7,
    248, 0x011D, 249, 0x00F9, 250, 0x00FA, 251, 0x00FB, 252, 0x00FC, 253, 0x016D, 254, 0x015D, 255, 0x02D9,
    0, 0 };

static TRANSCHAR latin4codetable[] = {
    161, 0x0104, 162, 0x0138, 163, 0x0156, 164, 0x00A4, 165, 0x0128, 166, 0x013B, 167, 0x00A7, 168, 0x00A8, 169, 0x0160, 170, 0x0112,
    171, 0x0122, 172, 0x0166, 174, 0x017D, 175, 0x00AF, 176, 0x00B0, 177, 0x0105, 178, 0x02DB, 179, 0x0157, 180, 0x00B4,
    181, 0x0129, 182, 0x013C, 183, 0x02C7, 184, 0x00B8, 185, 0x0161, 186, 0x0113, 187, 0x0123, 188, 0x0167, 189, 0x014A, 190, 0x017E,
    191, 0x014B, 192, 0x0100, 193, 0x00C1, 194, 0x00C2, 195, 0x00C3, 196, 0x00C4, 197, 0x00C5, 198, 0x00C6, 199, 0x012E, 200, 0x010C,
    201, 0x00C9, 202, 0x0118, 203, 0x00CB, 204, 0x0116, 205, 0x00CD, 206, 0x00CE, 207, 0x012A, 208, 0x0110, 209, 0x0145, 210, 0x014C,
    211, 0x0136, 212, 0x00D4, 213, 0x00D5, 214, 0x00D6, 215, 0x00D7, 216, 0x00D8, 217, 0x0172, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC,
    221, 0x0168, 222, 0x016A, 223, 0x00DF, 224, 0x0101, 225, 0x00E1, 226, 0x00E2, 227, 0x00E3, 228, 0x00E4, 229, 0x00E5, 230, 0x00E6,
    231, 0x012F, 232, 0x010D, 233, 0x00E9, 234, 0x0119, 235, 0x00EB, 236, 0x0117, 237, 0x00ED, 238, 0x00EE, 239, 0x012B, 240, 0x0111,
    241, 0x0146, 242, 0x014D, 243, 0x0137, 244, 0x00F4, 245, 0x00F5, 246, 0x00F6, 247, 0x00F7, 248, 0x00F8, 249, 0x0173, 250, 0x00FA,
    251, 0x00FB, 252, 0x00FC, 253, 0x0169, 254, 0x016B, 255, 0x02D9,
    0, 0 };

static TRANSCHAR latin5codetable[] = {
    161, 0x401, 162, 0x402, 163, 0x403, 164, 0x404, 165, 0x405, 166, 0x406, 167, 0x407, 168, 0x408, 169, 0x409,
    170, 0x40A, 171, 0x40B, 172, 0x40C, 173, 0x3, 174, 0x40E, 175, 0x40F, 176, 0x410, 177, 0x411, 178, 0x412, 179, 0x413, 180, 0x414, 181, 0x415,
    182, 0x416, 183, 0x417, 184, 0x418, 185, 0x419, 186, 0x41A, 187, 0x41B, 188, 0x41C, 189, 0x41D, 190, 0x41E, 191, 0x41F, 192, 0x420, 193, 0x421,
    194, 0x422, 195, 0x423, 196, 0x424, 197, 0x425, 198, 0x426, 199, 0x427, 200, 0x428, 201, 0x429, 202, 0x42A, 203, 0x42B, 204, 0x42C, 205, 0x42D,
    206, 0x42E, 207, 0x42F, 208, 0x430, 209, 0x431, 210, 0x432, 211, 0x433, 212, 0x434, 213, 0x435, 214, 0x436, 215, 0x437, 216, 0x438, 217, 0x439,
    218, 0x43A, 219, 0x43B, 220, 0x43C, 221, 0x43D, 222, 0x43E, 223, 0x43F, 224, 0x440, 225, 0x441, 226, 0x442, 227, 0x443, 228, 0x444, 229, 0x445,
    230, 0x446, 231, 0x447, 232, 0x448, 233, 0x449, 234, 0x44A, 235, 0x44B, 236, 0x44C, 237, 0x44D, 238, 0x44E, 239, 0x44F, 240, 0x2116, 241, 0x451, 242, 0x452,
    243, 0x453, 244, 0x454, 245, 0x455, 246, 0x456, 247, 0x457, 248, 0x458, 249, 0x459, 250, 0x45A, 251, 0x45B, 252, 0x45C, 253, 0xA7, 254, 0x45E, 255, 0x45F,
    0, 0 };

static TRANSCHAR latin6codetable[] = {
    164, 0x00A4, 172, 0x060C, 187, 0x061B, 191, 0x061F, 193, 0x0621, 194, 0x0622, 195, 0x0623, 196, 0x0624, 197, 0x0625,
    198, 0x0626, 199, 0x0627, 200, 0x0628, 201, 0x0629, 202, 0x062A, 203, 0x062B, 204, 0x062C, 205, 0x062D, 206, 0x062E, 207, 0x062F,
    208, 0x0630, 209, 0x0631, 210, 0x0632, 211, 0x0633, 212, 0x0634, 213, 0x0635, 214, 0x0636, 215, 0x0637, 216, 0x0638, 217, 0x0639,
    218, 0x063A, 224, 0x0640, 225, 0x0641, 226, 0x0642, 227, 0x0643, 228, 0x0644, 229, 0x0645, 230, 0x0646, 231, 0x0647, 232, 0x0648,
    233, 0x0649, 234, 0x064A, 0, 0 };

static TRANSCHAR latin7codetable[] = {
    163, 0x00A3, 164, 0x20AC, 165, 0x20AF, 166, 0x00A6, 167, 0x00A7, 168, 0x00A8, 169, 0x00A9, 170, 0x037A,
    171, 0x00AB, 172, 0x00AC, 175, 0x2015, 176, 0x00B0, 177, 0x00B1, 178, 0x00B2, 179, 0x00B3, 180, 0x0384, 181, 0x0385,
    182, 0x0386, 183, 0x00B7, 184, 0x0388, 185, 0x0389, 186, 0x038A, 187, 0x00BB, 188, 0x038C, 189, 0x00BD, 190, 0x038E, 191, 0x038F,
    192, 0x0390, 193, 0x0391, 194, 0x0392, 195, 0x0393, 196, 0x0394, 197, 0x0395, 198, 0x0396, 199, 0x0397, 200, 0x0398, 201, 0x0399,
    202, 0x039A, 203, 0x039B, 204, 0x039C, 205, 0x039D, 206, 0x039E, 207, 0x039F, 208, 0x03A0, 209, 0x03A1, 211, 0x03A3, 212, 0x03A4,
    213, 0x03A5, 214, 0x03A6, 215, 0x03A7, 216, 0x03A8, 217, 0x03A9, 218, 0x03AA, 219, 0x03AB, 220, 0x03AC, 221, 0x03AD, 222, 0x03AE,
    223, 0x03AF, 224, 0x03B0, 225, 0x03B1, 226, 0x03B2, 227, 0x03B3, 228, 0x03B4, 229, 0x03B5, 230, 0x03B6, 231, 0x03B7, 232, 0x03B8,
    233, 0x03B9, 234, 0x03BA, 235, 0x03BB, 236, 0x03BC, 237, 0x03BD, 238, 0x03BE, 239, 0x03BF, 240, 0x03C0, 241, 0x03C1, 242, 0x03C2,
    243, 0x03C3, 244, 0x03C4, 245, 0x03C5, 246, 0x03C6, 247, 0x03C7, 248, 0x03C8, 249, 0x03C9, 250, 0x03CA, 251, 0x03CB, 252, 0x03CC,
    253, 0x03CD, 254, 0x03CE, 0, 0 };

static TRANSCHAR latin8codetable[] = {
    162, 0x00A2, 163, 0x00A3, 164, 0x00A4, 165, 0x00A5, 166, 0x00A6, 167, 0x00A7, 168, 0x00A8, 169, 0x00A9, 170, 0x00D7, 171, 0x00AB,
    172, 0x00AC, 174, 0x00AE, 175, 0x00AF, 176, 0x00B0, 177, 0x00B1, 178, 0x00B2, 179, 0x00B3, 180, 0x00B4, 181, 0x00B5,
    182, 0x00B6, 183, 0x00B7, 184, 0x00B8, 185, 0x00B9, 186, 0x00F7, 187, 0x00BB, 188, 0x00BC, 189, 0x00BD, 190, 0x00BE, 223, 0x2017,
    224, 0x05D0, 225, 0x05D1, 226, 0x05D2, 227, 0x05D3, 228, 0x05D4, 229, 0x05D5, 230, 0x05D6, 231, 0x05D7, 232, 0x05D8, 233, 0x05D9,
    234, 0x05DA, 235, 0x05DB, 236, 0x05DC, 237, 0x05DD, 238, 0x05DE, 239, 0x05DF, 240, 0x05E0, 241, 0x05E1, 242, 0x05E2, 243, 0x05E3,
    244, 0x05E4, 245, 0x05E5, 246, 0x05E6, 247, 0x05E7, 248, 0x05E8, 249, 0x05E9, 250, 0x05EA, 0, 0 };

static TRANSCHAR latin9codetable[] = {
    161, 0x00A1, 162, 0x00A2, 163, 0x00A3, 164, 0x00A4, 165, 0x00A5, 166, 0x00A6, 167, 0x00A7, 168, 0x00A8, 169, 0x00A9, 170, 0x00AA,
    171, 0x00AB, 172, 0x00AC, 174, 0x00AE, 175, 0x00AF, 176, 0x00B0, 177, 0x00B1, 178, 0x00B2, 179, 0x00B3, 180, 0x00B4,
    181, 0x00B5, 182, 0x00B6, 183, 0x00B7, 184, 0x00B8, 185, 0x00B9, 186, 0x00BA, 187, 0x00BB, 188, 0x00BC, 189, 0x00BD, 190, 0x00BE,
    191, 0x00BF, 192, 0x00C0, 193, 0x00C1, 194, 0x00C2, 195, 0x00C3, 196, 0x00C4, 197, 0x00C5, 198, 0x00C6, 199, 0x00C7, 200, 0x00C8,
    201, 0x00C9, 202, 0x00CA, 203, 0x00CB, 204, 0x00CC, 205, 0x00CD, 206, 0x00CE, 207, 0x00CF, 208, 0x011E, 209, 0x00D1, 210, 0x00D2,
    211, 0x00D3, 212, 0x00D4, 213, 0x00D5, 214, 0x00D6, 215, 0x00D7, 216, 0x00D8, 217, 0x00D9, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC,
    221, 0x0130, 222, 0x015E, 223, 0x00DF, 224, 0x00E0, 225, 0x00E1, 226, 0x00E2, 227, 0x00E3, 228, 0x00E4, 229, 0x00E5, 230, 0x00E6,
    231, 0x00E7, 232, 0x00E8, 233, 0x00E9, 234, 0x00EA, 235, 0x00EB, 236, 0x00EC, 237, 0x00ED, 238, 0x00EE, 239, 0x00EF, 240, 0x011F,
    241, 0x00F1, 242, 0x00F2, 243, 0x00F3, 244, 0x00F4, 245, 0x00F5, 246, 0x00F6, 247, 0x00F7, 248, 0x00F8, 249, 0x00F9, 250, 0x00FA,
    251, 0x00FB, 252, 0x00FC, 253, 0x0131, 254, 0x015F, 255, 0x00FF, 0, 0 };

static TRANSCHAR latin10codetable[] = {
    161, 0x0104, 162, 0x0112, 163, 0x0122, 164, 0x012A, 165, 0x0128, 166, 0x0136, 167, 0x00A7, 168, 0x013B, 169, 0x0110, 170, 0x0160,
    171, 0x0166, 172, 0x017D, 174, 0x016A, 175, 0x014A, 176, 0x00B0, 177, 0x0105, 178, 0x0113, 179, 0x0123, 180, 0x012B,
    181, 0x0129, 182, 0x0137, 183, 0x00B7, 184, 0x013C, 185, 0x0111, 186, 0x0161, 187, 0x0167, 188, 0x017E, 189, 0x2015, 190, 0x016B,
    191, 0x014B, 192, 0x0100, 193, 0x00C1, 194, 0x00C2, 195, 0x00C3, 196, 0x00C4, 197, 0x00C5, 198, 0x00C6, 199, 0x012E, 200, 0x010C,
    201, 0x00C9, 202, 0x0118, 203, 0x00CB, 204, 0x0116, 205, 0x00CD, 206, 0x00CE, 207, 0x00CF, 208, 0x00D0, 209, 0x0145, 210, 0x014C,
    211, 0x00D3, 212, 0x00D4, 213, 0x00D5, 214, 0x00D6, 215, 0x0168, 216, 0x00D8, 217, 0x0172, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC,
    221, 0x00DD, 222, 0x00DE, 223, 0x00DF, 224, 0x0101, 225, 0x00E1, 226, 0x00E2, 227, 0x00E3, 228, 0x00E4, 229, 0x00E5, 230, 0x00E6,
    231, 0x012F, 232, 0x010D, 233, 0x00E9, 234, 0x0119, 235, 0x00EB, 236, 0x0117, 237, 0x00ED, 238, 0x00EE, 239, 0x00EF, 240, 0x00F0,
    241, 0x0146, 242, 0x014D, 243, 0x00F3, 244, 0x00F4, 245, 0x00F5, 246, 0x00F6, 247, 0x0169, 248, 0x00F8, 249, 0x0173, 250, 0x00FA,
    251, 0x00FB, 252, 0x00FC, 253, 0x00FD, 254, 0x00FE, 255, 0x0138, 0, 0 };

static TRANSCHAR latin11codetable[] = {
    161, 0x0E01, 162, 0x0E02, 163, 0x0E03, 164, 0x0E04, 165, 0x0E05, 166, 0x0E06, 167, 0x0E07, 168, 0x0E08, 169, 0x0E09, 170, 0x0E0A,
    171, 0x0E0B, 172, 0x0E0C, 173, 0x0E0D, 174, 0x0E0E, 175, 0x0E0F, 176, 0x0E10, 177, 0x0E11, 178, 0x0E12, 179, 0x0E13, 180, 0x0E14,
    181, 0x0E15, 182, 0x0E16, 183, 0x0E17, 184, 0x0E18, 185, 0x0E19, 186, 0x0E1A, 187, 0x0E1B, 188, 0x0E1C, 189, 0x0E1D, 190, 0x0E1E,
    191, 0x0E1F, 192, 0x0E20, 193, 0x0E21, 194, 0x0E22, 195, 0x0E23, 196, 0x0E24, 197, 0x0E25, 198, 0x0E26, 199, 0x0E27, 200, 0x0E28,
    201, 0x0E29, 202, 0x0E2A, 203, 0x0E2B, 204, 0x0E2C, 205, 0x0E2D, 206, 0x0E2E, 207, 0x0E2F, 208, 0x0E30, 210, 0x0E32,
    211, 0x0E33, 223, 0x0E3F, 224, 0x0E40,
    225, 0x0E41, 226, 0x0E42, 227, 0x0E43, 228, 0x0E44, 229, 0x0E45, 230, 0x0E46,
    239, 0x0E4F, 240, 0x0E50, 241, 0x0E51, 242, 0x0E52, 243, 0x0E53, 244, 0x0E54,
    245, 0x0E55, 246, 0x0E56, 247, 0x0E57, 248, 0x0E58, 249, 0x0E59, 250, 0x0E5A, 251, 0x0E5B, 0, 0 };

static TRANSCHAR latin12codetable[] = { 0x43, 0x43, 0x4c, 0x4c, 0x41, 0x41, 0x55, 0x55, 0x44, 0x44, 0x49, 0x49,
    0x55, 0x55, 0x53, 0x53, 0x52, 0x52, 0x55, 0x55, 0x46, 0x46, 0x55, 0x55, 0x53, 0x53, 0x54, 0x54,
    0x41, 0x41, 0x4d, 0x4d, 0x47, 0x47, 0x55, 0x55, 0x46, 0x46, 0x45, 0x45,
    0x43, 0x43, 0x49, 0x49, 0x54, 0x54, 0x41, 0x41, 0x4E, 0x4E, 0x4E, 0x4E, 0x4F, 0x4F, 0x4D, 0x4D,
    0x4D, 0x4D, 0x58, 0x58, 0x56, 0x56, 0x49, 0x49, 0x49, 0x49, 0x49, 0x49, 0x2D, 0x2D, 0x58, 0x58, 0, 0 };

static TRANSCHAR latin13codetable[] = {
    34, 0x201D, 162, 0x00A2, 163, 0x00A3, 164, 0x00A4, 165, 0x201E, 166, 0x00A6, 167, 0x00A7, 168, 0x00D8, 169, 0x00A9, 170, 0x0156,
    171, 0x00AB, 172, 0x00AC, 174, 0x00AE, 175, 0x00C6, 176, 0x00B0, 177, 0x00B1, 178, 0x00B2, 179, 0x00B3, 34, 0x201C,
    181, 0x00B5, 182, 0x00B6, 183, 0x00B7, 184, 0x00F8, 185, 0x00B9, 186, 0x0157, 187, 0x00BB, 188, 0x00BC, 189, 0x00BD, 190, 0x00BE,
    191, 0x00E6, 192, 0x0104, 193, 0x012E, 194, 0x0100, 195, 0x0106, 196, 0x00C4, 197, 0x00C5, 198, 0x0118, 199, 0x0112, 200, 0x010C,
    201, 0x00C9, 202, 0x0179, 203, 0x0116, 204, 0x0122, 205, 0x0136, 206, 0x012A, 207, 0x013B, 208, 0x0160, 209, 0x0143, 210, 0x0145,
    211, 0x00D3, 212, 0x014C, 213, 0x00D5, 214, 0x00D6, 215, 0x00D7, 216, 0x0172, 217, 0x0141, 218, 0x015A, 219, 0x016A, 220, 0x00DC,
    221, 0x017B, 222, 0x017D, 223, 0x00DF, 224, 0x0105, 225, 0x012F, 226, 0x0101, 227, 0x0107, 228, 0x00E4, 229, 0x00E5, 230, 0x0119,
    231, 0x0113, 232, 0x010D, 233, 0x00E9, 234, 0x017A, 235, 0x0117, 236, 0x0123, 237, 0x0137, 238, 0x012B, 239, 0x013C, 240, 0x0161,
    241, 0x0144, 242, 0x0146, 243, 0x00F3, 244, 0x014D, 245, 0x00F5, 246, 0x00F6, 247, 0x00F7, 248, 0x0173, 249, 0x0142, 250, 0x015B,
    251, 0x016B, 252, 0x00FC, 253, 0x017C, 254, 0x017E, 39, 0x2019, 0, 0 };

static TRANSCHAR latin14codetable[] = {
    161, 0x1E02, 162, 0x1E03, 163, 0x00A3, 164, 0x010A, 165, 0x010B, 166, 0x1E0A, 167, 0x00A7, 168, 0x1E80, 169, 0x00A9, 170, 0x1E82,
    171, 0x1E0B, 172, 0x1EF2, 174, 0x00AE, 175, 0x0178, 176, 0x1E1E, 177, 0x1E1F, 178, 0x0120, 179, 0x0121, 180, 0x1E40,
    181, 0x1E41, 182, 0x00B6, 183, 0x1E56, 184, 0x1E81, 185, 0x1E57, 186, 0x1E83, 187, 0x1E60, 188, 0x1EF3, 189, 0x1E84, 190, 0x1E85,
    191, 0x1E61, 192, 0x00C0, 193, 0x00C1, 194, 0x00C2, 195, 0x00C3, 196, 0x00C4, 197, 0x00C5, 198, 0x00C6, 199, 0x00C7, 200, 0x00C8,
    201, 0x00C9, 202, 0x00CA, 203, 0x00CB, 204, 0x00CC, 205, 0x00CD, 206, 0x00CE, 207, 0x00CF, 208, 0x0174, 209, 0x00D1, 210, 0x00D2,
    211, 0x00D3, 212, 0x00D4, 213, 0x00D5, 214, 0x00D6, 215, 0x1E6A, 216, 0x00D8, 217, 0x00D9, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC,
    221, 0x00DD, 222, 0x0176, 223, 0x00DF, 224, 0x00E0, 225, 0x00E1, 226, 0x00E2, 227, 0x00E3, 228, 0x00E4, 229, 0x00E5, 230, 0x00E6,
    231, 0x00E7, 232, 0x00E8, 233, 0x00E9, 234, 0x00EA, 235, 0x00EB, 236, 0x00EC, 237, 0x00ED, 238, 0x00EE, 239, 0x00EF, 240, 0x0175,
    241, 0x00F1, 242, 0x00F2, 243, 0x00F3, 244, 0x00F4, 245, 0x00F5, 246, 0x00F6, 247, 0x1E6B, 248, 0x00F8, 249, 0x00F9, 250, 0x00FA,
    251, 0x00FB, 252, 0x00FC, 253, 0x00FD, 254, 0x0177, 255, 0x00FF, 0, 0 };

static TRANSCHAR latin15codetable[] = {
    161, 0x00A1, 162, 0x00A2, 163, 0x00A3, 164, 0x20AC, 165, 0x00A5, 166, 0x0160, 167, 0x00A7, 168, 0x0161, 169, 0x00A9, 170, 0x00AA,
    171, 0x00AB, 172, 0x00AC, 174, 0x00AE, 175, 0x00AF, 176, 0x00B0, 177, 0x00B1, 178, 0x00B2, 179, 0x00B3, 180, 0x017D,
    181, 0x00B5, 182, 0x00B6, 183, 0x00B7, 184, 0x017E, 185, 0x00B9, 186, 0x00BA, 187, 0x00BB, 188, 0x0152, 189, 0x0153, 190, 0x0178,
    191, 0x00BF, 192, 0x00C0, 193, 0x00C1, 194, 0x00C2, 195, 0x00C3, 196, 0x00C4, 197, 0x00C5, 198, 0x00C6, 199, 0x00C7, 200, 0x00C8,
    201, 0x00C9, 202, 0x00CA, 203, 0x00CB, 204, 0x00CC, 205, 0x00CD, 206, 0x00CE, 207, 0x00CF, 208, 0x00D0, 209, 0x00D1, 210, 0x00D2,
    211, 0x00D3, 212, 0x00D4, 213, 0x00D5, 214, 0x00D6, 215, 0x00D7, 216, 0x00D8, 217, 0x00D9, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC,
    221, 0x00DD, 222, 0x00DE, 223, 0x00DF, 224, 0x00E0, 225, 0x00E1, 226, 0x00E2, 227, 0x00E3, 228, 0x00E4, 229, 0x00E5, 230, 0x00E6,
    231, 0x00E7, 232, 0x00E8, 233, 0x00E9, 234, 0x00EA, 235, 0x00EB, 236, 0x00EC, 237, 0x00ED, 238, 0x00EE, 239, 0x00EF, 240, 0x00F0,
    241, 0x00F1, 242, 0x00F2, 243, 0x00F3, 244, 0x00F4, 245, 0x00F5, 246, 0x00F6, 247, 0x00F7, 248, 0x00F8, 249, 0x00F9, 250, 0x00FA,
    251, 0x00FB, 252, 0x00FC, 253, 0x00FD, 254, 0x00FE, 255, 0x00FF, 0, 0 };

static TRANSCHAR latin16codetable[] = {
    161, 0x0104, 162, 0x0105, 163, 0x0141, 164, 0x20AC, 165, 0x201E, 166, 0x0160, 167, 0x00A7, 168, 0x0161, 169, 0x00A9, 170, 0x0218,
    171, 0x00AB, 172, 0x0179, 174, 0x017A, 175, 0x017B, 176, 0x00B0, 177, 0x00B1, 178, 0x010C, 179, 0x0142, 180, 0x017D,
    34, 0x201D, 182, 0x00B6, 183, 0x00B7, 184, 0x017E, 185, 0x010D, 186, 0x0219, 187, 0x00BB, 188, 0x0152, 189, 0x0153, 190, 0x0178,
    191, 0x017C, 192, 0x00C0, 193, 0x00C1, 194, 0x00C2, 195, 0x0102, 196, 0x00C4, 197, 0x0106, 198, 0x00C6, 199, 0x00C7, 200, 0x00C8,
    201, 0x00C9, 202, 0x00CA, 203, 0x00CB, 204, 0x00CC, 205, 0x00CD, 206, 0x00CE, 207, 0x00CF, 208, 0x0110, 209, 0x0143, 210, 0x00D2,
    211, 0x00D3, 212, 0x00D4, 213, 0x0150, 214, 0x00D6, 215, 0x015A, 216, 0x0170, 217, 0x00D9, 218, 0x00DA, 219, 0x00DB, 220, 0x00DC,
    221, 0x0118, 222, 0x021A, 223, 0x00DF, 224, 0x00E0, 225, 0x00E1, 226, 0x00E2, 227, 0x0103, 228, 0x00E4, 229, 0x0107, 230, 0x00E6,
    231, 0x00E7, 232, 0x00E8, 233, 0x00E9, 234, 0x00EA, 235, 0x00EB, 236, 0x00EC, 237, 0x00ED, 238, 0x00EE, 239, 0x00EF, 240, 0x0111,
    241, 0x0144, 242, 0x00F2, 243, 0x00F3, 244, 0x00F4, 245, 0x0151, 246, 0x00F6, 247, 0x015B, 248, 0x0171, 249, 0x00F9, 250, 0x00FA,
    251, 0x00FB, 252, 0x00FC, 253, 0x0119, 254, 0x021B, 255, 0x00FF, 0, 0 };

//it will be accessed as the 17 table...
static TRANSCHAR latinwindows1[] = {
    127,0x44,128,0x20ac,129,0x20,130,0x201a,131,0x192,132,0x201e,133,0x2026,134,0x2020,135,0x2021,136,0x2c6,137,0x2030,138,0x160,139,0x2039,
    140,0x152,141,0x20,142,0x17d,143,0x20,144,0x20,145,0x2018,146,0x2019,147,0x201c,148,0x201d,149,0x2022,150,0x2013,151,0x2014,152,0x2dc,
    153,0x2122,154,0x161,155,0x203a,156,0x153,157,0x20,158,0x17e,159,0x178,160,0x20,161,0xa1,162,0xa2,163,0xa3,164,0xa4,165,0xa5,166,0xa6,
    167,0xa7,168,0xa8,169,0xa9,170,0xaa,171,0xab,172,0xac,173,0x20,174,0xae,175,0xaf,176,0xb0,177,0xb1,178,0xb2,179,0xb3,180,0xb4,181,0xb5,
    182,0xb6,183,0xb7,184,0xb8,185,0xb9,186,0xba,187,0xbb,188,0xbc,189,0xbd,190,0xbe,191,0xbf,192,0xc0,193,0xc1,194,0xc2,195,0xc3,196,0xc4,
    197,0xc5,198,0xc6,199,0xc7,200,0xc8,201,0xc9,202,0xca,203,0xcb,204,0xcc,205,0xcd,206,0xce,207,0xcf,208,0xd0,209,0xd1,210,0xd2,211,0xd3,
    212,0xd4,213,0xd5,214,0xd6,215,0xd7,216,0xd8,217,0xd9,218,0xda,219,0xdb,220,0xdc,221,0xdd,222,0xde,223,0xdf,224,0xe0,225,0xe1,226,0xe2,
    227,0xe3,228,0xe4,229,0xe5,230,0xe6,231,0xe7,232,0xe8,233,0xe9,234,0xea,235,0xeb,236,0xec,237,0xed,238,0xee,239,0xef,240,0xf0,241,0xf1,
    242,0xf2,243,0xf3,244,0xf4,245,0xf5,246,0xf6,247,0xf7,248,0xf8,249,0xf9,250,0xfa,251,0xfb,252,0xfc,253,0xfd,254,0xfe,255,0xff,0,0};



static basebin_hash<TRANSCHAR> latincodes;
static basebin_hash<TRANSCHAR> latin2codes;
static basebin_hash<TRANSCHAR> latin3codes;
static basebin_hash<TRANSCHAR> latin4codes;
static basebin_hash<TRANSCHAR> latin5codes;
static basebin_hash<TRANSCHAR> latin6codes;
static basebin_hash<TRANSCHAR> latin7codes;
static basebin_hash<TRANSCHAR> latin8codes;
static basebin_hash<TRANSCHAR> latin9codes;
static basebin_hash<TRANSCHAR> latin10codes;
static basebin_hash<TRANSCHAR> latin11codes;
static basebin_hash<TRANSCHAR> latin12codes;
static basebin_hash<TRANSCHAR> latin13codes;
static basebin_hash<TRANSCHAR> latin14codes;
static basebin_hash<TRANSCHAR> latin15codes;
static basebin_hash<TRANSCHAR> latin16codes;
static basebin_hash<TRANSCHAR> latinwindowscodes;

static basebin_hash<TRANSCHAR> codeslatin;

static basebin_hash<basebin_hash<TRANSCHAR>* > alllatincodes;

static hmap<string, TRANSCHAR> utf8codes;
//------------------------------------------------------------------------------------------------------------
static char invertnumbers[1000][5];
static bin_hash<string> numbers(false);

static void InitNumbers() {
    char buff[10];
    int sz, pos;
    for (int i=0;i<=999;i++) {
        sz = sprintf_s(buff,10,"%03d",i);
        invertnumbers[i][sz--]=0;
        pos=0;
        while (buff[pos]) invertnumbers[i][sz--]=buff[pos++];
        invertnumbers[i][4]=pos;
    }
    
    wstring w;
    for (int i=0;i<=9999;i++) {
        sz = sprintf_s(buff,10,"%d",i);
        numbers[i]=buff;
    }
}

static void  InitLatinTables() {
    long i = 0;
    
    char ch[5];
    while (latin2codetable[i] != 0) {
        latin2codes[latin2codetable[i]] = latin2codetable[i + 1];
        codeslatin[latin2codetable[i + 1]] = latin2codetable[i];
        c_unicode_to_utf8(latin2codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin2codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin3codetable[i] != 0) {
        latin3codes[latin3codetable[i]] = latin3codetable[i + 1];
        codeslatin[latin3codetable[i + 1]] = latin3codetable[i];
        c_unicode_to_utf8(latin3codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin3codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin4codetable[i] != 0) {
        latin4codes[latin4codetable[i]] = latin4codetable[i + 1];
        codeslatin[latin4codetable[i + 1]] = latin4codetable[i];
        c_unicode_to_utf8(latin4codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin4codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin5codetable[i] != 0) {
        latin5codes[latin5codetable[i]] = latin5codetable[i + 1];
        codeslatin[latin5codetable[i + 1]] = latin5codetable[i];
        c_unicode_to_utf8(latin5codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin5codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin6codetable[i] != 0) {
        latin6codes[latin6codetable[i]] = latin6codetable[i + 1];
        codeslatin[latin6codetable[i + 1]] = latin6codetable[i];
        c_unicode_to_utf8(latin6codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin6codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin7codetable[i] != 0) {
        latin7codes[latin7codetable[i]] = latin7codetable[i + 1];
        codeslatin[latin7codetable[i + 1]] = latin7codetable[i];
        c_unicode_to_utf8(latin7codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin7codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin8codetable[i] != 0) {
        latin8codes[latin8codetable[i]] = latin8codetable[i + 1];
        codeslatin[latin8codetable[i + 1]] = latin8codetable[i];
        c_unicode_to_utf8(latin8codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin8codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin9codetable[i] != 0) {
        latin9codes[latin9codetable[i]] = latin9codetable[i + 1];
        codeslatin[latin9codetable[i + 1]] = latin9codetable[i];
        c_unicode_to_utf8(latin9codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin9codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin10codetable[i] != 0) {
        latin10codes[latin10codetable[i]] = latin10codetable[i + 1];
        codeslatin[latin10codetable[i + 1]] = latin10codetable[i];
        c_unicode_to_utf8(latin10codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin10codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin11codetable[i] != 0) {
        latin11codes[latin11codetable[i]] = latin11codetable[i + 1];
        codeslatin[latin11codetable[i + 1]] = latin11codetable[i];
        c_unicode_to_utf8(latin11codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin11codetable[i];
        i += 2;
    }
    
    i = 0;
    while (latin12codetable[i] != 0) {
        latin12codes[latin12codetable[i]] = latin12codetable[i + 1];
        codeslatin[latin12codetable[i + 1]] = latin12codetable[i];
        c_unicode_to_utf8(latin12codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin12codetable[i];
        i += 2;
    }
    
    i = 0;
    while (latin13codetable[i] != 0) {
        latin13codes[latin13codetable[i]] = latin13codetable[i + 1];
        codeslatin[latin13codetable[i + 1]] = latin13codetable[i];
        c_unicode_to_utf8(latin13codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin13codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin14codetable[i] != 0) {
        latin14codes[latin14codetable[i]] = latin14codetable[i + 1];
        codeslatin[latin14codetable[i + 1]] = latin14codetable[i];
        c_unicode_to_utf8(latin14codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin14codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin15codetable[i] != 0) {
        latin15codes[latin15codetable[i]] = latin15codetable[i + 1];
        codeslatin[latin15codetable[i + 1]] = latin15codetable[i];
        c_unicode_to_utf8(latin15codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin15codetable[i];
        i += 2;
    }
    
    
    i = 0;
    while (latin16codetable[i] != 0) {
        latin16codes[latin16codetable[i]] = latin16codetable[i + 1];
        codeslatin[latin16codetable[i + 1]] = latin16codetable[i];
        c_unicode_to_utf8(latin16codetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latin16codetable[i];
        i += 2;
    }
    
    i = 0;
    while (latinwindows1[i] != 0) {
        latinwindowscodes[latinwindows1[i]] = latinwindows1[i + 1];
        codeslatin[latinwindows1[i + 1]] = latinwindows1[i];
        c_unicode_to_utf8(latinwindows1[i + 1], (uchar*)ch);
        utf8codes[ch] = latinwindows1[i];
        i += 2;
    }
    
    i = 0;
    while (latincodetable[i] != 0) {
        latincodes[latincodetable[i]] = latincodetable[i + 1];
        codeslatin[latincodetable[i + 1]] = latincodetable[i];
        c_unicode_to_utf8(latincodetable[i + 1], (uchar*)ch);
        utf8codes[ch] = latincodetable[i];
        i += 2;
    }
}


//-----------------------------------------------------------------------------------------
//We only return the emoji head, when a head is present
TRANSCHAR getechar(wstring& s, long& i) {
	TRANSCHAR c = getachar(s, i);
	if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
		long j = i + 1;
		TRANSCHAR cc = getachar(s, j);
		while (handlingutf8->c_is_emojicomp(cc)) {
			i = j++;
			cc = getachar(s, j);
		}
	}
	return c;
}

long conversionpostochar(wstring& w, long spos) {
    long cpos = 0;
	long realpos = 0;
	TRANSCHAR c;
	while (realpos != spos) {
		c = getachar(w, realpos);
		if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
			long j = ++realpos;
			c = getachar(w, j);
			while (handlingutf8->c_is_emojicomp(c)) {
				realpos = ++j;
				c = getachar(w, j);
			}
		}
		else
			realpos++;
		cpos++;
	}
	return cpos;
}

void conversionpostochar(wstring& w, long& b, long& e) {
    long sbeg = b;
    long cpos = 0;
    long realpos = 0;
    TRANSCHAR c;
    while (realpos != e) {
        if (realpos == sbeg)
            b = cpos;
        c = getachar(w, realpos);
        if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
            long j = ++realpos;
            c = getachar(w, j);
            while (handlingutf8->c_is_emojicomp(c)) {
                realpos = ++j;
                c = getachar(w, j);
            }
        }
        else
            realpos++;
        cpos++;
    }
    e = cpos;
}

//-----------------------------------------------------------------------------------------

Exporting void inittableutf8(Chaine_UTF8* h) {
    static bool init = false;
    handlingutf8 = h;
    if (init)
        return;
    init=true;
    
    InitNumbers();
    InitLatinTables();
    
    
    alllatincodes[0] = &latincodes;
    alllatincodes[1] = &latincodes;
    alllatincodes[2] = &latin2codes;
    alllatincodes[3] = &latin3codes;
    alllatincodes[4] = &latin4codes;
    alllatincodes[5] = &latin5codes;
    alllatincodes[6] = &latin6codes;
    alllatincodes[7] = &latin7codes;
    alllatincodes[8] = &latin8codes;
    alllatincodes[9] = &latin9codes;
    alllatincodes[10] = &latin10codes;
    alllatincodes[11] = &latin11codes;
    alllatincodes[13] = &latin13codes;
    alllatincodes[14] = &latin14codes;
    alllatincodes[15] = &latin15codes;
    alllatincodes[16] = &latin16codes;
    alllatincodes[17] = &latinwindowscodes;
 
}

//===================================================================

Exporting bool valid_latin_table(short tableindex) {
    if (!alllatincodes.check(tableindex))
        return false;
    return true;
}
/*
 *----------------------------------------------------------------------
 *
 * strtod --
 *
 *    This procedure converts a floating-point number from an ASCII
 *    decimal representation to internal double-precision format.
 *
 * Results:
 *    The return value is the double-precision floating-point
 *    representation of the characters in string.  If endPtr isn't
 *    NULL, then *endPtr is filled in with the address of the
 *    next character after the last one that was part of the
 *    floating-point number.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */
//===================================================================

//Don de Frédéric Roussey
static const double arConvertExpMinus2[] =
{
  1.e-32, 1.e-31, 1.e-30,
  1.e-29, 1.e-28, 1.e-27, 1.e-26, 1.e-25, 1.e-24, 1.e-23, 1.e-22, 1.e-21, 1.e-20,
  1.e-19, 1.e-18, 1.e-17, 1.e-16, 1.e-15, 1.e-14, 1.e-13, 1.e-12, 1.e-11, 1.e-10,
  1.e-09, 1.e-08, 1.e-07, 1.e-06, 1.e-05, 1.e-04, 1.e-03, 1.e-02, 1.e-01, 1.e-00,
};

static const double arConvertExp[] =
{
  1.e00, 1.e01, 1.e02, 1.e03, 1.e04, 1.e05, 1.e06, 1.e07, 1.e08, 1.e09,
  1.e10, 1.e11, 1.e12, 1.e13, 1.e14, 1.e15, 1.e16, 1.e17, 1.e18, 1.e19,
  1.e20, 1.e21, 1.e22, 1.e23, 1.e24, 1.e25, 1.e26, 1.e27, 1.e28, 1.e29,
  1.e30, 1.e31, 1.e32,
};


static inline double power10(BLONG n)
{
  if (n > 0)
  {
     BLONG n1(n & 0x1f);   //n1 modulo 32
     BLONG n2(n & (~0x1f));//n2 div 32
     n2 >>= 5;
     if (n2 == 0)
        return arConvertExp[n1];
     double d = arConvertExp[n1];
     while (n2--)
        d *= arConvertExp[32];
     return d;
  }
  else if (n < 0)
  {
     BLONG n1(n & 0x1f);   //n1 modulo 32
     BLONG n2(n & (~0x1f));//n2 div 32
     n2 >>= 5;
     if (n2 == -1)
        return arConvertExpMinus2[n1];
     double d = arConvertExpMinus2[n1];
     while (++n2)
        d *= arConvertExpMinus2[0];
     return d;
  }
  return 1;
}
//----------------------------------------------------------------------------------------


//===================================================================
//===================================================================
extern "C" {
    //Implementation, which replaces strtod, which does not work properly on some platform...
    double conversiontofloathexa(const char* s, int sign) {
        BLONG v = 0;
        bool cont = true;
        uchar c = *s++;
        while (cont) {
            switch (digitaction[c]) {
                case '0':
                    v = (v << 4) | (c & 15);
                    c = *s++;
                    continue;
                case 'X':
                    v = (v << 4) | (c - 55);
                    c = *s++;
                    continue;
                case 'x':
                    v = (v << 4) | (c - 87);
                    c = *s++;
                    continue;
                default:
                    cont = false;
            }
        }
        
        double res = v;

        if (c == '.') {
            uchar mantissa = 0;
            v = 0;
            cont = true;
            c = *s++;
            while (cont) {
                switch (digitaction[c]) {
                    case '0':
                        v = (v << 4) | (c & 15);
                        c = *s++;
                        mantissa += 4;
                        continue;
                    case 'X':
                        v = (v << 4) | (c - 55);
                        mantissa += 4;
                        c = *s++;
                        continue;
                    case 'x':
                        v = (v << 4) | (c - 87);
                        mantissa += 4;
                        c = *s++;
                        continue;
                    default:
                        cont = false;
                }
            }
            
            res += (double)v/(double)(1 << mantissa);
        }
        

        if ((c &0xDF) == 'P') {
            bool sgn = false;
            if (*s == '-') {
                sgn = true;
                ++s;
            }
            else {
                if (*s == '+')
                    ++s;
            }
            
            v = *s++ & 15;
            while (isadigit(*s)) {
                v = (v << 3) + (v << 1) + (*s++ & 15);
            }
            v = 1 << v;
            if (sgn)
                res *= 1 / (double)v;
            else
                res *= v;

        }
        
        return res*sign;
    }
}

double conversionfloathexa(const char* s) {
    while (*s!=0 && *s<=32) ++s;
    //End of string...
    if (*s ==0 )
        return 0;
    
    int sign = 1;

    //Sign
    if (*s=='-') {
        sign = -1;
        ++s;
    }
    else
        if (*s=='+')
            ++s;
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        return conversiontofloathexa(s, sign);
    }
    
    BLONG v;
    if (isadigit(*s)) {
        v = *s++ & 15;
        while (isadigit(*s)) {
            v = (v << 3) + (v << 1) + (*s++ & 15);
        }
        if (!*s)
            return v*sign;
    }
    else
        return 0;
    
    double res = v;

    if (*s=='.') {
        ++s;
        if (isadigit(*s)) {
            uchar mantissa = 1;
            v = *s++ & 15;
            while (isadigit(*s)) {
                v = (v << 3) + (v << 1) + (*s++ & 15);
                ++mantissa;
            }
            res += (double)v / power10(mantissa);
        }
        else
            return res*sign;
    }
        
    if ((*s &0xDF) == 'E') {
        ++s;
        long sgn = 1;
        if (*s == '-') {
            sgn = -1;
            ++s;
        }
        else {
            if (*s == '+')
                ++s;
        }
        
        if (isadigit(*s)) {
            v = *s++ & 15;
            while (isadigit(*s))
                v = (v << 3) + (v << 1) + (*s++ & 15);
            
            res *= power10(v*sgn);
        }
    }
    return res*sign;
}

//------------------------------------------------------------------------------------
//Keep length
//------------------------------------------------------------------------------------
double conversiontofloathexa(const char* s, int sign, short& l) {
    BLONG v = 0;
    bool cont = true;
    uchar c = *s++;
    l++;
    while (cont) {
        switch (digitaction[c]) {
            case '0':
                v = (v << 4) | (c & 15);
                c = *s++;
                l++;
                continue;
            case 'X':
                v = (v << 4) | (c - 55);
                c = *s++;
                l++;
                continue;
            case 'x':
                v = (v << 4) | (c - 87);
                c = *s++;
                l++;
                continue;
            default:
                cont = false;
        }
    }
    
    double res = v;

    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        cont = true;
        c = *s++;
        l++;
        while (cont) {
            switch (digitaction[c]) {
                case '0':
                    v = (v << 4) | (c & 15);
                    c = *s++;
                    l++;
                    mantissa += 4;
                    continue;
                case 'X':
                    v = (v << 4) | (c - 55);
                    mantissa+=4;
                    c = *s++;
                    l++;
                    continue;
                case 'x':
                    v = (v << 4) | (c - 87);
                    mantissa += 4;
                    c = *s++;
                    l++;
                    continue;
                default:
                    cont = false;
            }
        }
        
        res += (double)v/(double)(1 << mantissa);
    }
    

    if ((c &0xDF) == 'P') {
        bool sgn = false;
        if (*s == '-') {
            sgn = true;
            ++s;
            l++;
        }
        else {
            if (*s == '+') {
                ++s;
                ++l;
            }
        }
        
        v = *s++ & 15;
        l++;
        while (isadigit(*s)) {
            v = (v << 3) + (v << 1) + (*s++ & 15);
            l++;
        }
        v = 1 << v;
        if (sgn)
            res *= 1 / (double)v;
        else
            res *= v;

    }
    
    return res*sign;
}

double conversionfloathexa(const char* s, short& l) {
    l = 0;
    //End of string...
    if (*s ==0 )
        return 0;
    
    int sign = 1;

    //Sign
    if (*s=='-') {
        sign = -1;
        l++;
        ++s;
    }
    else
        if (*s=='+') {
            ++s;
            l++;
        }
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        l++;
        return conversiontofloathexa(s, sign, l);
    }
    
    BLONG v;
    if (isadigit(*s)) {
        v = *s++ & 15;
        l++;
        while (isadigit(*s)) {
            v = (v << 3) + (v << 1) + (*s++ & 15);
            l++;
        }
        if (!*s)
            return v*sign;
    }
    else
        return 0;
    
    double res = v;

    if (*s=='.') {
        ++s;
        l++;
        if (isadigit(*s)) {
            uchar mantissa = 1;
            v = *s++ & 15;
            l++;
            while (isadigit(*s)) {
                v = (v << 3) + (v << 1) + (*s++ & 15);
                l++;
                ++mantissa;
            }
            res += (double)v / power10(mantissa);
        }
        else
            return res*sign;
    }
        
    if ((*s &0xDF) == 'E') {
        ++s;
        l++;
        long sgn = 1;
        if (*s == '-') {
            sgn = -1;
            ++s;
            l++;
        }
        else {
            if (*s == '+') {
                ++s;
                ++l;
            }
        }
        
        if (isadigit(*s)) {
            v = *s++ & 15;
            l++;
            while (isadigit(*s)) {
                v = (v << 3) + (v << 1) + (*s++ & 15);
                l++;
            }
            
            res *= power10(v*sgn);
        }
    }
    return res*sign;
}


//----- WIDE CHAR Versions
double conversiontofloathexa(const wchar_t* s, int sign, short& l) {
    BLONG v = 0;
    bool cont = true;
    uchar c = *s++;
    l++;
    while (cont) {
        switch (digitaction[c]) {
            case '0':
                v = (v << 4) | (c & 15);
                c = *s++;
                l++;
                continue;
            case 'X':
                v = (v << 4) | (c - 55);
                c = *s++;
                l++;
                continue;
            case 'x':
                v = (v << 4) | (c - 87);
                c = *s++;
                l++;
                continue;
            default:
                cont = false;
        }
    }
    
    double res = v;

    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        cont = true;
        c = *s++;
        l++;
        while (cont) {
            switch (digitaction[c]) {
                case '0':
                    v = (v << 4) | (c & 15);
                    c = *s++;
                    l++;
                    mantissa += 4;
                    continue;
                case 'X':
                    v = (v << 4) | (c - 55);
                    mantissa+=4;
                    c = *s++;
                    l++;
                    continue;
                case 'x':
                    v = (v << 4) | (c - 87);
                    mantissa += 4;
                    c = *s++;
                    l++;
                    continue;
                default:
                    cont = false;
            }
        }
        
        res += (double)v/(double)(1 << mantissa);
    }
    

    if ((c &0xDF) == 'P') {
        bool sgn = false;
        if (*s == '-') {
            sgn = true;
            ++s;
            l++;
        }
        else {
            if (*s == '+') {
                ++s;
                ++l;
            }
        }
        
        v = *s++ & 15;
        l++;
        while (isadigit(*s)) {
            v = (v << 3) + (v << 1) + (*s++ & 15);
            l++;
        }
        v = 1 << v;
        if (sgn)
            res *= 1 / (double)v;
        else
            res *= v;

    }
    
    return res*sign;
}

double conversionfloathexa(const wchar_t* s, short& l) {
    l = 0;
    //End of string...
    if (*s ==0 )
        return 0;
    
    int sign = 1;

    //Sign
    if (*s=='-') {
        sign = -1;
        l++;
        ++s;
    }
    else
        if (*s=='+') {
            ++s;
            l++;
        }
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        l++;
        return conversiontofloathexa(s, sign, l);
    }
    
    BLONG v;
    if (isadigit(*s)) {
        v = *s++ & 15;
        l++;
        while (isadigit(*s)) {
            v = (v << 3) + (v << 1) + (*s++ & 15);
            l++;
        }
        if (!*s)
            return v;
    }
    else
        return 0;
    
    double res = v;

    if (*s=='.') {
        ++s;
        l++;
        if (isadigit(*s)) {
            uchar mantissa = 1;
            v = *s++ & 15;
            l++;
            while (isadigit(*s)) {
                v = (v << 3) + (v << 1) + (*s++ & 15);
                l++;
                ++mantissa;
            }
            res += (double)v / power10(mantissa);
        }
        else
            return res;
    }
        
    if ((*s &0xDF) == 'E') {
        ++s;
        l++;
        long sgn = 1;
        if (*s == '-') {
            sgn = -1;
            ++s;
            l++;
        }
        else {
            if (*s == '+') {
                ++s;
                ++l;
            }
        }
        
        if (isadigit(*s)) {
            v = *s++ & 15;
            l++;
            while (isadigit(*s)) {
                v = (v << 3) + (v << 1) + (*s++ & 15);
                l++;
            }
            
            res *= power10(v*sgn);
        }
    }
    return res*sign;
}
//===================================================================
Exporting BLONG conversionintegerhexa(char* number) {
    while (*number!=0 && *number<=32) ++number;
    uchar c = *number;
    if (!c)
        return 0;
    
    int sign = 1;
    if (c == '-') {
        c = *++number;
        sign = -1;
    }
    else
        if (c == '+') {
            c = *++number;
        }
    
    
    ++number;
    BLONG v;
    if (c == '0' && *number == 'x') {
        ++number;
        
        v = 0;
        while (*number) {
            v <<= 4;
            c = *number;
            switch (digitaction[c]) {
                case '0':
                    v |= c & 15;
                    ++number;
                    continue;
                case 'X':
                    v |= c - 55;
                    ++number;
                    continue;
                case 'x':
                    v |= c - 87;
                    ++number;
                    continue;
                default:
                    return v*sign;
            }
        }
    }
    else {
        if (isadigit(c)) {
            v = c & 15;
            while (isadigit(*number))
                v = (v << 3) + (v << 1) + (*number++ & 15);
        }
        else
            return 0;
    }
    
    return v*sign;
}

BLONG conversionintegerhexa(wstring& number) {
    long ipos=0;
    
    while (number[ipos]<=32) ++ipos;
    

    int sign = 1;
    if (number[ipos] == '-') {
        ++ipos;
        sign = -1;
    }
    else
        if (number[ipos] == '+')
            ++ipos;
    
    BLONG v = 0;
    
    uchar c = number[ipos++];
    if (number.size() == ipos)
        return (c - 48);

    if (c == '0' || number[ipos] == 'x') {
        ipos++;
        c = number[ipos++];
        while (c) {
            v <<= 4;
            switch (digitaction[c]) {
                case '0':
                    v |= c & 15;
                    c = number[ipos++];
                    continue;
                case 'X':
                    v |= c - 55;
                    c = number[ipos++];
                    continue;
                case 'x':
                    v |= c - 87;
                    c = number[ipos++];
                    continue;
                default:
                    return v*sign;
            }
        }
    }
    else {
        if (isadigit(c)) {
            v = c & 15;
            c = number[ipos++];
            while (isadigit(c)) {
                v = (v << 3) + (v << 1) + (c & 15);
                c = number[ipos++];
            }
        }
    }
    return v*sign;
}

//===================================================================
//Conversion from string to double...
Exporting double convertfloat(char* s) {
    return conversionfloathexa(s);
}

Exporting double convertfloat(wstring value) {
    string v;
    sc_unicode_to_utf8(v, value);
    return conversionfloathexa(STR(v));
}

Exporting long convertinteger(wstring value) {
    return conversionintegerhexa(value);
}

Exporting double convertfloat(string v) {
    return conversionfloathexa(STR(v));
}

Exporting long convertinteger(string v) {
    return conversionintegerhexa(STR(v));
}

//-----------------------------------------------------------
Exporting double convertdouble(wstring value) {
    string v;
    sc_unicode_to_utf8(v, value);
    return convertfloat(STR(v));
}

Exporting long convertlong(wstring value) {
    string v;
    sc_unicode_to_utf8(v, value);
    return conversionintegerhexa(STR(v));
}

Exporting double convertdouble(string v) {
    return convertfloat(STR(v));
}

Exporting long convertlong(string v) {
    return conversionintegerhexa(STR(v));
}

//===================================================================
static wstring convertdigits(string& v, wstring w) {
    for (short i=0;i<v.size();i++)
        w+=(wchar_t)v[i];
    return w;
}

static wstring convertdigits(string& v) {
    wstring w;
    for (short i=0;i<v.size();i++)
        w+=(wchar_t)v[i];
    return w;
}

static void convertingdigits(string& v, wstring& w) {
    for (short i=0;i<v.size();i++)
        w+=(wchar_t)v[i];
}


const short nbits =  11;

class doubledecimal {
    public :
    long n;
    double v;
    
    doubledecimal(double d) {
        v = d;
        n=0;
    }
    
    bool check() {
        v=(v-n)*10;
        n=v;
        if (!n && v <= 1e-9)
            return false;
        return true;
    }
};

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------

Exporting void convertnumber(long v, string& s) {
    if (v>=0 && v <= 9999 && numbers.check(v)) {
        s = numbers.get(v);
        return;
    }
    
    bool sgn=false;
    if (v<0) {
        v=-v;
        if (v>=0 && v <= 9999 && numbers.check(v)) {
            s = "-";
            s+=numbers.get(v);
            return;
        }
        sgn=true;
    }
    
    char buff[20];
    char* num = buff+19;
    *num-- = 0;
    
    const char* nb;
    long inter=v;
    long rest=0;
    while (v) {
        inter = v/1000;
        rest = v-inter*1000;
        v = inter;
        nb=invertnumbers[rest];
        while (*nb) *num--=*nb++;
    }
    
    if (rest<10)
        num+=2;
    else
        if (rest<100)
            num++;

    if (sgn)
        *num='-';
    else
        ++num;
    s = num;
}

Exporting void convertnumber(long v, wstring& s) {
    if (v>=0 && v <= 9999 && numbers.check(v)) {
        s=L"";
        convertingdigits(numbers.get(v),s);
        return;
    }
    
    bool sgn=false;
    if (v<0) {
        v=-v;
        if (v>=0 && v <= 9999 && numbers.check(v)) {
            s = L"-";
            convertingdigits(numbers.get(v),s);
            return;
        }
        sgn=true;
    }
    
    wchar_t buff[50];
    
    wchar_t* num = buff+49;
    *num-- = 0;

    char* nb;
    long inter=v;
    long rest=0;
    while (v) {
        inter = v/1000;
        rest = v-inter*1000;
        v = inter;
        nb=invertnumbers[rest];
        while (*nb) *num--=(wchar_t)*nb++;
    }

    if (rest<10)
        num+=2;
    else
        if (rest<100)
            num++;

    if (sgn)
        *num='-';
    else
        ++num;
    
    s = num;
}

Exporting void convertnumber(double val, string& s) {
    if (!val) {
        s =  "0";
        return;
    }
    
    char buff[100];
    
    bool sgn=false;
    
    double v=val;
    if (v<0) {
        v=-v;
        sgn=true;
    }
    
    if (v <= 1e-7) {
        sprintf_s(buff,100,"%g",val);
        s = buff;
        return;
    }

    char* num = buff+50;
    
    *num-- = 0;
    
    //First the main digit
    BLONG vv=v;
    val=v-vv;
    const char* nbb;
    
    char nb=0;
    
    if (vv <= 9999 && numbers.check(vv)) {
        if (!val) {
            if (!sgn) {
                s = numbers[vv];
                return;
            }
            s = "-";
            s+=numbers[vv];
            return;
        }
        nb = numbers[vv].size();
        num-=nb-1;
        strcpy(num, STR(numbers[vv]));
        if (sgn)
            *--num='-';
    }
    else {
        BLONG inter=vv;
        BLONG rest = 0;
        while (vv) {
            inter = vv/1000;
            rest = vv-inter*1000;
            vv = inter;
            nbb=invertnumbers[rest];
            while (*nbb) {
                ++nb;
                *num--=*nbb++;
            }
        }

        if (rest<10) {
            num+=2;
            nb-=2;
        }
        else
            if (rest<100) {
                num++;
                --nb;
            }

        //98192819928999
        if (nb>8) {
            *num=num[1];
            num[1]='.';
            num[7]='e';
            num[8]='+';
            --nb;
            short sz;
            if (nb<10) {
                num[9]='0';
                num[10]=0x30|nb;
                sz=11;
            }
            else {
                nbb=invertnumbers[nb];
                nb = 8+nbb[4]; //the size of the string is stored on position 4
                sz = nb+1;
                while (*nbb) num[nb--]=*nbb++;
            }
            if (sgn) {
                *--num='-';
                ++sz;
            }
            num[sz]=0;
            s = num;
            return;
        }
        if (sgn)
            *num='-';
        else
            num++;
    }
    
    if (val) {
        nbb=num;
        //now, we now that we can start writing at buff+50;
        num=buff+50;
        *num++='.';
        nb = nbits;
        doubledecimal d(val);
        
        while (nb) {
            if (!d.check())
                break;
            *num++=0x30|d.n;
            nb--;
        }
        
        *num=0;
        if (!nb) { //arrondi
            if (num[-1] == '9' && num[-2] == '9' && buff[51] != '9') {
                num--;
                while (*num=='9') --num;
                num+=2;
            }
            
            if (num[-1]>='6' && num[-2] != '9')
                num[-2]++;
            num[-1] = 0;
            --num;
            s = nbb;
            return;
        }
        if (nb==nbits  || num[-1]==48) {
            //we remove the '.'
            num[-1]=0;
            --num;
        }
        s = nbb;
        return;
    }
    
    s = num;
}

Exporting void convertnumber(double val, wstring& s) {
    if (!val) {
        s = L"0";
        return;
    }
    
    wchar_t buff[100];
    bool sgn=false;

    double v=val;
    if (v<0) {
        v=-v;
        sgn=true;
    }

    if (v <= 1e-7) {
        swprintf_s(buff,100,L"%g",val);
        s = buff;
        return;
    }
    
    wchar_t* num = buff+50;
    *num-- = 0;
    
    //First the main digit
    BLONG vv=v;
    val=v-vv;
    
    char nb=0;
    
    if (vv <= 9999 && numbers.check(vv)) {
        if (!val) {
            if (!sgn) {
                s=L"";
                convertingdigits(numbers[vv],s);
                return;
            }
            s = L"-";
            convertingdigits(numbers[vv],s);
            return;
        }
        nb = numbers[vv].size();
        num-=nb-1;
        for (short i=0;i<nb;i++)
            num[i]= (wchar_t)(numbers[vv][i]);
        if (sgn)
            *--num='-';
    }
    else {
        BLONG inter=vv;
        BLONG rest=0;
        while (vv) {
            inter = vv/1000;
            rest = vv-inter*1000;
            vv = inter;
            char* nbb;
            nbb=invertnumbers[rest];
            while (*nbb) {
                ++nb;
                *num--=(wchar_t)*nbb++;
            }
        }

        if (rest<10) {
            num+=2;
            nb-=2;
        }
        else
            if (rest<100) {
                num++;
                --nb;
            }

        if (nb>8) {
            *num=num[1];
            num[1]='.';
            num[7]='e';
            num[8]='+';
            --nb;
            short sz;
            if (nb<10) {
                num[9]='0';
                num[10]=(wchar_t)(0x30|nb);
                sz=11;
            }
            else {
                const char* nbb=invertnumbers[nb];
                nb = 8+nbb[4]; //the size of the string is stored on position 4
                sz = nb+1;
                while (*nbb) num[nb--]=(wchar_t)*nbb++;
            }

            if (sgn) {
                *--num='-';
                ++sz;
            }
            num[sz]=0;
            s = num;
            return;
        }

        if (sgn)
            *num='-';
        else
            num++;
    }
    
    if (val) {
        wchar_t* beg=num;
        //now, we now that we can start writing at buff+50;
        num=buff+50;
        *num++='.';
        nb = nbits;
        doubledecimal d(val);
        
        while (nb) {
            if (!d.check())
                break;
            *num++= (wchar_t)(0x30|d.n);
            nb--;
        }
        
        *num=0;
        if (!nb) { //arrondi
            if (num[-1] == '9' && num[-2] == '9' && buff[51] != '9') {
                num--;
                while (*num=='9') --num;
                num+=2;
            }
            
            if (num[-1]>='6' && num[-2] != '9')
                num[-2]++;
            num[-1] = 0;
            --num;
            s= beg;
            return;
        }
        if (nb==nbits  || num[-1]==48) {
            //we remove the '.'
            num[-1]=0;
            --num;
        }
        s = beg;
        return;
    }
    s = num;
}

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------

Exporting string convertfromnumber(long v) {
    if (v>=0 && v <= 9999 && numbers.check(v))
        return numbers.get(v);
    
    bool sgn=false;
    if (v<0) {
        v=-v;
        if (v>=0 && v <= 9999 && numbers.check(v)) {
            string s = "-";
            s+=numbers.get(v);
            return s;
        }
        sgn=true;
    }
    
    char buff[20];
    char* num = buff+19;
    *num-- = 0;
    
    const char* nb;
    long inter=v;
    long rest=0;
    while (v) {
        inter = v/1000;
        rest = v-inter*1000;
        v = inter;
        nb=invertnumbers[rest];
        while (*nb) *num--=*nb++;
    }
    
    if (rest<10)
        num+=2;
    else
        if (rest<100)
            num++;

    if (sgn)
        *num='-';
    else
        ++num;
    return string(num,(buff+19-num));
}

Exporting wstring wconvertfromnumber(long v) {
    if (v>=0 && v <= 9999 && numbers.check(v))
        return convertdigits(numbers.get(v));
    
    bool sgn=false;
    if (v<0) {
        v=-v;
        if (v>=0 && v <= 9999 && numbers.check(v))
            return convertdigits(numbers.get(v),L"-");
        sgn=true;
    }
    
    wchar_t buff[20];
    
    wchar_t* num = buff + 19;
    *num-- = 0;
    
    char* nb;
    long inter=v;
    long rest=0;
    while (v) {
        inter = v/1000;
        rest = v-inter*1000;
        v = inter;
        nb=invertnumbers[rest];
        while (*nb) *num--=(wchar_t)*nb++;
    }

    if (rest<10)
        num+=2;
    else
        if (rest<100)
            num++;

    if (sgn)
        *num='-';
    else
        ++num;
    
    return wstring(num,buff-num+19);
}


//--------------------------------------------------------------------------------
// Conversion one character: UTF8, UNICODE, LATIN

Exporting TRANSCHAR c_latin_table_to_unicode(int tableindex, TRANSCHAR c) {
    if (c < 128 || c > 255 || !alllatincodes.check(tableindex))
        return c;
    
    if (alllatincodes[tableindex]->check(c))
        return (*alllatincodes[tableindex])[c];
    
    return c;
}

Exporting uchar c_latin_to_utf8(unsigned char code, unsigned char* utf) {
    if (!(code & 0x0080)) {
        utf[0] = (unsigned char)code;
        utf[1] = 0;
        return 1;
    }
    if (latincodes.check(code))
        c_unicode_to_utf8(latincodes[code], utf);
    else {
        utf[0] = 0xc0 | (code >> 6);
        utf[1] = 0x80 | (code & 0x3f);
        utf[2] = 0;
        return 2;
    }
    return 1;
}


uchar c_latin_to_utf8(unsigned char* utf, unsigned char* res, bool checktable = false) {
    uchar c = utf[0];
    if (!(c & 0x0080)) {
        res[0] = (unsigned char)c;
        res[1] = 0;
        return 1;
    }
    
    uchar nb = c_test_utf8(utf);
    //we take two, this is a UTF8 code...
    if (nb) {
        for (int i = 0; i <= nb; i++)
            res[i] = utf[i];
        res[nb+1] = 0;
        return nb+1;
    }
    
    if (checktable) {
        res[0] = '.';
        res[1] = 0;
        return 1;
    }
    
    if (latincodes.check(c))
        c_unicode_to_utf8(latincodes[c], res);
    else {
        res[0] = 0xc0 | (c >> 6);
        res[1] = 0x80 | (c & 0x3f);
        res[2] = 0;
    }
    return 1;
}

uchar concatenate_latin_unicode(wstring& res, unsigned char* utf, bool checktable) {
	uchar c = utf[0];
	if (!(c & 0x0080)) {
		res += (wchar_t&)c;
		return 1;
	}

	TRANSCHAR code;
	c = c_utf8_to_unicode(utf, code);

	concat_char_convert_utf16(res, code);

    if (!c) {
		if (checktable)
			res += L'.';
		else
			res += (wchar_t)utf[0];
	}
	return (c + 1);
}

uchar c_latin_to_unicode(unsigned char* utf, TRANSCHAR& code, bool checktable) {
    uchar c = utf[0];
    if (!(c & 0x0080)) {
        code = c;
        return 1;
    }
    
    //we take two, this is a UTF8 code...
    c= c_utf8_to_unicode(utf, code);
    if (!c) {
        if (checktable)
            code = '.';
        else
            code = utf[0];
    }
    return (c+1);
}

Exporting TRANSCHAR c_unicode_to_latin(TRANSCHAR u) {
    if (u < 0x0080)
        return u;
    
    if (codeslatin.check(u))
        return codeslatin[u];
    
    return 0;
}

uchar c_utf8_to_latin(unsigned char* utf, uchar& nb) {
    nb = 1;
    uchar c = utf[0];
    if (!(c & 0x0080))
        return c;
    
    uchar code;
    if ((utf[0] & 0xF0)== 0xF0) {
        if ((utf[1] & 0x80) == 0x80 && (utf[2] & 0x80)== 0x80 && (utf[3] & 0x80)== 0x80) {
            code = (utf[0] & 0x7) << 18;
            code |= (utf[1] & 0x3F) << 12;
            code |= (utf[2] & 0x3F) << 6;
            code |= (utf[3] & 0x3F);
            nb = 4;
            return code;
        }
        return c;
    }
    
    if ((utf[0] & 0xE0)== 0xE0) {
        if ((utf[1] & 0x80)== 0x80 && (utf[2] & 0x80)== 0x80) {
            code = (utf[0] & 0xF) << 12;
            code |= (utf[1] & 0x3F) << 6;
            code |= (utf[2] & 0x3F);
            nb = 3;
            return code;
        }
        return c;
    }
    
    if ((utf[0] & 0xC0)== 0xC0 && (utf[1] & 0x80)== 0x80) {
        code = (utf[0] & 0x1F) << 6;
        code |= (utf[1] & 0x3F);
        nb = 2;
        return code;
    }
    
    return c;
}


Exporting char c_detect_utf8(unsigned char* utf) {
    if (utf == NULL || !(utf[0] & 0x0080))
        return 0;

    if ((utf[0] & 0xF0)== 0xF0) {
        if ((utf[1] & 0x80) == 0x80 && (utf[2] & 0x80)== 0x80 && (utf[3] & 0x80)== 0x80)
            return 3;
        return 0;
    }
    
    if ((utf[0] & 0xE0)== 0xE0) {
        if ((utf[1] & 0x80)== 0x80 && (utf[2] & 0x80)== 0x80)
            return 2;
        return 0;
    }
    
    if ((utf[0] & 0xC0)== 0xC0 && (utf[1] & 0x80)== 0x80)
        return 1;
    
    return 0;
}

//---------------------------------------------------------------------------
//convert a character position from byte to char and back
void c_chartobyteposition(unsigned char* contenu, long sz, long& bcpos, long& ecpos) {
    if (ecpos > sz)
        return;
    
    long begcpos = bcpos;
    long endcpos = ecpos;
    long i = 0;
    long nb;
    
    while (endcpos > 0 && i<sz) {
        if (!begcpos)
            bcpos = i;
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        endcpos--;
        begcpos--;
    }
    ecpos = i;
}

void c_bytetocharposition(unsigned char* contenu, long& bbpos, long& ebpos) {
    long i = 0;
    long sz = 0;
    long nb;
    long bpos = bbpos;
    
    while (i < ebpos) {
        if (i == bpos)
            bbpos = sz;
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        sz++;
    }
    
    ebpos = sz;
}


Exporting long c_chartobyteposition(unsigned char* contenu, long sz, long charpos) {
    if (charpos > sz)
        return sz;
    
    long i = 0;
#ifdef INTELINTRINSICS
    if (check_ascii(contenu, charpos, i))
        return charpos;
    charpos-=i;
#endif
    
    long nb;
    while (charpos > 0 && i<sz) {
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        charpos--;
    }
    return i;
}

Exporting long c_chartobyteposition(unsigned char* contenu, long charpos) {
    long i = 0;
    long nb;
    while (charpos > 0 && contenu[i]) {
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        charpos--;
    }
    return i;
}

Exporting long c_bytetocharposition(unsigned char* contenu, long charpos) {
    long i = 0;
    long sz = 0;
    long nb;
    while (i < charpos) {
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        sz++;
    }
    return sz;
}

Exporting long c_bytetocharpositionidx(unsigned char* contenu, long charpos, long& sz, long& i) {
    long nb;
    while (i<charpos) {
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        sz++;
    }
    return sz;
}

Exporting long c_chartobytepositionidx(unsigned char* contenu, long charpos, long& sz, long& i) {
    long nb;
    while (charpos>0) {
        nb = c_test_utf8(contenu + i);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,i)) {
            i++;
            while (handlingutf8->c_is_emojicomp(contenu, i)) {
                i++;
            }
        }
        else
            i += nb + 1;
        sz++;
        charpos--;
    }
    return i;
}

//---------------------------------------------------------------------------
Exporting long size_utf16(unsigned char* str, long sz, long& charsize) {
    
#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(str, sz, i)) {
        charsize = sz;
        return sz;
    }
#endif
    
    charsize = 0;
    long sizeutf16 = 0;
    TRANSCHAR c;
    uchar nb;
    while (sz--) {
        if (*str & 0x80) {
            nb = c_utf8_to_unicode(str, c);
            str += nb + 1;
            sz -= nb;
            ++sizeutf16;
            ++charsize;
            if (c & 0xFFFF0000)
                ++sizeutf16;
            continue;
        }
        ++charsize;
        ++sizeutf16;
        ++str;
    }
    return sizeutf16;
}

wstring getfullchar(wstring& s, long& i) {
	TRANSCHAR c = getachar(s, i);
	wstring res;
	store_char_check_utf16(res, c);
	if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
		long j = i + 1;
		c = getachar(s, j);
		while (handlingutf8->c_is_emojicomp(c)) {
			i = j++;
			concat_char_check_utf16(res, c);
			c = getachar(s, j);
		}
	}
	return res;
}

void getafullchar(wstring& s, wstring& res, long& i) {
    TRANSCHAR c = getachar(s, i);
	i++;
    concat_char_check_utf16(res, c);
    if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
        c = getachar(s, i);
		i++;
        long j = i;
        while (handlingutf8->c_is_emojicomp(c)) {
            concat_char_check_utf16(res, c);
            i = j;
            c = getachar(s, j);
			j++;
        }
    }
}


// Conversion strings
#ifdef WSTRING_IS_UTF16
//Conversion of a Unicode character to UTF16 before concatenating it to res...
void concat_char_convert_utf16(wstring& res, TRANSCHAR code) {
	if (!(code & 0xFFFF0000))
		res += (wchar_t)code;
	else {
		TRANSCHAR c16;
		c_unicode_to_utf16(c16, code);
		res += (wchar_t)(c16 >> 16);
		res += (wchar_t)(c16 & 0xFFFF);
	}
}


//On windows wchar_t is only 16 bits long...
//wstring is encoded as a UTF16 string...
//We concatenate here a UTF16 characters that could be stored on 4 bytes to two uint16_t chars...
//The order of characters here is given by 0xDC00 or 0xD800
Exporting void store_char_check_utf16(wstring& res, TRANSCHAR code) {
	if ((code & 0xFF00FF000) == 0xD800D8000) {
		if ((code & 0xFF00FF000) == 0xDC00D8000) {
			res = (wchar_t)(code & 0xFFFF);
			res += (wchar_t)(code >> 16);
		}
		else {
			res = (wchar_t)(code >> 16);
			res += (wchar_t)(code & 0xFFFF);
		}
	}
	else {
		res = L"";
		concat_char_convert_utf16(res, code);
	}
}

Exporting void concat_char_check_utf16(wstring& res, TRANSCHAR code) {
	if ((code & 0xFF00FF000) == 0xD800D8000) {
		if ((code & 0xFF00FF000) == 0xDC00D8000) {
			res += (wchar_t)(code & 0xFFFF);
			res += (wchar_t)(code >> 16);
		}
		else {
			res += (wchar_t)(code >> 16);
			res += (wchar_t)(code & 0xFFFF);
		}
	}
	else
		concat_char_convert_utf16(res, code);
}

Exporting size_t size_w(wchar_t* w) {
	wstring s;
	long i = 0;
	while (w[i] != 0)
		s += w[i++];
	return size_w(s);
}

size_t size_w(wstring& w) {
    long lg = w.size();
	
	long sz = 0;
	long i = 0;

#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), lg, sz))
        return lg;
	i = sz;
#endif

	TRANSCHAR c;
	long j;
	for (; i < lg; i++) {
		if ((w[i] & 0xFF00) == 0xD800) {
			c = getachar(w, i);
			if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
				j = i + 1;
				c = getachar(w, j);
				while (handlingutf8->c_is_emojicomp(c)) {
					i = j++;
					c = getachar(w, j);
				}
			}
		}
		sz++;
	}
	return sz;
}

size_t size_w(wstring& w, long& first) {
    first = -1;

    long sz = 0;
    long lg = w.size();
    long i = 0;
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), lg, sz))
        return lg;
    i = sz;
#endif

	TRANSCHAR c;
	long j;
	for (; i < lg; i++) {
		if ((w[i] & 0xFF00) == 0xD800) {
			if (first == -1)
				first = i;
			c = getachar(w, i);
			if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
				j = i + 1;
				c = getachar(w, j);
				while (handlingutf8->c_is_emojicomp(c)) {
					i = j++;
					c = getachar(w, j);
				}
			}
		}
		sz++;
	}

	return sz;
}

//We convert a position in characters into an actual string position
long convertchartoposutf16(wstring& w, long first, long cpos) {
	long  i = first;
	long j;
	TRANSCHAR c;
	while (first != cpos) {
		if ((w[i] & 0xFF00) == 0xD800) {
			i += getChar(w, i, c);
			if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
				j = getChar(w, i, c);
				while (handlingutf8->c_is_emojicomp(c)) {
					i += j;
					j = getChar(w, i, c);
				}
			}
		}
		else
            i++;
		first++;
	}
	return i;
}

long convertpostocharutf16(wstring& w, long first, long spos) {
	long  i = first;
	long j;
	TRANSCHAR c;
	while (i != spos) {
		if ((w[i] & 0xFF00) == 0xD800) {
			i += getChar(w, i, c);
			if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
				j = getChar(w, i, c);
				while (handlingutf8->c_is_emojicomp(c)) {
					i += j;
					j = getChar(w, i, c);
				}
			}
		}
		else
			i++;
		first++;
	}
	return first;
}

Exporting long convertpostochar(wstring& w, long first, long spos) {
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
	if (!check_large_char(WSTR(w), w.size(), first))
        return spos;
#endif
    
    if (spos <= first)
        return spos;
    
	long  i = first;
	long j;
	TRANSCHAR c;
	while (i != spos) {
		if ((w[i] & 0xFF00) == 0xD800) {
			i += getChar(w, i, c);
			if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
				j = getChar(w, i, c);
				while (handlingutf8->c_is_emojicomp(c)) {
					i += j;
					j = getChar(w, i, c);
				}
			}
		}
		else
			i++;
		first++;
	}
	return first;
}

Exporting long convertchartopos(wstring& w, long first, long cpos) {
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
	if (!check_large_char(WSTR(w), w.size(), first))
        return cpos;
#endif
    
    if (cpos <= first)
        return cpos;
    
	long  i = first;
	long j;
	TRANSCHAR c;
	while (first != cpos) {
		if ((w[i] & 0xFF00) == 0xD800) {
			i += getChar(w, i, c);
			if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
				j = getChar(w, i, c);
				while (handlingutf8->c_is_emojicomp(c)) {
					i += j;
					j = getChar(w, i, c);
				}
			}
		}
		else
			i++;
		first++;
	}
	return i;
}

long convertpostocharraw(wstring& w, long first, long spos) {
    long  i = first;
    long j;
    TRANSCHAR c;
    while (i != spos) {
        if ((w[i] & 0xFF00) == 0xD800) {
            i += getChar(w, i, c);
            if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
                j = getChar(w, i, c);
                while (handlingutf8->c_is_emojicomp(c)) {
                    i += j;
                    j = getChar(w, i, c);
                }
            }
        }
        else
            i++;
        first++;
    }
    return first;
}

long convertchartoposraw(wstring& w, long first, long cpos) {
    long  i = first;
    long j;
    TRANSCHAR c;
    while (first != cpos) {
        if ((w[i] & 0xFF00) == 0xD800) {
            i += getChar(w, i, c);
            if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
                j = getChar(w, i, c);
                while (handlingutf8->c_is_emojicomp(c)) {
                    i += j;
                    j = getChar(w, i, c);
                }
            }
        }
        else
            i++;
        first++;
    }
    return i;
}


void convertpostochar(wstring& w, vector<long>& vspos) {
    long first = 0;
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), w.size(), first))
        return;
#endif

    vector<long> vcpos;
    long realpos = first;
    long charpos = first;
    TRANSCHAR c;
    long j;
    for (long i = 0; i < vspos.size(); i++) {
        if (vspos[i] <= first) {
            vcpos.push_back(vspos[i]);
            continue;
        }
        
        while (realpos != vspos[i]) {
            if ((w[realpos] & 0xFF00) == 0xD800) {
                realpos += getChar(w, realpos, c);
                if (((c & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(c)) {
                    j = getChar(w, realpos, c);
                    while (handlingutf8->c_is_emojicomp(c)) {
                        realpos += j;
                        j = getChar(w, realpos, c);
                    }
                }
            }
            else
                realpos++;
            
            charpos++;
        }
        vcpos.push_back(charpos);
    }
    vspos = vcpos;
}

Exporting void sc_unicode_to_utf8(string& s, wstring& str) {
	long i = 0;
	char inter[5];
	long ineo = 0;
	long sz = str.size();
	if (!sz)
		return;
	long szo = 1 + (sz << 1);
	char* neo = new char[szo];
	neo[0] = 0;
	long nb;
	TRANSCHAR c;

	while (i < sz) {
		if (str[i] < 0x0080 && ineo < szo - 1) {
			neo[ineo++] = (char)str[i];
			i++;
			continue;
		}

		if (c_utf16_to_unicode(c, str[i], false))
			c_utf16_to_unicode(c, str[++i], true);

		nb = c_unicode_to_utf8(c, (uchar*)inter);
		neo = concatstrings(neo, inter, ineo, szo, nb);
		i++;
	}

	neo[ineo] = 0;
	s = neo;
	delete[] neo;
}

Exporting void sc_utf8_to_unicode(wstring& w, unsigned char* str, long sz) {
	if (!sz)
		return;

    long ineo = 0;
    wchar_t* neo = new wchar_t[sz + 1];
    neo[0] = 0;

#ifdef INTELINTRINSICS
	long i;
	if (check_ascii(str, sz, i)) {
		for (i = 0; i < sz; i++)
			neo[ineo++] = (wchar_t)str[i];
        neo[ineo] = 0;
        w = neo;
        delete[] neo;
		return;
	}
#endif
	TRANSCHAR c, c16;

	uchar nb;
	while (sz--) {
		if (*str & 0x80) {
			nb = c_utf8_to_unicode(str, c);
			str += nb + 1;
			sz -= nb;
			if (!(c & 0xFFFF0000)) {
				neo[ineo++] = (wchar_t)c;
				continue;
			}

			c_unicode_to_utf16(c16, c);
			neo[ineo++] = (wchar_t)(c16 >> 16);
			neo[ineo++] = (wchar_t)(c16 & 0xFFFF);
			continue;
		}
        
		neo[ineo++] = (wchar_t)*str;
		++str;
	}
    
    neo[ineo] = 0;
    w = neo;
    delete[] neo;
}

Exporting void s_latin_to_unicode(wstring& res, unsigned char* contenu, long sz) {
	if (!sz)
		return;

    long ineo = 0;
    wchar_t* neo = new wchar_t[sz+1];
    neo[0] = 0;
    
#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(contenu, sz, i)) {
        for (i = 0; i< sz; i++)
            neo[ineo++]= (wchar_t)contenu[i];
        neo[ineo] = 0;
        res += neo;
        delete[] neo;
        return;
    }
#endif
    
    TRANSCHAR code, c16;
    uchar nb;
    
    while (sz--) {
        if (*contenu & 0x80) {
                //it could be a utf8 character...
            nb = c_utf8_to_unicode(contenu, code);
            if (!(code & 0xFFFF0000))
                neo[ineo++] = (wchar_t)code;
            else {
                c_unicode_to_utf16(c16, code);
                neo[ineo++] = (wchar_t)(c16 >> 16);
                neo[ineo++] = (wchar_t)(c16 & 0xFFFF);
            }
            if (nb) {
                contenu += nb + 1;
                sz -= nb;
#ifdef INTELINTRINSICS
                if (check_ascii(contenu, sz, i)) {
                    for (i = 0; i< sz; i++)
                        neo[ineo++] = (wchar_t)contenu[i];
                    neo[ineo] = 0;
                    res += neo;
                    delete[] neo;
                    return;
                }
#endif
            }
            
            continue;
        }
        
        neo[ineo++] = (wchar_t)*contenu;
        ++contenu;
    }
    
    neo[ineo] = 0;
    res += neo;
    delete[] neo;
}

Exporting void sc_latin_to_unicode(wstring& res, unsigned char* contenu, long sz) {
	if (!sz)
		return;

    long ineo = 0;
    wchar_t* neo = new wchar_t[sz+1];
    neo[0] = 0;

#ifdef INTELINTRINSICS
	long i;
	if (check_ascii(contenu, sz, i)) {
		for (i = 0; i< sz; i++)
			neo[ineo++] = (wchar_t)contenu[i];
        neo[ineo] = 0;
        res= neo;
        delete[] neo;
		return;
	}
#endif

	TRANSCHAR code, c16;
	uchar nb;

	while (sz--) {
		if (*contenu & 0x80) {
			//it could be a utf8 character...
			nb = c_utf8_to_unicode(contenu, code);
			if (!(code & 0xFFFF0000))
				neo[ineo++] = (wchar_t)code;
			else {
				c_unicode_to_utf16(c16, code);
				neo[ineo++] = (wchar_t)(c16 >> 16);
				neo[ineo++] = (wchar_t)(c16 & 0xFFFF);
			}
			if (nb) {
				contenu += nb + 1;
				sz -= nb;
#ifdef INTELINTRINSICS
				if (check_ascii(contenu, sz, i)) {
					for (i = 0; i< sz; i++)
						neo[ineo++] = (wchar_t)contenu[i];
                    neo[ineo] = 0;
                    res= neo;
                    delete[] neo;
					return;
				}
#endif
			}

			continue;
		}
        
		neo[ineo++] = (wchar_t)*contenu;
		++contenu;
	}
    
    neo[ineo] = 0;
    res= neo;
    delete[] neo;
}

#else

Exporting long convertchartopos(wstring& w, long first, long cpos) {
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), w.size(), first))
        return cpos;
#endif
    
    if (cpos <= first)
        return cpos;

    long realpos = first;
    while (first != cpos) {
        if (((w[realpos] & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(w[realpos])) {
            realpos++;
            while (handlingutf8->c_is_emojicomp(w[realpos])) {
                realpos++;
            }
        }
        else
            realpos++;
        first++;
    }
    return realpos;
}

Exporting long convertpostochar(wstring& w, long first, long spos) {
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), w.size(), first))
        return spos;
#endif

    if (spos <= first)
        return spos;
    
    long realpos = first;
    while (realpos != spos) {
        if (((w[realpos] & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(w[realpos])) {
            realpos++;
            while (handlingutf8->c_is_emojicomp(w[realpos])) {
                realpos++;
            }
        }
        else
            realpos++;
        first++;
    }
    return first;
}

Exporting long convertchartoposraw(wstring& w, long first, long cpos) {
    long realpos = first;
    while (first != cpos) {
        if (((w[realpos] & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(w[realpos])) {
            realpos++;
            while (handlingutf8->c_is_emojicomp(w[realpos])) {
                realpos++;
            }
        }
        else
            realpos++;
        first++;
    }
    return realpos;
}

Exporting long convertpostocharraw(wstring& w, long first, long spos) {
    long realpos = first;
    while (realpos != spos) {
        if (((w[realpos] & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(w[realpos])) {
            realpos++;
            while (handlingutf8->c_is_emojicomp(w[realpos])) {
                realpos++;
            }
        }
        else
            realpos++;
        first++;
    }
    return first;
}


void convertpostochar(wstring& w, vector<long>& vspos) {
    long first = 0;
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), w.size(), first))
        return;
#endif

    vector<long> vcpos;
    long realpos = first;
    long charpos = first;
    for (long i = 0; i < vspos.size(); i++) {
        if (vspos[i] <= first) {
            vcpos.push_back(vspos[i]);
            continue;
        }
        
        while (realpos != vspos[i]) {
            if (((w[realpos] & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(w[realpos])) {
                realpos++;
                while (handlingutf8->c_is_emojicomp(w[realpos])) {
                    realpos++;
                }
            }
            else
                realpos++;
            charpos++;
        }
        vcpos.push_back(charpos);
    }
    vspos = vcpos;
}


//We transform a character position into a position within w
Exporting long c_char_to_pos_emoji(wstring& w, long charpos) {
    long i = 0;
#ifdef INTELINTRINSICS
        //we check if we have any large characters between 0 and ipos
    if (!check_large_char(WSTR(w), charpos, i))
        return charpos;
#endif
    
        //there is one in the block starting at i...
    charpos -= i;
    while (charpos > 0) {
        if (((w[i] & 0x1F000) == 0x1F000) && handlingutf8->c_is_emoji(w[i])) {
            i++;
            while (handlingutf8->c_is_emojicomp(w[i]))
                i++;
        }
        else
            i++;
        charpos--;
    }
    return i;
}

Exporting void sc_latin_to_unicode(wstring& res, unsigned char* contenu, long sz) {
	if (!sz)
		return;

    long ineo = 0;
    wchar_t* neo = new wchar_t[sz+1];
    neo[0] = 0;

#ifdef INTELINTRINSICS
	long i;
	if (check_ascii(contenu,sz,i)) {
		for (i=0; i< sz; i++)
			neo[ineo++]=(wchar_t)contenu[i];
        neo[ineo] = 0;
        res = neo;
        delete[] neo;
		return;
	}
#endif

	wchar_t code;
	uchar nb;

	while (sz--) {
		if (*contenu & 0x80) {
			//it could be a utf8 character...
			nb = c_utf8_to_unicode(contenu, code);
			neo[ineo++] = (wchar_t)code;
			if (nb) {
				contenu += nb+1;
				sz-=nb;
#ifdef INTELINTRINSICS
				if (check_ascii(contenu,sz,i)) {
					for (i=0; i< sz; i++)
						neo[ineo++]=(wchar_t)contenu[i];
                    neo[ineo] = 0;
                    res = neo;
                    delete[] neo;
					return;
				}
#endif
			}

			continue;
		}
		neo[ineo++]= (wchar_t)*contenu;
		++contenu;
	}
    
    neo[ineo] = 0;
    res = neo;
    delete[] neo;
}


Exporting void sc_unicode_to_utf8(string& s, wstring& str) {
	long i = 0;
	char inter[5];
    long ineo = 0;
    long sz = str.size();
	if (!sz)
		return;
    long szo = 1 + (sz << 1);
    char* neo = new char[szo];
    neo[0] = 0;
    long nb;

    while (i < sz) {
        if (str[i] < 0x0080 && ineo < szo-1) {
            neo[ineo++] = (char)str[i];
            i++;
            continue;
        }
        
        nb = c_unicode_to_utf8(str[i], (uchar*)inter);
        neo = concatstrings(neo,inter,ineo, szo, nb);
        i++;
    }
    
    neo[ineo] = 0;
    s = neo;
    delete[] neo;
}

Exporting void s_unicode_to_utf8(string& s, wchar_t* str, long sz) {
	if (!sz)
		return;
    long i = 0;
    char inter[5];
    long ineo = 0;
    long szo = 1 + (sz << 1);
    char* neo = new char[szo];
    neo[0] = 0;
    long nb;

    while (i < sz) {
        if (str[i] < 0x0080 && ineo < szo-1) {
            neo[ineo++] = (char)str[i];
            i++;
            continue;
        }
        
        nb = c_unicode_to_utf8(str[i], (uchar*)inter);
        neo = concatstrings(neo,inter,ineo, szo, nb);
        i++;
    }
    
    neo[ineo] = 0;
    s += neo;
    delete[] neo;
}


Exporting void sc_utf8_to_unicode(wstring& w, unsigned char* str, long sz) {
	if (!sz)
		return;

	w = L"";
    long ineo = 0;
    wchar_t* neo = new wchar_t[sz+1];
    neo[0] = 0;

#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(str,sz,i)) {
        for (i=0; i < sz; i++)
            neo[ineo++] = (wchar_t)str[i];
        neo[ineo] = 0;
        w = neo;
        delete[] neo;
        return;
    }
#endif
    wchar_t c;

    uchar nb;
    while (sz--) {
        if (*str & 0x80) {
            nb = c_utf8_to_unicode(str, c);
            str += nb+1;
            sz-=nb;
            neo[ineo++] = c;
            continue;
        }
        neo[ineo++] = (wchar_t)*str;
        ++str;
    }
    neo[ineo] = 0;
    w = neo;
    delete[] neo;
}
#endif

Exporting void s_doubleutf8_to_unicode(wstring& s, wchar_t* str, long l) {
    string sutf8;
    for (long i = 0; i < l; i++) {
        char lc = str[i] & 0xFF;
        char lr = (str[i] & 0xFF00) >> 8;
        sutf8 += lc;
        sutf8 += lr;
    }
    s_utf8_to_unicode(s, USTR(sutf8), sutf8.size());
}

Exporting long conversion_utf8_to_fat(unsigned char* contenu, vector<string>& l) {
    long firstmul = -1;
    long lg = strlen((char*)contenu);
    l.reserve(lg);
    string res;
    for (long i = 0; i<lg; i++) {
        res = c_char_get(contenu, i);
        if (firstmul == -1 && res.size()>1)
            firstmul = res.size();
        l.push_back(res);
    }
    return firstmul;
}

Exporting unsigned char conversion_utf8_to_latin(short x) {
    if (x >= 32)
        return x;
    return 32;
}

Exporting void conversion_utf8_to_fatOne(unsigned char* contenu, string& s) {
    long i = 0;
    s = c_char_get(contenu, i);
}


Exporting long conversion_utf8_to_fat(string contenu, vector<string>& l) {
    return conversion_utf8_to_fat((unsigned char*)contenu.c_str(), l);
}

Exporting void conversion_utf8_to_fatOne(string contenu, string& s) {
    conversion_utf8_to_fatOne((unsigned char*)contenu.c_str(), s);
}

Exporting string s_utf8_to_latin(unsigned char* contenu, long sz) {
    string res;
    
#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(contenu,sz,i))
        return (char*)contenu;
    //otherwise, it is in ASCII up to i
    uchar c;
    if (i) {
        c=contenu[i];
        contenu[i]=0;
        res=(char*)contenu;
        contenu[i]=c;
        contenu+=i;
        sz -= i;
    }
#endif
    
    uchar nb;
    TRANSCHAR code;
    
    while (sz--) {
        
        if (*contenu & 0x80) {
            nb = c_utf8_to_unicode(contenu, code);
            switch(code) {
                case 338:
                    res+="OE";
                    break;
                case 339:
                    res+="oe";
                    break;
                default:
                    if (codeslatin.check(code))
                        res += codeslatin[code];
                    else
                        res += ".";
            }
            
            contenu += nb+1;
            sz -= nb;
#ifdef INTELINTRINSICS
            if (check_ascii(contenu,sz,i)) {
                res+=(char*)contenu;
                return res;
            }
            if (i) {
                c=contenu[i];
                contenu[i]=0;
                res += (char*)contenu;
                contenu[i]=c;
                contenu+=i;
                sz -= i;
            }
#endif
            continue;
        }
        
        res+=*contenu;
        ++contenu;
    }
    return res;
}

Exporting string conversion_unicode_to_latin(wstring& w) {
    string res;
    long sz = w.size();
	TRANSCHAR code;
    for (long i = 0; i < sz; i++) {
        code = w[i];
        if (code < 0x80)
            res += (char)w[i];
        else {
            switch(code) {
                case 338:
                    res+="OE";
                    break;
                case 339:
                    res+="oe";
                    break;
                default:
                    if (codeslatin.check(code))
                        res += codeslatin[code];
                    else
                        res += ".";
            }
        }
    }
    return res;
}

string conversion_latin_to_utf8(unsigned char* contenu, long sz) {
    string res;
    s_latin_to_utf8(res, contenu, sz);
    return res;
}

Exporting string conversion_latin_table_to_utf8(short tableindex, unsigned char* contenu, long sz) {
    string res;
    
    if (!tableindex || tableindex == 1) {
        s_latin_to_utf8(res, contenu, sz);
        return res;
    }
    
    if (!alllatincodes.check(tableindex))
        return "";
    
#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(contenu,sz,i))
        return (char*)contenu;
#endif
    
    uchar ch[5];
    uchar nb;
    
    
	basebin_hash<TRANSCHAR>& thetable = *alllatincodes[tableindex];
    while (sz--) {
        if (*contenu & 0x80) {
            if (thetable.check(*contenu)) {
                nb = c_unicode_to_utf8(thetable[*contenu], ch);
                res += (char*)ch;
                ++contenu;
                continue;
            }
            
            //it could be a utf8 character...
            nb = c_latin_to_utf8(contenu, ch, true);
            res += (char*)ch;
            contenu += nb--;
            sz -= nb;
            continue;
        }
        
        res += *contenu;
        ++contenu;
    }
    return res;
}

Exporting wstring conversion_latin_table_to_unicode(short tableindex, unsigned char* contenu, long sz) {
    wstring res;
    
    if (!tableindex || tableindex == 1) {
        sc_latin_to_unicode(res,contenu,sz);
        return res;
    }
    
    if (!alllatincodes.check(tableindex))
        return L"";
    
	basebin_hash<TRANSCHAR>& thetable = *alllatincodes[tableindex];
    uchar nb;
    
#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(contenu,sz,i)) {
        for (i = 0; i < sz; i++)
            res += (wchar_t)contenu[i];
        return res;
    }
#endif
    
    while (sz--) {
        if (*contenu & 0x80) {
            if (thetable.check(*contenu)) {
                res += thetable[*contenu];
                ++contenu;
                continue;
            }

			nb = concatenate_latin_unicode(res, contenu, true);
            contenu += nb--;
            sz -= nb;
            continue;
        }
        
        res += (wchar_t)*contenu;
        ++contenu;
    }
    
    return res;
}

Exporting void s_latin_to_utf8(string& res, unsigned char* contenu, long sz) {
    res = "";
#ifdef INTELINTRINSICS
    long i;
    if (check_ascii(contenu,sz,i)) {
        res = (char*)contenu;
        return;
    }
    
    //otherwise, it is in ASCII up to i
    uchar c;
    if (i) {
        c=contenu[i];
        contenu[i]=0;
        res=(char*)contenu;
        contenu[i]=c;
        contenu+=i;
        sz -= i;
    }
#endif
    uchar ch[5];
    uchar nb;
    
    while (sz--) {
        if (*contenu & 0x80) {
            //it could be a utf8 character...
            nb = c_latin_to_utf8(contenu, ch);
            res += (char*)ch;
            contenu += nb--;
            sz-=nb;
            
#ifdef INTELINTRINSICS
            if (check_ascii(contenu,sz,i)) {
                res += (char*)contenu;
                return;
            }
            
            if (i) {
                c=contenu[i];
                contenu[i]=0;
                res += (char*)contenu;
                contenu[i]=c;
                contenu+=i;
                sz -= i;
            }
#endif
            continue;
        }
        res += *contenu;
        ++contenu;
    }
}

Exporting string conversion_utf8_to_latin(string contenu) {
    return s_utf8_to_latin(USTR(contenu), contenu.size());
}

Exporting string conversion_latin_to_utf8(string contenu) {
    return conversion_latin_to_utf8(USTR(contenu), contenu.size());
}

Exporting string s_replacestring(string& s, string& reg, string& rep) {
    string neo;

#ifdef INTELINTRINSICS
    if (!replace_intel_all(neo, s, reg, rep))
        return s;
#else
    long gsz = reg.size();
    if (!gsz)
        return s;

    long rsz = s.size();
    long from = 0;

    long foundHere;
    while ((foundHere = s.find(reg, from)) != string::npos) {
        if (foundHere != from)
            neo += s.substr(from, foundHere - from);
        neo += rep;
        from = foundHere + gsz;
    }
    if (from < rsz)
        neo += s.substr(from, rsz - from);
#endif
    return neo;
}

Exporting string s_replacestrings(string& s, string reg, string rep) {
    string neo;
    
#ifdef INTELINTRINSICS
    if (!replace_intel_all(neo, s, reg, rep))
        return s;
#else
    long gsz = reg.size();
    if (!gsz)
        return s;
    
    long rsz = s.size();
    long from = 0;
    
    long foundHere;
    while ((foundHere = s.find(reg, from)) != string::npos) {
        if (foundHere != from)
            neo += s.substr(from, foundHere - from);
        neo += rep;
        from = foundHere + gsz;
    }
    if (from < rsz)
        neo += s.substr(from, rsz - from);
#endif
    return neo;
}

Exporting wstring s_replacestring(wstring& s, wstring reg, wstring rep) {
    wstring neo;
    
#ifdef INTELINTRINSICS
    if (!replace_intel_all(neo, s, reg, rep))
        return s;
#else
    long gsz = reg.size();
    if (!gsz)
        return s;
    
    long rsz = s.size();
    long from = 0;
    
    long foundHere;
    while ((foundHere = s.find(reg, from)) != string::npos) {
        if (foundHere != from)
            neo += s.substr(from, foundHere - from);
        neo += rep;
        from = foundHere + gsz;
    }
    if (from < rsz)
        neo += s.substr(from, rsz - from);
#endif
    return neo;
}

Exporting long c_char_next(unsigned char* m, long& i) {
    
    long nb = c_test_utf8(m + i);
    i += nb;
    return (nb + 1);
}

Exporting long c_code_get(unsigned char* m, long& i, TRANSCHAR& code) {
    long nb = c_utf8_to_unicode(m + i, code);
    i += nb;
    return nb;
}

Exporting string c_char_get_next(unsigned char* m, size_t& i) {
    long nb = c_test_utf8(m + i);
    char str[] = { (char)m[i], 0, 0, 0, 0, 0, 0, 0 };
    
    if (nb == 0) {
        i++;
        return str;
    }
    
    char cc = m[i + nb + 1];
    m[i + nb + 1] = 0;
    strcpy_s(str, 8, (char*)(m + i));
    m[i + nb + 1] = cc;
    i += nb + 1;
    return str;
}


Exporting string c_char_get(unsigned char* m, long& i) {
    char str[] = {(char)m[i],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    long nb = c_test_utf8(m + i);
    if (nb == 0)
        return str;

    long idx = 1;
    long j = i;
    if (nb == 3 && handlingutf8->c_is_emoji(m, j)) {
        str[idx++] = (char)m[i+1];
        str[idx++] = (char)m[i+2];
        str[idx++] = (char)m[i+3];
        i = ++j;
        while (handlingutf8->c_is_emojicomp(m, j)) {
            nb = c_test_utf8(m+i);
            str[idx++] = (char)m[i];
            switch (nb) {
                case 1:
                    str[idx++] = (char)m[i+1];
                    break;
                case 2:
                    str[idx++] = (char)m[i+1];
                    str[idx++] = (char)m[i+2];
                    break;
                case 3:
                    str[idx++] = (char)m[i+1];
                    str[idx++] = (char)m[i+2];
                    str[idx++] = (char)m[i+3];
            }
            i = ++j;
        }
        --i;
    }
    else {
        switch (nb) {
            case 1:
                str[idx] = (char)m[i+1];
                break;
            case 2:
                str[idx++] = (char)m[i+1];
                str[idx] = (char)m[i+2];
                break;
            case 3:
                str[idx++] = (char)m[i+1];
                str[idx++] = (char)m[i+2];
                str[idx] = (char)m[i+3];
        }
        i += nb;
    }
    return str;
}

Exporting TRANSCHAR c_char_get_wide(unsigned char* m, long& i) {
	TRANSCHAR code;
    i += c_utf8_to_unicode(m + i, code);
    return code;
}


//i is character position



Exporting bool c_char_index_assign(string& s, string c, long x) {
    if (x > s.size())
        return false;
    long i;
    uchar* contenu = USTR(s);
    long lg = s.size();
    for (i = 0; i<lg && x>0; i++) {
        i += c_test_utf8(contenu + i);
        x--;
    }
    if (i == lg)
        s += c;
    else {
        x = i;
        s.erase(i, c_test_utf8(contenu + x) + 1);
        s.insert(i, c);
    }
    return true;
}


//i is a character position
Exporting string c_char_index_remove(string& s, size_t i) {
    long lg = s.size();

    string res;
    
    unsigned char* content = USTR(s);
    
    long x = getindex(content, lg, i);
    if (x == i) {
        res = s[i];
        s.erase(i, 1);
        return res;
    }

    lg = x;
    res = c_char_get(content, lg);
    s.erase(x, res.size());
    return res;
}


Exporting string c_char_index(string& s, size_t i) {
    long lg = s.size();
    long x = getindex(USTR(s), lg, i);
    if (x >= lg)
        return "";    
    return c_char_get(USTR(s), x);
}

Exporting string s_char_reverse(string& s, long& length) {
    long lg = s.size();
    string res = "";
    string x;
    length = 0;
    for (long i = 0; i < lg; i++) {
        x = c_char_get(USTR(s), i);
        res.insert(0, x);
        length++;
    }
    return res;
}

Exporting string s_revert(string& s) {
    long lg = s.size();
    string res = "";
    string x;
    for (long i = 0; i < lg; i++) {
        x = c_char_get(USTR(s), i);
        res.insert(0, x);
    }
    return res;
}

Exporting TRANSCHAR c_char_index_code(string& s, size_t i) {
	long lg = s.size();
    uchar* contenu = USTR(s);
    long x;
    for (x = 0; x<lg && i>0; x++) {
        x += c_test_utf8(contenu + x);
        i--;
    }
    TRANSCHAR c;
    c_utf8_to_unicode(contenu + x, c);
    return c;
}

Exporting void c_char_index_code_all(string& s, vector<long>& vect) {
    long lg = s.size();
    uchar* contenu = USTR(s);
    vect.reserve(lg);
	TRANSCHAR code;
    for (long i = 0; i < lg; i++) {
        i += c_utf8_to_unicode(contenu + i, code);
        vect.push_back((long)code);
    }
}

Exporting void c_char_index_code_all_long(string& s, vector<TRANSCHAR>& vect) {
    size_t lg = s.size();
    uchar* contenu = USTR(s);
    vect.reserve(lg);
	TRANSCHAR code;
    for (size_t i = 0; i < lg; i++) {
        i += c_utf8_to_unicode(contenu + i, code);
        vect.push_back(code);
    }
}


//On renvoie le code LATIN correspondant, 0 sinon
Exporting unsigned char c_utf8_latin(string s) {
	TRANSCHAR v;
    c_utf8_to_unicode(USTR(s), v);
    if (v > 255)
        return 0;
    return (uchar)v;
}

//On renvoie la chaine UTF8 correspondante, 0 sinon
string c_latin_utf8(int c) {
    bulongchar xs;
    c_latin_to_utf8(c, xs.ustr);
    return xs.str;
}

Exporting bool s_is_space(string& str) {
    static unsigned char spaces[] = { 9, 10, 13, 32, 160 };
    long lg = str.size();
    uchar* contenu = USTR(str);
	TRANSCHAR code;
    for (long i = 0; i < lg; i++) {
        i += c_utf8_to_unicode(contenu + i, code);
        if ((code <= 160 && !strchr((char*)spaces, (char)code)) && code != 0x202F && code != 0x3000)
            return false;
    }
    return true;
}


//---------------------------------------------------------------------

Exporting void v_convertbytetocharposition(unsigned char* s, vector<long>& v) {
    long i = 0;
    long c = 0;
    long p = 0;
    long nb;

    while (p < v.size()) {
        while (i < v[p]) {
            nb = c_test_utf8(s + i);
            if (nb == 3 && handlingutf8->c_is_emoji(s,i)) {
                i++;
                while (handlingutf8->c_is_emojicomp(s, i)) {
                    i++;
                }
            }
            else
                i += nb + 1;
            c++;
        }
        v[p++] = c;
    }
}

Exporting void v_convertchartobyteposition(unsigned char* contenu, vector<long>& v) {
    long c = 0;
    long iv = 0;
    long nb;
    long i = 0;
    long sz = v.size();
    while(iv < sz) {
        while (v[iv] == i)
            v[iv++] = c;

        nb = c_test_utf8(contenu + c);
        if (nb == 3 && handlingutf8->c_is_emoji(contenu,c)) {
            c++;
            while (handlingutf8->c_is_emojicomp(contenu, c)) {
                c++;
            }
        }
        else
            c += nb + 1;
        i++;
    }
}

string& Trim(string& chaine) {
    long d, f;
    for (d = 0; d<chaine.size(); d++) {
        if ((uchar)chaine[d]>32)
            break;
    }
    
    for (f = chaine.size() - 1; f >= 0; f--) {
        if ((uchar)chaine[f] > 32)
            break;
    }
    long lg = f - d + 1;
    if (lg >= 1)
        chaine = chaine.substr(d, lg);
    else
        chaine = "";
    return chaine;
}

#ifdef WIN32
#define wset(x, y) x[0] = y[0]; x[1] = y[1];
#define waddtoken(tok, c, itok) tok[itok++] = c[0]; if (c[1]) tok[itok++] =  c[1]; tok[itok] = 0
#else
#define wset(x, y) x[0] = y[0]
#define waddtoken(tok, c, itok) tok[itok++] = c[0]; tok[itok] = 0
#endif

char x_wreading::loop(wstring& toparse, short i, wchar_t* token, wchar_t* chr, long& itoken, short& r, long& l, long& posc) {
    long sz;
    short type;
    
    vector<short>& element = ruleelements[i];
    short* closed = closing[i];
    vector<wstring>& rule = tokenizer[i];

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
        
        wstring& label = rule[r];
        
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
                            wchar_t cc[] = {0,0,0};
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

void x_wreading::apply(wstring& toparse, bool keepos, vector<wstring>* vstack, vector<unsigned char>* vtype) {
    wchar_t chr[] = {0,0,0};
    wchar_t currentchr[] = {0,0,0};

    long wsz=toparse.size();

    wchar_t* token =  new wchar_t[wsz+1];

    long itoken = 0;
    long line=0,i, l;
    short r;
    long pos=0, posc;
    long sztokenizer;
    
    short ty;
    
    bool getit=false;
    char found=true;
    bool storetype=true;


    if (!loaded) {
        setrules();
        parserules();
        loaded=true;
    }
    
    if (vstack==NULL)
        vstack=&stack;
    
    if (vtype==NULL) {
        vtype=&stacktype;
        storetype=false;
    }

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
                        
                        if (!juststack) {
                            stackln.push_back(line);
                            vtype->push_back(ty);
                            if (keepos) {
                                cpos.push_back(pos-1);
                            }
                        }
                        else
                            if (storetype)
                                vtype->push_back(ty);

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
                if (!juststack) {
                    stackln.push_back(line);
                    vtype->push_back(ty);
                    if (keepos) {
                        cpos.push_back(pos-1);
                    }
                }
                else
                    if (storetype)
                        vtype->push_back(ty);
            }
            getit=true;
            wset(currentchr,chr);
            line=l;
            pos=posc;
            break;
        }
        
        if (!getit) { //Character not taken into account by a rule, we suppose it is a simple UTF8 character...
            vstack->push_back(currentchr);
            stackln.push_back(line);
            vtype->push_back(255);
            getnext(toparse,currentchr, pos,l);
        }
    }
    delete[] token;
}


Exporting TRANSCHAR c_to_upper(TRANSCHAR c) {
    return handlingutf8->c_to_upper(c);
}

Exporting TRANSCHAR c_to_lower(TRANSCHAR c) {
    return handlingutf8->c_to_lower(c);
}

Exporting bool c_is_upper(TRANSCHAR c) {
    return handlingutf8->c_is_upper(c);
}

Exporting char c_is_alpha(TRANSCHAR c) {
    return handlingutf8->c_is_alpha(c);
}

Exporting bool c_is_lower(TRANSCHAR c) {
    return handlingutf8->c_is_lower(c);
}

Exporting bool c_is_consonant(TRANSCHAR c) {
    return handlingutf8->c_is_consonant(c);
}

Exporting bool c_is_vowel(TRANSCHAR c) {
    return handlingutf8->c_is_vowel(c);
}

Exporting bool compare_vowel(TRANSCHAR c, TRANSCHAR cc) {
    return handlingutf8->compare_vowel(c, cc);
}

Exporting bool c_is_punctuation(TRANSCHAR c) {
    return handlingutf8->c_is_punctuation(c);
}
