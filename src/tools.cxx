/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  tools.cxx
//

#ifdef WIN32
#include "Windows.h"
#endif

#include <stdio.h>
#include "tools.h"
#include "lispe.h"
#include "emojis.h"

#ifdef WIN32
#include <io.h>
#else
#include <dlfcn.h>
#define sprintf_s snprintf
#define swprintf_s swprintf
#endif

/*
 Some utility functions to manipulate strings
 Essentially:
 
 numeric conversions (including hexadecimal)
 utf8/unicode conversion
 utf8 string traversal (taking into account multi-byte characters)
 code indentation (no more only lisp by the way)
 upper case, lower case
 string trimming
 sub-string extraction: left, right, middle
 */

#define strcpy_s(a,c,b) strncpy(a,b,c)
#define strcat_s(a,c,b) strncat(a,b,c)

long bitcounter(uint_fast64_t x) {
    long nb = 0;
    while (x) {
        if (x & 1) nb++;
        x >>= 1;
    }
    return nb;
}

#define mini(x,y) x < y?x:y

static char digitaction[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,
    '0','0','0','0','0','0','0','0','0','0',0,0,0,0,0,0,0,
    'X','X','X','X','X','X',
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    'x','x','x','x','x','x'};

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


static inline double power10(long n)
{
    if (n > 0)
    {
        long n1(n & 0x1f);   //n1 modulo 32
        long n2(n & (~0x1f));//n2 div 32
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
        long n1(n & 0x1f);   //n1 modulo 32
        long n2(n & (~0x1f));//n2 div 32
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

//------------------------------------------------------------------------
#define isadigit(c) (c >= '0' && c <= '9')
#define uchar unsigned char

//The actual size of the displayed string, the problem here is that multibyte characters are sometimes displayed with an extra-space...
//Especially for CJK characters.... (Chinese, Japanese, Korean)... We need to integrate this extra-space into our calculus...

//Lines extracted from the function "mk_wcwidth": https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
bool ckjchar(wchar_t ucs) {
    return
    (ucs >= 0x1100 &&
     (ucs <= 0x115f ||                    /* Hangul Jamo init. consonants */
      ucs == 0x2329 || ucs == 0x232a ||
      (ucs >= 0x2e80 && ucs <= 0xa4cf &&
       ucs != 0x303f) ||                  /* CJK ... Yi */
      (ucs >= 0xac00 && ucs <= 0xd7a3) || /* Hangul Syllables */
      (ucs >= 0xf900 && ucs <= 0xfaff) || /* CJK Compatibility Ideographs */
      (ucs >= 0xfe10 && ucs <= 0xfe19) || /* Vertical forms */
      (ucs >= 0xfe30 && ucs <= 0xfe6f) || /* CJK Compatibility Forms */
      (ucs >= 0xff00 && ucs <= 0xff60) || /* Fullwidth Forms */
      (ucs >= 0xffe0 && ucs <= 0xffe6) ||
      (ucs >= 0x20000 && ucs <= 0x2fffd) ||
      (ucs >= 0x30000 && ucs <= 0x3fffd)));
}

wstring convertToWString(long d) {
    wchar_t buff[20];
    swprintf_s(buff, 20, L"%ld", d);
    return buff;
}

string convertToString(long d) {
    char buff[20];
    sprintf_s(buff, 20, "%ld", d);
    return buff;
}

u_ustring convertToUString(long d) {
    return w_to_u(convertToWString(d));
}

wstring convertToWString(double d) {
    wchar_t buff[20];
    swprintf_s(buff, 20, L"%g", d);
    return buff;
}

string convertToString(double d) {
    char buff[20];
    sprintf_s(buff, 20, "%g", d);
    return buff;
}

u_ustring convertToUString(double d) {
    return w_to_u(convertToWString(d));
}

wstring convertToWString(float d) {
    wchar_t buff[20];
    swprintf_s(buff, 20, L"%g", d);
    return buff;
}

string convertToString(float d) {
    char buff[20];
    sprintf_s(buff, 20, "%g", d);
    return buff;
}

u_ustring convertToUString(float d) {
    return w_to_u(convertToWString(d));
}

//------------------------------------------------------------------------
static wchar_t ponctuations[] = {
    0x21, 0x22, 0x23, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D,
    0x2E, 0x2F, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x5B, 0x5C,
    0x5D, 0x5E, 0x60, 0x7B, 0x7C, 0x7D, 0x7E, 0x9C, 0xA0, 0xA1, 0xA2,
    0xA4, 0xA5, 0xA6, 0xAA, 0xAB, 0xAC, 0xAD,0xAF, 0xB0, 0xB1, 0xB5,
    0xB6, 0xB7, 0xB8, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, 0xD7, 0xF7,
    0x2BC, 0x2013, 0x2014, 0x2015, 0x2018, 0x2019, 0x201C, 0x201D, 0x2020,
    0x2021, 0x2022, 0x2026, 0x2032, 0x2033, 0x203B, 0x212E, 0x2190, 0x2191,
    0x2192, 0x2193, 0x2264, 1470, 1472, 1475, 1478, 1523, 1524, 0x2265,
    0x263A, 0x3008, 0x3009, 1548, 1549, 1550, 1551, 1567, 1645, 1757, 1758, 1769, 0xFD3E,
    0xFD3F, 0x3001, 0xFF0C, 0x2025, 0x2026, 0x3002, 0x303D, 0x300C,0x300D, 0x300E,0x300F,
    0x301D, 0x301F, 0x301C, 0xff1a, 0xff01, 0xff1f, 0x266a, 0x2234, 0};

static int32_t codingtable[] = { 65,97,2,66,98,2,67,99,2,68,100,2,69,101,2,70,102,2,71,103,2,72,104,2,73,105,2,74,106,2,75,107,2,76,
    108,2,77,109,2,78,110,2,79,111,2,80,112,2,81,113,2,82,114,2,83,115,2,84,116,2,85,117,2,86,118,2,87,119,
    2,88,120,2,89,121,2,90,122,2,97,65,1,98,66,1,99,67,1,100,68,1,101,69,1,102,70,1,103,71,1,104,72,1,
    105,73,1,106,74,1,107,75,1,108,76,1,109,77,1,110,78,1,111,79,1,112,80,1,113,81,1,114,82,1,115,83,1,116,
    84,1,117,85,1,118,86,1,119,87,1,120,88,1,121,89,1,122,90,1,170,170,1,181,924,1,186,186,1,192,224,2,193,225,
    2,194,226,2,195,227,2,196,228,2,197,229,2,198,230,2,199,231,2,200,232,2,201,233,2,202,234,2,203,235,2,204,236,2,
    205,237,2,206,238,2,207,239,2,208,240,2,209,241,2,210,242,2,211,243,2,212,244,2,213,245,2,214,246,2,216,248,2,217,
    249,2,218,250,2,219,251,2,220,252,2,221,253,2,222,254,2,223,223,1,224,192,1,225,193,1,226,194,1,227,195,1,228,196,
    1,229,197,1,230,198,1,231,199,1,232,200,1,233,201,1,234,202,1,235,203,1,236,204,1,237,205,1,238,206,1,239,207,1,
    240,208,1,241,209,1,242,210,1,243,211,1,244,212,1,245,213,1,246,214,1,248,216,1,249,217,1,250,218,1,251,219,1,252,
    220,1,253,221,1,254,222,1,255,376,1,256,257,2,257,256,1,258,259,2,259,258,1,260,261,2,261,260,1,262,263,2,263,262,
    1,264,265,2,265,264,1,266,267,2,267,266,1,268,269,2,269,268,1,270,271,2,271,270,1,272,273,2,273,272,1,274,275,2,
    275,274,1,276,277,2,277,276,1,278,279,2,279,278,1,280,281,2,281,280,1,282,283,2,283,282,1,284,285,2,285,284,1,286,
    287,2,287,286,1,288,289,2,289,288,1,290,291,2,291,290,1,292,293,2,293,292,1,294,295,2,295,294,1,296,297,2,297,296,
    1,298,299,2,299,298,1,300,301,2,301,300,1,302,303,2,303,302,1,304,105,2,305,73,1,306,307,2,307,306,1,308,309,2,
    309,308,1,310,311,2,311,310,1,312,312,1,313,314,2,314,313,1,315,316,2,316,315,1,317,318,2,318,317,1,319,320,2,320,
    319,1,321,322,2,322,321,1,323,324,2,324,323,1,325,326,2,326,325,1,327,328,2,328,327,1,329,329,1,330,331,2,331,330,
    1,332,333,2,333,332,1,334,335,2,335,334,1,336,337,2,337,336,1,338,339,2,339,338,1,340,341,2,341,340,1,342,343,2,
    343,342,1,344,345,2,345,344,1,346,347,2,347,346,1,348,349,2,349,348,1,350,351,2,351,350,1,352,353,2,353,352,1,354,
    355,2,355,354,1,356,357,2,357,356,1,358,359,2,359,358,1,360,361,2,361,360,1,362,363,2,363,362,1,364,365,2,365,364,
    1,366,367,2,367,366,1,368,369,2,369,368,1,370,371,2,371,370,1,372,373,2,373,372,1,374,375,2,375,374,1,376,255,2,
    377,378,2,378,377,1,379,380,2,380,379,1,381,382,2,382,381,1,383,83,1,384,579,1,385,595,2,386,387,2,387,386,1,388,
    389,2,389,388,1,390,596,2,391,392,2,392,391,1,393,598,2,394,599,2,395,396,2,396,395,1,397,397,1,398,477,2,399,601,
    2,400,603,2,401,402,2,402,401,1,403,608,2,404,611,2,405,502,1,406,617,2,407,616,2,408,409,2,409,408,1,410,573,1,
    411,411,1,412,623,2,413,626,2,414,544,1,415,629,2,416,417,2,417,416,1,418,419,2,419,418,1,420,421,2,421,420,1,422,
    640,2,423,424,2,424,423,1,425,643,2,426,426,1,427,427,1,428,429,2,429,428,1,430,648,2,431,432,2,432,431,1,433,650,
    2,434,651,2,435,436,2,436,435,1,437,438,2,438,437,1,439,658,2,440,441,2,441,440,1,442,442,1,443,443,0,444,445,2,
    445,444,1,446,446,1,447,503,1,448,448,0,449,449,0,450,450,0,451,451,0,452,454,2,453,453,0,454,452,1,455,457,2,456,
    456,0,457,455,1,458,460,2,459,459,0,460,458,1,461,462,2,462,461,1,463,464,2,464,463,1,465,466,2,466,465,1,467,468,
    2,468,467,1,469,470,2,470,469,1,471,472,2,472,471,1,473,474,2,474,473,1,475,476,2,476,475,1,477,398,1,478,479,2,
    479,478,1,480,481,2,481,480,1,482,483,2,483,482,1,484,485,2,485,484,1,486,487,2,487,486,1,488,489,2,489,488,1,490,
    491,2,491,490,1,492,493,2,493,492,1,494,495,2,495,494,1,496,496,1,497,499,2,498,498,0,499,497,1,500,501,2,501,500,
    1,502,405,2,503,447,2,504,505,2,505,504,1,506,507,2,507,506,1,508,509,2,509,508,1,510,511,2,511,510,1,512,513,2,
    513,512,1,514,515,2,515,514,1,516,517,2,517,516,1,518,519,2,519,518,1,520,521,2,521,520,1,522,523,2,523,522,1,524,
    525,2,525,524,1,526,527,2,527,526,1,528,529,2,529,528,1,530,531,2,531,530,1,532,533,2,533,532,1,534,535,2,535,534,
    1,536,537,2,537,536,1,538,539,2,539,538,1,540,541,2,541,540,1,542,543,2,543,542,1,544,414,2,545,545,1,546,547,2,
    547,546,1,548,549,2,549,548,1,550,551,2,551,550,1,552,553,2,553,552,1,554,555,2,555,554,1,556,557,2,557,556,1,558,
    559,2,559,558,1,560,561,2,561,560,1,562,563,2,563,562,1,564,564,1,565,565,1,566,566,1,567,567,1,568,568,1,569,569,
    1,570,11365,2,571,572,2,572,571,1,573,410,2,574,11366,2,575,11390,1,576,11391,1,577,578,2,578,577,1,579,384,2,580,649,2,
    581,652,2,582,583,2,583,582,1,584,585,2,585,584,1,586,587,2,587,586,1,588,589,2,589,588,1,590,591,2,591,590,1,592,
    11375,1,593,11373,1,594,11376,1,595,385,1,596,390,1,597,597,1,598,393,1,599,394,1,600,600,1,601,399,1,602,602,1,603,400,
    1,604,604,1,605,605,1,606,606,1,607,607,1,608,403,1,609,609,1,610,610,1,611,404,1,612,612,1,613,613,1,614,614,1,
    615,615,1,616,407,1,617,406,1,618,618,1,619,11362,1,620,620,1,621,621,1,622,622,1,623,412,1,624,624,1,625,11374,1,626,
    413,1,627,627,1,628,628,1,629,415,1,630,630,1,631,631,1,632,632,1,633,633,1,634,634,1,635,635,1,636,636,1,637,11364,
    1,638,638,1,639,639,1,640,422,1,641,641,1,642,642,1,643,425,1,644,644,1,645,645,1,646,646,1,647,647,1,648,430,1,
    649,580,1,650,433,1,651,434,1,652,581,1,653,653,1,654,654,1,655,655,1,656,656,1,657,657,1,658,439,1,659,659,1,660,
    660,0,661,661,1,662,662,1,663,663,1,664,664,1,665,665,1,666,666,1,667,667,1,668,668,1,669,669,1,670,670,1,671,671,
    1,672,672,1,673,673,1,674,674,1,675,675,1,676,676,1,677,677,1,678,678,1,679,679,1,680,680,1,681,681,1,682,682,1,
    683,683,1,684,684,1,685,685,1,686,686,1,687,687,1,688,688,0,689,689,0,690,690,0,691,691,0,692,692,0,693,693,0,694,
    694,0,695,695,0,696,696,0,697,697,0,698,698,0,699,699,0,700,700,0,701,701,0,702,702,0,703,703,0,704,704,0,705,705,
    0,710,710,0,711,711,0,712,712,0,713,713,0,714,714,0,715,715,0,716,716,0,717,717,0,718,718,0,719,719,0,720,720,0,
    721,721,0,736,736,0,737,737,0,738,738,0,739,739,0,740,740,0,748,748,0,750,750,0,880,881,2,881,880,1,882,883,2,883,
    882,1,884,884,0,886,887,2,887,886,1,890,890,0,891,1021,1,892,1022,1,893,1023,1,902,940,2,904,941,2,905,942,2,906,943,
    2,908,972,2,910,973,2,911,974,2,912,912,1,913,945,2,914,946,2,915,947,2,916,948,2,917,949,2,918,950,2,919,951,2,
    920,952,2,921,953,2,922,954,2,923,955,2,924,956,2,925,957,2,926,958,2,927,959,2,928,960,2,929,961,2,931,963,2,932,
    964,2,933,965,2,934,966,2,935,967,2,936,968,2,937,969,2,938,970,2,939,971,2,940,902,1,941,904,1,942,905,1,943,906,
    1,944,944,1,945,913,1,946,914,1,947,915,1,948,916,1,949,917,1,950,918,1,951,919,1,952,920,1,953,921,1,954,922,1,
    955,923,1,956,924,1,957,925,1,958,926,1,959,927,1,960,928,1,961,929,1,962,931,1,963,931,1,964,932,1,965,933,1,966,
    934,1,967,935,1,968,936,1,969,937,1,970,938,1,971,939,1,972,908,1,973,910,1,974,911,1,975,983,2,976,914,1,977,920,
    1,978,978,2,979,979,2,980,980,2,981,934,1,982,928,1,983,975,1,984,985,2,985,984,1,986,987,2,987,986,1,988,989,2,
    989,988,1,990,991,2,991,990,1,992,993,2,993,992,1,994,995,2,995,994,1,996,997,2,997,996,1,998,999,2,999,998,1,1000,
    1001,2,1001,1000,1,1002,1003,2,1003,1002,1,1004,1005,2,1005,1004,1,1006,1007,2,1007,1006,1,1008,922,1,1009,929,1,1010,1017,1,1011,1011,
    1,1012,952,2,1013,917,1,1015,1016,2,1016,1015,1,1017,1010,2,1018,1019,2,1019,1018,1,1020,1020,1,1021,891,2,1022,892,2,1023,893,2,
    1024,1104,2,1025,1105,2,1026,1106,2,1027,1107,2,1028,1108,2,1029,1109,2,1030,1110,2,1031,1111,2,1032,1112,2,1033,1113,2,1034,1114,2,1035,
    1115,2,1036,1116,2,1037,1117,2,1038,1118,2,1039,1119,2,1040,1072,2,1041,1073,2,1042,1074,2,1043,1075,2,1044,1076,2,1045,1077,2,1046,1078,
    2,1047,1079,2,1048,1080,2,1049,1081,2,1050,1082,2,1051,1083,2,1052,1084,2,1053,1085,2,1054,1086,2,1055,1087,2,1056,1088,2,1057,1089,2,
    1058,1090,2,1059,1091,2,1060,1092,2,1061,1093,2,1062,1094,2,1063,1095,2,1064,1096,2,1065,1097,2,1066,1098,2,1067,1099,2,1068,1100,2,1069,
    1101,2,1070,1102,2,1071,1103,2,1072,1040,1,1073,1041,1,1074,1042,1,1075,1043,1,1076,1044,1,1077,1045,1,1078,1046,1,1079,1047,1,1080,1048,
    1,1081,1049,1,1082,1050,1,1083,1051,1,1084,1052,1,1085,1053,1,1086,1054,1,1087,1055,1,1088,1056,1,1089,1057,1,1090,1058,1,1091,1059,1,
    1092,1060,1,1093,1061,1,1094,1062,1,1095,1063,1,1096,1064,1,1097,1065,1,1098,1066,1,1099,1067,1,1100,1068,1,1101,1069,1,1102,1070,1,1103,
    1071,1,1104,1024,1,1105,1025,1,1106,1026,1,1107,1027,1,1108,1028,1,1109,1029,1,1110,1030,1,1111,1031,1,1112,1032,1,1113,1033,1,1114,1034,
    1,1115,1035,1,1116,1036,1,1117,1037,1,1118,1038,1,1119,1039,1,1120,1121,2,1121,1120,1,1122,1123,2,1123,1122,1,1124,1125,2,1125,1124,1,
    1126,1127,2,1127,1126,1,1128,1129,2,1129,1128,1,1130,1131,2,1131,1130,1,1132,1133,2,1133,1132,1,1134,1135,2,1135,1134,1,1136,1137,2,1137,
    1136,1,1138,1139,2,1139,1138,1,1140,1141,2,1141,1140,1,1142,1143,2,1143,1142,1,1144,1145,2,1145,1144,1,1146,1147,2,1147,1146,1,1148,1149,
    2,1149,1148,1,1150,1151,2,1151,1150,1,1152,1153,2,1153,1152,1,1162,1163,2,1163,1162,1,1164,1165,2,1165,1164,1,1166,1167,2,1167,1166,1,
    1168,1169,2,1169,1168,1,1170,1171,2,1171,1170,1,1172,1173,2,1173,1172,1,1174,1175,2,1175,1174,1,1176,1177,2,1177,1176,1,1178,1179,2,1179,
    1178,1,1180,1181,2,1181,1180,1,1182,1183,2,1183,1182,1,1184,1185,2,1185,1184,1,1186,1187,2,1187,1186,1,1188,1189,2,1189,1188,1,1190,1191,
    2,1191,1190,1,1192,1193,2,1193,1192,1,1194,1195,2,1195,1194,1,1196,1197,2,1197,1196,1,1198,1199,2,1199,1198,1,1200,1201,2,1201,1200,1,
    1202,1203,2,1203,1202,1,1204,1205,2,1205,1204,1,1206,1207,2,1207,1206,1,1208,1209,2,1209,1208,1,1210,1211,2,1211,1210,1,1212,1213,2,1213,
    1212,1,1214,1215,2,1215,1214,1,1216,1231,2,1217,1218,2,1218,1217,1,1219,1220,2,1220,1219,1,1221,1222,2,1222,1221,1,1223,1224,2,1224,1223,
    1,1225,1226,2,1226,1225,1,1227,1228,2,1228,1227,1,1229,1230,2,1230,1229,1,1231,1216,1,1232,1233,2,1233,1232,1,1234,1235,2,1235,1234,1,
    1236,1237,2,1237,1236,1,1238,1239,2,1239,1238,1,1240,1241,2,1241,1240,1,1242,1243,2,1243,1242,1,1244,1245,2,1245,1244,1,1246,1247,2,1247,
    1246,1,1248,1249,2,1249,1248,1,1250,1251,2,1251,1250,1,1252,1253,2,1253,1252,1,1254,1255,2,1255,1254,1,1256,1257,2,1257,1256,1,1258,1259,
    2,1259,1258,1,1260,1261,2,1261,1260,1,1262,1263,2,1263,1262,1,1264,1265,2,1265,1264,1,1266,1267,2,1267,1266,1,1268,1269,2,1269,1268,1,
    1270,1271,2,1271,1270,1,1272,1273,2,1273,1272,1,1274,1275,2,1275,1274,1,1276,1277,2,1277,1276,1,1278,1279,2,1279,1278,1,1280,1281,2,1281,
    1280,1,1282,1283,2,1283,1282,1,1284,1285,2,1285,1284,1,1286,1287,2,1287,1286,1,1288,1289,2,1289,1288,1,1290,1291,2,1291,1290,1,1292,1293,
    2,1293,1292,1,1294,1295,2,1295,1294,1,1296,1297,2,1297,1296,1,1298,1299,2,1299,1298,1,1300,1301,2,1301,1300,1,1302,1303,2,1303,1302,1,
    1304,1305,2,1305,1304,1,1306,1307,2,1307,1306,1,1308,1309,2,1309,1308,1,1310,1311,2,1311,1310,1,1312,1313,2,1313,1312,1,1314,1315,2,1315,
    1314,1,1316,1317,2,1317,1316,1,1329,1377,2,1330,1378,2,1331,1379,2,1332,1380,2,1333,1381,2,1334,1382,2,1335,1383,2,1336,1384,2,1337,1385,
    2,1338,1386,2,1339,1387,2,1340,1388,2,1341,1389,2,1342,1390,2,1343,1391,2,1344,1392,2,1345,1393,2,1346,1394,2,1347,1395,2,1348,1396,2,
    1349,1397,2,1350,1398,2,1351,1399,2,1352,1400,2,1353,1401,2,1354,1402,2,1355,1403,2,1356,1404,2,1357,1405,2,1358,1406,2,1359,1407,2,1360,
    1408,2,1361,1409,2,1362,1410,2,1363,1411,2,1364,1412,2,1365,1413,2,1366,1414,2,1369,1369,0,1377,1329,1,1378,1330,1,1379,1331,1,1380,1332,
    1,1381,1333,1,1382,1334,1,1383,1335,1,1384,1336,1,1385,1337,1,1386,1338,1,1387,1339,1,1388,1340,1,1389,1341,1,1390,1342,1,1391,1343,1,
    1392,1344,1,1393,1345,1,1394,1346,1,1395,1347,1,1396,1348,1,1397,1349,1,1398,1350,1,1399,1351,1,1400,1352,1,1401,1353,1,1402,1354,1,1403,
    1355,1,1404,1356,1,1405,1357,1,1406,1358,1,1407,1359,1,1408,1360,1,1409,1361,1,1410,1362,1,1411,1363,1,1412,1364,1,1413,1365,1,1414,1366,
    1,1415,1415,1,1488,1488,1,1489,1489,1,1490,1490,1,1491,1491,1,1492,1492,1,1493,1493,1,1494,1494,1,1495,1495,1,1496,1496,1,1497,1497,1,
    1498,1498,1,1499,1499,1,1500,1500,1,1501,1501,1,1502,1502,1,1503,1503,1,1504,1504,1,1505,1505,1,1506,1506,1,1507,1507,1,1508,1508,1,1509,
    1509,1,1510,1510,1,1511,1511,1,1512,1512,1,1513,1513,1,1514,1514,1,1570,1570,1,1571,1571,1,1576,1576,1,1577,1577,1,1578,1578,1,1579,1579,
    1,1580,1580,1,1581,1581,1,1582,1582,1,1583,1583,1,1584,1584,1,1585,1585,1,1586,1586,1,1587,1587,1,1588,1588,1,1589,1589,1,1590,1590,1,
    1591,1591,1,1592,1592,1,1593,1593,1,1594,1594,1,1601,1601,1,1602,1602,1,1603,1603,1,1604,1604,1,1605,1605,1,1606,1606,1,1607,1607,1,1608,
    1608,1,1609,1609,1,1610,1610,1,65153,65153,1,65154,65154,1,65155,65155,1,65156,65156,1,65167,65167,1,65168,65168,1,65169,65169,1,65170,65170,1,65171,65171,
    1,65172,65172,1,65173,65173,1,65174,65174,1,65175,65175,1,65176,65176,1,65177,65177,1,65178,65178,1,65179,65179,1,65180,65180,1,65181,65181,1,65182,65182,1,
    65183,65183,1,65184,65184,1,65185,65185,1,65186,65186,1,65187,65187,1,65188,65188,1,65189,65189,1,65190,65190,1,65191,65191,1,65192,65192,1,65193,65193,1,65194,
    65194,1,65195,65195,1,65196,65196,1,65197,65197,1,65198,65198,1,65199,65199,1,65200,65200,1,65201,65201,1,65202,65202,1,65203,65203,1,65204,65204,1,65205,65205,
    1,65206,65206,1,65207,65207,1,65208,65208,1,65209,65209,1,65210,65210,1,65211,65211,1,65212,65212,1,65213,65213,1,65214,65214,1,65215,65215,1,65216,65216,1,
    65217,65217,1,65218,65218,1,65219,65219,1,65220,65220,1,65221,65221,1,65222,65222,1,65223,65223,1,65224,65224,1,65225,65225,1,65226,65226,1,65227,65227,1,65228,
    65228,1,65229,65229,1,65230,65230,1,65231,65231,1,65232,65232,1,65233,65233,1,65234,65234,1,65235,65235,1,65236,65236,1,65237,65237,1,65238,65238,1,65239,65239,
    1,65240,65240,1,65241,65241,1,65242,65242,1,65243,65243,1,65244,65244,1,65245,65245,1,65246,65246,1,65247,65247,1,65248,65248,1,65249,65249,1,65250,65250,1,
    65251,65251,1,65252,65252,1,65253,65253,1,65254,65254,1,65255,65255,1,65256,65256,1,65257,65257,1,65258,65258,1,65259,65259,1,65260,65260,1,65261,65261,1,65262,
    65262,1,65263,65263,1,65264,65264,1,65265,65265,1,65266,65266,1,65267,65267,1,65268,65268,1,-1};

//------------------------------------------------------------------------
long UTF8_Handler::size_w(u_ustring& s) {
    long sz = 0;
    for (long i = 0; i < s.size(); i++) {
        scan_emoji(s, i);
        sz++;
    }
    return sz;
}

long UTF8_Handler::size_utf16(wstring& s) {
    long sz = 0;
    for (long i = 0; i < s.size(); i++) {
        scan_emoji16(s, i);
        sz++;
    }
    return sz;
}

bool UTF8_Handler::scan_emoji(unsigned char* u, long& i) {
    return emojis_characters->scan(u, i);
}

bool UTF8_Handler::get_emoji(unsigned char* u, string& res, long& i) {
    return emojis_characters->get(u, res, i);
}

bool UTF8_Handler::store_emoji(unsigned char* u, string& res, long& i) {
    return emojis_characters->store(u, res, i);
}

bool UTF8_Handler::scan_emoji(string& u, long& i) {
    return emojis_characters->scan(u, i);
}

bool UTF8_Handler::get_emoji(string& u, string& res, long& i) {
    return emojis_characters->get(u, res, i);
}

bool UTF8_Handler::store_emoji(string& u, string& res, long& i) {
    return emojis_characters->store(u, res, i);
}

#ifdef WIN32
Exporting void concat_to_wstring(wstring& res, UWCHAR code) {
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
		res += (wchar_t)code;
}

bool UTF8_Handler::scan_emoji(wstring& u, long& i) {
    return emojis_characters->scan16(u, i);
}

bool UTF8_Handler::get_emoji(wstring& u, wstring& res, long& i) {
    return emojis_characters->get16(u, res, i);
}

bool UTF8_Handler::store_emoji(wstring& u, wstring& res, long& i) {
    return emojis_characters->store16(u, res, i);
}
#else
bool UTF8_Handler::scan_emoji(wstring& u, long& i) {
    return emojis_characters->scan(u, i);
}

bool UTF8_Handler::get_emoji(wstring& u, wstring& res, long& i) {
    return emojis_characters->get(u, res, i);
}

bool UTF8_Handler::store_emoji(wstring& u, wstring& res, long& i) {
    return emojis_characters->store(u, res, i);
}
#endif

bool UTF8_Handler::scan_emoji16(wstring& u, long& i) {
    return emojis_characters->scan16(u, i);
}

bool UTF8_Handler::get_emoji16(wstring& u, wstring& res, long& i) {
    return emojis_characters->get16(u, res, i);
}

bool UTF8_Handler::store_emoji16(wstring& u, wstring& res, long& i) {
    return emojis_characters->store16(u, res, i);
}

bool UTF8_Handler::scan_emoji(u_ustring& u, long& i) {
    return emojis_characters->scan(u, i);
}

bool UTF8_Handler::get_emoji(u_ustring& u, u_ustring& res, long& i) {
    return emojis_characters->get(u, res, i);
}

bool UTF8_Handler::store_emoji(u_ustring& u, u_ustring& res, long& i) {
    return emojis_characters->store(u, res, i);
}

//------------------------------------------------------------------------

long UTF8_Handler::c_bytetocharposition(string& contenu, long charpos) {
    long i = 0;
    long sz = 0;
    while (i < charpos) {
        i += 1 + c_test_utf8(contenu, i);
        sz++;
    }
    return sz;
}

long UTF8_Handler::c_bytetoutf16position(string& contenu, long charpos) {
    long i = 0;
    long sz = 0;
    UWCHAR code;
    while (i < charpos) {
        i += 1 + c_utf8_to_unicode(contenu, i, code);
        if (code & 0xFFFF0000)
            sz++;
        sz++;
    }
    return sz;
}

long UTF8_Handler::getonchar(u_ustring& w, long position) {
    long i = 0;
    long nb = 0;
    while (nb < position) {
        scan_emoji(w, i);
        i++;
        nb++;
    }
    return i;
}

Exporting void get_one_char(string& utf, string& res, long& i) {
    res = utf[i];
    
    unsigned char check = utf[i] & 0xF0;
    
    switch (check) {
        case 0xC0:
            if ((utf[i + 1] & 0x80)== 0x80) {
                res += utf[i + 1];
                i += 1;
            }
            break;
        case 0xE0:
            if ((utf[i + 1] & 0x80)== 0x80 && (utf[i + 2] & 0x80)== 0x80) {
                res += utf[i + 1];
                res += utf[i + 2];
                i += 2;
            }
            break;
        case 0xF0:
            if ((utf[i + 1] & 0x80) == 0x80 && (utf[i + 2] & 0x80)== 0x80 && (utf[i + 3] & 0x80)== 0x80) {
                res += utf[i + 1];
                res += utf[i + 2];
                res += utf[i + 3];
                i += 3;
            }
            break;
    }
}

Exporting void add_one_char(string& utf, string& res, long& i) {
    res += utf[i];
    
    unsigned char check = utf[i] & 0xF0;
    
    switch (check) {
        case 0xC0:
            if ((utf[i + 1] & 0x80)== 0x80) {
                res += utf[i + 1];
                i += 1;
            }
            break;
        case 0xE0:
            if ((utf[i + 1] & 0x80)== 0x80 && (utf[i + 2] & 0x80)== 0x80) {
                res += utf[i + 1];
                res += utf[i + 2];
                i += 2;
            }
            break;
        case 0xF0:
            if ((utf[i + 1] & 0x80) == 0x80 && (utf[i + 2] & 0x80)== 0x80 && (utf[i + 3] & 0x80)== 0x80) {
                res += utf[i + 1];
                res += utf[i + 2];
                res += utf[i + 3];
                i += 3;
            }
            break;
    }
}

#ifdef WIN32
UWCHAR getonechar(unsigned char* s, long& i) {
    UWCHAR result, code;
    i += c_utf8_to_unicode(s + i, code);
    if (c_utf16_to_unicode(result, code, false)) {
        i += c_utf8_to_unicode(s + i, code);
        c_utf16_to_unicode(result, code,  true);
    }
    return result;
}

UWCHAR getonechar(string& s, long& i) {
    UWCHAR result, code;
    i += c_utf8_to_unicode(s, i, code);
    if (c_utf16_to_unicode(result, code, false)) {
        i += c_utf8_to_unicode(s, i, code);
        c_utf16_to_unicode(result, code,  true);
    }
    return result;
}
#else
UWCHAR getonechar(unsigned char* s, long& i) {
    UWCHAR code;
    i += c_utf8_to_unicode(s + i, code);
    return code;
}

UWCHAR getonechar(string& s, long& i) {
    UWCHAR code;
    i += c_utf8_to_unicode(s, i, code);
    return code;
}
#endif

string UTF8_Handler::insert_sep(string& s, string sep) {
    string res;
    long lg = s.size();
    long i = 0;
    while (i < lg) {
        if (i)
            res += sep;
        getandaddchar(s, res, i);
    }
    return res;
}

u_ustring UTF8_Handler::u_insert_sep(u_ustring& s, u_ustring sep) {
    u_ustring res;
    long lg = s.size();
    long i = 0;
    while (i < lg) {
        if (i)
            res += sep;
        getandaddchar(s, res, i);
    }
    return res;
}

UWCHAR UTF8_Handler::getachar(u_ustring& s, long& i) {
    UWCHAR res = s[i];
    scan_emoji(s, i);
    return res;
}

string UTF8_Handler::getachar(string& s, long& i) {
    string res;
    if (!get_emoji(s, res, i))
        get_one_char(s, res, i);
    return res;
}

void UTF8_Handler::getchar(u_ustring& s, u_ustring& res,  long& i) {
    if (!get_emoji(s, res, i))
        res = s[i];
    i++;
}

void UTF8_Handler::getchar(string& s, string& res,  long& i) {
    if (!get_emoji(s, res, i))
        get_one_char(s, res, i);
    i++;
}

void UTF8_Handler::getandaddchar(u_ustring& s, u_ustring& res, long& i) {
    if (!store_emoji(s, res, i))
        res += s[i];
    i++;
}

void UTF8_Handler::getandaddchar(string& s, string& res, long& i) {
    if (!store_emoji(s, res, i))
        add_one_char(s, res, i);
    i++;
}


void UTF8_Handler::getAtchar(string& s, string& res, long nb) {
    long i = 0;
    long sz = s.size();
    while (nb) {
        if (i >= sz)
            return;
        if (!scan_emoji(s, i))
            i += c_test_utf8(s, i);
        i++;
        nb--;
    }
    get_one_char(s, res, i);
}

//Transforms a character position into a byte position
//utf8 characters can occupy up to 4 bytes
long UTF8_Handler::charTobyte(string& s, long nb) {
    long i = 0;
    long sz = s.size();
    while (nb) {
        if (i >= sz)
            return -1;
        if (!scan_emoji(s, i))
            i += c_test_utf8(s, i);
        i++;
        nb--;
    }
    return i;
}

//Transforms a byte position into a character position
long UTF8_Handler::byteTochar(string& s, long sz) {
    long nb = 0;
    long i = 0;
    while (i < sz) {
        if (!scan_emoji(s, i))
            i += c_test_utf8(s, i);
        i++;
        nb++;
    }
    return nb;
}

//------------------------------------------------------------------------
// EMOJIS
//------------------------------------------------------------------------

bool UTF8_Handler::c_is_emoji(UWCHAR c) {
    return emojis_characters->isemoji(c);
}

bool UTF8_Handler::c_is_emojicomp(UWCHAR c) {
    return emojis_characters->isemojicomplement(c);
}

bool UTF8_Handler::c_is_emoji(string& m, long& i) {
    return emojis_characters->isemoji(getonechar(m, i));
}

bool UTF8_Handler::c_is_emojicomp(string& m, long& i) {
    return emojis_characters->isemojicomplement(getonechar(m, i));
}

bool UTF8_Handler::c_is_emoji(unsigned char* m, long& i) {
    return emojis_characters->isemoji(getonechar(m, i));
}

bool UTF8_Handler::c_is_emojicomp(unsigned char* m, long& i) {
    return emojis_characters->isemojicomplement(getonechar(m, i));
}

bool UTF8_Handler::s_is_emoji(string& s) {
    if (s == "")
        return false;
    
    long lg = s.size();
    long p;
    for (long i = 0; i < lg; i++) {
        p = i;
        if (scan_emoji(s, p)) {
            i = p;
            continue;
        }
        return false;
    }
    return true;
}

bool UTF8_Handler::u_is_emoji(u_ustring& s) {
    if (s == U"")
        return false;
    long lg = s.size();
    
    for (long i = 0; i < lg; i++) {
        if (scan_emoji(s, i))
            continue;
        return false;
    }
    return true;
}

bool UTF8_Handler::s_is_emoji(wstring& w) {
    u_pstring s =  _w_to_u(w);
    return u_is_emoji(s);
}

//------------------------------------------------------------------------
Exporting string cs_unicode_to_utf8(UWCHAR code) {
    char utf[5];
    unsigned char c = (bool(code & 0xFFFF0000) << 3) | (bool(code & 0xF800)<<2) | (bool(code & 0x780) << 1);
    switch (c) {
        case 8:
        case 10:
        case 12:
        case 14:
            utf[0] = 0xF0 | (code >> 18);
            utf[1] = 0x80 | ((code >> 12) & 0x3f);
            utf[2] = 0x80 | ((code >> 6) & 0x3f);
            utf[3] = 0x80 | (code& 0x3f);
            utf[4] = 0;
            break;
        case 4:
        case 6:
            utf[0] = 0xe0 | (code >> 12);
            utf[1] = 0x80 | ((code >> 6) & 0x3f);
            utf[2] = 0x80 | (code& 0x3f);
            utf[3] = 0;
            break;
        case 2:
            utf[0] = 0xc0 | (code >> 6);
            utf[1] = 0x80 | (code& 0x3f);
            utf[2] = 0;
            break;
        default:
            utf[0] = (unsigned char)code;
            utf[1] = 0;
            break;
    }
    return utf;
}

unsigned char c_unicode_to_utf8(UWCHAR code, unsigned char* utf) {
    unsigned char c = (bool(code & 0xFFFF0000) << 3) | (bool(code & 0xF800)<<2) | (bool(code & 0x780) << 1);
    
    switch (c) {
        case 8:
        case 10:
        case 12:
        case 14:
            utf[0] = 0xF0 | (code >> 18);
            utf[1] = 0x80 | ((code >> 12) & 0x3f);
            utf[2] = 0x80 | ((code >> 6) & 0x3f);
            utf[3] = 0x80 | (code& 0x3f);
            utf[4] = 0;
            return 4;
        case 4:
        case 6:
            utf[0] = 0xe0 | (code >> 12);
            utf[1] = 0x80 | ((code >> 6) & 0x3f);
            utf[2] = 0x80 | (code& 0x3f);
            utf[3] = 0;
            return 3;
        case 2:
            utf[0] = 0xc0 | (code >> 6);
            utf[1] = 0x80 | (code& 0x3f);
            utf[2] = 0;
            return 2;
        default:
            utf[0] = (unsigned char)code;
            utf[1] = 0;
            return 1;
    }
}

Exporting char* unicode_2_utf8(long code, char* utf) {
    unsigned char c = (bool(code & 0xFFFF0000) << 3) | (bool(code & 0xF800)<<2) | (bool(code & 0x780) << 1);
    switch (c) {
        case 8:
        case 10:
        case 12:
        case 14:
            utf[0] = 0xF0 | (code >> 18);
            utf[1] = 0x80 | ((code >> 12) & 0x3f);
            utf[2] = 0x80 | ((code >> 6) & 0x3f);
            utf[3] = 0x80 | (code& 0x3f);
            utf[4] = 0;
            break;
        case 4:
        case 6:
            utf[0] = 0xe0 | (code >> 12);
            utf[1] = 0x80 | ((code >> 6) & 0x3f);
            utf[2] = 0x80 | (code& 0x3f);
            utf[3] = 0;
            break;
        case 2:
            utf[0] = 0xc0 | (code >> 6);
            utf[1] = 0x80 | (code& 0x3f);
            utf[2] = 0;
            break;
        default:
            utf[0] = (unsigned char)code;
            utf[1] = 0;
            break;
    }
    return utf;
}


string c_unicode_to_utf8(UWCHAR code) {
    char utf[5];
    
    unsigned char c = (bool(code & 0xFFFF0000) << 3) | (bool(code & 0xF800)<<2) | (bool(code & 0x780) << 1);
    switch (c) {
        case 8:
        case 10:
        case 12:
        case 14:
            utf[0] = 0xF0 | (code >> 18);
            utf[1] = 0x80 | ((code >> 12) & 0x3f);
            utf[2] = 0x80 | ((code >> 6) & 0x3f);
            utf[3] = 0x80 | (code& 0x3f);
            utf[4] = 0;
            break;
        case 4:
        case 6:
            utf[0] = 0xe0 | (code >> 12);
            utf[1] = 0x80 | ((code >> 6) & 0x3f);
            utf[2] = 0x80 | (code& 0x3f);
            utf[3] = 0;
            break;
        case 2:
            utf[0] = 0xc0 | (code >> 6);
            utf[1] = 0x80 | (code& 0x3f);
            utf[2] = 0;
            break;
        default:
            utf[0] = (unsigned char)code;
            utf[1] = 0;
            break;
    }
    return utf;
}

Exporting char c_test_utf8(string& utf, long i) {
    if (i == utf.size())
        return 0;
    
    unsigned char check = utf[i] & 0xF0;
    
    switch (check) {
        case 0xC0:
            return bool((utf[i + 1] & 0x80)== 0x80)*1;
        case 0xE0:
            return bool(((utf[i + 1] & 0x80)== 0x80 && (utf[i + 2] & 0x80)== 0x80))*2;
        case 0xF0:
            return bool(((utf[i + 1] & 0x80) == 0x80 && (utf[i + 2] & 0x80)== 0x80 && (utf[i + 3] & 0x80)== 0x80))*3;
    }
    return 0;
}

Exporting char c_test_utf8(unsigned char* utf) {
    if (utf == NULL)
        return 0;
    
    unsigned char check = utf[0] & 0xF0;
    
    switch (check) {
        case 0xC0:
            return bool((utf[1] & 0x80)== 0x80)*1;
        case 0xE0:
            return bool(((utf[1] & 0x80)== 0x80 && (utf[2] & 0x80)== 0x80))*2;
        case 0xF0:
            return bool(((utf[1] & 0x80) == 0x80 && (utf[2] & 0x80)== 0x80 && (utf[3] & 0x80)== 0x80))*3;
    }
    return 0;
}

//------------------------------------------------------------------------

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

UTF8_Handler::~UTF8_Handler() {
    delete emojis_characters;
}

UTF8_Handler::UTF8_Handler() {
    int32_t unicode;
    bulongchar xs;
    bulongchar xse;
    long i;
    int maxtable;
    for (maxtable = 0; codingtable[maxtable] != -1; maxtable++);
    
    
    emojis_characters = new Emojis();
    
    for (i = 0; i < maxtable; i += 3) {
        unicode = codingtable[i];
        wchar_t equ = codingtable[i + 1];
        char type = codingtable[i + 2];
        xs.clear();
        xse.clear();
        c_unicode_to_utf8(unicode, xs.ustr);
        c_unicode_to_utf8(equ, xse.ustr);
        
        if (type == 1) {
            utf8codemin[unicode] = equ;
        }
        else {
            if (type == 2) {
                utf8codemaj[unicode] = equ;
            }
        }
    }

    i = 0;
    while (ponctuations[i] != 0) {
        punctuations[ponctuations[i]] = true;
        vpunctuations.push_back(ponctuations[i]);
        i++;
    }
    
    wconsonants[208] = 68;
    wconsonants[104] = 104;
    wconsonants[264] = 67;
    wconsonants[88] = 88;
    wconsonants[349] = 115;
    wconsonants[81] = 81;
    wconsonants[241] = 110;
    wconsonants[266] = 67;
    wconsonants[231] = 99;
    wconsonants[292] = 72;
    wconsonants[199] = 67;
    wconsonants[290] = 71;
    wconsonants[209] = 78;
    wconsonants[331] = 110;
    wconsonants[291] = 103;
    wconsonants[347] = 115;
    wconsonants[486] = 71;
    wconsonants[294] = 72;
    wconsonants[98] = 98;
    wconsonants[122] = 122;
    wconsonants[77] = 77;
    wconsonants[319] = 76;
    wconsonants[262] = 67;
    wconsonants[269] = 99;
    wconsonants[357] = 116;
    wconsonants[263] = 99;
    wconsonants[112] = 112;
    wconsonants[86] = 86;
    wconsonants[103] = 103;
    wconsonants[113] = 113;
    wconsonants[87] = 87;
    wconsonants[265] = 99;
    wconsonants[308] = 74;
    wconsonants[268] = 67;
    wconsonants[284] = 71;
    wconsonants[100] = 100;
    wconsonants[84] = 84;
    wconsonants[313] = 76;
    wconsonants[379] = 90;
    wconsonants[345] = 114;
    wconsonants[267] = 99;
    wconsonants[82] = 82;
    wconsonants[83] = 83;
    wconsonants[353] = 115;
    wconsonants[80] = 80;
    wconsonants[270] = 68;
    wconsonants[352] = 83;
    wconsonants[293] = 104;
    wconsonants[78] = 78;
    wconsonants[271] = 100;
    wconsonants[288] = 71;
    wconsonants[272] = 68;
    wconsonants[273] = 100;
    wconsonants[108] = 108;
    wconsonants[66] = 66;
    wconsonants[67] = 67;
    wconsonants[285] = 103;
    wconsonants[286] = 71;
    wconsonants[341] = 114;
    wconsonants[109] = 109;
    wconsonants[309] = 106;
    wconsonants[381] = 90;
    wconsonants[287] = 103;
    wconsonants[289] = 103;
    wconsonants[329] = 110;
    wconsonants[344] = 82;
    wconsonants[120] = 120;
    wconsonants[328] = 110;
    wconsonants[295] = 104;
    wconsonants[106] = 106;
    wconsonants[314] = 108;
    wconsonants[346] = 83;
    wconsonants[107] = 107;
    wconsonants[342] = 82;
    wconsonants[310] = 75;
    wconsonants[311] = 107;
    wconsonants[340] = 82;
    wconsonants[326] = 110;
    wconsonants[315] = 76;
    wconsonants[316] = 108;
    wconsonants[99] = 99;
    wconsonants[317] = 76;
    wconsonants[318] = 108;
    wconsonants[320] = 108;
    wconsonants[355] = 116;
    wconsonants[321] = 76;
    wconsonants[322] = 108;
    wconsonants[323] = 78;
    wconsonants[324] = 110;
    wconsonants[325] = 78;
    wconsonants[327] = 78;
    wconsonants[343] = 114;
    wconsonants[348] = 83;
    wconsonants[350] = 83;
    wconsonants[351] = 115;
    wconsonants[354] = 84;
    wconsonants[356] = 84;
    wconsonants[358] = 84;
    wconsonants[359] = 116;
    wconsonants[372] = 87;
    wconsonants[373] = 119;
    wconsonants[377] = 90;
    wconsonants[378] = 122;
    wconsonants[380] = 122;
    wconsonants[382] = 122;
    wconsonants[384] = 98;
    wconsonants[385] = 66;
    wconsonants[386] = 98;
    wconsonants[387] = 98;
    wconsonants[388] = 98;
    wconsonants[389] = 98;
    wconsonants[391] = 67;
    wconsonants[392] = 99;
    wconsonants[393] = 68;
    wconsonants[488] = 75;
    wconsonants[394] = 68;
    wconsonants[395] = 100;
    wconsonants[396] = 100;
    wconsonants[401] = 70;
    wconsonants[402] = 102;
    wconsonants[403] = 71;
    wconsonants[408] = 75;
    wconsonants[409] = 107;
    wconsonants[455] = 455;
    wconsonants[473] = 473;
    wconsonants[484] = 71;
    wconsonants[485] = 103;
    wconsonants[487] = 103;
    wconsonants[489] = 107;
    wconsonants[496] = 106;
    wconsonants[90] = 90;
    wconsonants[68] = 68;
    wconsonants[70] = 70;
    wconsonants[71] = 71;
    wconsonants[72] = 72;
    wconsonants[74] = 74;
    wconsonants[115] = 115;
    wconsonants[75] = 75;
    wconsonants[76] = 76;
    wconsonants[102] = 102;
    wconsonants[110] = 110;
    wconsonants[114] = 114;
    wconsonants[116] = 116;
    wconsonants[118] = 118;
    wconsonants[119] = 119;
    
    wvowels[194] = 65;
    wvowels[200] = 69;
    wvowels[277] = 101;
    wvowels[252] = 117;
    wvowels[192] = 65;
    wvowels[243] = 111;
    wvowels[239] = 105;
    wvowels[256] = 65;
    wvowels[276] = 69;
    wvowels[201] = 69;
    wvowels[207] = 105;
    wvowels[233] = 101;
    wvowels[73] = 73;
    wvowels[193] = 65;
    wvowels[279] = 101;
    wvowels[242] = 111;
    wvowels[202] = 69;
    wvowels[275] = 101;
    wvowels[278] = 69;
    wvowels[219] = 85;
    wvowels[195] = 65;
    wvowels[196] = 65;
    wvowels[218] = 85;
    wvowels[259] = 97;
    wvowels[197] = 65;
    wvowels[258] = 65;
    wvowels[217] = 85;
    wvowels[216] = 79;
    wvowels[228] = 97;
    wvowels[203] = 69;
    wvowels[260] = 65;
    wvowels[204] = 105;
    wvowels[226] = 97;
    wvowels[210] = 79;
    wvowels[205] = 105;
    wvowels[249] = 117;
    wvowels[246] = 111;
    wvowels[225] = 97;
    wvowels[206] = 105;
    wvowels[227] = 97;
    wvowels[211] = 79;
    wvowels[257] = 97;
    wvowels[244] = 111;
    wvowels[232] = 101;
    wvowels[280] = 69;
    wvowels[212] = 79;
    wvowels[213] = 79;
    wvowels[214] = 79;
    wvowels[235] = 101;
    wvowels[220] = 85;
    wvowels[221] = 89;
    wvowels[224] = 97;
    wvowels[69] = 69;
    wvowels[253] = 121;
    wvowels[229] = 97;
    wvowels[234] = 101;
    wvowels[236] = 105;
    wvowels[237] = 105;
    wvowels[298] = 73;
    wvowels[238] = 105;
    wvowels[250] = 117;
    wvowels[245] = 111;
    wvowels[376] = 89;
    wvowels[248] = 111;
    wvowels[251] = 117;
    wvowels[261] = 97;
    wvowels[255] = 121;
    wvowels[274] = 69;
    wvowels[281] = 101;
    wvowels[282] = 69;
    wvowels[283] = 101;
    wvowels[364] = 85;
    wvowels[296] = 73;
    wvowels[297] = 105;
    wvowels[117] = 117;
    wvowels[299] = 105;
    wvowels[300] = 73;
    wvowels[301] = 105;
    wvowels[302] = 73;
    wvowels[303] = 105;
    wvowels[304] = 73;
    wvowels[111] = 111;
    wvowels[305] = 105;
    wvowels[332] = 79;
    wvowels[333] = 111;
    wvowels[334] = 79;
    wvowels[335] = 111;
    wvowels[85] = 85;
    wvowels[336] = 79;
    wvowels[337] = 111;
    wvowels[360] = 85;
    wvowels[361] = 117;
    wvowels[362] = 85;
    wvowels[363] = 117;
    wvowels[365] = 117;
    wvowels[366] = 85;
    wvowels[367] = 117;
    wvowels[368] = 85;
    wvowels[369] = 117;
    wvowels[370] = 85;
    wvowels[371] = 117;
    wvowels[374] = 89;
    wvowels[375] = 121;
    wvowels[404] = 89;
    wvowels[461] = 65;
    wvowels[462] = 97;
    wvowels[463] = 73;
    wvowels[480] = 65;
    wvowels[464] = 105;
    wvowels[465] = 79;
    wvowels[466] = 111;
    wvowels[467] = 85;
    wvowels[468] = 117;
    wvowels[469] = 85;
    wvowels[470] = 117;
    wvowels[471] = 85;
    wvowels[479] = 97;
    wvowels[472] = 117;
    wvowels[474] = 117;
    wvowels[475] = 85;
    wvowels[476] = 117;
    wvowels[478] = 65;
    wvowels[481] = 97;
    wvowels[490] = 79;
    wvowels[491] = 111;
    wvowels[492] = 79;
    wvowels[493] = 111;
    wvowels[97] = 97;
    wvowels[105] = 105;
    wvowels[121] = 121;
    wvowels[65] = 65;
    wvowels[101] = 101;
    wvowels[79] = 79;
    wvowels[89] = 89;
}

Exporting unsigned char c_utf8_to_unicode(unsigned char* utf, UWCHAR& code) {
    code = utf[0];

    unsigned char check = utf[0] & 0xF0;
    
    switch (check) {
        case 0xC0:
            if ((utf[1] & 0x80)== 0x80) {
                code = (utf[0] & 0x1F) << 6;
                code |= (utf[1] & 0x3F);
                return 1;
            }
            break;
        case 0xE0:
            if ((utf[1] & 0x80)== 0x80 && (utf[2] & 0x80)== 0x80) {
                code = (utf[0] & 0xF) << 12;
                code |= (utf[1] & 0x3F) << 6;
                code |= (utf[2] & 0x3F);
                return 2;
            }
            break;
        case 0xF0:
            if ((utf[1] & 0x80) == 0x80 && (utf[2] & 0x80)== 0x80 && (utf[3] & 0x80)== 0x80) {
                code = (utf[0] & 0x7) << 18;
                code |= (utf[1] & 0x3F) << 12;
                code |= (utf[2] & 0x3F) << 6;
                code |= (utf[3] & 0x3F);
                return 3;
            }
            break;
    }
    return 0;
}

Exporting unsigned char c_utf8_to_unicode(string& utf, long i, UWCHAR& code) {
    code = utf[i];

    unsigned char check = utf[i] & 0xF0;
    
    switch (check) {
        case 0xC0:
            if ((utf[i + 1] & 0x80)== 0x80) {
                code = (utf[i] & 0x1F) << 6;
                code |= (utf[i + 1] & 0x3F);
                return 1;
            }
            break;
        case 0xE0:
            if ((utf[i + 1] & 0x80)== 0x80 && (utf[i + 2] & 0x80)== 0x80) {
                code = (utf[i] & 0xF) << 12;
                code |= (utf[i + 1] & 0x3F) << 6;
                code |= (utf[i + 2] & 0x3F);
                return 2;
            }
            break;
        case 0xF0:
            if ((utf[i + 1] & 0x80) == 0x80 && (utf[i + 2] & 0x80)== 0x80 && (utf[i + 3] & 0x80)== 0x80) {
                code = (utf[i] & 0x7) << 18;
                code |= (utf[i + 1] & 0x3F) << 12;
                code |= (utf[i + 2] & 0x3F) << 6;
                code |= (utf[i + 3] & 0x3F);
                return 3;
            }
            break;
    }
    return 0;
}

Exporting unsigned char c_utf8_to_unicode(string* utf, long i, UWCHAR& code) {
    code = (*utf)[i];

    unsigned char check = (*utf)[i] & 0xF0;
    
    switch (check) {
        case 0xC0:
            if (((*utf)[i + 1] & 0x80)== 0x80) {
                code = ((*utf)[i] & 0x1F) << 6;
                code |= ((*utf)[i + 1] & 0x3F);
                return 1;
            }
            break;
        case 0xE0:
            if (((*utf)[i + 1] & 0x80)== 0x80 && ((*utf)[i + 2] & 0x80)== 0x80) {
                code = ((*utf)[i] & 0xF) << 12;
                code |= ((*utf)[i + 1] & 0x3F) << 6;
                code |= ((*utf)[i + 2] & 0x3F);
                return 2;
            }
            break;
        case 0xF0:
            if (((*utf)[i + 1] & 0x80) == 0x80 && ((*utf)[i + 2] & 0x80)== 0x80 && ((*utf)[i + 3] & 0x80)== 0x80) {
                code = ((*utf)[i] & 0x7) << 18;
                code |= ((*utf)[i + 1] & 0x3F) << 12;
                code |= ((*utf)[i + 2] & 0x3F) << 6;
                code |= ((*utf)[i + 3] & 0x3F);
                return 3;
            }
            break;
    }
    return 0;
}

//--------------------------------------------------------------------
Exporting bool c_is_hexa(wchar_t code) {
    static const char hexas[]= {'a','b','c','d','e','f','A','B','C','D','E','F'};
    
    if (c_is_digit(code))
        return true;
    
    if (code <= 'f' && strchr(hexas,(char)code))
        return true;
    
    return false;
}
//--------------------------------------------------------------------
Exporting string jsonstring(string value) {
    if (value == "")
        return "\"\"";
    
    string res;
    
    if (value.find("\\") != -1)
        value = s_replacingstring(value, "\\", "\\\\");
    
    if (value.find("\"") != -1) {
        value = s_replacingstring(value, "\"", "\\\"");
    }
    res += "\"";
    res += value;
    res += "\"";
    return res;
}

Exporting wstring wjsonstring(wstring value) {
    if (value == L"")
        return L"\"\"";
    
    wstring res;
    
    if (value.find(L"\\") != -1)
        value = s_wreplacestring(value, L"\\", L"\\\\");
    
    if (value.find(L"\"") != -1) {
        value = s_wreplacestring(value, L"\"", L"\\\"");
    }
    res += L"\"";
    res += value;
    res += L"\"";
    return res;
}

Exporting wstring wjsonstring(u_ustring value) {
    return wjsonstring(_u_to_w(value));
}

Exporting u_ustring ujsonstring(u_ustring value) {
    if (value == U"")
        return U"\"\"";
    
    u_ustring res;
    
    if (value.find(U"\\") != -1)
        value = s_ureplacestring(value, U"\\", U"\\\\");
    
    if (value.find(U"\"") != -1) {
        value = s_ureplacestring(value, U"\"", U"\\\"");
    }
    res += U"\"";
    res += value;
    res += U"\"";
    return res;
}


//--------------------------------------------------------------------

bool UTF8_Handler::c_is_punctuation(u_uchar c) {
    return punctuations.check(c);
}

bool UTF8_Handler::c_is_punctuation(wchar_t c) {
    return punctuations.check(c);
}

bool UTF8_Handler::u_is_punctuation(u_ustring& str) {
    for (long i = 0; i < str.size(); i++) {
        if (!c_is_punctuation(str[i]))
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_punctuation(wstring& str) {
    for (long i = 0; i < str.size(); i++) {
        if (!c_is_punctuation(str[i]))
            return false;
    }
    return true;
}


char UTF8_Handler::c_is_alpha(unsigned char* m, long& i) {
    UWCHAR v;
    i += c_utf8_to_unicode(m + i, v);
    return (utf8codemin.check(v) + ((char)utf8codemaj.check(v) << 1));
}

char UTF8_Handler::c_is_alpha(u_uchar v) {
    return (utf8codemin.check(v) + (2 * (char)utf8codemaj.check(v)));
}

char UTF8_Handler::c_is_alpha(wchar_t v) {
    return (utf8codemin.check(v) + (2 * (char)utf8codemaj.check(v)));
}



char UTF8_Handler::is_a_valid_letter(unsigned char* m, long& i) {
    if (m[i] == '_' || isadigit(m[i]))
        return 1;
    return c_is_alpha(m, i);
}

char UTF8_Handler::is_a_valid_letter(UWCHAR c) {
    if (c == '_' || isadigit(c))
        return 1;
    return c_is_alpha(c);
}

char UTF8_Handler::is_a_valid_letter(wstring& m, long& i) {
    if (m[i] == '_' || isadigit(m[i]))
        return 1;
    return c_is_alpha(m[i]);
}

bool UTF8_Handler::u_is_alpha(u_ustring& s) {
    if (s == U"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (!c_is_alpha(s[i]))
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_digit(u_ustring& s) {
    if (s == U"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (!c_is_digit(s[i]))
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_alpha(wstring& s) {
    if (s == L"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (!c_is_alpha(s[i]))
            return false;
    }
    return true;
}


bool UTF8_Handler::u_is_upper(u_ustring& s) {
    if (s == U"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (c_is_alpha(s[i]) != 2)
            return false;
    }
    return true;
}

bool UTF8_Handler::u_is_lower(u_ustring& s) {
    if (s == U"")
        return false;
    
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (c_is_alpha(s[i]) != 1)
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_upper(wstring& s) {
    if (s == L"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (c_is_alpha(s[i]) != 2)
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_lower(wstring& s) {
    if (s == L"")
        return false;
    
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        if (c_is_alpha(s[i]) != 1)
            return false;
    }
    return true;
}

u_uchar UTF8_Handler::uc_to_lower(u_uchar c) {
    if (utf8codemaj.check(c))
        return utf8codemaj.at(c);
    return c;
}

u_uchar UTF8_Handler::uc_to_upper(u_uchar c) {
    if (utf8codemin.check(c))
        return utf8codemin.at(c);
    return c;
}

wchar_t UTF8_Handler::c_to_lower(wchar_t c) {
    if (utf8codemaj.check(c))
        return utf8codemaj.at(c);
    return c;
}

wchar_t UTF8_Handler::c_to_upper(wchar_t c) {
    if (utf8codemin.check(c))
        return utf8codemin.at(c);
    return c;
}


string UTF8_Handler::u_to_lower(string& u) {
    u_ustring res;
    u_ustring s;
    s_utf8_to_unicode(s, u, u.size());
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (u_uchar)uc_to_lower(s[i]);
    string r;
    s_unicode_to_utf8(r, res);
    return r;
}

string UTF8_Handler::u_to_upper(string& u) {
    u_ustring res;
    u_ustring s;
    s_utf8_to_unicode(s, u, u.size());
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (u_uchar)uc_to_upper(s[i]);
    string r;
    s_unicode_to_utf8(r, res);
    return r;
}

wstring UTF8_Handler::s_to_lower(wstring& s) {
    wstring res;
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (wchar_t)c_to_lower(s[i]);
    return res;
}

wstring UTF8_Handler::s_to_upper(wstring& s) {
    wstring res;
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (wchar_t)c_to_upper(s[i]);
    return res;
}
u_ustring UTF8_Handler::u_to_lower(u_ustring& s) {
    u_ustring res;
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (u_uchar)uc_to_lower(s[i]);
    return res;
}

u_ustring UTF8_Handler::u_to_upper(u_ustring& s) {
    u_ustring res;
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (u_uchar)c_to_upper(s[i]);
    return res;
}

bool UTF8_Handler::s_is_space(string& str) {
    long lg = str.size();
	UWCHAR code;
    for (long i = 0; i < lg; i++) {
        i += c_utf8_to_unicode(str, i, code);
        if (!c_is_space(code))
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_space(u_ustring& str) {
    long lg = str.size();
    for (long i = 0; i < lg; i++) {
        if (!c_is_space(str[i]))
            return false;
    }
    return true;
}

bool UTF8_Handler::s_is_space(wstring& str) {
    long lg = str.size();
    u_uchar code;
    for (long i = 0; i < lg; i++) {
        code = str[i];
        switch (code) {
            case 9:
            case 10:
            case 13:
            case 32:
            case 160:
            case 0x202F:
            case 0x3000:
                continue;
            default:
                return false;
        }
    }
    return true;
}


bool s_is_digit(string& str) {
    long lg = str.size();
    long i = 0;
    if (str[0] == '-' || str[0] == '.')
        i++;
    bool digit = false;
    bool dot = false;
    int countdigits = 0;
    for (; i < lg; i++) {
        if (str[i] == '.' || str[i] == ',') {
            if (!digit)
                return false;
            
            if (str[i] == '.') {
                if (dot)
                    return false;
                dot = true;
            }
            else {
                if (countdigits != 3)
                    return false;
            }
            countdigits = 0;
            continue;
        }
        
        if (str[i]<48 || str[i]>57)
            return false;
        countdigits++;
        digit = true;
    }
    
    if (!digit)
        return false;
    return true;
}

Exporting bool s_is_utf8(string& contenu, long longueur) {
    long i = 0;
    while (longueur--) {
        if ((contenu[i] & 0x80) && c_test_utf8(contenu, i))
            return true;
        i++;
    }
    return false;
}

//------------------------------------------------------------------------
wstring& s_trim0(wstring& strvalue) {
    if (strvalue.find(L".") != -1) {
        long i = strvalue.size() - 1;
        while (strvalue[i] == '0') i--;
        if (strvalue[i] == '.')
            i--;
        strvalue = strvalue.substr(0, i + 1);
    }
    return strvalue;
}

string& s_trim0(string& strvalue) {
    if (strvalue.find(".") != -1) {
        long i = strvalue.size() - 1;
        while (strvalue[i] == '0') i--;
        if (strvalue[i] == '.')
            i--;
        strvalue = strvalue.substr(0, i + 1);
    }
    return strvalue;
}

u_ustring& u_trim0(u_ustring& strvalue) {
    if (strvalue.find(U".") != -1) {
        long i = strvalue.size() - 1;
        while (strvalue[i] == '0') i--;
        if (strvalue[i] == '.')
            i--;
        strvalue = strvalue.substr(0, i + 1);
    }
    return strvalue;
}

wstring& s_trim(wstring& strvalue) {
    long d, f;
    for (d = 0; d<strvalue.size(); d++) {
        if (strvalue[d]>32)
            break;
    }
    
    for (f = strvalue.size() - 1; f >= 0; f--) {
        if (strvalue[f] > 32)
            break;
    }
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = L"";
    return strvalue;
}

u_ustring& u_trim(u_ustring& strvalue) {
    long d, f;
    for (d = 0; d<strvalue.size(); d++) {
        if (strvalue[d]>32)
            break;
    }
    
    for (f = strvalue.size() - 1; f >= 0; f--) {
        if (strvalue[f] > 32)
            break;
    }
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = U"";
    return strvalue;
}


wstring& s_trimleft(wstring& strvalue) {
    long d, f;
    f = strvalue.size() - 1;
    for (d = 0; d<strvalue.size(); d++) {
        if (strvalue[d]>32)
            break;
    }
    
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = L"";
    return strvalue;
}

wstring& s_trimright(wstring& strvalue) {
    long d = 0, f;
    
    for (f = strvalue.size() - 1; f >= 0; f--) {
        if (strvalue[f] > 32)
            break;
    }
    
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = L"";
    return strvalue;
}

u_ustring& u_trimleft(u_ustring& strvalue) {
    long d, f;
    f = strvalue.size() - 1;
    for (d = 0; d<strvalue.size(); d++) {
        if (strvalue[d]>32)
            break;
    }
    
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = U"";
    return strvalue;
}

u_ustring& u_trimright(u_ustring& strvalue) {
    long d = 0, f;
    
    for (f = strvalue.size() - 1; f >= 0; f--) {
        if (strvalue[f] > 32)
            break;
    }
    
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = U"";
    return strvalue;
}

string& s_trim(string& strvalue) {
    long d, f;
    for (d = 0; d<strvalue.size(); d++) {
        if ((uchar)strvalue[d]>32)
            break;
    }
    
    for (f = strvalue.size() - 1; f >= 0; f--) {
        if ((uchar)strvalue[f] > 32)
            break;
    }
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = "";
    return strvalue;
}

string& s_trimleft(string& strvalue) {
    long d, f;
    f = strvalue.size() - 1;
    for (d = 0; d<strvalue.size(); d++) {
        if ((uchar)strvalue[d]>32)
            break;
    }
    
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = "";
    return strvalue;
}

string& s_trimright(string& strvalue) {
    long d = 0, f;
    
    for (f = strvalue.size() - 1; f >= 0; f--) {
        if ((uchar)strvalue[f] > 32)
            break;
    }
    
    long lg = f - d + 1;
    if (lg >= 1)
        strvalue = strvalue.substr(d, lg);
    else
        strvalue = "";
    return strvalue;
}
//------------------------------------------------------------------------
long getindex(unsigned char* contenu, long lg, long i) {
    long x = 0;
    
    long nb;
    while (i > 0 && x < lg) {
        nb = c_test_utf8(contenu + x);
        x += nb + 1;
        i--;
    }
    
    return x;
}

long getindex(string& contenu, long lg, long i) {
    long x = 0;
    
    long nb;
    while (i > 0 && x < lg) {
        nb = c_test_utf8(contenu, x);
        x += nb + 1;
        i--;
    }
    
    return x;
}

Exporting long size_raw_c(string& contenu, long sz) {
    long i = 0;
    long size = i;
    long nb;
    
    while (i < sz) {
        nb = c_test_utf8(contenu, i);
        i += nb + 1;
        ++size;
    }
    
    return size;
}

long size_c(string& s) {
    return size_raw_c(s, s.size());
}

string s_left(string& s, long nb) {
    if (nb <= 0)
        return "";
    
    long lg = s.size();
    nb = getindex(s, lg, nb);
    if (nb >= lg)
        return s;
    
    return s.substr(0, nb);
}
string s_right(string& s, long nb) {
    if (nb <= 0)
        return "";

    long lg = s.size();

    long l = size_raw_c(s, lg) - nb;

    if (l <= 0)
        return s;
    
    l = getindex(s, lg, l);
    return s.substr(l, lg);
}

string s_middle(string& s, long l, long nb) {
    long lg = s.size();

    long i = getindex(s, lg, l);
    if (i >= lg)
        return "";

    nb += l;
    nb = getindex(s, lg, nb);
    return s.substr(i, nb - i);
}

wstring s_wleft(wstring& s, long nb) {
    if (nb <= 0)
        return L"";
    
    long lg = s.size();
    if (nb >= lg)
        return s;
    
    return s.substr(0, nb);
}

wstring s_wright(wstring& s, long nb) {
    if (nb <= 0)
        return L"";

    long lg = s.size();

    long l = lg - nb;

    if (l <= 0)
        return s;
    
    return s.substr(l, lg);
}

wstring s_wmiddle(wstring& s, long l, long nb) {
    long lg = s.size();

    if (l >= lg)
        return L"";

    return s.substr(l, nb);
}

u_ustring s_uleft(u_ustring& s, long nb) {
    if (nb <= 0)
        return U"";
    
    long lg = s.size();
    if (nb >= lg)
        return s;
    
    return s.substr(0, nb);
}

u_ustring s_uright(u_ustring& s, long nb) {
    if (nb <= 0)
        return U"";

    long lg = s.size();

    long l = lg - nb;

    if (l <= 0)
        return s;
    
    return s.substr(l, lg);
}

u_ustring s_umiddle(u_ustring& s, long l, long nb) {
    long lg = s.size();

    if (l >= lg)
        return U"";

    return s.substr(l, nb);
}
//------------------------------------------------------------------------
string s_replacingstring(string& s, string reg, string rep) {
    string neo;
    
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
    return neo;
}

wstring s_wreplacestring(wstring& s, wstring reg, wstring rep) {
    wstring neo;
    
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
    return neo;
}

u_ustring s_ureplacechar(u_ustring& s, u_uchar reg, u_ustring rep) {
    u_ustring neo;
    
    long gsz = 1;
    
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
    return neo;
}

u_ustring s_ureplacestring(u_ustring& s, u_ustring reg, u_ustring rep) {
    u_ustring neo;
    
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
    return neo;
}

long nb_ureplacestring(u_ustring& s, u_ustring reg, u_ustring rep) {
    u_ustring neo;
    
    long gsz = reg.size();
    if (!gsz)
        return 0;
    
    long rsz = s.size();
    long from = 0;
    long nb = 0;
    long foundHere;
    while ((foundHere = s.find(reg, from)) != string::npos) {
        if (foundHere != from)
            neo += s.substr(from, foundHere - from);
        neo += rep;
        from = foundHere + gsz;
        nb++;
    }
    if (from < rsz)
        neo += s.substr(from, rsz - from);
    s = neo;
    return nb;
}

long nb_replacestring(string& s, string reg, string rep) {
    string neo;
    
    long gsz = reg.size();
    if (!gsz)
        return 0;
    
    long rsz = s.size();
    long from = 0;
    long nb = 0;
    long foundHere;
    while ((foundHere = s.find(reg, from)) != string::npos) {
        if (foundHere != from)
            neo += s.substr(from, foundHere - from);
        neo += rep;
        from = foundHere + gsz;
        nb++;
    }
    if (from < rsz)
        neo += s.substr(from, rsz - from);
    s = neo;
    return nb;
}



//------------------------------------------------------------------------
/*
A little bit of explanation for hexadecimal conversion:
 
 v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3) ) + ((c & 64) >> 6);
 
 a) First, each 'digit' in an hexadecimal expression corresponds to 4 bits, hence (v << 4)
 b) digits are based on: 0011 0000, the bits 4,5 are set to 1, the last four bits are used to store the actual value
 c) uppercase letter are based on: 0100 0000, the bit 6 is set 1
 d) lowercase letter are based on: 0110 0000, the bits 6 and 5 are set to 1
 
 c & 0xF will remove the upper bits from the equation
 If the bit 6 is set to 1, this is a letter: A,B,C,D,E or a,b,c,d,e
 
 Let's convert 'B': 66 or 0100 0010
 We expect 11 as a value for 'B'
 c & 0xF        is  0000 0010   ; this is the 2nd letter in the alphabet
 (c & 64)>>3    is  0000 1000   ; its value is its position in the alphabet + 9
 union =        is  0000 1010   ; hence, we first add 8
 
 (c & 64)>>1    is  0000 0001   ; then we add 1
 we add:            0000 1011   --> 11
 
 Let's convert 'c': 99 or 0110 0011
 We expect 12 as a value for 'c'
 c & 0xF        is  0000 0011   ; this is the 3rd letter in the alphabet
 (c & 64)>>3    is  0000 1000   ; its value is its position in the alphabet + 9
 union =        is  0000 1011   ; hence, we first add 8
 
 (c & 64)>>1    is  0000 0001   ; then we add 1
 we add:            0000 1100   --> 12
 
 Let's convert '4': 50 or 0011 0100
 c & 0xF        is  0000 0100
 (c & 64)>>3    is  0000 0000   ; as it is not a letter, 6th bit is 0
 union =        is  0000 0100   ; nothing happens here
 
 (c & 64)>>1    is  0000 0000   ; same we add 0
 we add:            0000 0100   --> 4

 This calculus might seem a bit to much compared to a check on the characters.
 However, in most out-of-order processors, it makes sense as it removes any
 need for a test and is then much faster...

 */

double conversiontofloathexa(const char* s, int sign) {
    long v = 0;
    uchar c = *s++;
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = *s++;
    }
    
    double res = v;
    
    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = *s++;
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = *s++;
            mantissa += 4;
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

double convertingfloathexa(const char* s) {
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
    
    long v;
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                return conversiontofloathexa(s, sign);
            }
            
            if (s[1]=='b') {
                s+=2;
                v = *s++ & 15;
                while (*s == '1' || *s == '0') {
                    v = (v << 1) + (*s++ & 15);
                }
                return v;
            }
        }
        
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

double conversiontofloathexa(const wchar_t* s, int sign) {
    long v = 0;
    uchar c = *s++;
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = *s++;
    }
    
    double res = v;
    
    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = *s++;
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = *s++;
            mantissa += 4;
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
double convertingfloathexa(const wchar_t* s) {
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
    
    long v;
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                return conversiontofloathexa(s, sign);
            }
            
            if (s[1]=='b') {
                s+=2;
                v = *s++ & 15;
                while (*s == '1' || *s == '0') {
                    v = (v << 1) + (*s++ & 15);
                }
                return v;
            }
        }
        
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

double conversiontofloathexa(const u_uchar* s, int sign) {
    long v = 0;
    uchar c = *s++;
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = *s++;
    }
    
    double res = v;
    
    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = *s++;
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = *s++;
            mantissa += 4;
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

double convertingfloathexa(const u_uchar* s) {
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
    
    long v;
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                return conversiontofloathexa(s, sign);
            }
            
            if (s[1]=='b') {
                s+=2;
                v = *s++ & 15;
                while (*s == '1' || *s == '0') {
                    v = (v << 1) + (*s++ & 15);
                }
                return v;
            }
        }
        
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

double conversiontofloathexa(u_ustring& s, long& i, int sign) {
    long v = 0;
    uchar c = s[i++];
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = s[i++];
    }
    
    double res = v;
    
    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = s[i++];
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = s[i++];
            mantissa += 4;
        }
        
        res += (double)v/(double)(1 << mantissa);
    }
    
    
    if ((c &0xDF) == 'P') {
        bool sgn = false;
        if (s[i] == '-') {
            sgn = true;
            i++;
        }
        else {
            if (s[i] == '+')
                i++;
        }
        
        v = s[i++] & 15;
        while (isadigit(s[i])) {
            v = (v << 3) + (v << 1) + (s[i++] & 15);
        }
        v = 1 << v;
        if (sgn)
            res *= 1 / (double)v;
        else
            res *= v;
        
    }
    
    return res*sign;
}

double convertingfloathexa(u_ustring& s) {
    long i = 0;
    while (s[i]!=0 && s[i]<=32) i++;
    
    //End of string...
    if (s[i] == 0 )
        return 0;
    
    int sign = 1;
    
    //Sign
    if (s[i]=='-') {
        sign = -1;
        ++i;
    }
    else
        if (s[i]=='+')
            i++;
    
    long v;
    if (isadigit(s[i])) {
        if (s[i] == '0') {
            if (s[i+1] == 'x') {
                i+=2;
                return conversiontofloathexa(s, i, sign);
            }
            if (s[i+1] == 'b') {
                i+=2;
                v = 0;
                while (s[i] == '0' || s[i] == '1') {
                    v = (v << 1) + (s[i++] & 15);
                }
                return v;
            }
        }
        
        v = s[i++] & 15;
        while (isadigit(s[i])) {
            v = (v << 3) + (v << 1) + (s[i++] & 15);
        }
        if (!s[i])
            return v*sign;
    }
    else
        return 0;
    
    double res = v;
    
    if (s[i]=='.') {
        i++;
        if (isadigit(s[i])) {
            uchar mantissa = 1;
            v = s[i++] & 15;
            while (isadigit(s[i])) {
                v = (v << 3) + (v << 1) + (s[i++] & 15);
                ++mantissa;
            }
            res += (double)v / power10(mantissa);
        }
        else
            return res*sign;
    }
    
    if ((s[i] &0xDF) == 'E') {
        i++;
        long sgn = 1;
        if (s[i] == '-') {
            sgn = -1;
            i++;
        }
        else {
            if (s[i] == '+')
                i++;
        }
        
        if (isadigit(s[i])) {
            v = s[i++] & 15;
            while (isadigit(s[i]))
                v = (v << 3) + (v << 1) + (s[i++] & 15);
            
            res *= power10(v*sgn);
        }
    }
    return res*sign;
}

double conversiontofloathexa(string& s, long& i, int sign) {
    long v = 0;
    uchar c = s[i++];
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = s[i++];
    }
    
    double res = v;
    
    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = s[i++];
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = s[i++];
            mantissa += 4;
        }
        
        res += (double)v/(double)(1 << mantissa);
    }
    
    
    if ((c &0xDF) == 'P') {
        bool sgn = false;
        if (s[i] == '-') {
            sgn = true;
            i++;
        }
        else {
            if (s[i] == '+')
                i++;
        }
        
        v = s[i++] & 15;
        while (isadigit(s[i])) {
            v = (v << 3) + (v << 1) + (s[i++] & 15);
        }
        v = 1 << v;
        if (sgn)
            res *= 1 / (double)v;
        else
            res *= v;
        
    }
    
    return res*sign;
}

double convertingfloathexa(string& s) {
    long i = 0;
    while (s[i]!=0 && s[i]<=32) i++;
    
    //End of string...
    if (s[i] == 0 )
        return 0;
    
    int sign = 1;
    
    //Sign
    if (s[i]=='-') {
        sign = -1;
        ++i;
    }
    else
        if (s[i]=='+')
            i++;
    
    long v;
    if (isadigit(s[i])) {
        if (s[i] == '0') {
            if (s[i+1] == 'x') {
                i+=2;
                return conversiontofloathexa(s, i, sign);
            }
            if (s[i+1] == 'b') {
                i+=2;
                v = 0;
                while (s[i] == '0' || s[i] == '1') {
                    v = (v << 1) + (s[i++] & 15);
                }
                return v;
            }
        }
        
        v = s[i++] & 15;
        while (isadigit(s[i])) {
            v = (v << 3) + (v << 1) + (s[i++] & 15);
        }
        if (!s[i])
            return v*sign;
    }
    else
        return 0;
    
    double res = v;
    
    if (s[i]=='.') {
        i++;
        if (isadigit(s[i])) {
            uchar mantissa = 1;
            v = s[i++] & 15;
            while (isadigit(s[i])) {
                v = (v << 3) + (v << 1) + (s[i++] & 15);
                ++mantissa;
            }
            res += (double)v / power10(mantissa);
        }
        else
            return res*sign;
    }
    
    if ((s[i] &0xDF) == 'E') {
        i++;
        long sgn = 1;
        if (s[i] == '-') {
            sgn = -1;
            i++;
        }
        else {
            if (s[i] == '+')
                i++;
        }
        
        if (isadigit(s[i])) {
            v = s[i++] & 15;
            while (isadigit(s[i]))
                v = (v << 3) + (v << 1) + (s[i++] & 15);
            
            res *= power10(v*sgn);
        }
    }
    return res*sign;
}
//------------------------------------------------------------------------
double conversiontofloathexa(const char* s, int sign, long& l) {
    long v = 0;
    uchar c = *s++;
    l++;

    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = *s++;
        l++;
    }
    
    double res = v;

    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = *s++;
        l++;
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            l++;
            mantissa += 4;
            c = *s++;
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

double convertingfloathexa(const char* s, long& l) {
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
    
    long v;
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                l++;
                return conversiontofloathexa(s, sign, l);
            }
            if (s[1]=='b') {
                s+=2;
                v = *s++ & 15;
                l += 3;
                while (*s=='0' || *s == '1') {
                    v = (v << 1) + (*s++ & 15);
                    l++;
                }
                return v;
            }
        }
        
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

void noconversiontofloathexa(const char* s, int sign, int16_t& l) {
    uchar c = *s++;
    l++;
    
    while (c < 103 && digitaction[c]) {
        c = *s++;
        l++;
    }

    
    if (c == '.') {
        c = *s++;
        l++;
        while (c < 103 && digitaction[c]) {
            c = *s++;
            l++;
        }
    }
    

    if ((c &0xDF) == 'P') {
        if (*s == '-') {
            ++s;
            l++;
        }
        else {
            if (*s == '+') {
                ++s;
                ++l;
            }
        }
        
        s++;
        l++;
        while (isadigit(*s)) {
            s++;
            l++;
        }
    }
}

void noconvertingfloathexa(const char* s, int16_t& l) {
    l = 0;
    //End of string...
    if (*s ==0 )
        return;
    
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
    
    
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                l++;
                noconversiontofloathexa(s, sign, l);
                return;
            }
            
            if (s[1]=='b') {
                s+=2;
                l+=2;
                while (*s == '0' || *s == '1') {
                    s++;
                    l++;
                }
                return;
            }
        }
        
        s++;
        l++;
        while (isadigit(*s)) {
            s++;
            l++;
        }
        if (!*s)
            return;
    }
    else
        return;
    
    if (*s=='.') {
        ++s;
        l++;
        if (isadigit(*s)) {
            s++;
            l++;
            while (isadigit(*s)) {
                s++;
                l++;
            }
        }
        else
            return;
    }
        
    if ((*s &0xDF) == 'E') {
        ++s;
        l++;
        if (*s == '-') {
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
            s++;
            l++;
            while (isadigit(*s)) {
                s++;
                l++;
            }
        }
    }
}

void noconversiontofloathexa(wchar_t* s, int sign, long& l) {
    wchar_t c = *s++;
    l++;
    while (c < 103 && digitaction[c]) {
        c = *s++;
        l++;
    }

    
    if (c == '.') {
        c = *s++;
        l++;
        while (c < 103 && digitaction[c]) {
            c = *s++;
            l++;
        }
    }
    

    if ((c &0xDF) == 'P') {
        if (*s == '-') {
            ++s;
            l++;
        }
        else {
            if (*s == '+') {
                ++s;
                ++l;
            }
        }
        
        s++;
        l++;
        while (isadigit(*s)) {
            s++;
            l++;
        }
    }
}

void noconvertingfloathexa(wchar_t* s, long& l) {
    l = 0;
    //End of string...
    if (*s ==0 )
        return;
    
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
    
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                l++;
                noconversiontofloathexa(s, sign, l);
                return;
            }
            
            if (s[1]=='b') {
                s+=2;
                l+=2;
                while (*s == '0' || *s == '1') {
                    s++;
                    l++;
                }
                return;
            }
        }
        
        s++;
        l++;
        while (isadigit(*s)) {
            s++;
            l++;
        }
        if (!*s)
            return;
    }
    else
        return;
    
    if (*s=='.') {
        ++s;
        l++;
        if (isadigit(*s)) {
            s++;
            l++;
            while (isadigit(*s)) {
                s++;
                l++;
            }
        }
        else
            return;
    }
        
    if ((*s &0xDF) == 'E') {
        ++s;
        l++;
        if (*s == '-') {
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
            s++;
            l++;
            while (isadigit(*s)) {
                s++;
                l++;
            }
        }
    }
}

long convertinginteger(string& number) {
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
    
    long v = 0;
    
    uchar c = number[ipos++];
    if (number.size() == ipos)
        return (c - 48)*sign;

    if (c == '0') {
        if (number[ipos] == 'x') {
            ipos++;
            c = number[ipos++];
            while (c < 103 && digitaction[c]) {
                v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
                c = number[ipos++];
            }
            return v*sign;
        }
        if (number[ipos] == 'b') {
            ipos++;
            c = number[ipos++];
            while (c == '0' || c == '1') {
                v = (v << 1) + (c & 15);
                c = number[ipos++];
            }
            return v;
        }
    }
    
    if (isadigit(c)) {
        v = c & 15;
        c = number[ipos++];
        while (isadigit(c)) {
            v = (v << 3) + (v << 1) + (c & 15);
            c = number[ipos++];
        }
    }
    return v*sign;
}

long convertinginteger(wstring& number) {
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
    
    long v = 0;
    
    uchar c = number[ipos++];
    if (number.size() == ipos)
        return (c - 48)*sign;

    if (c == '0') {
        if (number[ipos] == 'x') {
            ipos++;
            c = number[ipos++];
            while (c < 103 && digitaction[c]) {
                v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
                c = number[ipos++];
            }
            return v*sign;
        }
        if (number[ipos] == 'b') {
            ipos++;
            c = number[ipos++];
            while (c == '0' || c == '1') {
                v = (v << 1) + (c & 15);
                c = number[ipos++];
            }
            return v;
        }
    }
    
    if (isadigit(c)) {
        v = c & 15;
        c = number[ipos++];
        while (isadigit(c)) {
            v = (v << 3) + (v << 1) + (c & 15);
            c = number[ipos++];
        }
    }
    return v*sign;
}

long convertinginteger(u_ustring& number) {
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
    
    long v = 0;
    
    uchar c = number[ipos++];
    if (number.size() == ipos)
        return (c - 48)*sign;

    if (c == '0') {
        if (number[ipos] == 'x') {
            ipos++;
            c = number[ipos++];
            while (c < 103 && digitaction[c]) {
                v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
                c = number[ipos++];
            }
            return v*sign;
        }
        if (number[ipos] == 'b') {
            ipos++;
            c = number[ipos++];
            while (c == '0' || c == '1') {
                v = (v << 1) + (c & 15);
                c = number[ipos++];
            }
            return v;
        }
    }
    
    if (isadigit(c)) {
        v = c & 15;
        c = number[ipos++];
        while (isadigit(c)) {
            v = (v << 3) + (v << 1) + (c & 15);
            c = number[ipos++];
        }
    }
    return v*sign;
}
//------------------------------------------------------------------------
double convertingtofloathexa(wchar_t* s, int sign, long& l) {
    long v = 0;
    wchar_t c = *s++;
    l++;
    
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = *s++;
        l++;
    }
    
    double res = v;

    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = *s++;
        l++;
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = *s++;
            l++;
            mantissa += 4;
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

double convertingfloathexa(wchar_t* s, long& l) {
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
    
    long v;
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                l++;
                return convertingtofloathexa(s, sign, l);
            }
            
            if (s[1]=='b') {
                s+=2;
                v = *s++ & 15;
                l+=3;
                while (*s == '0' || *s == '1') {
                    v = (v << 1) + (*s++ & 15);
                    l++;
                }
                return v;
            }
        }
        
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

double convertingtofloathexa(u_uchar* s, int sign, long& l) {
    long v = 0;
    u_uchar c = *s++;
    l++;
    
    while (c < 103 && digitaction[c]) {
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
        c = *s++;
        l++;
    }
    
    double res = v;

    if (c == '.') {
        uchar mantissa = 0;
        v = 0;
        c = *s++;
        l++;
        while (c < 103 && digitaction[c]) {
            v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
            c = *s++;
            l++;
            mantissa += 4;
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

double convertingfloathexa(u_uchar* s, long& l) {
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
    
    long v;
    if (isadigit(*s)) {
        if (*s=='0') {
            if (s[1]=='x') {
                s+=2;
                l++;
                return convertingtofloathexa(s, sign, l);
            }
            
            if (s[1]=='b') {
                s+=2;
                v = *s++ & 15;
                l+=3;
                while (*s == '0' || *s == '1') {
                    v = (v << 1) + (*s++ & 15);
                    l++;
                }
                return v;
            }
        }
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

//------------------------------------------------------------------------
Exporting bool c_char_index_insert(string& s, string c, long x) {
    if (x > s.size())
        return false;
    long i;
    long lg = s.size();
    for (i = 0; i<lg && x>0; i++) {
        i += c_test_utf8(s, i);
        x--;
    }
    if (i == lg)
        s += c;
    else
        s.insert(i, c);
    return true;
}

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
//------------------------------------------------------------------------
Exporting void s_utf16_to_utf8_clean(string& s, wstring& str) {
	s = "";
	s_utf16_to_utf8(s, str);
}

Exporting void s_utf16_to_utf8(string& s, int32_t* str, long sz) {
    s = "";
    if (!sz)
        return;

    long i = 0;
    char inter[5];
    long ineo = 0;
    long szo = 1 + (sz << 1);
    char* neo = new char[szo];
    neo[0] = 0;
    long nb;
    UWCHAR c;

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
    s += neo;
    delete[] neo;
}

Exporting void s_utf16_to_utf8(string& s, wchar_t* str, long sz) {
	if (!sz)
		return;

	long i = 0;
	char inter[5];
	long ineo = 0;
	long szo = 1 + (sz << 1);
	char* neo = new char[szo];
	neo[0] = 0;
	long nb;
	UWCHAR c;

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
	s += neo;
	delete[] neo;
}

Exporting void s_utf16_to_utf8(string& s, wstring& str) {
	long sz = str.size();
	if (!sz)
		return;

	long i = 0;
	char inter[5];
	long ineo = 0;
	long szo = 1 + (sz << 1);
	char* neo = new char[szo];
	neo[0] = 0;
	long nb;
	UWCHAR c;

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
	s += neo;
	delete[] neo;
}

Exporting void s_utf16_to_utf8(string& s, u_ustring& str) {
    long sz = str.size();
    if (!sz)
        return;

    long i = 0;
    char inter[5];
    long ineo = 0;
    long szo = 1 + (sz << 1);
    char* neo = new char[szo];
    neo[0] = 0;
    long nb;
    UWCHAR c;

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
    s += neo;
    delete[] neo;
}

#ifdef WIN32
Exporting void s_unicode_to_utf8_clean(string& s, wstring& str) {
	s = "";
	s_utf16_to_utf8(s, str);
}

Exporting void s_unicode_to_utf8(string& s, wstring& str) {
	s_utf16_to_utf8(s, str);
}

Exporting void s_unicode_to_utf8(string& s, wchar_t* str, long sz) {
	s_utf16_to_utf8(s, str, sz);
}
#else
Exporting void s_unicode_to_utf8_clean(string& s, wstring& str) {
	s = "";
	s_unicode_to_utf8(s, str);
}

Exporting void s_unicode_to_utf8(string& s, wstring& str) {
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
    s += neo;
    delete[] neo;
}

Exporting void s_unicode_to_utf8(string& s, wchar_t* str, long sz) {
	long i = 0;
	char inter[5];
	long ineo = 0;
	if (!sz)
		return;

	long szo = 1 + (sz << 1);
	char* neo = new char[szo];
	neo[0] = 0;
	long nb;

	while (i < sz) {
		if (str[i] < 0x0080 && ineo < szo - 1) {
			neo[ineo++] = (char)str[i];
			i++;
			continue;
		}

		nb = c_unicode_to_utf8(str[i], (uchar*)inter);
		neo = concatstrings(neo, inter, ineo, szo, nb);
		i++;
	}

	neo[ineo] = 0;
	s += neo;
	delete[] neo;
}
#endif

Exporting void s_unicode_to_utf8(string& s, u_ustring& str) {
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
		if (str[i] < 0x0080 && ineo < szo - 1) {
			neo[ineo++] = (char)str[i];
			i++;
			continue;
		}

		nb = c_unicode_to_utf8(str[i], (uchar*)inter);
		neo = concatstrings(neo, inter, ineo, szo, nb);
		i++;
	}

	neo[ineo] = 0;
	s += neo;
	delete[] neo;
}

Exporting void s_utf8_to_unicode_clean(wstring& w, string& str , long sz) {
    w = L"";
    s_utf8_to_unicode(w, str , sz);
}

Exporting void c_unicode_to_utf16(wstring& w, u_uchar c) {
    if (!(c & 0xFFFF0000))
        w = (wchar_t)c;
    else {
        u_uchar c_16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
        w = c_16;
        w += 0xDC00 | (c & 0x3FF);
    }
}

Exporting void s_utf8_to_utf16(wstring& w, string& str , long sz) {
    if (!sz)
        return;
    
    
    UWCHAR c;
    uchar nb;
    
    UWCHAR c16;
    long i = 0;
    while (sz--) {
        if (str[i] & 0x80) {
            nb = c_utf8_to_unicode(str, i, c);
            i += nb + 1;
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
        w += str[i++];
    }
}

Exporting void s_utf8_to_unicode_u(wstring& w, unsigned char* str , long sz) {
    if (!sz)
        return;

    long ineo = 0;
    wchar_t* neo = new wchar_t[sz+1];
    neo[0] = 0;

    UWCHAR c;
    uchar nb;
    
#ifdef WIN32
    UWCHAR c16;
    while (sz--) {
        if (*str & 0x80) {
            nb = c_utf8_to_unicode(str, c);
            str += nb + 1;
            sz = (sz >= nb)?sz-nb:0;
            if (!(c & 0xFFFF0000)) {
                neo[ineo++] = (wchar_t)c;
                continue;
            }

            c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
            neo[ineo++] = c16;
            neo[ineo++] = 0xDC00 | (c & 0x3FF);
            continue;
        }
        neo[ineo++] = (wchar_t)*str;
        ++str;
    }
#else
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
#endif
    
    neo[ineo] = 0;
    w += neo;
    delete[] neo;
}

Exporting void s_utf8_to_unicode(wstring& w, string& str , long sz) {
    if (!sz)
        return;

    long ineo = 0;
    wchar_t* neo = new wchar_t[sz+1];
    neo[0] = 0;

    UWCHAR c;
    uchar nb;
    long i = 0;
    
#ifdef WIN32
    UWCHAR c16;
    while (sz--) {
        if (str[i] & 0x80) {
            nb = c_utf8_to_unicode(str, i, c);
            i += nb + 1;
            sz = (sz >= nb)?sz-nb:0;
            if (!(c & 0xFFFF0000)) {
                neo[ineo++] = (wchar_t)c;
                continue;
            }

            c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
            neo[ineo++] = c16;
            neo[ineo++] = 0xDC00 | (c & 0x3FF);
            continue;
        }
        neo[ineo++] = (wchar_t)str[i++];
    }
#else
    while (sz--) {
        if (str[i] & 0x80) {
            nb = c_utf8_to_unicode(str, i, c);
            i += nb+1;
            sz = (sz >= nb)?sz-nb:0;
            neo[ineo++] = c;
            continue;
        }
        neo[ineo++] = (wchar_t)str[i++];
    }
#endif
    
    neo[ineo] = 0;
    w += neo;
    delete[] neo;
}

Exporting void s_utf8_to_unicode(u_ustring& w, string& str , long sz) {
    if (!sz)
        return;

    long ineo = 0;
    u_uchar* neo = new u_uchar[sz+1];
    neo[0] = 0;

    UWCHAR c;
    uchar nb;
    long i = 0;

    while (sz--) {
        if (str[i] & 0x80) {
            nb = c_utf8_to_unicode(str, i, c);
            i += nb+1;
            sz = (sz >= nb)?sz-nb:0;
            neo[ineo++] = c;
            continue;
        }
        neo[ineo++] = (wchar_t)str[i++];
    }
    
    neo[ineo] = 0;
    w += neo;
    delete[] neo;
}
//------------------------------------------------------------------------
void s_split(string& s, string& splitter, vector<string>& vs, bool keepblanks) {
    size_t pos = 0;
    size_t found = 0;
    string sub;
    long sz = splitter.size();
    while (pos != -1) {
        found = s.find(splitter, pos);
        if (found != -1) {
            sub = s.substr(pos, found - pos);
            if (keepblanks)
                vs.push_back(sub);
            else
                if (sub != "")
                    vs.push_back(sub);
            pos = found + sz;
        }
        else
            break;
    }
    
    sub = s.substr(pos, s.size() - pos);
    if (keepblanks)
        vs.push_back(sub);
    else
        if (sub != "")
            vs.push_back(sub);
}

void s_split(wstring& s, wstring& splitter, vector<wstring>& vs, bool keepblanks) {
    size_t pos = 0;
    size_t found = 0;
    wstring sub;
    long sz = splitter.size();
    while (pos != -1) {
        found = s.find(splitter, pos);
        if (found != -1) {
            sub = s.substr(pos, found - pos);
            if (keepblanks)
                vs.push_back(sub);
            else
                if (sub != L"")
                    vs.push_back(sub);
            pos = found + sz;
        }
        else
            break;
    }
    
    sub = s.substr(pos, s.size() - pos);
    if (keepblanks)
        vs.push_back(sub);
    else
        if (sub != L"")
            vs.push_back(sub);
}
//------------------------------------------------------------------------

static long blanksize = 3;
Exporting void SetBlankSize(long sz) {
    if (sz >= 1)
        blanksize = sz;
}

Exporting long GetBlankSize() {
    return blanksize;
}


static const unsigned char _tocheck[] = {'"', '\'', '`', '@', ':', ',','-', '+','0','1','2','3','4','5', '6','7','8', '9','[',']','{', '}', 171, 187, 0};
static const char _checkingmore[] = {'\n', '/', '(', ')', '<', '>','=',';', 0};

void split_container(string& src, long lensrc, vector<long>& pos, bool forindent) {
    uchar c;
    
    for (long e = 0; e < lensrc; e++) {
        c = src[e];
        if (forindent && strchr(_checkingmore, c))
            pos.push_back(e);
        else
            if (strchr((char*)_tocheck, (char)c))
                pos.push_back(e);
    }
}

void cr_normalise(string& code) {
    code = s_replacingstring(code, "\r\n", "\n");
    code = s_replacingstring(code, "\r", "\n");
}

long IndentationCode(string& codestr) {
    static vector<long> pos;
    pos.clear();
    string token;

    long szstr = codestr.size();
    split_container(codestr, szstr, pos, true);

    long sz = pos.size();
    long r = 0;
    long i = 0;
    long iblank = 0;
    long p;
    long finalblank = 0;

    int16_t addspace = 0;
    int16_t checkspace = 0;
    int16_t iparenthesis = 0;
    int16_t l;

    bool consumeblanks = true;

    uchar c = 0;

    i = 0;
    
    while (r < sz) {
        c = codestr[i++];
        
        if (c <= 32) {
            //here we have a CR, the next line should see its white characters being replaced with out indentation pack
            if (c == '\n') {
                consumeblanks = true;
                r++;
                continue;
            }
            //this is a line beginning, we need to remove the blanks first
            if (consumeblanks)
                continue;
            continue;
        }
        
        if (consumeblanks) {
            if (!strchr(")]}>", c)) {
                l = iblank;
                switch (checkspace) {
                    case 0:
                        addspace = 0;
                        break;
                    case 1:
                        if (addspace)
                            addspace--;
                        checkspace = 0;
                        break;
                    case 2:
                        checkspace = 1;
                        break;
                    case 3:
                        iblank+=blanksize;
                        checkspace = 1;
                        break;
                    case 4:
                        checkspace = 5;
                        break;
                    case 5:
                    case 6:
                        iblank -= blanksize;
                        break;
                }

                //we need to insert our blanks before...
                if (addspace)
                    iblank += blanksize*addspace;
                if (iblank) {
                    finalblank = iblank;
                }
                else
                    finalblank = 0;
                iblank = l;
                consumeblanks = false;
            }
        }

        
        if (i != pos[r] + 1) {
            if (c < 'A') {
                continue;
            }
            
            p = i;
            while (codestr[p] > 32 && p < pos[r]) p++;
            c = codestr[p];
            codestr[p] = 0;
            token = (char*)STR(codestr)+i-1;
            codestr[p] = c;
            i = p;
            continue;
        }
        
        r++;
        switch (c) {
            case ';':
                if (checkspace == 2) {
                    addspace--;
                    checkspace = 0;
                }
                else
                    if (checkspace == 3)
                        checkspace = 0;
                break;
            case '#':
                p = i + 1;
                //this is a comment, up to the last CR
                while (r < sz) {
                    p = pos[r++];
                    if (codestr[p] == '\n') {
                        r--; //it will be consumed later
                        break;
                    }
                }
                i = p;
                break;
            case '"':
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if ((codestr[p-1] != '\\' && codestr[p] == '"') || codestr[p] == '\n')
                        break;
                }
                p++;
                i = p;
                break;
            case '`':
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if (codestr[p] == '`')
                        break;
                }
                p++;
                i = p;
                break;
            case 171:
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if ((uchar)codestr[p] == 187)
                        break;
                }
                p++;
                i = p;
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                noconvertingfloathexa((const char*)STR(codestr)+i-1, l);
                p =  i + l - 1;
                while (pos[r] < p) r++;
                i = p;
                break;
            case '{':
            case '(':
                iparenthesis++;
                iblank += blanksize;
                break;
            case '}':
            case ')':
                iparenthesis--;
                iblank -= blanksize;
                if (iblank < 0)
                    iblank = 0;
                if (consumeblanks) {
                    l = iblank;
                    //we need to insert our blanks before...
                    if (addspace)
                        iblank += blanksize*addspace;
                    if (iblank) {
                        finalblank = iblank;
                    }
                    else
                        finalblank = 0;
                    addspace = 0;
                    iblank = l;
                    consumeblanks = false;
                }
                break;
        }
    }

    if (consumeblanks) {
        switch (checkspace) {
            case 0:
                addspace = 0;
                break;
            case 1:
                if (addspace)
                    addspace--;
                break;
            case 2:
                break;
            case 3:
                iblank+=blanksize;
                break;
            case 4:
                break;
            case 5:
            case 6:
                iblank -= blanksize;
                break;
        }
        
        //we need to insert our blanks before...
        if (addspace)
            iblank += blanksize*addspace;
        if (iblank) {
            finalblank = iblank;
        }
        else
            finalblank = 0;
    }
    return finalblank;
}

long VirtualIndentation(string& codestr, bool lisp, bool python) {
    s_trimright(codestr);
    codestr += "\n";
    cr_normalise(codestr);
    return IndentationCode(codestr);
}


void IndentationCode(string& str, string& codeindente) {
    string token;

    uchar* codestr = USTR(str);
    char* blanks;
    
    vector<long> pos;
    long szstr = str.size();
    long r = 0;
    long sz, i, p = 0;
    long iblank = 0;
    long iparenthesis = 0;
    long current_line = 0;
        
    int16_t l;
    int16_t addspace = 0;
    int16_t checkspace = 0;
    
    bool consumeblanks = true;
    
    uchar c;

    split_container(str, szstr, pos, true);
    sz = pos.size();

    for (i = 0; i < sz; i++) {
        c = codestr[pos[i]];
        if (c == '(' || c == '{') {
            iparenthesis++;
            if (p < iparenthesis)
                p = iparenthesis;
        }
        else
            if (c == ')' || c == '}')
                iparenthesis--;
    }
    
    p += 10;
    p *= blanksize;
    string _blanks(p, ' ');
    blanks = STR(_blanks);

    iparenthesis = 0;
    i = 0;
    
    while (r < sz) {
        c = codestr[i++];
        
        if (c <= 32) {
            //here we have a CR, the next line should see its white characters being replaced with out indentation pack
            if (c == '\n') {
                codeindente += c;
                consumeblanks = true;
                r++;
                current_line++;
                continue;
            }
            //this is a line beginning, we need to remove the blanks first
            if (consumeblanks)
                continue;
            codeindente += c;
            continue;
        }
        
        if (consumeblanks) {
            if (!strchr(")]}>", c)) {
                l = iblank;
                switch (checkspace) {
                    case 0:
                        addspace = 0;
                        break;
                    case 1:
                        if (addspace)
                            addspace--;
                        checkspace = 0;
                        break;
                    case 2:
                        checkspace = 1;
                        break;
                    case 3:
                        iblank+=blanksize;
                        checkspace = 1;
                        break;
                    case 4:
                        checkspace = 5;
                        break;
                    case 5:
                    case 6:
                        iblank -= blanksize;
                        break;
                }

                //we need to insert our blanks before...
                if (addspace)
                    iblank += blanksize*addspace;
                if (iblank) {
                    blanks[iblank] = 0;
                    codeindente += blanks;
                    blanks[iblank] = 32;
                }
                iblank = l;
                consumeblanks = false;
            }
        }

        if (i != pos[r] + 1) {
            if (c < 'A') {
                codeindente += c;
                continue;
            }
            
            p = i;
            while (codestr[p] > 32 && p < pos[r]) p++;
            c = codestr[p];
            codestr[p] = 0;
            token = (char*)codestr+i-1;
            codestr[p] = c;
            codeindente += token;
            i = p;
            continue;
        }
        
        r++;
        switch (c) {
            case ';':
                if (checkspace == 2) {
                    addspace--;
                    checkspace = 0;
                }
                else
                    if (checkspace == 3)
                        checkspace = 0;
                codeindente += c;
                break;
            case '#':
                p = i + 1;
                //this is a comment, up to the last CR
                while (r < sz) {
                    p = pos[r++];
                    if (codestr[p] == '\n') {
                        r--; //it will be consumed later
                        break;
                    }
                }
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '"':
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if ((codestr[p-1] != '\\' && codestr[p] == '"') || codestr[p] == '\n')
                        break;
                }
                p++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '`':
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if (codestr[p] == '`')
                        break;
                }
                p++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case 171:
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if ((uchar)codestr[p] == 187)
                        break;
                }
                p++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                noconvertingfloathexa((const char*)codestr+i-1, l);
                p =  i + l - 1;
                while (pos[r] < p) r++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '{':
            case '(':
                iparenthesis++;
                codeindente += c;
                iblank += blanksize;
                break;
            case '}':
            case ')':
                iparenthesis--;
                iblank -= blanksize;
                if (iblank < 0)
                    iblank = 0;
                if (consumeblanks) {
                    l = iblank;
                    //we need to insert our blanks before...
                    if (addspace)
                        iblank += blanksize*addspace;
                    if (iblank) {
                        blanks[iblank] = 0;
                        codeindente += blanks;
                        blanks[iblank] = 32;
                    }
                    addspace = 0;
                    iblank = l;
                    consumeblanks = false;
                }
                codeindente += c;
                break;
            default:
                codeindente += c;
        }
    }
    
    for (;i < szstr; i++)
        codeindente += codestr[i];
}
   
void IndentCode(string& codestr, string& codeindente, long blancs, bool lisp, bool python) {
    long bl = blanksize;
    if (blancs)
        blanksize = blancs;
    codeindente = "";
    IndentationCode(codestr, codeindente);
    blanksize = bl;
    if (codeindente.find("/@") != string::npos || codeindente.find("@\"") != string::npos)
        cr_normalise(codeindente);
    
    s_trimright(codeindente);
    codeindente += "\n";
}

void IndentatingCode(string& str, string& codeindente) {
    string token;

    uchar* codestr = USTR(str);
    char* blanks;
    
    vector<long> pos;
    long szstr = str.size();
    long r = 0;
    long counterlisp = 0;
    long sz, i, p = 0;
    long iblank = 0;
    long iparenthesis = 0;
    long ligne = 0;
        
    short l;
    short addspace = 0;
    short checkspace = 0;
    
    bool consumeblanks = true;
    bool beginning = true;
    bool equalsign = false;
    
    uchar c;

    split_container(str, szstr, pos, true);
    sz = pos.size();

    for (i = 0; i < sz; i++) {
        c = codestr[pos[i]];
        if (strchr("({[",c)) {
            counterlisp++;
            if (p < counterlisp)
                p = counterlisp;
        }
        else
            if (strchr(")]}",c))
                counterlisp--;
    }
    
    p += 10;
    p *= blanksize;
    string _blanks(p, ' ');
    blanks = STR(_blanks);

    counterlisp = 0;
    i = 0;
    
    while (r < sz) {
        c = codestr[i++];
        
        if (c <= 32) {
            //here we have a CR, the next line should see its white characters being replaced with out indentation pack
            if (c == '\n') {
                codeindente += c;
                consumeblanks = true;
                r++;
                ligne++;
                continue;
            }
            //this is a line beginning, we need to remove the blanks first
            if (consumeblanks)
                continue;
            codeindente += c;
            continue;
        }
        
        beginning = false;
        if (consumeblanks) {
            beginning = true;
            if (!strchr(")]}>", c)) {
                l = iblank;
                switch (checkspace) {
                    case 0:
                        addspace = 0;
                        break;
                    case 1:
                        if (addspace)
                            addspace--;
                        checkspace = 0;
                        break;
                    case 2:
                        checkspace = 1;
                        break;
                    case 3:
                        iblank+=blanksize;
                        checkspace = 1;
                        break;
                    case 4:
                        checkspace = 5;
                        break;
                    case 5:
                    case 6:
                        iblank -= blanksize;
                        break;
                }

                //we need to insert our blanks before...
                if (addspace)
                    iblank += blanksize*addspace;
                if (iblank) {
                    blanks[iblank] = 0;
                    codeindente += blanks;
                    blanks[iblank] = 32;
                }
                iblank = l;
                consumeblanks = false;
            }
        }

        equalsign = (c == '<') ? equalsign : false;
        
        if (i != pos[r] + 1) {
            if (c < 'A') {
                codeindente += c;
                continue;
            }
            
            p = i;
            while (codestr[p] > 32 && p < pos[r]) p++;
            c = codestr[p];
            codestr[p] = 0;
            token = (char*)codestr+i-1;
            codestr[p] = c;
            if (token == "case") {
                if (checkspace == 5) {
                    //we need to remove an extra blank size;
                    codeindente = codeindente.substr(0, codeindente.size()-blanksize);
                    checkspace = 6;
                }
                else {
                    if (checkspace < 5) {
                        checkspace = 4;
                        addspace++;
                    }
                }
            }
            else {
                if (token == "else") {
                    checkspace = 3;
                }
                else {
                    if (token == "if" || token == "elif" || token == "for" || token == "while" || token == "default") {
                        if (checkspace == 6) {
                            //extra space missing
                            string space(blanksize, ' ');
                            codeindente += space;
                        }
                        
                        checkspace = 2;
                        addspace++;
                    }
                }
            }
            codeindente += token;
            i = p;
            continue;
        }
        
        r++;
        switch (c) {
            case ';':
                if (checkspace == 2) {
                    addspace--;
                    checkspace = 0;
                }
                else
                    if (checkspace == 3)
                        checkspace = 0;
                codeindente += c;
                break;
            case '/':
                switch (codestr[i]) {
                    case '/':
                        p = i + 1;
                        //this is a comment, up to the last CR
                        r++; //the next element in pos is also a '/'
                        while (r < sz) {
                            p = pos[r++];
                            if (codestr[p] == '\n') {
                                r--; //it will be consumed later
                                break;
                            }
                        }
                        c = codestr[p];
                        codestr[p] = 0;
                        codeindente += (char*)codestr+i-1;
                        codestr[p] = c;
                        i = p;
                        break;
                    case '@':
                        p = i;
                        //this is a long comment...
                        while (r < sz) {
                            p = pos[r++];
                            if (codestr[p-1] == '@' && codestr[p] == '/')
                                break;
                        }
                        p++;
                        c = codestr[p];
                        codestr[p] = 0;
                        codeindente += (char*)codestr+i-1;
                        codestr[p] = c;
                        i = p;
                        break;
                    case '*': // /*C comments*/
                        p = i;
                        //this is a long comment...
                        while (r < sz) {
                            p = pos[r++];
                            if (codestr[p-1] == '*' && codestr[p] == '/')
                                break;
                        }
                        p++;
                        c = codestr[p];
                        codestr[p] = 0;
                        codeindente += (char*)codestr+i-1;
                        codestr[p] = c;
                        i = p;
                        break;
                    default:
                        codeindente += c;
                }
                break;
            case '=':
                equalsign = true;
                codeindente += c;
                break;
            case '@':
                if (codestr[i] == '"') {
                    p = i;
                    while (r < sz) {
                        p = pos[r++];
                        if (codestr[p-1] != '\\' && codestr[p] == '"' && codestr[p+1] == '@') {
                            r++;
                            break;
                        }
                    }
                    p+=2;
                    c = codestr[p];
                    codestr[p] = 0;
                    codeindente += (char*)codestr+i-1;
                    codestr[p] = c;
                    i = p;
                }
                else
                    codeindente += c;
                break;
            case '"':
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if ((codestr[p-1] != '\\' && codestr[p] == '"') || codestr[p] == '\n')
                        break;
                }
                p++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '\'':
                p = i;
                while (r < sz) {
                    p = pos[r++];
                    if (codestr[p] == '\'' || codestr[p] == '\n')
                        break;
                }
                p++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                noconvertingfloathexa((const char*)codestr+i-1, l);
                p =  i + l - 1;
                while (pos[r] < p) r++;
                c = codestr[p];
                codestr[p] = 0;
                codeindente += (char*)codestr+i-1;
                codestr[p] = c;
                i = p;
                break;
            case '{':
                if (!iparenthesis) { //a little hack to handle {}
                    checkspace = 0;
                    addspace = 0;
                    iblank += blanksize;
                }
                codeindente += c;
                break;
            case '(':
                iparenthesis++;
            case '[':
                codeindente += c;
                iblank += blanksize;
                break;
            case ')':
                iparenthesis--;
                iblank -= blanksize;
                if (iblank < 0)
                    iblank = 0;
                if (consumeblanks) {
                    l = iblank;
                    //we need to insert our blanks before...
                    if (addspace)
                        iblank += blanksize*addspace;
                    if (iblank) {
                        blanks[iblank] = 0;
                        codeindente += blanks;
                        blanks[iblank] = 32;
                    }
                    addspace = 0;
                    iblank = l;
                    consumeblanks = false;
                }
                codeindente += c;
                break;
            case '}':
                if (iparenthesis) {
                    codeindente += c;
                    break;
                }
                checkspace = 0;
                addspace = 0;
            case ']':
                iblank -= blanksize;
                if (iblank < 0)
                    iblank = 0;
                if (consumeblanks) {
                    l = iblank;
                    //we need to insert our blanks before...
                    if (addspace)
                        iblank += blanksize*addspace;
                    if (iblank) {
                        blanks[iblank] = 0;
                        codeindente += blanks;
                        blanks[iblank] = 32;
                    }
                    addspace = 0;
                    iblank = l;
                    consumeblanks = false;
                }
                codeindente += c;
                break;
            default:
                codeindente += c;
        }
    }
    
    for (;i < szstr; i++)
        codeindente += codestr[i];
}
   
//---------------------------------------------------------------------------------------

void NormalizeFileName(char* fileName, char* buffer, long buffersz) {
    //All paths to UNIX are first brought back to UNIX
    //We normalize the paths
    //s if there is an environment variable
    char* vari = strchr(buffer, '$');
    fileName[0] = 0;
    while (vari) {
        char* reper = getenv(vari + 1);
        char* pt = strchr(vari + 1, '/');
        
        if (pt != NULL)
            *pt = 0;
        
        //We copy the part preceding the variable
        long lvar = vari - buffer;
        long lnom = strlen(fileName);
        memcpy(fileName + lnom, buffer, lvar);
        fileName[lvar + lnom] = 0;
        
        if (reper != NULL)
            strcat_s(fileName, buffersz, reper);
        
        if (pt != NULL) {
            *pt = '/';
            static char inter[1000];
            strcpy_s(inter,1000, pt);
            strcpy_s(buffer, buffersz, inter);
        }
        else
            buffer[0] = 0;
        vari = strchr(buffer, '$');
    }
    
    strcat_s(fileName, buffersz, buffer);
    char localpath[4096];
#ifdef WIN32
	_fullpath(localpath, fileName, 4096);
#else
    realpath(fileName, localpath);
#endif
    strcpy_s(fileName, buffersz, localpath);
}

string NormalizePathname(string n) {
    if (n == "")
        return "";
    
    char buff[4096];
    char name[4096];
    strcpy_s(name, 4096, STR(n));
    
    NormalizeFileName(buff, name, 4096);
    return buff;
}

typedef bool (*LibraryEntryPoint)(LispE*);

#ifdef WIN32
Element* LispE::load_library(string nom_bib) {
	string name = NormalizePathname(nom_bib);
    if (delegation->libraries.count(name))
        return delegation->_TRUE;


	if (name == "")
		return false;

	HINSTANCE LoadMe;
	string lname = name;
	int pos = lname.find_last_of('\\');
	string subname = lname;
	if (pos != -1) {
		subname = lname.substr(pos + 1, lname.size() - pos - 1);
		pos = subname.find(".");
		if (pos != -1)
			subname = subname.substr(0, pos);
	}

	string moduleinitname = "InitialisationModule";

	//If it has already been loaded, we return...
	nom_bib = lname;
	LibraryEntryPoint LibEntryPoint;

	LoadMe = LoadLibraryA(STR(lname));
	if (LoadMe == 0) {
		string atanlib;
		if (getenv("LISPEPATH") != NULL)
			atanlib = getenv("LISPEPATH");
		else {
			stringstream message;
			message << "Error: You need to execute: set LISPEPATH=your_lispe_library_path";
			throw new Error(message.str());
		}

		if (getenv("PATH") != NULL) {
			string path = "Path=";
			path += atanlib;
			path += ";";
			path += getenv("PATH");
			_putenv(STR(path));
		}

		atanlib += "\\";
		atanlib += subname;
		atanlib += ".dll";
		atanlib = NormalizePathname(atanlib);
		nom_bib = atanlib;
		LoadMe = LoadLibraryA(STR(nom_bib));
		if (LoadMe == 0) {
			DWORD err = GetLastError();
			stringstream message;
			message << "Cannot load library: " << name;
			throw new Error(message.str());
		}
	}

	LibEntryPoint = (LibraryEntryPoint)GetProcAddress(LoadMe, STR(moduleinitname));

	if (LibEntryPoint == NULL) {
		stringstream message;
		message << "No entry point in this library: " << name;
		throw new Error(message.str());
	}

	if ((*LibEntryPoint)(this) == false) {
		stringstream message;
		message << "Error: missing entry point" << name;
		throw new Error(message.str());
	}

	delegation->libraries[name] = true;
	return delegation->_TRUE;
}
#else
Element* LispE::load_library(string nom_bib) {
    string name = NormalizePathname(nom_bib);
    
    if (delegation->libraries.count(name))
        return delegation->_TRUE;
    
    void* LoadMe;
    
    string lname;
    string rawlname;
    
    lname = name;
    rawlname = name;
    
    char* error;
    long ipt = lname.rfind('/');
    bool addso = (lname.substr(lname.size()-3,3) != ".so");
    string basename;
    string rawbasename;
    string pt;
    string rawpt;
    if (ipt != -1) {
        pt = lname.substr(ipt + 1, lname.size());
        if (pt.substr(0,3) == "lib")
            basename = pt;
        else
            basename = "lib" + pt;
        
        if (addso)
            basename += ".so";
        //no lib
        rawbasename = basename.substr(3, basename.size());
        lname = lname.substr(0, ipt + 1) + basename;
        rawlname = rawlname.substr(0, ipt + 1) + rawbasename;
    }
    else {
        if (lname.substr(0,3) == "lib")
            basename = lname;
        else
            basename = "lib" + lname;
        
        if (addso)
            basename += ".so";
        
        //no lib
        rawbasename = basename.substr(3, basename.size());
        lname = basename;
        rawlname = rawbasename;
    }
    
    lname = NormalizePathname(lname);
    rawlname = NormalizePathname(rawlname);

    string moduleinitname = "InitialisationModule";
    
    LibraryEntryPoint LibEntryPoint;

    nom_bib = lname;

    LoadMe = dlopen(lname.c_str(), RTLD_LAZY | RTLD_GLOBAL);
    if (LoadMe == NULL) {
        nom_bib = rawlname;
        LoadMe = dlopen(rawlname.c_str(), RTLD_LAZY | RTLD_GLOBAL);
    }
    
    string baselib;
    if (LoadMe == NULL) {
        string atanlib;
        if (getenv("LISPEPATH") != NULL)
            atanlib = getenv("LISPEPATH");
        else {
            stringstream message;
            message << "Error: The variable LISPEPATH has not been initialized." << endl;
            message << "LISPEPATH should point to the directory containing liblispe.so and your libraries:" << endl;
            message << "\texport LISPEPATH=the_path_to_the_libraries" << endl;
            throw new Error(message.str());
        }
        
        string ldlibpath;
#ifdef __apple_build_version__
        if (getenv("DYLD_LIBRARY_PATH") != NULL) {
            ldlibpath = getenv("DYLD_LIBRARY_PATH");
        }
        
        if (ldlibpath.find(atanlib) == -1) {
            ldlibpath = "/usr/local/lib:" + atanlib + ":" + ldlibpath;
            setenv("DYLD_LIBRARY_PATH", ldlibpath.c_str(), 1);
        }
#endif

        ldlibpath="";
        if (getenv("LD_LIBRARY_PATH") != NULL) {
            ldlibpath = getenv("LD_LIBRARY_PATH");
        }
        
        if (ldlibpath.find(atanlib) == -1) {
            ldlibpath = atanlib + ":" + ldlibpath;
            setenv("LD_LIBRARY_PATH", ldlibpath.c_str(), 1);
        }
                
        ldlibpath = "";
        if (getenv("PATH") != NULL) {
            ldlibpath = getenv("PATH");
        }
        
        if (ldlibpath.find(atanlib) == -1) {
            ldlibpath = atanlib + ":" + ldlibpath;
            setenv("PATH", ldlibpath.c_str(), 1);
        }

        if (atanlib.back() != '/')
            atanlib += "/";
        
        baselib = atanlib;
        atanlib += basename;
        atanlib = NormalizePathname(atanlib);
        nom_bib = atanlib;
        LoadMe = dlopen(STR(atanlib), RTLD_LAZY | RTLD_GLOBAL);
        if (LoadMe == NULL) {
            atanlib = baselib;
            atanlib += rawbasename;
            atanlib = NormalizePathname(atanlib);
            nom_bib = atanlib;
            LoadMe = dlopen(STR(atanlib), RTLD_LAZY | RTLD_GLOBAL);
        }
    }
    
    // Check to see if the library was loaded successfully
    if (LoadMe == NULL) {
        //We try without lib in the name
        error = dlerror();
        std::cerr << error << std::endl;
        baselib += nom_bib;
        LoadMe = dlopen(STR(baselib), RTLD_LAZY | RTLD_GLOBAL);
    }
    
    if (LoadMe == NULL) {
        error = dlerror();
        stringstream message;
        message << error << ": " << lname;
        throw new Error(message.str());
    }
    
    
    LibEntryPoint = (LibraryEntryPoint)dlsym(LoadMe, STR(moduleinitname));
    
    if ((error = dlerror()) != NULL) {
        stringstream message;
        message << error << ": " << name;
        throw new Error(message.str());
    }
        
    if ((*LibEntryPoint)(this) == false) {
        stringstream message;
        message << "Error: missing entry point" << name;
        throw new Error(message.str());
    }

    delegation->libraries[name] = true;
    return delegation->_TRUE;
}
#endif
//JSon

void split_container(u_uchar* src, long lensrc, vector<long>& pos) {
    u_uchar c;
    
    for (long e = 0; e < lensrc; e++) {
        c = src[e];
        if (strchr((char*)_tocheck, (char)c))
            pos.push_back(e);
    }
}

void replacemetas(u_ustring& sub) {
    static u_ustring search(U"\\");
    
    if (sub.find(search) == -1)
        return;
    
    u_ustring thestr;
    long sz = sub.size();
    for (long i=0;i<sz;i++) {
        if (sub[i]=='\\') {
            switch(sub[++i]) {
                case 'n':
                    thestr+=U"\n";
                    break;
                case 'r':
                    thestr+=U"\r";
                    break;
                case 't':
                    thestr+=U"\t";
                    break;
                default:
                    thestr+=sub[i];
                    
            }
        }
        else
            thestr+=sub[i];
    }
    sub = thestr;
}

bool LispEJsonCompiler::compile(LispE* lisp, u_ustring& s) {
    s = u_trim(s);    
    if (s[0] == '{')
        compiled_result = lisp->provideDictionary();
    else
        compiled_result = lisp->provideList();

    pos.clear();
    src = (u_uchar*)s.c_str();
    split_container(src, s.size(), pos);
    r = 1;
    i = 1;
    line = 0;
    sz = pos.size();
    
    if (!buildexpression(lisp, compiled_result) || r != pos.size()) {
        compiled_result->release();
        return false;
    }
    return true;
}

static void displaytoken(u_ustring& tok) {
    string stok;
    s_unicode_to_utf8(stok, tok);
    cerr << stok << endl;
}

char LispEJsonCompiler::buildexpression(LispE* lisp, Element* container) {
    bool checknext = false;
    to = 0;
    while (r < pos.size()) {
        c = src[i++];
        if (c <= 32) {
            if (c == '\n')
                line++;
            continue;
        }
        
        if (i != pos[r] + 1) {
            if (checknext)
                return false;
            
            to = i;
            while (src[to] > 32 && to < pos[r]) to++;
            c = src[to];
            src[to] = 0;
            token = (u_uchar*)src+i-1;
            if (token == U"false")
                container->append(false_);
            else {
                if (token == U"true")
                    container->append(true_);
                else {
                    if (token == U"null" || token == U"nil")
                        container->append(null_);
                    else
                        container->append(lisp, token);
                }
            }
            checknext = container->verify();
            src[to] = c;
            i = to;
            continue;
        }
        
        r++;
        switch (c) {
            case 34:
                if (checknext)
                    return false;
                
                while (r < sz) {
                    to  = pos[r++];
                    if (src[to-1] != '\\' && src[to] == '"')
                        break;
                }
                c= src[to];
                src[to] = 0;
                token = (u_uchar*)src+i;
                replacemetas(token);
                container->append(lisp, token);
                checknext = container->verify();
                src[to] = c;
                i = to + 1;
                break;
            case 39: {
                if (checknext)
                    return false;
                
                while (r < sz) {
                    to  = pos[r++];
                    if (src[to] == '\'')
                        break;
                }
                c= src[to];
                src[to] = 0;
                u_ustring locstr = (u_uchar*)src+i;
                container->append(lisp, locstr);
                
                checknext = container->verify();
                src[to] = c;
                i = to + 1;
                break;
            }
            case '+':
            case '-':
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                if (checknext)
                    return false;
                
                v = convertingfloathexa(src+i-1, l);
                to =  i + l - 1;
                while (pos[r] < to) r++;
                c = src[to];
                src[to] = 0;
                
                if (v == (long)v)
                    container->append(lisp, (long)v);
                else
                    container->append(lisp, v);
                
                checknext = container->verify();
                src[to] = c;
                i = to;
                break;
            case '{': {
                if (checknext)
                    return false;
                Dictionary* local = lisp->provideDictionary();
                if (src[i] == '}') {
                    r++;
                    i++;
                }
                else {
                    if (!buildexpression(lisp, local)) {
                        local->release();
                        return false;
                    }
                }
                container->append(local);
                checknext = container->verify();
                break;
            }
            case '[': {
                if (checknext)
                    return false;
                List* local = lisp->provideList();
                if (src[i] == ']') {
                    r++;
                    i++;
                }
                else {
                    if (!buildexpression(lisp, local)) {
                        local->release();
                        return false;
                    }
                }
                container->append(local);
                checknext=true;
                break;
            }
            case '}':
                return container->verify();
            case ']':
                if (container->isDictionary())
                    return false;
                return true;
            case ':':
                container->reversechoice();
                if (!container->isDictionary() || container->verify())
                    return false;
                checknext = false;
                break;
            case ',':
                if (!checknext)
                    return false;
                checknext = false;
        }
    }
    return false;
}


//Convert a unicode character into a utf16 character
Exporting bool c_utf16(u_uchar code) {
    //A unicode character is encoded over 4 bytes: 3 -> 0
    //if we have bits on byte 2, then we need to provide 4 bytes...
    return ((code & 0x1F0000));
}

//Convert a unicode character into a utf16 character
Exporting bool c_unicode_to_utf16(u_uchar& res, u_uchar code) {
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

Exporting bool c_utf16_to_unicode(u_uchar& r, u_uchar code, bool second) {
    if (second) {
        r |= code & 0x3FF;
        return false;
    }
    
    //if the first byte is  0xD8000000 then it is a four bytes coding
    if ((code & 0xFF00) == 0xD800) {
        //first we extract w
        r = ((((code & 0x03C0) >> 6) + 1) << 16) | ((code & 0x3F) << 10);
        return true;
    }
    
    //else r is code...
    r = code;
    return false;
}

void s_unicode_to_utf16(wstring& w, u_ustring& u) {
    w = L"";
    u_uchar c;
    u_uchar c16;
    
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

void s_unicode_to_utf16(std::u16string& w, u_ustring& u) {
    w.clear();
    u_uchar c;
    u_uchar c16;
    
    for (long i = 0; i < u.size(); i++) {
        c = u[i];
        if (!(c & 0xFFFF0000)) {
            w += (char16_t)c;
            continue;
        }
        
        c16 = 0xD800 | ((c & 0xFC00) >> 10) | ((((c & 0x1F0000) >> 16) - 1) << 6);
        w += (char16_t)c16;
        w += (char16_t)(0xDC00 | (c & 0x3FF));
    }
}

void s_utf16_to_unicode(u_ustring& u, wstring& w) {
    u = U"";
    
    u_uchar c;
    for (long i = 0; i < w.size(); i++) {
        if (c_utf16_to_unicode(c, w[i], false))
            c_utf16_to_unicode(c, w[++i], true);
        u += c;
    }
}

void s_utf16_to_unicode(u_ustring& u, int32_t* w, long sz) {
    u = U"";
    
    u_uchar c;
    for (long i = 0; i < sz; i++) {
        if (c_utf16_to_unicode(c, (u_uchar)w[i], false))
            c_utf16_to_unicode(c, (u_uchar)w[++i], true);
        u += c;
    }
}



#ifdef WIN32
Exporting wstring u_to_w(u_ustring u) {
    wstring w;
    
    u_uchar c;
    u_uchar c16;
    
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
    return w;
}

Exporting u_ustring w_to_u(wstring w) {
    u_ustring u;
    u_uchar c;
    for (long i = 0; i < w.size(); i++) {
        if (c_utf16_to_unicode(c, w[i], false))
            c_utf16_to_unicode(c, w[++i], true);
        u += c;
    }
    return u;
}

Exporting wstring _u_to_w(u_ustring& u) {
    wstring w;
    
    u_uchar c;
    u_uchar c16;
    
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
    return w;
}

Exporting u_ustring _w_to_u(wstring& w) {
    u_ustring u;
    u_uchar c;
    for (long i = 0; i < w.size(); i++) {
        if (c_utf16_to_unicode(c, w[i], false))
            c_utf16_to_unicode(c, w[++i], true);
        u += c;
    }
    return u;
}
#endif

//------------------------------------------------------------
//Automaton implementation for tokenization
//------------------------------------------------------------

void tokenizer_automaton::display(tokenizer_node* a, int nbblanks, bool top) {
    static std::set<int16_t> found;
    //We start from initial
    if (top) {
        found.clear();
    }
    //This node was already in our set
    bool ret = false;
    if (found.count(a->idpos))
        ret = true;
    else
        found.insert(a->idpos);
    
    string blanks(nbblanks, ' ');
    cout << blanks;
    
    if (a->check_negation())
        cout << "~";
    if (a->check_skip())
        cout << '-';
    if (a->check_start_fail()) {
        if (a->check_fail())
            cout << "<BE>";
        else
            cout << "<B>";
    }
    else {
        if (a->check_fail())
            cout << "<E>";
    }
    
    if (a->action == act_meta) {
        cout << '%';
    }
    
    if (a->action == act_end) {
        if (a->label < 33)
            cout << '$' << (int)a->label;
        else
            cout << '$' << (uchar)a->label;
    }
    else {
        if (!a->label) {
            if (a->action == act_any)
                cout << "ANY";
            else
                cout << "EPSILON";
        }
        else {
            if (a->label < 33)
                cout << (int)a->label;
            else
                cout << (uchar)a->label;
            if (a->endlabel) {
                if (a->endlabel < 33)
                    cout << "-" << (int)a->endlabel;
                else
                    cout << "-" << (uchar)a->endlabel;
            }
        }
    }
    
    if (ret && a->action != act_end) {
        cout << "_SEE_" << a->idpos << endl;
        return;
    }
    
    cout << '_' << a->idpos << endl;

    //we explore the subarcs to give them a position
    for (long i = 0; i < a->arcs.size(); i++) {
        display(a->arcs[i], nbblanks + 2, false);
    }
    if (top)
        cout << endl;
}

void tokenizer_automaton::asString(std::set<int16_t>& shared, std::wstringstream& str, tokenizer_node* a, int nbblanks, bool top) {
    //This node was already in our set
    bool ret = false;
    if (shared.count(a->idpos))
        ret = true;
    else
        shared.insert(a->idpos);
    
    wstring blanks(nbblanks, ' ');
    str << blanks;
    
    if (a->check_negation())
        str << "~";
    if (a->check_skip())
        str << '-';
    if (a->check_start_fail()) {
        if (a->check_fail())
            str << "<BE>";
        else
            str << "<B>";
    }
    else {
        if (a->check_fail())
            str << "<E>";
    }
    if (a->action == act_meta) {
        str << '%';
    }
    
    if (a->action == act_end) {
        if (a->label < 33)
            str << '$' << (int)a->label;
        else
            str << '$' << (uchar)a->label;
    }
    else {
        if (!a->label) {
            if (a->action == act_any)
                str << "ANY";
            else
                str << "EPSILON";
        }
        else {
            if (a->label < 33)
                str << (int)a->label;
            else
                str << (uchar)a->label;
            if (a->endlabel) {
                if (a->endlabel < 33)
                    str << "-" << (int)a->endlabel;
                else
                    str << "-" << (uchar)a->endlabel;
            }
        }
    }
    if (ret && a->action != act_end) {
        str << "_SEE_" << a->idpos << endl;
        return;
    }
    
    str << '_' << a->idpos << endl;

    //we explore the subarcs to give them a position
    for (long i = 0; i < a->arcs.size(); i++) {
        asString(shared, str, a->arcs[i], nbblanks + 2, false);
    }
    if (top)
        str << endl;
}

//------------------------------------------------------------
//The main function to check a character with a tokenize node
//------------------------------------------------------------

bool tokenizer_node::check(u_uchar chr, UTF8_Handler* access, std::set<u_uchar>& operators, bool not_neg) {
    if (!chr || check_blocking_gate())
        return false;
    
    switch(action) {
        case act_char:
            return (not_neg == (label == chr));
        case act_uppercase:
            return (not_neg == access->c_is_upper(chr));
        case act_ckj://Chinese/Japanese/Korean
            return (not_neg == ckjchar(chr));
        case act_space_and_carriage:
            return (not_neg == c_is_space_or_cr(chr));
        case act_alphabetical:
            return (not_neg == (chr=='_' || access->c_is_alpha(chr)));
        case act_lowercase:
            return (not_neg == access->c_is_lower(chr));
        case act_digit:
            return (not_neg == c_is_digit(chr));
        case act_greek:
            return (not_neg == (chr >= 913 && chr <= 987));
        case act_non_breaking_space: //non-breaking space
            return (not_neg == c_is_nbs_space(chr));
        case act_operator:
            return (not_neg == operators.count(chr));
        case act_punctuation:
            return (not_neg == access->c_is_punctuation(chr));
        case act_carriage:
            return (not_neg == (chr == 10 || chr == 13));
        case act_space:
            return (not_neg == c_is_space(chr));
        case act_interval:
            return (not_neg == (chr >= label && chr <= endlabel));
        case act_any:
        case act_epsilon:
            return true;
        case act_none:
        case act_end:
        case act_meta:
            return false;
        case act_emoji:
            return access->c_is_emoji(chr);
        case act_emojicomp:
            return access->c_is_emojicomp(chr);
    }
    return false;
}

//------------------------------------------------------------
//Cleaning epsilon nodes in tokenize automata
//------------------------------------------------------------

//These nodes have been detected as sub-nodes of themselves. Basically, it is a loop generated out of a Kleene operator (a loop)
void tokenizer_node::processing_postponed_nodes(std::set<long>& visited, vector<tokenizer_node*>& nodes, long idpos) {
    if (check_seen())
        return;
    
    add_flag(flags, flag_seen);
    
    long found = -1;
    for (long i = 0; i < arcs.size(); i++) {
        if (arcs[i]->idpos == idpos) {
            found = i;
        }
        else
            arcs[i]->processing_postponed_nodes(visited, nodes, idpos);
    }

    if (found != -1) {
        vector<tokenizer_node*> local;
        std::set<long> v;
        v.insert(idpos);
        for (long i = 0; i < arcs.size(); i++) {
            if (i == found) {
                for (auto& e : visited) {
                    if (v.count(e))
                        continue;
                    local.push_back(nodes[e]);
                    v.insert(e);
                }
            }
            else {
                if (!v.count(arcs[i]->idpos)) {
                    local.push_back(arcs[i]);
                    v.insert(arcs[i]->idpos);
                }
            }
        }
        arcs = local;
    }
}

//We are looking for epsilon nodes that can be merged...
//When we find an epsilon nodes, we go down in recursion to gather all existing nodes that are not epsilon down the way
//We then replace these epsilon nodes with these nodes.
//When we have an epsilon node that points to a termination node, this epsilon node becomes the new termination node.
void tokenizer_node::remove_epsilon_nodes(std::unordered_map<long,
                                          std::set<long> >& visited, std::set<long>& current,
                                          vector<tokenizer_node*>& nodes, bool epsilon) {
    if (check_visited())
        return;

        add_flag(flags, flag_visited);
    
    std::set<long> locals;
    std::set<long>* vect;
    tokenizer_node* n;
    long i;
    
    long sz = size();
    
    if (sz) {
        //We merge the arcs that share the same definition
        long key;
        std::unordered_map<long, tokenizer_node*> commons;
        vector<tokenizer_node*> nds;
        for (i = 0; i < sz; i++) {
            n = arcs[i];
            if (n->pure_arc()) {
                key = n->action*256 + n->label;
                if (commons.count(key)) {
                    tokenizer_node* nbase = commons[key];
                    for (key = 0; key < n->size(); key++)
                        nbase->arcs.push_back(n->arcs[key]);
                }
                else {
                    commons[key] = n;
                    nds.push_back(n);
                }
            }
            else
                nds.push_back(n);
        }
        //We have found some common arcs
        if (nds.size() != sz) {
            arcs.clear();
            arcs = nds;
            sz = arcs.size();
        }
    }

    if (epsilon)
        vect = &current;
    else
        vect = &locals;
    
    for (i = 0; i < sz; i++) {
        n = arcs[i];
        if (n->is_epsilon()) {
            if (n->size() == 1 && n->arcs[0]->action == act_end) {
                add_flag(n->arcs[0]->flags, flag_visited);
                n->clone(n->arcs[0]);
                n->arcs.clear();
                vect->insert(n->idpos);
            }
            else {
                if (n->check_visited()) {
                    if (visited.count(n->idpos)) {
                        for (auto& e: visited[n->idpos])
                            vect->insert(e);
                    }
                    else {
                        add_flag(n->flags, flag_postpone);
                        vect->insert(n->idpos);
                    }
                }
                else {
                    n->remove_epsilon_nodes(visited, *vect, nodes, true);
                    visited[n->idpos] = *vect;
                    if (n->check_postpone() && visited.count(n->idpos))
                        n->processing_postponed_nodes(visited[n->idpos], nodes, n->idpos);
                }
            }
        }
        else {
            vect->insert(n->idpos);
            n->remove_epsilon_nodes(visited, *vect, nodes, false);
        }
    }
    
    if (locals.size() != 0) {
        arcs.clear();
        for (auto& e : locals) {
            n = nodes[e];
            if (n->pure_arc())
                arcs.push_back(n);
        }
        
        for (auto& e : locals) {
            n = nodes[e];
            if (!n->pure_arc())
                arcs.push_back(n);
        }
        
        for (auto& e : locals) {
            n = nodes[e];
            if (n->check_postpone() && visited.count(n->idpos))
                processing_postponed_nodes(visited[n->idpos], nodes, n->idpos);
        }
    }
}

//----------------------------------------------------------------------------------------
//Specialization of extracting one character from a buffer for tokenizer_result
//tokenizer_result is a template that depends on the type of the string being manupulated.
//However each type has a specific expected behaviour
//----------------------------------------------------------------------------------------

template<> void tokenizer_result<u_ustring>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    sz_read = 1;
    line += (currentchr == '\n');
    currentchr = (*buffer)[position++];
}

template<> void tokenizer_result<wstring>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    sz_read = 1;
    line += (currentchr == '\n');
    if (c_utf16_to_unicode(currentchr, (*buffer)[position++], false)) {
        sz_read = 2;
        c_utf16_to_unicode(currentchr, (*buffer)[position++], true);
    }
}

template<> void tokenizer_result<u16string>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    sz_read = 1;
    line += (currentchr == '\n');
    if (c_utf16_to_unicode(currentchr, (*buffer)[position++], false)) {
        sz_read = 2;
        c_utf16_to_unicode(currentchr, (*buffer)[position++], true);
    }
}

template<> void tokenizer_result<string>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    line += (currentchr == '\n');
    sz_read = 1 + c_utf8_to_unicode(buffer, position, currentchr);
    position += sz_read;
}



template<> void tokenizer_result<u_ustring>::store_currentchr() {
    stack_ptr->push_back(u_ustring(1, currentchr));
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - 1);
    }
    start = position;
}

#ifdef WIN32
template<> void tokenizer_result<wstring>::store_currentchr() {
    u_uchar c;
    wstring w;
    if (c_unicode_to_utf16(c, currentchr)) {
        w = (wchar_t)(c >> 16);
        w += (wchar_t)(c & 0xFFFF);
    }
    else
        w = (wchar_t)c;

    
    stack_ptr->push_back(w);
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - sz_read);
    }
    start = position;
}
#else
template<> void tokenizer_result<wstring>::store_currentchr() {
    stack_ptr->push_back(wstring(1, currentchr));
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - 1);
    }
    start = position;

}
#endif

template<> void tokenizer_result<std::u16string>::store_currentchr() {
    u_uchar c;
    u16string w;
    if (c_unicode_to_utf16(c, currentchr)) {
        w = (char16_t)(c >> 16);
        w += (char16_t)(c & 0xFFFF);
    }
    else
        w = (char16_t)c;
        
    
    stack_ptr->push_back(w);
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - sz_read);
    }
    start = position;
}

template<> void tokenizer_result<string>::store_currentchr() {
    string w = c_unicode_to_utf8(currentchr);
    stack_ptr->push_back(w);
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - sz_read);
    }
    start = position;
}


template<> void tokenizer_result<u_ustring>::store(int32_t label) {
    if (label != -1) {
        stack_ptr->push_back(token);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - 1);
        }
    }
}

#ifdef WIN32
template<> void tokenizer_result<wstring>::store(int32_t label) {
    if (label != -1) {
        wstring w;
        s_unicode_to_utf16(w, token);
        stack_ptr->push_back(w);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - sz_read);
        }
    }
}
#else
template<> void tokenizer_result<wstring>::store(int32_t label) {
    if (label != -1) {
        stack_ptr->push_back((wstring&)token);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - 1);
        }
    }
}
#endif

template<> void tokenizer_result<u16string>::store(int32_t label) {
    if (label != -1) {
        u16string w;
        s_unicode_to_utf16(w, token);
        stack_ptr->push_back(w);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - sz_read);
        }
    }
}

template<> void tokenizer_result<string>::store(int32_t label) {
    if (label != -1) {
        string w;
        s_unicode_to_utf8(w, token);
        stack_ptr->push_back(w);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - sz_read);
        }
    }
}

//----------------------------------------------------------------------------------------
//Compiler for rules
//----------------------------------------------------------------------------------------

void tokenizer_automaton::compile() {
    /*
     
     The rules are compiled into tokenizer_node arcs and the results are stored in vectorization_table.
     This table contains: 1611 elements (see table_character_size).
     
     When a character code is < 1611, then the associated rules are stored in vectorization_table at character code location.
     For instance, if a rule starts with "A", then the rule will be stored at position 65 (ASCII code for "A")in vectorization_table.
     
     For rules that cannot be indexed, either because they start with an epsilon or because the initial character is > 1611, the
     rule is then stored at position 0.
     
     We have different potential action associated with an arc
     act_char             -> regular character
     act_meta             -> meta-character (%x)
     act_interval         -> interval between two characters: #A-F
     act_any              -> any character
     act_epsilon          -> an epsilon arc, which has created to handle disjunction or optionalaties
     act_end              -> the termination act, associated with a rule identifier, the returned value when the rule has applied
     
     We also have different flags to handle the node status
     flag_negation (1)        -> negation (~)
     flag_skip (2)            -> Character not stored (-)
     flag_fail (4)            -> Set to Element in negated disjunctions. When element is true, rule fails thank to this flag.
     flag_action (7)          -> flag_negation | flag_skip | flag_fail
     
     //The next flags are used to mark node utilisation
     flag_vectorized (8)   //has been added to vectorization_table as an index based on the first value of the rule
     flag_added (16)       //has been added to vectorization_table at position 0
     flag_postpone (32)    //While removing epsilon, a node that has already been seen but not fully evaluated since
     flag_visited (64)     //A node that has already been visited
     flag_seen (128)       //A node that has already been seen when processing postponed nodes
     
     -> IMPORTANT: for meta-characters such as %a or %d, we also use each possible value for these meta-characters as index in vectorization_table.
     */
    
    //The meta-characters for which there is a specific semantics
    char x_actions[]="CEHSacdehnoprs";
    
    map<u_ustring, u_ustring> metalines;
    vector<tokenizer_node*> brackets;
    vector<char> currentbracket;
    
    u_ustring line;
    u_ustring action_id;
    
    tokenizer_node* anode = NULL;
    tokenizer_node* current = NULL;
    tokenizer_node* root = NULL;
    
    u_uchar e;
    u_uchar cc;
    
    int32_t irule, final_action = -1;
    long pos;
    
    bool metarule_available = false;
    char error = 0;
    long begin_nodes;
    bool first;
    bool first_value;
    
    bool tail = false;
    
    for (irule = 0; irule < rules.size() && !error; irule++) {
        tail = false;
        line = rules[irule];
        begin_nodes = nodes.size();
        
        if (line[1]==':') { //we detect a meta-rule...
            cc = line[0];
            line=line.c_str()+2;
            action_id = U"%";
            action_id += cc;
            if (metarule_available)
                replacemetas(metalines,line);
            metalines[action_id] = line;
            metarule_available = true;
            compiled_rules.push_back(NULL);
            continue;
        }
        
        anode = NULL;
        current = NULL;
        disjunction = false;
        initial = NULL;
        root = NULL;
        //We need a dummy value in currentbracket
        currentbracket.clear();
        currentbracket.push_back('*');
        
        //first we look for the = sign at the end of the string...
        
        pos = line.rfind(U"=",line.size()-1);
        if (pos == -1)
            error = 1;
        else {
            // =: forces tail recursion
            if (line[pos + 1] == ':') {
                tail = true;
                pos++;
            }

            action_id = line.c_str()+pos+1;
            
            switch (action_id[0]) {
                case '!':
                    final_action = -2;
                    break;
                case '#':
                    final_action = -1;
                    break;
                default:
                    final_action = (int32_t)convertinginteger(action_id);
            }
            
            if (tail)
                line = line.substr(0,pos - 1);
            else
                line = line.substr(0,pos);

            //We have some meta rules
            if (metarule_available)
                replacemetas(metalines,line);
        }
        
        first = true;
        first_value = false;
        for (pos = 0; pos < line.size() && !error; pos++) {
            cc = line[pos];
            if (!currentbracket.size()) {
                error = 2;
                break;
            }
            
            switch(cc) {
                case '%':
                    //We record the first rule not starting with a character
                    cc = line[pos+1];
                    
                    if (first) {
                        //Note: current = append(current, anode) might seem redundant
                        //but it is important to call it when in a disjunction, as when
                        //the disjunction is the first element, we need to vectorize
                        //for each element that it contains. In that case, append will push
                        //the value in current and returns again current as the main root.
                        switch (cc) {
                            case 'C': //Upper-case characters
                                anode = node(act_meta, cc);
                                for (e = 'A'; e < table_character_size; e++) {
                                    if (access->c_is_upper(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'H':
                                anode = node(act_meta, cc);
                                current = append(current, anode);
                                break;
                            case 'E': //An emoji complement cannot start a rule
                                error = 3;
                                break;
                            case 'S': //table space or CR
                                anode = node(act_meta, cc);
                                vectorization(anode, 9);
                                vectorization(anode, 10);
                                vectorization(anode, 13);
                                vectorization(anode, 32);
                                vectorization(anode, 160);
                                vectorization(anode, 0); //for other non-breaking spaces
                                current = append(current, anode);
                                break;
                            case 'a': //Alphabetical characters
                                anode = node(act_meta, cc);
                                vectorization(anode, '_');
                                for (e = 'A'; e < table_character_size; e++) {
                                    if (access->c_is_alpha(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'c': //Lower-case characters
                                anode = node(act_meta, cc);
                                for (e = 'a'; e < table_character_size; e++) {
                                    if (access->c_is_lower(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'd':
                                anode = node(act_meta, cc);
                                for (e = '0'; e <= '9'; e++)
                                    vectorization(anode, e);
                                current = append(current, anode);
                                break;
                            case 'e':
                                anode = node(act_meta, cc);
                                for (auto& a: access->emojis_characters->utf32_arcs)
                                    vectorization(anode, a.first);
                                current = append(current, anode);
                                break;
                            case 'h': //Greek characters
                                anode = node(act_meta, cc);
                                for (e = 913; e <= 987; e++) {
                                    if (access->c_is_alpha(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'n':
                                anode = node(act_meta, cc);
                                vectorization(anode, 160);
                                //there are more than one non breaking space
                                vectorization(anode, 0);
                                current = append(current, anode);
                                break;
                            case 'o': {
                                anode = node(act_meta, cc);
                                for (const auto& a : operators)
                                    vectorization(anode, a);
                                current = append(current, anode);
                                break;
                            }
                            case 'p': {
                                anode = node(act_meta, cc);
                                for (const auto& a: access->vpunctuations)
                                    vectorization(anode, a);
                                current = append(current, anode);
                                break;
                            }
                            case 'r': //CR
                                anode = node(act_meta, cc);
                                vectorization(anode, 10);
                                vectorization(anode, 13);
                                current = append(current, anode);
                                break;
                            case 's': //tab or space or non-breaking spaces
                                anode = node(act_meta, cc);
                                vectorization(anode, 9);
                                vectorization(anode, 32);
                                vectorization(anode, 160);
                                vectorization(anode, 0); //for other non-breaking space characters
                                current = append(current, anode);
                                break;
                            default:
                                anode = node(act_char, cc);
                                vectorization(anode, cc);
                                current = append(current, anode);
                        }
                        pos++;
                        break;
                    }
                    
                    //This is a direct comparison or a meta
                    if (strchr(x_actions,cc) == NULL)
                        anode = node(act_char, cc);
                    else
                        anode = node(act_meta, cc);
                    
                    root = current;
                    current = append(current, anode);
                    pos++;
                    break;
                case '(':
                    //The first element of a rule can never be optional
                    if (first) {
                        currentbracket.push_back('^');
                        first_value = true;
                    }
                    
                    anode = node(act_epsilon, 0);
                    current = append(current, anode);
                    brackets.push_back(current);
                    currentbracket.push_back('(');
                    disjunction = false;
                    break;
                case ')':
                    if (brackets.size()) {
                        root = brackets.back();
                        brackets.pop_back();
                        anode = node(act_epsilon, 0);
                        root->append(anode);
                        current = append(current, anode);
                        currentbracket.pop_back();
                        first_value = false;
                        if (currentbracket.back() == '^') {
                            currentbracket.pop_back();
                            first_value = true;
                        }
                        disjunction = (currentbracket.back() == '{');
                        if (first_value) {
                            first_value = false;
                            first = true;
                            continue;
                        }
                    }
                    else
                        error = 4;
                    break;
                case '[': { //recording a sequence
                    //This is a sequence of elements
                    //If we are in a disjunction, then first_value might be true
                    //if this disjunction starts a rule.
                    //We add a specific marker to return to the right value
                    //once the structure analysis is completed.
                    if (first_value)
                        currentbracket.push_back('^');
                    currentbracket.push_back('[');
                    brackets.push_back(current);
                    current = node(act_epsilon, 0);
                    brackets.push_back(current);
                    disjunction = false;
                    break;
                }
                case ']':
                    if (brackets.size()) {
                        anode = brackets.back();
                        brackets.pop_back();
                        current = brackets.back();
                        brackets.pop_back();
                        if (current->check_negation()) {
                            //We mark the last node as a fail arc
                            current->fail_arcs();
                            anode = node(act_any, 0);
                            current->append(anode);
                        }
                        
                        root = current;
                        current->append(anode->arcs[0]);
                        currentbracket.pop_back();
                        if (currentbracket.back() == '^') {
                            //we were in a disjunction, we need to put first_value back to true
                            currentbracket.pop_back();
                            first_value = true;
                        }
                        disjunction = (currentbracket.back() == '{');
                    }
                    else
                        error = 5;
                    break;
                case '{':
                    anode = node(act_epsilon, 0);
                    disjunction = false;
                    current = append(current, anode);
                    brackets.push_back(current);
                    disjunction = true;
                    currentbracket.push_back('{');
                    //In this case, we need to dispatch elements to vectorize
                    if (first)
                        first_value = true;
                    break;
                case '}':
                    if (brackets.size()) {
                        first_value = false;
                        disjunction = false;
                        current = brackets.back();
                        brackets.pop_back();
                        root = current;
                        if (current->check_negation()) {
                            current->fail_arcs();
                            anode = node(act_any, 0);
                            current->append(anode);
                        }
                        
                        cc = line[pos+1];
                        if (cc != '+' && cc != '*') {
                            anode = node(act_epsilon, 0);
                            for (e = 0; e < current->size(); e++) {
                                current->arcs[e]->append_final(anode);
                            }
                            if (current->check_negation())
                                current->append(anode);
                            current = anode;
                        }
                        
                        currentbracket.pop_back();
                        disjunction = (currentbracket.back() == '{');
                    }
                    else
                        error = 6;
                    break;
                case '+':
                    if (pos) { //only if it is not the first character
                        //append_final appends first current, then anode
                        //on each final nodes in current (to take into account [...])
                        if (check_flag(current->action, act_epsilon)) {
                            anode = node(act_epsilon, 0);
                            for (e = 0; e < current->size(); e++) {
                                current->arcs[e]->append_final(current, anode);
                            }
                            if (current->arcs[0]->check_start_fail())
                                current->append(anode);
                        }
                        else {
                            anode = node(act_epsilon, 0);
                            current->append(current);
                            current = append(current, anode);
                        }
                        current = anode;
                        break;
                    }
                    else
                        error = 7;
                    break;
                case '*':
                    if (pos) { //only if it is not the first character
                        //append_final appends first current, then anode
                        //on each final nodes in current (to take into account [...])
                        //We need to add one more layer of node
                        if (root == NULL) {
                            error = 8;
                            break;
                        }
                        
                        if (check_flag(current->action, act_epsilon)) {
                            anode = node(act_epsilon, 0);
                            for (e = 0; e < current->size(); e++) {
                                current->arcs[e]->append_final(current, anode);
                            }
                            if (current->arcs[0]->check_start_fail())
                                current->append(anode);
                            current = anode;
                        }
                        else {
                            anode = node(act_epsilon, 0);
                            current->append(current);
                            current = append(current, anode);
                            current = anode;
                        }
                        //We add an arc to skip the whole structure since *
                        //also means 0 case...
                        root->append(anode);
                        break;
                    }
                    else
                        error = 9;
                    break;
                case '#': //introduce an interval or a code
                    //interval
                    if (line[pos+2] == '-') {
                        anode = node(act_interval, line[pos+1], line[pos+3]);
                        if (first) {
                            for (u_uchar c = line[pos+1]; c <= line[pos+3]; c++) {
                                vectorization(anode, c);
                            }
                        }
                        root = current;
                        current = append(current, anode);
                        pos += 3;
                    }
                    else {//code
                        e = 0;
                        if (isdigit(line[pos+1])) {
                            pos++;
                            e = line[pos++] - 48;
                            while (pos < line.size() && isdigit(line[pos])) {
                                e *= 10;
                                e += line[pos++] - 48;
                            }
                            pos--;
                            anode = node(act_char, e);
                            if (first)
                                vectorization(anode, e);
                            root = current;
                            current = append(current, anode);
                        }
                        else
                            error = 10;
                    }
                    break;
                case '-':
                    if (pos)
                        add_flag(current->flags, flag_skip);
                    else
                        error = 11;
                    break;
                case '~':
                    negation = flag_negation;
                    break;
                case '?':
                    anode = node(act_any, cc);
                    root = current;
                    current = append(current, anode);
                    break;
                case ' ': //in disjunction we separate each element with a space
                    if (currentbracket.back() == '{')
                        continue;
                default: {
                    //Elements are separated with blanks in disjunction
                    anode = node(act_char, cc);
                    if (first)
                        vectorization(anode , cc);
                    root = current;
                    current = append(current, anode);
                }
            }
            first = first_value;
            //In this case, we need to index on the first element
            //not the following one...
            if (first_value && (currentbracket.back() == '[' || currentbracket.back() == '('))
                first_value = false;
        }
        
        if (initial == NULL || error || brackets.size()) {
            u_ustring error_msg = U"Error in: ";
            error_msg += line;
            error_msg += U". Error is: ";
            switch (error) {
                case 1:
                    error_msg += U"missing '=' sign for label assignment.";
                    break;
                case 2:
                    error_msg += U"too many closing brackets.";
                    break;
                case 3:
                    error_msg += U"a rule cannot start with %E (emoji complement).";
                    break;
                case 4:
                    error_msg += U"Closing parenthesis ')' does not match open parenthesis '('.";
                    break;
                case 5:
                    error_msg += U"Closing bracket ']' does not match open bracket '['.";
                    break;
                case 6:
                    error_msg += U"Closing brace '}' does not match open brace '{'.";
                    break;
                case 7:
                    error_msg += U"'+' operator is not associated with any element.";
                    break;
                case 8:
                case 9:
                    error_msg += U"'*' operator is not associated with any element.";
                    break;
                case 10:
                    error_msg += U"'#' operator is not associated with a number or an interval.";
                    break;
                case 11:
                    error_msg += U"'-' operator is not associated with any element.";
                    break;
            }
            throw new Error(error_msg);
        }
        
        if (final_action == -2) {
            final_action = 0;
            if (action_id.size() > 1) {
                action_id = action_id.substr(1, action_id.size());
                final_action = (int32_t)convertinginteger(action_id);
            }
            add_flag(initial->flags, flag_blocking_gate);
        }
        
        anode = node(act_end, final_action);
        append(current, anode);
        
        if (displayautomata) {
            string s;
            s_unicode_to_utf8(s, line);
            cout << s << endl;
            display(initial, 0, true);
        }
        
        initial->trim_epsilon(nodes);
        if (final_action > 0)
            indexed_on_label[final_action].push_back(initial);

        if (tail) {
            anode = initial;
            while (anode->arcs.size() == 1)
                anode = anode->arcs[0];
            add_flag(anode->flags, flag_tail);
        }

        compiled_rules.push_back(initial);
        
        if (displayautomata) {
            display(initial, 0, true);
        }
    }
    
    //Cleaning useless nodes
    //Removing all marks except flag_negation, flag_skip and flag_fail
    for (auto& nd : nodes)
        nd->flags &= flag_action;
    
    //When there is only one rule in vectorization_table, no need to keep the epsilon
    vectorization_table[0]->mark_nodes();
    
    //the position 0 can never be removed, thus we start at 1
    for (e = 1; e < table_character_size; e++) {
        if (vectorization_table[e] != NULL) {
            if (vectorization_table[e]->size() == 1)
                vectorization_table[e] = vectorization_table[e]->arcs[0];
            vectorization_table[e]->mark_nodes();
        }
    }
    
    vector<tokenizer_node*> intermediary;
    for (auto& nd : nodes) {
        if (!nd->check_visited())
            delete nd;
        else {
            nd->idpos = intermediary.size();
            intermediary.push_back(nd);
        }
    }
    
    nodes = intermediary;
    loaded = true;
}

//----------------------------------------------------------------------------------------
//Rules for the LispE tokenizer
//----------------------------------------------------------------------------------------
void tokenizer_automaton::setrules() {
    /*
     a) A metarule is composed of two parts: c:expression, where c is the metacharacter that be accessed through %c and expression is a single body rule.
     
     for instance, we could have encoded %o as:
     rules.push_back(U"o:[≠ ∨ ∧ ÷ × ² ³ ¬]");
     
     IMPORTANT: These rules should be declared with one single operation.
     Their body will replace the call to a %c in other rules  (see the test on metas in the parse section)
     
     If you use a character that is already a meta-character (such as "a" or "d"), then the meta-character will be replaced with
     this new description... However, its content might still use the standard declaration:
     
     rules.push_back(U"a:{%a %d %p}"); "%1 is now a combination of alphabetical characters, digits and punctuations
     
     
     b) A rule is composed of two parts: body = action (action is either an integer or a #.)
     
     body uses the following instructions:
     
     x   is a character that should be recognized
     
     #x     comparison with character of code x...
     #x-y   comparison between x and y. x and y should be ascii characters...
     
     %x  is a meta-character with the following possibilities:
     
     ?  is any character
     %a  is any alphabetical character (including unicode ones such as éè)
     %C  is any uppercase character
     %c  is any lowercase character
     %d  is any digits
     %e  is an emoji
     %E  is an emoji complement (cannot start a rule)
     %h  is a Greek letter
     %H  is any hangul character
     %n  is a non-breaking space
     %o  is any operators
     %p  is any punctuations
     %r  is a carriage return both \n and \r
     %s  is a space (32) or a tab (09)
     %S  is both a carriage return or a space (%s or %r)
     
     %nn  you can create new metarules associated with any OTHER characters...
     
     
     (..) is a sequence of optional instructions
     [..] is a sequence of characters in a disjunction
     {..} is a disjunction of meta-characters
     x*   means that the instruction can be repeated zero or n times
     x+   means that the instruction can be repeated at least once
     x-   means that the character should be recognized but not stored in the parsing string
     ~..  means that all character will be recognized except for those in the list after the tilda.
     
     IMPORTANT: spaces are considered characters except in disjunctions
     
     Three more rules:
     
     ...=:NN -> '=:' in this context forces the evaluation to be handled iteratively, which avoids recursive explosion of the stack for very long strings
     ...=!NN -> '=!' means that the rule has been flagged with flag_blocking_gate. It is basically deactivated.
     ...=# -> the '#' means that the token will be recognized but not stored in the final list of tokens
     
     The flag_blocking_gate can be used to activate or deactivate rules on the fly
     */
    
    //Spaces, skipped in the parsing string
    rules.push_back(U"%s+=32");                    //space (kept)
    rules.push_back(U"#10+=10");                    //cr (kept)
    rules.push_back(U"#13=#");                      //lf (not kept)

    //Quoted ;
    rules.push_back(U"';+=59");                       //quote ;

    //Single quote
    rules.push_back(U"'=39");                       //quote

    rules.push_back(U"#27%[{%d;,}+m=#");           //Color definition in terminal

    rules.push_back(U".=46");                       //The list internal separator: '(a b . c)
    rules.push_back(U":=58");                       //Separator in dictionaries
    
    //The opening and closing characters in LispE
    rules.push_back(U"%(=40");
    rules.push_back(U"%)=41");
    
    rules.push_back(U"%[=91");
    rules.push_back(U"%]=93");
    
    rules.push_back(U"%{=123");
    rules.push_back(U"%}=125");
    
    //Comments
    rules.push_back(U";;?+;;=:67");                      //comments starting with ;;..;;
    rules.push_back(U";?+%r=67");                      //comments starting with ; up to the end of the line
    rules.push_back(U"%#!?+%r=67");                      //specific to Linux, calling function integrated in file

    //Strings
    rules.push_back(U"\"\"\"?*\"\"\"=:96");                   //long strings Python way """.."""
    rules.push_back(U"\"{[\\-\"] ~%r}*\"=:34");     //string "" does not contain CR and can escape characters
    rules.push_back(U"b\"{[\\-\"] ~%r}*\"=:34");     //string "" does not contain CR and can escape characters
    rules.push_back(U"`?*`=:96");                   //long strings Unix way
    rules.push_back(U"b`?*`=:97");                   //long strings Unix way
#ifdef WIN32
    //In Windows, strings are always stored as UTF-8 strings...
    //For this reason, we need to handle them through their code...
    rules.push_back(U"#171?*#187=:96");                   //long strings French way
    rules.push_back(U"#8220?*#8221=:96");                   //long strings English
    rules.push_back(U"#8216?*#8217=:96");                   //long strings with single quotes (English)
    rules.push_back(U"#8222?*#8221=:96");                //long strings German/Polish
    rules.push_back(U"#10077?*#10078=:96");                   //long strings
    rules.push_back(U"b#171?*#187=:96");                   //long strings French way
    rules.push_back(U"b#8220?*#8221=:96");                   //long strings English
    rules.push_back(U"b#8216?*#8217=:96");                   //long strings with single quotes (English)
    rules.push_back(U"b#8222?*#8221=:96");                //long strings German/Polish
    rules.push_back(U"b#10077?*#10078=:96");                   //long strings
#else
    rules.push_back(U"«?*»=:96");                   //long strings French way
    rules.push_back(U"“?*”=:96");                   //long strings English
    rules.push_back(U"‘?*’=:96");                   //long strings with single quotes (English)
    rules.push_back(U"„?*”=:96");                //long strings German/Polish
    rules.push_back(U"❝?*❞=:96");                   //long strings
    rules.push_back(U"b«?*»=:96");                   //long strings French way
    rules.push_back(U"b“?*”=:96");                   //long strings English
    rules.push_back(U"b‘?*’=:96");                   //long strings with single quotes (English)
    rules.push_back(U"b„?*”=:96");                //long strings German/Polish
    rules.push_back(U"b❝?*❞=:96");                   //long strings
#endif
    
    //The definition of a rule is divided into metarules for better clarity
    rules.push_back(U"N:{%- %+}");   //The exponential part of a hexadecimal number
    rules.push_back(U"F:{%d #A-F #a-f}");     //a hexadecimal character is one of these
    rules.push_back(U"P:{pP}(%N)%d+");   //The exponential part of a hexadecimal number
    rules.push_back(U"D:.%F+(%P)");           //The decimal part of a hexadecimal number
    rules.push_back(U"X:{eE}(%N)%d+");   //The exponential part of a decimal number
    rules.push_back(U"V:.%d+(%X)");          //the decimal part of a number
    rules.push_back(U"R:%d+(%V)");          //The real part
    rules.push_back(U"I:,(%N)(%R)i");  //Imaginary part of a number

    //Hexa decimal rules
    rules.push_back(U"%N0x%F+(%D)=57");  //hexadecimal: can handle 0x1.16bca4f9165dep-3
    rules.push_back(U"0x%F+(%D)=57");   //hexadecimal: can handle 0x1.16bca4f9165dep-3

    rules.push_back(U"%N0b{1 0}+=57");  //binaires: -0b11011
    rules.push_back(U"0b{1 0}+=57");  //binaires

    //regular numbers
    rules.push_back(U"%N%R(%I)=57"); //The second part after the "," is the imaginary part: 12.23,2i
    rules.push_back(U"%R(%I)=57");    //exponential digits
    
    rules.push_back(U"%%%d+=65");               //%132 is a token
    rules.push_back(U"%%{%o %p}=65");           //%. is a token
    rules.push_back(U"${%a %d}+=65");           //$abs23 is a token
    rules.push_back(U"%#{%a %d}+=65");          //#abs23 is a token
    
    rules.push_back(U"%o+=63");                 //operator
    rules.push_back(U"%e%E*=65");               //An emoji character
    rules.push_back(U"%H{%H %d %o}*=65");       //Asian characters (Chinese, Korean, Japanese)
    rules.push_back(U"%h{%h %d %o}*=65");       //Greek
    rules.push_back(U"%a{%a %d %o}*=65");       //Regular strings
    rules.push_back(U"%p=32");                  //punctuation
    rules.push_back(U"~{%S %o %p}+=65");           //Any combination of Unicode characters ending at a space or a punctuation
}

//----------------------------------------------------------------------------------------
//Rules for the segmenter tokenizer
//----------------------------------------------------------------------------------------

void segmenter_automaton::setrules() {
    rules.push_back(U"%S=!99");                                  //gated arcs to keep carriage or not
    rules.push_back(U"%S+=#");                                  //we skip all spaces
    
    //regular numbers
    rules.push_back(U"%d+{[rd][th][nd][er][ième][ieme]}=1");    //3rd or 4th
    rules.push_back(U"%d(%d)(:%d%d){[am][pm]}=1");              //American hours 6:30am, 9pm
    rules.push_back(U"%d(%d){:h}%d%d=1");                       //European hours 6h30, 21:00
    
    //We put this meta-rule here to avoid checking all the rules belows with it
    rules.push_back(U"1:{%d #A-F #a-f}");                    //metarule on 1, for hexadecimal digits
    
    //Tokenizing numbers
    //rules are either 22 or 44 never both
    //22 using a decimal point
    //66 using a decimal comma
    //88 using both
    //The order of the rules is important, if blocs of 3 are to be taken into account
    rules.push_back(U"{%- %+}%d+[,%d%d%d]+=22");          //digits separated with a , by block of 3
    rules.push_back(U"{%- %+}%d+[{.#32}%d%d%d]+=!66");    //digits separated with a . or a space by block of 3
    rules.push_back(U"{%- %+}%d+[{,.#32}%d%d%d]+=!88");
    rules.push_back(U"%d+[,%d%d%d]+=22");  //digits separated with a , by block of 3
    rules.push_back(U"%d+[{.#32}%d%d%d]+=!66"); //digits separated with a . or a space by block of 3
    rules.push_back(U"%d+[{,.#32}%d%d%d]+=!88");

    rules.push_back(U"{%- %+}0x%1+(.%1+({pP}({%- %+})%d+))=22");  //hexadecimal with a decimal point
    rules.push_back(U"{%- %+}0x%1+(,%1+({pP}({%- %+})%d+))=!66"); //Gated: hexadecimal with a decimal comma
    rules.push_back(U"{%- %+}0x%1+({,.}%1+({pP}({%- %+})%d+))=!88");
    rules.push_back(U"{%- %+}%d+(.%d+({eE}({%- %+})%d+))=22");    //Numbers with decimal point
    rules.push_back(U"{%- %+}%d+(,%d+({eE}({%- %+})%d+))=!66"); //Gated: Numbers with decimal comma
    rules.push_back(U"{%- %+}%d+({,.}%d+({eE}({%- %+})%d+))=!88");

    rules.push_back(U"0x%1+(.%1+({p P}({%- %+})%d+))=22");   //hexadecimal
    rules.push_back(U"0x%1+(,%1+({p P}({%- %+})%d+))=!66"); //Gated rules
    rules.push_back(U"0x%1+({,.}%1+({p P}({%- %+})%d+))=!88");
    rules.push_back(U"%d+(.%d+({eE}({%- %+})%d+))=22");      //digits
    rules.push_back(U"%d+(,%d+({eE}({%- %+})%d+))=!66"); //Gated rules
    rules.push_back(U"%d+({,.}%d+({eE}({%- %+})%d+))=!88");

    rules.push_back(U"{%- %+}0b{1 0}+=1");  //binaries
    rules.push_back(U"0b{1 0}+=1");  //binaires
    
    rules.push_back(U"%#{%a %d}+=1");       //Regular strings
    rules.push_back(U"${%a %d}+=1");       //Regular strings
    
    rules.push_back(U"http(s)://{%a %d . = %# & %? / %- %+}+=1");       //http
    rules.push_back(U"{%a %d . %- %+}+@{%a %d . = & %? %# %- %+}+=1");       //mail address
    
    rules.push_back(U"%o=1");                 //operator
    rules.push_back(U"%p=1");                  //punctuation
    rules.push_back(U"%e%E*=1");               //An emoji character
    rules.push_back(U"%H{%H %d}*=1");       //Asian characters (Chinese, Korean, Japanese)
    rules.push_back(U"%h{%h %- %d}*=1");       //Greek
    rules.push_back(U"%a{%a %- '’ %d}*=1");       //Regular strings
    rules.push_back(U"~{%S %o %p}+=1");           //Any combination of Unicode characters ending at a space or a punctuation
}

//------------------------------------------------------------------------
