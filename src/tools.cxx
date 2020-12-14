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

#ifdef WIN32
#include <io.h>
#else
#include <dlfcn.h>
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
    0x301D, 0x301F, 0x301C, 0xff1a, 0xff01, 0xff1f, 0x266a, 0, 0x43, 0x4C, 0x41, 0x55, 0x44,
    0x49, 0x55, 0x53, 0x52, 0x55, 0x46, 0x55, 0x53, 0x4C, 0x49, 0x53, 0x50, 0x45, 0x4D, 0x46,
    0x45, 0x43, 0x49, 0x54, 0x41, 0x4E,0x4E, 0x4F, 0x4D, 0x4D, 0x58, 0x58
};

static wchar_t codingtable[] = { 65,97,2,66,98,2,67,99,2,68,100,2,69,101,2,70,102,2,71,103,2,72,104,2,73,105,2,74,106,2,75,107,2,76,
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
    65262,1,65263,65263,1,65264,65264,1,65265,65265,1,65266,65266,1,65267,65267,1,65268,65268,1,0};

/*
 Some explanations:
 
 Emojis characters can be simply one unicode character such as: 😀 (0x1F600) or a sequence of unicode characters: 👋🏼 (0x1F44B, 0x1F3FC)
 We keep our emojis in two tables:
 One table with the emoji code associated to its definition in English. If the emoji is a sequence, then this code is the first element of the sequence
 Another table with the unicode characters that can complement an emoji into a sequence of codes
 */

static const char* temojis[] = { "0x1F004","mahjong red dragon","0x1F0CF","joker","0x1F170","A button (blood type)","0x1F171","B button (blood type)","0x1F17E","O button (blood type)",
    "0x1F17F","P button","0x1F18E","AB button (blood type)","0x1F191","CL button","0x1F192","COOL button","0x1F193","FREE button",
    "0x1F194","ID button","0x1F195","NEW button","0x1F196","NG button","0x1F197","OK button","0x1F198","SOS button",
    "0x1F199","UP! button","0x1F19A","VS button","0x1F1E6","flag: Ascension Island","0x1F1E7","flag: Bosnia & Herzegovina","0x1F1E8","flag: Canada",
    "0x1F1E9","flag: Germany","0x1F1EA","flag: Ceuta & Melilla","0x1F1EB","flag: Finland","0x1F1EC","flag: Gabon","0x1F1ED","flag: Hong Kong SAR China",
    "0x1F1EE","flag: Canary Islands","0x1F1EF","flag: Jersey","0x1F1F0","flag: Kenya","0x1F1F1","flag: Laos","0x1F1F2","flag: Morocco",
    "0x1F1F3","flag: Namibia","0x1F1F4","flag: Oman","0x1F1F5","flag: Panama","0x1F1F6","flag: Qatar","0x1F1F7","flag: Réunion",
    "0x1F1F8","flag: Saudi Arabia","0x1F1F9","flag: Tristan da Cunha","0x1F1FA","flag: Ukraine","0x1F1FB","flag: Vatican City","0x1F1FC","flag: Wallis & Futuna",
    "0x1F1FD","flag: Kosovo","0x1F1FE","flag: Yemen","0x1F1FF","flag: South Africa","0x1F201","Japanese here button","0x1F202","Japanese service charge button",
    "0x1F21A","Japanese free of charge button","0x1F22F","Japanese reserved button","0x1F232","Japanese prohibited button","0x1F233","Japanese vacancy button","0x1F234","Japanese passing grade button",
    "0x1F235","Japanese no vacancy button","0x1F236","Japanese not free of charge button","0x1F237","Japanese monthly amount button","0x1F238","Japanese application button","0x1F239","Japanese discount button",
    "0x1F23A","Japanese open for business button","0x1F250","Japanese bargain button","0x1F251","Japanese acceptable button","0x1F300","cyclone","0x1F301","foggy",
    "0x1F302","closed umbrella","0x1F303","night with stars","0x1F304","sunrise over mountains","0x1F305","sunrise","0x1F306","cityscape at dusk",
    "0x1F307","sunset","0x1F308","rainbow","0x1F309","bridge at night","0x1F30A","water wave","0x1F30B","volcano",
    "0x1F30C","milky way","0x1F30D","globe showing Europe-Africa","0x1F30E","globe showing Americas","0x1F30F","globe showing Asia-Australia","0x1F310","globe with meridians",
    "0x1F311","new moon","0x1F312","waxing crescent moon","0x1F313","first quarter moon","0x1F314","waxing gibbous moon","0x1F315","full moon",
    "0x1F316","waning gibbous moon","0x1F317","last quarter moon","0x1F318","waning crescent moon","0x1F319","crescent moon","0x1F31A","new moon face",
    "0x1F31B","first quarter moon face","0x1F31C","last quarter moon face","0x1F31D","full moon face","0x1F31E","sun with face","0x1F31F","glowing star",
    "0x1F320","shooting star","0x1F321","thermometer","0x1F324","sun behind small cloud","0x1F325","sun behind large cloud","0x1F326","sun behind rain cloud",
    "0x1F327","cloud with rain","0x1F328","cloud with snow","0x1F329","cloud with lightning","0x1F32A","tornado","0x1F32B","fog",
    "0x1F32C","wind face","0x1F32D","hot dog","0x1F32E","taco","0x1F32F","burrito","0x1F330","chestnut",
    "0x1F331","seedling","0x1F332","evergreen tree","0x1F333","deciduous tree","0x1F334","palm tree","0x1F335","cactus",
    "0x1F336","hot pepper","0x1F337","tulip","0x1F338","cherry blossom","0x1F339","rose","0x1F33A","hibiscus",
    "0x1F33B","sunflower","0x1F33C","blossom","0x1F33D","ear of corn","0x1F33E","sheaf of rice","0x1F33F","herb",
    "0x1F340","four leaf clover","0x1F341","maple leaf","0x1F342","fallen leaf","0x1F343","leaf fluttering in wind","0x1F344","mushroom",
    "0x1F345","tomato","0x1F346","eggplant","0x1F347","grapes","0x1F348","melon","0x1F349","watermelon",
    "0x1F34A","tangerine","0x1F34B","lemon","0x1F34C","banana","0x1F34D","pineapple","0x1F34E","red apple",
    "0x1F34F","green apple","0x1F350","pear","0x1F351","peach","0x1F352","cherries","0x1F353","strawberry",
    "0x1F354","hamburger","0x1F355","pizza","0x1F356","meat on bone","0x1F357","poultry leg","0x1F358","rice cracker",
    "0x1F359","rice ball","0x1F35A","cooked rice","0x1F35B","curry rice","0x1F35C","steaming bowl","0x1F35D","spaghetti",
    "0x1F35E","bread","0x1F35F","french fries","0x1F360","roasted sweet potato","0x1F361","dango","0x1F362","oden",
    "0x1F363","sushi","0x1F364","fried shrimp","0x1F365","fish cake with swirl","0x1F366","soft ice cream","0x1F367","shaved ice",
    "0x1F368","ice cream","0x1F369","doughnut","0x1F36A","cookie","0x1F36B","chocolate bar","0x1F36C","candy",
    "0x1F36D","lollipop","0x1F36E","custard","0x1F36F","honey pot","0x1F370","shortcake","0x1F371","bento box",
    "0x1F372","pot of food","0x1F373","cooking","0x1F374","fork and knife","0x1F375","teacup without handle","0x1F376","sake",
    "0x1F377","wine glass","0x1F378","cocktail glass","0x1F379","tropical drink","0x1F37A","beer mug","0x1F37B","clinking beer mugs",
    "0x1F37C","baby bottle","0x1F37D","fork and knife with plate","0x1F37E","bottle with popping cork","0x1F37F","popcorn","0x1F380","ribbon",
    "0x1F381","wrapped gift","0x1F382","birthday cake","0x1F383","jack-o-lantern","0x1F384","Christmas tree","0x1F385","Santa Claus",
    "0x1F386","fireworks","0x1F387","sparkler","0x1F388","balloon","0x1F389","party popper","0x1F38A","confetti ball",
    "0x1F38B","tanabata tree","0x1F38C","crossed flags","0x1F38D","pine decoration","0x1F38E","Japanese dolls","0x1F38F","carp streamer",
    "0x1F390","wind chime","0x1F391","moon viewing ceremony","0x1F392","backpack","0x1F393","graduation cap","0x1F396","military medal",
    "0x1F397","reminder ribbon","0x1F399","studio microphone","0x1F39A","level slider","0x1F39B","control knobs","0x1F39E","film frames",
    "0x1F39F","admission tickets","0x1F3A0","carousel horse","0x1F3A1","ferris wheel","0x1F3A2","roller coaster","0x1F3A3","fishing pole",
    "0x1F3A4","microphone","0x1F3A5","movie camera","0x1F3A6","cinema","0x1F3A7","headphone","0x1F3A8","artist palette",
    "0x1F3A9","top hat","0x1F3AA","circus tent","0x1F3AB","ticket","0x1F3AC","clapper board","0x1F3AD","performing arts",
    "0x1F3AE","video game","0x1F3AF","direct hit","0x1F3B0","slot machine","0x1F3B1","pool 8 ball","0x1F3B2","game die",
    "0x1F3B3","bowling","0x1F3B4","flower playing cards","0x1F3B5","musical note","0x1F3B6","musical notes","0x1F3B7","saxophone",
    "0x1F3B8","guitar","0x1F3B9","musical keyboard","0x1F3BA","trumpet","0x1F3BB","violin","0x1F3BC","musical score",
    "0x1F3BD","running shirt","0x1F3BE","tennis","0x1F3BF","skis","0x1F3C0","basketball","0x1F3C1","chequered flag",
    "0x1F3C2","snowboarder","0x1F3C3","person running","0x1F3C4","person surfing","0x1F3C5","sports medal","0x1F3C6","trophy",
    "0x1F3C7","horse racing","0x1F3C8","american football","0x1F3C9","rugby football","0x1F3CA","person swimming","0x1F3CB","person lifting weights",
    "0x1F3CC","person golfing","0x1F3CD","motorcycle","0x1F3CE","racing car","0x1F3CF","cricket game","0x1F3D0","volleyball",
    "0x1F3D1","field hockey","0x1F3D2","ice hockey","0x1F3D3","ping pong","0x1F3D4","snow-capped mountain","0x1F3D5","camping",
    "0x1F3D6","beach with umbrella","0x1F3D7","building construction","0x1F3D8","houses","0x1F3D9","cityscape","0x1F3DA","derelict house",
    "0x1F3DB","classical building","0x1F3DC","desert","0x1F3DD","desert island","0x1F3DE","national park","0x1F3DF","stadium",
    "0x1F3E0","house","0x1F3E1","house with garden","0x1F3E2","office building","0x1F3E3","Japanese post office","0x1F3E4","post office",
    "0x1F3E5","hospital","0x1F3E6","bank","0x1F3E7","ATM sign","0x1F3E8","hotel","0x1F3E9","love hotel",
    "0x1F3EA","convenience store","0x1F3EB","school","0x1F3EC","department store","0x1F3ED","factory","0x1F3EE","red paper lantern",
    "0x1F3EF","Japanese castle","0x1F3F0","castle","0x1F3F3","white flag","0x1F3F4","black flag","0x1F3F5","rosette",
    "0x1F3F7","label","0x1F3F8","badminton","0x1F3F9","bow and arrow","0x1F3FA","amphora","0x1F3FB","light skin tone",
    "0x1F3FC","medium-light skin tone","0x1F3FD","medium skin tone","0x1F3FE","medium-dark skin tone","0x1F3FF","dark skin tone","0x1F400","rat",
    "0x1F401","mouse","0x1F402","ox","0x1F403","water buffalo","0x1F404","cow","0x1F405","tiger",
    "0x1F406","leopard","0x1F407","rabbit","0x1F408","cat","0x1F409","dragon","0x1F40A","crocodile",
    "0x1F40B","whale","0x1F40C","snail","0x1F40D","snake","0x1F40E","horse","0x1F40F","ram",
    "0x1F410","goat","0x1F411","ewe","0x1F412","monkey","0x1F413","rooster","0x1F414","chicken",
    "0x1F415","dog","0x1F416","pig","0x1F417","boar","0x1F418","elephant","0x1F419","octopus",
    "0x1F41A","spiral shell","0x1F41B","bug","0x1F41C","ant","0x1F41D","honeybee","0x1F41E","lady beetle",
    "0x1F41F","fish","0x1F420","tropical fish","0x1F421","blowfish","0x1F422","turtle","0x1F423","hatching chick",
    "0x1F424","baby chick","0x1F425","front-facing baby chick","0x1F426","bird","0x1F427","penguin","0x1F428","koala",
    "0x1F429","poodle","0x1F42A","camel","0x1F42B","two-hump camel","0x1F42C","dolphin","0x1F42D","mouse face",
    "0x1F42E","cow face","0x1F42F","tiger face","0x1F430","rabbit face","0x1F431","cat face","0x1F432","dragon face",
    "0x1F433","spouting whale","0x1F434","horse face","0x1F435","monkey face","0x1F436","dog face","0x1F437","pig face",
    "0x1F438","frog","0x1F439","hamster","0x1F43A","wolf","0x1F43B","bear","0x1F43C","panda",
    "0x1F43D","pig nose","0x1F43E","paw prints","0x1F43F","chipmunk","0x1F440","eyes","0x1F441","eye in speech bubble",
    "0x1F442","ear","0x1F443","nose","0x1F444","mouth","0x1F445","tongue","0x1F446","backhand index pointing up",
    "0x1F447","backhand index pointing down","0x1F448","backhand index pointing left","0x1F449","backhand index pointing right","0x1F44A","oncoming fist","0x1F44B","waving hand",
    "0x1F44C","OK hand","0x1F44D","thumbs up","0x1F44E","thumbs down","0x1F44F","clapping hands","0x1F450","open hands",
    "0x1F451","crown","0x1F452","womans hat","0x1F453","glasses","0x1F454","necktie","0x1F455","t-shirt",
    "0x1F456","jeans","0x1F457","dress","0x1F458","kimono","0x1F459","bikini","0x1F45A","womans clothes",
    "0x1F45B","purse","0x1F45C","handbag","0x1F45D","clutch bag","0x1F45E","mans shoe","0x1F45F","running shoe",
    "0x1F460","high-heeled shoe","0x1F461","womans sandal","0x1F462","womans boot","0x1F463","footprints","0x1F464","bust in silhouette",
    "0x1F465","busts in silhouette","0x1F466","boy","0x1F467","girl","0x1F468","man","0x1F469","woman",
    "0x1F46A","family","0x1F46B","woman and man holding hands","0x1F46C","men holding hands","0x1F46D","women holding hands","0x1F46E","police officer",
    "0x1F46F","people with bunny ears","0x1F470","bride with veil","0x1F471","person: blond hair","0x1F472","man with Chinese cap","0x1F473","person wearing turban",
    "0x1F474","old man","0x1F475","old woman","0x1F476","baby","0x1F477","construction worker","0x1F478","princess",
    "0x1F479","ogre","0x1F47A","goblin","0x1F47B","ghost","0x1F47C","baby angel","0x1F47D","alien",
    "0x1F47E","alien monster","0x1F47F","angry face with horns","0x1F480","skull","0x1F481","person tipping hand","0x1F482","guard",
    "0x1F483","woman dancing","0x1F484","lipstick","0x1F485","nail polish","0x1F486","person getting massage","0x1F487","person getting haircut",
    "0x1F488","barber pole","0x1F489","syringe","0x1F48A","pill","0x1F48B","kiss mark","0x1F48C","love letter",
    "0x1F48D","ring","0x1F48E","gem stone","0x1F48F","kiss","0x1F490","bouquet","0x1F491","couple with heart",
    "0x1F492","wedding","0x1F493","beating heart","0x1F494","broken heart","0x1F495","two hearts","0x1F496","sparkling heart",
    "0x1F497","growing heart","0x1F498","heart with arrow","0x1F499","blue heart","0x1F49A","green heart","0x1F49B","yellow heart",
    "0x1F49C","purple heart","0x1F49D","heart with ribbon","0x1F49E","revolving hearts","0x1F49F","heart decoration","0x1F4A0","diamond with a dot",
    "0x1F4A1","light bulb","0x1F4A2","anger symbol","0x1F4A3","bomb","0x1F4A4","zzz","0x1F4A5","collision",
    "0x1F4A6","sweat droplets","0x1F4A7","droplet","0x1F4A8","dashing away","0x1F4A9","pile of poo","0x1F4AA","flexed biceps",
    "0x1F4AB","dizzy","0x1F4AC","speech balloon","0x1F4AD","thought balloon","0x1F4AE","white flower","0x1F4AF","hundred points",
    "0x1F4B0","money bag","0x1F4B1","currency exchange","0x1F4B2","heavy dollar sign","0x1F4B3","credit card","0x1F4B4","yen banknote",
    "0x1F4B5","dollar banknote","0x1F4B6","euro banknote","0x1F4B7","pound banknote","0x1F4B8","money with wings","0x1F4B9","chart increasing with yen",
    "0x1F4BA","seat","0x1F4BB","laptop computer","0x1F4BC","briefcase","0x1F4BD","computer disk","0x1F4BE","floppy disk",
    "0x1F4BF","optical disk","0x1F4C0","dvd","0x1F4C1","file folder","0x1F4C2","open file folder","0x1F4C3","page with curl",
    "0x1F4C4","page facing up","0x1F4C5","calendar","0x1F4C6","tear-off calendar","0x1F4C7","card index","0x1F4C8","chart increasing",
    "0x1F4C9","chart decreasing","0x1F4CA","bar chart","0x1F4CB","clipboard","0x1F4CC","pushpin","0x1F4CD","round pushpin",
    "0x1F4CE","paperclip","0x1F4CF","straight ruler","0x1F4D0","triangular ruler","0x1F4D1","bookmark tabs","0x1F4D2","ledger",
    "0x1F4D3","notebook","0x1F4D4","notebook with decorative cover","0x1F4D5","closed book","0x1F4D6","open book","0x1F4D7","green book",
    "0x1F4D8","blue book","0x1F4D9","orange book","0x1F4DA","books","0x1F4DB","name badge","0x1F4DC","scroll",
    "0x1F4DD","memo","0x1F4DE","telephone receiver","0x1F4DF","pager","0x1F4E0","fax machine","0x1F4E1","satellite antenna",
    "0x1F4E2","loudspeaker","0x1F4E3","megaphone","0x1F4E4","outbox tray","0x1F4E5","inbox tray","0x1F4E6","package",
    "0x1F4E7","e-mail","0x1F4E8","incoming envelope","0x1F4E9","envelope with arrow","0x1F4EA","closed mailbox with lowered flag","0x1F4EB","closed mailbox with raised flag",
    "0x1F4EC","open mailbox with raised flag","0x1F4ED","open mailbox with lowered flag","0x1F4EE","postbox","0x1F4EF","postal horn","0x1F4F0","newspaper",
    "0x1F4F1","mobile phone","0x1F4F2","mobile phone with arrow","0x1F4F3","vibration mode","0x1F4F4","mobile phone off","0x1F4F5","no mobile phones",
    "0x1F4F6","antenna bars","0x1F4F7","camera","0x1F4F8","camera with flash","0x1F4F9","video camera","0x1F4FA","television",
    "0x1F4FB","radio","0x1F4FC","videocassette","0x1F4FD","film projector","0x1F4FF","prayer beads","0x1F500","shuffle tracks button",
    "0x1F501","repeat button","0x1F502","repeat single button","0x1F503","clockwise vertical arrows","0x1F504","counterclockwise arrows button","0x1F505","dim button",
    "0x1F506","bright button","0x1F507","muted speaker","0x1F508","speaker low volume","0x1F509","speaker medium volume","0x1F50A","speaker high volume",
    "0x1F50B","battery","0x1F50C","electric plug","0x1F50D","magnifying glass tilted left","0x1F50E","magnifying glass tilted right","0x1F50F","locked with pen",
    "0x1F510","locked with key","0x1F511","key","0x1F512","locked","0x1F513","unlocked","0x1F514","bell",
    "0x1F515","bell with slash","0x1F516","bookmark","0x1F517","link","0x1F518","radio button","0x1F519","BACK arrow",
    "0x1F51A","END arrow","0x1F51B","ON! arrow","0x1F51C","SOON arrow","0x1F51D","TOP arrow","0x1F51E","no one under eighteen",
    "0x1F51F","keycap: 10","0x1F520","input latin uppercase","0x1F521","input latin lowercase","0x1F522","input numbers","0x1F523","input symbols",
    "0x1F524","input latin letters","0x1F525","fire","0x1F526","flashlight","0x1F527","wrench","0x1F528","hammer",
    "0x1F529","nut and bolt","0x1F52A","kitchen knife","0x1F52B","pistol","0x1F52C","microscope","0x1F52D","telescope",
    "0x1F52E","crystal ball","0x1F52F","dotted six-pointed star","0x1F530","Japanese symbol for beginner","0x1F531","trident emblem","0x1F532","black square button",
    "0x1F533","white square button","0x1F534","red circle","0x1F535","blue circle","0x1F536","large orange diamond","0x1F537","large blue diamond",
    "0x1F538","small orange diamond","0x1F539","small blue diamond","0x1F53A","red triangle pointed up","0x1F53B","red triangle pointed down","0x1F53C","upwards button",
    "0x1F53D","downwards button","0x1F549","om","0x1F54A","dove","0x1F54B","kaaba","0x1F54C","mosque",
    "0x1F54D","synagogue","0x1F54E","menorah","0x1F550","one oclock","0x1F551","two oclock","0x1F552","three oclock",
    "0x1F553","four oclock","0x1F554","five oclock","0x1F555","six oclock","0x1F556","seven oclock","0x1F557","eight oclock",
    "0x1F558","nine oclock","0x1F559","ten oclock","0x1F55A","eleven oclock","0x1F55B","twelve oclock","0x1F55C","one-thirty",
    "0x1F55D","two-thirty","0x1F55E","three-thirty","0x1F55F","four-thirty","0x1F560","five-thirty","0x1F561","six-thirty",
    "0x1F562","seven-thirty","0x1F563","eight-thirty","0x1F564","nine-thirty","0x1F565","ten-thirty","0x1F566","eleven-thirty",
    "0x1F567","twelve-thirty","0x1F56F","candle","0x1F570","mantelpiece clock","0x1F573","hole","0x1F574","man in suit levitating",
    "0x1F575","detective","0x1F576","sunglasses","0x1F577","spider","0x1F578","spider web","0x1F579","joystick",
    "0x1F57A","man dancing","0x1F587","linked paperclips","0x1F58A","pen","0x1F58B","fountain pen","0x1F58C","paintbrush",
    "0x1F58D","crayon","0x1F590","hand with fingers splayed","0x1F595","middle finger","0x1F596","vulcan salute","0x1F5A4","black heart",
    "0x1F5A5","desktop computer","0x1F5A8","printer","0x1F5B1","computer mouse","0x1F5B2","trackball","0x1F5BC","framed picture",
    "0x1F5C2","card index dividers","0x1F5C3","card file box","0x1F5C4","file cabinet","0x1F5D1","wastebasket","0x1F5D2","spiral notepad",
    "0x1F5D3","spiral calendar","0x1F5DC","clamp","0x1F5DD","old key","0x1F5DE","rolled-up newspaper","0x1F5E1","dagger",
    "0x1F5E3","speaking head","0x1F5E8","left speech bubble","0x1F5EF","right anger bubble","0x1F5F3","ballot box with ballot","0x1F5FA","world map",
    "0x1F5FB","mount fuji","0x1F5FC","Tokyo tower","0x1F5FD","Statue of Liberty","0x1F5FE","map of Japan","0x1F5FF","moai",
    "0x1F600","grinning face","0x1F601","beaming face with smiling eyes","0x1F602","face with tears of joy","0x1F603","grinning face with big eyes","0x1F604","grinning face with smiling eyes",
    "0x1F605","grinning face with sweat","0x1F606","grinning squinting face","0x1F607","smiling face with halo","0x1F608","smiling face with horns","0x1F609","winking face",
    "0x1F60A","smiling face with smiling eyes","0x1F60B","face savoring food","0x1F60C","relieved face","0x1F60D","smiling face with heart-eyes","0x1F60E","smiling face with sunglasses",
    "0x1F60F","smirking face","0x1F610","neutral face","0x1F611","expressionless face","0x1F612","unamused face","0x1F613","downcast face with sweat",
    "0x1F614","pensive face","0x1F615","confused face","0x1F616","confounded face","0x1F617","kissing face","0x1F618","face blowing a kiss",
    "0x1F619","kissing face with smiling eyes","0x1F61A","kissing face with closed eyes","0x1F61B","face with tongue","0x1F61C","winking face with tongue","0x1F61D","squinting face with tongue",
    "0x1F61E","disappointed face","0x1F61F","worried face","0x1F620","angry face","0x1F621","pouting face","0x1F622","crying face",
    "0x1F623","persevering face","0x1F624","face with steam from nose","0x1F625","sad but relieved face","0x1F626","frowning face with open mouth","0x1F627","anguished face",
    "0x1F628","fearful face","0x1F629","weary face","0x1F62A","sleepy face","0x1F62B","tired face","0x1F62C","grimacing face",
    "0x1F62D","loudly crying face","0x1F62E","face with open mouth","0x1F62F","hushed face","0x1F630","anxious face with sweat","0x1F631","face screaming in fear",
    "0x1F632","astonished face","0x1F633","flushed face","0x1F634","sleeping face","0x1F635","dizzy face","0x1F636","face without mouth",
    "0x1F637","face with medical mask","0x1F638","grinning cat with smiling eyes","0x1F639","cat with tears of joy","0x1F63A","grinning cat","0x1F63B","smiling cat with heart-eyes",
    "0x1F63C","cat with wry smile","0x1F63D","kissing cat","0x1F63E","pouting cat","0x1F63F","crying cat","0x1F640","weary cat",
    "0x1F641","slightly frowning face","0x1F642","slightly smiling face","0x1F643","upside-down face","0x1F644","face with rolling eyes","0x1F645","person gesturing NO",
    "0x1F646","person gesturing OK","0x1F647","person bowing","0x1F648","see-no-evil monkey","0x1F649","hear-no-evil monkey","0x1F64A","speak-no-evil monkey",
    "0x1F64B","person raising hand","0x1F64C","raising hands","0x1F64D","person frowning","0x1F64E","person pouting","0x1F64F","folded hands",
    "0x1F680","rocket","0x1F681","helicopter","0x1F682","locomotive","0x1F683","railway car","0x1F684","high-speed train",
    "0x1F685","bullet train","0x1F686","train","0x1F687","metro","0x1F688","light rail","0x1F689","station",
    "0x1F68A","tram","0x1F68B","tram car","0x1F68C","bus","0x1F68D","oncoming bus","0x1F68E","trolleybus",
    "0x1F68F","bus stop","0x1F690","minibus","0x1F691","ambulance","0x1F692","fire engine","0x1F693","police car",
    "0x1F694","oncoming police car","0x1F695","taxi","0x1F696","oncoming taxi","0x1F697","automobile","0x1F698","oncoming automobile",
    "0x1F699","sport utility vehicle","0x1F69A","delivery truck","0x1F69B","articulated lorry","0x1F69C","tractor","0x1F69D","monorail",
    "0x1F69E","mountain railway","0x1F69F","suspension railway","0x1F6A0","mountain cableway","0x1F6A1","aerial tramway","0x1F6A2","ship",
    "0x1F6A3","person rowing boat","0x1F6A4","speedboat","0x1F6A5","horizontal traffic light","0x1F6A6","vertical traffic light","0x1F6A7","construction",
    "0x1F6A8","police car light","0x1F6A9","triangular flag","0x1F6AA","door","0x1F6AB","prohibited","0x1F6AC","cigarette",
    "0x1F6AD","no smoking","0x1F6AE","litter in bin sign","0x1F6AF","no littering","0x1F6B0","potable water","0x1F6B1","non-potable water",
    "0x1F6B2","bicycle","0x1F6B3","no bicycles","0x1F6B4","person biking","0x1F6B5","person mountain biking","0x1F6B6","person walking",
    "0x1F6B7","no pedestrians","0x1F6B8","children crossing","0x1F6B9","mens room","0x1F6BA","womens room","0x1F6BB","restroom",
    "0x1F6BC","baby symbol","0x1F6BD","toilet","0x1F6BE","water closet","0x1F6BF","shower","0x1F6C0","person taking bath",
    "0x1F6C1","bathtub","0x1F6C2","passport control","0x1F6C3","customs","0x1F6C4","baggage claim","0x1F6C5","left luggage",
    "0x1F6CB","couch and lamp","0x1F6CC","person in bed","0x1F6CD","shopping bags","0x1F6CE","bellhop bell","0x1F6CF","bed",
    "0x1F6D0","place of worship","0x1F6D1","stop sign","0x1F6D2","shopping cart","0x1F6D5","hindu temple","0x1F6E0","hammer and wrench",
    "0x1F6E1","shield","0x1F6E2","oil drum","0x1F6E3","motorway","0x1F6E4","railway track","0x1F6E5","motor boat",
    "0x1F6E9","small airplane","0x1F6EB","airplane departure","0x1F6EC","airplane arrival","0x1F6F0","satellite","0x1F6F3","passenger ship",
    "0x1F6F4","kick scooter","0x1F6F5","motor scooter","0x1F6F6","canoe","0x1F6F7","sled","0x1F6F8","flying saucer",
    "0x1F6F9","skateboard","0x1F6FA","auto rickshaw","0x1F7E0","orange circle","0x1F7E1","yellow circle","0x1F7E2","green circle",
    "0x1F7E3","purple circle","0x1F7E4","brown circle","0x1F7E5","red square","0x1F7E6","blue square","0x1F7E7","orange square",
    "0x1F7E8","yellow square","0x1F7E9","green square","0x1F7EA","purple square","0x1F7EB","brown square","0x1F90D","white heart",
    "0x1F90E","brown heart","0x1F90F","pinching hand","0x1F910","zipper-mouth face","0x1F911","money-mouth face","0x1F912","face with thermometer",
    "0x1F913","nerd face","0x1F914","thinking face","0x1F915","face with head-bandage","0x1F916","robot","0x1F917","hugging face",
    "0x1F918","sign of the horns","0x1F919","call me hand","0x1F91A","raised back of hand","0x1F91B","left-facing fist","0x1F91C","right-facing fist",
    "0x1F91D","handshake","0x1F91E","crossed fingers","0x1F91F","love-you gesture","0x1F920","cowboy hat face","0x1F921","clown face",
    "0x1F922","nauseated face","0x1F923","rolling on the floor laughing","0x1F924","drooling face","0x1F925","lying face","0x1F926","person facepalming",
    "0x1F927","sneezing face","0x1F928","face with raised eyebrow","0x1F929","star-struck","0x1F92A","zany face","0x1F92B","shushing face",
    "0x1F92C","face with symbols on mouth","0x1F92D","face with hand over mouth","0x1F92E","face vomiting","0x1F92F","exploding head","0x1F930","pregnant woman",
    "0x1F931","breast-feeding","0x1F932","palms up together","0x1F933","selfie","0x1F934","prince","0x1F935","man in tuxedo",
    "0x1F936","Mrs. Claus","0x1F937","person shrugging","0x1F938","person cartwheeling","0x1F939","person juggling","0x1F93A","person fencing",
    "0x1F93C","people wrestling","0x1F93D","person playing water polo","0x1F93E","person playing handball","0x1F93F","diving mask","0x1F940","wilted flower",
    "0x1F941","drum","0x1F942","clinking glasses","0x1F943","tumbler glass","0x1F944","spoon","0x1F945","goal net",
    "0x1F947","1st place medal","0x1F948","2nd place medal","0x1F949","3rd place medal","0x1F94A","boxing glove","0x1F94B","martial arts uniform",
    "0x1F94C","curling stone","0x1F94D","lacrosse","0x1F94E","softball","0x1F94F","flying disc","0x1F950","croissant",
    "0x1F951","avocado","0x1F952","cucumber","0x1F953","bacon","0x1F954","potato","0x1F955","carrot",
    "0x1F956","baguette bread","0x1F957","green salad","0x1F958","shallow pan of food","0x1F959","stuffed flatbread","0x1F95A","egg",
    "0x1F95B","glass of milk","0x1F95C","peanuts","0x1F95D","kiwi fruit","0x1F95E","pancakes","0x1F95F","dumpling",
    "0x1F960","fortune cookie","0x1F961","takeout box","0x1F962","chopsticks","0x1F963","bowl with spoon","0x1F964","cup with straw",
    "0x1F965","coconut","0x1F966","broccoli","0x1F967","pie","0x1F968","pretzel","0x1F969","cut of meat",
    "0x1F96A","sandwich","0x1F96B","canned food","0x1F96C","leafy green","0x1F96D","mango","0x1F96E","moon cake",
    "0x1F96F","bagel","0x1F970","smiling face with hearts","0x1F971","yawning face","0x1F973","partying face","0x1F974","woozy face",
    "0x1F975","hot face","0x1F976","cold face","0x1F97A","pleading face","0x1F97B","sari","0x1F97C","lab coat",
    "0x1F97D","goggles","0x1F97E","hiking boot","0x1F97F","flat shoe","0x1F980","crab","0x1F981","lion",
    "0x1F982","scorpion","0x1F983","turkey","0x1F984","unicorn","0x1F985","eagle","0x1F986","duck",
    "0x1F987","bat","0x1F988","shark","0x1F989","owl","0x1F98A","fox","0x1F98B","butterfly",
    "0x1F98C","deer","0x1F98D","gorilla","0x1F98E","lizard","0x1F98F","rhinoceros","0x1F990","shrimp",
    "0x1F991","squid","0x1F992","giraffe","0x1F993","zebra","0x1F994","hedgehog","0x1F995","sauropod",
    "0x1F996","T-Rex","0x1F997","cricket","0x1F998","kangaroo","0x1F999","llama","0x1F99A","peacock",
    "0x1F99B","hippopotamus","0x1F99C","parrot","0x1F99D","raccoon","0x1F99E","lobster","0x1F99F","mosquito",
    "0x1F9A0","microbe","0x1F9A1","badger","0x1F9A2","swan","0x1F9A5","sloth","0x1F9A6","otter",
    "0x1F9A7","orangutan","0x1F9A8","skunk","0x1F9A9","flamingo","0x1F9AA","oyster","0x1F9AE","guide dog",
    "0x1F9AF","probing cane","0x1F9B0","red hair","0x1F9B1","curly hair","0x1F9B2","bald","0x1F9B3","white hair",
    "0x1F9B4","bone","0x1F9B5","leg","0x1F9B6","foot","0x1F9B7","tooth","0x1F9B8","superhero",
    "0x1F9B9","supervillain","0x1F9BA","safety vest","0x1F9BB","ear with hearing aid","0x1F9BC","motorized wheelchair","0x1F9BD","manual wheelchair",
    "0x1F9BE","mechanical arm","0x1F9BF","mechanical leg","0x1F9C0","cheese wedge","0x1F9C1","cupcake","0x1F9C2","salt",
    "0x1F9C3","beverage box","0x1F9C4","garlic","0x1F9C5","onion","0x1F9C6","falafel","0x1F9C7","waffle",
    "0x1F9C8","butter","0x1F9C9","mate","0x1F9CA","ice cube","0x1F9CD","person standing","0x1F9CE","person kneeling",
    "0x1F9CF","deaf person","0x1F9D0","face with monocle","0x1F9D1","person","0x1F9D2","child","0x1F9D3","older person",
    "0x1F9D4","man: beard","0x1F9D5","woman with headscarf","0x1F9D6","person in steamy room","0x1F9D7","person climbing","0x1F9D8","person in lotus position",
    "0x1F9D9","mage","0x1F9DA","fairy","0x1F9DB","vampire","0x1F9DC","merperson","0x1F9DD","elf",
    "0x1F9DE","genie","0x1F9DF","zombie","0x1F9E0","brain","0x1F9E1","orange heart","0x1F9E2","billed cap",
    "0x1F9E3","scarf","0x1F9E4","gloves","0x1F9E5","coat","0x1F9E6","socks","0x1F9E7","red envelope",
    "0x1F9E8","firecracker","0x1F9E9","puzzle piece","0x1F9EA","test tube","0x1F9EB","petri dish","0x1F9EC","dna",
    "0x1F9ED","compass","0x1F9EE","abacus","0x1F9EF","fire extinguisher","0x1F9F0","toolbox","0x1F9F1","brick",
    "0x1F9F2","magnet","0x1F9F3","luggage","0x1F9F4","lotion bottle","0x1F9F5","thread","0x1F9F6","yarn",
    "0x1F9F7","safety pin","0x1F9F8","teddy bear","0x1F9F9","broom","0x1F9FA","basket","0x1F9FB","roll of paper",
    "0x1F9FC","soap","0x1F9FD","sponge","0x1F9FE","receipt","0x1F9FF","nazar amulet","0x1FA70","ballet shoes",
    "0x1FA71","one-piece swimsuit","0x1FA72","swim brief","0x1FA73","shorts","0x1FA78","drop of blood","0x1FA79","adhesive bandage",
    "0x1FA7A","stethoscope","0x1FA80","yo-yo","0x1FA81","kite","0x1FA82","parachute","0x1FA90","ringed planet",
    "0x1FA91","chair","0x1FA92","razor","0x1FA93","axe","0x1FA94","diya lamp","0x1FA95","banjo",
    "0x203C","double exclamation mark","0x2049","exclamation question mark","0x2122","trade mark","0x2139","information","0x2194","left-right arrow",
    "0x2195","up-down arrow","0x2196","up-left arrow","0x2197","up-right arrow","0x2198","down-right arrow","0x2199","down-left arrow",
    "0x21A9","right arrow curving left","0x21AA","left arrow curving right","0x231A","watch","0x231B","hourglass done","0x2328","keyboard",
    "0x23CF","eject button","0x23E9","fast-forward button","0x23EA","fast reverse button","0x23EB","fast up button","0x23EC","fast down button",
    "0x23ED","next track button","0x23EE","last track button","0x23EF","play or pause button","0x23F0","alarm clock","0x23F1","stopwatch",
    "0x23F2","timer clock","0x23F3","hourglass not done","0x23F8","pause button","0x23F9","stop button","0x23FA","record button",
    "0x24C2","circled M","0x25AA","black small square","0x25AB","white small square","0x25B6","play button","0x25C0","reverse button",
    "0x25FB","white medium square","0x25FC","black medium square","0x25FD","white medium-small square","0x25FE","black medium-small square","0x2600","sun",
    "0x2601","cloud","0x2602","umbrella","0x2603","snowman","0x2604","comet","0x260E","telephone",
    "0x2611","check box with check","0x2614","umbrella with rain drops","0x2615","hot beverage","0x2618","shamrock","0x261D","index pointing up",
    "0x2620","skull and crossbones","0x2622","radioactive","0x2623","biohazard","0x2626","orthodox cross","0x262A","star and crescent",
    "0x262E","peace symbol","0x262F","yin yang","0x2638","wheel of dharma","0x2639","frowning face","0x263A","smiling face",
    "0x2640","female sign","0x2642","male sign","0x2648","Aries","0x2649","Taurus","0x264A","Gemini",
    "0x264B","Cancer","0x264C","Leo","0x264D","Virgo","0x264E","Libra","0x264F","Scorpio",
    "0x2650","Sagittarius","0x2651","Capricorn","0x2652","Aquarius","0x2653","Pisces","0x265F","chess pawn",
    "0x2660","spade suit","0x2663","club suit","0x2665","heart suit","0x2666","diamond suit","0x2668","hot springs",
    "0x267B","recycling symbol","0x267E","infinity","0x267F","wheelchair symbol","0x2692","hammer and pick","0x2693","anchor",
    "0x2694","crossed swords","0x2695","medical symbol","0x2696","balance scale","0x2697","alembic","0x2699","gear",
    "0x269B","atom symbol","0x269C","fleur-de-lis","0x26A0","warning","0x26A1","high voltage","0x26AA","white circle",
    "0x26AB","black circle","0x26B0","coffin","0x26B1","funeral urn","0x26BD","soccer ball","0x26BE","baseball",
    "0x26C4","snowman without snow","0x26C5","sun behind cloud","0x26C8","cloud with lightning and rain","0x26CE","Ophiuchus","0x26CF","pick",
    "0x26D1","rescue workers helmet","0x26D3","chains","0x26D4","no entry","0x26E9","shinto shrine","0x26EA","church",
    "0x26F0","mountain","0x26F1","umbrella on ground","0x26F2","fountain","0x26F3","flag in hole","0x26F4","ferry",
    "0x26F5","sailboat","0x26F7","skier","0x26F8","ice skate","0x26F9","person bouncing ball","0x26FA","tent",
    "0x26FD","fuel pump","0x2702","scissors","0x2705","check mark button","0x2708","airplane","0x2709","envelope",
    "0x270A","raised fist","0x270B","raised hand","0x270C","victory hand","0x270D","writing hand","0x270F","pencil",
    "0x2712","black nib","0x2714","check mark","0x2716","multiplication sign","0x271D","latin cross","0x2721","star of David",
    "0x2728","sparkles","0x2733","eight-spoked asterisk","0x2734","eight-pointed star","0x2744","snowflake","0x2747","sparkle",
    "0x274C","cross mark","0x274E","cross mark button","0x2753","question mark","0x2754","white question mark","0x2755","white exclamation mark",
    "0x2757","exclamation mark","0x2763","heart exclamation","0x2764","red heart","0x2795","plus sign","0x2796","minus sign",
    "0x2797","division sign","0x27A1","right arrow","0x27B0","curly loop","0x27BF","double curly loop","0x2934","right arrow curving up",
    "0x2935","right arrow curving down","0x2B05","left arrow","0x2B06","up arrow","0x2B07","down arrow","0x2B1B","black large square",
    "0x2B1C","white large square","0x2B50","star","0x2B55","hollow red circle","0x3030","wavy dash","0x303D","part alternation mark",
    "0x3297","Japanese congratulations button","0x3299","Japanese secret button",
    "", "" };


static UWCHAR temojiscomplement[] = {
    0x1F1E6,0x1F1E7,0x1F1E8,0x1F1E9,0x1F1EA,0x1F1EB,0x1F1EC,0x1F1ED,0x1F1EE,0x1F1EF,
    0x1F1F0,0x1F1F1,0x1F1F2,0x1F1F3,0x1F1F4,0x1F1F5,0x1F1F6,0x1F1F7,0x1F1F8,0x1F1F9,
    0x1F1FA,0x1F1FB,0x1F1FC,0x1F1FD,0x1F1FE,0x1F1FF,0x1F308,0x1F33E,0x1F373,0x1F393,
    0x1F3A4,0x1F3A8,0x1F3EB,0x1F3ED,0x1F3FB,0x1F3FC,0x1F3FD,0x1F3FE,0x1F3FF,0x1F466,
    0x1F467,0x1F468,0x1F469,0x1F48B,0x1F4BB,0x1F4BC,0x1F527,0x1F52C,0x1F5E8,0x1F680,
    0x1F692,0x1F91D,0x1F9AF,0x1F9B0,0x1F9B1,0x1F9B2,0x1F9B3,0x1F9BA,0x1F9BC,0x1F9BD,
    0x1F9D1,0x200D,0x20E3,0x2620,0x2640,0x2642,0x2695,0x2696,0x2708,0x2764,
    0xE0062,0xE0063,0xE0065,0xE0067,0xE006C,0xE006E,0xE0073,0xE0074,0xE0077,0xE007F,
    0xFE0F, 0};


bool Chaine_UTF8::c_is_emoji(UWCHAR c) {
    if (c < min_emoji)
        return false;

    try {
        emojis.at(c);
        return true;
    }
    catch(const std::out_of_range& oor) {
        return false;
    }
}

bool Chaine_UTF8::c_is_emojicomp(UWCHAR c) {
    if (c < min_emojicomp)
        return false;
    
    try {
        emojiscomplement.at(c);
        return true;
    }
    catch(const std::out_of_range& oor) {
        return false;
    }
}

//We position on 'position' in the string
long Chaine_UTF8::getonchar(wstring& w, long position) {
    long i = 0;
    long nb = 0;
    UWCHAR c;
    while (nb < position) {
        c = getonewchar(w, i);
        if (c_is_emoji(c)) {
            i++;
            c = getonewchar(w, i);
            while (c_is_emojicomp(c)) {
                i++;
                c = getonewchar(w, i);
            }
        }
        i++;
        nb++;
    }
    return i;
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

inline UWCHAR getuwchar(wstring& s, long& i) {
    UWCHAR c;
    if (c_utf16_to_unicode(c, s[i], false))
        c_utf16_to_unicode(c, s[++i], true);
    i++;
    return c;
}

void Chaine_UTF8::getchar(wstring& s, wstring& res,  size_t& i, long sz) {
    UWCHAR c = getuwchar(s, i);
    res = c;
    try {
        emojis.at(c);
        long j = i;
        c = getuwchar(s, j);
        while (j < sz && c_is_emojicomp(c)) {
            res += c;
            i = j;
            c = getuwchar(s, j);
        }
    }
    catch(const std::out_of_range& oor) {
    }
}

UWCHAR Chaine_UTF8::getachar(wstring& s, long& i) {
    UWCHAR res = getuwchar(s, i);
    try {
        emojis.at(res);
        UWCHAR c;
        long j = i;
        c = getuwchar(s, j);
        while (j < s.size() && c_is_emojicomp(c)) {
            i = j;
            c = getuwchar(s, j);
        }
    }
    catch(const std::out_of_range& oor) {
    }
    
    i--;
    return res;
}
#else

UWCHAR getonechar(unsigned char* s, long& i) {
    UWCHAR code;
    i += c_utf8_to_unicode(s + i, code);
    return code;
}

void Chaine_UTF8::getchar(wstring& s, wstring& res,  size_t& i, long sz) {
    try {
        emojis.at(s[i]);
        res = s[i++];
        while (i < sz && c_is_emojicomp(s[i])) {
            res += s[i++];
        }
    }
    catch(const std::out_of_range& oor) {
        res = s[i++];
    }
}

UWCHAR Chaine_UTF8::getachar(wstring& s, long& i) {
    UWCHAR res;
    try {
        emojis.at(s[i]);
        res = s[i++];
        while (i < s.size() && c_is_emojicomp(s[i])) {++i;}
        --i;
    }
    catch(const std::out_of_range& oor) {
        res = s[i];
    }
    return res;
}
#endif

bool Chaine_UTF8::c_is_emoji(unsigned char* m, long& i) {
    return c_is_emoji(getonechar(m, i));
}

bool Chaine_UTF8::c_is_emojicomp(unsigned char* m, long& i) {
    return c_is_emojicomp(getonechar(m, i));
}

bool Chaine_UTF8::s_is_emoji(string& s) {
    if (s == "")
        return false;
    long lg = s.size();
    bool check_comp = false;
    long p;
    for (long i = 0; i < lg; i++) {
        p = i;
        if (c_is_emoji(USTR(s), p)) {
            check_comp = true;
            i = p;
            continue;
        }
        
        if (check_comp) {
            p = i;
            if (c_is_emojicomp(USTR(s), p)) {
                i = p;
                continue;
            }
        }
        return false;
    }
    return true;
}

bool Chaine_UTF8::s_is_emoji(wstring& s) {
    if (s == L"")
        return false;
    long lg = s.size();
    bool check_comp = false;
    
    UWCHAR c;
    
    for (long i = 0; i < lg; i++) {
        c = getonewchar(s, i);
        if (c_is_emoji(c)) {
            check_comp = true;
            continue;
        }
        
        if (check_comp) {
            if (c_is_emojicomp(c))
                continue;
        }
        return false;
    }
    return true;
}

string Chaine_UTF8::emoji_description(string& s) {
    if (s.size() == 0)
        return "";
    UWCHAR c;
    c_utf8_to_unicode(USTR(s), c);
    if (emojis.find(c) != emojis.end())
        return emojis[c];
    return "";
}

string Chaine_UTF8::emoji_description(wstring& s) {
    if (s.size() == 0)
        return "";

    try {
        return emojis.at(s[0]);
    }
    catch(const std::out_of_range& oor) {
        return "";
    }
}

string Chaine_UTF8::emoji_description(UWCHAR c) {
    try {
        return emojis.at(c);
    }
    catch(const std::out_of_range& oor) {
        return "";
    }
}

void Chaine_UTF8::l_emojis(map<UWCHAR, string>& dico) {
    for (auto& it : emojis)
        dico[it.first] = it.second;
}
//------------------------------------------------------------------------
string cs_unicode_to_utf8(UWCHAR code) {
    char utf[5];
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


unsigned char c_unicode_to_utf8(UWCHAR code, unsigned char* utf) {
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


string c_unicode_to_utf8(UWCHAR code) {
    char utf[5];
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

char c_test_utf8(unsigned char* utf) {
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

void c_chars_get_next(unsigned char* m, char* str, size_t& i) {
    long nb = c_test_utf8(m + i);
    str[0] = (char)m[i];
    
    switch (nb) {
        case 0:
            str[1] = 0;
            i++;
            return;
        case 1:
            str[1] = m[i + 1];
            str[2] = 0;
            i += 2;
            return;
        case 2:
            str[1] = m[i + 1];
            str[2] = m[i + 2];
            str[3] = 0;
            i += 3;
            return;
        case 3:
            str[1] = m[i + 1];
            str[2] = m[i + 2];
            str[3] = m[i + 3];
            str[4] = 0;
            i += 4;
    }
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

Chaine_UTF8::Chaine_UTF8() {    
    wchar_t unicode;
    bulongchar xs;
    bulongchar xse;
    long i;
    int maxtable;
    for (maxtable = 0; codingtable[maxtable] != 0; maxtable++);
    
    
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
        i++;
    }

    //emoji
    string code;
    uint32_t c;
    min_emoji = 0xFFFFF;
    while (temojis[i][0] != 0) {
        code = temojis[i];
        c = (uint32_t)convertinginteger(code);
        min_emoji = std::min(min_emoji, c);
        if (c > 127)
            emojis[c] = temojis[i + 1];
        i += 2;
    }
    
    i = 0;
    min_emojicomp = 0xFFFFF;;
    while (temojiscomplement[i]) {
        c = temojiscomplement[i];
        min_emojicomp = std::min(min_emojicomp, c);
        emojiscomplement[c] = true;
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

unsigned char c_utf8_to_unicode(unsigned char* utf, UWCHAR& code) {
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

//--------------------------------------------------------------------
bool c_is_hexa(wchar_t code) {
    static const char hexas[]= {'a','b','c','d','e','f','A','B','C','D','E','F'};
    
    if (c_is_digit(code))
        return true;
    
    if (code <= 'f' && strchr(hexas,(char)code))
        return true;
    
    return false;
}
//--------------------------------------------------------------------
string jsonstring(string value) {
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

wstring wjsonstring(wstring value) {
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
//--------------------------------------------------------------------

bool Chaine_UTF8::c_is_punctuation(wchar_t c) {
    
    try {
        punctuations.at(c);
        return true;
    }
    catch(const std::out_of_range& oor) {
        return false;
    }
}

bool Chaine_UTF8::s_is_punctuation(wstring& str) {
    for (long i = 0; i < str.size(); i++) {
        if (!c_is_punctuation(str[i]))
            return false;
    }
    return true;
}


char Chaine_UTF8::c_is_alpha(unsigned char* m, long& i) {
    wchar_t v;
    i += c_utf8_to_unicode(m + i, v);
    
    try {
        utf8codemin.at(v);
        return 1;
    }
    catch(const std::out_of_range& oor) {
        try {
            utf8codemaj.at(v);
            return 2;
        }
        catch(const std::out_of_range& oor) {}
    }
    return 0;
}

char Chaine_UTF8::c_is_alpha(wchar_t v) {
    try {
        utf8codemin.at(v);
        return 1;
    }
    catch(const std::out_of_range& oor) {
        try {
            utf8codemaj.at(v);
            return 2;
        }
        catch(const std::out_of_range& oor) {}
    }
    return 0;
}


char Chaine_UTF8::is_a_valid_letter(unsigned char* m, long& i) {
    if (m[i] == '_' || isadigit(m[i]))
        return 1;
    return c_is_alpha(m, i);
}

char Chaine_UTF8::is_a_valid_letter(UWCHAR c) {
    if (c == '_' || isadigit(c))
        return 1;
    return c_is_alpha(c);
}

char Chaine_UTF8::is_a_valid_letter(wstring& m, long& i) {
    if (m[i] == '_' || isadigit(m[i]))
        return 1;
    return c_is_alpha(m[i]);
}

bool Chaine_UTF8::s_is_alpha(wstring& s) {
    if (s == L"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        char ty = c_is_alpha(s[i]);
        if (ty == 0)
            return false;
    }
    return true;
}

bool Chaine_UTF8::s_is_upper(wstring& s) {
    if (s == L"")
        return false;
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        char ty = c_is_alpha(s[i]);
        if (ty != 2)
            return false;
    }
    return true;
}

bool Chaine_UTF8::s_is_lower(wstring& s) {
    if (s == L"")
        return false;
    
    long lg = s.size();
    for (long i = 0; i < lg; i++) {
        char ty = c_is_alpha(s[i]);
        if (ty != 1)
            return false;
    }
    return true;
}

wchar_t Chaine_UTF8::c_to_lower(wchar_t c) {
    try {
        return utf8codemaj.at(c);
    }
    catch(const std::out_of_range& oor) {
        return c;
    }
}

wchar_t Chaine_UTF8::c_to_upper(wchar_t c) {
    try {
        return utf8codemin.at(c);
    }
    catch(const std::out_of_range& oor) {
        return c;
    }
}

wstring Chaine_UTF8::s_to_lower(wstring& s) {
    wstring res;
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (wchar_t)c_to_lower(s[i]);
    return res;
}

wstring Chaine_UTF8::s_to_upper(wstring& s) {
    wstring res;
    long lg = s.size();
    for (long i = 0; i < lg; i++)
        res += (wchar_t)c_to_upper(s[i]);
    return res;
}

bool c_is_space(wchar_t code) {
    static unsigned char spaces[] = { 9, 10, 13, 32, 160 };
    if ((code <= 160 && strchr((char*)spaces, (char)code)) || code == 0x202F || code == 0x3000)
        return true;
    return false;
}

bool Chaine_UTF8::s_is_space(string& str) {
    static unsigned char spaces[] = { 9, 10, 13, 32, 160 };
    long lg = str.size();
    uchar* contenu = USTR(str);
    wchar_t code;
    for (long i = 0; i < lg; i++) {
        i += c_utf8_to_unicode(contenu + i, code);
        if ((code <= 160 && !strchr((char*)spaces, (char)code)) && code != 0x202F && code != 0x3000)
            return false;
    }
    return true;
}

bool Chaine_UTF8::s_is_space(wstring& str) {
    long lg = str.size();
    wchar_t code;
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

bool s_is_utf8(unsigned char* contenu, long longueur) {
    while (longueur--) {
        if ((*contenu & 0x80) && c_test_utf8(contenu))
            return true;
        ++contenu;
    }
    return false;
}

//------------------------------------------------------------------------
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

long size_c(unsigned char* contenu, long sz) {
    long i = 0;
    long size = i;
    long nb;
    
    while (i < sz) {
        nb = c_test_utf8(contenu + i);
        i += nb + 1;
        ++size;
    }
    
    return size;
}

long size_c(string& s) {
    return size_c(USTR(s), s.size());
}

string s_left(string& s, long nb) {
    if (nb <= 0)
        return "";
    
    long lg = s.size();
    nb = getindex(USTR(s), lg, nb);
    if (nb >= lg)
        return s;
    
    return s.substr(0, nb);
}
string s_right(string& s, long nb) {
    if (nb <= 0)
        return "";

    long lg = s.size();

    long l = size_c(USTR(s), lg) - nb;

    if (l <= 0)
        return s;
    
    l = getindex(USTR(s), lg, l);
    return s.substr(l, lg);
}

string s_middle(string& s, long l, long nb) {
    long lg = s.size();

    uchar* contenu = USTR(s);
    
    long i = getindex(contenu, lg, l);
    if (i >= lg)
        return "";

    nb += l;
    nb = getindex(contenu, lg, nb);
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

//------------------------------------------------------------------------
double conversiontofloathexa(const char* s, int sign) {
    long v = 0;
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
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        return conversiontofloathexa(s, sign);
    }
    
    long v;
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
//------------------------------------------------------------------------
double conversiontofloathexa(const char* s, int sign, long& l) {
    long v = 0;
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
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        l++;
        return conversiontofloathexa(s, sign, l);
    }
    
    long v;
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

void noconversiontofloathexa(const char* s, int sign, short& l) {
    bool cont = true;
    uchar c = *s++;
    l++;
    while (cont) {
        switch (digitaction[c]) {
            case '0':
                c = *s++;
                l++;
                continue;
            case 'X':
                c = *s++;
                l++;
                continue;
            case 'x':
                c = *s++;
                l++;
                continue;
            default:
                cont = false;
        }
    }
    
    if (c == '.') {
        cont = true;
        c = *s++;
        l++;
        while (cont) {
            switch (digitaction[c]) {
                case '0':
                    c = *s++;
                    l++;
                    continue;
                case 'X':
                    c = *s++;
                    l++;
                    continue;
                case 'x':
                    c = *s++;
                    l++;
                    continue;
                default:
                    cont = false;
            }
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

void noconvertingfloathexa(const char* s, short& l) {
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
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        l++;
        noconversiontofloathexa(s, sign, l);
        return;
    }
    
    if (isadigit(*s)) {
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
    bool cont = true;
    wchar_t c = *s++;
    l++;
    while (cont) {
        switch (digitaction[c]) {
            case '0':
                c = *s++;
                l++;
                continue;
            case 'X':
                c = *s++;
                l++;
                continue;
            case 'x':
                c = *s++;
                l++;
                continue;
            default:
                cont = false;
        }
    }
    
    if (c == '.') {
        cont = true;
        c = *s++;
        l++;
        while (cont) {
            switch (digitaction[c]) {
                case '0':
                    c = *s++;
                    l++;
                    continue;
                case 'X':
                    c = *s++;
                    l++;
                    continue;
                case 'x':
                    c = *s++;
                    l++;
                    continue;
                default:
                    cont = false;
            }
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
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        l++;
        noconversiontofloathexa(s, sign, l);
        return;
    }
    
    if (isadigit(*s)) {
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
//------------------------------------------------------------------------
double convertingtofloathexa(wchar_t* s, int sign, long& l) {
    long v = 0;
    bool cont = true;
    wchar_t c = *s++;
    l++;
    while (cont) {
        switch (digitaction[(uchar)c]) {
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
            switch (digitaction[(uchar)c]) {
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
    
    if (*s=='0' && s[1]=='x') {
        s+=2;
        l++;
        return convertingtofloathexa(s, sign, l);
    }
    
    long v;
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

//------------------------------------------------------------------------
bool c_char_index_insert(string& s, string c, size_t x) {
    if (x > s.size())
        return false;
    long i;
    long lg = s.size();
    uchar* contenu = USTR(s);
    for (i = 0; i<lg && x>0; i++) {
        i += c_test_utf8(contenu + i);
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

void s_unicode_to_utf8(string& s, wstring& str) {
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

void s_utf8_to_unicode(wstring& w, unsigned char* str , long sz) {
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
#else
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
#endif
    
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
void SetBlankSize(long sz) {
    if (sz >= 1)
        blanksize = sz;
}

long GetBlankSize() {
    return blanksize;
}


static const char _tocheck[] = {'"', '\'', '@', ':', ',','-', '+','0','1','2','3','4','5', '6','7','8', '9','[',']','{', '}', 0};
static const char _checkingmore[] = {'\n', '/', '(', ')', '<', '>','=',';', 0};

void split_container(unsigned char* src, long lensrc, vector<long>& pos, bool forindent) {
    uchar c;
    
    for (long e = 0; e < lensrc; e++) {
        c = src[e];
        if (forindent && strchr(_checkingmore, c))
            pos.push_back(e);
        else
            if (strchr(_tocheck, c))
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
    split_container(USTR(codestr), szstr, pos, true);

    long sz = pos.size();
    long r = 0;
    long i = 0;
    long iblank = 0;
    long p;
    long finalblank = 0;

    short addspace = 0;
    short checkspace = 0;
    short iparenthesis = 0;
    short l;

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

long VirtualIndentation(string& codestr) {
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
        
    short l;
    short addspace = 0;
    short checkspace = 0;
    
    bool consumeblanks = true;
    
    uchar c;

    split_container(codestr, szstr, pos, true);
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
   
void IndentCode(string& codestr, string& codeindente, long blancs) {
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

	try {
		delegation->libraries.at(name);
		return delegation->_TRUE;
	}
	catch (const std::out_of_range& oor) {}

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
			message << "Error: Please set TAMGULIBS: " << name;
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
    
    try {
        delegation->libraries.at(name);
        return delegation->_TRUE;
    }
    catch(const std::out_of_range& oor) {}
    
    void* LoadMe;
    
    char lname[4096];
    char rawlname[4096];
    
    strcpy_s(lname, 4096, STR(name));
    strcpy_s(rawlname, 4096, STR(name));
    
    char* error;
    char* pt = strrchr(lname, '/');
    char* rawpt = strrchr(rawlname, '/');
    char buff[4096];
    bool addso = true;
    string basename;
    string rawbasename;
    if (strstr(lname + strlen(lname) - 3, ".so"))
        addso = false;
    if (pt != NULL) {
        if (memcmp(pt + 1, "lib", 3)) {
            if (addso)
                sprintf(buff, "lib%s.so", pt + 1);
            else
                sprintf(buff, "lib%s", pt + 1);
        }
        else {
            if (addso)
                sprintf(buff, "%s.so", pt + 1);
            else
                strcpy(buff, pt + 1);
        }
        basename = buff;
        //no lib
        rawbasename = basename.substr(3, basename.size());
        strcpy(pt + 1, buff);
        strcpy(rawpt + 1, STR(basename));
    }
    else {
        if (memcmp(lname, "lib", 3)) {
            if (addso)
                sprintf(buff, "lib%s.so", lname);
            else
                sprintf(buff, "lib%s", lname);
        }
        else {
            if (addso)
                sprintf(buff, "%s.so", lname);
            else
                strcpy(buff, lname);
        }
        basename = buff;
        //no lib
        rawbasename = basename.substr(3, basename.size());
        strcpy(lname, buff);
        strcpy(rawlname, STR(basename));
    }
    
    NormalizeFileName(buff, lname, 4096);
    strcpy(lname, buff);

    NormalizeFileName(buff, rawlname, 4096);
    strcpy(rawlname, buff);

    string moduleinitname = "InitialisationModule";
    
    LibraryEntryPoint LibEntryPoint;

    nom_bib = lname;

    LoadMe = dlopen(lname, RTLD_LAZY);
    if (LoadMe == NULL) {
        nom_bib = rawlname;
        LoadMe = dlopen(rawlname, RTLD_LAZY);
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
        
        char erreur = 0;
        string ldlibpath;
        
#ifdef __apple_build_version__
        if (erreur) {
            ldlibpath="";
            //Specific to MAC OS
            if (getenv("DYLD_LIBRARY_PATH") != NULL)
                ldlibpath = getenv("DYLD_LIBRARY_PATH");
            
            if (ldlibpath.find(atanlib) == -1) {
                stringstream message;
                message << "On Mac OS you need to initialize DYLD_LIBRARY_PATH." << endl;
                message << "Place this initialisation in your bash or zsh profile:" << endl;
                message << "\texport DYLD_LIBRARY_PATH=$LISPEPATH:$DYLD_LIBRARY_PATH" << endl;
                throw new Error(message.str());
            }
        }
#else
        if (getenv("LD_LIBRARY_PATH") != NULL)
            ldlibpath = getenv("LD_LIBRARY_PATH");

        if (ldlibpath.find(atanlib) == -1) {
            stringstream message;
            message << "Error: on most platforms, you need to initialize LD_LIBRARY_PATH." << endl;
            message << "Place this initialisation in your bash or zsh profile:" << endl;
            message << "\texport LD_LIBRARY_PATH=$LISPEPATH:$LD_LIBRARY_PATH" << endl;
            throw new Error(message.str());
        }
#endif
        
        if (atanlib.back() != '/')
            atanlib += "/";
        
        baselib = atanlib;
        atanlib += basename;
        atanlib = NormalizePathname(atanlib);
        nom_bib = atanlib;
        LoadMe = dlopen(STR(atanlib), RTLD_LAZY);
        if (LoadMe == NULL) {
            atanlib = baselib;
            atanlib += rawbasename;
            atanlib = NormalizePathname(atanlib);
            nom_bib = atanlib;
            LoadMe = dlopen(STR(atanlib), RTLD_LAZY);
        }
    }
    
    // Check to see if the library was loaded successfully
    if (LoadMe == NULL) {
        //We try without lib in the name
        error = dlerror();
        std::cerr << error << std::endl;
        baselib += nom_bib;
        LoadMe = dlopen(STR(baselib), RTLD_LAZY);
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

void split_container(wchar_t* src, long lensrc, vector<long>& pos) {
    wchar_t c;
    
    for (long e = 0; e < lensrc; e++) {
        c = src[e];
        if (strchr(_tocheck, (uchar)c))
            pos.push_back(e);
    }
}

void replacemetas(wstring& sub) {
    static wstring search(L"\\");
    
    if (sub.find(search) == -1)
        return;
    
    wstring thestr;
    long sz = sub.size();
    for (long i=0;i<sz;i++) {
        if (sub[i]=='\\') {
            switch(sub[++i]) {
                case 'n':
                    thestr+=L"\n";
                    break;
                case 'r':
                    thestr+=L"\r";
                    break;
                case 't':
                    thestr+=L"\t";
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

bool LispEJsonCompiler::compile(LispE* lisp, wstring& s) {
    s = s_trim(s);    
    if (s[0] == '{')
        compiled_result = new Dictionary_as_list;
    else
        compiled_result = new List;

    pos.clear();
    src = (wchar_t*)s.c_str();
    split_container(src, s.size(), pos);
    r = 1;
    i = 1;
    line = 0;
    sz = pos.size();
    
    if (!buildexpression(lisp, compiled_result) || r != pos.size()) {
        compiled_result->release();
        return false;
    }
    if (s[0] == '{') {
        Element* e = compiled_result->rawdictionary(lisp);
        if (e != compiled_result) {
            delete compiled_result;
            compiled_result = e;
        }
    }
    return true;
}

char LispEJsonCompiler::buildexpression(LispE* lisp, Element* kf) {
    Element* local;

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
            token = (wchar_t*)src+i-1;
            if (token == L"false")
                local = false_;
            else {
                if (token == L"true")
                    local = true_;
                else {
                    if (token == L"null" || token == L"nil")
                        local = null_;
                    else {
                        local = lisp->provideString(token);
                    }
                }
            }
            kf->append(local);
            checknext = kf->verify();
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
                token = (wchar_t*)src+i;
                replacemetas(token);
                local = lisp->provideString(token);
                kf->append(local);
                checknext = kf->verify();
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
                wstring locstr = (wchar_t*)src+i;
                local = lisp->provideString(locstr);
                
                kf->append(local);
                
                checknext = kf->verify();
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
                    local = lisp->provideInteger((long)v);
                else
                    local = lisp->provideNumber(v);
                kf->append(local);
                
                checknext = kf->verify();
                src[to] = c;
                i = to;
                break;
            case '{':
                if (checknext)
                    return false;
                local = new Dictionary_as_list;
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
                kf->append(local->rawdictionary(lisp));
                delete local;
                checknext = kf->verify();
                break;
            case '[':
                if (checknext)
                    return false;
                local = new List;
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
                kf->append(local);
                checknext=true;
                break;
            case '}':
                return kf->verify();
            case ']':
                if (kf->isDictionary())
                    return false;
                return true;
            case ':':
                kf->reversechoice();
                if (!kf->isDictionary() || kf->verify())
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
Exporting bool c_unicode_to_utf16(uint32_t& res, uint32_t code) {
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

Exporting bool c_utf16_to_unicode(uint32_t& r, uint32_t code, bool second) {
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
