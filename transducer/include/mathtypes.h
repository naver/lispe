/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//mathtypes.h

#ifndef mathtypes_h
#define mathtypes_h

#include <cinttypes>

#define bshort int16_t
#define bushort uint16_t
#define ushort uint16_t
#define bint int32_t
#define buint uint32_t
#define binuint64 uint64_t
#define BULONG unsigned long long
#define BLONG long long

#ifdef WIN32
#define bitscanforward(r,x) if (!_BitScanForward(&r,x)) r = 0
#define bitscanreverse(r,x) if (!_BitScanReverse(&r,x)) r = 0
#else
#define bitscanforward(r,x) r = _bit_scan_forward(x)
#define bitscanreverse(r,x) r = _bit_scan_reverse(x)
#endif

#ifdef UNIX
#include <math.h>
#define strcpy_s(a,c,b) strncpy(a,b,c)
#define strcat_s(a,c,b) strncat(a,b,c)
#define sprintf_s snprintf
#define swprintf_s swprintf
#endif

#ifdef WIN32
#define Endl "\r\n"
#define WSTRING_IS_UTF16 1
#else
#define Endl "\n"
#endif

#define uchar unsigned char
#define STR(x) (char*)x.c_str()
#define USTR(x) (uchar*)x.c_str()
#define WSTR(x) (wchar_t*)x.c_str()

#ifdef WSTRING_IS_UTF16
#define TRANSCHAR uint32_t
#else
#define TRANSCHAR wchar_t
#endif

#ifndef Exporting
#if defined(WIN32)
#ifdef TamguSTATIC
#define Exporting
#define Exchanging
#else
#define Exporting __declspec(dllexport)
#ifdef TamguDLL //This flag should be USED when LINKING with the tamgu DLL, not within the tamgu DLL
#define Exchanging __declspec(dllimport)
#else
#define Exchanging __declspec(dllexport)
#endif
#endif
#else
#define Exporting
#define Exchanging
#endif
#endif

#endif /* mathtypes_h */
