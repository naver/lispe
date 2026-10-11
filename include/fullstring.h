// LispE
// Copyright 2020-present NAVER Corp.
// The 3-Clause BSD License
//
//  fullstring.h
//
//

#ifndef fullstring_h
#define fullstring_h

using std::wstring;
#define u_uchar char32_t
#define u_ustring std::u32string

// The type of LispE integers. In WebAssembly (wasm32), long is only 32 bits:
// li_long is then int64_t, so that integers keep their 64 bits everywhere.
// In native builds, it is long, exactly as before.
#ifdef LISPE_WASM
#include <cstdint>
#define li_long int64_t
#define li_ulong uint64_t
#else
#define li_long long
#define li_ulong unsigned long
#endif

// On Windows, _w_to_u is called with a function
// while on Unix platforms, it is called with a cast (in a #define)
// u_pstring will be a simple definition, while it will be a reference
// exchange on Unix platforms (faster, no copy)
#ifdef WIN32
#define u_pstring std::u32string
#else
#define u_pstring std::u32string&
#endif

#endif /* fullstring */
