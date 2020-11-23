/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//Specific code for Window
//
// jagwin.cxx

#include <windows.h>
#include <io.h>
#include <stdio.h>

#include <conio.h>
#include <ctype.h>

#include "tools.h"

#define hmap std::unordered_map


#if (_MSC_VER >= 1900)
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif

//Handling console tune up
static HANDLE hStdout = 0;
static HANDLE hStdin = 0;
static DWORD lpMode = 0;
static DWORD fdwSaveOldMode;
static UINT codepage = 0;

#define DUP _dup
#define DUP2 _dup2
#define FILENO _fileno

void ResetWindowsConsole();
void Getscreensizes();
bool checkresize();
void Returnscreensize(long& rs, long& cs, long& sr, long& sc);

static char check_size_utf8(int utf) {
    if ((utf & 0xF0) == 0xF0)
        return 3;

    if ((utf & 0xE0) == 0xE0)
        return 2;

    if ((utf & 0xC0) == 0xC0)
        return 1;

    return 0;
}

static long row_size = -1;
static long col_size = -1;


static long size_row = 0;
static long size_col = 0;

void Returnscreensize(long& rs, long& cs, long& sr, long& sc) {
    rs = row_size;
    cs = col_size;
    sr = size_row;
    sc = size_col;
}

bool checkresize() {
    static CONSOLE_SCREEN_BUFFER_INFO csbiInfo;

    GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
    long rs = csbiInfo.srWindow.Bottom - csbiInfo.srWindow.Top;
    long cs = csbiInfo.srWindow.Right - csbiInfo.srWindow.Left;

    if (rs != row_size || cs != col_size) {
        row_size = rs;
        col_size = cs;

        size_row = csbiInfo.dwMaximumWindowSize.X;
        size_col = csbiInfo.dwMaximumWindowSize.Y;
        return true;
    }
    return false;
}

void ResetWindowsConsole() {
    SetConsoleCP(codepage);
    SetConsoleOutputCP(codepage);
    SetConsoleMode(hStdout, lpMode);
    SetConsoleMode(hStdin, fdwSaveOldMode);
}

void getcursor(int& xcursor, int& ycursor) {
    static CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
    GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
    xcursor = csbiInfo.dwCursorPosition.X;
    ycursor = csbiInfo.dwCursorPosition.Y;
}

string getwinchar(void(*f)()) {
    static hmap<int, bool> keys;
    static bool init = false;
    if (!init) {
        //this is the list of control keys that we actually care for
        keys[77] = true;
        keys[75] = true;
        keys[80] = true;
        keys[72] = true;
        keys[83] = true;
        keys[71] = true;
        keys[79] = true;
        keys[119] = true;
        keys[117] = true;
        keys[73] = true;
        keys[81] = true;
        keys[116] = true;
        keys[115] = true;
        init = true;
    }

    wstring w;
    string s;
    DWORD cNumRead;
    WCHAR kar;
    INPUT_RECORD irInBuf[512];
    bool stop = false;
    int i;

    //This is the most important trick here, you need to reset the flags at each call...
    DWORD fdwMode = ENABLE_WINDOW_INPUT;
    SetConsoleMode(hStdin, fdwMode);

    kar = 0;
    while (s == "") {
        ReadConsoleInput(
            hStdin,      // input buffer handle
            irInBuf,     // buffer to read into
            512,         // size of read buffer
            &cNumRead); // number of records read

        for (i = 0; i < cNumRead; i++)
        {
            switch (irInBuf[i].EventType)
            {
            case KEY_EVENT: {// keyboard input
                KEY_EVENT_RECORD& key = irInBuf[i].Event.KeyEvent;
                if (key.bKeyDown) {
                    kar = key.uChar.UnicodeChar;
                    if (!kar) {
                        if (keys.find(key.wVirtualScanCode) != keys.end()) {
                            s = (uchar)224;
                            s += key.wVirtualScanCode;
                            return s;
                        }
                    }
                    else {
                        w = kar;
                        s_unicode_to_utf8(s, w);
                        return s;
                    }
                }
                break;
            }
            case WINDOW_BUFFER_SIZE_EVENT: // scrn buf. resizing
                if (checkresize())
                    (*f)();
                break;
            }
        }
    }

    return s;
}

bool checkresize();


string getcharacter() {
    static hmap<int, bool> keys;
    static bool init = false;
    if (!init) {
        //this is the list of control keys that we actually care for
        keys[77] = true;
        keys[75] = true;
        keys[80] = true;
        keys[72] = true;
        keys[83] = true;
        keys[71] = true;
        keys[79] = true;
        keys[119] = true;
        keys[117] = true;
        keys[73] = true;
        keys[81] = true;
        keys[116] = true;
        keys[115] = true;
        init = true;
    }

    string s;

    DWORD cNumRead;
    WCHAR kar;
    wstring w;
    INPUT_RECORD irInBuf[512];
    bool stop = false;
    int i;
    DWORD fdwMode;

    //This is the most important trick here, you need to reset the flags at each call...
	fdwMode = ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS;

    SetConsoleMode(hStdin, fdwMode);

    kar = 0;
    while (s == "") {
        ReadConsoleInput(
            hStdin,      // input buffer handle
            irInBuf,     // buffer to read into
            512,         // size of read buffer
            &cNumRead); // number of records read

        for (i = 0; i < cNumRead; i++)
        {
            switch (irInBuf[i].EventType)
            {
            case KEY_EVENT: {// keyboard input
                KEY_EVENT_RECORD& key = irInBuf[i].Event.KeyEvent;
                if (key.bKeyDown) {
                    kar = key.uChar.UnicodeChar;
                    if (!kar) {
                        if (keys.find(key.wVirtualScanCode) != keys.end()) {
                            s = (uchar)224;
                            s += key.wVirtualScanCode;
                            return s;
                        }
                    }
                    else {
                        w = kar;
                        s_unicode_to_utf8(s, w);
                        return s;
                    }
                }
                break;
            }
            case WINDOW_BUFFER_SIZE_EVENT: // scrn buf. resizing
				checkresize();
            }
        }
    }

    return s;
}

void sendstring(string s) {
    cout << s;
}


void Getscreensizes() {
    static CONSOLE_SCREEN_BUFFER_INFO csbiInfo;

    if (row_size == -1 && col_size == -1) {
		system("cls");
        DWORD fdwMode;

        hStdin = GetStdHandle(STD_INPUT_HANDLE);

        // Save the current input mode, to be restored on exit.

        if (!GetConsoleMode(hStdin, &fdwSaveOldMode)) {
            printf("GetConsoleMode (%d)\n", GetLastError());
            return;
        }

        // Enable the window and mouse input events.
        fdwMode = ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT | ENABLE_EXTENDED_FLAGS;;
        if (!SetConsoleMode(hStdin, fdwMode)) {
            printf("SetConsoleMode (%d)\n", GetLastError());
            return;
        }

		codepage = GetConsoleOutputCP();
        //UTF8 setting
        SetConsoleOutputCP(65001);
        SetConsoleCP(65001);

        // Get the current screen buffer size and window position.
        hStdout = GetStdHandle(STD_OUTPUT_HANDLE);

		//Selecting a different font to display all Unicode characters
        CONSOLE_FONT_INFOEX info = { 0 };
        info.cbSize = sizeof(info);
        info.dwFontSize.Y = 18; // leave X as zero
        info.FontWeight = FW_NORMAL;
        wcscpy(info.FaceName, L"Consolas");
        SetCurrentConsoleFontEx(hStdout, NULL, &info);

		//We set the specific mode to handle terminal commands
        GetConsoleMode(hStdout, &lpMode);
        SetConsoleMode(hStdout, lpMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);

        if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
        {
            printf("GetConsoleScreenBufferInfo (%d)\n", GetLastError());
            return;
        }
        row_size = csbiInfo.srWindow.Bottom - csbiInfo.srWindow.Top;
        col_size = csbiInfo.srWindow.Right - csbiInfo.srWindow.Left;

        size_row = csbiInfo.dwMaximumWindowSize.X;
        size_col = csbiInfo.dwMaximumWindowSize.Y;
    }
}
