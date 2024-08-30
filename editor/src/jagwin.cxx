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
void Getscreensizes(bool);
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

//We generate the Unix strings in Windows, to keep the whole code constant across platform
string MouseEventProc(MOUSE_EVENT_RECORD mer) {
	static bool tracking = false;
    stringstream stre;

    stre << "\033[";
    int x = 0, y = 0;
    COORD mousep = mer.dwMousePosition;

	mousep.X++;
	mousep.Y++;

    long wheel = mer.dwButtonState;

	switch (mer.dwEventFlags)
	{
	case 0:
		if (!mer.dwButtonState) {
			stre << 35 << ";" << mousep.X << ";" << mousep.Y << "M";
			tracking = false;
		}
		else if (mer.dwButtonState == FROM_LEFT_1ST_BUTTON_PRESSED)
		{
			stre << 32 << ";" << mousep.X << ";" << mousep.Y << "M";
			tracking = true;
		}
		else if (mer.dwButtonState == RIGHTMOST_BUTTON_PRESSED)
        {
            stre << 34 << ";" << mousep.X << ";" << mousep.Y<< "M";
            tracking = true;
        }
        else
        {
            stre << 34 << ";" << mousep.X << ";" << mousep.Y<< "M";
            tracking = true;
        }
        break;
	case 2: //double-click
		if (!mer.dwButtonState) {
			stre << 35 << ";" << mousep.X << ";" << mousep.Y << "DM";
			tracking = false;
		}
		else if (mer.dwButtonState == FROM_LEFT_1ST_BUTTON_PRESSED)
		{
			stre << 32 << ";" << mousep.X << ";" << mousep.Y << "DM";
			tracking = true;
		}
		else if (mer.dwButtonState == RIGHTMOST_BUTTON_PRESSED)
		{
			stre << 34 << ";" << mousep.X << ";" << mousep.Y << "DM";
			tracking = true;
		}
		else
		{
			stre << 34 << ";" << mousep.X << ";" << mousep.Y << "DM";
			tracking = true;
		}
		break;
	case MOUSE_HWHEELED:
        if (wheel < 0)
            stre << 97 << ";" << mousep.X << ";" << mousep.Y << "M";
        else
            stre << 96 << ";" << mousep.X << ";" << mousep.Y << "M";
        break;
    case MOUSE_MOVED:
		if (tracking)
            stre << 64 << ";" << mousep.X << ";" << mousep.Y << "M";
        else
            stre << 67 << ";" << mousep.X << ";" << mousep.Y << "M";
        break;
    case MOUSE_WHEELED:
        if (wheel < 0)
            stre << 97 << ";" << mousep.X << ";" << mousep.Y<< "M";
        else
            stre << 96 << ";" << mousep.X << ";" << mousep.Y<< "M";
        break;
    default:
		stre << 67 << ";" << mousep.X << ";" << mousep.Y << "M";
    }

	return stre.str();
}

string getwinchar(void(*f)(), bool mouseenabled) {
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

    DWORD fdwMode;
    
    //This is the most important trick here, you need to reset the flags at each call...
    if (mouseenabled)
        fdwMode = ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT | ENABLE_EXTENDED_FLAGS;
    else
        fdwMode = ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS;

    SetConsoleMode(hStdin, fdwMode);
    
    kar = 0;
	bool alt = false;
	bool shift = false;
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
							//Alt key is pressed, we look for its combination
							if (key.wVirtualScanCode == 42) {
								shift = true;
								break;
							}
							if (key.wVirtualScanCode == 56) {
								alt = true;
								break;
							}
							if (keys.find(key.wVirtualScanCode) != keys.end()) {
								if (shift || key.dwControlKeyState == 304)
									s = (uchar)225;
								else
									s = (uchar)224;
                                s += key.wVirtualScanCode;
                                return s;
                            }
                        }
                        else {
                            w = kar;
							if (alt && (w == L"c" || w == L"v" || w == L"x" || w == L"=" || w == L"-")) {
								s = (uchar)226;
								s += (uchar)w[0];
								return s;
							}
                            s_unicode_to_utf8(s, w);
                            return s;
                        }
                    }
                    break;
                }
                case MOUSE_EVENT: // mouse input
                    return MouseEventProc(irInBuf[i].Event.MouseEvent);
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


void sendstring(string s) {
    cout << s;
}


void Getscreensizes(bool mouseenabled) {
    static CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	DWORD fdwMode;

    if (row_size == -1 && col_size == -1) {
        hStdin = GetStdHandle(STD_INPUT_HANDLE);

        // Save the current input mode, to be restored on exit.

        if (!GetConsoleMode(hStdin, &fdwSaveOldMode)) {
            printf("GetConsoleMode (%d)\n", GetLastError());
            return;
        }

        // Enable the window and mouse input events.
		if (mouseenabled)
			fdwMode = ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT | ENABLE_EXTENDED_FLAGS;
		else
			fdwMode = ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS;

		SetConsoleMode(hStdin, fdwMode);


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
	else {
		if (mouseenabled) {
			fdwMode = ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT | ENABLE_EXTENDED_FLAGS;
			SetConsoleMode(hStdin, fdwMode);
		}
	}
}
