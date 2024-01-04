/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  jag.cxx
//
//


#include <stdio.h>
#include <string.h>

#ifndef WIN32
#include <unistd.h>   //_getch
#include <termios.h>  //_getch
#include <sys/ioctl.h>
#endif

#include <signal.h>
#include <iomanip>

#include "jagget.h"
#include "tools.h"

//------------------------------------------------------------------------------------
long margin_value_reference = 3;
//------------------------------------------------------------------------------------

static const int16_t _getbuffsize = 128;
char m_scrollmargin[] = { 27, 91, '0', '0', '0', ';', '0', '0','0', 'r', 0 };
static char m_deletechar[] = { 27, 91, '1', 'P', 0 };
static char m_oneleft[] = { 27, '[', '1', 68, 0 };
static char m_oneright[] = {27, '[', '0', '0', '1', 67, 0};

unsigned char* alt_c= NULL;
char* alt_vbis= NULL;
unsigned char* alt_v= NULL;
char* page_down= NULL;
unsigned char* alt_x= NULL;
uchar is_up= 0;
char* c_down= NULL;
char* c_left= NULL;
unsigned char* alt_xbis= NULL;
char* c_up= NULL;
char* endkey= NULL;
char* down= NULL;
char* homekey= NULL;
char* up= NULL;
char* right= NULL;
unsigned char* alt_cbis= NULL;
unsigned char* shift_left= NULL;
char* page_up= NULL;
char* left= NULL;
unsigned char* alt_plus= NULL;
uchar is_down= 0;
char* c_right= NULL;
unsigned char* alt_minus= NULL;
unsigned char* shift_right= NULL;
char* del= NULL;


#ifdef WIN32
static char m_delback = 8;
static char m_delbackbis = 8;
void ResetWindowsConsole();
void Getscreensizes(bool);
void Returnscreensize(long& rs, long& cs, long& sr, long& sc);
#else
static char  m_delback = 127;
static char m_delbackbis = 15;
#endif

static jag_get* main_handler = NULL;
//------------------------------------------------------------------------------------
jag_get* get_jag_handler() {
    if (main_handler == NULL)
        return new jag_get(false);
    return main_handler;
}

void clean_get_handler(jag_get* h) {
    if (!h->inside_editor)
        delete h;
}

void lispe_readfromkeyboard(string& code, void* o) {
    jag_get* h = (jag_get*)o;
    h->get_a_string(code);
}

string get_char(jag_get* h) {
    return h->getch();
}
//------------------------------------------------------------------------------------
void jag_get::initialisation() {
    if (initialized)
        return;
    initialized = true;
#ifdef WIN32
    set_window_control_codes();
	Getscreensizes(mouse_status);
#else
    tcgetattr(0, &oldterm);
    init_control_codes();
    
    //We enable ctrl-s and ctrl-q within the editor
    termios theterm;
    tcgetattr(0, &theterm);
    theterm.c_iflag &= ~IXON;
    theterm.c_iflag |= IXOFF;
    theterm.c_cc[VSTART] = NULL;
    theterm.c_cc[VSTOP] = NULL;
    theterm.c_cc[VSUSP] = NULL;
    tcsetattr(0, TCSADRAIN, &theterm);
#endif
}

jag_get::jag_get(bool inside) {
    initialized = false;
    row_size = -1;
    col_size = -1;
    margin = margin_value_reference;

    if (inside) {
        main_handler = this;
        initialisation();
    }
    
    inside_editor = inside;
    mouse_status = false;
    activate_mouse = false;
	nbclicks = 0;

#ifdef XTERM_MOUSE_VT100
    vt100 = true;
#else
    vt100 = false;
#endif

}

void jag_get::resetterminal() {
    if (!initialized)
        return;
    
    mouseoff();
#ifdef WIN32
    ResetWindowsConsole();
#else
    tcsetattr(0, TCSADRAIN, &oldterm);
#endif
}

#ifdef WIN32
string getwinchar(void(*f)(), bool mouseenabled);
void _resizewindow() {}
string jag_get::getch() {
    initialisation();
    return getwinchar(_resizewindow, mouse_status);
}
#else
string jag_get::getch(){
    initialisation();
    static char buf[_getbuffsize+2];
    memset(buf,0, _getbuffsize);

    struct termios remove_echo={0};
    fflush(stdout);
    if(tcgetattr(0, &remove_echo)<0) {
        perror("tcsetattr()");
        exit(-1);
    }
    
    remove_echo.c_lflag&=~ICANON;
    remove_echo.c_lflag&=~ECHO;
    remove_echo.c_cc[VMIN]=1;
    remove_echo.c_cc[VTIME]=0;
    if(tcsetattr(0, TCSANOW, &remove_echo)<0) {
        perror("tcsetattr ICANON");
        return "";
    }
    
    //If you need to get the absolute cursor position, you can decomment these lines
    //cout << cursor_position;
    //scanf("\033[%d;%dR", &xcursor, &ycursor);
    
    string res;
    long nb;
    
    do {
        nb = read(0,buf,_getbuffsize);
        if (nb < 0)
            perror("read()");
        buf[nb] = 0;
        res += buf;
        memset(buf,0, _getbuffsize);
    }
    while (nb == _getbuffsize);
    
    
    remove_echo.c_lflag|=ICANON;
    if (!isMouseAction(res))
        remove_echo.c_lflag|=ECHO;
    if(tcsetattr(0, TCSADRAIN, &remove_echo)<0) {
        perror ("tcsetattr ~ICANON");
        return "";
    }
    
    return res;
}
#endif

void jag_get::screensizes() {
#ifdef WIN32
	system("cls");
	Getscreensizes(mouse_status);
    Returnscreensize(row_size, col_size, size_row, size_col);
    row_size -= 1;
    col_size -= margin;
#else
    ioctl(STDOUT_FILENO, TIOCGWINSZ, &wns);
    row_size = wns.ws_row - 2; //we need to keep a final line on screen for operators
    col_size = wns.ws_col - margin;
    if (row_size < 0)
        row_size = 55;
    if (col_size < 0)
        col_size = 190;
#endif
    //We set the botton limit for scrolling
    char buff[] = { 0,0,0,0,0,0 };
    sprintf_s(buff,4, "%0.3ld", row_size+2);
    m_scrollmargin[6] = buff[0];
    m_scrollmargin[7] = buff[1];
    m_scrollmargin[8] = buff[2];
    cout << m_scrollmargin;
}

void jag_get::reset() {
#ifndef WIN32
    tcsetattr(0, TCSADRAIN, &oldterm);
    
    termios theterm;
    tcgetattr(0, &theterm);
    theterm.c_iflag &= ~IXON;
    theterm.c_iflag |= IXOFF;
    //The next modifications allows for the use of ctrl-q and ctrl-s
    theterm.c_cc[VSTART] = 0;
    theterm.c_cc[VSTOP] = 0;
    theterm.c_cc[VSUSP] = 0;
    tcsetattr(0, TCSADRAIN, &theterm);
#endif
}

//This our own implementation of a full input on screen
//that is compatible with the debug mode and different from
//std::cin
void jag_get::get_a_string(string& string_input) {
    string buff;
    std::wstring wbuff;
    std::wstring sub;
    cout << string_input;
    cout.flush();
    
    std::wstring input_string;
    
    s_utf8_to_unicode(input_string, string_input, string_input.size());
    
    long cursor = input_string.size();
    long i;
    
    while (true) {
        buff = getch();
        
#ifdef WIN32
    if (buff == c_homekey) {
        while (cursor != 0) {
            cout << m_oneleft;
            cursor--;
        }
        continue;
    }

    if (buff == c_endkey) {
        if (cursor < input_string.size()) {
            i = cursor;
            cursor = input_string.size();
            
            while (i < cursor) {
                cout << m_oneright;
                i++;
            }
        }
        continue;
    }
#endif
        
        if (buff[0] == 1) { //ctrl-a
            //beginning of the string
            while (cursor != 0) {
                cout << m_oneleft;
                cursor--;
            }
            continue;
        }
        
        if (buff[0] ==  5) { ////ctrl-e: at the end of the string
            if (cursor < input_string.size()) {
                i = cursor;
                cursor = input_string.size();
                
                while (i < cursor) {
                    cout << m_oneright;
                    i++;
                }
            }
            continue;
        }
        
        if (buff[0] == 11) { //ctrl-k, delete the rest of the string
            if (cursor == input_string.size())
                continue;
            cout << m_clear_line;
            input_string = input_string.substr(0, cursor);
            continue;
        }
        
        if (buff == left) {
            if (!cursor)
                continue;
            cursor--;
            cout << buff;
            continue;
        }
        
        if (buff == right) {
            if (cursor == input_string.size())
                continue;
            cursor++;
            cout << buff;
            continue;
        }

        if (buff == del) {
            if (!input_string.size())
                continue;
            if (cursor < input_string.size()) {
                sub = input_string.substr(cursor+1,input_string.size());
                input_string = input_string.substr(0, cursor)+sub;
                cout << m_deletechar;
            }
            continue;
        }

        if (buff[0] == m_delback || buff[0] == m_delbackbis) {
            if (!input_string.size())
                continue;
            cout << m_oneleft << m_deletechar;
            if (cursor < input_string.size()) {
                sub = input_string.substr(cursor,input_string.size());
                input_string = input_string.substr(0, cursor-1)+sub;
            }
            else
                input_string = input_string.substr(0, input_string.size()-1);
            cursor--;
            continue;
        }
        
        if (buff[0] == 10 || buff[0] == 13) {
            cout << buff;
            cout.flush();
            break;
        }
        
        cout << buff;
        s_utf8_to_unicode_clean(wbuff, buff, buff.size());
        
        if (cursor < input_string.size()) {
            sub = input_string.substr(cursor,input_string.size());
            input_string = input_string.substr(0, cursor)+wbuff+sub;
            s_unicode_to_utf8_clean(buff, sub);
            cout << buff;
            for (i = 0; i < sub.size(); i++)
                cout << m_oneleft;
        }
        else
            input_string += wbuff;
        cursor++;
    }
    
    s_unicode_to_utf8_clean(string_input, input_string);
}

