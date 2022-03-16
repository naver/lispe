/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  jagget.h
//
//

#ifndef jagget_h
#define jagget_h

//--------------------------------------------------------------------------------
#include <string>
#include <vector>
#include <unordered_map>
#include <list>
#include <ostream>
#include <iostream>
#include <fstream>
#include <sstream>

#define STR(x) (char*)x.c_str()
#define USTR(x) (uchar*)x.c_str()
#define uchar unsigned char
#define sprintf_s snprintf
#define swprintf_s swprintf

using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::vector;

//--------------------------------------------------------------------------------
extern long margin_value_reference;
//--------------------------------------------------------------------------------

#ifdef PASDECOULEUR
const char m_current[] = {0,0};
const char m_red[] = {0,0};
const char m_redital[] = {0,0};
const char m_redbold[] = {0,0};
const char m_green[] = {0,0};
const char m_dore[] = {0,0};
const char m_blue[] = {0,0};
const char m_blueblack[] = {0,0};
const char m_gray[] = {0,0};
const char m_lightgray[] = {0,0};
const char m_selectgray[] = {0,0};
#else
const char m_current[] = "\033[0m";
const char m_redital[] = "\033[3;31;49m";
const char m_redbold[] = "\033[1;31;49m";
const char m_redboldblink[] = "\033[5;31;49m";
const char m_green[] = "\033[0;32;49m";
const char m_dore[] = "\033[0;33;49m";
const char m_dark_yellow[] = "\033[0;93;40m";
const char m_yellow[] = "\033[1;91;49m";
#ifdef WIN32
const char m_red[] = "\033[1;31;49m";
const char m_blue[] = "\033[0;36m";
const char m_blueblack[] = "\033[0;36;49m";
#else
const char m_red[] = "\033[0;31;49m";
const char m_blue[] = "\033[0;34;49m";
const char m_blueblack[] = "\033[0;36;49m";
#endif
const char m_ital[] = "\033[3m";
const char m_dark_function[] = "\033[0;33;40m";

const char m_gray[] = "\033[0;90;49m";
const char m_lightgray[] = "\033[0;90;49m";

const char m_selectgray[] = "\033[7;93;40m";
#endif

//Background
const int m_clbg[] = {40, 41, 42, 43, 44, 45, 46, 47, 49, 100, 101, 102, 103, 104, 105, 106, 107, 0};
//Foreground
const int m_clfg[] =  {30, 31, 32, 33, 34, 35, 36, 37, 39, 90, 91, 92, 93, 94, 95, 96, 97, 0};
//Formatting
const int m_attr[] = {0, 1, 2, 3, 4, 5, 7, -1};

//action, moving the cursor...
extern char m_down[];
const char m_up[] = {27, '[', '1', 65, 0};
//On macos you might need to insert these key combination in the keyboard preference as: \033[1;5A and \033[1;5B

//arrow moving

const char a_right[] = {27, 102, 0};
const char a_left[] = {27, 98, 0};

const char m_clear_line[] = { 27, 91, '0', 'K', 0 };
const char m_clear[] = {27, 91, '2', 'J', 0};
const char m_clear_scrolling[] = {27, 91, '3', 'J', 0};
const char m_scrollup[] = {27, 91, '1', 'S', 0};
const char m_scrollup3[] = { 27, 91, '3', 'S', 0 };
const char m_scrolldown[] = { 27, 91, '1', 'T', 0 };
const char m_insertline[] = { 27, 91, '1', 'L', 0 };
const char m_deleteline[] = { 27, 91, '1', 'M', 0 };
const char m_home[] = {27, 91, 'H', 0};

#ifdef WIN32
const uchar is_up = 72;
const uchar is_down = 80;
const char right[] = { 224, 77, 0 };
const char left[] = { 224, 75, 0 };
const char down[] = {224,80, 0 };
const char up[] = {224,72, 0 };
const char del[] = { 224, 83, 0 };
const char homekey[] = { 224, 71, 0 };
const char endkey[] = { 224, 79, 0 };
const char c_homekey[] = { 224, 119, 0 };
const char c_endkey[] = { 224, 117, 0 };
const char c_up[] = { 224, 73, 0 };
const char c_down[] = { 224,81, 0 };
const char c_right[] = { 224,116, 0 };
const char c_left[] = { 224,115, 0 };
const unsigned char alt_x[] = { 226, 'x', 0 };
const unsigned char alt_c[] = { 226, 'c', 0 };
const unsigned char alt_v[] = { 226, 'v', 0 };
const unsigned char alt_plus[] = { 226, '+', 0 };
const unsigned char alt_minus[] = { 226, '-', 0 };
const unsigned char shift_right[] = { 225, 77, 0 };
const unsigned char shift_left[] = { 225, 75, 0 };
#else
const char c_right[] = { 27, 91, 49, 59, 53, 67, 0 };
const char c_left[] = { 27, 91, 49, 59, 53, 68, 0 };
#ifdef APPLE
const char c_up[] = { 27, 91, 53, 126, 0 };
const char c_down[] = { 27, 91, 54, 126, 0 };
#else
const char c_up[] = { 27, 91, 49, 59, 53, 65, 0 }; //\033[1;5A
const char c_down[] = { 27, 91, 49, 59, 53, 66, 0 }; //\033[1;5B
#endif

#ifdef XTERM_MOUSE_VT100
const char enablemouse[] = {27,91,'?','1','0','0','3','h',0};
#else
const char enablemouse[] = {27,91,'?','1','0','0','3','h',27,91,'?','1','0','1','5','h',27,91,'?','1','0','1','6','h',0};
#endif
const char disablemouse[] = {27,91,'?','1','0','0','0','l',0};
const char showcursor[] = {27,91,'?','2','5','h',0};
const char hidecursor[] = {27,91,'?','2','5','l',0};
const char cursor_position[] = { 27, 91, '6', 'n', 0 };

const uchar is_up = 65;
const uchar is_down = 66;
const char right[] = { 27, 91, 67, 0 };
const char left[] = { 27, 91, 68, 0 };
const char down[] = {27, 91, 66, 0};
const char up[] = {27, 91, 65, 0};
const char del[] = { 27, 91, 51, 126, 0 };
const char homekey[] = { 27, 91, 72, 0 };
const char endkey[] = { 27, 91, 70, 0 };

const unsigned char alt_plus[] = {226, 137, 160, 0};
const unsigned char alt_minus[] = {226, 128, 148, 0};

const unsigned char alt_x[] = {226, 137, 136, 0};
const unsigned char alt_c[] = {194, 169, 0};
const unsigned char alt_v[] = {226, 151, 138, 0};
const unsigned char shift_right[] = {27, 91, 49, 59, 50, 67, 0};
const unsigned char shift_left[] = {27, 91, 49, 59, 50, 68, 0};
#endif


const char back = 13;

//--------------------------------------------------------------------------------
class jag_get {
public:
#ifdef WIN32
    long size_row, size_col;
#else
    struct termios oldterm;
    struct winsize wns;
#endif
    
    long row_size, col_size;
    long margin;
    
    int nbclicks;
    
    bool inside_editor;
    bool initialized;
    bool mouse_status;
    
    jag_get(bool inside);
    ~jag_get() {
        resetterminal();
    }
    
#ifdef WIN32
    virtual string getch();
#else
    string getch();
#endif
    void resetterminal();
    void screensizes();
    void reset();
    void get_a_string(string&);
    void initialisation();
    void clearline() {
        cout << back << m_clear_line << back;
    }
    
    void sendcommand(string s) {
        cout.flush();
        cout << s;
    }
    
#ifdef WIN32
    void mouseon() {
        mouse_status = true;
    }
    
    void mouseoff() {
        mouse_status = false;
    }
    
    void togglemouse() {
        mouse_status = 1 - mouse_status;
    }
    
#else
    
    void mouseon() {
        sendcommand(enablemouse);
        mouse_status = true;
    }
    
    void mouseoff() {
        sendcommand(disablemouse);
        mouse_status = false;
    }
    
    void togglemouse() {
        if (!mouse_status) {
            sendcommand(enablemouse);
            mouse_status = true;
        }
        else {
            sendcommand(disablemouse);
            mouse_status = false;
        }
    }
#endif
    
#ifdef XTERM_MOUSE_VT100
    bool isScrollingUp(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M') {
            //This a move
            action = mousectrl[3];
            mxcursor = mousectrl[4] - 32;
            mycursor = mousectrl[5] - 32;
            if (action == 96) {
                vect.push_back(mycursor);
                vect.push_back(mxcursor);
                return true;
            }
        }
        return false;
    }
    
    bool isScrollingDown(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M') {
            //This a move
            action = mousectrl[3];
            mxcursor = mousectrl[4] - 32;
            mycursor = mousectrl[5] - 32;
            if (action == 97) {
                vect.push_back(mycursor);
                vect.push_back(mxcursor);
                return true;
            }
        }
        return false;
    }
    
    long nbClicks() {
        return nbclicks;
    }
    
    bool isClickFirstMouseDown(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M') {
            action = mousectrl[3];
            mxcursor = mousectrl[4] - 32;
            mycursor = mousectrl[5] - 32;
            if (action == 32) {
                vect.push_back(mycursor);
                vect.push_back(mxcursor);
                return true;
            }
        }
        return false;
    }
    
    bool checkMouseup(string& mousectrl) {
        string mouse_up;
        mouse_up = 27;
        mouse_up += '[';
        mouse_up += 35;
        
        if (mouse_status && mousectrl.size() >= 6 && mousectrl.back() == 'M') {
            return (mousectrl.find(mouse_up) != -1);
        }
        return false;
    }

    bool isClickMouseUp(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M') {
            //This a move
            action = mousectrl[3];
            mxcursor = mousectrl[4] - 32;
            mycursor = mousectrl[5] - 32;
            if (action == 35) {
                vect.push_back(mycursor);
                vect.push_back(mxcursor);
                return true;
            }
        }
        return false;
    }
    
    bool isClickSecondMouseDown(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M') {
            //This a move
            action = mousectrl[3];
            mxcursor = mousectrl[4] - 32;
            mycursor = mousectrl[5] - 32;
            if (action == 34) {
                vect.push_back(mycursor);
                vect.push_back(mxcursor);
                return true;
            }
        }
        return false;
    }
    
    bool mouseTracking(string& mousectrl, int& mxcursor, int& mycursor) {
        int action;
        if (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M') {
            //This a move
            action = mousectrl[3];
            mxcursor = mousectrl[5] - 32;
            mycursor = mousectrl[4] - 32;
            if (action == 64)
                return true;
        }
        return false;
    }
    
    bool isMouseAction(string mousectrl) {
        return (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M');
    }
    
    bool isMouseAction(std::wstring& mousectrl) {
        return (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M');
    }
};

#else
void mouseLocation(vector<int>& vect, string& mousectrl) {
    int action, mxcursor, mycursor;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
        if (action == 67) {
            vect.push_back(mxcursor);
            vect.push_back(mycursor);
        }
    }
}

bool isScrollingUp(vector<int>& vect, string& mousectrl) {
    int action, mxcursor, mycursor;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
        if (action == 96) {
            vect.push_back(mxcursor);
            vect.push_back(mycursor);
            return true;
        }
    }
    return false;
}

bool isScrollingDown(vector<int>& vect, string& mousectrl) {
    int action, mxcursor, mycursor;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
        if (action == 97) {
            vect.push_back(mxcursor);
            vect.push_back(mycursor);
            return true;
        }
    }
    return false;
}

long nbClicks() {
    return nbclicks;
}

bool isClickFirstMouseDown(vector<int>& vect, string& mousectrl) {
    int action, mxcursor, mycursor;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
#ifdef WIN32
        //On Windows, a double-click contains a D
        if (mousectrl[mousectrl.size() - 2] == 'D') {
            sscanf(STR(mousectrl), "\033[%d;%d;%dDM", &action, &mycursor, &mxcursor);
            nbclicks = 2;
        }
        else {
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
            nbclicks = 1;
        }
#else
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
#endif
        if (action == 32) {
            vect.push_back(mxcursor);
            vect.push_back(mycursor);
            return true;
        }
    }
    return false;
}

bool checkMouseup(string& mousectrl) {
    string mouse_up;
    mouse_up = 27;
    mouse_up += '[';
    mouse_up += "35";
    
    
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M') {
        return (mousectrl.find(mouse_up) != -1);
    }
    return false;
}

bool isClickMouseUp(vector<int>& vect, string& mousectrl) {
    int action, mxcursor, mycursor;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
#ifdef WIN32
        //On Windows, a double-click contains a D
        if (mousectrl[mousectrl.size() - 2] == 'D')
            sscanf(STR(mousectrl), "\033[%d;%d;%dDM", &action, &mycursor, &mxcursor);
        else
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
#else
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
#endif
        
        if (action == 35) {
            vect.push_back(mxcursor);
            vect.push_back(mycursor);
            return true;
        }
    }
    return false;
}

bool isClickSecondMouseDown(vector<int>& vect, string& mousectrl) {
    int action, mxcursor, mycursor;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
#ifdef WIN32
        //On Windows, a double-click contains a D
        if (mousectrl[mousectrl.size() - 2] == 'D') {
            sscanf(STR(mousectrl), "\033[%d;%d;%dDM", &action, &mycursor, &mxcursor);
            nbclicks = 2;
        }
        else {
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
            nbclicks = 1;
        }
#else
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
#endif
        if (action == 34) {
            vect.push_back(mxcursor);
            vect.push_back(mycursor);
            return true;
        }
    }
    return false;
}

bool mouseTracking(string& mousectrl, int& mxcursor, int& mycursor) {
    int action;
    if (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
        //This a move
        sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
        if (action == 64)
            return true;
    }
    return false;
}

bool isMouseAction(string& mousectrl) {
    return (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[');
}

bool isMouseAction(std::wstring& mousectrl) {
    return (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == L'M' && mousectrl[0] == 27 && mousectrl[1] == L'[');
}
};
#endif
#endif
