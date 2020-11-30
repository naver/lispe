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
const char m_current[] = {27, '[', '0', 'm', 0};
const char m_redital[] = {27, '[', '3', ';', '3','1', ';','4','9','m',0};
const char m_redbold[] = {27, '[', '1', ';', '3','1', ';','4','9','m',0};
const char m_redboldblink[] = {27, '[', '5', ';', '3','1', ';','4','9','m',0};
const char m_green[] = {27, '[', '0', ';', '3','2', ';','4','9','m',0};
const char m_dore[] = {27, '[', '0', ';', '3','3', ';','4','9','m',0};
#ifdef WIN32
const char m_red[] = { 27, '[', '1', ';', '3','1', ';','4','9','m',0 };
const char m_blue[] = { 27, '[', '0', ';', '3','6','m',0 };
const char m_blueblack[] = { 27, '[', '0', ';', '3','6', ';','4','9','m',0 };
#else
const char m_red[] = { 27, '[', '0', ';', '3','1', ';','4','9','m',0 };
const char m_blue[] = {27, '[', '0', ';', '3','4', ';','4','9','m',0};
const char m_blueblack[] = {27, '[', '0', ';', '3','6', ';','4','9','m',0};
#endif
const char m_gray[] = {27, '[', '0', ';', '9','0', ';','4','9','m',0};
const char m_lightgray[] = {27, '[', '0', ';', '9','0', ';','4','9','m',0};

const char m_selectgray[] = {27, '[', '7', ';', '9','3', ';','4','0','m',0};
#endif

const string colordenomination[] = {"strings", "methods", "keywords", "functions", "comments"};

    //Background
const int m_clbg[] = {40, 41, 42, 43, 44, 45, 46, 47, 49, 100, 101, 102, 103, 104, 105, 106, 107, 0};
    //Foreground
const int m_clfg[] =  {30, 31, 32, 33, 34, 35, 36, 37, 39, 90, 91, 92, 93, 94, 95, 96, 97, 0};
    //Formatting
const int m_attr[] = {0, 1, 2, 4, 5, 7, -1};

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

const char enablemouse[] = {27,91,'?','1','0','0','3','h',27,91,'?','1','0','1','5','h',27,91,'?','1','0','1','6','h',0};
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
    long spacemargin;

    bool inside_editor;
    bool initialized;
    bool mouse_status;
    
    jag_get(bool inside);
    ~jag_get() {
        resetterminal();
    }
    
    string getch();
    void resetterminal();
    void screensizes();
    void reset();
    void get_a_string(string&);
    void initialisation();

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
    
    bool isMouseAction(std::wstring& mousectrl) {
        return false;
    }
    
    void togglemouse() {}
    
#else
    
    void mouseon() {
        if (!mouse_status) {
            sendcommand(enablemouse);
            mouse_status = true;
        }
    }
    
    void mouseoff() {
        if (mouse_status) {
            sendcommand(disablemouse);
            mouse_status = false;
        }
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
    
    void mouseLocation(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
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
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
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
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
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

    bool isClickFirstMouseDown(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
            //This a move
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
            if (action == 32) {
                vect.push_back(mxcursor);
                vect.push_back(mycursor);
                return true;
            }
        }
        return false;
    }

    bool isClickSecondMouseDown(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
            //This a move
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
            if (action == 34) {
                vect.push_back(mxcursor);
                vect.push_back(mycursor);
                return true;
            }
        }
        return false;
    }

    bool isClickMouseUp(vector<int>& vect, string& mousectrl) {
        int action, mxcursor, mycursor;
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
            //This a move
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
            if (action == 35) {
                vect.push_back(mxcursor);
                vect.push_back(mycursor);
                return true;
            }
        }
        return false;
    }

    bool mouseTracking(string& mousectrl, int& mxcursor, int& mycursor) {
        int action;
        if (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[') {
            //This a move
            sscanf(STR(mousectrl), "\033[%d;%d;%dM", &action, &mycursor, &mxcursor);
            if (action == 64)
                return true;
        }
        return false;
    }

    bool isMouseAction(string mousectrl) {
        return (mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[');
    }

    bool isMouseAction(std::wstring& mousectrl) {
        return (mousectrl.size() >= 8 && mousectrl.back() == L'M' && mousectrl[0] == 27 && mousectrl[1] == L'[');
    }

#endif
    
};

#endif
