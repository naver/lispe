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
const uchar u_is_up = 72;
const uchar u_is_down = 80;
const char u_right[] = { 224, 77, 0 };
const char u_left[] = { 224, 75, 0 };
const char u_down[] = {224,80, 0 };
const char u_up[] = {224,72, 0 };
const char u_delete[] = { 224, 83, 0 };
const char u_homekey[] = { 224, 71, 0 };
const char u_endkey[] = { 224, 79, 0 };
const char c_homekey[] = { 224, 119, 0 };
const char c_endkey[] = { 224, 117, 0 };
const char u_c_up[] = { 224, 73, 0 };
const char u_c_down[] = { 224,81, 0 };
const char u_page_up[] = { 224, 73, 0 };
const char u_page_down[] = { 224,81, 0 };
const char u_c_right[] = { 224,116, 0 };
const char u_c_left[] = { 224,115, 0 };
const unsigned char u_alt_xbis[] = { 226, 'x', 0 };
const unsigned char u_alt_cbis[] = { 226, 'c', 0 };
const unsigned char u_alt_vbis[] = { 226, 'v', 0 };

const unsigned char u_alt_x[] = { 226, 'x', 0 };
const unsigned char u_alt_c[] = { 226, 'c', 0 };
const unsigned char u_alt_v[] = { 226, 'v', 0 };
const unsigned char u_alt_plus[] = { 226, '+', 0 };
const unsigned char u_alt_minus[] = { 226, '-', 0 };
const unsigned char u_shift_right[] = { 225, 77, 0 };
const unsigned char u_shift_left[] = { 225, 75, 0 };
#else
const char u_c_right[] = { 27, 91, 49, 59, 53, 67, 0 };
const char u_c_left[] = { 27, 91, 49, 59, 53, 68, 0 };
#ifdef __apple_build_version__
const char u_c_up[] = { 27, 91, 53, 126, 0 };
const char u_c_down[] = { 27, 91, 54, 126, 0 };
#else
const char u_c_up[] = { 27, 91, 49, 59, 53, 65, 0 }; //\033[1;5A
const char u_c_down[] = { 27, 91, 49, 59, 53, 66, 0 }; //\033[1;5B
#endif

const char enablemouse100[] = {27,91,'?','1','0','0','3','h',0};
const char enablemouse[] = {27,91,'?','1','0','0','3','h',27,91,'?','1','0','1','5','h',27,91,'?','1','0','1','6','h',0};

const char disablemouse100[] = {27,91,'?','1','0','0','3','l',0};
const char disablemouse[] = {27,91,'?','1','0','0','0','l',0};
const char showcursor[] = {27,91,'?','2','5','h',0};
const char hidecursor[] = {27,91,'?','2','5','l',0};
const char cursor_position[] = { 27, 91, '6', 'n', 0 };

const uchar u_is_up = 65;
const uchar u_is_down = 66;
const char u_right[] = { 27, 91, 67, 0 };
const char u_left[] = { 27, 91, 68, 0 };
const char u_down[] = {27, 91, 66, 0};
const char u_up[] = {27, 91, 65, 0};
const char u_delete[] = { 27, 91, 51, 126, 0 };
const char u_page_up[] = { 27, 91, '5', '~', 0 }; //\033[5~
const char u_page_down[] = { 27, 91, '6', '~', 0 }; //\033[6~
const char u_homekey[] = { 27, 91, 72, 0 };
const char u_endkey[] = { 27, 91, 70, 0 };

const unsigned char u_alt_plus[] = {226, 137, 160, 0};
const unsigned char u_alt_minus[] = {226, 128, 148, 0};

const unsigned char u_alt_xbis[] = { 27, 'x', 0 };
const unsigned char u_alt_cbis[] = { 27, 'c', 0 };
const unsigned char u_alt_vbis[] = { 27, 'v', 0 };

const unsigned char u_alt_x[] = {226, 137, 136, 0};
const unsigned char u_alt_c[] = {194, 169, 0};
const unsigned char u_alt_v[] = {226, 151, 138, 0};
const unsigned char u_shift_right[] = {27, 91, 49, 59, 50, 67, 0};
const unsigned char u_shift_left[] = {27, 91, 49, 59, 50, 68, 0};
#endif

extern unsigned char* alt_c;
extern char* alt_vbis;
extern unsigned char* alt_v;
extern char* page_down;
extern unsigned char* alt_x;
extern uchar is_up;
extern char* c_down;
extern char* c_left;
extern unsigned char* alt_xbis;
extern char* c_up;
extern char* endkey;
extern char* down;
extern char* homekey;
extern char* up;
extern char* right;
extern unsigned char* alt_cbis;
extern unsigned char* shift_left;
extern char* page_up;
extern char* left;
extern unsigned char* alt_plus;
extern uchar is_down;
extern char* c_right;
extern unsigned char* alt_minus;
extern unsigned char* shift_right;
extern char* del;

const uchar wnd_is_up = 72;
const uchar wnd_is_down = 80;
const uchar wnd_right[] = { 224, 77, 0 };
const uchar wnd_left[] = { 224, 75, 0 };
const uchar wnd_down[] = {224,80, 0 };
const uchar wnd_up[] = {224,72, 0 };
const uchar wnd_del[] = { 224, 83, 0 };
const uchar wnd_homekey[] = { 224, 71, 0 };
const uchar wnd_endkey[] = { 224, 79, 0 };
const uchar wnd_c_homekey[] = { 224, 119, 0 };
const uchar wnd_c_endkey[] = { 224, 117, 0 };
const uchar wnd_c_up[] = { 224, 73, 0 };
const uchar wnd_c_down[] = { 224,81, 0 };
const uchar wnd_page_up[] = { 224, 73, 0 };
const uchar wnd_page_down[] = { 224,81, 0 };
const uchar wnd_c_right[] = { 224,116, 0 };
const uchar wnd_c_left[] = { 224,115, 0 };
const unsigned char wnd_alt_xbis[] = { 226, 'x', 0 };
const unsigned char wnd_alt_cbis[] = { 226, 'c', 0 };
const unsigned char wnd_alt_vbis[] = { 226, 'v', 0 };

const unsigned char wnd_alt_x[] = { 226, 'x', 0 };
const unsigned char wnd_alt_c[] = { 226, 'c', 0 };
const unsigned char wnd_alt_v[] = { 226, 'v', 0 };
const unsigned char wnd_alt_plus[] = { 226, '+', 0 };
const unsigned char wnd_alt_minus[] = { 226, '-', 0 };
const unsigned char wnd_shift_right[] = { 225, 77, 0 };
const unsigned char wnd_shift_left[] = { 225, 75, 0 };

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
    bool vt100;
    
    bool activate_mouse;
    
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
        cerr << s;
    }
    
#ifdef WIN32
    void mouseon() {
        mouse_status = true;
        activate_mouse = true;
    }
    
    void mouseoff() {
        mouse_status = false;
    }
    
    void togglemouse() {
        mouse_status = 1 - mouse_status;
        if (mouse_status)
            activate_mouse = true;
    }
    
#else
    
    void mouseon() {
        if (vt100)
            sendcommand(enablemouse100);
        else
            sendcommand(enablemouse);
        mouse_status = true;
        activate_mouse = true;
    }
    
    void mouseoff() {
        if (vt100)
            sendcommand(disablemouse100);
        else
            sendcommand(disablemouse);
        mouse_status = false;
    }
    
    void togglemouse() {
        if (!mouse_status) {
            if (vt100)
                sendcommand(enablemouse100);
            else
                sendcommand(enablemouse);
            mouse_status = true;
            activate_mouse = true;
        }
        else {
            if (vt100)
                sendcommand(disablemouse100);
            else
                sendcommand(disablemouse);
            mouse_status = false;
        }
    }
#endif
    
    void set_window_control_codes() {
        alt_c = (unsigned char*)wnd_alt_c;
        alt_vbis = (char*)wnd_alt_vbis;
        alt_v = (unsigned char*)wnd_alt_v;
        page_down = (char*)wnd_page_down;
        alt_x = (unsigned char*)wnd_alt_x;
        is_up = wnd_is_up;
        c_down = (char*)wnd_c_down;
        c_left = (char*)wnd_c_left;
        alt_xbis = (unsigned char*)wnd_alt_xbis;
        c_up = (char*)wnd_c_up;
        endkey = (char*)wnd_endkey;
        down = (char*)wnd_down;
        homekey = (char*)wnd_homekey;
        up = (char*)wnd_up;
        right = (char*)wnd_right;
        alt_cbis = (unsigned char*)wnd_alt_cbis;
        shift_left = (unsigned char*)wnd_shift_left;
        page_up = (char*)wnd_page_up;
        left = (char*)wnd_left;
        alt_plus = (unsigned char*)wnd_alt_plus;
        is_down = wnd_is_down;
        c_right = (char*)wnd_c_right;
        alt_minus = (unsigned char*)wnd_alt_minus;
        shift_right = (unsigned char*)wnd_shift_right;
        del = (char*)wnd_del;
    }


    void init_control_codes() {
        alt_c = (unsigned char*)u_alt_c;
        alt_vbis = (char*)u_alt_vbis;
        alt_v = (unsigned char*)u_alt_v;
        page_down = (char*)u_page_down;
        alt_x = (unsigned char*)u_alt_x;
        is_up = u_is_up;
        c_down = (char*)u_c_down;
        c_left = (char*)u_c_left;
        alt_xbis = (unsigned char*)u_alt_xbis;
        c_up = (char*)u_c_up;
        endkey = (char*)u_endkey;
        down = (char*)u_down;
        homekey = (char*)u_homekey;
        up = (char*)u_up;
        right = (char*)u_right;
        alt_cbis = (unsigned char*)u_alt_cbis;
        shift_left = (unsigned char*)u_shift_left;
        page_up = (char*)u_page_up;
        left = (char*)u_left;
        alt_plus = (unsigned char*)u_alt_plus;
        is_down = u_is_down;
        c_right = (char*)u_c_right;
        alt_minus = (unsigned char*)u_alt_minus;
        shift_right = (unsigned char*)u_shift_right;
        del = (char*)u_delete;
    }

    bool isScrollingUp100(vector<int>& vect, string& mousectrl) {
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
    
    bool isScrollingDown100(vector<int>& vect, string& mousectrl) {
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
    
    bool isClickFirstMouseDown100(vector<int>& vect, string& mousectrl) {
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
    
    bool checkMouseup100(string& mousectrl) {
        string mouse_up;
        mouse_up = 27;
        mouse_up += '[';
        mouse_up += 35;
        
        if (mouse_status && mousectrl.size() >= 6 && mousectrl.back() == 'M') {
            return (mousectrl.find(mouse_up) != -1);
        }
        return false;
    }

    bool isClickMouseUp100(vector<int>& vect, string& mousectrl) {
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
    
    bool isClickSecondMouseDown100(vector<int>& vect, string& mousectrl) {
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
    
    bool mouseTracking100(string& mousectrl, int& mxcursor, int& mycursor) {
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
    
    bool isMouseAction100(string mousectrl) {
        return (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M');
    }
    
    bool isMouseAction100(std::wstring& mousectrl) {
        return (mouse_status && mousectrl.size() >= 6 && mousectrl[0] == 27 && mousectrl[1] == '[' && mousectrl[2] == 'M');
    }
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
        if (vt100)
            return isScrollingUp100(vect, mousectrl);
        
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
        if (vt100)
            return isScrollingDown100(vect, mousectrl);
        
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
        if (vt100)
            return isClickFirstMouseDown100(vect, mousectrl);
        
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
        if (vt100)
            return checkMouseup100(mousectrl);
        
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
        if (vt100)
            return isClickMouseUp100(vect, mousectrl);
        
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
        if (vt100)
            return isClickSecondMouseDown100(vect, mousectrl);
        
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
        if (vt100)
            return mouseTracking100(mousectrl, mxcursor, mycursor);
        
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
        if (vt100)
            return isMouseAction100(mousectrl);
        
        return (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == 'M' && mousectrl[0] == 27 && mousectrl[1] == '[');
    }
    
    bool isMouseAction(std::wstring& mousectrl) {
        if (vt100)
            return isMouseAction100(mousectrl);
        
        return (mouse_status && mousectrl.size() >= 8 && mousectrl.back() == L'M' && mousectrl[0] == 27 && mousectrl[1] == L'[');
    }
};
#endif
