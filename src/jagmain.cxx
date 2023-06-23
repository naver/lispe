/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  jagmain.cxx
//
//

#include <stdio.h>
#ifndef WIN32
#include <unistd.h>   //_getch
#include <termios.h>  //_getch
#include <sys/ioctl.h>
#endif

#include <signal.h>
#include <iomanip>

#include <string>

#include "jag.h"

static string version = "0.99 build 18";

#ifndef WIN32
static void handle_ctrl_c(int theSignal) {
    if (JAGEDITOR->tobesaved) {
        JAGEDITOR->tobesaved = false;
        JAGEDITOR->displayonlast("File not saved... ctrl-c again to quit", true);
        std::cout.flush();
        return;
    }
    
    JAGEDITOR->terminate();
    exit(-1);
}
#endif

bool movedup() {
    return JAGEDITOR->moveup;
}

int main(int argc, char *argv[]) {
    
    vector<string> newcolors;
    
#ifndef WIN32
    signal(SIGINT,handle_ctrl_c);
#endif
    
    JAGEDITOR = new jag_editor;
    JAGEDITOR->setnoprefix();
    
    std::vector<std::string> args;
    bool darkmode = false;
    int i = 1;
    string cmd;
    
#if defined(APPLE) || defined(WIN32)
    JAGEDITOR->mouseon();
#endif
    
    while(i < argc) {
        cmd = argv[i++];
        if (cmd == "-h" || cmd == "-help" || cmd == "--help" || cmd == "--h") {
            JAGEDITOR->mouseoff();
            cout << m_clear << m_home;
            cerr << m_red << "jag(작): micro text processor (version: " << version << ")" << m_current << endl;
            cerr << m_red << "Copyright 2019-present NAVER Corp." << m_current << endl;
            cerr << m_redital << "-b:" << m_current << " to launch the editor in dark mode" << endl;
            cerr << m_redital << "-n:" << m_current << " to display lines with line number" << endl << endl;
            cerr << m_redital << "-wn:" << m_current << " Windows mode (before -m)" << endl << endl;
            cerr << m_redital << "-vt100:" << m_current << " VT100 mouse mode (before -m)" << endl << endl;
            cerr << m_redital << "-m:" << m_current << " to activate mouse at launch" << endl << endl;
            cerr << m_redital << "Ctrl-xh:" << m_current << " to display this help from within" << endl << endl;
            cout << m_red << "-syncolor (Colors for: strings, definition, instruction, quote, comments, call), 3 digits each" << endl;
            cout << m_redital << "     -syncolor no (no colors)" << endl << endl;
            cout << m_redital << "     -syncolor att fg bg att fg bg att fg bg att fg bg att fg bg att fg bg" << endl << endl;
            JAGEDITOR->displaythehelp(true);
            cerr << endl;
            JAGEDITOR->terminate();
            exit(0);
        }
        
        if (cmd == "-syncolor") {
            if (i < argc) {
                string args = argv[i + 1];
                if (args == "no") {
                    for (long j = 0; j < nbdenomination - 1; j++)
                        newcolors.push_back(m_current);
                    newcolors.push_back(m_selectgray);
                    i++;
                    continue;
                }
            }
            long nb = 3*nbdenomination;
            if ((i + nb) >= argc) {
                cerr << "There should be: "<< nb << " values, 3 digits for each denomination: string, definition, instruction, quote, comments, call, selection" << endl;
                JAGEDITOR->mouseoff();
                exit(-1);
            }
            i++;
            nb += i;
            long col;
            char cp = 0;
            stringstream color;
            color <<  "\033[";
            string args;
            while (i < nb) {
                args = argv[i++];
                col = convertinginteger(args);
                color << col;
                cp++;
                if (cp == 3) {
                    cp = 0;
                    color << "m";
                    newcolors.push_back(color.str());
                    color.str("");
                    color << "\033[";
                }
                else
                    color << ";";
            }
            i--;
            continue;
        }
        
        if (cmd == "-m") {
            JAGEDITOR->mouseon();
            continue;
        }
        
        if (cmd == "-vt100") {
            JAGEDITOR->vt100 = true;
            continue;
        }
        
        if (cmd == "-wn") {
            JAGEDITOR->set_window_control_codes();
            continue;
        }
        
        if (cmd == "-b") {
            darkmode = true;
            continue;
        }
        
        if (cmd == "-n") {
            JAGEDITOR->setnoprefix();
            continue;
        }
        
        if (cmd[0] == '-') {
            cerr << "Unknown command: " << cmd << endl;
            exit(0);
        }
        
        if (JAGEDITOR->loadfile(cmd))
            JAGEDITOR->launchterminal(darkmode, true, args, newcolors);
        else
            JAGEDITOR->launchterminal(darkmode, false, args, newcolors);
        exit(0);
    }
    
    JAGEDITOR->launchterminal(darkmode, false, args, newcolors);
}

