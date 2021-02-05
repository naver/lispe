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

#include <unistd.h>   //_getch
#include <termios.h>  //_getch
#include <sys/ioctl.h>
#include <signal.h>
#include <iomanip>

#include <string>

#include "jag.h"

static string version = "0.99 build 01";

#ifndef WIN32
static void handle_ctrl_c(int theSignal) {
    if (JAGEDITOR->tobesaved) {
        JAGEDITOR->tobesaved = false;
        JAGEDITOR->displayonlast("File not saved... ctrl-c again to quit", true);
        std::cout.flush();
        return;
    }

    JAGEDITOR->clear();
    exit(-1);
}
#endif

int main(int argc, char *argv[]) {
    
#ifndef WIN32
    signal(SIGINT,handle_ctrl_c);
#endif

    JAGEDITOR = new jag_editor;
    JAGEDITOR->mouseon();
    std::vector<std::string> args;
    bool darkmode = false;
    int i = 1;
    string cmd;
    
    while(i < argc) {
        cmd = argv[i++];
        if (cmd == "-h" || cmd == "-help" || cmd == "--help" || cmd == "--h") {
            cout << m_clear << m_home;
            cerr << m_red << "jag(작): micro text processor (version: " << version << ")" << m_current << endl;
            cerr << m_red << "Copyright 2019-present NAVER Corp." << m_current << endl;
            cerr << m_redital << "Ctrl-xh:" << m_current << " to display this help from within" << endl << endl;
            JAGEDITOR->displaythehelp(true);
            cerr << endl;
            exit(0);
        }
        if (cmd == "-b") {
            darkmode = true;
            continue;
        }
        
        if (JAGEDITOR->loadfile(cmd))
            JAGEDITOR->launchterminal(darkmode, true, args);
        else
            JAGEDITOR->launchterminal(darkmode, false, args);
        exit(0);
    }
    
    JAGEDITOR->launchterminal(darkmode, false, args);
}

