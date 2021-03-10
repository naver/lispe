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

static string version = "0.99 build 06";

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

#ifndef WIN32
    signal(SIGINT,handle_ctrl_c);
#endif

    JAGEDITOR = new jag_editor;
    JAGEDITOR->mouseon();
    JAGEDITOR->setnoprefix();
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
            cerr << m_redital << "-b:" << m_current << " to launch the editor in dark mode" << endl;
            cerr << m_redital << "-n:" << m_current << " to display lines with line number" << endl << endl;
            cerr << m_redital << "Ctrl-xh:" << m_current << " to display this help from within" << endl << endl;
            JAGEDITOR->displaythehelp(true);
            cerr << endl;
            JAGEDITOR->terminate();
            exit(0);
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
            JAGEDITOR->launchterminal(darkmode, true, args);
        else
            JAGEDITOR->launchterminal(darkmode, false, args);
        exit(0);
    }

    JAGEDITOR->launchterminal(darkmode, false, args);
}
