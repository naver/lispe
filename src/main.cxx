/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  main.cxx
//
//

#ifdef WIN32
#include <windows.h>
#endif

#ifdef WIN32
#define PATH_MAX 4096
#include <conio.h>
#endif

#include <stdio.h>
#include <fstream>
#include <thread>
#include <map>

#ifndef WIN32
#include <unistd.h>   //_getch
#include <termios.h>  //_getch
#include <sys/ioctl.h>

#define GCC_VERSION (__GNUC__ * 10000 \
+ __GNUC_MINOR__ * 100 \
+ __GNUC_PATCHLEVEL__)

#endif

#include <signal.h>
#include <iomanip>

#include <map>
#include "tools.h"
#include "jag.h"

#include "lispe.h"


using std::stringstream;
static vector<string> arguments;

//--------------------------------------
string Normalizefilename(string path) {
    char localpath[4096];
    #ifdef WIN32
    _fullpath(localpath, STR(path), 4096);
    #else
    realpath(STR(path), localpath);
    #endif
    return localpath;
}

#ifdef WIN32
BOOL WINAPI handle_ctrl_c(_In_ DWORD dwCtrlType) {
    if (JAGEDITOR != NULL)
        JAGEDITOR->clear();
    return true;
}
#else
static void handle_ctrl_c(int theSignal) {
    if (JAGEDITOR != NULL)
        JAGEDITOR->clear();
}
#endif

class lispe_editor;
void resizewindow(int theSignal);

void debuggerthread(lispe_editor* call);

//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
// The main editor class for handling LispE expressions and debugger
//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------

class lispe_editor : public jag_editor {
    Tokenizer parse;
    
    hmap<string, short> filenames;
    vector<string> ifilenames;
    vector<wstring> codes;
    
    vector<editor_keep> editors_undos;
    vector<editor_keep> editors_redos;
    vector<long> lastlines;

    std::thread* tid;
    
    unordered_map<long, unordered_map<long, bool> > breakpoints;
    
    vector<string> displaying;
    
    string historyfilename;

    long currentfileid;
    long pcursor;
    long idCode;
    long lastline;
    bool editmode;
    
    string current_code;

    
public:
    
    LispE* lispe;
    long current_line_debugger;
    bool debugmode;

    lispe_editor() {
        current_line_debugger = -1;
        debugmode = false;
        currentfileid = -1;
        editmode = false;
        pcursor = -1;
        idCode = -1;
        lastline = 0;
        lispe = NULL;
        tid = NULL;
    }
    
    bool checkbreakpoint() {
        try {
            return breakpoints.at(lispe->delegation->i_current_file).at(lispe->delegation->current_line);
        }
        catch(const std::out_of_range& oor) {
            return false;
        }
    }
    
    void LispSetCode(string& c) {
        current_code = c;
    }
    
    void LispSetCode(wstring& c) {
        current_code = convert(c);
    }
    
    wstring WListing() {
        return wconvert(current_code);
    }
    
    bool emode() {
        if (editmode && option == x_none)
            return true;
        return false;
    }
    
    
    string coloringline(wstring& l) {
        string line = convert(l);
        return coloringline(line);
    }
    
    string coloringline(string line) {
        if (line == "")
            return line;
        
        string sub = line;
        s_trimleft(sub);
        if (sub[0] == '#' || sub[0] == ';') {
            line = m_green + line + m_current;
            return line;
        }
        
        string substring;
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }
        bool add = false;
        
        parse.clear();
        lispe->delegation->add_to_listing = false;
        lispe->segmenting(line, parse);
        
        long left, right = -1;
        for (long isegment = parse.tokens.size() - 1, ipos = parse.positions.size() -1; ipos >= 0; ipos-=2, isegment--) {
            left = parse.positions[ipos-1];
            right = parse.positions[ipos];
            sub = line.substr(0, left);
            add = false;
            switch (parse.types[isegment]) {
                case e_emptystring:
                case e_string:
                    right += 1;
                    sub += colors[0];
                    add = true;
                    break;
                case e_token: //methods
                    if (lispe->is_instruction(parse.tokens[isegment])) {
                        sub += colors[2];
                        add = true;
                    }
                    break;
                default:
                    add = false;
            }
            
            if (add) {
                if (right > left)
                    sub += line.substr(left, right-left);
                sub += m_current;
                if (right < line.size())
                    sub += line.substr(right, line.size() - right);
                line = sub;
            }
            
        }
        return line;
    }
    
    void displaythehelp(long i) {
        cout << m_clear << m_home;
        
        cerr << endl << m_redbold << "Commandes :" << m_current << endl << endl ;
        
        if (!i || i == 1) {
            cerr << "   - " << m_redbold << "1. Programs:" << m_current << endl;
            cerr << "   \t- " << m_redbold << "create filename:" << m_current << " create a file space with a specific file name" << endl;
            cerr << "   \t- " << m_redbold << "open filename:" << m_current << " load a program (use "<< m_redital << "run" << m_current <<" to execute it)" << endl;
            cerr << "   \t- " << m_redbold << "open:" << m_current << " reload the program (use "<< m_redital << "run" << m_current <<" to execute it)" << endl;
            cerr << "   \t- " << m_redbold << "save filename:" << m_current << " save the buffer content in a file" << endl;
            cerr << "   \t- " << m_redbold << "save:" << m_current << " save the buffer content with the current filename" << endl;
            cerr << "   \t- " << m_redbold << "run filename:" << m_current << " load and run a program filename" << endl;
            cerr << "   \t- " << m_redbold << "run:" << m_current << " execute the current content" << endl;
            cerr << "   \t- " << m_redbold << "args v1 v2...vn:" << m_current << " set a list of values to the argument list" << endl;
            cerr << "   \t- " << m_redbold << "filename:" << m_current << " display the current filename" << endl;
            cerr << "   \t- " << m_redbold << "debug filename:" << m_current << " load and debug a program filename" << endl;
            cerr << "   \t- " << m_redbold << "debug:" << m_current << " debug the current content" << endl;
            cerr << "   \t\t- " << m_redbold << "While Debugging:" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << "$: execute the program up to the end" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << "!: stop the execution" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << "%: display all variables" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << ">: jump to the next breakpoint" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << "CR: execute the next line" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << "var: display the content of the variable 'var'" << m_current << endl;
            cerr << "   \t\t\t- " << m_redbold << "(...): execute some lisp code" << m_current << endl << endl;
        }
        
        if (!i || i == 2) {
            cerr << "   - " << m_redbold << "2. Command line mode:" << m_current << endl;
            cerr << "   \t- " << m_redbold << "help:" << m_current << " display the help" << endl;
            cerr << "   \t- " << m_redbold << "help n:" << m_current << " display one of the help sections (from 1 to 5)" << endl;
            cerr << "   \t- " << m_redbold << "cls:" << m_current << " clear screen" << endl;
            cerr << "   \t- " << m_redbold << "history:" << m_current << " display the command history" << endl;
            cerr << "   \t- " << m_redbold << "load filename:" << m_current << " load the command history from a file" << endl;
            cerr << "   \t- " << m_redbold << "store filename:" << m_current << " store the command history in a file" << endl;
            cerr << "   \t- " << m_redbold << "filename:" << m_current << " display the current file name" << endl;
            cerr << "   \t- " << m_redbold << "spaces:" << m_current << " display all the files stored in memory with their file space id" << endl;
            cerr << "   \t- " << m_redbold << "select space:" << m_current << " select a file space" << endl;
            cerr << "   \t- " << m_redbold << "create filename:" << m_current << " create a new empty file space" << endl;
            cerr << "   \t- " << m_redbold << "rm:" << m_current << " clear the buffer content" << endl;
            cerr << "   \t- " << m_redbold << "rm b:e:" << m_current << " remove the lines from b to e (b: or :e is also possible)" << endl;
            cerr << "   \t- " << m_redbold << "list:" << m_current << " list the buffer content" << endl;
            cerr << "   \t- " << m_redbold << "list b:e:" << m_current << " display the buffer content from line b to e (b: or :e is also possible)" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-t:" << m_current << " up in the code listing" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-g:" << m_current << " down in the code listing" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-f:" << m_current << " force the current line to be appended at the end of the code" << endl << endl;
        }
        
        if (!i || i == 3) {
            cerr << "   - " << m_redbold << "3. edit (space):" << m_current << " edit mode. You can optionally select also a file space" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-b:" << m_current << " add a breakpoint at the current line" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-k:" << m_current << " delete from cursor up to the end of the line" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-d:" << m_current << " delete a full line" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-u:" << m_current << " undo last modification" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-r:" << m_current << " redo last modification" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-f:" << m_current << " find a string" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-n:" << m_current << " find next" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-g:" << m_current << " move to a specific line, '$' is the end of the code" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-l:" << m_current << " toggle between top and bottom of the screen" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-t:" << m_current << " reindent the code" << endl;
#ifdef WIN32
            cerr << "   \t- " << m_redbold << "Ctrl+Alt-h:" << m_current << " local help" << endl;
#else
            cerr << "   \t- " << m_redbold << "Ctrl-h:" << m_current << " local help" << endl;
#endif
            cerr << "   \t- " << m_redbold << "Ctrl-w:" << m_current << " write file to disk" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-c:" << m_current << " exit the editor" << endl << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-x:" << m_redital << " Combined Commands" << m_current << endl;
            cerr << "   \t\t- " << m_redital << "D:" << m_current << " delete a bloc of lines" << endl;
            cerr << "   \t\t- " << m_redital << "n:" << m_current << " hide/display line numbers" << endl;
            cerr << "   \t\t- " << m_redital << "c:" << m_current << " copy a bloc of lines" << endl;
            cerr << "   \t\t- " << m_redital << "x:" << m_current << " cut a bloc of lines" << endl;
            cerr << "   \t\t- " << m_redital << "v:" << m_current << " paste a bloc of lines" << endl;
            cerr << "   \t\t- " << m_redital << "d:" << m_current << " debug the code" << endl;
            cerr << "   \t\t- " << m_redital << "r:" << m_current << " run the code" << endl;
            cerr << "   \t\t- " << m_redital << "w:" << m_current << " write and quit" << endl;
            cerr << "   \t\t- " << m_redital << "l:" << m_current << " load a file" << endl;
            cerr << "   \t\t- " << m_redital << "m:" << m_current << " display meta-characters" << endl;
            cerr << "   \t\t- " << m_redital << "h:" << m_current << " full help" << endl;
            cerr << "   \t\t- " << m_redital << "q:" << m_current << " quit" << endl << endl;
        }
        
        if (!i || i == 5) {
            cerr << "   - " << m_redbold << "5. System:" << m_current << endl;
            cerr << "   \t- " << m_redbold << "!unix:" << m_current << " what follows the " << m_redital << "'!'" << m_current << " will be executed as a Unix command (ex: "<< m_redital << "!ls" << m_current << ")" << endl;
            cerr << "   \t- " << m_redbold << "!vs=unix:" << m_current << " what follows the " << m_redital << "'='" << m_current << " will be executed as a Unix command ("
                 << m_redital << "!vs=ls" << m_current << ")" << endl;
            cerr << "   \t- " << m_redbold << "clear (space):" << m_current << " clear the current environment or a specifc file space" << endl;
            cerr << "   \t- " << m_redbold << "reinit:" << m_current << " clear the buffer content and initialize predeclared variables" << endl;
            cerr << "   \t- " << m_redbold << "Ctrl-d:" << m_current << " end the session and exit tamgu" << endl;
            cerr << "   \t- " << m_redbold << "exit:" << m_current << " end the session and exit tamgu" << endl << endl;
        }
        
        cerr << endl;
    }

    long taille(wstring& s) {
        long sz = s.size();
        long pref = prefixego() + 1;
        long pos = pref;
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }
        for (long i = 0; i < sz; i++) {
            if (lispe->handlingutf8->c_is_emojicomp(s[i]))
                continue;
            
            if (lispe->handlingutf8->c_is_emoji(s[i])) {
                pos += 2;
                continue;
            }
                
            if (ckjchar(s[i])) {
                pos += 2;
                continue;
            }
            if (s[i] == 9) //tab position
                pos += (8 - (pos%8))%8;
            pos++;
        }
        return (pos-pref);
    }
    
    long sizestring(wstring& s) {
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }
        long sz = s.size();
        long szstr = 0;
        for (long i = 0; i < sz; i++) {
            if (lispe->handlingutf8->c_is_emojicomp(s[i]))
                continue;
            szstr++;
        }
        return szstr;
    }

    void cleanlongemoji(wstring& s, wstring& cleaned, long p) {
        long i = 0;
        while (i < p) {
            cleaned += s[i++];
            while (lispe->handlingutf8->c_is_emojicomp(s[i])) {i++;}
        }
    }

    //The deletion of a character is different if it is an emoji...
    long deleteachar(wstring& l, bool last, long pins) {
        if (l == L"")
            return pins;

        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }

        if (last) {
            while (lispe->handlingutf8->c_is_emojicomp(l.back())) {
                l.pop_back();
                pins--;
            }
            l.pop_back();
        }
        else {
            long nb = 1;
            long i = pins;
            if (lispe->handlingutf8->c_is_emoji(l[i++])) {
                while (lispe->handlingutf8->c_is_emojicomp(l[i++])) nb++;
            }
            l.erase(pins, nb);
        }
        return pins;
    }
    
    void forwardemoji() {
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }

        if (lispe->handlingutf8->c_is_emoji(line[posinstring])) {
            posinstring++;
            while (lispe->handlingutf8->c_is_emojicomp(line[posinstring]))
                posinstring++;
        }
        else
            posinstring++;
    }

    void backwardemoji() {
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }

        posinstring--;
        long i = 0;
        if (posinstring < i)
            return;
        
        i = posinstring;
        if (lispe->handlingutf8->c_is_emojicomp(line[i])) {
            i--;
            while (i > 0 && lispe->handlingutf8->c_is_emojicomp(line[i]))
                i--;
            if (i >= 0 && lispe->handlingutf8->c_is_emoji(line[i]))
                posinstring = i;
        }
    }

    //------------------------------------------------------------------------------------------------
    //Cursor movements...
    //------------------------------------------------------------------------------------------------
    string prefixstring(long n) {
        if (breakpoints.size() && lispe != NULL) {
            long idfile = lispe->id_file(thecurrentfilename);
            if (idfile != -1) {
                try {
                    if (breakpoints.at(idfile).at(n))
                        return "^^";
                }
                catch(const std::out_of_range& oor) {}
            }
        }
        return prefix;
    }
    
    void printline(long n, string l) {
        if (!emode()) {
            if (prefixesize(n) > prefixsize)
                setprefixesize(n);
            cout << back << m_dore << prefix << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << l;
        }
        else {
            if (noprefix)
                cout << back << l;
            else
                cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << l;
        }
    }
    
    void printline(long n) {
        if (!emode()) {
            if (prefixesize(n) > prefixsize)
                setprefixesize(n);
            cout << back << m_dore << prefix << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current;
        }
        else {
            if (noprefix)
                cout << back;
            else
                cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current;
        }
        
    }

    void printline(long n, wstring& l, long i = -1) {
        if (emode()) {
            if (noprefix)
                cout << back << coloringline(l);
            else
                cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << coloringline(l);
        }
        else
            cout << back << m_dore << prefix << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << coloringline(l);
    }

    //this is the method to handle up and down key strokes
    bool updown(char drt, long& pos) {
        if (emode())
            return jag_editor::updown(drt, pos);
        
        if (drt == is_up) {
            if (poscommand > 0 && poscommand <= commandlines.size()) {
                clearline();
                line = commandlines[--poscommand];
                posinstring = linesize();
                printline(pos+1, line);
            }
            return false;
        }
        
        clearline();
        long i = commandlines.size();
        if (poscommand >= 0 && poscommand < i - 1) {
            line = commandlines[++poscommand];
            posinstring = linesize();
            printline(pos+1, line);
        }
        else {
            line = L"";
            poscommand = i;
            posinstring = 0;
            printline(pos+1);
        }
        return false;
    }

    void displaygo(bool full) {
        if (option != x_none) {
            jag_editor::displaygo(full);
            return;
        }

        if (full) {
            if (editmode) {
                if (lines.status[pos] == concat_line) {
                    if (noprefix)
                        cout << back << coloringline(line);
                    else {
                        string space(prefixe(), ' ');
                        cout << back << space << coloringline(line);
                    }
                }
                else {
                    if (!lines.check(pos) || noprefix)
                        printline(lines.numeros[pos], line, pos);
                    else {
                        string prf = prefix;
						prefix = ">>";
                        printline(lines.numeros[pos], line, pos);
                        prefix = prf;
                    }
                }
            }
            else
                printline(pos+1, line);
        }
    }

    //We detect long commented lines or long strings
    void scanforlonglines() {
        longstrings.clear();
        //We check for comments and long lines
        long r;
        for (long i = 0; i < lines.size(); i++) {
            if (lines[i].find(L"//") != -1) {
                longstrings.push_back(l_com_one);
                continue;
            }
            
            r = lines[i].find(L"/@");
            if (r != -1) {
                longstrings.push_back(l_com);
                if (lines[i].find(L"@/", r) != -1)
                    continue;
                i++;
                while (i < lines.size() && lines[i].find(L"@/") == -1) {
                    longstrings.push_back(l_com);
                    i++;
                }
                longstrings.push_back(l_com);
                continue;
            }
            
            r = lines[i].find(L"@\"");
            if (r != -1) {
                if (lines[i].find(L"\"@", r) != -1) {
                    longstrings.push_back(l_str);
                    continue;
                }
                
                longstrings.push_back((r+1)*-1);
                i++;
                while (i < lines.size() && lines[i].find(L"\"@") == -1) {
                    longstrings.push_back(l_str);
                    i++;
                }
                longstrings.push_back(l_str);
                continue;
            }

            longstrings.push_back(0);
        }
    }
    
    void displaylist(long beg, long end) {
        if (!lines.size()) {
            clearline();
            if (!noprefix)
                cout << back << m_dore << prefix << m_current << m_lightgray << std::setw(prefixsize) << "1> " << endl;
            return;
        }

        stringstream blk;
        
        if (beg < 0)
            beg = 0;
        
        if (modified) {
            scanforlonglines();
            modified = false;
        }

        if (emode()) {
            x_option g = option;
            option = x_none;
            
            poslines.clear();
            
            lines.updatesize();
            
            for (long i = beg; i < lines.size(); i++) {
                string space(prefixe(), ' ');
                if (lines.status[i] == concat_line)
                    blk << space << coloringline(lines[i]) << endl;
                else {
                    if (noprefix)
                        blk << coloringline(lines[i]) << endl;
                    else {
                        blk << m_dore << prefixstring(lines.numeros[i]) << m_current << m_lightgray << std::setw(prefixsize) << lines.numeros[i] << "> " << m_current << coloringline(lines[i]) << endl;
                    }
                }
                poslines.push_back(i);
                if (poslines.size() > row_size)
                    break;
            }
            option = g;
            clearscreen();
            cout << blk.str();
            return;
        }
        
        if (end >= lines.size())
            end = lines.size() - 1;
        
        long lastsz = 0;
        long thesz, asz;

        if (beg)
            beg = lines.getlinenumber(beg)+1;
        
        if (end)
            end = lines.getlinenumber(end)+1;

        for (long i = beg; i <= end; i++) {
            string space(prefixe(), ' ');
            if (lines.status[i] == concat_line)
                blk << space << coloringline(lines[i]) << endl;
            else {
                if (noprefix)
                    blk << coloringline(lines[i]) << endl;
                else {
                    blk << m_dore << prefixstring(lines.numeros[i]) << m_current << m_lightgray << std::setw(prefixsize) << lines.numeros[i] << "> " << m_current << coloringline(lines[i]);
                }
            }
            asz = blk.str().size();
            thesz = col_size - (asz - lastsz);
            if (thesz > 0) {
                string space(thesz,' ');
                blk << space;
            }
            blk << endl;
            lastsz = asz;
        }
        cout << back << blk.str();
    }
    

    //------------------------------------------------------------------------------------------------
    //Deletion methods...
    //------------------------------------------------------------------------------------------------
    long prefixego() {
        if (option != x_none)
            return jag_editor::prefixego();
        
        return prefixe();
    }

    void setpcursor() {
        getcursor();
        pcursor = xcursor;
    }
    
    //------------------------------------------------------------------------------------------------
    //formating method...
    //------------------------------------------------------------------------------------------------

    //------------------------------------------------------------------------------------------------
    //search method...
    //------------------------------------------------------------------------------------------------
    
    static bool isempty(wstring c) {
        s_trim(c);
        if (c == L"")
            return true;
        return false;
    }
    
    static bool isempty(string c) {
        s_trim(c);
        if (c == "")
            return true;
        return false;
    }
    
    //We create an empty space
    void addspace(string filename, bool fromwrite = false) {
        thecurrentfilename = s_trim(filename);
        if (thecurrentfilename == "")
            return;
        
        thecurrentfilename = Normalizefilename(thecurrentfilename);
        currentfileid = ifilenames.size();
        ifilenames.push_back(thecurrentfilename);
        codes.push_back(L"");
        editor_keep kp;
        editors_undos.push_back(kp);
        editors_redos.push_back(kp);
        lastlines.push_back(0);
        filenames[thecurrentfilename] = currentfileid;
        if (!fromwrite) {
            //We create a local undos/redos section..
            cerr << m_redbold << "File space creation: " << thecurrentfilename << m_current <<" (" << currentfileid << ")" << endl;
            wstring c = L"\n";
            LispSetCode(c);
            lines.setcode(c);
        }
    }
    
    bool loadfile(wstring& name) {
        return loadfile(convert(name));
    }

    //We keep different files in memory...
    bool loadfile(string filename) {
        wstring code;
        thecurrentfilename = s_trim(filename);
        if (thecurrentfilename == "")
            return false;
        
        thecurrentfilename = Normalizefilename(thecurrentfilename);
        if (filenames.find(thecurrentfilename) != filenames.end()) {
            if (currentfileid != filenames[thecurrentfilename]) {
                //We backup our current undo/redo buffer
                undos.storein(editors_undos[currentfileid]);
                redos.storein(editors_redos[currentfileid]);
                
                currentfileid = filenames[thecurrentfilename];
                
                //We then activate the redo/undo in hold...
                editors_undos[currentfileid].storein(undos);
                editors_redos[currentfileid].storein(redos);
                code = codes[currentfileid];
                LispSetCode(code);
            }
        }
        else {
            ifstream rd(thecurrentfilename, openMode);
            if (rd.fail()) {
                cerr << m_redbold << " Cannot load: " << thecurrentfilename << m_current << endl;
                return false;
            }
            string ln;
            string cde;
            while (!rd.eof()) {
                getline(rd, ln);
                ln = s_trimright(ln);
                cde += ln + "\n";
            }
            LispSetCode(cde);
            code = WListing();
            if (currentfileid != -1) {
                //We backup our current undo/redo buffer
                undos.storein(editors_undos[currentfileid]);
                redos.storein(editors_redos[currentfileid]);
                //This is a new loading, with no undo/redo in hold
                undos.clear();
                redos.clear();
            }
            currentfileid = ifilenames.size();
            ifilenames.push_back(thecurrentfilename);
            codes.push_back(code);
            
            //We create a new redo/undo section
            editor_keep kp;
            editors_undos.push_back(kp);
            editors_redos.push_back(kp);
            filenames[thecurrentfilename] = currentfileid;
        }
        line = L"";
        posinstring = 0;
        lines.setcode(code);
        return true;

    }
    
    bool writetofile() {
        wstring code = lines.code();
        
        ofstream wd(thecurrentfilename, std::ios::binary);
        if (wd.fail())
            return false;
        wd << convert(code);
        wd.close();
        if (currentfileid == -1) {
            addspace(thecurrentfilename, true);
            codes[0] = code;
        }
        tobesaved = false;
        return true;
    }

    
    bool evallocalcode(string code, bool disp=false) {
        s_trim(code);
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }
        
        Element* e = lispe->eval(code);
        cout << m_redbold;
        cout << e->toString(lispe) << endl;
        cout << m_current;
        if (e->isError()) {
            e->release();
            return false;
        }
        e->release();
        return true;
    }
    
    long handlingcommands(long pos, bool& dsp, char initvar) {
        typedef enum {cmd_none, cmd_args, cmd_filename, cmd_spaces, cmd_select, cmd_edit, cmd_run, cmd_debug, cmd_cls, cmd_echo, cmd_help, cmd_list,
            cmd_rm, cmd_history, cmd_open, cmd_create, cmd_save, cmd_exit, cmd_load_history, cmd_store_history, cmd_clear, cmd_reinit} thecommands;

        static bool init = false;
        static hmap<wstring, thecommands> commands;
        
        if (!init) {
            init = true;
            commands[L"args"] = cmd_args;
            commands[L"filename"] = cmd_filename;
            commands[L"spaces"] = cmd_spaces;
            commands[L"edit"] = cmd_edit;
            commands[L"select"] = cmd_select;
            commands[L"run"] = cmd_run;
            commands[L"debug"] = cmd_debug;
            commands[L"cls"] = cmd_cls;
            commands[L"echo"] = cmd_echo;
            commands[L"help"] = cmd_help;
            commands[L"list"] = cmd_list;
            commands[L"rm"] = cmd_rm;
            commands[L"history"] = cmd_history;
            commands[L"load"] = cmd_load_history;
            commands[L"store"] = cmd_store_history;
            commands[L"open"] = cmd_open;
            commands[L"create"] = cmd_create;
            commands[L"save"] = cmd_save;
            commands[L"exit"] = cmd_exit;
            commands[L"clear"] = cmd_clear;
            commands[L"reinit"] = cmd_reinit;
        }
        
        cout << endl;
        wstring code;
        
        long i;
        
        vector<wstring> v;
        vsplit(line, L" ", v);
        if (v.size() == 0)
            return pos;
        
        thecommands command = cmd_none;
        if (commands.find(v[0]) != commands.end()) {
            command = commands[v[0]];
        }
        else {
            if (line[0] == '!' || line[0] == '?') {
                if (line[0] == '!') {
                    addcommandline(line);

                    //We launch a Unix command...
                    code = line.substr(1, line.size() -1);
                    long iquote = line.find(L"\"");
                    long iequal = line.find(L"=");
                    if (iequal != -1 && (iquote == -1 || iequal < iquote)) {
                        code = line.substr(iequal+1, line.size()-iequal);
                        line = line.substr(1, iequal-1);
                        line = L"(setq " +line + L" ";
                        line += L"(command \"";
                        line += code;
                        line += L"\"))";
                    }
                    else {
                        line = L"(command \"";
                        line += code;
                        line += L"\")";
                    }
                    
                    Executesomecode(line);
                    
                    code = WListing();
                    lines.setcode(code);
                    lines.pop_back();
                    code = lines.code();
                    LispSetCode(code);
                    return lines.size();
                }
                
                if (line[0] == '?') {
                    line = s_trim(line);
                    line.erase(0, 1);
                    i = line.rfind(L".");
                    if (i != -1) {
                        code = line.substr(i+1 , line.size() - 2);
                        line.erase(i, string::npos);
                        line += L".info('";
                        line += code;
                        line += L"')";
                        Executesomecode(line);
                    }
                    else {
                        code = L"printjln(_info('" + line + L"'), \"\\n\", ' for type: ');";
                        Executesomecode(code);
                    }
                    return pos;
                }
            }
        }
        
        switch(command) {
            case cmd_none:
                break;
            case cmd_args: //args
            {
                arguments.clear();
                string arg;
                for (i = 1; i < v.size(); i++) {
                    s_unicode_to_utf8(arg, v[i]);
                    arguments.push_back(arg);
                }
                
                if (arguments.size()) {
                    if (lispe == NULL)
                        lispe = new LispE;
                    lispe->arguments(arguments);
                    lispe->set_pathname(thecurrentfilename);
                }
            }
                return pos;
            case cmd_filename:
                cout << back << m_redbold << "Fichier: " << m_red << thecurrentfilename << m_current << endl;
                return pos;
            case cmd_spaces:
                if (v.size() == 1) {
                    cout << back << m_redbold << "Espace:" << endl << endl;
                    for (i = 0; i < ifilenames.size(); i++) {
                        if (i == currentfileid)
                            cout << back << m_redbold << "Fichier " << i <<": " << m_red << ifilenames[i] << m_current << " <<< " << endl;
                        else
                            cout << back << m_redbold << "Fichier " << i <<": " << m_red << ifilenames[i] << m_current << endl;
                    }
                    cout << endl;
                    return pos;
                } //if a value is provided, then it works as a select...
            case cmd_select:
                addcommandline(line);
                if (v.size() == 1) {
                    cout << back << m_redbold << "Identifiant espace manquant" << endl;
                    return pos;
                }
                i = convertinginteger(v[1]);
                if (i < 0 || i >= ifilenames.size()) {
                    cout << back << m_redbold << "espace: " << i << " n'existe pas" << endl;
                    return pos;
                }
                if (i != currentfileid) {
                    //We backup our current undo/redo section
                    undos.storein(editors_undos[currentfileid]);
                    redos.storein(editors_redos[currentfileid]);
                    
                    currentfileid = i;
                    code = codes[i];
                    thecurrentfilename = ifilenames[i];
                    
                    //we now reactivate the current undo/redo section
                    editors_undos[i].storein(undos);
                    editors_redos[i].storein(redos);
                    
                    lines.setcode(code);
                    LispSetCode(code);
                    posinstring = 0;
                    line = L"";
                    cout << "Espace fichier: " << thecurrentfilename << " (" << i << ")" << endl;
                    pos = 0;
                    modified = true;
                }
                return pos;
           case cmd_edit:
#ifndef WIN32
                signal(SIGWINCH, resizewindow);
#endif
                if (v.size() == 2) {
                    i = convertinginteger(v[1]);
                    if (i < 0 || i >= ifilenames.size()) {
                        cout << back << m_redbold << "espace: " << i <<" n'existe pas" << endl;
                        return pos;
                    }
                    if (i != currentfileid) {
                        //We backup our current undo/redo section
                        undos.storein(editors_undos[currentfileid]);
                        redos.storein(editors_redos[currentfileid]);
                        lastlines[currentfileid] = lastline;
                        
                        currentfileid = i;
                        code = codes[i];
                        thecurrentfilename = ifilenames[i];
                        lastline = lastlines[i];
                        
                        //we now reactivate the current undo/redo section
                        editors_undos[i].storein(undos);
                        editors_redos[i].storein(redos);
                        
                        lines.setcode(code);
                        modified = true;
                    }
                }
                
                addcommandline(line);
                prefix = ">>";

                if (lines.size() == 0) {
                    lines.push(L"");
                    poslines.clear();
                    poslines.push_back(0);
                }

                editmode = true;
                currentline = 0;
                dsp = false;
                
                pos = lastline;
                option = x_none;
                posinstring = 0;
                line = lines[lastline];
                displaylist(lastline, row_size);
                movetoline(currentline);
                movetobeginning();
                return pos;
            case cmd_run:
                addcommandline(line);
                if (v.size() == 1) {
                    if (isempty(current_code))
                        return pos;
                    
                    if (lispe != NULL)
                        delete lispe;
                    
                    lispe = new LispE;
                    lispe->arguments(arguments);
                    lispe->set_pathname(thecurrentfilename);
                    line = L"";
                    editmode = false;
                    
                    debugmode = false;
                    runcode();
                    return pos;
                }
                
                if (loadfile(v[1])) {
                    if (lispe != NULL)
                        delete lispe;
                    lispe = new LispE;
                    lispe->arguments(arguments);
                    cout << m_red;
                    lispe->load(thecurrentfilename);
                    cout << m_current;
                }
                else
                    cerr << m_redbold << "Cannot load: " << thecurrentfilename << m_current << endl;
                return pos;
            case cmd_debug:
                current_line_debugger = -1;
                addcommandline(line);
                if (v.size() == 1) {
                    if (isempty(current_code))
                        return pos;
                    
                    if (lispe != NULL)
                        delete lispe;
                    
                    lispe = new LispE;
                    lispe->arguments(arguments);
                    lispe->set_pathname(thecurrentfilename);
                    //We initialize the breakpoints and the trace mode
                    lispe->trace = true;
                    if (breakpoints.size())
                        lispe->delegation->breakpoints = breakpoints;
                    else
                        lispe->stop_at_next_line(true);
                    line = L"";
                    editmode = false;
                    debugmode = true;
                    option = x_debug;
                    tid = new std::thread(debuggerthread, this);
                    return pos;
                }
                
                if (loadfile(v[1])) {
                    if (lispe != NULL)
                        delete lispe;
                    lispe = new LispE;
                    lispe->arguments(arguments);
                    cout << m_red;
                    lispe->load(thecurrentfilename);
                    cout << m_current;
                    debugmode = true;
                }
                else
                    cerr << m_redbold << "Cannot load: " << thecurrentfilename << m_current << endl;
                return pos;
            case cmd_cls:
                clearscreen();
                return pos;
            case cmd_echo:
                echochar = true;
                return pos;
            case cmd_help:
                i = 0;
                if (v.size() == 2) {
                    i = v[1][0] - 48;
                    if (i < 1 || i > 6)
                        i = 0;
                }
                
                displaythehelp(i);
                return pos;
            case cmd_list:
            case cmd_rm:
                addcommandline(line);
            {
                code = WListing();
                if (isempty(code))
                    return pos;
                
                if (v.size() >= 2 && v[1].size() > 1) {
                    wstring c = v[1];
                    long ps = c.find(L":");
                    if (ps != -1) {
                        long sz = c.size();
                        sz--;
                        v.pop_back();
                        if (ps == 0) { //:10
                            v.push_back(L":");
                            c.erase(0,1);
                            v.push_back(c);
                        }
                        else {
                            if (ps == sz) {//the end...
                                c.erase(sz, 1);
                                v.push_back(c);
                                v.push_back(L":");
                            }
                            else {//10:20
                                v.push_back(c.substr(0, ps));
                                v.push_back(L":");
                                v.push_back(c.substr(ps+1, c.size()));
                            }
                        }
                    }
                }
                
                string codeindente;
                i = 3;
                string cd = convert(code);
                codeindente = "";
                IndentCode(cd, codeindente, i);
                code = wconvert(codeindente);
                lines.setcode(code);
                
                if (lines.size() == 0)
                    return pos;
                
                long lastline = lines.numeros.back();
                long beg = 0, end = lastline;
                if (v.size() >= 2) {
                    if (v[1] == L":") { //list :20
                                        //we display up to the next element (if it part of it...
                        if (v.size() >= 3) {
                            if (v[2] == L"$")
                                end = lastline;
                            else {
                                end = convertinginteger(v[2]) - 1;
                                if (end > lastline)
                                    end = lastline;
                            }
                            pos = lines.getlinenumber(end);
                        }
                    }
                    else {
                        if (v[1] == L"$")
                            beg = lines.size();
                        else {
                            beg = convertinginteger(v[1]) - 1;
                            if (beg < 0)
                                beg = 0;
                            if (beg > lastline)
                                beg = lastline;
                        }
                        pos = lines.getlinenumber(beg);
                        if (v.size() >= 3) {
                            if (v[2] == L":") { //list 23:
                                if (v.size() >= 4)
                                    end = convertinginteger(v[3]) - 1; //list 23:40
                            }
                            else
                                end = convertinginteger(v[2]) - 1; //list 10 23
                            
                            if (end > lastline)
                                end = lastline;
                            pos = lines.getlinenumber(end);
                        }
                        else
                            end = beg; // we display one line only
                    }
                }
                else
                    pos = lines.getlinenumber(lastline);
                

                if (command == cmd_list) { //list
                    cout << endl;
                    displaylist(beg, end);
                    cout << endl;
                }
                else {
                    beg = lines.getlinenumber(beg)+1;
                    end = lines.getlinenumber(end)+2;
                    lines.erase(beg, end);
                    code = lines.code();
                    LispSetCode(code);
                }
            }
                line = L"";
                posinstring = 0;
                return pos;
            case cmd_history:
                cerr << endl;
                if (historyfilename != "")
                    cerr << m_redbold << "History:" << historyfilename << m_current << endl;
                
                for (i = 0; i < commandlines.size(); i++)
                    cerr << i+1 << " = " << convert(commandlines[i]) << endl;
                cerr << endl;
                addcommandline(v[0]);
                return pos;
            case cmd_load_history: {
                if (v.size() != 2) {
                    if (historyfilename == "") {
                        cerr << m_redbold << "Missing filename.." << m_current << endl;
                        return pos;
                    }
                }
                else
                    historyfilename = Normalizefilename(convert(v[1]));

                    
                ifstream ld(historyfilename, openMode);
                if (ld.fail()) {
                    cerr << m_redbold << "Cannot load:" << historyfilename << m_current << endl;
                    return pos;
                }
                string s;
                while (!ld.eof()) {
                    getline(ld, s);
                    s=s_trim(s);
                    if (s!="") {
                        code = wconvert(s);
                        commandlines.push_back(code);
                    }
                }
                return pos;
            }
            case cmd_store_history: {
                if (v.size() != 2) {
                    if (historyfilename == "") {
                        cerr << m_redbold << "Missing filename.." << m_current << endl;
                        return pos;
                    }
                }
                else
                    historyfilename = Normalizefilename(convert(v[1]));
                ofstream st(historyfilename, std::ios::binary);
                for (i = 0; i < commandlines.size(); i++)
                    st << convert(commandlines[i]) << endl;
                return pos;
            }
            case cmd_open:
                addcommandline(line);

                if (v.size() == 1) {
                    if (thecurrentfilename == "") {
                        cerr << m_redbold << "Missing filename.." << m_current << endl;
                        return pos;
                    }
                }
                if (loadfile(v[1]))
                    cerr << m_red << "ok." << m_current << endl;
                if (lispe == NULL) {
                    lispe = new LispE;
                    lispe->arguments(arguments);
                }
                lispe->set_pathname(thecurrentfilename);
                return pos;
            case cmd_create:
                addcommandline(line);
                if (v.size() == 1) {
                    cerr << m_redbold << "Missing filename.." << m_current << endl;
                    return pos;
                }
                addspace(convert(v[1]));
                line = L"";
                return pos;
            case cmd_save:
                addcommandline(line);
                if (v.size() == 1) {
                    if (thecurrentfilename == "") {
                        cerr << m_redbold << "Missing filename.." << m_current << endl;
                        return pos;
                    }
                }
                else {
                    thecurrentfilename = convert(v[1]);
                    thecurrentfilename = s_trim(thecurrentfilename);
                    thecurrentfilename = Normalizefilename(thecurrentfilename);
                }
            {
                string codeindente;
                string cd = current_code;
                i = 3;
                IndentCode(cd, codeindente, i);
                ofstream wd(thecurrentfilename, std::ios::binary);
                wd << codeindente;
                wd.close();
                
                //if this is a first saving of this code...
                if (filenames.find(thecurrentfilename) == filenames.end()) {
                    if (lispe == NULL) {
                        lispe = new LispE;
                        lispe->arguments(arguments);
                    }
                    lispe->set_pathname(thecurrentfilename);
                    currentfileid = ifilenames.size();
                    ifilenames.push_back(thecurrentfilename);
                    filenames[thecurrentfilename] = currentfileid;
                    code = wconvert(cd);
                    codes.push_back(code);
                    
                    //We also backup our undo/redo section
                    editors_undos.push_back(undos);
                    editors_redos.push_back(redos);
                }
            }
                return pos;
            case cmd_exit:
                return !terminate();
            case cmd_clear:
                addcommandline(line);
                if (v.size() == 2) {
                    i = convertinginteger(v[1]);
                    if (i < 0 || i >= ifilenames.size()) {
                        cout << back << m_redbold << "Cet espace de fichier n'existe pas" << endl;
                        return pos;
                    }
                    //In this case, we clear one space...
                    string current =  ifilenames[i];
                    filenames.erase(current);
                    ifilenames.erase(ifilenames.begin()+i);
                    codes.erase(codes.begin()+i);
                    editors_undos.erase(editors_undos.begin()+i);
                    editors_redos.erase(editors_redos.begin()+i);

                    //We need to resynchronize the id for each file...
                    hmap<string, short>:: iterator it;
                    for (it = filenames.begin(); it != filenames.end(); it++) {
                        if (it->second > i)
                            --it->second;
                    }
                    if (ifilenames.size() != 0) {
                        //if i points to currentfileid, we clear it and replaces it, with position 0
                        if (i == currentfileid) {
                            currentfileid = 0; //we select the first one, by default...
                            code = codes[currentfileid];
                            thecurrentfilename = ifilenames[currentfileid];
                            undos.storein(editors_undos[currentfileid]);
                            redos.storein(editors_redos[currentfileid]);
                            lines.setcode(code);
                            LispSetCode(code);
                            posinstring = 0;
                            line = L"";
                            cout << "Espace fichier: " << currentfileid << endl;
                            pos = 0;
                            modified = true;
                        }
                    }
                }
                else {
                    filenames.clear();
                    ifilenames.clear();
                    codes.clear();
                    editors_undos.clear();
                    editors_redos.clear();
                }

                if (!ifilenames.size()) {
                    thecurrentfilename = "";
                    lines.clear();
                }
                
                line = L"";
                posinstring = 0;
                if (lispe != NULL)
                    delete lispe;
                lispe = new LispE;
                lispe->arguments(arguments);
                lispe->set_pathname(thecurrentfilename);
                pos = 0;
                return pos;
            case cmd_reinit:
                addcommandline(line);

                thecurrentfilename = "";
                lines.clear();
                line = L"";
                posinstring = 0;
                if (lispe != NULL)
                    delete lispe;
                lispe = new LispE;
                lispe->arguments(arguments);
                lispe->set_pathname(thecurrentfilename);
                breakpoints.clear();
                pos = 0;
                return pos;
        }
        
        
        //Adding a line into the code
        if (line.size()) {
            Executesomecode(line);
            code = WListing();
            
            lines.setcode(code);
            pos = lines.size();
            return pos;
        }
        
        return pos;
    }
    
    void init() {
        lines.clear();
        lines.push(L"");
        poslines.clear();
        poslines.push_back(0);
        displaylist(0,1);
        pos = 0;
        posinstring = 0;
        line = L"";
        kbuffer = L"";
        currentline = 0;
        clearscreen();
        printline(1);
    }

    bool terminate() {
        replaceall = false;
        if (tobesaved) {
            tobesaved = false;
            if (emode())
                displayonlast("Fichier non sauvé... même commande pour quitter true_ment", true);
            else
                printline(pos+1, "Fichier non sauvé... ctrl-d encore pour quitter");
            return false;
        }
        
        movetolastline();
        clearline();
        cout << back << m_redbold << "A plus!!!" << m_current << endl;
        
        fflush(stdout);
        resetterminal();
        exit(0);
        return true;
    }
    
    void clear() {
        echochar = false;
        pos = lines.size();
        if (!editmode) {
            cout << "^C" << endl;
            printline(pos+1);
        }
        else {
            lastline = poslines[0];
            editmode = false;
            currentline = 0;
            
            wstring code = lines.code();
            if (currentfileid != -1)
                codes[currentfileid] = code;
            LispSetCode(code);
            movetolastline();
            clearline();
            string l = m_red;
            l += "exit editor";
            l += m_current;
			prefix = "<>";
            printline(pos+1, l);
            cout << endl;
            clearline();
            printline(pos+1);
        }

        if (lispe != NULL) {
            if (debugmode && lispe->checking_trace_lock())
                lispe->releasing_trace_lock();
            
            lispe->stop();
            debugmode = false;
        }
        
        fflush(stdout);
        line = L"";
        posinstring = 0;
        currentline = 0;
    }

    bool checkcommand(char c) {
        switch (c) {
            case 'r':
                if (emode()) {
                    lastline = poslines[0];
                    if (lines.size()) {
                        bool dsp = true;
                        handle_ctrl_c(0);
                        line = L"run";
                        posinstring = linesize();
                        pos = 0;
                        editmode = true;
                        clearscreen();
                        
                        handlingcommands(pos, dsp, false);
                        editmode = false;
                        posinstring = 0;
                        pos = lines.size()-1;
                        line = L"";
                        printline(pos+1);
                    }
                }
                return true;
            case 'd':
                if (emode()) {
                    lastline = poslines[0];
                    if (lines.size()) {
                        bool dsp = true;
                        handle_ctrl_c(0);
                        line = L"debug";
                        posinstring = linesize();
                        pos = 0;
                        editmode = true;
                        clearscreen();
                        handlingcommands(pos, dsp, false);
                        editmode = false;
                        posinstring = 0;
                        pos = lines.size()-1;
                        line = L"";
                        printline(pos+1);
                    }
                }
                return true;
            default:
                return jag_editor::checkcommand(c);
        }
        return false;
    }
    
    void ls(string cmd, string path, vector<wstring>& paths) {
        FILE *fp;

        char chemin[PATH_MAX];
        
        cmd += path;
        
#ifdef WIN32
        fp = _popen(STR(cmd), "r");
#else
        fp = popen(STR(cmd), "r");
#endif
        if (fp == NULL)
            return;
        
        wstring l;
        while (fgets(chemin, PATH_MAX, fp) != NULL) {
            cmd = chemin;
            cmd = s_trim(cmd);
            l = wconvert(cmd);
            paths.push_back(l);
        }
        
#ifdef WIN32
        _pclose(fp);
#else
        pclose(fp);
#endif
    }
    
#ifdef WIN32
    bool checkpath() {
        //The first part should be a command such as open or load...
        long pos = line.rfind(' ');
        if (pos == -1)
            return false;
        
        wstring root = line.substr(0, pos);
        wstring name;
        wstring path = line.substr(pos, line.size());
        path = s_trim(path);
        //Two cases, we have a "\\" in it...
        pos = path.rfind(L"\\");
        //We need to extract it
        if (pos != -1) {
            name = path.substr(pos+1, path.size()-pos);
            path = path.substr(0, pos+1);
        }
        else {
            name = path;
            path = L".";
        }
        vector<wstring> paths;
        vector<wstring> targets;
        //First the directories
        string cmd = "dir /B ";
        ls(cmd, convert(path), paths);
        //Now we look for continuation
        long i;
        for (i = 0; i < paths.size(); i++) {
            if (paths[i].substr(0, name.size()) == name)
                targets.push_back(paths[i]);
        }
        if (path == L".")
            path = L"";
        
        if (targets.size() == 0)
            return false;
        
        paths.clear();
        //Only directories, we want to add a _sep at the end...
        cmd = "dir /AD /B ";
        ls(cmd, convert(path), paths);
        for (i = 0; i < paths.size(); i++) {
            for (long j = 0; j < targets.size(); j++) {
                if (targets[j] == paths[i])
                    targets[j] += L"\\";
            }
        }

        if (targets.size() == 1) {
            line = root;
            line += L" ";
            line += path;
            line += targets[0];
            clearline();
            displaygo(true);
            posinstring = line.size();
            movetoposition();
            return true;
        }
        
        wstring common;
        long ln  = name.size();
        bool end = false;
        while (!end) {
            //We add one letter from the targets and see if it is common to all targets
            for (i = 0; i < targets.size(); i++) {
                if (ln >= targets[i].size()) {
                    end = true;
                    break;
                }
            }
            if (!end) {
                ++ln;
                common = targets[0].substr(0, ln);
                for (i = 1; i < targets.size(); i++) {
                    if (targets[i].substr(0, ln) != common) {
                        end = true;
                        break;
                    }
                }
                if (!end)
                    name = common;
            }
        }
        
        
        cerr << endl << endl << m_red;
        for (i = 0; i < targets.size(); i++)
            cerr << convert(targets[i]) << " ";
        cerr << m_current << endl << endl;
        
        line = root;
        line += L" ";
        line += path;
        line += name;
        clearline();
        displaygo(true);
        posinstring = line.size();
        movetoposition();
        return true;
    }
#else
    bool checkpath() {
        //The first part should be a command such as open or load...
        long pos = line.rfind(' ');
        if (pos == -1)
            return false;
        
        wstring root = line.substr(0, pos);
        wstring name;
        wstring path = line.substr(pos, line.size());
        path = s_trim(path);
        //Two cases, we have a "/" in it...
        pos = path.rfind(L"/");
        if (pos != -1) {
            name = path.substr(pos+1, path.size()-pos);
            path = path.substr(0, pos+1);
        }
        else {
            name = path;
            path = L".";
        }
        vector<wstring> paths;
        vector<wstring> targets;
        string cmd = "ls -1 -p ";
        ls(cmd, convert(path), paths);
        //Now we look for continuation
        long i;
        for (i = 0; i < paths.size(); i++) {
            if (paths[i].substr(0, name.size()) == name)
                targets.push_back(paths[i]);
        }
        if (path == L".")
            path = L"";
        
        if (targets.size() == 0)
            return false;
        
        if (targets.size() == 1) {
            line = root;
            line += L" ";
            line += path;
            line += targets[0];
            clearline();
            displaygo(true);
            posinstring = line.size();
            movetoposition();
            return true;
        }
        
        wstring common;
        long ln  = name.size();
        bool end = false;
        while (!end) {
            //We add one letter from the targets and see if it is common to all targets
            for (i = 0; i < targets.size(); i++) {
                if (ln >= targets[i].size()) {
                    end = true;
                    break;
                }
            }
            if (!end) {
                ++ln;
                common = targets[0].substr(0, ln);
                for (i = 1; i < targets.size(); i++) {
                    if (targets[i].substr(0, ln) != common) {
                        end = true;
                        break;
                    }
                }
                if (!end)
                    name = common;
            }
        }
        
        
        cerr << endl << endl << m_redital;
        for (i = 0; i < targets.size(); i++)
            cerr << convert(targets[i]) << " ";
        cerr << m_current << endl << endl;
        
        line = root;
        line += L" ";
        line += path;
        line += name;
        clearline();
        displaygo(true);
        posinstring = line.size();
        movetoposition();
        return true;
    }
#endif
    
    bool checkkeyboard(string& buff, long& first, long& last, bool& dsp, char noinit) {
        switch ((uchar)buff[0]) {
            case 2:  //ctrl-b, adding breakpoints
                if (emode() && lispe != NULL) {
                    long idfile = lispe->id_file(thecurrentfilename);
                    if (idfile == -1)
                        return true;
                    long idline = lines.numeros[pos];
                    
                    try {
                        breakpoints.at(idfile).at(idline) = 1 - breakpoints.at(idfile).at(idline);
                    }
                    catch(const std::out_of_range& oor) {
                        breakpoints[idfile][idline] = true;
                    }

                    cout << back << m_dore << prefixstring(idline) << m_current;
                    movetoposition();
                }
                return true;
#ifdef WIN32
            case 3: //ctrl-c, only here on Windows
                clear();
                return true;
#endif
            case 4: //ctrl-d exiting
                if (emode()) { //we delete a line
                    deleteline(0);
                    return true;
                }
                return !terminate();
            case 9:
                if (emode())
                    return false;
                //We try to interpret the string as a path
                return checkpath();
#ifdef WIN32
            case 13: //this is a carriage return
#else
            case 10:
#endif
                if (option != x_none) {
                    checkaction(buff, first, last, true);
                    return true;
                }
                if (editmode) {
                    pos = handlingeditorline(true);
                    return true;
                }
                pos = handlingcommands(pos, dsp, noinit);
                if (dsp) {
                    line = L"";
                    printline(pos+1);
                    posinstring = line.size();
                }
                else
                    movetoposition();
                return true;
            case 11: //ctrl-k: delete trailing characters
                if (emode()) {
                    deleteallafter();
                    return true;
                }
                
                clearline();
                kbuffer = line.substr(posinstring, line.size());
                line = line.substr(0, posinstring);
                if (pos < lines.size())
                    lines[pos] = line;
                if (option != x_none)
                    displaygo(true);
                else
                    printline(pos+1, line);
                return true;
            case 17:
                if (emode()) {
                    clear();
                    return true;
                }
                return checkaction(buff, first, last, true);
#ifdef WIN32
            case 224:
#else
            case 27: //Escape...
#endif
                //we clear the current line if it is the only character...
                if (buff.size() == 1) {
                    if (option != x_none || tooglehelp) {
                        displayonlast("", true);
                        tooglehelp = false;
                        option = x_none;
                        return true;
                    }
                    
                    if (editmode)
                        return true;
                    
                    clearline();
                    printline(pos+1);
                    line = L"";
                    posinstring = 0;
                    return true;
                }
                
                evaluateescape(buff);
                return true;
            default:
                return checkaction(buff, first, last, true);
        }
        return false;
    }
    
    void launchterminal(char noinit) {
        clearscreen();

        localhelp << m_red << "^c/q" << m_current << ":cmd line " << m_red << "^xq" << m_current << ":quit";
        
        option = x_none;
        prefix = ">";
#ifdef WIN32
        cerr << endl << m_redbold << "Lisp Elémentaire (" << LispVersion() << ")" << m_current << endl;
		SetConsoleCtrlHandler(handle_ctrl_c, TRUE);
#else
		cerr << endl << m_redbold << "Lisp Elémentaire (" << LispVersion() << "/" << GCC_VERSION << ")" << m_current << endl;
		signal(SIGINT,handle_ctrl_c);
#endif
        
        bool dsp = true;
        
        if (ifilenames.size() > 1) {
            currentfileid = 0;
            thecurrentfilename = ifilenames[0];
            lines.setcode(codes[0]);
            LispSetCode(codes[0]);
        }

        switch (noinit) {
            case 1:
				prefix = "<>";
                pos = 1;
                line = L"";
                lines.push_back(line);
                poslines.push_back(0);
                line = L"";
                cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
                printline(pos+1);
                break;
            case 2:
				prefix = "<>";
				cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
                lispe = new LispE;
                if (arguments.size())
                    lispe->arguments(arguments);
                
                lines.push(L"");
                poslines.push_back(0);
                line = L"";
                printline(1);
                break;
            case 3:
                //switch to edit mode
                pos = 0;
                line = L"edit";
                pos = handlingcommands(pos, dsp, noinit);
                break;
            default:
				prefix = "<>";
				cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
                printline(pos+1);
        }
        

        clearst();
        wstring code;
        wstring b;
        string buffer;
        string buff;

        long first = 0, last;

        bool inbuffer = false;
        bool instring = false;
        
        while (1) {
            buff = getch();
            
            if (debugmode) {
                if (buff == "$") {
                    cout << endl << endl;
                    cout.flush();
                    debugmode = false;
                    lispe->releasing_trace_lock();
                    lispe->stop_trace();
                    line = L"";
                    continue;
                }
                
                if (buff == "!") {
                    debugmode = false;
                    lispe->stop();
                    lispe->releasing_trace_lock();
                    lispe->trace = false;
                    line = L"";
                    continue;
                }
                
                if (buff == ">") {
                    lispe->releasing_trace_lock();
                    cout << endl << endl;
                    continue;
                }
                
                if (buff == "%") {
                    wstring wvars = lispe->stackAsString();
                    string vars;
                    s_unicode_to_utf8(vars, wvars);
                    cout << endl << endl << colors[2] << vars << m_red << endl;
                    displaygo(true);
                    continue;
                }
                
                if (buff == down) {
                    lispe->stop_at_next_line(2);
                    lispe->releasing_trace_lock();
                    cout << endl << endl;
                    cout.flush();
                    continue;
                }
                
                if (buff == up) {
                    lispe->stop_at_next_line(0);
                    lispe->releasing_trace_lock();
                    cout << endl << endl;
                    cout.flush();
                    continue;
                }

                if (buff[0] == 10 || buff[0] == 13) {
                    if (line.size()) {
                        lispe->trace = false;
                        string var = "%";
                        if (line.find(L"(") == -1) {
                            var = "";
                            s_unicode_to_utf8(var, line);
                            line = L"(string " + line + L")";
                        }
                        Element* e = lispe->eval(line);
                        cout << endl << endl << colors[2] << var << ": " <<  e->toString(lispe) << m_red << endl << endl;
                        e->release();
                        lispe->trace = true;
                        line = L"";
                        displaygo(true);
                        continue;
                    }
                    lispe->stop_at_next_line(1);
                    lispe->releasing_trace_lock();
                    cout << endl << endl;
                    cout.flush();
                    continue;
                }
            }
            
            if (linematch == -2) {
                displaygo(true);
                movetoposition();
            }
            else {
                if (linematch != -1) {
                    printline(linematch+1, lines[linematch]);
                    movetoposition();
                }
            }

            linematch = -1;
            dsp = true;
            if (checkkeyboard(buff, first, last, dsp, noinit))
                continue;
            
#ifdef WIN32
            if (!buff[0] && buff[1] == '#') {
                //Special case for ctrl+alt+h (ctrl-h is backdelete on windows
                if (emode()) {
                    option = x_none;
                    if (!tooglehelp)
                        displayonlast(localhelp.str(), true);
                    else {
                        jag_editor::displaylist(poslines[0]);
                        movetoline(currentline);
                        movetoposition();
                    }
                    tooglehelp = 1 - tooglehelp;
                }
                continue;
            }
#endif
            
            if (inbuffer) {
                buffer += buff;
                buff = buffer;
                inbuffer = false;
            }
            
            if (buff.size() == getbuffsize())
                inbuffer = check_utf8(buff, buffer);
            
            code = wconvert(buff);
            cleanheaders(code);
                        
            //We keep track of the initial form of the line...
            if (emode())
                undo(lines[pos],pos, u_modif); //The value is negative to indicate a deletion
            
            //Only one character to add, no need for further inspection, no CR in the string as well
            if (code.size() == 1 || buff.find(10) == -1)
                addabuffer(code, instring);
            else {
                for (long j = 0; j < code.size(); j++) {
                    b = code[j];
                    if (b[0] == 10) {
                        pos = handlingeditorline(false);
                        continue;
                    }
                    addabuffer(b, instring);
                }
            }
        }
    }
    
    void setcode(string& code) {
        LispSetCode(code);
        wstring cd = wconvert(code);
        lines.setcode(cd);
        pos = lines.size();
    }
    
    bool Executesomecode(wstring& c) {
        debugmode = false;
        
        string code = convert(c);
        
        if (lispe == NULL) {
            lispe = new LispE;
            lispe->arguments(arguments);
            lispe->set_pathname(thecurrentfilename);
        }
        
        //Seulement un nom de variable
        if (code.find("(") == -1 && code.find(")") == -1) {
            code = "(print "+ code + ")";
        }
        
        if (!editmode)
            addcommandline(c);
        
        line = L"";
        posinstring = 0;
        
        cout << m_red;
        Element* e = lispe->execute(code);
        std::cout << e->toString(lispe) << std::endl;
        e->release();
        cout << m_current;
        
        
        return true;
    }

    bool runcode() {
        cout << m_red;
        Element* e = lispe->execute(current_code, thecurrentfilename);
        std::cout << e->toString(lispe) << std::endl;
        e->release();
        cout << m_current;
        return true;
    }
    

    void addcommandline(wstring& w) {
        commandlines.push_back(w);
        poscommand = commandlines.size();
    }
    
    void resetscreen() {
        screensizes();
        if (editmode && option == x_none) {
            modified = true;
            wstring code = lines.code();
            lines.setcode(code);
            displaylist(poslines[0], poslines.back());
            if ((currentline+1) >= poslines.size())
                currentline = poslines.size() - 1;
            movetoline(currentline);
            posinstring = 0;
            movetobeginning();
        }
    }

};

//-------------------------------------------------------------------------------------------
// Debug Functions
//-------------------------------------------------------------------------------------------
void debug_function_lispe(LispE* lisp, List* instructions, void* o) {        

#ifdef WIN32
    system("cls");
#else
    cout << m_clear << m_home;
#endif

    cout << endl << m_current;
    lispe_editor* editor = (lispe_editor*)o;
    
    long current_line = lisp->delegation->current_line;
    if (editor->current_line_debugger == current_line) {
        lisp->delegation->next_stop = true;
        return;
    }
    
    editor->current_line_debugger = current_line;
    long line =  current_line - 15;
    if (line < 1)
        line = 1;
    
    string theline;
    map<long, string>::iterator it = lisp->delegation->listing[lisp->delegation->i_current_file].upper_bound(line);
    line = current_line + 15;
    for (; it != lisp->delegation->listing[lisp->delegation->i_current_file].end(); it++) {
        if (it->first > line)
            break;
        
        if (it->first == current_line)
            cout << m_selectgray << "[" << it->first << "] " << it->second << m_current;
        else
            cout << "(" << it->first << ") " << editor->coloringline(it->second) << m_current;
    }
    cout << endl;

    string thevalue = instructions->toString(lisp);
    if (thevalue.size() > 64) {
        thevalue = thevalue.substr(0,64);
        thevalue += "...";
    }

    cout << m_selectgray << thevalue << m_current << endl;
    
    vector<Element*> atomes;
    lisp->extractAllAtoms(instructions, atomes);
    Element* value;
    
    if (atomes.size())
        cout << endl;
    
    std::map<string, Element*> uniques;
    for (auto& a: atomes) {
        if (a->label() == v_true)
            continue;
        uniques[a->toString(lisp)] = a;
    }
    Element* e;
    for (auto& a: uniques) {
        e = a.second;
        value = lisp->getvalue(e->label());
        if (!value->isFunction()) {
            thevalue =  value->toString(lisp);
            if (thevalue.size() > 512) {
                thevalue = thevalue.substr(0,512);
                thevalue += "...";
            }
            cout << editor->colors[2] << e->toString(lisp) << ":" << m_red << thevalue << m_current << endl;
        }
    }
    
    cout << endl << m_gray << "$:end !:stop down:inside up:out >:go %:variables: " << m_red;
    cout.flush();
    lisp->blocking_trace_lock();
}

//This is the debugger thread
void debuggerthread(lispe_editor* call) {
    call->lispe->set_debug_function(debug_function_lispe, call);
    cout << m_red;
    call->runcode();
    call->debugmode = false;
    call->option = x_none;
    cout << back << "program terminated" << endl;
    call->line = L"";
    call->displaygo(true);
    cout.flush();
}
//-------------------------------------------------------------------------------------------

void execute_pipe(string& code, string& codeinitial, string& codefinal, string& rgx, bool with_file) {
    LispE lisp;
    Element* e;
    
    if (codeinitial != "") {
        e = lisp.eval(codeinitial);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
    }

    lisp.eval("(setq accu1 0) (setq accu2 0) (setq accu3 0) (setq accu4 0) (setq accu5 0) (setq accu6 0) (setq accu7 0) (setq accu8 0) (setq accu9 0)");

    if (with_file) {
        e = lisp.load(code);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
        if (!lisp.checkencoding(L"runpipe")) {
            std::cerr << "Error: function '(defun runpipe() ...)' is not defined" << endl;
            exit(-1);
        }
        code = "(runpipe)";
    }
    
    string line_in;
    vector<string> fields;
    string blanc(" ");
    short l;
    long i;
    std::stringstream setq_list;
    std::stringstream setq;
    char rgxtype[] = {0,0,0};
    if (rgx != "") {
        if (rgx[0] == 'p') {
            rgxtype[0] = 'p';
            rgxtype[1] = 'r';
        }
        else
            rgxtype[0] = 'r';
        rgx = rgx.substr(1, rgx.size());
        setq << "(setq rx (" << rgxtype << "gx `" << rgx <<"`))";
        e = lisp.eval(setq.str());
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
        setq.str("");
        setq.clear();
    }
    
    
    while (!std::cin.eof()) {
        getline(std::cin, line_in);
        
        s_trim(line_in);
        if (line_in == "")
            continue;
            
        //Initialisation des champs
        setq << "(setq l0 " << jsonstring(line_in) << ")" << endl;
        setq << "(setq ln " << fields.size() << ")" << endl;

        if (rgxtype[0]) {
            setq << "(if (eq (" << rgxtype << "gx_find rx l0) ``) (print) (block ";
        }

        fields.clear();

        s_split(line_in, blanc, fields);
        setq_list << "(setq ll (list";
        for (i = 0; i < fields.size(); i++) {
            setq_list << " ";
            setq << "(setq l" << i + 1 << " ";
            setq_list << "l" << i + 1;
            line_in = fields[i];
            noconvertingfloathexa(STR(line_in), l);
            if (l == line_in.size()) {
                setq << line_in << ")";
            }
            else
                setq << jsonstring(line_in) << ")";
            setq << endl;
        }
        setq_list << "))";

        for (i = fields.size(); i < 16; i++)
            setq << "(setq l" << i + 1 << "\"\")" << endl;

        setq << setq_list.str() << endl << code << endl;
        
        if (rgxtype[0])
            setq << "))" << endl;
        
        
        e = lisp.eval(setq.str());
        
        line_in = e->toString(&lisp);
        if (line_in != "" && codefinal == "")
            std::cout << line_in << endl;
        e->release();

        setq_list.str("");
        setq_list.clear();
        setq.str("");
        setq.clear();
    }
    if (codefinal != "") {
        e = lisp.eval(codefinal);
        line_in = e->toString(&lisp);
        if (line_in != "")
            std::cout << line_in << endl;
        e->release();
    }
}

#ifdef DEBUG
//Minimale version without the internal editor
int main(int argc, char *argv[]) {
    LispE lisp;
    Element* e;
    
    if (argc == 2) {
        string fichier = argv[1];
        e = lisp.load(fichier);
        std::cout << e->toString(&lisp) << std::endl;
        e->release();
        return 0;
    }

    string code;

    cout << endl << "Lisp Elémentaire (" << LispVersion() << ")" << endl << endl;
     
    while (true) {
        std::cout << "> ";
        while (getline(std::cin, code)) {
            s_trim(code);
            if (code != "") {
                if (code == "fin") {
                    cout << "Fin" << endl;
                    exit(-1);
                }
                if (code[0] != '(' && code.back() != ')') {
                    code  = "(print " + code + ")";
                }
                e = lisp.execute(code);
                std::cout << e->toString(&lisp) << std::endl;
                e->release();
                code = "";
            }
            std::cout << "> ";
        }
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << ">> try again !!!" << endl;
    }
}
#else
//Version avec éditeur jag intégré
int main(int argc, char *argv[]) {
    string args;
    string code;
    string file_name;
    string rgx;
    string codeinitial;
    string codefinal;
    long i;
    for (i = 1; i < argc; i++) {
        args = argv[i];
        if (args == "-h") {
            cout << endl << m_red << "Different types of operation: " << m_current << endl;
            cout << m_red << "\tlispee program arg1 arg2:" << m_blue <<" execution of 'program' with optional list of arguments" << m_current << endl << endl;
            cout << m_red << "\tlispee -a:" << m_blue << "The entire pipe is stored in '_args'. Two uses:" << m_current << endl;
            cout << m_red << "\t\t- lispee -a: " << m_blue <<"gives back the hand in the interactive interpreter" << m_current << endl;
            cout << m_red << "\t\t- lispee program -a " << m_blue <<"execute 'program' with _args containing the output of the 'pipe'" << m_current << endl << endl;
            cout << m_red << "\tlispee -pb/-pe/-p 'code' arg1 arg2:" << m_blue << "Execution of 'code' on a pipe output: ls -al | lisp -p '(+ l2 l3)'" << m_current << endl;
            cout << m_red << "\t\t- '-pb' allows to execute an initial code (must be placed before '-p')" << m_current << endl;
            cout << m_red << "\t\t- '-pe' allows to execute a final code (must be placed before '-p')" << m_current << endl;
            cout << m_red << "\t\t- The lines are automatically cut along the spaces into variables: " << m_current << endl;
            cout << m_red << "\t\t\t- accu1, accu2,..., accu9: " << m_blue <<"Nine predefined accumulators initialized to 0 at startup" << m_current << endl;
            cout << m_red << "\t\t\t- ln: " << m_blue <<"is the number of fields" << m_current << endl;
            cout << m_red << "\t\t\t- ll: " << m_blue <<"is the list of fields" << m_current << endl;
            cout << m_red << "\t\t\t- l0: " << m_blue <<"is the full line" << m_current << endl;
            cout << m_red << "\t\t\t\t- l1, l2, l3...: " << m_blue << "corresponds to a field in the line in the order in which these fields were cut" << m_current << endl;
            cout << m_red << "\tlispe -P program.lisp arg1 arg2:" << m_blue << "Execution of program file on a pipe output: ls -al | lisp -P file" << m_current << endl;
            cout << m_red << "\t\t- '-P' has the same variables as '-p'. m_current" << m_current << endl;
            cout << m_red << "\t\t- '-P' 'program.lisp' must contain the function: (defun runpipe()...) which corresponds to the entry point of the processing. "<< m_current << endl << endl;

            cout << m_red << "\tlispee -r 'rgx':" << m_blue << " Condition (posix regular expressions) on 'stdin'" << endl;
            cout << "\t\t to be used with '-p' or '-P'. This condition must appear before -p" << m_current << endl;
            cout << m_red << "\tlispee -R 'rgx':" << m_blue << "Condition (internal style regular expressions) on 'stdin'" << endl;
            cout << "\t\t to be used with '-p' or '-P'. This condition must appear before -p" << m_current << endl;

            cout << m_red << "\tlispee -e program arg1 arg2:" << m_blue << "edit 'program' with optional list of arguments" << m_current << endl << endl;
            cout << m_red << "\tlispee: " << m_blue <<" interactive interpreter" << m_current << endl << endl;

            return -1;
        }
            
        //Lecture du pipe dans args
        if (args == "-a") {
            while (!std::cin.eof()) {
                getline(std::cin, code);
                arguments.push_back(code);
            }
            std::cin.clear();
            continue;
        }

        if (args == "-pb") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-pb'" << endl;
                exit(-1);
            }
            codeinitial = argv[++i];
            continue;
        }

        if (args == "-pe") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-pe'" << endl;
                exit(-1);
            }
            codefinal = argv[++i];
            continue;
        }

        if (args == "-r") {
            if (i >= argc - 1) {
                cerr << "Missing regular expression for option  '-r'" << endl;
                exit(-1);
            }
            rgx = "p";
            rgx += argv[++i];
            continue;
        }

        if (args == "-R") {
            if (i >= argc - 1) {
                cerr << "Missing regular expression for option '-R'" << endl;
                exit(-1);
            }
            rgx = "r";
            rgx += argv[++i];
            continue;
        }

        //Lecture du pipe
        if (args == "-p") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-p'" << endl;
                exit(-1);
            }
            i++;
            code = argv[i];
            while (i < argc) {
                arguments.push_back(argv[i]);
                i++;
            }
            execute_pipe(code, codeinitial, codefinal, rgx, false);
            exit(0);
        }

        if (args == "-P") {
            if (i >= argc - 1) {
                cerr << "Missing program file for option '-P'" << endl;
                exit(-1);
            }
            i++;
            file_name = argv[i];
            execute_pipe(file_name,codeinitial, codefinal, rgx, true);
            exit(0);
        }


        if (args == "-e") {
            JAGEDITOR = new lispe_editor();
            string nomfichier;
            if (i < argc - 1) {
                i++;
                nomfichier = argv[i];
                JAGEDITOR->setpathname(nomfichier);
                while (i < argc) {
                    arguments.push_back(argv[i]);
                    i++;
                }
            }
            JAGEDITOR->loadfile(nomfichier);
            ((lispe_editor*)JAGEDITOR)->addspace(nomfichier);
            wstring w;
            string line = "read ";
            line += nomfichier;
            w = JAGEDITOR->wconvert(line);
            JAGEDITOR->addcommandline(w);
            line = "";
            JAGEDITOR->launchterminal(3);
            return 0;
        }
        
        if (file_name == "")
            file_name = argv[i];
        arguments.push_back(argv[i]);
    }
    
    if (file_name != "") {
        LispE lisp;
        lisp.arguments(arguments);
        string the_file = file_name;
        Element* e = lisp.load(the_file);
        std::cout << e->toString(&lisp) << std::endl;
        e->release();
        return 0;
    }
    
    JAGEDITOR = new lispe_editor();
    JAGEDITOR->launchterminal(2);
}
#endif
