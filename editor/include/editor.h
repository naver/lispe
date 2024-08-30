/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  listes.h
//
//

//This is the implementation of the List object

#ifndef editor_h
#define editor_h

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

#include "rgx.h"


string Normalizefilename(string path);
u_ustring from_ascii(string s);
bool checkOtherCases(u_ustring& u);

class interpreter_editor : public jag_editor {
    hmap<string, int16_t> filenames;
    vector<string> ifilenames;
    vector<wstring> codes;
    
    vector<editor_keep> editors_undos;
    vector<editor_keep> editors_redos;
    vector<long> lastlines;

    vector<string> displaying;
    
    string historyfilename;

    long currentfileid;
    long pcursor;
    long idCode;
    long lastline;
    
    

    
public:
    unordered_map<string, unordered_map<long, bool> > editor_breakpoints;    
    
    string current_code;
    string title_string;
    long current_thread_id;
    
    std::atomic<bool> reading;
    string input_string;
    string output_string;
    bool editmode;
    bool displaying_print;
    bool displaying_local_variables;

    interpreter_editor() {
        title_string = "Editor";
        selected_x = -1;
        selected_y = -1;
        selected_pos = -1;
        double_click = 0;
        reading = false;
        displaying_print = true;
        displaying_local_variables = true;
        currentfileid = -1;
        editmode = false;
        pcursor = -1;
        idCode = -1;
        lastline = 0;
        filetype = no_type;
    }
    
    void SetCurrentCode(string& c) {
        current_code = c;
    }
    
    
    void SetCurrentCode(wstring& c) {
        current_code = convert(c);
    }
    
    /*
    ------------------------------------------------------------------------------------------    
    This is the section of methods that should be overloaded for a new interpreter
    ------------------------------------------------------------------------------------------
    */

    virtual void clean_breakpoints(long) {}

    virtual bool interpreter_active() {
        return false;
    }

    virtual bool run_code() {
        cout << m_red;
        //Execution here and display
        cout << "Run Output" << endl;
        cout << m_current;
        return true;
    }

    virtual void stop_execution() {}

    bool execute_code_with_history(wstring& c) {
        addcommandline(c);
        return execute_code(c);
    }

    virtual bool execute_code(wstring& c) {
        cout << m_red;
        //Execution here and display
        cout << "Execution Output" << endl;
        cout << m_current;
        return true;
    }

    virtual void load_code(string& n) {}
    virtual void init_interpreter(bool init, string filename) {}
    virtual void initialize_breakpoints() {}

    virtual wstring unix_command(wstring);

    wstring WListing() {
        return wconvert(current_code);
    }
    
    bool emode() {
        if (editmode)
            return (option == x_none);

        noprefix = false;
        return false;
    }
    
    string coloringline(wstring& l, long i) {
        string line = convert(l);
        return coloringaline(line, i, false);
    }
    
    //long splitline(wstring& l, long linenumber, vector<wstring>& subs);

    
    
    void displaythehelp(long i);

    //long taille(wstring& s);
    //long sizestring(wstring& s);
    //void cleanlongemoji(wstring& s, wstring& cleaned, long p);
    //long size_upto(wstring& s, long p);
    //The deletion of a character is different if it is an emoji...
    //long deleteachar(wstring& l, bool last, long pins);
    //void forwardemoji();
    //void backwardemoji();

    //------------------------------------------------------------------------------------------------
    //Cursor movements...
    //------------------------------------------------------------------------------------------------
    string prefixstring(long n) {
        if (editor_breakpoints.size() && interpreter_active()) {
            try {
                if (editor_breakpoints.at(thecurrentfilename).at(n))
                    return "^^";
            }
            catch (...) {}
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

    void printline(long n, wstring& l, long i) {
        if (emode()) {
            if (n > 0 && n < lines.size())
                n = lines.numeros[n - 1];
            if (noprefix)
                cout << back << coloringline(l, i);
            else {
                if (n == -1) {
                    string space(prefixe(), ' ');
                    cout << back << space << coloringline(l, i);
                }
                else {
                    cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << coloringline(l, i);
                }
            }
        }
        else
            cout << back << m_dore << prefix << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << coloringline(l, i);
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
                printline(pos+1, line, -1);
            }
            return false;
        }
        
        clearline();
        long i = commandlines.size();
        if (poscommand >= 0 && poscommand < i - 1) {
            line = commandlines[++poscommand];
            posinstring = linesize();
            printline(pos+1, line, -1);
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
                        cout << back << coloringline(line, poslines[currentline]);
                    else {
                        string space(prefixe(), ' ');
                        cout << back << space << coloringline(line, poslines[currentline]);
                    }
                }
                else {
                    if (!lines.check(pos) || noprefix)
                        printline(lines.numeros[pos], line, pos);
                    else {
                        string prf = prefix;
                        prefix = editor_prefix;
                        printline(lines.numeros[pos], line, pos);
                        prefix = prf;
                    }
                }
            }
            else
                printline(pos+1, line, -1);
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
        
        lines.detectlongstrings(filetype);
        modified = false;

        if (emode()) {
            x_option g = option;
            option = x_none;
            
            poslines.clear();
            
            lines.updatesize();
            
            for (long i = beg; i < lines.size(); i++) {
                string space(prefixe(), ' ');
                if (lines.status[i] == concat_line)
                    blk << space << coloringline(lines[i], i) << endl;
                else {
                    if (noprefix)
                        blk << coloringline(lines[i], i) << endl;
                    else {
                        blk << m_dore << prefixstring(lines.numeros[i]) << m_current << m_lightgray << std::setw(prefixsize) << lines.numeros[i] << "> " << m_current << coloringline(lines[i], i) << endl;
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
                blk << space << coloringline(lines[i], i) << endl;
            else {
                if (noprefix)
                    blk << coloringline(lines[i], i) << endl;
                else {
                    blk << m_dore << prefixstring(lines.numeros[i]) << m_current << m_lightgray << std::setw(prefixsize) << lines.numeros[i] << "> " << m_current << coloringline(lines[i], i);
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
        lastlines.push_back(0);
        codes.push_back(L"");
        editor_keep kp;
        editors_undos.push_back(kp);
        editors_redos.push_back(kp);
        filenames[thecurrentfilename] = currentfileid;
        if (!fromwrite) {
            //We create a local undos/redos section..
            cerr << m_redbold << "File space creation: " << thecurrentfilename << m_current <<" (" << currentfileid << ")" << endl;
            wstring c = L"\n";
            SetCurrentCode(c);
            lines.setcode(c, false);
        }
    }
    
    bool loadfile(wstring& name) {
        return loadfile(convert(name));
    }

    bool reloadfile() {
        if (thecurrentfilename == "")
            return false;
        
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
        SetCurrentCode(cde);
        wstring code = WListing();
        editors_undos[currentfileid].clear();
        editors_redos[currentfileid].clear();

        line = L"";
        lines.setcode(code, true);
        editmode = true;

        currentline = 0;
        posinstring = 0;
        lastline = 0;

        pos = lastline;
        option = x_none;
        posinstring = 0;
        line = lines[lastline];
        displaylist(lastline, row_size);
        movetoline(currentline);
        movetobeginning();
        return true;
    }
    
    //We keep different files in memory...
    bool loadfile(string filename) {
        setpathname(filename);
        wstring code;
        thecurrentfilename = s_trim(filename);
        if (thecurrentfilename == "")
            return false;
        
        thecurrentfilename = Normalizefilename(thecurrentfilename);
        setpathname(thecurrentfilename);
        if (filenames.count(thecurrentfilename)) {
            if (currentfileid != filenames[thecurrentfilename]) {
                //We backup our current undo/redo buffer
                undos.storein(editors_undos[currentfileid]);
                redos.storein(editors_redos[currentfileid]);
                
                currentfileid = filenames[thecurrentfilename];
                
                //We then activate the redo/undo in hold...
                editors_undos[currentfileid].storein(undos);
                editors_redos[currentfileid].storein(redos);
                code = codes[currentfileid];
                SetCurrentCode(code);
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
            SetCurrentCode(cde);
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
            lastlines.push_back(0);
            codes.push_back(code);
            
            //We create a new redo/undo section
            editor_keep kp;
            editors_undos.push_back(kp);
            editors_redos.push_back(kp);
            filenames[thecurrentfilename] = currentfileid;
        }
        line = L"";
        lines.setcode(code, true);
        currentline = 0;
        posinstring = 0;
        lastline = 0;
        pos = lastline;
        
        option = x_none;
        line = lines[lastline];
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
    
    long handlingcommands(long pos, bool& dsp);
    
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
                displayonlast("File not saved...", true);
            else
                printline(pos+1, "File not saved... ctrl-d to quit");
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
        
    void backtoconsole() {
        lastline = poslines[0];
        currentline = poslines.back();

        mouseoff();
        editmode = false;
        movetolastline();
        clearline();
        string l = m_red;
        l += "console";
        l += m_current;
        prefix = cmd_line_prefix;
        printline(pos+1, l);
        cout << endl;
        clearline();
        printline(pos+1);
                
        line = L"";
        posinstring = 0;
    }
    
    void clear() {
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
            SetCurrentCode(code);
            movetolastline();
            clearline();
            string l = m_red;
            l += "console";
            l += m_current;
            prefix = cmd_line_prefix;
            printline(pos+1, l);
            cout << endl;
            clearline();
            printline(pos+1);
        }

        stop_execution();
        
        fflush(stdout);
        line = L"";
        posinstring = 0;
        currentline = 0;
    }

    bool checkcommand(char c);
        
    bool checkkeyboard(string& buff, long& first, long& last, bool& dsp, char noinit) {
        switch ((uchar)buff[0]) {
            case 2:  //ctrl-b, adding breakpoints
                if (emode() && interpreter_active()) {
                    clean_breakpoints(lines.numeros[pos]);
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
                pos = handlingcommands(pos, dsp);
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
                    printline(pos+1, line, -1);
                return true;
            case 12: //ctrl-l: display one line down in the command history or toggle between top/bottom in edit mode
                if (emode()) {
                    clearst();
                    st << "load:";
                    displayonlast(false);
                    line = currentfind;
                    currentreplace = L"";
                    posinstring = currentfind.size();
                    option = x_load;
                }
                return true;
            case 17:
                if (emode()) {
                    backtoconsole();
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
        
    void display_indication() {
        cout << endl << m_gray << "$:end !:stop ↓:inside ↑:out →:go ←:breakpoint %:variables ";
        if (displaying_local_variables)
            cout << "#:local off ";
        else
            cout << "#:local on ";
        
        if (displaying_print)
            cout << "&:display off: " << m_red;
        else
            cout << "&:display on: " << m_red;
    }
    
    void launchterminal(bool darkmode, char noinit, vector<string>& theargs, vector<string>& newcolors);
    
    void setcode(string& code, bool clean) {
        SetCurrentCode(code);
        wstring cd = wconvert(code);
        lines.setcode(cd, clean);
        pos = lines.size();
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
            lines.setcode(code, false);
            displaylist(poslines[0], poslines.back());
            if ((currentline+1) >= poslines.size())
                currentline = poslines.size() - 1;
            movetoline(currentline);
            posinstring = 0;
            movetobeginning();
        }
    }

};

#endif
