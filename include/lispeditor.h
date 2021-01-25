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

#ifndef lispeditor_h
#define lispeditor_h

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
#include "rgx.h"


string Normalizefilename(string path);

class lispe_editor : public jag_editor {
    Tokenizer parse;
    
    hmap<string, short> filenames;
    vector<string> ifilenames;
    vector<wstring> codes;
    
    vector<editor_keep> editors_undos;
    vector<editor_keep> editors_redos;
    vector<long> lastlines;

    std::thread* tid;
    

    vector<string> displaying;
    
    string historyfilename;

    long currentfileid;
    long pcursor;
    long idCode;
    long lastline;
    
    
    bool editmode;
    

    
public:
    ThreadLock lock;
    ThreadLock printlock;

    unordered_map<string, unordered_map<long, bool> > editor_breakpoints;

    //--------------------------------------------------
    //This is the section, which is dedicated to LispE
    //master_lisp is used for multi-threaded programs debugging
    LispE* lispe;
    LispE* master_lisp;
    //--------------------------------------------------
    
    
    string current_code;
    long current_line_debugger;
    long current_file_debugger;
    long current_thread_id;
    
    std::atomic<bool> reading;
    string input_string;
    string output_string;
    bool debugmode;
    bool displaying_print;
    bool displaying_local_variables;

    lispe_editor() {
        selected_x = -1;
        selected_y = -1;
        selected_pos = -1;
        double_click = 0;
        reading = false;
        displaying_print = true;
        displaying_local_variables = true;
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
        return coloringline(line, false);
    }
    
    void initlisp(bool init, bool setpath);
    
    long splitline(wstring& l, long linenumber, vector<wstring>& subs);

    string coloringline(string line, bool thread);
    
    void displaythehelp(long i);

    long taille(wstring& s);
    long sizestring(wstring& s);
    void cleanlongemoji(wstring& s, wstring& cleaned, long p);
    long size_upto(wstring& s, long p);
    //The deletion of a character is different if it is an emoji...
    long deleteachar(wstring& l, bool last, long pins);
    void forwardemoji();
    void backwardemoji();

    //------------------------------------------------------------------------------------------------
    //Cursor movements...
    //------------------------------------------------------------------------------------------------
    string prefixstring(long n) {
        if (editor_breakpoints.size() && lispe != NULL) {
            try {
                if (editor_breakpoints.at(thecurrentfilename).at(n))
                    return "^^";
            }
            catch(const std::out_of_range& oor) {}
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
            LispSetCode(c);
            lines.setcode(c);
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
        LispSetCode(cde);
        wstring code = WListing();
        lines.setcode(code);
        editors_undos[currentfileid].clear();
        editors_redos[currentfileid].clear();
        line = L"edit";
        bool dsp = true;
        pos = handlingcommands(pos, dsp);
        displayonlast("Reloaded", true);
        return true;
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
            lastlines.push_back(0);
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

    
    bool evallocalcode(string code, bool disp=false);
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

    bool checkcommand(char c);    
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
                    long idline = lines.numeros[pos];
                    try {
                        editor_breakpoints.at(thecurrentfilename).at(idline) = 1 - editor_breakpoints.at(thecurrentfilename).at(idline);
                    }
                    catch(const std::out_of_range& oor) {
                        editor_breakpoints[thecurrentfilename][idline] = true;
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
                    printline(pos+1, line);
                return true;
            case 12: //ctrl-l: reload file
                if (emode())
                    reloadfile();
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
    
    void reading_a_string(string& buff) {
        input_string += buff;
        get_a_string(input_string);
        lispe->releasing_trace_lock();
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
    
    void launchterminal(bool darkmode, char noinit, vector<string>& theargs);
    
    void setcode(string& code) {
        LispSetCode(code);
        wstring cd = wconvert(code);
        lines.setcode(cd);
        pos = lines.size();
    }
    
    bool Executesomecode(wstring& c);

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

#endif
