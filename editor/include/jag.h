/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  jag.h
//
//

#ifndef jag_h
#define jag_h

#define solo_line 0
#define beg_line 1
#define concat_line 2

#define u_del_linked -2
#define u_del -1
#define u_modif 0
#define u_modif_linked 1
#define u_ins 2
#define u_ins_linked 3

#define l_str 1
#define l_com 2
#define l_com_one 3

#define openMode std::ios::in|std::ios::binary

#include "jagget.h"

#ifdef BOOSTPOSIXREGEX
#include <boost/regex.hpp>
using boost::regex;
using boost::sregex_token_iterator;
using boost::smatch;
using boost::match_results;
using boost::wregex;
using boost::wsregex_token_iterator;
using boost::wsmatch;
using boost::sregex_iterator;
using boost::wsregex_iterator;
#else
#ifdef POSIXREGEX
#include <regex>
#include <iterator>
using std::regex;
using std::sregex_token_iterator;
using std::smatch;
using std::match_results;
using std::wregex;
using std::wsregex_token_iterator;
using std::wsmatch;
using std::sregex_iterator;
using std::wsregex_iterator;
#endif
#endif

#include "rgx.h"

using std::list;
using std::string;
using std::wstring;
using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::ifstream;
using std::ofstream;

#include "tools.h"
#define hmap std::unordered_map
#define uchar unsigned char

class jag_editor;

extern jag_editor* JAGEDITOR;
bool evaluate_quotes(wstring& l);

const string colordenomination[] = {"string", "definition", "instruction", "quote", "comment", "call", "selection"};
const int16_t nbdenomination = 7;
//##### Prefix taht are displayed on top #####
const char editor_prefix[] = "";
const wchar_t editor_wprefix[] = L"";
const char cmd_line_prefix[] = "<>";

class wstring_controlled : public wstring {
public:

    inline wchar_t& operator [](long pos) {
        if (pos >= 0 && pos < size())
            return at(pos);
        return back();
    }

    inline wstring& operator= (const wstring& str) {
        this->assign(str);
        return *this;
    }
};

typedef enum {no_type, c_like_type, lisp_type, python_type, tamgu_type} file_types;

class editor_lines {
public:
    jag_editor* jag;
    vector<wstring> lines;
    vector<char> status;
    vector<char> longlines;
    vector<long> numeros;
    hmap<long, bool> checks;

    editor_lines(jag_editor* j) {
        jag = j;
    }

    bool check(long p) {
        return checks.count(p);
    }

    void setcode(wstring& code, bool clean);

    bool updatesize();

    long indent(long p);

    //we check if we have spaces from the start of the string and on
    void checkspace(long pos, long l, char code) {
        for (long i = 0; i < pos; i++) {
            if (lines[l][i] > 32) {
                longlines.push_back(0);
                return;
            }
        }
        longlines.push_back(code);
    }

    void detectlisp() {
        char l_strings = 0;
        char l_comments = 0;
        long pos;

        for (long i = 0; i < lines.size(); i++) {
            if (lines[i].find(L";;") != -1) {
                if (l_comments) {
                    longlines.push_back(2);
                    l_comments = 0;
                }
                else {
                    l_comments = 2;
                    longlines.push_back(l_comments);
                }
                continue;
            }

            if (l_comments) {
                longlines.push_back(l_comments);
                continue;
            }

            /*
            rules.push_back(U"«?*»=96");                   //long strings French way
            rules.push_back(U"“?*”=96");                   //long strings English
            rules.push_back(U"‘?*’=96");                   //long strings with single quotes (English)
            rules.push_back(U"„?*”=96");                //long strings German/Polish
            rules.push_back(U"❝?*❞=96");                   //long strings

            rules.push_back(U"#171?*#187=96");                   //long strings French way
            rules.push_back(U"#8220?*#8221=96");                   //long strings English
            rules.push_back(U"#8216?*#8217=96");                   //long strings with single quotes (English)
            rules.push_back(U"#8222?*#8221=96");                //long strings German/Polish
            rules.push_back(U"#10077?*#10078=96");                   //long strings
             */
            
            wstring closing_value;
            pos = lines[i].find(171);
            closing_value = 187;
            if (pos == -1) {
                pos = lines[i].find(8220);
                closing_value = 8221;
                if (pos == -1) {
                    pos = lines[i].find(8216);
                    closing_value = 8217;
                    if (pos == -1) {
                        pos = lines[i].find(8222);
                        closing_value = 8221;
                        if (pos == -1) {
                            pos = lines[i].find(10077);
                            closing_value = 10078;
                            if (pos == -1) {
                                closing_value = L"`";
                                pos = lines[i].find(closing_value);
                                if (pos == -1) {
                                    closing_value = L"\"\"\"";
                                    pos = lines[i].find(closing_value);
                                }
                            }
                        }
                    }
                }
            }
            
            if (pos != -1 && lines[i].find(closing_value, pos + 1) == -1) {
                if (l_strings) {
                    longlines.push_back(1);
                    l_strings = 0;
                }
                else {
                    l_strings = 1;
                    checkspace(pos, i, l_strings);
                }
            }
            else
                longlines.push_back(l_strings);
        }
    }

    void detectpython() {
        char l_strings = 0;
        long pos;

        for (long i = 0; i < lines.size(); i++) {
            pos = lines[i].find(L"'''");
            if (pos != -1 && lines[i].find(L"'''", pos + 1) == -1) {
                if (l_strings) {
                    longlines.push_back(1);
                    l_strings = 0;
                }
                else {
                    l_strings = 1;
                    checkspace(pos, i, l_strings);
                }
            }
            else {
                pos = lines[i].find(L"\"\"\"");
                if (pos != -1 && lines[i].find(L"\"\"\"", pos + 1) == -1) {
                    if (l_strings) {
                        longlines.push_back(1);
                        l_strings = 0;
                    }
                    else {
                        l_strings = 1;
                        checkspace(pos, i, l_strings);
                    }
                }
                else
                    longlines.push_back(l_strings);
            }
        }
    }

    void detectclike() {
        char l_comments = 0;
        long pos;
        for (long i = 0; i < lines.size(); i++) {
            pos = lines[i].find(L"/*");
            if (pos != -1) {
                if (lines[i].find(L"*/") == -1) {
                    l_comments = 2;
                    checkspace(pos, i, l_comments);
                }
                else
                    longlines.push_back(0);
            }
            else {
                if (lines[i].find(L"*/") != -1) {
                    longlines.push_back(l_comments);
                    l_comments = 0;
                }
                else
                    longlines.push_back(l_comments);
            }
        }
    }

    void detecttamgu() {
        char l_strings = 0;
        char l_comments = 0;
        long pos;

        for (long i = 0; i < lines.size(); i++) {
            if (lines[i].find(L"/@") != -1) {
                if (lines[i].find(L"@/") == -1) {
                    l_comments = 2;
                    longlines.push_back(l_comments);
                }
                else
                    longlines.push_back(0);
            }
            else {
                if (lines[i].find(L"@/") != -1) {
                    longlines.push_back(l_comments);
                    l_comments = 0;
                }
                else {
                    if (l_comments)
                        longlines.push_back(l_comments);
                    else {
                        pos = lines[i].find(L"@\"");
                        if (pos != -1) {
                            if (lines[i].find(L"\"@") == -1) {
                                l_strings = 1;
                                checkspace(pos, i, l_strings);
                            }
                            else
                                longlines.push_back(0);
                        }
                        else {
                            if (lines[i].find(L"\"@") != -1) {
                                longlines.push_back(l_strings);
                                l_strings = 0;
                            }
                            else
                                longlines.push_back(l_strings);
                        }
                    }
                }
            }
        }
    }

    void detectlongstrings(file_types ftype) {
        longlines.clear();
        switch (ftype) {
            case lisp_type:
                detectlisp();
                break;
            case python_type:
                detectpython();
                break;
            case tamgu_type:
                detecttamgu();
                break;
            case c_like_type:
                detectclike();
                break;
            default:
                for (long i = 0; i < lines.size(); i++)
                    longlines.push_back(0);
        }
    }


    void numbers() {
        long nb = 0;
        numeros.clear();
        for (long i = 0; i < lines.size(); i++) {
            if (status[i] != 2)
                nb++;
            numeros.push_back(nb);
        }

        updatesize();
    }

    long getlinenumber(long l) {
        for (long i = 0; i < numeros.size(); i++) {
            if (numeros[i] == l)
                return i;
        }
        return 0;
    }

    wstring code() {
        wstring c;
        bool rc = false;
        for (long i = 0; i < lines.size(); i++) {
            if (status[i] != concat_line) {
                if (rc) {
                    c += L"\n";
                    rc = false;
                }
                c += lines[i];
                if (status[i] == solo_line)
                    c += '\n';
                continue;
            }
            c += lines[i];
            rc = true;
        }
        return c;
    }

    wstring code(long first, long end) {
        wstring c;
        bool rc = false;
        if (first < 0)
            first = 0;
        if (end > lines.size())
            end = lines.size();
        for (long i = first; i < end; i++) {
            if (status[i] != concat_line) {
                if (rc) {
                    c += L"\n";
                    rc = false;
                }
                c += lines[i];
                if (status[i] == solo_line)
                    c += '\n';
                continue;
            }
            c += lines[i];
            rc = true;
        }
        return c;
    }

    wstring& operator[](long pos) {
        if (pos == lines.size())
            push(L"");
        return lines[pos];
    }

    long size() {
        return lines.size();
    }

    long splitline(wstring& l, long linenumber, vector<wstring>& subs);

    char Status(long pos) {
        if (pos >= 0 && pos < lines.size())
            return status[pos];
        return 0;
    }

    //Check if we can go up to the end of the line
    bool eol(long p) {
        if (Status(p) != solo_line &&  Status(p+1) == concat_line)
            return false;
        return true;
    }
        //the line is cut after pos (either for destruction of copy)
        //the line is cut after pos (either for destruction of copy)
    char updatestatus(long pos);

    void erase(long pos) {
        lines.erase(lines.begin()+pos);
        status.erase(status.begin()+pos);
    }

    void erase(long pos, long end) {
        if (end >= lines.size())
            end = -1;

        if (end == -1) {
            lines.erase(lines.begin()+pos, lines.end());
            status.erase(status.begin()+pos, status.end());
        }
        else {
            lines.erase(lines.begin()+pos, lines.begin() + end);
            status.erase(status.begin()+pos, status.begin() + end);
        }
    }

    void insert(long pos, wstring& sub, char st) {
        lines.insert(lines.begin() + pos, sub);
        status.insert(status.begin()+pos, st);
        updatesize();
    }

    void insert(long pos, wstring& sub) {
        lines.insert(lines.begin() + pos, sub);
        status.insert(status.begin()+pos, solo_line);
        if (numeros.size())
            numeros.push_back(numeros.back()+1);
        else
            numeros.push_back(1);
        updatesize();
    }

    void inserting(long pos, wstring sub) {
        lines.insert(lines.begin() + pos, sub);
        status.insert(status.begin()+pos, solo_line);
        if (numeros.size())
            numeros.push_back(numeros.back()+1);
        else
            numeros.push_back(1);
        updatesize();
    }

    void push_back(wstring& sub) {
        lines.push_back(sub);
        status.push_back(solo_line);
        if (numeros.size())
            numeros.push_back(numeros.back()+1);
        else
            numeros.push_back(1);
        updatesize();
    }

    void push(wstring sub) {
        lines.push_back(sub);
        status.push_back(solo_line);
        if (numeros.size())
            numeros.push_back(numeros.back()+1);
        else
            numeros.push_back(1);
        updatesize();
    }

    void pop_back() {
        if (lines.size()) {
            lines.pop_back();
            status.pop_back();
            numeros.pop_back();
        }
    }

    wstring getoneline(long pos, long& end) {
        if (status[pos] == solo_line) {
            end = pos;
            return lines[pos];
        }

        wstring line = lines[pos++];
        while (pos < lines.size() && Status(pos) == concat_line)
            line += lines[pos++];

        end = pos-1;
        return line;
    }

    void clear() {
        checks.clear();
        lines.clear();
        status.clear();
        numeros.clear();
    }

    wstring back() {
        return lines.back();
    }

    bool checkfullsize(wstring&, bool&);
    bool checksize(long p);
    void undo(wstring& l, long p, char a);

    void replaceline(long p, long end, wstring& line) {
        bool equal = false;
        if (checkfullsize(line, equal)) {
                //We need to protect the last line
            vector<wstring> subs;
            splitline(line, numeros[p], subs);
            long u;
            for (u = 0; u < subs.size(); u++) {
                if ((p+u) < end)
                    lines[p+u] =  subs[u];
                else {
                    lines.insert(lines.begin()+p+u, subs[u]);
                    status.insert(status.begin()+p+u, concat_line);
                }
            }

            if ((p+u) < end)
                erase(p+u, end);
            numbers();
            return;
        }
        lines[p] = line;
    }

    bool refactoring(long p);


};
    ///------------------------------------------------------------------------------------

class editor_keep {
public:
    list<wstring> l_keeplines;
    list<long> l_keeppos;
    list<char> l_keepactions;
    list<long> l_keepposinstring;
    list<long> l_keepcurrentline;
    list<long> l_keeptop;
    list<char> l_keepstatus;

    void pop() {
        l_keeplines.pop_back();
        l_keeppos.pop_back();
        l_keepactions.pop_back();
        l_keepcurrentline.pop_back();
        l_keepposinstring.pop_back();
        l_keeptop.pop_back();
        l_keepstatus.pop_back();
    }

    void move(wstring& l, editor_keep& e) {
        l_keeplines.push_back(l);
        l_keeptop.push_back(e.l_keeptop.back());
        l_keeppos.push_back(e.l_keeppos.back());
        l_keepactions.push_back(e.l_keepactions.back());
        l_keepposinstring.push_back(e.l_keepposinstring.back());
        l_keepcurrentline.push_back(e.l_keepcurrentline.back());
        l_keepstatus.push_back(e.l_keepstatus.back());
        e.pop();
    }

    void clear() {
        l_keeplines.clear();
        l_keeppos.clear();
        l_keepactions.clear();
        l_keepcurrentline.clear();
        l_keepposinstring.clear();
        l_keeptop.clear();
        l_keepstatus.clear();
    }

    void store(long postop, wstring& line, long pos, char action, long currentline, long posinstring, char status) {
        l_keeptop.push_back(postop);
        l_keeplines.push_back(line);
        l_keeppos.push_back(pos);
        l_keepactions.push_back(action); //deletion in this case in a modification
        l_keepcurrentline.push_back(currentline);
        l_keepposinstring.push_back(posinstring);
        l_keepstatus.push_back(status);
    }

    void storein(editor_keep& e) {
        e.l_keeptop = l_keeptop;
        e.l_keeplines = l_keeplines;
        e.l_keeppos = l_keeppos;
        e.l_keepactions = l_keepactions;
        e.l_keepcurrentline = l_keepcurrentline;
        e.l_keepposinstring = l_keepposinstring;
        e.l_keepstatus = l_keepstatus;
    }

    void prune() {
        if (l_keepactions.size() >= 10000) {
            list<wstring>::iterator srange_end = l_keeplines.begin();
            std::advance(srange_end,1000);
            l_keeplines.erase(l_keeplines.begin(), srange_end);

            list<long>::iterator prange_end = l_keeppos.begin();
            std::advance(prange_end,1000);
            l_keeppos.erase(l_keeppos.begin(), prange_end);

            list<char>::iterator arange_end = l_keepactions.begin();
            std::advance(arange_end,1000);
            l_keepactions.erase(l_keepactions.begin(), arange_end);

            list<long>::iterator psrange_end = l_keepposinstring.begin();
            std::advance(psrange_end,1000);
            l_keepposinstring.erase(l_keepposinstring.begin(), psrange_end);

            list<long>::iterator crange_end = l_keepcurrentline.begin();
            std::advance(crange_end,1000);
            l_keepcurrentline.erase(l_keepcurrentline.begin(), crange_end);

            list<long>::iterator trange_end = l_keeptop.begin();
            std::advance(trange_end,1000);
            l_keeptop.erase(l_keeptop.begin(), trange_end);

            list<char>::iterator strange_end = l_keepstatus.begin();
            std::advance(strange_end,1000);
            l_keepstatus.erase(l_keepstatus.begin(), strange_end);
        }
    }

    bool empty() {
        if (l_keeplines.size() == 0)
            return true;
        return false;
    }

    void display();

};

//--------------------------------------------------------------------------------


typedef enum { x_none, x_goto, x_find, x_replace, x_findnocase, x_replacenocase, x_rgx, x_replacergx, x_prgx, x_replaceprgx, x_write, x_count, x_delete, x_copy, x_cut, x_paste, x_pasteselect, x_deleting, x_cutting, x_load, x_exitprint, x_debug, x_togglemouse} x_option;


class Jag_automaton;

class jag_editor : public jag_get {
public:

    vector<string> arguments;

    vector<wstring> commandlines;
    vector<long> longstrings;
    vector<string> colors;
    vector<long> poslines;

    editor_lines lines;

    editor_keep undos;
    editor_keep redos;

    file_types filetype;

    std::stringstream localhelp;
    std::wstringstream st;

    string thecurrentfilename;
    string prefix;

    wstring wprefix;
    wstring_controlled line;
    wstring currentfind;
    wstring currentreplace;
    wstring kbuffer;
    wstring copybuffer;

    long selected_x, selected_y, selected_pos, selected_posnext, selected_firstline, double_click;

    long poscommand;

    long linematch;

    long pos;
    long posinstring;
    long currentposinstring;
    long currentfindpos;
    long currentline;

    int prefixsize;
    int xcursor, ycursor;

    x_option option;

    bool replaceall;
    bool modified;
    bool tobesaved;
    bool tooglehelp;
    bool updateline;
    bool noprefix;
    bool previous_noprefix;
	bool insertaline;
    bool taskel;
    bool moveup;

    Au_automate* rgx;

#ifdef POSIXREGEX
    wregex* posixrgx;
#endif
    char regularexpressionfind;

    jag_editor();
    ~jag_editor();


#ifdef WIN32
	string getch();
#endif

    virtual string coloringline(wstring& l, long i);

    virtual void displaythehelp(long noclear = 0);

    void deleteselection();
    void displayextract(wstring& sub, long pos, long from_pos, long to_pos, bool select = true);
    void selectlines(long from_line, long to_line, long from_pos, long to_pos);
    void unselectlines(long from_line, long to_line, long from_pos, long to_pos);
    virtual void handlemousectrl(string& mousectrl);
    void indentplus();
    void deindentminus();

    virtual void setpathname(string path) {
        thecurrentfilename =  path;
        if (thecurrentfilename.find(".lisp") != -1)
            filetype = lisp_type;
        else {
            if (thecurrentfilename.find(".py") != -1)
                filetype = python_type;
            else {
                if (thecurrentfilename.find(".tmg") != -1)
                    filetype = tamgu_type;
                else
                    if (thecurrentfilename.find(".java") != -1 ||
                        thecurrentfilename.find(".cpp") != -1 ||
                        thecurrentfilename.find(".cxx") != -1 ||
                        thecurrentfilename.find(".hpp") != -1 ||
                        thecurrentfilename.find(".h") != -1)
                        filetype = c_like_type;
                    else
                        filetype = no_type;
            }
        }
    }

	inline void resetselection() {
		selected_firstline = -1;
		selected_x = -1;
		selected_y = -1;
		selected_pos = -1;
		selected_posnext = -1;
	}

    string pathname() {
        return thecurrentfilename;
    }

    wstring wpathname() {
        wstring name = wconvert(thecurrentfilename);
        return name;
    }

    void setscrolling();
    void resetscrolling();

    long colsize() {
        return col_size;
    }

    void clearst() {
        st.str(L"");
        st.clear();
    }

    virtual bool emode() {
        if (option == x_none)
            return true;
        return false;
    }

    bool checkpath(bool checkcmd = true);
    void ls(string cmd, string path, vector<wstring>& paths);


    //------------------------------------------------------------------------------------------------
    //Undo/Redo
    //------------------------------------------------------------------------------------------------

    long computeparenthesis(string& ln, char checkcar, long limit) {
        long posmatch = -1;
        vector<long> positions;
        char check;
        if (checkcar == ')')
            check = '(';
        else
            check = checkcar - 2;

        for (long i = 0; i < limit; i++) {
            switch (ln[i]) {
                case '"':
                    i++;
                    while (i < limit && ln[i] != '"') {
                        if (ln[i] == '\\')
                            i++;
                        i++;
                    }
                    break;
                case '`':
                    i++;
                    while (i < limit && ln[i] != '`') {
                        i++;
                    }
                    break;
                case '(':
                case '{':
                case '[':
                    if (check == ln[i])
                        positions.push_back(i);
                    break;
                case ')':
                case '}':
                case ']':
                    if (checkcar == ln[i]) {
                        if (positions.size())
                            positions.pop_back();
                    }
                    break;
            }
        }
        if (positions.size())
            posmatch = positions.back();
        return posmatch;
    }

    void setnoprefix() {
        noprefix = 1 - noprefix;
        previous_noprefix = noprefix;
        if (noprefix) {
            prefix = "";
            wprefix = L"";
            prefixsize = 0;
            margin = 2;
        }
        else {
            margin = margin_value_reference;
            prefix = editor_prefix;
            wprefix = editor_wprefix;
            setprefixesize(lines.size());
        }
        resetscreen();
    }

    virtual string coloringaline(string line, long i, bool thread);

    void undo(wstring& l, long p, char a) {
        if (!emode() || p >= lines.size())
            return;

        modified = true;
        redos.clear();
        undos.prune();
        undos.store(poslines[0], l, p, a, currentline, posinstring, lines.Status(p));
    }

    void indentcode(long ps) {
        if (!lines.size())
            return;

        wstring code = lines.code();

        string codeindente;
        string cd = convert(code);
        IndentCode(cd, codeindente, GetBlankSize(), (filetype == lisp_type), (filetype == python_type));
        lines.clear();
        code = wconvert(codeindente);
        code += L"\n\n";

        lines.setcode(code, true);

        displaylist(poslines[0]);
        movetoline(currentline);
        movetoposition();
        line = lines[pos];
    }

    void processredos();

    void processundos();

        //------------------------------------------------------------------------------------------------
        //Syntactic coloration...
        //------------------------------------------------------------------------------------------------

    void vsplit(wstring& thestr, wstring thesplitter, vector<wstring>& vs);

        //------------------------------------------------------------------------------------------------
        //Cursor movements...
        //------------------------------------------------------------------------------------------------

    void selectfound(long l, long r);
    void movetoposition();
    void movetobeginning();
    void movetoend(bool remove = true);
    void movetolastline();
    void movetoline(long e);
    void gotoline(long p);
    virtual bool updown(char drt, long& pos);

    void getcursor();

    virtual bool reloadfile() {
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
        wstring code = wconvert(cde);
        lines.setcode(code, true);
        displayonlast("Reloaded", true);
        posinstring = 0;
        pos = 0;
        currentline = 0;
        displaylist(0);
        return true;
    }

    void toggletopbottom() {
        if (poslines.size() == 0)
            return;

        if (currentline == 0)
            currentline = poslines.size() -1;
        else
            currentline = 0;

        movetoline(currentline);
        pos = poslines[currentline];
        line = lines[pos];
        posinstring = line.size();
        movetoend();
    }

        //------------------------------------------------------------------------------------------------
        //Size calculations...
        //------------------------------------------------------------------------------------------------

        //Since there is always a prefix at the beginning of the line, we compute it here...

    virtual string prefixstring(long n) {
        return prefix;
    }

    long prefixesize(long sz) {
        if (noprefix)
            return 0;
        return (sz > 9999 ? 5 : sz > 999 ? 4: sz > 99 ? 3 : sz > 9 ? 2 : 1);
    }

    void setprefixesize(long sz) {
        if (noprefix) {
            prefixsize = 0;
            return;
        }
        prefixsize = sz > 9999 ? 5 : sz > 999 ? 4: sz > 99 ? 3 : sz > 9 ? 2 : 1;
    }

	long prefixe() {
		if (noprefix)
			return 0;
		return (2 + prefix.size() + prefixsize);
	}

    virtual long prefixego() {
        wstring s;
        switch(option) {
            case x_goto:
                s = L"Line:";
                break;
            case x_find:
                if (regularexpressionfind)
                    s = L"Find(rgx):";
                else
                    s = L"Find:";
                break;
            case x_replace:
                if (regularexpressionfind)
                    s = L"Find(rgx):";
                else
                    s = L"Find:";
                s += currentfind;
                s += L"  Replace:";
                break;
            case x_write:
                s = L"File:";
                break;
            case x_count:
                s = L"Count:";
                break;
            case x_delete:
            case x_copy:
            case x_cut:
            case x_deleting:
            case x_paste:
            case x_cutting:
            case x_load:
                s = st.str();
                break;
            default:
                return prefixe();
        }
        return s.size();
    }

    long splitline(wstring& l, long linenumber, vector<wstring>& subs);

        //The main problem here is that emojis can be composed...
        //An emoji can be composed of up to 7 emojis...
    void forwardemoji();
    void backwardemoji();

        //This size is computed to take into account Chinese/Japanese/Korean characters...
        //These characters can occupy up to two columns... We also take into account the tab size
    long taille(wstring& s);

    long cjk_size(wstring& l) {
        return (taille(l) - sizestring(l));
    }

    long sizestring(wstring& l);

    inline long fullsize(wstring& l) {
        return taille(l);
    }

    long size_upto(wstring& l, long p);
    void computeposition(int& p, long line);

    long linesize() {
        if (emode())
            return lines[poslines[currentline]].size();
        return line.size();
    }

        //--------------------------------------------------------------------------------
        //Display methods....
        //--------------------------------------------------------------------------------
    void clearscreen();
	void clearlastline();
    void displayonlast(bool bck);
    void displayonlast(wstring w, bool bck = false);
    void displayonlast(string s, bool bck);
    virtual void displaygo(bool full);

    //We detect long commented lines or long strings
    void resetlist(long i) {
        poslines.clear();
        long mx = i + row_size;

        while (i <= mx) {
            poslines.push_back(i);
            i++;
        }
    }

    void switch_darkmode() {
        if (colors[2] == m_blue) {
            //Dark Mode Colors
            colors[1] = m_dore;
            colors[2] = m_blueblack;
            colors[5] = m_dark_yellow;
        }
        else {
            colors[1] = m_ital;
            colors[2] = m_blue;
            colors[5] = m_yellow;
        }
    }

    void displaylist(long beg);
    virtual void displaylist(long beg, long end) {
        displaylist(beg);
    }

    virtual void printline(long n, string l) {
        if (noprefix)
            cout << back << l;
        else
            cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << l;
    }

    virtual void printline(long n) {
        if (noprefix)
            cout << back;
        else
            cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current;
    }

    virtual void printline(long n, wstring& l, long i) {
        if (noprefix)
            cout << back << coloringline(l, i);
        else {
            if (n == -1) {
                string space(prefixe(), ' ');
                cout << back << space << coloringline(l, i);
            }
            else
                cout << back << m_dore << prefixstring(n) << m_current << m_lightgray << std::setw(prefixsize) << n << "> " << m_current << coloringline(l, i);
        }
    }

        //------------------------------------------------------------------------------------------------
        //Deletion methods...
        //------------------------------------------------------------------------------------------------

        //The deletion of a character is different if it is an emoji...
    virtual long deleteachar(wstring& l, bool last, long pins);
    void deletechar(bool);

        //Delete all characters after the cursor
    void deleteallafter() {
        undo(lines[pos],pos, u_modif); //The value is negative to indicate a deletion

        wstring code = lines[poslines[currentline]];
        kbuffer = code.substr(posinstring, code.size());
        code = code.substr(0, posinstring);
        lines[poslines[currentline]] = code;
        lines.refactoring(poslines[currentline]);
        displaylist(poslines[0]);
        movetoline(currentline);
        movetoposition();
    }

        //moveup means that the cursor must be positionned on the line above...
    void deleteline(char moveup);

        //------------------------------------------------------------------------------------------------
        //formating method...
        //------------------------------------------------------------------------------------------------

    virtual void setcode(string& c, bool clean) {
        wstring code = wconvert(c);
        lines.setcode(code, clean);
        displaylist(0);
        line = L"";
        currentline = 0;
        movetoline(0);
        if (poslines.size()) {
            line = lines[0];
            posinstring = line.size();
            movetoend();
        }
    }

        //------------------------------------------------------------------------------------------------
        //search method...
        //------------------------------------------------------------------------------------------------

    bool search(wstring& l, long& first, long& last, long ps);
    void processgo();
    bool processfind();
    void processreplace();
    bool findnext();

    bool resetsearch();

        //------------------------------------------------------------------------------------------------
        //command methods...
        //------------------------------------------------------------------------------------------------
    virtual bool writetofile() {
        wstring code = lines.code();

        ofstream wd(thecurrentfilename, std::ios::binary);
        if (wd.fail())
            return false;
        wd << convert(code);
        wd.close();
        tobesaved = false;
        return true;
    }

    long getbuffsize();
    bool check_utf8(string& buff, string& buffer);

    virtual bool loadfile(wstring& name) {
        if (!loadfile(convert(name))) {
            clearst();
            st << L"Cannot load:" << name;
            displayonlast(true);
            return false;
        }
        return true;
    }

    virtual bool loadfile(string name) {
        setpathname(name);
        ifstream rd(pathname(), openMode);
        if (rd.fail())
            return false;

        string code = "";
        string line;
        while (!rd.eof()) {
            getline(rd, line);
            line = s_trimright(line);
            code += line + "\n";
        }
        s_trimright(code);
        code += "\n\n";
        setcode(code, true);
        noprefix = previous_noprefix;

        return true;
    }

    string convert(wstring& w) {
        string s;
        s_unicode_to_utf8(s, w);
        return s;
    }

    wstring wconvert(string& s) {
        wstring w;
        s_utf8_to_unicode(w, s, s.size());
        return w;
    }


        //A CR was hit, we need either to modify the current line or to add a new one...
        //If the cursor was in the middle of a line, then we split it in two

    long handlemultiline() ;
    long handlingeditorline(bool computespace = true);
	void Scrolldown();

    bool evaluateescape(string& buff);
    virtual void init();
    virtual void clear();


    //This is the main instruction to add characters into the editor
    void addabuffer(wstring& b, bool instring);
    void cleanheaders(wstring& w);

    void handleblock(wstring& bl);

    //This section handles combined commands introduced with Ctrl-x
    virtual bool checkcommand(char);
    void handlecommands();
    virtual void stop_execution() {}

    //This a case of copy/paste within the editor, we need to remove the prefixes
    //This is the main method that launches the terminal
    virtual void launchterminal(bool darkmode, char loadedcode, vector<string>& args, vector<string>& newcolors);
    bool checkaction(string&, long& first, long& last, bool lsp = false);

    virtual void addcommandline(wstring& w) {}

    virtual bool terminate();

    virtual void resetscreen() {
        modified = true;
        screensizes();
        wstring code = lines.code();
        lines.setcode(code, false);
        posinstring = 0;
        if (lines.size()) {
            displaylist(poslines[0]);
            movetoline(currentline);
            movetobeginning();
        }
    }

    bool checksize(wstring& l) {
        if (fullsize(l) > col_size)
            return true;
        return false;
    }

    bool checksizeequal(wstring& l, bool& equal) {
        long ll = fullsize(l);
        if (ll == col_size) {
            equal = true;
            return true;
        }

        if (ll > col_size)
            return true;
        return false;
    }

    string the_code() {
        wstring c = lines.code();
        return convert(c);
    }

    wstring the_wcode() {
        return lines.code();
    }

};

#endif

