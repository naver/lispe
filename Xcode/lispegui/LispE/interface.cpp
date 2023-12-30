/*
 *  LispE
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : LispE
 Version    : See lispe.cxx for the version number
 filename   : interface.cpp
 Date       : 2017/09/01
 Purpose    : Functions and Methods to communicate with LispE API
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#include <stdio.h>


#include "lispe.h"
#include "tools.h"
#include <set>

#define openMode std::ios::in|std::ios::binary

//------------------------------------------------------------------------------------------------UTF8
static UTF8_Handler special_characters;
//------------------------------------------------------------------------------------------------DEBUG
static string displaybuffer;
static string current_code;
static bool windowmode = false;
static bool running = false;

//The following functions are called from within the GUI to handle debugging
static LispE* lispe = NULL;
extern "C" {
const char* Inputtext(const char* msg);
void Rappel(char threading, const char* txt);
void Initlispelibspath();
void displaydebug(const char* locals, const char* globals, const char* sstack, const char* filename, long currentline);
}
//------------------------------------------------------------------------------------------------RUN AND COMPILE
//In the case of a mac GUI, we need to call a specific version of kget, which is based on alert...
//This function is only called if we are not implementing a FLTK GUI, in that case we use ProcEditor
void ProcMacEditor(string& code, void* o) {
    string label = code;
    if (label == "")
        label = "Enter your text:";
    const char* buff = Inputtext(STR(label));
    if (buff == NULL) {
        code = "";
        return;
    }
    
    code=buff;
}

void sendresult(string& code, void*) {
    displaybuffer = code;
    Rappel(0, displaybuffer.c_str());
}

static std::map<string,std::set<long> > breakpoints;
static bool debugmode = false;
static bool fullstring = true;
static string current_path_name;
static long last_line = -1;
static long last_file = -1;


string display_variables(LispE* lisp, Element* instructions, bool full) {
    vector<Element*> atomes;
    if (instructions == NULL)
        lisp->atomsOnStack(atomes);
    else
        lisp->extractAllAtoms(instructions, atomes);
    
    Element* value;
    
    std::map<string, Element*> uniques;
    for (auto& a: atomes) {
        if (a->label() == v_true)
            continue;
        uniques[a->toString(lisp)] = a;
    }
    string localvariables;
    string thevalue;
    Element* e;
    for (auto& a: uniques) {
        e = a.second;
        value = lisp->getvalue(e->label());
        if (value != NULL && !value->isFunction()) {
            thevalue = e->toString(lisp);
            thevalue += ":";
            thevalue +=  value->toString(lisp);
            if (!full && thevalue.size() > 80) {
                thevalue = thevalue.substr(0,80);
                thevalue += "...";
            }
            localvariables += thevalue + "\n";
        }
    }
    return localvariables;
}

bool debug_function_lispe(LispE* lisp, List* instructions, void* o) {
    static string locals;
    static string globals;
    static string name;
    static string stack;
    
    if (last_line == lisp->delegation->i_current_line && last_file == lisp->delegation->i_current_file)
        return false;
    
    last_line = lisp->delegation->i_current_line;
    last_file = lisp->delegation->i_current_file;
    
    locals = display_variables(lisp, instructions, fullstring);
    globals = display_variables(lisp, NULL, fullstring);
    name = lisp->name_file(lisp->delegation->i_current_file);
    stack = lisp->stackImage();
    
    displaydebug(locals.c_str(), globals.c_str(), stack.c_str(), name.c_str(), last_line);
    lisp->blocking_trace_lock();
    return true;
}

extern "C" {

char IsRunning(void) {
    return running;
}

void CleanGlobal() {
    if (lispe != NULL)
        delete lispe;
    current_path_name = "";
    debugmode = false;
    lispe = NULL;
    windowmode = false;
}

int Compilecode(const char* cde, const char* filename, char console) {
    Initlispelibspath();
    if (lispe == NULL) {
        lispe = new LispE(&special_characters);
        lispe->delegation->display_string_function = sendresult;
        lispe->delegation->reading_string_function = &ProcMacEditor;
    }
    current_path_name = filename;
    lispe->set_pathname(current_path_name);
    current_code = cde;
    lispe->setwindowmode(&windowmode);
    lispe->precompile(current_path_name);
    return 0;
}

char NextLine(void) {
    lispe->stop_at_next_line(debug_next);
    lispe->releasing_trace_lock();
    return 1;
}

char Gotonextbreak(void) {
    //We reset the current line to allow for a loop
    //on the same breakpoint
    last_line = -1;
    lispe->stop_at_next_line(debug_goto);
    lispe->releasing_trace_lock();
    return 1;
}

char Getin(void) {
    lispe->stop_at_next_line(debug_inside_function);
    lispe->releasing_trace_lock();
    return 1;
}

char Getout(void) {
    lispe->stop_at_next_line(debug_none);
    lispe->releasing_trace_lock();
    return 1;
}

char Gotoend(void) {
    lispe->stop_trace();
    lispe->releasing_trace_lock();
    return 1;
}

void Setdebugmode(char d) {
    debugmode = d;
}

void addabreakpoint(const char* filename, long numline, char add) {
    string fn = filename;
    if (add)
        breakpoints[fn].insert(numline);
    else {
        if (breakpoints.count(fn) && breakpoints[fn].count(numline))
            breakpoints[fn].erase(numline);
    }
}

void Shortname(char v) {
    fullstring = v;
}

char StopDebug(void) {
    debugmode = false;
    lispe->stop();
    lispe->stop_trace();
    return 1;
}

char WindowModeActivated(void) {
    return windowmode;
}

void Blocked(void) {
    lispe->blocking_trace_lock();
}

void Released(void) {
    lispe->releasing_trace_lock();
}

long CurrentLine(void) {
    if (lispe == NULL)
        return -1;
    return lispe->delegation->i_current_line;
}

const char* Currentfilename(void) {
    static string filename;
    filename=lispe->current_name_file();
    return filename.c_str();
}

const char* Getcurrentfilename(void) {
    static string filename;
    filename=lispe->current_name_file();
    return filename.c_str();
}

void setlispepath(const char* homepath, const char* v) {
    char* start=(char*)v;
    
    while (start[0]<=32) start++;
    long lg=strlen(start)-1;
    while (start[lg]<=32)lg--;
    start[lg+1]=0;
    
    setenv("LISPEPATH",start,1);
    char path[4096];
    sprintf_s(path,4096,"%s/.lispe",homepath);
    std::ofstream savepath(path);
    savepath.write(start,strlen(start));
    savepath.write("\n",1);
    savepath.close();
}

void initlispepath(const char* homepath) {
    char path[4096];
    sprintf_s(path,4096, "%s/.lispe",homepath);
    std::ifstream getpath(path);
    if (getpath.fail()) {
        getpath.close();
        std::ofstream savepath(path);
        strcpy(path,"/usr/local/lib/lispe");
        savepath.write(path,strlen(path));
        savepath.write("\n",1);
        savepath.close();
        setenv("LISPEPATH",path,1);
        return;
    }
    memset(path,0,4096);
    getpath.read(path,4096);
    char* start=path;
    
    while (start[0]<=32) start++;
    long lg=strlen(start)-1;
    while (start[lg]<=32)lg--;
    start[lg+1]=0;
    setenv("LISPEPATH",start,1);
}

const char* Listing(void) {
    displaybuffer=current_code;
    return STR(displaybuffer);
}

const char* Readfile(const char* path) {
    std::ifstream f(path, openMode);
    if (f.fail())
        return NULL;
    current_code="";
    string line;
    while (!f.eof()) {
        getline(f,line);
        current_code+=line;
        current_code += "\n";
    }
    displaybuffer = current_code;
    return STR(displaybuffer);
}

void StopExecution(void) {
    if (lispe != NULL)
        lispe->stop();
}

void InitialisationDisplay(short id, char th) {
    static bool threading;
    
    threading=th;
    displaybuffer="";
}

void Initdisplay(short id) {
    displaybuffer="";
}

const char* Getdisplay(void) {
    static string buff;
    
    buff=displaybuffer;
    buff+="\r";
    displaybuffer="";
    return STR(buff);
}

char Run(short idcode) {
    if (running)
        return false;
    
    running = true;
    
    if (lispe == NULL) {
        lispe = new LispE(&special_characters);
        lispe->delegation->display_string_function = sendresult;
        lispe->delegation->reading_string_function = &ProcMacEditor;
        lispe->setwindowmode(&windowmode);
    }
    bool eval_code = false;
    if (current_code.find("(") == -1 && current_code.find(")") == -1) {
        current_code = "(print "+ s_trim(current_code) + ")";
        eval_code = true;
    }
    
    if (debugmode) {
        last_line = -1;
        last_file = -1;
        if (breakpoints.size()) {
            lispe->delegation->breakpoints.clear();
            long idfile;
            string pathname;
            for (auto& a: breakpoints) {
                pathname = NormalizePathname(a.first);
                idfile = lispe->id_file(pathname);
                for (auto& e: a.second)
                    lispe->delegation->breakpoints[idfile][e] = true;
            }
            lispe->stop_at_next_line(debug_goto);
        }
        else
            lispe->stop_at_next_line(debug_next);
        lispe->set_debug_function(debug_function_lispe, NULL);
    }
    
    Element* res;
    try {
        if (eval_code)
            res = lispe->eval(current_code);
        else
            res = lispe->execute(current_code, current_path_name);
        displaybuffer = "\n";
        displaybuffer += res->toString(lispe);
        displaybuffer += "\n";
        res->release();
    }
    catch (Error* x) {
        displaybuffer = x->toString(lispe);
        delete x;
    }
    catch (void* x) {
        displaybuffer = "\nUnknown Error...\n";
    }
    Rappel(0, displaybuffer.c_str());
    running = false;
    return true;
}

void LispEFinalClean(void) {
    if (lispe != NULL)
        delete lispe;
    current_path_name = "";
    breakpoints.clear();
    debugmode = false;
    lispe = NULL;
    windowmode = false;
    running = false;
}

void clearinternalbreakpoints(void) {
    breakpoints.clear();
}

long indentationVirtuel(char* cr, char* acc) {
    if (cr == NULL)
        return 0;
    
    string codestr(cr);
    return VirtualIndentation(codestr, true, false);
}

const char* lindentation(char* basecode, int blancs) {
    static string codeindente;
    
    string codestr = basecode;
    s_trimright(codestr);
    codestr+="\n";
    bool lisp = false;
    if (codestr[0] == '(' && codestr[1] == ')')
        lisp = true;
    
    cr_normalise(codestr);
    codeindente = "";
    IndentCode(codestr, codeindente, GetBlankSize(), true, false);
    codeindente += "\n";
    return codeindente.c_str();
}

class coloring_automaton : public tokenizer_automaton {
public:
    
    coloring_automaton(UTF8_Handler* a) : tokenizer_automaton(a) {}
    
    void setrules() {
        rules.push_back(U"%S+=#");                                  //we skip all spaces
        
        rules.push_back(U"';+=#");
        rules.push_back(U"'=39");

        rules.push_back(U";;?*;;=:67");
        rules.push_back(U";?*%r=67");
        rules.push_back(U"%#!?+%r=67");                      //specific to Linux, calling function integrated in file
        
        //Strings
        rules.push_back(U"\"\"\"?*\"\"\"=:34");
        rules.push_back(U"\"{[\\-\"] ~%r}*\"=:34");     //string "" does not contain CR and can escape characters
        rules.push_back(U"`?*`=:34");                   //long strings Unix way
        rules.push_back(U"«?*»=:34");                   //long strings French way
        rules.push_back(U"“?*”=:34");                   //long strings English
        rules.push_back(U"‘?*’=:34");                   //long strings with single quotes (English)
        rules.push_back(U"„?*”=:34");                //long strings German/Polish
        rules.push_back(U"❝?*❞=:34");                   //long strings
        
        rules.push_back(U"%a+=65");       //Regular strings
        rules.push_back(U"{%a %h %H}{%a %d %h %H %o}+=#");       //All other types of strings
        rules.push_back(U"%d+=#");
        rules.push_back(U"%p=#");
        rules.push_back(U"%o=#");
    }
    
};


long* colorparser(const uint16_t* txt, long from, long sz) {
    static vector<long> limits;
    static tokenizer_result<u16string> tokres;
    static coloring_automaton lsp_tok(&special_characters);
    
    limits.clear();
    
    if (lispe == NULL) {
        lispe = new LispE(&special_characters);
        lispe->delegation->display_string_function = sendresult;
        lispe->delegation->reading_string_function = &ProcMacEditor;
        windowmode = false;
    }
    
    u16string w((char16_t*)txt);
    lsp_tok.tokenize<u16string>(w, tokres);

#ifdef MACDEBUG
    //for debugging
    vector<u16string> codes;
    tokres.stack.to_vector(codes);
    vector<int16_t> icodes;
    tokres.stacktype.to_vector(icodes);
#endif

    long left = 0, right = 0;
    wstring sub;
    short type;
    char quote = 8;
    for (long isegment = 0; isegment < tokres.size(); isegment++) {
        left =  tokres.positions[isegment<<1];
        if (left < from)
            continue;
        type = tokres.stacktype[isegment];
        right = tokres.positions[1+(isegment<<1)] - left;
        
        //sub = line.substr(left, right);
        //left += drift;
        switch (type) {
            case '"':
                limits.push_back(1);
                limits.push_back(left);
                limits.push_back(right);
                quote = 8;
                break;
            case '\'':
                limits.push_back(2);
                limits.push_back(left);
                limits.push_back(right);
                quote = 2;
                break;
            case 'A':
                if (lispe != NULL && lispe->delegation->is_basic_atom(tokres.stack[isegment]) != -1 && quote == 8)
                    limits.push_back(5);
                else {
                    limits.push_back(quote);
                    quote = 8;
                }
                limits.push_back(left);
                limits.push_back(right);
                break;
            case 'C': //comments
                limits.push_back(7);
                limits.push_back(left);
                limits.push_back(right);
                quote = 8;
                break;
            default:
                limits.push_back(quote);
                limits.push_back(left);
                limits.push_back(right);
                quote = 8;
                break;
        }
    }
    
    long* res=new long[limits.size()+1];
    for (long i=0;i<limits.size();i++) {
        res[i]=limits[i];
    }
    res[limits.size()]=-1;
    return res;
}

void deletion(long* l) {
    delete[] l;
}

long computeparenthesis(const char* w, char checkcar, long limit) {
    wstring ln;
    s_utf8_to_unicode_u(ln, (unsigned char*)w, strlen((char*)w));
    ln = ln.substr(0, limit+1);
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
}

