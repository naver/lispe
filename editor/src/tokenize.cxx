/*
 *  JAG
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  tokenize.cxx
// Rules to tokenize and display colors

#ifdef WIN32
#include "Windows.h"
#endif

#include <stdio.h>
#include <set>
#include "tools.h"

#ifdef WIN32
#include <io.h>
#else
#include <dlfcn.h>
#endif

#ifndef WIN32
#include <unistd.h>   //_getch
#include <termios.h>  //_getch
#include <sys/ioctl.h>
#endif

#include <signal.h>
#include <iomanip>

#include<set>

#include "emojis.h"
#include "jag.h"
#include "jagrgx.h"
#include "tokenize.h"

#include <algorithm>

/*
 Some utility functions to manipulate strings
 Essentially:

 numeric conversions (including hexadecimal)
 utf8/unicode conversion
 utf8 string traversal (taking into account multi-byte characters)
 code indentation (no more only lisp by the way)
 upper case, lower case
 string trimming
 sub-string extraction: left, right, middle
 */

#define strcpy_s(a,c,b) strncpy(a,b,c)
#define strcat_s(a,c,b) strncat(a,b,c)
bool evaluate_quotes(wstring& l);

const char* _keywords[] = { "!","!=","#checking","#compose","#folding","#mapping","%","%=","&","&=",
    "*","*=","+","+=","-","-=","/","/=","<","<<",
    "<<=","<=","=",">",">=",">>",">>=","False","GPSdistance","Maybe",
    "None","Nothing","True","^","^=","^^","^^=","_breakpoint","_dependencies","_erroronkey",
    "_eval","_evalfunction","_exit","_forcelocks","_getdefaulttokenizerules","_info","_initial","_mirrordisplay","_nbthreads","_poolstats",
    "_setenv","_setmaxrange","_setmaxthreads","_setstacksize","_setvalidfeatures","_stdin","_symbols","_threadhandle","_threadid","a_bool",
    "a_change","a_delete","a_features","a_first","a_float","a_fvector","a_insert","a_int","a_ivector","a_longest",
    "a_mapff","a_mapfi","a_mapfs","a_mapfu","a_mapif","a_mapii","a_mapis","a_mapiu","a_mapsf","a_mapsi",
    "a_mapss","a_mapuf","a_mapui","a_mapuu","a_nocase","a_offsets","a_random","a_repetition","a_string","a_surface",
    "a_svector","a_switch","a_ustring","a_uvector","a_vector","a_vowel","abs","absent","acos","acosh",
    "activate","add","addchild","addnext","addprevious","addstyle","after","alert","align","all",
    "allobjects","and","annotate","annotator","any","aopen","append","apply","arc","asin",
    "asinh","ask","assert","asserta","assertz","at","atan","atanh","atom","atomp",
    "atoms","attributes","autorun","backgroundcolor","barcolor","base","before","begin","begincomplexpolygon","beginline",
    "beginloop","beginpoints","beginpolygon","binmap","binmapf","binmapi","binmapl","binmaps","binmapu","bit",
    "bitmap","bits","block","blocking","bloop","bodies","bool","border","boundaries","bounds",
    "box","boxtype","break","browser","buffersize","build","button","bvector","byte","byteposition",
    "bytes","cadr","call","car","case","cast","catch","cbrt","cdr","cell", "cout", "cin", "cerr", "void",
    "cellalign","cellalignheadercol","cellalignheaderrow","char","charposition","check","checklabel","checkword","child","children",
    "choice","chr","circle","clear","close","color","colorbg","colorfg","column","columnchar",
    "columnheader","columnheaderwidth","columnwidth","command","commit","common","compact","compile","compilelexicons","compilergx",
    "concept","cond","connect","cons","consp","const","constmap","constvector","content","continue",
    "copy","cos","cosh","count","counter","create","created","createdirectory","createserver","curl",
    "current","cursor","cursorchar","cursorstyle","curve","cut","cycle","data","date","day",
    "deaccentuate","decimal","decode","def","default","define","definitions","deflib","defmacro","defpat",
    "defun","degree","delete","dependencies","dependency","deriving","deselect","determinant","dethread","dimension",
    "dloop","do","document","dos","dostoutf8","double","doublemetaphone","drawcolor","drawtext","drop",
    "dropWhile","dropwhile","dthrough","dvector","e_arabic","e_baltic","e_celtic","e_cp1252","e_cyrillic","e_greek",
    "e_hebrew","e_latin_ce","e_latin_ffe","e_latin_ne","e_latin_se","e_latin_see","e_latin_we","e_nordic","e_thai","e_turkish",
    "e_windows","editdistance","editor","elif","else","emoji","emojis","empty","encode","end",
    "endcomplexpolygon","endian","endif","endline","endloop","endpoints","endpolygon","env","eof","eq",
    "erf","erfc","eval","evaluate","even","exclusive","execute","exit","exp","exp2",
    "explode","expm1","exponent","extension","extract","factorize","factors","fail","false","fappend",
    "features","fibre","file","filebrowser","fileinfo","fill","filter","find","findall","first",
    "flatten","flip","float","floop","floor","flush","fmatrix","focus","foldl","foldl1",
    "foldr","foldr1","font","fontnumber","fontsize","for","format","formatchar","fraction","frame",
    "frameinstance","fread","from","fthrough","function","fvector","fwrite","gap","get","getchar",
    "geterr","getfont","getfontsizes","gethostname","getpeername","getstd","getstyle","gotoline","grammar","grammar_macros",
    "group","hash","hide","highlight","hour","html","hvector","id","if","ifdef","cp", "sudo", "chmod", "rm", "ls", "ll",
    "ife","ifnot","iloop","image","imatrix","import","in","include","indent","info",
    "initializefonts","input","insert","insertbind","int","integer","invert","irange","is","isa",
    "isalpha","isconsonant","iscontainer","isdigit","isdirectory","isemoji","ishangul","isjamo","islower","ismap",
    "isnumber","isprime","ispunctuation","isstring","isupper","isutf8","isvector","isvowel","items","iterator",
    "ithrough","ivector","jamo","join","joined","json","key","keyn","keys","kget",
    "label","labelcolor","labelfont","labels","labelsize","labeltype","lambda","last","latin","leaves",
    "left","length","let","levenshtein","lexicon","lgamma","line","linebounds","linecharbounds","lineshape",
    "link","lisp","list","listdirectory","lloop","ln","load","loadgif","loadin","loadjpeg",
    "lock","log","log1p","log2","logb","long","lookdown","lookup","loop","loopcount",
    "lower","ls","lstep","lthrough","lvector","mantissa","map","mapf","mapff","mapfi",
    "mapfl","mapfs","mapfu","mapi","mapif","mapii","mapis","mapiu","mapl","maplf",
    "mapll","mapls","maplu","mapsf","mapsi","mapsl","mapss","mapu","mapuf","mapui",
    "mapul","mapuu","mark","match","max","maximum","maybe","memory","menu","merge",
    "method","methods","mid","min","minimum","minute","mkdir","modal","month","mp3",
    "mthrough","multisplit","multmatrix","name","namespace","nbchildren","ncheck","nconc","nd","nearbyint",
    "neq","new","next","ngrams","node","nope","normalizehangul","not","notin","null", "NULL",
    "nullp","number","numberp","odd","of","ok","onclose","onclosing","ondragdrop","onhscroll",
    "onkey","onmouse","ontime","ontology","onvscroll","open","openappend","openread","openwrite","options",
    "or","ord","otherwise","padding","parameters","parent","parse","password","paste","pause",
    "permute","pie","pipe","play","plot","plotcoords","point","polygon","polynomial","ponder",
    "pop","popclip","popfirst","poplast","popmatrix","port","position","post","precede","pred",
    "predicate","predicatedump","predicatevar","preg","prettify","previous","primemap","primemapf","primemapff","primemapfi",
    "primemapfl","primemapfs","primemapfu","primemapi","primemapif","primemapii","primemapis","primemapiu","primemapl","primemaplf",
    "primemapll","primemapls","primemaplu","primemapsf","primemapsi","primemapsl","primemapss","primemapu","primemapuf","primemapui",
    "primemapul","primemapuu","print","printerr","printerrln","printj","printjerr","printjln","printjlnerr","println",
    "printlnerr","private","product","progress","properties","property","protected","proxy","push","pushclip",
    "pushfirst","pushlast","pushmatrix","quote","radian","raise","random","range","rawstring","read",
    "readline","real","realpath","receive","rectangle","rectanglefill","redirectoutput","redraw","redrawing","remove",
    "removefirst","removelast","repeat","replace","replaceall","replicate","reserve","reset","resetmark","resizable",
    "resize","restateoutput","retract","retractall","return","reverse","rfind","rgbcolor","right","ring",
    "rint","role","romanization","root","ropen","rotate","rotation","round","row","rowheader",
    "rowheaderheight","rowheight","rule","run","save","scale","scan","scanl","scanl1","scanr",
    "scanr1","scrollbar","second","seek","select","selection","selectioncolor","self","send","serialize",
    "serializestring","setdate","setg","setq","setstyle","settimeout","short","shortcut","show","shuffle",
    "sibling","signature","simplify","sin","sinh","sisters","size","sizeb","sizerange","sleep",
    "slice","slider","sloop","socket","sort","sortfloat","sortint","sortstring","sortustring","sound",
    "spans","split","splite","sqlite","sqrt","static","step","steps","sthrough","stokenize",
    "stop","store","strict","string","stringp","succ","succeed","sum","svector","switch",
    "synode","sys","table","tabs","tags","take","takeWhile","takewhile","tamgu","tan",
    "tanh","tell","term","test","textcolor","textsize","tgamma","this","thread","threadclear",
    "threadretrieve","threadstore","throw","time","tohtml","token","tokenize","tokens","tooltip","toxml",
    "trace","transducer","transformdx","transformdy","transformedvertex","transformx","transformy","translate","transpose","treemap",
    "treemapf","treemapff","treemapfi","treemapfl","treemapfs","treemapfu","treemapi","treemapif","treemapii","treemapis",
    "treemapiu","treemapl","treemaplf","treemapll","treemapls","treemaplu","treemapsf","treemapsi","treemapsl","treemapss",
    "treemapu","treemapuf","treemapui","treemapul","treemapuu","treg","trigger","trim","trimleft","trimright",
    "true","trunc","try","type","ufile","uloop","unblock","unget","unhighlight","unique",
    "unlink","unlock","unmark","upper","url","use","ustring","utf8","uthrough","uvector",
    "value","values","vector","version","vertex","vthrough","wait","waiton","waitonfalse","waitonjoined",
    "weekday","wfile","when","where","while","window","winput","with","wopen","word",
    "words","woutput","wrap","write","writebin","writeln","writeutf16","wstring","wtable","xml",
    "xmldoc","xmlstring","xmltype","xor","xpath","year","yearday","zerop","zip","zipWith",
    "zipwith","|","|=","∏","∑","√","∛", "" };

static bool is_keyword(string s) {
    static std::unordered_map<string, bool> words;
    bool initial = true;
    if (initial) {
        for (int i = 0; _keywords[i][0] != 0; i++)
        words[_keywords[i]]=true;
        initial = false;
    }
    if (words.find(s) != words.end())
        return words[s];
    return false;
}

void tokenize_line(string& code, Segmentingtype& infos, file_types filetype) {
    long idx;

    long sz = code.size();
    long i, current_i;
    //The first line of the code is 1
    long line_number = 1;
    long last_i = 0;
    UWCHAR c, nxt;
    uchar lc;
    string current_line;
    string tampon;
    bool point = false;
    infos.clear();
    short string_end;

    for (i = 0; i < sz; i++) {
        current_i = i;
        c = getonechar(USTR(code), i);
        string_end = 187;
        switch (c) {
            case 27: {//if it is an escape character
                //We might have a color definition
                idx = i + 1;
                if (code[idx] == '[') {
                    //we are looking for the final 'm'
                    while (idx < sz && code[idx] != 'm')
                        idx++;

                    if (idx != sz)
                        i = idx;
                }
                break;
            }
            case '.':
                point = true;
                break;
            case '@':
                point = false;
                if (filetype == tamgu_type) {
                    idx = i + 1;
                    if (code[idx] == '"') {
                        idx++;
                        while (idx < sz && code[idx]  != '\n') {
                            if (code[idx] == '"' && code[idx + 1] == '@')
                                break;
                            idx++;
                        }
                        if (idx < sz && code[idx] != '\n') {
                            infos.append(jt_string, i, idx + 1);
                            i = idx;
                        }
                        else {
                            infos.append(jt_string, i, idx);
                            if (code[idx] == '\n') {
                                i = idx;
                                line_number++;
                                last_i = i + 2;
                            }
                            i = idx;
                        }
                    }
                    else {
                        if (filetype == c_like_type && code[idx] == '/') {
                            idx++;
                            infos.append(jt_finalcomment, last_i, idx);
                            i = idx;
                        }
                    }
                }
                break;
            case '/':
                point = false;
                if (filetype == c_like_type) {
                    idx = i + 1;
                    if (code[idx] == '/' || code[idx] == '*') {
                        idx++;
                        while (idx < sz && code[idx] != '\n') idx++;
                        infos.append(jt_comment, i, idx);
                        i = idx;
                        line_number++;
                        last_i = i + 2;
                    }
                }
                else {
                    if (filetype == tamgu_type) {
                        idx = i + 1;
                        if (code[idx] == '/' || code[idx] == '@') {
                            idx++;
                            while (idx < sz && code[idx] != '\n') idx++;
                            infos.append(jt_comment, i, idx);
                            i = idx;
                            line_number++;
                            last_i = i + 2;
                        }
                    }
                }
                break;
            case '*':
                point = false;
                if (filetype == c_like_type) {
                    idx = i + 1;
                    if (code[idx] == '/') {
                        idx++;
                        infos.append(jt_finalcomment, last_i, idx);
                        i = idx;
                    }
                }
                break;
            case ';':
                point = false;
                if (filetype == lisp_type) {
                    idx = i;
                    while (idx < sz && code[idx] != '\n') idx++;
                    infos.append(jt_comment, i, idx);
                    i = idx;
                    line_number++;
                    last_i = i + 2;
                }
                break;
            case '#': //comments (we accept both with ; and #)
                point = false;
                if (filetype == python_type || filetype == no_type) {
                    idx = i;
                    while (idx < sz && code[idx] != '\n') idx++;
                    infos.append(jt_comment, i, idx);
                    i = idx;
                    line_number++;
                    last_i = i + 2;
                }
                break;
            case '\n':
                point = false;
                line_number++;
                last_i = i + 2;
                break;
            case '\t':
            case '\r':
            case ' ':
                point = false;
                continue;
            case '\'': { // a string containing what we want...
                point = false;
                idx = i + 1;
                tampon = "";
                while (idx < sz && code[idx] != '\'') {
                    c = (uchar)code[idx];
                    if (c == '\\') {
                        idx++;
                        switch (code[idx]) {
                            case 'n':
                                tampon += '\n';
                                idx++;
                                continue;
                            case 'r':
                                tampon += '\r';
                                idx++;
                                continue;
                            case 't':
                                tampon += '\t';
                                idx++;
                                continue;

                        }
                    }
                    tampon += code[idx];
                    idx++;
                }

                if (idx == sz) {
                    continue;
                }

                if (tampon == "")
                    infos.append(jt_emptystring, i, idx);
                else
                    infos.append(jt_string, i, idx);
                i = idx;
                break;            }
            case '`':
                string_end = '`';
            case 171: { // a string containing what we want...
                point = false;
                if (filetype == lisp_type) {
                    idx = i + 1;
                    tampon = "";
                    while (idx < sz && code[idx] != string_end) {
                        idx++;
                    }
                    tampon = code.substr(i+1, idx-i-1);
                    if (tampon == "")
                        infos.append(jt_emptystring, i, idx);
                    else
                        infos.append(jt_string, i, idx);
                    i = idx;
                }
                break;
            }
            case '"': {
                point = false;
                idx = i + 1;
                if (filetype == tamgu_type) {
                    if (code[idx] == '@') {
                        idx++;
                        infos.append(jt_longstring, last_i, idx);
                        i = idx;
                        break;
                    }
                }

                tampon = "";
                while (idx < sz && code[idx] != '"') {
                    c = (uchar)code[idx];

                    if (c == '\\') {
                        idx++;
                        switch (code[idx]) {
                            case 'n':
                                tampon += '\n';
                                idx++;
                                continue;
                            case 'r':
                                tampon += '\r';
                                idx++;
                                continue;
                            case 't':
                                tampon += '\t';
                                idx++;
                                continue;
                        }
                    }
                    tampon += code[idx];
                    idx++;
                }
                if (tampon == "")
                    infos.append(jt_emptystring, i, idx);
                else
                    infos.append(jt_string, i, idx);
                i = idx;
                break;
            }
            case '+':
            case '-':
                if (!isdigit(code[i+1])) {
                    point = false;
                    if (c == '-' && code[i+1] == '>') {
                        point = true;
                        i++;
                    }
                    break;
                }
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
                point = false;
                idx = 0;
                convertingfloathexa(code.c_str() + i, idx);
                infos.append(jt_number, i, i + idx);
                i += idx - 1;
                break;
            }
            default: {
                //If the character is a multi-byte character, we need
                //to position on the beginning of the character again
                if (special_characters.c_is_punctuation(c)) {
                    point = false;
                    break;
                }

                idx = i + 1;
                nxt = c;
                while (idx <= sz && nxt > 32 && !special_characters.c_is_punctuation(nxt) && !isspace(nxt)) {
                    i = idx;
                    nxt = getonechar(USTR(code), idx);
                    idx++;
                }

                if ((i - current_i) <= 1) {
                    tampon = c;
                }
                else
                    tampon = code.substr(current_i, i - current_i);

                lc = tampon[0];

                if (point)
                    infos.append(jt_method, current_i, i);
                else {
                    if (is_keyword(tampon))
                        infos.append(jt_keyword, current_i, i);
                }

                if (i != current_i)
                    i--;
                point = false;
                break;
            }
        }
    }
}

string coloring_line(editor_lines& lines, long currentline,
                     string& line, vector<string>& colors,
                     file_types filetype) {
    static Segmentingtype segments;
    string sub = line;
    s_trim(sub);

    if (sub == "")
        return line;

    string root = "";

    long pos = currentline;


    pos = currentline;

    if (pos >= 0 && pos < lines.size()) {
        char long_line = lines.longlines[pos];
        if (long_line == 1) {
            line = colors[0] + line + m_current;
            return line;
        }
        else {
            if (long_line == 2) {
                line = colors[4] + line + m_current;
                return line;
            }
        }
    }

    if (filetype == python_type || filetype == no_type) {
        if (sub[0] == '#') {
            line = colors[4] + line + m_current;
            return line;
        }
    }
    else {
        if (filetype == lisp_type) {
            if (sub[0] == '#' || sub[0] == ';') {
                line = colors[4] + line + m_current;
                return line;
            }
        }
        else {
            if (sub[0] == '/' && sub[1] == '/') {
                line = colors[4] + line + m_current;
                return line;
            }
            if (sub[0] == '#' && (sub[1] == 'e' || sub[1] == 'i')) {
                line = colors[2] + line + m_current;
                return line;
            }
        }
    }

    string substring;

    char add = false;

    tokenize_line(line, segments, filetype);

    long left, right = -1;
    for (long isegment = segments.types.size() - 1, ipos = segments.positions.size() -1; ipos >= 0; ipos-=2, isegment--) {
        left = segments.positions[ipos-1];
        right = segments.positions[ipos];
        sub = line.substr(0, left);
        add = false;
        switch (segments.types[isegment]) {
            case jt_emptystring:
            case jt_string:
                right += 1;
                sub += colors[0];
                add = true;
                break;
            case jt_number: //methods
                sub += colors[1];
                add = true;
                break;
            case jt_keyword: //methods
                sub += colors[2];
                add = true;
                break;
            case jt_method: //methods
                sub += colors[3];
                add = true;
                break;
            case jt_comment:
                sub += colors[4];
                if (right > left)
                    sub += line.substr(left, right-left);
                sub += m_current;
                line = sub;
                break;
            case jt_finalcomment:
                sub += colors[4];
                if (right > left)
                    sub += line.substr(left, right-left);
                sub += m_current;
                if (right < line.size())
                    sub += line.substr(right, line.size() - right);
                line = sub;
                add = 2;
                break;
            case jt_longstring:
                sub += colors[0];
                if (right > left)
                    sub += line.substr(left, right-left);
                sub += m_current;
                if (right < line.size())
                    sub += line.substr(right, line.size() - right);
                line = sub;
                add = 2;
                break;
            default:
                add = false;
        }

        if (add) {
            if (add == 2)
                break;

            if (right > left)
                sub += line.substr(left, right-left);
            sub += m_current;
            if (right < line.size())
                sub += line.substr(right, line.size() - right);
            line = sub;
        }

    }

    if (root != "")
        line = root + line;
    return line;
}
//---------------------------------------------------------------------------
