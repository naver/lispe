/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  systeme.cxx
//
//


/*
 This is an example of how to extend LispE with new features.
 This 'system' extension adds system functionality to the language.
*/

#include <stdio.h>
#include <sys/stat.h>
#include <stdlib.h>

#include <signal.h>
#include <iomanip>

#ifdef WIN32
#define PATH_MAX 4096
#else
#include <unistd.h>
#endif

#include <ctime>
#include <chrono>

#include "lispe.h"
#include "directorylisting.h"

typedef enum {sys_command, sys_ls, sys_setenv, sys_getenv, sys_isdirectory, sys_fileinfo, sys_realpath} systeme;
typedef enum {file_open, file_close, file_eof, file_read, file_readline, file_readlist, file_getchar, file_write, file_writeln, file_seek, file_tell, file_getstruct} file_command;
typedef enum {date_setdate, date_year, date_month, date_day, date_hour, date_minute, date_second, date_yearday, date_raw, date_weekday } tempus;

/*
First of all we create a new Element derivation
We then implement the "eval" and "asString" methods.
The example we provide here, allows to execute a Unix command
and return the results in the form of a list.
This class is used to implement both setenv, getenv and command
at the creation of the object, we provide its behavior via a system value
 */

using std::ifstream;
using std::ofstream;

class Stream : public Element {
public:
    string pathname;
    std::fstream myfile;
    file_command mode;
    bool signature;
    bool open;
    
    Stream(int16_t idstream) : Element(idstream) {
        mode = file_open;
        signature = false;
        open = false;
    }
    
    ~Stream() {
        if (open)
            myfile.close();
    }
    
    void readsignature() {
        uchar c = myfile.get();        
        if (c == 239) {
            c = myfile.get();
            if (c == 187) {
                c = myfile.get();
                if (c == 191) {
                    signature = true;
                    return;
                }
                myfile.unget();
            }
            myfile.unget();
        }
        myfile.unget();
    }
    
    Element* open_read(LispE* lisp, string path) {
        mode = file_read;
        pathname = path;
        myfile.open(STR(path), std::ios::in|std::ios::binary);
        if (myfile.fail()) {
            string msg = "Error: cannot open file: ";
            msg += path;
            throw new Error(msg);
        }
        readsignature();
        open = true;
        return this;
    }

    Element* open_write(LispE* lisp, string path) {
        mode = file_write;
        pathname = path;
        myfile.open(STR(path), std::ios::out|std::ios::binary);
        if (myfile.fail()) {
            string msg = "Error: cannot open file: ";
            msg += path;
            throw new Error(msg);
        }
        open = true;
        return this;
    }
    
    Element* open_append(LispE* lisp, string path) {
        mode = file_write;
        pathname = path;
        myfile.open(STR(path), std::ios::app|std::ios::binary);
        if (myfile.fail()) {
            string msg = "Error: cannot open file: ";
            msg += path;
            throw new Error(msg);
        }
        open = true;
        return this;
    }
    
    Element* seek(LispE* lisp, long nb, long off) {
        if (!open)
            throw new Error ("Error: 'stream' is not opened");
        if (!off)
            myfile.seekg(nb, std::ios_base::beg);
        else
            if (off == 1)
                myfile.seekg(nb, std::ios_base::cur);
            else
                myfile.seekg(nb, std::ios_base::end);
        return True_;
    }

    Element* tell(LispE* lisp) {
        if (!open)
            throw new Error ("Error: 'stream' is not opened");
        
        return lisp->provideInteger(myfile.tellg());
    }

    Element* read(LispE* lisp, long nb) {
        if (!open || mode != file_read)
            throw new Error ("Error: expecting 'stream' to be in 'read' mode");
        
        if (nb == -1) {
            string ch = "";
            string ln;
            while (!myfile.eof()) {
                getline(myfile, ln);
                ch += ln + "\n";
            }
            open = false;
            myfile.close();
            mode = file_open;
            return lisp->provideString(ch);
        }
        
        wstring res;
        for (long i = 0; i <nb && !myfile.eof(); i++) {
            res += getchar();
        }
        
        if (myfile.eof())
            mode = file_open;
        
        return lisp->provideString(res);
    }

    Element* loop(LispE* lisp, int16_t label, List* code) {
        if (!open || mode != file_read)
            throw new Error ("Error: expecting 'stream' to be in 'read' mode");

        long i_loop;
        Element* e = null_;
        lisp->recording(null_, label);
        Element* element;
        long sz = code->liste.size();
        string localvalue;
        try {
            while (!myfile.eof()) {
                getline(myfile, localvalue);
                localvalue += "\n";
                element = lisp->provideString(localvalue);
                lisp->replacestackvalue(element, label);
                _releasing(e);
                //We then execute our instructions
                for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
                    e->release();
                    e = code->liste[i_loop]->eval(lisp);
                }
                if (e->type == l_return) {
                    myfile.close();
                    mode = file_open;
                    //the break is local, the return is global to a function
                    if (e->isBreak())
                        return null_;
                    return e;
                }
            }
        }
        catch (Error* err) {
            myfile.close();
            mode = file_open;
            throw err;
        }
        
        myfile.close();
        mode = file_open;

        return e;
    }
    
    Element* getstruct(LispE* lisp, wstring& o, wstring& c) {
        if (!open || mode != file_read)
            throw new Error ("Error: expecting 'stream' to be in 'read' mode");
        
        wstring res;
        res = getchar();
        while (res != o && !myfile.eof())
            res = getchar();
        
        if (myfile.eof()) {
            mode = file_open;
            return null_;
        }
        
        //We extract a full-fledge structure...
        //Which could contain embeddings...
        long count = 1;
        wstring str = res;
        
        res = getchar();
        if (res == c)
            count--;

        while (count && !myfile.eof()) {
            if (res == o)
                count++;
            str += res;
            if (res == L"\"") {
                res = getchar();
                while (res != L"\"" && !myfile.eof()) {
                    str += res;
                    if (res == L"\\")
                        str += getchar();
                    res = getchar();
                }
                str += res;
            }
            res = getchar();
            if (res == c)
                count--;
        }
        
        if (myfile.eof()) {
            mode = file_open;
            return null_;
        }

        str += c;
        return lisp->provideString(str);
    }

    Element* readlist(LispE* lisp) {
        if (!open || mode != file_read)
            throw new Error ("Error: expecting 'stream' to be in 'read' mode");
        
        Strings* l = lisp->provideStrings();
        string ch = "";
        string ln;
        while (!myfile.eof()) {
            getline(myfile, ln);
            l->append(ln);
        }
        open = false;
        myfile.close();
        mode = file_open;
        return l;
    }


    Element* readline(LispE* lisp) {
        if (!open || mode != file_read)
            throw new Error ("Error: expecting 'stream' to be in 'read' mode");
        
        if (myfile.eof()) {
            mode = file_open;
            return emptystring_;
        }
        
        string ln;
        getline(myfile, ln);
        ln += "\n";
        return lisp->provideString(ln);
    }
    
    inline uchar get() {
        return myfile.get();
    }

    inline void unget() {
        myfile.unget();
    }
    
    //reading one full unicode character from file...
    wchar_t getchar() {
        unsigned char c = get();
        if (!(c & 0x0080))
            return c;

        wchar_t code;
        unsigned char utf[] = { c, 0, 0, 0, 0 };
        utf[1] = get();
        
        if ((utf[0] & 0xC0)== 0xC0 && (utf[1] & 0x80)== 0x80) {
            code = (utf[0] & 0x1F) << 6;
            code |= (utf[1] & 0x3F);
            return code;
        }
        
        utf[2] = get();
        if ((utf[0] & 0xE0)== 0xE0) {
            if ((utf[1] & 0x80)== 0x80 && (utf[2] & 0x80)== 0x80) {
                code = (utf[0] & 0xF) << 12;
                code |= (utf[1] & 0x3F) << 6;
                code |= (utf[2] & 0x3F);
                return code;
            }
        }

        utf[3] = get();
        if ((utf[0] & 0xF0)== 0xF0) {
            if ((utf[1] & 0x80) == 0x80 && (utf[2] & 0x80)== 0x80 && (utf[3] & 0x80)== 0x80) {
                code = (utf[0] & 0x7) << 18;
                code |= (utf[1] & 0x3F) << 12;
                code |= (utf[2] & 0x3F) << 6;
                code |= (utf[3] & 0x3F);
                return code;
            }
        }
        
        unget();
        unget();
        unget();
        return c;
    }
       
    Element* get(LispE* lisp) {
        if (!open || mode != file_read)
            throw new Error ("Error: expecting 'stream' to be in 'read' mode");
        
        return lisp->provideString(getchar());
    }

    Element* write(LispE* lisp, Element* line) {
        if (!open || mode != file_write)
            throw new Error ("Error: expecting 'stream' to be in 'write' mode");
        
        string ch = line->toString(lisp);
        myfile << ch;
        if (myfile.fail())
            throw new Error("Error: 'write' operation did fail");
        return True_;
    }
    
    Element* writeln(LispE* lisp, Element* line) {
        if (!open || mode != file_write)
            throw new Error ("Error: expecting 'stream' to be in 'write' mode");
        
        string ch = line->toString(lisp);
        myfile << ch << std::endl;
        if (myfile.fail())
            throw new Error("Error: 'writeln' operation did fail");
        return True_;
    }
    
    Element* close(LispE* lisp) {
        if (!open || mode == file_open)
            return False_;
        open = false;
        myfile.close();
        mode = file_open;
        return True_;
    }
    
    Element* eof(LispE* lisp) {
        if (!open || mode == file_read) {
            if (myfile.eof()) {
                mode = file_open;
                return True_;
            }
        }
        return False_;
    }

    wstring asString(LispE* lisp) {
        wstring res;
        s_utf8_to_unicode(res, pathname, pathname.size());
        return res;
    }

    u_ustring asUString(LispE* lisp) {
        u_ustring res;
        s_utf8_to_unicode(res, pathname, pathname.size());
        return res;
    }
    

};

class Streamoperation : public Element {
public:
    file_command fcommand;
    Element* r;
    Element* w;
    Element* a;
    
    Streamoperation(LispE* lisp, file_command fc, int16_t l_file) : fcommand(fc), Element(l_file) {
        r = lisp->provideAtom(U"r");
        w = lisp->provideAtom(U"w");
        a = lisp->provideAtom(U"a");
    }

    Element* getstream(LispE* lisp) {
        Element* a_stream = lisp->get_variable(U"stream");
        if (a_stream->type != type)
            throw new Error("Erreur: missing 'file' stream");
        return a_stream;
    }
    
    Element* eval(LispE* lisp) {
        
        switch (fcommand) {
            case file_open: {
                //two parameters: pathname and mode
                string pathname = lisp->get_variable(U"path")->toString(lisp);
                Element* mode = lisp->get_variable(U"mode");
                Stream* str = new Stream(type);
                if (mode == r)
                    return str->open_read(lisp, pathname);
                if (mode == w)
                    return str->open_write(lisp, pathname);
                if (mode == a)
                    return str->open_append(lisp, pathname);
                delete str;
                throw new Error("Error: could not open a file");
            }
            case file_close: {
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->close(lisp);
            }
            case file_eof: {
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->eof(lisp);
            }
            case file_read: {
                Element* a_stream = getstream(lisp);
                long nb = lisp->get_variable(U"nb")->asInteger();
                return ((Stream*)a_stream)->read(lisp, nb);
            }
            case file_getstruct: {
                Element* a_stream = getstream(lisp);
                wstring o = lisp->get_variable(U"open")->asString(lisp);
                wstring c = lisp->get_variable(U"close")->asString(lisp);
                return ((Stream*)a_stream)->getstruct(lisp, o, c);
            }
            case file_readline: {
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->readline(lisp);
            }
            case file_readlist:{
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->readlist(lisp);
            }
            case file_getchar: {
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->get(lisp);
            }
            case file_seek: {
                Element* a_stream = getstream(lisp);
                long nb = lisp->get_variable(U"nb")->asInteger();
                long off = lisp->get_variable(U"off")->asInteger();
                return ((Stream*)a_stream)->seek(lisp, nb, off);
            }
            case file_tell: {
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->tell(lisp);
            }
            case file_write:{
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->write(lisp, lisp->get_variable(U"str"));
            }
            case file_writeln:{
                Element* a_stream = getstream(lisp);
                return ((Stream*)a_stream)->writeln(lisp, lisp->get_variable(U"str"));
            }
        }
		return null_;
    }
    
    wstring asString(LispE* lisp) {
        switch (fcommand) {
            case file_open:
                return L"Open a file with 'pathname' and action == 'r|'w|'a";
            case file_close:
                return L"Close a file";
            case file_eof:
                return L"Check if a file reached end of file";
            case file_read:
                return L"Read the whole file at once or nb characters";
            case file_readline:
                return L"Read one line from the file";
            case file_readlist:
                return L"Read the whole file and store each line in a list";
            case file_getchar:
                return L"Read one UTF8 character at a time";
            case file_write:
                return L"Write a string to a file";
            case file_writeln:
                return L"Write a string to a file and add a Carriage Return at the end";
            case file_seek:
                return L"Positions the head within the file at position 'nb'. 'off' can be 0(beginning), 1(current) or 2(end)";
            case file_tell:
                return L"Returns the position within the file of the head";
            case file_getstruct:
                return L"Read an embedded structure in the file that starts at openning character 'o' and stops at closing character 'c'.";
        }
		return L"";
    }
};

class Dateitem : public Element {
public:
    time_t the_time;
    
    Dateitem(Dateitem* d, bool copy) : Element(d->type) {
        if (copy)
            the_time = d->the_time;
        else
            time(&the_time);
    }
    
    Dateitem(LispE* lisp, time_t t) : Element(l_lib) {
        the_time = t;
        u_ustring d(U"date_");
        type = lisp->encode(d);
    }
    
    Dateitem(int16_t letype) : Element(letype) {
        time(&the_time);
    }
    
    Element* year(LispE* lisp, int vl) {
        struct tm* temps = localtime(&the_time);
        if (vl != -1) {
            temps->tm_year = vl - 1900;
            the_time = mktime(temps);
            return this;
        }
        
        vl = temps->tm_year + 1900;
        return lisp->provideInteger((long)vl);
    }

    Element* month(LispE* lisp, int vl) {
        struct tm* temps = localtime(&the_time);
        if (vl != -1) {
            temps->tm_mon = vl - 1;
            the_time = mktime(temps);
            return this;
        }
        
        vl = temps->tm_mon + 1;
        return lisp->provideInteger((long)vl);
    }

    Element* day(LispE* lisp, int vl) {
        struct tm* temps = localtime(&the_time);
        if (vl != -1) {
            temps->tm_mday = vl;
            the_time = mktime(temps);
            return this;
        }
        
        vl = temps->tm_mday;
        return lisp->provideInteger((long)vl);
    }

    Element* hour(LispE* lisp, int vl) {
        struct tm* temps = localtime(&the_time);
        if (vl != -1) {
            temps->tm_hour = vl;
            the_time = mktime(temps);
            return this;
        }
        
        vl = temps->tm_hour;
        return lisp->provideInteger((long)vl);
    }

    Element* minute(LispE* lisp, int vl) {
        struct tm* temps = localtime(&the_time);
        if (vl != -1) {
            temps->tm_min = vl;
            the_time = mktime(temps);
            return this;
        }
        
        vl = temps->tm_min;
        return lisp->provideInteger((long)vl);
    }

    Element* second(LispE* lisp, int vl) {
        struct tm* temps = localtime(&the_time);
        if (vl != -1) {
            temps->tm_sec = vl;
            the_time = mktime(temps);
            return this;
        }
        
        vl = temps->tm_sec;
        return lisp->provideInteger((long)vl);
    }

    Element* weekday(LispE* lisp) {
        //First parameter is a string
        struct tm* temps = localtime(&the_time);
        long vl = temps->tm_wday;
        return lisp->provideInteger((long)vl);
    }

    Element* yearday(LispE* lisp) {
        //First parameter is a string
        struct tm* temps = localtime(&the_time);
        long vl = temps->tm_yday;
        return lisp->provideInteger((long)vl);
    }

    Element* setdate(int y, int m, int d, int H, int M, int S) {
        struct tm* temps = localtime(&the_time);
        
        if (y != -1) {
            temps->tm_year = y - 1900;
            the_time = mktime(temps);
        }
        
        if (m != -1) {
            temps->tm_mon = m - 1;
            the_time = mktime(temps);
        }
        
        if (d != -1) {
            temps->tm_mday = d;
            the_time = mktime(temps);
        }
        if (H != -1) {
            temps->tm_hour = H;
            the_time = mktime(temps);
        }

        if (M != -1) {
            temps->tm_min = M;
            the_time = mktime(temps);
        }
        if (S != -1) {
            temps->tm_sec = S;
            the_time = mktime(temps);
        }
        
        return this;
    }

    Element* eval(LispE* lisp) {
        return this;
    }
    
    double asNumber() {
        return the_time;
    }

    float asFloat() {
        return the_time;
    }

    int16_t asShort() {
        return the_time;
    }
    
    long asInteger() {
        return the_time;
    }
    
    wstring asString(LispE* l) {
        wstring s;
        char buffer[100];
        long sz;
        struct tm* ladate = localtime(&the_time);
        sz = strftime(buffer, 100, "%Y/%m/%d %H:%M:%S", ladate);
        for (long i = 0; i < sz; i++)
            s+= (wchar_t)buffer[i];
        return s;
    }

    u_ustring asUString(LispE* l) {
        u_ustring s;
        char buffer[100];
        long sz;
        struct tm* ladate = localtime(&the_time);
        sz = strftime(buffer, 100, "%Y/%m/%d %H:%M:%S", ladate);
        for (long i = 0; i < sz; i++)
            s+= (u_uchar)buffer[i];
        return s;
    }

    wstring stringInList(LispE* lisp) {
        wstring s;
        char buffer[100];
        long sz;
        struct tm* ladate = localtime(&the_time);
        sz = strftime(buffer, 100, "\"%Y/%m/%d %H:%M:%S\"", ladate);
        for (long i = 0; i < sz; i++)
            s+= (wchar_t)buffer[i];
        return s;
    }

    u_ustring stringInUList(LispE* lisp) {
        u_ustring s;
        char buffer[100];
        long sz;
        struct tm* ladate = localtime(&the_time);
        sz = strftime(buffer, 100, "\"%Y/%m/%d %H:%M:%S\"", ladate);
        for (long i = 0; i < sz; i++)
            s+= (u_uchar)buffer[i];
        return s;
    }

};

class Date : public Element {
public:

    tempus tmp;
    int16_t v_d;
    int16_t v_date;
    
    Date(LispE* lisp, int16_t l_date, tempus t) : tmp(t),  Element(l_date) {
        u_ustring s(U"d");
        v_d = lisp->encode(s);
        s = U"adate";
        v_date = lisp->encode(s);
    }

    // We recover the default values described as a list
    //in the parameters of the deflib functions (see below)
    int extract_value(LispE* lisp, int16_t identifier) {
        return (int)lisp->get_variable(identifier)->asInteger();
    }

    // We recover the default values described as a list
    //in the parameters of the deflib functions (see below)
    int valeur(LispE* lisp, u_ustring identifier) {
        return (int)lisp->get_variable(identifier)->asInteger();
    }
    

    Element* year(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'year'");
        Dateitem* dt = (Dateitem*)e;
        int vl = extract_value(lisp, v_d);
        return dt->year(lisp, vl);
    }

    Element* month(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'month'");
        Dateitem* dt = (Dateitem*)e;
        int vl = extract_value(lisp, v_d);
        return dt->month(lisp, vl);
    }

    Element* day(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'day'");
        Dateitem* dt = (Dateitem*)e;
        int vl = extract_value(lisp, v_d);
        return dt->day(lisp, vl);
    }

    Element* hour(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'hour'");
        Dateitem* dt = (Dateitem*)e;
        int vl = extract_value(lisp, v_d);
        return dt->hour(lisp, vl);
    }

    Element* minute(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: Wrong type for 'minute'");
        Dateitem* dt = (Dateitem*)e;
        int vl = extract_value(lisp, v_d);
        return dt->minute(lisp, vl);
    }

    Element* second(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'second'");
        Dateitem* dt = (Dateitem*)e;
        int vl = extract_value(lisp, v_d);
        return dt->second(lisp, vl);
    }

    Element* weekday(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'weekday'");
        Dateitem* dt = (Dateitem*)e;
        return dt->weekday(lisp);
    }

    Element* yearday(LispE* lisp) {
        Element* e = lisp->get_variable(v_date);
        if (e->type != type)
            throw new Error("Error: wrong type for 'yearday'");
        Dateitem* dt = (Dateitem*)e;
        return dt->yearday(lisp);
    }

    Element* setdate(LispE* lisp) {
        Dateitem* dt = new Dateitem(type);

        int y = valeur(lisp, U"y");
        int m = valeur(lisp, U"m");
        int d = extract_value(lisp, v_d);
        int H = valeur(lisp, U"H");
        int M = valeur(lisp, U"M");
        int S = valeur(lisp, U"S");
        return dt->setdate(y,m,d,H,M,S);
    }

    Element* eval(LispE* lisp) {
        switch (tmp) {
            case date_setdate: {
                return setdate(lisp);
            }
            case date_raw: {
                return new Dateitem(type);
            }
            case date_year: {
                return year(lisp);
            }
            case date_month: {
                return month(lisp);
            }
            case date_day: {
                return day(lisp);
            }
            case date_yearday: {
                return yearday(lisp);
            }
            case date_weekday: {
                return weekday(lisp);
            }
            case date_hour: {
                return hour(lisp);
            }
            case date_minute: {
                return minute(lisp);
            }
            case date_second: {
                return second(lisp);
            }
        }
        
        return null_;
    }
    
    wstring asString(LispE* lisp) {
        switch (tmp) {
            case date_setdate: {
                return L"setdate(y,m,d,H,M,S): Creates a date from its compasantes (-1 for the default value)";
            }
            case date_year: {
                return L"year(int d): Returns the year (d = -1) or modifies it";
            }
            case date_month: {
                return L"month(int d): Returns the month (d = -1) or modifies it";
            }
            case date_day: {
                return L"day(int d): returns the day (d = -1) or modifies it";
            }
            case date_yearday: {
                return L"yearday(): returns the number of the day in the year";
            }
            case date_weekday: {
                return L"weekday(): returns the day of the week";
            }
            case date_hour: {
                return L"hour(int d): returns the hour (d = -1) or changes it";
            }
            case date_minute: {
                return L"minute(int d): returns the minute (d = -1) or changes it";
            }
            case date_second: {
                return L"second(int d): returns the second (d = -1) or changes it";
            }
            case date_raw: {
                return L"Initializes a 'date' object";
            }
        }
        return L"";
    }
};

class Chrono : public Element {
public:
    std::chrono::high_resolution_clock::time_point chrono_value;

    Chrono(Chrono* c, bool copy) : Element(c->type) {
        if (copy)
            chrono_value = c->chrono_value;
        else
            chrono_value = std::chrono::high_resolution_clock::now();
    }
    
    Chrono(LispE* lisp, int16_t l_chrono) : Element(l_chrono) {
        chrono_value = std::chrono::high_resolution_clock::now();

    }
    
    Element* eval(LispE* lisp) {
        return new Chrono(this, false);
    }
    
    int16_t label() {
        return type;
    }
    
    double asNumber() {
        return chrono_value.time_since_epoch().count();
    }

    float asFloat() {
        return chrono_value.time_since_epoch().count();
    }

    long asInteger() {
        return chrono_value.time_since_epoch().count();
    }

    int16_t asShort() {
        return chrono_value.time_since_epoch().count();
    }

    Element* minus(LispE* lisp, Element* temps) {
        if (temps->label() == type) {
            return lisp->provideNumber(std::chrono::duration_cast<std::chrono::milliseconds>( chrono_value - ((Chrono*)temps)->chrono_value).count());
        }
        return zero_;
    }
    
    wstring asString(LispE* lisp) {
        return convertToWString((long)chrono_value.time_since_epoch().count());
    }

    u_ustring asUString(LispE* lisp) {
        return convertToUString((long)chrono_value.time_since_epoch().count());
    }

    string toString(LispE* lisp) {
        return convertToString((long)chrono_value.time_since_epoch().count());
    }

};

class Command : public Element {
public:
    systeme sys;
    int16_t v_name;
    int16_t v_path;
    int16_t v_cmd;
    int16_t v_value;
    
    Command(LispE* lisp, systeme s) : sys(s), Element(l_lib) {
        u_ustring nom = U"name";
        v_name = lisp->encode(nom);
        nom = U"value";
        v_value = lisp->encode(nom);
        nom = U"cmd";
        v_cmd = lisp->encode(nom);
        nom = U"path";
        v_path = lisp->encode(nom);
    }
    
    void recording(Dictionary* mp, u_ustring k, Element* e) {
        mp->recording(k,e);
    }
    
    Element* fullPath(LispE* lisp) {
        char localpath[4096];
        Element* chemin = lisp->get_variable(v_path);
        string path = chemin->toString(lisp);
#ifdef WIN32
		_fullpath(localpath, STR(path), 4096);
#else
        realpath(STR(path), localpath);
#endif
        path = localpath;
        return lisp->provideString(path);
    }
    
    Element* fileinfo(LispE* lisp) {
        Element* chemin = lisp->get_variable(v_path);
        string filename = chemin->toString(lisp);
        
        struct stat scible;
        int stcible = stat(STR(filename), &scible);
        if (stcible >= 0) {
            Element* size = lisp->provideInteger((long)scible.st_size);
            Element* change = new Dateitem(lisp, scible.st_mtime);
            Element* adate = new Dateitem(lisp, scible.st_atime);
            Element* cdate = new Dateitem(lisp, scible.st_ctime);
            Dictionary* mp = lisp->provideDictionary();
            
            recording(mp, U"size", size);
            recording(mp, U"date", change);
            recording(mp, U"access", adate);
            recording(mp, U"change", cdate);
            recording(mp, U"pathname", chemin->copying());
            
            if ((scible.st_mode & S_IFMT) == S_IFDIR)
                recording(mp, U"directory", true_);
            else
                recording(mp, U"directory", false_);
            return mp;
        }
        
        return null_;
    }
    
    Element* isDirectory(LispE* lisp) {
        string dirName_in = lisp->get_variable(v_path)->toString(lisp);
    #ifdef WIN32
        DWORD ftyp = GetFileAttributesA(dirName_in.c_str());
        if (ftyp == INVALID_FILE_ATTRIBUTES)
            return null_;  //something is wrong with your path!
        if (ftyp & FILE_ATTRIBUTE_DIRECTORY)
            return True_;   // this is a directory!
    #else
        struct stat st;
        if (stat(STR(dirName_in), &st) == 0)
            if ((st.st_mode & S_IFMT) == S_IFDIR)
                return True_;
    #endif
        return null_;    // this is not a directory!
    }

    Element* listeDirectory(LispE* lisp) {
        Element* kpath = lisp->get_variable(v_path);
        string path = kpath->toString(lisp);
        directorylisting dir(path);
        bool go = dir.initial();
        
        if (!go)
            return emptylist_;
        
        Strings* kvect = lisp->provideStrings();
        string name;
        if (path.back() != '/')
            path += "/";
        
        while (dir.getnext(name)) {
            if (name == "." || name == "..")
                continue;
            if (path != "./")
                name = path + name;
            kvect->append(name);
        }
        return kvect;
    }

    Element* eval(LispE* lisp) {
        switch (sys) {
            case sys_ls:
                return listeDirectory(lisp);
            case sys_isdirectory:
                return isDirectory(lisp);
            case sys_command: {
                //Important we called our variable "cmd" (see below)
                Element* lisp_cmd = lisp->get_variable(v_cmd);
                // We recover the corresponding value
                string cmd = lisp_cmd->toString(lisp);
                FILE *fp;
                int status;
                
                char res[PATH_MAX];
                
#ifdef WIN32
				fp = _popen(STR(cmd), "r");
#else
                fp = popen(STR(cmd), "r");
#endif                
                if (fp == NULL)
                    throw new Error("Error: the pipe did not open properly");
                
                Strings* resultat = lisp->provideStrings();
                while (fgets(res, PATH_MAX, fp) != NULL) {
                    cmd  = res;
                    resultat->append(cmd);
                }
                
#ifdef WIN32
				status = _pclose(fp);
#else
				status = pclose(fp);
#endif
                if (status == -1) {
                    resultat->release();
                    throw new Error("Error: when closing the pipe");
                }
                
                return resultat;
            }
            case sys_setenv: {
                // Here we have two arguments
                Element* lisp_nom = lisp->get_variable(v_name);
                Element* lisp_valeur = lisp->get_variable(v_value);
                
                string nom = lisp_nom->toString(lisp);
                string valeur = lisp_valeur->toString(lisp);

#ifdef WIN32
				nom += "=";
				nom += valeur;
				_putenv(STR(nom));
#else
                setenv(nom.c_str(), valeur.c_str(), 1);
#endif
                return True_;
            }
            case sys_getenv: {
                Element* lisp_nom = lisp->get_variable(v_name);
                string nom = lisp_nom->toString(lisp);
                char* n = getenv(nom.c_str());
                if (n == NULL)
                    return emptystring_;
                string s = n;
                return lisp->provideString(s);
            }
            case sys_realpath: {
                return fullPath(lisp);
            }
            case sys_fileinfo:
                return fileinfo(lisp);
        }
		return null_;
    }
    
    //We use this instruction to return a description of the instruction
    //in effect, just do: (print getenv) to get this information
    wstring asString(LispE* lisp) {
        switch (sys) {
            case sys_command:
                return L"Executes a system command";
            case sys_ls:
                return L"Returns the content of a directory";
            case sys_isdirectory:
                return L"Check if the path is a directory";
            case sys_setenv:
                return L"Creates or modifies an environment variable";
            case sys_getenv:
                return L"Retrieves the value of an environment variable";
            case sys_fileinfo:
                return L"Returns information about a file";
            case sys_realpath:
                return L"Returns the full path corresponding to a partial path";
        }
		return L"";
    }
};


//We are also going to implement the body of the call
void moduleSysteme(LispE* lisp) {
    //We first create the body of the function
    lisp->extension("deflib command (cmd)", new Command(lisp, sys_command));
    //We also add setenv
    lisp->extension("deflib setenv (name value)", new Command(lisp, sys_setenv));
    //getenv
    lisp->extension("deflib getenv (name)", new Command(lisp, sys_getenv));
    //endofpipe
    lisp->extension("deflib fileinfo (path)", new Command(lisp, sys_fileinfo));
    lisp->extension("deflib realpath (path)", new Command(lisp, sys_realpath));
    lisp->extension("deflib ls (path)", new Command(lisp, sys_ls));
    lisp->extension("deflib isdirectory (path)", new Command(lisp, sys_isdirectory));

    //------------------------------------------

    u_ustring w = U"chrono_";
    int16_t identifier = lisp->encode(w);
    lisp->extension("deflib chrono ()", new Chrono(lisp, identifier));
    
    //------------------------------------------

    w = U"date_";
    identifier =  lisp->encode(w);
    lisp->extension("deflib date ()", new Date(lisp, identifier,  date_raw));
    
    //Note that "d" is provided as a list, to indicate that it is optional
    //If the list consists of two items, it means that the second item is the default value
    lisp->extension("deflib year (adate (d -1))", new Date(lisp, identifier,  date_year));
    lisp->extension("deflib month (adate (d -1))", new Date(lisp, identifier,  date_month));
    lisp->extension("deflib day (adate (d -1))", new Date(lisp, identifier,  date_day));
    lisp->extension("deflib hour (adate (d -1))", new Date(lisp, identifier,  date_hour));
    lisp->extension("deflib minute (adate (d -1))", new Date(lisp, identifier,  date_minute));
    lisp->extension("deflib second (adate (d -1))", new Date(lisp, identifier,  date_second));
    lisp->extension("deflib yearday (adate)", new Date(lisp, identifier,  date_yearday));
    lisp->extension("deflib weekday (adate)", new Date(lisp, identifier,  date_weekday));
    //m, d,H, M, S are optional
    lisp->extension("deflib setdate (y (m -1) (d -1) (H -1) (M -1) (S -1))", new Date(lisp, identifier,  date_setdate));
    //------------------------------------------

    w = U"file_";
    identifier = lisp->encode(w);
    lisp->extension("deflib file (path (mode 'r))", new Streamoperation(lisp, file_open, identifier));
    lisp->extension("deflib file_close (stream)", new Streamoperation(lisp, file_close, identifier));
    lisp->extension("deflib file_eof (stream)", new Streamoperation(lisp, file_eof, identifier));
    lisp->extension("deflib file_read (stream (nb -1))", new Streamoperation(lisp, file_read, identifier));
    lisp->extension("deflib file_getstruct (stream open close)", new Streamoperation(lisp, file_getstruct, identifier));
    lisp->extension("deflib file_readlist (stream)", new Streamoperation(lisp, file_readlist, identifier));
    lisp->extension("deflib file_readline (stream)", new Streamoperation(lisp, file_readline, identifier));
    lisp->extension("deflib file_getchar (stream)", new Streamoperation(lisp, file_getchar, identifier));
    lisp->extension("deflib file_seek (stream nb (off 0))", new Streamoperation(lisp, file_seek, identifier));
    lisp->extension("deflib file_tell (stream)", new Streamoperation(lisp, file_tell, identifier));
    lisp->extension("deflib file_write (stream str)", new Streamoperation(lisp, file_write, identifier));
    lisp->extension("deflib file_writeln (stream str)", new Streamoperation(lisp, file_writeln, identifier));
}

