/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//x_toknize.h

#ifndef x_tokenize_h
#define x_tokenize_h
#include "x_node.h"

#define c_is_digit(c) (c >= '0' && c <= '9')

void getdefaultrules(vector<string>& rules);

class x_tokenize : public x_reading {
public:
    vector<short> points;
    vector<short> separators;
    
    void setrules();
    void resetrules(vector<string>& rules);
    
    void load() {
        if (!loaded) {
            setrules();
            parserules();
            loaded=true;
        }
    }
    
    void selectcomma(bool v) {
        long r;
        if (v) {
            action[separators[0]]=xr_skiprule;
            for (r=0;r<points.size();r+=2)
                tokenizer[points[r]][points[r+1]]=',';
        }
        else {
            action[separators[0]]=0;
            for (r=0;r<points.size();r+=2)
                tokenizer[points[r]][points[r+1]]='.';
        }
    }
    
    void keeprc(bool tr) {
        if (tr) {
            action[1]=0;
        }
        else {
            action[1]=-1;
        }
    }
    
    bool parseexternalrules(string& message) {

        long ocurly = 0;
        long osquare = 0;
        long ccurly = 0;
        long csquare = 0;
        long opar = 0;
        long cpar = 0;
        long pos;
        bool err;
        string line;
        string equal("=");

        for (long i=0;i<rules.size();i++) {
            line=rules[i];
            
            if (line[1]==':') { //we detect a meta-rule...
                continue;
            }
            
            ocurly = 0;
            osquare = 0;
            ccurly = 0;
            csquare = 0;
            opar = 0;
            cpar = 0;
            //first we check whether brackets are all balanced...
            for (pos =0; pos<line.size(); pos++) {
                if (line[pos]=='%') {
                    pos++;
                    continue;
                }
                switch(line[pos]) {
                    case '{':
                        ocurly++;
                        break;
                    case '}':
                        ccurly++;
                        break;
                    case '[':
                        osquare++;
                        break;
                    case ']':
                        csquare++;
                        break;
                    case '(':
                        opar++;
                        break;
                    case ')':
                        cpar++;
                        break;
                }
            }

            if (ocurly != ccurly) {
                err = true;
                if (ocurly == 1 && line[0] == '{' && ccurly == 0)
                    err=false;
                
                if (ocurly == 0 && line[0] == '}' && ccurly == 1)
                    err=false;
                
                if (err) {
                    message = line;
                    message += ": '{}' Mismatch";
                    return false;
                }
            }

            if (osquare != csquare) {
                err = true;
                if (osquare == 1 && line[0] == '[' && csquare == 0)
                    err=false;
                
                if (osquare == 0 && line[0] == ']' && csquare == 1)
                    err=false;
                
                if (err) {
                    message = line;
                    message += ": '[]' Mismatch";
                    return false;
                }
            }

            if (opar != cpar) {
                err = true;
                if (opar == 1 && line[0] == '(' && cpar == 0)
                    err=false;
                
                if (opar == 0 && line[0] == ')' && cpar == 1)
                    err=false;
                
                if (err) {
                    message = line;
                    message += ": '()' Mismatch";
                    return false;
                }
            }

            
            //Second we look for the = sign at the end of the string...
            
            pos=line.rfind(equal,line.size()-1);
            if (pos == -1 || pos == line.size()-1) {
                message = line;
                message+=": Cannot find code assignment...";
                return false;
            }
        }
        //We have checked that all lines were ok
        //We can parse our rules now
        parserules();
        return true;
    }
    
    void separator(bool s) {
        if (s) {
            action[separators[0]]=0;
            action[separators[1]]=0;
        }
        else {
            action[separators[0]]=xr_skiprule;
            action[separators[1]]=xr_skiprule;
        }
    }
};


class x_wtokenize : public x_wreading {
public:
    vector<short> points;
    vector<short> separators;
    
    void setrules();
    void resetrules(vector<string>& rules);

    void load() {
        if (!loaded) {
            setrules();
            parserules();
            loaded=true;
        }
    }
    
    void selectcomma(bool v) {
        long r;
        if (v) {
            action[separators[0]]=xr_skiprule;
            for (r=0;r<points.size();r+=2)
                tokenizer[points[r]][points[r+1]]=',';
        }
        else {
            action[separators[0]]=0;
            for (r=0;r<points.size();r+=2)
                tokenizer[points[r]][points[r+1]]='.';
        }
    }
    
    void keeprc(bool tr) {
        if (tr) {
            action[1]=0;
        }
        else {
            action[1]=-1;
        }
    }
    
    void separator(bool s) {
        if (s) {
            action[separators[0]]=0;
            action[separators[1]]=0;
        }
        else {
            action[separators[0]]=xr_skiprule;
            action[separators[1]]=xr_skiprule;
        }
    }
    
    bool parseexternalrules(wstring& message) {
        
        long ocurly = 0;
        long osquare = 0;
        long ccurly = 0;
        long csquare = 0;
        long opar = 0;
        long cpar = 0;
        long pos;
        bool err;
        wstring line;
        wstring equal(L"=");
        
        for (long i=0;i<rules.size();i++) {
            sc_utf8_to_unicode(line, USTR(rules[i]), rules[i].size());
            
            if (line[1]==':') { //we detect a meta-rule...
                continue;
            }
            
            ocurly = 0;
            osquare = 0;
            ccurly = 0;
            csquare = 0;
            opar = 0;
            cpar = 0;
            //first we check whether brackets are all balanced...
            for (pos =0; pos<line.size(); pos++) {
                if (line[pos]=='%') {
                    pos++;
                    continue;
                }
                switch(line[pos]) {
                    case '{':
                        ocurly++;
                        break;
                    case '}':
                        ccurly++;
                        break;
                    case '[':
                        osquare++;
                        break;
                    case ']':
                        csquare++;
                        break;
                    case '(':
                        opar++;
                        break;
                    case ')':
                        cpar++;
                        break;
                }
            }
            
            if (ocurly != ccurly) {
                err = true;
                if (ocurly == 1 && line[0] == '{' && ccurly == 0)
                    err=false;
                
                if (ocurly == 0 && line[0] == '}' && ccurly == 1)
                    err=false;
                
                if (err) {
                    message = line;
                    message += L": '{}' Mismatch";
                    return false;
                }
            }
            
            if (osquare != csquare) {
                err = true;
                if (osquare == 1 && line[0] == '[' && csquare == 0)
                    err=false;
                
                if (osquare == 0 && line[0] == ']' && csquare == 1)
                    err=false;
                
                if (err) {
                    message = line;
                    message += L": '[]' Mismatch";
                    return false;
                }
            }
            
            if (opar != cpar) {
                err = true;
                if (opar == 1 && line[0] == '(' && cpar == 0)
                    err=false;
                
                if (opar == 0 && line[0] == ')' && cpar == 1)
                    err=false;
                
                if (err) {
                    message = line;
                    message += L": '()' Mismatch";
                    return false;
                }
            }
            
            
            //Second we look for the = sign at the end of the string...
            
            pos=line.rfind(equal,line.size()-1);
            if (pos == -1 || pos == line.size()-1) {
                message = line;
                message+=L": Cannot find code assignment...";
                return false;
            }
        }
        //We have checked that all lines were ok
        //We can parse our rules now
        parserules();
        return true;
    }
};

class x_forindent : public x_reading {
public:
    
    
    //We keep all characters...
    void setrules() {
        
        lookforquotes = true;
        
        //Spaces are now all kept
        rules.push_back(" =0");                         //0     space
        rules.push_back("\t=0");                        //1     tab
        rules.push_back("\n=0");                        //2     cr
        rules.push_back("\r=0");                        //3     cr
        
        rules.push_back("1:{%d #A-F #a-f}");            //2     metarule on 1, for hexadecimal digits

        //Fast tracks for recurrent punctations
        rules.push_back(";=0");                         //4     ;
        rules.push_back(",=0");                         //5     ,
        rules.push_back("==0");                         //6     =
        rules.push_back("~=0");                         //7     ~
        rules.push_back("!=0");                         //8     !
        rules.push_back("(=0");                         //9     (
        rules.push_back(")=0");                         //10    )
        rules.push_back("[=0");                         //11    [
        rules.push_back("]=0");                         //12    ]
        rules.push_back("{=0");                         //13    {
        rules.push_back("}=0");                         //14    }
        rules.push_back(".=0");                         //15    .
        rules.push_back("^=0");                         //16    ^
        rules.push_back("+=0");                         //17    +
        rules.push_back("-=0");                         //18    -
        rules.push_back("*=0");                         //19    *
        rules.push_back("\\=0");                        //19    \
        rules.push_back("%=0");                         //20    %
        rules.push_back("<=0");                         //21    <
        rules.push_back(">=0");                         //22    >
        rules.push_back("|=0");                         //23    |
        rules.push_back("&=0");                         //24    &
        rules.push_back(":=0");                         //25    :
        rules.push_back("$=0");                         //26    $
        rules.push_back("#=0");                         //27    #
        rules.push_back("?=0");                         //28    ?
        
        
        //Comments are also kept here...
        rules.push_back("//%.~%r+=9");                  //29    comments starting with // with no carriage return (CR) inside
        rules.push_back("//=9");                        //30    empty comment
        rules.push_back("/@%.+@/=9");                   //31    long comments starting with /@ and finishing with @/
        rules.push_back("/@@/=9");                      //32    empty long comment starting with /@ and finishing with @/
        rules.push_back("/=0");                         //33    /
        
        //Strings
        //Double quote
        rules.push_back("\"\"=1");                      //34    empty string ""
        rules.push_back("\"%?~%r+\"=1");                //35    string "" does not contain CR and can escape characters (%?)
        
        //Single quote
        rules.push_back("''=2");                        //36    empty string ''
        rules.push_back("'%.~%r+'=2");                  //37    string '' does not contain CR and does not process escape characters
        
        rules.push_back("r\"%?+\"=2");                 //44    tamgu regular expressions...
        rules.push_back("r'%?+'=2");                   //44    tamgu regular expressions...
        rules.push_back("p\"%?+\"=2");                 //44    posix regular expressions...
        rules.push_back("p'%?+'=2");                   //44    posix regular expressions...

        //Long quotes
        rules.push_back("@\"\"@=5");                  //38    empty string @""@
        rules.push_back("@\"%?+\"@=5");               //39    string @" "@ can contain CR and escape characters (we do not keep the @s)
        
        //Unicode double quote strings
        rules.push_back("u\"\"=6");                    //40    empty string u""
        rules.push_back("u\"%?~%r+\"=6");              //41    string u"" unicode string (we do not keep the u in the parse)
        
        //Unicode single quote strings
        rules.push_back("u''=7");                      //42    empty string u''
        rules.push_back("u'%.~%r+'=7");                //43    string u'' unicode string
        
        //Unicode long quote strings
        rules.push_back("u@\"%?+\"@=8");             //44    empty string u@""@
        rules.push_back("u@\"%?+\"@=8");             //45    string u@".."@ unicode string
        
        rules.push_back("0x%1+(.%1+)([p P]([- +])%d+)=3"); //47 hexadecimal
        rules.push_back("%d+(.%d+)([e E]([- +])%d+)=3");         //47    exponential digits
        
        // Rules start here        
        rules.push_back("{%a %d %H}+=4");               //48    label a combination of alpha, digits and hangul characters
        rules.push_back("%n=0");
        rules.push_back("%o=0");                        //50    operators
        rules.push_back("%p=0");                        //49    punctuation
        rules.push_back("%.~{%S %p %o}+=4");            //51    An unknown UTF8 token separated with spaces, punctuation or operators...
    }
    
    //We tokenize our string...
    void tokenize(string& thestr, bool keeppos=false, vector<string>* vstack=NULL, vector<unsigned char>* vtype=NULL) {
        //only stack is necessary
        if (vstack==NULL)
            stack.clear();
        if (vtype==NULL)
            stacktype.clear();
        parcours=thestr;
        juststack=true;
        apply(keeppos, vstack,vtype);
    }
};

//------------------------------------------------------------------------------------------------Coloring tokenizer rules

class x_coloringrule : public x_reading {
public:
    
    void setrules() {
        lookforquotes = true;
            //spaces
        rules.push_back(" =#");                         //0     space
        rules.push_back("\t=#");                        //1     tab
        rules.push_back("\n=#");                        //2     cr
        rules.push_back("\r=#");                        //3     cr
        
        rules.push_back("1:{%d #A-F #a-f}");            //2     metarule on 1, for hexadecimal digits
        
            //Fast tracks for recurrent punctuations
        rules.push_back(";=#");                         //4     ;
        rules.push_back(",=#");                         //5     ,
        rules.push_back("==0");                         //6     =
        rules.push_back(")=#");                         //7     )
        rules.push_back("[=#");                         //8     [
        rules.push_back("]=#");                         //9     ]
        rules.push_back("{=#");                         //10    {
        rules.push_back("}=#");                         //11    }
        rules.push_back("~=#");                         //12    ~
        rules.push_back("!=#");                         //13    !
        rules.push_back("^=#");                         //14    ^
        rules.push_back("+=#");                         //15    +
        rules.push_back("-=#");                         //16    -
        rules.push_back("*=#");                         //17    *
        rules.push_back("%=#");                         //18    %
        rules.push_back(">=#");                         //20    >
        rules.push_back("|=#");                         //21    |
        rules.push_back("&=#");                         //22    &
        rules.push_back(":=#");                         //23    :
        rules.push_back("\\=#");                        //19    \
        
        rules.push_back("${%d %a %H}+=10");             //24    $%d+
        rules.push_back("$=#");                         //25    $
        
        rules.push_back("#{%d %a %H}+=10");             //26    #label
        rules.push_back("#=#");                         //27    #
        
        rules.push_back("?{%a %d %H}+=10");             //28    ?label
        rules.push_back("?=#");                         //29    ?
        
            // Characters that we need
        rules.push_back("<%a{%a %d %H}+=13");           //30    <label
        rules.push_back("<=#");                         //19    <
        rules.push_back(".{%a %d %H}+(=11");            //30    .label(
        rules.push_back(".=#");                         //31    .
        rules.push_back("(=#");                         //32    (
        
            //Comments
        rules.push_back("//%.~%r+=5");                  //33    comments
        rules.push_back("//+=5");                       //34    empty comment
        rules.push_back("/@%.+@/=5");                   //35    long comments
        rules.push_back("/@@/=5");                      //36    empty long comments
        rules.push_back("/=#");                         //37    /
        
            //Strings
            //Double quotes
        rules.push_back("\"\"=1");                      //38    empty string ""
        rules.push_back("\"%?~%r+\"=1");                //39    string ""
                                                        
            //Single quote
        rules.push_back("''=2");                        //40    empty string ''
        rules.push_back("'%.~%r+'=2");                  //41    string ''
        
        rules.push_back("r\"%?+\"=2");                 //44    tamgu regular expressions...
        rules.push_back("r'%?+'=2");                   //44    tamgu regular expressions...
        rules.push_back("p\"%?+\"=2");                 //44    posix regular expressions...
        rules.push_back("p'%?+'=2");                   //44    posix regular expressions...
        
            //Long quotes
        rules.push_back("@\"\"@=3");                    //42    empty string @""@
        rules.push_back("@\"%?+\"@=3");                 //43    string @" "@
        rules.push_back("@{%a %d}+=14");                 //43    annotation rule head
        
            //Unicode double quote strings
        rules.push_back("u\"\"=1");                     //44    empty string u""
        rules.push_back("u\"%?~%r+\"=1");               //45    string u"" unicode string (we do not keep the u in the parse)
        
            //Unicode single quote strings
        rules.push_back("u''=2");                       //46    empty string
        rules.push_back("u'%.~%r+'=2");                 //47    string u'' unicode string
        
            //Unicode long quote strings
        rules.push_back("u@\"%?+\"@=3");                //48    empty string u@""@
        rules.push_back("u@\"%?+\"@=3");                //49    string u@".."@ unicode string
        
        rules.push_back("0x%1+(.%1+)([p P]([- +])%d+)=0"); //47 hexadecimal
        rules.push_back("%d+(.%d+)([e E]([- +])%d+)=0");    //51    exponential digits
        
            //Rules start here
        rules.push_back("{%a %d}+(=12");                //52    label(
        rules.push_back("{%a %d %H}+=4");               //53    label
        rules.push_back("%n=#");                        //52    non-breaking space (not kept)
        rules.push_back("%o=#");                        //55    operators
        rules.push_back("%p=#");                        //54    punctuation
        rules.push_back("%.~{%S %p %o}+=4");            //56    An unknown token separated with spaces or punctuation...
        rules.push_back("%=#");                         //56    An unknown character...
    }
};

#endif

