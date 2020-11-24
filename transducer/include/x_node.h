/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//x_nodes.h

#ifndef x_node_h
#define x_node_h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ostream>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include <sys/stat.h>

#define xr_char 1
#define xr_meta 2
#define xr_metachar 3
#define xr_chardisjunction 4
#define xr_metadisjunction 8
#define xr_negation 16
#define xr_plus 32
#define xr_skip 64
#define xr_neednext 128
#define xr_singlebody 256
#define xr_optional 512
#define xr_endoptional 1024
#define xr_optionality 1536
#define xr_skiprule 2048

#define verif(a,b) ((a&b)==b)

#define c_is_digit(c) (c >= '0' && c <= '9')


class x_rules {
public:
    vector<string> rules;
    vector<vector<short> > ruleelements;
    vector<short*> closing;
    vector<short> action;

    vector<long> stackln;
    vector<unsigned char> stacktype;
    vector<long> cpos;

    short table[255];


    long firstrule;
    bool loaded;
    bool juststack;
    bool lookforquotes;
    bool lispmode;
    unsigned char escape;

    x_rules() {
        firstrule=-1;
        juststack=false;
        loaded=false;
        lookforquotes = false;
        lispmode = false;

        for (short i=0; i< 255; i++) {
            table[i]=255;
        }
        
        //the escape character: by default it is \x
        escape='\\';
    }
    
    ~x_rules() {
        for (long i = 0; i < closing.size(); i++) {
            if (closing[i] != NULL)
                delete[] closing[i];
        }
    }
    
    virtual void reset() {
        for (short i=0; i< 255; i++) {
            table[i]=255;
        }
        
        //the escape character: by default it is \x
        escape='\\';
        firstrule=-1;
        juststack=false;
        loaded=false;
        
        action.clear();
        rules.clear();
        ruleelements.clear();
        for (long i = 0; i < closing.size(); i++) {
            if (closing[i] != NULL)
                delete[] closing[i];
        }
        closing.clear();
    }
    
    void setrules(vector<string>& r) {
        reset();
        rules=r;
        parserules();
    }
    
    void getrules(vector<string>& r) {
        r=rules;
    }
    
    virtual void setrules() {
        lookforquotes = true;
        
        /*
         a) A metarule is composed of two parts: c:expression, where c is the metacharacter that be accessed through %c and expression is a single body rule.
         
         for instance, we could have encoded %o as:
                    rules.push_back("o:[≠ ∨ ∧ ÷ × ² ³ ¬]");
        
         (see conversion.cxx: x_tokenize for examples of these rules)
         
         IMPORTANT: These rules should be declared with one single operation.
                    Their body will replace the call to a %c in other rules  (see the test on metas in the parse section)
         
         If you use a character that is already a meta-character (such as "a" or "d"), then the meta-character will be replaced with
         this new description... However, its content might still use the standard declaration:
         
                rules.push_back("a:{%a %d %p}"); "%1 is now a combination of alphabetical characters, digits and punctuations

         
        b) A rule is composed of two parts: body = action
         
         action is either an integer or a #, which is evaluated later in codeparse with the following encoding:
         
         1 is a regular string enclosed in ""
         2 is a string enclosed in ''
         3 is a number
         4 is a token
         5 is a string enclosed in @" "@
         6 is a unicode string enclosed in u""
         7 is a unicode string enclosed in u''
         8 is a unicode string enclosed in u@""@
         # means that the extracted string will not be stored for parsing (spaces, cr and comments mainly)
         
         IMPORTANT: each of the code from 1 to 8 is ASSOCIATED with the parser with a specific method. More exactly, in the BNF grammar
         specific rules such as word, anumber are associated with a code that will match one of these numbers...
         For instance, here is the implementation of word:
         
         char bnf_tamgu::m_word(string& lreturn,x_node** tree) {
         ....
         if (fx->stacktype[currentpos]==4) { //the type (populated by apply check the value 4, which is a token...
         
         In the BNF grammar word is declared as:
         
         ^4 word := .  the ^4 indicates that it should be interpreted as a token, which leads the implement the test with 4
         ^1 astringdouble := . the ^1 indicates that this a double quote string, which leads to a test with 1
         
         etc.
         
         body uses the following instructions:
         
         x   is a character that should be recognized

         #x     comparison with character x...
         #x-y   comparison between x and y. x and y should be ascii characters...
         
         %x  is a meta-character with the following possibilities:
         
         %.  is any character
         %a  is any alphabetical character (including unicode ones such as éè)
         %C  is any uppercase character
         %c  is any lowercase character
         %d  is any digits
         %H  is any hangul character
         %n  is a non-breaking space
         %o  is any operators
         %p  is any punctuations
         %r  is a carriage return both \n and \r
         %s  is a space (32) or a tab (09)
         %S  is both a carriage return or a space (%s or %r)
         %?  is any character with the possibility of escaping characters with a '\' such as: \r \t \n or \"

         %nn  you can create new metarules associated with any OTHER characters...

         
         (..) is a sequence of optional instructions
         [..] is a disjunction of possible characters
         {..} is a disjunction of meta-characters
         x+   means that the instruction can be repeated at least once
         x-   means that the character should be recognized but not stored in the parsing string
         %.~..  means that all character will be recognizd except for those in the list after the tilda.
         
         IMPORTANT: do not add any spaces as they would be considered as a character to test...
         
         */
        
        //Spaces, skipped in the parsing string
        rules.push_back(" =#");                         //0     space (not kept)
        rules.push_back("\t=#");                        //1     tab (not kept)
        rules.push_back("\n=#");                        //2     cr (not kept)
        rules.push_back("\r=#");                        //3     cr (not kept)
        
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
        rules.push_back("..=0");                        //15    ..
        rules.push_back(".=0");                         //16    .
        rules.push_back("^=0");                         //17    ^
        rules.push_back("+=0");                         //18    +
        rules.push_back("-=0");                         //19    -
        rules.push_back("*=0");                         //20    *
        rules.push_back("%=0");                         //21    %
        rules.push_back("<=0");                         //22    <
        rules.push_back(">=0");                         //23    >
        rules.push_back("|=0");                         //24    |
        rules.push_back("&=0");                         //25    &
        rules.push_back(":=0");                         //26    :
        rules.push_back("$=0");                         //27    $
        rules.push_back("#=0");                         //28    #
        rules.push_back("?=0");                         //29    ?
        rules.push_back("\\=0");                        //29    ?

        //Comments
        rules.push_back("//%.~%r+=#");                  //30    comments starting with // with no carriage return (CR) inside (not kept)
        rules.push_back("//=#");                        //31    empty comment starting with // with no carriage return (CR) inside (not kept)
        rules.push_back("/@%.+@/=#");                   //32    long comments starting with /@ and finishing with @/ (not kept)
        rules.push_back("/@@/=#");                      //33    empty long comment starting with /@ and finishing with @/
        rules.push_back("/=0");                         //34    /
        
        //Strings
        //Double quote
        rules.push_back("\"\"=1");                      //35    empty string ""
        rules.push_back("\"%?~%r+\"=1");                //36    string "" does not contain CR and can escape characters (%?)
        
        //Single quote
        rules.push_back("''=2");                        //37    empty string ''
        rules.push_back("'%.~%r+'=2");                  //38    string '' does not contain CR and does not process escape characters
        
        //Long quotes
        rules.push_back("@-\"\"@-=5");                  //39    empty string @""@
        rules.push_back("@-\"%?+\"@-=5");               //40    string @" "@ can contain CR and escape characters (we do not keep the @s)
        
        //tamgu regular expression strings
        rules.push_back("r-\"%?~%r+\"=9");              //42    string r"" tamgu regular expression (we do not keep the r in the parse)
        rules.push_back("r-'%?~%r+'=10");               //42    string r"" tamgu regular expression (we do not keep the r in the parse)
        rules.push_back("p-\"%?~%r+\"=11");             //42    string p"" tamgu posix expression (we do not keep the p in the parse)
        rules.push_back("p-'%?~%r+'=12");               //42    string p"" tamgu posix expression (we do not keep the p in the parse)

        //Unicode double quote strings
        rules.push_back("u-\"\"=6");                    //41    empty string u""
        rules.push_back("u-\"%?~%r+\"=6");              //42    string u"" unicode string (we do not keep the u in the parse)
        
        //Unicode single quote strings
        rules.push_back("u-''=7");                      //43    empty string u''
        rules.push_back("u-'%.~%r+'=7");                //44    string u'' unicode string
        
        //Unicode long quote strings
        rules.push_back("u-@-\"%?+\"@-=8");             //45    empty string u@""@
        rules.push_back("u-@-\"%?+\"@-=8");             //46    string u@".."@ unicode string
        
        
        rules.push_back("0x%1+(.%1+)([p P]([- +])%d+)=3");  //47 hexadecimal: can handle 0x1.16bca4f9165dep-3
        rules.push_back("%d+(.%d+)([e E]([- +])%d+)=3");    //48    exponential digits
        
        // Rules start here
        //This character should be interpreted as one
        rules.push_back("{%a %d %H}+=4");               //49    label a combination of alpha, digits and hangul characters
        rules.push_back("%n=#");                        //1     non-breaking space (not kept)
        rules.push_back("%o=0");                        //51    operators
        rules.push_back("%p=0");                        //50    punctuation
        rules.push_back("%.~{%S %p %o}+=4");            //52    An unknown UTF8 token separated with spaces, punctuation or operators...
    }

    virtual void parserules() {}
};

class x_reading  : public x_rules {
public:
    vector<string> stack;
    vector<long> bpos;
    vector<vector<string> > tokenizer;
    vector<vector<string> > disjunctions;

    
    hmap<string, bool> operators;
    
    agnostring parcours;
    
    x_reading() : parcours("") {
        //The list of operators that matches %o
        operators["≠"]=true;
        operators["∨"]=true;
        operators["∧"]=true;
        operators["÷"]=true;
        operators["×"]=true;
        operators["²"]=true;
        operators["³"]=true;
        operators["¬"]=true;
    }
    
    long size() {
        return stack.size();
    }
    
    virtual void reset() {
        x_rules::reset();

        tokenizer.clear();
        disjunctions.clear();
        
        operators.clear();
        operators["≠"]=true;
        operators["∨"]=true;
        operators["∧"]=true;
        operators["÷"]=true;
        operators["×"]=true;
        operators["²"]=true;
        operators["³"]=true;
        operators["¬"]=true;

    }

    void replacemetas(map<string, string>& metalines, string& line) {
        if (line.find("%") == -1 || line.size()==1)
            return;
        
        string rep;
        string fd;
        for (auto& k : metalines) {
            if (line.find(k.first) != -1) {
                fd = k.first;
                rep = k.second;
                line = s_replacestring(line, fd, rep);
            }
        }
    }

    void parserules() {
        /*
         The rules are parsed and the results is stored both in tokenizer and in ruleelements.
         
         ruleelements uses the following binary encoding to define an instruction
         
         xr_char (1)             -> regular character
         xr_meta (2)             -> meta-character (%x)
         xr_chardisjunction (4)  -> disjunction of characters ([..])
         xr_metadisjunction (8)  -> disjunction of metacharacters ({%...})
         xr_negation (16)        -> negation (~)
         xr_plus (32)            -> Kleene operator (+)
         xr_skip (64)            -> Character not stored (-)
         xr_neednext (128)       -> the metacharacter is a . or a ?
         xr_singlebody (256)     -> Single body rule (only one character)
         xr_optional (512)       -> Optional section
         xr_endoptional (1024)   -> End optional section
         
         -> IMPORTANT: rules that start with a regular character are also indexed with their first character in "table".
         rules that start with %d are also indexed on all possibile 10 digits.
         
         -> IMPORTANT: rules should be ordered with rules starting with a character first,
         THEN rules starting with a meta character (%x) or a disjunction after.
         
         -> VERY IMPORTANT: rules starting with a character should always next to the rules sharing the same first character...
         
         The variable firstrule records the position of the first rule starting with a meta-character...
         
         -> IMPORTANT: we keep tracks of rules that are reduced to one single character check in order to process them immediatly
         
         BEWARE: This parser DOES not check if RULES are VALID...
         
         */
        
        char x_actions[]="?aCcdHnopSsr.";

        string line;
        string equal("=");
        string res;
        string sub;

        long k,i, pos;

        short opening;

        char cc;
        char brk=']';
        char typebrk=xr_chardisjunction;
        bool aplus;
        bool neg=false;
        bool addfirstrule;
        unsigned char metakey;
        map<string,string> metalines;
        bool initmetakey=false;
        short mx = 0;
        
        vector<short> e;
        vector<short> stackopen;
        vector<short> stackpar;
        vector<string> rule;

        for (i=0;i<rules.size();i++) {
            line=rules[i];
            
            ruleelements.push_back(e);
            closing.push_back(NULL);

            rule.clear();
            stackopen.clear();
            stackpar.clear();

            if (line[1]==':') { //we detect a meta-rule...
                metakey=line[0];
                line=line.c_str()+2;
                string key="%";
                key+=metakey;
                if (initmetakey)
                    replacemetas(metalines, line);
                
                metalines[key] = line;
                initmetakey=true;
                action.push_back(xr_skiprule);
                tokenizer.push_back(rule);
                continue;
            }
            
            neg=false;
            aplus=false;
            opening=0;
            metakey=0;
            
            //first we look for the = sign at the end of the string...
            
            pos=line.rfind(equal,line.size()-1);
            
            res=line.c_str()+pos+1;
            
            short r=-1;
            if (res!="#")
                r=atoi(STR(res));
            action.push_back(r);
            line=line.substr(0,pos);

            if (initmetakey)
                replacemetas(metalines, line);

            for (long j=0;j<line.size();j++) {
                switch(line[j]) {
                    case '%':
                        //We record the first rule not starting with a character
                        //we check if the character is a metarule...
                        addfirstrule=false;
                        if (!j) {
                            if (line.size()==1) {
                                //The % operator as a single character... %=0
                                if (!metakey && table['%']==255)
                                    table['%']=i;
                                
                                sub="%";
                                rule.push_back(sub);
                                ruleelements[i].push_back(xr_char);
                                break;
                            }

                            addfirstrule=true;
                            if (line[1]=='s' || line[1]=='S' || line[1]=='r') {
                                if (!metakey) {
                                    addfirstrule=false;
                                    if (line[1]!='r') {
                                        if (table[32]==255)
                                            table[32]=i;
                                        if (table[9]==255)
                                            table[9]=i;
                                    }
                                    if (line[1]!='s') {
                                        if (table[10]==255)
                                            table[10]=i;
                                        if (table[13]==255)
                                            table[13]=i;
                                    }
                                }
                            }
                            else
                            //specific case: %d, since we know all possible digits, we can index on them all
                            if (line[1]=='d') {
                                if (!metakey) {
                                    addfirstrule=false;
                                    for (k=48;k<58;k++) {
                                        if (table[(char)k]==255)
                                            table[(char)k]=i;
                                    }
                                }
                            }
                        }

                        cc=line[j+1];

                        if (!neg && strchr(x_actions,cc)==NULL) {
                            //this is a direct comparison
                            if (!j && table[cc]==255)
                                table[cc]=i;
                            sub=cc;
                            rule.push_back(sub);
                            ruleelements[i].push_back(xr_char);
                            j++;
                            break;
                        }
                        
                        if (addfirstrule && firstrule==-1 && !metakey)
                            firstrule=i;

                        sub="%";
                        sub+=cc;
                        if (neg) {
                            rule.back()+=sub;
                            ruleelements[i].back() |= xr_meta;
                            neg=false;
                        }
                        else {
                            rule.push_back(sub);
                            ruleelements[i].push_back(xr_meta);
                        }
                        
                        if (sub[1] == '.' || sub[1] == '?')
                            ruleelements[i].back() |= xr_neednext;
                        j++;
                        break;
                    case '{':
                        brk='}';
                        typebrk=xr_metadisjunction;
                    case '[':
                        k=j+1;
                        while (k<line.size() && line[k]!=brk) k++;
                        if (k==line.size()) {
                            //then it is not a disjunction but a simple character recognition
                            neg=false;
                            sub=line[j];
                            if (!j) {
                                if (!metakey && table[sub[0]] == 255)
                                    table[sub[0]]=i;
                            }
                            rule.push_back(sub);
                            ruleelements[i].push_back(xr_char);
                            brk=']';
                            typebrk=xr_chardisjunction;
                            break;
                        }
                        //We record the first rule not starting with a character
                        if (!j && firstrule==-1 && !metakey)
                            firstrule=i;

                        sub=line.substr(j+1,k-j-1);

                        if (typebrk==xr_chardisjunction)
                            sub+=" ";
                        else {
                            vector<string> vsub;
                            //we split at the " "
                            long d=0,e;
                            string sx;
                            for (e=0;e<sub.size();e++) {
                                if (sub[e]==' ') {
                                    sx=sub.substr(d,e-d);
                                    vsub.push_back(sx);
                                    d=e+1;
                                }
                            }
                            sx=sub.substr(d,e-d);
                            vsub.push_back(sx);
                            convertnumber((long)disjunctions.size(), sub);
                            disjunctions.push_back(vsub);
                        }
                        if (neg) {
                            rule.back()+=sub;
                            ruleelements[i].back() |= typebrk;
                            neg=false;
                        }
                        else {
                            rule.push_back(sub);
                            ruleelements[i].push_back(typebrk);
                        }
                        j=k;
                        brk=']';
                        typebrk=xr_chardisjunction;
                        break;
                    case '+':
                        if (j) { //only if it is not the first character
                            aplus=true;
                            ruleelements[i].back() |= xr_plus;
                            break;
                        }
                    case '-':
                        if (j) { //only if it is not the first character
                            ruleelements[i].back() |= xr_skip;
                            break;
                        }
                    case '~': //only if it is not the first character
                        if (j) { //otherwise, it is a character as the others...
                            neg=true;
                            rule.back()+=line[j];
                            ruleelements[i].back() |= xr_negation;
                            break;
                        }
                    case '(':
                        if (j) {
                            bool found=false;
                            short nb=1;
                            k=j+1;
                            while (k<line.size()) {
                                if (line[k]=='(')
                                    nb++;
                                else
                                    if (line[k]==')') {
                                        nb--;
                                        if (!nb) {
                                            found=true;
                                            break;
                                        }
                                    }
                                k++;
                            }
                            if (found) {
                                sub="(";
                                rule.push_back(sub);
                                mx = ruleelements[i].size();
                                stackopen.push_back(mx);
                                ruleelements[i].push_back(xr_optional);
                                opening++;
                                break;
                            }
                        }
                    case ')':
                        if (j) {
                            if (opening) {
                                sub=")";
                                rule.push_back(sub);
                                stackpar.push_back(stackopen.back());
                                stackpar.push_back(ruleelements[i].size());
                                ruleelements[i].push_back(xr_endoptional);
                                stackopen.pop_back();
                                opening--;
                                break;
                            }
                        }
                    default:
                        neg=false;
                        sub=line[j];
                        if (!j) {
                            if (!metakey && table[sub[0]] == 255)
                                table[sub[0]]=i;
                        }
                        rule.push_back(sub);
                        ruleelements[i].push_back(xr_char);
                }
            }
            
            //one character rules are identified for fast processing (binary code 128)...
            if (rule.size()==1 && !aplus)
                ruleelements[i][0] |= xr_singlebody;
            
            if (stackpar.size()) {
                short* clos = new short[mx+1];
                for (k = 0; k < stackpar.size(); k+=2)
                    clos[stackpar[k]] = stackpar[k+1];
                closing[closing.size()-1] = clos;
            }
            
            tokenizer.push_back(rule);
        }
        rules.clear();
    }

    char check(string& label, short type, char* chr) {
        if (!chr[0])
            return false;

        if (verif(type,xr_char)) {
            if (label==chr)
                return true;
            return false;
        }
        
        if (verif(type,xr_negation)) { //negation
            string sb;
            if (verif(type,xr_metadisjunction)) {
                type=8;
                sb=label.c_str()+3;
            }
            else {
                if (verif(type,xr_metadisjunction)) {
                    type=4;
                    sb=label.c_str()+3;
                }
                else {
                    if (label[3]=='%') {
                        type=2;
                        sb=label.c_str()+3;
                    }
                    else {
                        type=1;
                        sb=label.c_str()+2;
                    }
                }
            }
            
            if (check(sb,type,chr))
                return false;
            type=2;
        }
        
        if (verif(type,xr_metadisjunction)) { // {} brackets
            long j=convertinteger(label);
            for (long i=0;i<disjunctions[j].size();i++) {
                if (check(disjunctions[j][i],2,chr))
                    return true;
            }
            return false;
        }
        
        if (verif(type,xr_chardisjunction)) { // [] brackets
            string sb=chr;
            sb+=" ";
            if (label.find(sb)!= string::npos)
                return true;
            return false;
        }
        
        if (verif(type,xr_meta)) {
            unsigned char car=chr[0];
            //here we extract the rest of the string, which can be longer than 1 if it is a UTF8 character...
            string lb=label.c_str()+1;

            if (label[0]=='#') {
                if (lb[1]=='-') {
                    //an interval...
                    if (car>=lb[0] && car <= lb[2])
                        return true;
                }
                if (chr == lb)
                    return true;
                return false;
            }
            
            switch (label[1]) {
                case '?':
                    if (car==escape)
                        return 2;
                    return true;
                case '.':
                    return true;
                case 'C':
                    lb = chr;
                    if (s_is_upper(lb))
                        return true;
                    return false;
                case 'a':
                    lb = chr;
                    if (car=='_' || car == '#' || s_is_alpha(lb))
                        return true;
                    return false;
                case 'c':
                    lb = chr;
                    if (s_is_lower(lb))
                        return true;
                    return false;
                case 'd':
                    if (c_is_digit(car))
                        return true;
                    return false;
                case 'n': //non-breaking space
                    if (car == 194 && ((unsigned char)chr[1]) == 160)
                        return true;
                    return false;
                case 'p':
                    lb = chr;
                    if (s_is_punctuation(lb))
                        return true;
                    return false;
                case 'o':
                    if (operators.find(chr) != operators.end())
                        return true;
                    return false;
                case 'S':
                    if (car <= 32)
                        return true;
                    return false;
                case 's':
                    if (car == 9 || car == 32)
                        return true;
                    return false;
                case 'r':
                    if (car == 10 || car == 13)
                        return true;
                    return false;
                default:
                    if (lb == chr)
                        return true;
                    return false;
            }
            return false;
        }
        
        return false;
    }
    
    void apply(bool keeppos, vector<string>* vstack, vector<unsigned char>* vtype);
    char loop(short i, Fast_String& token, char* chr, short& r, long& line);
    
    //We tokenize our string...
    virtual void tokenize(string& thestr, bool keeppos=false, vector<string>* vstack=NULL, vector<unsigned char>* vtype=NULL) {
        //only stack is necessary
        if (vstack==NULL)
            stack.clear();
        if (!juststack) {
            stackln.clear();
            if (vtype==NULL)
                stacktype.clear();
            if (keeppos) {
                cpos.clear();
                bpos.clear();
            }
        }
        parcours=thestr;
        apply(keeppos, vstack, vtype);
    }
    
    virtual void tokenize(const char* thestr, bool keeppos=false, vector<string>* vstack=NULL, vector<unsigned char>* vtype=NULL) {
        //only stack is necessary
        if (vstack==NULL)
            stack.clear();
        if (!juststack) {
            stackln.clear();
            if (vtype==NULL)
                stacktype.clear();
            if (keeppos) {
                cpos.clear();
                bpos.clear();
            }
        }
        parcours=thestr;
        apply(keeppos,vstack, vtype);
    }
    
};


class x_wreading : public x_rules {
public:
    vector<wstring> stack;
    vector<vector<wstring> > disjunctions;

    hmap<wstring, bool> operators;
    vector<vector<wstring> > tokenizer;

    x_wreading() {
        operators[L"≠"]=true;
        operators[L"∨"]=true;
        operators[L"∧"]=true;
        operators[L"÷"]=true;
        operators[L"×"]=true;
        operators[L"²"]=true;
        operators[L"³"]=true;
        operators[L"¬"]=true;
    }

    virtual void reset() {
        x_rules::reset();
        
        tokenizer.clear();
        disjunctions.clear();
        
        operators.clear();
        operators[L"≠"]=true;
        operators[L"∨"]=true;
        operators[L"∧"]=true;
        operators[L"÷"]=true;
        operators[L"×"]=true;
        operators[L"²"]=true;
        operators[L"³"]=true;
        operators[L"¬"]=true;
    }

    void replacemetas(map<wstring, wstring>& metalines, wstring& line) {
        if (line.find(L"%") == -1 || line.size()==1)
            return;

        wstring rep;
        wstring fd;
        for (auto& k : metalines) {
            if (line.find(k.first) != -1) {
                fd = k.first;
                rep = k.second;
                line = s_replacestring(line, fd, rep);
            }
        }
    }
    
    void parserules() {
        
        /*
         
         The rules are parsed and the results is stored both in tokenizer and in ruleelements.
         
         ruleelements uses the following binary encoding to define an instruction
         
         xr_char (1)             -> regular character
         xr_meta (2)             -> meta-character (%x)
         xr_chardisjunction (4)  -> disjunction of characters ([..])
         xr_metadisjunction (8)  -> disjunction of metacharacters ({%...})
         xr_negation (16)        -> negation (~)
         xr_plus (32)            -> Kleene operator (+)
         xr_skip (64)            -> Character not stored (-)
         xr_neednext (128)       -> the metacharacter is a . or a ?
         xr_singlebody (256)     -> Single body rule (only one character)
         xr_optional (512)       -> Optional section
         xr_endoptional (1024)   -> End optional section
         
         -> IMPORTANT: rules that start with a regular character are also indexed with their first character in "table".
         rules that start with %d are also indexed on all possibile 10 digits.
         
         -> IMPORTANT: rules should be ordered with rules starting with a character first,
         THEN rules starting with a meta character (%x) or a disjunction after.
         
         -> VERY IMPORTANT: rules starting with a character should always next to the rules sharing the same first character...
         
         The variable firstrule records the position of the first rule starting with a meta-character...
         
         -> IMPORTANT: we keep tracks of rules that are reduced to one single character check in order to process them immediatly
         
         BEWARE: This parser DOES not check if RULES are VALID...
         
         */
        
        char x_actions[]="?aCcdHnopSsr.";
        
        wstring line;
        wstring sub;
        wstring equal(L"=");
        wstring res;

        long k;
        long i, pos;

        wchar_t brk=L']', metakey;
        wchar_t cc;
        
        short opening;
        short mx = 0;
        
        char typebrk=xr_chardisjunction;
        bool aplus;
        bool neg=false;
        bool addfirstrule;
        map<wstring, wstring> metalines;
        bool initmetakey=false;
        vector<short> e;
        vector<short> stackopen;
        vector<short> stackpar;
        vector<wstring> rule;


        for (i=0;i<rules.size();i++) {
            sc_utf8_to_unicode(line, USTR(rules[i]), rules[i].size());
            
            ruleelements.push_back(e);
            closing.push_back(NULL);

            rule.clear();
            stackopen.clear();
            stackpar.clear();

            if (line[1]==':') { //we detect a meta-rule...
                metakey=line[0];
                line=line.c_str()+2;
                wstring key=L"%";
                key+=metakey;
                if (initmetakey)
                    replacemetas(metalines,line);
                metalines[key] = line;
                initmetakey=true;
                action.push_back(xr_skiprule);
                tokenizer.push_back(rule);
                continue;
            }
            
            
            neg=false;
            aplus=false;
            opening=0;
            metakey=0;

            short r=-1;

            //first we look for the = sign at the end of the string...
            
            pos=line.rfind(equal,line.size()-1);
            
            res=line.c_str()+pos+1;
            
            if (res!=L"#")
                r=convertinteger(res);
            
            action.push_back(r);
            line=line.substr(0,pos);

            if (initmetakey)
                replacemetas(metalines,line);

            for (long j=0;j<line.size();j++) {

                switch(line[j]) {
                    case L'%':
                        addfirstrule=false;
                        //We record the first rule not starting with a character
                        if (!j) {
                            if (line.size()==1) {
                                //The % operator as a single character... %=0
                                if (!metakey && table['%']==255)
                                    table['%']=i;
                                
                                sub=L"%";
                                rule.push_back(sub);
                                ruleelements[i].push_back(xr_char);
                                break;
                            }
                            
                            addfirstrule=true;

                            if (line[1]=='s' || line[1]=='S' || line[1]=='r') {
                                if (!metakey) {
                                    addfirstrule=false;
                                    if (line[1]!='r') {
                                        if (table[32]==255)
                                            table[32]=i;
                                        if (table[9]==255)
                                            table[9]=i;
                                    }
                                    if (line[1]!='s') {
                                        if (table[10]==255)
                                            table[10]=i;
                                        if (table[13]==255)
                                            table[13]=i;
                                    }
                                }
                            }
                            else
                            //specific case: %d, since we know all possible digits, we can index on them all
                            if (line[1]=='d') {
                                if (!metakey) {
                                    addfirstrule=false;
                                    for (k=48;k<58;k++) {
                                        if (table[(char)k]==255)
                                            table[(char)k]=i;
                                    }
                                }
                            }
                        }
                        
                        cc=line[j+1];

                        if (!neg && cc < 128 && strchr(x_actions,cc)==NULL) {
                            //this is a direct comparison
                            if (!j && table[cc]==255)
                                table[cc]=i;
                            sub=cc;
                            rule.push_back(sub);
                            ruleelements[i].push_back(xr_char);
                            j++;
                            break;
                        }
                        
                        if (addfirstrule && firstrule==-1 && !metakey)
                            firstrule=i;

                        sub= L"%";
                        sub+=cc;
                        if (neg) {
                            rule.back()+=sub;
                            ruleelements[i].back() |= xr_meta;
                            neg=false;
                        }
                        else {
                            rule.push_back(sub);
                            ruleelements[i].push_back(xr_meta);
                        }
                        
                        if (sub[1] == '.' || sub[1] == '?')
                            ruleelements[i].back() |= xr_neednext;
                        j++;
                        break;
                    case L'{':
                        brk='}';
                        typebrk=xr_metadisjunction;
                    case L'[':
                        k=j+1;
                        while (k<line.size() && line[k]!=brk) k++;
                        if (k==line.size()) {
                            //then it is not a disjunction but a simple character recognition
                            neg=false;
                            sub=line[j];
                            if (!j && sub[0]<256) {
                                if (!metakey && table[sub[0]] == 255)
                                    table[sub[0]]=i;
                            }
                            rule.push_back(sub);
                            ruleelements[i].push_back(xr_char);
                            brk=']';
                            typebrk=xr_chardisjunction;
                            break;
                        }
                        //We record the first rule not starting with a character
                        if (!j && firstrule==-1 && !metakey)
                            firstrule=i;
                        
                        sub=line.substr(j+1,k-j-1);

                        if (typebrk==xr_chardisjunction)
                            sub+=L" ";
                        else {
                            vector<wstring> vsub;
                            //we split at the " "
                            long d=0,e;
                            wstring sx;
                            for (e=0;e<sub.size();e++) {
                                if (sub[e]==' ') {
                                    sx=sub.substr(d,e-d);
                                    vsub.push_back(sx);
                                    d=e+1;
                                }
                            }
                            sx=sub.substr(d,e-d);
                            vsub.push_back(sx);
                            sub = wconvertfromnumber((long)disjunctions.size());
                            disjunctions.push_back(vsub);
                        }
                        
                        if (neg) {
                            rule.back()+=sub;
                            ruleelements[i].back() |= typebrk;
                            neg=false;
                        }
                        else {
                            rule.push_back(sub);
                            ruleelements[i].push_back(typebrk);
                        }
                        j=k;
                        brk=']';
                        typebrk=xr_chardisjunction;
                        break;
                    case L'+':
                        if (j) { //only if it is not the first character
                            aplus=true;
                            ruleelements[i].back() |= xr_plus;
                            break;
                        }
                    case L'-':
                        if (j) { //only if it is not the first character
                            ruleelements[i].back() |= xr_skip;
                            break;
                        }
                    case L'~': //only if it is not the first character
                        if (j) { //otherwise, it is a character as the others...
                            neg=true;
                            rule.back()+=line[j];
                            ruleelements[i].back() |= xr_negation;
                            break;
                        }
                    case L'(':
                        if (j) {
                            bool found=false;
                            short nb=1;
                            k=j+1;
                            while (k<line.size()) {
                                if (line[k]=='(')
                                    nb++;
                                else
                                    if (line[k]==')') {
                                        nb--;
                                        if (!nb) {
                                            found=true;
                                            break;
                                        }
                                    }
                                k++;
                            }
                            if (found) {
                                sub=L"(";
                                rule.push_back(sub);
                                mx = ruleelements[i].size();
                                stackopen.push_back(mx);
                                ruleelements[i].push_back(xr_optional);
                                opening++;
                                break;
                            }
                        }
                    case L')':
                        if (j) {
                            if (opening) {
                                sub=L")";
                                rule.push_back(sub);
                                stackpar.push_back(stackopen.back());
                                stackpar.push_back(ruleelements[i].size());
                                ruleelements[i].push_back(xr_endoptional);
                                stackopen.pop_back();
                                opening--;
                                break;
                            }
                        }
                    default:
                        neg=false;
                        sub=line[j];
                        if (!j && sub[0]<256) {
                            if (!metakey && table[sub[0]] == 255)
                                table[sub[0]]=i;
                        }
                        rule.push_back(sub);
                        ruleelements[i].push_back(xr_char);
                }
            }
            
            //one character rules are identified for fast processing (binary code 128)...
            if (rule.size()==1 && !aplus)
                ruleelements[i][0] |= xr_singlebody;
            tokenizer.push_back(rule);
            if (stackpar.size()) {
                short* clos = new short[mx+1];
                for (k = 0; k < stackpar.size(); k+=2)
                    clos[stackpar[k]] = stackpar[k+1];
                closing[closing.size()-1] = clos;
            }
        }
        rules.clear();
    }

    char check(wstring& label, short type, wchar_t* chr) {
        if (!chr[0])
            return false;
        
        if (verif(type,xr_char)) {
            if (label == chr)
                return true;
            return false;
        }
        
        if (verif(type,xr_negation)) { //negation
            wstring sb;
            if (verif(type,xr_metadisjunction)) {
                type=8;
                sb=label.c_str()+3;
            }
            else {
                if (verif(type,xr_metadisjunction)) {
                    type=4;
                    sb=label.c_str()+3;
                }
                else {
                    if (label[3]=='%') {
                        type=2;
                        sb=label.c_str()+3;
                    }
                    else {
                        type=1;
                        sb=label.c_str()+2;
                    }
                }
            }
            if (check(sb,type,chr))
                return false;
            type=2;
        }
        
        if (verif(type,xr_metadisjunction)) { // {} brackets
            long j=convertinteger(label);
            for (long i=0;i<disjunctions[j].size();i++) {
                if (check(disjunctions[j][i],2,chr))
                    return true;
            }
            return false;
        }

        if (verif(type,xr_chardisjunction)) { // [] brackets
            wstring sb=chr;
            sb+=L" ";
            if (label.find(sb)!= string::npos)
                return true;
            return false;
        }
        
        if (verif(type,xr_meta)) {
            wchar_t lb=label[1];
            wchar_t car=chr[0];
            
            if (label[0]==L'#') {
                if (label[2]=='-') {
                    if (car>=lb && car <= label[3])
                        return true;
                    return false;
                }
                if (car == lb)
                    return true;
                return false;
            }
            
            switch (lb) {
                case '?':
                    if (car==escape)
                        return 2;
                    return true;
                case '.':
                    return true;
                case 'C':
                    if (c_is_upper(car))
                        return true;
                    return false;
                case 'a':
                    if (car=='_' || car == '#' || c_is_alpha(car))
                        return true;
                    return false;
                case 'c':
                    if (c_is_lower(car))
                        return true;
                    return false;
                case 'd':
                    if (c_is_digit(car))
                        return true;
                    return false;
                case 'n': //non-breaking space
                    if (car == 160)
                        return true;
                    return false;
                case 'p':
                    if (c_is_punctuation(car))
                        return true;
                    return false;
                case 'o':
                    if (operators.find(chr) != operators.end())
                        return true;
                    return false;
                case 'S':
                    if (car <= 32)
                        return true;
                    return false;
                case 's':
                    if (car == 9 || car == 32)
                        return true;
                    return false;
                case 'r':
                    if (car == 10 || car == 13)
                        return true;
                    return false;
                default:
                    if (lb == (uchar)car)
                        return true;
                    return false;
            }
            return false;
        }
        
        return false;
    }
    
    void apply(wstring& toparse, bool keeppos, vector<wstring>* vstack, vector<unsigned char>* vtype);
    char loop(wstring& toparse, short i, wchar_t* token, wchar_t* chr, long& itoken, short& r, long& line, long& posc);

    wstring next(wstring& w, long& pos, long& l) {
        if (pos>=w.size())
            return L"";
        
        if (w[pos]==L'\n')
            l++;
        wstring res;
        res=w[pos++];
#ifdef WSTRING_IS_UTF16
		if (checklargeutf16(res[0]))
			res += w[pos++];
#endif
        return res;
    }
    
    void getnext(wstring& w, wchar_t* res, long& pos, long& l) {
        if (pos>=w.size()) {
            res[0] =  0;
            return;
        }
        
        if (w[pos]==L'\n')
            l++;
        res[0] = w[pos++];
#ifdef WSTRING_IS_UTF16
        if (checklargeutf16(res[0]))
            res[1] = w[pos++];
        else
            res[1] = 0;
#endif
    }
    
    void getnext(wstring& w, wchar_t* res, long& pos) {
        if (pos>=w.size()) {
            res[0] =  0;
            return;
        }
        
        res[0] = w[pos++];
#ifdef WSTRING_IS_UTF16
        if (checklargeutf16(res[0]))
            res[1] = w[pos++];
        else
            res[1] = 0;
#endif
    }
    

    virtual void tokenize(wstring& thestr, bool keeppos=false, vector<wstring>* vstack=NULL, vector<unsigned char>* vtype=NULL) {
        //only stack is necessary
        if (vstack==NULL)
            stack.clear();
        if (!juststack) {
            stackln.clear();
            if (vtype==NULL)
                stacktype.clear();
            if (keeppos)
                cpos.clear();
        }
        apply(thestr, keeppos, vstack, vtype);
    }

};

class x_localparse : public x_reading {
public:
    
    
    //We only parse digits and labels. No comments or strings extracted...
    void setrules() {
        //Spaces, skipped in the parsing string
        rules.push_back(" =#");                         //0     space (not kept)
        rules.push_back("\t=#");                        //1     tab (not kept)
        rules.push_back("\n=#");                        //2     cr (not kept)
        rules.push_back("\r=#");                        //3     cr (not kept)
        
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
        rules.push_back("%=0");                         //20    %
        rules.push_back("<=0");                         //21    <
        rules.push_back(">=0");                         //22    >
        rules.push_back("|=0");                         //23    |
        rules.push_back("&=0");                         //24    &
        rules.push_back(":=0");                         //25    :
        rules.push_back("/=0");                         //26    /
        rules.push_back("\"=0");                        //27    "
        rules.push_back("\\=0");                        //27    \
        rules.push_back("'=0");                         //28    '
        rules.push_back("$=0");                         //29    $
        rules.push_back("#=0");                         //30    #
        rules.push_back("?=0");                         //31    ?

        rules.push_back("0x%1+(.%1+)([p P]([- +])%d+)=3"); //32 hexadecimal
        rules.push_back("%d+(.%d+)([e E]([- +])%d+)=3");    //33    exponential digits
        
        // Rules start here
        rules.push_back("{%a %d %H}+=4");               //34    label a combination of alpha, digits and hangul characters
        rules.push_back("%n=#");                        //1     non-breaking space (not kept)
        rules.push_back("%o=0");                        //36    operators
        rules.push_back("%p=0");                        //35    punctuation
        rules.push_back("%.=4");                        //37    An unknown UTF8 token separated with spaces, punctuation or operators...
    }
};

#endif



