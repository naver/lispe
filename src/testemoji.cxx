#include <ostream>
#include <iostream>

#include "emojis_alone.h"
using std::cout;
using std::endl;

void parseutf8(Emojis& e, string strvalue) {
    // UTF-8 strings are of type: std::string
    //a working variable
    string localvalue;

    cout << endl << "We display the different characters" << endl;
    for (long i = 0; i < strvalue.size(); i++) {
        //if the character at the current position is an emoji
        //then localvalue contains it.
        //i then points to the last character of the sequence
        if (!e.get(strvalue, localvalue, i))
            get_one_char(strvalue, localvalue, i); // this is the main difference
        cout << localvalue.c_str() << endl;

    }

}

void parseutf32(Emojis& e, u32string strvalue) {
    //a working variable
    u32string localvalue;
    
    cout <<endl << "We display the different codes for each character in this UTF-32 string" << endl;

    for (long i = 0; i < strvalue.size(); i++) {
        //if the character at the current position is an emoji
        //then localvalue contains it.
        //i then points to the last character of the sequence
        if (!e.get(strvalue, localvalue, i))
            localvalue = strvalue[i];
        //In this case we display the list of codes...
        for (long u = 0; u < localvalue.size(); u++)
            cout << localvalue[u] << " ";
        cout << endl;
    }
}

void parseWithConversion(Emojis& e, string str) {
    u32string strvalue;
    s_utf8_to_unicode(strvalue, str);
    cout <<endl << "First we convert to UTF-32" << endl;
    parseutf32(e, strvalue);
    
    string strbis;
    cout << endl << "We then convert back to UTF-8" << endl;
    s_unicode_to_utf8(strbis, strvalue);
    parseutf8(e, strbis);
}

int main(int argc, char *argv[]) {
    //e is an object of type Emojis...
    Emojis e;

    parseutf8(e, "éèà123👨‍⚕️👩🏾‍🚀🐕‍🦺");
    
    //UTF-32 strings are preceded with U
    parseutf32(e, U"éèà123👨‍⚕️👩🏾‍🚀🐕‍🦺");
    parseWithConversion(e, "éèà123👨‍⚕️👩🏾‍🚀🐕‍🦺");
    cout << endl;
}
