#include <ostream>
#include <iostream>

#include "emojis_alone.h"

int main(int argc, char *argv[]) {

    // UTF-8 strings are of type: std::string

    string strvalue = "éèà123👨‍⚕️👩🏾‍🚀🐕‍🦺";
    //e is an object of type Emojis...
    Emojis e;

    //strvalue is a string of type std::string
    long i;
    //The size of our string in elements of type char
    long sz = strvalue.size();

    //a working variable
    string localvalue;

    for (i = 0; i < sz; i++) {
        //if the character at the current position is an emoji
        //then localvalue contains it.
        //i then points to the last character of the sequence
        if (!e.get(strvalue, localvalue, i))
            get_one_char(strvalue, localvalue, i); // this is the main difference
        std::cout << localvalue.c_str() << std::endl;

    }
}
