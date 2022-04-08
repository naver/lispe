#include <ostream>
#include <iostream>

#include "emojis_alone.h"

int main(int argc, char *argv[]) {

    //Les chaines UTF-8 sont de type: std::string

    string strvalue = "éèà123👨‍⚕️👩🏾‍🚀🐕‍🦺";
    //e est un objet de type Emojis
    Emojis e;

    //strvalue est une chaine de type std::string
    long i;
    //La taille de notre chaine en éléments de type char
    long sz = strvalue.size();

    //une variable de travail
    string localvalue;

    for (i = 0; i < sz; i++) {
        //si le caractère à la position courante est un emoji
        //alors localvalue le contient.
        //i pointe dès lors sur le dernier caractère de la séquence
        if (!e.get(strvalue, localvalue, i))
            get_one_char(strvalue, localvalue, i); //c'est la différence principale
        std::cout << localvalue.c_str() << std::endl;

    }
}

