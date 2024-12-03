/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  lispe_curl.h

#ifndef lispe_curl_h
#define lispe_curl_h

#include <curl/curl.h>
#include "lispe.h"

class Lispe_curl : public Element {
public:
    LispE* lisp;
    
    Element* function;
    Element* argument;

    string data;

    CURL *curl;
    struct curl_slist *headers;
    
    char* urlbuffer;
    int urlsize;

    Lispe_curl(LispE* l, short idcurl) : Element(idcurl) {
        function = NULL;
        argument = NULL;
        lisp = l;
        curl = curl_easy_init();
        urlsize = 2048;
        urlbuffer = (char*)malloc(urlsize);
        headers = NULL;
    }

    ~Lispe_curl() {
        if (urlbuffer != NULL)
            free(urlbuffer);
        if (curl != NULL)
            curl_easy_cleanup(curl);
        if (argument != NULL)
            argument->decrement();
        if (headers != NULL)
            curl_slist_free_all(headers);
    }
    
    void clear() {
        data = "";
    }
    
    Element* eval(LispE* lisp);

    //Nous utilisons cette instruction pour renvoyer une description de l'instruction
    //en effet, il suffit de faire: (print getenv) pour obtenir cette information
    wstring asString(LispE* lisp);
    
    string toString(LispE* lisp) {
        return data;
    }
    
    Element* protected_index(LispE*, Element* k);
    Element* extraction(LispE* lisp, List* l);
};

typedef enum {curl_curl, curl_passwrd, curl_proxy, curl_url, curl_options, curl_execute, curl_function, curl_headers} curl_type;

class Lispe_curl_function : public Element {
public:
    curl_type curltype;
    short idcurl;
    
    Lispe_curl_function(LispE* lisp, curl_type c) : curltype(c), Element(l_lib) {
        wstring nom = L"curl_";
        idcurl = lisp->encode(nom);
    }

    Element* MethodProxy(LispE* lisp);
    Element* MethodPWD(LispE* lisp);
    Element* MethodURL(LispE* lisp);
    Element* MethodExecute(LispE* lisp);
    Element* MethodOptions(LispE* lisp);
    Element* MethodCallback(LispE* lisp);
    Element* MethodHeaders(LispE* lisp);
    
    Element* eval(LispE* lisp);

    Element* errormsg(CURLcode res) {
        const char* errmsg = curl_easy_strerror(res);
        char ch[1024];
        sprintf_s(ch, 1024, "Error: URL(%d): %s", res, errmsg);
        return new Error(ch);
    }

    //Nous utilisons cette instruction pour renvoyer une description de l'instruction
    //en effet, il suffit de faire: (print getenv) pour obtenir cette information
    wstring asString(LispE* lisp);
};

#endif


