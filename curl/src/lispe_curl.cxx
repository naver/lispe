/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  lispe_curl.cxx


/*
An extension to lispee to use cURL
 */

#ifdef WIN32
#include <windows.h>
#endif

#include "lispe.h"
#include "lispe_curl.h"
#include <map>

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif

#define strcpy_s(a,c,b) strncpy(a,b,c)

static std::map<string, CURLoption> curloptions;

static void Init() {
    static bool init = false;
    if (!init) {
        curloptions["CURLOPT_ACCEPTTIMEOUT_MS"] = CURLOPT_ACCEPTTIMEOUT_MS;
        curloptions["CURLOPT_ACCEPT_ENCODING"] = CURLOPT_ACCEPT_ENCODING;
        curloptions["CURLOPT_ADDRESS_SCOPE"] = CURLOPT_ADDRESS_SCOPE;
        curloptions["CURLOPT_APPEND"] = CURLOPT_APPEND;
        curloptions["CURLOPT_AUTOREFERER"] = CURLOPT_AUTOREFERER;
        curloptions["CURLOPT_BUFFERSIZE"] = CURLOPT_BUFFERSIZE;
        curloptions["CURLOPT_CAINFO"] = CURLOPT_CAINFO;
        curloptions["CURLOPT_CAPATH"] = CURLOPT_CAPATH;
        curloptions["CURLOPT_CERTINFO"] = CURLOPT_CERTINFO;
        curloptions["CURLOPT_CHUNK_BGN_FUNCTION"] = CURLOPT_CHUNK_BGN_FUNCTION;
        curloptions["CURLOPT_CHUNK_DATA"] = CURLOPT_CHUNK_DATA;
        curloptions["CURLOPT_CHUNK_END_FUNCTION"] = CURLOPT_CHUNK_END_FUNCTION;
        curloptions["CURLOPT_CLOSESOCKETDATA"] = CURLOPT_CLOSESOCKETDATA;
        curloptions["CURLOPT_CLOSESOCKETFUNCTION"] = CURLOPT_CLOSESOCKETFUNCTION;
        curloptions["CURLOPT_CONNECTTIMEOUT"] = CURLOPT_CONNECTTIMEOUT;
        curloptions["CURLOPT_CONNECTTIMEOUT_MS"] = CURLOPT_CONNECTTIMEOUT_MS;
        curloptions["CURLOPT_CONNECT_ONLY"] = CURLOPT_CONNECT_ONLY;
        curloptions["CURLOPT_CONV_FROM_NETWORK_FUNCTION"] = CURLOPT_CONV_FROM_NETWORK_FUNCTION;
        curloptions["CURLOPT_CONV_FROM_UTF8_FUNCTION"] = CURLOPT_CONV_FROM_UTF8_FUNCTION;
        curloptions["CURLOPT_CONV_TO_NETWORK_FUNCTION"] = CURLOPT_CONV_TO_NETWORK_FUNCTION;
        curloptions["CURLOPT_COOKIE"] = CURLOPT_COOKIE;
        curloptions["CURLOPT_COOKIEFILE"] = CURLOPT_COOKIEFILE;
        curloptions["CURLOPT_COOKIEJAR"] = CURLOPT_COOKIEJAR;
        curloptions["CURLOPT_COOKIELIST"] = CURLOPT_COOKIELIST;
        curloptions["CURLOPT_COOKIESESSION"] = CURLOPT_COOKIESESSION;
        curloptions["CURLOPT_COPYPOSTFIELDS"] = CURLOPT_COPYPOSTFIELDS;
        curloptions["CURLOPT_CRLF"] = CURLOPT_CRLF;
        curloptions["CURLOPT_CRLFILE"] = CURLOPT_CRLFILE;
        curloptions["CURLOPT_CUSTOMREQUEST"] = CURLOPT_CUSTOMREQUEST;
        curloptions["CURLOPT_DEBUGDATA"] = CURLOPT_DEBUGDATA;
        curloptions["CURLOPT_DEBUGFUNCTION"] = CURLOPT_DEBUGFUNCTION;
        curloptions["CURLOPT_DIRLISTONLY"] = CURLOPT_DIRLISTONLY;
        curloptions["CURLOPT_DNS_CACHE_TIMEOUT"] = CURLOPT_DNS_CACHE_TIMEOUT;
        curloptions["CURLOPT_DNS_SERVERS"] = CURLOPT_DNS_SERVERS;
        curloptions["CURLOPT_DNS_USE_GLOBAL_CACHE"] = CURLOPT_DNS_USE_GLOBAL_CACHE;
        curloptions["CURLOPT_EGDSOCKET"] = CURLOPT_EGDSOCKET;
        curloptions["CURLOPT_ERRORBUFFER"] = CURLOPT_ERRORBUFFER;
        curloptions["CURLOPT_FAILONERROR"] = CURLOPT_FAILONERROR;
        curloptions["CURLOPT_FILETIME"] = CURLOPT_FILETIME;
        curloptions["CURLOPT_FNMATCH_DATA"] = CURLOPT_FNMATCH_DATA;
        curloptions["CURLOPT_FNMATCH_FUNCTION"] = CURLOPT_FNMATCH_FUNCTION;
        curloptions["CURLOPT_FOLLOWLOCATION"] = CURLOPT_FOLLOWLOCATION;
        curloptions["CURLOPT_FORBID_REUSE"] = CURLOPT_FORBID_REUSE;
        curloptions["CURLOPT_FRESH_CONNECT"] = CURLOPT_FRESH_CONNECT;
        curloptions["CURLOPT_FTPPORT"] = CURLOPT_FTPPORT;
        curloptions["CURLOPT_FTPSSLAUTH"] = CURLOPT_FTPSSLAUTH;
        curloptions["CURLOPT_FTP_ACCOUNT"] = CURLOPT_FTP_ACCOUNT;
        curloptions["CURLOPT_FTP_ALTERNATIVE_TO_USER"] = CURLOPT_FTP_ALTERNATIVE_TO_USER;
        curloptions["CURLOPT_FTP_CREATE_MISSING_DIRS"] = CURLOPT_FTP_CREATE_MISSING_DIRS;
        curloptions["CURLOPT_FTP_FILEMETHOD"] = CURLOPT_FTP_FILEMETHOD;
        curloptions["CURLOPT_FTP_RESPONSE_TIMEOUT"] = CURLOPT_FTP_RESPONSE_TIMEOUT;
        curloptions["CURLOPT_FTP_SKIP_PASV_IP"] = CURLOPT_FTP_SKIP_PASV_IP;
        curloptions["CURLOPT_FTP_SSL_CCC"] = CURLOPT_FTP_SSL_CCC;
        curloptions["CURLOPT_FTP_USE_EPRT"] = CURLOPT_FTP_USE_EPRT;
        curloptions["CURLOPT_FTP_USE_EPSV"] = CURLOPT_FTP_USE_EPSV;
        curloptions["CURLOPT_FTP_USE_PRET"] = CURLOPT_FTP_USE_PRET;
        curloptions["CURLOPT_GSSAPI_DELEGATION"] = CURLOPT_GSSAPI_DELEGATION;
        curloptions["CURLOPT_HEADER"] = CURLOPT_HEADER;
        curloptions["CURLOPT_HEADERDATA"] = CURLOPT_HEADERDATA;
        curloptions["CURLOPT_HEADERFUNCTION"] = CURLOPT_HEADERFUNCTION;
        curloptions["CURLOPT_HTTP200ALIASES"] = CURLOPT_HTTP200ALIASES;
        curloptions["CURLOPT_HTTPAUTH"] = CURLOPT_HTTPAUTH;
        curloptions["CURLOPT_HTTPGET"] = CURLOPT_HTTPGET;
        curloptions["CURLOPT_HTTPHEADER"] = CURLOPT_HTTPHEADER;
        curloptions["CURLOPT_HTTPPOST"] = CURLOPT_HTTPPOST;
        curloptions["CURLOPT_HTTPPROXYTUNNEL"] = CURLOPT_HTTPPROXYTUNNEL;
        curloptions["CURLOPT_HTTP_CONTENT_DECODING"] = CURLOPT_HTTP_CONTENT_DECODING;
        curloptions["CURLOPT_HTTP_TRANSFER_DECODING"] = CURLOPT_HTTP_TRANSFER_DECODING;
        curloptions["CURLOPT_HTTP_VERSION"] = CURLOPT_HTTP_VERSION;
        curloptions["CURLOPT_IGNORE_CONTENT_LENGTH"] = CURLOPT_IGNORE_CONTENT_LENGTH;
        curloptions["CURLOPT_INFILESIZE"] = CURLOPT_INFILESIZE;
        curloptions["CURLOPT_INFILESIZE_LARGE"] = CURLOPT_INFILESIZE_LARGE;
        curloptions["CURLOPT_INTERLEAVEDATA"] = CURLOPT_INTERLEAVEDATA;
        curloptions["CURLOPT_INTERLEAVEFUNCTION"] = CURLOPT_INTERLEAVEFUNCTION;
        curloptions["CURLOPT_IOCTLDATA"] = CURLOPT_IOCTLDATA;
        curloptions["CURLOPT_IOCTLFUNCTION"] = CURLOPT_IOCTLFUNCTION;
        curloptions["CURLOPT_IPRESOLVE"] = CURLOPT_IPRESOLVE;
        curloptions["CURLOPT_ISSUERCERT"] = CURLOPT_ISSUERCERT;
        curloptions["CURLOPT_KEYPASSWD"] = CURLOPT_KEYPASSWD;
        curloptions["CURLOPT_KRBLEVEL"] = CURLOPT_KRBLEVEL;
        curloptions["CURLOPT_LOCALPORT"] = CURLOPT_LOCALPORT;
        curloptions["CURLOPT_LOCALPORTRANGE"] = CURLOPT_LOCALPORTRANGE;
        curloptions["CURLOPT_LOW_SPEED_LIMIT"] = CURLOPT_LOW_SPEED_LIMIT;
        curloptions["CURLOPT_LOW_SPEED_TIME"] = CURLOPT_LOW_SPEED_TIME;
        curloptions["CURLOPT_MAIL_FROM"] = CURLOPT_MAIL_FROM;
        curloptions["CURLOPT_MAIL_RCPT"] = CURLOPT_MAIL_RCPT;
        curloptions["CURLOPT_MAXCONNECTS"] = CURLOPT_MAXCONNECTS;
        curloptions["CURLOPT_MAXFILESIZE"] = CURLOPT_MAXFILESIZE;
        curloptions["CURLOPT_MAXFILESIZE_LARGE"] = CURLOPT_MAXFILESIZE_LARGE;
        curloptions["CURLOPT_MAXREDIRS"] = CURLOPT_MAXREDIRS;
        curloptions["CURLOPT_MAX_RECV_SPEED_LARGE"] = CURLOPT_MAX_RECV_SPEED_LARGE;
        curloptions["CURLOPT_MAX_SEND_SPEED_LARGE"] = CURLOPT_MAX_SEND_SPEED_LARGE;
        curloptions["CURLOPT_NETRC"] = CURLOPT_NETRC;
        curloptions["CURLOPT_NETRC_FILE"] = CURLOPT_NETRC_FILE;
        curloptions["CURLOPT_NEW_DIRECTORY_PERMS"] = CURLOPT_NEW_DIRECTORY_PERMS;
        curloptions["CURLOPT_NEW_FILE_PERMS"] = CURLOPT_NEW_FILE_PERMS;
        curloptions["CURLOPT_NOBODY"] = CURLOPT_NOBODY;
        curloptions["CURLOPT_NOPROGRESS"] = CURLOPT_NOPROGRESS;
        curloptions["CURLOPT_NOPROXY"] = CURLOPT_NOPROXY;
        curloptions["CURLOPT_NOSIGNAL"] = CURLOPT_NOSIGNAL;
        curloptions["CURLOPT_OPENSOCKETDATA"] = CURLOPT_OPENSOCKETDATA;
        curloptions["CURLOPT_OPENSOCKETFUNCTION"] = CURLOPT_OPENSOCKETFUNCTION;
        curloptions["CURLOPT_PASSWORD"] = CURLOPT_PASSWORD;
        curloptions["CURLOPT_PORT"] = CURLOPT_PORT;
        curloptions["CURLOPT_POST"] = CURLOPT_POST;
        curloptions["CURLOPT_POSTFIELDS"] = CURLOPT_POSTFIELDS;
        curloptions["CURLOPT_POSTFIELDSIZE"] = CURLOPT_POSTFIELDSIZE;
        curloptions["CURLOPT_POSTFIELDSIZE_LARGE"] = CURLOPT_POSTFIELDSIZE_LARGE;
        curloptions["CURLOPT_POSTQUOTE"] = CURLOPT_POSTQUOTE;
        curloptions["CURLOPT_POSTREDIR"] = CURLOPT_POSTREDIR;
        curloptions["CURLOPT_PREQUOTE"] = CURLOPT_PREQUOTE;
        curloptions["CURLOPT_PRIVATE"] = CURLOPT_PRIVATE;
        curloptions["CURLOPT_PROGRESSDATA"] = CURLOPT_PROGRESSDATA;
        curloptions["CURLOPT_PROGRESSFUNCTION"] = CURLOPT_PROGRESSFUNCTION;
        curloptions["CURLOPT_PROTOCOLS"] = CURLOPT_PROTOCOLS;
        curloptions["CURLOPT_PROXY"] = CURLOPT_PROXY;
        curloptions["CURLOPT_PROXYAUTH"] = CURLOPT_PROXYAUTH;
        curloptions["CURLOPT_PROXYPASSWORD"] = CURLOPT_PROXYPASSWORD;
        curloptions["CURLOPT_PROXYPORT"] = CURLOPT_PROXYPORT;
        curloptions["CURLOPT_PROXYTYPE"] = CURLOPT_PROXYTYPE;
        curloptions["CURLOPT_PROXYUSERNAME"] = CURLOPT_PROXYUSERNAME;
        curloptions["CURLOPT_PROXYUSERPWD"] = CURLOPT_PROXYUSERPWD;
        curloptions["CURLOPT_PROXY_TRANSFER_MODE"] = CURLOPT_PROXY_TRANSFER_MODE;
        curloptions["CURLOPT_PUT"] = CURLOPT_PUT;
        curloptions["CURLOPT_QUOTE"] = CURLOPT_QUOTE;
        curloptions["CURLOPT_RANDOM_FILE"] = CURLOPT_RANDOM_FILE;
        curloptions["CURLOPT_RANGE"] = CURLOPT_RANGE;
        curloptions["CURLOPT_READDATA"] = CURLOPT_READDATA;
        curloptions["CURLOPT_READFUNCTION"] = CURLOPT_READFUNCTION;
        curloptions["CURLOPT_REDIR_PROTOCOLS"] = CURLOPT_REDIR_PROTOCOLS;
        curloptions["CURLOPT_REFERER"] = CURLOPT_REFERER;
        curloptions["CURLOPT_RESOLVE"] = CURLOPT_RESOLVE;
        curloptions["CURLOPT_RESUME_FROM"] = CURLOPT_RESUME_FROM;
        curloptions["CURLOPT_RESUME_FROM_LARGE"] = CURLOPT_RESUME_FROM_LARGE;
        curloptions["CURLOPT_RTSP_CLIENT_CSEQ"] = CURLOPT_RTSP_CLIENT_CSEQ;
        curloptions["CURLOPT_RTSP_REQUEST"] = CURLOPT_RTSP_REQUEST;
        curloptions["CURLOPT_RTSP_SERVER_CSEQ"] = CURLOPT_RTSP_SERVER_CSEQ;
        curloptions["CURLOPT_RTSP_SESSION_ID"] = CURLOPT_RTSP_SESSION_ID;
        curloptions["CURLOPT_RTSP_STREAM_URI"] = CURLOPT_RTSP_STREAM_URI;
        curloptions["CURLOPT_RTSP_TRANSPORT"] = CURLOPT_RTSP_TRANSPORT;
        curloptions["CURLOPT_SEEKDATA"] = CURLOPT_SEEKDATA;
        curloptions["CURLOPT_SEEKFUNCTION"] = CURLOPT_SEEKFUNCTION;
        curloptions["CURLOPT_SHARE"] = CURLOPT_SHARE;
        curloptions["CURLOPT_SOCKOPTDATA"] = CURLOPT_SOCKOPTDATA;
        curloptions["CURLOPT_SOCKOPTFUNCTION"] = CURLOPT_SOCKOPTFUNCTION;
        curloptions["CURLOPT_SOCKS5_GSSAPI_NEC"] = CURLOPT_SOCKS5_GSSAPI_NEC;
        curloptions["CURLOPT_SOCKS5_GSSAPI_SERVICE"] = CURLOPT_SOCKS5_GSSAPI_SERVICE;
        curloptions["CURLOPT_SSH_AUTH_TYPES"] = CURLOPT_SSH_AUTH_TYPES;
        curloptions["CURLOPT_SSH_HOST_PUBLIC_KEY_MD5"] = CURLOPT_SSH_HOST_PUBLIC_KEY_MD5;
        curloptions["CURLOPT_SSH_KEYDATA"] = CURLOPT_SSH_KEYDATA;
        curloptions["CURLOPT_SSH_KEYFUNCTION"] = CURLOPT_SSH_KEYFUNCTION;
        curloptions["CURLOPT_SSH_KNOWNHOSTS"] = CURLOPT_SSH_KNOWNHOSTS;
        curloptions["CURLOPT_SSH_PRIVATE_KEYFILE"] = CURLOPT_SSH_PRIVATE_KEYFILE;
        curloptions["CURLOPT_SSH_PUBLIC_KEYFILE"] = CURLOPT_SSH_PUBLIC_KEYFILE;
        curloptions["CURLOPT_SSLCERT"] = CURLOPT_SSLCERT;
        curloptions["CURLOPT_SSLCERTTYPE"] = CURLOPT_SSLCERTTYPE;
        curloptions["CURLOPT_SSLENGINE"] = CURLOPT_SSLENGINE;
        curloptions["CURLOPT_SSLENGINE_DEFAULT"] = CURLOPT_SSLENGINE_DEFAULT;
        curloptions["CURLOPT_SSLKEY"] = CURLOPT_SSLKEY;
        curloptions["CURLOPT_SSLKEYTYPE"] = CURLOPT_SSLKEYTYPE;
        curloptions["CURLOPT_SSLVERSION"] = CURLOPT_SSLVERSION;
        curloptions["CURLOPT_SSL_CIPHER_LIST"] = CURLOPT_SSL_CIPHER_LIST;
        curloptions["CURLOPT_SSL_CTX_DATA"] = CURLOPT_SSL_CTX_DATA;
        curloptions["CURLOPT_SSL_CTX_FUNCTION"] = CURLOPT_SSL_CTX_FUNCTION;
        curloptions["CURLOPT_SSL_SESSIONID_CACHE"] = CURLOPT_SSL_SESSIONID_CACHE;
        curloptions["CURLOPT_SSL_VERIFYHOST"] = CURLOPT_SSL_VERIFYHOST;
        curloptions["CURLOPT_SSL_VERIFYPEER"] = CURLOPT_SSL_VERIFYPEER;
        curloptions["CURLOPT_STDERR"] = CURLOPT_STDERR;
        curloptions["CURLOPT_TELNETOPTIONS"] = CURLOPT_TELNETOPTIONS;
        curloptions["CURLOPT_TFTP_BLKSIZE"] = CURLOPT_TFTP_BLKSIZE;
        curloptions["CURLOPT_TIMECONDITION"] = CURLOPT_TIMECONDITION;
        curloptions["CURLOPT_TIMEOUT"] = CURLOPT_TIMEOUT;
        curloptions["CURLOPT_TIMEOUT_MS"] = CURLOPT_TIMEOUT_MS;
        curloptions["CURLOPT_TIMEVALUE"] = CURLOPT_TIMEVALUE;
        curloptions["CURLOPT_TLSAUTH_PASSWORD"] = CURLOPT_TLSAUTH_PASSWORD;
        curloptions["CURLOPT_TLSAUTH_TYPE"] = CURLOPT_TLSAUTH_TYPE;
        curloptions["CURLOPT_TLSAUTH_USERNAME"] = CURLOPT_TLSAUTH_USERNAME;
        curloptions["CURLOPT_TRANSFERTEXT"] = CURLOPT_TRANSFERTEXT;
        curloptions["CURLOPT_TRANSFER_ENCODING"] = CURLOPT_TRANSFER_ENCODING;
        curloptions["CURLOPT_UNRESTRICTED_AUTH"] = CURLOPT_UNRESTRICTED_AUTH;
        curloptions["CURLOPT_UPLOAD"] = CURLOPT_UPLOAD;
        curloptions["CURLOPT_URL"] = CURLOPT_URL;
        curloptions["CURLOPT_USERAGENT"] = CURLOPT_USERAGENT;
        curloptions["CURLOPT_USERNAME"] = CURLOPT_USERNAME;
        curloptions["CURLOPT_USERPWD"] = CURLOPT_USERPWD;
        curloptions["CURLOPT_USE_SSL"] = CURLOPT_USE_SSL;
        curloptions["CURLOPT_VERBOSE"] = CURLOPT_VERBOSE;
        curloptions["CURLOPT_WILDCARDMATCH"] = CURLOPT_WILDCARDMATCH;
        curloptions["CURLOPT_WRITEDATA"] = CURLOPT_WRITEDATA;
        curloptions["CURLOPT_WRITEFUNCTION"] = CURLOPT_WRITEFUNCTION;
        curloptions["CURLOPT_SSL_OPTIONS"] = CURLOPT_SSL_OPTIONS;
        curloptions["CURLOPT_INTERFACE"] = CURLOPT_INTERFACE;
        curloptions["CURLOPT_MAIL_AUTH"] = CURLOPT_MAIL_AUTH;
        curloptions["CURLOPT_TCP_KEEPALIVE"] = CURLOPT_TCP_KEEPALIVE;
        curloptions["CURLOPT_TCP_KEEPIDLE"] = CURLOPT_TCP_KEEPIDLE;
        curloptions["CURLOPT_TCP_KEEPINTVL"] = CURLOPT_TCP_KEEPINTVL;
        curloptions["CURLOPT_TCP_NODELAY"] = CURLOPT_TCP_NODELAY;

#ifndef __apple_build_version__
        curloptions["CURLOPT_UNIX_SOCKET_PATH"] = CURLOPT_UNIX_SOCKET_PATH;
        curloptions["CURLOPT_XFERINFODATA"] = CURLOPT_XFERINFODATA;
        curloptions["CURLOPT_XFERINFOFUNCTION"] = CURLOPT_XFERINFOFUNCTION;
        curloptions["CURLOPT_XOAUTH2_BEARER"] = CURLOPT_XOAUTH2_BEARER;
        curloptions["CURLOPT_SSL_ENABLE_ALPN"] = CURLOPT_SSL_ENABLE_ALPN;
        curloptions["CURLOPT_SSL_ENABLE_NPN"] = CURLOPT_SSL_ENABLE_NPN;
        curloptions["CURLOPT_SSL_FALSESTART"] = CURLOPT_SSL_FALSESTART;
        curloptions["CURLOPT_SASL_IR"] = CURLOPT_SASL_IR;
        curloptions["CURLOPT_SERVICE_NAME"] = CURLOPT_SERVICE_NAME;
        curloptions["CURLOPT_PROXYHEADER"] = CURLOPT_PROXYHEADER;
        curloptions["CURLOPT_PATH_AS_IS"] = CURLOPT_PATH_AS_IS;
        curloptions["CURLOPT_PINNEDPUBLICKEY"] = CURLOPT_PINNEDPUBLICKEY;
        curloptions["CURLOPT_PIPEWAIT"] = CURLOPT_PIPEWAIT;
        curloptions["CURLOPT_LOGIN_OPTIONS"] = CURLOPT_LOGIN_OPTIONS;
        curloptions["CURLOPT_HEADEROPT"] = CURLOPT_HEADEROPT;
        curloptions["CURLOPT_DNS_INTERFACE"] = CURLOPT_DNS_INTERFACE;
        curloptions["CURLOPT_DNS_LOCAL_IP4"] = CURLOPT_DNS_LOCAL_IP4;
        curloptions["CURLOPT_DNS_LOCAL_IP6"] = CURLOPT_DNS_LOCAL_IP6;
        curloptions["CURLOPT_EXPECT_100_TIMEOUT_MS"] = CURLOPT_EXPECT_100_TIMEOUT_MS;
        curloptions["CURLOPT_PROXY_SERVICE_NAME"] = CURLOPT_PROXY_SERVICE_NAME;
        curloptions["CURLOPT_SSL_VERIFYSTATUS"] = CURLOPT_SSL_VERIFYSTATUS;
#endif
        init = true;
    }
}


/*
Element* eval_body_as_argument(LispE* lisp, Element* function, const unsigned long arity);
static size_t call_writing_old(char *ptr, size_t size, size_t nmemb, Lispe_curl* userdata) {
    long real_size = size*nmemb;
    if (userdata->function != NULL) {
        LispE* lisp = userdata->lisp;
        userdata->data.assign(static_cast<char*>(ptr), real_size);
        if (userdata->data == "404 page not found")
            return real_size;
        Element* data = lisp->provideString(userdata->data);
        List* call = (List*)eval_body_as_argument(lisp, userdata->function, P_THREE);
        call->append(data);
        call->append(lisp->quoted(userdata->argument));
        Element* e = userdata->lisp->n_null;
        try {
            e = call->eval(userdata->lisp);
        }
        catch (Error* err) {
            cerr << err->toString(lisp) << endl;
            err->release();
        }
        call->force_release();
        e->release();
    }
    else
        userdata->data.append(static_cast<char*>(ptr), real_size);
    return real_size;
}
*/

//Callback function
static size_t call_writing(char *ptr, size_t size, size_t nmemb, Lispe_curl* userdata) {
    if (ptr == NULL || userdata == NULL) return 0;
    
    long real_size = size*nmemb;
    if (userdata->function != NULL && userdata->lisp != NULL) {
        LispE* lisp = userdata->lisp;
        userdata->data.assign(static_cast<char*>(ptr), real_size);
        if (userdata->data == "404 page not found")
            return real_size;
        vector<Element*> arguments;
        arguments.push_back(lisp->provideString(userdata->data));
        arguments.push_back(userdata->argument);
        Element* e = lispe_eval_callback(lisp, userdata->function, arguments);
        if (e->isError()) {
            cerr << e->toString(lisp) << endl;
        }
        e->release();
    }
    else
        userdata->data.append(static_cast<char*>(ptr), real_size);
    return real_size;
}

//-------------------------------------------------------------------------------------------
Element* Lispe_curl::eval(LispE* lisp) {
    return this;
}

//Nous utilisons cette instruction pour renvoyer une description de l'instruction
//en effet, il suffit de faire: (print curl_exemple) pour obtenir cette information
wstring Lispe_curl::asString(LispE* lisp) {
    wstring res;
    s_utf8_to_unicode(res, data, data.size());
    return res;
}

Element* Lispe_curl::protected_index(LispE* lisp, Element* ix) {
    Element* s = lisp->provideString(data);
    Element* r = s->protected_index(lisp, ix);
    s->release();
    return r;
}

Element* Lispe_curl::extraction(LispE* lisp, List* liste) {
    Element* s = lisp->provideString(data);
    Element* r = s->extraction(lisp, liste);
    s->release();
    return r;
}
//-------------------------------------------------------------------------------------------
Element* Lispe_curl_function::MethodProxy(LispE* lisp) {
    //In our example, we have only two parameters
    //0 is the first parameter and so on...
    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;

    string sproxy = lisp->get_variable("str")->toString(lisp);
    strcpy_s(lcurl->urlbuffer, lcurl->urlsize, STR(sproxy));
    CURLcode res = curl_easy_setopt(lcurl->curl, CURLOPT_PROXY, lcurl->urlbuffer);
    if (res == 0)
        return true_;
    return errormsg(res);
}

Element* Lispe_curl_function::MethodPWD(LispE* lisp) {
    //In our example, we have only two parameters
    //0 is the first parameter and so on...
    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;

    string user = lisp->get_variable("user")->toString(lisp);
    string pwd = lisp->get_variable("pswd")->toString(lisp);
    user += ":";
    user += pwd;
    strcpy_s(lcurl->urlbuffer, lcurl->urlsize, STR(user));
    CURLcode res = curl_easy_setopt(lcurl->curl, CURLOPT_PROXYUSERPWD, lcurl->urlbuffer);
    if (res == 0)
        return true_;
    return errormsg(res);
}

Element* Lispe_curl_function::MethodURL(LispE* lisp) {
    //In our example, we have only two parameters
    //0 is the first parameter and so on...
    FILE* tmp = NULL;

    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;

    string urlstr = lisp->get_variable("str")->toString(lisp);
    if (urlstr != "") {
        if (urlstr.size() >= lcurl->urlsize) {
            char* new_buffer = (char*)malloc(urlstr.size()*1.5 + 1);
            if (new_buffer == NULL) {
                throw new Error("Error: Memory allocation failed");
            }
            free(lcurl->urlbuffer);
            lcurl->urlbuffer = new_buffer;
            lcurl->urlsize = urlstr.size()*1.5 + 1;
        }
        strcpy_s(lcurl->urlbuffer, lcurl->urlsize, STR(urlstr));
        curl_easy_setopt(lcurl->curl, CURLOPT_URL, lcurl->urlbuffer);
    }
    Element* lfilename = lisp->get_variable("filename");

    if (lfilename == null_) {
        lcurl->clear();
        curl_easy_setopt(lcurl->curl, CURLOPT_WRITEFUNCTION, call_writing);
        curl_easy_setopt(lcurl->curl, CURLOPT_WRITEDATA, lcurl);
    }
    else {
        string filename = lfilename->toString(lisp);
        tmp = fopen(STR(filename), "wb");

        if (tmp == NULL) {
            std::stringstream err;
            err << "Error: URL(009): Cannot create file: " << filename;
            throw new Error(err.str());
        }
        curl_easy_setopt(lcurl->curl, CURLOPT_WRITEDATA, tmp);
    }
    CURLcode res = curl_easy_perform(lcurl->curl);
    if (tmp != NULL)
        fclose(tmp);
    if (res == 0)
        return lcurl;
    return errormsg(res);
}

Element* Lispe_curl_function::MethodExecute(LispE* lisp) {
    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;

    Element* lfilename = lisp->get_variable("filename");

    //In our example, we have only two parameters
    //0 is the first parameter and so on...
    FILE* tmp = NULL;
    if (lfilename == null_) {
        lcurl->clear();
        curl_easy_setopt(lcurl->curl, CURLOPT_WRITEFUNCTION, call_writing);
        curl_easy_setopt(lcurl->curl, CURLOPT_WRITEDATA, lcurl);
    }
    else {
        string filename = lfilename->toString(lisp);
        tmp = fopen(STR(filename), "wb");
        if (tmp == NULL) {
            std::stringstream err;
            err << "Error: URL(009): Cannot create file: " << filename;
            throw new Error(err.str());
        }
        curl_easy_setopt(lcurl->curl, CURLOPT_WRITEDATA, tmp);
    }
    CURLcode res = curl_easy_perform(lcurl->curl);
    if (tmp != NULL)
        fclose(tmp);
    if (res == 0) {
        return lisp->provideString(lcurl->data);
    }
    return errormsg(res);
}

Element* Lispe_curl_function::MethodCallback(LispE* lisp) {
    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;
    lcurl->function = lisp->get_variable("function");
    if (lcurl->argument != NULL)
        lcurl->argument->decrement();
    lcurl->argument = lisp->get_variable("data");
    lcurl->argument->increment();
    return true_;
}

Element* Lispe_curl_function::MethodHeaders(LispE* lisp) {
    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;


    string data = lisp->get_variable("data")->toString(lisp);
    if (data.size() >= lcurl->urlsize) {
        char* new_buffer = (char*)malloc(data.size()*1.5 + 1);
        if (new_buffer == NULL) {
            throw new Error("Error: Memory allocation failed");
        }
        free(lcurl->urlbuffer);
        lcurl->urlbuffer = new_buffer;
        lcurl->urlsize = data.size()*1.5 + 1;
    }
    strcpy_s(lcurl->urlbuffer, lcurl->urlsize, STR(data));
    lcurl->headers = curl_slist_append(lcurl->headers, lcurl->urlbuffer);
    return true_;
}

Element* Lispe_curl_function::MethodOptions(LispE* lisp) {
    Element* crl = lisp->get_variable("crl");
    if (crl->type != idcurl)
        throw new Error("Error: first argument is not a 'curl' objet");

    Lispe_curl* lcurl = (Lispe_curl*)crl;


    string option = lisp->get_variable("str")->toString(lisp);
    if (curloptions.find(option) == curloptions.end()) {
        std::stringstream err;
        err << "Error: URL(031): Unknown option: " << option;
        throw new Error(err.str());
    }

    CURLcode res;
    CURLoption noption = curloptions[option];
    if (noption == CURLOPT_HTTPHEADER) {
        if (lcurl->headers == NULL)
            throw new Error("No headers provided with 'curl_set_headers'");
        res = curl_easy_setopt(lcurl->curl, noption, lcurl->headers);   
    }
    else {
        Element*  kdata = lisp->get_variable("data");
        if (kdata->isString()) {
            string data = kdata->toString(lisp);
            if (data.size() >= lcurl->urlsize) {
                char* new_buffer = (char*)malloc(data.size()*1.5 + 1);
                if (new_buffer == NULL) {
                    throw new Error("Error: Memory allocation failed");
                }
                free(lcurl->urlbuffer);
                lcurl->urlbuffer = new_buffer;
                lcurl->urlsize = data.size()*1.5 + 1;
            }
            strcpy_s(lcurl->urlbuffer, lcurl->urlsize, STR(data));
            res = curl_easy_setopt(lcurl->curl, noption, lcurl->urlbuffer);
        }
        else {
            long data = kdata->asInteger();
            res = curl_easy_setopt(lcurl->curl, noption, data);
        }
    }
    if (res == 0)
        return true_;
    return errormsg(res);
}

Element* Lispe_curl_function::eval(LispE* lisp) {

    switch (curltype) {
        case curl_curl: {
            //Lispe_curl object
            return new Lispe_curl(lisp, idcurl);
        }
        case curl_passwrd:
            return MethodPWD(lisp);
        case curl_proxy:
            return MethodProxy(lisp);
        case curl_url:
            return MethodURL(lisp);
        case curl_execute:
            return MethodExecute(lisp);
        case curl_options:
            return MethodOptions(lisp);
        case curl_function:
            return MethodCallback(lisp);
        case curl_headers:
            return MethodHeaders(lisp);

    }
    return null_;
}

wstring Lispe_curl_function::asString(LispE* lisp) {
    switch (curltype) {
        case curl_curl:
            return L"Create a 'curl' object";
        case curl_passwrd:
            return L"Username and password";
        case curl_proxy:
            return L"Setting a proxy";
        case curl_url:
            return L"Loading an URL";
        case curl_execute:
            return L"Execution with storage in a file";
        case curl_options:
            return L"Initialisation of an option";
        case curl_function:
            return L"Set a callback function and an object";
        case curl_headers:
            return L"Set the headers for CURLOPT_HTTPHEADER";
    }
	return L"";
}


extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    Init();
    //Nous créons d'abord le corps de la function
    lisp->extension("deflib curl ()", new Lispe_curl_function(lisp, curl_curl));
    lisp->extension("deflib curl_passwrd (crl user pswd)", new Lispe_curl_function(lisp, curl_passwrd));
    lisp->extension("deflib curl_url (crl str (filename))", new Lispe_curl_function(lisp, curl_url));
    lisp->extension("deflib curl_proxy (crl str)", new Lispe_curl_function(lisp, curl_proxy));
    lisp->extension("deflib curl_execute (crl (filename))", new Lispe_curl_function(lisp, curl_execute));
    lisp->extension("deflib curl_options (crl str data)", new Lispe_curl_function(lisp, curl_options));
    lisp->extension("deflib curl_set_function (crl function data)", new Lispe_curl_function(lisp, curl_function));
    lisp->extension("deflib curl_set_headers (crl data)", new Lispe_curl_function(lisp, curl_headers));

    return true;
}
}

