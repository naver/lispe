/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  sockets.cxx
//
//



#ifdef WIN32
#include <WinSock.h>
#define closesock closesocket
#define readsock(x,y,z) recv(x,y,z,0)
#define writesock(x,y,z) send(x,y,z,0)
#else
#include <unistd.h>
#include <sys/utsname.h>        /* for uname() */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <fcntl.h>
#define SOCKET_ERROR -1
#define closesock close
#define readsock read
#define writesock write
#define SOCKET int
#endif

const int MAXHOSTNAME = 1000;

#include "lispe.h"
#include <list>

const int MAXSIZEINPUT = 256;
const int POSSIGNATURE = 5;
const char* FORMATSIGNATURE = "%4x";

class Socketelement;

#ifdef WIN32
#define BIND ::bind
#else
#define BIND bind
#endif

string Msgerror() {
    char* s = strerror(errno);
    if (strstr(s, "No error") != NULL)
        return "";
    return s;
}


static short XConvert(char* number, int nb) {
    unsigned short v = 0;
    uchar c;
    for (int i = 0; i<nb; i++) {
        c = number[i];
        v = ( (v << 4) | (c & 0xF) | ((c & 64) >> 3)) + ((c & 64) >> 6);
    }
    return v;
}

static bool checkipaddres(string serveraddr) {
    int countpoint = 0;
    for (int i = 0; i<serveraddr.size(); i++) {
        if (serveraddr[i] == '.') {
            countpoint++;
            if (countpoint>3)
                return false;
        }
        else
            if (serveraddr[i]<48 || serveraddr[i]>57)
                return false;
    }
    return true;
}

static bool validstream(long nb) {
    if (nb == SOCKET_ERROR || nb == 0)
        return false;
    return true;
}


//----------------------------------------------------------------------------------------------
//We need to declare once again our local definitions.

typedef enum {sock_create, sock_connect, sock_wait, sock_read, sock_write, sock_receive, sock_get, sock_send, sock_close, sock_blocking, sock_timeout, sock_gethostname, sock_port, sock_getpeername} socket_action;


class Socketelement : public Element {
public:
    
    struct sockaddr_in servAddr;
    int idclient;
    bool blocking;
    struct hostent *hp;
    char servername[MAXHOSTNAME + 10];
    int port;
    SOCKET sock;
    bool server;
    //For inserting a timeout
    fd_set readfds;
    fd_set writefds;
    struct timeval tv;
    bool timeout;
    int v_timeout;
    
    unordered_map<SOCKET, bool> socketclients;
    
#ifdef WIN32
    int len;
#else
    socklen_t len;
#endif
    
    Socketelement(short ty) : Element(ty) {
        //Do not forget your variable initialisation
        hp = NULL;
        sock = -1;
        idclient = 0;
        blocking = true;
        timeout = false;
    }
    
    ~Socketelement() {
        Close();
    }
    
    Element* methodCreateServer(LispE* lisp, int nbclients, int kport, string server_name) {
        //TamguThreadLock _lock(general);
        //In our example, we have only two parameters
        if (hp != NULL)
            throw new Error("Error: SOCKET(831): Server already launched on this socket");
        if (server_name.size() >= MAXHOSTNAME)
            throw new Error("Error: SOCKET(850): Wrong server name");
        strcpy(servername, (char*)server_name.c_str());
        port = kport;
        Element* ret = createSocket(lisp);
        if (ret == null_)
            return ret;
#ifndef WIN32
        int yes = 1;
        if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
            string errmessage = "Error: SOCKET(872): Error on reuse addess";
            errmessage += Msgerror();
            throw new Error(errmessage);
        }
#endif
        servAddr.sin_addr.s_addr = INADDR_ANY;
        if (BIND(sock, (struct sockaddr*)&servAddr, len) < 0) {
            string errmessage = "Error: SOCKET(853): Error on bind ";
            errmessage += Msgerror();
            throw new Error(errmessage);
        }
        if (listen(sock, nbclients) < 0) {
            string errmessage = "Error: SOCKET(854): Error on listen ";
            errmessage += Msgerror();
            throw new Error(errmessage);
        }
        server = true;
        return this;
    }
    
    Element* methodCreateClient(LispE* lisp, string& kserver, int kport) {
        if (hp != NULL)
            throw new Error("Error: SOCKET(831): Server already launched on this socket");
        //0 is the first parameter and so on...
        strcpy(servername, STR(kserver));
        port = kport;
        Element* ret = createSocket(lisp);
        if (ret == null_)
            return ret;
        server = false;
        std::cerr << servername << " on " << port << ": " << sock << endl;
        if (connect(sock, (struct sockaddr*)&servAddr, sizeof(servAddr)) < 0) {
            string errmessage = "Error: SOCKET(857): Error on connection ";
            errmessage += Msgerror();
            throw new Error(errmessage);
        }
        return this;
    }
    
    Element* methodWait(LispE* lisp) {
        //TamguThreadLock _lock(waiting);
        struct sockaddr_in cliAddr;
        SOCKET socketclient = -1;
        if (server == true) {
            if ((socketclient = accept(sock, (struct sockaddr*)&cliAddr, &len)) < 0) {
                string errmessage = "Error: SOCKET(855): Error on read ";
                errmessage += Msgerror();
                throw new Error(errmessage);
            }
            
            socketclients[socketclient] = true;
            return lisp->provideInteger(socketclient);
        }
        return minusone_;
    }
    
    Element* methodRead(LispE* lisp, int socketclient) {
        short ssz;
        string res;
        SOCKET currentsock;
        if (server == true) {
            currentsock = socketclient;
        }
        else
            currentsock = sock;
        if (currentsock == SOCKET_ERROR)
            throw new Error("Error: SOCKET(858): No client connected");
        char inputstr[MAXSIZEINPUT + 1];
        long nbcharread = 0;
        long nbloc;
        while (nbcharread < POSSIGNATURE) {
            if (testTimeOutRead(currentsock) == false)
                throw new Error("Error: timeout");
            nbloc = readsock(currentsock, inputstr + nbcharread, POSSIGNATURE - nbcharread);
            if (validstream(nbloc) == false) {
                string errmessage = "Error: SOCKET(861): Error on read";
                errmessage += Msgerror();
                throw new Error(errmessage);
            }
            nbcharread += nbloc;
        }
        inputstr[POSSIGNATURE] = 0;
        ssz = XConvert(inputstr + 1, POSSIGNATURE - 1);
        int maxtoread;
        //cout<<"Reading:"<<ssz<<":"<<inputstr<<endl;
        while (ssz > 0) {
            inputstr[0] = 0;
            if (testTimeOutRead(currentsock) == false)
                throw new Error("Error: timeout");
            maxtoread = ssz;
            if (maxtoread > MAXSIZEINPUT)
                maxtoread = MAXSIZEINPUT;
            nbcharread = readsock(currentsock, inputstr, maxtoread);
            if (validstream(nbcharread) == false) {
                string errmessage = "Error: SOCKET(861): Error on read";
                errmessage += Msgerror();
                throw new Error(errmessage);
            }
            inputstr[nbcharread] = 0;
            ssz -= nbcharread;
            res += inputstr;
        }
        return lisp->provideString(res);
    }
    
    Element* methodReadRaw(LispE* lisp, SOCKET currentsock, int nbbytes) {
        string res;
        long nb = -1;
        //TamguThreadLock _lock(reading);
        bool decrement = false;
        if (server == true) {
            if (currentsock == SOCKET_ERROR)
                throw new Error("Error: SOCKET(824): Non connected socket");
        }
        else {
            currentsock = sock;
        }
        
        decrement = (nbbytes == MAXSIZEINPUT);
        
        if (currentsock == SOCKET_ERROR)
            throw new Error("Error: SOCKET(858): No client connected");
        if (testTimeOutRead(currentsock) == false)
            throw new Error("Error: timeout");
        
        char inputstr[MAXSIZEINPUT + 1];
        if (nbbytes >= MAXSIZEINPUT)
            nb = readsock(currentsock, inputstr, MAXSIZEINPUT);
        else
            nb = readsock(currentsock, inputstr, nbbytes);
        if (validstream(nb) == false) {                        //In the case of non blocking socket, we simply return the empty string
            string errmessage = "Error: SOCKET(860): Error on RECEIVE";
            errmessage += Msgerror();
            throw new Error(errmessage);
        }
        if (nb != 0) {
            inputstr[nb] = 0;
            res += inputstr;
        }
        if (decrement)
            nbbytes -= nb;
        while (nb != 0 && nbbytes > 0) {
            if (testTimeOutRead(currentsock) == false)
                throw new Error("Error: timeout");
            if (nbbytes >= MAXSIZEINPUT)
                nb = readsock(currentsock, inputstr, MAXSIZEINPUT);
            else
                nb = readsock(currentsock, inputstr, nbbytes);
            if (decrement)
                nbbytes -= nb;
            if (nb != 0) {
                inputstr[nb] = 0;
                res += inputstr;
            }
            if (validstream(nb) == false) {
                string errmessage = "Error: SOCKET(860): Error on RECEIVE";
                errmessage += Msgerror();
                throw new Error(errmessage);
            }
        }
        return lisp->provideString(res);
    }
    
    Element* methodGet(LispE* lisp, SOCKET currentsock) {
        long nb = -1;
        if (!server)
            currentsock = sock;
        
        if (currentsock == SOCKET_ERROR)
            throw new Error("Error: SOCKET(858): No client connected");
        if (testTimeOutRead(currentsock) == false)
            throw new Error("Error: timeout");
        
        char rd[] = { 0, 0, 0 };
        nb = readsock(currentsock, rd, 1);
        if (validstream(nb) == false) {                        //In the case of non blocking socket, we simply return the empty string
            string errmessage = "Error: SOCKET(860): Error on RECEIVE ";
            errmessage += Msgerror();
            throw new Error(errmessage);
        }
        if (!nb)
            return null_;
        return lisp->provideInteger(rd[0]);
    }
    
    Element* methodWriteRaw(LispE* lisp, SOCKET currentsock, string& strc) {
        if (!server)
            currentsock = sock;
        
        if (testTimeOutWrite(currentsock) == false)
            throw new Error("Error: timeout");
        
        char* buff;
        buff = STR(strc);
        long sz = strc.size();
        while (sz>0) {
            long nbsz = sz;
            if (nbsz > MAXSIZEINPUT)
                nbsz = MAXSIZEINPUT;
            if (writesock(currentsock, buff, nbsz) < 0) {
                string errmessage = "Error: SOCKET(859): Error sending";
                errmessage += Msgerror();
                throw new Error(errmessage);
            }
            buff += MAXSIZEINPUT;
            sz -= MAXSIZEINPUT;
        }
        return true_;
    }

    void Close() {
        //If it has already been closed
        //Nothing to do...
        if (hp == NULL)
            return;
        
        if (server) {
            for(auto& a: socketclients)
                closesock(a.first);
        }
        else
            closesock(sock);
    }
    
    Element* methodClose(LispE* lisp, int socketclient) {
        //TamguThreadLock _lock(general);
        if (server == true) {
            if (hp == NULL || sock == SOCKET_ERROR)
                return null_;
            //We clean a client connection
            closesock(socketclient);
            if (socketclients.find(socketclient) != socketclients.end())
                socketclients.erase(socketclient);
            return true_;
        }
        //otherwise we clean the current socket
        if (hp != NULL && sock != SOCKET_ERROR)
            closesock(sock);
        hp = NULL;
        return true_;
    }
    
    Element* methodBlocking(LispE* lisp, bool blocking) {
        //TamguThreadLock _lock(general);
#ifdef WIN32
        u_long iMode = 0;
        if (blocking == false)
            iMode = 1;
        ioctlsocket(sock, FIONBIO, &iMode);
#else
        int flags;
        flags = fcntl(sock, F_GETFL, 0);
        if (blocking == false)
            flags |= O_NONBLOCK;
        else
            flags &= ~O_NONBLOCK;
        fcntl(sock, F_SETFL, flags);
#endif
        return true_;
    }
    
    Element* methodTimeout(LispE* lisp, int v_timeout) {
        //TamguThreadLock _lock(general);
        //In our example, we have only two parameters
        if (v_timeout == -1) {
            timeout = false;
            return true_;
        }
        //We create our timeout
        tv.tv_sec = v_timeout;
        tv.tv_usec = 0;
        timeout = true;
        FD_ZERO(&readfds);
        FD_ZERO(&writefds);
        return true_;
    }
    
    Element* methodGethostname(LispE* lisp) {
        gethostname(servername, MAXHOSTNAME);
        string s = servername;
        return lisp->provideString(s);
    }
    
    Element* methodPort(LispE* lisp) {
        return lisp->provideInteger(port);
    }
    
    Element* methodGetpeername(LispE* lisp, int socketclient) {
        if (server == true) {
            if (socketclient == SOCKET_ERROR)
                throw new Error("Error: SOCKET(824): Non connected socket");
            struct sockaddr cliAddr;
#ifdef WIN32
            int len = sizeof(struct sockaddr);
#else
            socklen_t len = sizeof(struct sockaddr);
#endif
            getpeername(socketclient, &cliAddr, &len);
            struct sockaddr_in* client = (struct sockaddr_in*)&cliAddr;
            char* nm = inet_ntoa(client->sin_addr);
            Dictionary* kmap = new Dictionary;
            wstring key1 = L"port";
            kmap->recording(key1, lisp->provideInteger(client->sin_port));
            wstring key2 = L"address";
            string nms = nm;
            kmap->recording(key2, lisp->provideString(nms));
            return kmap;
        }
        throw new Error("Error: SOCKET(852): You cannot use GETPEERNAME on the client side");
    }
        
    Element* createSocket(LispE* lisp) {
        
        sock = socket(AF_INET, SOCK_STREAM, 0);
        if (sock == SOCKET_ERROR)
            throw new Error("Error: SOCKET(856): Socket error");
#ifdef WIN32
        memset((char *)&servAddr, '\0', sizeof(servAddr));
#else
        bzero((char *)&servAddr, sizeof(servAddr));
#endif
        
        if (checkipaddres(servername) == false) {
            hp = gethostbyname(servername);                  /* get our address info */
            if (hp == NULL)                             /* we don't exist !? */
                throw new Error("Error: SOCKET(851): Cannot find host by name on this machine");
            memcpy((char *)&servAddr.sin_addr, hp->h_addr, hp->h_length);     /* set address */
            servAddr.sin_family = hp->h_addrtype;              /* this is our host address */
            /* this is our port number */
        }
        else {
            servAddr.sin_addr.s_addr = inet_addr(servername);
            servAddr.sin_family = AF_INET;
        }
        
        servAddr.sin_port = htons(port);
        
        len = sizeof(servAddr);
        
        
        return true_;
    }
    
    bool testTimeOutRead(SOCKET currentsock) {
        if (timeout == true) {
            tv.tv_sec = v_timeout;
            tv.tv_usec = 0;
            FD_ZERO(&readfds);
            //Then we insert our socket in it
            FD_CLR(currentsock, &readfds);
            FD_SET(currentsock, &readfds);
            //int sl=select(currentsock+1,&readfds,NULL,NULL, &tv);
            int sl = select(FD_SETSIZE, &readfds, NULL, NULL, &tv);
            if (sl < 0) {
                return false;
            }
            sl = FD_ISSET(currentsock, &readfds);
            if (sl == 0) {
                return false;
            }
        }
        return true;
    }
    
    bool testTimeOutWrite(SOCKET currentsock) {
        if (timeout == true) {
            tv.tv_sec = v_timeout;
            tv.tv_usec = 0;
            FD_ZERO(&writefds);
            //Then we insert our socket in it
            FD_CLR(currentsock, &writefds);
            FD_SET(currentsock, &writefds);
            //int sl=select(currentsock+1,NULL,&writefds,NULL,&tv);
            int sl = select(FD_SETSIZE, NULL, &writefds, NULL, &tv);
            if (sl < 0) {
                return false;
            }
            sl = FD_ISSET(currentsock, &writefds);
            if (sl == 0) {
                return false;
            }
        }
        return true;
    }
    
    Element* methodWrite(LispE* lisp, SOCKET currentsock, string& strc) {
        return Write("N", lisp, currentsock, strc);
    }

    Element* Write(string act, LispE* lisp, SOCKET currentsock, string& strc) {
        short sz;
        char padding[POSSIGNATURE + 1];
        
        memset(padding, '\0', POSSIGNATURE + 1);
        
        if (!server)
            currentsock = sock;
        
        if (currentsock == SOCKET_ERROR)
            throw new Error("Error: SOCKET(858): No client connected");
        
        char* buff;
        bool written = false;
        sz = strc.size();
        if (sz>0) {
            written = true;
            sprintf_s(padding + 1, POSSIGNATURE, FORMATSIGNATURE, sz);
            padding[0] = act[0];
            //cout<<"Writing:"<<sz<<":"<<padding<<endl;
            
            if (testTimeOutWrite(currentsock) == false)
                throw new Error("Error: timeout");
                
                if (writesock(currentsock, padding, POSSIGNATURE)<0) {
                    string errmessage = "Error: SOCKET(859): Error sending";
                    errmessage += Msgerror();
                    throw new Error(errmessage);
                }
            buff = STR(strc);
            while (sz>0) {
                int nbsz = sz;
                if (nbsz > MAXSIZEINPUT)
                    nbsz = MAXSIZEINPUT;
                
                if (testTimeOutWrite(currentsock) == false)
                    throw new Error("Error: timeout");
                    
                    if (writesock(currentsock, buff, nbsz) < 0) {
                        string errmessage = "Error: SOCKET(859): Error sending";
                        errmessage += Msgerror();
                        throw new Error(errmessage);
                    }
                buff += MAXSIZEINPUT;
                sz -= MAXSIZEINPUT;
            }
        }
        
        if (written == false) {
            //Empty strings... We still write it...
            sprintf_s(padding + 1, POSSIGNATURE, FORMATSIGNATURE, 0);
            padding[0] = act[0];
            if (testTimeOutWrite(currentsock) == false)
                throw new Error("Error: timeout");
                
                if (writesock(currentsock, padding, POSSIGNATURE) < 0) {
                    string errmessage = "Error: SOCKET(859): Error sending";
                    errmessage += Msgerror();
                    throw new Error(errmessage);
                }
        }
        return true_;
    }
};


class Socket : public Element {
public:
#ifdef WIN32
    static WSADATA WSAData;
    static bool rootsocket;
#endif
    socket_action action;
    short type_socket;
    short type_socket_element;
    short id_socketClientId;
    short id_sock;
    short id_port;

    Socket(LispE* lisp, socket_action a, short ty) : action(a), Element(ty) {
        wstring w = L"socket";
        type_socket_element = lisp->encode(w);
        
        w = L"socketClientId";
        id_socketClientId = lisp->encode(w);

        w = L"sock";
        id_sock = lisp->encode(w);
        
        w = L"port";
        id_port = lisp->encode(w);
    }

#ifdef WIN32
    void initialisation() {
        if (!rootsocket) {
            WSAStartup(MAKEWORD(2, 0), &WSAData);
        }
        rootsocket = true;
    }
    
    ~Socket() {
        if (rootsocket)
            WSACleanup();
        rootsocket = false;
    }
#else
//dummy version for all other platforms
    void initialisation() {}
#endif
    
    Element* eval(LispE* lisp) {
        switch (action) {
            case sock_create: {
                initialisation();
                Socketelement* socket = new Socketelement(type_socket_element);
                int port = (int)lisp->get(id_port)->asInteger();
                int nbclients = (int)lisp->get("nbclients")->asInteger();
                string hostname = lisp->get("hostname")->toString(lisp);
                try {
                    return socket->methodCreateServer(lisp, nbclients,port, hostname);
                }
                catch(Error* err) {
                    delete socket;
                    throw err;
                }
            }
            case sock_connect: {
                initialisation();
                Socketelement* socket = new Socketelement(type_socket_element);
                int port = (int)lisp->get(id_port)->asInteger();
                string hostname = lisp->get("hostname")->toString(lisp);
                try {
                    return socket->methodCreateClient(lisp, hostname, port);
                }
                catch(Error* err) {
                    delete socket;
                    throw err;
                }
            }
            case sock_wait: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                return ((Socketelement*)sock)->methodWait(lisp);
            }
            case sock_read: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                return ((Socketelement*)sock)->methodRead(lisp, socketClientId);
            }
            case sock_write: {
                Element* sock = lisp->get(id_sock);

                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                string s = lisp->get("str")->toString(lisp);
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                return ((Socketelement*)sock)->methodWrite(lisp, socketClientId, s);
            }
            case sock_receive: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                int nb = (int)lisp->get("nb")->asInteger();
                return ((Socketelement*)sock)->methodReadRaw(lisp, socketClientId, nb);
            }
            case sock_get: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                return ((Socketelement*)sock)->methodGet(lisp, socketClientId);
            }
            case sock_send: {
                Element* sock = lisp->get(id_sock);

                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                string s = lisp->get("str")->toString(lisp);
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                return ((Socketelement*)sock)->methodWriteRaw(lisp, socketClientId, s);
            }
            case sock_close: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                return ((Socketelement*)sock)->methodClose(lisp, socketClientId);
            }
            case sock_blocking: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                bool flag = lisp->get("flag")->Boolean();
                return ((Socketelement*)sock)->methodBlocking(lisp, flag);
            }
            case sock_timeout: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                
                int timeout = (int)lisp->get("tm")->asInteger();
                return ((Socketelement*)sock)->methodTimeout(lisp, timeout);
            }
            case sock_gethostname: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                return ((Socketelement*)sock)->methodGethostname(lisp);
            }
            case sock_port: {
                Element* sock = lisp->get(id_sock);
                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                return ((Socketelement*)sock)->methodPort(lisp);
            }
            case sock_getpeername: {
                Element* sock = lisp->get(id_sock);

                if (sock->type != type_socket_element)
                    throw new Error("Error: expecting a 'socket' object");
                SOCKET socketClientId = (int)lisp->get(id_socketClientId)->asInteger();
                return ((Socketelement*)sock)->methodGetpeername(lisp, socketClientId);
            }
        }
        return null_;
    }

    wstring asString(LispE* lisp) {
        switch (action) {
            case sock_create:
                return L"create a server if hostname is omitted then the local hostname is used";
            case sock_connect:
                return L"connect to the server";
            case sock_wait:
                return L"wait for a client to connect and returns its socket id";
            case sock_read:
                return L"read a string on a socket. On the server side 'socketClientId' is the value returned by 'wait()'. Use 'read()' on the client side";
            case sock_write:
                return L"write the string s on the socket. On the server side num is the value returned by wait()'. Use 'write(s)' on the client side";
            case sock_receive:
                return L"read a string on a socket in a raw environment. On the server side 'socketClientId' is the value returned by 'wait()'. Use 'receive()' on the client side";
            case sock_get:
                return L"get one character from a socket in a non Tamgu environment. On the server side 'socketClientId' is the value returned by 'wait()'. Use 'get()' on the client side";
            case sock_send:
                return L"write the string s on the socket in a non Tamgu environment. On the server side num is the value returned by wait()'. Use 'send(string s)' on the client side";
            case sock_close:
                return L"Close a socket. On the server side if 'socketClientId' is provided (it is the value returned by wait()) it closes the client socket otherwise it closes the current socket.";
            case sock_blocking:
                return L"if 'flag' is true the socket works in 'blocking' mode otherwise in 'non blocking' mode";
            case sock_timeout:
                return L"Set a timeout of 't' seconds on the socket";
            case sock_gethostname:
                return L"return the current host name";
            case sock_port:
                return L"return the current port number";
            case sock_getpeername:
                return L"return the current peer name";
        }
    }
    
};

#ifdef WIN32
WSADATA Socket::WSAData;
bool Socket::rootsocket = false;
#endif


void moduleSocket(LispE* lisp) {
    wstring w = L"socketmethods";
    short idsocket = lisp->encode(w);
        
    lisp->extension("deflib socket_create(port nbclients (hostname))", new Socket(lisp, sock_create, idsocket));
    lisp->extension("deflib socket_connect(hostname port)", new Socket(lisp, sock_connect, idsocket));
    lisp->extension("deflib socket_wait(sock)", new Socket(lisp, sock_wait, idsocket));
    lisp->extension("deflib socket_read(sock (socketClientId -1))", new Socket(lisp, sock_read, idsocket));
    lisp->extension("deflib socket_write(sock str (socketClientId -1))", new Socket(lisp, sock_write, idsocket));
    lisp->extension("deflib socket_receive(sock nb (socketClientId -1))", new Socket(lisp, sock_receive, idsocket));
    lisp->extension("deflib socket_get(sock (socketClientId -1))", new Socket(lisp, sock_get, idsocket));
    lisp->extension("deflib socket_send(sock str (socketClientId -1))", new Socket(lisp, sock_send, idsocket));
    lisp->extension("deflib socket_close(sock (socketClientId -1))", new Socket(lisp, sock_close, idsocket));
    lisp->extension("deflib socket_blocking(sock flag)", new Socket(lisp, sock_blocking, idsocket));
    lisp->extension("deflib socket_settimeout(sock tm)", new Socket(lisp, sock_timeout, idsocket));
    lisp->extension("deflib socket_gethostname(sock)", new Socket(lisp, sock_gethostname, idsocket));
    lisp->extension("deflib socket_port(sock)", new Socket(lisp, sock_port, idsocket));
    lisp->extension("deflib socket_getpeername(sock socketClientId)", new Socket(lisp, sock_getpeername, idsocket));
    
}

