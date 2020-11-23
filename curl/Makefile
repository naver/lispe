include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourcecurl = lispe_curl.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectcurl = $(sourcecurl:%.cxx=$(LOBJPATH)/curl/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c -O3 -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude -Iinclude/linux
#------------------------------------------------------------
$(LOBJPATH)/curl/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectcurl)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_curl.so $(objectcurl) -L$(LBINPATH) -llispe -ldl $(LIBBOOST) $(CURLLIB) -lpthread

install:
	mkdir -p $(LOBJPATH)/curl

clean:
	rm -f $(LOBJPATH)/curl/*.o

