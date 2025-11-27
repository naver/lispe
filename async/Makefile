include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourceasync = lispe_async.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectasync = $(sourceasync:%.cxx=$(LOBJPATH)/async/%.o)
#------------------------------------------------------------
C++11Flag = $(versionC++) -fPIC -w -c $(COPTION) -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/async/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectasync)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_async.so $(objectasync) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/async

clean:
	rm -f $(LOBJPATH)/async/*.o


