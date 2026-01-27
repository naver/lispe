include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourcetiktoken = lispe_tiktoken.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objecttiktoken = $(sourcetiktoken:%.cxx=$(LOBJPATH)/tiktoken/%.o)
#------------------------------------------------------------
MACICUPATH = -L/opt/homebrew/opt/icu4c/lib 
ICULIBS = $(MACICUPATH) -licuuc -licui18n
C++11Flag = $(versionC++) -fPIC -w -c $(COPTION) -DUNIX 
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude -I/opt/homebrew/opt/icu4c/include
#------------------------------------------------------------
$(LOBJPATH)/tiktoken/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objecttiktoken)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_tiktoken.so $(objecttiktoken) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST) $(ICULIBS)

install:
	mkdir -p $(LOBJPATH)/tiktoken

clean:
	rm -f $(LOBJPATH)/tiktoken/*.o


