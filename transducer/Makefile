include ../Makefile.in
###################################################################
sourcetransducer = lispe_transducer.cxx automaton.cxx conversion.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objecttransducer = $(sourcetransducer:%.cxx=$(LOBJPATH)/transducer/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/transducer/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objecttransducer)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_transducer.so $(objecttransducer) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/transducer

clean:
	rm -f $(LOBJPATH)/transducer/*.o

