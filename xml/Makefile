include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourcexml = lispe_xml.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectxml = $(sourcexml:%.cxx=$(LOBJPATH)/xml/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/xml/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectxml)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_xml.so $(objectxml) -L$(LBINPATH) -lxml2 -llispe -ldl -pthread $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/xml

clean:
	rm -f $(LOBJPATH)/xml/*.o

