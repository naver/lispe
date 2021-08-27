include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
source%1 = lispe_%1.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
object%1 = $(source%1:%.cxx=$(LOBJPATH)/%1/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/%1/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(object%1)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_%1.so $(object%1) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/%1

clean:
	rm -f $(LOBJPATH)/%1/*.o

