include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourcegui = lispe_gui.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectgui = $(sourcegui:%.cxx=$(LOBJPATH)/gui/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c -O3 -DUNIX -DFL_INTERNALS $(FLTKVERSION)
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude -Iinclude/$(PLATFORM)/fltk
#------------------------------------------------------------
$(LOBJPATH)/gui/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectgui)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/libgui.so $(objectgui) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBFLTK) $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/gui

clean:
	rm -f $(LOBJPATH)/gui/*.o
