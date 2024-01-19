################################################################
include ../Makefile.in
################ Compiler LispE #################################
SOURCEWITHLISPE = lispemain.cxx jag.cxx editor.cxx tokenize.cxx
SOURCEMYEDITOR = editor.cxx editormain.cxx jag.cxx jagget.cxx jagrgx.cxx jagtools.cxx tokenize.cxx
#------------------------------------------------------------
OBJECTWITHLISPE = $(SOURCEWITHLISPE:%.cxx=objs/%.o)
OBJECTMYEDITOR = $(SOURCEMYEDITOR:%.cxx=objs/myeditor/%.o)
#------------------------------------------------------------
INCLUDES = -Iinclude -I../include
# Selon les compilateurs, il faudra peut-être choisir l'un des flags suivants
#C++11Flag = -std=c++0x -w -c -fPIC $(COPTION) -DUNIX $(VTERM_MOUSE)
#C++11Flag = -std=gnu++0x -w -c -fPIC $(COPTION) -DUNIX $(VTERM_MOUSE)
LIBPATH = ../bin
C++11Flag = -fPIC -std=c++11 -w -c $(COPTION) $(REGEX) -DUNIX $(VTERM_MOUSE)
#------------------------------------------------------------
objs/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDES) $< -o $@

objs/myeditor/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@
#------------------------------------------------------------

# For those who prefer a small executable linked with a dynamic library
lispeditor: install $(OBJECTWITHLISPE)
	$(COMPPLUSPLUS) -o bin/lispeditor $(OBJECTWITHLISPE) -L$(LIBPATH) -llispe -ldl $(LIBBOOST) -lpthread

myeditor: install $(OBJECTMYEDITOR)
	$(COMPPLUSPLUS) -o bin/myeditor $(OBJECTMYEDITOR) -L$(LIBPATH) -ldl $(LIBBOOST) -lpthread

install:
	mkdir -p bin
	mkdir -p objs
	mkdir -p objs/myeditor

clean:
	rm -Rf objs
	rm -Rf bin
