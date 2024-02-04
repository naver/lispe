################################################################
include ../Makefile.in
################ Compiler LispE #################################
SOURCEWITHLISPE = lispemain.cxx jag.cxx editor.cxx tokenize.cxx
SOURCEMYEDITOR = editor.cxx editormain.cxx jag.cxx jagget.cxx jagrgx.cxx tools.cxx tokenize.cxx
SOURCESHELL = editor.cxx shellmain.cxx jag.cxx jagget.cxx jagrgx.cxx tools.cxx tokenize.cxx
SOURCEMINI = editor.cxx minilispmain.cxx jag.cxx jagget.cxx jagrgx.cxx tools.cxx tokenize.cxx tokens.cxx minilisp.cxx
SOURCELISP = minimain.cxx tokens.cxx minilisp.cxx tokenize.cxx tools.cxx
#------------------------------------------------------------
OBJECTWITHLISPE = $(SOURCEWITHLISPE:%.cxx=objs/%.o)
OBJECTMYEDITOR = $(SOURCEMYEDITOR:%.cxx=objs/myeditor/%.o)
OBJECTSHELL = $(SOURCESHELL:%.cxx=objs/shell/%.o)
OBJECTMINI = $(SOURCEMINI:%.cxx=objs/mini/%.o)
OBJECTLISP = $(SOURCELISP:%.cxx=objs/lisp/%.o)
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

objs/shell/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@

objs/mini/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@

objs/lisp/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@
#------------------------------------------------------------

# For those who prefer a small executable linked with a dynamic library
lispeditor: install $(OBJECTWITHLISPE)
	$(COMPPLUSPLUS) -o bin/lispeditor $(OBJECTWITHLISPE) -L$(LIBPATH) -llispe -ldl $(LIBBOOST) -lpthread

myeditor: install $(OBJECTMYEDITOR)
	$(COMPPLUSPLUS) -o bin/myeditor $(OBJECTMYEDITOR) -L$(LIBPATH) -ldl $(LIBBOOST) -lpthread

shell: install $(OBJECTSHELL)
	$(COMPPLUSPLUS) -o bin/shelleditor $(OBJECTSHELL) -L$(LIBPATH) -ldl $(LIBBOOST) -lpthread

mini: install $(OBJECTMINI)
	$(COMPPLUSPLUS) -o bin/minilisp $(OBJECTMINI) -L$(LIBPATH) -ldl $(LIBBOOST) -lpthread

lispmini: install $(OBJECTLISP)
	$(COMPPLUSPLUS) -o bin/lisp $(OBJECTLISP) -L$(LIBPATH) -ldl $(LIBBOOST) -lpthread

install:
	mkdir -p bin
	mkdir -p objs
	mkdir -p objs/myeditor
	mkdir -p objs/shell
	mkdir -p objs/mini
	mkdir -p objs/lisp

clean:
	rm -Rf objs
	rm -Rf bin
