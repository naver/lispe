################################################################
CPPLUSPLUS = g++
################ Compiler SHELL #################################
SOURCEMYEDITOR = editor.cxx editormain.cxx jag.cxx jagget.cxx jagrgx.cxx tools.cxx tokenize.cxx
SOURCESHELL = editor.cxx shellmain.cxx jag.cxx jagget.cxx jagrgx.cxx tools.cxx tokenize.cxx

################ Compiler LISP MINI #################################
SOURCELIB = tokens.cxx minilisp.cxx tools.cxx eval.cxx compiling.cxx

################ WITH A RICH EDITOR #################################
SOURCEMINI = editor.cxx minilispmain.cxx jag.cxx jagget.cxx jagrgx.cxx tokenize.cxx

############## WITH A VERY POOR ONE #################################
SOURCELISP = minimain.cxx
#------------------------------------------------------------
OBJECTMYEDITOR = $(SOURCEMYEDITOR:%.cxx=objs/myeditor/%.o)
OBJECTSHELL = $(SOURCESHELL:%.cxx=objs/shell/%.o)

OBJECTMINI = $(SOURCEMINI:%.cxx=objs/mini/%.o)
OBJECTLISP = $(SOURCELISP:%.cxx=objs/lisp/%.o)
OBJECTLIB = $(SOURCELIB:%.cxx=objs/lisp/%.o)
#------------------------------------------------------------
DEBUGMODE = -g -DDEBUGGER
MODEFAST = -O3
MODECLANG = -Ofast

# Choose DEBUGMODE instead of MODEFAST if you need to check the memory leaks
OPTIONS = $(MODEFAST)

# Some compilers need the following options
#C++11Flag = -std=c++0x -w -c $(OPTIONS) $(VTERM_MOUSE)
#C++11Flag = -std=gnu++0x -w -c $(OPTIONS) $(VTERM_MOUSE)
C++11Flag =  -fPIC -std=c++11 -w -c $(OPTIONS) $(VTERM_MOUSE)
#------------------------------------------------------------
objs/myeditor/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@

objs/shell/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@

objs/mini/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@

objs/lisp/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++11Flag) -Iinclude $< -o $@
#------------------------------------------------------------

# For those who prefer a small executable linked with a dynamic library
myeditor: installbase $(OBJECTMYEDITOR)
	$(CPPLUSPLUS) -o bin/myeditor $(OBJECTMYEDITOR) -ldl -lpthread

shell: installbase $(OBJECTSHELL)
	$(CPPLUSPLUS) -o bin/shelleditor $(OBJECTSHELL) -ldl -lpthread

#------------------------------------------------------------
# With lib mini
#------------------------------------------------------------
libmini: installlisp $(OBJECTLIB)
	ar rcs bin/libminilisp.a $(OBJECTLIB)
	ranlib bin/libminilisp.a

mini: installlisp libmini $(OBJECTMINI)
	$(CPPLUSPLUS) -o bin/minilisp $(OBJECTMINI) -ldl -lpthread  -Lbin -lminilisp

lispmini: installlisp libmini $(OBJECTLISP)
	$(CPPLUSPLUS) -o bin/lisp $(OBJECTLISP) -ldl -lpthread -Lbin -lminilisp
#------------------------------------------------------------
installbase:
	mkdir -p bin
	mkdir -p objs
	mkdir -p objs/myeditor
	mkdir -p objs/shell

installlisp:
	mkdir -p bin
	mkdir -p objs
	mkdir -p objs/mini
	mkdir -p objs/lisp

clean:
	rm -Rf objs
	rm -Rf bin
