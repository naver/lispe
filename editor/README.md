# Implementing your own custom editor

This directory contains the code to implement an editor to edit and run your own interpreter.

### Compiling

You can compile four different versions:

* Your customizable editor: `make myeditor` (in that case, this directory is enough)
* A demonstrator based on the shell interpreter: `make shell`
* A demonstrator based on lisp mini: `make mini` with the full fledged editor
* A very simple interface to plat with lisp mini: `make lispmini`

## Documentation
The documentation on how to use the editor is here: [Jag Editor](https://github.com/naver/lispe/wiki/1.2-Jag:-Terminal-Editor-With-Mouse-Support-and-Colour-Highlighting)

## Files

We propose different files to show how to proceed.

1. [editormain.cxx](https://github.com/naver/lispe/tree/master/editor/src/editormain.cxx) contains a generic stub that you can modify to implement your own custom editor (use `make myeditor`)
1. [shellmain.cxx](https://github.com/naver/lispe/tree/master/editor/src/shellmain.cxx) contains a simple example of the integration of a shell interpreter (use `make shell`)
1. [minimain.cxx](https://github.com/naver/lispe/tree/master/editor/src/minimain.cxx) contains a specific implementation to execute lisp mini instructions. This is given as an example to help you better understand how to proceed (use `make mini`).
1. [lispminimain.cxx](https://github.com/naver/lispe/tree/master/editor/src/lispminimain.cxx) contains a specific implementation to execute lisp mini instructions (make `lispmini`). The interface is very very limited to one instruction per line.


### Lisp Mini

_Lisp Mini_ is a very simple lisp interpreter. You can test it in two different ways:

1. `make mini` includes the jag editor to test _lisp mini_. It also compiles the lispmini library.
1. `make lispmin` compiles the _lisp mini_ library and a very simple line by line interpreter.

You can read the document: [lisp mini](https://github.com/naver/lispe/tree/master/editor/lisp_mini.md) for more information.

## Deriving your own class

Creating a custom editor is very simple.

You derive a class from _interpreter_editor_ and then you overload the following methods:


### The constructor

We will use the class that is described in [editormain.cxx](https://github.com/naver/lispe/tree/master/editor/src/editormain.cxx) as an example: _my_editor_.

In this class, you need first to provide all the details that are necessary to run your programming language. For instance, in [lispemain.cxx](https://github.com/naver/lispe/tree/master/editor/src/lispemain.cxx), you can see that you need a pointer to _LispE*_ in order to execute some LispE code. 

```C++
class my_editor : public intepreter_editor {
public:
//The code necessary to call your interpreter should be declared here...
```

You then need to initialize this class with the following constructor:

```C++
    my_editor() {
        //Change the message that is displayed when launching the editor
        title_string = "This is myeditor";
        // do  you initialisation here
    }

```
When the editor is launched, it displays the content of *title_string*. You can use this string to display your programming language name and version.

### Initializing your interpreter

You need to overload the following method:

```C++
    //Initialisation of your interpreter
    void init_interpreter(bool reinitialize, string filename) {
        //See [lispemain.cxx](https://github.com/naver/lispe/tree/master/editor/src/lispemain.cxx) for an example of how to set this method
        if (reinitialize)
            cout << m_red << "Reinitializing your interpreter" << endl;
        else
            cout << m_red << "Creating your interpreter ONLY if it is not available yet" << endl;
    }

```

The first argument tells the system whether or not a new instance of the interpreter is needed. If _reinitialize_ is _false_ and there is already an instance of your interpreter, don't do anything. It simply means that you are executing your code line by line.

_filename_ is the current programm that is being executed. It could be an empty string is your executing your instruction in _prompt mode_.

### Loading your code

This function should load the content of a file and compile it. Note that when this method is called, the variable _currentfilename_ is initialized with a pathname that has been provided by the editor when you loaded a program in memory.

```C++
    //Load code from a file name
    void load_code(string& filename) {
        cout << m_red;
        cout << "Loading your code here:" << filename << endl;
        cout << m_current;
    }

```

### Running a program

The following method is called when you press: "ctrl-x-r" in the editor.

```C++
bool run_code() {
    ...
}
```

Note that this command will re-initialize the interpreter and run the code that has been loaded in the editor.

### Executing a command

The following method is executed in _prompt mode_ and allows for a step by step line execution.

```C++
    bool execute_code(wstring& c) {
        //If no interpreter exists, we create one
        //The second parameter records in history this line of code
        init_interpreter(false, thecurrentfilename);
        string code = convert(c);
        cout << m_red;
        cout << "Executing your line of code here:" << code << endl;
        cout << m_current;
        return true;
    }
```

Note that we *init_interpreter* with _false_ as argument. If the interpreter already exists then we will use it, otherwise we create it. (see [lispemain.cxx](https://github.com/naver/lispe/tree/master/editor/src/lispemain.cxx) for an example.)

* convert is a method that convert a _wstring_ into a _string_ object.
* m_red and m_current are predefined colors in [jagget.h](https://github.com/naver/lispe/tree/master/editor/include/jagget.h)

### A Unix command

When the name of a command starts with a "!", the editor will consider the next line to be a Unix or a DOS command.

Two commands are possible:

* !ls : a Unix command is executed
* !v = ls : a Unix command and its content is stored in v

If your language allows for system commands, you can modifiy this method to handle your commands properly:

```C++
wstring unix_command(wstring line) {
    ...
}
```

### Miscelleneous

We have two more commands:

```C++
bool interpreter_active() {...}
stop_execution() {...}
```

1. *interpreter_active* should return _true_ if your interpreter is up and running.
1. *stop_execution* is called by *ctrl-c* to stop the current execution of your interpreter.


## Colors

It is possible to choose other colors than the one that were selected here.

Once you have compiled a version of your editor, you can use the following commands:

### The color codes of the current editor

```sh
# Command to execute in your editor at the prompt
syntax syncolor
# Returns a command that you can use as argument of your editor at launch
-syncolor 0 31 49 3 0 0 0 34 49 0 90 49 0 32 49 1 91 49 7 93 40 
```

A color is defined by three numbers:
1. The _intensity_, whether it is regular, bold, italic or blinking (between 0 and 7)
1. The _foreground or font_ color
1. The _background_ color

When using the "-syncolor" as argument to your editor, notice that each block of 3 digits refer to different types of elements in your editor.
The order is important:


* string: a string in your language such as: "skdjsjk"
* definition: a definition of a function
* instruction: an instruction which is a keyword (see: [tokenize.cxx](https://github.com/naver/lispe/tree/master/editor/src/tokenize.cxx) for the list of keywords that you might want to change)
* quote : In Lisp, elements can be preceeded by a quote
* comments: A comment in your language
* call: A function call in your language
* selection: When you select a line


### File type

When a file is read, its extension triggers a selection (see _setpathname_ in [jag.h](https://github.com/naver/lispe/tree/master/editor/include/jag.h)).

Then a type is selected from the following list (see [jag.h](https://github.com/naver/lispe/tree/master/editor/include/jag.h)):

```C++
no_type: no known type
clike_type: a C-like program
lisp_type: a Lisp program
python_type: a Python program
tamgu_type: a Tamgu program (see https://github.com/naver/tamgu)
```
This _type_ is used in *tokenize_line* (see [tokenize.cxx](https://github.com/naver/lispe/tree/master/editor/src/tokenize.cxx)) to detect which elements should be identified with one of the above definitions.
Every instance is then stored in a _Segmentingtype_ element (see [tokenize.cxx](https://github.com/naver/lispe/tree/master/editor/src/tokenize.cxx)) with its proper definition.

For instance, a C comment such as _//_ will be stored with the definition *jt_comment*:

```C++
infos.append(jt_finalcomment, posbeg, posend); 
```

*posbeg, posend* are the position range of this comment in the code.

These informations are then used to select the right colors in your documents.

You can modify this function for your needs of course.


### The colors available on your terminal

Execute:

```sh
colors
```

If you want to examine a specific color:
```sh
colors 5 95 46
```
