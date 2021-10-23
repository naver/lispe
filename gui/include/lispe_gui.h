/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_gui.cxx
//

#ifndef lispe_gui_h
#define lispe_gui_h


class Doublewindow;

class Fltk_widget : public Element {
public:
    LispE* lisp;
    Fl_Widget* widget;
    Element* function;
    Element* object;
    

    Fltk_widget(LispE* lsp, short t, Element* f, Element* o) : Element(t) {
        lisp = lsp;
        widget = NULL;
        object = null_;
        function = null_;
        
        if (o != null_ && o != NULL) {
            object = new List;
            object->append(lisp->provideAtom(l_quote));
            object->append(o);
            lisp->garbaging(object);
        }

        if (f != null_ && f != NULL)
            function = f;
        
        //In case an error occurs, we use this mechanism to ensure a clean
        //deletion of FLTK objects
    }
    
    ~Fltk_widget() {
        if (widget != NULL)
            delete widget;
    }
    
    Element* labeltype(LispE*);
    Element* labelcolor(LispE*);
    Element* labelfont(LispE*);
    Element* labelsize(LispE*);
    Element* backgroundcolor(LispE*);
    Element* labelwindow(LispE*);
    Element* selectionColor(LispE* lisp);

    void align(LispE*);
    
    void hide(LispE*);
    void focus(LispE*);
    void show(LispE*);

    void  circle(LispE* lisp);
    void  point(LispE* lisp);
    void  pie(LispE* lisp);
    void  rectangleFill(LispE* lisp);
    void  rectangle(LispE* lisp);
    void  drawText(LispE* lisp);
    void  arc(LispE* lisp);
    void  line(LispE* lisp);
    void  lineShape(LispE* lisp);
    

    Element* coordinates(LispE* lisp);
    Element* linerotation(LispE* lisp);
    Element* transform_x(LispE* lisp);
    Element* transform_y(LispE* lisp);
    Element* transform_dx(LispE* lisp);
    Element* transform_dy(LispE* lisp);
    Element* plot(LispE* lisp);
    Element* plotcoords(LispE* lisp);
    Element* textsize(LispE* lisp);
    Element* rgbcolor(LispE* lisp);;

        
    void drawcolor(LispE* lisp);
    void textfont(LispE* lisp);
    void polygon(LispE* lisp);
    void loop(LispE* lisp);
    void scale(LispE* lisp);
    void translate(LispE* lisp);
    void rotate(LispE* lisp);
    void multmatrix(LispE* lisp);
    void transform_vertex(LispE* lisp);
    void pushclip(LispE*);
    void popclip(LispE*);
    
    
    virtual void run() {}
    virtual void finalize(LispE*) {}
    virtual void  close() {}
    virtual Element* ask(LispE* lisp) {return null_;}
    virtual void alert(LispE*) {}


    virtual void bitmap(LispE* lisp) {}
    virtual void gif_image(LispE* lisp) {}
    
    virtual void onclose(Element* f) {}
    
    Element* widget_label(LispE*);
    
    virtual void resize() {}

    virtual Element* value(LispE*) {
        return emptystring_;
    }
    
    virtual void insert(LispE*) {}
    virtual Element* selection(LispE* lisp) {
        return emptystring_;
    }
    
    virtual void wrap() {}
    virtual void boundaries() {}
    virtual void step() {}

    void redraw() {
        if (widget != NULL)
            widget->redraw();
    }
    
    bool check();

    virtual void clean() {
        if (widget != NULL) {
            delete widget;
            widget = NULL;
        }
    }
    virtual void push(Fltk_widget* w) {}
};

class Fltk_window : public Fltk_widget {
public:
    Element* on_close_function;
    string label;
    double time_value;
    bool update;
    bool finalized;
    vector<Fltk_widget*> items;

    Fltk_window(LispE* lsp, short t,  int x, int y, int w, int h, string& label, Element* f, Element* o);
    Fltk_window(LispE* lsp, short t,  int x, int y, string& label, Element* f, Element* o);
    ~Fltk_window();
    
    void push(Fltk_widget* w) {
        items.push_back(w);
        w->increment();
    }
    
    void run();
    void finalize(LispE*);
    void close();
    
    Element* ask(LispE*);
    void alert(LispE*);

    void onclose(Element* f);
    void resize();
    void bitmap(LispE*);
    void gif_image(LispE*);
    
    Doublewindow* window() {
        return (Doublewindow*)widget;
    }

    void clean() {
        close();
    }
};

//------------------------------------------------------------------------------------

class Fltk_input : public Fltk_widget {
public:
    string text;
    string buf;
    
    Fltk_input(LispE* lsp, short t,  int x, int y, int w, int h, bool mltline, string& label, Element* f, Element* o);
    
    Element* value(LispE*);
    void insert(LispE*);
    Element* selection(LispE*);
};

//------------------------------------------------------------------------------------

class Fltk_output : public Fltk_widget {
public:
    string text;
    string buf;
    
    Fltk_output(LispE* lsp, short t,  int x, int y, int w, int h, bool mltline, string& label);
    
    Element* value(LispE*);
    void wrap();

};

//------------------------------------------------------------------------------------

class Fltk_button : public Fltk_widget {
public:

    string text;
    bool image;
    
    Fltk_button(LispE* lsp, short t,  int x, int y, int w, int h, int thetype, int shape, string& label, Element* f, Element* o);
    Element* value(LispE*);
    void bitmap(LispE*);
    void gif_image(LispE*);
    
};

//------------------------------------------------------------------------------------

class Fltk_slider : public Fltk_widget {
public:

    string text;
    
    Fltk_slider(LispE* lsp, short t,  int x, int y, int w, int h, int align, bool value_slider, string& label, Element* f, Element* o);
    Element* value(LispE*);
    void boundaries();
    void step();
};

//------------------------------------------------------------------------------------

class Fltk_bitmap : public Element {
public:
    Fl_Bitmap* bitmap;
    uchar* bm;
    int szw, szh;

    Fltk_bitmap(LispE* lisp, short ty, List* kbitmaps, int w, int h, int sz);
};

//------------------------------------------------------------------------------------
class Fltk_gif : public Element {
public:
    Fl_Image* image;
    string filename;

    Fltk_gif(LispE* lisp, short ty, string& name);
};

//------------------------------------------------------------------------------------

class Doublewindow : public Fl_Double_Window {
public:
    LispE* lisp;
    Fltk_window* fltk_window;
    long i_fltk_window;

    Doublewindow(LispE* lsp, int x, int y, int w, int h, const char* l, Fltk_window* wn);
    Doublewindow(LispE* lsp, int x, int y, const char* l, Fltk_window* wn);

    ~Doublewindow();

    void draw();
};


typedef enum {
    fltk_create_bitmap, fltk_create_gif, fltk_create, fltk_create_resizable,
    fltk_input, fltk_output, fltk_button, fltk_slider, fltk_bitmap, fltk_gif_image,
    fltk_run, fltk_end, fltk_close, fltk_on_close, fltk_value, fltk_insert, fltk_selection, fltk_resize,
    fltk_redraw, fltk_circle, fltk_drawtext, fltk_rectangle, fltk_wrap, fltk_step, fltk_label,
    fltk_rectanglefill, fltk_arc, fltk_pie, fltk_point, fltk_line, fltk_boundaries,
    fltk_textfont, fltk_rgbcolor, fltk_show, fltk_focus, fltk_align, fltk_coordinates,
    fltk_selectioncolor, fltk_labelwindow, fltk_labeltype, fltk_labelcolor, fltk_labelfont, fltk_labelsize, fltk_drawcolor, fltk_polygon,
    fltk_loop, fltk_linerotation, fltk_scale, fltk_translate, fltk_rotate, fltk_multmatrix, fltk_transform_x, fltk_transform_y,
    fltk_transform_dx, fltk_transform_dy, fltk_transform_vertex, fltk_pushclip, fltk_popclip, fltk_textsize, fltk_hide,
    fltk_backgroundcolor, fltk_plot, fltk_plotcoords, fltk_ask, fltk_alert, fltk_lineshape
} fltk_action;

class Lispe_gui : public Element {
public:
    fltk_action action;
    short fltk_widget;
    
    Lispe_gui(LispE*, short idgui, short idwn, fltk_action a);

    Element* eval(LispE* lisp);

    //We use this instruction to return a description of the instruction
    //Indeed, just do: (print getenv) to get this information
    wstring asString(LispE* lisp);
};

#endif


