/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_gui.cxx


/*
 This the template for new 'extensions' in lispe
 */

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Table.H>
#include <FL/Fl_Table_Row.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Multiline_Input.H>
#include <FL/Fl_Output.H>
#include <FL/Fl_Multiline_Output.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Check_Button.H>
#include <FL/Fl_Repeat_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Round_Button.H>
#include <FL/Fl_Text_Editor.H>
#include <FL/Fl_Menu.H>
#include <FL/Fl_Choice.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Value_Slider.H>
#include <FL/fl_draw.H>
#include <FL/Fl_File_Chooser.H>
#include <FL/fl_ask.H>
#include <FL/Fl_GIF_Image.H>
#include <FL/Fl_Text_Display.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/Fl_Tree.H>
#include <FL/Fl_Counter.H>
#include <FL/Fl_Progress.H>

#ifndef FLTKNOJPEG
#include <FL/Fl_JPEG_Image.H>
#endif

#ifdef FLTK14
#include "Fl/Fl_Sys_Menu_Bar.H"
#endif

#include "lispe.h"
#include "lispe_gui.h"

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
void FlResetTimer();
void Fltkclearmainthread();
void Flresetmainthread();
#endif
#else
#include <math.h>
#endif

#define strcpy_s(a,c,b) strncpy(a,b,c)
static std::atomic<bool> stopall;

#define c_min(x,y) (x < y)?x:y
#define c_max(x,y) (x > y)?x:y

static short fltk_type_bitmap = 0;
static short fltk_type_gif = 0;


static void close_callback(Fl_Widget *w, void *data) {
    if (w != NULL) {
        Fltk_window* lispwnd = (Fltk_window*)data;
        if (lispwnd->widget == w) {
#ifdef WIN32
            FlResetTimer();
#else
            Fl::remove_timeout(NULL, NULL);
#endif
            if (!lispwnd->finalized) {
                lispwnd->window()->end();
                lispwnd->finalized = true;
            }

            for (auto& e: lispwnd->items) {
                if (e->widget != NULL) {
                    delete e->widget;
                    e->widget = NULL;
                }
                e->decrement();
            }
            lispwnd->widget = NULL;
            if (lispwnd->main_window)
                stopall = true;
            delete w;
        }
    }
}

#define locking(lisp)  if (lisp->threaded()) Fl::lock();
#define unlocking(lisp)  if (lisp->threaded()) {Fl::unlock(); Fl::awake();}

static void timeout_callback(void *data) {
    if (stopall || data == NULL)
        return;

    Fltk_window* wnd = (Fltk_window*)data;
    Doublewindow* doublewnd = wnd->window();
    if (doublewnd == NULL || !wnd->check())
        return;

    doublewnd->redraw();
    Fl::repeat_timeout(wnd->time_value, timeout_callback, data);
}

static void fltk_callback(Fl_Widget *w, void *data) {
    Fltk_widget* widget = (Fltk_widget*)data;
    Element* func = widget->function;

    LispE* lisp = widget->lisp;

    if (func != NULL) {
        List* call = lisp->provideCall(func, 2);
        call->in_quote(1, widget);
        call->in_quote(2, widget->object);
        Element* res = lisp->delegation->_NULL;
        try {
            res = call->eval(lisp);
        }
        catch (Error* err) {
            call->release();
            throw err;
        }
        res->release();
        call->release();
    }
}

static void fltk_close_callback(Fl_Widget *w, void *data) {
    Fltk_window* widget = (Fltk_window*)data;
    Element* func = widget->on_close_function;

    bool closing = true;

    LispE* lisp = widget->lisp;

    if (func != NULL) {
        List* call = lisp->provideCall(func, 2);
        call->in_quote(1, widget);
        call->in_quote(2, widget->object);
        Element* res = lisp->delegation->_NULL;
        try {
            res = call->eval(widget->lisp);
            closing = res->Boolean();
        }
        catch (Error* err) {
            call->release();
            throw err;
        }
        res->release();
        call->release();
    }

    if (closing)
        close_callback(widget->window(), widget);
}

//------------------------------------------------------------------------------------
bool Fltk_widget::check() {
    if (function != null_ && function != NULL)
        return true;
    return false;
}

void Fltk_widget::hide(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    locking(lisp);
    widget->hide();
    unlocking(lisp);
}

Element* Fltk_widget::backgroundcolor(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(805): Widget not initialized");
    Element* ke = lisp->get_variable(U"color");
    if (ke == null_)
        return lisp->provideInteger(widget->color());

    Fl_Color color = ke->asInt();

    locking(lisp);
    widget->color(color);
    unlocking(lisp);

    return true_;
}


Element* Fltk_widget::selectionColor(LispE* lisp) {

    if (widget == NULL)
        throw new Error("Error: Widget does not exist");

    Element* ke = lisp->get_variable(U"color");
    if (ke == null_)
        return lisp->provideInteger(widget->selection_color());

    Fl_Color color = ke->asInt();

    locking(lisp);
    widget->selection_color(color);
    unlocking(lisp);

    return true_;
}

void Fltk_widget::show(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    locking(lisp);
    widget->show();
    unlocking(lisp);
}

void Fltk_widget::focus(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    locking(lisp);
    widget->take_focus();
    unlocking(lisp);
}

void Fltk_widget::align(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(805): Widget not initialized");
    Element* ke = lisp->get_variable(U"align");
    locking(lisp);
    widget->align(ke->asInt());
    unlocking(lisp);
}

Element* Fltk_widget::widget_label(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(805): Widget not initialized");

    Element* name = lisp->get_variable(U"name");

    string label;
    if (name == null_) {
        label = widget->label();
        return lisp->provideString(label);
    }
    label = name->toString(lisp);
    char* ltxt = new char[label.size() + 1];
    memcpy(ltxt, STR(label), label.size());
    ltxt[label.size()] = 0;
    strcpy_s(ltxt, label.size() + 1, label.c_str());
    widget->label(ltxt);
    delete[] ltxt;
    return true_;
}

Element* Fltk_widget::labelwindow(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(102): wdg not created");
    Element* lab = lisp->get_variable(U"label");
    string label;
    if (lab == null_) {
        label = widget->label();
        return lisp->provideString(label);
    }
    label = lab->toString(lisp);
    char* ltxt = (char*)malloc(label.size() + 1);
    memcpy(ltxt, STR(label), label.size());
    ltxt[label.size()] = 0;
    strcpy_s(ltxt, label.size() + 1, label.c_str());

    locking(lisp);
    widget->label(ltxt);
    unlocking(lisp);
    return true_;
}

Element* Fltk_widget::labeltype(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(102): wdg not created");

    Element* labeltype = lisp->get_variable(U"thetype");
    if (labeltype == null_)
        return lisp->provideInteger(widget->labeltype());

    locking(lisp);
    widget->labeltype((Fl_Labeltype)labeltype->asInt());
    unlocking(lisp);
    return true_;
}

Element* Fltk_widget::labelcolor(LispE* lisp) {

    //In our example, we have only two parameters
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* ke = lisp->get_variable(U"color");
    if (ke == null_)
        return lisp->provideInteger(widget->labelcolor());

    locking(lisp);
    widget->labelcolor(ke->asInt());
    unlocking(lisp);
    return true_;
}

Element* Fltk_widget::labelsize(LispE* lisp) {

    if (widget == NULL)
        throw new Error(L"Error: wdg not initialized");
    Element* ke = lisp->get_variable(U"sz");
    if (ke == null_)
        return lisp->provideInteger(widget->labelsize());

    //0 is the first parameter and so on...
    locking(lisp);
    widget->labelsize(ke->asInt());
    unlocking(lisp);
    return true_;
}

Element* Fltk_widget::labelfont(LispE* lisp) {

    if (widget == NULL)
        throw new Error(L"WND(102): wdg not created");

    Element* labelfont = lisp->get_variable(U"font");
    if (labelfont == null_)
        return lisp->provideInteger(widget->labelfont());

    int font = labelfont->asInt();
    locking(lisp);
    widget->labelfont(font);
    unlocking(lisp);
    return true_;
}

void Fltk_widget::drawText(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
#ifdef WIN32
    //On WIN32, if no font is given beforehand, the whole stuff crashes...
    if (fl_graphics_driver->font_descriptor() == NULL)
        fl_font(FL_HELVETICA, 12);
#endif
    Element* t = lisp->get_variable(U"text");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    string buf = t->toString(lisp);
    char* label = new char[buf.size() + 1];
    strcpy_s(label, buf.size() + 1, buf.c_str());

    locking(lisp);
    fl_draw(label, x->asInt(), y->asInt());
    unlocking(lisp);
    delete[] label;
}

void Fltk_widget::rectangle(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* wx = lisp->get_variable(U"wx");
    Element* hy = lisp->get_variable(U"hy");
    Element* ke = lisp->get_variable(U"color");

    locking(lisp);
    if (ke != null_) {
        Fl_Color color = ke->asInt();
        fl_rect(x->asInt(), y->asInt(), wx->asInt(), hy->asInt(), color);
    }
    else
        fl_rect(x->asInt(), y->asInt(), wx->asInt(), hy->asInt());
    unlocking(lisp);
}

void Fltk_widget::rectangleFill(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* wx = lisp->get_variable(U"wx");
    Element* hy = lisp->get_variable(U"hy");
    Element* ke = lisp->get_variable(U"color");

    locking(lisp);
    if (ke != null_) {
        Fl_Color color = ke->asInt();
        fl_rectf(x->asInt(), y->asInt(), wx->asInt(), hy->asInt(), color);
    }
    else
        fl_rectf(x->asInt(), y->asInt(), wx->asInt(), hy->asInt());
    unlocking(lisp);
}

void Fltk_widget::pie(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* w = lisp->get_variable(U"w");
    Element* h = lisp->get_variable(U"h");
    Element* a1 = lisp->get_variable(U"a1");
    Element* a2 = lisp->get_variable(U"a2");
    locking(lisp);
    fl_pie(x->asInt(), y->asInt(), w->asInt(), h->asInt(), a1->asNumber(), a2->asNumber());
    unlocking(lisp);
}

void Fltk_widget::point(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    fl_point(x->asInt(), y->asInt());
    unlocking(lisp);
}


void Fltk_widget::arc(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* w = lisp->get_variable(U"w");
    Element* h = lisp->get_variable(U"h");
    Element* a1 = lisp->get_variable(U"a1");
    Element* a2 = lisp->get_variable(U"a2");

    locking(lisp);
    if (a2 == null_)
        fl_arc(x->asNumber(), y->asNumber(), w->asNumber(), h->asNumber(), a1->asNumber());
    else
        fl_arc(x->asInt(), y->asInt(), w->asInt(), h->asInt(), a1->asNumber(), a2->asNumber());

    unlocking(lisp);
}

void Fltk_widget::circle(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    int x = lisp->get_variable(U"x")->asInt();
    int y = lisp->get_variable(U"y")->asInt();
    int r = lisp->get_variable(U"r")->asInt();
    Element* kcolor = lisp->get_variable(U"color");

    if (kcolor == null_) {
        locking(lisp);
        fl_circle(x, y, r);
        unlocking(lisp);
        return;
    }

    Fl_Color color = kcolor->asInt();
    //we set the color
    locking(lisp);
    fl_color(color);
    fl_circle(x, y, r);
    unlocking(lisp);}

void Fltk_widget::lineShape(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    int ke = lisp->get_variable(U"type_shape")->asInt();
    int w = lisp->get_variable(U"w")->asInt();

    locking(lisp);
    fl_line_style(ke, w);
    unlocking(lisp);
}

void Fltk_widget::line(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* x1 = lisp->get_variable(U"x1");
    Element* y1 = lisp->get_variable(U"y1");
    Element* x2 = lisp->get_variable(U"x2");
    Element* y2 = lisp->get_variable(U"y2");
    locking(lisp);
    if (x2 == null_ && y2 == null_)
        fl_line(x->asInt(), y->asInt(), x1->asInt(), y1->asInt());
    else {
        fl_line(x->asInt(), y->asInt(), x1->asInt(), y1->asInt(), x2->asInt(), y2->asInt());
    }
    unlocking(lisp);
}

void Fltk_widget::textfont(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* f = lisp->get_variable(U"f");
    Element* sz = lisp->get_variable(U"sz");
    int i = f->asInt();

    locking(lisp);
    fl_font(i, sz->asInt());
    unlocking(lisp);
}

Element* Fltk_widget::rgbcolor(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* r = lisp->get_variable(U"r");
    Element* g = lisp->get_variable(U"g");
    Element* b = lisp->get_variable(U"b");
    return lisp->provideInteger(fl_rgb_color(r->asInt(), g->asInt(), b->asInt()));
}

Element* Fltk_widget::coordinates(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");

    Element* xx = lisp->get_variable(U"x");
    Element* yy = lisp->get_variable(U"y");
    Element* ww = lisp->get_variable(U"w");
    Element* hh = lisp->get_variable(U"h");

    if (xx == null_) {
        Integers* kvect = lisp->provideIntegers();
        kvect->liste.push_back((long)widget->x());
        kvect->liste.push_back((long)widget->y());
        kvect->liste.push_back((long)widget->w());
        kvect->liste.push_back((long)widget->h());
        return kvect;
    }

    int x = xx->asInt();
    int y = yy->asInt();
    int w = ww->asInt();
    int h = hh->asInt();

    if (x >= w || y >= h)
        throw new Error(L"WND(905): Incoherent coordinates");

    locking(lisp);
    widget->resize(x, y, w, h);
    unlocking(lisp);

    return true_;
}


void Fltk_widget::drawcolor(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* ke = lisp->get_variable(U"color");
    Fl_Color color = ke->asInt();
    locking(lisp);
    fl_color(color);
    unlocking(lisp);
}

void Fltk_widget::polygon(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* x1 = lisp->get_variable(U"x1");
    Element* y1 = lisp->get_variable(U"y1");
    Element* x2 = lisp->get_variable(U"x2");
    Element* y2 = lisp->get_variable(U"y2");
    Element* x3 = lisp->get_variable(U"x3");
    Element* y3 = lisp->get_variable(U"y3");

    locking(lisp);
    if (x3 == null_)
        fl_polygon(x->asInt(), y->asInt(), x1->asInt(), y1->asInt(), x2->asInt(), y2->asInt());
    else
        fl_polygon(x->asInt(), y->asInt(), x1->asInt(), y1->asInt(), x2->asInt(), y2->asInt(), x3->asInt(), y3->asInt());
    unlocking(lisp);
}

void Fltk_widget::loop(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* x1 = lisp->get_variable(U"x1");
    Element* y1 = lisp->get_variable(U"y1");
    Element* x2 = lisp->get_variable(U"x2");
    Element* y2 = lisp->get_variable(U"y2");
    Element* x3 = lisp->get_variable(U"x3");
    Element* y3 = lisp->get_variable(U"y3");

    locking(lisp);
    if (x3 == null_)
        fl_loop(x->asInt(), y->asInt(), x1->asInt(), y1->asInt(), x2->asInt(), y2->asInt());
    else {
        fl_loop(x->asInt(), y->asInt(), x1->asInt(), y1->asInt(), x2->asInt(), y2->asInt(), x3->asInt(), y3->asInt());
    }
    unlocking(lisp);
}

Element* Fltk_widget::linerotation(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    double x = lisp->get_variable(U"x")->asNumber();
    double y = lisp->get_variable(U"y")->asNumber();
    double distance = lisp->get_variable(U"distance")->asNumber();
    double angle = lisp->get_variable(U"angle")->asNumber();
    bool draw = lisp->get_variable(U"draw")->Boolean();

    double x1, y1;
    x1 = x + cos(angle)*distance;
    y1 = y - sin(angle)*distance;
    if (draw) {
        locking(lisp);
        fl_line((int)x, (int)y, (int)x1, (int)y1);
        unlocking(lisp);
    }
    Numbers* kvect = lisp->provideNumbers();
    kvect->liste.push_back(x1);
    kvect->liste.push_back(y1);
    return kvect;
}

void Fltk_widget::scale(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    if (y != null_)
        fl_scale(x->asNumber(), y->asNumber());
    else
        fl_scale(x->asNumber());
    unlocking(lisp);
}

void Fltk_widget::translate(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    fl_translate(x->asNumber(), y->asNumber());
    unlocking(lisp);
}

void Fltk_widget::rotate(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* d = lisp->get_variable(U"d");
    locking(lisp);
    fl_rotate(d->asNumber());
    unlocking(lisp);
}

void Fltk_widget::multmatrix(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* a = lisp->get_variable(U"a");
    Element* b = lisp->get_variable(U"b");
    Element* c = lisp->get_variable(U"c");
    Element* d = lisp->get_variable(U"d");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");

    locking(lisp);
    fl_mult_matrix(a->asNumber(), b->asNumber(), c->asNumber(), d->asNumber(), x->asNumber(), y->asNumber());
    unlocking(lisp);
}

Element* Fltk_widget::transform_x(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    double v = fl_transform_x(x->asNumber(), y->asNumber());
    unlocking(lisp);
    return lisp->provideNumber(v);
}

Element* Fltk_widget::transform_y(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    double v = fl_transform_y(x->asNumber(), y->asNumber());
    unlocking(lisp);
    return lisp->provideNumber(v);
}

Element* Fltk_widget::transform_dx(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    double v = fl_transform_dx(x->asNumber(), y->asNumber());
    unlocking(lisp);
    return lisp->provideNumber(v);
}

Element* Fltk_widget::transform_dy(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    locking(lisp);
    double v = fl_transform_dy(x->asNumber(), y->asNumber());
    unlocking(lisp);
    return lisp->provideNumber(v);
}

void Fltk_widget::transform_vertex(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");

    locking(lisp);
    fl_transformed_vertex(x->asNumber(), y->asNumber());
    unlocking(lisp);
}

void Fltk_widget::pushclip(LispE* lisp) {
    //In our example, we have only two parameters
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    //0 is the first parameter and so on...
    Element* x = lisp->get_variable(U"x");
    Element* y = lisp->get_variable(U"y");
    Element* wx = lisp->get_variable(U"wx");
    Element* hy = lisp->get_variable(U"hy");

    locking(lisp);
    fl_push_clip(x->asInt(), y->asInt(), wx->asInt(), hy->asInt());
    unlocking(lisp);
}

void Fltk_widget::popclip(LispE* lisp) {
    //In our example, we have only two parameters
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");

    locking(lisp);
    fl_pop_clip();
    unlocking(lisp);
}

Element* Fltk_widget::textsize(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    Element* t = lisp->get_variable(U"text");
    string buf = t->toString(lisp);
    int w;
    int h;
    fl_measure(buf.c_str(), w, h, 1);
    Integers* iv = lisp->provideIntegers();
    iv->liste.push_back((long)w);
    iv->liste.push_back((long)h);
    return iv;
}

Element* Fltk_widget::plot(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(303): No window available");
    Element* points = lisp->get_variable(U"points");
    if (!points->isList())
        throw new Error(L"WND(872): We expect a list as first parameter.");
    long thickness = lisp->get_variable(U"thickness")->asInt();
    double x, y, a = 0.0, b = 0.0;
    double maxX=0, maxY=0, minX=0, minY=0;
    double minx = 0;
    double miny = 0;
    double maxx = widget->w();
    double maxy = widget->h();
    double incx = 0.0;
    double incy = 0.0;
    char action = 0;
    Element* klandmark = lisp->get_variable(U"landmark");

    if (klandmark != null_) {
        if (!klandmark->isList())
            throw new Error(L"WND(873): We expect a vector as third parameter");
        if (klandmark->size() >= 4) {
            minx = klandmark->index(0)->asNumber();
            miny = klandmark->index(1)->asNumber();
            maxx = klandmark->index(2)->asNumber();
            maxy = klandmark->index(3)->asNumber();
            if (klandmark->size() >= 8) {
                action = 1;
                minX = klandmark->index(4)->asNumber();
                minY = klandmark->index(5)->asNumber();
                maxX = klandmark->index(6)->asNumber();
                maxY = klandmark->index(7)->asNumber();
                if (klandmark->size() >= 10) {
                    action = 2;
                    incx = klandmark->index(8)->asNumber();
                    incy = klandmark->index(9)->asNumber();
                }
            }
        }
    }

    Element* table = points;
    long sz = points->size();
    Numbers* kvect = lisp->provideNumbers();

    long i;

    if (points->isList() && points->protected_index(lisp,(long)0)->isList()) {
        Numbers* fv = lisp->provideNumbers();
        Element* a;
        for (i = 0; i < sz; i++) {
            a = points->index(i);
            if (!a->isList() || (a->size() != 2 && a->size() != 3)) {
                fv->release();
                throw new Error(L"WND(871): The vector should contain vectors of two elements.");
            }

            x = a->index(0)->asNumber();
            y = a->index(1)->asNumber();
                

            fv->liste.push_back(x);
            fv->liste.push_back(y);
            if (a->size() == 3)
                fv->liste.push_back(a->index(2)->asNumber());
            else
                fv->liste.push_back(-1);
            
            if (!action) {
                if (!i) {
                    maxX = x;
                    minX = x;
                    minY = y;
                    maxY = y;
                }
                else {
                    maxX = c_max(x, maxX);
                    minX =  c_min(x, minX);
                    minY =  c_min(y, minY);
                    maxY = c_max(y, maxY);
                }
            }
        }

        if (maxX == minX || maxY == minY) {
            fv->release();
            return kvect;
        }

        table = fv;
        sz = fv->size();
    }
    else {
        if (sz % 2 != 0)
            throw new Error(L"WND(871): The vector should contain an even number of elements.");

        if (!action) {
            for (i = 0; i < sz; i += 2) {
                x = table->index(i)->asNumber();
                y = table->index(i + 1)->asNumber();
                if (!i) {
                    maxX = x;
                    minX = maxX;
                    minY = x;
                    maxY = minY;
                }
                else {
                    minX =  c_min(minX, x);
                    maxX = c_max(maxX, x);
                    minY =  c_min(minY, y);
                    maxY = c_max(maxY, y);
                }
            }
            if (maxX == minX || maxY == minY)
                return kvect;
        }
    }

    kvect->liste.push_back(minx); //0
    kvect->liste.push_back(miny); //1
    kvect->liste.push_back(maxx); //2
    kvect->liste.push_back(maxy); //3
    if (thickness > 1) {
        minx += thickness;
        miny += thickness >> 1;
        maxx -= thickness;
        maxy -= (thickness << 1) - (thickness >> 1);
    }
    if (action != 2) {
        incx = (maxx - minx) / (maxX - minX);
        incy = (maxy - miny) / (maxY - minY);
    }
    
    int point_color;
    
    for (i = 0; i < sz; i += 3) {
        x = table->index(i)->asNumber();
        y = table->index(i + 1)->asNumber();
        point_color = table->index(i + 2)->asInt();
        x = minx + incx*x - incx*minX;
        y = miny + maxy - incy*y + incy*minY;
        if (point_color != -1)
            fl_color((unsigned int)point_color);
        
        if (!thickness) {
            if (i) {
                locking(lisp);
                fl_line((int)a, (int)b, (int)x, (int)y);
                unlocking(lisp);
            }
            a = x;
            b = y;
        }
        else {
            locking(lisp);
            if (thickness == 1)
                fl_point((int)x, (int)y);
            else
                fl_circle((int)x, (int)y, thickness);
            unlocking(lisp);
        }
    }

    kvect->liste.push_back(minX); //4
    kvect->liste.push_back(minY); //5
    kvect->liste.push_back(maxX); //6
    kvect->liste.push_back(maxY); //7
    kvect->liste.push_back(incx); //8
    kvect->liste.push_back(incy); //9
    kvect->liste.push_back((double)thickness); //10

    if (table != points)
        table->release();

    return kvect;
}

Element* Fltk_widget::plotcoords(LispE* lisp) {
    Element* kvect = lisp->get_variable(U"landmark");
    if (kvect->size() != 11)
        throw new Error(L"WND(862): Wrong values to compute coordinates");
    double x = lisp->get_variable(U"x")->asNumber();
    double y = lisp->get_variable(U"y")->asNumber();
    double minx = kvect->index(0)->asNumber();
    double miny = kvect->index(1)->asNumber();
    double maxy = kvect->index(3)->asNumber();
    double minX = kvect->index(4)->asNumber();
    double minY = kvect->index(5)->asNumber();
    double incx = kvect->index(8)->asNumber();
    double incy = kvect->index(9)->asNumber();
    int thickness = kvect->index(10)->asNumber();
    if (thickness > 1) {
        minx += thickness;
        miny += thickness >> 1;
        maxy -= (thickness << 1) - (thickness >> 1);
    }
    x = minx + incx*x - incx*minX;
    y = miny + maxy - incy*y + incy*minY;
    Numbers* kres = lisp->provideNumbers();
    kres->liste.push_back(x);
    kres->liste.push_back(y);
    return kres;
}

//------------------------------------------------------------------------------------
Fltk_input::Fltk_input(LispE* lsp, short t,  int x, int y, int w, int h, bool multiline, string& label, Element* f, Element* o) : Fltk_widget(lsp, t, f, o) {
    text = label;

    if (multiline)
        widget = new Fl_Multiline_Input(x, y, w, h, text.c_str());
    else
        widget = new Fl_Input(x, y, w, h, text.c_str());

    if (function != null_)
        widget->callback(fltk_callback, this);
}

Element* Fltk_input::value(LispE* lisp) {
    if (widget == NULL)
        throw new Error("WND(677): Input not initialized");
    Element* val = lisp->get_variable(U"val");

    locking(lisp);
    if (val == null_) {
        buf = ((Fl_Input*)widget)->value();
        unlocking(lisp);
        return lisp->provideString(buf);
    }
    buf = val->toString(lisp);
    ((Fl_Input*)widget)->value(buf.c_str());
    unlocking(lisp);
    return true_;
}

void Fltk_input::insert(LispE* lisp) {
    if (widget == NULL)
        throw new Error("WND(677): Input not initialized");

    buf = lisp->get_variable(U"val")->toString(lisp);
    int pos = lisp->get_variable(U"pos")->asInt();
    locking(lisp);
    ((Fl_Input*)widget)->insert(buf.c_str(), pos);
    unlocking(lisp);
}


Element* Fltk_input::selection(LispE* lisp) {
    if (widget == NULL)
        throw new Error(L"WND(677): Input not initialized");

    locking(lisp);
    int pos = ((Fl_Input*)widget)->position();
    int nb = ((Fl_Input*)widget)->mark();
    buf =((Fl_Input*)widget)->value();
    unlocking(lisp);

    buf = buf.substr(pos, nb);
    return lisp->provideString(buf);
}
//------------------------------------------------------------------------------------
Fltk_output::Fltk_output(LispE* lsp, short t,  int x, int y, int w, int h, bool multiline, string& label) : Fltk_widget(lsp, t, NULL, NULL) {
    text = label;

    if (multiline)
        widget = new Fl_Multiline_Output(x, y, w, h, text.c_str());
    else
        widget = new Fl_Output(x, y, w, h, text.c_str());

    if (function != null_)
        widget->callback(fltk_callback, this);
}

Element* Fltk_output::value(LispE* lisp) {
    if (widget == NULL)
        throw new Error("WND(679): Output not initialized");

    Element* val = lisp->get_variable(U"val");


    if (val == null_) {
        locking(lisp);
        buf = ((Fl_Output*)widget)->value();
        unlocking(lisp);
        return lisp->provideString(buf);
    }
    buf = val->toString(lisp);
    locking(lisp);
    ((Fl_Output*)widget)->value(buf.c_str());
    unlocking(lisp);
    return true_;
}

void Fltk_output::wrap() {
    if (widget == NULL)
        throw new Error("WND(679): Output not initialized");

    bool mode = lisp->get_variable(U"mode")->Boolean();
    locking(lisp);
    if (mode)
        ((Fl_Output*)widget)->align(FL_ALIGN_WRAP);
    else
        ((Fl_Output*)widget)->align(FL_ALIGN_NOWRAP);
    unlocking(lisp);
}

//------------------------------------------------------------------------------------
class ButtonImage : public Fl_Button {
public:
    // CONSTRUCTOR
    Fl_Image* myimage;
    bool isbitmap;
    unsigned int thecolor;
    string mytext;
    int myalign;

    ButtonImage(int X, int Y, int W, int H, const char*L = 0) : Fl_Button(X, Y, W, H, L) {
        myimage = NULL;
        myalign = FL_ALIGN_CENTER;
        isbitmap = false;
        thecolor = FL_BLACK;
    }

    void draw() {
        int X = x() + Fl::box_dx(box());  // area inside the button's frame
        int Y = y() + Fl::box_dy(box());
        int W = w() - Fl::box_dw(box());
        int H = h() - Fl::box_dh(box());
        draw_box(value() ? (down_box() ? down_box() : fl_down(box())) : box(), color());  // button up/dn
        fl_push_clip(X, Y, W, H);                  // clip inside button's frame
        {
            if (myimage != NULL) {
                int imgx = X + ((myimage->w() < W) ? (W - myimage->w()) / 2 : 0);
                // horiz center
                int imgy = Y + ((myimage->h() < H) ? (H - myimage->h()) / 2 : 0);
                // vert center
                if (isbitmap)
                    fl_color(thecolor);
                myimage->draw(imgx, imgy);
            }
            if (mytext != "") {
                fl_color(labelcolor());
                fl_font(labelfont(), labelsize());
                fl_draw(mytext.c_str(), x(), y(), w(), h(), myalign);
            }
        }
        fl_pop_clip();
    }
};

Fltk_button::Fltk_button(LispE* lsp, short t,  int x, int y, int w, int h, int thetype, int shape, string& label, Element* f, Element* o) :
Fltk_widget(lsp, t, f, o) {

    image = false;
    text = label;
    switch (thetype) {
    case 1: //check
        widget = new Fl_Check_Button(x, y, w, h, text.c_str());
        break;
    case 2://light
        widget = new Fl_Light_Button(x, y, w, h);
        break;
    case 3://Repeat
        widget = new Fl_Repeat_Button(x, y, w, h, text.c_str());
        break;
    case 4://Return
        widget = new Fl_Return_Button(x, y, w, h, text.c_str());
        break;
    case 5://Round
        widget = new Fl_Round_Button(x, y, w, h, text.c_str());
        break;
    case 6://Image
        widget = new ButtonImage(x, y, w, h, text.c_str());
        image = true;
        break;
    default://regular
        widget = new Fl_Button(x, y, w, h, text.c_str());
    }
    widget->type(shape);
    if (function != null_)
        widget->callback(fltk_callback, this);
}

Element* Fltk_button::value(LispE* lisp) {
    //In our example, we have only two parameters
    if (widget == NULL)
        throw new Error("WND(805): Button not initialized");

    Element* v = lisp->get_variable(U"val");
    locking(lisp);
    if (v == null_) {
        long val = ((Fl_Button*)widget)->value();
        unlocking(lisp);
        return lisp->provideInteger(val);
    }
    ((Fl_Button*)widget)->value(v->asInt());
    unlocking(lisp);
    return true_;
}

void Fltk_button::bitmap(LispE* lisp) {
    if (widget == NULL)
        throw new Error("WND(805): Button not initialized");
    if (!image)
        throw new Error("WND(809): Image button required");

    Element* kbitmap = lisp->get_variable(U"bitmap");
    if (kbitmap->type != fltk_type_bitmap)
        throw new Error("Error: this is not a bitmap");

    Fl_Color color = lisp->get_variable(U"color")->asInt();

    ((ButtonImage*)widget)->thecolor = color;
    ((ButtonImage*)widget)->isbitmap = true;
    ((ButtonImage*)widget)->myimage = ((Fltk_bitmap*)kbitmap)->bitmap;
}

void Fltk_button::gif_image(LispE* lisp) {
    if (widget == NULL)
        throw new Error("WND(805): Button not initialized");
    if (!image)
        throw new Error("WND(809): Image button required");

    Element* gif = lisp->get_variable(U"gif");
    if (gif->type != fltk_type_gif)
        throw new Error("Error: this is not a GIF object");

    ((ButtonImage*)widget)->myimage = ((Fltk_gif*)gif)->image;
}

//------------------------------------------------------------------------------------
Fltk_slider::Fltk_slider(LispE* lsp, short t,  int x, int y, int w, int h, int align, bool value_slider, string& label, Element* f, Element* o) : Fltk_widget(lsp, t, f, o) {
    text = label;

    if (value_slider)
        widget = new Fl_Value_Slider(x, y, w, h, text.c_str());
    else
        widget = new Fl_Slider(x, y, w, h, text.c_str());

    ((Fl_Slider*)widget)->align(align);
    ((Fl_Slider*)widget)->type(1);
    ((Fl_Slider*)widget)->step(1);

    if (function != null_)
        widget->callback(fltk_callback, this);
}

void Fltk_slider::boundaries() {
    if (widget == NULL)
        throw new Error("WND(101): No slider available");
    double x = lisp->get_variable(U"low")->asNumber();
    double y = lisp->get_variable(U"high")->asNumber();
    locking(lisp);
    ((Fl_Slider*)widget)->bounds(x, y);
    unlocking(lisp);
}

void Fltk_slider::step() {
    if (widget == NULL)
        throw new Error("WND(101): No slider available");
    double stp = lisp->get_variable(U"stp")->asNumber();
    locking(lisp);
    ((Fl_Slider*)widget)->step(stp);
    unlocking(lisp);
}

Element* Fltk_slider::value(LispE* lisp) {
    if (widget == NULL)
        throw new Error("WND(101): No slider available");
    Element* val = lisp->get_variable(U"val");
    locking(lisp);
    if (val == null_) {
        double v = ((Fl_Slider*)widget)->value();
        unlocking(lisp);
        return lisp->provideNumber(v);
    }
    ((Fl_Slider*)widget)->value(val->asNumber());
    unlocking(lisp);
    return true_;
}

//------------------------------------------------------------------------------------
Fltk_bitmap::Fltk_bitmap(LispE* lisp, short ty, List* kbitmaps, int w, int h, int sz) : Element(ty) {
    //If you do not have any parameters, then your function might return whatever you want... Here itself
    szw = w;
    szh = h;

    int rowBytes = (szw + 7) >> 3;
    bm = new uchar[rowBytes*szh];
    for (int i = 0; i < sz; i++) {
        bm[i] = (uchar)kbitmaps->index(i)->asInt();
    }
    bitmap = new Fl_Bitmap(bm, szw, szh);
}
//------------------------------------------------------------------------------------

Fltk_gif::Fltk_gif(LispE* lisp, short ty, string& name) : Element(ty) {
    image = NULL;
    filename = name;
    if (filename != "") {
        image = new Fl_GIF_Image(filename.c_str());
        if (image == NULL)
            throw new Error("Error: Image not loaded");
    }
}
//------------------------------------------------------------------------------------

Fltk_window::Fltk_window(LispE* lsp, short t, int x, int y, int w, int h, string& l, Element* f, Element* o) : Fltk_widget(lsp, t, f, o) {
    main_window = false;
    update = true;
    finalized = false;
    label = l;
    widget = new Doublewindow(lisp, x,y,w,h,label.c_str(), this);
    on_close_function = null_;
    lisp->delegation->toBeCleanedOnError(this, lisp->threaded());
#ifdef FLTK14
    Fl_Sys_Menu_Bar::window_menu_style(Fl_Sys_Menu_Bar::no_window_menu);
#endif

}

Fltk_window::Fltk_window(LispE* lsp, short t, int x, int y, string& l, Element* f, Element* o) : Fltk_widget(lsp, t, f, o) {
    main_window = false;
    update = true;
    finalized = false;
    label = l;
    widget = new Doublewindow(lisp, x,y,label.c_str(), this);
    on_close_function = null_;
    lisp->delegation->toBeCleanedOnError(this, lisp->threaded());
#ifdef FLTK14
    Fl_Sys_Menu_Bar::window_menu_style(Fl_Sys_Menu_Bar::no_window_menu);
#endif

}

Fltk_window::~Fltk_window() {
    lisp->delegation->removeFromForceClean(this, lisp->threaded());
    close_callback(widget, this);
}

void Fltk_window::finalize(LispE* lisp) {
    stopall = false;
    window()->end();
    finalized = true;

    Element* timer = lisp->get_variable(U"timer");
    if (timer != null_) {
        time_value = timer->asNumber();
#ifdef WIN32
        FlResetTimer();
#endif
        Fl::add_timeout(time_value, timeout_callback, this);
    }
    widget->show();
}

void fltk_reinit() {
#ifdef WIN32
    Fltkclearmainthread();
#endif

    Fl::lock();

#ifdef WIN32
    Fl::unlock();
    Flresetmainthread();
    Fl::lock();
#endif

#ifdef __apple_build_version__
    Fl::awake_ring_head_ = 0;
    Fl::awake_ring_tail_ = 0;
    Fl::unlock();
    Fl::lock();
#endif
}

void Fltk_window::close() {
    close_callback(window(), this);
}

#define FOREVER 1e20

void Fltk_window::run() {
    if (lisp->threadId())
        throw new Error("Error: cannot execute 'run' from a thread");
    if (!finalized)
        throw new Error("Error: you need to call 'end' on the main window");
    fltk_reinit();
    main_window = true;
    Error* error = NULL;

    if (widget != NULL) {
        long tt = 1;
        try {
            while (!stopall && tt && !lisp->hasStopped()) {
                tt = Fl::wait(FOREVER);
            }
        }
        catch (Error* err) {
            stopall = true;
            Fl::wait(0);
            Fl::unlock();
            throw err;
        }
        Fl::wait(0);
        Fl::unlock();
    }
    stopall = true;
}

void Fltk_window::bitmap(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");
    Element* kbitmap = lisp->get_variable(U"bitmap");
    if (kbitmap->type != fltk_type_bitmap)
        throw new Error("Error: this is not a bitmap");

    Fl_Color color = lisp->get_variable(U"color")->asInt();

    int x = lisp->get_variable(U"x")->asInt();
    int y = lisp->get_variable(U"y")->asInt();
    int w = ((Fltk_bitmap*)kbitmap)->szw;
    int h = ((Fltk_bitmap*)kbitmap)->szh;

    Fl_Bitmap* fl = ((Fltk_bitmap*)kbitmap)->bitmap;
    if (!finalized) {
        Fl_Box* box = new Fl_Box(x, y, w, h);
        box->labelcolor(color);
        box->image(fl);
        return;
    }
    fl_color(color);
    fl->draw(x, y, w, h);
}

void Fltk_window::gif_image(LispE* lisp) {
    if (widget == NULL)
        throw new Error("Error: Window does not exist");

    Element* gif = lisp->get_variable(U"gif");
    if (gif->type != fltk_type_gif)
        throw new Error("Error: this is not a GIF image");

    int x = lisp->get_variable(U"x")->asInt();
    int y = lisp->get_variable(U"y")->asInt();
    int wx = lisp->get_variable(U"wx")->asInt();
    int wy = lisp->get_variable(U"wy")->asInt();

    Fl_Image* fl = ((Fltk_gif*)gif)->image;
    if (fl == NULL)
        throw new Error("Error: Unloaded image");
    if (!finalized) {
        Fl_Box* box = new Fl_Box(x, y, wx, wy);
        box->image(fl);
    }
    else
        fl->draw(x, y, wx, wy);
}

void Fltk_window::resize() {
    if (widget == NULL)
        throw new Error(L"Error: Widget not initialized");
    int minw = lisp->get_variable(U"minw")->asInt();
    int minh = lisp->get_variable(U"minh")->asInt();
    int maxw = lisp->get_variable(U"maxw")->asInt();
    int maxh = lisp->get_variable(U"maxh")->asInt();
    window()->size_range(minw, minh, maxw, maxh);
}

Element* Fltk_window::ask(LispE* lisp) {
    int a;
    string msg = lisp->get_variable(U"msg")->toString(lisp);
    string msg1 = lisp->get_variable(U"msg1")->toString(lisp);
    string msg2 = lisp->get_variable(U"msg2")->toString(lisp);
    Element* emsg3 = lisp->get_variable(U"msg3");
    Element* emsg4 = lisp->get_variable(U"msg4");
    locking(lisp);
    if (emsg3 == null_)
        a = fl_choice(msg.c_str(), msg1.c_str(), msg2.c_str(), NULL);
    else {
        string msg3 = emsg3->toString(lisp);
        if (emsg4 == null_)
            a = fl_choice(msg.c_str(), msg1.c_str(), msg2.c_str(), msg3.c_str());
        else {
            string msg4 = emsg4->toString(lisp);
            a = fl_choice(msg.c_str(), msg1.c_str(), msg2.c_str(), msg3.c_str(), msg4.c_str());
        }
    }
    unlocking(lisp);
    return lisp->provideInteger(a);
}

void Fltk_window::alert(LispE* lisp) {
    string msg = lisp->get_variable(U"msg")->toString(lisp);
    locking(lisp);
    fl_alert(msg.c_str());
    unlocking(lisp);
}

void Fltk_window::onclose(Element* f) {
    on_close_function = f;
    widget->callback(fltk_close_callback, this);
}

//------------------------------------------------------------------------------------------------

Doublewindow::Doublewindow(LispE* lsp, int x, int y, int w, int h, const char* l, Fltk_window* wn) : Fl_Double_Window(x, y, w, h, l) {
    lisp = lsp;
    fltk_window = wn;

    i_fltk_window = lisp->vpool_add(fltk_window);
    callback(close_callback, fltk_window);
}

Doublewindow::Doublewindow(LispE* lsp, int x, int y, const char* l, Fltk_window* wn) : Fl_Double_Window(x, y, l) {
    lisp = lsp;
    fltk_window = wn;

    i_fltk_window = lisp->vpool_add(fltk_window);
    callback(close_callback, fltk_window);
}

Doublewindow::~Doublewindow() {
    lisp->vpool_release(i_fltk_window);
}

void Doublewindow::draw() {
    if (stopall)
        return;

    if (lisp->vpool_check(fltk_window, i_fltk_window)) {
        if (fltk_window->update)
            Fl_Double_Window::draw();
    }
    else
        return;

    if (fltk_window->check()) {
        fl_color(FL_BLACK); //we set FL_BLACK as the default color, it can be modified with drawcolor in the code...
        List* call = lisp->provideCall(fltk_window->function, 2);
        call->in_quote(1, fltk_window);
        call->in_quote(2, fltk_window->object);
        Element* res = null_;
        try {
            res = call->eval(lisp);
        }
        catch (Error* err) {
            string label = err->toString(lisp);
            fl_draw(STR(label), 10, 10);
            call->release();
            throw err;
        }
        catch (...) {
            res->release();
            call->release();
            stopall = true;
            return;
        }
        res->release();
        call->release();
    }
}

Lispe_gui::Lispe_gui(LispE* lisp, short fltk, short fltk_w, fltk_action a) : action(a), Element(fltk) {
    fltk_widget = fltk_w;
}

Element* Lispe_gui::eval(LispE* lisp) {
    //The name defined in the extension is not insignificant, it is used to retrieve our arguments.
    Fltk_widget* wnd = NULL;

    //Before fltk_run, object creation methods
    if (action > fltk_create_resizable) {
        Element* e = lisp->get_variable(U"widget");
        if (e->type != fltk_widget)
            throw new Error("Error: Expecting a 'widget' object");
        wnd = (Fltk_window*)e;
    }

    switch (action) {
        case fltk_create: {
            int x = lisp->get_variable(U"x")->asInt();
            int y = lisp->get_variable(U"y")->asInt();
            int w = lisp->get_variable(U"w")->asInt();
            int h = lisp->get_variable(U"h")->asInt();
            string label = lisp->get_variable(U"label")->toString(lisp);
            Element* function  = lisp->get_variable(U"function");
            Element* object  = lisp->get_variable(U"object");
            return new Fltk_window(lisp,fltk_widget,x,y,w,h, label,function, object);
        }
        case fltk_create_resizable: {
            int x = lisp->get_variable(U"x")->asInt();
            int y = lisp->get_variable(U"y")->asInt();
            string label = lisp->get_variable(U"label")->toString(lisp);
            Element* function  = lisp->get_variable(U"function");
            Element* object  = lisp->get_variable(U"object");
            return new Fltk_window(lisp,fltk_widget,x,y, label,function, object);
        }
        case fltk_input: {
            int x = lisp->get_variable(U"x")->asInt();
            int y = lisp->get_variable(U"y")->asInt();
            int w = lisp->get_variable(U"w")->asInt();
            int h = lisp->get_variable(U"h")->asInt();
            bool multiline = lisp->get_variable(U"multiline")->Boolean();
            string label = lisp->get_variable(U"label")->toString(lisp);
            Element* function  = lisp->get_variable(U"function");
            Element* object  = lisp->get_variable(U"object");
            Fltk_input* wdg = new Fltk_input(lisp,fltk_widget,x,y,w,h, multiline,label,function, object);
            wnd->push(wdg);
            return wdg;
        }
        case fltk_output: {
            int x = lisp->get_variable(U"x")->asInt();
            int y = lisp->get_variable(U"y")->asInt();
            int w = lisp->get_variable(U"w")->asInt();
            int h = lisp->get_variable(U"h")->asInt();
            bool multiline = lisp->get_variable(U"multiline")->Boolean();
            string label = lisp->get_variable(U"label")->toString(lisp);
            Fltk_output* wdg = new Fltk_output(lisp,fltk_widget,x,y,w,h, multiline,label);
            wnd->push(wdg);
            return wdg;
        }
        case fltk_button: {
            //widget x y w h label function (object) (button_type) (button_shape)
            int x = lisp->get_variable(U"x")->asInt();
            int y = lisp->get_variable(U"y")->asInt();
            int w = lisp->get_variable(U"w")->asInt();
            int h = lisp->get_variable(U"h")->asInt();
            int thetype = lisp->get_variable(U"button_type")->asInt();
            int shape = lisp->get_variable(U"button_shape")->asInt();
            string label = lisp->get_variable(U"label")->toString(lisp);
            Element* function  = lisp->get_variable(U"function");
            Element* object  = lisp->get_variable(U"object");
            Fltk_button* wdg = new Fltk_button(lisp,fltk_widget,x,y,w,h, thetype, shape, label,function, object);
            wnd->push(wdg);
            return wdg;
        }
        case fltk_slider: {
            int x = lisp->get_variable(U"x")->asInt();
            int y = lisp->get_variable(U"y")->asInt();
            int w = lisp->get_variable(U"w")->asInt();
            int h = lisp->get_variable(U"h")->asInt();
            int align = lisp->get_variable(U"slider_orientation")->asInt();
            bool value_slider = lisp->get_variable(U"slider_value_type")->Boolean();
            string label = lisp->get_variable(U"label")->toString(lisp);
            Element* function  = lisp->get_variable(U"function");
            Element* object  = lisp->get_variable(U"object");
            Fltk_slider* wdg = new Fltk_slider(lisp,fltk_widget,x,y,w,h, align,value_slider,label,function, object);
            wnd->push(wdg);
            return wdg;
        }
        case fltk_on_close: {
            Element* function  = lisp->get_variable(U"function");
            wnd->onclose(function);
            return true_;
        }
        case fltk_run:
            wnd->run();
            return true_;
        case fltk_resize:
            wnd->resize();
            return true_;
        case fltk_close:
            wnd->close();
            return true_;
        case fltk_end:
            wnd->finalize(lisp);
            return true_;
        case fltk_redraw:
            wnd->redraw();
            return true_;
        case fltk_circle:
            wnd->circle(lisp);
            return true_;
        case fltk_selectioncolor:
            return wnd->selectionColor(lisp);
        case fltk_drawtext:
            wnd->drawText(lisp);
            return true_;
        case fltk_rectangle:
            wnd->rectangle(lisp);
            return true_;
        case fltk_rectanglefill:
            wnd->rectangleFill(lisp);
            return true_;
        case fltk_arc:
            wnd->arc(lisp);
            return true_;
        case fltk_pie:
            wnd->pie(lisp);
            return true_;
        case fltk_point:
            wnd->point(lisp);
            return true_;
        case fltk_line:
            wnd->line(lisp);
            return true_;
        case fltk_lineshape:
            wnd->lineShape(lisp);
            return true_;
        case fltk_textfont:
            wnd->textfont(lisp);
            return true_;
        case fltk_rgbcolor:
            return wnd->rgbcolor(lisp);
        case fltk_show:
            wnd->show(lisp);
            return true_;
        case fltk_focus:
            wnd->focus(lisp);
            return true_;
        case fltk_align:
            wnd->align(lisp);
            return true_;
        case fltk_coordinates:
            return wnd->coordinates(lisp);
        case fltk_labelwindow:
            return wnd->labelwindow(lisp);
        case fltk_labeltype:
            return wnd->labeltype(lisp);
        case fltk_labelcolor:
            return wnd->labelcolor(lisp);
        case fltk_labelfont:
            return wnd->labelfont(lisp);
        case fltk_labelsize:
            return wnd->labelsize(lisp);
        case fltk_drawcolor:
            wnd->drawcolor(lisp);
            return true_;
        case fltk_polygon:
            wnd->polygon(lisp);
            return true_;
        case fltk_loop:
            wnd->loop(lisp);
            return true_;
        case fltk_linerotation:
            return wnd->linerotation(lisp);
        case fltk_scale:
            wnd->scale(lisp);
            return true_;
        case fltk_translate:
            wnd->translate(lisp);
            return true_;
        case fltk_rotate:
            wnd->rotate(lisp);
            return true_;
        case fltk_multmatrix:
            wnd->multmatrix(lisp);
            return true_;
        case fltk_transform_x:
            return wnd->transform_x(lisp);
        case fltk_transform_y:
            return wnd->transform_y(lisp);
        case fltk_transform_dx:
            return wnd->transform_dx(lisp);
        case fltk_transform_dy:
            return wnd->transform_dy(lisp);
        case fltk_transform_vertex:
            wnd->transform_vertex(lisp);
            return true_;
        case fltk_pushclip:
            wnd->pushclip(lisp);
            return true_;
        case fltk_popclip:
            wnd->popclip(lisp);
            return true_;
        case fltk_textsize:
            return wnd->textsize(lisp);
        case fltk_hide:
            wnd->hide(lisp);
            return true_;
        case fltk_backgroundcolor:
            return wnd->backgroundcolor(lisp);
        case fltk_plot:
            return wnd->plot(lisp);
        case fltk_ask:
            return wnd->ask(lisp);
        case fltk_alert:
            wnd->alert(lisp);
            return true_;
        case fltk_plotcoords:
            return wnd->plotcoords(lisp);
        case fltk_value:
            return wnd->value(lisp);
        case fltk_insert:
            wnd->insert(lisp);
            return true_;
        case fltk_selection:
            return wnd->selection(lisp);
        case fltk_wrap:
            wnd->wrap();
            return true_;
        case fltk_step:
            wnd->step();
            return true_;
        case fltk_boundaries:
            wnd->boundaries();
            return true_;
        case fltk_label:
            return wnd->widget_label(lisp);
        case fltk_create_bitmap: {
            Element* kbitmaps = lisp->get_variable(U"bitmap");
            if (!kbitmaps->isList())
                throw new Error("Error: The argument should be a list of integers in 'fltk_create_bitmap'");
            
            int w = lisp->get_variable(U"length")->asInt();
            int h = lisp->get_variable(U"height")->asInt();
            int sz = (int)kbitmaps->size();
            
            if (sz != ((w*h) >> 3) )
                throw new Error("Wrong size for the bitmap. sz=(width*height)/8 in 'fltk_create_bitmap'");
            
            return new Fltk_bitmap(lisp, fltk_type_bitmap, (List*)kbitmaps, w, h, sz);
        }
        case fltk_bitmap:
            wnd->bitmap(lisp);
            return true_;
        case fltk_create_gif: {
            string filename = lisp->get_variable(U"filename")->toString(lisp);
            return new Fltk_gif(lisp, fltk_type_gif, filename);
        }
        case fltk_gif_image:
            wnd->gif_image(lisp);
            return true_;
    }
    
    return true_;
}

//We use this instruction to return a description of the instruction
//Indeed, just do: (print gui_example) to get this information
wstring Lispe_gui::asString(LispE* lisp) {
    switch (action) {
        case fltk_create:
            return L"Create a window";
        case fltk_run:
            return L"Launch the GUI";
        case fltk_create_resizable:
            return L"Create a window";
        case fltk_create_gif:
            return L"Create a GIF container";
        case fltk_gif_image:
            return L"Add a GIF image to a widget";
        case fltk_resize:
            return L"resize a window";
        case fltk_close:
            return L"close window";
        case fltk_redraw:
            return L"Redraw the window";
        case fltk_circle:
            return L"Draw a circle. 'color' is optional.";
        case fltk_drawtext:
            return L"Put a text at position xy";
        case fltk_rectangle:
            return L"Draw a rectangle with optional color c";
        case fltk_rectanglefill:
            return L"Fill a rectangle with optional color c";
        case fltk_arc:
            return L"Draw an arc.\nAdd a series of points to the current path on the arc of a circle;";
        case fltk_pie:
            return L"Draw a pie";
        case fltk_point:
            return L"Draw a pixel";
        case fltk_on_close:
            return L"This method sets a callback function to catch when the window is closing";
        case fltk_line:
            return L"Draw a line between points x2 and y2 are optional";
        case fltk_rgbcolor:
            return L"return the value of rgb values combined";
        case fltk_show:
            return L"Show the window";
        case fltk_focus:
            return L"Get the focus";
        case fltk_align:
            return L"define the label alignment";
        case fltk_selectioncolor:
            return L"Color for the selected elements";
        case fltk_labeltype:
            return L"set or return the label type";
        case fltk_labelcolor:
            return L"set or return the label color";
        case fltk_labelfont:
            return L"set or return the label font";
        case fltk_labelsize:
            return L"set or return the label font size";
        case fltk_drawcolor:
            return L"set the color for the next drawings";
        case fltk_polygon:
            return L"Draw a polygon x3 and y3 are optional";
        case fltk_loop:
            return L"Draw a series of lines x3 and y3 are optional";
        case fltk_scale:
            return L"Scale the current transformation";
        case fltk_translate:
            return L"translate the current transformation";
        case fltk_rotate:
            return L"rotate of degree d the current transformation";
        case fltk_multmatrix:
            return L"combine transformations";
        case fltk_pushclip:
            return L"Insert a clip region with the following coordinates";
        case fltk_popclip:
            return L"Release a clip region";
        case fltk_hide:
            return L"Hide the window";
        case fltk_backgroundcolor:
            return L"set the background color";
        case fltk_plot:
            return L"Plot a graph from a table of successive xy points according to window size. If thickness===0 then points are continuously plotted else defines the diameter of the point. Return a which is used with plotcoords. The landmark is optional it is has the following structure: [XmaxWindow,YmaxWindow,XminValue,YminValue,XmaxValue,YmaxValue,incX,incY]. incX,incY are also optional.";
        case fltk_plotcoords:
            return L"Compute the coordinates of a point(xy) according to the previous scale computed with plot. Returns a of two elements [xsys] corresponding to the screen coordinates in the current window.";
        case fltk_ask:
            return L"Pop up window to pose a question";
        case fltk_alert:
            return L"Pop up window to display an alert";
        case fltk_lineshape:
            return L"Select the line shape and its thikness";
        case fltk_end:
            return L"Finalize the creation of a window a set an optional timer";
        case fltk_textfont:
            return L"Set a font with a size";
        case fltk_coordinates:
            return L"return the coordinates of the window or set new coordinates";
        case fltk_labelwindow:
            return L"return the window label or set a new label to the window";
        case fltk_linerotation:
            return L"Compute the coordinate of a rotated point from point xy using a distance and an angle. Return a vector of floats with the new coordinates.";
        case fltk_transform_x:
            return L"Transform a coordinate X using the current transformation matrix";
        case fltk_transform_y:
            return L"Transform a coordinate Y using the current transformation matrix";
        case fltk_transform_dx:
            return L"Transform a distance DX using the current transformation matrix.";
        case fltk_transform_dy:
            return L"Transform a distance DY using the current transformation matrix.";
        case fltk_transform_vertex:
            return L"Add transformations to vertices list.";
        case fltk_textsize:
            return L"Return a map with w and h as key to denote width and height of the string in pixels";
        case fltk_input:
            return L"Creating an input object";
        case fltk_value:
            return L"Set or return the widget value";
        case fltk_insert:
            return L"Insert text in a widget value";
        case fltk_selection:
            return L"Selection in a widget value";
        case fltk_wrap:
            return L"Set wrap mode for a widget";
        case fltk_output:
            return L"Create an output widget";
        case fltk_button:
            return L"Create a button";
        case fltk_slider:
            return L"Create a slider";
        case fltk_step:
            return L"Define the step in a slider";
        case fltk_boundaries:
            return L"Define the boundaries of a slider";
        case fltk_label:
            return L"Return or set the label of a widget";
        case fltk_create_bitmap:
            return L"Create a bitmap object out of a list of integers and its dimension: size = (width*height)/8";
        case fltk_bitmap:
            return L"Add a bitmap to a window or to a button";
    }
    return L"fltk";
}

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    //We first create the body of the function
    u_ustring nom = U"fltk_gui_";
    short fltk_gui = lisp->encode(nom);
    nom = U"fltk_widget_";
    short fltk_widget = lisp->encode(nom);

    nom = U"fltk_bitmap_";
    fltk_type_bitmap = lisp->encode(nom);

    nom = U"fltk_gif_image_";
    fltk_type_gif = lisp->encode(nom);

    lisp->storing_global(U"FL_FOREGROUND_COLOR", lisp->provideConstinteger(FL_FOREGROUND_COLOR));
    lisp->storing_global(U"FL_BACKGROUND2_COLOR", lisp->provideConstinteger(FL_BACKGROUND2_COLOR));
    lisp->storing_global(U"FL_BACKGROUND_COLOR", lisp->provideConstinteger(FL_BACKGROUND_COLOR));
    lisp->storing_global(U"FL_INACTIVE_COLOR", lisp->provideConstinteger(FL_INACTIVE_COLOR));
    lisp->storing_global(U"FL_SELECTION_COLOR", lisp->provideConstinteger(FL_SELECTION_COLOR));
    lisp->storing_global(U"FL_GRAY0", lisp->provideConstinteger(FL_GRAY0));
    lisp->storing_global(U"FL_DARK3", lisp->provideConstinteger(FL_DARK3));
    lisp->storing_global(U"FL_DARK2", lisp->provideConstinteger(FL_DARK2));
    lisp->storing_global(U"FL_DARK1", lisp->provideConstinteger(FL_DARK1));
    lisp->storing_global(U"FL_LIGHT1", lisp->provideConstinteger(FL_LIGHT1));
    lisp->storing_global(U"FL_LIGHT2", lisp->provideConstinteger(FL_LIGHT2));
    lisp->storing_global(U"FL_LIGHT3", lisp->provideConstinteger(FL_LIGHT3));
    lisp->storing_global(U"FL_BLACK", lisp->provideConstinteger(FL_BLACK));
    lisp->storing_global(U"FL_RED", lisp->provideConstinteger(FL_RED));
    lisp->storing_global(U"FL_GREEN", lisp->provideConstinteger(FL_GREEN));
    lisp->storing_global(U"FL_YELLOW", lisp->provideConstinteger(FL_YELLOW));
    lisp->storing_global(U"FL_BLUE", lisp->provideConstinteger(FL_BLUE));
    lisp->storing_global(U"FL_MAGENTA", lisp->provideConstinteger(FL_MAGENTA));
    lisp->storing_global(U"FL_CYAN", lisp->provideConstinteger(FL_CYAN));
    lisp->storing_global(U"FL_DARK_RED", lisp->provideConstinteger(FL_DARK_RED));
    lisp->storing_global(U"FL_DARK_GREEN", lisp->provideConstinteger(FL_DARK_GREEN));
    lisp->storing_global(U"FL_DARK_YELLOW", lisp->provideConstinteger(FL_DARK_YELLOW));
    lisp->storing_global(U"FL_DARK_BLUE", lisp->provideConstinteger(FL_DARK_BLUE));
    lisp->storing_global(U"FL_DARK_MAGENTA", lisp->provideConstinteger(FL_DARK_MAGENTA));
    lisp->storing_global(U"FL_DARK_CYAN", lisp->provideConstinteger(FL_DARK_CYAN));
    lisp->storing_global(U"FL_WHITE", lisp->provideConstinteger(FL_WHITE));

    lisp->storing_global(U"FL_SOLID",lisp->provideConstinteger(FL_SOLID));
    lisp->storing_global(U"FL_DASH",lisp->provideConstinteger(FL_DASH));
    lisp->storing_global(U"FL_DOT",lisp->provideConstinteger(FL_DOT));
    lisp->storing_global(U"FL_DASHDOT",lisp->provideConstinteger(FL_DASHDOT));
    lisp->storing_global(U"FL_DASHDOTDOT",lisp->provideConstinteger(FL_DASHDOTDOT));
    lisp->storing_global(U"FL_CAP_FLAT",lisp->provideConstinteger(FL_CAP_FLAT));
    lisp->storing_global(U"FL_CAP_ROUND",lisp->provideConstinteger(FL_CAP_ROUND));
    lisp->storing_global(U"FL_CAP_SQUARE",lisp->provideConstinteger(FL_CAP_SQUARE));
    lisp->storing_global(U"FL_JOIN_MITER",lisp->provideConstinteger(FL_JOIN_MITER));
    lisp->storing_global(U"FL_JOIN_ROUND",lisp->provideConstinteger(FL_JOIN_ROUND));
    lisp->storing_global(U"FL_JOIN_BEVEL",lisp->provideConstinteger(FL_JOIN_BEVEL));

    lisp->storing_global(U"FL_VERT_SLIDER", lisp->provideConstinteger(FL_VERT_SLIDER));
    lisp->storing_global(U"FL_HOR_SLIDER", lisp->provideConstinteger(FL_HOR_SLIDER));
    lisp->storing_global(U"FL_VERT_FILL_SLIDER", lisp->provideConstinteger(FL_VERT_FILL_SLIDER));
    lisp->storing_global(U"FL_HOR_FILL_SLIDER", lisp->provideConstinteger(FL_HOR_FILL_SLIDER));
    lisp->storing_global(U"FL_VERT_NICE_SLIDER", lisp->provideConstinteger(FL_VERT_NICE_SLIDER));
    lisp->storing_global(U"FL_HOR_NICE_SLIDER", lisp->provideConstinteger(FL_HOR_NICE_SLIDER));

    lisp->storing_global(U"FL_HELVETICA", lisp->provideConstinteger(0));
    lisp->storing_global(U"FL_HELVETICA_BOLD", lisp->provideConstinteger(1));
    lisp->storing_global(U"FL_HELVETICA_ITALIC", lisp->provideConstinteger(2));
    lisp->storing_global(U"FL_HELVETICA_BOLD_ITALIC", lisp->provideConstinteger(3));
    lisp->storing_global(U"FL_COURIER", lisp->provideConstinteger(4));
    lisp->storing_global(U"FL_COURIER_BOLD", lisp->provideConstinteger(5));
    lisp->storing_global(U"FL_COURIER_ITALIC", lisp->provideConstinteger(6));
    lisp->storing_global(U"FL_COURIER_BOLD_ITALIC", lisp->provideConstinteger(7));
    lisp->storing_global(U"FL_TIMES", lisp->provideConstinteger(8));
    lisp->storing_global(U"FL_TIMES_BOLD", lisp->provideConstinteger(9));
    lisp->storing_global(U"FL_TIMES_ITALIC", lisp->provideConstinteger(10));
    lisp->storing_global(U"FL_TIMES_BOLD_ITALIC", lisp->provideConstinteger(11));
    lisp->storing_global(U"FL_SYMBOL", lisp->provideConstinteger(12));
    lisp->storing_global(U"FL_SCREEN", lisp->provideConstinteger(13));
    lisp->storing_global(U"FL_SCREEN_BOLD", lisp->provideConstinteger(14));
    lisp->storing_global(U"FL_ZAPF_DINGBATS", lisp->provideConstinteger(15));
    lisp->storing_global(U"FL_FREE_FONT", lisp->provideConstinteger(16));
    lisp->storing_global(U"FL_BOLD", lisp->provideConstinteger(1));
    lisp->storing_global(U"FL_ITALIC", lisp->provideConstinteger(2));
    lisp->storing_global(U"FL_BOLD_ITALIC", lisp->provideConstinteger(3));

    lisp->storing_global(U"FL_REGULAR_BUTTON_TYPE", lisp->provideConstinteger(0));
    lisp->storing_global(U"FL_CHECK_BUTTON_TYPE", lisp->provideConstinteger(1));
    lisp->storing_global(U"FL_LIGHT_BUTTON_TYPE", lisp->provideConstinteger(2));
    lisp->storing_global(U"FL_REPEAT_BUTTON_TYPE", lisp->provideConstinteger(3));
    lisp->storing_global(U"FL_RETURN_BUTTON_TYPE", lisp->provideConstinteger(4));
    lisp->storing_global(U"FL_ROUND_BUTTON_TYPE", lisp->provideConstinteger(5));
    lisp->storing_global(U"FL_IMAGE_BUTTON_TYPE", lisp->provideConstinteger(6));

    lisp->storing_global(U"FL_NORMAL_BUTTON_SHAPE", lisp->provideConstinteger(FL_NORMAL_BUTTON));
    lisp->storing_global(U"FL_TOGGLE_BUTTON_SHAPE", lisp->provideConstinteger(FL_TOGGLE_BUTTON));
    lisp->storing_global(U"FL_RADIO_BUTTON_SHAPE", lisp->provideConstinteger(FL_RADIO_BUTTON));
    lisp->storing_global(U"FL_HIDDEN_BUTTON_SHAPE", lisp->provideConstinteger(FL_HIDDEN_BUTTON));

    lisp->storing_global(U"FL_WHEN_NEVER", lisp->provideConstinteger(FL_WHEN_NEVER));
    lisp->storing_global(U"FL_WHEN_CHANGED", lisp->provideConstinteger(FL_WHEN_CHANGED));
    lisp->storing_global(U"FL_WHEN_RELEASE", lisp->provideConstinteger(FL_WHEN_RELEASE));
    lisp->storing_global(U"FL_WHEN_RELEASE_ALWAYS", lisp->provideConstinteger(FL_WHEN_RELEASE_ALWAYS));
    lisp->storing_global(U"FL_WHEN_ENTER_KEY", lisp->provideConstinteger(FL_WHEN_ENTER_KEY));
    lisp->storing_global(U"FL_WHEN_ENTER_KEY_ALWAYS", lisp->provideConstinteger(FL_WHEN_ENTER_KEY_ALWAYS));

    lisp->storing_global(U"FL_ALIGN_CENTER", lisp->provideConstinteger(FL_ALIGN_CENTER));
    lisp->storing_global(U"FL_ALIGN_TOP", lisp->provideConstinteger(FL_ALIGN_TOP));
    lisp->storing_global(U"FL_ALIGN_BOTTOM", lisp->provideConstinteger(FL_ALIGN_BOTTOM));
    lisp->storing_global(U"FL_ALIGN_LEFT", lisp->provideConstinteger(FL_ALIGN_LEFT));
    lisp->storing_global(U"FL_ALIGN_RIGHT", lisp->provideConstinteger(FL_ALIGN_RIGHT));
    lisp->storing_global(U"FL_ALIGN_INSIDE", lisp->provideConstinteger(FL_ALIGN_INSIDE));
    lisp->storing_global(U"FL_ALIGN_TEXT_OVER_IMAGE", lisp->provideConstinteger(FL_ALIGN_TEXT_OVER_IMAGE));
    lisp->storing_global(U"FL_ALIGN_IMAGE_OVER_TEXT", lisp->provideConstinteger(FL_ALIGN_IMAGE_OVER_TEXT));
    lisp->storing_global(U"FL_ALIGN_CLIP", lisp->provideConstinteger(FL_ALIGN_CLIP));
    lisp->storing_global(U"FL_ALIGN_WRAP", lisp->provideConstinteger(FL_ALIGN_WRAP));
    lisp->storing_global(U"FL_ALIGN_IMAGE_NEXT_TO_TEXT", lisp->provideConstinteger(FL_ALIGN_IMAGE_NEXT_TO_TEXT));
    lisp->storing_global(U"FL_ALIGN_TEXT_NEXT_TO_IMAGE", lisp->provideConstinteger(FL_ALIGN_TEXT_NEXT_TO_IMAGE));
    lisp->storing_global(U"FL_ALIGN_IMAGE_BACKDROP", lisp->provideConstinteger(FL_ALIGN_IMAGE_BACKDROP));
    lisp->storing_global(U"FL_ALIGN_TOP_LEFT", lisp->provideConstinteger(FL_ALIGN_TOP_LEFT));
    lisp->storing_global(U"FL_ALIGN_TOP_RIGHT", lisp->provideConstinteger(FL_ALIGN_TOP_RIGHT));
    lisp->storing_global(U"FL_ALIGN_BOTTOM_LEFT", lisp->provideConstinteger(FL_ALIGN_BOTTOM_LEFT));
    lisp->storing_global(U"FL_ALIGN_BOTTOM_RIGHT", lisp->provideConstinteger(FL_ALIGN_BOTTOM_RIGHT));
    lisp->storing_global(U"FL_ALIGN_LEFT_TOP", lisp->provideConstinteger(FL_ALIGN_LEFT_TOP));
    lisp->storing_global(U"FL_ALIGN_RIGHT_TOP", lisp->provideConstinteger(FL_ALIGN_RIGHT_TOP));
    lisp->storing_global(U"FL_ALIGN_LEFT_BOTTOM", lisp->provideConstinteger(FL_ALIGN_LEFT_BOTTOM));
    lisp->storing_global(U"FL_ALIGN_RIGHT_BOTTOM", lisp->provideConstinteger(FL_ALIGN_RIGHT_BOTTOM));
    lisp->storing_global(U"FL_ALIGN_NOWRAP", lisp->provideConstinteger(FL_ALIGN_NOWRAP));
    lisp->storing_global(U"FL_ALIGN_POSITION_MASK", lisp->provideConstinteger(FL_ALIGN_POSITION_MASK));
    lisp->storing_global(U"FL_ALIGN_IMAGE_MASK", lisp->provideConstinteger(FL_ALIGN_IMAGE_MASK));

    lisp->extension("deflib fltk_create (x y w h label (function) (object))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_create));
    lisp->extension("deflib fltk_create_resizable (x y label (function) (object))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_create_resizable));

    lisp->extension("deflib fltk_resize (widget minw minh maxw maxh)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_resize));
    lisp->extension("deflib fltk_end (widget (timer))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_end));
    lisp->extension("deflib fltk_run (widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_run));

    lisp->extension("deflib fltk_arc(widget x y w h a1 (a2))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_arc));
    lisp->extension("deflib fltk_selection_color(widget (color))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_selectioncolor));
    lisp->extension("deflib fltk_drawtext(widget text x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_drawtext));
    lisp->extension("deflib fltk_rectangle(widget x y wx hy (color))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_rectangle));
    lisp->extension("deflib fltk_rectanglefill(widget x y wx hy (color))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_rectanglefill));
    lisp->extension("deflib fltk_pie(widget x y w h a1 a2)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_pie));
    lisp->extension("deflib fltk_point(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_point));
    lisp->extension("deflib fltk_circle(widget x y r (color))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_circle));
    lisp->extension("deflib fltk_lineshape(widget type_shape w)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_lineshape));
    lisp->extension("deflib fltk_line(widget x y x1 y1 (x2) (y2))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_line));

    lisp->extension("deflib fltk_show(widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_show));
    lisp->extension("deflib fltk_redraw(widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_redraw));
    lisp->extension("deflib fltk_focus(widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_focus));

    lisp->extension("deflib fltk_textfont(widget f sz)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_textfont));
    lisp->extension("deflib fltk_rgbcolor(widget r g b)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_rgbcolor));
    lisp->extension("deflib fltk_align(widget align)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_align));
    lisp->extension("deflib fltk_coordinates(widget (x) (y) (w) (h) )", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_coordinates));
    lisp->extension("deflib fltk_labelwindow(widget (label))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_labelwindow));
    lisp->extension("deflib fltk_labeltype(widget (thetype))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_labeltype));
    lisp->extension("deflib fltk_labelcolor(widget (color) )", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_labelcolor));
    lisp->extension("deflib fltk_labelfont(widget (font) )", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_labelfont));
    lisp->extension("deflib fltk_labelsize(widget (sz))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_labelsize));
    lisp->extension("deflib fltk_drawcolor(widget (color))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_drawcolor));
    lisp->extension("deflib fltk_polygon(widget x y x1 y1 x2 y2 (x3) (y3))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_polygon));
    lisp->extension("deflib fltk_loop(widget x y x1 y1 x2 y2 (x3) (y3))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_loop));
    lisp->extension("deflib fltk_rotation(widget x y distance angle (draw))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_linerotation));
    lisp->extension("deflib fltk_scale(widget x (y))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_scale));
    lisp->extension("deflib fltk_translate(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_translate));
    lisp->extension("deflib fltk_rotate(widget d)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_rotate));
    lisp->extension("deflib fltk_multmatrix(widget a b c d x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_multmatrix));
    lisp->extension("deflib fltk_transform_x(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_transform_x));
    lisp->extension("deflib fltk_transform_y(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_transform_y));
    lisp->extension("deflib fltk_transform_dx(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_transform_dx));
    lisp->extension("deflib fltk_transform_dy(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_transform_dy));
    lisp->extension("deflib fltk_transform_vertex(widget x y)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_transform_vertex));
    lisp->extension("deflib fltk_pushclip(widget x y wx wy)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_pushclip));
    lisp->extension("deflib fltk_popclip(widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_popclip));
    lisp->extension("deflib fltk_textsize(widget text)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_textsize));
    lisp->extension("deflib fltk_hide(widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_hide));
    lisp->extension("deflib fltk_backgroundcolor(widget (color))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_backgroundcolor));
    lisp->extension("deflib fltk_plot(widget points thickness (landmark) )", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_plot));
    lisp->extension("deflib fltk_plotcoords(widget x y landmark)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_plotcoords));
    lisp->extension("deflib fltk_ask(widget msg msg1 msg2 (msg3) (msg4))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_ask));
    lisp->extension("deflib fltk_alert(widget msg)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_alert));
    lisp->extension("deflib fltk_close(widget)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_close));
    lisp->extension("deflib fltk_on_close(widget function)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_on_close));

    lisp->extension("deflib fltk_label(widget (name))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_label));

    lisp->extension("deflib fltk_input (widget x y w h label multiline (function) (object))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_input));
    lisp->extension("deflib fltk_value (widget (val))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_value));
    lisp->extension("deflib fltk_insert (widget val)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_insert));
    lisp->extension("deflib fltk_selection (widget pos nb)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_selection));

    lisp->extension("deflib fltk_output (widget x y w h label multiline)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_output));
    lisp->extension("deflib fltk_wrap (widget mode)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_wrap));

    lisp->extension("deflib fltk_button (widget x y w h label function (button_type) (button_shape) (object))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_button));

    lisp->extension("deflib fltk_slider (widget x y w h label slider_orientation slider_value_type function (object))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_slider));
    lisp->extension("deflib fltk_step (widget stp)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_step));
    lisp->extension("deflib fltk_boundaries (widget low high)", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_boundaries));

    lisp->extension("deflib fltk_create_bitmap (bitmap length height)", new Lispe_gui(lisp, fltk_gui, fltk_type_bitmap, fltk_create_bitmap));
    lisp->extension("deflib fltk_bitmap (widget bitmap color (x) (y))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_bitmap));


    lisp->extension("deflib fltk_create_gif (filename)", new Lispe_gui(lisp, fltk_gui, fltk_type_gif, fltk_create_gif));
    lisp->extension("deflib fltk_gif (widget gif (x) (y) (wx) (wy))", new Lispe_gui(lisp, fltk_gui, fltk_widget, fltk_gif_image));

    return true;
}

}

#ifdef FLTKGUI
void moduleGUI(LispE* lisp) {
    InitialisationModule(lisp);
}
#endif

