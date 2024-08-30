/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  jag.cxx
//
//


#include <stdio.h>

#ifdef WIN32
#define PATH_MAX 4096
#else
#include <unistd.h>   //_getch
#include <termios.h>  //_getch
#include <sys/ioctl.h>
#endif

#include <signal.h>
#include <iomanip>

#include "tools.h"
#include "jag.h"
#include "rgx.h"

#ifdef WIN32
void ResetWindowsConsole();
void Getscreensizes(bool);
bool checkresize();
void Returnscreensize(long& rs, long& cs, long& sr, long& sc);
#endif

//--------------------------------------------------------


#ifdef APPLE_COPY
void quoted_string(string& value) {
    if (value == "")
        return;

    value = s_replacingstring(value, "\\", "\\\\");
    value = s_replacingstring(value, "\"", "\\\"");
    value = s_replacingstring(value, "\\t", "\\\\t");
    value = s_replacingstring(value, "\\n", "\\\\n");
    value = s_replacingstring(value, "\\r", "\\\\r");
}

string exec_command(const char* cmd) {
	FILE* pipe = popen(cmd, "r");

    if (!pipe)
        return "";

    char buffer[256];
    string result = "";
    while(!feof(pipe))
    {
        if(fgets(buffer, 256, pipe) != NULL)
        {
            result += buffer;
        }
    }
	pclose(pipe);
    return result;
}

string paste_from_clipboard() {
    return exec_command("pbpaste");
}

void copy_to_clipboard(string buffer) {
    quoted_string(buffer);
    stringstream cmd;
    cmd << "echo \"" << STR(buffer) << "\" | pbcopy";
    exec_command(cmd.str().c_str());
}
#else
string paste_from_clipboard() {
    return "";
}
void copy_to_clipboard(string buffer) {}
#endif


using std::stringstream;

extern char m_scrollmargin[];
static char m_right[] = {27, '[', '0', '0', '1', 67, 0};
char m_down[] = {27, '[', '0', '0', '1', 66, 0};

static const int16_t _getbuffsize = 128;

//Moving to a specific line/column
char sys_row_column[] = { 27, 91, '0', '0', '0', ';', '0', '0','0', 'H', 0 };
static const char* localn999[] = { "000","001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428","429","430","431","432","433","434","435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480","481","482","483","484","485","486","487","488","489","490","491","492","493","494","495","496","497","498","499","500","501","502","503","504","505","506","507","508","509","510","511","512","513","514","515","516","517","518","519","520","521","522","523","524","525","526","527","528","529","530","531","532","533","534","535","536","537","538","539","540","541","542","543","544","545","546","547","548","549","550","551","552","553","554","555","556","557","558","559","560","561","562","563","564","565","566","567","568","569","570","571","572","573","574","575","576","577","578","579","580","581","582","583","584","585","586","587","588","589","590","591","592","593","594","595","596","597","598","599","600","601","602","603","604","605","606","607","608","609","610","611","612","613","614","615","616","617","618","619","620","621","622","623","624","625","626","627","628","629","630","631","632","633","634","635","636","637","638","639","640","641","642","643","644","645","646","647","648","649","650","651","652","653","654","655","656","657","658","659","660","661","662","663","664","665","666","667","668","669","670","671","672","673","674","675","676","677","678","679","680","681","682","683","684","685","686","687","688","689","690","691","692","693","694","695","696","697","698","699","700","701","702","703","704","705","706","707","708","709","710","711","712","713","714","715","716","717","718","719","720","721","722","723","724","725","726","727","728","729","730","731","732","733","734","735","736","737","738","739","740","741","742","743","744","745","746","747","748","749","750","751","752","753","754","755","756","757","758","759","760","761","762","763","764","765","766","767","768","769","770","771","772","773","774","775","776","777","778","779","780","781","782","783","784","785","786","787","788","789","790","791","792","793","794","795","796","797","798","799","800","801","802","803","804","805","806","807","808","809","810","811","812","813","814","815","816","817","818","819","820","821","822","823","824","825","826","827","828","829","830","831","832","833","834","835","836","837","838","839","840","841","842","843","844","845","846","847","848","849","850","851","852","853","854","855","856","857","858","859","860","861","862","863","864","865","866","867","868","869","870","871","872","873","874","875","876","877","878","879","880","881","882","883","884","885","886","887","888","889","890","891","892","893","894","895","896","897","898","899","900","901","902","903","904","905","906","907","908","909","910","911","912","913","914","915","916","917","918","919","920","921","922","923","924","925","926","927","928","929","930","931","932","933","934","935","936","937","938","939","940","941","942","943","944","945","946","947","948","949","950","951","952","953","954","955","956","957","958","959","960","961","962","963","964","965","966","967","968","969","970","971","972","973","974","975","976","977","978","979","980","981","982","983","984","985","986","987","988","989","990","991","992","993","994","995","996","997","998","999" };

static void moveto_row_column(long r, long c) {
	sys_row_column[2] = localn999[r][0];
	sys_row_column[3] = localn999[r][1];
	sys_row_column[4] = localn999[r][2];
	sys_row_column[6] = localn999[c][0];
	sys_row_column[7] = localn999[c][1];
	sys_row_column[8] = localn999[c][2];
	cout << sys_row_column;
}

void jag_editor::displaythehelp(long noclear) {
    if (!noclear) {
        cout << m_clear << m_clear_scrolling << m_home;
        cerr << m_redital << "Help Jag Editor" << m_current << endl << endl;
    }

    cerr << "   " << m_redbold << "Commands" << m_current << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-d:" << m_current << " delete a full line" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-f:" << m_current << " find a string (press 'tab' to switch between case sensitive or not)" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-g:" << m_current << " move to a specific line, '$' is the end of the code" << endl;
#ifdef WIN32
    cerr << "   \t- " << m_redbold << "Ctrl+Alt-h:" << m_current << " local help" << endl;
#else
    cerr << "   \t- " << m_redbold << "Ctrl-h:" << m_current << " local help" << endl;
#endif
    cerr << "   \t- " << m_redbold << "Ctrl-k:" << m_current << " delete from cursor up to the end of the line" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-l:" << m_current << " load file from disk" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-n:" << m_current << " find next" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-q:" << m_current << " exit the editor" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-r:" << m_current << " redo last modification" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-t:" << m_current << " reindent the code" << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-u:" << m_current << " undo last modification" << endl << endl;
    cerr << "   \t- " << m_redbold << "Alt-x:" << m_current << " cut mouse selection" << endl;
    cerr << "   \t- " << m_redbold << "Alt-c:" << m_current << " copy mouse selection" << endl;
    cerr << "   \t- " << m_redbold << "Alt-v:" << m_current << " paste mouse selection" << endl;
    cerr << "   \t- " << m_redbold << "Alt-+:" << m_current << " indent current line or selected lines to the right" << endl;
    cerr << "   \t- " << m_redbold << "Alt--:" << m_current << " de-indent current line or selected lines to the left" << endl << endl;
    cerr << "   \t- " << m_redbold << "Ctrl-x:" << m_redital << " Combined Commands" << m_current << endl;
    cerr << "   \t\t- " << m_redital << "D:" << m_current << " delete a bloc of lines" << endl;
    cerr << "   \t\t- " << m_redital << "n:" << m_current << " hide/display line numbers" << endl;
    cerr << "   \t\t- " << m_redital << "c:" << m_current << " copy a bloc of lines" << endl;
    cerr << "   \t\t- " << m_redital << "x:" << m_current << " cut a bloc of lines" << endl;
    cerr << "   \t\t- " << m_redital << "v:" << m_current << " paste a bloc of lines" << endl;
    cerr << "   \t\t- " << m_redital << "f:" << m_current << " find with jag regular expressions" << endl;
    cerr << "   \t\t- " << m_redital << "F:" << m_current << " find with posix regular expressions" << endl;
    cerr << "   \t\t- " << m_redital << "d:" << m_current << " debug the code" << endl;
    cerr << "   \t\t- " << m_redital << "r:" << m_current << " run the code" << endl;
    cerr << "   \t\t- " << m_redital << "w:" << m_current << " write and quit" << endl;
    cerr << "   \t\t- " << m_redital << "l:" << m_current << " reload a file" << endl;
    cerr << "   \t\t- " << m_redital << "h:" << m_current << " full help" << endl;
    cerr << "   \t\t- " << m_redital << "m:" << m_current << " toggle mouse on/off" << endl;
    cerr << "   \t\t- " << m_redital << "u:" << m_current << " toggle between top and bottom of the screen" << endl;
    cerr << "   \t\t- " << m_redital << "+:" << m_current << " indent current line or selected lines to the right" << endl;
    cerr << "   \t\t- " << m_redital << "-:" << m_current << " de-indent current line or selected lines to the left" << endl;
    cerr << "   \t\t- " << m_redital << "q:" << m_current << " quit" << endl << endl;
}

//--------------------------------------------------------
long editor_lines::indent(long p) {
    long i = p;
    wchar_t c;
    while (i >= 0) {
        c = lines[i][0];
        if (c != 32 && c!= 9 && c)
            break;
        i--;
    }
    wstring cd = code(i, p + 1);
    string ccd;
    s_unicode_to_utf8(ccd, cd);
    long ln = VirtualIndentation(ccd, (jag->filetype == lisp_type), (jag->filetype == python_type));
    return ln;
}

void editor_keep::display() {
    string s;
    wstring w = l_keeplines.back();
    s_unicode_to_utf8(s, w);
    cout << s << endl;
    cout << l_keeppos.back() << endl;
    cout << l_keepactions.back() << endl;
    cout << l_keepcurrentline.back() << endl;
    cout << l_keepposinstring.back() << endl;
    cout << l_keeptop.back() << endl;
    cout << l_keepstatus.back() << endl;
}

//--------------------------------------------------------
static int xcursor = 0, ycursor = 0;

static int getxcursor() {
    return xcursor;
}

static int getycursor() {
    return ycursor;
}

static void scrollingdown(long rowsize) {
	char buff[] = { 0,0,0,0,0,0 };
	sprintf_s(buff, 4, "%0.3ld", rowsize + 1);
	m_scrollmargin[6] = buff[0];
	m_scrollmargin[7] = buff[1];
	m_scrollmargin[8] = buff[2];
	cout << m_scrollmargin << m_scrolldown;
	sprintf_s(buff, 4, "%0.3ld", rowsize + 2);
	m_scrollmargin[6] = buff[0];
	m_scrollmargin[7] = buff[1];
	m_scrollmargin[8] = buff[2];
	cout << m_scrollmargin;
}

//--------------------------------------------------------
//------------------------------------------------------------------------------------
//We check if the buffer ends in an incomplete utf8 character...
//In that case, we remove the ending and return it as a value to be added later
bool jag_editor::check_utf8(string& buff, string& buffer) {
    long sz = buff.size()-1;
    unsigned char utf[4];
    utf[2] = buff[sz];
    utf[1] = buff[sz-1];
    utf[0] = buff[sz-2];

    if (utf[2] < 0x80)
        return false;

    if ((utf[2] & 0xF0)== 0xF0 || (utf[2] & 0xE0) == 0xE0 || (utf[2] & 0xC0) == 0xC0) {
        buff = buff.substr(0, sz);
        buffer = utf[2];
        return true;
    }

    if ((utf[2] & 0xC0) == 0xC0)
        return false;

    if ((utf[1] & 0xF0) == 0xF0 || (utf[1] & 0xE0) == 0xE0) {
        buff = buff.substr(0, sz - 1);
        buffer = utf[1];
        buffer += utf[2];
        return true;
    }

    if ((utf[0] & 0xE0)== 0xE0)
        return false;

    if ((utf[0] & 0xF0)== 0xF0) {
        buff = buff.substr(0, sz - 2);
        buffer = utf[1];
        buffer += utf[2];
        buffer += utf[3];
        return true;
    }

    return false;
}

//------------------------------------------------------------------------------------
bool evaluate_quotes(wstring& l) {
    long pos = l.find(L"\"");
    char nb = false;
    while (pos != -1) {
        if (!pos || l[pos-1] != '\\')
            nb = 1 - nb;
        pos = l.find(L"\"", pos + 1);
    }
    return nb;
}
//------------------------------------------------------------------------------------
void jag_editor::vsplit(wstring& thestr, wstring thesplitter, vector<wstring>& vs) {
    s_split(thestr, thesplitter, vs, true);
}

//------------------------------------------------------------------------------------
jag_editor* JAGEDITOR = NULL;
//------------------------------------------------------------------------------------
#ifdef WIN32
void resizewindow() {
	bool m = JAGEDITOR->mouse_status;
	JAGEDITOR->resetscreen();
	JAGEDITOR->mouse_status = m;
}
string getwinchar(void(*f)(), bool mouseenabled);
string jag_editor::getch() {
	initialisation();
	return getwinchar(resizewindow, mouse_status);
}

#else
void resizewindow(int theSignal) {
	JAGEDITOR->resetscreen();
}
#endif

//------------------------------------------------------------------------------------

jag_editor::jag_editor() : lines(this), jag_get(true) {

	insertaline = false;

    moveup = false;

    selected_x = -1;
    selected_firstline = -1;
    selected_y = -1;
    selected_pos = -1;
    selected_posnext = -1;
    double_click = 0;

    noprefix = false;
    previous_noprefix = false;
    tooglehelp = false;
    regularexpressionfind = false;
    rgx = NULL;
    filetype = no_type;

#ifdef POSIXREGEX
    posixrgx = NULL;
#endif

    prefixsize = 0;

    xcursor = 0;
    ycursor = 0;

#ifndef WIN32
    signal(SIGWINCH, resizewindow);
#endif
    colors.push_back(m_red); //0
    colors.push_back(m_ital); //1
    colors.push_back(m_blue); //2
    colors.push_back(m_gray); //3
    colors.push_back(m_green); //4
    colors.push_back(m_yellow); //5
    colors.push_back(m_selectgray); //6

    poscommand = 0;
    option = x_none;
    pos = 0;
    posinstring  = 0;
    currentline = 0;
    currentfindpos = 0;
    currentposinstring = -1;

    linematch = -1;

    prefix = editor_prefix;
    wprefix = editor_wprefix;

    replaceall = false;
    modified = true;
    tobesaved = false;
    screensizes();
    localhelp << m_red<< "^xh" << m_current << ":help " << m_red<< "^k" << m_current << ":del after " << m_red<< "^p" << m_current << ":k-buffer " <<  m_red<< "^d" << m_current << ":del line " << m_red<< "^uz/^r" << m_current << ":un/redo " << m_red<< "^f" << m_current << ":find " << m_red<< "^n" << m_current << ":next " << m_red<< "^g" << m_current << ":go " << m_red<< "^l" << m_current << ":load " << m_red<< "^t" << m_current << ":indent " << m_red<< "^s/w" << m_current << ":write " << m_red<< "^x" << m_current << ":commands ";

    updateline = true;
    taskel = true;

    JAGEDITOR = this;
}

jag_editor::~jag_editor() {
    if (rgx != NULL)
        delete rgx;
#ifdef POSIXREGEX
    if (posixrgx != NULL)
        delete posixrgx;
#endif

}

//------------------------------------------------------------------------------------



void jag_editor::setscrolling() {
    getcursor();
#ifndef WIN32
    ioctl(STDOUT_FILENO, TIOCGWINSZ, &wns);
    char buffer[20];
    sprintf_s(buffer,20, "\33[%d,%dr", xcursor, wns.ws_row);
    cout << buffer;
#endif
}

void jag_editor::resetscrolling() {
    char buffer[10];
    strcpy(buffer, "\33[r");
    cout << buffer;
}

void jag_editor::selectfound(long l, long r) {
    wstring ln = lines[pos];

    wstring lsub = ln.substr(0,l);
    lsub += L"<!@!<";

    if (r > ln.size())
        r = ln.size();
    else
        lsub += ln.substr(r, ln.size());

    string line = convert(lsub);

    lsub = ln.substr(l, r-l);
    string inter = colors[6];
    inter += convert(lsub);
    inter += m_current;

    l = line.find("<!@!<");
    if (l==-1) {
        movetoposition();
        return;
    }
    line.replace(l, 5, inter);
    if (noprefix)
        printline(lines.numeros[pos], line);
    else {
        if (lines.status[pos] == concat_line) {
            string space(prefixe(), ' ');
            cout << back << space << line;
        }
        else
            printline(lines.numeros[pos], line);
    }
    movetoposition();
}

void jag_editor::getcursor() {
    xcursor = getxcursor();
    ycursor = getycursor();
}

inline void move_right(long sc) {
    m_right[2] = localn999[sc][0];
    m_right[3] = localn999[sc][1];
    m_right[4] = localn999[sc][2];
    cout << m_right;
    m_right[2] = '0';
    m_right[3] = '0';
    m_right[4] = '1';
}

void jag_editor::movetoposition() {
    long sc;
    if (emode())
        sc = size_upto(lines[poslines[currentline]], posinstring) + prefixe();
    else
        sc = size_upto(line, posinstring) + prefixego();
    cout << back;
    if (!sc)
        return;

    move_right(sc);
}

void jag_editor::movetobeginning() {
    long sc = prefixego();
    cout << back;
    if (!sc)
        return;

    move_right(sc);
}

void jag_editor::movetoend(bool remove) {
    long sc;
    if (emode()) {
        long p = poslines[currentline];
        sc = fullsize(lines[p]) + prefixe();
        if (remove && lines.eol(p))
            sc--;
    }
    else
        sc = fullsize(line) + prefixego();

    if (sc <= 0)
        return;

    cout << back;

    move_right(sc);
}

void jag_editor::movetolastline() {
	moveto_row_column(row_size + 2, 1);
}

void jag_editor::movetoline(long e) {
    if (e >= poslines.size()) {
        currentline = poslines.size()-1;
        e =  currentline;
    }
    //first line is 0
	moveto_row_column(e + 1, 1);
}

void jag_editor::gotoline(long p) {
    bool fnd = false;
    currentline = 0;
    if (p >= poslines[0] && p <= poslines.back()) {
        for (long u = 0; u < poslines.size(); u++) {
            if (p == poslines[u]) {
                currentline = u;
                fnd = true;
            }
        }
    }

    if (!fnd)
        displaylist(p);
    else
        displaylist(poslines[0]);

    movetoline(currentline);
}

bool jag_editor::updown(char drt, long& pos) {
    long sz = lines.size();

    char exec = 0;
    if (drt == is_up) { // we are going up
        moveup = true;
        currentline--;
        if (currentline < 0) {
                //we need to scroll down...
            currentline = 0;
            if (pos > 0) {
                //the current position in lines should not be the top line...
                //we check one position up...
                --pos;
                resetlist(poslines[0]-1);
				scrollingdown(row_size);
				line = lines[pos];
				movetoline(0);
				displaygo(true);
				if (posinstring > linesize())
                    posinstring = linesize();
                return true;
            }
            return false;
        }
        else {
            pos = poslines[currentline];
            exec = 1;
        }
    }
    else {
        moveup = false;
        if ((currentline+1) >= poslines.size()) {
            if (pos < (sz -1)) {
				resetlist(poslines[0] + 1);
                pos = poslines.back();
				clearlastline();
				cout << m_scrollup;
				line = lines[pos];
				moveto_row_column(currentline+1, 1);
				displaygo(true);
                if (posinstring > linesize())
                    posinstring = linesize();
				return true;
            }
            return false;
        }
        pos = poslines[++currentline];
        exec = 2;
    }

    if (exec) {
        if (drt == is_up)
            cout << m_up;
        else
            cout << m_down;
        line = lines[pos];
        if (posinstring > line.size())
            posinstring = line.size();
        movetoposition();
        return true;
    }

    return false;
}

void jag_editor::clearscreen() {
#ifdef WIN32
	system("cls");
#else
    cout << m_clear << m_clear_scrolling << m_home;
#endif
}

void jag_editor::deletechar(bool left) {
    long sz = line.size();
    bool last = false;
    if (posinstring >= sz - 1) {
            //We need to know if we are in the middle of a large string across more than one line...
        if (!emode())
            last = true;
        else {
            if (currentline >= poslines.size() - 1 || poslines[currentline + 1] != pos)
                last = true;
        }
    }
    bool emoji = false;
    if (sizestring(line) != line.size())
        emoji = true;

    if (!emode() || (posinstring >= 0 && posinstring < sz)) {

        if (emode()) {
            line = lines[pos];
            undo(line, pos, u_modif);
        }
        else
            clearline();

        long pins = deleteachar(line, last, posinstring);

        if (option != x_none) {
            displaygo(false);
            return;
        }

        //We update our line
        if (emode()) {
			long dif = pins - posinstring;
			posinstring = pins;
            if (!dif)
                dif = 1;

            tobesaved = true;
			lines[pos] = line;

            if (lines.Status(pos)) {
                if (lines.refactoring(pos)) {
                    displaylist(poslines[0]);
                    movetoline(currentline);
                }
                else {
                    clearline();
                    long p = pos + 1;
                    if (lines.Status(pos) == concat_line)
                        printline(-1, lines[pos], -1);
                    else
                        printline(p, lines[pos], -1);
                    long cl = currentline + 1;
                    while (lines.Status(p) == concat_line) {
                        movetoline(cl++);
                        clearline();
                        printline(-1, lines[p], -1);
                        p++;
                    }
                    movetoline(currentline);
                }
            }
            else {
                clearline();
                printline(pos+1, line, -1);
            }
		}
        else {
			posinstring = pins;
            printline(pos+1, line, -1);
        }

        movetoposition();
    }
}

void jag_editor::deleteline(char movingup) {

    if (lines.size() == 1 && lines[0].size() == 0) {
        init();
        return;
    }

    tobesaved = true;
    if (!movingup) {
        //we delete the full line
        undo(lines[pos],pos, u_del); //deletion
        char update = lines.updatestatus(pos);

        kbuffer = lines[pos] + L"\n"; //we keep track of that line...
        if (pos >= lines.size())
            lines.pop_back();
        else {
            lines.erase(pos);
            lines.numbers();
        }

        if (update != -1) {
            lines.status[pos] = concat_line;
            undo(lines[pos],pos, u_modif_linked); //status modification
            lines.status[pos] = update;
        }

        displaylist(poslines[0]);
        if (pos < lines.size())
            line = lines[pos];
        else
            line = L"";
        movetoline(currentline);
        posinstring = 0;
        movetobeginning();
        return;
    }

        //left top, cannot do anything

        //We need to kill the current line, however it can be merged with the previous one if moveup is true...
        //We are at position currentline...
        //if the line above already belong to the same line, we kill the last character of that line above
    wstring code;
    if (movingup == -1) { // the destruction was done with backspace
        if (!pos && !posinstring && !currentline)
            return;

        char stat = lines.status[pos];

        code =  lines[pos]; //our current line...

            //We need to know if we are deleting a line or a subset of a line
        if (stat != concat_line) {
            if (stat == beg_line) {
                undo(lines[pos], pos, u_modif); //modification
                undo(lines[pos+1],pos+1, u_modif_linked); //we are modifying a multiple line
            }
            else
                undo(lines[pos],pos, u_del); //deletion

            lines.erase(pos); //here the line is deleted from our list of lines
            if (stat == beg_line)// it is a long line...
                lines.status[pos] = concat_line;

            if (currentline > 0)
                pos = poslines[--currentline];
            else
                resetlist(--pos);

            undo(lines[pos], pos, u_modif_linked); //modification
            if (stat == beg_line) {
                if (lines.Status(pos) == solo_line)
                    lines.status[pos] = beg_line;
            }

            posinstring = lines[pos].size();
        }
        else {
            undo(lines[pos], pos, u_modif); //modification
            undo(lines[pos+1],pos+1, u_modif_linked); //we are modifying a multiple line

            lines.erase(pos); //here the line is deleted from our list of lines
            if (currentline > 0)
                pos = poslines[--currentline];
            else
                resetlist(--pos);

            undo(lines[pos],pos, u_modif_linked); //we are modifying a multiple line

                //we delete the last character on the previous line since this is one single string across different lines.
            posinstring = lines[pos].size() - 1;
            deleteachar(lines[pos], true, posinstring);
        }


        lines[pos] += code;
        line = lines[pos];
        //we merge the current line and the above line...
        lines.refactoring(pos);
    }
    else { //the destruction was done with the delete key, we stay on the same line
        line = lines[pos];
        undo(line, pos, u_modif); //modification...
                                  //Three cases, the next line is actually part of the current line
        long p = pos+1;
        if (lines.Status(p) == concat_line) {
            undo(lines[p], p, u_modif_linked);
            code = lines[p];
            deleteachar(code, false, 0);
            lines[p] = code;
            lines.refactoring(p);
        }
        else {
            if (pos < lines.size() - 1) {
                code = lines[p];
                undo(code, p, u_del_linked);
                line += code;
                lines[pos] = line;
                lines.erase(p);
                lines.refactoring(pos);
            }
            else
                return;
        }
    }

    displaylist(poslines[0]);
    movetoline(currentline);
    movetoposition();
}

void jag_editor::clearlastline() {
	movetolastline();
	clearline();
}

void jag_editor::displayonlast(bool bck) {
    clearlastline();
    wstring w = st.str();
    string s = convert(w);
    cout << back << s;

    if (bck) {
        movetoline(currentline);
        if (currentposinstring != -1)
            posinstring = currentposinstring;
        currentposinstring = -1;
        if (posinstring > lines[poslines[currentline]].size()) {
            posinstring = lines[poslines[currentline]].size();
            movetoend();
        }
        else
            movetoposition();
    }
}

void jag_editor::displayonlast(wstring w, bool bck) {
	clearlastline();
    string s = convert(w);
    cout << back << s;

    if (bck) {
        movetoline(currentline);
        if (currentposinstring != -1)
            posinstring = currentposinstring;
        currentposinstring = -1;
        if (posinstring > lines[poslines[currentline]].size()) {
            posinstring = lines[poslines[currentline]].size();
            movetoend(false);
        }
        else
            movetoposition();
    }
}

void jag_editor::displayonlast(string s, bool bck) {
    clearlastline();
    cout << back << s;

    if (bck) {
        movetoline(currentline);
        if (currentposinstring != -1)
            posinstring = currentposinstring;
        currentposinstring = -1;
        if (posinstring > lines[poslines[currentline]].size()) {
            posinstring = lines[poslines[currentline]].size();
            movetoend();
        }
        else
            movetoposition();
    }
}

void jag_editor::displaygo(bool full) {
    wstring val;
    switch(option) {
        case x_goto:
			clearline();
            cout << back << "Line:" << convert(line);
            break;
        case x_rgx:
            clearline();
            cout << back << "Rgx:" << convert(line);
            break;
        case x_prgx:
            clearline();
            cout << back << "Prgx:" << convert(line);
            break;
        case x_find:
            clearline();
            cout << back << "Find:" << convert(line);
            break;
        case x_findnocase:
            clearline();
            cout << back << "find:" << convert(line);
            break;
        case x_replace:
            clearline();
            cout << back << "Find:" << convert(currentfind) << "  Replace:" << convert(line);
            break;
        case x_replacenocase:
            clearline();
            cout << back << "find:" << convert(currentfind) << "  replace:" << convert(line);
            break;
        case x_replacergx:
            clearline();
            cout << back << "Rgx:" << convert(currentfind) << "  Replace:" << convert(line);
            break;
        case x_replaceprgx:
            clearline();
            cout << back << "Prgx:" << convert(currentfind) << "  Replace:" << convert(line);
            break;
        case x_write:
			clearline();
            cout << back << "File:" << convert(line);
            break;
        case x_delete:
        case x_copy:
        case x_cut:
        case x_paste:
        case x_pasteselect:
        case x_deleting:
        case x_cutting:
        case x_load:
			clearline();
            val = st.str();
            cout << back << convert(val) << convert(line);
            break;
        case x_debug:
            clearline();
            cout << endl << m_gray << "$:end !:stop ↓:inside ↑:out →:go ←:breakpoint %:variables:" << m_current << convert(line);
            break;
        default:
            if (full) {
                if (lines.status[pos] == concat_line) {
                    string space(prefixe(), ' ');
                    cout << back << space << coloringline(line, pos);
                }
                else
                    printline(lines.numeros[pos], line, pos);
            }
    }
}

void jag_editor::Scrolldown() {
	//From currentline down, pos is the new line number...
    long ps = currentline + 1;
	m_scrollmargin[2] = localn999[ps][0];
	m_scrollmargin[3] = localn999[ps][1];
	m_scrollmargin[4] = localn999[ps][2];
	cout << m_scrollmargin << m_scrolldown;
	m_scrollmargin[2] = '0';
	m_scrollmargin[3] = '0';
	m_scrollmargin[4] = '0';
	cout << m_scrollmargin;
	movetoline(currentline - 1);
	ps = pos;
	//We simply change the line numbers
	long i;
	if (poslines.size() < row_size)
		poslines.push_back(poslines.size());

	for (i = currentline - 1; i <= row_size && i < poslines.size(); i++) {
		printline(ps);
		cout << m_down;
		ps++;
	}
	clearline();
	movetoline(currentline);
}

void jag_editor::displaylist(long beg) {
	if (!lines.size()) {
		clearline();
		if (!noprefix)
			cout << back << m_dore << prefix << m_current << m_lightgray << std::setw(prefixsize) << "1> " << endl;
		return;
	}

	if (beg < 0)
		beg = 0;

    long nb = 0;
    x_option g = option;
    option = x_none;

    modified = false;
    lines.detectlongstrings(filetype);
    if (!lines.updatesize()) {
        if (poslines.size() && beg == poslines[0] && currentline && insertaline) {
            Scrolldown();
            return;
        }
    }

	poslines.clear();

    stringstream blk;
    for (long i = beg; i < lines.size(); i++) {
        poslines.push_back(i);
        string space(prefixe(), ' ');
        if (noprefix)
            blk << coloringline(lines[i],i) << endl;
        else
        if (lines.status[i] == concat_line)
            blk << space << coloringline(lines[i], i) << endl;
        else
            blk << m_dore << prefixstring(lines.numeros[i]) << m_current << m_lightgray << std::setw(prefixsize) << lines.numeros[i] << "> " << m_current << coloringline(lines[i], i) << endl;
        nb++;
        if (nb > row_size) // we have displayed all lines
            break;
    }
    option = g;
    clearscreen();
    cout << blk.str();
}

void jag_editor::processgo() {
    long i;
    if (line == L"$") {
        pos = lines.size() - 1;
        option = x_none;
        displaylist(pos - row_size);
        line = lines[pos];
        posinstring = line.size();
        currentline = poslines.size() - 1;
        movetoline(currentline);
        movetoend();
        return;
    }

    i = convertinginteger(line);
    if (i >= 0 && i < lines.size()) {
        pos = lines.getlinenumber(i);
        option = x_none;
        displaylist(pos);
        currentline = 0;
        line = lines[poslines[currentline]];
        posinstring = 0;
        cout << m_home;
        movetobeginning();
        return;
    }

    clearline();
    cout << back << "Unknown line";
    movetoline(currentline);
    movetoend();
}

bool jag_editor::resetsearch() {
    return true;
}

bool jag_editor::search(wstring& l, long& first, long& last, long ps) {
    if (regularexpressionfind == 1) {
        if (rgx == NULL)
            rgx = new Au_automate(currentfind);
        return rgx->search(l, first, last, ps);
    }
#ifdef POSIXREGEX
    if (regularexpressionfind == 2) {
        if (posixrgx == NULL) {
            posixrgx = new wregex(currentfind);
        }

        first = ps;

        if (first >= l.size())
            return false;

        wsmatch posixresult;

        wstring val=l.substr(first,l.size()-first);
        if (regex_search(val, posixresult, *posixrgx)) {
            first += posixresult.position();
            last = first + posixresult.length();
            return true;
        }
        return false;
    }
#endif
    if (regularexpressionfind == 3) {
        wstring u = special_characters.s_to_lower(l);
        wstring c = special_characters.s_to_lower(currentfind);
        first = u.find(c, ps);
    }
    else
        first = l.find(currentfind, ps);

    if (first == -1)
        return false;
    last = first + currentfind.size();
    return true;
}

bool jag_editor::processfind() {
    long i;
        //Search part...
    long first = 0, last, end;
    long ps = currentposinstring;
    currentfindpos = 0;
    if (currentfind !=  L"") {
        wstring_controlled l;
        if (!resetsearch()) {
            displayonlast("Bad Expression", true);
            return false;
        }

        for (i = pos; i < lines.size(); i++) {
            l = lines.getoneline(i, end);
            if (search(l, first, last, ps)) {
                pos = i;
                if (first > col_size) {
                    ps = first/col_size;
                    pos += ps;
                    ps *= col_size;
                    first -= ps;
                    last -= ps;
                    resetlist(pos);
                }

                posinstring = first;
                currentfindpos = last;
                gotoline(pos);
                x_option g = option;
                option = x_none;
                selectfound(first, last);
                option = g;
                return true;
            }
            ps = 0;
            i = end;
        }
    }

    clearline();
    cout << back << "'"  << convert(currentfind) << "' Not found";
    movetoline(currentline);
    movetoend();
    return false;
}

void jag_editor::processreplace() {
#ifdef POSIXREGEX
    wstring wrep;
#endif

    string resp;
    option = x_none;
    movetoposition();
    if (!replaceall) {
        currentposinstring = posinstring;
        displayonlast(L"Replace: Y/N/A", true);
        resp = getch();
        if (resp == "A" || resp == "a")
            replaceall = true;
    }

    if (replaceall || resp == "Y" || resp == "y") {
        tobesaved = true;
        //We know where the word is...
        long first = 0, last, end;
        wstring ws = lines.getoneline(pos, end);
        if (search(ws, first, last, posinstring)) {
            if (regularexpressionfind != 2) {
                ws.replace(first, last-first, currentreplace);
            }
#ifdef POSIXREGEX
            else {
                wrep = ws.substr(first, last-first);
                wrep = regex_replace(wrep, *posixrgx, currentreplace);
                ws.replace(first, last-first, wrep);
            }
#endif
            lines.replaceline(pos, end+1, ws);

            if (first > col_size) {
                long ps = first/col_size;
                pos += ps;
                ps *= col_size;
                first -= ps;
                resetlist(pos);
                posinstring = first;
                gotoline(pos);
            }
            else {
                posinstring = first;
                gotoline(poslines[0]);
            }

            if (!findnext())
                replaceall = false;
        }
        return;
    }

    if (resp == "N" || resp == "n") {
        if (!findnext())
            replaceall = false;

    }
    else
        displayonlast("", true);
}

bool jag_editor::findnext() {
    if (currentfind != L"") {
        long i, ps = currentfindpos;
        long first = 0, last, end;
        wstring_controlled l;
        for (i = pos; i < lines.size(); i++) {
            l = lines.getoneline(i, end);
            if (search(l, first, last, ps)) {
                pos = i;
                if (first > col_size) {
                    ps = first/col_size;
                    pos += ps;
                    ps *= col_size;
                    first -= ps;
                    last -= ps;
                    posinstring = first;
                    resetlist(pos);
                }

                posinstring = first;
                currentfindpos = last;
                gotoline(pos);
                selectfound(first, last);
                if (currentreplace != L"")
                    processreplace();
                return true;
            }
            i = end;
            ps = 0;
        }

        if (!replaceall) {
            for (i = 0; i < lines.size(); i++) {
                l = lines.getoneline(i, end);
                if (search(l, first, last, 0)) {
                    pos = i;
                    if (first > col_size) {
                        ps = first/col_size;
                        pos += ps;
                        ps *= col_size;
                        first -= ps;
                        last -= ps;
                        resetlist(pos);
                    }

                    posinstring = first;
                    currentfindpos = last;
                    gotoline(pos);
                    selectfound(first, last);
                    if (currentreplace != L"")
                        processreplace();
                    return true;
                }
            }
        }
    }
    return false;
}


    //A CR was hit, we need either to modify the current line or to add a new one...
    //If the cursor was in the middle of a line, then we split it in two

long jag_editor::handlemultiline() {
        //We are in the middle of a very long line across multiple lines on screen
        //We need to cut it in half...

    line = lines[poslines[currentline]]; //our current line...
                                         //we need to cut it in half
    wstring sub;

    char stat = lines.Status(pos);

    //We keep track of our line before splitting...
    undo(lines[pos],pos, u_modif); //modification

    //We are in the middle of our substring... We extract some sub-elements...

    //We cut our line at the right position.
    //sub is the right part of the string
    long sz = line.size() - posinstring;
    //We cut our line at the right position.
    //sub is the right part of the string
    if (sz > 0)
        sub = line.substr(posinstring, sz);

    //line keeps the left part...
    line = line.substr(0, posinstring);

    lines[pos] = line;
    long sp = lines.indent(pos);
    if (sub == L")" || sub == L"}" || sub == L"]")
        sp -= GetBlankSize();
    if (sp > 0) {
        wstring space(sp, L' ');
        sub = space + sub;
    }
    else
        sp = 0;

    currentline++;
    pos++;
    lines.insert(pos, sub);

    undo(sub, pos, u_ins_linked); //insertion on following line

    if (stat == solo_line)
        //In this case, the solution is easy...
        lines.numbers();
    else {
        if (stat == beg_line)
            lines.status[pos-1] = solo_line;
        if (lines.Status(pos+1) == concat_line) {
            lines.status[pos] = beg_line;
            lines.refactoring(pos);
        }
        else {
            lines.status[pos] = solo_line;
            lines.numbers();
        }
    }

    if (currentline > row_size) {
        displaylist(poslines[0] + 1);
        currentline = row_size;
    }
	else {
		if (pos > 0 && stat == solo_line) {
			clearline();
			printline(pos, lines[pos - 1], pos - 1);
			insertaline = true;
			displaylist(poslines[0]);
			printline(pos + 1, lines[pos], pos);
			insertaline = false;
		}
		else
			displaylist(poslines[0]);
	}


    movetoline(currentline);
    posinstring = sp;
    movetoposition();
    line = sub;
    return pos;
}

void jag_editor::handleblock(wstring& bl) {

    if (bl == L"")
        return;

    tobesaved = true;
    line = lines[pos];
    if (posinstring > line.size())
        posinstring = line.size();

    //We keep track of the initial form of the line...
    undo(line, pos, u_modif); //The value is negative to indicate a deletion

    vector<wstring> vs;
    vsplit(bl, L"\n", vs);
    if (vs.size() == 1) {
        if (!posinstring && bl.back() == '\n') {
            undo(lines[pos],pos, u_ins_linked);
            lines.insert(pos, vs[0]);
            posinstring = 0;
        }
        else {
            line = line.substr(0, posinstring) + vs[0] + line.substr(posinstring, line.size());
            lines[pos] = line;
            posinstring += bl.size();
        }
    }
    else {
        lines[pos++] = line.substr(0, posinstring) + vs[0];
        vs.back() += line.substr(posinstring, line.size());
        for (long i = vs.size() -1; i >= 1; i--) {
            undo(lines[pos],pos, u_ins_linked);
            lines.insert(pos, vs[i]);
        }
        currentline += vs.size()-1;
        pos = poslines[currentline];
        posinstring = 0;
    }

    displaylist(poslines[0]);
    movetoline(currentline);
    movetoposition();
}

long jag_editor::handlingeditorline(bool computespace) {
    long sz = lines.size();

    tobesaved = true;

    //if we are in the middle of a line...
    if (pos < sz) {
        if ((posinstring > 0 && posinstring < line.size()) || lines.status[pos] == concat_line)
            return handlemultiline();
    }
    else
        pos = sz;

    //We keep track of our line before splitting...
    undo(lines[pos],pos, u_modif); //modification

    long sp = 0;
    char spa = ' ';
    //we record the number of space on the current line...
    //which we will use as a seed to display the following line
    sp = lines.indent(pos);
    if (sp < 0)
        sp = 0;

    wstring space(sp, spa);

        //we have different cases...
    if (!posinstring) {
            //beginning of the line, we insert before...
        int prf = prefixsize;
        lines.insert(pos, space); //we insert it into our line...
        undo(space, pos, u_ins_linked); //The value is inserted
        lines.numbers();
        //We need to insert a line at this position
        insertaline = true;
        prefixsize = prf;
        insertaline = false;
        sz = poslines.size();
        //if we are at the end of the screen, we need to scroll up
        if (sz > row_size && currentline == sz-1)
            displaylist(poslines[0] + 1);
        else {
            displaylist(poslines[0]);
            currentline++;
        }

        pos = poslines[currentline];
        line = lines[pos];
        posinstring = 0;
        movetoline(currentline);
        movetobeginning();
        return pos;
    }

    char action = u_ins;
    if (pos >= sz - 1) //we already are at the end...
        lines.push_back(space);
    else {
        lines.insert(pos+1, space); //we insert it into our line...
        lines.numbers();
    }

    pos++;
    undo(space, pos, action); //The value is negative to indicate a deletion

    if (currentline < row_size) {
        displaylist(poslines[0]);
        currentline++;
    }
    else //in this case, we are at the end of the process...
        displaylist(poslines[0] + 1);

    movetoline(currentline);
    if (action == u_ins && sp) {
        posinstring = space.size();
        movetoend(false);
    }
    else {
        posinstring = 0;
        movetobeginning();
    }
    line = space;
    return pos;
}


long jag_editor::getbuffsize() {
    return _getbuffsize;
}

void jag_editor::clear() {
    mouseoff();
    pos = lines.size();
    currentline = 0;
    movetolastline();
    printline(pos+1);
    fflush(stdout);
    line = L"";
    posinstring = 0;
    currentline = 0;
}

void jag_editor::processredos() {
    if (!emode() || redos.empty())
        return;

    line = redos.l_keeplines.back();
    pos = redos.l_keeppos.back();
    char a = redos.l_keepactions.back();
    currentline = redos.l_keepcurrentline.back();
    posinstring = redos.l_keepposinstring.back();
    char status = redos.l_keepstatus.back();

    long posfirst = redos.l_keeptop.back();

    modified = true;
    if (emode())
        tobesaved = true;

    bool refact = false;

    wstring_controlled l;

    switch (a) {
        case u_del_linked:
        case u_del:
            l = lines[pos];
            lines.erase(pos);
            refact = true;
            break;
        case u_modif: //modification
        case u_modif_linked:
            l = lines[pos];
            lines[pos] = line;
            lines.status[pos] = status;
            break;
        case u_ins:
        case u_ins_linked:
            lines.insert(pos, line);
            lines.status[pos] = status;
            refact = true;
            break;
    }

    undos.move(l, redos);

    if (!redos.empty()) {
            //the linked actions are in other way around...
        a = redos.l_keepactions.back();
        if (a == u_del_linked || a == u_modif_linked || a == u_ins_linked) {
            processredos();
            return;
        }
    }

    if (refact)
        lines.refactoring(pos);

    displaylist(posfirst);

    movetoline(currentline);
    movetoposition();
}

void jag_editor::processundos() {
    if (!emode() || undos.empty())
        return;

    line = undos.l_keeplines.back();
    pos = undos.l_keeppos.back();
    char a = undos.l_keepactions.back();
    currentline = undos.l_keepcurrentline.back();
    posinstring = undos.l_keepposinstring.back();
    char status = undos.l_keepstatus.back();

    long posfirst = undos.l_keeptop.back();

    modified = true;
    if (emode())
        tobesaved = true;
    bool refact = false;
    wstring_controlled l;

    switch (a) {
        case u_del_linked: //delete a line
        case u_del:
                //the line was removed from lines
            lines.insert(pos, line);
            lines.status[pos] = status;
            refact = true;
            break;
        case u_modif: //modification
        case u_modif_linked:
            l = lines[pos];
            lines[pos] = line;
            lines.status[pos] = status;
            break;
        case u_ins: //the line was inserted, we remove it...
        case u_ins_linked:
            l = lines[pos];
            lines.erase(pos);
            refact = true;
            break;
    }

    redos.move(l, undos);

        //if we have linked actions (two undos done for one action), we need to apply them in sequence...
    if (a == u_del_linked || a == u_modif_linked || a == u_ins_linked) {
        processundos();
        return;
    }

    if (refact)
        lines.refactoring(pos);

    displaylist(posfirst);

    movetoline(currentline);
    movetoposition();
}

bool jag_editor::evaluateescape(string& buff) {

#ifdef WIN32
	if (buff == c_homekey) {
        pos = 0;
        posinstring = 0;
        currentline = 0;
        line = lines[pos];
        displaylist(0);
        movetoline(0);
        movetobeginning();
		return true;
	}

	if (buff == c_endkey) {
		long ps = lines.size() - row_size;
		if (ps > 0) {
            pos = ps;
            currentline = 0;
            posinstring = 0;
            line = lines[pos];
            displaylist(pos);
            movetoline(0);
            movetobeginning();
			return true;
		}
		return true;
	}

    if (buff == homekey) {
        movetobeginning();
        posinstring = 0;
        return true;
    }

    if (buff == endkey) {
        posinstring = line.size();
		if (posinstring && !lines.eol(pos))
			posinstring--;
		movetoposition();
		return true;
    }
#else
    if (buff == homekey) {
        pos = 0;
        posinstring = 0;
        currentline = 0;
        line = lines[pos];
        displaylist(0);
        movetoline(0);
        movetobeginning();
        return true;
    }

    if (buff == endkey) {
        long ps = lines.size() - row_size;
        if (ps > 0) {
            pos = ps;
            currentline = 0;
            posinstring = 0;
            line = lines[pos];
            displaylist(pos);
            movetoline(0);
            movetobeginning();
            return true;
        }
        return true;
    }

#endif
        //ctrl-up, up 10 lines
    if (buff == c_up || buff == page_up) {
        if ((pos - row_size) < 0) {
            pos = 0;
            currentline = 0;
        }
        else
            pos -= row_size;
        posinstring = 0;
        line = lines[pos];
        displaylist(pos);
        movetoline(currentline);
        movetobeginning();
        return true;
    }

        //ctrl-down, down 10 lines
    if (buff == c_down  || buff == page_down) {
        long mxline = poslines.size() - 1;

        posinstring = 0;
        if ((pos + mxline) == lines.size()) {
            currentline = mxline;
            movetoline(currentline);
            movetobeginning();
            return true;
        }

        if ((pos + mxline) > lines.size()) {
            pos = lines.size()-mxline;
            if (pos < 0)
                pos = lines.size()-1;
        }
        else
            pos += mxline;

        line = lines[pos];
        displaylist(pos);
        movetoline(currentline);
        movetobeginning();
        return true;
    }

    if (buff == up || buff == down) {
#ifdef WIN32
		updown(buff[1], pos);
#else
		updown(buff[2], pos);
#endif
		return true;
    }

    long sz = line.size();
    if (buff == c_right || buff == a_right) {
        long ipos = posinstring;
        if (ipos> 0 && ipos < sz-1 && c_is_space(line[ipos+1]))
            ipos += 2;
        bool fnd = false;
        while (ipos < sz) {
            if (c_is_space(line[ipos])) {
                fnd = true;
                break;
            }
            ipos++;
        }

        if (!fnd) {
            posinstring = line.size();
            movetoend();
        }
        else {
            posinstring = ipos + 1;
            movetoposition();
        }

        return true;
    }

    if (buff == c_left || buff == a_left) {
        long ipos = posinstring;
        bool fnd = false;
        if (ipos > 1 && ipos <= sz && c_is_space(line[ipos-1]))
            ipos -= 2;
        while (ipos > 0) {
            if (c_is_space(line[ipos])) {
                fnd = true;
                break;
            }
            ipos--;
        }

        if (!fnd)
            posinstring = 0;
        else
            posinstring = ipos + 1;

        movetoposition();
        return true;
    }

    if (buff == right) {
        long mx = line.size();
        if (!lines.eol(pos))
            mx--;
        if (posinstring < mx) {
            forwardemoji();
            movetoposition();
        }
        else {//we go down at the beginning of the next line
            if (emode()) {
                updown(is_down, pos);
                posinstring = 0;
                movetobeginning();
            }
        }

        return true;
    }

    if (buff == (char*)alt_plus) {
        indentplus();
        return true;
    }

    if (buff == (char*)alt_minus) {
        deindentminus();
        return true;
    }

    if (buff == (char*)alt_v || buff == (char*)alt_vbis) {
        handleblock(copybuffer);
        return true;
    }

    if (buff == (char*)alt_x || buff == (char*)alt_xbis) {
        if (selected_pos == pos) {
            copybuffer = kbuffer;
            copy_to_clipboard(convert(copybuffer));
            kbuffer = L"";
            deleteselection();
            line = lines[pos];
        }
        return true;
    }

    if (buff == (char*)alt_c || buff == (char*)alt_cbis) {
        if (selected_pos == pos) {
            copybuffer = kbuffer;
            copy_to_clipboard(convert(copybuffer));
            kbuffer = L"";
            currentline += selected_posnext-selected_pos;
            pos = selected_posnext;
            movetoline(currentline);
            line = lines[pos];
            posinstring = selected_y;
            movetoposition();
        }
        return true;
    }

    if (buff == left) {
        if (posinstring > 0) {
            backwardemoji();
            movetoposition();
        }
        else {//we go up at the end of the previous line
            if (emode() && pos > 0) {
                updown(is_up, pos);
                posinstring = line.size();
                if (posinstring && !lines.eol(pos))
                    posinstring--;
                movetoposition();
            }
        }
        return true;
    }

    if (buff == del) {
        if (selected_pos != -1) {
            deleteselection();
            return true;
        }

        if (emode() && pos < lines.size()) {
            if (posinstring >= lines[pos].size()) {
                deleteline(1);
                return true;
            }
            deletechar(false);
        }
        else {
            if (posinstring < line.size())
                deletechar(false);
        }

        return true;
    }
    return false;
}

void jag_editor::init() {
    lines.clear();
    lines.push(L"");
    poslines.clear();
    poslines.push_back(0);

    displaylist(0);
    pos = 0;
    posinstring = 0;
    kbuffer = L"";
    currentline = 0;
    clearscreen();
    printline(1);
}


void jag_editor::indentplus() {
    wstring blanks(GetBlankSize(), ' ');
    tobesaved = true;
    if (selected_pos != -1 && selected_posnext > selected_pos) {
        uchar modif = u_modif;
        for (long i = selected_posnext - 1; i >= selected_pos; i--) {
            line = lines[i];
            line = blanks + line;
            undo(lines[i],i, modif);
            modif = u_modif_linked;
            lines[i] = line;
        }
		selectlines(selected_pos, selected_posnext, selected_x, selected_y);
		return;
    }
    line = lines[pos];
    line = blanks + line;
    undo(lines[pos],pos, u_modif);
    lines[pos] = line;
    printline(pos, line, -1);
    movetoline(currentline);
    movetoposition();
}

void jag_editor::deindentminus() {
    //We remove some blanks from lines...
    long nb = GetBlankSize();
    long u = 0;
    if (selected_pos != -1 && selected_posnext > selected_pos) {
        uchar modif = u_modif;
        for (long i = selected_posnext - 1; i >= selected_pos; i--) {
            line = lines[i];
            //can we remove nb blanks from the beginning of the line
            u = 0;
            for (; u < line.size() && line[u] == ' '; u++) {}
            if (u >= nb) {
                tobesaved = true;
                line = line.substr(nb, line.size());
                undo(lines[i],i, modif);
                modif = u_modif_linked;
                lines[i] = line;
            }
        }
        selectlines(selected_pos, selected_posnext, selected_x, selected_y);
        return;
    }

    line = lines[pos];
    for (; u < line.size() && line[u] == ' '; u++) {}
    if (u >= nb) {
        tobesaved = true;
        long sz = line.size();
        line = line.substr(nb, line.size());
        undo(lines[pos],pos, u_modif);
        lines[pos] = line;
        wstring blanks(sz, ' ');
        printline(pos, blanks, -1);
        printline(pos, line, -1);
        movetoline(currentline);
        movetoposition();
    }
}


bool jag_editor::checkcommand(char cmd) {
    switch (cmd) {
        case 'b': //black mode
            switch_darkmode();
            displaylist(poslines[0]);
            movetoline(currentline);
            movetoposition();
            return true;
        case '-': {
            deindentminus();
			displayonlast("", true);
            return true;
        }
        case '+': {//we indent all the lines up
            indentplus();
			displayonlast("", true);
            return true;
        }
        case 'l': //reload a file
            reloadfile();
            return true;
        case 'u':
            if (emode())
                toggletopbottom();
            displayonlast("", true);
            return true;
        case 'm':
            if (emode()) {
                togglemouse();
                clearst();
                st << "mouse:";
                if (mouse_status)
                    st << "on";
                else
                    st << "off";
                displayonlast(true);
            }
            return true;
        case 'f':
            if (emode()) {
                if (rgx != NULL)
                    delete rgx;
                rgx = NULL;
                regularexpressionfind = true;
                string sub = "Rgx:";
                sub += convert(currentfind);
                displayonlast(sub, false);
                line = currentfind;
                posinstring = currentfind.size();
                currentreplace = L"";
                option = x_rgx;
            }
            return true;
        case 'F':
            if (emode()) {
#ifdef POSIXREGEX
                if (posixrgx != NULL)
                    delete posixrgx;
                posixrgx = NULL;
#endif
                regularexpressionfind = 2;
                string sub = "Prgx:";
                sub += convert(currentfind);
                displayonlast(sub, false);
                line = currentfind;
                posinstring = currentfind.size();
                currentreplace = L"";
                option = x_prgx;
            }
            return true;
        case 'c': //copy
            clearst();
            st << "copy from:";
            displayonlast(false);
            line = L"";
            option = x_copy;
            return true;
        case 'x': //cut
            clearst();
            st << "cut from:";
            displayonlast(false);
            line = L"";
            option = x_cut;
            return true;
        case 'v': //paste
            handleblock(copybuffer);
            return true;
        case 'D': //delete a bloc of lines...
            clearst();
            st << "delete from:";
            displayonlast(false);
            line = L"";
            option = x_delete;
            return true;
        case 'q':
            return !terminate();
        case 'w': //write and quit
            if (thecurrentfilename == "")
                return true;
            if (writetofile()) {
                currentposinstring = -1;
                terminate();
            }
            else
                displayonlast("Write error", true);
            return true;
        case 'n':
            setnoprefix();
            displaylist(poslines[0]);
            movetoline(currentline);
            movetoposition();
            return true;
        case 'h': {
            bool m = mouse_status;
            if (mouse_status)
                mouseoff();
            displaythehelp();
            getch();
            displaylist(poslines[0]);
            if (m)
                mouseon();
            movetoline(currentline);
            movetoposition();
            return true;
        }
    }
    return false;
}

void jag_editor::ls(string cmd, string path, vector<wstring>& paths) {
    FILE *fp;

    char chemin[PATH_MAX];

    cmd += path;

#ifdef WIN32
    fp = _popen(STR(cmd), "r");
#else
    fp = popen(STR(cmd), "r");
#endif
    if (fp == NULL)
        return;

    wstring l;
    while (fgets(chemin, PATH_MAX, fp) != NULL) {
        cmd = chemin;
        cmd = s_trim(cmd);
        l = wconvert(cmd);
        paths.push_back(l);
    }

#ifdef WIN32
    _pclose(fp);
#else
    pclose(fp);
#endif
}

#ifdef WIN32
    bool jag_editor::checkpath(bool checkcmd) {
        //The first part should be a command such as open or load...
        long pos = line.rfind(' ');
        wstring root;
        wstring name;
        wstring path;
        if (checkcmd) {
            if (pos == -1) {
                checkcmd = false;
                path = line;
            }
            else {
                root = line.substr(0, pos);
                path = line.substr(pos, line.size());
            }
        }
        else
            path = line;

        path = s_trim(path);
        //Two cases, we have a "\\" in it...
        pos = path.rfind(L"\\");
        //We need to extract it
        if (pos != -1) {
            name = path.substr(pos+1, path.size()-pos);
            path = path.substr(0, pos+1);
        }
        else {
            name = path;
            path = L".";
        }
        vector<wstring> paths;
        vector<wstring> targets;
        //First the directories
        string cmd = "dir /B ";
        ls(cmd, convert(path), paths);
        //Now we look for continuation
        long i;
        for (i = 0; i < paths.size(); i++) {
            if (paths[i].substr(0, name.size()) == name)
                targets.push_back(paths[i]);
        }
        if (path == L".")
            path = L"";

        if (targets.size() == 0)
            return false;

        paths.clear();
        //Only directories, we want to add a _sep at the end...
        cmd = "dir /AD /B ";
        ls(cmd, convert(path), paths);
        for (i = 0; i < paths.size(); i++) {
            for (long j = 0; j < targets.size(); j++) {
                if (targets[j] == paths[i])
                    targets[j] += L"\\";
            }
        }

        if (targets.size() == 1) {
            if (checkcmd) {
                line = root;
                line += L" ";
                line += path;
            }
            else
                line = path;
            line += targets[0];
            clearline();
            displaygo(true);
            posinstring = line.size();
            movetoposition();
            return true;
        }

        wstring common;
        long ln  = name.size();
        bool end = false;
        while (!end) {
            //We add one letter from the targets and see if it is common to all targets
            for (i = 0; i < targets.size(); i++) {
                if (ln >= targets[i].size()) {
                    end = true;
                    break;
                }
            }
            if (!end) {
                ++ln;
                common = targets[0].substr(0, ln);
                for (i = 1; i < targets.size(); i++) {
                    if (targets[i].substr(0, ln) != common) {
                        end = true;
                        break;
                    }
                }
                if (!end)
                    name = common;
            }
        }


        cerr << endl << endl << m_red;
        for (i = 0; i < targets.size(); i++)
            cerr << convert(targets[i]) << " ";
        cerr << m_current << endl << endl;

        if (checkcmd) {
            line = root;
            line += L" ";
            line += path;
        }
        else
            line = path;
        line += name;
        clearline();
        displaygo(true);
        posinstring = line.size();
        movetoposition();
        return true;
    }
#else
    bool jag_editor::checkpath(bool checkcmd) {
        //The first part should be a command such as open or load...
        long pos = line.rfind(' ');
        wstring root;
        wstring name;
        wstring path;
        if (checkcmd) {
            if (pos == -1) {
                checkcmd = false;
                path = line;
            }
            else {
                root = line.substr(0, pos);
                path = line.substr(pos, line.size());
            }
        }
        else
            path = line;

        path = s_trim(path);
        //Two cases, we have a "/" in it...
        pos = path.rfind(L"/");
        if (pos != -1) {
            name = path.substr(pos+1, path.size()-pos);
            path = path.substr(0, pos+1);
        }
        else {
            name = path;
            path = L".";
        }
        vector<wstring> paths;
        vector<wstring> targets;
        string cmd = "ls -1 -p ";
        ls(cmd, convert(path), paths);
        //Now we look for continuation
        long i;
        for (i = 0; i < paths.size(); i++) {
            if (paths[i].substr(0, name.size()) == name)
                targets.push_back(paths[i]);
        }
        if (path == L".")
            path = L"";

        if (targets.size() == 0)
            return true;

        if (targets.size() == 1) {
            if (checkcmd) {
                line = root;
                line += L" ";
                line += path;
            }
            else
                line = path;
            line += targets[0];
            clearline();
            displaygo(true);
            posinstring = line.size();
            movetoposition();
            return true;
        }

        wstring common;
        long ln  = name.size();
        bool end = false;
        while (!end) {
            //We add one letter from the targets and see if it is common to all targets
            for (i = 0; i < targets.size(); i++) {
                if (ln >= targets[i].size()) {
                    end = true;
                    break;
                }
            }
            if (!end) {
                ++ln;
                common = targets[0].substr(0, ln);
                for (i = 1; i < targets.size(); i++) {
                    if (targets[i].substr(0, ln) != common) {
                        end = true;
                        break;
                    }
                }
                if (!end)
                    name = common;
            }
        }


        cerr << endl << endl << m_redital;
        for (i = 0; i < targets.size(); i++)
            cerr << convert(targets[i]) << " ";
        cerr << m_current << endl << endl;
        if (checkcmd) {
            line = root;
            line += L" ";
            line += path;
        }
        else
            line = path;
        line += name;
        clearline();
        displaygo(true);
        posinstring = line.size();
        movetoposition();
        return true;
    }
#endif

//This section handles combined commands introduced with Ctrl-x
void jag_editor::handlecommands() {
    currentposinstring = posinstring;
    string buff = getch();

    if (checkcommand(buff[0]))
        return;

    displayonlast("", true);
}

bool jag_editor::terminate() {
    replaceall = false;

    if (tobesaved) {
        tobesaved = false;
        displayonlast("File not saved... ctrl-c again to quit", true);
        std::cout.flush();
        return false;
    }

    movetolastline();
    string space(colsize(), ' ');
    cout << back << space << back << m_redbold << "Salut!!!" << m_current << endl;
    fflush(stdout);
	resetterminal();
    exit(0);
    return true;
}

bool jag_editor::checkaction(string& buff, long& first, long& last, bool lisp) {
    wstring code;
    long i;
    switch (buff[0]) {
        case 1: //ctrl-a, moving to beginning of line
            movetobeginning();
            posinstring = 0;
            return true;
#ifdef WIN32
		case 3:
			return !terminate();
#endif
        case 4: //ctrl-d exiting
            if (emode())
                deleteline(0);
            return true;
        case 9:
            if (option == x_find) {
                option = x_findnocase;
                regularexpressionfind = 3;
                string sub = "find:";
                displayonlast(sub, false);
                return true;
            }
            if (option == x_findnocase) {
                option = x_find;
                regularexpressionfind = 0;
                string sub = "Find:";
                displayonlast(sub, false);
                return true;
            }
            if (emode())
                return false;
            checkpath(false);
            return true;
        case 5://ctrl-e, moving to the end of the line...
            posinstring = line.size();
            if (posinstring && !lines.eol(pos))
                posinstring--;
            movetoposition();
            return true;
        case 6: // ctrl-f find
            if (emode()) {
                currentposinstring = posinstring;
                regularexpressionfind = false;
                string sub = "Find:";
                currentfind = L"";
                displayonlast(sub, false);
                line = currentfind;
                posinstring = currentfind.size();
                currentreplace = L"";
                option = x_find;
            }
            return true;
        case 7: //ctrl-g go in edit mode
            if (emode()) {
                displayonlast("Line:", false);
                line = L"";
                option = x_goto;
            }
            return true;
#ifndef WIN32
		case 8: //ctrl-h display help
			if (emode()) {
                option = x_none;
                if (!tooglehelp)
                    displayonlast(localhelp.str(), true);
                else {
                    displaylist(poslines[0]);
                    movetoline(currentline);
                    movetoposition();
                }
                tooglehelp = 1 - tooglehelp;
            }
            return true;
		case 10: //this is a carriage return
#else
		case 13:
#endif
                 //we need to check the different options...
            switch (option) {
                case x_none:
                    break;
                case x_goto: //go to
                    processgo();
                    option = x_none;
                    return true;
                case x_find: //find
                case x_findnocase:
                case x_rgx:
                case x_prgx:
                    currentfind = line;
                    processfind();
                    line = lines[poslines[currentline]];
                    option = x_none;
                    return true;
                case x_replace: //replace
                case x_replacenocase:
                case x_replacergx:
                case x_replaceprgx:
                    if (processfind()) {
                        currentreplace = line;
                        replaceall = false;
                        processreplace();
                        line = lines[poslines[currentline]];
                    }
                    option = x_none;
                    return true;
                case x_write: //write to a file
                    code = line;
                    thecurrentfilename = convert(code);
                    if (writetofile())
                        code = L"written to: ";
                    else
                        code = L"cannot write to: ";
                    code += wconvert(thecurrentfilename);
                    option = x_none;
                    displayonlast(code, true);
                    line = lines[poslines[currentline]];
                    return true;
                case x_delete: //delete a bloc of line (6)
                    option = x_deleting;
                    first = convertinginteger(line) - 1;
                    st << line << "  to:";
                    line = L"";
                    displayonlast(false);
                    return true;
                case x_copy: //copy (8)
                    first = convertinginteger(line) - 1;
                    st << line << "  to:";
                    line = L"";
                    displayonlast(false);
                    option = x_paste;
                    return true;
                case x_cut: //cut (9)
                    first = convertinginteger(line) - 1;
                    st << line << "  to:";
                    line = L"";
                    displayonlast(false);
                    option = x_cutting;
                    return true;
                case x_paste: //the pasting
                    copybuffer = L"";
                    if (line == L"$")
                        last = lines.size();
                    else
                        last = convertinginteger(line);
                    line = L"";
                    if (first >= 0 && first < last && last < lines.size())
                        copybuffer = lines.code(first, last);

                    st << line << " copied";
                    line = lines[poslines[currentline]];
                    option = x_none;
                    displayonlast(true);
                    return true;
                case x_pasteselect:
                    copybuffer = kbuffer;
                    line = lines[poslines[currentline]];
                    option = x_none;
                    displayonlast(true);
                    return true;
                case x_deleting: //delete
                case x_cutting: //cut
                    if (line == L"$") {
                        if (first >= 0 && first < lines.size()) {
                            last = lines.size() - 1;
                            if (option == x_cutting) {
                                copybuffer = lines.code(first, last+1);
                                st << line << " cut";
                                displayonlast(true);
                            }
                            undo(lines[last], last, u_del);
                            for (i = last-1; i > first; i--)
                                undo(lines[i], i, u_del_linked);
                            undo(lines[first], first, u_del_linked);
                            lines.erase(first, -1);
                        }
                        else
                            break;
                    }
                    else {
                        last = convertinginteger(line);
                        if (first >= 0 && first < last && last < lines.size()) {
                            if (option == x_cutting) {
                                copybuffer = lines.code(first, last);
                                st << line << " cut";
                                displayonlast(true);
                            }
                            undo(lines[last-1], last-1, u_del);
                            for (i = last-2; i > first; i--)
                                undo(lines[i], i, u_del_linked);
                            undo(lines[first], first, u_del_linked);
                            lines.erase(first, last);
                        }
                        else
                            break;
                    }

                    line = L"";
                    posinstring = 0;
                    if (lines.size()) {
                        option = x_none;
                        displaylist(poslines[0]);
                        currentline = poslines.size()-1;
                        pos = poslines[currentline];
                        line = lines[poslines[currentline]];
                        displayonlast("", true);
                        return true;
                    }
                    break;
                case x_load:
                    loadfile(line);
                    if (!emode()) {
                        option = x_none;
                        displayonlast("", true);
                    }
                    else {
                        noprefix = previous_noprefix;
                        displaylist(0, row_size);
                        movetoline(currentline);
                        movetobeginning();
                    }
                    return true;
                default:
                    break;
            }

            if (!emode()) {
                option = x_none;
                displayonlast("", true);
                return true;
            }

            pos = handlingeditorline();
            return true;
        case 11: //ctrl-k: delete trailing characters
            if (emode()) {
                deleteallafter();
                return true;
            }

            clearline();
            kbuffer = line.substr(posinstring, line.size());
            line = line.substr(0, posinstring);
            displaygo(true);
            return true;
        case 12: //ctrl-l: load a file
            if (emode()) {
                clearst();
                st << "load:";
                displayonlast(false);
                line = currentfind;
                currentreplace = L"";
                posinstring = currentfind.size();
                option = x_load;
            }
            return true;
        case 14:
            if (emode()) { //in edit mode looking for the next find
                findnext();
                line = lines[poslines[currentline]];
            }
            return true;
        case 16: //ctrl-p insert kbuffer
            modified = true;
            if (emode())
                tobesaved = true;
            handleblock(kbuffer);
            return true;
        case 17: //ctrl-q terminate
            return !terminate();
        case 18: //ctrl-r: redo...
            if (option == x_find || option == x_findnocase ||option == x_rgx || option == x_prgx) { //replace mode called after a ctrl-f
                currentfind = line;
                line = L"";
                if (option == x_findnocase)
                    cout << "  replace:";
                else
                    cout << "  Replace:";
                option = (x_option)((int)option + 1);
                return true;
            }
            processredos();
            return true;
        case 20: //ctrl-t
            if (emode())
                indentcode(pos);
            return true;
        case 21: //undo ctrl-u
        case 26: //ctrl-z
            if (emode()) {
                processundos();
            }
            return true;
        case 19: //ctrl-s
            if (thecurrentfilename != "") {
                if (writetofile())
                    code = L"written to: ";
                else
                    code = L"cannot write to: ";
                code += wconvert(thecurrentfilename);
                displayonlast(code, true);
                return true;
            }
        case 23: //ctrl-w write the content in a file
            if (emode()) {
                line = wconvert(thecurrentfilename);
                code = L"File:";
                if (line.size())
                    code += line;

                displayonlast(code, false);
                posinstring = line.size();
                option = x_write;
            }
            return true;
        case 24: //Insert commands
            displayonlast("Ctrl-X", true);
            handlecommands();
            if (option == x_exitprint)
                return false;
            return true;
        case 27: //Escape...
                 //we clear the current line if it is the only character...
            if (buff.size() == 1) {
                if (!emode() || tooglehelp) {
                    displayonlast("", true);
                    tooglehelp = false;
                    option = x_none;
                    return true;
                }
                return true;
            }

            return evaluateescape(buff);
#ifdef WIN32
		case 8:
#else
#ifdef __apple_build_version__
		case 15:
#endif
		case 127: //back delete
#endif
            if (selected_pos != -1) {
                deleteselection();
                return true;
            }

			if (posinstring > 0) {
                backwardemoji();
                deletechar(true);
            }
            else {
                if (emode())
                    deleteline(-1);
            }
            return true;
        default:
            if (buff.size() > 1 && emode())
                return evaluateescape(buff);
    }

    return false;
}
static bool isitempty(wstring& w, wchar_t c) {
    char nb = false;
    for (long i = 0; i < w.size(); i++) {
        if (w[i] > 32) {
            if (w[i] != c || nb)
                return false;
            nb = true;
        }
    }
    return true;
}

void jag_editor::cleanheaders(wstring& w) {
    static wstring pattern(L">>%s*%d+> ");

    if (w.find(wprefix) != -1) {
            //we might try to copy stuff from the editor itself...
            //We will remove anything which is: "작%d+> " from our string
        Au_automate rgx(pattern);
        vecte_a<long> values;
        rgx.searchall(w,values);
        for (long i=values.size()-2; i>=0; i-=2) {
            w.erase(values[i], values[i+1]-values[i]);
        }
    }
}

void jag_editor::addabuffer(wstring& b, bool instring) {
    if (isMouseAction(b) || b == L"")
        return;

    //We only keep displayable characters
    wchar_t c;
    wstring code;
    for (long i = 0; i < b.size(); i++) {
        c = b[i];
        //We replace these characters with a blank
        if (c < 32 && c != 9 && c != 10 && c != 13)
            continue;
        code += c;
    }
    if (code == L"")
        return;
    
    b = code;

    if (emode())
        tobesaved = true;
    modified = true;
    //We are in the middle of a line... Insertion mode...
    if (line.size() && posinstring < line.size()) {
        if (emode())
            line = lines[poslines[currentline]];

            //We insert the character within our current line...
        code = line.substr(0, posinstring);
        code += b;
        code += line.substr(posinstring,line.size());

        if (emode()) {
            pos = poslines[currentline];
            lines[pos] = code;
            lines.refactoring(pos);

            line = lines[pos];
            if (fullsize(code) > col_size) {
                if (currentline == row_size)
                    displaylist(poslines[0] + 1);
                else
                    displaylist(poslines[0]);

                if (posinstring >= col_size) {
                    currentline++;
                    posinstring = 0;
                    pos++;
                }

                posinstring += b.size();
                movetoline(currentline);
                movetoposition();
                return;
            }
        }

        clearline();
        line = code;
        displaygo(true);
        if (b[0] == ')' || b[0] == '}' || b[0] == ']') {
            string ln = convert(line);
            long posmatch = computeparenthesis(ln, b[0], posinstring);
            if (posmatch != -1) {
                linematch = pos;
                lines[pos] = line;
                string res = ln.substr(0, posmatch);
                res += m_redbold;
                res += ln[posmatch];
                res += m_current;
                res += ln.substr(posmatch+1,ln.size());
                printline(pos+1, res);
            }
        }
        posinstring += b.size();
        movetoposition();
        return;
    }


        //We extend our line...
    line += b;

    if (b[0]=='"' || b[0]=='\'')
        instring = 1 - instring;

    bool fndchr =  false;
    if (b[0] == ')' || b[0] == '}' || b[0] == ']') {
        fndchr = true;
        if (!instring && emode() && isitempty(line, b[0])) {
            long sp = lines.indent(pos) - GetBlankSize();
            if (sp > 0) {
                wstring space(sp, ' ');
                line = space;
                posinstring = sp;
                line += b;
            }
            else {
                line = b;
                posinstring = 0;
            }
            printline(pos, line, -1);
        }
        else {
            string ln = convert(line);
            long posmatch = computeparenthesis(ln, b[0], ln.size()-1);

            if (posmatch != -1) {
                if (emode()) {
                    linematch = pos;
                    lines[pos] = line;
                }
                else
                    linematch = -2;
                string res = ln.substr(0, posmatch);
                res += m_redbold;
                res += ln[posmatch];
                res += m_current;
                res += ln.substr(posmatch+1,ln.size());
                wstring lsave = line;
                line = L"";
                s_utf8_to_unicode(line, res, res.size());
                displaygo(true);
                line = lsave;
                posinstring += b.size();
                movetoposition();
                return;
            }
        }
    }

    posinstring += b.size();

    if (emode()) {
        lines[pos] = line;
        lines.refactoring(pos);

            //our line is now too long... we need to split it...
        if (fullsize(line) > col_size) {
            currentline++;

            if (currentline >= row_size) {
                displaylist(poslines[0] + 1);
                currentline = row_size;
            }
            else
                displaylist(poslines[0]);

            pos++;
            posinstring = 1;
            line = lines[pos];
            movetoline(currentline);
            movetoposition();
            return;
        }
    }


    if (fndchr) {
        clearline();
        displaygo(true);
        movetoposition();
    }
    else {
        if (option == x_none) {
            printline(pos+1, line, -1);
            movetoposition();
        }
        else
            cout << convert(b);
    }
}

static bool checkcar(wchar_t c) {
    if (c <= 32)
        return false;

    switch (c) {
        case '(':
        case ')':
        case ']':
        case '{':
        case '}':
        case '"':
        case '`':
        case ':':
        case ',':
        case ';':
            return false;
        default:
            return true;
    }
}

void jag_editor::deleteselection() {
    if (selected_pos == -1)
        return;

    if (selected_pos == selected_posnext) {
        if (selected_x == 0 && selected_y == line.size()) {
            deleteline(0);
			resetselection();
			return;
        }

        undo(lines[pos],pos, u_modif);
        deleteachar(line, false, selected_x);
        lines[selected_pos] = line;
        clearline();
        printline(pos+1, line, -1);
        posinstring = selected_x;
        movetoposition();
		resetselection();
        return;
    }

    long first = selected_pos;
    long last = selected_posnext;
    wstring savedline = lines[selected_posnext];

    if (selected_x == 0) {
        //first line must be deleted, last line is modified or deleted
        if (selected_y < savedline.size()) {
            undo(savedline, selected_posnext, u_modif);
            last--;
        }
        else {
            undo(savedline, selected_posnext, u_del);
            last--;
        }
    }
    else {
        undo(savedline, selected_posnext, u_del);
        last--;
    }

    while (last > first) {
        undo(lines[last], last, u_del_linked);
        last--;
    }

    if (selected_x == 0)
        undo(lines[selected_pos], selected_pos, u_del_linked);
    else
        undo(lines[selected_pos], selected_pos, u_modif_linked);

    last = selected_posnext;

    posinstring  = 0;

    if (selected_y >= savedline.size())
        selected_y = savedline.size();


    if (selected_x == 0) {
        if (selected_y != savedline.size()) {
            lines[selected_posnext] = savedline.substr(selected_y,savedline.size());
            posinstring = selected_y;
            last--;
        }
    }
    else {
        lines[selected_pos] = lines[selected_pos].substr(0, selected_x);
        if (selected_y != savedline.size()) {
            lines[selected_pos] += savedline.substr(selected_y,savedline.size());
        }
        posinstring = lines[selected_pos].size();
        first++;
    }

    lines.erase(first, last+1);

    displaylist(poslines[0]);
    movetoline(selected_firstline);
    movetoposition();
	resetselection();
    double_click = 0;
}


void jag_editor::displayextract(wstring& sub, long pos, long from_pos, long to_pos, bool select) {
    if (from_pos > sub.size())
        return;

    if (to_pos > sub.size())
        to_pos = sub.size();

    posinstring = to_pos;
    clearline();
    if (select) {
        wstring start = sub.substr(0, from_pos);
        wstring middle = sub.substr(from_pos, to_pos-from_pos);
        wstring end = sub.substr(to_pos, sub.size());
        kbuffer += middle;
        string inter = convert(start);
        inter += colors[6];
        inter += convert(middle);
        inter +=  m_current;
        inter += convert(end);
        printline(pos + 1, inter);
    }
    else
        printline(pos + 1, sub, -1);
}

void jag_editor::selectlines(long from_line, long to_line, long from_pos, long to_pos) {
    wstring sub;

    if (to_line < from_line)
        return;

    //If we are moving backward, we need to invert the values to correctly select our span of text
    //from left to right
    if (to_pos < from_pos) {
        long val = to_pos;
        to_pos = from_pos;
        from_pos = val;
    }
    
    kbuffer = L"";
    sub = lines[from_line];
    char stat = lines.Status(from_line);
    if (from_line == to_line) {
        movetoline(currentline);
        displayextract(sub, from_line, from_pos, to_pos);
        if (stat == solo_line || lines.Status(from_line+1)!= concat_line) {
            if (to_pos == sub.size())
                kbuffer += L"\n";
        }
        movetoposition();
        return;
    }

    long current = selected_firstline;
    //Else we display first, the missing part
    movetoline(current++);
    displayextract(sub, from_line++, from_pos, sub.size());
    if (stat == solo_line || lines.Status(from_line)!= concat_line)
        kbuffer += L"\n";

    while (from_line < to_line) {
        sub = lines[from_line];
        movetoline(current++);
        stat = lines.Status(from_line);
        displayextract(sub, from_line++, 0, sub.size());
        if (stat == solo_line || lines.Status(from_line) != concat_line) {
            kbuffer += L"\n";
        }
    }

    stat = lines.Status(from_line);
    movetoline(current);
    sub = lines[from_line];
    displayextract(sub, from_line, 0, to_pos);
    if (stat == solo_line || lines.Status(from_line+1) != concat_line) {
        if (to_pos == sub.size())
            kbuffer += L"\n";
    }
    movetoposition();
}

void jag_editor::unselectlines(long from_line, long to_line, long from_pos, long to_pos) {
    wstring sub;

    if (to_line < from_line)
        return;

    sub = lines[from_line];
    if (from_line == to_line) {
        movetoline(currentline);
        displayextract(sub, from_line, from_pos, to_pos, false);
        return;
    }

    long current = currentline;
    //Else we display first, the missing part
    movetoline(current++);
    displayextract(sub, from_line++, from_pos, sub.size(), false);
    while (from_line < to_line) {
        sub = lines[from_line];
        movetoline(current++);
        displayextract(sub, from_line++, 0, sub.size(), false);
    }
    movetoline(current);
    sub = lines[from_line];
    displayextract(sub, from_line, 0, to_pos, false);
}


void jag_editor::computeposition(int& p, long position) {
    wstring s = lines[position];
    p -= prefixe() + 1;
    long i = 0;
    int pos = 0;
    UWCHAR c;
    while (pos < p) {
        if (special_characters.scan_emoji(s, i))
            pos += 2;
        else {
            c = getonewchar(s, i);
            if (ckjchar(c)) {
                pos+=2;
            }
            else {
                if (c == 9) //tab position
                    pos += 8;
                else
                    pos++;
            }
        }
        i++;
    }
    p = (int)i;
}

void jag_editor::handlemousectrl(string& mousectrl) {
    vector<int> location;

    if (isScrollingUp(location,mousectrl)) {
        if (selected_pos != -1) {
            unselectlines(selected_pos, selected_posnext, selected_x, selected_y);
			resetselection();
            double_click = 0;
        }

        pos = poslines[0];
        currentline = 0;
        updown(is_up, pos);
        return;
    }

    if (isScrollingDown(location,mousectrl)) {
        if (selected_pos != -1) {
            unselectlines(selected_pos, selected_posnext, selected_x, selected_y);
			resetselection();
            double_click = 0;
        }

        pos = poslines.back();
        currentline = poslines.size() - 1;
        updown(is_down, pos);
        return;
    }

    if (isClickFirstMouseDown(location, mousectrl)) {
        if (selected_pos != -1)
            unselectlines(selected_pos, selected_posnext, selected_x, selected_y);

        currentline = location[0] - 1;

        if (currentline < 0)
            currentline = 0;
        if (currentline >= poslines.size())
            currentline = poslines.size()-1;

        pos = poslines[currentline];
        line = lines[pos];
        long endofstring = line.size();
        if (endofstring && !lines.eol(pos))
            endofstring--;


        int mxcursor = 0, mycursor;
        long l, r;
        computeposition(location[1], pos);
        if (location[1] < endofstring)
            l = location[1];
        else
            l = endofstring;

        if (l < 0)
            l = 0;

        mycursor = location[1];
        int cursor_y;
        long posnext = pos;
        r = l;


        resetselection();
        selected_firstline = currentline;

        if (!checkMouseup(mousectrl)) {
            mousectrl = getch();

            while (mouseTracking(mousectrl, mxcursor, cursor_y)) {
                if (cursor_y != mycursor)
                    unselectlines(pos, posnext, l, r);
                double_click = 0;
                if (mxcursor >= poslines.size())
                    mxcursor = (int)poslines.size();

                posnext = poslines[mxcursor - 1];
                computeposition(cursor_y, posnext);

                if (cursor_y < 0)
                    cursor_y = 0;
                mycursor = cursor_y;
                r = cursor_y;
                selectlines(pos, posnext, l, r);
                mousectrl = getch();
            }
        }
        else {
            mxcursor = location[0];
            if (mxcursor >= poslines.size())
                mxcursor = (int)poslines.size();

        }

        //a simple click
#ifdef WIN32
        if (location[0] == mxcursor && location[1] == mycursor) {
			double_click += nbClicks();
#else
		if (location[0] == mxcursor && location[1] == mycursor) {
			double_click++;
#endif
            posinstring = l;

            if (double_click >= 2) {
                if (double_click >= 3) {
                    double_click = 0;
					nbclicks = 0;
                    l = 0;
                    r = endofstring;
                    selected_pos = pos;
                    selected_posnext = pos;

                    selected_x = l;
                    selected_y = r;
                    selectlines(pos, pos, l,r);
                    posinstring = r;
                    movetoposition();
                    return;
                }

                while (l >= 0 && l < line.size() && checkcar(line[l])) l--;
                if (l < 0)
                    l = 0;
                else {
                    if (!checkcar(line[l]))
                        l++;
                }
                r = l+1;
                while (r < endofstring && checkcar(line[r])) r++;
                if (l < r && l < endofstring) {
                    selected_pos = pos;
                    selected_posnext = pos;
                    selected_x = l;
                    selected_y = r;
                    selectlines(pos, pos, l, r);
                    posinstring = r;
                    movetoposition();
                    return;
                }
            }

            movetoline(currentline);
            movetoposition();
            return;
        }

        //a selection
        selected_pos = pos;
        selected_posnext = posnext;
        double_click = 0;
		nbclicks = 0;
        selected_x = l;
        selected_y = r;
        return;
    }

    double_click = 0;
}

//This function is defined as a stud in lispeditor or jagtools
string coloring_line(editor_lines& lines, long currentline, string& line, vector<string>& colors, file_types filetype);
string jag_editor::coloringaline(string line, long i,  bool thread) {
    return coloring_line(lines, i, line, colors, filetype);
}

string jag_editor::coloringline(wstring& w, long i) {
    string l = convert(w);
    return coloring_line(lines, i, l, colors, filetype);
}


//This is the main method that launches the terminal
void jag_editor::launchterminal(bool darkmode, char loadedcode, vector<string>& args, vector<string>& newcolors) {
    arguments = args;

    if (darkmode)
        switch_darkmode();

    if (newcolors.size() != 0) {
        colors.clear();
        colors = newcolors;
    }

    localhelp << m_red << "^c/q" << m_current << ":exit";

    if (loadedcode) {
        displaylist(0);
        movetoline(currentline);
        posinstring = 0;
        movetobeginning();
    }
    else {
        init();
    }

    option = x_none;

    wstring bl;
    wstring b;

    wstring code;
    string buffer;
    bool inbuffer = false;

    bool instring = false;
    long first = 0, last;

    string buff = paste_from_clipboard();
    copybuffer = wconvert(buff);
    kbuffer = copybuffer;

    buff = "";

    while (1) {
        buff = getch();

        if (emode()) {
			while (isMouseAction(buff)) {
				handlemousectrl(buff);
				buff = getch();
			}
		}

        if (selected_pos != -1 && buff[0] != 24)
            unselectlines(selected_pos, selected_posnext, selected_x, selected_y);

        if (checkaction(buff, first, last)) {
            double_click = 0;
            if (buff[0] != 24) {
                selected_x = -1;
                selected_y = -1;
                selected_pos = -1;
            }
            continue;
        }

        if (option == x_exitprint)
            return;

        if (selected_pos == pos) {
            //We are going to replace a sequence of characters
            //we delete it first
            deleteselection();
        }

        double_click = 0;
        selected_x = -1;
        selected_y = -1;
        selected_pos = -1;

        if (inbuffer) {
            buffer += buff;
            buff = buffer;
            inbuffer = false;
        }

        if (buff.size() == _getbuffsize)
            inbuffer = check_utf8(buff, buffer);

        bl = wconvert(buff);
        cleanheaders(bl);

        if (emode()) {
            //We keep track of the initial form of the line...
            undo(lines[pos],pos, u_modif); //The value is negative to indicate a deletion
        }

            //Only one character to add, no need for further inspection, no CR in the string as well
        if (bl.size() == 1 || buff.find(10) == -1) {
            addabuffer(bl, instring);
            continue;
        }

        for (long j = 0; j < bl.size(); j++) {
            b = bl[j];
            if (b[0] == 10) {
                pos = handlingeditorline(false);
                continue;
            }
            addabuffer(b, instring);
        }
    }
}

//---------------------------------------------------------------------------------

void editor_lines::setcode(wstring& code, bool clean) {
    if (code == L"")
        return;

    vector<wstring> buff;
    status.clear();
    lines.clear();
    numeros.clear();
    longlines.clear();

    jag->vsplit(code, L"\n", buff);
    jag->setprefixesize(buff.size());
    if (!buff.size())
        return;

    long u;
    vector<wstring> subs;

    if (clean) {
        while (buff.size() && buff.back() == L"") {
            buff.pop_back();
        }

        buff.push_back(L"");
    }

    for (long i = 0; i < buff.size(); i++) {
        numeros.push_back(i + 1);
        if (splitline(buff[i], i, subs) == 1) {
            lines.push_back(buff[i]);
            status.push_back(solo_line);
            subs.clear();
            continue;
        }

        lines.push_back(subs[0]);
        status.push_back(beg_line);
        for (u = 1; u < subs.size(); u++) {
            lines.push_back(subs[u]);
            status.push_back(concat_line);
            numeros.push_back(i + 1);
        }
        subs.clear();
    }

    updatesize();
}

long jag_editor::splitline(wstring& l, long linenumber, vector<wstring>& subs) {
    //we compute the position of each segment of l on string...

    long sz = prefixe();

    if ((l.size() + sz) < (col_size / 2)) {
        subs.push_back(l);
        return 1;
    }

    wstring code;
    UWCHAR c;

    for (long i = 0; i < l.size(); i++) {
        if (special_characters.store_emoji(l, code, i))
            sz++;
        else {
            c = getonewchar(l, i);
            concat_to_wstring(code, c);
            if (c == 9) {//tab position
                sz += (8 - (sz%8))%8;
                sz--;
            }
            else {
                if (ckjchar(c)) //double space on screen
                    sz++;
            }
        }
        sz++;
        if (sz >= col_size) {
            subs.push_back(code);
            code = L"";
            sz = prefixe();
        }
    }

    if (code != L"" || !subs.size())
        subs.push_back(code);

    return subs.size();
}

long jag_editor::taille(wstring& s) {
    long sz = s.size();
    long pref = prefixego() + 1;
    long pos = pref;
    UWCHAR c;
    for (long i = 0; i < sz; i++) {
        if (special_characters.scan_emoji(s, i))
            pos += 2;
        else {
            c = getonewchar(s, i);
            if (ckjchar(c)) {
                pos += 2;
            }
            else {
                if (c == 9) //tab position
                    pos += (8 - (pos%8))%8;
                pos++;
            }
        }
    }
    return (pos-pref);
}

long jag_editor::sizestring(wstring& s) {
    long sz = s.size();
    long szstr = 0;
    for (long i = 0; i < sz; i++) {
        if (!special_characters.scan_emoji(s, i)) {
            getonewchar(s, i);
        }
        szstr++;
    }
    return szstr;
}

long jag_editor::size_upto(wstring& s, long p) {
    long pref = prefixego() + 1;
    long pos = pref;
    UWCHAR c;
    for (long i = 0; i < p; i++) {
        if (special_characters.scan_emoji(s, i))
            pos += 2;
        else {
            c = getonewchar(s, i);
            if (ckjchar(c)) {
                pos += 2;
            }
            else {
                if (c == 9) //tab position
                    pos += (8 - (pos%8))%8;
                pos++;
            }
        }
    }
    return (pos-pref);
}


//The deletion of a character is different if it is an emoji...
long jag_editor::deleteachar(wstring& l, bool last, long pins) {
    if (l == L"")
        return pins;

    long mx = 1;
    if (selected_pos != -1) {
        pins = selected_x;
        mx = selected_y - selected_x;
    }

    if (last) {
        while (mx) {
            if (special_characters.c_is_emojicomp(l.back())) {
                long sz = l.size() - 2;
                long i = sz;
                while (sz >= 0 && !special_characters.scan_emoji(l, i)) {
                    sz--;
                    i = sz;
                }
                if (sz >= 0) {
                    pins -= i - sz;
                    l.erase(i, sz - i + 1);
                }
                else
                    l.pop_back();
            }
            mx--;
        }
        l.pop_back();
    }
    else {
        long nb = 0;
        long i = pins;
        long j;
        while (mx) {
            j = i;
            if (special_characters.scan_emoji(l, j)) {
                nb += j - i;
                i = j;
            }
            else
                i++;
            nb++;
            mx--;
        }
        l.erase(pins, nb);
    }
    return pins;
}

void jag_editor::forwardemoji() {
    special_characters.scan_emoji(line, posinstring);
    posinstring++;
}

void jag_editor::backwardemoji() {
    posinstring--;
    if (posinstring < 0)
        return;

    long p = 0;
    for (long i = 0; i < line.size(); i++) {
        p = i;
        if (!special_characters.scan_emoji(line, p)) {
            getonewchar(line, p);
            if (p >= posinstring) {
                posinstring = i;
                return;
            }
        }
        else {
            if (p >= posinstring) {
                posinstring = i;
                return;
            }
        }
        i = p;
    }
}


long editor_lines::splitline(wstring& l, long linenumber, vector<wstring>& subs) {
    return jag->splitline(l,linenumber, subs);
}

bool editor_lines::checksize(long p) {
    return jag->checksize(lines[p]);
}

bool editor_lines::checkfullsize(wstring& l, bool& equal) {
    return jag->checksizeequal(l, equal);
}

void editor_lines::undo(wstring& l, long p, char a) {
    jag->undo(l,p,a);
}

bool editor_lines::refactoring(long pos) {
    long p = pos;
        //First we look for the first line of our group
    if (Status(p) == concat_line) {
        while (Status(p) != beg_line) p--;
    }

        //We keep this track...
    long first = p;
    wstring baseline = lines[p++];

    //It was not at the beginning of a line, we concatenate our block
    while (Status(p) == concat_line)
        baseline += lines[p++];

    if (baseline == L"") {
        return false;
    }

    //We then clean all these lines...

    editor_lines sublines(jag);
    sublines.setcode(baseline, false);
    //same number of lines
    long i;
    for (i = 0; i < sublines.size(); i++) {
        if (first != pos)
            jag->undo(lines[first], first, u_modif_linked);
        if (first < p) {
            lines[first] = sublines[i];
            status[first] = sublines.status[i];
        }
        else
            insert(first, sublines[i], sublines.status[i]);
        first++;
    }

    bool delete_line = false;
    while (first < p) {
        delete_line = true;
        jag->undo(lines[first], first, u_del_linked);
        erase(first);
        p--;
    }

    numbers();
    return delete_line;
}

char editor_lines::updatestatus(long pos) {
    if (Status(pos) == beg_line) {
        status[pos] = solo_line;
        if (Status(pos+2) == concat_line)
            status[pos+1] = beg_line;
        else
            status[pos+1] = solo_line;
        return status[pos+1];
    }
    return -1;
}

bool editor_lines::updatesize() {
    long prf = jag->prefixsize;
    jag->setprefixesize(lines.size());
    if (jag->prefixsize != prf)
        return true;
    return false;
}

//------------------------------------------------------------------------------------

//static const char m_current[] = {27, '[', '0', 'm', 0};
bool check_string(editor_lines& lines, string& line, long currentline, wstring op, wstring cl, string color) {
    long pos = currentline;
    //We check if we are in a long comment
    while (pos >= 0 && lines[pos].find(op) ==-1) {
        wstring l = lines[pos];
        if (lines[pos].find(cl) != -1) {
            pos = -1;
            break;
        }
        pos--;
    }

    //We are in a long comment
    if (pos != -1) {
        while (pos < lines.size() && lines[pos].find(cl) == -1)
            pos++;
        if (pos >= currentline) {
            line = color + line + m_current;
            return true;
        }
    }
    return false;
}
