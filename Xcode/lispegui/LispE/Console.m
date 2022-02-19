/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : Console.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

void Initdisplay(short id);
const char* Getdisplay(void);
int Compilecode(const char* cde, const char* filename, char console);
char Run(int);
void InitialisationDisplay(short id, char);
void CleanGlobal(void);
long CurrentLine(void);
const char* Currentfilename(void);
char WindowModeActivated(void);
char StopExecution(void);
const char* lispversion(void);
const char* Listing(void);
void Setdebugmode(char);
const char* Readfile(const char* path);
void setlispepath(const char*,const char* v);
void initlispepath(const char* homepath);
char IsRunning(void);
char* initastring(const char* r, long sz);
char* returninput(void);
long computeparenthesis(const char* ln, char checkcar, long limit);

#import "AppDelegate.h"
#import "Console.h"
#import "ViewController.h"
#import "animationSegue.h"
#import "tamgudebugger.h"

NSMutableDictionary *allfiles;

extern AppDelegate* currentdelegate;

extern tamgudebugger* debugger;

BOOL nouveau = NO;
extern BOOL runingmode;
extern BOOL dark;

Console* tview;
tamgudebugger* debugcontroller = nil;

extern ViewController* vue;

BOOL isItMainThread(void) {
    if ([NSThread isMainThread]==YES)
        return YES;
    return NO;
}

void Initlispelibspath(void) {
    static bool init=false;
    if (!init) {
        NSString* homepath=NSHomeDirectory();
        initlispepath([homepath UTF8String]);
        init = true;
    }
}

void Rappel(char th, const char* txt) {
    if (isItMainThread())
        [tview rappel:txt];
    else {//We are in a thread, in that case, we post our request for display
        NSString* msg = [NSString stringWithUTF8String:txt];
        if (msg == nil)
            msg = @"@#&...";

        [tview performSelectorOnMainThread:@selector(displayinconsole:)
                                withObject:msg
                             waitUntilDone:YES];
    }
}

const char* Inputtext(const char* msg) {
    NSString* nmsg=[NSString stringWithUTF8String:msg];
    if (isItMainThread())
        return [tview InputText:nmsg];
    else {//We are in a thread, in that case, we post our request for display
        [tview performSelectorOnMainThread:@selector(InputText:)
                                withObject:nmsg
                             waitUntilDone:YES];
    }
    
    if (tview->inputtxt==NULL)
        return NULL;
    
    return [tview->inputtxt UTF8String];
}


@implementation Console

-(BOOL)appearanceIsDark:(NSAppearance*) appearance
{
    if (@available(*, macOS 10.14)) {
        NSAppearanceName basicAppearance = [appearance bestMatchFromAppearancesWithNames:@[
            NSAppearanceNameAqua,
            NSAppearanceNameDarkAqua
        ]];
        return [basicAppearance isEqualToString:NSAppearanceNameDarkAqua];
    }
    return NO;
}

-(void)awakeFromNib {
    
    dark = [self appearanceIsDark: NSAppearance.currentAppearance];
    
    [self toggleAutomaticQuoteSubstitution: false];
    [self toggleGrammarChecking:false];
    [self toggleAutomaticDashSubstitution:false];
    
    tview = self;
    NSFont* font=[NSFont fontWithName:@"Helvetica" size:16.0];
    [self setFont: font];
    runlock=[NSRecursiveLock alloc];
    inputtxt=NULL;
    [self setDelegate: currentdelegate];
}

-(void)insertNewline:(id)sender {
    
    NSRange r = [self selectedRange];
    r = [[self string] paragraphRangeForRange:r];
    if (r.length != 0) {
        NSString *string = [[self string] substringWithRange:r];
        
        const char* code=[string UTF8String];
        bool found=false;
        long sz = strlen(code);
        for (int i=0;i<sz;i++) {
            unsigned char c = (unsigned char)code[i];
            if (c>32) {
                found=true;
                break;
            }
        }
        
        if (!found) {
            [super insertNewline:sender];
            return;
        }
        
        if (dark)
            [self setTextColor: [NSColor whiteColor] range:r];
        else
            [self setTextColor: [NSColor blackColor] range:r];
        if (Compilecode(code, "", true) != -1) {
            InitialisationDisplay(0, false);
            if (!Run(0)) {
                [super insertNewline:sender];
                [tview displayerror: nil];
            }
        }
        else {
            [super insertNewline:sender];
            [self displayerror:nil];
            return;
        }
    }
    
    [super insertNewline:sender];
}

- (IBAction)opendocument:(id)sender {
    NSOpenPanel* openDlg = [NSOpenPanel openPanel];
    
    // Enable the selection of files in the dialog.
    
    // Display the dialog.  If the OK button was pressed,
    // process the files.
    if ( [openDlg runModal] == NSModalResponseOK )
    {
        // Get an array containing the full filenames of all
        // files and directories selected.
        nouveau=NO;
        NSArray* files = [openDlg URLs];
        
        // Loop through all the files and process them.
        NSURL* fileName = [files objectAtIndex:0];
        if (allfiles != nil && [allfiles objectForKey:[fileName path]]) {
            CodeViewController* cvc = (CodeViewController*)[allfiles objectForKey:[fileName path]];
            [[[cvc view] window] orderFrontRegardless];
            return;
        }
        
        [[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:fileName];
        currentdelegate->currentfilename= [fileName path];
        
        NSStoryboard *storyboard = [NSStoryboard storyboardWithName:@"Main" bundle:nil];
        CodeViewController* cvc = [storyboard instantiateControllerWithIdentifier:@"lecodeeditor"];
        cvc->a= [[animationSegue alloc] init];
        if (allfiles == nil)
            allfiles = [NSMutableDictionary new];
        
        allfiles[currentdelegate->currentfilename] = cvc;
        [vue presentViewController:cvc animator:cvc->a];
    }
}

- (IBAction)execution:(id)sender {
    [tview Executecode:false cleaning:true];
}


-(NSString*)GetText:(const char*)msg {
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert addButtonWithTitle:@"Cancel"];
    
    NSString* nmsg=[NSString stringWithUTF8String:msg];
    [alert setMessageText:nmsg];

    NSRect rect= NSMakeRect(0,0,800,25);
    NSTextField* txt = [[NSTextField alloc] initWithFrame:rect];
    [alert setAccessoryView:txt];
    NSModalResponse res = [alert runModal];
    NSString* rep= @"";
    if (res == 1001)
        return rep;
    rep=[txt stringValue];
    return rep;
}

-(BOOL)DisplayAlert:(NSString*)msg {
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert addButtonWithTitle:@"Cancel"];
    [alert setMessageText:@"Warning!!!"];
    [alert setInformativeText:msg];
    [alert setAlertStyle:NSAlertStyleWarning];
    NSModalResponse res = [alert runModal];
    if (res == 1001)
        return NO;
    return YES;
}

- (IBAction)endofapp:(id)sender {
    [debugger clearlock];
    StopExecution();
    if ([self checkstatus] == YES) {
        if ([self DisplayAlert:@"Some files have been modified. Quit anyway?"] == NO)
            return;
    }
    vue=nil;
    tview=nil;
    [NSApp terminate:nil];
}

-(void)Runcode:(NSString*)fileName {
    const char* filename=[fileName UTF8String];
    const char* code = Readfile(filename);
    if (code != NULL) {
        CleanGlobal();
        if (Compilecode(code, filename, false) != -1) {
            InitialisationDisplay(0, false);
            if (!Run(0)) {
                [tview displayerror: nil];
            }
        }
        else {
            [tview displayerror:nil];
        }
    }
}

-(BOOL)Executecode:(BOOL)debugmode cleaning:(BOOL)clean {
    
    if (IsRunning())
        return false;

    NSViewController* n;
    if (clean)
        [tview setString:@""];
    
    NSWindow* wnd=[[NSApp orderedWindows] objectAtIndex:0];
    n=[wnd contentViewController];
    if (n == vue)
        return false;
    
    CodeViewController* cn=(CodeViewController*)n;
    const char* filename = [[[cn Code] Filename] UTF8String];
    const char* code = [[[cn Code] Contenu] UTF8String];
    Setdebugmode(false);
    
    CleanGlobal();
    if (Compilecode(code, filename, false) != -1) {
        if (WindowModeActivated() == 0) { //if we have a GUI, we do not launch it in a thread
            //the second parameter "true" indicates that the code is executing in thread mode...
            InitialisationDisplay(0, true);
            NSThread* myThread;
            runingmode = YES;

            myThread = [[NSThread alloc] initWithTarget:[Console class] selector:@selector(runinthread:) object:cn];
            
            NSInteger stacksz=[myThread stackSize];
            [myThread setStackSize:stacksz<<4];
            [myThread start];
            return true;
        }
        
        InitialisationDisplay(0, false);
        if (!Run(0)) {
            [tview displayerror: cn];
        }
        //If we run a FLTK program, we may change the current delegate...
        //We need to put it back with the current value...
        ((AppDelegate *)[NSApplication sharedApplication]).delegate = currentdelegate;
    }
    else {
        [tview displayerror:cn];
    }
    return true;
}

- (IBAction)debugmode:(id)sender {
    CodeViewController* cn=[self topview];
    const char* filename = [[[cn Code] Filename] UTF8String];
    const char* code = [[[cn Code] Contenu] UTF8String];
    CleanGlobal();
    
    Setdebugmode(true);
    if (Compilecode(code, filename, false) != -1) {
        if (WindowModeActivated()) { //if we have a GUI, we do not launch it in a thread
            [self DisplayMessage:@"Sorry, we cannot debug GUI programs"];
            return;
        }
        
        InitialisationDisplay(0, true);
        
        NSStoryboard *storyboard = [NSStoryboard storyboardWithName:@"Main" bundle:nil];
        debugcontroller = [storyboard instantiateControllerWithIdentifier:@"ledebug"];
        [vue presentViewController:debugcontroller animator:[[animationSegue alloc] init]];
        NSThread* myThread;
        runingmode = YES;

        myThread = [[NSThread alloc] initWithTarget:[Console class] selector:@selector(runinthread:) object:cn];
        
        NSInteger stacksz=[myThread stackSize];
        [myThread setStackSize:stacksz<<4];
        [myThread start];
        return;
    }
    else
        [tview displayerror:cn];
}

- (IBAction)matchingbracket:(id)sender {
    CodeViewController* cn = [self topview];
    if (cn != nil) {
        [[cn Code] blinkMatchingParenthesis];
    }
}

- (void)matchingparenthesis:(char)closingcharacter {
    NSRange rcursor = [self selectedRange];
    NSRange rline = [[self string] paragraphRangeForRange:rcursor];
    
    NSString *string = [[self string] substringWithRange:rline];
    
    const char* code=[string UTF8String];
    long pos = computeparenthesis(code, closingcharacter, rcursor.location - rline.location - 1);
    if (pos != -1) {
        if (dark)
            [self setTextColor: [NSColor whiteColor] range:rline];
        else
            [self setTextColor: [NSColor blackColor] range:rline];
        rline.location += pos;
        rline.length = 1;
        [self setTextColor: [NSColor redColor] range:rline];
    }
}


+(void)runinthread:(CodeViewController*)cn {
    
    [tview->runlock lock];
    if (!Run(0)) {
        [tview performSelectorOnMainThread:@selector(displayerror:)
                                withObject:cn
                             waitUntilDone:YES];
    }
    [tview->runlock unlock];
    runingmode = NO;
    [[NSCursor arrowCursor] set];
}

-(void)Selectline:(NSNumber*)ln {
    CodeViewController* cn = [self topview];
    if (cn != nil) {
        long l = [ln integerValue];
        [[cn Code] selectLineInCode:l];
    }
}

-(void)displayerror:(CodeViewController*)cn {
    if (cn != nil) {
        long errorline = CurrentLine();
        const char* filename = Currentfilename();
        CodeViewController* cfn = [self searchwindow: filename];
        if (cfn != nil)
            cn = cfn;
        [[cn Code] selectLineInCode: errorline];
    }
    
    const char* err = Getdisplay();
    NSString* errmsg = [NSString stringWithUTF8String:err];
    [tview displayinconsole: errmsg];
    [tview DisplayMessage: errmsg];
}

- (IBAction)savedocument:(id)sender {
    CodeViewController* cn=[self topview];
    
    NSURL* fn;
    bool update=false;
    if ([[cn Code] isTobecreated] == YES) {
        fn = [[cn Code] Setname];
        if ([[cn Code] isTobecreated] == YES)
            return;
        
        NSString* name =  [fn path];
        if (allfiles == nil)
            allfiles = [NSMutableDictionary new];
        
        allfiles[name] = cn;

        update=true;
    }
    else
        fn =[[cn Code] FileURL];
    
    NSError* err=nil;
    
    NSString* content=[[cn Code] Contenu];
    [content writeToURL:fn atomically:YES encoding:NSUTF8StringEncoding error:&err];
    //[cnt writeToFile:[[cn Code] Filename] atomically:TRUE encoding:NSUTF8StringEncoding error:&err];
    if (err != nil) {
        [tview displayinconsole: err.localizedDescription];
        [tview DisplayMessage: err.localizedDescription];
    }
    else {
        [[cn Code] setmodified:NO];
        if (update)
            [[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:fn];
    }
}

- (IBAction)saveas:(id)sender {
    CodeViewController* cn=[self topview];

    NSString* previous = [[cn Code] Filename];
    NSURL* fn;
    
    fn = [[cn Code] Setname];
    if ([[cn Code] isTobecreated] == YES) {
        if (![previous isEqualToString:@""])
            [[cn Code] Tobecreated: NO];
        return;
    }
    
    NSString* name =  [fn path];
    if (allfiles == nil)
        allfiles = [NSMutableDictionary new];
    
    allfiles[name] = cn;

    NSError* err=nil;
    
    [[[cn Code] Contenu] writeToURL:fn atomically:YES encoding:NSUTF8StringEncoding error:&err];
    //[cnt writeToFile:[[cn Code] Filename] atomically:TRUE encoding:NSUTF8StringEncoding error:&err];
    if (err != nil) {
        [tview displayinconsole: err.localizedDescription];
        [tview DisplayMessage: err.localizedDescription];
    }
    else {
        [[cn Code] setmodified:NO];
        [[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:fn];
    }
}

-(CodeViewController*)searchwindow:(const char*)filename {
    
    NSString* lookingfor =[NSString stringWithUTF8String:filename];
    
    NSArray* files = [NSApp orderedWindows];
    long nb = [files count];
    NSWindow* wnd;
    NSViewController* n;
    for (long i = 0;i < nb; i++) {
        wnd=[[NSApp orderedWindows] objectAtIndex:i];
        if (wnd == [debugger window])
            continue;
        
        @try {
            n=[wnd contentViewController];
        }
        @catch (NSException *exception) {
            continue;
        }
        
        if (n==vue)
            continue;
        
        if ([n isKindOfClass:[CodeViewController class]]) {
            CodeViewController* cn=(CodeViewController*)n;
            NSString* fn = [[cn Code] Filename];
            if ([fn isEqualToString:lookingfor])
                return cn;
        }
    }
    return nil;
}

-(BOOL)checkstatus {
    NSArray* files = [NSApp orderedWindows];
    long nb = [files count];
    NSWindow* wnd;
    NSViewController* n;
    for (long i = 0;i < nb; i++) {
        wnd=[[NSApp orderedWindows] objectAtIndex:i];
        if (wnd == [debugger window])
            continue;

        @try {
            n=[wnd contentViewController];
        }
        @catch (NSException *exception) {
            continue;
        }
        
        if (n==vue)
            continue;
        
        if ([n isKindOfClass:[CodeViewController class]]) {
            CodeViewController* cn=(CodeViewController*)n;
            if ([[cn Code] ismodified] == YES)
                return YES;
        }
    }
    return NO;
}

- (IBAction)indentation:(id)sender {
    NSWindow* wnd=[[NSApp orderedWindows] objectAtIndex:0];
    NSViewController* n=[wnd contentViewController];
    if (n == vue)
        return;
    
    CodeViewController* cn=(CodeViewController*)n;
    [[cn Code] indentation];
}

-(CodeViewController*)topview {
    NSWindow* wnd=[[NSApp orderedWindows] objectAtIndex:0];
    NSViewController* n=[wnd contentViewController];
    if (n == vue) {
        return nil;
    }
    
    CodeViewController* cn=(CodeViewController*)n;
    return cn;
}

-(void)displaychar: (NSString*)car {
    NSWindow* wnd=[[NSApp orderedWindows] objectAtIndex:0];
    NSViewController* n=[wnd contentViewController];
    if (n == vue) {
        [tview displayinconsole:car];
        return;
    }
    
    CodeViewController* cn=(CodeViewController*)n;
    [[cn Code] insere:car];
}

- (IBAction)negation:(id)sender {
    [tview displaychar:@"¬"];
}

- (IBAction)different:(id)sender {
    [tview displaychar:@"≠"];
}

- (IBAction)disjunction:(id)sender {
    [tview displaychar:@"∨"];
}

- (IBAction)conjunction:(id)sender {
    [tview displaychar:@"∧"];
}

- (IBAction)pi:(id)sender {
    [tview displaychar:@"π"];
}

- (IBAction)tau:(id)sender {
    [tview displaychar:@"τ"];
}

- (IBAction)euler:(id)sender {
    [tview displaychar:@"ℯ"];
}

- (IBAction)golden:(id)sender {
    [tview displaychar:@"φ"];
}

- (IBAction)breakpoint:(id)sender {
    CodeViewController* cn = [tview topview];
    if (cn != nil)
        [[cn Code] insertBreakpoint];
}

- (IBAction)multiply:(id)sender {
    [tview displaychar:@"×"];
}

- (IBAction)divide:(id)sender {
    [tview displaychar:@"÷"];
}

- (IBAction)product:(id)sender {
    [tview displaychar:@"∏"];
}

- (IBAction)sum:(id)sender {
    [tview displaychar:@"∑"];
}

- (IBAction)squareroot:(id)sender {
    [tview displaychar:@"√"];
}

- (IBAction)cubicroot:(id)sender {
    [tview displaychar:@"∛"];
}

- (IBAction)square:(id)sender {
    [tview displaychar:@"²"];
}

- (IBAction)cube:(id)sender {
    [tview displaychar:@"³"];
}

- (IBAction)arrow:(id)sender {
    [tview displaychar:@"←"];
}

- (IBAction)arrowright:(id)sender {
    [tview displaychar:@"→"];
}

- (IBAction)setpath:(id)sender {
    NSString* rep=[self GetText:"Set LISPEPATH"];
    if ([rep isEqualToString: @""])
        return;
    
    NSString* path=NSHomeDirectory();
    setlispepath([path UTF8String],[rep UTF8String]);
}

- (IBAction)stopexecution:(id)sender {
    StopExecution();
    [tview displayinconsole:@"\n\nbreak...\n\n"];
}

- (IBAction)abouttamgu:(id)sender {
    NSString* message = [NSString stringWithUTF8String:lispversion()];
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert setMessageText:@"Version (LispE)"];
    [alert setInformativeText:message];
    [alert setAlertStyle:NSAlertStyleWarning];
    [alert runModal];
}

- (IBAction)listing:(id)sender {
    const char* code = Listing();
    [tview displayinconsole: [NSString stringWithUTF8String:code]];
}

- (IBAction)clearallbreakpoints:(id)sender {
    CodeViewController* cn = [tview topview];
    if (cn != nil)
        [[cn Code] clearallbreakpoints];
}

- (IBAction)runnotclean:(id)sender {
    [tview Executecode:false cleaning:false];
}


-(void)DisplayMessage: (NSString*) message {
    
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert setMessageText:@"Error!!!"];
    [alert setInformativeText:message];
    [alert setAlertStyle:NSAlertStyleWarning];
    [alert runModal];
}

- (void) prepareForSegue:(NSStoryboardSegue *)segue sender:(id)sender {
    nouveau = YES;
}

-(void)rappel: (const char*)code {
    NSString* txt;
    if (code[0]!='\r') {
        txt=@"\r";
        txt = [txt stringByAppendingString: [NSString stringWithUTF8String:code]];
    }
    else
        txt=[NSString stringWithUTF8String:code];
    
    if (txt == nil)
        txt=@"@#&...";
    
    [self displayinconsole: txt];
}

-(void) displayinconsole: (NSString*)output {
    if (output != nil) {
        [self setDelegate: nil];
        NSRange r = [self selectedRange];
        [self insertText: output replacementRange: r];
        [self setDelegate: currentdelegate];
    }
}

-(const char*)InputText:(NSString*)nmsg {
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert addButtonWithTitle:@"Cancel"];
    
    [alert setMessageText:nmsg];
    
    NSRect rect= NSMakeRect(0,0,800,25);
    NSTextField* txt = [[NSTextField alloc] initWithFrame:rect];
    [alert setAccessoryView:txt];
    NSModalResponse res = [alert runModal];
    if (res == 1001) {
        inputtxt=NULL;
        return NULL;
    }
    
    inputtxt=[txt stringValue];
    return [inputtxt UTF8String];
}


@end

