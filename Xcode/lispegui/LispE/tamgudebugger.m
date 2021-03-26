/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : tamgudebugger.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import "AppDelegate.h"
#import "tamgudebugger.h"
#import "Console.h"
char NextLine(void);
char Gotonextbreak(void);
char Getin(void);
char Getout(void);
char StopDebug(void);
char Gotoend(void);
void Blocked(void);
const char* Getcurrentfilename(void);
void Shortname(char v);
void Released(void);

extern NSMutableDictionary *allfiles;
extern Console* tview;
extern AppDelegate* currentdelegate;

@implementation tamgudebugger

tamgudebugger* debugger = nil;

-(void)awakeFromNib {
    debugger = self;
    delegatenotdone=YES;
}

void displaydebug(const char* locals, const char* globals, const char* sstack, const char* filename, long currentline) {
    [debugger performSelectorOnMainThread:@selector(Setthedelegate:) withObject:nil waitUntilDone:YES];
    [debugger performSelectorOnMainThread:@selector(Setlocals:) withObject:[NSString stringWithUTF8String:locals] waitUntilDone:YES];
    [debugger performSelectorOnMainThread:@selector(Setglobals:) withObject:[NSString stringWithUTF8String:globals] waitUntilDone:YES];
    [debugger performSelectorOnMainThread:@selector(Setstack:) withObject:[NSString stringWithUTF8String:sstack] waitUntilDone:YES];
    [debugger Selectline:currentline];
}

- (IBAction)nextline:(id)sender {
    if (!NextLine())
        [self Setlocals:@"End of Execution reached"];
}

- (IBAction)gotonextbreak:(id)sender {
    if (!Gotonextbreak())
        [self Setlocals:@"End of Execution reached"];
    
}

- (IBAction)stoprunning:(id)sender {
    StopDebug();
    [self Setlocals:@"End of Execution reached"];
}

- (IBAction)runuptotheend:(id)sender {
    Gotoend();
    [self Setlocals:@"End of Execution reached"];
}

- (IBAction)intfunction:(id)sender {
    if (!Getin())
        [self Setlocals:@"End of Execution reached"];
    
}

- (IBAction)outfunction:(id)sender {
    if (!Getout())
        [self Setlocals:@"End of Execution reached"];
    
}

- (IBAction)fulldisplay:(id)sender {
    BOOL b = [sender state];
    if (b)
        Shortname(1);
    else
        Shortname(0);
}

-(void)clearlock {
    Released();
}

-(void)Setthedelegate:(id)sender {
    if (debugger->delegatenotdone==YES) {
        [[debugger window] setDelegate:currentdelegate];
        debugger->delegatenotdone=NO;
    }
}

-(void)Setlocals:(NSString*)l {
    [_localvariables setStringValue:l];
}

-(void)Setglobals:(NSString*)l {
    [_globalvariables setStringValue:l];
}

-(void)Setstack:(NSString*)l {
    [_thestack setStringValue:l];
}

-(void)Selectline:(long)l {
    NSString* filename = [NSString stringWithUTF8String: Getcurrentfilename()];
    NSNumber* ln=[NSNumber numberWithInteger:l];
        
    
    NSViewController* n = [allfiles objectForKey: filename];

    if (n==nil)
        return;
    
    CodeViewController* cn=(CodeViewController*)n;
    [cn performSelectorOnMainThread:@selector(selectLine:)
                                      withObject:ln
                                   waitUntilDone:YES];
}

@end
