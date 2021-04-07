/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : AppDelegate.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import "AppDelegate.h"
#import "tamgudebugger.h"
#import "CodeViewController.h"
#import "ViewController.h"

void LispEFinalClean(void);
char StopExecution(void);
BOOL runingmode;

extern ViewController* vue;
extern Console* tview;
AppDelegate* currentdelegate = nil;
extern tamgudebugger* debugger;

#import "animationSegue.h"
extern NSMutableDictionary* allfiles;

@interface AppDelegate ()

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    initialization = NO;
    if (currentfilename != nil) {
        if (vue == nil)
            [tview Runcode:currentfilename];
        else {
            NSStoryboard *storyboard = [NSStoryboard storyboardWithName:@"Main" bundle:nil];
            controleur = [storyboard instantiateControllerWithIdentifier:@"lecodeeditor"];
            controleur->a= [[animationSegue alloc] init];
            [vue presentViewController:controleur animator:controleur->a];
        }
        currentfilename = nil;
    }
    NSWindow* wnd=[tview window];
    [wnd setDelegate: self];
}

-(instancetype)init {
    initialization = YES;
    if (currentdelegate == nil)
        currentdelegate=self;
    controleur = nil;
    currentfilename=nil;
    runingmode=NO;
    return [super init];
}

-(void)windowDidUpdate:(NSNotification *)notification {
    if (runingmode)
        [[NSCursor operationNotAllowedCursor] set];
}

- (BOOL)windowShouldClose:(NSWindow *)sender {
    if (sender == [tview window])
        return NO;
    
    if (sender == [debugger window]) {
        [debugger clearlock];
        StopExecution();
        debugger=nil;
        return YES;
    }
    
    NSViewController* n = [sender contentViewController];
    if (n == vue || n == nil)
        return YES;
    CodeViewController* cn = (CodeViewController*)n;
    if ([[cn Code] ismodified] == YES) {
        return [tview DisplayAlert:@"This file has been modified. Quit anyway?"];
    }
    
    [allfiles removeObjectForKey:[[cn Code] Filename]];
    return YES;
}

-(void)applicationWillTerminate:(NSNotification *)aNotification {
    // Insert code here to tear down your application
    LispEFinalClean();
 }

-(void)textDidChange:(NSNotification *)notification {
    static const char cc[]={'}',']',')','"','\'',';','/','\r',0};
    
    NSWindow* wnd=[[NSApp orderedWindows] objectAtIndex:0];
    NSEvent* evenement=[wnd currentEvent];
    NSString * key = [evenement characters];
    const char c = [key characterAtIndex:0];
    if (wnd == [tview window]) {
        if (c == ')' || c == '}' || c == ']') {
            //looking for the corresponding (
            [tview matchingparenthesis: c];
        }
        return;
    }

    NSViewController* n=[wnd contentViewController];
    CodeViewController* cn=(CodeViewController*)n;
    
    if (strchr(cc,c)!= NULL)
        [[cn Code] localcolor: c];
    else
        [[cn Code] setmodified:YES];
}

-(BOOL)application:(NSApplication *)sender openFile:(NSString *)filename {
    currentfilename=filename;
    if (allfiles != nil && [allfiles objectForKey:currentfilename]) {
        CodeViewController* cvc = (CodeViewController*)[allfiles objectForKey:currentfilename];
        [[[cvc view] window] orderFrontRegardless];
        return NO;
    }


    if (initialization == NO) {
        NSStoryboard *storyboard = [NSStoryboard storyboardWithName:@"Main" bundle:nil];
        controleur = [storyboard instantiateControllerWithIdentifier:@"lecodeeditor"];
        CodeViewController* cvc = (CodeViewController*)controleur;
        if (allfiles == nil)
            allfiles = [NSMutableDictionary new];
        
        allfiles[currentfilename] = cvc;
        
        cvc->a= [[animationSegue alloc] init];
        [vue presentViewController:controleur animator:cvc->a];
        currentfilename = nil;
    }
    return YES;
}


@end
