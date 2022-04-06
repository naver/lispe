/*
 *  LispE
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : LispE
 Version    : See lispe.cxx for the version number
 filename   : animationSegue.m
 Date       : 2017/09/01
 Purpose    : animation to switch to a different window
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import "AppDelegate.h"
#import "animationSegue.h"
#import "lispedebugger.h"

extern AppDelegate* currentdelegate;
extern lispedebugger* debugcontroller;

@import QuartzCore;

@implementation animationSegue


#define kPushAnimationDuration 0.3f

- (void)animatePresentationOfViewController:(NSViewController *)viewController fromViewController:(NSViewController *)fromViewController
{
    NSWindow *wnd = fromViewController.view.window;
    
    thewindow = [NSWindow windowWithContentViewController:viewController];
    
    NSRect oldWindowFrame = wnd.frame;
    if (viewController != debugcontroller)
        thewindow.title = currentdelegate->currentfilename;
    
    NSRect viewFrame = viewController.view.frame;
    viewFrame.size = viewController.view.fittingSize;
    
    NSArray *constraints = viewController.view.constraints;
    [viewController.view removeConstraints:constraints];
    
    NSRect windowFrame = [wnd frameRectForContentRect:viewFrame];
    windowFrame.origin = NSMakePoint(wnd.frame.origin.x + (NSWidth(oldWindowFrame) - NSWidth(windowFrame)) * 0.5, NSMaxY(wnd.frame) - NSHeight(windowFrame));
    
    viewController.view.wantsLayer = YES;
    viewController.view.layerContentsRedrawPolicy = NSViewLayerContentsRedrawOnSetNeedsDisplay;
    [viewController.view addConstraints:constraints];

    [thewindow makeKeyAndOrderFront:NSApp];

}

- (void)animateDismissalOfViewController:(NSViewController *)viewController fromViewController:(NSViewController *)fromViewController
{
    NSLog(@"ixi");
 }

@end
