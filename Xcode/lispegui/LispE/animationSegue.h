/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : animationSegue.h
 Date       : 2017/09/01
 Purpose    : Displaying the content of a file 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>

@interface animationSegue : NSObject <NSViewControllerPresentationAnimator> {
    NSWindow* thewindow;
}

@end
