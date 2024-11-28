/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  composing.cxx
//
//

#include <iostream>
#include <string>

#ifdef APPLE
#import <Cocoa/Cocoa.h>

bool copyToClipboard(const std::string& text) {
    @autoreleasepool {
        // Get the pasteboard (clipboard)
        NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
        
        // Clear the pasteboard
        [pasteboard clearContents];
        
        // Convert std::string to NSString
        NSString *nsString = [NSString stringWithUTF8String:text.c_str()];
        
        // Write to pasteboard
        NSArray *types = @[NSPasteboardTypeString];
        [pasteboard declareTypes:types owner:nil];
        
        return [pasteboard setString:nsString forType:NSPasteboardTypeString];
    }
}
#else
bool copyToClipboard(const std::string& text) {
    return true;
}
#endif
