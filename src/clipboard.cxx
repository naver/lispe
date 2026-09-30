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

        // Convert std::string to NSString (nil if text is not valid UTF-8)
        NSString *nsString = [NSString stringWithUTF8String:text.c_str()];
        if (nsString == nil)
            return false;

        // Clear the pasteboard and write to it
        [pasteboard clearContents];

        return [pasteboard setString:nsString forType:NSPasteboardTypeString];
    }
}
#else
bool copyToClipboard(const std::string& text) {
    //No system clipboard available on this platform
    return false;
}
#endif
