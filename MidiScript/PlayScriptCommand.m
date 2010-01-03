//
//  PlayScriptCommand.m
//  MidiScript
//
//  Created by Nicolas Sceaux on 10/09/05.
//  Copyright 2005 __MyCompanyName__. All rights reserved.
//

#import "PlayScriptCommand.h"
#import "AppDelegate.h"

@implementation PlayScriptCommand

- (id) performDefaultImplementation
{
  int pitch = [[self directParameter] intValue];
  printf ("play pitch: %d\n", pitch);
  [[NSApp delegate] playNote:pitch];
  return nil;
}

@end
