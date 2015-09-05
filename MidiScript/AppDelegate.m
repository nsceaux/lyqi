#import "AppDelegate.h"
#import "MidiController.h"

@implementation AppDelegate
- (void) playNote: (int) pitch
{
  [midiController playPitch:pitch withLength:200 withVelocity:80];
}
@end
