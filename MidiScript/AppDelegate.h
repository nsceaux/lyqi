/* AppDelegate */

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject
{
    IBOutlet id midiController;
}
- (void) playNote: (int) pitch;
@end
