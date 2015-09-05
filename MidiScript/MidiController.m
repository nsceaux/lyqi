
#import <pthread.h> // used for usleep...

#import "MidiController.h"

@implementation MidiController

- (id)init {
  
  // some MIDI constants:
  enum {
    kMidiMessage_ControlChange  = 0xB,
    kMidiMessage_ProgramChange  = 0xC,
    kMidiMessage_BankMSBControl = 0,
    kMidiMessage_BankLSBControl = 32
  };

  // Create the graph
  NewAUGraph (&graph_);
  AudioComponentDescription cd;
  // Open the DLS synth
  cd.componentType         = kAudioUnitType_MusicDevice;
  cd.componentSubType      = kAudioUnitSubType_DLSSynth;
  cd.componentManufacturer = kAudioUnitManufacturer_Apple;
  cd.componentFlags        = 0;
  cd.componentFlagsMask    = 0;
  AUGraphAddNode (graph_, &cd, &synthNode_);
  // Set the midi channel
  midiChannel_ = 0;
  // Open the filter
  cd.componentType         = kAudioUnitType_Effect;
  cd.componentSubType      = kAudioUnitSubType_LowPassFilter;
  cd.componentManufacturer = kAudioUnitManufacturer_Apple;
  cd.componentFlags        = 0;
  cd.componentFlagsMask    = 0;
  AUGraphAddNode (graph_, &cd, &filterNode_);
  // Open the output device
  cd.componentType         = kAudioUnitType_Output;
  cd.componentSubType      = kAudioUnitSubType_DefaultOutput;
  cd.componentManufacturer = kAudioUnitManufacturer_Apple;
  cd.componentFlags        = 0;
  cd.componentFlagsMask    = 0;
  AUGraphAddNode (graph_, &cd, &outputNode_);
  // Connect the devices up
  AUGraphConnectNodeInput (graph_, synthNode_, 1, filterNode_, 0);
  AUGraphConnectNodeInput (graph_, filterNode_, 0, outputNode_, 0);
  AUGraphUpdate (graph_, NULL);
  // Open and initialize the audio units
  AUGraphOpen (graph_);
  AUGraphInitialize (graph_);
  // Turn off the reverb on the synth
  AUGraphNodeInfo (graph_, synthNode_, NULL, &synthUnit);
  UInt32 usesReverb = 0;
  AudioUnitSetProperty (synthUnit,
                        kMusicDeviceProperty_UsesInternalReverb, kAudioUnitScope_Global,
                        0,
                        &usesReverb, sizeof (usesReverb)
                        );

  // Set filter cut off
  AudioUnit filterUnit;
  AUGraphNodeInfo (graph_, filterNode_, NULL, &filterUnit);
  AudioUnitSetParameter (filterUnit, 0, kAudioUnitScope_Global, 0, 12000.0, 0);

  AUGraphNodeInfo (graph_, synthNode_, NULL, &synthUnit);
  AUGraphStart (graph_);
  return self;
}

- (void)playPitch:(int)pitch withLength:(int)length withVelocity:(int)velocity
{
  // some MIDI constants:
  enum {
    kMidiMessage_NoteOn         = 0x9,
    kMidiMessage_NoteOff        = 0x8
  };

  UInt32 noteOnCommand = kMidiMessage_NoteOn << 4 | midiChannel_;
  UInt32 noteOffCommand = kMidiMessage_NoteOff << 4 | midiChannel_;
  MusicDeviceMIDIEvent(synthUnit, noteOnCommand, pitch, velocity, 0);
  usleep (length * 1000);
  MusicDeviceMIDIEvent(synthUnit, noteOffCommand, pitch, 0, 0);
}

- (void)dealloc {
  AUGraphStop (graph_);
  DisposeAUGraph (graph_);
}
@end
