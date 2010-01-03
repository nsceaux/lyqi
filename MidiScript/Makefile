
PROJECT = MidiScript

SOURCES = AppDelegate.m MidiController.m PlayScriptCommand.m main.m
HEADERS = AppDelegate.h MidiController.h PlayScriptCommand.h BUNDLE = MidiScript.app
OBJECTS = $(SOURCES:%.m=out/%.o)

CONTENTS = Info.plist PkgInfo
PROJECT_BIN = out/$(PROJECT)
RESOURCES = $(PROJECT).sdef
LPROJ_RESOURCES = InfoPlist.strings MainMenu.nib

BUNDLE = $(PROJECT).app
BUNDLE_CONTENTS = $(BUNDLE)/Contents
BUNDLE_BIN_DIR = $(BUNDLE_CONTENTS)/MacOs
BUNDLE_RESOURCES = $(BUNDLE_CONTENTS)/Resources
BUNDLE_LPROJ = $(BUNDLE_RESOURCES)/English.lproj

OBJC_FLAGS=-x objective-c -arch i386 -fmessage-length=0 -pipe -Wno-trigraphs -fpascal-strings -fasm-blocks -Os -mdynamic-no-pic -Wreturn-type -Wunused-variable -fvisibility=hidden
LD_FLAGS=-arch i386 -framework Cocoa -framework AudioToolbox -framework AudioUnit

out/%.o : %.m
	@mkdir -p out
	LANG=en_US.US-ASCII gcc $(OBJC_FLAGS) -c $< -o $@

$(PROJECT_BIN): $(OBJECTS)
	gcc $(LD_FLAGS) $(OBJECTS) -o $@

$(BUNDLE): $(PROJECT_BIN)
	mkdir -p $(BUNDLE_BIN_DIR) $(BUNDLE_LPROJ)
	cp -f $(CONTENTS) $(BUNDLE_CONTENTS)
	cp -f $(PROJECT_BIN) $(BUNDLE_BIN_DIR)
	cp -f $(RESOURCES) $(BUNDLE_RESOURCES)
	cp -f $(LPROJ_RESOURCES) $(BUNDLE_LPROJ)

clean:
	rm -rf $(BUNDLE) out

all: $(BUNDLE)

.PHONY: clean all