
build_script:
	@echo 'tell application "AppleScript Editor"' > $@
	@echo '  open "$(PWD)/EmacsPointAndClick.scpt"' >> $@
	@echo '  save document "EmacsPointAndClick.scpt" as "application" in "$(PWD)/EmacsPointAndClick.app" with stay open' >> $@
	@echo '  close document "EmacsPointAndClick"' >> $@
	@echo 'end tell' >> $@

EmacsPointAndClick.app: build_script
	/usr/bin/osascript $<
	@perl -i -pe "s|^</dict>|\
		<key>CFBundleURLTypes</key>\n\
		<array>\n\
			<dict>\n\
				<key>CFBundleURLName</key>\n\
				<string>Open textedit:// locations in Emacs</string>\n\
				<key>CFBundleURLSchemes</key>\n\
				<array>\n\
					<string>textedit</string>\n\
				</array>\n\
			</dict>\n\
		</array>\n\
</dict>|" EmacsPointAndClick.app/Contents/Info.plist

all:
	rm -rf EmacsPointAndClick.app
	make EmacsPointAndClick.app
.PHONY: all
