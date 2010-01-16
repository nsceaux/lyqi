
EmacsPointAndClick.app:
	echo "tell application \"AppleScript Editor\"\n\
	open \"$(PWD)/EmacsPointAndClick.scpt\"\n\
	save document \"EmacsPointAndClick.scpt\" as \"application\" in \"$(PWD)/EmacsPointAndClick.app\" with stay open\n\
	close document \"EmacsPointAndClick\"\n\
	end tell" | /usr/bin/osascript
	echo 'APPLepac' > EmacsPointAndClick.app/Contents/PkgInfo
	perl -i -pe "s|<string>aplt</string>|<string>epac</string>|" EmacsPointAndClick.app/Contents/Info.plist
	perl -i -pe "s|^</dict>|\
		<key>CFBundleIdentifier</key>\n\
		<string>EmacsPointAndClick</string>\n\
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
		<key>NSUIElement</key>\n\
		<true/>\n\
</dict>|" EmacsPointAndClick.app/Contents/Info.plist

all:
	rm -rf EmacsPointAndClick.app
	make EmacsPointAndClick.app
.PHONY: all
