-- -*- Mode: AppleScript -*-
property urlPrefix : "textedit://"

on open location texteditURL
	-- strip prefix	
	set the characterCount to the number of characters of the urlPrefix
	set the emacsclientURL to (characters (the characterCount + 1) thru -1 of the texteditURL) as string
	
	-- extract PATH LINE and COLUMN
	set AppleScript's text item delimiters to ":"
	set emacsclientPATH to first text item of emacsclientURL
	set emacsclientLINE to second text item of emacsclientURL
	set emacsclientCOLUMN to last text item of emacsclientURL
	set AppleScript's text item delimiters to ""
	
	-- launch emacsclient
	do shell script "emacsclient --no-wait +" & emacsclientLINE & ":" & emacsclientCOLUMN & " " & emacsclientPATH
end open location
