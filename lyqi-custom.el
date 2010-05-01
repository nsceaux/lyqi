;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customization
;;;

(defgroup lyqi nil
  "LilyPond quick insert mode."
  :prefix "lyqi:"
  :group 'applications)

(defcustom lyqi:prefered-languages '(nederlands italiano)
  "Prefered languages for note names.  The first choice is used
in new files, or when the language of an existing file cannot be
guessed."
  :group 'lyqi
  :type '(set (const :tag "Italian/French" italiano)
              (const :tag "Dutch" nederlands)
              (const :tag "German" deutsch)
              (const :tag "English" english)))

(defcustom lyqi:keyboard-mapping 'qwerty
  "Keyboard mapping, used to associate keys to commands in quick
insert mode map."
  :group 'lyqi
  :type '(choice (const :tag "AZERTY" azerty)
                 (const :tag "QWERTY" qwerty)))

(defcustom lyqi:custom-key-map nil
  "Key/command alist, for customizing the quick insertion mode map.

The keys of the alist are eiter strings or vector meaning a sequence of
keystrokes, e.g:
   \"a\"   for key a
   \"\\C-ca\" for C-c a
   [enter] for RET

Values can be either:
- an existing command;
- a string, in which case a command which inserts the given string
will be implicitely created;
- a cons, which car is space-around and cdr is the string to be
inserted.

For example, if `lyqi:custom-key-map' is set to:

  '((\"w\" \"\\prall\")
    (\"x\" '(space-around . \"\\appoggiatura\")))

then when pressing the key \"w\" in quick insert mode, \\prall
is inserted, and when pressing \"x\", \\appoggiatura is inserted with
space around it (unless at the beginning or end of a line).

This variable is normally read when `lyqi-mode' is loaded.  To
force the redefinition of `lyqi:quick-insert-mode-map', invoke
command `lyqi:force-mode-map-definition'.
"
  :group 'lyqi
  :type '(alist :key-type (choice string vector)
                :value-type (choice function string alist)))

(defcustom lyqi:projects-language nil
  "Specify which note language use for projects in given directories.
For instance, if `lyqi:projects-language' is set to:

  '((\"~/src/lilypond\" nederlands)
    (\"~/Documents/MyProjects\" italiano)
    (\"~/Documents/MyProjects/MyOpera\" nederlands))

then nederlands language will be used when opening a file in \"~/src/lilypond/...\"
or \"~/Documents/MyProjects/MyOpera/...\", and italiano language will be used for
files located in other places in \"~/Documents/MyProjects\"."
  :group 'lyqi
  :type '(alist :key-type string
                :value-type (group (choice (const :tag "Italian/French" italiano)
                                           (const :tag "Dutch" nederlands)
                                           (const :tag "German" deutsch)
                                           (const :tag "English" english)))))

(defcustom lyqi:midi-backend nil
  "Midi backend to use to play notes when entering music in quick insert mode: 'osx or 'alsa"
  :group 'lyqi
  :type '(choice (const :tag "Linux/ALSA backend (lyqikbd)" alsa)
                 (const :tag "Mac OS X backend (MidiScript)" osx)
                 (const :tag "None" nil)))

(defcustom lyqi:lilypond-command "lilypond"
  "Command used to compile .ly files"
  :group 'lyqi
  :type 'string)

(defcustom lyqi:pdf-command "xpdf"
  "Command used to open .pdf files"
  :group 'lyqi
  :type 'string)

(defcustom lyqi:midi-command "timidity"
  "Command used to open .midi files"
  :group 'lyqi
  :type 'string)

(provide 'lyqi-custom)