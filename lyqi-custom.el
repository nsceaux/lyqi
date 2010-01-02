;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

;;;
;;; Customization
;;;
(defgroup lyqi nil
  "LilyPond quick insert mode."
  :prefix "lyqi:"
  :group 'applications)

(defcustom lyqi:prefered-languages '(italiano nederlands)
  "Prefered languages for note names.  The first choice is used
in new files, or when the language of an existing file cannot be
guessed."
  :group 'lyqi
  :type '(set (const :tag "Italian/French" italiano)
              (const :tag "Dutch" nederlands)
              (const :tag "German" deutsch)
              (const :tag "English" english)))

(defcustom lyqi:prefered-octave-mode 'absolute
  "Prefered octave mode, used in new files."
  :group 'lyqi
  :type '(choice (const :tag "Absolute octaves" absolute)
                 (const :tag "Relative octaves" relative)))

(defcustom lyqi:keyboard-mapping 'azerty
  "Keyboard mapping, used to associate keys to commands in quick
insert mode map."
  :group 'lyqi
  :type '(choice (const :tag "AZERTY" azerty)
                 (const :tag "QWERTY" qwerty)))

(defcustom lyqi:custom-key-map nil
  "Key/command alist, for customizing the quick insertion mode map."
  :group 'lyqi
  :type '(alist :key-type string :value-type function))

;;;
;;; Faces
;;;

(defgroup lyqi-faces nil
  "Faces for LilyPond buffers."
  :prefix "lyqi:"
  :group 'lyqi)

;; TODO: these are debug faces, define real ones.
(defface lyqi:note-face
  '((((class color) (background dark)) :foreground "blue")
    (((class color) (background light)) :foreground "blue"))
  "Face for notes."
  :group 'lyqi-faces)

(defface lyqi:rest-face
  '((((class color) (background dark)) :foreground "green")
    (((class color) (background light)) :foreground "green"))
  "Face for rests and skips."
  :group 'lyqi-faces)

(defface lyqi:duration-face
  '((((class color) (background dark)) :foreground "red")
    (((class color) (background light)) :foreground "red"))
  "Face for rests and skips."
  :group 'lyqi-faces)

(defface lyqi:verbatim-face
  '((((class color) (background dark)) :background "yellow")
    (((class color) (background light)) :background "yellow"))
  "Face for rests and skips."
  :group 'lyqi-faces)

(defface lyqi:scheme-face
  '((((class color) (background dark)) :background "violet")
    (((class color) (background light)) :background "violet"))
  "Face for scheme forms."
  :group 'lyqi-faces)

(defface lyqi:delimiter-face
  '((((class color) (background dark)) :foreground "grey")
    (((class color) (background light)) :foreground "grey"))
  "Face for delimiters."
  :group 'lyqi-faces)

(provide 'lyqi-custom)