;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lp-base)
(require 'lyqi-pitchnames)

;;;
;;; Lexer states
;;;
(defclass lyqi:lexer-toplevel-state (lp:lexer-state) ())
(defvar lyqi:*lexer-toplevel-state* (make-instance 'lyqi:lexer-toplevel-state))

(defclass lyqi:lexer-duration?-state (lp:lexer-state) ())
(defvar lyqi:*lexer-duration?-state* (make-instance 'lyqi:lexer-duration?-state))

(defclass lyqi:lexer-note-duration?-state (lp:lexer-state) ())
(defvar lyqi:*lexer-note-duration?-state* (make-instance 'lyqi:lexer-note-duration?-state))
(defclass lyqi:lexer-note-rest?-state (lp:lexer-state) ())
(defvar lyqi:*lexer-note-rest?-state* (make-instance 'lyqi:lexer-note-rest?-state))

(defclass lyqi:lexer-incomplete-chord-state (lp:lexer-state) ())
(defvar lyqi:*lexer-incomplete-chord-state* (make-instance 'lyqi:lexer-incomplete-chord-state))

(defclass lyqi:lexer-line-comment-state (lp:lexer-state) ())
(defvar lyqi:*lexer-line-comment-state* (make-instance 'lyqi:lexer-line-comment-state))

;;;
;;; LilyPond syntax (language dependent)
;;;
(defclass lyqi:lilypond-syntax (lp:syntax)
  ((pitch-data            :initarg :pitch-data)
   (pitch-regex           :initarg :pitch-regex)
   (octave-regex          :initarg :octave-regex)
   (note-regex            :initarg :note-regex)
   (rest-skip-regex       :initarg :rest-skip-regex)
   (duration-data         :initarg :duration-data)
   (duration-length-regex :initarg :duration-length-regex)
   (duration-regex        :initarg :duration-regex)))

(defun lyqi:make-lilypond-syntax (&optional language)
  (let* ((pitch-data (case language
                       ((italiano francais) lyqi:+italian-pitchnames+)
                       ((english) lyqi:+english-pitchnames+)
                       ((deutsch) lyqi:+german-pitchnames+)
                       (t lyqi:+dutch-pitchnames+)))
         (pitch-regex (format "\\(%s\\)" 
                              (lp:join "\\|" (lp:sort-string-by-length
                                                (mapcar 'car pitch-data)))))
         (octave-regex "\\('+\\|,+\\)")
         (note-regex (format "%s%s?\\([^a-zA-Z]\\|$\\)" pitch-regex octave-regex))
         (rest-skip-regex "\\(r\\|R\\|s\\|q\\|\\\\skip\\)\\([^a-zA-Z]\\|$\\)")
         (duration-data '(("4" . 2)
                          ("8" . 3)
                          ("32" . 5)
                          ("64" . 6)
                          ("128" . 7) 
                          ("16" . 4)
                          ("256" . 8)
                          ("2" . 1)
                          ("1" . 0)
                          ("\\breve" . -1)
                          ("\\longa" . -2)
                          ("\\maxima" . -3)))
         (duration-length-regex
          (format "\\(%s\\)"
                  (lp:join "\\|" (mapcar 'regexp-quote
                                           (lp:sort-string-by-length
                                            (mapcar 'car duration-data))))))
         (duration-regex (format "%s\\.*\\(\\*[0-9]+\\(/[0-9]+\\)?\\)?"
                                 duration-length-regex)))
    (make-instance 'lyqi:lilypond-syntax
                   :default-lexer-state    lyqi:*lexer-toplevel-state*
                   :pitch-data             pitch-data
                   :pitch-regex            pitch-regex
                   :octave-regex           octave-regex
                   :note-regex             note-regex
                   :rest-skip-regex        rest-skip-regex
                   :duration-data          duration-data
                   :duration-length-regex  duration-length-regex
                   :duration-regex         duration-regex)))

;;;
;;; Lexemes
;;;

(defclass lyqi:verbatim-lexeme (lp:lexeme) ())

(defclass lyqi:note-lexeme (lp:lexeme)
  ((pitch :initarg :pitch)
   (alteration :initarg :alteration
               :initform 0)
   (octave-modifier :initarg :octave-modifier
                    :initform 0)))

(defclass lyqi:rest-skip-etc-lexeme (lp:lexeme) ())
(defclass lyqi:rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:mm-rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:space-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:skip-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:chord-repetition-lexeme (lyqi:rest-skip-etc-lexeme) ())

(defclass lyqi:chord-start-lexeme (lp:lexeme) ())
(defclass lyqi:chord-end-lexeme (lp:lexeme) ())

(defclass lyqi:base-duration-lexeme (lp:lexeme) ())
(defclass lyqi:duration-lexeme (lyqi:base-duration-lexeme)
  ((length      :initarg :length
                :initform nil)
   (dot-count   :initarg :dot-count
                :initform 0)
   (numerator   :initarg :numerator
                :initform 1)
   (denominator :initarg :denominator
                :initform 1)))
(defclass lyqi:no-duration-lexeme (lyqi:base-duration-lexeme) ())

(defclass lyqi:line-comment-start-lexeme (lp:comment-delimiter-lexeme) ())
(defclass lyqi:line-comment-lexeme (lp:comment-lexeme) ())

;;;
;;; forms
;;;
(defclass lyqi:verbatim-form (lp:form) ())
(defclass lyqi:music-form (lp:form)
  ((duration :initarg :duration
             :initform nil
             :accessor lyqi:duration-of)))
(defclass lyqi:simple-note-form (lyqi:music-form)
  ((rest :initarg :rest
         :initform nil)))
(defclass lyqi:rest-skip-etc-form (lyqi:music-form) ())
(defclass lyqi:chord-form (lyqi:music-form) ())
(defclass lyqi:chord-end-form (lyqi:music-form) ())
(defclass lyqi:incomplete-chord-form (lp:form) ())

(defclass lyqi:line-comment-form (lp:form) ())
(defclass lyqi:multi-line-comment-form (lp:form) ())

;;;
;;; Lex functions
;;;
(defmethod lp:lex ((lexer lyqi:lexer-toplevel-state) syntax parse-data)
  "Lexing in {toplevel} state"
  (labels ((reduce-lexemes
            () ; indentation is broken
            (let ((non-reduced-lexemes (and parse-data (slot-value parse-data 'lexemes))))
              (when non-reduced-lexemes
                (let* ((last-lexeme (first non-reduced-lexemes))
                       (verbatim-lexemes (nreverse non-reduced-lexemes))
                       (marker (lp:marker (first verbatim-lexemes)))
                       (size (- (+ (lp:marker last-lexeme)
                                   (lp:size last-lexeme))
                                marker)))
                  (make-instance 'lyqi:verbatim-form
                                 :marker marker
                                 :size size
                                 :children verbatim-lexemes))))))
    (lyqi:skip-whitespace)
    (cond ((eolp)
           ;; at end of line, reduce remaining lexemes
           (values lyqi:*lexer-toplevel-state* (reduce-lexemes) nil nil))
          ;; a note
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the note and add the lexeme to the output parse data
          ;; - switch to {note-duration?} lexer state
          ((looking-at (slot-value syntax 'note-regex))
           (values lyqi:*lexer-note-duration?-state*
                   (reduce-lexemes)
                   (make-instance 'lp:parse-data
                                  :form-class 'lyqi:simple-note-form
                                  :lexemes (list (lyqi:lex-note syntax))
                                  :next-lexer-state lyqi:*lexer-toplevel-state*)
                   t))
          ;; rest, mm-rest, skip or spacer
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the rest/skip/etc and add the lexeme to the output parse data
          ;; - switch to {duration?} lexer state
          ((looking-at (slot-value syntax 'rest-skip-regex))
           (values lyqi:*lexer-duration?-state*
                   (reduce-lexemes)
                   (make-instance 'lp:parse-data
                                  :form-class 'lyqi:rest-skip-etc-form
                                  :lexemes (list (lyqi:lex-rest-skip-etc syntax))
                                  :next-lexer-state lyqi:*lexer-toplevel-state*)
                   t))
          ;; a chord start: '<'
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the chord start and add the lexeme to the output parse data
          ;; - switch to {incomplete-chord} state
          ((looking-at "<\\([^<]\\|$\\)")
           (let ((marker (point-marker)))
             (forward-char 1)
             (values lyqi:*lexer-incomplete-chord-state*
                     (reduce-lexemes)
                     (make-instance 'lp:parse-data
                                    :form-class 'lyqi:chord-form
                                    :lexemes (list (make-instance
                                                    'lyqi:chord-start-lexeme
                                                    :marker marker
                                                    :size (- (point) marker)))
                                    :next-lexer-state lyqi:*lexer-toplevel-state*)
                     t)))
          ;; TODO: multi line comment

          ;; line comment
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the line-comment start and add the lexeme to the output parse data
          ;; - switch to {line-comment} state
          ((looking-at "%+")
           (let ((marker (point-marker)))
             (lp:forward-match)
             (values lyqi:*lexer-line-comment-state*
                     (reduce-lexemes)
                     (make-instance 'lp:parse-data
                                    :form-class 'lyqi:line-comment-form
                                    :lexemes (list (make-instance
                                                    'lyqi:line-comment-start-lexeme
                                                    :marker marker
                                                    :size (- (point) marker)))
                                    :next-lexer-state lyqi:*lexer-toplevel-state*)
                     t)))
          ;; TODO: a backslashed keyword, command or variable
          ;; ((looking-at "\\\\[a-zA-Z]+")

          ;; other top level expressions are treated as verbatim
          ;; - lex the verbatim word and add the lexeme to the output parse data
          ;; - continue lexing in {toplevel} state
          (t
           (let* ((lexeme (lyqi:lex-verbatim syntax))
                  (parse-data (if parse-data
                                  (progn
                                    (lp:push-lexeme parse-data lexeme)
                                    parse-data)
                                  (make-instance 'lp:parse-data
                                                 :lexemes (list lexeme)
                                                 :form-class 'lyqi:verbatim-form
                                                 :next-lexer-state lyqi:*lexer-toplevel-state*))))
             (values lyqi:*lexer-toplevel-state* nil parse-data t))))))

(defmethod lp:lex ((lexer lyqi:lexer-duration?-state) syntax parse-data)
  "Lexing in duration?-state:
duration | no-duration -> toplevel-state"
  (let* ((duration-lexeme (lyqi:lex-duration syntax))
         (all-lexemes (nreverse (cons duration-lexeme
                                      (slot-value parse-data 'lexemes))))
         (first-lexeme (first all-lexemes))
         (marker (lp:marker first-lexeme))
         (size (- (+ (lp:marker duration-lexeme)
                     (lp:size duration-lexeme))
                  marker)))
    (lyqi:skip-whitespace)
    ;; something + duration
    ;; ==> reduce previous lexeme and duration
    (values (slot-value parse-data 'next-lexer-state)
            (make-instance (slot-value parse-data 'form-class)
                           :marker marker
                           :size size
                           :children all-lexemes
                           :duration duration-lexeme)
            nil
            (not (eolp)))))

(defmethod lp:lex ((lexer lyqi:lexer-note-duration?-state) syntax parse-data)
  ;; note + duration
  ;; switch to {note-rest?} state
  (lp:push-lexeme parse-data (lyqi:lex-duration syntax))
  (values lyqi:*lexer-note-rest?-state*
          nil
          parse-data
          t))

(defmethod lp:lex ((lexer lyqi:lexer-note-rest?-state) syntax parse-data)
  ;; note + duration + \\rest
  ;; or note + duration
  ;; ==> reduce
  (lyqi:skip-whitespace)
  (let* ((non-reduced-lexemes (slot-value parse-data 'lexemes))
         (duration (first non-reduced-lexemes))
         (marker (point-marker))
         (rest-lexeme (when (looking-at "\\\\rest")
                        (lp:forward-match)
                        (make-instance 'lyqi:verbatim-lexeme
                                       :marker marker
                                       :size (- (point) marker))))
         (all-lexemes (nreverse (if rest-lexeme
                                    (cons rest-lexeme non-reduced-lexemes)
                                    non-reduced-lexemes)))
         (last-lexeme (or rest-lexeme duration))
         (first-lexeme (first all-lexemes))
         (first-marker (lp:marker first-lexeme))
         (size (- (+ (lp:marker last-lexeme)
                     (lp:size last-lexeme))
                  first-marker)))
    (lyqi:skip-whitespace)
    (values (slot-value parse-data 'next-lexer-state)
            (make-instance (slot-value parse-data 'form-class)
                           :marker first-marker
                           :size size
                           :children all-lexemes
                           :duration duration
                           :rest (not (not rest-lexeme)))
            nil
            (not (eolp)))))

(defmethod lp:lex ((lexer lyqi:lexer-line-comment-state) syntax parse-data)
  ;; %+ + anything until EOL ==> reduce to a line comment form
  (lyqi:skip-whitespace)
  (let* ((marker (point-marker))
         (start-lexeme (first (slot-value parse-data 'lexemes)))
         (comment-lexeme (and (not (eolp))
                              (progn
                                (end-of-line)
                                (make-instance 'lyqi:line-comment-lexeme
                                               :marker marker
                                               :size (- (point) marker))))))
    (values (slot-value parse-data 'next-lexer-state)
            (make-instance (slot-value parse-data 'form-class)
                           :marker (lp:marker start-lexeme)
                           :size (if comment-lexeme
                                     (- (point) (lp:marker start-lexeme))
                                     (lp:size start-lexeme))
                           :children (if comment-lexeme
                                         (list start-lexeme comment-lexeme)
                                         (list start-lexeme)))
            nil
            nil)))

(defmethod lp:lex ((lexer lyqi:lexer-incomplete-chord-state) syntax parse-data)
  "Lexing in incomplete-chord-state:
 '>' -> duration?-state
 note | other tokens -> incomplete-chord-state"
  (let ((parse-data (or parse-data
                        (make-instance 'lp:parse-data
                                       :next-lexer-state lyqi:*lexer-toplevel-state*
                                       :form-class 'lyqi:chord-end-form))))
    (lyqi:skip-whitespace)
    (cond ((eolp)
           ;; at end of line, reduce remaining lexemes
           (let* ((children (nreverse (slot-value parse-data 'lexemes)))
                  (marker (lp:marker (first children))))
             (values lyqi:*lexer-incomplete-chord-state*
                     (make-instance 'lyqi:incomplete-chord-form
                                    :marker marker
                                    :size (- (point) marker)
                                    :children children)
                     nil
                     nil)))
          ;; a note
          ((looking-at (slot-value syntax 'note-regex))
           (lp:push-lexeme parse-data (lyqi:lex-note syntax))
           (values lyqi:*lexer-incomplete-chord-state*
                   nil
                   parse-data
                   t))
          ;; a chord end: '>'
          ;; switch to {duration?} state
          ((eql (char-after) ?\>)
           (let ((marker (point-marker)))
             (forward-char 1)
             (lp:push-lexeme parse-data
                             (make-instance 'lyqi:chord-end-lexeme
                                            :marker marker
                                            :size (- (point) marker)))
             (values lyqi:*lexer-duration?-state*
                     nil
                     parse-data
                     t)))
          ;; something else
          (t
           (lp:push-lexeme parse-data
                           (lyqi:lex-verbatim syntax "[^ \t\r\n>]+"))
           (values lyqi:*lexer-incomplete-chord-state*
                 nil
                 parse-data
                 t)))))

;;;
;;; specific lexing functions
;;;

(defun lyqi:skip-whitespace ()
  "Skip white space (except new lines)."
  (when (looking-at "[ 	]+")
    (lp:forward-match)))

(defun lyqi:lex-verbatim (syntax &optional verbatim-regex)
  (let ((marker (point-marker)))
    (looking-at (or verbatim-regex "\\S-+"))
    (lp:forward-match)
    (make-instance 'lyqi:verbatim-lexeme
                   :marker marker
                   :size (- (point) marker))))

(defun lyqi:lex-note (syntax)
  (let ((pitch 0)
        (alteration 0)
        (octave-modifier 0)
        (marker (point-marker)))
    (when (looking-at (slot-value syntax 'pitch-regex))
      ;; pitch and alteration
      (let ((pitch-data (assoc (match-string-no-properties 0)
                               (slot-value syntax 'pitch-data))))
        (setf pitch (second pitch-data))
        (setf alteration (third pitch-data)))
      (lp:forward-match)
      ;; octave
      (when (looking-at (slot-value syntax 'octave-regex))
        (setf octave-modifier (* (if (eql (char-after) ?\,) -1 1)
                                 (- (match-end 0) (match-beginning 0))))
        (lp:forward-match)))
    (make-instance 'lyqi:note-lexeme
                   :pitch pitch
                   :alteration alteration
                   :octave-modifier octave-modifier
                   :marker marker
                   :size (- (point) marker))))

(defun lyqi:lex-rest-skip-etc (syntax)
  (let* ((marker (point-marker))
         (size 1))
    (make-instance (cond ((looking-at "r")
                          (forward-char 1)
                          'lyqi:rest-lexeme)
                         ((looking-at "R")
                          (forward-char 1)
                          'lyqi:mm-rest-lexeme)
                         ((looking-at "s")
                          (forward-char 1)
                          'lyqi:space-lexeme)
                         ((looking-at "q")
                          (forward-char 1)
                          'lyqi:chord-repetition-lexeme)
                         ((looking-at "\\\\skip")
                          (lp:forward-match)
                          (setf size (- (point) marker))
                          (lyqi:skip-whitespace)
                          'lyqi:skip-lexeme))
                   :marker marker :size size)))

(defun lyqi:lex-duration (syntax)
  (if (or (eolp)
          (not (looking-at (slot-value syntax 'duration-regex))))
      ;; implicit duration
      (make-instance 'lyqi:no-duration-lexeme
                     :marker (point-marker)
                     :size 0)
      ;; explicit duration
      (let ((length 2)
            (dot-count 0)
            (num 1)
            (den 1)
            (marker (point-marker)))
        (when (looking-at (slot-value syntax 'duration-length-regex))
          ;; length
          (setf length (cdr (assoc (match-string-no-properties 0)
                                   (slot-value syntax 'duration-data))))
          (lp:forward-match)
          ;; dots
          (when (and (not (eolp))
                     (looking-at "\\.+"))
            (setf dot-count (- (match-end 0) (match-beginning 0)))
            (lp:forward-match))
          ;; numerator
          (when (and (not (eolp))
                     (looking-at "\\*\\([0-9]+\\)"))
            (setf num (string-to-number (match-string-no-properties 1)))
            (lp:forward-match)
            ;; denominator
            (when (and (not (eolp))
                       (looking-at "/\\([0-9]+\\)"))
              (setf den (string-to-number (match-string-no-properties 1)))
              (lp:forward-match))))
        (make-instance 'lyqi:duration-lexeme
                       :length length
                       :dot-count dot-count
                       :numerator num
                       :denominator den
                       :marker marker
                       :size (- (point) marker)))))

(provide 'lyqi-syntax)