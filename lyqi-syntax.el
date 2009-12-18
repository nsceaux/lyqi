;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2003-2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

;;; this lexer only looks for notes, chords, etc, and treat other
;;; things as verbatim.

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lyqi-pitchnames)

;;;
;;; regex and match utilities
;;;

;; for XEmacs21 compatibility
(if (not (fboundp 'match-string-no-properties))
    (defalias 'match-string-no-properties 'match-string))

(defun lyqi:join (join-string strings)
  "Returns a concatenation of all strings elements, with join-string between elements"
  (apply 'concat 
	 (car strings) 
	 (mapcar (lambda (str) (concat join-string str))
		 (cdr strings))))

(defun lyqi:sort-string-by-length (string-list)
  "Sort the given string list by decreasing string length."
  (nreverse 
   (sort string-list
	 (lambda (str1 str2)
	   (or (< (length str1) (length str2))
	       (and (= (length str1) (length str2))
		    (string< str1 str2)))))))

(defun lyqi:forward-match ()
  (forward-char (- (match-end 0) (match-beginning 0))))

;;;
;;; LilyPond syntax (language dependent)
;;;
(defclass lyqi:lilypond-syntax ()
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
                              (lyqi:join "\\|" (lyqi:sort-string-by-length
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
                  (lyqi:join "\\|" (mapcar 'regexp-quote
                                           (lyqi:sort-string-by-length
                                            (mapcar 'car duration-data))))))
         (duration-regex (format "%s\\.*\\(\\*[0-9]+\\(/[0-9]+\\)?\\)?"
                                 duration-length-regex)))
    (make-instance 'lyqi:lilypond-syntax
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

(defclass lyqi:parser-symbol ()
  ((start-point :initform nil
                :initarg :start-point)
   (end-point :initform nil
              :initarg :end-point)
   (children :initarg :children
             :initform nil
             :accessor lyqi:children-of)))

(defclass lyqi:lexeme (lyqi:parser-symbol) ())

(defclass lyqi:verbatim-lexeme (lyqi:lexeme) ())

(defclass lyqi:note-lexeme (lyqi:lexeme)
  ((pitch :initarg :pitch)
   (alteration :initarg :alteration
               :initform 0)
   (octave-modifier :initarg :octave-modifier
                    :initform 0)))

(defclass lyqi:rest-skip-etc-lexeme (lyqi:lexeme) ())
(defclass lyqi:rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:mm-rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:space-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:skip-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:chord-repetition-lexeme (lyqi:rest-skip-etc-lexeme) ())

(defclass lyqi:chord-start-lexeme (lyqi:lexeme) ())
(defclass lyqi:chord-end-lexeme (lyqi:lexeme) ())

(defclass lyqi:base-duration-lexeme (lyqi:lexeme) ())
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

;;;
;;; Lexer
;;;

(defclass lyqi:lexer-state () ())
(defclass lyqi:lexer-toplevel-state (lyqi:lexer-state) ())
(defvar lyqi:*lexer-toplevel-state* (make-instance 'lyqi:lexer-toplevel-state))
(defclass lyqi:lexer-duration?-state (lyqi:lexer-state) ())
(defvar lyqi:*lexer-duration?-state* (make-instance 'lyqi:lexer-duration?-state))
(defclass lyqi:lexer-incomplete-chord-state (lyqi:lexer-state) ())
(defvar lyqi:*lexer-incomplete-chord-state* (make-instance 'lyqi:lexer-incomplete-chord-state))

(defgeneric lyqi:lex (lexer-state syntax)
  "If a lexeme can be found after point, return two values: a
lexeme and the new lexer state. Advance point at the end of the
returned lexeme.  Otherwise, return NIL.")

(defun lyqi:lex-line (syntax)
  (loop for current-state = lyqi:*lexer-toplevel-state* then new-state
        for (lexeme new-state) = (lyqi:lex current-state syntax)
        while lexeme
        collect lexeme into result
        finally return result))

(defmethod lyqi:lex ((lexer lyqi:lexer-toplevel-state) syntax)
  "Lexing in toplevel-state:
 note | rest | mm-rest | skip | space -> duration?-state
 '<' -> incomplete-chord-state
 other tokens -> toplevel-state"
  (lyqi:skip-whitespace)
  (unless (eolp)
    (cond (;; a note
           (looking-at (slot-value syntax 'note-regex))
           (values (lyqi:lex-note syntax) lyqi:*lexer-duration?-state*))
          ;; rest, mm-rest, skip or spacer
          ((looking-at (slot-value syntax 'rest-skip-regex))
           (values (lyqi:lex-rest-skip-etc syntax) lyqi:*lexer-duration?-state*))
          ;; a chord start: '<'
          ((looking-at "<\\([^<]\\|$\\)")
           (let ((start (point)))
             (forward-char 1)
             (values (make-instance 'lyqi:chord-start-lexeme
                                    :start-point start
                                    :end-point (point))
                     lyqi:*lexer-incomplete-chord-state*)))
          ;; other top level expression are treated as verbatim
          (t (values (lyqi:lex-verbatim syntax)
                     lyqi:*lexer-toplevel-state*)))))

(defmethod lyqi:lex ((lexer lyqi:lexer-duration?-state) syntax)
  "Lexing in duration?-state:
duration | no-duration -> toplevel-state"
  (values (if (and (not (eolp))
                   (looking-at (slot-value syntax 'duration-regex)))
              (lyqi:lex-duration syntax)
              (make-instance 'lyqi:no-duration-lexeme
                             :start-point (point)
                             :end-point (point)))
          lyqi:*lexer-toplevel-state*))


(defmethod lyqi:lex ((lexer lyqi:lexer-incomplete-chord-state) syntax)
  "Lexing in incomplete-chord-state:
 '>' -> duration?-state
 note | other tokens -> incomplete-chord-state"
  (lyqi:skip-whitespace)
  (unless (eolp)
    (cond ((looking-at (slot-value syntax 'note-regex)) ;; a note
           (values (lyqi:lex-note syntax) lyqi:*lexer-incomplete-chord-state*))
          ((eql (char-after) ?\>) ;; a chord end: '>'
           (let ((start (point)))
             (forward-char 1)
             (values (make-instance 'lyqi:chord-end-lexeme
                                   :start-point start
                                   :end-point (point))
                    lyqi:*lexer-duration?-state*)))
          (t ;; something else
           (values (lyqi:lex-verbatim syntax)
                   lyqi:lexer-incomplete-chord-state)))))

;;; specific lexing functions

(defun lyqi:skip-whitespace ()
  (when (looking-at "\\s-+")
    (lyqi:forward-match)))

(defun lyqi:lex-verbatim (syntax)
  (let ((start (point)))
    (looking-at "\\S-+")
    (lyqi:forward-match)
    (make-instance 'lyqi:verbatim-lexeme
                           :start-point start
                           :end-point (point))))

(defun lyqi:lex-note (syntax)
  (let ((pitch 0)
        (alteration 0)
        (octave-modifier 0)
        (start (point)))
    (when (looking-at (slot-value syntax 'pitch-regex))
      ;; pitch and alteration
      (let ((pitch-data (assoc (match-string-no-properties 0)
                               (slot-value syntax 'pitch-data))))
        (setf pitch (second pitch-data))
        (setf alteration (third pitch-data)))
      (lyqi:forward-match)
      ;; octave
      (when (looking-at (slot-value syntax 'octave-regex))
        (setf octave-modifier (* (if (eql (char-after) ?\,) -1 1)
                                 (- (match-end 0) (match-beginning 0))))
        (lyqi:forward-match)))
    (make-instance 'lyqi:note-lexeme
                   :pitch pitch
                   :alteration alteration
                   :octave-modifier octave-modifier
                   :start-point start
                   :end-point (point))))

(defun lyqi:lex-rest-skip-etc (syntax)
  (let* ((start (point))
         (end (1+ start)))
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
                          (lyqi:forward-match)
                          (setf end (point))
                          (lyqi:skip-whitespace)
                          'lyqi:skip-lexeme))
                   :start-point start :end-point end)))

(defun lyqi:lex-duration (syntax)
  (let ((length 2)
        (dot-count 0)
        (num 1)
        (den 1)
        (start (point)))
    (when (looking-at (slot-value syntax 'duration-length-regex))
      ;; length
      (setf length (cdr (assoc (match-string-no-properties 0)
                                  (slot-value syntax 'duration-data))))
      (lyqi:forward-match)
      ;; dots
      (when (and (not (eolp))
                 (looking-at "\\.+"))
        (setf dot-count (- (match-end 0) (match-beginning 0)))
        (lyqi:forward-match))
      ;; numerator
      (when (and (not (eolp))
                 (looking-at "\\*\\([0-9]+\\)"))
        (setf num (string-to-number (match-string-no-properties 1)))
        (lyqi:forward-match)
        ;; denominator
        (when (and (not (eolp))
                   (looking-at "/\\([0-9]+\\)"))
          (setf den (string-to-number (match-string-no-properties 1)))
          (lyqi:forward-match))))
    (make-instance 'lyqi:duration-lexeme
                   :length length
                   :dot-count dot-count
                   :numerator num
                   :denominator den
                   :start-point start
                   :end-point (point))))

;;;
;;;
;;;

;;;
;;;
;;;
(defclass lyqi:form (lyqi:parser-symbol) ())
(defclass lyqi:verbatim-form (lyqi:form) ())
(defclass lyqi:music-form (lyqi:form)
  ((duration :initarg :duration
             :initform nil
             :accessor lyqi:duration-of)))

(defclass lyqi:simple-note-form (lyqi:music-form) ())
(defclass lyqi:rest-skip-etc-form (lyqi:music-form) ())
(defclass lyqi:chord-form (lyqi:music-form) ())
(defclass lyqi:incomplete-chord-form (lyqi:form) ())

(defmethod lyqi:parse-one-form ((this lyqi:note-lexeme) rest-lexemes)
  ;; note + duration -> simple-note
  (let ((duration (first rest-lexemes)))
    (values (make-instance 'lyqi:simple-note-form
                           :start-point (slot-value this 'start-point)
                           :end-point (if duration
                                         (slot-value duration 'end-point)
                                         (slot-value this 'end-point))
                           :children (list this duration)
                           :duration duration)
            (cdr rest-lexemes))))

(defmethod lyqi:parse-one-form ((this lyqi:rest-skip-etc-lexeme) rest-lexemes)
  ;; rest|skip|... + duration -> rest-skip-etc
  (let ((duration (first rest-lexemes)))
    (values (make-instance 'lyqi:rest-skip-etc-form
                           :start-point (slot-value this 'start-point)
                           :end-point (if duration
                                          (slot-value duration 'end-point)
                                          (slot-value this 'end-point))
                           :children (list this duration)
                           :duration duration)
            (cdr rest-lexemes))))

(defmethod lyqi:parse-one-form ((this lyqi:chord-start-lexeme) rest-lexemes)
  ;; '<' + (note | verbatim)* + '>' + duration -> chord
  ;; '<' + (note | verbatim)*                  -> incomplete-chord
  (loop for lexemes on rest-lexemes
        for lexeme = (first lexemes)
        for rest-lexemes = (cdr lexemes)
        collect lexeme into children
        if (object-of-class-p lexeme 'lyqi:base-duration-lexeme)
        return (values (make-instance 'lyqi:chord-form
                                      :start-point (slot-value this 'start-point)
                                      :end-point (slot-value lexeme 'end-point)
                                      :duration lexeme
                                      :children (cons this children))
                       rest-lexemes)
        finally return (values (make-instance 'lyqi:incomplete-chord-form
                                              :start-point (slot-value this 'start-point)
                                              :end-point (slot-value lexeme 'end-point)
                                              :children (cons this children))
                               rest-lexemes)))

(defmethod lyqi:parse-one-form ((this lyqi:lexeme) rest-lexemes)
  ;; default rule:
  ;; anything + verbatim* -> verbatim
  (destructuring-bind (children last rest)
      (loop for lexemes on rest-lexemes
            with last-child = this
            for lexeme = (first lexemes)
            if (object-of-class-p lexeme 'lyqi:verbatim-lexeme)
            collect lexeme into children and do (setf last-child lexeme)
            else return (list children last-child lexemes)
            finally return (list children last-child lexemes))
    (princ (format "first: %s\nlast: %s\nrest: %s\n"
                   this last (car rest)))
    (values (make-instance 'lyqi:verbatim-form
                           :start-point (slot-value this 'start-point)
                           :end-point (slot-value last 'end-point)
                           :children (cons this children))
            rest)))

(defun lyqi:parse-all-forms (lexemes acc)
  (if lexemes
      (multiple-value-bind (form rest) (lyqi:parse-one-form (first lexemes) (rest lexemes))
        (if form
            (progn
              (princ (format "Parsed %s\n " (object-class form)))
              (mapcar (lambda (lexeme)
                        (princ (format " %s" (object-class lexeme))))
                      (slot-value form 'children))
              (princ (format "\n  Next lexeme: %s\n"
                             (if rest
                                 (object-class (first rest))
                                 "<eol>"))))
            (princ "no form...\n"))
        (lyqi:parse-all-forms rest (if form
                                       (cons form acc)
                                       acc)))
      (nreverse acc)))

(defun lyqi:parse-line (syntax)
  (lyqi:parse-all-forms (lyqi:lex-line syntax) (list)))
