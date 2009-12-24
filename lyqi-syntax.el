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
(defclass lyqi:toplevel-parser-state (lp:parser-state)
  ((form-class :initform 'lyqi:verbatim-form)))
(defclass lyqi:duration?-parser-state (lp:parser-state) ())
(defclass lyqi:note-duration?-parser-state (lp:parser-state) ())
(defclass lyqi:note-rest?-parser-state (lp:parser-state) ())
(defclass lyqi:incomplete-chord-parser-state (lp:parser-state) ())
(defclass lyqi:line-comment-parser-state (lp:parser-state) ())
(defclass lyqi:string-parser-state (lp:parser-state) ())

(defclass lyqi:scheme-list-parser-state (lp:parser-state)
  ((depth :initform 1
          :initarg :depth)))
(defmethod lp:same-parser-state-p ((this lyqi:scheme-list-parser-state) other-state)
  (and (call-next-method)
       (= (slot-value this 'depth) (slot-value other-state 'depth))))

(defclass lyqi:embedded-toplevel-parser-state (lyqi:toplevel-parser-state) ())

;;;
;;; LilyPond syntax (language dependent)
;;;
(defclass lyqi:lilypond-syntax (lp:syntax)
  ((language              :initarg :language)
   (relative-mode         :initarg :relative-mode
                          :initform nil)
   (quick-edit-mode       :initform nil)
   ;; regex stuff
   (pitch-data            :initarg :pitch-data)
   (pitch-regex           :initarg :pitch-regex)
   (octave-regex          :initarg :octave-regex)
   (note-regex            :initarg :note-regex)
   (rest-skip-regex       :initarg :rest-skip-regex)
   (duration-data         :initarg :duration-data)
   (duration-length-regex :initarg :duration-length-regex)
   (duration-regex        :initarg :duration-regex)))

(defun lyqi:make-lilypond-syntax (language relative-mode)
  (let* ((pitch-data (case language
                       ((italiano) lyqi:+italian-pitchnames+)
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
                   :language               language
                   :relative-mode          relative-mode
                   :default-parser-state
                   (make-instance 'lyqi:toplevel-parser-state)
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
                    :initform 0)
   (accidental :initform nil
               :initarg :accidental)))

(defclass lyqi:rest-skip-etc-lexeme (lp:lexeme) ())
(defclass lyqi:rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:note-rest-lexeme (lyqi:rest-lexeme) ())
(defclass lyqi:mm-rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:space-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:skip-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:chord-repetition-lexeme (lyqi:rest-skip-etc-lexeme) ())

(defclass lyqi:delimiter-lexeme (lp:lexeme) ())
(defclass lyqi:delimiter-start-lexeme (lyqi:delimiter-lexeme) ())
(defclass lyqi:delimiter-end-lexeme (lyqi:delimiter-lexeme) ())
(defclass lyqi:simultaneous-start-lexeme (lyqi:delimiter-start-lexeme) ())
(defclass lyqi:simultaneous-end-lexeme (lyqi:delimiter-end-lexeme) ())
(defclass lyqi:sequential-start-lexeme (lyqi:delimiter-start-lexeme) ())
(defclass lyqi:sequential-end-lexeme (lyqi:delimiter-end-lexeme) ())

(defclass lyqi:chord-start-lexeme (lp:lexeme) ())
(defclass lyqi:chord-end-lexeme (lp:lexeme) ())

(defclass lyqi:string-start-lexeme (lp:string-lexeme) ())
(defclass lyqi:string-lexeme (lp:string-lexeme) ())
(defclass lyqi:string-end-lexeme (lp:string-lexeme) ())

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

;; scheme
(defclass lyqi:scheme-lexeme (lp:lexeme) ())
(defclass lyqi:sharp-lexeme (lyqi:scheme-lexeme) ())
(defclass lyqi:left-parenthesis-lexeme (lyqi:scheme-lexeme
                                        lyqi:delimiter-start-lexeme) ())
(defclass lyqi:right-parenthesis-lexeme (lyqi:scheme-lexeme
                                         lyqi:delimiter-end-lexeme) ())
(defclass lyqi:embedded-lilypond-start-lexeme (lyqi:scheme-lexeme
                                               lyqi:delimiter-start-lexeme) ())
(defclass lyqi:embedded-lilypond-end-lexeme (lyqi:scheme-lexeme
                                             lyqi:delimiter-end-lexeme) ())

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

(defclass lyqi:scheme-list-form (lp:form) ())

;;;
;;; Lex functions
;;;
(defmacro lyqi:with-forward-match (args &rest body)
  (destructuring-bind (regex marker-symbol size-symbol) (if (= (length args) 2)
                                                            (cons nil args)
                                                            args)
    `(let* ((,marker-symbol (point-marker))
            (,size-symbol (progn
                            ,@(if regex `((looking-at ,regex)))
                            (lp:forward-match)
                            (- (point) ,marker-symbol))))
       ,@body)))
(put 'lyqi:with-forward-match 'lisp-indent-function 1)

(defmethod lp:lex ((parser-state lyqi:toplevel-parser-state) syntax)
  (labels ((reduce-lexemes
            (&rest additional-forms)
            (remove-if-not #'identity (cons (lp:reduce-lexemes parser-state)
                                            additional-forms))))
    (lyqi:skip-whitespace)
    (cond ((eolp)
           ;; at end of line, reduce remaining lexemes
           (values parser-state (reduce-lexemes) nil))
          ;; a note
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the note and add the lexeme to the output parse data
          ;; - switch to {note-duration?} lexer state
          ((looking-at (slot-value syntax 'note-regex))
           (values (make-instance 'lyqi:note-duration?-parser-state
                                  :form-class 'lyqi:simple-note-form
                                  :lexemes (list (lyqi:lex-note syntax))
                                  :next-parser-state parser-state)
                   (reduce-lexemes)
                   t))
          ;; rest, mm-rest, skip or spacer
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the rest/skip/etc and add the lexeme to the output parse data
          ;; - switch to {duration?} lexer state
          ((looking-at (slot-value syntax 'rest-skip-regex))
           (values (make-instance 'lyqi:duration?-parser-state
                                  :form-class 'lyqi:rest-skip-etc-form
                                  :lexemes (list (lyqi:lex-rest-skip-etc syntax))
                                  :next-parser-state parser-state)
                   (reduce-lexemes)
                   t))
          ;; block delimiters
          ((looking-at "\\(<<\\|>>\\|{\\|}\\)")
           (lyqi:with-forward-match (marker size)
             (values parser-state
                     (reduce-lexemes (make-instance (case (char-after marker)
                                                      ((?\<) 'lyqi:simultaneous-start-lexeme)
                                                      ((?\>) 'lyqi:simultaneous-end-lexeme)
                                                      ((?\{) 'lyqi:sequential-start-lexeme)
                                                      ((?\}) 'lyqi:sequential-end-lexeme))
                                                    :marker marker
                                                    :size size))
                     (not (eolp)))))
          ;; a chord start: '<'
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the chord start and add the lexeme to the output parse data
          ;; - switch to {incomplete-chord} state
          ((looking-at "<")
           (lyqi:with-forward-match (marker size)
             (values (make-instance 'lyqi:incomplete-chord-parser-state
                                    :form-class 'lyqi:chord-form
                                    :lexemes (list (make-instance 'lyqi:chord-start-lexeme
                                                                  :marker marker
                                                                  :size size))
                                    :next-parser-state parser-state)
                     (reduce-lexemes)
                     t)))
          ;; TODO: multi line comment
          
          ;; line comment
          ;; - reduce preceding verbatim lexemes (if any)
          ;; - lex the line-comment start and add the lexeme to the output parse data
          ;; - switch to {line-comment} state
          ((looking-at "%+")
           (values (make-instance 'lyqi:line-comment-parser-state
                                  :form-class 'lyqi:line-comment-form
                                  :lexemes (list (lyqi:lex-comment-start syntax))
                                  :next-parser-state parser-state)
                   (reduce-lexemes)
                   t))
          ;; a one line string
          ((looking-at "#?\"\\([^\"\\\\]\\|\\\\.\\)*\"")
           (lyqi:with-forward-match (marker size)
             (values parser-state
                     (reduce-lexemes (make-instance 'lyqi:string-lexeme
                                                    :marker marker
                                                    :size size))
                     (not (eolp)))))
          ;; a unterminated string
          ((looking-at "#?\"\\([^\"\\\\]\\|\\\\.\\)*$")
           (lyqi:with-forward-match (marker size)
             (values (make-instance 'lyqi:string-parser-state
                                    :next-parser-state parser-state)
                     (reduce-lexemes (make-instance 'lyqi:string-lexeme
                                                    :marker marker
                                                    :size size))
                     nil)))
          ;; a scheme form (has to be after strings)
          ((looking-at "#")
           (lyqi:with-forward-match (marker size)
             (let ((sharp-lexeme (make-instance 'lyqi:sharp-lexeme
                                                :marker marker
                                                :size size)))
               (if (looking-at "['`]?(")
                   (lyqi:with-forward-match (marker size)
                     ;; a list form
                     (values (make-instance 'lyqi:scheme-list-parser-state
                                            :next-parser-state parser-state)
                             (reduce-lexemes sharp-lexeme
                                             (make-instance 'lyqi:right-parenthesis-lexeme
                                                            :marker marker
                                                            :size size))
                             (not (eolp))))
                     ;; a simple token
                     (lyqi:with-forward-match ("[^ \t\r\n]+" marker size)
                       (values parser-state
                               (reduce-lexemes sharp-lexeme
                                               (make-instance 'lyqi:scheme-lexeme
                                                              :marker marker
                                                              :size size))
                               (not (eolp))))))))
          ;; a backslashed keyword, command or variable
          ((looking-at "\\\\[a-zA-Z]+")
           (lyqi:with-forward-match (marker size)
             (values parser-state
                     (reduce-lexemes (make-instance 'lp:keyword-lexeme
                                                    :marker marker
                                                    :size size))
                     t)))
          ;; other top level expressions are treated as verbatim
          ;; - lex the verbatim word and add the lexeme to the output parse data
          ;; - continue lexing in {toplevel} state
          (t
           (lp:push-lexeme parser-state (lyqi:lex-verbatim syntax))
           (values parser-state
                   nil
                   t)))))

(defmethod lp:lex ((parser-state lyqi:duration?-parser-state) syntax)
  (let ((duration (lyqi:lex-duration syntax)))
    (lyqi:skip-whitespace)
    (lp:push-lexeme parser-state duration)
    (let ((music-form (lp:reduce-lexemes parser-state)))
      (set-slot-value music-form 'duration duration)
      (values (lp:next-parser-state parser-state)
              (list music-form)
              (not (eolp))))))

(defmethod lp:lex ((parser-state lyqi:note-duration?-parser-state) syntax)
  (lp:push-lexeme parser-state (lyqi:lex-duration syntax))
  (values (lp:change-parser-state parser-state 'lyqi:note-rest?-parser-state)
          nil
          t))

(defmethod lp:lex ((parser-state lyqi:note-rest?-parser-state) syntax)
  (lyqi:skip-whitespace)
  (let* ((marker (point-marker))
         (rest-lexeme (when (looking-at "\\\\rest")
                        (lp:forward-match)
                        (make-instance 'lyqi:note-rest-lexeme
                                       :marker marker
                                       :size (- (point) marker)))))
    (when rest-lexeme
      (lp:push-lexeme parser-state rest-lexeme))
    (let ((note-form (lp:reduce-lexemes parser-state)))
      (set-slot-value note-form 'rest (not (not rest-lexeme)))
      (lyqi:skip-whitespace)
      (values (lp:next-parser-state parser-state)
              (list note-form)
              (not (eolp))))))

(defmethod lp:lex ((parser-state lyqi:line-comment-parser-state) syntax)
  (let* ((marker (point-marker))
         (comment-lexeme (and (not (eolp))
                              (progn
                                (end-of-line)
                                (make-instance 'lyqi:line-comment-lexeme
                                               :marker marker
                                               :size (- (point) marker))))))
    (when comment-lexeme
      (lp:push-lexeme parser-state comment-lexeme))
    (values (lp:next-parser-state parser-state)
            (list (lp:reduce-lexemes parser-state))
            nil)))

(defmethod lp:lex ((parser-state lyqi:string-parser-state) syntax)
  (cond ((looking-at "\\([^\"\\\\]\\|\\\\.\\)*\"") ;; string end
         (lyqi:with-forward-match (marker size)
           (values (lp:next-parser-state parser-state)
                   (list (make-instance 'lyqi:string-lexeme
                                        :marker marker
                                        :size size))
                   (not (eolp)))))
        ;; a unterminated string
        ((looking-at "\\([^\"\\\\]\\|\\\\.\\)*$")
         (lyqi:with-forward-match (marker size)
           (values parser-state
                   (list (make-instance 'lyqi:string-lexeme
                                        :marker marker
                                        :size size))
                   nil)))))

(defmethod lp:lex ((parser-state lyqi:incomplete-chord-parser-state) syntax)
  (lyqi:skip-whitespace)
  (cond ((eolp)
         ;; at end of line, reduce remaining lexemes to produce an
         ;; incomplete chord form
         (values parser-state
                 (if (slot-value parser-state 'lexemes)
                     (list (lp:reduce-lexemes parser-state 'lyqi:incomplete-chord-form)))
                 nil))
        ;; a note
        ((looking-at (slot-value syntax 'note-regex))
         (lp:push-lexeme parser-state (lyqi:lex-note syntax))
         (values parser-state
                 nil
                 t))
        ;; at chord end '>', switch to {duration?} state
        ((eql (char-after) ?\>)
         (let ((marker (point-marker)))
           (forward-char 1)
           (lp:push-lexeme parser-state
                           (make-instance 'lyqi:chord-end-lexeme
                                          :marker marker
                                          :size (- (point) marker)))
           (values (lp:change-parser-state parser-state 'lyqi:duration?-parser-state)
                   nil
                   t)))
        ;; a comment
        ((looking-at "%+")
         (values (make-instance 'lyqi:line-comment-parser-state
                                :next-parser-state parser-state
                                :lexemes (list (lyqi:lex-comment-start syntax))
                                :form-class 'lyqi:line-comment-form)
                 (list (lp:reduce-lexemes parser-state 'lyqi:incomplete-chord-form))
                 t))
        ;; something else
        (t
         (lp:push-lexeme parser-state
                         (lyqi:lex-verbatim syntax "[^ \t\r\n>]+"))
         (values parser-state
                 nil
                 t))))

;;;
;;; Basic scheme lexing
;;;

(defmethod lp:lex ((parser-state lyqi:scheme-list-parser-state) syntax)
  (lyqi:skip-whitespace)
  (cond ((eolp)
         (values parser-state
                 nil
                 nil))
        ((looking-at "(")
         (lyqi:with-forward-match (marker size)
           (values (make-instance 'lyqi:scheme-list-parser-state
                                  :depth (1+ (slot-value parser-state 'depth))
                                  :next-parser-state parser-state)
                   (list (make-instance 'lyqi:left-parenthesis-lexeme
                                        :marker marker
                                        :size size))
                   (not (eolp)))))
        ((looking-at ")")
         (lyqi:with-forward-match (marker size)
           (values (lp:next-parser-state parser-state)
                   (list (make-instance 'lyqi:right-parenthesis-lexeme
                                        :marker marker
                                        :size size))
                   (not (eolp)))))
        ((looking-at "#{")
         (lyqi:with-forward-match (marker size)
           (values (make-instance 'lyqi:embedded-toplevel-parser-state
                                  :next-parser-state parser-state)
                   (list (make-instance 'lyqi:embedded-lilypond-start-lexeme
                                        :marker marker
                                        :size size))
                   (not (eolp)))))
        ;; TODO: strings
        ;; TODO: special tokens, like define, let, etc
        (t
         (lyqi:with-forward-match ("[^ \t\r\n()]+" marker size)
           (values parser-state
                   (list (make-instance 'lyqi:scheme-lexeme
                                        :marker marker
                                        :size size))
                   (not (eolp)))))))

(defmethod lp:lex ((parser-state lyqi:embedded-toplevel-parser-state) syntax)
  (lyqi:skip-whitespace)
  (if (looking-at "#}")
      (lyqi:with-forward-match (marker size)
        (values (lp:next-parser-state parser-state)
                (list (make-instance 'lyqi:embedded-lilypond-end-lexeme
                                     :marker marker
                                     :size size))
                (not (eolp))))
      (call-next-method)))

;;;
;;; specific lexing functions
;;;

(defun lyqi:skip-whitespace ()
  "Skip white space (except new lines)."
  (when (looking-at "[ 	]+")
    (lp:forward-match)))

(defun lyqi:lex-verbatim (syntax &optional verbatim-regex)
  (lyqi:with-forward-match ((or verbatim-regex "[^ \t\r\n\"<>{}]+") marker size)
    (make-instance 'lyqi:verbatim-lexeme
                   :marker marker
                   :size size)))

(defun lyqi:lex-comment-start (syntax)
  (let ((marker (point-marker)))
    (looking-at "%+")
    (lp:forward-match)
    (make-instance 'lyqi:line-comment-start-lexeme
                   :marker marker
                   :size (- (point) marker))))

(defun lyqi:lex-note (syntax)
  (let ((pitch 0)
        (alteration 0)
        (octave-modifier 0)
        (marker (point-marker))
        (accidental nil))
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
        (lp:forward-match))
      ;; accidental
      (cond ((eql (char-after) ?\!)
             (forward-char 1)
             (setf accidental 'forced))
            ((eql (char-after) ?\?)
             (forward-char 1)
             (setf accidental 'cautionary))))
    (make-instance 'lyqi:note-lexeme
                   :pitch pitch
                   :alteration alteration
                   :octave-modifier octave-modifier
                   :accidental accidental
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