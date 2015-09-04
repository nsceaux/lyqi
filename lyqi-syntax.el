;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LilyPond syntax
;;;

(require 'cl)
(require 'eieio)
(require 'lp-base)
(require 'lyqi-pitchnames)
(require 'lyqi-custom)
(require 'lyqi-words)

;;;
;;; Lexer states
;;;
(defclass lyqi:lilypond-parser-state (lp:parser-state)
  ((embedded-lilypond :initform nil
                      :initarg :embedded-lilypond
                      :accessor lyqi:embedded-lilypond-state-p)))

(defclass lyqi:base-parser-state (lyqi:lilypond-parser-state)
  ((form-class :initform 'lyqi:verbatim-form)))

(defclass lyqi:toplevel-parser-state (lyqi:base-parser-state) ())
(defclass lyqi:embedded-toplevel-parser-state (lyqi:toplevel-parser-state)
  ((embedded-lilypond :initform t)))
(defclass lyqi:duration?-parser-state (lyqi:lilypond-parser-state) ())
(defclass lyqi:note-duration?-parser-state (lyqi:lilypond-parser-state) ())
(defclass lyqi:note-rest?-parser-state (lyqi:lilypond-parser-state) ())
(defclass lyqi:chord-parser-state (lyqi:base-parser-state) ())
(defclass lyqi:string-parser-state (lyqi:lilypond-parser-state)
  ((lexeme-class :initform 'lyqi:string-lexeme)
   (end-lexeme-class :initform 'lyqi:string-end-lexeme)))
(defclass lyqi:scheme-string-parser-state (lyqi:string-parser-state)
  ((lexeme-class :initform 'lyqi:scheme-string-lexeme)
   (end-lexeme-class :initform 'lyqi:scheme-string-end-lexeme)))
(defclass lyqi:comment-parser-state (lyqi:lilypond-parser-state) ())

(defclass lyqi:scheme-list-parser-state (lyqi:lilypond-parser-state)
  ((depth :initform 1
          :initarg :depth)))
(defmethod lp:same-parser-state-p ((this lyqi:scheme-list-parser-state) other-state)
  (and (call-next-method)
       (= (slot-value this 'depth) (slot-value other-state 'depth))))

(defmethod lyqi:scheme-state-p ((this lyqi:lilypond-parser-state))
  nil)
(defmethod lyqi:scheme-state-p ((this lyqi:scheme-list-parser-state))
  t)

(defun lyqi:make-parser-state (class next-parser-state &rest initargs)
  (apply 'make-instance
         class
         :next-parser-state next-parser-state
         :embedded-lilypond (and next-parser-state
                                 (lyqi:embedded-lilypond-state-p next-parser-state))
         initargs))

;;;
;;; LilyPond syntax (language dependent)
;;;
(defclass lyqi:lilypond-syntax (lp:syntax)
  ((language              :initarg :language
                          :accessor lyqi:language)
   (possible-languages)
   (quick-edit-mode       :initform nil)))

(defmethod initialize-instance :AFTER ((this lyqi:lilypond-syntax) &optional fields)
  (set-slot-value this 'possible-languages
                  (copy-list lyqi:prefered-languages))
  (set-slot-value this 'default-parser-state
                  (make-instance 'lyqi:toplevel-parser-state)))

;;;
;;; Music type mixins
;;;
(defclass lyqi:note-mixin ()
  ((pitch :initarg :pitch)
   (alteration :initarg :alteration
               :initform 0)
   (octave-modifier :initarg :octave-modifier
                    :initform 0)
   (accidental :initform nil
               :initarg :accidental)))

(defclass lyqi:duration-mixin ()
  ((length      :initarg :length
                :initform nil)
   (dot-count   :initarg :dot-count
                :initform 0)
   (numerator   :initarg :numerator
                :initform 1)
   (denominator :initarg :denominator
                :initform 1)))

;;;
;;; Lexemes
;;;

(defclass lyqi:verbatim-lexeme (lp:lexeme) ())

(defclass lyqi:note-lexeme (lp:lexeme lyqi:note-mixin) ())
(defclass lyqi:rest-skip-etc-lexeme (lp:lexeme) ())
(defclass lyqi:rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:note-rest-lexeme (lyqi:rest-lexeme) ())
(defclass lyqi:mm-rest-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:space-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:skip-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:chord-repetition-lexeme (lyqi:rest-skip-etc-lexeme) ())
(defclass lyqi:chord-start-lexeme (lp:lexeme) ())
(defclass lyqi:chord-end-lexeme (lp:lexeme) ())
(defclass lyqi:base-duration-lexeme (lp:lexeme) ())
(defclass lyqi:duration-lexeme (lyqi:base-duration-lexeme lyqi:duration-mixin) ())
(defclass lyqi:no-duration-lexeme (lyqi:base-duration-lexeme) ())

(defclass lyqi:simultaneous-start-lexeme (lp:opening-delimiter-lexeme) ())
(defclass lyqi:simultaneous-end-lexeme (lp:closing-delimiter-lexeme) ())
(defclass lyqi:sequential-start-lexeme (lp:opening-delimiter-lexeme) ())
(defclass lyqi:sequential-end-lexeme (lp:closing-delimiter-lexeme) ())

(defclass lyqi:backslashed-lexeme (lp:lexeme) ())
(defclass lyqi:keyword-lexeme (lp:builtin-lexeme lyqi:backslashed-lexeme) ())
(defclass lyqi:variable-lexeme (lp:variable-name-lexeme lyqi:backslashed-lexeme) ())
(defclass lyqi:function-lexeme (lp:function-name-lexeme lyqi:backslashed-lexeme) ())
(defclass lyqi:user-command-lexeme (lp:keyword-lexeme lyqi:backslashed-lexeme) ())

(defclass lyqi:string-lexeme (lp:string-lexeme) ())

(defclass lyqi:one-line-comment-lexeme (lp:comment-lexeme)
  ((level :initarg :level)))
(defclass lyqi:multi-line-comment-lexeme (lp:comment-lexeme) ())

(defgeneric lyqi:explicit-duration-p (duration)
  "Return T iff `duration' is an explicit duration lexeme,
and NIL if it is an implicit duration lexeme.")
(defmethod lyqi:explicit-duration-p ((this lyqi:base-duration-lexeme))
  nil)
(defmethod lyqi:explicit-duration-p ((this lyqi:duration-lexeme))
  t)

;; scheme
(defclass lyqi:scheme-lexeme (lp:lexeme) ())
(defclass lyqi:scheme-number-lexeme (lyqi:scheme-lexeme) ())
(defclass lyqi:scheme-symbol-lexeme (lyqi:scheme-lexeme)
  ((special-args :initarg :special-args
                 :initform nil)))
(defclass lyqi:scheme-macro-lexeme (lyqi:scheme-symbol-lexeme lp:builtin-lexeme) ())
(defclass lyqi:scheme-function-lexeme (lyqi:scheme-symbol-lexeme lp:function-name-lexeme) ())
(defclass lyqi:scheme-variable-lexeme (lyqi:scheme-symbol-lexeme lp:variable-name-lexeme) ())
(defclass lyqi:sharp-lexeme (lyqi:scheme-lexeme) ())
(defclass lyqi:left-parenthesis-lexeme (lyqi:scheme-lexeme
                                        lp:opening-delimiter-lexeme) ())
(defclass lyqi:right-parenthesis-lexeme (lyqi:scheme-lexeme
                                         lp:closing-delimiter-lexeme) ())
(defclass lyqi:embedded-lilypond-start-lexeme (lyqi:scheme-lexeme
                                               lp:opening-delimiter-lexeme) ())
(defclass lyqi:embedded-lilypond-end-lexeme (lyqi:scheme-lexeme
                                             lp:closing-delimiter-lexeme) ())
(defclass lyqi:scheme-comment-lexeme (lyqi:one-line-comment-lexeme lyqi:scheme-lexeme) ())
(defclass lyqi:scheme-string-lexeme (lp:string-lexeme lyqi:scheme-lexeme) ())

;;;
;;; forms
;;;
(defclass lyqi:verbatim-form (lp:form) ())
(defclass lyqi:music-form (lp:form)
  ((duration :initarg :duration
             ;;:initform nil
             :accessor lyqi:duration-of)))
(defclass lyqi:simple-note-form (lyqi:music-form)
  ((rest :initarg :rest
         :initform nil)))
(defclass lyqi:rest-skip-etc-form (lyqi:music-form) ())
(defclass lyqi:chord-end-form (lyqi:music-form) ())

(defclass lyqi:scheme-list-form (lp:form) ())

(defgeneric lyqi:form-with-duration-p (parser-symbol)
  "If `parser-symbol' is a music form, i.e. somthing
with a duration, then return its duration.")
(defmethod lyqi:form-with-duration-p ((this lp:parser-symbol))
  nil)
(defmethod lyqi:form-with-duration-p ((this lyqi:music-form))
  (lyqi:duration-of this))

(defun lyqi:form-with-note-p (parser-symbol)
  "If `parser-symbol' has, or is a note lexeme, then return the note.
Oterwise, return NIL."
  (cond ((lyqi:note-lexeme-p parser-symbol)
         parser-symbol)
        ((lyqi:simple-note-form-p parser-symbol)
         (loop for lexeme in (slot-value parser-symbol 'children)
               for note = (lyqi:form-with-note-p lexeme)
               if note return note))))

;;;
;;; Fontification of lexemes and forms
;;;

(defmethod lp:fontify ((this lyqi:verbatim-form))
  nil)

(defmethod lp:face ((this lyqi:note-lexeme))
  '(face font-lock-constant-face))

(defmethod lp:face ((this lyqi:chord-start-lexeme))
  '(face font-lock-constant-face))

(defmethod lp:face ((this lyqi:music-form))
  '(face font-lock-constant-face))

(defmethod lp:fontify ((this lyqi:music-form))
  (let* ((start (marker-position (lp:marker this)))
         (end (+ start (lp:size this))))
    (when (> end start)
      (lp:fontify-region start end (lp:face this)))))

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

(defmethod lyqi:reduce-lexemes ((this lp:parser-state) &rest additional-forms)
  (remove-if-not #'identity
                 (cons (lp:reduce-lexemes this)
                       additional-forms)))

(defmethod lyqi:reduce-lexemes ((this lyqi:chord-parser-state) &rest additional-forms)
  (let ((forms (nreverse (slot-value this 'lexemes))))
    (set-slot-value this 'lexemes nil)
    (if additional-forms
        (nconc forms additional-forms)
        forms)))

(defmethod lp:lex ((parser-state lyqi:base-parser-state) syntax)
  (lyqi:skip-whitespace)
  (cond ((eolp)
         ;; at end of line, reduce remaining lexemes
         (values parser-state (lyqi:reduce-lexemes parser-state) nil))
        ;; multi-line comment
        ((looking-at "%{")
         (multiple-value-bind (lexeme comment-ended)
             (lyqi:lex-comment syntax 'lyqi:multi-line-comment-lexeme t)
           (values (if comment-ended
                       parser-state
                       (lyqi:make-parser-state 'lyqi:comment-parser-state parser-state))
                   (lyqi:reduce-lexemes parser-state
                                        lexeme)
                   (not (eolp)))))
        ;; one line comment
        ((looking-at "\\(%+\\).*$")
         (lyqi:with-forward-match (marker size)
           (values parser-state
                   (lyqi:reduce-lexemes parser-state
                                        (make-instance 'lyqi:one-line-comment-lexeme
                                                       :level (- (match-end 1) (match-beginning 1))
                                                       :marker marker
                                                       :size size))
                   nil)))
        ;; string
        ((looking-at "#?\"")
         (multiple-value-bind (lexeme string-ended)
             (lyqi:lex-string syntax 'lyqi:string-lexeme t)
           (values (if string-ended
                       parser-state
                       (lyqi:make-parser-state 'lyqi:string-parser-state parser-state))
                   (lyqi:reduce-lexemes parser-state
                                        lexeme)
                   (not (eolp)))))
        ;; a scheme form
        ((looking-at "#")
         (lyqi:with-forward-match (marker size)
           (let ((sharp-lexeme (make-instance 'lyqi:sharp-lexeme
                                              :marker marker
                                              :size size)))
             (cond ((eolp)
                    (values parser-state
                            (lyqi:reduce-lexemes parser-state sharp-lexeme)
                            nil))
                   ((looking-at "['`$]?(")
                    (lyqi:with-forward-match (marker size)
                      ;; a list form
                      (values (lyqi:make-parser-state 'lyqi:scheme-list-parser-state
                                                      parser-state)
                              (lyqi:reduce-lexemes parser-state
                                                   sharp-lexeme
                                                   (make-instance 'lyqi:left-parenthesis-lexeme
                                                                  :marker marker
                                                                  :size size))
                              (not (eolp)))))
                   ;; a simple token
                   ((lyqi:with-forward-match ("[^ \t\r\n]+" marker size)
                      (values parser-state
                              (lyqi:reduce-lexemes parser-state
                                                   sharp-lexeme
                                                   (make-instance 'lyqi:scheme-lexeme
                                                                  :marker marker
                                                                  :size size))
                              (not (eolp)))))))))
        ;; a backslashed keyword, command or variable
        ((looking-at "[_^-]?\\\\\\([a-zA-Z-]+\\|[<>!]\\)")
         (lyqi:with-forward-match (marker size)
           (let ((sym (intern (match-string-no-properties 1))))
             (values parser-state
                     (lyqi:reduce-lexemes
                      parser-state
                      (make-instance (cond ((memq sym lyqi:lilypond-keywords)
                                            'lyqi:keyword-lexeme)
                                           ((memq sym lyqi:lilypond-music-variables)
                                            'lyqi:variable-lexeme)
                                           ((or (memq sym lyqi:lilypond-music-functions)
                                                (memq sym lyqi:lilypond-markup-commands)
                                                (memq sym lyqi:lilypond-markup-list-commands))
                                            'lyqi:function-lexeme)
                                           (t
                                            'lyqi:user-command-lexeme))
                                     :marker marker
                                     :size size))
                     t))))
        ;; other top level expressions are treated as verbatim
        ;; - lex the verbatim word and add the lexeme to the output parse data
        ;; - continue lexing in {toplevel} state
        (t
         (lp:push-lexeme parser-state (lyqi:lex-verbatim syntax))
         (values parser-state
                 nil
                 t))))

(defmethod lp:lex ((parser-state lyqi:toplevel-parser-state) syntax)
  (lyqi:skip-whitespace)
  (cond ((eolp)
         ;; at end of line, reduce remaining lexemes
         (values parser-state (lyqi:reduce-lexemes parser-state) nil))
        ;; a note
        ;; - reduce preceding verbatim lexemes (if any)
        ;; - lex the note and add the lexeme to the output parse data
        ;; - switch to {note-duration?} lexer state
        ((looking-at (slot-value (lyqi:language syntax) 'note-regex))
         (values (lyqi:make-parser-state 'lyqi:note-duration?-parser-state
                                         parser-state
                                         :form-class 'lyqi:simple-note-form
                                         :lexemes (list (lyqi:lex-note syntax)))
                 (lyqi:reduce-lexemes parser-state)
                 t))
        ;; rest, mm-rest, skip or spacer
        ;; - reduce preceding verbatim lexemes (if any)
        ;; - lex the rest/skip/etc and add the lexeme to the output parse data
        ;; - switch to {duration?} lexer state
        ((looking-at (slot-value (lyqi:language syntax) 'rest-skip-regex))
         (values (lyqi:make-parser-state 'lyqi:duration?-parser-state
                                         parser-state
                                         :form-class 'lyqi:rest-skip-etc-form
                                         :lexemes (list (lyqi:lex-rest-skip-etc syntax)))
                 (lyqi:reduce-lexemes parser-state)
                 t))
        ;; block delimiters
        ((looking-at "\\(<<\\|>>\\|{\\|}\\)")
         (lyqi:with-forward-match (marker size)
           (values parser-state
                   (lyqi:reduce-lexemes parser-state
                                        (make-instance (case (char-after marker)
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
        ;; - switch to {chord} state
        ((looking-at "<")
         (lyqi:with-forward-match (marker size)
           (values (lyqi:make-parser-state 'lyqi:chord-parser-state
                                           parser-state
                                           :form-class 'lyqi:chord-end-form
                                           :lexemes (list (make-instance 'lyqi:chord-start-lexeme
                                                                         :marker marker
                                                                         :size size)))
                   (lyqi:reduce-lexemes parser-state)
                   t)))
        (t
         (call-next-method))))

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
                                       :size (- (point) marker))))
         (duration-lexeme (first (slot-value parser-state 'lexemes))))
    (when rest-lexeme
      (lp:push-lexeme parser-state rest-lexeme))
    (let ((note-form (lp:reduce-lexemes parser-state)))
      (set-slot-value note-form 'rest (not (not rest-lexeme)))
      (set-slot-value note-form 'duration duration-lexeme)
      (lyqi:skip-whitespace)
      (values (lp:next-parser-state parser-state)
              (list note-form)
              (not (eolp))))))

(defmethod lp:lex ((parser-state lyqi:string-parser-state) syntax)
  (multiple-value-bind (lexeme string-ended)
      (lyqi:lex-string syntax (slot-value parser-state 'lexeme-class) nil)
    (values (if string-ended
                (lp:next-parser-state parser-state)
                parser-state)
            (list lexeme)
            (not (eolp)))))

(defmethod lp:lex ((parser-state lyqi:comment-parser-state) syntax)
  (multiple-value-bind (lexeme comment-ended)
      (lyqi:lex-comment syntax 'lyqi:multi-line-comment-lexeme nil)
    (values (if comment-ended
                (lp:next-parser-state parser-state)
                parser-state)
            (list lexeme)
            (not (eolp)))))

(defmethod lp:lex ((parser-state lyqi:chord-parser-state) syntax)
  (lyqi:skip-whitespace)
  (cond ((eolp)
         ;; this form is on several lines.
         ;; return all lexemes
         (values parser-state
                 (lyqi:reduce-lexemes parser-state)
                 nil))
        ;; a note
        ((looking-at (slot-value (lyqi:language syntax) 'note-regex))
         (lp:push-lexeme parser-state (lyqi:lex-note syntax))
         (values parser-state
                 nil
                 t))
        ;; at chord end '>', switch to {duration?} state
        ((eql (char-after) ?\>)
         (let* ((marker (point-marker)))
           (forward-char 1)
           (let ((forms (lyqi:reduce-lexemes parser-state))
                 (new-state (lp:change-parser-state parser-state 'lyqi:duration?-parser-state)))
             (lp:push-lexeme new-state (make-instance 'lyqi:chord-end-lexeme
                                                      :marker marker
                                                      :size (- (point) marker)))
             (values new-state
                     forms
                     t))))
        (t
         (call-next-method))))

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
           (values (lyqi:make-parser-state 'lyqi:scheme-list-parser-state
                                           parser-state
                                           :depth (1+ (slot-value parser-state 'depth)))
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
           (values (lyqi:make-parser-state 'lyqi:embedded-toplevel-parser-state
                                           parser-state)
                   (list (make-instance 'lyqi:embedded-lilypond-start-lexeme
                                        :marker marker
                                        :size size))
                   (not (eolp)))))
        ((looking-at "\"")
         (multiple-value-bind (lexeme string-ended)
             (lyqi:lex-string syntax 'lyqi:scheme-string-lexeme t)
           (values (if string-ended
                       parser-state
                       (lyqi:make-parser-state 'lyqi:scheme-string-parser-state parser-state))
                   (list lexeme)
                   (not (eolp)))))
        ((looking-at "\\(;+\\).*$")
         (lyqi:with-forward-match (marker size)
           (values parser-state
                   (list (make-instance 'lyqi:scheme-comment-lexeme
                                        :level (- (match-end 1) (match-beginning 1))
                                        :marker marker
                                        :size size))
                   nil)))
        ((looking-at "#[ieobx][^ \t\r\n()]+")
         (lyqi:with-forward-match (marker size)
           (values parser-state
                   (list (make-instance 'lyqi:scheme-number-lexeme
                                        :marker marker :size size))
                   (not (eolp)))))
        ((looking-at "[0-9.]+[0-9.#]*\\([esfdl][+-]?[0-9]+\\)?")
         (lyqi:with-forward-match (marker size)
           (values parser-state
                   (list (make-instance 'lyqi:scheme-number-lexeme
                                        :marker marker :size size))
                   (not (eolp)))))
        ;; TODO: literal vectors, etc
        (t ;; symbols
         (lyqi:with-forward-match ("[^ \t\r\n()]+" marker size)
           (let* ((symbol-name (match-string-no-properties 0))
                  (symbol (intern symbol-name))
                  (special-args
                   (case symbol
                     ;; TODO: define a method
                     ((define-markup-command define-music-function
                        defmacro defmacro*-public defmacro-public)
                      '(2 t))
                     ((lambda case let let* parameterize define
                        define-public define-macro)
                      '(1 t))
                     ((begin) '(0 t))
                     (t (if (string-match "define.*" symbol-name)
                            '(1 t)
                            '(nil nil))))))
             (values parser-state
                     (list (make-instance (cond ((or (memq symbol lyqi:scheme-guile-macros)
                                                     (memq symbol lyqi:scheme-lily-macros))
                                                 'lyqi:scheme-macro-lexeme)
                                                ((or (memq symbol lyqi:scheme-guile-procedures)
                                                     (memq symbol lyqi:scheme-lily-procedures))
                                                 'lyqi:scheme-function-lexeme)
                                                ((memq symbol lyqi:scheme-lily-variables)
                                                 'lyqi:scheme-variable-lexeme)
                                                (t
                                                 'lyqi:scheme-symbol-lexeme))
                                            :special-args special-args
                                            :marker marker
                                            :size size))
                       (not (eolp))))))))

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
  (lyqi:with-forward-match ((or verbatim-regex ".[^ \t\r\n\"%<>{}\\]*") marker size)
    (make-instance 'lyqi:verbatim-lexeme
                   :marker marker
                   :size size)))

(defun lyqi:lex-string (syntax lexeme-class with-start)
  "Lex a double-quote delimited string. If `with-start' is true,
then the first double-quote character found after point is the
opening string delimiter. Any character found before this first
double quote is considered as taking part in the string, as the
sharp in: #\" ... \". If `with-start' is NIL, then the opening
double quote is not searched.

Return two values:
- a string lexeme, of class `lexeme-class'
- T if the string is terminated, or NIL otherwise."
  (let ((marker (point-marker))
        (size 0))
    (when with-start
      (looking-at "[^\"]*\"")
      (lp:forward-match)
      (incf size (- (match-end 0) (match-beginning 0))))
    (loop for char = (char-after)
          for next-char = (char-after (1+ (point)))
          if (eolp)
          return (values (make-instance lexeme-class
                                        :marker marker
                                        :size size)
                         nil)
          else if (eql char ?\")
          ;; string end
          do (forward-char 1) and do (incf size)
          and return (values (make-instance lexeme-class
                                            :marker marker
                                            :size size)
                             t)
          ;; an escaped double quote
          else if (and (eql char ?\\) next-char (eql next-char ?\"))
          do (forward-char 2) and do (incf size 2)
          ;; continue scan
          else do (forward-char 1) and do (incf size))))

(defun lyqi:lex-comment (syntax lexeme-class with-start)
  "Lex a multi line lilypond comment. If `with-start' is true,
then %{ is supposed to be found after point. If `with-start' is
NIL, then the opening %{ is not searched.

Return two values:
- a comment lexeme, of class `lexeme-class'
- T if the comment is terminated, or NIL otherwise."
  (let ((marker (point-marker))
        (size 0))
    (when with-start
      (lyqi:with-forward-match ("%{" m s)
        (incf size s)))
    (loop for char = (char-after)
          for next-char = (char-after (1+ (point)))
          if (eolp)
          ;; end of line => comment is not finished
          return (values (make-instance lexeme-class
                                        :marker marker
                                        :size size)
                         nil)
          else if (and next-char (eql char ?\%) (eql next-char ?\}))
          ;; comment end
          do (forward-char 2) and do (incf size 2)
          and return (values (make-instance lexeme-class
                                            :marker marker
                                            :size size)
                             t)
          ;; continue scan
          else do (forward-char 1) and do (incf size))))

(defun lyqi:lex-note (syntax)
  (let ((pitch 0)
        (alteration 0)
        (octave-modifier 0)
        (marker (point-marker))
        (accidental nil))
    (when (looking-at (slot-value (lyqi:language syntax) 'pitch-regex))
      ;; pitch and alteration
      (let ((pitch-data (assoc (match-string-no-properties 0)
                               (slot-value (lyqi:language syntax) 'name->pitch))))
        (setf pitch (second pitch-data))
        (setf alteration (third pitch-data)))
      (lp:forward-match)
      ;; octave
      (when (looking-at (slot-value (lyqi:language syntax) 'octave-regex))
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
          (not (looking-at (slot-value (lyqi:language syntax) 'duration-regex))))
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
        (when (looking-at (slot-value (lyqi:language syntax) 'duration-length-regex))
          ;; length
          (setf length (cdr (assoc (match-string-no-properties 0)
                                   (slot-value (lyqi:language syntax) 'duration-data))))
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