;;; Lexing and Parsing for emacs modes
;;; Mainly inspired by Drei component of McClim
;;;
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)

;;;
;;; regex and match utilities
;;;

;; for XEmacs21 compatibility
(if (not (fboundp 'match-string-no-properties))
    (defalias 'match-string-no-properties 'match-string))

(defun lp:join (join-string strings)
  "Returns a concatenation of all strings elements, with join-string between elements"
  (apply 'concat 
	 (car strings) 
	 (mapcar (lambda (str) (concat join-string str))
		 (cdr strings))))

(defun lp:sort-string-by-length (string-list)
  "Sort the given string list by decreasing string length."
  (nreverse 
   (sort string-list
	 (lambda (str1 str2)
	   (or (< (length str1) (length str2))
	       (and (= (length str1) (length str2))
		    (string< str1 str2)))))))

(defun lp:forward-match ()
  (forward-char (- (match-end 0) (match-beginning 0))))

;;;
;;; Buffer syntax
;;;
(defclass lp:syntax ()
  ((default-lexer-state
     :initform nil
     :initarg :default-lexer-state
     :accessor lp:default-lexer-state)
   (first-line :initform nil
               :accessor lp:first-line)
   (last-line :initform nil
              :accessor lp:last-line))
  :documentation "Base class for defining a buffer syntax, which
  instance shall be buffer-local.  It contains a double-linked
  list of lines, containing the parsed forms on each line of the
  buffer.")

(defvar lp:*current-syntax* nil
  "The current buffer syntax object")

(defun lp:current-syntax ()
  lp:*current-syntax*)

(defmethod object-print ((this lp:syntax) &rest strings)
  (format "#<%s>" (object-class this)))

;;;
;;; The "parse tree" of a buffer is represented as a double-linked
;;; list of lines, each line containing a list of forms, each form
;;; possibly containing lexemes
;;;

;;; Lines
(defclass lp:line-parse ()
  ((marker :initarg :marker
           :accessor lp:marker)
   (forms :initarg :forms
          :accessor lp:line-forms)
   (lexer-state :initform nil
                :initarg :lexer-state)
   (previous-line :initform nil
                  :initarg :previous-line
                  :accessor lp:previous-line)
   (next-line :initform nil
              :initarg :next-line
              :accessor lp:next-line)))

(defun lp:link-lines (first next)
  (when first
    (set-slot-value first 'next-line next))
  (when next
    (set-slot-value next 'previous-line first)))

;;; Base class for forms and lexemes
(defclass lp:parser-symbol ()
  ((marker :initform nil
           :initarg :marker
           :accessor lp:marker)
   (size :initform nil
         :initarg :size
         :accessor lp:size)
   (children :initarg :children
             :initform nil
             :accessor lp:children)))

(defmethod object-write ((this lp:parser-symbol) &optional comment)
  (let* ((marker (lp:marker this))
         (size (lp:size this))
         (start (and marker (marker-position marker)))
         (end (and marker size (+ start size))))
    (princ
     (format "#<%s [%s-%s] \"%s\""
             (object-class this)
             (or start "?") (or end "?")
             (buffer-substring-no-properties start end)))
    (mapcar (lambda (lexeme)
              (princ " ")
              (princ (object-class lexeme)))
            (lp:children this))
    (princ ">\n")))

(defmethod object-print ((this lp:parser-symbol) &rest strings)
  (let* ((marker (lp:marker this))
         (size (lp:size this))
         (start (and marker (marker-position marker)))
         (end (and marker size (+ start size))))
    (format "#<%s [%s-%s]>"
            (object-class this)
            (or start "?") (or end "?"))))

;;; Forms (produced by reducing lexemes)
(defclass lp:form (lp:parser-symbol) ())

;;; Lexemes (produced when lexing)
(defclass lp:lexeme (lp:parser-symbol) ())

(defclass lp:comment-lexeme (lp:lexeme) ())
(defclass lp:comment-delimiter-lexeme (lp:lexeme) ())
(defclass lp:string-lexeme (lp:lexeme) ())
(defclass lp:doc-lexeme (lp:lexeme) ())
(defclass lp:keyword-lexeme (lp:lexeme) ())
(defclass lp:builtin-lexeme (lp:lexeme) ())
(defclass lp:function-name-lexeme (lp:lexeme) ())
(defclass lp:variable-name-lexeme (lp:lexeme) ())
(defclass lp:type-lexeme (lp:lexeme) ())
(defclass lp:constant-lexeme (lp:lexeme) ())
(defclass lp:warning-lexeme (lp:lexeme) ())
(defclass lp:negation-char-lexeme (lp:lexeme) ())
(defclass lp:preprocessor-lexeme (lp:lexeme) ())


;;; Parsing data (used when reducing lexemes to produce forms)
(defclass lp:parse-data ()
  ((lexemes :initarg :lexemes
            :initform nil)
   (form-class :initarg :form-class)
   (next-lexer-state :initarg :next-lexer-state
                     :initform lp:*lexer-toplevel-state*)))

(defmethod lp:push-lexeme ((this lp:parse-data) lexeme)
  (set-slot-value this 'lexemes
                  (cons lexeme (slot-value this 'lexemes))))

;;;
;;; Lexer states
;;;

(defclass lp:lexer-state () ())

;;;
;;; Lex function
;;;

(defgeneric lp:lex (lexer-state syntax parse-data)
  "Lex or parse one element.

Depending on `lexer-state' and the text at current point, either
lex a lexeme, or reduce previous lexemes (accumulated in
`parse-data') to build a form, or both.

Return four values:
- the new lexer state;
- a form if lexemes have been reduced, or NIL otherwise;
- updated parse data (the `parse-data' argument may be modified),
or NIL if parse data is not relevant.
- NIL if the line parsing is finished, T otherwise.

The parse data contains the list a lexemes yet to be reduced, the
class of the form to be produced, and the lexer state to use
after the reduce.  The input `parse-data' may be NIL if parse
data is not relevant at this step (for instance, at the
beginning.)")

;; a default implementation to avoid compilation warnings
(defmethod lp:lex (lexer-state syntax parse-data)
  (when (looking-at "[ \t]+")
    (lp:forward-match))
  (let ((marker (point-marker)))
    (if (eolp)
        (values nil nil nil nil)
        (progn
          (looking-at "\\S-+")
          (lp:forward-match)
          (values nil
                  (make-instance 'lp:form
                                 :marker marker
                                 :size (- (point) marker))
                  nil
                  (not (eolp)))))))

;;;
;;; Parse functions
;;;

(defun lp:parse (syntax &rest cl-keys)
  "Parse lines in current buffer from point up to `end-position'.
Return three values: the first parse line, the last parse
line (i.e. both ends of double linked parse line list.), and the
lexer state applicable to the following line.

Keywords supported:
  :lexer-state (lp:default-lexer-state syntax)
  :end-position (point-max)"
  (cl-parsing-keywords ((:lexer-state (lp:default-lexer-state syntax))
                        (:end-position (point-max))) ()
    (loop with result = nil
          with first-line = nil
          for previous-line = nil then line
          for state = cl-lexer-state then next-state
          for marker = (point-marker)
          for (forms next-state) = (lp:parse-line syntax state)
          for line = (make-instance 'lp:line-parse
                                    :marker marker
                                    :previous-line previous-line
                                    :lexer-state state
                                    :forms forms)
          unless first-line do (setf first-line line)
          if previous-line do (set-slot-value previous-line 'next-line line)
          do (forward-line 1) ;; go to next-line
          if (>= (point) cl-end-position) return (values first-line line next-state))))

(defun lp:parse-line (syntax state)
  "Return a form list, built by parsing current buffer starting
from current point up to the end of the current line."
  (loop with end-point = (point-at-eol)
        for finished = nil then (>= (point) end-point)
        for (new-state form parse-data continue)
        = (lp:lex (or state (lp:default-lexer-state syntax)) syntax nil)
        then (lp:lex new-state syntax parse-data)
        if form collect form into result
        while continue
        finally return (values result new-state)))

;;;
;;; Parse update
;;;

(defun lp:parse-and-highlight-buffer ()
  "Make a full parse of current buffer and highlight text.
Set current syntax parse data (`first-line' and `last-line'
slots)."
  ;; prevent after-change-functions (i.e. lp:parse-update)
  ;; from being called
  (let ((after-change-functions nil)
        (syntax (lp:current-syntax)))
    ;; initialize the parse tree
    (save-excursion
      (goto-char (point-min))
      (multiple-value-bind (first last state) (lp:parse syntax)
        (set-slot-value syntax 'first-line first)
        (set-slot-value syntax 'last-line last)))
    ;; fontify the buffer
    (loop for line = (lp:first-line syntax)
          then (lp:next-line line)
          while line
          do (mapcar #'lp:fontify (lp:line-forms line)))))

(defun lp:parse-update (beginning end old-length)
  "Update current syntax parse-tree after a buffer modification,
and fontify the changed text.

  `beginning' is the beginning of the changed text.
  `end' is the end of the changed text.
  `length' is the length the pre-changed text."
  (let ((syntax (lp:current-syntax)))
    (cond ((not (lp:first-line syntax))
           (lp:parse-and-highlight-buffer))
          (t
           ;; find the portion of the parse-tree that needs an update
           (multiple-value-bind (first-modified-line last-modified-line)
               ;; TODO: from top or from bottom, depending on position
               ;; of the modified region
               ;;
               ;; from bottom:
               ;; the first modified line is found as soon as
               ;; (<= (lyqi-marker line) beginning)
               ;; the last modified line is found as soon as
               ;; (< (lp:marker line) old-end)
               (loop for line = (lp:last-line syntax)
                     then (lp:previous-line line)
                     with old-end = (+ beginning old-length)
                     with first-modified-line = nil
                     with last-modified-line = nil
                     if (and (not last-modified-line)
                             (< (lp:marker line) old-end))
                     do (setf last-modified-line line)
                     if (<= (lp:marker line) beginning)
                     do (setf first-modified-line line)
                     and return (values first-modified-line last-modified-line)
                     finally return (values first-modified-line last-modified-line))
             (save-excursion
               (let ((end-position (progn
                                     (goto-char end)
                                     (point-at-eol))))
                 (goto-char beginning)
                 (forward-line 0)
                 (multiple-value-bind (first-new-line last-new-line next-state0)
                     (lp:parse syntax
                               :lexer-state (slot-value first-modified-line 'lexer-state)
                               :end-position end-position)
                   ;; fontify new lines
                   (loop for line = first-new-line then (lp:next-line line)
                         while line
                         do (mapcar #'lp:fontify (lp:line-forms line)))
                   ;; replace the old lines with the new ones in the
                   ;; double-linked list
                   (if (eql (lp:first-line syntax)
                            first-modified-line)
                       (set-slot-value syntax 'first-line first-new-line)
                       (lp:link-lines (lp:previous-line first-modified-line)
                                      first-new-line))
                   (if (eql (lp:last-line syntax)
                            last-modified-line)
                       (set-slot-value syntax 'last-line last-new-line)
                       (lp:link-lines last-new-line
                                      (lp:next-line last-modified-line)))
                   ;; If the lexer state at the end of last-new-line is
                   ;; different from the lexer state at the beginning of
                   ;; the next line, then parse next line again (and so
                   ;; on)
                   (loop for line = (lp:next-line last-new-line)
                         then (lp:next-line line)
                         for new-state = next-state0 then next-state
                         while (and line (not (eql new-state (slot-value line 'lexer-state))))
                         for (forms next-state) = (lp:parse-line syntax new-state)
                         do (progn
                              (set-slot-value line 'forms forms)
                              (set-slot-value line 'lexer-state new-state)
                              (lp:fontify line)
                              (forward-line 1)))
                   ;; debug
                   ;;(princ (format "Parse update [%s-%s] [%s-%s]"
                   ;;               beginning end
                   ;;               (marker-position (lp:marker first-modified-line))
                   ;;               (marker-position (lp:marker last-modified-line))))
                   ))))))))


;;;
;;; Fontification
;;;

(defgeneric lp:fontify (parser-symbol)
  "Fontify a lexeme or form")

(defgeneric lp:face (parser-symbol)
  "The face of a lexeme or form, used in fontification.")

(defmethod lp:fontify ((this lp:line-parse))
  (mapcar #'lp:fontify (lp:line-forms this)))

(defmethod lp:fontify ((this lp:parser-symbol))
  (let* ((start (marker-position (lp:marker this)))
         (end (+ start (lp:size this))))
    (when (> end start)
      (set-text-properties start end (or (lp:face this) '())))))

(defmethod lp:fontify ((this lp:form))
  (let ((children (slot-value this 'children)))
    (if children
        (mapcar 'lp:fontify children)
        (call-next-method))))

(defmethod lp:face ((this lp:parser-symbol))
  nil)

(defmethod lp:face ((this lp:comment-lexeme))
  '(face font-lock-comment-face))
(defmethod lp:face ((this lp:comment-delimiter-lexeme))
  '(face font-lock-comment-delimiter-face))
(defmethod lp:face ((this lp:string-lexeme))
  '(face font-lock-string-face))
(defmethod lp:face ((this lp:doc-lexeme))
  '(face font-lock-doc-face))
(defmethod lp:face ((this lp:keyword-lexeme))
  '(face font-lock-keyword-face))
(defmethod lp:face ((this lp:builtin-lexeme))
  '(face font-lock-builtin-face))
(defmethod lp:face ((this lp:function-name-lexeme))
  '(face font-lock-function-name-face))
(defmethod lp:face ((this lp:variable-name-lexeme))
  '(face font-lock-variable-name-face))
(defmethod lp:face ((this lp:type-lexeme))
  '(face font-lock-type-face))
(defmethod lp:face ((this lp:constant-lexeme))
  '(face font-lock-constant-face))
(defmethod lp:face ((this lp:warning-lexeme))
  '(face font-lock-warning-face))
(defmethod lp:face ((this lp:negation-char-lexeme))
  '(face font-lock-negation-char-face))
(defmethod lp:face ((this lp:preprocessor-lexeme))
  '(face font-lock-preprocessor-face))

;;;
(provide 'lp-base)