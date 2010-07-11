;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base incremental lexing and parsing
;;;

(eval-when-compile (require 'cl))
(require 'eieio)

;;;
;;; regex and match utilities
;;;

;; for XEmacs21 compatibility
(if (not (fboundp 'match-string-no-properties))
    (defalias 'match-string-no-properties 'match-string))

(defun lp:forward-match ()
  (forward-char (- (match-end 0) (match-beginning 0))))

;;;
;;; Buffer syntax
;;;
(defclass lp:syntax ()
  ((default-parser-state
     :initform nil
     :initarg :default-parser-state
     :accessor lp:default-parser-state)
   (first-line :initform nil
               :accessor lp:first-line)
   (last-line :initform nil
              :accessor lp:last-line)
   (current-line :initform nil
                 :accessor lp:current-line)
   ;; set by a before-change function, so that
   ;; an after-change-function can update the parse:
   (first-modified-line :initform nil)
   (last-modified-line :initform nil))
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
   (parser-state :initform nil
                 :initarg :parser-state)
   (previous-line :initform nil
                  :initarg :previous-line
                  :accessor lp:previous-line)
   (next-line :initform nil
              :initarg :next-line
              :accessor lp:next-line)))

(defmethod lp:line-end-position ((this lp:line-parse))
  (save-excursion
    (goto-char (lp:marker this))
    (point-at-eol)))

(defmethod object-print ((this lp:line-parse) &rest strings)
  (format "#<%s [%s] %s forms>"
            (object-class this)
            (marker-position (lp:marker this))
            (length (lp:line-forms this))))

(defmethod lp:debug-display ((this lp:syntax) &optional indent)
  (let ((indent (or indent 0)))
    (loop for line = (lp:first-line this) then (lp:next-line line)
          for i from 1
          while line
          do (princ (format "Line %d " i))
          do (lp:debug-display line indent))
    t))

(defmethod lp:debug-display ((this lp:line-parse) &optional indent)
  (let ((indent (or indent 0)))
    (princ
     (format "%s[%s] %s (%s forms)\n"
             (make-string indent ?\ )
             (marker-position (lp:marker this))
             (object-class (slot-value this 'parser-state))
             (length (lp:line-forms this))))
    (loop for form in (lp:line-forms this)
          do (lp:debug-display form (+ indent 2)))
    t))

(defun lp:link-lines (first next)
  (when first
    (set-slot-value first 'next-line next))
  (when next
    (set-slot-value next 'previous-line first)))

(defun lp:previous-form (line &optional rest-forms)
  "For backward iteration on forms, across lines.
Return three values:
 - the previous form
 - the form line
 - the remaining forms on this line

See also `lp:form-before-point'."
  (if rest-forms
      (values (first rest-forms) line (rest rest-forms))
      (when line
        (let* ((prev-line (lp:previous-line line))
               (prev-forms (and prev-line (reverse (lp:line-forms prev-line)))))
          (if prev-forms
              (values (first prev-forms) prev-line (rest prev-forms))
              (lp:previous-form prev-line))))))

(defun lp:form-before-point (syntax position)
  "Return three values:
- the form preceding position
- the form line
- the remaining forms on this line

To perform a backward search on forms from (point), do e.g.:

  (loop for (form line rest-forms) = (lp:form-before-point syntax position)
        then (lp:previous-form line rest-forms)
        ...)"
  (let ((line (lp:find-line syntax position)))
    (when line
      (loop for forms on (reverse (lp:line-forms line))
            if (< (lp:marker (first forms)) position)
            return (values (first forms) line (rest forms))
            finally return (lp:previous-form line)))))

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

(defmethod lp:debug-display ((this lp:parser-symbol) &optional indent)
  (let ((indent (or indent 0)))
    (princ (format "%s[%s-%s] %s: %s\n"
                   (make-string indent ?\ )
                   (marker-position (lp:marker this))
                   (+ (lp:marker this) (lp:size this))
                   (object-class this)
                   (lp:string this)))))

(defmethod lp:string ((this lp:parser-symbol))
  (with-slots (marker size) this
    (buffer-substring-no-properties marker (+ marker size))))

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

(defclass lp:delimiter-lexeme (lp:lexeme) ())
(defclass lp:opening-delimiter-lexeme (lp:delimiter-lexeme) ())
(defclass lp:closing-delimiter-lexeme (lp:delimiter-lexeme) ())

(defmethod lp:opening-delimiter-p ((this lp:parser-symbol))
  nil)
(defmethod lp:opening-delimiter-p ((this lp:opening-delimiter-lexeme))
  t)
(defmethod lp:closing-delimiter-p ((this lp:parser-symbol))
  nil)
(defmethod lp:closing-delimiter-p ((this lp:closing-delimiter-lexeme))
  t)

;;; Parsing data (used when reducing lexemes to produce forms)
(defclass lp:parser-state ()
  ((lexemes :initarg :lexemes
            :initform nil)
   (form-class :initarg :form-class)
   (next-parser-state :initarg :next-parser-state
                      :initform nil
                      :accessor lp:next-parser-state)))

(defmethod lp:push-lexeme ((this lp:parser-state) lexeme)
  (set-slot-value this 'lexemes
                  (cons lexeme (slot-value this 'lexemes))))

(defmethod lp:reduce-lexemes ((this lp:parser-state) &optional form-class)
  (let ((reversed-lexemes (slot-value this 'lexemes)))
    (when reversed-lexemes
      (let* ((last-lexeme (first reversed-lexemes))
             (lexemes (nreverse (slot-value this 'lexemes)))
             (first-lexeme (first lexemes)))
        (set-slot-value this 'lexemes nil)
        (make-instance (or form-class (slot-value this 'form-class))
                       :children lexemes
                       :marker (lp:marker first-lexeme)
                       :size (- (+ (lp:marker last-lexeme)
                                   (lp:size last-lexeme))
                                (lp:marker first-lexeme)))))))

(defmethod lp:same-parser-state-p ((this lp:parser-state) other-state)
  (and other-state
       (eql (object-class this)
            (object-class other-state))
       (let ((next-class (and (lp:next-parser-state this)
                              (object-class (lp:next-parser-state this))))
             (other-next-class (and (lp:next-parser-state other-state)
                                    (object-class (lp:next-parser-state other-state)))))
         (eql next-class other-next-class))))

(defmethod lp:change-parser-state ((original lp:parser-state) new-class)
  (with-slots (lexemes form-class next-parser-state) original
    (make-instance new-class
                   :lexemes lexemes
                   :form-class form-class
                   :next-parser-state next-parser-state)))

;;;
;;; Lex function
;;;

(defgeneric lp:lex (parser-state syntax)
  "Lex or parse one element.

Depending on `parser-state' and the text at current point, either
lex a lexeme, or reduce previous lexemes (accumulated up to now
in `parser-state') to build a form, or both.

Return three values:
- the new parser state. The input `parser-state' may be modified,
  in particular its `lexemes' slot;
- a list of forms, if lexemes have been reduced, or NIL otherwise;
- NIL if the line parsing is finished, T otherwise.")

;; a default implementation to avoid compilation warnings
(defmethod lp:lex (parser-state syntax)
  (when (looking-at "[ \t]+")
    (lp:forward-match))
  (if (eolp)
      (values parser-state nil nil)
      (let ((marker (point-marker)))
        (looking-at "\\S-+")
        (lp:forward-match)
        (values parser-state
                (list (make-instance 'lp:lexeme
                                     :marker marker
                                     :size (- (point) marker)))
                (not (eolp))))))

;;;
;;; Parse functions
;;;

(defun lp:parse (syntax &optional first-parser-state end-position)
  "Parse lines in current buffer from point up to `end-position'.
Return three values: the first parse line, the last parse
line (i.e. both ends of double linked parse line list.), and the
lexer state applicable to the following line.

Default values:
  parser-state (lp:default-parser-state syntax)
  end-position (point-max)"
  (let ((first-parser-state (or first-parser-state (lp:default-parser-state syntax)))
        (end-position (save-excursion
                        (goto-char (or end-position (point-max)))
                        (point-at-bol))))
    (loop with result = nil
          with first-line = nil
          for previous-line = nil then line
          for parser-state = first-parser-state then next-parser-state
          for marker = (point-marker)
          for (forms next-parser-state) = (lp:parse-line syntax parser-state)
          do (assert (= marker (point-at-bol))
                     nil
                     "lp:parse error: lp:parse-line outreached a line end (%d, %d)"
                     (marker-position marker) (point-at-bol)) ;; debug
          for line = (make-instance 'lp:line-parse
                                    :marker marker
                                    :previous-line previous-line
                                    :parser-state parser-state
                                    :forms forms)
          unless first-line do (setf first-line line)
          if previous-line do (set-slot-value previous-line 'next-line line)
          if (>= (point) end-position)
          ;; end position has been reached: stop parsing
          return (values first-line line next-parser-state)
          ;; otherwise go to next line
          do (forward-line 1))))

(defun lp:parse-line (syntax parser-state)
  "Return a form list, built by parsing current buffer starting
from current point up to the end of the current line."
  (loop for (new-parser-state forms continue)
        = (lp:lex (or parser-state (lp:default-parser-state syntax)) syntax)
        then (lp:lex new-parser-state syntax)
        nconc forms into result
        while continue
        finally return (values result new-parser-state)))

(defun lp:reparse-line (syntax line)
  "Perform a new parse of `line' (the surrounding context is
supposed to be unchanged)"
  (save-excursion
    (goto-char (lp:marker line))
    (forward-line 0)
    (let ((marker (point-marker)))
      (set-marker-insertion-type marker nil)
      (set-slot-value line 'marker marker))
    (multiple-value-bind (forms next-state)
        (lp:parse-line syntax (slot-value line 'parser-state))
      (set-slot-value line 'forms forms))))

;;;
;;; Parse search
;;;

(defun lp:find-lines (syntax position &optional length)
  "Search parse lines covering the region starting from
`position' and covering `length' (which defaults to 0). Return
two values: the first and the last parse line."
  ;; Compare the region position with the syntax current line (its
  ;; previously modified line, saved for quicker access), the first
  ;; line and the last line, to determine from which end start the
  ;; search.
  (let* ((beginning-position (save-excursion
                               (goto-char position)
                               (point-at-bol)))
         (end-position (if length
                           (save-excursion
                             (goto-char (+ position length))
                             (point-at-bol))
                           beginning-position)))
    (multiple-value-bind (search-type from-line)
        (let ((current-line (lp:current-line syntax)))
          (if current-line
              (let* ((point-0/4 (point-min))
                     (point-4/4 (point-max))
                     (point-2/4 (lp:marker current-line))
                     (point-1/4 (/ (- point-2/4 point-0/4) 2))
                     (point-3/4 (/ (+ point-2/4 point-4/4) 2)))
                (cond ((<= point-3/4 end-position)
                       (values 'backward (lp:last-line syntax)))
                      ((<= beginning-position point-1/4)
                       (values 'forward (lp:first-line syntax)))
                      ((and (<= point-2/4 beginning-position) (<= beginning-position point-3/4))
                       (values 'forward current-line))
                      ((and (<= point-1/4 end-position) (<= end-position point-2/4))
                       (values 'backward current-line))
                      (t ;; (<= point-1/4 beginning-position point-1/2 end-position point-3/4)
                       (values 'both current-line))))
              (let* ((point-1/2 (/ (+ (point-max) (point-min)) 2)))
                (if (>= end-position point-1/2)
                    (values 'backward (lp:last-line syntax))
                    (values 'forward (lp:first-line syntax))))))
      (case search-type
        ((forward) ;; forward search from `from-line'
         (loop with first-line = nil
               for line = from-line then (lp:next-line line)
               if (= (lp:marker line) beginning-position)
               do (setf first-line line)
               if (= (lp:marker line) end-position)
               return (values first-line line)))
        ((backward) ;; backward search from `from-line'
         (loop with last-line = nil
               for line = from-line then (lp:previous-line line)
               if (= (lp:marker line) end-position)
               do (setf last-line line)
               if (= (lp:marker line) beginning-position)
               return (values line last-line)))
        (t ;; search first line backward, and last-line forward from `from-line'
         (values (loop for line = from-line then (lp:previous-line line)
                       if (= (lp:marker line) beginning-position) return line)
                 (loop for line = from-line then (lp:next-line line)
                       if (= (lp:marker line) end-position) return line)))))))

(defun lp:find-line (syntax position)
  (let ((current-line (lp:current-line syntax)))
    (if (and current-line
             (= (lp:marker current-line)
                (save-excursion
                  (goto-char position)
                  (point-at-bol))))
        current-line
        (first (lp:find-lines syntax position)))))

;;;
;;; Parse update
;;;

(defmacro lp:without-parse-update (&rest body)
  `(let ((before-change-functions nil)
         (after-change-functions nil))
     ,@body))
(put 'lp:without-parse-update 'lisp-indent-function 0)

(defun lp:parse-and-highlight-buffer ()
  "Make a full parse of current buffer and highlight text.  Set
current syntax parse data (`first-line' and `last-line' slots)."
  (let ((syntax (lp:current-syntax)))
    (lp:without-parse-update
      ;; initialize the parse tree
      (save-excursion
        (goto-char (point-min))
        (multiple-value-bind (first last state) (lp:parse syntax)
          (set-slot-value syntax 'first-line first)
          (set-slot-value syntax 'last-line last)))
      ;; remove previous fontification
      (lp:unfontify syntax)
      ;; fontify the buffer
      (loop for line = (lp:first-line syntax)
            then (lp:next-line line)
            while line
            do (mapcar #'lp:fontify (lp:line-forms line))))))

(defun lp:update-line-if-different-parser-state (line parser-state syntax)
  (when (and line
           (not (lp:same-parser-state-p
                 parser-state
                 (slot-value line 'parser-state))))
    (multiple-value-bind (forms next-state)
        (lp:parse-line syntax parser-state)
      (set-slot-value line 'forms forms)
      (set-slot-value line 'parser-state parser-state)
      (lp:unfontify line)
      (lp:fontify line)
      (forward-line 1)
      (lp:update-line-if-different-parser-state (lp:next-line line) next-state syntax))))

(defun lp:before-parse-update (beginning end)
  "Find the modified parse lines, covering the region starting
from `beginning' to `end'.  Set the `first-modified-line' and
`last-modified-line' slots of the current syntax."
  (let ((syntax (lp:current-syntax)))
    (unless (lp:first-line syntax)
      (lp:parse-and-highlight-buffer))
    ;; find the portion of the parse-tree that needs an update
    (multiple-value-bind (first-modified-line last-modified-line)
        (lp:find-lines syntax beginning (- end beginning))
      (set-slot-value syntax 'first-modified-line first-modified-line)
      (set-slot-value syntax 'last-modified-line last-modified-line))))

(defun lp:parse-update (beginning end old-length)
  "Update current syntax parse-tree after a buffer modification,
and fontify the changed text.

  `beginning' is the beginning of the changed text.
  `end' is the end of the changed text.
  `length' is the length the pre-changed text."
  (save-match-data
    (save-excursion
      (let ((syntax (lp:current-syntax))
            (end-position (progn
                            (goto-char end)
                            (point-at-eol))))
        (goto-char beginning)
        (forward-line 0)
        (let ((first-modified-line (slot-value syntax 'first-modified-line))
              (last-modified-line (slot-value syntax 'last-modified-line)))
          ;; re-parse the modified lines
          (multiple-value-bind (first-new-line last-new-line next-state)
              (lp:parse syntax
                        (if first-modified-line
                            (slot-value first-modified-line 'parser-state)
                            (slot-value syntax 'default-parser-state))
                        end-position)
            ;; fontify new lines
            (loop for line = first-new-line then (lp:next-line line)
                  while line
                  do (lp:unfontify line)
                  do (lp:fontify line))
            ;; replace the old lines with the new ones in the
            ;; double-linked list
            (if (or (not first-modified-line)
                    (eql (lp:first-line syntax) first-modified-line))
                (set-slot-value syntax 'first-line first-new-line)
                (lp:link-lines (lp:previous-line first-modified-line)
                               first-new-line))
            (if (or (not last-modified-line)
                    (eql (lp:last-line syntax) last-modified-line))
                (set-slot-value syntax 'last-line last-new-line)
                (lp:link-lines last-new-line
                               (lp:next-line last-modified-line)))
            ;; debug
            ;;(princ (format "old: [%s-%s] new: [%s-%s]"
            ;;               (marker-position (lp:marker first-modified-line))
            ;;               (marker-position (lp:marker last-modified-line))
            ;;               (marker-position (lp:marker first-new-line))
            ;;               (marker-position (lp:marker last-new-line))))
            ;; Update the syntax `current-line', from quick access
            (set-slot-value syntax 'current-line last-new-line)
            (set-slot-value syntax 'first-modified-line nil)
            (set-slot-value syntax 'last-modified-line nil)
            ;; If the lexer state at the end of last-new-line is
            ;; different from the lexer state at the beginning of
            ;; the next line, then parse next line again (and so
            ;; on)
            (lp:update-line-if-different-parser-state
             (lp:next-line last-new-line) next-state syntax)))))))

;;;
;;; Fontification
;;;

(defun lp:fontify-region (start end face)
  (let ((overlay (make-overlay start end nil t nil)))
    (overlay-put overlay 'face face)))

(defgeneric lp:unfontify (thing)
  "Remove all fontification from `thing'")

(defmethod lp:unfontify ((this lp:line-parse))
  (remove-overlays (lp:marker this) (lp:line-end-position this)))

(defmethod lp:unfontify ((this lp:syntax))
  (remove-overlays))

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
      (lp:fontify-region start end (lp:face this)))))

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