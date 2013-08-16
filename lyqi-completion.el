;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;
(require 'lyqi-words)
(defvar lyqi:backslashed-words
  (sort (mapcar 'symbol-name (append lyqi:lilypond-keywords
                                     lyqi:lilypond-music-variables
                                     lyqi:lilypond-music-functions
                                     lyqi:lilypond-markup-commands
                                     lyqi:lilypond-markup-list-commands))
        'string-lessp))

(defvar lyqi:scheme-words
  (sort (mapcar 'symbol-name (append lyqi:scheme-lily-procedures
                                     lyqi:scheme-lily-variables
                                     lyqi:scheme-lily-macros
                                     lyqi:scheme-guile-procedures
                                     lyqi:scheme-guile-macros))
        'string-lessp))

(defclass lyqi:abbrev ()
  ((abbrev :initarg :abbrev)
   (collection :initarg :collection)
   (start-position :initarg :start-position)))

(defgeneric lyqi:form-abbrev (form)
  "Return the completion data of form, or NIL is form cannot be completed.")

(defmethod lyqi:form-abbrev ((this lp:parser-symbol))
  nil)

(defmethod lyqi:form-abbrev ((this lyqi:backslashed-lexeme))
  (multiple-value-bind (start abbrev)
      (let ((string (lp:string this)))
        (string-match ".*\\\\\\(.*\\)" string)
        (values (+ (match-beginning 1) (lp:marker this))
                (match-string 1 string)))
    (make-instance 'lyqi:abbrev
                   :abbrev abbrev
                   :collection lyqi:backslashed-words
                   :start-position start)))

(defmethod lyqi:form-abbrev ((this lyqi:scheme-symbol-lexeme))
  (make-instance 'lyqi:abbrev
                 :abbrev (lp:string this)
                 :collection lyqi:scheme-words
                 :start-position (lp:marker this)))

(defmethod lyqi:complete-abbrev ((this lyqi:abbrev))
  (let* ((abbrev (slot-value this 'abbrev))
         (collection (slot-value this 'collection))
         (completion (try-completion abbrev collection)))
    (cond ((not completion) ;; no completion
           nil)
          ((eql completion t) ;; already complete
           nil)
          ((string-equal abbrev completion) ;; propose completions
           (save-excursion
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list (all-completions abbrev collection) abbrev)))
           t)
          (t ;; expand abbrev
           (choose-completion-string completion
                                     (current-buffer)
                                     (list (slot-value this 'start-position)))
           t))))

(defun lyqi:complete-word ()
  (interactive)
  (multiple-value-bind (form line rest-forms)
      (lp:form-before-point (lp:current-syntax) (point))
    (when (and form (= (point) (+ (lp:marker form) (lp:size form))))
      (let ((abbrev (lyqi:form-abbrev form)))
        (when abbrev
          (lyqi:complete-abbrev abbrev))))))

(defun lyqi:complete-or-indent ()
  (interactive)
  (or (lyqi:complete-word)
      (lyqi:indent-line)))

(provide 'lyqi-completion)
