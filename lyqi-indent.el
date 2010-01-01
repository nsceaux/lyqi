;;; lyqi-indent.el
;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(require 'lp-base)
(require 'lyqi-syntax)

(defvar lyqi:indent-level 2)

(defun lyqi:offset-from-bol (token)
  "Return the number of columns from the beginning of the line
where `token' is placed, to `token'"
  (save-excursion
    (goto-char (lp:marker token))
    (forward-line 0)
    (- (lp:marker token) (point))))

(defun lyqi:indent-line ()
  "Indent current line."
  (interactive)
  (lp:without-parse-update
    (let (final-position)
      (save-excursion
        (forward-line 0)
        (let* ((syntax (lp:current-syntax))
               (line (first (lp:find-lines syntax (point-marker)))))
          (indent-line-to (max 0
                               (if (or (lyqi:scheme-state-p (slot-value line 'parser-state))
                                       (and (lp:line-forms line)
                                            (object-of-class-p (first (lp:line-forms line))
                                                               'lyqi:embedded-lilypond-end-lexeme)))
                                   (lyqi:scheme-line-indentation line)
                                   (lyqi:lilypond-line-indentation line))))
          ;; reparse the line
          ;; FIXME: why markers are not automatically updated?
          (lp:reparse-line syntax line)
          (setf final-position (point))))
      (when (< (point) final-position)
        (goto-char final-position)))))

;; (defun lyqi:indent-region (start end)
;;   (save-excursion
;;     (loop with prev-lilypond-line = nil
;;           with prev-embedded-lilypond-line = nil
;;           with prev-scheme-line = nil
;;           with prev-embedded-scheme-line = nil
;;           with (first last) = (lp:find-lines (lp:current-syntax) start end)
;;           for line = first then (lp:next-line line)
;;           while (not (eql line
;;           then (lp:next-line line)
          
;;         for line-forms = (lp:line-forms line)
;;         for this-indent = nil then (if (and forms (lp:closing-delimiter-p (first forms)))
;;                                        (if opened-blocks
;;                                            (lyqi:offset-from-bol (caar opened-blocks))
;;                                            (- this-indent lyqi:indent-region))
;;                                        next-indent)
;;         for next-indent = (loop for forms on line-forms
;;                                 for form = (first forms)
;;                                 if (lp:opening-delimiter-p form)
;;                                 do (push (cons form (second forms)) opened-blocks)
;;                                 else if (lp:closing-delimiter-p form)
;;                                 do (if opened-blocks
;;                                        (pop opened-blocks)
;;                                        (push form closed-blocks))
;;                                 finally return
;;                                 (cond ((and opened-blocks (cdar opened-blocks))
;;                                        (lyqi:offset-from-bol (cdar opened-blocks)))
;;                                       ((or opened-blocks closed-blocks)
;;                                        (+ this-indent
;;                                           (* lyqi:indent-level
;;                                              (- (length opened-blocks)
;;                                                 (length closed-blocks)))))
;;                                       (t this-indent)))
;;         do ...)))

(defun lyqi:sexp-beginning (indent-line)
  "Return two values: the opening parenthesis and the first forms,
if any, of the s-expression containing `indent-line' beginning."
  (loop with depth = 0
        with sexp-forms = nil
        with in-scheme = t
        for (form line rest-forms) = (lp:previous-form indent-line)
        then (lp:previous-form line rest-forms)
        while form
        if (object-of-class-p form 'lyqi:embedded-lilypond-end-lexeme)
        do (setf in-scheme nil)
        if (object-of-class-p form 'lyqi:embedded-lilypond-start-lexeme)
        do (setf in-scheme t)
        if (and in-scheme (lp:closing-delimiter-p form))
        do (incf depth)
        else if (and in-scheme (lp:opening-delimiter-p form))
        do (cond ((> depth 1)
                  (decf depth))
                 ((= depth 1)
                  (decf depth)
                  (push form sexp-forms))
                 (t
                  ;; s-expr starting left parenthesis is found
                  (return (values form sexp-forms))))
        else if (and in-scheme (= depth 0))
        do (push form sexp-forms)))

(defun lyqi:scheme-operator-special-arg-number (operator-form first-args indent-line-first-form)
  "Tell how many special argument (which indentation is different
from other arguments) an operator has.  For instance, `lambda' has
one special argument (its lambda list):
  (lambda (args...)
    ..body..)
For a function, return NIL: all arguments shall be treated as
regular function arguments.
For a macro with no special argument (like `begin'), return 0.
The result for a macro may depend on its arguments, for instance in the
case of optional keyword arguments."
  (let* ((string (lp:string operator-form))
         (symbol (intern string)))
    (case symbol
      ((define-markup-command) 6)
      ((lambda let define) 1)
      ((begin) 0)
      (t nil))))

(defun lyqi:scheme-line-indentation (indent-line)
  "Compute the indentation of a scheme line.  Returns a number of
spaces from the beginning of the line.

`indent-line': line-parse object corresponding to the line to be
indented."
  (multiple-value-bind (sexp-paren sexp-forms)
      (lyqi:sexp-beginning indent-line)
    (if (first sexp-forms)
        ;; line to be indented starts with a function or macro
        ;; argument, or with an element of a literal list (but not
        ;; the first one)
        (let* ((first-form (first sexp-forms))
               (operator-form (and (object-of-class-p first-form 'lyqi:scheme-symbol-lexeme)
                                   first-form))
               (arg-forms (rest sexp-forms))
               (special-args (and operator-form
                                  (lyqi:scheme-operator-special-arg-number
                                   operator-form
                                   arg-forms
                                   (first (lp:line-forms indent-line)))))
               (next-arg-is-special (and special-args
                                         (> (- special-args (length arg-forms)) 0))))
          (cond (next-arg-is-special
                 (+ (lyqi:offset-from-bol sexp-paren) (* 2 lyqi:indent-level)))
                (special-args
                 (+ (lyqi:offset-from-bol sexp-paren) lyqi:indent-level))
                (arg-forms
                 (lyqi:offset-from-bol (first arg-forms)))
                (operator-form
                 (lyqi:offset-from-bol operator-form))
                (t
                 (+ (lyqi:offset-from-bol sexp-paren) (lp:size sexp-paren)))))
        ;; line to be indented starts with the list first form
        (+ (lyqi:offset-from-bol sexp-paren) (lp:size sexp-paren)))))

(defun lyqi:line-start-with-closing-delimiter (line)
  (and (lp:line-forms line)
       (lp:closing-delimiter-p (first (lp:line-forms line)))))

;; As a start, make things easy: use previous non-empty line
;; indentation, and account for nesting level compared to the
;; beginning of this line
(defun lyqi:lilypond-line-indentation (indent-line)
  (loop with nesting-level = (if (lyqi:line-start-with-closing-delimiter indent-line)
                                 -1
                                 0)
        with embedded-lilypond = (lyqi:embedded-lilypond-state-p (slot-value indent-line 'parser-state))
        with in-lilypond = t
        for line = (lp:previous-line indent-line) then (lp:previous-line line)
        while line
        for prev-indent = (loop for forms on (reverse (lp:line-forms line))
                                for form = (first forms)
                                for is-last-form = (not (rest forms))
                                if (and embedded-lilypond
                                        (object-of-class-p form 'lyqi:embedded-lilypond-start-lexeme))
                                return lyqi:indent-level
                                if (and in-lilypond (object-of-class-p form 'lyqi:scheme-lexeme))
                                do (setf in-lilypond nil)
                                if (and (not in-lilypond) (object-of-class-p form 'lyqi:sharp-lexeme))
                                do (setf in-lilypond t)
                                if (and in-lilypond (lp:opening-delimiter-p form))
                                do (incf nesting-level)
                                if (and in-lilypond (lp:closing-delimiter-p form) (not is-last-form))
                                do (decf nesting-level)
                                if (and in-lilypond is-last-form)
                                return (lyqi:offset-from-bol form))
        if prev-indent return (+ prev-indent
                                 (* nesting-level lyqi:indent-level))))

(provide 'lyqi-indent)
