;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation commands
;;;

(require 'lp-base)
(require 'lyqi-syntax)

(defvar lyqi:indent-level 2)

(defun lyqi:offset-from-bol (token)
  "Return the number of columns from the beginning of the line
where `token' is placed, to `token'"
  (save-excursion
    (goto-char (lp:marker token))
    (- (point) (point-at-bol))))

(defun lyqi:indent-line ()
  "Indent current line."
  (interactive)
  (lp:without-parse-update
    (let (final-position)
      (save-excursion
        (forward-line 0)
        (let* ((syntax (lp:current-syntax))
               (line (first (lp:find-lines syntax (point-marker)))))
          (indent-line-to
           (max 0
                (cond ((bobp) 0)
                      ((or (lyqi:scheme-state-p (slot-value line 'parser-state))
                           (and (lp:line-forms line)
                                (object-of-class-p (first (lp:line-forms line))
                                                   'lyqi:embedded-lilypond-end-lexeme)))
                       (lyqi:scheme-line-indentation line))
                      (t (lyqi:lilypond-line-indentation line)))))
          (setf final-position (point))
          ;; reparse the line
          ;; FIXME
          (lp:reparse-line syntax line)))
      (when (< (point) final-position)
        (goto-char final-position)))))

(defun lyqi:indent-region (start end)
  (save-excursion
    (goto-char end)
    (let ((end-position (point-at-bol))
          (endmark (copy-marker end)))
      (goto-char start)
      (forward-line 0)
      (loop for pos = (point)
            while (<= pos end-position)
            if (and (bolp) (not (eolp))) do (lyqi:indent-line)
            do (forward-line 1))
      (set-marker endmark nil))))

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

(defun lyqi:scheme-line-indentation (indent-line)
  "Compute the indentation of a scheme line.  Returns a number of
spaces from the beginning of the line.

`indent-line': line-parse object corresponding to the line to be
indented."
  (multiple-value-bind (sexp-paren sexp-forms)
      (lyqi:sexp-beginning indent-line)
    ;; TODO: indent-line starts with a closing paren
    (if (first sexp-forms)
        ;; line to be indented starts with a function or macro
        ;; argument, or with an element of a literal list (but not
        ;; the first one)
        (let* ((first-form (first sexp-forms))
               (operator-form (and (object-of-class-p first-form 'lyqi:scheme-symbol-lexeme)
                                   first-form))
               (arg-forms (rest sexp-forms))
               (special-args (and operator-form
                                  (slot-value operator-form 'special-args)))
               (is-special (second special-args))
               (nb-special (first special-args))
               (next-arg-is-special (and is-special
                                         (or (eql nb-special t) ;; all specials
                                             (> (- nb-special (length arg-forms)) 0)))))
          (cond (next-arg-is-special
                 (+ (lyqi:offset-from-bol sexp-paren) (* 2 lyqi:indent-level)))
                (is-special
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
  (lyqi:calc-lilypond-line-indentation
   (lp:previous-line indent-line)
   (lyqi:embedded-lilypond-state-p (slot-value indent-line 'parser-state))
   (if (lyqi:line-start-with-closing-delimiter indent-line)
       -1
       0)
   t))

(defun lyqi:calc-lilypond-line-indentation (line embedded-lilypond nesting-level in-lilypond)
  (let* ((new-nesting-level nesting-level)
         (new-in-lilypond in-lilypond)
         (line-indent (loop for forms on (reverse (lp:line-forms line))
                            for previous-form = nil then form
                            for form = (first forms)
                            for is-line-first-form = (not (rest forms))
                            if (and embedded-lilypond
                                    (object-of-class-p form 'lyqi:embedded-lilypond-start-lexeme))
                            ;; #{ token starting the embedded lilypond block
                            ;; that means that the line to be indented is a line of
                            ;; embedded lilypond, following a line with #{
                            ;; if #{ is the first token on its line, align next line
                            ;; with it. Otherwise, use an arbitrary lyqi:indent-level
                            ;; indentation.
                            return (cond ((and is-line-first-form previous-form)
                                          (lyqi:offset-from-bol previous-form))
                                         (is-line-first-form
                                          (+ (lyqi:offset-from-bol form)
                                             (lp:size form)
                                             1))
                                         (t
                                          lyqi:indent-level))
                            else if new-in-lilypond
                            do (cond ((object-of-class-p form 'lyqi:scheme-lexeme)
                                      (setf new-in-lilypond nil))
                                     ((lp:opening-delimiter-p form)
                                      (incf new-nesting-level))
                                     ((and (lp:closing-delimiter-p form) (not is-line-first-form))
                                      (decf new-nesting-level)))
                            else if (object-of-class-p form 'lyqi:sharp-lexeme)
                            do (setf new-in-lilypond t)
                            ;; when reaching the first token on line
                            ;; if in lilypond code, then return the line indent
                            if is-line-first-form return (and new-in-lilypond (lyqi:offset-from-bol form)))))
    (cond (line-indent
           (+ line-indent (* new-nesting-level lyqi:indent-level)))
          ((lp:previous-line line)
           (lyqi:calc-lilypond-line-indentation (lp:previous-line line)
                                                embedded-lilypond
                                                new-nesting-level
                                                new-in-lilypond))
          (t
           0))))
        

(provide 'lyqi-indent)
