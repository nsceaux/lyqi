;;; lyqi-indent.el
;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(require 'lp-base)

(defvar lyqi:indent-level 2)

(defun lyqi:indent-lilypond-line (line)
  "Return the indentation of a line of LilyPond code"
  (let* ((previous-line (loop for previous-line = (lp:previous-line line)
                              then (lp:previous-line previous-line)
                              while previous-line
                              if (lp:line-forms previous-line) return previous-line))
         (previous-indent (if previous-line
                              (save-excursion
                                (goto-char (lp:marker previous-line))
                                (forward-line 0)
                                (skip-chars-forward " \t"))
                              0)))
    (max 0
         (cond ((and (lp:line-forms line)
                     (lp:closing-delimiter-p (first (lp:line-forms line))))
                ;; `line' starts with a closing delimiter.  If the
                ;; matching opening delimiter is on previous line,
                ;; then use the same indentation as previous line.
                ;; Otherwise, decrease indentation.
                (loop for form in (and previous-line
                                       (reverse (lp:line-forms previous-line)))
                      if (lp:opening-delimiter-p form) return previous-indent
                      finally return (- previous-indent lyqi:indent-level)))
               (previous-line
                (loop with all-forms = (lp:line-forms previous-line)
                      with first-form = (first all-forms)
                      with opening-delimiters = nil
                      with closing-delimiters = nil
                      with following-forms = nil
                      for forms on all-forms
                      for form = (first forms)
                      if (lp:opening-delimiter-p form)
                      do (progn
                           (push form opening-delimiters)
                           (push (second forms) following-forms))
                      if (lp:closing-delimiter-p form) 
                      do (cond (opening-delimiters
                                (pop opening-delimiters)
                                (pop following-forms))
                               ((not (eql form first-form))
                                (push form closing-delimiters)))
                      finally return
                      (cond ((and following-forms (first following-forms))
                             ;; return the offset of the token following
                             ;; the last non closed opening delimiter
                             (- (lp:marker (first following-forms))
                                (lp:marker previous-line)))
                            (opening-delimiters
                             ;; previous line ends with an opening
                             ;; delimiter
                             (+ previous-indent
                                (* lyqi:indent-level
                                   (- (length opening-delimiters)
                                      (length closing-delimiters)))))
                            (closing-delimiters
                             (- previous-indent
                                (* lyqi:indent-level (length closing-delimiters))))
                            (t previous-indent))))
               (t 0)))))

(defun lyqi:offset-from-bol (pos)
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (- pos (point))))

(defun lyqi:indent-scheme-line (indent-line)
  "Return the indentation of a line of Scheme code"
  ;; find opening ( enclosing the first token
  (loop with depth = 0
        with upper-paren = nil
        with in-embedded-lilypond = nil
        with previous-forms = nil
        for (form line rest-forms) = (lp:previous-form indent-line)
        then (lp:previous-form line rest-forms)
        while form
        if (object-of-class-p form 'lyqi:embedded-lilypond-end-lexeme)
        do (setf in-embedded-lilypond t)
        else if (object-of-class-p form 'lyqi:embedded-lilypond-start-lexeme)
        do (setf in-embedded-lilypond nil)
        else if (and (not in-embedded-lilypond) (lp:closing-delimiter-p form))
        do (incf depth)
        else if (and (not in-embedded-lilypond) (lp:opening-delimiter-p form))
        do (if (> depth 0)
               (decf depth)
               ;; opening paren is found
               (setf upper-paren form))
        else if (and (not in-embedded-lilypond)
                     (object-of-class-p form 'lyqi:sharp-lexeme))
        return (+ (lyqi:offset-from-bol (lp:marker form))
                  (lp:size form))
        ;; collect previous forms which have the same level as the
        ;; first token of the indent line
        if (and (not upper-paren)
                (= depth 0)
                (object-of-class-p form 'lyqi:scheme-lexeme))
        do (push form previous-forms)
        ;; TODO: take into account define, let, do, etc
        if upper-paren return (cond ((cdr previous-forms)
                                     (lyqi:offset-from-bol (lp:marker (second previous-forms))))
                                    (previous-forms
                                     (lyqi:offset-from-bol (lp:marker (first previous-forms))))
                                    (t
                                     (1+ (lyqi:offset-from-bol (lp:marker upper-paren)))))))

(defun lyqi:indent-line ()
  "Indent current line."
  (interactive)
  (let ((line (first (lp:find-lines (lp:current-syntax) (point)))))
    (lp:without-parse-update
      (let ((final-position (save-excursion
                              (forward-line 0)
                              (indent-line-to (if (lyqi:scheme-state-p (slot-value line 'parser-state))
                                                  (lyqi:indent-scheme-line line)
                                                  (lyqi:indent-lilypond-line line)))
                              (point))))
        (when (< (point) final-position)
          (goto-char final-position))))))

(provide 'lyqi-indent)
