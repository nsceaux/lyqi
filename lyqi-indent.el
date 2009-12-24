;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(defun lyqi:indent-line ()
  "Indent current line."
  (interactive)
  (let ((line (first (lp:find-lines (lp:current-syntax) (point)))))
    ;; TODO: if line starts with a closing delimiter, find previous
    ;; opening delimiter
    (let* ((previous-line (loop for previous-line = (lp:previous-line line)
                                then (lp:previous-line previous-line)
                                while previous-line
                                if (lp:line-forms previous-line) return previous-line))
           (previous-indent (if previous-line
                                (- (lp:marker (first (lp:line-forms previous-line)))
                                   (lp:marker previous-line))
                                0))
           (new-indent
            (max 0
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
                       finally return (cond ((and following-forms (first following-forms))
                                             ;; return the offset of the token
                                             ;; following the last non closed
                                             ;; opening delimiter
                                             (- (lp:marker (first following-forms))
                                                (lp:marker previous-line)))
                                            (opening-delimiters
                                             ;; previous line ends with an opening
                                             ;; delimiter
                                             (+ previous-indent
                                                (* 2
                                                   (- (length opening-delimiters)
                                                      (length closing-delimiters)))))
                                            (closing-delimiters
                                             (- previous-indent
                                                (* 2 (length closing-delimiters))))
                                            (t previous-indent))))))
      (lp:without-parse-update
        (save-excursion
          (forward-line 0)
          (indent-line-to new-indent))))))

(provide 'lyqi-indent)
