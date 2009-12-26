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

(defun lyqi:indent-scheme-line (line)
  "Return the indentation of a line of Scheme code"
  0)

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
