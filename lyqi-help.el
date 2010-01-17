;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Help
;;;
(require 'lyqi-help-index)

(defun lyqi:help (entry)
  (interactive
   (list (completing-read "Entry:" lyqi:help-index nil t)))
  (lyqi:help-describe-entry entry))

(defun lyqi:help-describe-entry (entry)
  (save-excursion
    (with-output-to-temp-buffer "*Help*"
      (princ (format "%s\n\n" entry))
      (princ (format "In LilyPond documentation:\n"))
      (let ((urls (loop with data = (cdr (assoc entry lyqi:help-index))
                        for (entry-link section section-link) in data
                        for entry-url = (format "%s/%s" lyqi:help-url-base entry-link)
                        do (princ (format "  `%s'\n" section))
                        collect entry-url)))
        (with-current-buffer standard-output
          (save-excursion
            (save-match-data
              (loop for match = (re-search-backward "`\\([^`']+\\)'" nil t)
                    for url in urls
                    while (and match url)
                    do (help-xref-button 1 'help-url url))))))
      (with-current-buffer standard-output
        ;; Return the text we displayed.
        (buffer-string)))))

(provide 'lyqi-help)
