;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lyqi-syntax)
(require 'lyqi-fontify)

;;;
;;; Parse update
;;;

(defun lyqi:parse-update (beginning end length)
  "Update `lyqi:*lilypond-syntax*' parse-tree after a buffer
modification, and fontify the changed text.
  `beginning' is the beginning of the changed text.
  `end' is the end of the changed text.
  `length' is the length the pre-changed text."
  (cond ((not (lyqi:parse-tree lyqi:*lilypond-syntax*))
         ;; parse the whole buffer
         (save-excursion
           (goto-char (point-min))
           (set-slot-value lyqi:*lilypond-syntax* 'parse-tree
                           (lyqi:parse lyqi:*lilypond-syntax*))))
        (t
         ;; find the portion of the parse-tree that needs an update
         (let ((parse-tree (lyqi:parse-tree lyqi:*lilypond-syntax*))
               (old-end (+ beginning length)))
           (multiple-value-bind (lines-before-modification
                                 line-after-modification)
               (loop for lines on parse-tree
                     for forms = (first lines)
                     with lines-before-modification = nil
                     with line-after-modification = nil
                     if (< (lyqi:marker (first forms)) beginning)
                     do (setf lines-before-modification (cdr lines))
                     and return (values lines-before-modification
                                        line-after-modification)
                     if (> (lyqi:marker (first forms)) old-end)
                     do (setf line-after-modification lines)
                     finally return (values lines-before-modification
                                            line-after-modification))
             (save-excursion
               (let ((end-position (progn
                                     (goto-char end)
                                     (point-at-eol))))
                 (goto-char beginning)
                 (forward-line 0)
                 (let ((new-lines (lyqi:parse lyqi:*lilypond-syntax* end-position)))
                   ;; update presentation of new lines
                   (loop for forms in new-lines
                         do (mapcar #'lyqi:fontify forms))
                   (set-slot-value lyqi:*lilypond-syntax* 'parse-tree
                                   (cond ((and line-after-modification
                                               lines-before-modification)
                                          (setcdr line-after-modification new-lines)
                                          (nconc parse-tree lines-before-modification))
                                         (line-after-modification
                                          (setcdr line-after-modification new-lines)
                                          parse-tree)
                                         (lines-before-modification
                                          (nconc new-lines lines-before-modification))
                                         (t
                                          new-lines)))))))))))

(provide 'lyqi-parse-update)
