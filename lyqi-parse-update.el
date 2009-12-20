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

(defun lyqi:parse-and-highlight-buffer ()
  "Make a full parse of current buffer and highlight text.
Set `lyqi:*lilypond-syntax*' parse data (`first-line' and
`last-line' slots)."
  ;; prevent after-change-functions (i.e. lyqi:parse-update)
  ;; from being called
  (let ((after-change-functions nil))
    ;; initialize the parse tree
    (save-excursion
      (goto-char (point-min))
      (multiple-value-bind (first last) (lyqi:parse lyqi:*lilypond-syntax*)
        (set-slot-value lyqi:*lilypond-syntax* 'first-line first)
        (set-slot-value lyqi:*lilypond-syntax* 'last-line last)))
    ;; fontify the buffer
    (loop for line = (lyqi:first-line lyqi:*lilypond-syntax*)
          then (lyqi:next-line line)
          while line
          do (mapcar #'lyqi:fontify (lyqi:line-forms line)))))

(defun lyqi:parse-update (beginning end old-length)
  "Update `lyqi:*lilypond-syntax*' parse-tree after a buffer
modification, and fontify the changed text.
  `beginning' is the beginning of the changed text.
  `end' is the end of the changed text.
  `length' is the length the pre-changed text."
  (cond ((not (lyqi:first-line lyqi:*lilypond-syntax*))
         (lyqi:parse-and-highlight-buffer))
        (t
         ;; find the portion of the parse-tree that needs an update
         (let ((first (lyqi:first-line lyqi:*lilypond-syntax*))
               (last (lyqi:last-line lyqi:*lilypond-syntax*))
               (old-end (+ beginning old-length)))
           (multiple-value-bind (first-modified-line last-modified-line)
               ;; TODO: from top or from bottom, depending on modified region
               ;; from bottom:
               ;; the first modified line is found as soon as
               ;; (<= (lyqi-marker line) beginning)
               ;; the last modified line is found as soon as
               ;; (< (lyqi:marker line) eld-end)
               (loop for line = (lyqi:last-line lyqi:*lilypond-syntax*)
                     then (lyqi:previous-line line)
                     with first-modified-line = nil
                     with last-modified-line = nil
                     if (<= (lyqi:marker line) beginning)
                     do (setf first-modified-line line)
                     if (< (lyqi:marker line) old-end)
                     do (setf last-modified-line line)
                     and return (values first-modified-line last-modified-line)
                     finally return (values first-modified-line last-modified-line))
             (save-excursion
               (let ((end-position (progn
                                     (goto-char end)
                                     (point-at-eol))))
                 (goto-char beginning)
                 (forward-line 0)
                 (multiple-value-bind (first-new-line last-new-line)
                     (lyqi:parse lyqi:*lilypond-syntax*
                                 :lexer-state (slot-value first-modified-line 'lexer-state)
                                 :end-position end-position)
                   ;; update fontification of new lines
                   (loop for line = first-new-line then (lyqi:next-line line)
                         while line
                         do (mapcar #'lyqi:fontify (lyqi:line-forms line)))
                   ;; replace the old lines with the new ones
                   (if (eql (lyqi:first-line lyqi:*lilypond-syntax*)
                            first-modified-line)
                       (set-slot-value lyqi:*lilypond-syntax*
                                       'first-line
                                       first-new-line)
                       (lyqi:link-lines (lyqi:previous-line first-modified-line)
                                        first-new-line))
                   (if (eql (lyqi:last-line lyqi:*lilypond-syntax*)
                            last-modified-line)
                       (set-slot-value lyqi:*lilypond-syntax*
                                       'last-line
                                       last-new-line)
                       (lyqi:link-lines last-new-line
                                        (lyqi:next-line last-modified-line)))))))))))

(provide 'lyqi-parse-update)
