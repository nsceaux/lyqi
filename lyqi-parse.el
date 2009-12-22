;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lyqi-syntax)
(require 'lyqi-fontify)

;;; Buffer local syntax object
(defvar lyqi:*lilypond-syntax* nil)

;;; The parse tree: a double-linked list of lines, containing forms
;;; parsed on each line.
(defclass lyqi:line-parse ()
  ((marker :initarg :marker
           :accessor lyqi:marker)
   (forms :initarg :forms
          :accessor lyqi:line-forms)
   (lexer-state :initform nil
                :initarg :lexer-state)
   (previous-line :initform nil
                  :initarg :previous-line
                  :accessor lyqi:previous-line)
   (next-line :initform nil
              :initarg :next-line
              :accessor lyqi:next-line)))

(defmethod object-print ((this lyqi:line-parse) &rest strings)
  (let* ((marker (lyqi:marker this))
         (start (and marker (marker-position marker))))
    (format "#<%s [%s]>"
            (object-class this)
            (or start "?"))))

(defun lyqi:link-lines (first next)
  (when first
    (set-slot-value first 'next-line next))
  (when next
    (set-slot-value next 'previous-line first)))

;;; Parse functions
(defun lyqi:parse (syntax &rest cl-keys)
  "Parse lines in current buffer from point up to `end-position'.
Return three values: the first parse line, the last parse
line (i.e. both ends of double linked parse line list.), and the
lexer state applicable to the following line.

Keywords supported:
  :lexer-state lyqi:*lexer-toplevel-state*
  :end-position (point-max)"
  (cl-parsing-keywords ((:lexer-state lyqi:*lexer-toplevel-state*)
                        (:end-position (point-max))) ()
    (loop with result = nil
          with first-line = nil
          for previous-line = nil then line
          for state = cl-lexer-state then next-state
          for marker = (point-marker)
          for (forms next-state) = (lyqi:parse-line syntax state)
          for line = (make-instance 'lyqi:line-parse
                                    :marker marker
                                    :previous-line previous-line
                                    :lexer-state state
                                    :forms forms)
          unless first-line do (setf first-line line)
          if previous-line do (set-slot-value previous-line 'next-line line)
          do (forward-line 1) ;; go to next-line
          if (>= (point) cl-end-position) return (values first-line line next-state))))

(defun lyqi:parse-line (syntax state)
  "Return a form list, built by parsing current buffer starting
from current point up to the end of the current line."
  (loop with end-point = (point-at-eol)
        for finished = nil then (>= (point) end-point)
        for (new-state form parse-data continue)
        = (lyqi:lex (or state lyqi:*lexer-toplevel-state*) syntax nil)
        then (lyqi:lex new-state syntax parse-data)
        if form collect form into result
        while continue
        finally return (values result new-state)))


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
      (multiple-value-bind (first last state) (lyqi:parse lyqi:*lilypond-syntax*)
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
         (multiple-value-bind (first-modified-line last-modified-line)
             ;; TODO: from top or from bottom, depending on position
             ;; of the modified region
             ;; from bottom:
             ;; the first modified line is found as soon as
             ;; (<= (lyqi-marker line) beginning)
             ;; the last modified line is found as soon as
             ;; (< (lyqi:marker line) old-end)
             (loop for line = (lyqi:last-line lyqi:*lilypond-syntax*)
                   then (lyqi:previous-line line)
                   with old-end = (+ beginning old-length)
                   with first-modified-line = nil
                   with last-modified-line = nil
                   if (and (not last-modified-line)
                           (< (lyqi:marker line) old-end))
                   do (setf last-modified-line line)
                   if (<= (lyqi:marker line) beginning)
                   do (setf first-modified-line line)
                   and return (values first-modified-line last-modified-line)
                   finally return (values first-modified-line last-modified-line))
           (save-excursion
             (let ((end-position (progn
                                   (goto-char end)
                                   (point-at-eol))))
               (goto-char beginning)
               (forward-line 0)
               (multiple-value-bind (first-new-line last-new-line next-state0)
                   (lyqi:parse lyqi:*lilypond-syntax*
                               :lexer-state (slot-value first-modified-line 'lexer-state)
                               :end-position end-position)
                 ;; fontify new lines
                 (loop for line = first-new-line then (lyqi:next-line line)
                       while line
                       do (mapcar #'lyqi:fontify (lyqi:line-forms line)))
                 ;; replace the old lines with the new ones in the
                 ;; double-linked list
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
                                      (lyqi:next-line last-modified-line)))
                 ;; If the lexer state at the end of last-new-line is
                 ;; different from the lexer state at the beginning of
                 ;; the next line, then parse next line again (and so
                 ;; on)
                 (loop for line = (lyqi:next-line last-new-line)
                       then (lyqi:next-line line)
                       for new-state = next-state0 then next-state
                       while (and line (not (eql new-state (slot-value line 'lexer-state))))
                       for (forms next-state) = (lyqi:parse-line lyqi:*lilypond-syntax* new-state)
                       do (progn
                            (set-slot-value line 'forms forms)
                            (set-slot-value line 'lexer-state new-state)
                            (lyqi:fontify line)
                            (forward-line 1)))
                 ;; debug
                 (princ (format "Parse update [%s-%s] [%s-%s]"
                                beginning end
                                (marker-position (lyqi:marker first-modified-line))
                                (marker-position (lyqi:marker last-modified-line))))
                 )))))))

(provide 'lyqi-parse)
