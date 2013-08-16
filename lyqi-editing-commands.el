;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Editing commands
;;;

(require 'cl)
(require 'lp-base)
(require 'lyqi-pitchnames)
(require 'lyqi-syntax)
(require 'lyqi-midi)
(require 'lyqi-indent)

(defvar lyqi:*alterations* (make-vector 7 0))

(defmacro lyqi:save-excursion-unless-farther (&rest body)
  (let ((final-position (gensym "final-position"))
        (result (gensym "result")))
    `(let* (,final-position
            (,result (save-excursion
                       (prog1
                           (progn
                             ,@body)
                         (setf ,final-position (point))))))
       (when (< (point) ,final-position)
         (goto-char ,final-position)))))
(put 'lyqi:save-excursion-unless-farther 'lisp-indent-function 0)

;;;
;;; Commands on duration
;;; - change last duration
;;; - change last duration dot number
;;;
(defun lyqi:find-durations-backward (syntax position)
  "Find one duration (implicit or explicit) plus one explicit duration, backwards.
Return two values."
  (loop with duration1 = nil
        for (form current-line rest-forms) = (lp:form-before-point syntax position)
        then (lp:previous-form current-line rest-forms)
        while form
        for duration = (lyqi:form-with-duration-p form)
        if (and (not duration1) duration)
        do (setf duration1 duration)
        else if (and duration1 duration (lyqi:explicit-duration-p duration))
        return (values duration1 duration)
        finally return (values duration1 nil)))

(defun lyqi:same-duration-p (duration-number duration-object)
  "Test if `duration-object' is equal to `duration-number',
i.e. if (log duration 2) == length of `duration-object', and
duration object has no dot nor numerator nor denominator."
  (and (= (slot-value duration-object 'length)
          (round (log duration-number 2)))
       (= (slot-value duration-object 'dot-count) 0)
       (= (slot-value duration-object 'numerator) 1)
       (= (slot-value duration-object 'denominator) 1)))

(defun lyqi:change-duration (syntax duration)
  "Change the duration of the music found at or before point.
`duration': a number, e.g. 1 2 4 8 16 32 etc"
  (when (and (not (eolp))
             (eql (char-after (point)) ?\>)
             (not (eql (char-after (1+ (point))) ?\>)))
    (forward-char))
  (multiple-value-bind (current-duration previous-duration)
      (lyqi:find-durations-backward syntax (point))
    (when current-duration
      (lyqi:save-excursion-unless-farther
        (goto-char (lp:marker current-duration))
        (cond ((not (lyqi:explicit-duration-p current-duration))
               ;; if current duration is implicit, insert the new
               ;; duration
               (insert (number-to-string duration)))
              ((or (and previous-duration (lyqi:same-duration-p duration previous-duration))
                   (lyqi:same-duration-p duration current-duration))
               ;; if the new duration is equal to previous duration or
               ;; current duration, then just delete current duration
               (delete-char (lp:size current-duration)))
              (t
               ;; delete the current duration and insert the new one
               (combine-after-change-calls
                 (delete-char (lp:size current-duration))
                 (insert (number-to-string duration)))))))))

(defun lyqi:change-duration-1 ()
  "Change the duration of the music found at or before point to 1."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 1))

(defun lyqi:change-duration-2 ()
  "Change the duration of the music found at or before point to 2."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 2))

(defun lyqi:change-duration-4 ()
  "Change the duration of the music found at or before point to 4."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 4))

(defun lyqi:change-duration-8 ()
  "Change the duration of the music found at or before point to 8."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 8))

(defun lyqi:change-duration-16 ()
  "Change the duration of the music found at or before point to 16."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 16))

(defun lyqi:change-duration-32 ()
  "Change the duration of the music found at or before point to 32."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 32))

(defun lyqi:change-duration-64 ()
  "Change the duration of the music found at or before point to 64."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 64))

(defun lyqi:change-duration-128 ()
  "Change the duration of the music found at or before point to 128."
  (interactive)
  (lyqi:change-duration (lp:current-syntax) 128))

(defun lyqi:change-duration-dots ()
  "Increase the previous music duration dot number, modulo 5"
  (interactive)
  (let ((syntax (lp:current-syntax)))
    (multiple-value-bind (current-duration previous-duration)
        (lyqi:find-durations-backward syntax (point))
      (when current-duration
        (let ((explicit (lyqi:explicit-duration-p current-duration)))
          (multiple-value-bind (main-duration dot-count numerator denominator)
              (let ((ref-duration (if explicit
                                      current-duration
                                      previous-duration)))
                (if ref-duration
                    (values (car (rassoc (slot-value ref-duration 'length)
                                         (slot-value (lyqi:language syntax) 'duration-data)))
                            (mod (1+ (slot-value ref-duration 'dot-count)) 5)
                            (slot-value ref-duration 'numerator)
                            (slot-value ref-duration 'denominator))
                    (values "4" 1 1 1)))
            (lyqi:save-excursion-unless-farther
              (goto-char (lp:marker current-duration))
              (combine-after-change-calls
                (when explicit
                  (delete-char (lp:size current-duration)))
                (insert (concat main-duration
                                (make-string dot-count ?\.)
                                (cond ((and (= numerator 1) (= denominator 1))
                                       "")
                                      ((= denominator 1)
                                       (concat "*" (number-to-string numerator)))
                                      (t
                                       (concat "*" (number-to-string numerator)
                                               "/" (number-to-string denominator))))))))))))))

;;;
;;; Commands on rest, skip, etc
;;;
(defun lyqi:maybe-insert-space ()
  (unless (or (bolp)
              (= (char-before (point)) ?\ )
              (= (char-before (point)) ?\t)
              (= (char-before (point)) ?\<))
    (insert " ")))

(defun lyqi:maybe-insert-space-after ()
  (unless (or (eolp)
              (member (char-after)
                      '(?\- ?\_ ?\^ ?\( ?\) ?\[ ?\] ?\\ ?\1 ?\2 ?\4 ?\8 ?\3 ?\6 ?\> ?\ )))
    (insert " ")))

(defmacro lyqi:with-space-around (&rest body)
  `(progn
     (lyqi:maybe-insert-space)
     (prog1
         (progn
           ,@body)
       (lyqi:maybe-insert-space-after))))
(put 'lyqi:with-space-around 'lisp-indent-function 1)

(defun lyqi:insert-rest ()
  "Insert a rest at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:with-space-around
     (insert "r"))))

(defun lyqi:insert-mm-rest ()
  "Insert a rest at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:with-space-around
     (insert "R"))))

(defun lyqi:insert-spacer ()
  "Insert a skip at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:with-space-around
     (insert "s"))))

(defun lyqi:insert-chord-repetition ()
  "Insert a chord repetition at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:with-space-around
     (insert "q"))))

(defun lyqi:insert-skip ()
  "Insert a \\skip ar point"
  (interactive)
  (combine-after-change-calls
    (lyqi:with-space-around
     (insert "\\skip 4")))) ;; TODO: use previous duration?

;;;
;;; Commands on notes
;;; - insert a note
;;; - change a note octave
;;; - change a note alteration
;;;

(defun lyqi:find-note-backward (syntax position &optional stop-on-rests)
  "Find note backwards.  If `stop-on-rests' is not NIL, then stop
searching as soon as a rest, skip, etc is found."
  (loop for (form current-line rest-forms) = (lp:form-before-point syntax position)
        then (lp:previous-form current-line rest-forms)
        while form
        if (and stop-on-rests (lyqi:rest-skip-etc-form-p form)) return nil
        for note = (lyqi:form-with-note-p form)
        if note return note))

(defun lyqi:insert-note (syntax note play-note &optional insert-default-duration)
  (with-slots (pitch alteration octave-modifier accidental) note
    (let ((note-string (lyqi:pitchname (lyqi:language syntax) pitch alteration))
          (octave-string (cond ((= octave-modifier 0)
                                "")
                               ((> octave-modifier 0)
                                (make-string octave-modifier ?\'))
                               (t
                                (make-string (- octave-modifier) ?\,))))
          (accidental-string (case accidental
                               ((forced) "!")
                               ((cautionary) "?")
                               (t ""))))
      (insert (format "%s%s%s%s"
                      note-string octave-string accidental-string
                      (if insert-default-duration "4" "")))))
  (when play-note
    (lyqi:play-note note)))

(defun lyqi:re-insert-note (syntax note play-note)
  (goto-char (lp:marker note))
  (combine-after-change-calls
    (delete-char (lp:size note))
    (lyqi:insert-note syntax note play-note)))

(defun lyqi:insert-note-by-pitch (pitch)
  (let* ((syntax (lp:current-syntax))
         (previous-note (lyqi:find-note-backward syntax (point)))
         (new-note (if previous-note
                       (with-slots ((pitch0 pitch) (octave0 octave-modifier)) previous-note
                         (make-instance 'lyqi:note-mixin
                                        :pitch pitch
                                        :alteration (aref lyqi:*alterations* pitch)
                                        :octave-modifier (cond ((> (- pitch pitch0) 3) (1- octave0))
                                                               ((> (- pitch0 pitch) 3) (1+ octave0))
                                                               (t octave0))))
                       (make-instance 'lyqi:note-mixin
                                      :pitch pitch
                                      :alteration (aref lyqi:*alterations* pitch))))
         (previous-duration
          (multiple-value-bind (current-duration previous-duration)
              (lyqi:find-durations-backward syntax (point))
            current-duration)))
    (combine-after-change-calls
      (lyqi:with-space-around
       (lyqi:insert-note syntax new-note t (not previous-duration))))))

(defun lyqi:insert-note-c ()
  "Insert a c/do note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 0))

(defun lyqi:insert-note-d ()
  "Insert a d/re note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 1))

(defun lyqi:insert-note-e ()
  "Insert a e/mi note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 2))

(defun lyqi:insert-note-f ()
  "Insert a f/fa note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 3))

(defun lyqi:insert-note-g ()
  "Insert a g/sol note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 4))

(defun lyqi:insert-note-a ()
  "Insert a a/la note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 5))

(defun lyqi:insert-note-b ()
  "Insert a b/si note at point"
  (interactive)
  (lyqi:insert-note-by-pitch 6))

(defun lyqi:change-alteration (alteration-direction)
  (let* ((syntax (lp:current-syntax))
         (note (lyqi:find-note-backward syntax (point) t)))
    (when note
      (with-slots (pitch alteration) note
        (let ((new-alteration (cond ((> alteration-direction 0) ;; increment
                                     (cond ((= alteration -2) 0) ;;  flat -> neutral
                                           ((= alteration 0) 2) ;;  neutral -> sharp
                                           ((or (= alteration 2) ;;  sharp -> sharp
                                                (= alteration 4)) alteration)
                                           (t (1+ alteration))))
                                    ((< alteration-direction 0) ;; decrement
                                     (cond ((= alteration 2) 0) ;;  sharp -> neutral
                                           ((= alteration 0) -2) ;;  neutral -> flat
                                           ((or (= alteration -2) ;;  flat -> flat
                                                (= alteration -4)) alteration)
                                           (t (1- alteration))))
                                    (t 0)))) ;; neutral
          (when (/= alteration new-alteration)
            (set-slot-value note 'alteration new-alteration)
            ;; memorize alteration
            (aset lyqi:*alterations* pitch new-alteration)
            ;; delete old note, and insert new one
            (lyqi:save-excursion-unless-farther
              (lyqi:re-insert-note syntax note t))))))))

(defun lyqi:change-alteration-up ()
  "Increase the alteration of the note found at or before point:
flat -> neutral -> sharp"
  (interactive)
  (lyqi:change-alteration 1))

(defun lyqi:change-alteration-down ()
  "Decrease the alteration of the note found at or before point:
sharp -> neutral -> flat"
  (interactive)
  (lyqi:change-alteration -1))

(defun lyqi:change-alteration-neutral ()
  "Set the alteration of the note found at or before point to neutral"
  (interactive)
  (lyqi:change-alteration 0))

(defun lyqi:change-octave (octave-direction)
  (let* ((syntax (lp:current-syntax))
         (note (lyqi:find-note-backward syntax (point) t)))
    (when note
      (with-slots (octave-modifier) note
        (let ((new-octave (if (= 0 octave-direction)
                              0
                              (+ octave-direction octave-modifier))))
          (when (/= octave-modifier new-octave)
            (set-slot-value note 'octave-modifier new-octave)
            ;; delete old note, and insert new one
            (lyqi:save-excursion-unless-farther
              (lyqi:re-insert-note syntax note t))))))))

(defun lyqi:change-octave-up ()
  "Increase the octave of the note found at or before point"
  (interactive)
  (lyqi:change-octave 1))

(defun lyqi:change-octave-down ()
  "Decrease the octave of the note found at or before point"
  (interactive)
  (lyqi:change-octave -1))

(defun lyqi:change-octave-zero ()
  "Reset the octave of the note found at or before point"
  (interactive)
  (lyqi:change-octave 0))

;;;
;;; Transposition
;;;

(defun lyqi:transpose-note (note syntax note-diff alteration-diff)
  "Transpose `note'. Ex: (lyqi:transpose-note [do] -5 -2) -> [mib,]"
  (with-slots (pitch octave-modifier alteration) note
    (let* ((scale-up   [0 2 4 5 7 9 11])
           (scale-down [0 2 3 5 7 8 10])
           (new-pitch (mod (+ pitch note-diff) 7))
           (new-octave (+ octave-modifier
                          (cond ((< (+ pitch note-diff) 0)
                                 (/ (+ pitch note-diff -6) 7))
                                ((> (+ pitch note-diff) 6)
                                 (/ (+ pitch note-diff) 7))
                                (t 0))))
           (half-tones-from-C )
           (half-tones-from-pitch )
           (new-alteration (+ alteration
                              alteration-diff
                              (* 2 (- (aref (if (minusp note-diff)
                                                scale-down
                                                scale-up)
                                            (mod note-diff 7))
                                      (mod (- (aref scale-up new-pitch)
                                              (aref scale-up pitch))
                                           12))))))
      (set-slot-value note 'pitch new-pitch)
      (set-slot-value note 'alteration new-alteration)
      (set-slot-value note 'octave-modifier new-octave)))
  (lyqi:re-insert-note syntax note nil))

(defun lyqi:transpose-region (interval)
  "Transpose current region by `interval', a number possibly followed by `+' or `-'.
For instance:
  \"3\"  transpose a \"do\" to a \"mi\"
  \"3-\" transpose a \"do\" to a \"mib\"
  \"4+\" transpose a \"do\" to a \"fad\"
  \"-3\" transpose a \"do\" to a \"lad,\"
"
  (interactive "sTranspose by interval (interval[+-]): ")
  (let* ((num (string-to-number interval))
         (adj (substring interval -1)))
    (save-excursion
      (loop with syntax = (lp:current-syntax)
            with note-diff = (cond ((< num 0) (1+ num))
                                   ((> num 0) (1- num))
                                   (t num))
            with alteration-diff = (cond ((equal adj "+") 2)
                                         ((equal adj "-") -2)
                                         (t 0))
            with beginning = (region-beginning)
            for (form current-line rest-forms) = (lp:form-before-point syntax (region-end))
            then (lp:previous-form current-line rest-forms)
            while (and form (>= (lp:marker form) beginning))
            if (lyqi:note-lexeme-p form)
            do (lyqi:transpose-note form syntax note-diff alteration-diff)
            if (lyqi:simple-note-form-p form)
            do (loop for lexeme in (slot-value form 'children)
                     if (lyqi:note-lexeme-p lexeme)
                     do (lyqi:transpose-note lexeme syntax note-diff alteration-diff))))))

;;;
;;; misc editing commands
;;;

(defun lyqi:insert-pipe-and-return ()
  "Insert a bar check, a line feed, and indent the line"
  (interactive)
  (if (or (bolp) (= (char-before) ?\|))
      (insert "\n")
      (progn
        (lyqi:maybe-insert-space)
        (insert "|\n")
        (lyqi:indent-line))))

(defun lyqi:insert-opening-delimiter (&optional n)
  "Insert an opening delimiter and the corresponding closing
delimiter.  This depends on the current context: in a LilyPond
block, delimiters are {} and <>,
whereas in a Scheme block, delimiters are parens."
  (interactive "p")
  (multiple-value-bind (form line rest-forms)
      (lp:form-before-point (lp:current-syntax) (point))
    (let ((delim last-command-event))
      (if (and form (object-of-class-p form 'lyqi:scheme-lexeme))
          ;; scheme context
          (if (= delim ?\()
              (progn
                (insert "()")
                (backward-char))
              (insert-char delim n))
          ;; LilyPond context
          (let ((closing-delim (cdr (assq delim '((?\< . ?\>) (?\{ . ?\}))))))
            (if closing-delim
                (progn
                  (lyqi:maybe-insert-space)
                  ;; TODO: account for prefix argument
                  (insert (format "%c%c" delim closing-delim))
                  (backward-char))
                (insert-char delim n)))))))

(defun lyqi:insert-closing-delimiter (&optional n)
  "Insert a closing delimiter, unless char after point is the same closing delimiter."
  (interactive "p")
  (let ((delim last-command-event))
    (if (and (not (eolp))
             (= delim (char-after (point))))
        (forward-char)
        (insert-char delim n))))

(defun lyqi:insert-delimiter (&optional n)
  "Insert a delimiter twice, unless char after point is this delimiter."
  (interactive "p")
  (let ((delim last-command-event))
    (if (and (not (eolp))
             (= delim (char-after (point))))
        (forward-char)
        (progn
          (insert-char delim (* 2 (or n 1)))
          (backward-char (or n 1))))))

(provide 'lyqi-editing-commands)
