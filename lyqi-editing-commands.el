;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'lp-base)
(require 'lyqi-pitchnames)
(require 'lyqi-syntax)

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
  "Find one duration (explicit and explicit) plus one explicit duration, backwards.
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
              (eql (char-before (point)) ?\ )
              (eql (char-before (point)) ?\t))
    (insert " ")))

(defun lyqi:insert-rest ()
  "Insert a rest at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:maybe-insert-space)
    (insert "r")))

(defun lyqi:insert-mm-rest ()
  "Insert a rest at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:maybe-insert-space)
    (insert "R")))

(defun lyqi:insert-spacer ()
  "Insert a skip at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:maybe-insert-space)
    (insert "s")))

(defun lyqi:insert-chord-repetition ()
  "Insert a chord repetition at point"
  (interactive)
  (combine-after-change-calls
    (lyqi:maybe-insert-space)
    (insert "q")))

(defun lyqi:insert-skip ()
  "Insert a \\skip ar point"
  (interactive)
  (combine-after-change-calls
    (lyqi:maybe-insert-space)
    (insert "\\skip 4"))) ;; TODO: use previous duration?

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

(defun lyqi:insert-note (syntax note)
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
      (insert (format "%s%s%s" note-string octave-string accidental-string)))))

(defun lyqi:re-insert-note (syntax note)
  (goto-char (lp:marker note))
  (combine-after-change-calls
    (delete-char (lp:size note))
    (lyqi:insert-note syntax note)))

(defun lyqi:insert-note-by-pitch (pitch)
  (let ((syntax (lp:current-syntax)))
    (combine-after-change-calls
      (lyqi:maybe-insert-space)
      (lyqi:insert-note syntax
                        (make-instance 'lyqi:note-lexeme
                                       :pitch pitch
                                       :alteration (aref (slot-value syntax 'alterations)
                                                         pitch))))))

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
            (aset (slot-value syntax 'alterations) pitch new-alteration)
            ;; delete old note, and insert new one
            (lyqi:save-excursion-unless-farther
              (lyqi:re-insert-note syntax note))))))))

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
              (lyqi:re-insert-note syntax note))))))))

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

(provide 'lyqi-editing-commands)