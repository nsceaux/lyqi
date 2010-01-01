;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'lp-base)
(require 'lyqi-pitchnames)
(require 'lyqi-syntax)

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

(defun lyqi:same-duration-p (duration-number duration-lexeme)
  "Test if `duration-lexeme' is equal to `duration-number',
i.e. if (log duration 2) == length of `duration-lexeme', and
duration lexeme has no dot nor numerator nor denominator."
  (and (= (slot-value duration-lexeme 'length)
          (round (log duration-number 2)))
       (= (slot-value duration-lexeme 'dot-count) 0)
       (= (slot-value duration-lexeme 'numerator) 1)
       (= (slot-value duration-lexeme 'denominator) 1)))

(defun lyqi:change-duration (syntax duration)
  "Change the duration of the music found at or before point.
`duration': a number, e.g. 1 2 4 8 16 32 etc"
  (multiple-value-bind (current-duration previous-duration)
      (lyqi:find-durations-backward syntax (point))
    (when current-duration
      (save-excursion
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
                                         (slot-value syntax 'duration-data)))
                            (mod (1+ (slot-value ref-duration 'dot-count)) 5)
                            (slot-value ref-duration 'numerator)
                            (slot-value ref-duration 'denominator))
                    (values "4" 1 1 1)))
            (save-excursion
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

;;;
;;; Commands on notes
;;; - insert a note
;;; - change a note octave
;;; - change a note alteration
;;;

(defun lyqi:find-note-backward (syntax position)
  "Find note backwards."
  (loop for (form current-line rest-forms) = (lp:form-before-point syntax position)
        then (lp:previous-form current-line rest-forms)
        while form
        for note = (lyqi:form-with-note-p form)
        if note return note))

(defun lyqi:insert-note (syntax note)
  (with-slots (pitch alteration octave-modifier accidental) note
    
    (insert "<note>")))

(defun lyqi:re-insert-note (syntax note)
  (goto-char (lp:marker note))
  (combine-after-change-calls
    (delete-char (lp:size note))
    (lyqi:insert-note syntax note)))

(defun lyqi:change-alteration (syntax pitch alteration-direction)
  "Change "
  (let ((note (lyqi:find-note-backward syntax (point))))
    (when note
      (with-slots (pitch alteration) note
        (let ((new-alteration (cond ((> 0 alteration-direction)  ;; increment
                                     (cond ((= alteration -2) 0) ;;  flat -> neutral
                                           ((= alteration 0) 2)  ;;  neutral -> sharp
                                           ((or (= alteration 2) ;;  sharp -> sharp
                                                (= alteration 4)) alteration)
                                           (t (1+ alteration))))
                                    ((< 0 alteration-direction)   ;; decrement
                                     (cond ((= alteration 2) 0)   ;;  sharp -> neutral
                                           ((= alteration 0) -2)  ;;  neutral -> flat
                                           ((or (= alteration -2) ;;  flat -> flat
                                                (= alteration -4)) alteration)
                                           (t (1- alteration))))
                                    (t 0)))) ;; neutral
          (when (/= alteration new-alteration)
            (set-slot-value note 'alteration new-alteration)
            ;; delete old note, and insert new one
            (save-excursion
              (lyqi:re-insert-note syntax note))
            ;; memorize alteration
            (aset (slot-value syntax 'alterations) pitch new-alteration)))))))


(provide 'lyqi-editing-commands)