;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lp-base)
(require 'lyqi-syntax)
(require 'lyqi-fontify)

(defvar lyqi:prefered-languages '(italiano nederlands))
(defvar lyqi:prefered-octave-mode 'absolute)

(defun lyqi:select-next-language (&optional syntax)
  (interactive)
  (let* ((syntax (or syntax (lp:current-syntax)))
         (current-language (slot-value syntax 'language))
         (relative-mode (slot-value syntax 'relative-mode)))
    (setq lp:*current-syntax*
          (lyqi:make-lilypond-syntax 
           (loop for langs on lyqi:prefered-languages
                 for lang = (first langs)
                 if (eql lang current-language)
                 return (or (cadr langs) (first lyqi:prefered-languages)))
           relative-mode))
  (force-mode-line-update)
  (lp:parse-and-highlight-buffer)))

(defun lyqi:toggle-relative-mode (&optional syntax)
  (interactive)
  (let ((syntax (or syntax (lp:current-syntax))))
    (set-slot-value syntax 'relative-mode
                    (not (slot-value syntax 'relative-mode))))
  (force-mode-line-update))

(defun lyqi:toggle-quick-edit-mode (&optional syntax)
  (interactive)
  (let ((syntax (or syntax (lp:current-syntax))))
    (set-slot-value syntax 'quick-edit-mode
                    (not (slot-value syntax 'quick-edit-mode))))
  (force-mode-line-update))

(defun lyqi:set-header-line-format ()
  (setq header-line-format
        '(" "
          (:eval (propertize (symbol-name (slot-value (lp:current-syntax) 'language))
                             'help-echo "select next language"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap (header-line
                                                  keymap (mouse-1 . lyqi:select-next-language)))))
          " | "
          (:eval (propertize (if (slot-value (lp:current-syntax) 'relative-mode)
                                 "relative"
                                 "absolute")
                             'help-echo "toggle octave mode"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap (header-line
                                                  keymap (mouse-1 . lyqi:toggle-relative-mode)))))
          " mode | "
          (:eval (propertize (if (slot-value (lp:current-syntax) 'quick-edit-mode)
                                 "quick insert"
                                 "normal")
                             'help-echo "toggle edit mode"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap (header-line
                                                  keymap (mouse-1 . lyqi:toggle-quick-edit-mode)))))
          " edition"
          (:eval (if after-change-functions "" " | "))
          (:eval (if after-change-functions
                     ""
                     (propertize "¡BUG!"
                                 'help-echo "re-run lyqi-mode"
                                 'mouse-face 'mode-line-highlight
                                 'local-map '(keymap (header-line
                                                      keymap (mouse-1 . lyqi-mode)))))))))

(defun lyqi-mode ()
  "Major mode for editing LilyPond music files, with quick insertion."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lyqi-mode)
  (setq mode-name "Lyqi")
  ;; local variables
  (make-local-variable 'after-change-functions)
  (setq after-change-functions '(lp:parse-update))
  (make-local-variable 'lp:*current-syntax*)
  (let* ((previous-syntax lp:*current-syntax*)
         (language (or (and previous-syntax
                            (slot-value previous-syntax 'language))
                       (first lyqi:prefered-languages)))
         (relative-mode (if previous-syntax
                            (slot-value previous-syntax 'relative-mode)
                            (eql lyqi:prefered-octave-mode 'relative))))
    (setq lp:*current-syntax*
          (lyqi:make-lilypond-syntax language relative-mode)))
  (lp:parse-and-highlight-buffer)
  ;; header line shows info on lyqi mode
  (lyqi:set-header-line-format))
